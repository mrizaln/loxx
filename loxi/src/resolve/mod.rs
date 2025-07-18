use std::fmt::Display;
use std::mem;

use rustc_hash::FxHashMap;
use thiserror::Error;
use vec_map::VecMap;

use crate::interp::interner::{Interner, Key};
use crate::parse::ast::Ast;
use crate::parse::expr::{ExprId, ExprL, RefExpr, ValExpr};
use crate::parse::stmt::{StmtFunctionId, StmtFunctionL, StmtId, StmtL};
use crate::parse::{expr::Expr, stmt::Stmt, Program};
use crate::util::Loc;

use self::scope::{Bind, Captures, Class, Fun, Hoists, Kind, Scope, ScopeError};

pub mod scope;

pub struct Resolver<'a, 'b> {
    scope: Scope,
    resolved_expr: FxHashMap<ExprId, usize>,
    global_refs: FxHashMap<Key, Vec<ExprId>>, // track global references (may be undefined)
    captures_map: FxHashMap<StmtFunctionId, Captures>,
    hoists_map: FxHashMap<StmtId, Hoists>,
    interner: &'a Interner,
    ast: &'b Ast,
}

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error("{0} SyntaxError: Variable is used in its own initializer")]
    VariableInInitializer(Loc),

    #[error("{0} SyntaxError: A class that inherits from itself doesn't make sense")]
    ClassInheritItself(Loc),

    #[error("{0} SyntaxError: Variable with the same name already defined at {1}")]
    DuplicateDeclaration(Loc, Loc),

    #[error("{0} SyntaxError: Stray return statement outside of function")]
    StrayReturn(Loc),

    #[error("{0} SyntaxError: Stray 'this' outside of a class")]
    StrayThis(Loc),

    #[error("{0} SyntaxError: Can't have a 'super' outside of a class")]
    StraySuper(Loc),

    #[error("{0} SyntaxError: Can't have a 'super' inside a non-inheriting class")]
    NonInheritingSuper(Loc),

    #[error("{0} SyntaxError: Can't return a value from initializer")]
    FobiddenReturn(Loc),

    #[error("{0} LogicError: Trying to access undefined variable: '{1}'")]
    UndefinedVariable(Loc, String),
}

#[derive(Default)]
pub struct ResolveMap {
    resolved_expr: VecMap<usize>,
    captures_map: FxHashMap<StmtFunctionId, Captures>,
    hoists_map: FxHashMap<StmtId, Hoists>,
    globals: Vec<(Key, Loc)>,
}

impl Resolver<'_, '_> {
    pub fn new<'a, 'b>(interner: &'a mut Interner, ast: &'b Ast) -> Resolver<'a, 'b> {
        let scope = Scope::new();
        let mut define = |name: &str| {
            let name = interner.get_or_intern(name);
            scope
                .define_global(name, Bind::Def(Loc::default()))
                .ok()
                .unwrap();
        };

        define("clock");

        #[cfg(feature = "loxlox")]
        {
            define("getc");
            define("chr");
            define("exit");
            define("print_error");
        }

        Resolver {
            scope,
            resolved_expr: FxHashMap::default(),
            global_refs: FxHashMap::default(),
            captures_map: FxHashMap::default(),
            hoists_map: FxHashMap::default(),
            interner,
            ast,
        }
    }

    /// Can resolve multiple times and every time it resolves it will update its internal state and
    /// return a new but updated `ResolveMap`. Useful for REPLs.
    pub fn resolve(&mut self, program: &Program) -> Result<ResolveMap, Vec<ResolveError>> {
        for stmt in program.statements.iter() {
            if let Err(e) = self.resolve_stmt(stmt) {
                return Err(vec![e]);
            }
        }

        let globals = self.scope.globals();

        // check whether global references reference to valid defined variables
        let mut global_refs = mem::take(&mut self.global_refs);
        for name in globals.iter().map(|v| v.0) {
            global_refs.remove_entry(&name);
        }

        if !global_refs.is_empty() {
            let unresolved = global_refs.iter().flat_map(|(name, exprs)| {
                exprs.iter().map(|v| {
                    ResolveError::UndefinedVariable(
                        self.ast.get_expr(v).loc,
                        self.interner.resolve(*name).to_owned(),
                    )
                })
            });
            let mut unresolved = unresolved.collect::<Vec<_>>();
            unresolved.sort_by_key(|l| l.loc());
            return Err(unresolved);
        }

        let resolved_expr = self
            .resolved_expr
            .clone()
            .into_iter()
            .map(|(k, v)| (k.inner(), v))
            .collect();

        Ok(ResolveMap {
            resolved_expr,
            captures_map: mem::take(&mut self.captures_map),
            hoists_map: mem::take(&mut self.hoists_map),
            globals,
        })
    }

    fn resolve_stmt(&mut self, id: &StmtId) -> Result<(), ResolveError> {
        let StmtL { stmt, loc } = self.ast.get_stmt(id);
        match stmt {
            Stmt::Expr { expr } => self.resolve_expr(expr),
            Stmt::Print { expr } => self.resolve_expr(expr),
            Stmt::Var { name, init } => {
                self.declare_var(*name, *loc)?;
                if let Some(init) = init {
                    self.resolve_expr(init)?;
                };
                self.define_var(*name, *loc);
                Ok(())
            }
            Stmt::Block { statements } => {
                self.scope.create_scope(Kind::Block);
                for stmt in statements.iter() {
                    self.resolve_stmt(stmt)?
                }
                let (_, hoists) = self.scope.drop_scope();
                self.hoists_map.insert(*id, hoists);
                Ok(())
            }
            Stmt::If {
                condition,
                then,
                otherwise,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then)?;
                match otherwise {
                    Some(otherwise) => self.resolve_stmt(otherwise),
                    _ => Ok(()),
                }
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)
            }
            Stmt::Function { func: func_id } => {
                let func = &self.ast.get_func(func_id).func;
                self.declare_and_define_var(func.name, *loc)?;
                let body = match &self.ast.get_stmt(&func.body).stmt {
                    Stmt::Block { statements } => statements,
                    _ => unreachable!("Shold be block"),
                };
                let (captures, hoists) = self.resolve_function(&func.params, body, Fun::Reg)?;
                self.captures_map.insert(*func_id, captures);
                self.hoists_map.insert(func.body, hoists);
                Ok(())
            }
            Stmt::Return { value } => match self.scope.in_fun_scope() {
                None => Err(ResolveError::StrayReturn(*loc)),
                Some(Fun::Ctor) if value.is_some() => Err(ResolveError::FobiddenReturn(*loc)),
                Some(_) => match value {
                    Some(value) => self.resolve_expr(value),
                    None => Ok(()),
                },
            },
            Stmt::Class {
                name,
                base,
                methods,
            } => {
                self.declare_and_define_var(*name, *loc)?;

                if let Some(expr_id) = base {
                    let ExprL { expr, loc } = self.ast.get_expr(expr_id);
                    match expr {
                        Expr::RefExpr(RefExpr::Variable { var }) => {
                            if var.name == *name {
                                Err(ResolveError::ClassInheritItself(*loc))?;
                            }
                            self.resolve_expr(expr_id)?;

                            let kind = match self.scope.in_class_scope() {
                                Some(_) => Class::Sub,
                                None => Class::Reg,
                            };

                            self.scope.create_scope(Kind::Class(kind));
                            self.declare_and_define_var(self.interner.key_super, *loc)?;
                        }
                        _ => unreachable!("base should be a RefExpr::Variable"),
                    }
                    self.scope.create_scope(Kind::Class(Class::Sub));
                } else {
                    self.scope.create_scope(Kind::Class(Class::Reg));
                }

                self.declare_and_define_var(self.interner.key_this, *loc)?;

                for method in methods.iter() {
                    let StmtFunctionL { func, loc: _ } = self.ast.get_func(method);
                    let kind = match func.name == self.interner.key_init {
                        true => Fun::Ctor,
                        false => Fun::Method,
                    };
                    let body = match &self.ast.get_stmt(&func.body).stmt {
                        Stmt::Block { statements } => statements,
                        _ => unreachable!("Shold be block"),
                    };
                    let (captures, hoists) = self.resolve_function(&func.params, body, kind)?;
                    self.captures_map.insert(*method, captures);
                    self.hoists_map.insert(func.body, hoists);
                }

                let _ = self.scope.drop_scope();
                if base.is_some() {
                    let _ = self.scope.drop_scope();
                }

                Ok(())
            }

            #[cfg(feature = "debug")]
            Stmt::Debug { expr } => self.resolve_expr(expr),
        }
    }

    fn resolve_expr(&mut self, expr_id: &ExprId) -> Result<(), ResolveError> {
        let ExprL { expr, loc } = self.ast.get_expr(expr_id);
        match expr {
            Expr::ValExpr(expr) => self.resolve_val_expr(expr),
            Expr::RefExpr(expr) => self.resolve_ref_expr(expr, expr_id, *loc),
        }
    }

    fn resolve_val_expr(&mut self, expr: &ValExpr) -> Result<(), ResolveError> {
        match expr {
            ValExpr::Literal { .. } => Ok(()),
            ValExpr::Unary { right, .. } => self.resolve_expr(right),
            ValExpr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            ValExpr::Grouping { expr, .. } => self.resolve_expr(expr),
            ValExpr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            }
            ValExpr::Call { callee, args, .. } => {
                self.resolve_expr(callee)?;
                for arg in args.iter() {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
        }
    }

    fn resolve_ref_expr(
        &mut self,
        expr: &RefExpr,
        id: &ExprId,
        loc: Loc,
    ) -> Result<(), ResolveError> {
        match expr {
            RefExpr::Variable { var } => {
                let name = var.name;
                if !self.scope.is_empty() {
                    if let Some(Bind::Decl(loc)) = self.scope.get_current(name).as_deref() {
                        return Err(ResolveError::VariableInInitializer(*loc));
                    }
                }
                self.resolve_local(*id, name);
                Ok(())
            }
            RefExpr::Grouping { expr, .. } => self.resolve_expr(expr),
            RefExpr::Assignment { var, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(*id, var.name);
                Ok(())
            }
            RefExpr::Get { object, prop: _ } => {
                // prop is looked up dynamically
                self.resolve_expr(object)
            }
            RefExpr::Set {
                object,
                prop: _,
                value,
            } => {
                // prop is looked up dynamically
                self.resolve_expr(value)?;
                self.resolve_expr(object)
            }
            RefExpr::This => match self.scope.in_class_scope() {
                None => Err(ResolveError::StrayThis(loc)),
                Some(_) => {
                    self.resolve_local(*id, self.interner.key_this);
                    Ok(())
                }
            },
            RefExpr::Super { prop: _ } => match self.scope.in_class_scope() {
                None => Err(ResolveError::StraySuper(loc)),
                Some(Class::Reg) => Err(ResolveError::NonInheritingSuper(loc)),
                Some(Class::Sub) => {
                    // prop are looked up dynamically
                    self.resolve_local(*id, self.interner.key_super);
                    Ok(())
                }
            },
        }
    }

    fn resolve_local(&mut self, expr_id: ExprId, name: Key) {
        if !self.scope.is_empty() {
            if let Some(distance) = self.scope.resolve(name) {
                self.resolved_expr.insert(expr_id, distance);
                return;
            }
        }
        // assume global
        self.global_refs.entry(name).or_default().push(expr_id)
    }

    fn resolve_function(
        &mut self,
        params: &[(Key, Loc)],
        body: &[StmtId],
        kind: Fun,
    ) -> Result<(Captures, Hoists), ResolveError> {
        self.scope.create_scope(Kind::Fun(kind));

        for (param, loc) in params.iter() {
            self.declare_and_define_var(*param, *loc)?;
        }
        for stmt in body.iter() {
            self.resolve_stmt(stmt)?;
        }

        Ok(self.scope.drop_scope())
    }

    fn declare_and_define_var(&mut self, name: Key, loc: Loc) -> Result<(), ResolveError> {
        self.declare_var(name, loc)?;
        self.define_var(name, loc);
        Ok(())
    }

    fn declare_var(&mut self, name: Key, loc: Loc) -> Result<(), ResolveError> {
        let define = match self.scope.is_empty() {
            true => Scope::define_global,
            false => Scope::define,
        };
        define(&self.scope, name, Bind::Decl(loc)).map_err(|err| match err {
            ScopeError::DuplicateDefine(prev) => ResolveError::DuplicateDeclaration(loc, prev),
        })
    }

    fn define_var(&mut self, name: Key, loc: Loc) {
        let get = match self.scope.is_empty() {
            true => Scope::get_global,
            false => Scope::get_current,
        };
        match get(&self.scope, name) {
            Some(mut val) => *val = Bind::Def(loc),
            None => panic!("variable not declared first, programmer error"),
        }
        if self.scope.is_empty() {
            self.global_refs.remove_entry(&name);
        }
    }
}

impl ResolveError {
    pub fn loc(&self) -> Loc {
        match self {
            ResolveError::VariableInInitializer(loc) => *loc,
            ResolveError::DuplicateDeclaration(loc, _) => *loc,
            ResolveError::StrayReturn(loc) => *loc,
            ResolveError::StrayThis(loc) => *loc,
            ResolveError::FobiddenReturn(loc) => *loc,
            ResolveError::ClassInheritItself(loc) => *loc,
            ResolveError::StraySuper(loc) => *loc,
            ResolveError::NonInheritingSuper(loc) => *loc,
            ResolveError::UndefinedVariable(loc, _) => *loc,
        }
    }
}

impl ResolveMap {
    pub fn distance(&self, expr_id: ExprId) -> Option<usize> {
        self.resolved_expr.get(expr_id.inner()).copied()
    }

    pub fn get_captures(&self, func_id: StmtFunctionId) -> Option<&Captures> {
        self.captures_map.get(&func_id)
    }

    pub fn get_hoists(&self, stmt_id: StmtId) -> Option<&Hoists> {
        self.hoists_map.get(&stmt_id)
    }

    pub fn globals(&self) -> &[(Key, Loc)] {
        &self.globals
    }
}

impl Display for ResolveMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Resolved expressions {}:", self.resolved_expr.len())?;
        for (expr_id, distance) in self.resolved_expr.iter() {
            writeln!(f, "  {expr_id:?} -> {distance}")?;
        }
        Ok(())
    }
}
