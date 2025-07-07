use std::cell::RefCell;
use std::fmt::Display;
use std::mem;

use rustc_hash::FxHashMap;
use thiserror::Error;
use vec_map::VecMap;

use crate::interp::interner::{Interner, Key};
use crate::parse::ast::Ast;
use crate::parse::expr::{ExprId, ExprL, RefExpr, ValExpr};
use crate::parse::stmt::{StmtFunctionL, StmtId, StmtL};
use crate::parse::{expr::Expr, stmt::Stmt, Program};
use crate::util::Location;

use self::scope::{Scope, ScopeError, VarBind};

mod scope;

pub struct Resolver<'a, 'b> {
    scope: Scope,
    resolved_expr: FxHashMap<ExprId, usize>,
    global_refs: RefCell<FxHashMap<Key, ExprId>>, // track global references (may be undefined)
    func_context: FunctionContext,
    class_context: ClassContext,
    interner: &'a Interner,
    ast: &'b Ast,
}

enum FunctionContext {
    Function,
    Method,
    Constructor,
    None,
}

enum ClassContext {
    Class,
    SubClass, // A class that inherits from another class
    None,
}

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error("{0} SyntaxError: Variable is used in its own initializer")]
    VariableInInitializer(Location),

    #[error("{0} SyntaxError: A class that inherits from itself doesn't make sense")]
    ClassInheritItself(Location),

    #[error("{0} SyntaxError: Variable with the same name already defined at {1}")]
    DuplicateDeclaration(Location, Location),

    #[error("{0} SyntaxError: Stray return statement outside of function")]
    StrayReturn(Location),

    #[error("{0} SyntaxError: Stray 'this' outside of a class")]
    StrayThis(Location),

    #[error("{0} SyntaxError: Can't have a 'super' outside of a class")]
    StraySuper(Location),

    #[error("{0} SyntaxError: Can't have a 'super' inside a non-inheriting class")]
    NonInheritingSuper(Location),

    #[error("{0} SyntaxError: Can't return a value from initializer")]
    FobiddenReturn(Location),

    #[error("{0} LogicError: Trying to access undefined variable: '{1}'")]
    UndefinedVariable(Location, String),
}

#[derive(Default)]
pub struct ResolveMap {
    resolved_expr: VecMap<usize>,
}

impl Resolver<'_, '_> {
    pub fn new<'a, 'b>(interner: &'a Interner, ast: &'b Ast) -> Resolver<'a, 'b> {
        Resolver {
            scope: Scope::new_empty(),
            resolved_expr: FxHashMap::default(),
            global_refs: RefCell::default(),
            func_context: FunctionContext::None,
            class_context: ClassContext::None,
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

        // check whether global references reference to valid defined variables
        let mut global_refs = self.global_refs.take();
        for global in self.scope.globals() {
            global_refs.remove_entry(&global);
        }

        if !global_refs.is_empty() {
            return Err(global_refs
                .into_iter()
                .map(|(k, v)| {
                    ResolveError::UndefinedVariable(
                        self.ast.get_expr(&v).loc,
                        self.interner.resolve(k).to_owned(),
                    )
                })
                .collect());
        }

        Ok(ResolveMap {
            resolved_expr: self
                .resolved_expr
                .clone()
                .into_iter()
                .map(|(k, v)| (k.inner(), v))
                .collect(),
        })
    }

    fn resolve_stmt(&mut self, stmt: &StmtId) -> Result<(), ResolveError> {
        let StmtL { stmt, loc } = self.ast.get_stmt(stmt);
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
                self.scope.create_scope();
                for stmt in statements.iter() {
                    self.resolve_stmt(stmt)?
                }
                self.scope.drop_scope();
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
            Stmt::Function { func } => {
                let func = &self.ast.get_func(func).func;
                self.declare_and_define_var(func.name, *loc)?;
                let body = match &self.ast.get_stmt(&func.body).stmt {
                    Stmt::Block { statements } => statements,
                    _ => unreachable!("Shold be block"),
                };
                self.resolve_function(&func.params, body, *loc, FunctionContext::Function)
            }
            Stmt::Return { value } => {
                match self.func_context {
                    FunctionContext::None => Err(ResolveError::StrayReturn(*loc))?,
                    FunctionContext::Constructor => {
                        if value.is_some() {
                            Err(ResolveError::FobiddenReturn(*loc))?
                        }
                    }
                    _ => (),
                };
                match value {
                    Some(value) => self.resolve_expr(value),
                    None => Ok(()),
                }
            }
            Stmt::Class {
                name,
                base,
                methods,
            } => {
                let mut prev_context = mem::replace(&mut self.class_context, ClassContext::Class);
                self.declare_and_define_var(*name, *loc)?;

                if let Some(expr_id) = base {
                    let ExprL { expr, loc } = self.ast.get_expr(expr_id);
                    match expr {
                        Expr::RefExpr(RefExpr::Variable { var }) => {
                            if var.name == *name {
                                Err(ResolveError::ClassInheritItself(*loc))?;
                            }

                            self.class_context = ClassContext::SubClass;
                            self.resolve_expr(expr_id)?;

                            self.scope.create_scope();
                            let superr = self.interner.key_super;
                            self.declare_and_define_var(superr, *loc)?;
                        }
                        _ => unreachable!("base should be a RefExpr::Variable"),
                    }
                }

                self.scope.create_scope();
                let this = self.interner.key_this;
                self.declare_and_define_var(this, *loc)?;

                for method in methods.iter() {
                    let StmtFunctionL { func, loc } = self.ast.get_func(method);
                    let context = match func.name == self.interner.key_init {
                        true => FunctionContext::Constructor,
                        false => FunctionContext::Method,
                    };
                    let body = match &self.ast.get_stmt(&func.body).stmt {
                        Stmt::Block { statements } => statements,
                        _ => unreachable!("Shold be block"),
                    };
                    self.resolve_function(&func.params, body, *loc, context)?;
                }
                self.scope.drop_scope();

                if base.is_some() {
                    self.scope.drop_scope();
                }

                mem::swap(&mut self.class_context, &mut prev_context);
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
        loc: Location,
    ) -> Result<(), ResolveError> {
        match expr {
            RefExpr::Variable { var } => {
                let name = var.name;
                if !self.scope.is_empty() {
                    if let Some(VarBind::Decl(loc)) = self.scope.get_current(name).as_deref() {
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
                // prop are looked up dynamically
                self.resolve_expr(object)
            }
            RefExpr::Set {
                object,
                prop: _,
                value,
            } => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)
            }
            RefExpr::This => match self.class_context {
                ClassContext::None => Err(ResolveError::StrayThis(loc)),
                _ => {
                    self.resolve_local(*id, self.interner.get("this"));
                    Ok(())
                }
            },
            RefExpr::Super { prop: _ } => match self.class_context {
                ClassContext::None => Err(ResolveError::StraySuper(loc)),
                ClassContext::Class => Err(ResolveError::NonInheritingSuper(loc)),
                _ => {
                    // prop are looked up dynamically
                    let superr = self.interner.key_super;
                    self.resolve_local(*id, superr);
                    Ok(())
                }
            },
        }
    }

    fn resolve_local(&mut self, expr_id: ExprId, name: Key) {
        if self.scope.is_empty() {
            return;
        }

        let result = self.scope.get(name);
        match result {
            Some((_, distance)) => {
                self.resolved_expr.insert(expr_id, distance);
            }
            None => {
                // if we can't find the variable we assume it's global
                self.global_refs.borrow_mut().insert(name, expr_id);
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &[Key],
        body: &[StmtId],
        loc: Location,
        context: FunctionContext,
    ) -> Result<(), ResolveError> {
        let mut prev_context = mem::replace(&mut self.func_context, context);
        self.scope.create_scope();

        for param in params.iter() {
            self.declare_and_define_var(*param, loc)?;
        }
        for stmt in body.iter() {
            self.resolve_stmt(stmt)?;
        }

        self.scope.drop_scope();
        mem::swap(&mut self.func_context, &mut prev_context);

        Ok(())
    }

    fn declare_and_define_var(&self, name: Key, loc: Location) -> Result<(), ResolveError> {
        self.declare_var(name, loc)?;
        self.define_var(name, loc);
        Ok(())
    }

    fn declare_var(&self, name: Key, loc: Location) -> Result<(), ResolveError> {
        let define = match self.scope.is_empty() {
            true => Scope::define_global,
            false => Scope::define,
        };
        define(&self.scope, name, VarBind::Decl(loc)).map_err(|err| match err {
            ScopeError::DuplicateDefine(prev) => ResolveError::DuplicateDeclaration(loc, prev),
        })
    }

    fn define_var(&self, name: Key, loc: Location) {
        let get = match self.scope.is_empty() {
            true => Scope::get_global,
            false => Scope::get_current,
        };
        match get(&self.scope, name) {
            Some(mut val) => *val = VarBind::Def(loc),
            None => panic!("variable not declared first, programmer error"),
        }
        if self.scope.is_empty() {
            self.global_refs.borrow_mut().remove_entry(&name);
        }
    }
}

impl ResolveError {
    pub fn loc(&self) -> Location {
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
}

impl Display for ResolveMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Resolved expressions {}:", self.resolved_expr.len())?;
        for (expr_id, distance) in self.resolved_expr.iter() {
            writeln!(f, "  {:?} -> {}", expr_id, distance)?;
        }
        Ok(())
    }
}
