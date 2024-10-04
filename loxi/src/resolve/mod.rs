use std::fmt::Display;

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::interp::interner::Key;
use crate::parse::expr::{ExprId, RefExpr, ValExpr};
use crate::parse::{expr::Expr, stmt::Stmt, Program};
use crate::util::Location;

use self::scope::{Scope, ScopeError, VarBind};

mod scope;

pub struct Resolver {
    scope: Scope,
    resolved_expr: FxHashMap<ExprId, usize>,
    context: Context, // track if we are in a function or not, acts like a stack
}

enum Context {
    Function,
    Method,
    None,
}

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error("{0} SyntaxError: Variable is used in its own initializer")]
    VariableInInitializer(Location),

    #[error("{0} SyntaxError: Variable with the same name already defined at {1}")]
    DuplicateDeclaration(Location, Location),

    #[error("{0} SyntaxError: Stray return statement outside of function")]
    StrayReturn(Location),
}

#[derive(Default)]
pub struct ResolveMap {
    resolved_expr: FxHashMap<ExprId, usize>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            scope: Scope::new_empty(),
            resolved_expr: FxHashMap::default(),
            context: Context::None,
        }
    }

    /// Can resolve multiple times and every time it resolves it will update its internal state and
    /// return a new but updated `ResolveMap`. Useful for REPLs.
    pub fn resolve(&mut self, program: &Program) -> Result<ResolveMap, ResolveError> {
        for stmt in program.statements.iter() {
            self.resolve_stmt(stmt)?;
        }
        Ok(ResolveMap {
            resolved_expr: self.resolved_expr.clone(),
        })
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveError> {
        match stmt {
            Stmt::Expr { expr } => self.resolve_expr(expr),
            Stmt::Print { expr, .. } => self.resolve_expr(expr),
            Stmt::Var { loc, name, init } => {
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
                ..
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then)?;
                match otherwise {
                    Some(otherwise) => self.resolve_stmt(otherwise),
                    _ => Ok(()),
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)
            }
            Stmt::Function { func } => {
                self.declare_and_define_var(func.name, func.loc)?;
                self.resolve_function(&func.params, &func.body, func.loc, Context::Function)
            }
            Stmt::Return { value, loc } => {
                if let Context::None = self.context {
                    Err(ResolveError::StrayReturn(*loc))?;
                }
                match value {
                    Some(value) => self.resolve_expr(value),
                    None => Ok(()),
                }
            }
            Stmt::Class { loc, name, methods } => {
                self.declare_and_define_var(*name, *loc)?;
                // NOTE: methods resolved separately
                for method in methods.iter() {
                    self.resolve_function(
                        &method.params,
                        &method.body,
                        method.loc,
                        Context::Method,
                    )?;
                }
                Ok(())
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        match expr {
            Expr::ValExpr(expr, _id) => self.resolve_val_expr(expr),
            Expr::RefExpr(expr, id) => self.resolve_ref_expr(expr, *id),
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
            ValExpr::Grouping { expr, .. } => self.resolve_val_expr(expr),
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

    fn resolve_ref_expr(&mut self, expr: &RefExpr, id: ExprId) -> Result<(), ResolveError> {
        match expr {
            RefExpr::Variable { var } => {
                let name = var.tok.name;
                if !self.scope.is_empty() {
                    if let Some(VarBind::Decl(loc)) = self.scope.get_current(name).as_deref() {
                        return Err(ResolveError::VariableInInitializer(*loc));
                    }
                }
                self.resolve_local(id, name, var.loc);
                Ok(())
            }
            RefExpr::Grouping { expr, .. } => self.resolve_ref_expr(expr, id),
            RefExpr::Assignment { var, value } => {
                let name = var.tok.name;
                self.resolve_expr(value)?;
                self.resolve_local(id, name, var.loc);
                Ok(())
            }
            RefExpr::Get { object, prop: _ } => {
                // NOTE: prop are looked up dynamically
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
        }
    }

    fn resolve_local(&mut self, expr_id: ExprId, name: Key, loc: Location) {
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
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &[Key],
        body: &[Stmt],
        loc: Location,
        context: Context,
    ) -> Result<(), ResolveError> {
        let mut prev_context = std::mem::replace(&mut self.context, context);
        self.scope.create_scope();

        for param in params.iter() {
            self.declare_and_define_var(*param, loc)?;
        }
        for stmt in body.iter() {
            self.resolve_stmt(stmt)?;
        }

        self.scope.drop_scope();
        std::mem::swap(&mut self.context, &mut prev_context);

        Ok(())
    }

    fn declare_and_define_var(&self, name: Key, loc: Location) -> Result<(), ResolveError> {
        self.declare_var(name, loc)?;
        self.define_var(name, loc);
        Ok(())
    }

    fn declare_var(&self, name: Key, loc: Location) -> Result<(), ResolveError> {
        // variables declared at global scope are not resolved
        if self.scope.is_empty() {
            return Ok(());
        }
        self.scope
            .define(name, VarBind::Decl(loc))
            .map_err(|err| match err {
                ScopeError::DuplicateDefine(prev) => ResolveError::DuplicateDeclaration(loc, prev),
            })
    }

    fn define_var(&self, name: Key, loc: Location) {
        // variables declared at global scope are not resolved
        if self.scope.is_empty() {
            return;
        }
        match self.scope.get_current(name) {
            Some(mut val) => *val = VarBind::Def(loc),
            None => panic!("variable not declared first, programmer error"),
        }
    }
}

impl ResolveError {
    pub fn loc(&self) -> Location {
        match self {
            ResolveError::VariableInInitializer(loc) => *loc,
            ResolveError::DuplicateDeclaration(loc, _) => *loc,
            ResolveError::StrayReturn(loc) => *loc,
        }
    }
}

impl ResolveMap {
    pub fn distance(&self, expr_id: ExprId) -> Option<usize> {
        self.resolved_expr
            .iter()
            .find(|v| *v.0 == expr_id)
            .map(|v| v.1)
            .copied()
    }
}

impl Display for ResolveMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Resolved expressions:")?;
        for (expr_id, distance) in self.resolved_expr.iter() {
            writeln!(f, "  {:?} -> {}", expr_id, distance)?;
        }
        Ok(())
    }
}
