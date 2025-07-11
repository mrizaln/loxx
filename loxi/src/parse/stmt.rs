use std::fmt::Display;

use crate::interp::interner::{Interner, Key};
use crate::interp::value::Value;
use crate::util::Location;

use super::ast::Ast;
use super::expr::ExprId;

pub enum Unwind {
    Return(Value),
    None,
}

pub struct StmtL {
    pub stmt: Stmt,
    pub loc: Location,
}

pub enum Stmt {
    Expr {
        expr: ExprId,
    },
    Print {
        expr: ExprId,
    },
    Var {
        name: Key,
        init: Option<ExprId>,
    },
    Block {
        statements: Box<[StmtId]>,
    },
    If {
        condition: ExprId,
        then: StmtId,
        otherwise: Option<StmtId>,
    },
    While {
        condition: ExprId,
        body: StmtId,
    },
    Function {
        func: StmtFunctionId,
    },
    Return {
        value: Option<ExprId>,
    },
    Class {
        name: Key,
        base: Option<ExprId>,
        methods: Box<[StmtFunctionId]>,
    },

    #[cfg(feature = "debug")]
    Debug {
        expr: ExprId,
    },
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct StmtId {
    id: usize,
}

pub struct StmtFunctionL {
    pub func: StmtFunction,
    pub loc: Location,
}

pub struct StmtFunction {
    pub name: Key,
    pub params: Box<[(Key, Location)]>,
    pub body: StmtId,                   // only Stmt::Block is valid here
    pub captures: Vec<(Key, Location)>, // captures will be filled after resolve stage
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct StmtFunctionId {
    id: usize,
}

/// A wrapper for `Stmt` that can be displayed. This is necessary to "pass" interner as addtional
/// argument to `Display::fmt` method.
pub struct DisplayedStmt<'a, 'b, 'c> {
    stmt: &'a Stmt,
    interner: &'b Interner,
    ast: &'c Ast,
}

pub struct DisplayedStmtFunction<'a, 'b, 'c> {
    func: &'a StmtFunction,
    interner: &'b Interner,
    ast: &'c Ast,
}

impl Stmt {
    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedStmt<'a, 'b, 'c> {
        DisplayedStmt {
            stmt: self,
            interner,
            ast,
        }
    }

    pub fn expr(expr: ExprId) -> Self {
        Self::Expr { expr }
    }

    pub fn print(expr: ExprId) -> Self {
        Self::Print { expr }
    }

    pub fn var(name: Key, init: Option<ExprId>) -> Self {
        Self::Var { name, init }
    }

    pub fn block(statements: Box<[StmtId]>) -> Self {
        Self::Block { statements }
    }

    pub fn if_else(condition: ExprId, then: StmtId, otherwise: StmtId) -> Self {
        Self::If {
            condition,
            then,
            otherwise: Some(otherwise),
        }
    }

    pub fn if_(condition: ExprId, then: StmtId) -> Self {
        Self::If {
            condition,
            then,
            otherwise: None,
        }
    }

    pub fn while_(condition: ExprId, body: StmtId) -> Self {
        Self::While { condition, body }
    }

    pub fn function(func: StmtFunctionId) -> Self {
        Self::Function { func }
    }

    pub fn return_(value: Option<ExprId>) -> Self {
        Self::Return { value }
    }

    pub fn class(name: Key, base: Option<ExprId>, methods: Box<[StmtFunctionId]>) -> Self {
        Self::Class {
            name,
            base,
            methods,
        }
    }

    #[cfg(feature = "debug")]
    pub fn debug(expr: ExprId) -> Self {
        Self::Debug { expr }
    }
}

impl StmtId {
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn inner(&self) -> usize {
        self.id
    }
}

impl StmtFunctionId {
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn inner(&self) -> usize {
        self.id
    }
}

impl Display for DisplayedStmt<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        let ast = self.ast;

        match self.stmt {
            Stmt::Expr { expr } => {
                let expr = &ast.get_expr(expr).expr;
                Display::fmt(&expr.display(interner, ast), f)
            }
            Stmt::Print { expr } => {
                let expr = &ast.get_expr(expr).expr;
                write!(f, "(print {})", expr.display(interner, ast))
            }
            Stmt::Var { name, init } => {
                let name = interner.resolve(*name);
                match init {
                    Some(val) => {
                        let val = &ast.get_expr(val).expr;
                        write!(f, "(var {name} {})", val.display(interner, ast))
                    }
                    None => write!(f, "(var {name} nil)"),
                }
            }
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    let stmt = &ast.get_stmt(stmt).stmt;
                    write!(f, " {}", stmt.display(interner, ast))?;
                }
                write!(f, ")")
            }
            Stmt::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = ast.get_expr(condition).expr.display(interner, ast);
                let then = ast.get_stmt(then).stmt.display(interner, ast);
                match otherwise {
                    Some(other) => {
                        let other = ast.get_stmt(other).stmt.display(interner, ast);
                        write!(f, "(if-else {condition} {then} {other})")
                    }
                    None => {
                        write!(f, "(if {condition} {then})")
                    }
                }
            }
            Stmt::While { condition, body } => {
                let condition = ast.get_expr(condition).expr.display(interner, ast);
                let body = ast.get_stmt(body).stmt.display(interner, ast);
                write!(f, "(while {condition} {body})")
            }
            Stmt::Function { func } => {
                let func = &ast.get_func(func).func;
                write!(f, "{}", func.display(interner, ast))
            }
            Stmt::Return { value } => match value {
                Some(val) => {
                    let val = &ast.get_expr(val).expr;
                    write!(f, "(return {})", val.display(interner, ast))
                }
                None => write!(f, "(return nil)"),
            },
            Stmt::Class { name, methods, .. } => {
                // TODO: show inheritance list
                write!(f, "(class {}", interner.resolve(*name))?;
                for func in methods {
                    let func = &ast.get_func(func).func;
                    write!(f, " {}", func.display(interner, ast))?;
                }
                write!(f, ")")
            }

            #[cfg(feature = "debug")]
            Stmt::Debug { expr } => {
                let expr = &ast.get_expr(expr).expr;
                write!(f, "(debug {})", expr.display(interner, ast))
            }
        }
    }
}

impl StmtFunction {
    pub fn new(name: Key, params: Box<[(Key, Location)]>, body: StmtId) -> Self {
        Self {
            name,
            params,
            body,
            captures: Vec::default(),
        }
    }

    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedStmtFunction<'a, 'b, 'c> {
        DisplayedStmtFunction {
            func: self,
            interner,
            ast,
        }
    }
}

impl Display for DisplayedStmtFunction<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        let func = self.func;
        let ast = self.ast;

        write!(f, "(fun {} (", interner.resolve(func.name))?;
        for param in &func.params {
            write!(f, " {}", interner.resolve(param.0))?;
        }
        write!(f, ")")?;
        let body = match &ast.get_stmt(&func.body).stmt {
            Stmt::Block { statements } => statements,
            _ => unreachable!(),
        };
        for stmt in body {
            let stmt = &self.ast.get_stmt(stmt).stmt;
            write!(f, " {}", stmt.display(interner, ast))?;
        }
        write!(f, ")")
    }
}
