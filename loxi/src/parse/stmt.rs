use std::fmt::{Debug, Display};

use lasso::{Rodeo, Spur};

use crate::interp::{function::Function, value::Value};
use crate::util::Location;

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>,
    },
    Print {
        loc: Location,
        expr: Box<Expr>,
    },
    Var {
        loc: Location,
        name: Spur,
        init: Option<Box<Expr>>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        loc: Location,
        condition: Box<Expr>,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    While {
        loc: Location,
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Function {
        func: Box<Function>,
    },
    Return {
        loc: Location,
        value: Option<Box<Expr>>,
    },
}

/// A wrapper for `Stmt` that can be displayed. This is necessary to "pass" arena as addtional
/// argument to `Display::fmt` method.
pub struct DisplayedStmt<'a, 'b> {
    stmt: &'a Stmt,
    arena: &'b Rodeo,
}

pub enum Unwind {
    None,
    Return(Value, Location),
}

impl Stmt {
    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedStmt<'a, 'b> {
        DisplayedStmt { stmt: self, arena }
    }
}

impl Display for DisplayedStmt<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.stmt {
            Stmt::Expr { expr } => Display::fmt(&expr.display(arena), f),
            Stmt::Print { expr, .. } => write!(f, "(print {})", expr.display(arena)),
            Stmt::Var { name, init, .. } => {
                let name = arena.resolve(name);
                match init {
                    Some(val) => write!(f, "(var {} {})", name, val.display(arena)),
                    None => write!(f, "(var {} nil)", name),
                }
            }
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {}", stmt.display(arena))?;
                }
                write!(f, ")")
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => {
                let condition = condition.display(arena);
                let then = then.display(arena);
                match otherwise {
                    Some(other) => {
                        let other = other.display(arena);
                        write!(f, "(if-else {condition} {then} {other})")
                    }
                    None => {
                        write!(f, "(if {condition} {then})")
                    }
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                let condition = condition.display(arena);
                let body = body.display(arena);
                write!(f, "(while {condition} {body})")
            }
            Stmt::Function { func } => {
                write!(f, "(fun (")?;
                for param in &func.params {
                    write!(f, " {}", arena.resolve(param))?;
                }
                write!(f, ")")?;
                for stmt in &func.body {
                    write!(f, " {}", stmt.display(arena))?;
                }
                write!(f, ")")
            }
            Stmt::Return { value, .. } => match value {
                Some(val) => write!(f, "(return {})", val.display(arena)),
                None => write!(f, "(return nil)"),
            },
        }
    }
}

impl Unwind {
    pub fn expect_none(self) {
        match self {
            Unwind::None => {}
            _ => panic!("expected no value"),
        }
    }
}
