use std::fmt::{Debug, Display};

use crate::interp::interner::{Interner, Key};
use crate::interp::{function::UserDefined, value::Value};
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
        name: Key,
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
        func: Box<UserDefined>,
    },
    Return {
        loc: Location,
        value: Option<Box<Expr>>,
    },
}

/// A wrapper for `Stmt` that can be displayed. This is necessary to "pass" interner as addtional
/// argument to `Display::fmt` method.
pub struct DisplayedStmt<'a, 'b> {
    stmt: &'a Stmt,
    interner: &'b Interner,
}

pub enum Unwind {
    None,
    Return(Value, Location),
}

impl Stmt {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedStmt<'a, 'b> {
        DisplayedStmt {
            stmt: self,
            interner,
        }
    }
}

impl Display for DisplayedStmt<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.stmt {
            Stmt::Expr { expr } => Display::fmt(&expr.display(interner), f),
            Stmt::Print { expr, .. } => write!(f, "(print {})", expr.display(interner)),
            Stmt::Var { name, init, .. } => {
                let name = interner.resolve(*name);
                match init {
                    Some(val) => write!(f, "(var {} {})", name, val.display(interner)),
                    None => write!(f, "(var {} nil)", name),
                }
            }
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {}", stmt.display(interner))?;
                }
                write!(f, ")")
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => {
                let condition = condition.display(interner);
                let then = then.display(interner);
                match otherwise {
                    Some(other) => {
                        let other = other.display(interner);
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
                let condition = condition.display(interner);
                let body = body.display(interner);
                write!(f, "(while {condition} {body})")
            }
            Stmt::Function { func } => {
                write!(f, "(fun (")?;
                for param in &func.params {
                    write!(f, " {}", interner.resolve(*param))?;
                }
                write!(f, ")")?;
                for stmt in &func.body {
                    write!(f, " {}", stmt.display(interner))?;
                }
                write!(f, ")")
            }
            Stmt::Return { value, .. } => match value {
                Some(val) => write!(f, "(return {})", val.display(interner)),
                None => write!(f, "(return nil)"),
            },
        }
    }
}
