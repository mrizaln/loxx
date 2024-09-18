use std::fmt::{Debug, Display};

use crate::util::Location;

use super::expr::Expr;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Expr {
        expr: Expr,
    },
    Print {
        loc: Location,
        expr: Expr,
    },
    Var {
        loc: Location,
        name: String,
        init: Option<Expr>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Display::fmt(&expr, f),
            Stmt::Print { expr, .. } => write!(f, "('print' {expr})"),
            Stmt::Var { name, init, .. } => match init {
                Some(val) => write!(f, "('var' {name} {val})"),
                None => write!(f, "('var' {name} nil)"),
            },
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Debug::fmt(&expr, f),
            Stmt::Print { loc, expr } => write!(f, "('print'{loc} {expr})"),
            Stmt::Var { loc, name, init } => match init {
                Some(val) => write!(f, "('var'{loc} {name} {val})"),
                None => write!(f, "('var'{loc} {name} nil)"),
            },
        }
    }
}
