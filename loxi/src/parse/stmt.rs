use std::fmt::{Debug, Display};

use crate::util::Location;

use super::expr::Expr;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Expr(Expr),
    Print { loc: Location, expr: Expr },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => Display::fmt(&expr, f),
            Stmt::Print { expr, .. } => write!(f, "('print' {expr})"),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => Debug::fmt(&expr, f),
            Stmt::Print { loc, expr } => write!(f, "('print'{loc} {expr})"),
        }
    }
}
