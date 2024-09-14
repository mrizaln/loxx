use std::fmt::{Debug, Display};

use super::token;
use crate::util::TokLoc;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Expr {
    Literal {
        value: TokLoc<token::Literal>,
    },
    Unary {
        operator: TokLoc<token::UnaryOp>,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: TokLoc<token::BinaryOp>,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            Expr::Literal { value } => (&value.tok).into(),
            Expr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                format!("({op} {right})")
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                format!("({op} {left} {right})")
            }

            Expr::Grouping { expr } => {
                format!("('group' {expr})")
            }
        };
        write!(f, "{}", string)
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            Expr::Literal { value } => {
                let str: String = (&value.tok).into();
                format!("{}{}", str, value.loc)
            }
            Expr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                format!("({op}{} {right:?})", operator.loc)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                format!("({op}{} {left:?} {right:?})", operator.loc)
            }

            Expr::Grouping { expr } => {
                format!("('group' {expr:?})")
            }
        };
        write!(f, "{}", string)
    }
}
