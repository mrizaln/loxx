use std::fmt::{Debug, Display};

use super::token;

///! Lox Grammar (unfinished)
///  ------------------------
/// expression -> literal | unary | binary | grouping ;
/// literal    -> NUMBER | STRING | "true" | "false" | "nil" ;
/// grouping   -> "(" expression ")"
/// unary      -> ( "-" | "!" ) expression ;
/// binary     -> expression operator expression ;
/// operator   -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" ;

// TODO: create another enum for token but in terms of Expr not lexeme

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Expr {
    Literal {
        value: token::Literal,
    },
    Unary {
        operator: token::UnaryOp,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: token::BinaryOp,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            Expr::Literal { value } => value.into(),
            Expr::Unary { operator, right } => {
                let op: &str = operator.into();
                format!("({op} {right})")
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = operator.into();
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
        Display::fmt(self, f)
    }
}
