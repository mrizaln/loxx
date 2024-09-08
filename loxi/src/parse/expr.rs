use std::fmt::{Debug, Display};

use crate::lex::token::{tokens, Token, TokenValue};

///! Lox Grammar (unfinished)
///  ------------------------
/// expression -> literal | unary | binary | grouping ;
/// literal    -> NUMBER | STRING | "true" | "false" | "nil" ;
/// grouping   -> "(" expression ")"
/// unary      -> ( "-" | "!" ) expression ;
/// binary     -> expression operator expression ;
/// operator   -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" ;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Expr {
    Literal {
        value: Token,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            Expr::Literal { value } => match &value.value {
                TokenValue::Literal(value) => value.into(),
                TokenValue::Keyword(value) => match &value {
                    tokens::Keyword::True => Into::<&str>::into(value).to_string(),
                    tokens::Keyword::False => Into::<&str>::into(value).to_string(),
                    tokens::Keyword::Nil => Into::<&str>::into(value).to_string(),
                    _ => panic!("Literal::Expr only considers true, false, and nil as literals!"),
                },
                _ => panic!("Literal::Expr can only contains Literal token!"),
            },

            Expr::Unary { operator, right } => match &operator.value {
                TokenValue::Operator(operator) => {
                    let op: &str = operator.into();
                    format!("({op} {right})")
                }
                _ => panic!(
                    "Expr::Unary only accept ! or - operator (got {} instead)",
                    operator
                ),
            },

            Expr::Binary {
                left,
                operator,
                right,
            } => match &operator.value {
                TokenValue::Operator(operator) => {
                    let op: &str = operator.into();
                    format!("({op} {left} {right})")
                }
                _ => panic!("Expr::Binary can't accept ! or - opeartor"),
            },

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
