use std::fmt::{Debug, Display};

use super::token;
use crate::interp::object::Value;
use crate::interp::RuntimeError;
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
    Variable {
        value: TokLoc<token::Variable>,
    },
}

impl Expr {
    pub fn eval(self) -> Result<Value, RuntimeError> {
        match self {
            Expr::Literal { value } => match value.tok {
                token::Literal::Number(num) => Ok(Value::Number(num)),
                token::Literal::String(str) => Ok(Value::String(str)),
                token::Literal::True => Ok(Value::Bool(true)),
                token::Literal::False => Ok(Value::Bool(false)),
                token::Literal::Nil => Ok(Value::Nil),
            },
            Expr::Grouping { expr } => expr.eval(),
            Expr::Unary { operator, right } => {
                let eval = right.eval()?;
                match operator.tok {
                    token::UnaryOp::Minus => eval.minus(),
                    token::UnaryOp::Not => eval.not(),
                }
                .ok_or_else(|| {
                    RuntimeError::InvalidUnaryOp(operator.loc, operator.tok, eval.name())
                })
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.eval()?;
                let right = right.eval()?;

                match operator.tok {
                    token::BinaryOp::Add => left.add(&right),
                    token::BinaryOp::Sub => left.sub(&right),
                    token::BinaryOp::Mul => left.mul(&right),
                    token::BinaryOp::Div => left.div(&right),
                    token::BinaryOp::Equal => left.eq(&right),
                    token::BinaryOp::NotEqual => left.neq(&right),
                    token::BinaryOp::Less => left.lt(&right),
                    token::BinaryOp::LessEq => left.le(&right),
                    token::BinaryOp::Greater => left.gt(&right),
                    token::BinaryOp::GreaterEq => left.ge(&right),
                }
                .ok_or_else(|| {
                    RuntimeError::InvalidBinaryOp(
                        operator.loc,
                        operator.tok,
                        left.name(),
                        right.name(),
                    )
                })
            }
            Expr::Variable { .. } => unimplemented!(),
        }
    }
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
            Expr::Variable { .. } => unimplemented!(),
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
            Expr::Variable { .. } => unimplemented!(),
        };
        write!(f, "{}", string)
    }
}
