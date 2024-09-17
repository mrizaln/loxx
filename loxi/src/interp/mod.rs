use thiserror::Error;

use crate::parse::{expr::Expr, token};
use crate::util::Location;

use self::object::Value;

pub mod object;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{0} RuntimeError: Invalid binary operation '{1}' between '{2}' and '{3}'")]
    InvalidBinaryOp(Location, token::BinaryOp, &'static str, &'static str),

    #[error("{0} RuntimeError: Invalid unary operation '{1}' on '{2}'")]
    InvalidUnaryOp(Location, token::UnaryOp, &'static str),
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
        }
    }
}
