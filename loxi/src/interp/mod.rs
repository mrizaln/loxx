use crate::parse::expr::Expr;

use self::object::{Value, ValueError};

pub mod object;

impl Expr {
    pub fn eval(self) -> Result<Value, ValueError> {
        match self {
            Expr::Literal { value } => match value.tok {
                crate::parse::token::Literal::Number(num) => Ok(Value::Number(num)),
                crate::parse::token::Literal::String(str) => Ok(Value::String(str)),
                crate::parse::token::Literal::True => Ok(Value::Bool(true)),
                crate::parse::token::Literal::False => Ok(Value::Bool(false)),
                crate::parse::token::Literal::Nil => Ok(Value::Nil),
            },
            Expr::Grouping { expr } => expr.eval(),
            Expr::Unary { operator, right } => {
                let eval = right.eval()?;
                match operator.tok {
                    crate::parse::token::UnaryOp::Minus => eval.minus(),
                    crate::parse::token::UnaryOp::Not => eval.not(),
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.eval()?;
                let right = right.eval()?;

                match operator.tok {
                    crate::parse::token::BinaryOp::Add => left.add(&right),
                    crate::parse::token::BinaryOp::Sub => left.sub(&right),
                    crate::parse::token::BinaryOp::Mul => left.mul(&right),
                    crate::parse::token::BinaryOp::Div => left.div(&right),
                    crate::parse::token::BinaryOp::Equal => left.eq(&right),
                    crate::parse::token::BinaryOp::NotEqual => left.neq(&right),
                    crate::parse::token::BinaryOp::Less => left.lt(&right),
                    crate::parse::token::BinaryOp::LessEq => left.le(&right),
                    crate::parse::token::BinaryOp::Greater => left.gt(&right),
                    crate::parse::token::BinaryOp::GreaterEq => left.ge(&right),
                }
            }
        }
    }
}
