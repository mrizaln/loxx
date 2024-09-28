use std::cell::RefMut;
use std::fmt::{Debug, Display};

use super::token;
use crate::interp::env::Env;
use crate::interp::value::Value;
use crate::interp::RuntimeError;
use crate::util::TokLoc;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Expr {
    ValExpr(ValExpr),
    RefExpr(RefExpr),
}

/// Expression that produces `Value`
#[derive(Clone, PartialEq, PartialOrd)]
pub enum ValExpr {
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
        expr: Box<ValExpr>,
    },
    Logical {
        left: Box<Expr>,
        kind: TokLoc<token::LogicalOp>,
        right: Box<Expr>,
    },
}

/// Expression that produces a reference to `Value`
#[derive(Clone, PartialEq, PartialOrd)]
pub enum RefExpr {
    Variable {
        var: TokLoc<token::Variable>,
    },
    Grouping {
        expr: Box<RefExpr>,
    },
    Assignment {
        var: TokLoc<token::Variable>,
        value: Box<Expr>,
    },
}

impl Expr {
    /// Evaluate `Expr` as if it produces a `&mut Value`. The reference can only be used in `f`.
    pub fn eval_fn<R, F>(&self, env: &mut Env, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&mut Value) -> R,
    {
        let val = match self {
            Expr::ValExpr(expr) => &mut expr.eval(env)?,
            Expr::RefExpr(expr) => &mut expr.eval(env)?,
        };
        Ok(f(val))
    }

    /// Evaluate the expression and return a `Value`. If the `Expr` is a `RefExpr`, the contained
    /// `Value` will be cloned, if it's a ValExpr the value will be returned as is.
    pub fn eval_cloned(&self, env: &mut Env) -> Result<Value, RuntimeError> {
        match self {
            Expr::ValExpr(expr) => expr.eval(env),
            Expr::RefExpr(expr) => expr.eval(env).map(|v| v.clone()),
        }
    }

    /// Evaluate the expression without returning a value.
    pub fn eval_unit(&self, env: &mut Env) -> Result<(), RuntimeError> {
        match self {
            Expr::ValExpr(expr) => expr.eval(env).map(|_| ()),
            Expr::RefExpr(expr) => expr.eval(env).map(|_| ()),
        }
    }
}

impl ValExpr {
    pub fn eval(&self, env: &mut Env) -> Result<Value, RuntimeError> {
        match self {
            ValExpr::Literal { value } => match &value.tok {
                token::Literal::Number(num) => Ok(Value::Number(*num)),
                token::Literal::String(str) => Ok(Value::String(str.clone())),
                token::Literal::True => Ok(Value::Bool(true)),
                token::Literal::False => Ok(Value::Bool(false)),
                token::Literal::Nil => Ok(Value::Nil),
            },
            ValExpr::Grouping { expr } => expr.eval(env),
            ValExpr::Unary { operator, right } => {
                let value = right.eval_cloned(env)?;
                match operator.tok {
                    token::UnaryOp::Minus => value.minus(),
                    token::UnaryOp::Not => value.not(),
                }
                .ok_or(RuntimeError::InvalidUnaryOp(
                    operator.loc,
                    operator.tok.clone(),
                    value.name(),
                ))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let lhs = left.eval_cloned(env)?;
                let rhs = right.eval_cloned(env)?;

                let lname = lhs.name();
                let rname = rhs.name();

                match operator.tok {
                    token::BinaryOp::Add => lhs.add(rhs),
                    token::BinaryOp::Sub => lhs.sub(rhs),
                    token::BinaryOp::Mul => lhs.mul(rhs),
                    token::BinaryOp::Div => lhs.div(rhs),
                    token::BinaryOp::Equal => lhs.eq(&rhs),
                    token::BinaryOp::NotEqual => lhs.neq(&rhs),
                    token::BinaryOp::Less => lhs.lt(&rhs),
                    token::BinaryOp::LessEq => lhs.le(&rhs),
                    token::BinaryOp::Greater => lhs.gt(&rhs),
                    token::BinaryOp::GreaterEq => lhs.ge(&rhs),
                }
                .ok_or(RuntimeError::InvalidBinaryOp(
                    operator.loc,
                    operator.tok.clone(),
                    lname,
                    rname,
                ))
            }
            ValExpr::Logical { left, kind, right } => {
                let lhs = left.eval_cloned(env)?;

                match (&kind.tok, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => return Ok(lhs),
                    (token::LogicalOp::Or, true) => return Ok(lhs),
                    (_, _) => (),
                };

                right.eval_cloned(env)
            }
        }
    }
}

impl RefExpr {
    pub fn eval<'a>(&self, env: &'a mut Env) -> Result<RefMut<'a, Value>, RuntimeError> {
        match self {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => env
                .get(&tok.name)
                .ok_or(RuntimeError::UndefinedVariable(*loc, tok.name.clone())),
            RefExpr::Grouping { expr } => expr.eval(env),
            RefExpr::Assignment { var, value } => {
                let value = value.eval_cloned(env)?;
                env.get(&var.tok.name)
                    .ok_or(RuntimeError::UndefinedVariable(
                        var.loc,
                        var.tok.name.clone(),
                    ))
                    .map(|mut v| {
                        *v = value;
                        v
                    })
            }
        }
    }
}

impl Display for ValExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            ValExpr::Literal { value } => (&value.tok).into(),
            ValExpr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                format!("({op} {right})")
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                format!("({op} {left} {right})")
            }

            ValExpr::Grouping { expr } => {
                format!("(group {expr})")
            }
            ValExpr::Logical { left, kind, right } => {
                let op: &str = (&kind.tok).into();
                format!("({op} {left} {right})")
            }
        };
        write!(f, "{}", string)
    }
}

impl Debug for ValExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string: String = match &self {
            ValExpr::Literal { value } => {
                let str: String = (&value.tok).into();
                format!("{}{}", str, value.loc)
            }
            ValExpr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                format!("({op}{} {right:?})", operator.loc)
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                format!("({op}{} {left:?} {right:?})", operator.loc)
            }

            ValExpr::Grouping { expr } => {
                format!("(group {expr:?})")
            }
            ValExpr::Logical { left, kind, right } => {
                let op: &str = (&kind.tok).into();
                format!("({op}{} {left:?} {right:?})", kind.loc)
            }
        };
        write!(f, "{}", string)
    }
}

impl Display for RefExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefExpr::Variable {
                var: TokLoc { tok, .. },
            } => write!(f, "(var {})", tok.name),
            RefExpr::Grouping { expr } => Display::fmt(&expr, f),
            RefExpr::Assignment { var, value } => write!(f, "(= {} {})", var.tok.name, value),
        }
    }
}

impl Debug for RefExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => write!(f, "(var{} {})", loc, tok.name),
            RefExpr::Grouping { expr } => Debug::fmt(&expr, f),
            RefExpr::Assignment { var, value } => {
                write!(f, "(={} {} {:?})", var.loc, var.tok.name, value)
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::ValExpr(expr) => Display::fmt(&expr, f),
            Expr::RefExpr(expr) => Display::fmt(&expr, f),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::ValExpr(expr) => Debug::fmt(&expr, f),
            Expr::RefExpr(expr) => Debug::fmt(&expr, f),
        }
    }
}

pub mod macros {
    macro_rules! val_expr {
        ($kind:tt $xpr:tt) => {
            Expr::ValExpr(ValExpr::$kind $xpr)
        };
    }

    macro_rules! ref_expr {
        ($kind:tt $xpr:tt) => {
            Expr::RefExpr(RefExpr::$kind $xpr)
        };
    }

    macro_rules! group_expr {
        ($xpr:expr) => {
            match $xpr {
                Expr::ValExpr(expr) => val_expr!(Grouping {
                    expr: Box::new(expr)
                }),
                Expr::RefExpr(expr) => ref_expr!(Grouping {
                    expr: Box::new(expr)
                }),
            }
        };
    }

    pub(crate) use group_expr;
    pub(crate) use ref_expr;
    pub(crate) use val_expr;
}
