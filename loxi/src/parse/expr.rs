use std::cell::RefMut;
use std::fmt::Display;

use lasso::Rodeo;

use super::token;
use crate::interp::env::Env;
use crate::interp::function::Callable;
use crate::interp::value::Value;
use crate::interp::RuntimeError;
use crate::util::{Location, TokLoc};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    ValExpr(ValExpr),
    RefExpr(RefExpr),
}

/// Expression that produces `Value`
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Call {
        callee: Box<Expr>,
        args: Box<[Expr]>,
        loc: Location,
    },
}

/// Expression that produces a reference to `Value`
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

pub struct DisplayedExpr<'a, 'b> {
    expr: &'a Expr,
    arena: &'b Rodeo,
}

pub struct DisplayedValExpr<'a, 'b> {
    expr: &'a ValExpr,
    arena: &'b Rodeo,
}

pub struct DisplayedRefExpr<'a, 'b> {
    expr: &'a RefExpr,
    arena: &'b Rodeo,
}

impl Expr {
    /// Lox has maximum number of arguments for its functions, because the original implementation
    /// is in Java and it's limited there to 255 arguments only.
    pub const MAX_FUNC_ARGS: usize = 255;

    /// Evaluate `Expr` as if it produces a `&mut Value`. The reference can only be used in `f`.
    pub fn eval_fn<R, F>(&self, env: &mut Env, arena: &Rodeo, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&mut Value) -> R,
    {
        let val = match self {
            Expr::ValExpr(expr) => &mut expr.eval(env, arena)?,
            Expr::RefExpr(expr) => &mut expr.eval(env, arena)?,
        };
        Ok(f(val))
    }

    /// Evaluate the expression and return a `Value`. If the `Expr` is a `RefExpr`, the contained
    /// `Value` will be cloned, if it's a ValExpr the value will be returned as is.
    pub fn eval_cloned(&self, env: &mut Env, arena: &Rodeo) -> Result<Value, RuntimeError> {
        match self {
            Expr::ValExpr(expr) => expr.eval(env, arena),
            Expr::RefExpr(expr) => expr.eval(env, arena).map(|v| v.clone()),
        }
    }

    /// Evaluate the expression without returning a value.
    pub fn eval_unit(&self, env: &mut Env, arena: &Rodeo) -> Result<(), RuntimeError> {
        match self {
            Expr::ValExpr(expr) => {
                expr.eval(env, arena)?;
            }
            Expr::RefExpr(expr) => {
                expr.eval(env, arena)?;
            }
        }
        Ok(())
    }

    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedExpr<'a, 'b> {
        DisplayedExpr { expr: self, arena }
    }
}

impl ValExpr {
    pub fn eval(&self, env: &mut Env, arena: &Rodeo) -> Result<Value, RuntimeError> {
        match self {
            ValExpr::Literal { value } => match &value.tok {
                token::Literal::Number(num) => Ok(Value::number(*num)),
                token::Literal::String(str) => Ok(Value::string_literal(*str)),
                token::Literal::True => Ok(Value::bool(true)),
                token::Literal::False => Ok(Value::bool(false)),
                token::Literal::Nil => Ok(Value::nil()),
            },
            ValExpr::Grouping { expr } => expr.eval(env, arena),
            ValExpr::Unary { operator, right } => {
                let value = right.eval_cloned(env, arena)?;
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
                let lhs = left.eval_cloned(env, arena)?;
                let rhs = right.eval_cloned(env, arena)?;

                let lname = lhs.name();
                let rname = rhs.name();

                match operator.tok {
                    token::BinaryOp::Add => lhs.add(rhs, arena),
                    token::BinaryOp::Sub => lhs.sub(rhs),
                    token::BinaryOp::Mul => lhs.mul(rhs),
                    token::BinaryOp::Div => lhs.div(rhs),
                    token::BinaryOp::Equal => lhs.eq(&rhs, arena),
                    token::BinaryOp::NotEqual => lhs.neq(&rhs, arena),
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
                let lhs = left.eval_cloned(env, arena)?;

                match (&kind.tok, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => return Ok(lhs),
                    (token::LogicalOp::Or, true) => return Ok(lhs),
                    (_, _) => (),
                };

                right.eval_cloned(env, arena)
            }
            ValExpr::Call { callee, loc, args } => {
                let callee = callee.eval_cloned(env, arena)?;
                let args = args
                    .into_iter()
                    .map(|a| a.eval_cloned(env, arena))
                    .collect::<Result<Box<[_]>, _>>()?;

                match callee {
                    Value::Function(expr) => {
                        let mut new_env = env.child();
                        let result = expr.call(args, &mut new_env, arena)?;
                        Ok(result)
                    }
                    Value::NativeFunction(expr) => {
                        let mut new_env = env.child();
                        let result = expr.call(args, &mut new_env, arena)?;
                        Ok(result)
                    }
                    _ => Err(RuntimeError::NotCallable(*loc)),
                }
            }
        }
    }

    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedValExpr<'a, 'b> {
        DisplayedValExpr { expr: self, arena }
    }
}

impl RefExpr {
    pub fn eval<'a>(
        &self,
        env: &'a mut Env,
        arena: &Rodeo,
    ) -> Result<RefMut<'a, Value>, RuntimeError> {
        match self {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => env.get(&tok.name).ok_or_else(|| {
                let var_name = arena.resolve(&tok.name);
                RuntimeError::UndefinedVariable(*loc, var_name.to_string())
            }),
            RefExpr::Grouping { expr } => expr.eval(env, arena),
            RefExpr::Assignment { var, value } => {
                let value = value.eval_cloned(env, arena)?;
                env.get(&var.tok.name)
                    .ok_or_else(|| {
                        let var_name = arena.resolve(&var.tok.name);
                        RuntimeError::UndefinedVariable(var.loc, var_name.to_string())
                    })
                    .map(|mut v| {
                        *v = value;
                        v
                    })
            }
        }
    }

    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedRefExpr<'a, 'b> {
        DisplayedRefExpr { expr: self, arena }
    }
}

impl Display for DisplayedExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            Expr::ValExpr(expr) => write!(f, "{}", expr.display(arena)),
            Expr::RefExpr(expr) => write!(f, "{}", expr.display(arena)),
        }
    }
}

impl Display for DisplayedValExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            ValExpr::Literal { value } => {
                write!(f, "{}", value.tok.display(arena))
            }
            ValExpr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                write!(f, "({op} {})", right.display(arena))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                write!(f, "({op} {} {})", left.display(arena), right.display(arena))
            }
            ValExpr::Grouping { expr } => {
                write!(f, "(group {})", expr.display(arena))
            }
            ValExpr::Logical { left, kind, right } => {
                let op: &str = (&kind.tok).into();
                write!(f, "({op} {} {})", left.display(arena), right.display(arena))
            }
            ValExpr::Call { callee, args, .. } => {
                write!(f, "(call {} ", callee.display(arena))?;
                write!(f, "(args")?;
                for expr in args {
                    write!(f, " {}", expr.display(arena))?;
                }
                write!(f, "))")
            }
        }
    }
}

// TODO: remove these impl for Display and Debug then replace with proper conversion from Spur

impl Display for DisplayedRefExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            RefExpr::Variable { var } => write!(f, "(var {})", arena.resolve(&var.tok.name)),
            RefExpr::Grouping { expr } => write!(f, "{}", expr.display(arena)),
            RefExpr::Assignment { var, value } => {
                let name = arena.resolve(&var.tok.name);
                write!(f, "(= {} {})", name, value.display(arena))
            }
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
