use std::cell::RefMut;

use lasso::Rodeo;
use thiserror::Error;

use crate::parse::expr::{Expr, RefExpr, ValExpr};
use crate::parse::{stmt::Stmt, stmt::Unwind, token, Program};
use crate::util::{Location, TokLoc};

use self::env::Env;
use self::function::Callable;
use self::function::NativeFunction;
use self::value::Value;

pub mod env;
pub mod function;
pub mod object;
pub mod value;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{0} RuntimeError: Invalid binary operation '{1}' between '{2}' and '{3}'")]
    InvalidBinaryOp(Location, token::BinaryOp, &'static str, &'static str),

    #[error("{0} RuntimeError: Invalid unary operation '{1}' on '{2}'")]
    InvalidUnaryOp(Location, token::UnaryOp, &'static str),

    #[error("{0} RuntimeError: Trying to access undefined variable: '{1}'")]
    UndefinedVariable(Location, String),

    #[error("{0}")]
    FunctionError(#[from] function::FunctionError),

    #[error("{0} RuntimeError: Not a function or a callable object")]
    NotCallable(Location),

    // NOTE: maybe pushing this error to parsing stage is better
    #[error("{0} RuntimeError: Stray return statement")]
    StrayReturn(Location),
}

impl RuntimeError {
    pub fn loc(&self) -> Location {
        match self {
            RuntimeError::InvalidBinaryOp(loc, _, _, _) => *loc,
            RuntimeError::InvalidUnaryOp(loc, _, _) => *loc,
            RuntimeError::UndefinedVariable(loc, _) => *loc,
            RuntimeError::FunctionError(err) => err.loc(),
            RuntimeError::NotCallable(loc) => *loc,
            RuntimeError::StrayReturn(loc) => *loc,
        }
    }
}

pub struct Interpreter {
    environment: Env,
    str_arena: Rodeo,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            environment: Env::new(),
            str_arena: Rodeo::new(),
        };
        interp.populate_env();
        interp
    }

    pub fn arena(&mut self) -> &mut Rodeo {
        &mut self.str_arena
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), RuntimeError> {
        for stmt in program.statements.iter() {
            match self.execute(stmt)? {
                Unwind::None => (),
                Unwind::Return(_, loc) => {
                    Err(RuntimeError::StrayReturn(loc))?;
                }
            }
        }
        Ok(())
    }

    fn populate_env(&mut self) {
        let name = self.str_arena.get_or_intern("clock");
        let clock = NativeFunction::new(name, Box::new([]), native_functions::clock);
        self.environment.define(name, Value::native_function(clock));
    }

    fn execute(&self, stmt: &Stmt) -> Result<Unwind, RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => {
                self.eval_unit(expr)?;
                Ok(Unwind::None)
            }
            Stmt::Print { expr, .. } => {
                self.eval_fn(expr, |v| println!("{}", v.display(&self.str_arena)))?;
                Ok(Unwind::None)
            }
            Stmt::Var { name, init, .. } => {
                //
                // there are two ways to implement this if the init is a RefExpr:
                // - clone the init         -> the variable then becomes separate entity
                // - reference the init     -> the variable then becomes an alias
                // at this point, I don't know how Lox handle this thing, maybe in future chapters.
                // if it was the latter, then I'll be damned, I have to implement the garbage
                // collector very early on... or at least a system that can track the entities and
                // their references. for now, I'll just clone the init.
                //
                //      -- 2024/09/19 02:57 [chapter 8.2: global variables]

                let value = match init {
                    Some(expr) => self.eval_cloned(expr)?,
                    None => Value::nil(),
                };

                // TODO: add location metadata
                self.environment.define(name.clone(), value);
                Ok(Unwind::None)
            }
            Stmt::Block { statements } => {
                let _local = self.environment.create_scope();
                for stmt in statements {
                    if let Unwind::Return(value, loc) = self.execute(stmt)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => match self.eval_fn(condition, |v| v.truthiness())? {
                true => {
                    if let Unwind::Return(value, loc) = self.execute(then)? {
                        Ok(Unwind::Return(value, loc))
                    } else {
                        Ok(Unwind::None)
                    }
                }
                false => {
                    if let Some(stmt) = otherwise {
                        Ok(self.execute(stmt)?)
                    } else {
                        Ok(Unwind::None)
                    }
                }
            },
            Stmt::While {
                condition, body, ..
            } => {
                while self.eval_fn(condition, |v| v.truthiness())? {
                    if let Unwind::Return(value, loc) = self.execute(body)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            // should I really clone here?
            Stmt::Function { func } => {
                self.environment
                    .define(func.name, Value::function(*func.clone()));
                Ok(Unwind::None)
            }
            Stmt::Return { value, loc } => {
                let value = match value {
                    Some(expr) => self.eval_cloned(expr)?,
                    None => Value::nil(),
                };
                Ok(Unwind::Return(value, *loc))
            }
        }
    }

    /// Evaluate `Expr` as if it produces a `&mut Value`. The reference can only be used in `f`.
    pub fn eval_fn<R, F>(&self, expr: &Expr, f: F) -> Result<R, RuntimeError>
    where
        F: FnOnce(&mut Value) -> R,
    {
        let val = match expr {
            Expr::ValExpr(expr) => &mut self.eval_val(expr)?,
            Expr::RefExpr(expr) => &mut self.eval_ref(expr)?,
        };
        Ok(f(val))
    }

    /// Evaluate the expression and return a `Value`. If the `Expr` is a `RefExpr`, the contained
    /// `Value` will be cloned, if it's a ValExpr the value will be returned as is.
    pub fn eval_cloned(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::ValExpr(expr) => self.eval_val(expr),
            Expr::RefExpr(expr) => self.eval_ref(expr).map(|v| v.clone()),
        }
    }

    /// Evaluate the expression without returning a value.
    pub fn eval_unit(&self, expr: &Expr) -> Result<(), RuntimeError> {
        match expr {
            Expr::ValExpr(expr) => self.eval_val(expr).map(|_| {})?,
            Expr::RefExpr(expr) => self.eval_ref(expr).map(|_| {})?,
        };
        Ok(())
    }

    fn eval_val(&self, expr: &ValExpr) -> Result<Value, RuntimeError> {
        match expr {
            ValExpr::Literal { value } => match &value.tok {
                token::Literal::Number(num) => Ok(Value::number(*num)),
                token::Literal::String(str) => Ok(Value::string_literal(*str)),
                token::Literal::True => Ok(Value::bool(true)),
                token::Literal::False => Ok(Value::bool(false)),
                token::Literal::Nil => Ok(Value::nil()),
            },
            ValExpr::Grouping { expr } => self.eval_val(expr),
            ValExpr::Unary { operator, right } => {
                let value = self.eval_cloned(right)?;
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
                let lhs = self.eval_cloned(left)?;
                let rhs = self.eval_cloned(right)?;

                match operator.tok {
                    token::BinaryOp::Add => lhs.add(rhs, &self.str_arena),
                    token::BinaryOp::Sub => lhs.sub(rhs),
                    token::BinaryOp::Mul => lhs.mul(rhs),
                    token::BinaryOp::Div => lhs.div(rhs),
                    token::BinaryOp::Equal => lhs.eq(&rhs, &self.str_arena),
                    token::BinaryOp::NotEqual => lhs.neq(&rhs, &self.str_arena),
                    token::BinaryOp::Less => lhs.lt(&rhs),
                    token::BinaryOp::LessEq => lhs.le(&rhs),
                    token::BinaryOp::Greater => lhs.gt(&rhs),
                    token::BinaryOp::GreaterEq => lhs.ge(&rhs),
                }
                .ok_or(RuntimeError::InvalidBinaryOp(
                    operator.loc,
                    operator.tok.clone(),
                    "adfj",
                    "adfh",
                ))
            }
            ValExpr::Logical { left, kind, right } => {
                let lhs = self.eval_cloned(left)?;
                match (&kind.tok, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => return Ok(lhs),
                    (token::LogicalOp::Or, true) => return Ok(lhs),
                    (_, _) => (),
                };
                self.eval_cloned(right)
            }
            ValExpr::Call { callee, loc, args } => {
                let callee = self.eval_cloned(callee)?;
                let args = args
                    .into_iter()
                    .map(|a| self.eval_cloned(a))
                    .collect::<Result<Box<[_]>, _>>()?;

                match callee {
                    Value::Function(expr) => {
                        let _local = self.environment.create_scope();
                        let result =
                            expr.call(args, &self.environment, |stmt| self.execute(stmt))?;
                        Ok(result)
                    }
                    Value::NativeFunction(expr) => {
                        let _local = self.environment.create_scope();
                        let result = expr.call(args, &self.environment, |_| unreachable!())?;
                        Ok(result)
                    }
                    _ => Err(RuntimeError::NotCallable(*loc)),
                }
            }
        }
    }

    fn eval_ref(&self, expr: &RefExpr) -> Result<RefMut<'_, Value>, RuntimeError> {
        match expr {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => self.environment.get(tok.name).ok_or_else(|| {
                let var_name = self.str_arena.resolve(&tok.name);
                RuntimeError::UndefinedVariable(*loc, var_name.to_string())
            }),
            RefExpr::Grouping { expr } => self.eval_ref(expr),
            RefExpr::Assignment { var, value } => {
                let value = self.eval_cloned(value)?;
                self.environment
                    .get(var.tok.name)
                    .ok_or_else(|| {
                        let var_name = self.str_arena.resolve(&var.tok.name);
                        RuntimeError::UndefinedVariable(var.loc, var_name.to_string())
                    })
                    .map(|mut v| {
                        *v = value;
                        v
                    })
            }
        }
    }
}

mod native_functions {
    use super::*;

    pub fn clock(_args: Box<[Value]>, _env: &Env) -> Result<Value, RuntimeError> {
        let now = std::time::SystemTime::now();
        let seconds = now
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64();
        Ok(Value::number(seconds))
    }
}
