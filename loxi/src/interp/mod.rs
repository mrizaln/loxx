use std::ops::Deref;

use thiserror::Error;

use crate::parse::expr::{Expr, RefExpr, ValExpr};
use crate::parse::{stmt::Stmt, stmt::Unwind, token, Program};
use crate::util::{Location, TokLoc};

use self::env::Env;
use self::function::Native;
use self::interner::Interner;
use self::value::Value;

pub mod env;
pub mod function;
pub mod interner;
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
    env: Env,
    interner: Interner,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            env: Env::new_with_global(),
            interner: Interner::new(),
        };
        interp.populate_env();
        interp
    }

    pub fn interner(&mut self) -> &mut Interner {
        &mut self.interner
    }

    pub fn interpret(&self, program: Program) -> Result<(), RuntimeError> {
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
        let name = self.interner.get_or_intern("clock");
        let clock = Native::new(name, Box::new([]), native_functions::clock);
        self.env.define(name, Value::native_function(clock));
    }

    fn execute(&self, stmt: &Stmt) -> Result<Unwind, RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => {
                self.eval(expr)?;
                Ok(Unwind::None)
            }
            Stmt::Print { expr, .. } => {
                let value = self.eval(expr)?;
                println!("{}", value.display(&self.interner));
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
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };

                // TODO: add location metadata
                self.env.define(*name, value);
                Ok(Unwind::None)
            }
            Stmt::Block { statements } => {
                let _local = self.env.create_scope();
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
            } => match self.eval(condition)?.truthiness() {
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
                while self.eval(condition)?.truthiness() {
                    if let Unwind::Return(value, loc) = self.execute(body)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            // should I really clone here?
            Stmt::Function { func } => {
                self.env.define(func.name, Value::function(*func.clone()));
                Ok(Unwind::None)
            }
            Stmt::Return { value, loc } => {
                let value = match value {
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };
                Ok(Unwind::Return(value, *loc))
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::ValExpr(expr, _) => self.eval_val(expr),
            Expr::RefExpr(expr, _) => self.eval_ref(expr),
        }
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
            ValExpr::Grouping { expr, .. } => self.eval_val(expr),
            ValExpr::Unary { operator, right } => {
                let value = self.eval(right)?;
                match operator.tok {
                    token::UnaryOp::Minus => value.minus(),
                    token::UnaryOp::Not => Ok(value.not()),
                }
                .map_err(|err| match err {
                    value::InvalidOp::Unary(s) => {
                        RuntimeError::InvalidUnaryOp(operator.loc, operator.tok.clone(), s)
                    }
                    _ => unreachable!(),
                })
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let lhs = self.eval(left)?;
                let rhs = self.eval(right)?;

                match operator.tok {
                    token::BinaryOp::Add => lhs.add(rhs, &self.interner),
                    token::BinaryOp::Sub => lhs.sub(rhs),
                    token::BinaryOp::Mul => lhs.mul(rhs),
                    token::BinaryOp::Div => lhs.div(rhs),
                    token::BinaryOp::Equal => Ok(lhs.eq(&rhs, &self.interner)),
                    token::BinaryOp::NotEqual => Ok(lhs.neq(&rhs, &self.interner)),
                    token::BinaryOp::Less => lhs.lt(&rhs),
                    token::BinaryOp::LessEq => lhs.le(&rhs),
                    token::BinaryOp::Greater => lhs.gt(&rhs),
                    token::BinaryOp::GreaterEq => lhs.ge(&rhs),
                }
                .map_err(|err| match err {
                    value::InvalidOp::Binary(l, r) => {
                        RuntimeError::InvalidBinaryOp(operator.loc, operator.tok.clone(), l, r)
                    }
                    _ => unreachable!(),
                })
            }
            ValExpr::Logical { left, kind, right } => {
                let lhs = self.eval(left)?;
                match (&kind.tok, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => return Ok(lhs),
                    (token::LogicalOp::Or, true) => return Ok(lhs),
                    (_, _) => (),
                };
                self.eval(right)
            }
            ValExpr::Call { callee, loc, args } => {
                let callee = self.eval(callee)?;

                match callee {
                    Value::Function(func) => {
                        let args = args
                            .into_iter()
                            .map(|a| self.eval(a))
                            .collect::<Result<Box<[_]>, _>>()?;

                        match func.deref() {
                            function::Function::Native(func) => func.call(args, &self.env),
                            function::Function::UserDefined(func) => {
                                func.call(args, &self.env, |stmt| self.execute(stmt))
                            }
                        }
                    }
                    _ => Err(RuntimeError::NotCallable(*loc)),
                }
            }
        }
    }

    fn eval_ref(&self, expr: &RefExpr) -> Result<Value, RuntimeError> {
        match expr {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => self.env.get(tok.name).ok_or_else(|| {
                let var_name = self.interner.resolve(tok.name);
                RuntimeError::UndefinedVariable(*loc, var_name.to_string())
            }),
            RefExpr::Grouping { expr, .. } => self.eval_ref(expr),
            RefExpr::Assignment { var, value } => {
                let value = self.eval(value)?;
                self.env.modify(var.tok.name, |v| *v = value);
                Ok(self.env.get(var.tok.name).unwrap())
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
