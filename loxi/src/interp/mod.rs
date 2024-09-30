use lasso::Rodeo;
use thiserror::Error;

use crate::parse::{token, Program};
use crate::util::Location;

use self::env::Env;
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
}

impl RuntimeError {
    pub fn loc(&self) -> Location {
        match self {
            RuntimeError::InvalidBinaryOp(loc, _, _, _) => *loc,
            RuntimeError::InvalidUnaryOp(loc, _, _) => *loc,
            RuntimeError::UndefinedVariable(loc, _) => *loc,
            RuntimeError::FunctionError(err) => err.loc(),
            RuntimeError::NotCallable(loc) => *loc,
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
        let env = &mut self.environment;
        for stmt in program.statements.into_iter() {
            stmt.execute(env, &self.str_arena)?
        }
        Ok(())
    }

    fn populate_env(&mut self) {
        let name = self.str_arena.get_or_intern("clock");
        let clock = NativeFunction::new(name, Box::new([]), native_functions::clock);
        self.environment.define(name, Value::native_function(clock));
    }
}

mod native_functions {
    use super::*;

    pub fn clock(
        _args: Box<[Value]>,
        _env: &mut Env,
        _arena: &Rodeo,
    ) -> Result<Value, RuntimeError> {
        let now = std::time::SystemTime::now();
        let seconds = now
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64();
        Ok(Value::number(seconds))
    }
}
