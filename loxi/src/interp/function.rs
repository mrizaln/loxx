use std::fmt::Debug;

use thiserror::Error;

use crate::parse::stmt::{Stmt, Unwind};
use crate::util::Location;

use super::interner::Key;
use super::RuntimeError;
use super::{env::Env, value::Value};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Function {
    Native(Native),
    UserDefined(UserDefined),
}

type NativeFn = fn(args: Box<[Value]>, env: &Env) -> Result<Value, RuntimeError>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Native {
    pub name: Key,
    pub params: Box<[Key]>,
    pub body: NativeFn,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct UserDefined {
    pub name: Key,
    pub params: Box<[Key]>,
    pub body: Box<[Stmt]>,
    pub loc: Location,
}

#[derive(Debug, Error)]
pub enum FunctionError {
    #[error("{loc} RuntimeError: Mismatched number of arguments. Expected {expect} argument(s) got {got} instead")]
    MismatchedArgument {
        loc: Location,
        expect: usize,
        got: usize,
    },
}

impl Native {
    pub fn new(name: Key, params: Box<[Key]>, body: NativeFn) -> Self {
        Self { name, params, body }
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn call(&self, args: Box<[Value]>, env: &Env) -> Result<Value, RuntimeError> {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: Location::new(0, 0),
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        let _local = env.create_scope();
        (self.body)(args, env)
    }
}

impl FunctionError {
    pub fn loc(&self) -> Location {
        match self {
            FunctionError::MismatchedArgument { loc, .. } => *loc,
        }
    }
}

impl UserDefined {
    pub fn new(name: Key, params: Box<[Key]>, body: Box<[Stmt]>, loc: Location) -> Self {
        Self {
            name,
            params,
            body,
            loc,
        }
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn call<F>(&self, args: Box<[Value]>, env: &Env, mut exec: F) -> Result<Value, RuntimeError>
    where
        F: FnMut(&Stmt) -> Result<Unwind, RuntimeError>,
    {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: self.loc,
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        let _local = env.create_scope();

        // https://github.com/rust-lang/rust/issues/59878
        for (i, arg) in args.into_vec().into_iter().enumerate() {
            env.define(self.params[i], arg.clone());
        }

        for stmt in self.body.iter() {
            if let Unwind::Return(value, _) = exec(stmt)? {
                return Ok(value);
            }
        }

        Ok(Value::nil())
    }
}
