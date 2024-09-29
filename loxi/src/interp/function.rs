use std::fmt::Debug;

use thiserror::Error;

use crate::parse::stmt::Stmt;
use crate::util::Location;

use super::RuntimeError;
use super::{env::Env, value::Value};

/// For anything callable in Lox
pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&mut self, args: Box<[Value]>, env: &mut Env) -> Result<Value, RuntimeError>;
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Function {
    pub name: String,
    pub params: Box<[String]>,
    pub body: Vec<Stmt>,
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

impl FunctionError {
    pub fn loc(&self) -> Location {
        match self {
            FunctionError::MismatchedArgument { loc, .. } => *loc,
        }
    }
}

impl Function {
    pub fn new(name: String, params: Box<[String]>, body: Vec<Stmt>, loc: Location) -> Self {
        Self {
            name,
            params,
            body,
            loc,
        }
    }
}

// TODO: Implement return statement
impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&mut self, args: Box<[Value]>, env: &mut Env) -> Result<Value, RuntimeError> {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: self.loc,
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        for (param, arg) in self.params.iter().zip(args.iter()) {
            env.define(param.clone(), arg.clone());
        }

        for stmt in self.body.iter() {
            stmt.execute(env)?;
        }

        Ok(Value::Nil)
    }
}

type NativeFn = fn(args: Box<[Value]>, env: &mut Env) -> Result<Value, RuntimeError>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct NativeFunction {
    pub name: String,
    pub params: Box<[String]>,
    pub body: NativeFn,
}

impl NativeFunction {
    pub fn new(name: String, params: Box<[String]>, body: NativeFn) -> Self {
        Self { name, params, body }
    }
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&mut self, args: Box<[Value]>, env: &mut Env) -> Result<Value, RuntimeError> {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: Location::new(0, 0),
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        (self.body)(args, env)
    }
}
