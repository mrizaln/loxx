use std::fmt::Debug;
use std::rc::Rc;

use thiserror::Error;

use crate::lex::token::Keyword;
use crate::parse::stmt::{Stmt, Unwind};
use crate::util::Location;

use super::class::Instance;
use super::env::DynamicEnv;
use super::interner::{Interner, Key};
use super::RuntimeError;
use super::{env::Env, value::Value};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    Function,
    Constructor,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Function {
    Native(Native),
    UserDefined(UserDefined),
}

type NativeFn = fn(args: Box<[Value]>) -> Result<Value, RuntimeError>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Native {
    pub name: Key,
    pub params: Box<[Key]>,
    pub body: NativeFn,
}

#[derive(Clone, Debug)]
pub struct UserDefined {
    pub name: Key,
    pub params: Box<[Key]>,
    pub body: Box<[Stmt]>,
    pub loc: Location,
    pub capture: Rc<Env>,
    pub kind: Kind,
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

impl Function {
    /// Access the inner `UserDefined` function. Panic if self is not a `UserDefined` function.
    /// Useful to get back `UserDefined` fuction from `Function` for `UserDefined` function that
    /// needs to be a `Function` because of design constraints.
    pub fn as_user_defined(&self) -> &UserDefined {
        match self {
            Function::Native(_) => panic!("self is not a UserDefined function"),
            Function::UserDefined(func) => func,
        }
    }
}

impl Native {
    pub fn new(name: Key, params: Box<[Key]>, body: NativeFn) -> Self {
        Self { name, params, body }
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn call(&self, args: Box<[Value]>) -> Result<Value, RuntimeError> {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: Location::new(0, 0),
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        (self.body)(args)
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
    pub fn new(
        name: Key,
        params: Box<[Key]>,
        body: Box<[Stmt]>,
        loc: Location,
        capture: Rc<Env>,
        kind: Kind,
    ) -> Self {
        Self {
            name,
            params,
            body,
            loc,
            capture,
            kind,
        }
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }

    pub fn call<F>(
        &self,
        args: Box<[Value]>,
        interner: &Interner,
        env: &DynamicEnv,
        mut exec: F,
    ) -> Result<Value, RuntimeError>
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

        let _guard = env.bind_scope(Rc::new(Env::new_with_parent(Rc::clone(&self.capture))));

        // https://github.com/rust-lang/rust/issues/59878
        for (i, arg) in args.into_vec().into_iter().enumerate() {
            env.define(self.params[i], arg);
        }

        for stmt in self.body.iter() {
            if let Unwind::Return(value, _) = exec(stmt)? {
                match self.kind {
                    Kind::Function => return Ok(value),
                    Kind::Constructor => match value {
                        Value::Nil => break,
                        _ => unreachable!("constructor should not return a value from it"),
                    },
                }
            }
        }

        match self.kind {
            Kind::Function => Ok(Value::nil()),
            Kind::Constructor => {
                let this = interner.keyword(Keyword::This);
                Ok(env
                    .get_at(this, 0)
                    .expect("this must exist if function is an initializer/constructor"))
            }
        }
    }

    // NOTE: this function creates a copy of self that binds an instance into its new capture
    pub fn bind(&self, instance: Rc<Instance>, interner: &Interner) -> UserDefined {
        let new_capture = Env::new_with_parent(Rc::clone(&self.capture));
        let this = interner.keyword(Keyword::This);
        new_capture.define(this, Value::Instance(instance));
        UserDefined::new(
            self.name,
            self.params.clone(),
            self.body.clone(),
            self.loc,
            new_capture.into(),
            self.kind,
        )
    }
}

impl PartialEq for UserDefined {
    fn eq(&self, other: &Self) -> bool {
        (self.name, self.arity(), self.loc) == (other.name, other.arity(), other.loc)
            && Rc::ptr_eq(&self.capture, &other.capture)
    }
}

impl PartialOrd for UserDefined {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let lhs = (self.name, self.arity(), self.loc);
        let rhs = (other.name, other.arity(), other.loc);

        if lhs != rhs {
            lhs.partial_cmp(&rhs)
        } else {
            let lhs = Rc::as_ptr(&self.capture) as usize;
            let rhs = Rc::as_ptr(&other.capture) as usize;
            lhs.partial_cmp(&rhs)
        }
    }
}
