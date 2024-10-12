use std::fmt::Debug;
use std::rc::Rc;

use thiserror::Error;

use crate::parse::stmt::{StmtFunction, Unwind};
use crate::util::Location;

use super::class::Instance;
use super::interner::{Interner, Key};
use super::{env::Env, value::Value};
use super::{Interpreter, RuntimeError};

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
    pub func: Rc<StmtFunction>,
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
    pub fn new(func: Rc<StmtFunction>, capture: Rc<Env>, kind: Kind) -> Self {
        Self {
            func,
            capture,
            kind,
        }
    }

    pub fn arity(&self) -> usize {
        self.func.params.len()
    }

    pub fn call(
        &self,
        args: Box<[Value]>,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: self.func.loc,
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        let (env, interner) = (&interpreter.dyn_env, &interpreter.interner);
        let _guard = env.bind_scope(Rc::new(Env::with_parent(Rc::clone(&self.capture))));

        // https://github.com/rust-lang/rust/issues/59878
        for (i, arg) in args.into_vec().into_iter().enumerate() {
            env.define(self.func.params[i], arg);
        }

        for stmt in self.func.body.iter() {
            if let Unwind::Return(value, _) = interpreter.execute(stmt)? {
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
            Kind::Constructor => Ok(env
                .get_at(interner.key_this, 0)
                .expect("this must exist if function is an initializer/constructor")),
        }
    }

    // NOTE: this function creates a copy of self that binds an instance into its new capture
    pub fn bind(&self, instance: Rc<Instance>, interner: &Interner) -> UserDefined {
        let new_capture = Env::with_parent(Rc::clone(&self.capture));
        new_capture.define(interner.key_this, Value::Instance(instance));
        UserDefined::new(Rc::clone(&self.func), new_capture.into(), self.kind)
    }
}

impl PartialEq for UserDefined {
    fn eq(&self, other: &Self) -> bool {
        self.func == other.func && Rc::ptr_eq(&self.capture, &other.capture)
    }
}

impl PartialOrd for UserDefined {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let lhs = &self.func;
        let rhs = &other.func;

        if lhs != rhs {
            lhs.partial_cmp(&rhs)
        } else {
            let lhs = Rc::as_ptr(&self.capture) as usize;
            let rhs = Rc::as_ptr(&other.capture) as usize;
            lhs.partial_cmp(&rhs)
        }
    }
}
