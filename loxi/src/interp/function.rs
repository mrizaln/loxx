use std::fmt::Debug;
use std::rc::Rc;

use thiserror::Error;

use crate::parse::expr::ExprId;
use crate::parse::stmt::{Stmt, StmtFunctionId, StmtFunctionL, Unwind};
use crate::util::Location;

use super::class::Instance;
use super::interner::{Interner, Key};
use super::value::ValueGen;
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

type NativeFn = fn(args: &[Value]) -> Result<Value, RuntimeError>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Native {
    pub name: Key,
    pub params: Box<[Key]>,
    pub body: NativeFn,
}

#[derive(Clone, Debug)]
pub struct UserDefined {
    pub func: StmtFunctionId,
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

    pub fn call<F>(&self, args: ValueGen<'_, '_, F>) -> Result<Value, RuntimeError>
    where
        F: Fn(ExprId) -> Result<Value, RuntimeError>,
    {
        if args.len() != self.arity() {
            return Err(FunctionError::MismatchedArgument {
                loc: Location::new(0, 0),
                expect: self.arity(),
                got: args.len(),
            }
            .into());
        }

        let args = args.collect::<Result<Box<_>, _>>()?;
        (self.body)(&args)
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
    pub fn new(func: StmtFunctionId, capture: Rc<Env>, kind: Kind) -> Self {
        Self {
            func,
            capture,
            kind,
        }
    }

    pub fn call<F>(
        &self,
        interpreter: &Interpreter,
        args: ValueGen<'_, '_, F>,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(ExprId) -> Result<Value, RuntimeError>,
    {
        let StmtFunctionL { func, loc } = interpreter.ast.get_func(&self.func);
        if args.len() != func.params.len() {
            return Err(FunctionError::MismatchedArgument {
                loc: *loc,
                expect: func.params.len(),
                got: args.len(),
            }
            .into());
        }

        let args = args.collect::<Result<Vec<_>, _>>()?;

        let (env, interner) = (&interpreter.dyn_env, &interpreter.interner);
        let _guard = env.bind_scope(Rc::new(Env::with_parent(Rc::clone(&self.capture))));

        for (i, arg) in args.into_iter().enumerate() {
            env.define(func.params[i], arg);
        }

        let body = match &interpreter.ast.get_stmt(&func.body).stmt {
            Stmt::Block { statements } => statements,
            _ => unreachable!("Function body should be block"),
        };

        for stmt in body.iter() {
            if let Unwind::Return(value) = interpreter.execute(stmt)? {
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
        UserDefined::new(self.func, new_capture.into(), self.kind)
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
            lhs.partial_cmp(rhs)
        } else {
            let lhs = Rc::as_ptr(&self.capture) as usize;
            let rhs = Rc::as_ptr(&other.capture) as usize;
            lhs.partial_cmp(&rhs)
        }
    }
}
