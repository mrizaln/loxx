use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::lex::token::Special;
use crate::parse::stmt::{Stmt, Unwind};
use crate::util::Location;

use super::env::DynamicEnv;
use super::function::{Function, FunctionError};
use super::interner::{Interner, Key};
use super::value::Value;
use super::RuntimeError;

// TODO: create the actual object
#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: Key,
    pub base: Option<Rc<Class>>,
    pub constructor: Option<Rc<Function>>,
    pub methods: FxHashMap<Key, Rc<Function>>,
    pub loc: Location,
    counter: RefCell<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub id: usize,
    pub class: Rc<Class>,
    pub fields: RefCell<FxHashMap<Key, Value>>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Property {
    Field(Value),
    Method(Rc<Function>),
}

impl Class {
    pub fn new(
        name: Key,
        base: Option<Rc<Class>>,
        constructor: Option<Rc<Function>>,
        methods: FxHashMap<Key, Rc<Function>>,
        loc: Location,
    ) -> Self {
        Self {
            name,
            base,
            constructor,
            methods,
            loc,
            counter: 1.into(),
        }
    }

    pub fn construct<F>(
        self: &Rc<Self>,
        args: Box<[Value]>,
        interner: &Interner,
        env: &DynamicEnv,
        loc: Location,
        exec: F,
    ) -> Result<Rc<Instance>, RuntimeError>
    where
        F: FnMut(&Stmt) -> Result<Unwind, RuntimeError>,
    {
        let instance = Instance::new(self);

        if self.constructor.is_none() {
            return match args.len() {
                0 => Ok(instance),
                len => Err(FunctionError::MismatchedArgument {
                    loc,
                    expect: 0,
                    got: len,
                }
                .into()),
            };
        }

        let ctor = self.constructor.as_ref().unwrap().as_user_defined();
        let func = ctor.bind(Rc::clone(&instance), interner);
        let value = func.call(args, interner, env, exec)?;

        match value {
            Value::Nil => (),
            Value::Instance(ret) => match Rc::ptr_eq(&instance, &ret) {
                true => (),
                false => unreachable!("constructor can't return a different instance!"),
            },
            _ => unreachable!("constructor can only return the instance itself"),
        }

        Ok(instance)
    }

    pub fn find_method(&self, name: Key) -> Option<Rc<Function>> {
        match self.methods.get(&name) {
            Some(func) => Some(Rc::clone(func)),
            None => self.base.as_ref().and_then(|b| b.find_method(name)),
        }
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.name, self.loc).partial_cmp(&(other.name, other.loc))
    }
}

impl Instance {
    fn new(class: &Rc<Class>) -> Rc<Self> {
        let id = *class.counter.borrow();
        *class.counter.borrow_mut() += 1;
        Rc::new(Instance {
            id,
            class: Rc::clone(class),
            fields: FxHashMap::default().into(),
        })
    }

    pub fn get(self: &Rc<Instance>, name: Key, interner: &Interner) -> Option<Property> {
        if let Some(value) = self.fields.borrow().get(&name) {
            return Some(Property::Field(value.clone()));
        }

        // Accessing the constructor directly using `instance.init` expression.
        // Apparently, this "reinitialize" any field that is touched by constructor while retaining
        // other fields that is not... what a bizarre choice dear Robert Nystrom o_O.
        // If I were him, I will disallow calling init or define a property named init.
        if name == interner.special(Special::Init) && self.class.constructor.is_some() {
            let ctor = self.class.constructor.as_ref().unwrap().as_user_defined();
            let bound = Function::UserDefined(ctor.bind(Rc::clone(self), interner));
            return Some(Property::Method(Rc::new(bound)));
        }

        if let Some(func) = self.class.find_method(name) {
            match func.as_ref() {
                Function::UserDefined(func) => {
                    let bound = Function::UserDefined(func.bind(Rc::clone(self), interner));
                    return Some(Property::Method(Rc::new(bound)));
                }
                Function::Native(_) => panic!("native function should not be inside an instance!"),
            }
        }

        None
    }

    pub fn set(&self, name: Key, value: Value) {
        self.fields.borrow_mut().insert(name, value);
    }
}

impl PartialOrd for Instance {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let lfields = self.fields.borrow();
        let rfields = other.fields.borrow();
        let lkeys = lfields.keys().collect::<Vec<_>>();
        let rkeys = rfields.borrow().keys().collect::<Vec<_>>();
        (self.id, &self.class, lkeys).partial_cmp(&(other.id, &other.class, rkeys))
    }
}
