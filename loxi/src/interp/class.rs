use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::parse::expr::ExprId;
use crate::util::Loc;

use super::function::{FunctionError, UserDefined};
use super::interner::{Interner, Key};
use super::value::{Value, ValueGen};
use super::{Interpreter, RuntimeError};

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: Key,
    pub base: Option<Rc<Class>>,
    pub constructor: Option<UserDefined>, // constructor is not simply a method
    pub methods: FxHashMap<Key, UserDefined>,
    pub loc: Loc,
    counter: RefCell<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub id: usize,
    pub class: Rc<Class>,
    pub fields: RefCell<FxHashMap<Key, Value>>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Property {
    Field(Value),
    Method(UserDefined),
}

impl Class {
    pub fn new(
        name: Key,
        base: Option<Rc<Class>>,
        constructor: Option<UserDefined>,
        methods: FxHashMap<Key, UserDefined>,
        loc: Loc,
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
        loc: Loc,
        interpreter: &Interpreter,
        args: ValueGen<'_, '_, F>,
    ) -> Result<Rc<Instance>, RuntimeError>
    where
        F: Fn(ExprId) -> Result<Value, RuntimeError>,
    {
        let instance = Instance::new(self);

        let ctor = match self.find_ctor() {
            Some(ctor) => ctor,
            None => match args.len() {
                0 => return Ok(instance),
                len => {
                    return Err(Into::into(FunctionError::MismatchedArgument {
                        loc,
                        expect: 0,
                        got: len,
                    }))
                }
            },
        };

        let func = ctor.bind(Rc::clone(&instance), &interpreter.interner);
        let value = func.call(interpreter, args, loc)?;

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

    /// Search for a method in the class and its base class (constructor is not included).
    pub fn find_method(&self, name: Key) -> Option<&UserDefined> {
        match self.methods.get(&name) {
            Some(func) => Some(func),
            None => self.base.as_ref().and_then(|b| b.find_method(name)),
        }
    }

    /// Search for a constructor in the class and its base class.
    pub fn find_ctor(&self) -> Option<&UserDefined> {
        match &self.constructor {
            Some(ctor) => Some(ctor),
            None => self.base.as_ref().and_then(|b| b.find_ctor()),
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

    pub fn get_method(self: &Rc<Instance>, name: Key, interner: &Interner) -> Option<UserDefined> {
        let find_func = |name| match name {
            // Accessing the constructor directly using `instance.init` expression.
            // Apparently, this "reinitialize" any field that is touched by constructor while retaining
            // other fields that is not... what a bizarre choice dear Robert Nystrom o_O.
            // If I were him, I will disallow calling init or define a property named init.
            x if x == interner.key_init => self.class.find_ctor(),

            _ => self.class.find_method(name),
        };

        find_func(name).map(|func| func.bind(Rc::clone(self), interner))
    }

    pub fn get(self: &Rc<Instance>, name: Key, interner: &Interner) -> Option<Property> {
        match self.fields.borrow().get(&name) {
            Some(value) => Some(Property::Field(value.clone())),
            None => self.get_method(name, interner).map(Property::Method),
        }
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
