use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::util::Location;

use super::function::Function;
use super::interner::Key;
use super::value::Value;

// TODO: create the actual object
#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: Key,
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
    pub fn new(name: Key, methods: FxHashMap<Key, Rc<Function>>, loc: Location) -> Self {
        Self {
            name,
            methods,
            loc,
            counter: 1.into(),
        }
    }

    pub fn construct(self: &Rc<Self>, args: Box<[Value]>) -> Instance {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        Instance {
            id,
            class: Rc::clone(self),
            fields: FxHashMap::default().into(),
        }
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.name, self.loc).partial_cmp(&(other.name, other.loc))
    }
}

impl Instance {
    pub fn get(&self, name: Key) -> Option<Property> {
        if let Some(value) = self.fields.borrow().get(&name) {
            return Some(Property::Field(value.clone()));
        }
        if let Some(func) = self.class.methods.get(&name) {
            return Some(Property::Method(Rc::clone(func)));
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
