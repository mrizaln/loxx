use std::cell::RefCell;
use std::rc::Rc;

use crate::parse::stmt::StmtFunction;
use crate::util::Location;

use super::interner::Key;
use super::value::Value;

// TODO: create the actual object
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Class {
    pub name: Key,
    pub methods: Box<[StmtFunction]>,
    pub loc: Location,
    counter: RefCell<usize>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Instance {
    pub id: usize,
    pub class: Rc<Class>,
    pub fields: Vec<Value>,
}

impl Class {
    pub fn new(name: Key, methods: Box<[StmtFunction]>, loc: Location) -> Self {
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
            fields: Vec::new(),
        }
    }
}

impl Instance {}
