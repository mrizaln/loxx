use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::bytecode::Address;
use crate::memory::HeapId;

pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Class(Rc<Class>),
    String(Rc<String>),
    Function(Rc<Function>),
    Instance(Rc<Instance>),
}

pub enum Constant<'a> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'a str),
}

pub struct Function {
    captures: Vec<HeapId>,
    address: Address,
}

pub struct Class {
    constructor: Option<Function>,
    methods: FxHashMap<String, Function>,
}

pub struct Instance {
    id: usize,
    class: *const Class,
    fields: FxHashMap<String, Value>,
}
