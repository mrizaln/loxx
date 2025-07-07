use std::cell::{RefCell, RefMut};

use rustc_hash::FxHashMap;

use crate::interp::interner::Key;
use crate::util::Location;

#[derive(Debug)]
pub enum VarBind {
    Decl(Location),
    Def(Location),
}

#[derive(Debug)]
pub struct Scope {
    stack: RefCell<Vec<FxHashMap<Key, VarBind>>>,
    globals: RefCell<FxHashMap<Key, VarBind>>,
}

pub enum ScopeError {
    DuplicateDefine(Location),
}

impl VarBind {
    pub fn loc(&self) -> Location {
        match self {
            VarBind::Decl(loc) => *loc,
            VarBind::Def(loc) => *loc,
        }
    }
}

impl Scope {
    pub fn new_empty() -> Self {
        Self {
            stack: RefCell::new(vec![]),
            globals: RefCell::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.stack.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn create_scope(&self) {
        self.stack.borrow_mut().push(FxHashMap::default());
    }

    pub fn drop_scope(&self) {
        self.stack.borrow_mut().pop();
    }

    pub fn define(&self, key: Key, value: VarBind) -> Result<(), ScopeError> {
        let mut map = self.get_map(0);
        match map.insert(key, value) {
            Some(kind) => Err(ScopeError::DuplicateDefine(kind.loc())),
            None => Ok(()),
        }
    }

    // globals is allowed to be redefined
    pub fn define_global(&self, key: Key, value: VarBind) -> Result<(), ScopeError> {
        self.globals.borrow_mut().insert(key, value);
        Ok(())
    }

    pub fn get_at(&self, key: Key, distance: usize) -> Option<RefMut<'_, VarBind>> {
        if distance > self.index() {
            panic!(
                "distance exceed the len of vec (index: {}, size: {})",
                distance,
                self.len()
            );
        }
        let map = self.get_map(distance);
        RefMut::filter_map(map, |m| m.get_mut(&key)).ok()
    }

    pub fn get_current(&self, key: Key) -> Option<RefMut<'_, VarBind>> {
        if self.is_empty() {
            panic!("accessing empty Scope")
        }
        self.get_at(key, 0)
    }

    pub fn get_global(&self, key: Key) -> Option<RefMut<'_, VarBind>> {
        RefMut::filter_map(self.globals.borrow_mut(), |m| m.get_mut(&key)).ok()
    }

    // get the value of the key and the distance from the current scope
    pub fn get(&self, key: Key) -> Option<(RefMut<'_, VarBind>, usize)> {
        for i in 0..=self.index() {
            if let Some(value) = self.get_at(key, i) {
                return Some((value, i));
            }
        }
        None
    }

    pub fn globals(&self) -> Vec<Key> {
        self.globals.borrow().iter().map(|(k, _)| *k).collect()
    }

    fn get_map(&self, distance: usize) -> RefMut<'_, FxHashMap<Key, VarBind>> {
        let index = self.index() - distance;
        let stack = self.stack.borrow_mut();
        RefMut::map(stack, |stack| {
            stack.get_mut(index).unwrap_or_else(|| {
                panic!(
                    "iter should point to a valid stack frame (index: {}, len: {})",
                    index,
                    self.len()
                )
            })
        })
    }

    fn index(&self) -> usize {
        self.len() - 1
    }
}
