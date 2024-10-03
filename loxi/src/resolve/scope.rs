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
}

impl Scope {
    pub fn new_empty() -> Self {
        Self {
            stack: RefCell::new(vec![]),
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

    pub fn define(&self, key: Key, value: VarBind) {
        let mut map = self.get_map(self.index());
        map.insert(key, value);
    }

    pub fn get_at(&self, key: Key, index: usize) -> Option<RefMut<'_, VarBind>> {
        if index >= self.len() {
            panic!(
                "index exceed the len of vec (index: {}, size: {})",
                index,
                self.len()
            );
        }
        let map = self.get_map(index);
        RefMut::filter_map(map, |m| m.get_mut(&key)).ok()
    }

    pub fn get_current(&self, key: Key) -> Option<RefMut<'_, VarBind>> {
        if self.is_empty() {
            panic!("accessing empty Scope")
        }
        self.get_at(key, self.len() - 1)
    }

    // get the value of the key and the distance from the current scope
    pub fn get(&self, key: Key) -> Option<(RefMut<'_, VarBind>, usize)> {
        for i in (0..=self.index()).rev() {
            // distance must be calculated first since Self::get_at mutably borrows self
            let distance = self.index() - 1;
            if let Some(value) = self.get_at(key, i) {
                return Some((value, distance));
            }
        }
        None
    }

    fn get_map(&self, index: usize) -> RefMut<'_, FxHashMap<Key, VarBind>> {
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
