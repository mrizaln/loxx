use std::cell::{RefCell, RefMut};

use rustc_hash::FxHashMap;

use super::interner::Key;
use super::value::Value;

#[derive(Debug)]
pub struct Env {
    stack: RefCell<Vec<FxHashMap<Key, Value>>>,
}

#[must_use = "EnvGuard lifetime defines the lifetime of the scope, it will immediately drop if not used"]
pub struct EnvGuard<'a> {
    stack: &'a Env,
    index: usize,
}

impl Env {
    pub fn new_with_global() -> Self {
        Self {
            stack: RefCell::new(vec![FxHashMap::default()]),
        }
    }

    pub fn len(&self) -> usize {
        self.stack.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn create_scope(&self) -> EnvGuard<'_> {
        self.stack.borrow_mut().push(FxHashMap::default());
        EnvGuard {
            stack: self,
            index: self.len() - 1,
        }
    }

    pub fn define(&self, key: Key, value: Value) {
        let mut map = self.get_map(self.index());
        map.insert(key, value);
    }

    pub fn get_at(&self, key: Key, index: usize) -> Option<RefMut<'_, Value>> {
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

    pub fn get(&self, key: Key) -> Option<RefMut<'_, Value>> {
        for i in (0..=self.index()).rev() {
            if let Some(value) = self.get_at(key, i) {
                return Some(value);
            }
        }
        None
    }

    fn get_map(&self, index: usize) -> RefMut<'_, FxHashMap<Key, Value>> {
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

impl Drop for EnvGuard<'_> {
    fn drop(&mut self) {
        // check if the stack is invalid (parent scope is dropped before child scope)
        if self.index != self.stack.len() - 1 {
            panic!(
                "invalid stack frame drop order (index: {}, len: {})",
                self.index,
                self.stack.len()
            );
        }
        self.stack.stack.borrow_mut().pop();
    }
}
