use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use super::interner::Key;
use super::value::Value;

#[derive(Debug)]
pub struct Env {
    pub values: RefCell<FxHashMap<Key, Value>>,
    pub parent: Option<Rc<Env>>,
}

#[derive(Debug)]
pub struct DynamicEnv {
    global: Rc<Env>,
    current: RefCell<Rc<Env>>,
}

#[must_use]
pub struct EnvGuard<'a> {
    env: &'a DynamicEnv,
}

#[must_use]
pub struct EnvBindGuard<'a> {
    env: &'a DynamicEnv,
    previous: Rc<Env>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(FxHashMap::default()),
            parent: None,
        }
    }

    pub fn new_with_parent(parent: Rc<Env>) -> Self {
        Self {
            values: RefCell::new(FxHashMap::default()),
            parent: Some(parent),
        }
    }

    pub fn define(&self, key: Key, value: Value) {
        self.values.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: Key) -> Option<Value> {
        match self.values.borrow().get(&key) {
            Some(value) => Some(value.clone()),
            _ => match &self.parent {
                Some(parent) => parent.get(key),
                _ => None,
            },
        }
    }

    pub fn modify<F, R>(&self, key: Key, f: F) -> Option<R>
    where
        F: FnOnce(&mut Value) -> R,
    {
        match self.values.borrow_mut().get_mut(&key) {
            Some(value) => Some(f(value)),
            _ => match &self.parent {
                Some(parent) => parent.modify(key, f),
                _ => None,
            },
        }
    }
}

impl DynamicEnv {
    pub fn new_with_global() -> Self {
        let env = Rc::new(Env::new());
        Self {
            current: Rc::clone(&env).into(),
            global: env,
        }
    }

    pub fn current(&self) -> Rc<Env> {
        Rc::clone(&self.current.borrow())
    }

    pub fn create_scope(&self) -> EnvGuard<'_> {
        let parent = Rc::clone(&self.current.borrow());
        let env = Rc::new(Env::new_with_parent(parent));
        *self.current.borrow_mut() = env;
        EnvGuard { env: self }
    }

    fn destroy_scope(&self) {
        let parent = Rc::clone(self.current.borrow().parent.as_ref().unwrap());
        *self.current.borrow_mut() = parent;
    }

    pub fn bind_scope(&self, env: Rc<Env>) -> EnvBindGuard<'_> {
        let previous = std::mem::replace(&mut *self.current.borrow_mut(), env);
        EnvBindGuard {
            env: self,
            previous,
        }
    }

    fn rebind_scope(&self, env: Rc<Env>) {
        *self.current.borrow_mut() = env;
    }

    pub fn define(&self, key: Key, value: Value) {
        self.current.borrow_mut().define(key, value);
    }

    pub fn get_global(&self, key: Key) -> Option<Value> {
        self.global.get(key)
    }

    pub fn get_at(&self, key: Key, mut distance: usize) -> Option<Value> {
        let mut current = Rc::clone(&self.current.borrow());
        while distance > 0 {
            current = Rc::clone(current.parent.as_ref()?);
            distance -= 1;
        }
        current.get(key)
    }

    pub fn modify_global<F, R>(&self, key: Key, f: F) -> Option<R>
    where
        F: FnOnce(&mut Value) -> R,
    {
        self.global.modify(key, f)
    }

    pub fn modify_at<F, R>(&self, key: Key, mut distance: usize, f: F) -> Option<R>
    where
        F: FnOnce(&mut Value) -> R,
    {
        let mut current = Rc::clone(&self.current.borrow());
        while distance > 0 {
            current = Rc::clone(current.parent.as_ref()?);
            distance -= 1;
        }
        current.modify(key, f)
    }
}

impl Drop for EnvGuard<'_> {
    fn drop(&mut self) {
        self.env.destroy_scope();
    }
}

impl Drop for EnvBindGuard<'_> {
    fn drop(&mut self) {
        self.env.rebind_scope(Rc::clone(&self.previous));
    }
}
