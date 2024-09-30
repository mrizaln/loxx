use std::cell::{RefCell, RefMut};
use std::rc::Rc;

use fnv::FnvHashMap;
use lasso::Spur;

use super::value::Value;

// inspired by: https://stackoverflow.com/a/48298865/16506263
pub struct Env {
    inner: Option<Rc<Node>>,
}

struct Node {
    values: RefCell<FnvHashMap<Spur, Value>>,
    parent: Env,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: Some(Rc::new(Node {
                values: RefCell::new(FnvHashMap::default()),
                parent: Env { inner: None },
            })),
        }
    }

    pub fn child(&self) -> Self {
        let node = Node {
            values: RefCell::new(FnvHashMap::default()),
            parent: self.clone(),
        };
        Env {
            inner: Some(Rc::new(node)),
        }
    }

    pub fn define(&self, key: Spur, value: Value) {
        self.inner
            .as_ref()
            .unwrap()
            .values
            .borrow_mut()
            .insert(key, value);
    }

    pub fn define_parentmost(&self, key: Spur, value: Value) {
        let mut env = self;
        loop {
            let node = env.inner.as_ref().unwrap();
            if let None = node.parent.inner {
                break env.define(key, value);
            }
            env = &node.parent;
        }
    }

    pub fn get(&self, key: &Spur) -> Option<RefMut<'_, Value>> {
        // lookup at the current node
        let values = self.inner.as_ref()?.values.borrow_mut();
        let value = RefMut::filter_map(values, |values| values.get_mut(key));

        match value {
            // else lookup at parent node
            Err(_) => self.inner.as_ref().unwrap().parent.get(key),
            _ => value.ok(),
        }
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use lasso::Rodeo;

    use super::*;

    #[test]
    fn env_has_parent() {
        let clone = |v: RefMut<'_, Value>| v.clone();

        let mut arena = Rodeo::default();
        let mut intern = |str| arena.get_or_intern(str);

        let parent = Env::new();
        parent.define(intern("a"), Value::number(1.0));
        assert_eq!(
            parent.get(&intern("a")).map(clone),
            Some(Value::number(1.0))
        );

        let child = parent.child();
        child.define(intern("b"), Value::number(2.0));
        assert_eq!(child.get(&intern("a")).map(clone), Some(Value::number(1.0)));
        assert_eq!(child.get(&intern("b")).map(clone), Some(Value::number(2.0)));

        let one = borrow1(&child, &arena);
        let two = borrow2(&child, &arena);

        assert_eq!(one, Value::number(1.0));
        assert_eq!(two, Value::number(2.0));
    }

    fn borrow1(env: &Env, arena: &Rodeo) -> Value {
        env.get(&arena.get("a").unwrap()).unwrap().clone()
    }

    fn borrow2(env: &Env, arena: &Rodeo) -> Value {
        env.get(&arena.get("b").unwrap()).unwrap().clone()
    }
}
