use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::object::Value;

// inspired by: https://stackoverflow.com/a/48298865/16506263
pub struct Env {
    inner: Option<Rc<Node>>,
}

struct Node {
    values: RefCell<HashMap<String, Value>>,
    parent: Env,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: Some(Rc::new(Node {
                values: RefCell::new(HashMap::new()),
                parent: Env { inner: None },
            })),
        }
    }

    pub fn child(&self) -> Self {
        let node = Node {
            values: RefCell::new(HashMap::new()),
            parent: self.clone(),
        };
        Env {
            inner: Some(Rc::new(node)),
        }
    }

    pub fn define(&self, key: String, value: Value) {
        self.inner
            .as_ref()
            .unwrap()
            .values
            .borrow_mut()
            .insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Ref<'_, Value>> {
        // lookup at the current node
        let values = self.inner.as_ref()?.values.borrow();

        // the lookup is done twice, is there other way to do this?
        if let Some(value) = match values.contains_key(key) {
            true => Some(Ref::map(values, |values| values.get(key).unwrap())),
            false => None,
        } {
            Some(value)
        } else {
            self.inner.as_ref().unwrap().parent.get(key)
        }
    }

    pub fn get_mut(&self, key: &str) -> Option<RefMut<'_, Value>> {
        // lookup at the current node
        let values = self.inner.as_ref()?.values.borrow_mut();

        // the lookup is done twice, is there other way to do this?
        if let Some(value) = match values.contains_key(key) {
            true => Some(RefMut::map(values, |values| values.get_mut(key).unwrap())),
            false => None,
        } {
            Some(value)
        } else {
            self.inner.as_ref().unwrap().parent.get_mut(key)
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
    use super::*;

    #[test]
    fn env_has_parent() {
        let clone = |v: Ref<'_, Value>| v.clone();

        let mut parent = Env::new();
        parent.define("a".to_string(), Value::Number(1.0));
        assert_eq!(parent.get("a").map(clone), Some(Value::Number(1.0)));

        let mut child = parent.child();
        child.define("b".to_string(), Value::Number(2.0));
        assert_eq!(child.get("a").map(clone), Some(Value::Number(1.0)));
        assert_eq!(child.get("b").map(clone), Some(Value::Number(2.0)));

        let one = borrow1(&mut child);
        let two = borrow2(&mut child);

        assert_eq!(one, Value::Number(1.0));
        assert_eq!(two, Value::Number(2.0));
    }

    fn borrow1(env: &mut Env) -> Value {
        env.get_mut("a").unwrap().clone()
    }

    fn borrow2(env: &mut Env) -> Value {
        env.get_mut("b").unwrap().clone()
    }
}
