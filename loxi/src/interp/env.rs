use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::value::Value;

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

    pub fn get(&self, key: &str) -> Option<RefMut<'_, Value>> {
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
    use super::*;

    #[test]
    fn env_has_parent() {
        let clone = |v: RefMut<'_, Value>| v.clone();

        let parent = Env::new();
        parent.define("a".to_string(), Value::Number(1.0));
        assert_eq!(parent.get("a").map(clone), Some(Value::Number(1.0)));

        let child = parent.child();
        child.define("b".to_string(), Value::Number(2.0));
        assert_eq!(child.get("a").map(clone), Some(Value::Number(1.0)));
        assert_eq!(child.get("b").map(clone), Some(Value::Number(2.0)));

        let one = borrow1(&child);
        let two = borrow2(&child);

        assert_eq!(one, Value::Number(1.0));
        assert_eq!(two, Value::Number(2.0));
    }

    fn borrow1(env: &Env) -> Value {
        env.get("a").unwrap().clone()
    }

    fn borrow2(env: &Env) -> Value {
        env.get("b").unwrap().clone()
    }
}
