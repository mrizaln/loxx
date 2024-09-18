use std::collections::HashMap;

use super::object::Value;

// TODO: use hash result of String as representation of variable name instead of String

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.values.get_mut(name)
    }
}
