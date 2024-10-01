use lasso::{Rodeo, Spur};

pub type Key = Spur;

#[derive(Debug)]
pub struct Interner {
    rodeo: Rodeo,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            rodeo: Rodeo::default(),
        }
    }

    pub fn resolve(&self, key: Key) -> &str {
        self.rodeo.resolve(&key)
    }

    pub fn get_or_intern<T>(&mut self, string: T) -> Key
    where
        T: AsRef<str>,
    {
        self.rodeo.get_or_intern(string)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
