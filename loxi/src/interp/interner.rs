use lasso::{Rodeo, Spur};
use strum::IntoEnumIterator;

use crate::lex::token::{Keyword, Special};
use crate::util::LoxToken;

pub type Key = Spur;

#[derive(Debug)]
pub struct Interner {
    rodeo: Rodeo,
}

impl Interner {
    pub fn new() -> Self {
        let mut interner = Self {
            rodeo: Rodeo::default(),
        };
        for keyword in Keyword::iter() {
            interner.get_or_intern(keyword.as_str());
        }
        for special in Special::iter() {
            interner.get_or_intern(special.as_str());
        }
        interner
    }

    pub fn contains(&self, str: &str) -> bool {
        self.rodeo.contains(str)
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

    pub fn get<T>(&self, string: T) -> Key
    where
        T: AsRef<str>,
    {
        self.rodeo
            .get(string)
            .expect("the key should be interned beforehand")
    }

    pub fn keyword(&self, keyword: Keyword) -> Key {
        self.get(keyword.as_str())
    }

    pub fn special(&self, special: Special) -> Key {
        self.get(special.as_str())
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
