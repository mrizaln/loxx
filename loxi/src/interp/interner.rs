use lasso::{Rodeo, Spur};
use strum::IntoEnumIterator;

use crate::lex::token::Keyword;
use crate::util::LoxToken;

pub type Key = Spur;

#[derive(Debug)]
pub struct Interner {
    rodeo: Rodeo,
}

impl Interner {
    fn new() -> Self {
        Self {
            rodeo: Rodeo::default(),
        }
    }

    pub fn new_populate_with_keywords() -> Self {
        let mut interner = Self::new();
        for keyword in Keyword::iter() {
            interner.get_or_intern(keyword.as_str());
        }
        interner
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
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}
