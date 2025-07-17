use std::cell::{RefCell, RefMut};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::interp::interner::Key;
use crate::util::Loc;

#[derive(Debug)]
struct Info {
    kind: Kind,
    variables: FxHashMap<Key, Bind>,
    captures: Captures,
    hoists: Hoists,
}

#[derive(Debug, Default)]
pub struct Captures {
    inner: FxHashSet<(Key, Loc)>,
}

#[derive(Debug, Default)]
pub struct Hoists {
    inner: FxHashSet<(Key, Loc)>,
}

#[derive(Debug, Clone)]
pub enum Fun {
    Reg,
    Method,
    Ctor,
}

#[derive(Debug, Clone)]
pub enum Class {
    Reg,
    Sub,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Fun(Fun),
    Class(Class),
    Block,
}

#[derive(Debug)]
pub enum Bind {
    Decl(Loc),
    Def(Loc),
}

#[derive(Debug)]
pub struct Scope {
    stacks: RefCell<Vec<Info>>,
    globals: RefCell<FxHashMap<Key, Bind>>,
}

pub enum ScopeError {
    DuplicateDefine(Loc),
}

impl Captures {
    pub fn contains(&self, key: Key, loc: Loc) {
        self.inner.contains(&(key, loc));
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<(Key, Loc)> {
        self.inner.iter()
    }
}

impl Hoists {
    pub fn contains(&self, key: Key, loc: Loc) {
        self.inner.contains(&(key, loc));
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<(Key, Loc)> {
        self.inner.iter()
    }
}

impl Bind {
    pub fn loc(&self) -> Loc {
        match self {
            Bind::Decl(loc) => *loc,
            Bind::Def(loc) => *loc,
        }
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            stacks: RefCell::new(vec![]),
            globals: RefCell::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.stacks.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn create_scope(&self, kind: Kind) {
        self.stacks.borrow_mut().push(Info {
            kind,
            variables: FxHashMap::default(),
            captures: Captures::default(),
            hoists: Hoists::default(),
        });
    }

    #[must_use]
    pub fn drop_scope(&self) -> (Captures, Hoists) {
        self.stacks
            .borrow_mut()
            .pop()
            .map(|v| (v.captures, v.hoists))
            .expect("drop should not be excessively called")
    }

    pub fn define(&self, key: Key, value: Bind) -> Result<(), ScopeError> {
        let mut map = self.get_map(0);
        match map.variables.insert(key, value) {
            Some(kind) => Err(ScopeError::DuplicateDefine(kind.loc())),
            None => Ok(()),
        }
    }

    // globals is allowed to be redefined
    pub fn define_global(&self, key: Key, value: Bind) -> Result<(), ScopeError> {
        self.globals.borrow_mut().insert(key, value);
        Ok(())
    }

    pub fn get_current(&self, key: Key) -> Option<RefMut<'_, Bind>> {
        if self.is_empty() {
            panic!("accessing empty Scope")
        }
        self.get_at(key, 0)
    }

    pub fn get_global(&self, key: Key) -> Option<RefMut<'_, Bind>> {
        RefMut::filter_map(self.globals.borrow_mut(), |m| m.get_mut(&key)).ok()
    }

    // get the value of the key and the distance from the current scope
    pub fn get(&self, key: Key) -> Option<(RefMut<'_, Bind>, usize)> {
        for i in 0..=self.index() {
            if let Some(value) = self.get_at(key, i) {
                return Some((value, i));
            }
        }
        None
    }

    pub fn resolve(&self, key: Key) -> Option<usize> {
        self.get(key).map(|(bind, dist)| {
            let loc = bind.loc();
            drop(bind);

            let index = self.index();
            let start = index - dist + 1;
            let mut captured = false;

            for i in start..=index {
                let mut stack = self.stacks.borrow_mut();
                let info = stack.get_mut(i).unwrap();
                if let Kind::Fun(_) = info.kind {
                    info.captures.inner.insert((key, loc));
                    captured = true;
                }
            }

            if captured {
                let mut stack = self.stacks.borrow_mut();
                let stack = &mut stack.get_mut(index - dist).unwrap();
                stack.hoists.inner.insert((key, loc));
            }
            dist
        })
    }

    pub fn globals(&self) -> Vec<Key> {
        self.globals.borrow().iter().map(|(k, _)| *k).collect()
    }

    pub fn in_fun_scope(&self) -> Option<Fun> {
        let stacks = self.stacks.borrow_mut();
        for info in stacks.iter().rev() {
            if let Kind::Fun(f) = &info.kind {
                return Some(f.clone());
            }
        }
        None
    }

    pub fn in_class_scope(&self) -> Option<Class> {
        let stacks = self.stacks.borrow_mut();
        for info in stacks.iter().rev() {
            if let Kind::Class(c) = &info.kind {
                return Some(c.clone());
            }
        }
        None
    }

    fn get_at(&self, key: Key, distance: usize) -> Option<RefMut<'_, Bind>> {
        if distance > self.index() {
            panic!(
                "distance exceed the len of vec (index: {}, size: {})",
                distance,
                self.len()
            );
        }
        let map = self.get_map(distance);
        RefMut::filter_map(map, |m| m.variables.get_mut(&key)).ok()
    }

    fn get_map(&self, distance: usize) -> RefMut<'_, Info> {
        let index = self.index() - distance;
        let stack = self.stacks.borrow_mut();
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
