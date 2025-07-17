use thiserror::Error;

use crate::value::{Instance, Value};

pub struct Heap {
    pages: Vec<Page<{ Heap::PAGE_SIZE }>>,
}

pub struct Stack {
    values: Vec<Value>,
    blocks: Vec<usize>,
}

#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("Stack overflow (wants: {0:010x}, limit: {1:010x}")]
    StackOverflow(usize, usize),

    #[error("Stack is empty")]
    EmptyStack,
}

pub enum HeapValue {
    Value(Value),
    String(String),
    Instance(Instance),
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct HeapId {
    page: usize,
    index: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct ValueId {
    index: usize,
}

struct PageValue {
    value: HeapValue,
    mark: Mark,
}

struct Page<const N: usize> {
    mem: Box<[Option<PageValue>; N]>,
    count: usize,       // number of filled slots
    first_empty: usize, // first empty slot
}

enum Mark {
    Live,
    Dead,
}

impl Heap {
    const PAGE_SIZE: usize = size_of::<Value>() * 1024;

    pub fn new() -> Self {
        Self { pages: Vec::new() }
    }

    pub fn construct(&mut self, value: HeapValue) -> HeapId {
        for (p, page) in self.pages.iter_mut().enumerate() {
            if page.can_insert() {
                let i = page.insert(value);
                return HeapId { page: p, index: i };
            }
        }

        self.pages.push(Page::new());

        let p = self.pages.len() - 1;
        let i = (&mut self.pages[p]).insert(value);

        HeapId { page: p, index: i }
    }

    pub fn destruct(&mut self, id: HeapId) {
        let HeapId { page, index } = id;
        let page = &mut self.pages[page];
        page.clear(index);
    }

    pub fn replace(&mut self, id: HeapId, value: HeapValue) -> Option<HeapValue> {
        let HeapId { page, index } = id;
        let page = &mut self.pages[page];
        page.replace(index, value)
    }

    pub fn get(&self, id: HeapId) -> Option<&HeapValue> {
        self.get_with_mark(id).map(|v| &v.value)
    }

    pub fn get_mut(&mut self, id: HeapId) -> Option<&mut HeapValue> {
        self.get_with_mark_mut(id).map(|v| &mut v.value)
    }

    pub fn get_two(&self, one: HeapId, two: HeapId) -> Option<(&HeapValue, &HeapValue)> {
        let (page_one, page_two) = (&self.pages[one.page], &self.pages[two.page]);
        Some((
            &page_one.get(one.index)?.value,
            &page_two.get(two.index)?.value,
        ))
    }

    pub fn get_two_mut(
        &mut self,
        one: HeapId,
        two: HeapId,
    ) -> Option<(&mut HeapValue, &mut HeapValue)> {
        // inspired by: https://stackoverflow.com/a/74296885
        let (start, end) = match one.page < two.page {
            true => (one.page, two.page),
            false => (two.page, one.page),
        };
        let (left, right) = self.pages.split_at_mut(start + 1);
        let (page_one, page_two) = (&mut left[start], &mut right[end - start - 1]);
        Some((
            &mut page_one.get_mut(one.index)?.value,
            &mut page_two.get_mut(two.index)?.value,
        ))
    }

    fn get_with_mark(&self, id: HeapId) -> Option<&PageValue> {
        let HeapId { page, index } = id;
        let page = &self.pages[page];
        page.get(index)
    }

    fn get_with_mark_mut(&mut self, id: HeapId) -> Option<&mut PageValue> {
        let HeapId { page, index } = id;
        let page = &mut self.pages[page];
        page.get_mut(index)
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            values: Vec::default(),
            blocks: Vec::default(),
        }
    }

    pub fn create_block(&mut self) {
        self.blocks.push(self.values.len())
    }

    pub fn drop_block(&mut self) {
        let block = self.blocks.pop().expect("excessive drop");
        self.values.truncate(block);
    }

    pub fn push(&mut self, value: Value) -> ValueId {
        let i = self.values.len();
        self.values.push(value);
        ValueId { index: i }
    }

    pub fn dup(&mut self, index: usize) -> Result<(), MemoryError> {
        let value = self.peek(index)?;
        self.values.push(value.clone());
        Ok(())
    }

    pub fn pop(&mut self) -> Result<Value, MemoryError> {
        self.values.pop().ok_or(MemoryError::EmptyStack)
    }

    pub fn top(&self) -> Result<&Value, MemoryError> {
        self.values.last().ok_or(MemoryError::EmptyStack)
    }

    pub fn top_mut(&mut self) -> Result<&mut Value, MemoryError> {
        self.values.last_mut().ok_or(MemoryError::EmptyStack)
    }

    pub fn peek(&self, index: usize) -> Result<&Value, MemoryError> {
        self.values
            .get(index)
            .ok_or(MemoryError::StackOverflow(index, self.values.len()))
    }

    pub fn peek_mut(&mut self, index: usize) -> Result<&mut Value, MemoryError> {
        let len = self.values.len();
        self.values
            .get_mut(index)
            .ok_or(MemoryError::StackOverflow(index, len))
    }
}

impl<const N: usize> Page<N> {
    pub fn new() -> Self {
        let vec = Vec::from_iter((0..N).map(|_| Option::<PageValue>::None));
        let ptr = Box::into_raw(vec.into_boxed_slice());
        Self {
            mem: unsafe { Box::from_raw(ptr as *mut [_; N]) },
            count: 0,
            first_empty: 0,
        }
    }

    fn can_insert(&self) -> bool {
        self.count < N
    }

    fn insert(&mut self, value: HeapValue) -> usize {
        let i = self.first_empty;

        self.mem[i] = Some(PageValue {
            value,
            mark: Mark::Dead,
        });
        self.count += 1;

        let first = self.mem[i..N].iter().position(|v| v.is_none());
        self.first_empty = first.unwrap_or(N);

        i
    }

    fn clear(&mut self, index: usize) {
        self.mem[index] = None;
        if self.first_empty > index {
            self.first_empty = index;
        }
        self.count -= 1;
    }

    fn get(&self, index: usize) -> Option<&PageValue> {
        self.mem[index].as_ref()
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut PageValue> {
        self.mem[index].as_mut()
    }

    fn replace(&mut self, index: usize, value: HeapValue) -> Option<HeapValue> {
        std::mem::replace(
            &mut self.mem[index],
            Some(PageValue {
                value,
                mark: Mark::Dead,
            }),
        )
        .map(|v| v.value)
    }
}

impl HeapValue {
    pub fn as_string_mut(&mut self) -> &mut String {
        match self {
            HeapValue::String(s) => s,
            _ => panic!("internal heap value is not a string!"),
        }
    }

    pub fn as_string(&self) -> &String {
        match self {
            HeapValue::String(s) => s,
            _ => panic!("internal heap value is not a string!"),
        }
    }
}

pub fn gc(stack: &Stack, heap: &mut Heap) {
    for v in stack.values.iter() {
        match v {
            Value::Nil => todo!(),
            Value::Bool(_) => todo!(),
            Value::Number(_) => todo!(),
            Value::String(heap_id) => todo!(),
            Value::Class(class_id) => todo!(),
            Value::Instance(heap_id) => todo!(),
            Value::Function(func_id, heap_id) => todo!(),
        }
    }
}
