#![allow(dead_code)]

pub struct Value {}

#[derive(Debug, Copy, Clone)]
pub struct HeapId {
    page: usize,
    index: usize,
}

pub struct ValueId {
    index: usize,
}

struct Page<const N: usize> {
    mem: Box<[Option<Value>; N]>,
    count: usize,       // number of filled slots
    first_empty: usize, // first empty slot
}

enum PageInsert {
    Success(usize),
    Failed(Value),
}

pub enum StackValue {
    Stack(Value),
    Heap(HeapId),
}

pub struct Heap {
    pages: Vec<Page<{ Heap::PAGE_SIZE }>>,
}

pub struct Stack {
    stack: Vec<StackValue>,
    blocks: Vec<usize>,
}

pub struct Memory {}

impl<const N: usize> Page<N> {
    pub fn new() -> Self {
        let vec = Vec::from_iter((0..N).map(|_| Option::<Value>::None));
        let ptr = Box::into_raw(vec.into_boxed_slice());
        Self {
            mem: unsafe { Box::from_raw(ptr as *mut [_; N]) },
            count: 0,
            first_empty: 0,
        }
    }

    pub fn insert(&mut self, value: Value) -> PageInsert {
        if self.count < N {
            let i = self.first_empty;

            self.mem[i] = Some(value);
            self.count += 1;

            let first = self.mem[i..N].iter().position(|v| v.is_none());
            self.first_empty = first.unwrap_or(N);

            PageInsert::Success(i)
        } else {
            PageInsert::Failed(value)
        }
    }

    pub fn clear(&mut self, index: usize) {
        self.mem[index] = None;
        if self.first_empty > index {
            self.first_empty = index;
        }
        self.count -= 1;
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        self.mem[index].as_ref()
    }

    pub fn replace(&mut self, index: usize, value: Value) -> Option<Value> {
        std::mem::replace(&mut self.mem[index], Some(value))
    }
}

impl Heap {
    const PAGE_SIZE: usize = size_of::<Value>() * 1024;

    pub fn new() -> Self {
        Self { pages: Vec::new() }
    }

    pub fn construct(&mut self, mut value: Value) -> HeapId {
        for (p, page) in self.pages.iter_mut().enumerate() {
            value = match page.insert(value) {
                PageInsert::Success(i) => return HeapId { page: p, index: i },
                PageInsert::Failed(v) => v,
            }
        }

        self.pages.push(Page::new());

        let p = self.pages.len() - 1;
        let page = &mut self.pages[p];

        match page.insert(value) {
            PageInsert::Success(i) => HeapId { page: p, index: i },
            PageInsert::Failed(_) => unreachable!(),
        }
    }

    pub fn destruct(&mut self, HeapId { page, index }: HeapId) {
        let page = &mut self.pages[page];
        page.clear(index);
    }

    pub fn replace(&mut self, HeapId { page, index }: HeapId, value: Value) -> Option<Value> {
        let page = &mut self.pages[page];
        page.replace(index, value)
    }

    pub fn get(&self, HeapId { page, index }: HeapId) -> Option<&Value> {
        let page = &self.pages[page];
        page.get(index)
    }
}

impl Stack {
    pub fn new() -> Self {
        Self {
            stack: Vec::default(),
            blocks: Vec::default(),
        }
    }

    pub fn create_block(&mut self) {
        self.blocks.push(self.stack.len())
    }

    pub fn drop_block(&mut self) {
        let block = self.blocks.pop().expect("excessive drop");
        self.stack.truncate(block);
    }

    pub fn push(&mut self, value: StackValue) -> ValueId {
        let i = self.stack.len();
        self.stack.push(value);
        ValueId { index: i }
    }

    pub fn pop(&mut self) -> Option<StackValue> {
        self.stack.pop()
    }
}
