use core::str;
use std::fmt::Display;
use std::mem::transmute;
use std::str::Utf8Error;

use loxi::interner::{Interner, Key};
use loxi::util::Loc;
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::value::Constant;

#[derive(Copy, Clone, Default, Debug)]
pub struct ConstAddr(pub u32);

#[derive(Copy, Clone, Default, Debug)]
pub struct InstrAddr(pub u32);

#[derive(Copy, Clone, Default, Debug)]
pub struct InstrOff(pub i32);

#[derive(Copy, Clone, Default, Debug)]
pub struct StackIdx(pub u32);

#[derive(Copy, Clone, Default, Debug)]
pub struct Patch(u32);

#[derive(Debug)]
pub enum LoadOp {
    Local(StackIdx),
    Global(StackIdx),
    Upvalue(StackIdx),
}

#[derive(Debug)]
pub enum StoreOp {
    Local(StackIdx),
    Global(StackIdx),
    Upvalue(StackIdx),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Debug)]
pub enum JumpOp {
    Unconditional(InstrOff),
    OnFalse(InstrOff),
    OnTrue(InstrOff),
}

#[derive(Debug)]
pub enum Op<'a> {
    Pop(u32),
    Upvalue,
    Load(LoadOp),
    Store(StoreOp),
    Const(Constant<'a>),
    Jump(JumpOp),
    Unary(UnaryOp),
    Binary(BinOp),
    Print,
}

pub enum ConstKind {
    Nil,
    Bool(bool),
    Number(ConstAddr),
    String(ConstAddr),
}

pub enum JumpKind {
    Unconditional,
    OnFalse,
    OnTrue,
}

pub enum JumpDest {
    Current,
    Addr(InstrAddr),
}

#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("Encoded bytecode operation is invalid at {0:010x}")]
    InvalidOp(u32),

    #[error("{1} at {0:010x}")]
    StringError(u32, Utf8Error),

    #[error("Unterminated string literal at {0:010x}")]
    UnerminatedString(u32),
}

#[repr(u8)]
#[derive(Debug)]
enum ConstKindValue {
    Nil,
    Bool,
    Number,
    String,
}

#[repr(u8)]
enum OpValue {
    Pop,
    Upvalue,
    LoadLocal,
    LoadGlobal,
    LoadUpvalue,
    StoreLocal,
    StoreGlobal,
    StoreUpvalue,
    Const,       // 1 byte type + (0 byte: nil, 0 byte: bool | 8 byte: number | N byte: string)
    Jump,        // 4 byte destination
    JumpIfFalse, // 4 byte destination
    JumpIfTrue,  // 4 byte destination
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Not,
    Negate,
    Print,
}

pub struct Chunk {
    id: u32,
    name: ConstAddr,
    loc: Loc,
    code: Vec<u8>,
    reloc: Vec<u32>,
}

pub struct Bytecode {
    constant: Vec<u8>,
    code: Vec<u8>,
}

#[derive(Default)]
pub struct BytecodeBuilder {
    constant: Vec<u8>,
    constant_map: FxHashMap<Key, ConstAddr>,
    chunks_map: Vec<(Loc, Option<InstrAddr>)>, // finished chunks
    chunks_active: Vec<Chunk>,                 // pending and currently processed chunks
    chunk: Vec<u8>,                            // combined chunk
    reloc: Vec<u32>,                           // combined relocations
}

#[derive(Clone)]
pub struct BytecodeReader<'a> {
    constant: &'a [u8],
    code: &'a [u8],
    index: usize,
}

pub struct DisplayedBytecode<'a> {
    reader: BytecodeReader<'a>,
}

impl From<OpValue> for u8 {
    fn from(val: OpValue) -> Self {
        val as u8
    }
}

impl TryFrom<u8> for OpValue {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let lowest = OpValue::Pop as u8;
        let highest = OpValue::Print as u8;

        match value <= highest && value >= lowest {
            true => unsafe { Ok(transmute::<u8, OpValue>(value)) },
            false => Err(()),
        }
    }
}

impl TryFrom<u8> for ConstKindValue {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let lowest = ConstKindValue::Nil as u8;
        let highest = ConstKindValue::String as u8;

        match value <= highest && value >= lowest {
            true => unsafe { Ok(transmute::<u8, ConstKindValue>(value)) },
            false => Err(()),
        }
    }
}

impl ConstAddr {
    pub fn to_be_bytes(self) -> [u8; size_of::<u32>()] {
        self.0.to_be_bytes()
    }
}

impl InstrAddr {
    pub fn to_be_bytes(self) -> [u8; size_of::<u32>()] {
        self.0.to_be_bytes()
    }
}

impl InstrOff {
    pub fn to_be_bytes(self) -> [u8; size_of::<i32>()] {
        self.0.to_be_bytes()
    }
}

impl StackIdx {
    pub fn to_be_bytes(self) -> [u8; size_of::<u32>()] {
        self.0.to_be_bytes()
    }
}

impl From<[u8; size_of::<u32>()]> for ConstAddr {
    fn from(value: [u8; size_of::<u32>()]) -> Self {
        let num = u32::from_be_bytes(value);
        Self(num)
    }
}

impl From<[u8; size_of::<u32>()]> for InstrAddr {
    fn from(value: [u8; size_of::<u32>()]) -> Self {
        let num = u32::from_be_bytes(value);
        Self(num)
    }
}

impl From<[u8; size_of::<i32>()]> for InstrOff {
    fn from(value: [u8; size_of::<i32>()]) -> Self {
        let num = i32::from_be_bytes(value);
        Self(num)
    }
}

impl From<[u8; size_of::<u32>()]> for StackIdx {
    fn from(value: [u8; size_of::<u32>()]) -> Self {
        let num = u32::from_be_bytes(value);
        Self(num)
    }
}

impl Chunk {
    fn new(id: u32, name: ConstAddr, loc: Loc) -> Self {
        Self {
            id,
            name,
            loc,
            code: Vec::new(),
            reloc: Vec::new(),
        }
    }
}

impl Bytecode {
    pub fn display(&self) -> DisplayedBytecode {
        DisplayedBytecode {
            reader: self.into_iter(),
        }
    }
}

impl BytecodeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(mut self) -> Bytecode {
        for loc in self.reloc.iter().map(|v| *v as usize) {
            let slice = &mut self.chunk[loc..loc + 4];
            let index = u32::from_be_bytes(slice.try_into().unwrap());
            let addr = self.chunks_map[index as usize].1.unwrap();
            slice.copy_from_slice(&addr.to_be_bytes());
        }

        Bytecode {
            constant: self.constant,
            code: self.chunk,
        }
    }

    pub fn start_chunk(&mut self, name: ConstAddr, loc: Loc) {
        let chunk = Chunk::new(self.chunks_map.len() as u32, name, loc);
        self.chunks_active.push(chunk);
        self.chunks_map.push((loc, None));
    }

    pub fn end_chunk(&mut self) {
        let chunk = self.chunks_active.pop().unwrap();
        let offset = self.chunk.len();
        for loc in chunk.reloc {
            self.reloc.push(offset as u32 + loc);
        }
        self.chunk.extend(&chunk.code);
    }

    pub fn create_constant_string(&mut self, key: Key, interner: &Interner) -> ConstAddr {
        if let Some(addr) = self.constant_map.get(&key) {
            return *addr;
        }

        let str = interner.resolve(key);
        let addr = self.constant.len();
        self.constant.extend(str.as_bytes());
        self.constant.push(b'\0');

        ConstAddr(addr as u32)
    }

    pub fn create_consant_num(&mut self, num: f64) -> ConstAddr {
        let addr = self.constant.len();
        self.constant.extend(num.to_be_bytes());
        ConstAddr(addr as u32)
    }

    pub fn emit_pop(&mut self, amount: u32) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        code.push(OpValue::Pop as u8);
        code.extend(amount.to_be_bytes());
        InstrAddr(index as u32)
    }

    pub fn emit_upvalue(&mut self) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        code.push(OpValue::Upvalue as u8);
        InstrAddr(index as u32)
    }

    pub fn emit_load(&mut self, op: LoadOp) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        let (op, off) = match op {
            LoadOp::Local(offset) => (OpValue::LoadLocal, offset),
            LoadOp::Global(offset) => (OpValue::LoadGlobal, offset),
            LoadOp::Upvalue(offset) => (OpValue::LoadUpvalue, offset),
        };
        code.push(op as u8);
        code.extend(off.to_be_bytes());
        InstrAddr(index as u32)
    }

    pub fn emit_store(&mut self, op: StoreOp) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        let (op, off) = match op {
            StoreOp::Local(offset) => (OpValue::StoreLocal, offset),
            StoreOp::Global(offset) => (OpValue::StoreGlobal, offset),
            StoreOp::Upvalue(offset) => (OpValue::StoreUpvalue, offset),
        };
        code.push(op as u8);
        code.extend(off.to_be_bytes());
        InstrAddr(index as u32)
    }

    pub fn emit_print(&mut self) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        code.push(OpValue::Print as u8);
        InstrAddr(index as u32)
    }

    pub fn emit_constant(&mut self, kind: ConstKind) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        code.push(OpValue::Const as u8);
        match kind {
            ConstKind::Nil => code.push((ConstKindValue::Nil as u8) << 6),
            ConstKind::Bool(b) => {
                let byte = (ConstKindValue::Bool as u8) << 6;
                code.push(byte | if b { 1 } else { 0 });
            }
            ConstKind::Number(address) => {
                code.push((ConstKindValue::Number as u8) << 6);
                code.extend(address.0.to_be_bytes());
            }
            ConstKind::String(address) => {
                code.push((ConstKindValue::String as u8) << 6);
                code.extend(address.0.to_be_bytes());
            }
        }
        InstrAddr(index as u32)
    }

    pub fn emit_jump(&mut self, jump: JumpKind) -> (InstrAddr, Patch) {
        let code = &mut self.current_mut().code;
        let start = code.len();
        let kind = match jump {
            JumpKind::Unconditional => OpValue::Jump,
            JumpKind::OnFalse => OpValue::JumpIfFalse,
            JumpKind::OnTrue => OpValue::JumpIfTrue,
        };
        code.push(kind as u8);
        code.extend(u32::MAX.to_be_bytes());

        (InstrAddr(start as u32), Patch(start as u32))
    }

    pub fn emit_unary(&mut self, unary_op: UnaryOp) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        let kind = match unary_op {
            UnaryOp::Not => OpValue::Not,
            UnaryOp::Negate => OpValue::Negate,
        };
        code.push(kind as u8);
        InstrAddr(index as u32)
    }

    pub fn emit_binary(&mut self, bin_op: BinOp) -> InstrAddr {
        let code = &mut self.current_mut().code;
        let index = code.len();
        let op = match bin_op {
            BinOp::Add => OpValue::Add,
            BinOp::Sub => OpValue::Sub,
            BinOp::Mul => OpValue::Mul,
            BinOp::Div => OpValue::Div,
            BinOp::Equal => OpValue::Equal,
            BinOp::NotEqual => OpValue::NotEqual,
            BinOp::Less => OpValue::Less,
            BinOp::LessEq => OpValue::LessEq,
            BinOp::Greater => OpValue::Greater,
            BinOp::GreaterEq => OpValue::GreaterEq,
        };
        code.push(op as u8);
        InstrAddr(index as u32)
    }

    pub fn emit_call(&mut self, loc: Loc) -> Option<InstrAddr> {
        let pos = self.chunks_map.iter().position(|v| v.0 == loc)?;
        let addr = self.chunks_map[pos].1;
        let chunk = self.current_mut();

        let index = chunk.code.len();
        match addr {
            Some(addr) => chunk.code.extend(addr.to_be_bytes()),
            None => {
                chunk.reloc.push(index as u32);
                chunk.code.extend(pos.to_be_bytes());
            }
        };

        Some(InstrAddr(index as u32))
    }

    pub fn patch_jump(&mut self, patch: Patch, dest: JumpDest) {
        let code = &mut self.current_mut().code;
        let idx = patch.0 as usize;
        let offset = 1 + size_of::<u32>();
        let new = match dest {
            JumpDest::Current => code.len() as i32 - idx as i32 - offset as i32,
            JumpDest::Addr(addr) => addr.0 as i32 - idx as i32 - offset as i32,
        };
        let slice = &mut code[idx + 1..idx + offset];
        slice.copy_from_slice(&new.to_be_bytes());
    }

    pub fn current_address(&self) -> InstrAddr {
        InstrAddr(self.current().code.len() as u32)
    }

    fn current(&self) -> &Chunk {
        self.chunks_active.last().unwrap()
    }

    fn current_mut(&mut self) -> &mut Chunk {
        self.chunks_active.last_mut().unwrap()
    }

    fn is_global_chunk(&self) -> bool {
        self.chunks_active.len() == 1
    }
}

impl<'a> BytecodeReader<'a> {
    pub fn index(&self) -> usize {
        self.index
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn jump(&mut self, offset: InstrOff) {
        self.index = self.index.wrapping_add_signed(offset.0 as isize);
    }

    pub fn read_constant_num(&self, addr: InstrAddr) -> f64 {
        let index = addr.0 as usize;
        let slice = &self.constant[index..];

        let mut num = [0; 8];
        num.copy_from_slice(&slice[..8]);
        f64::from_be_bytes(num)
    }

    pub fn read_constant_string(&self, addr: InstrAddr) -> Result<&'a str, BytecodeError> {
        let index = addr.0 as usize;
        let slice = &self.constant[index..];
        let size = slice
            .iter()
            .position(|v| *v == b'\0')
            .ok_or(BytecodeError::UnerminatedString(addr.0))?;
        str::from_utf8(&slice[..size]).map_err(|e| BytecodeError::StringError(addr.0, e))
    }

    fn advance_one(&mut self) -> u8 {
        let byte = self.code[self.index];
        self.index += 1;
        byte
    }

    fn advance<const N: usize>(&mut self) -> [u8; N] {
        let mut num = [0; N];
        num.copy_from_slice(&self.code[self.index..self.index + N]);
        self.index += N;
        num
    }

    fn advance_slice(&mut self, size: usize) -> &'a [u8] {
        let slice = &self.code[self.index..self.index + size];
        self.index += size;
        slice
    }
}

impl<'a> IntoIterator for &'a Bytecode {
    type Item = <BytecodeReader<'a> as Iterator>::Item;
    type IntoIter = BytecodeReader<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            constant: &self.constant,
            code: &self.code,
            index: 0,
        }
    }
}

impl<'a> Iterator for BytecodeReader<'a> {
    type Item = (InstrAddr, Result<Op<'a>, BytecodeError>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.code.len() {
            return None;
        }

        let index = self.index;
        let byte = self
            .advance_one()
            .try_into()
            .map_err(|_| BytecodeError::InvalidOp(index as u32));

        let op = byte.and_then(|op| match op {
            OpValue::Pop => {
                let amount = u32::from_be_bytes(self.advance());
                Ok(Op::Pop(amount))
            }
            OpValue::Upvalue => Ok(Op::Upvalue),
            OpValue::LoadLocal => {
                let offset = self.advance().into();
                Ok(Op::Load(LoadOp::Local(offset)))
            }
            OpValue::LoadGlobal => {
                let offset = self.advance().into();
                Ok(Op::Load(LoadOp::Global(offset)))
            }
            OpValue::LoadUpvalue => {
                let offset = self.advance().into();
                Ok(Op::Load(LoadOp::Upvalue(offset)))
            }
            OpValue::StoreLocal => {
                let offset = self.advance().into();
                Ok(Op::Store(StoreOp::Local(offset)))
            }
            OpValue::StoreGlobal => {
                let offset = self.advance().into();
                Ok(Op::Store(StoreOp::Global(offset)))
            }
            OpValue::StoreUpvalue => {
                let offset = self.advance().into();
                Ok(Op::Store(StoreOp::Upvalue(offset)))
            }
            OpValue::Const => {
                let byte = self.advance_one();
                let kind = (byte >> 6)
                    .try_into()
                    .map_err(|_| BytecodeError::InvalidOp(index as u32))?;
                let val = match kind {
                    ConstKindValue::Nil => Constant::Nil,
                    ConstKindValue::Bool => Constant::Bool(byte & 1 == 1),
                    ConstKindValue::Number => {
                        let addr = self.advance().into();
                        Constant::Number(self.read_constant_num(addr))
                    }
                    ConstKindValue::String => {
                        let addr = self.advance().into();
                        Constant::String(self.read_constant_string(addr)?)
                    }
                };
                Ok(Op::Const(val))
            }
            OpValue::Jump => {
                let addr = self.advance().into();
                Ok(Op::Jump(JumpOp::Unconditional(addr)))
            }
            OpValue::JumpIfFalse => {
                let addr = self.advance().into();
                Ok(Op::Jump(JumpOp::OnFalse(addr)))
            }
            OpValue::JumpIfTrue => {
                let addr = self.advance().into();
                Ok(Op::Jump(JumpOp::OnTrue(addr)))
            }
            OpValue::Add => Ok(Op::Binary(BinOp::Add)),
            OpValue::Sub => Ok(Op::Binary(BinOp::Sub)),
            OpValue::Mul => Ok(Op::Binary(BinOp::Mul)),
            OpValue::Div => Ok(Op::Binary(BinOp::Div)),
            OpValue::Equal => Ok(Op::Binary(BinOp::Equal)),
            OpValue::NotEqual => Ok(Op::Binary(BinOp::NotEqual)),
            OpValue::Less => Ok(Op::Binary(BinOp::Less)),
            OpValue::LessEq => Ok(Op::Binary(BinOp::LessEq)),
            OpValue::Greater => Ok(Op::Binary(BinOp::Greater)),
            OpValue::GreaterEq => Ok(Op::Binary(BinOp::GreaterEq)),
            OpValue::Not => Ok(Op::Unary(UnaryOp::Not)),
            OpValue::Negate => Ok(Op::Unary(UnaryOp::Negate)),
            OpValue::Print => Ok(Op::Print),
        });

        Some((InstrAddr(index as u32), op))
    }
}

impl Display for DisplayedBytecode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reader = self.reader.clone();
        for (addr, op) in reader.into_iter() {
            write!(f, "{:<010x} ", addr.0)?;
            match op {
                Err(_) => writeln!(f, "ERROR"),
                Ok(op) => match op {
                    Op::Pop(v) => writeln!(f, "{:<16} {:010x}", "POP", v),
                    Op::Upvalue => writeln!(f, "{:<16}", "UPVALUE"),
                    Op::Load(load_op) => match load_op {
                        LoadOp::Local(o) => writeln!(f, "{:<16} {:010x}", "LOAD_LOCAL", o.0),
                        LoadOp::Global(o) => writeln!(f, "{:<16} {:010x}", "LOAD_GLOBAL", o.0),
                        LoadOp::Upvalue(o) => writeln!(f, "{:<16} {:010x}", "LOAD_UPVALUE", o.0),
                    },
                    Op::Store(store_op) => match store_op {
                        StoreOp::Local(o) => writeln!(f, "{:<16} {:010x}", "STORE_LOCAL", o.0),
                        StoreOp::Global(o) => writeln!(f, "{:<16} {:010x}", "STORE_GLOBAL", o.0),
                        StoreOp::Upvalue(o) => writeln!(f, "{:<16} {:010x}", "STORE_UPVALUE", o.0),
                    },
                    Op::Const(constant) => {
                        write!(f, "{:<16}", "CONST")?;
                        match constant {
                            Constant::Nil => writeln!(f, " {:<10}", "nil"),
                            Constant::Bool(b) => writeln!(f, " {b:<10}"),
                            Constant::Number(n) => writeln!(f, "'{n:<10}"),
                            Constant::String(s) => writeln!(f, " {s:?}"),
                        }
                    }
                    Op::Jump(jump_op) => match jump_op {
                        JumpOp::Unconditional(a) => writeln!(f, "{:<16} {:>010x}", "JMP", a.0),
                        JumpOp::OnFalse(a) => writeln!(f, "{:<16} {:>010x}", "JMP_FALSE", a.0),
                        JumpOp::OnTrue(a) => writeln!(f, "{:<16} {:>010x}", "JMP_TRUE", a.0),
                    },
                    Op::Unary(unary_op) => match unary_op {
                        UnaryOp::Not => writeln!(f, "{:<16}", "NOT"),
                        UnaryOp::Negate => writeln!(f, "{:<16}", "NEGATE"),
                    },
                    Op::Binary(bin_op) => match bin_op {
                        BinOp::Add => writeln!(f, "{:<16}", "ADD",),
                        BinOp::Sub => writeln!(f, "{:<16}", "SUB",),
                        BinOp::Mul => writeln!(f, "{:<16}", "MUL",),
                        BinOp::Div => writeln!(f, "{:<16}", "DIV",),
                        BinOp::Equal => writeln!(f, "{:<16}", "EQ",),
                        BinOp::NotEqual => writeln!(f, "{:<16}", "NEQ",),
                        BinOp::Less => writeln!(f, "{:<16}", "LT",),
                        BinOp::LessEq => writeln!(f, "{:<16}", "LE",),
                        BinOp::Greater => writeln!(f, "{:<16}", "GT",),
                        BinOp::GreaterEq => writeln!(f, "{:<16}", "GE",),
                    },
                    Op::Print => writeln!(f, "{:<16}", "PRINT"),
                },
            }?
        }
        Ok(())
    }
}
