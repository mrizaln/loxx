use core::str;
use std::fmt::Display;
use std::mem::transmute;
use std::str::Utf8Error;

use loxi::interner::{Interner, Key};
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::value::Constant;

#[derive(Copy, Clone, Default, Debug)]
pub struct Address(pub u32);

#[derive(Copy, Clone, Default, Debug)]
pub struct Offset(pub u32);

#[derive(Copy, Clone, Default, Debug)]
pub struct Patch(pub u32);

#[derive(Debug)]
pub enum LoadOp {
    Local(Offset),
    Global(Offset),
    Upvalue(Offset),
}

#[derive(Debug)]
pub enum StoreOp {
    Local(Offset),
    Global(Offset),
    Upvalue(Offset),
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
    Unconditional(Address),
    OnFalse(Address),
    OnTrue(Address),
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
    Number(Address),
    String(Address),
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

#[derive(Default)]
pub struct Bytecode {
    constant: Vec<u8>,
    constant_map: FxHashMap<Key, Address>,
    global: Vec<u8>,
    code: Vec<u8>,
}

#[derive(Clone)]
pub struct BytecodeReader<'a> {
    constant: &'a [u8],
    global: &'a [u8],
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

impl Address {
    pub fn to_be_bytes(self) -> [u8; size_of::<u32>()] {
        self.0.to_be_bytes()
    }
}

impl From<[u8; size_of::<u32>()]> for Address {
    fn from(value: [u8; size_of::<u32>()]) -> Self {
        let num = u32::from_be_bytes(value);
        Self(num)
    }
}

impl Offset {
    pub fn to_be_bytes(self) -> [u8; size_of::<u32>()] {
        self.0.to_be_bytes()
    }
}

impl From<[u8; size_of::<u32>()]> for Offset {
    fn from(value: [u8; size_of::<u32>()]) -> Self {
        let num = u32::from_be_bytes(value);
        Self(num)
    }
}

impl Bytecode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create_constant_string(&mut self, key: Key, interner: &Interner) -> Address {
        if let Some(addr) = self.constant_map.get(&key) {
            return *addr;
        };

        let str = interner.resolve(key);
        let addr = self.constant.len();
        self.constant.extend(str.as_bytes());
        self.constant.push(b'\0');

        Address(addr as u32)
    }

    pub fn create_consant_num(&mut self, num: f64) -> Address {
        let addr = self.constant.len();
        self.constant.extend(num.to_be_bytes());
        Address(addr as u32)
    }

    pub fn emit_pop(&mut self, amount: u32) -> Address {
        let index = self.code.len();
        self.code.push(OpValue::Pop as u8);
        self.code.extend(amount.to_be_bytes());
        Address(index as u32)
    }

    pub fn emit_upvalue(&mut self) -> Address {
        let index = self.code.len();
        self.code.push(OpValue::Upvalue as u8);
        Address(index as u32)
    }

    pub fn emit_load(&mut self, op: LoadOp) -> Address {
        let index = self.code.len();
        let (op, off) = match op {
            LoadOp::Local(offset) => (OpValue::LoadLocal, offset),
            LoadOp::Global(offset) => (OpValue::LoadGlobal, offset),
            LoadOp::Upvalue(offset) => (OpValue::LoadUpvalue, offset),
        };
        self.code.push(op as u8);
        self.code.extend(off.to_be_bytes());
        Address(index as u32)
    }

    pub fn emit_store(&mut self, op: StoreOp) -> Address {
        let index = self.code.len();
        let (op, off) = match op {
            StoreOp::Local(offset) => (OpValue::StoreLocal, offset),
            StoreOp::Global(offset) => (OpValue::StoreGlobal, offset),
            StoreOp::Upvalue(offset) => (OpValue::StoreUpvalue, offset),
        };
        self.code.push(op as u8);
        self.code.extend(off.to_be_bytes());
        Address(index as u32)
    }

    pub fn emit_print(&mut self) -> Address {
        let index = self.code.len();
        self.code.push(OpValue::Print as u8);
        Address(index as u32)
    }

    pub fn emit_constant(&mut self, kind: ConstKind) -> Address {
        let index = self.code.len();
        self.code.push(OpValue::Const as u8);
        match kind {
            ConstKind::Nil => self.code.push((ConstKindValue::Nil as u8) << 6),
            ConstKind::Bool(b) => {
                let byte = (ConstKindValue::Bool as u8) << 6;
                self.code.push(byte | if b { 1 } else { 0 });
            }
            ConstKind::Number(address) => {
                self.code.push((ConstKindValue::Number as u8) << 6);
                self.code.extend(address.0.to_be_bytes());
            }
            ConstKind::String(address) => {
                self.code.push((ConstKindValue::String as u8) << 6);
                self.code.extend(address.0.to_be_bytes());
            }
        }
        Address(index as u32)
    }

    pub fn emit_jump(&mut self, jump: JumpOp) -> (Address, Patch) {
        let start = self.code.len();
        let (kind, addr) = match jump {
            JumpOp::Unconditional(addr) => (OpValue::Jump, addr.0),
            JumpOp::OnFalse(addr) => (OpValue::JumpIfFalse, addr.0),
            JumpOp::OnTrue(addr) => (OpValue::JumpIfTrue, addr.0),
        };
        self.code.push(kind as u8);

        let index = self.code.len();
        self.code.extend(addr.to_be_bytes());

        (Address(start as u32), Patch(index as u32))
    }

    pub fn emit_unary(&mut self, unary_op: UnaryOp) -> Address {
        let index = self.code.len();
        let kind = match unary_op {
            UnaryOp::Not => OpValue::Not,
            UnaryOp::Negate => OpValue::Negate,
        };
        self.code.push(kind as u8);
        Address(index as u32)
    }

    pub fn emit_binary(&mut self, bin_op: BinOp) -> Address {
        let index = self.code.len();
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
        self.code.push(op as u8);
        Address(index as u32)
    }

    pub fn patch_jump(&mut self, patch: Patch) {
        let current = (self.code.len() as u32).to_be_bytes();
        let index = patch.0 as usize;
        let slice = &mut self.code[index..index + 4];
        slice.copy_from_slice(&current);
    }

    pub fn display(&self) -> DisplayedBytecode {
        DisplayedBytecode {
            reader: self.into_iter(),
        }
    }

    pub fn current_address(&self) -> Address {
        Address(self.code.len() as u32)
    }
}

impl<'a> BytecodeReader<'a> {
    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn jump(&mut self, address: Address) {
        self.index = address.0 as usize;
    }

    pub fn read_constant_num(&self, addr: Address) -> f64 {
        let index = addr.0 as usize;
        let slice = &self.constant[index..];

        let mut num = [0; 8];
        num.copy_from_slice(&slice[..8]);
        f64::from_be_bytes(num)
    }

    pub fn read_constant_string(&self, addr: Address) -> Result<&'a str, BytecodeError> {
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
            global: &self.global,
            code: &self.code,
            index: 0,
        }
    }
}

impl<'a> Iterator for BytecodeReader<'a> {
    type Item = (Address, Result<Op<'a>, BytecodeError>);

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
                Ok(Op::Store(StoreOp::Local(offset)))
            }
            OpValue::StoreUpvalue => {
                let offset = self.advance().into();
                Ok(Op::Store(StoreOp::Local(offset)))
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

        Some((Address(index as u32), op))
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
