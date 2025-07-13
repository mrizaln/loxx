use core::str;
use std::fmt::Display;
use std::mem::transmute;

use crate::value::Constant;

pub struct Address(u32);

pub struct Offset(pub u16);

pub enum BinOp {
    Add(Offset, Offset),
    Sub(Offset, Offset),
    Mul(Offset, Offset),
    Div(Offset, Offset),
    Equal(Offset, Offset),
    NotEqual(Offset, Offset),
    Less(Offset, Offset),
    LessEq(Offset, Offset),
    Greater(Offset, Offset),
    GreaterEq(Offset, Offset),
}

pub enum UnaryOp {
    Not(Offset),
    Negate(Offset),
}

pub enum JumpOp {
    Unconditional(Address),
    OnFalse(Address),
    OnTrue(Address),
}

pub enum Op<'a> {
    Pop,
    Const(Constant<'a>),
    Jump(JumpOp),
    Unary(UnaryOp),
    Binary(BinOp),
}

#[repr(u8)]
#[derive(Debug)]
enum ConstType {
    Nil,
    Bool,
    Number,
    String,
}

#[repr(u8)]
enum OpValue {
    Pop,
    Const,       // 1 byte type + (0 byte: nil, 0 byte: bool | 8 byte: number | N byte: string)
    Jump,        // 4 byte: destination offset in code
    JumpIfFalse, // 4 byte: destination offset in code
    JumpIfTrue,  // 4 byte: destination offset in code
    Add,         // 2 byte left op 2 byte right op
    Sub,         // 2 byte left op 2 byte right op
    Mul,         // 2 byte left op 2 byte right op
    Div,         // 2 byte left op 2 byte right op
    Equal,       // 2 byte left op 2 byte right op
    NotEqual,    // 2 byte left op 2 byte right op
    Less,        // 2 byte left op 2 byte right op
    LessEq,      // 2 byte left op 2 byte right op
    Greater,     // 2 byte left op 2 byte right op
    GreaterEq,   // 2 byte left op 2 byte right op
    Not,         // 2 byte op
    Negate,      // 2 byte op
}

#[derive(Default)]
pub struct Bytecode {
    constant: Vec<u8>,
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
        let highest = OpValue::Negate as u8;

        if value <= highest && value >= lowest {
            unsafe { Ok(transmute::<u8, OpValue>(value)) }
        } else {
            Err(())
        }
    }
}

impl TryFrom<u8> for ConstType {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let lowest = ConstType::Nil as u8;
        let highest = ConstType::String as u8;

        if value <= highest && value >= lowest {
            unsafe { Ok(transmute::<u8, ConstType>(value)) }
        } else {
            Err(())
        }
    }
}

impl Bytecode {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create_constant(&mut self, constant: Constant) -> Address {
        let addr = self.constant.len();

        // only 2 msb bit is used for the type detection
        match constant {
            Constant::Nil => {
                let byte = (ConstType::Nil as u8) << 6;
                self.constant.push(byte);
            }
            Constant::Bool(b) => {
                let byte = (ConstType::Bool as u8) << 6;
                self.constant.push(byte | if b { 1 } else { 0 });
            }
            Constant::Number(n) => {
                let byte = (ConstType::Number as u8) << 6;
                self.constant.push(byte);
                let num_bytes = n.to_be_bytes();
                self.constant.extend(num_bytes);
            }
            Constant::String(s) => {
                let byte = (ConstType::String as u8) << 6;
                self.constant.push(byte);
                self.constant.extend(s.as_bytes());
                self.constant.push(b'\0');
            }
        }

        Address(addr as u32)
    }

    pub fn emit_pop(&mut self) {
        self.code.push(OpValue::Pop as u8);
    }

    pub fn emit_constant(&mut self, addr: Address) {
        self.code.push(OpValue::Const as u8);
        self.code.extend(addr.0.to_be_bytes());
    }

    pub fn emit_jump(&mut self, jump: JumpOp) {
        let (kind, addr) = match jump {
            JumpOp::Unconditional(addr) => (OpValue::Jump, addr.0),
            JumpOp::OnFalse(addr) => (OpValue::JumpIfFalse, addr.0),
            JumpOp::OnTrue(addr) => (OpValue::JumpIfTrue, addr.0),
        };
        self.code.push(kind as u8);
        self.code.extend(addr.to_be_bytes());
    }

    pub fn emit_unary(&mut self, unary_op: UnaryOp) {
        let (kind, opd) = match unary_op {
            UnaryOp::Not(opd) => (OpValue::Not, opd),
            UnaryOp::Negate(opd) => (OpValue::Negate, opd),
        };
        self.code.push(kind as u8);
        self.code.extend(opd.0.to_be_bytes());
    }

    pub fn emit_binary(&mut self, bin_op: BinOp) {
        let (kind, lhs, rhs) = match bin_op {
            BinOp::Add(lhs, rhs) => (OpValue::Add, lhs, rhs),
            BinOp::Sub(lhs, rhs) => (OpValue::Sub, lhs, rhs),
            BinOp::Mul(lhs, rhs) => (OpValue::Mul, lhs, rhs),
            BinOp::Div(lhs, rhs) => (OpValue::Div, lhs, rhs),
            BinOp::Equal(lhs, rhs) => (OpValue::Equal, lhs, rhs),
            BinOp::NotEqual(lhs, rhs) => (OpValue::NotEqual, lhs, rhs),
            BinOp::Less(lhs, rhs) => (OpValue::Less, lhs, rhs),
            BinOp::LessEq(lhs, rhs) => (OpValue::LessEq, lhs, rhs),
            BinOp::Greater(lhs, rhs) => (OpValue::Greater, lhs, rhs),
            BinOp::GreaterEq(lhs, rhs) => (OpValue::GreaterEq, lhs, rhs),
        };
        self.code.push(kind as u8);
        self.code.extend(lhs.0.to_be_bytes());
        self.code.extend(rhs.0.to_be_bytes());
    }

    pub fn display(&self) -> DisplayedBytecode {
        DisplayedBytecode {
            reader: self.into_iter(),
        }
    }
}

impl<'a> BytecodeReader<'a> {
    pub fn jump(&mut self, address: Address) {
        self.index = address.0 as usize;
    }

    pub fn read_constant(&self, addr: Address) -> Result<Constant<'a>, ()> {
        let index = addr.0 as usize;
        let slice = &self.constant[index..];

        let byte = slice[0];
        let kind = (byte >> 6).try_into()?;
        match kind {
            ConstType::Nil => Ok(Constant::Nil),
            ConstType::Bool => Ok(Constant::Bool((byte & 1) == 1)),
            ConstType::Number => {
                let mut num = [0; 8];
                num.copy_from_slice(&slice[1..9]);
                let num = f64::from_be_bytes(num);
                Ok(Constant::Number(num))
            }
            ConstType::String => {
                let slice = &slice[1..];
                let size = slice.iter().position(|v| *v == b'\0').unwrap();
                let string = str::from_utf8(&slice[..size]).unwrap();
                Ok(Constant::String(string))
            }
        }
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
    type Item = Result<Op<'a>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.code.len() {
            return None;
        }

        let byte = self.advance_one().try_into();

        Some(byte.and_then(|op| match op {
            OpValue::Pop => Ok(Op::Pop),
            OpValue::Const => {
                let addr = Address(u32::from_be_bytes(self.advance()));
                Ok(Op::Const(self.read_constant(addr)?))
            }
            OpValue::Jump => {
                let addr = u32::from_be_bytes(self.advance());
                Ok(Op::Jump(JumpOp::Unconditional(Address(addr))))
            }
            OpValue::JumpIfFalse => {
                let addr = u32::from_be_bytes(self.advance());
                Ok(Op::Jump(JumpOp::OnFalse(Address(addr))))
            }
            OpValue::JumpIfTrue => {
                let addr = u32::from_be_bytes(self.advance());
                Ok(Op::Jump(JumpOp::OnTrue(Address(addr))))
            }
            OpValue::Add => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Add(Offset(lhs), Offset(rhs))))
            }
            OpValue::Sub => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Sub(Offset(lhs), Offset(rhs))))
            }
            OpValue::Mul => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Mul(Offset(lhs), Offset(rhs))))
            }
            OpValue::Div => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Div(Offset(lhs), Offset(rhs))))
            }
            OpValue::Equal => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Equal(Offset(lhs), Offset(rhs))))
            }
            OpValue::NotEqual => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::NotEqual(Offset(lhs), Offset(rhs))))
            }
            OpValue::Less => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Less(Offset(lhs), Offset(rhs))))
            }
            OpValue::LessEq => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::LessEq(Offset(lhs), Offset(rhs))))
            }
            OpValue::Greater => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::Greater(Offset(lhs), Offset(rhs))))
            }
            OpValue::GreaterEq => {
                let lhs = u16::from_be_bytes(self.advance());
                let rhs = u16::from_be_bytes(self.advance());
                Ok(Op::Binary(BinOp::GreaterEq(Offset(lhs), Offset(rhs))))
            }
            OpValue::Not => {
                let opnd = u16::from_be_bytes(self.advance());
                Ok(Op::Unary(UnaryOp::Not(Offset(opnd))))
            }
            OpValue::Negate => {
                let opnd = u16::from_be_bytes(self.advance());
                Ok(Op::Unary(UnaryOp::Negate(Offset(opnd))))
            }
        }))
    }
}

impl Display for DisplayedBytecode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reader = self.reader.clone();
        for op in reader.into_iter() {
            match op {
                Err(_) => writeln!(f, "ERROR"),
                Ok(op) => match op {
                    Op::Pop => writeln!(f, "POP"),
                    Op::Const(constant) => {
                        write!(f, "{:<16}", "CONSTANT")?;
                        match constant {
                            Constant::Nil => writeln!(f, " {:<10}", "nil"),
                            Constant::Bool(b) => writeln!(f, " {b:<10}"),
                            Constant::Number(n) => writeln!(f, " {n:<10e}"),
                            Constant::String(s) => writeln!(f, " {s:?}"),
                        }
                    }
                    Op::Jump(jump_op) => match jump_op {
                        JumpOp::Unconditional(address) => {
                            writeln!(f, "{:<16} {:>010x}", "JUMP", address.0)
                        }
                        JumpOp::OnFalse(address) => {
                            writeln!(f, "{:<16} {:>010x}", "JUMP_IF_FALSE", address.0)
                        }
                        JumpOp::OnTrue(address) => {
                            writeln!(f, "{:<16} {:>010x}", "JUMP_IF_TRUE", address.0)
                        }
                    },
                    Op::Unary(unary_op) => match unary_op {
                        UnaryOp::Not(offset) => writeln!(f, "{:<16} {:>010x}", "NOT", offset.0),
                        UnaryOp::Negate(offset) => {
                            writeln!(f, "{:<16} {:>010x}", "NEGATE", offset.0)
                        }
                    },
                    Op::Binary(bin_op) => match bin_op {
                        BinOp::Add(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "ADD", lhs.0, rhs.0)
                        }
                        BinOp::Sub(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "SUB", lhs.0, rhs.0)
                        }
                        BinOp::Mul(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "MUL", lhs.0, rhs.0)
                        }
                        BinOp::Div(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "DIV", lhs.0, rhs.0)
                        }
                        BinOp::Equal(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "EQ", lhs.0, rhs.0)
                        }
                        BinOp::NotEqual(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "NEQ", lhs.0, rhs.0)
                        }
                        BinOp::Less(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "LT", lhs.0, rhs.0)
                        }
                        BinOp::LessEq(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "LE", lhs.0, rhs.0)
                        }
                        BinOp::Greater(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "GT", lhs.0, rhs.0)
                        }
                        BinOp::GreaterEq(lhs, rhs) => {
                            writeln!(f, "{:<16} {:>010x} {:>010x}", "GE", lhs.0, rhs.0)
                        }
                    },
                },
            }?
        }
        Ok(())
    }
}
