use std::ops::Not;

use boolinator::Boolinator;
use thiserror::Error;

use crate::bytecode::{Address, BinOp, Bytecode, BytecodeError, JumpOp, LoadOp, Op, UnaryOp};
use crate::memory::{Heap, HeapValue, MemoryError, Stack};
use crate::value::{Constant, Value, ValueError};

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum LocalKind {
    Ref,
    Temp,
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("RuntimeError: {0}")]
    BytecodeError(#[from] BytecodeError),

    #[error("RuntimeError: {0}")]
    InvalidValueOp(#[from] ValueError),

    #[error("RuntimeError: {0}")]
    MemoryError(#[from] MemoryError),
}

pub struct Vm {
    stack: Stack,
    heap: Heap,
    stack_offset: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            heap: Heap::new(),
            stack_offset: 0,
        }
    }

    pub fn interpret(&mut self, program: &Bytecode) -> Result<(), RuntimeError> {
        let mut reader = program.into_iter();

        while let Some((_, op)) = reader.next() {
            match op? {
                Op::Pop => self.stack.pop().map_or((), |_| {}),
                Op::Upvalue => todo!(),
                Op::Load(load_op) => self.interpret_load(load_op)?,
                Op::Const(constant) => self.interpret_constant(constant),
                Op::Jump(jump_op) => _ = self.interpret_jump(jump_op)?.map(|a| reader.jump(a)),
                Op::Unary(unary_op) => self.interpret_unary(unary_op)?,
                Op::Binary(bin_op) => self.interpret_binary(bin_op)?,
                Op::Print => self.interpret_print()?,
            };
        }

        Ok(())
    }

    fn interpret_load(&mut self, op: LoadOp) -> Result<(), RuntimeError> {
        match op {
            LoadOp::Local(off) => self.stack.dup(off.0 as usize + self.stack_offset - 1)?,
            LoadOp::Global(off) => todo!(),
            LoadOp::Upvalue(off) => todo!(),
        };
        Ok(())
    }

    fn interpret_constant(&mut self, constant: Constant) {
        match constant {
            Constant::Nil => _ = self.stack.push(Value::Nil),
            Constant::Bool(b) => _ = self.stack.push(Value::Bool(b)),
            Constant::Number(n) => _ = self.stack.push(Value::Number(n)),
            Constant::String(s) => {
                let id = self.heap.construct(HeapValue::String(s.to_owned()));
                self.stack.push(Value::String(id));
            }
        };
    }

    fn interpret_jump(&mut self, jump: JumpOp) -> Result<Option<Address>, RuntimeError> {
        match jump {
            JumpOp::Unconditional(address) => Ok(Some(address)),
            JumpOp::OnFalse(address) => {
                let value = self.stack.top_mut()?;
                Ok(value.truthy().not().as_some(address))
            }
            JumpOp::OnTrue(address) => {
                let value = self.stack.top_mut()?;
                Ok(value.truthy().as_some(address))
            }
        }
    }

    fn interpret_unary(&mut self, op: UnaryOp) -> Result<(), RuntimeError> {
        let top = self.stack.top_mut()?;
        match op {
            UnaryOp::Not => top.not(),
            UnaryOp::Negate => top.negate()?,
        };
        Ok(())
    }

    fn interpret_binary(&mut self, op: BinOp) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop()?;
        let lhs = self.stack.pop()?;

        let value = match op {
            BinOp::Add => lhs.add(rhs, &mut self.heap)?,
            BinOp::Sub => lhs.sub(rhs)?,
            BinOp::Mul => lhs.mul(rhs)?,
            BinOp::Div => lhs.div(rhs)?,
            BinOp::Equal => lhs.eq(&rhs, &self.heap),
            BinOp::NotEqual => lhs.neq(&rhs, &self.heap),
            BinOp::Less => lhs.lt(&rhs)?,
            BinOp::LessEq => lhs.le(&rhs)?,
            BinOp::Greater => lhs.gt(&rhs)?,
            BinOp::GreaterEq => lhs.ge(&rhs)?,
        };

        self.stack.push(value);
        Ok(())
    }

    fn interpret_print(&self) -> Result<(), RuntimeError> {
        let value = self.stack.top()?;
        println!("{}", value.display(&self.heap));
        Ok(())
    }
}
