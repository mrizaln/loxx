use std::fmt::Display;

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::memory::{Heap, HeapId, HeapValue};
use crate::metadata::{ClassId, FuncId};

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(HeapId),
    Class(ClassId),
    Instance(HeapId),
    Function(FuncId, Option<HeapId>), // HeapId must be instance
}

#[derive(Debug)]
pub enum Constant<'a> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'a str),
}

#[derive(Debug)]
pub struct Instance {
    id: usize,
    class: ClassId,
    fields: FxHashMap<String, Value>,
}

#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Invalid unary operation '{0}' on '{1}'")]
    Unary(&'static str, &'static str),

    #[error("Invalid binary operation '{0}' between '{1}' and '{2}'")]
    Binary(&'static str, &'static str, &'static str),
}

pub struct DisplayedValue<'a, 'b> {
    value: &'a Value,
    heap: &'b Heap,
}

impl Value {
    pub fn name(&self) -> &'static str {
        match self {
            Value::Nil => "<nil>",
            Value::Bool(_) => "<bool>",
            Value::Number(_) => "<number>",
            Value::String(_) => "<string>",
            Value::Class(_) => "<class>",
            Value::Instance(_) => "<instance>",
            Value::Function(_, _) => "<function>",
        }
    }

    pub fn display<'a, 'b>(&'a self, heap: &'b Heap) -> DisplayedValue<'a, 'b> {
        DisplayedValue { value: self, heap }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn not(&mut self) {
        *self = Value::Bool(!self.truthy());
    }

    pub fn negate(&mut self) -> Result<(), ValueError> {
        if let Value::Number(n) = self {
            *n = -*n;
            return Ok(());
        };
        Err(ValueError::Unary("-", self.name()))
    }

    pub fn add(self, other: Self, heap: &mut Heap) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 + num2)),
            (Value::String(str1), Value::String(str2)) => {
                let (str1, str2) = heap.get_two(str1, str2).unwrap();
                let mut str1 = str1.as_string().clone();
                let str2 = str2.as_string().as_str();
                str1.push_str(str2);
                Ok(Value::String(heap.construct(HeapValue::String(str1))))
            }
            (lhs, rhs) => Err(ValueError::Binary("+", lhs.name(), rhs.name())),
        }
    }

    pub fn sub(self, other: Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 - num2)),
            (lhs, rhs) => Err(ValueError::Binary("-", lhs.name(), rhs.name())),
        }
    }

    pub fn mul(self, other: Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 * num2)),
            (lhs, rhs) => Err(ValueError::Binary("*", lhs.name(), rhs.name())),
        }
    }

    pub fn div(self, other: Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(num1 / num2)),
            (lhs, rhs) => Err(ValueError::Binary("/", lhs.name(), rhs.name())),
        }
    }

    pub fn eq(&self, other: &Self, heap: &Heap) -> Value {
        Value::Bool(self.is_equal(other, heap))
    }

    pub fn neq(&self, other: &Self, heap: &Heap) -> Value {
        Value::Bool(!self.is_equal(other, heap))
    }

    pub fn gt(&self, other: &Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 > *num2)),
            (lhs, rhs) => Err(ValueError::Binary(">", lhs.name(), rhs.name())),
        }
    }

    pub fn ge(&self, other: &Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 >= *num2)),
            (lhs, rhs) => Err(ValueError::Binary(">=", lhs.name(), rhs.name())),
        }
    }

    pub fn lt(&self, other: &Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 < *num2)),
            (lhs, rhs) => Err(ValueError::Binary("<", lhs.name(), rhs.name())),
        }
    }

    pub fn le(&self, other: &Self) -> Result<Value, ValueError> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 <= *num2)),
            (lhs, rhs) => Err(ValueError::Binary("<=", lhs.name(), rhs.name())),
        }
    }

    fn is_equal(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Number(num1), Value::Number(num2)) => num1 == num2,
            (Value::Class(cls1), Value::Class(cls2)) => cls1 == cls2,
            (Value::Instance(ins1), Value::Instance(ins2)) => ins1 == ins2,
            (Value::Function(func1, ins1), Value::Function(func2, ins2)) => {
                func1 == func2 && ins1 == ins2
            }
            (Value::String(str1), Value::String(str2)) => {
                let (str1, str2) = heap.get_two(*str1, *str2).unwrap();
                str1.as_string() == str2.as_string()
            }
            _ => false,
        }
    }
}

impl Display for DisplayedValue<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { value, heap } = self;
        match value {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(num) => write!(f, "{num}"),
            Value::String(id) => {
                let str = heap.get(*id).ok_or(std::fmt::Error)?.as_string();
                write!(f, "{str}")
            }
            Value::Class(id) => todo!(),
            Value::Instance(id) => todo!(),
            Value::Function(id, inst) => todo!(),
        }
    }
}
