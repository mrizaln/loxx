use std::fmt::{Debug, Display};

use super::{
    function::{Function, NativeFunction},
    object::Object,
};

// TODO: use Rc for heavy object (String and LoxObject)
#[derive(Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
    Object(Object),
}

impl Value {
    /// follows Ruby's simple rule: `false` and `nil` are falsy, everything else truthy
    pub fn truthiness(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn not(&self) -> Option<Value> {
        Some(Value::Bool(!self.truthiness()))
    }

    pub fn minus(&self) -> Option<Value> {
        match self {
            Value::Number(num) => Some(Value::Number(-num)),
            _ => None,
        }
    }

    pub fn add(self, other: Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Number(num1 + num2)),
            (Value::String(str1), Value::String(str2)) => Some(Value::String(str1 + &str2)),
            _ => None,
        }
    }

    pub fn sub(self, other: Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Number(num1 - num2)),
            _ => None,
        }
    }

    pub fn mul(self, other: Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Number(num1 * num2)),
            _ => None,
        }
    }

    pub fn div(self, other: Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Number(num1 / num2)),
            _ => None,
        }
    }

    pub fn eq(&self, other: &Self) -> Option<Value> {
        match (self, other) {
            (Value::Nil, Value::Nil) => Some(Value::Bool(true)),
            (Value::Bool(b1), Value::Bool(b2)) => Some(Value::Bool(b1 == b2)),
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Bool(num1 == num2)),
            (Value::String(str1), Value::String(str2)) => Some(Value::Bool(str1 == str2)),
            (Value::Object(_), Value::Object(_)) => unimplemented!(),
            _ => Some(Value::Bool(false)),
        }
    }

    pub fn neq(&self, other: &Self) -> Option<Value> {
        self.eq(other)?.not()
    }

    pub fn gt(&self, other: &Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Bool(*num1 > *num2)),
            _ => None,
        }
    }

    pub fn ge(&self, other: &Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Bool(*num1 >= *num2)),
            _ => None,
        }
    }

    pub fn lt(&self, other: &Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Bool(*num1 < *num2)),
            _ => None,
        }
    }

    pub fn le(&self, other: &Self) -> Option<Value> {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Some(Value::Bool(*num1 <= *num2)),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Value::Nil => "<nil>",
            Value::Bool(_) => "<bool>",
            Value::Number(_) => "<number>",
            Value::String(_) => "<string>",
            Value::Object(_) => "<object>",
            Value::Function(_) => "<function>",
            Value::NativeFunction(_) => "<function>",
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::Number(num) => write!(f, "Number({num})"),
            Value::String(str) => write!(f, "String({str})"),
            Value::Object(_) => write!(f, "Object(<dummy>)"),
            Value::Function(Function { name, .. }) => write!(f, "Function({name})"),
            Value::NativeFunction(NativeFunction { name, .. }) => write!(f, "Function({name})"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(num) => write!(f, "{num}"),
            Value::String(str) => write!(f, "{str}"),
            Value::Object(_) => write!(f, "<object>"),
            Value::Function(Function { name, .. }) => write!(f, "<fun {name}>"),
            Value::NativeFunction(NativeFunction { name, .. }) => write!(f, "<fun {name}>"),
        }
    }
}
