use std::fmt::{Debug, Display};
use std::ops::Deref;
use std::rc::Rc;

use super::{
    function::{Function, NativeFunction},
    object::Object,
};

// TODO: use Rc for heavy object (String and LoxObject)
#[derive(PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Function(Function),
    NativeFunction(NativeFunction),
    String(Rc<String>),
    Object(Rc<Object>),
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
            (Value::String(str1), Value::String(str2)) => {
                let mut new_str = str1.deref().clone();
                new_str.push_str(str2.deref().as_str());
                Some(Value::String(Rc::new(new_str)))
            }
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

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Nil => Value::Nil,
            Value::Bool(b) => Value::Bool(*b),
            Value::Number(num) => Value::Number(*num),
            Value::Function(fun) => Value::Function(fun.clone()),
            Value::NativeFunction(fun) => Value::NativeFunction(fun.clone()),
            Value::String(str) => Value::String(Rc::clone(str)),
            Value::Object(obj) => Value::Object(Rc::clone(obj)),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::Number(num) => write!(f, "Number({num})"),
            Value::String(str) => write!(f, "String({})", str.deref()),
            Value::Object(_) => write!(f, "Object(<dummy>)"),
            Value::Function(Function { name, .. }) => {
                write!(f, "Function(spur|{}|)", name.into_inner())
            }
            Value::NativeFunction(NativeFunction { name, .. }) => {
                write!(f, "Function(spur|{}|)", name.into_inner())
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(num) => write!(f, "{num}"),
            Value::String(str) => write!(f, "{}", str.deref()),
            Value::Object(_) => write!(f, "<object>"),
            Value::Function(Function { name, .. }) => {
                write!(f, "<fun spur|{}|>", name.into_inner())
            }
            Value::NativeFunction(NativeFunction { name, .. }) => {
                write!(f, "<fun spur|{}|>", name.into_inner())
            }
        }
    }
}
