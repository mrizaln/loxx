use std::rc::Rc;
use std::{fmt::Display, ops::Deref};

use super::class::{Class, Instance};
use super::function::{Function, Native, UserDefined};
use super::interner::{Interner, Key};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Class(Rc<Class>),
    String(Rc<String>),
    Function(Rc<Function>),
    Instance(Rc<Instance>),

    /// `StringLiteral` is a special case of string, the value is static.
    /// It can only produces real `String` if it was operated on.
    ///
    /// NOTE: There is no distinction between `String` and string literal in Lox, this is just an
    ///       optimization.
    StringLiteral(Key),
}

/// A wrapper for `Value` that can be displayed. This is necessary to "pass" interner as addtional
/// argument to `Display::fmt` method.
pub struct DisplayedValue<'a, 'b> {
    value: &'a Value,
    interner: &'b Interner,
}

pub enum InvalidOp {
    Unary(&'static str),
    Binary(&'static str, &'static str),
}

type OpResult = Result<Value, InvalidOp>;

impl Value {
    pub fn nil() -> Self {
        Value::Nil
    }

    pub fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn number(num: f64) -> Self {
        Value::Number(num)
    }

    pub fn class(class: Class) -> Self {
        Value::Class(Rc::new(class))
    }

    pub fn string(str: String) -> Self {
        Value::String(Rc::new(str))
    }

    pub fn function(func: UserDefined) -> Self {
        Value::Function(Rc::new(Function::UserDefined(func)))
    }

    pub fn native_function(func: Native) -> Self {
        Value::Function(Rc::new(Function::Native(func)))
    }

    pub fn instance(instance: Instance) -> Self {
        Value::Instance(Rc::new(instance))
    }

    pub fn string_literal(key: Key) -> Self {
        Value::StringLiteral(key)
    }

    /// follows Ruby's simple rule: `false` and `nil` are falsy, everything else truthy
    pub fn truthiness(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn not(&self) -> Value {
        Value::Bool(!self.truthiness())
    }

    pub fn minus(&self) -> OpResult {
        match self {
            Value::Number(num) => Ok(Value::number(-num)),
            _ => invalid_unary(self),
        }
    }

    pub fn add(self, other: Self, interner: &Interner) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::number(num1 + num2)),
            (Value::String(str1), Value::String(str2)) => {
                let mut new_str = str1.deref().clone();
                new_str.push_str(str2.deref().as_str());
                Ok(Value::string(new_str))
            }
            (Value::String(str1), Value::StringLiteral(str2)) => {
                let mut new_str = str1.deref().clone();
                let str2 = interner.resolve(str2);
                new_str.push_str(str2);
                Ok(Value::string(new_str))
            }
            (Value::StringLiteral(str1), Value::String(str2)) => {
                let mut new_str = str2.deref().clone();
                let str1 = interner.resolve(str1);
                new_str.insert_str(0, str1);
                Ok(Value::string(new_str))
            }
            (Value::StringLiteral(str1), Value::StringLiteral(str2)) => {
                let mut new_str = String::new();
                let str1 = interner.resolve(str1);
                let str2 = interner.resolve(str2);
                new_str.push_str(str1);
                new_str.push_str(str2);
                Ok(Value::string(new_str))
            }
            (lhs, rhs) => invalid_binary(&lhs, &rhs),
        }
    }

    pub fn sub(self, other: Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::number(num1 - num2)),
            (lhs, rhs) => invalid_binary(&lhs, &rhs),
        }
    }

    pub fn mul(self, other: Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::number(num1 * num2)),
            (lhs, rhs) => invalid_binary(&lhs, &rhs),
        }
    }

    pub fn div(self, other: Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::number(num1 / num2)),
            (lhs, rhs) => invalid_binary(&lhs, &rhs),
        }
    }

    pub fn eq(&self, other: &Self, interner: &Interner) -> Value {
        Value::Bool(self.is_equal(other, interner))
    }

    pub fn neq(&self, other: &Self, interner: &Interner) -> Value {
        Value::Bool(!self.is_equal(other, interner))
    }

    pub fn gt(&self, other: &Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::bool(*num1 > *num2)),
            (lhs, rhs) => invalid_binary(lhs, rhs),
        }
    }

    pub fn ge(&self, other: &Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::bool(*num1 >= *num2)),
            (lhs, rhs) => invalid_binary(lhs, rhs),
        }
    }

    pub fn lt(&self, other: &Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::bool(*num1 < *num2)),
            (lhs, rhs) => invalid_binary(lhs, rhs),
        }
    }

    pub fn le(&self, other: &Self) -> OpResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::bool(*num1 <= *num2)),
            (lhs, rhs) => invalid_binary(lhs, rhs),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Value::Nil => "<nil>",
            Value::Bool(_) => "<bool>",
            Value::Number(_) => "<number>",
            Value::String(_) => "<string>",
            Value::Class(_) => "<class>",
            Value::Instance(_) => "<instance>",
            Value::Function(_) => "<function>",
            Value::StringLiteral(_) => "<string_literal>",
        }
    }

    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedValue<'a, 'b> {
        DisplayedValue {
            value: self,
            interner,
        }
    }

    fn is_equal(&self, other: &Self, interner: &Interner) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Number(num1), Value::Number(num2)) => num1 == num2,
            (Value::String(str1), Value::String(str2)) => str1 == str2,

            // FIXME: not working as expected (failing: test/class/methods_equality.lox)
            (Value::Function(func1), Value::Function(func2)) => func1 == func2,

            (Value::Class(cls1), Value::Class(cls2)) => cls1 == cls2,
            (Value::Instance(_), Value::Instance(_)) => todo!(),
            (Value::String(str1), Value::StringLiteral(str2)) => {
                str1.as_str() == interner.resolve(*str2)
            }
            (Value::StringLiteral(str1), Value::String(str2)) => {
                interner.resolve(*str1) == str2.as_str()
            }
            (Value::StringLiteral(str1), Value::StringLiteral(str2)) => str1 == str2,
            _ => false,
        }
    }
}

impl Display for DisplayedValue<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.value {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(num) => write!(f, "{num}"),
            Value::String(str) => write!(f, "{}", str.as_str()),

            // TODO: show inheritance list
            Value::Class(class) => write!(f, "<class {}>", interner.resolve(class.name)),

            Value::Instance(instance) => write!(
                f,
                "<instance of:{} no:{}>",
                interner.resolve(instance.class.name),
                instance.id
            ),
            Value::Function(func) => match func.deref() {
                Function::UserDefined(func) => {
                    let name = interner.resolve(func.name);
                    write!(f, "<fun {name}>")
                }
                Function::Native(func) => {
                    let name = interner.resolve(func.name);
                    write!(f, "<native_fun {name}>")
                }
            },
            Value::StringLiteral(key) => {
                let name = interner.resolve(*key);
                write!(f, "{}", name)
            }
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Nil => Value::Nil,
            Value::Bool(b) => Value::Bool(*b),
            Value::Number(num) => Value::Number(*num),
            Value::Function(fun) => Value::Function(Rc::clone(fun)),
            Value::String(str) => Value::String(Rc::clone(str)),
            Value::Class(class) => Value::Class(Rc::clone(class)),
            Value::Instance(instance) => Value::Instance(Rc::clone(instance)),
            Value::StringLiteral(key) => Value::StringLiteral(*key),
        }
    }
}

fn invalid_unary(value: &Value) -> OpResult {
    Err(InvalidOp::Unary(value.name()))
}

fn invalid_binary(left: &Value, right: &Value) -> OpResult {
    Err(InvalidOp::Binary(left.name(), right.name()))
}
