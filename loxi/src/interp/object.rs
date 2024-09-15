use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Object(LoxObject),
}

// TODO: create the actual object
#[derive(Debug, PartialEq, PartialOrd)]
pub struct LoxObject {
    //
}

// TODO: add other info
#[derive(Debug)]
pub enum ValueError {
    UnsupportedOperation,
}

type ValueResult = Result<Value, ValueError>;

impl Value {
    /// follows Ruby's simple rule: `false` and `nil` are falsy, everything else truthy
    pub fn truthiness(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn not(&self) -> ValueResult {
        Ok(Value::Bool(!self.truthiness()))
    }

    pub fn minus(&self) -> ValueResult {
        match self {
            Value::Number(num) => Ok(Value::Number(-num)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn add(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(*num1 + *num2)),
            (Value::String(str1), Value::String(str2)) => Ok(Value::String(str1.clone() + str2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn sub(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(*num1 - *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn mul(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(*num1 * *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn div(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Number(*num1 / *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn eq(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(num1 == num2)),
            (Value::String(str1), Value::String(str2)) => Ok(Value::Bool(str1 == str2)),
            (Value::Object(_obj1), Value::Object(_obj2)) => unimplemented!(),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn neq(&self, other: &Self) -> ValueResult {
        self.eq(other)?.not()
    }

    pub fn gt(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 > *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn ge(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 >= *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn lt(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 < *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }

    pub fn le(&self, other: &Self) -> ValueResult {
        match (self, other) {
            (Value::Number(num1), Value::Number(num2)) => Ok(Value::Bool(*num1 <= *num2)),
            _ => Err(ValueError::UnsupportedOperation),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::Number(num) => write!(f, "Number({num})"),
            Value::String(str) => write!(f, "String({str})"),
            Value::Object(_obj) => write!(f, "Object(<dummy>)"),
        }
    }
}
