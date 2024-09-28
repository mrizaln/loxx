use std::fmt::Display;

use crate::util::Token;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Into<String> for &Literal {
    fn into(self) -> String {
        match self {
            Literal::Number(num) => format!("{num}"),
            Literal::String(str) => format!(r#""{str}""#),
            Literal::True => "true".into(),
            Literal::False => "false".into(),
            Literal::Nil => "nil".into(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: String = self.into();
        write!(f, "{str}")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOp {
    Minus,
    Not,
}

impl Into<&str> for &UnaryOp {
    fn into(self) -> &'static str {
        match self {
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum BinaryOp {
    Equal,
    NotEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Add,
    Sub,
    Mul,
    Div,
}

impl Into<&str> for &BinaryOp {
    fn into(self) -> &'static str {
        match self {
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum LogicalOp {
    And,
    Or,
}

impl Into<&str> for &LogicalOp {
    fn into(self) -> &'static str {
        match self {
            LogicalOp::And => "and",
            LogicalOp::Or => "or",
        }
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

// TODO: place the name inside a global container or something then use a reference to access the
//       name so theres no cloning required
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Variable {
    pub name: String,
}

impl Token for Literal {}
impl Token for UnaryOp {}
impl Token for BinaryOp {}
impl Token for LogicalOp {}
impl Token for Variable {}
