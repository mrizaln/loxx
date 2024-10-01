use std::fmt::Display;

use crate::interp::interner::{Interner, Key};
use crate::util::Token;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
    String(Key),
    True,
    False,
    Nil,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOp {
    Minus,
    Not,
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

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Variable {
    pub name: Key,
}

pub struct DisplayedLiteral<'a, 'b> {
    literal: &'a Literal,
    interner: &'b Interner,
}

pub struct DisplayedVariable<'a, 'b> {
    variable: &'a Variable,
    interner: &'b Interner,
}

impl Literal {
    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedLiteral<'a, 'b> {
        DisplayedLiteral {
            literal: self,
            interner,
        }
    }
}

impl Variable {
    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedVariable<'a, 'b> {
        DisplayedVariable {
            variable: self,
            interner,
        }
    }
}

impl From<&Literal> for &str {
    fn from(val: &Literal) -> Self {
        match val {
            Literal::Number(_) => "<number>",
            Literal::String(_) => "<string>",
            Literal::True => "true",
            Literal::False => "false",
            Literal::Nil => "nil",
        }
    }
}

impl From<&UnaryOp> for &str {
    fn from(val: &UnaryOp) -> Self {
        match val {
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl From<&BinaryOp> for &str {
    fn from(val: &BinaryOp) -> Self {
        match val {
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

impl From<&LogicalOp> for &str {
    fn from(val: &LogicalOp) -> Self {
        match val {
            LogicalOp::And => "and",
            LogicalOp::Or => "or",
        }
    }
}

impl From<&Variable> for &str {
    fn from(_val: &Variable) -> Self {
        "<variable>"
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: &str = self.into();
        write!(f, "{str}")
    }
}

impl Display for DisplayedLiteral<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.literal {
            Literal::Number(num) => write!(f, "{num}"),
            Literal::String(str) => write!(f, r#""{}""#, interner.resolve(*str)),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for DisplayedVariable<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        write!(f, "{}", interner.resolve(self.variable.name))
    }
}

impl Token for Literal {}
impl Token for UnaryOp {}
impl Token for BinaryOp {}
impl Token for LogicalOp {}
impl Token for Variable {}
