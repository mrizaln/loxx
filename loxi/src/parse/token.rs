use std::fmt::Display;

use lasso::{Rodeo, Spur};

use crate::util::Token;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
    String(Spur),
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
    pub name: Spur,
}

pub struct DisplayedLiteral<'a, 'b> {
    literal: &'a Literal,
    arena: &'b Rodeo,
}

pub struct DisplayedVariable<'a, 'b> {
    variable: &'a Variable,
    arena: &'b Rodeo,
}

impl Literal {
    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedLiteral<'a, 'b> {
        DisplayedLiteral {
            literal: self,
            arena,
        }
    }
}

impl Variable {
    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedVariable<'a, 'b> {
        DisplayedVariable {
            variable: self,
            arena,
        }
    }
}

impl Into<&str> for &Literal {
    fn into(self) -> &'static str {
        match self {
            Literal::Number(_) => "<number>",
            Literal::String(_) => "<string>",
            Literal::True => "true".into(),
            Literal::False => "false".into(),
            Literal::Nil => "nil".into(),
        }
    }
}

impl Into<&str> for &UnaryOp {
    fn into(self) -> &'static str {
        match self {
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
        }
    }
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

impl Into<&str> for &LogicalOp {
    fn into(self) -> &'static str {
        match self {
            LogicalOp::And => "and",
            LogicalOp::Or => "or",
        }
    }
}

impl Into<&str> for &Variable {
    fn into(self) -> &'static str {
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
        let arena = self.arena;
        match self.literal {
            Literal::Number(num) => write!(f, "{num}"),
            Literal::String(str) => write!(f, r#""{}""#, arena.resolve(str)),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for DisplayedVariable<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        write!(f, "{}", arena.resolve(&self.variable.name))
    }
}

impl Token for Literal {}
impl Token for UnaryOp {}
impl Token for BinaryOp {}
impl Token for LogicalOp {}
impl Token for Variable {}
