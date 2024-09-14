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
            Literal::String(str) => str.clone(),
            Literal::True => "true".into(),
            Literal::False => "false".into(),
            Literal::Nil => "nil".into(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl Into<&str> for &UnaryOp {
    fn into(self) -> &'static str {
        match self {
            UnaryOp::Minus => "-",
            UnaryOp::Bang => "!",
        }
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
    Plus,
    Minus,
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
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
        }
    }
}

impl Token for Literal {}
impl Token for UnaryOp {}
impl Token for BinaryOp {}
