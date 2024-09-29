use lasso::Spur;

use crate::util::Token;

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Punctuation {
    ParenLeft,
    ParenRight,
    BraceLeft,
    BraceRight,
    Comma,
    Dot,
    Semicolon,
}

impl Into<char> for &Punctuation {
    fn into(self) -> char {
        match self {
            Punctuation::ParenLeft => '(',
            Punctuation::ParenRight => ')',
            Punctuation::BraceLeft => '{',
            Punctuation::BraceRight => '}',
            Punctuation::Comma => ',',
            Punctuation::Dot => '.',
            Punctuation::Semicolon => ';',
        }
    }
}

impl Into<&str> for &Punctuation {
    fn into(self) -> &'static str {
        match self {
            Punctuation::ParenLeft => "(",
            Punctuation::ParenRight => ")",
            Punctuation::BraceLeft => "{",
            Punctuation::BraceRight => "}",
            Punctuation::Comma => ",",
            Punctuation::Dot => ".",
            Punctuation::Semicolon => ";",
        }
    }
}

impl TryFrom<char> for Punctuation {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '(' => Ok(Punctuation::ParenLeft),
            ')' => Ok(Punctuation::ParenRight),
            '{' => Ok(Punctuation::BraceLeft),
            '}' => Ok(Punctuation::BraceRight),
            ',' => Ok(Punctuation::Comma),
            '.' => Ok(Punctuation::Dot),
            ';' => Ok(Punctuation::Semicolon),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Operator {
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

impl Into<&str> for &Operator {
    fn into(self) -> &'static str {
        match self {
            Operator::Bang => "!",
            Operator::BangEqual => "!=",
            Operator::Equal => "=",
            Operator::EqualEqual => "==",
            Operator::Greater => ">",
            Operator::GreaterEqual => ">=",
            Operator::Less => "<",
            Operator::LessEqual => "<=",
            Operator::Minus => "-",
            Operator::Plus => "+",
            Operator::Slash => "/",
            Operator::Star => "*",
        }
    }
}

impl TryFrom<&str> for Operator {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "!" => Ok(Operator::Bang),
            "!=" => Ok(Operator::BangEqual),
            "=" => Ok(Operator::Equal),
            "==" => Ok(Operator::EqualEqual),
            ">" => Ok(Operator::Greater),
            ">=" => Ok(Operator::GreaterEqual),
            "<" => Ok(Operator::Less),
            "<=" => Ok(Operator::LessEqual),
            "-" => Ok(Operator::Minus),
            "+" => Ok(Operator::Plus),
            "/" => Ok(Operator::Slash),
            "*" => Ok(Operator::Star),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Keyword {
    True,
    False,
    And,
    Or,
    Class,
    If,
    Else,
    For,
    While,
    Fun,
    Nil,
    Print,
    Return,
    Super,
    This,
    Var,
}

impl Into<&str> for &Keyword {
    fn into(self) -> &'static str {
        match self {
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Class => "class",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::While => "while",
            Keyword::Fun => "fun",
            Keyword::Nil => "nil",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::Var => "var",
        }
    }
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            "class" => Ok(Keyword::Class),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "for" => Ok(Keyword::For),
            "while" => Ok(Keyword::While),
            "fun" => Ok(Keyword::Fun),
            "nil" => Ok(Keyword::Nil),
            "print" => Ok(Keyword::Print),
            "return" => Ok(Keyword::Return),
            "super" => Ok(Keyword::Super),
            "this" => Ok(Keyword::This),
            "var" => Ok(Keyword::Var),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    String(Spur),
    Identifier(Spur),
    Number(f64),
}

// TODO: remove this impl and create a proper conversion to String using the lasso::RodeoReader
impl Into<String> for &Literal {
    fn into(self) -> String {
        match self {
            Literal::String(spur) => format!("spur|{}|", spur.into_inner()),
            Literal::Identifier(spur) => format!("spur|{}|", spur.into_inner()),
            Literal::Number(num) => format!("{num}"),
        }
    }
}

impl Token for Punctuation {}
impl Token for Operator {}
impl Token for Keyword {}
impl Token for Literal {}
