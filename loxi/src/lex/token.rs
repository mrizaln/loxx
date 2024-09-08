use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TokenValue {
    Punctuation(tokens::Punctuation),
    Operator(tokens::Operator),
    Keyword(tokens::Keyword),
    Literal(tokens::Literal),
    Eof,
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[at {}:{}]", self.line, self.column)
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Token {
    pub value: TokenValue,
    pub loc: Location,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tok {}: {:?}", self.loc, self.value)
    }
}

pub mod tokens {
    pub enum TokenParseError {
        InvalidToken,
    }

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

    impl TryFrom<char> for Punctuation {
        type Error = TokenParseError;

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                '(' => Ok(Punctuation::ParenLeft),
                ')' => Ok(Punctuation::ParenRight),
                '{' => Ok(Punctuation::BraceLeft),
                '}' => Ok(Punctuation::BraceRight),
                ',' => Ok(Punctuation::Comma),
                '.' => Ok(Punctuation::Dot),
                ';' => Ok(Punctuation::Semicolon),
                _ => Err(TokenParseError::InvalidToken),
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
        type Error = TokenParseError;

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
                _ => Err(TokenParseError::InvalidToken),
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
        type Error = TokenParseError;

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
                _ => Err(TokenParseError::InvalidToken),
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, PartialOrd)]
    pub enum Literal {
        String(String),
        Identifier(String),
        Number(f64),
    }

    impl Into<String> for &Literal {
        fn into(self) -> String {
            match self {
                Literal::String(str) => str.clone(),
                Literal::Identifier(ident) => ident.clone(),
                Literal::Number(num) => format!("{num}"),
            }
        }
    }
}

pub mod macros {
    macro_rules! tok {
        {[$line:expr,$col:expr] -> $type:ident} => {
            Token {
                value: TokenValue::$type,
                loc: Location { line: $line, column: $col },
            }
        };
        {[$line:expr,$col:expr] -> $type:ident = $value:expr} => {
            Token {
                value: TokenValue::$type($value),
                loc: Location { line: $line, column: $col },
            }
        };
        {[$line:expr,$col:expr] -> $type:ident::$value:ident} => {
            Token {
                value: TokenValue::$type(tokens::$type::$value),
                loc: Location { line: $line, column: $col },
            }
        };
        {[$line:expr,$col:expr] -> $type:ident::$name:ident = $value:expr} => {
            Token {
                value: TokenValue::$type(tokens::$type::$name($value)),
                loc: Location { line: $line, column: $col },
            }
        };
        {[$loc:expr] -> $type:ident} => {
            Token {
                value: TokenValue::$type,
                loc: $loc,
            }
        };
        {[$loc:expr] -> $type:ident = $value:expr} => {
            Token {
                value: TokenValue::$type($value),
                loc: $loc,
            }
        };
        {[$loc:expr] -> $type:ident::$value:ident} => {
            Token {
                value: TokenValue::$type(tokens::$type::$value),
                loc:  $loc,
            }
        };
        {[$loc:expr] -> $type:ident::$name:ident = $value:expr} => {
            Token {
                value: TokenValue::$type(tokens::$type::$name($value)),
                loc: $loc,
            }
        };
    }

    pub(crate) use tok;
}
