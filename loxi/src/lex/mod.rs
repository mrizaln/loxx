use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::str::CharIndices;

use thiserror::Error;
use unicode_width::UnicodeWidthChar;

use crate::interp::interner::{Interner, Key};
use crate::util::{self, Location, LoxToken, TokLoc};
use macros::tok;

pub mod token;

#[cfg(test)]
mod test;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Punctuation(TokLoc<token::Punctuation>),
    Operator(TokLoc<token::Operator>),
    Keyword(TokLoc<token::Keyword>),
    Literal(TokLoc<token::Literal>),
    Eof(Location),
}

pub struct DisplayedToken<'a, 'b> {
    token: &'a Token,
    interner: &'b Interner,
}

impl Token {
    pub fn loc(&self) -> Location {
        match self {
            Token::Punctuation(tokl) => tokl.loc,
            Token::Operator(tokl) => tokl.loc,
            Token::Keyword(tokl) => tokl.loc,
            Token::Literal(tokl) => tokl.loc,
            Token::Eof(loc) => *loc,
        }
    }

    pub fn static_str(&self) -> &'static str {
        match self {
            Token::Punctuation(TokLoc { tok, .. }) => tok.into(),
            Token::Operator(TokLoc { tok, .. }) => tok.into(),
            Token::Keyword(TokLoc { tok, .. }) => tok.into(),
            Token::Literal(TokLoc { tok, .. }) => tok.into(),
            Token::Eof(_) => "<eof>",
        }
    }

    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedToken<'a, 'b> {
        DisplayedToken {
            token: self,
            interner,
        }
    }
}

impl Display for DisplayedToken<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.token {
            Token::Literal(tokl) => {
                let name = self.token.static_str();
                let value = tokl.tok.display(interner);
                write!(f, r#"{} Tok{{ "{}": "{}" }}"#, tokl.loc, name, value)
            }
            Token::Punctuation(tokl) => {
                let name = tokl.tok.as_str();
                write!(f, r#"{} Tok{{ "<punctuation>": "{}" }}"#, tokl.loc, name)
            }
            Token::Operator(tokl) => {
                let name = tokl.tok.as_str();
                write!(f, r#"{} Tok{{ "<operator>": "{}" }}"#, tokl.loc, name)
            }
            Token::Keyword(tokl) => {
                let name = tokl.tok.as_str();
                write!(f, r#"{} Tok{{ "<keyword>": "{}" }}"#, tokl.loc, name)
            }
            Token::Eof(loc) => {
                write!(f, r#"{0} Tok{{ "{1}": "{1}" }}"#, loc, "<eof>")
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("{0} Unknown token ({1}) [{2:#x}]")]
    UnknownToken(Location, char, u32),

    #[error("{0} Unterminated string")]
    UnterminatedString(Location),

    #[error("{0} Unable to parse Number '{1}'")]
    UnableToParseNumber(Location, String),
}

#[derive(Debug)]
pub struct Lexer<'a, 'b> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    lines: Vec<&'a str>,
    interner: &'b mut Interner,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
    line: LineLocation,
}

#[derive(Debug)]
pub struct ScanResult<'a> {
    pub lines: Vec<&'a str>,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

impl<'a, 'b> Lexer<'a, 'b> {
    pub fn new(program: &'a str, interner: &'b mut Interner) -> Self {
        Self {
            source: program,
            chars: program.char_indices().peekable(),
            lines: Vec::new(),
            interner,
            tokens: Vec::new(),
            errors: Vec::new(),
            line: LineLocation {
                index: 1,
                start: 0,
                column: 0,
                char: '\0',
            },
        }
    }

    pub fn scan(mut self) -> ScanResult<'a> {
        while let Some((i, ch)) = self.advance() {
            self.scan_token(i, ch);
        }
        self.add_token(Token::Eof(self.line.to_loc()));

        ScanResult {
            lines: self.lines,
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    #[cfg(feature = "unicode")]
    fn scan_token(&mut self, current: usize, single: char) {
        match single.is_ascii() {
            true => match single {
                '\n' => self.newline_handler(current),
                '/' => self.slash_handler(),
                '"' => self.string_handler(current),
                c if c.is_ascii_digit() => self.number_handler(current),
                c if c.is_whitespace() => self.whitespace_handler(),
                c if is_ascii_identifier(c) => self.ascii_identifier_handler(current, single),
                _ => self.other_handler(single),
            },
            false => match single {
                c if c.is_whitespace() => self.whitespace_handler(),
                _ => self.unicode_identifier_handler(current, single),
            },
        }
    }

    #[cfg(not(feature = "unicode"))]
    fn scan_token(&mut self, current: usize, single: char) {
        match single {
            '\n' => self.newline_handler(current),
            '/' => self.slash_handler(),
            '"' => self.string_handler(current),
            c if c.is_ascii_digit() => self.number_handler(current),
            c if c.is_whitespace() => self.whitespace_handler(),
            c if is_ascii_identifier(c) => self.ascii_identifier_handler(current, single),
            _ => self.other_handler(single),
        }
    }

    fn newline_handler(&mut self, current: usize) {
        self.lines.push(&self.source[self.line.start..current]);

        self.line.index += 1;
        self.line.start = current + 1;
        self.line.column = 0;
    }

    fn slash_handler(&mut self) {
        // might be comment
        if self.if_next_is('/') {
            self.advance();
            while let Some((i, ch)) = self.advance() {
                if ch == '\n' {
                    self.newline_handler(i);
                    break;
                }
            }
            return;
        }

        self.add_token(tok! { [self.line.to_loc()] -> Operator::Slash });
    }

    fn string_handler(&mut self, current: usize) {
        let start = self.line.to_loc();

        let mut index = None;
        while let Some((i, ch)) = self.advance() {
            if ch == '"' {
                index = Some(i);
                break;
            } else if ch == '\n' {
                self.newline_handler(i);
            }
        }

        match index {
            Some(idx) => {
                let value = &self.source[current + 1..idx];
                let key = self.intern(value);
                self.add_token(tok!([start] -> Literal::String = key));
            }
            None => self.add_error(LexError::UnterminatedString(start)),
        }
    }

    fn number_handler(&mut self, current: usize) {
        let start = self.line.to_loc();
        let count = self.advance_while(|(_, ch)| ch.is_ascii_digit());
        let mut index = current + count;
        let mut trailing_dot = false;

        if let Some((_, '.')) = self.peek() {
            let _ = self.advance();
            let count = self.advance_while(|(_, ch)| ch.is_ascii_digit());
            if count > 0 {
                index += count + 1;
            } else {
                trailing_dot = true;
            }
        }

        match self.source[current..index + 1].parse::<f64>() {
            Ok(value) => {
                self.add_token(tok! { [start] -> Literal::Number = value });
            }
            Err(_) => {
                self.add_error(LexError::UnableToParseNumber(
                    self.line.to_loc(),
                    self.source[current..index + 1].to_string(),
                ));
            }
        }

        // NOTE: I add Dot token here since I can't peek the next next char. I must advance from
        // the dot on the if let block above so on the next iteration the dot is already consumed.
        if trailing_dot {
            self.add_token(tok! { [self.line.to_loc()] -> Punctuation::Dot });
        }
    }

    fn whitespace_handler(&mut self) {
        self.advance_while(|(_, ch)| ch.is_whitespace() && *ch != '\n');
    }

    fn ascii_identifier_handler(&mut self, current: usize, single: char) {
        let start = self.line.to_loc();
        let count = self.advance_while(|(_, ch)| {
            if cfg!(feature = "unicode") {
                match ch.is_ascii() {
                    true => is_ascii_identifier(*ch),
                    false => !ch.is_whitespace(),
                }
            } else {
                is_ascii_identifier(*ch)
            }
        });
        let end = current + count + single.len_utf8();
        let value = &self.source[current..end];
        let key = self.intern(value);

        let token = match token::Keyword::try_from(value) {
            Ok(keyword) => tok! { [start] -> Keyword = keyword },
            Err(_) => tok! { [start] -> Literal::Identifier = key },
        };

        self.add_token(token);
    }

    // NOTE: any non-whitespace unicode is considered identifier
    // WARN: braille blank is not considered as whitespace!
    #[cfg(feature = "unicode")]
    fn unicode_identifier_handler(&mut self, current: usize, single: char) {
        let start = self.line.to_loc();
        let count = self.advance_while(|(_, ch)| match ch.is_ascii() {
            true => is_ascii_identifier(*ch),
            false => !ch.is_whitespace(),
        });
        let end = current + count + single.len_utf8();
        let value = &self.source[current..end];
        let key = self.intern(value);
        self.add_token(tok! { [start] -> Literal::Identifier = key});
    }

    fn other_handler(&mut self, single: char) {
        let start = self.line.to_loc();

        if let Ok(token) = token::Punctuation::try_from(single) {
            self.add_token(tok! { [start] -> Punctuation = token });
            return;
        }

        let mut two_char_buf = [0u8; 8];

        // for double chars operators
        if let Some((_, ch)) = self.peek() {
            let double_slice = util::to_str(&mut two_char_buf, &[single, *ch]);
            if let Ok(token) = token::Operator::try_from(double_slice) {
                self.add_token(tok! { [start] -> Operator = token });
                let _ = self.advance();
                return;
            }
        }

        // for single chars operators
        let single_slice = util::to_str(&mut two_char_buf, &[single]);
        if let Ok(token) = token::Operator::try_from(single_slice) {
            self.add_token(tok! { [start] -> Operator = token });
            return;
        }

        self.add_error(LexError::UnknownToken(
            self.line.to_loc(),
            single,
            single as u32,
        ));
    }

    fn intern(&mut self, str: &str) -> Key {
        self.interner.get_or_intern(str)
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token)
    }
    fn add_error(&mut self, err: LexError) {
        self.errors.push(err)
    }

    fn peek(&mut self) -> Option<&(usize, char)> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.chars.next().inspect(|(_, ch)| {
            self.line.column += ch.width().unwrap_or(0);
            self.line.char = *ch;
        })
    }

    /// peekable has issue of getting into the limitation of the borrow checker.
    /// https://stackoverflow.com/a/37509009 [accessed: 2024/09/07]
    fn advance_while<F>(&mut self, pred: F) -> usize
    where
        F: Fn(&(usize, char)) -> bool,
    {
        let mut count = 0;
        let map = |res: &(usize, char)| {
            let cont = pred(res);
            let size = res.1.len_utf8();
            match cont {
                true => size,
                false => usize::MAX,
            }
        };

        while let Some(size) = self.peek().map(map) {
            match size {
                std::usize::MAX => break,
                _ => {
                    count += size;
                    let _ = self.advance();
                }
            }
        }
        count
    }

    fn if_next_is(&mut self, ch: char) -> bool {
        matches!(self.chars.peek(), Some((_, c)) if *c == ch)
    }
}

#[derive(Debug, Clone)]
struct LineLocation {
    pub index: usize,
    pub start: usize,
    pub column: usize, // the displayed column
    pub char: char,
}

impl LineLocation {
    pub fn to_loc(&self) -> Location {
        Location {
            line: self.index,
            column: self.column - self.char.width().unwrap_or(0) + 1,
        }
    }
}

fn is_ascii_identifier(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

mod macros {
    macro_rules! tok {
        {[$loc:expr] -> $type:ident = $value:expr} => {
            Token::$type(TokLoc {
                tok: $value,
                loc: $loc,
            })
        };
        {[$loc:expr] -> $type:ident::$value:ident} => {
            Token::$type(TokLoc {
                tok: token::$type::$value,
                loc: $loc,
            })
        };
        {[$loc:expr] -> $type:ident::$name:ident = $value:expr} => {
            Token::$type(TokLoc {
                tok: token::$type::$name($value),
                loc: $loc,
            })
        };
    }

    pub(crate) use tok;
}
