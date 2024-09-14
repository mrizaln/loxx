use core::panic;
use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

use self::token::{macros::tok, tokens, Token, TokenValue};
use crate::util::{self, Location};

mod test;
pub mod token;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("{0} Unknown token ({1}) [{2:#x}]")]
    UnknownToken(Location, char, u32),

    #[error("{0} Unterminated string")]
    UnterminatedString(Location),

    #[error("{0} Unable to parse Number {1}")]
    UnableToParseNumber(Location, String),
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    lines: Vec<&'a str>,
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

impl<'a> Lexer<'a> {
    pub fn new(program: &'a str) -> Self {
        Lexer {
            source: program,
            chars: program.char_indices().peekable(),
            lines: Vec::new(),
            tokens: Vec::new(),
            errors: Vec::new(),
            line: LineLocation { index: 1, start: 0 },
        }
    }

    pub fn scan(mut self) -> ScanResult<'a> {
        while let Some((i, ch)) = self.advance() {
            self.scan_token(i, ch);

            // simple debugging to see whether my code is stalling, or just slow :D
            // print!("\r{} chars out of {} scanned", i, self.source.len());
        }
        self.add_token(tok! { [self.loc_rel(self.source.len())] -> Eof });

        ScanResult {
            lines: self.lines,
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    #[rustfmt::skip]
    #[cfg(feature = "unicode")]
    fn scan_token(&mut self, current: usize, single: char) {
        match single.is_ascii() {
            true => match single {
                '\n'                        => self.newline_handler(current),
                '/'                         => self.slash_handler(current),
                '"'                         => self.string_handler(current),
                c if c.is_digit(10)         => self.number_handler(current),
                c if c.is_whitespace()      => self.whitespace_handler(),
                c if is_ascii_identifier(c) => self.ascii_identifier_handler(current, single),
                _                           => self.other_handler(current, single),
            },
            false => match single {
                c if c.is_whitespace()      => self.whitespace_handler(),
                _                           => self.unicode_identifier_handler(current, single),
            },
        }
    }

    #[rustfmt::skip]
    #[cfg(not(feature = "unicode"))]
    fn scan_token(&mut self, current: usize, single: char) {
        match single {
            '\n'                        => self.newline_handler(current),
            '/'                         => self.slash_handler(current),
            '"'                         => self.string_handler(current),
            c if c.is_digit(10)         => self.number_handler(current),
            c if c.is_whitespace()      => self.whitespace_handler(),
            c if is_ascii_identifier(c) => self.ascii_identifier_handler(current, single),
            _                           => self.other_handler(current, single),
        }
    }

    fn newline_handler(&mut self, current: usize) {
        self.lines.push(&self.source[self.line.start..current]);

        self.line.index += 1;
        self.line.start = current + 1;
    }

    fn slash_handler(&mut self, current: usize) {
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

        self.add_token(tok! { [self.loc_rel(current)] -> Operator::Slash });
    }

    fn string_handler(&mut self, current: usize) {
        let start_line = self.line.clone();

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
                let value = self.source[current + 1..idx].to_string();
                self.add_token(tok! {
                    [start_line.loc_rel(current).unwrap()] -> Literal::String = value
                });
            }
            None => self.add_error(LexError::UnterminatedString(
                start_line.loc_rel(current).unwrap(),
            )),
        }
    }

    fn number_handler(&mut self, current: usize) {
        let count = self.advance_while(|(_, ch)| ch.is_digit(10));
        let mut index = current + count;
        let mut trailing_dot = false;

        if let Some((_, '.')) = self.peek() {
            let _ = self.advance();
            let count = self.advance_while(|(_, ch)| ch.is_digit(10));
            if count > 0 {
                index += count + 1;
            } else {
                trailing_dot = true;
            }
        }

        match self.source[current..index + 1].parse::<f64>() {
            Ok(value) => {
                self.add_token(tok! { [self.loc_rel(current)] -> Literal::Number = value });
            }
            Err(_) => {
                self.add_error(LexError::UnableToParseNumber(
                    self.loc_rel(current),
                    self.source[current..index + 1].to_string(),
                ));
            }
        }

        // NOTE: I add Dot token here since I can't peek the next next char. I must advance from
        // the dot on the if let block above so on the next iteration the dot is already consumed.
        if trailing_dot {
            self.add_token(tok! { [self.loc_rel(index)] -> Punctuation::Dot });
        }
    }

    fn whitespace_handler(&mut self) {
        self.advance_while(|(_, ch)| ch.is_whitespace() && *ch != '\n');
    }

    fn ascii_identifier_handler(&mut self, current: usize, single: char) {
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
        let value = self.source[current..end].to_string();

        let token = match tokens::Keyword::try_from(value.as_str()) {
            Ok(keyword) => tok! { [self.loc_rel(current)] -> Keyword = keyword },
            Err(_) => tok! { [self.loc_rel(current)] -> Literal::Identifier = value },
        };

        self.add_token(token);
    }

    // NOTE: any non-whitespace unicode is considered identifier
    // WARN: braille blank is not considered as whitespace!
    #[cfg(feature = "unicode")]
    fn unicode_identifier_handler(&mut self, current: usize, single: char) {
        let count = self.advance_while(|(_, ch)| match ch.is_ascii() {
            true => is_ascii_identifier(*ch),
            false => !ch.is_whitespace(),
        });
        let end = current + count + single.len_utf8();
        let value = self.source[current..end].to_string();
        self.add_token(tok! { [self.loc_rel(current)] -> Literal::Identifier = value });
    }

    fn other_handler(&mut self, current: usize, single: char) {
        if let Ok(token) = tokens::Punctuation::try_from(single) {
            self.add_token(tok! { [self.loc_rel(current)] -> Punctuation = token });
            return;
        }

        let mut two_char_buf = [0u8; 8];

        // for double chars operators
        if let Some((_, ch)) = self.peek() {
            let double_slice = util::to_str(&mut two_char_buf, &[single, *ch]);
            if let Ok(token) = tokens::Operator::try_from(double_slice) {
                self.add_token(tok! { [self.loc_rel(current)] -> Operator = token });
                let _ = self.advance();
                return;
            }
        }

        // for single chars operators
        let single_slice = util::to_str(&mut two_char_buf, &[single]);
        if let Ok(token) = tokens::Operator::try_from(single_slice) {
            self.add_token(tok! { [self.loc_rel(current)] -> Operator = token });
            return;
        }

        self.add_error(LexError::UnknownToken(
            self.loc_rel(current),
            single,
            single as u32,
        ));
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
        self.chars.next()
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
                false => std::usize::MAX,
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
        match self.chars.peek() {
            Some((_, c)) if *c == ch => true,
            _ => false,
        }
    }

    fn loc_rel(&self, index: usize) -> Location {
        match self.line.loc_rel(index) {
            Ok(loc) => loc,
            Err(err) => panic!("Fatal error: {err}"),
        }
    }
}

#[derive(Debug, Clone)]
struct LineLocation {
    pub index: usize,
    pub start: usize,
}

#[derive(Debug, Error)]
enum LineLocationError {
    #[error("Index less than start: {index} < {start}")]
    IndexLessThanStart { start: usize, index: usize },
}

impl LineLocation {
    pub fn col_rel(&self, index: usize) -> Result<usize, LineLocationError> {
        match index {
            i if i < self.start => Err(LineLocationError::IndexLessThanStart {
                start: self.start,
                index,
            }),
            _ => Ok(index - self.start + 1), // 1-indexed
        }
    }

    pub fn loc_rel(&self, index: usize) -> Result<Location, LineLocationError> {
        self.col_rel(index).map(|column| Location {
            line: self.index,
            column,
        })
    }
}

// NOTE: only works for ascii
fn is_ascii_identifier(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
