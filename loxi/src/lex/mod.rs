use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

use self::token::{tokens, Location, Token, TokenValue};
use crate::util;

mod tests;
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
            print!("\r{} chars out of {} scanned", i, self.source.len());
        }

        self.tokens.push(Token {
            value: TokenValue::Eof,
            loc: self.line.loc_rel(self.source.len()).unwrap(),
        });

        ScanResult {
            lines: self.lines,
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn scan_token(&mut self, current: usize, single: char) {
        match single {
            '\n' => self.newline_handler(current),
            '/' => self.slash_handler(current),
            '"' => self.string_handler(current),
            c if c.is_digit(10) => self.number_handler(current),
            c if c.is_whitespace() => self.whitespace_handler(),
            c if Lexer::is_identifier(c) => self.alphabetic_handler(current),
            _ => self.other_handler(current, single),
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

        self.tokens.push(Token {
            value: TokenValue::Operator(tokens::Operator::Slash),
            loc: self.line.loc_rel(current).unwrap(),
        });
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
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::String(value)),
                    loc: start_line.loc_rel(current).unwrap(),
                });
            }
            None => {
                self.errors.push(LexError::UnterminatedString(
                    start_line.loc_rel(current).unwrap(),
                ));
            }
        }
    }

    // TODO: add checks on number of dots encountered
    // TODO: check whether at the end of the number an identifier exist, error if yes
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
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::Number(value)),
                    loc: self.line.loc_rel(current).unwrap(),
                });
            }
            Err(_) => {
                self.errors.push(LexError::UnableToParseNumber(
                    self.line.loc_rel(current).unwrap(),
                    self.source[current..index + 1].to_string(),
                ));
            }
        }

        // NOTE: I do the addition here since I can't peek the next next char. I must advance from
        // the dot on the if let block above so on the next iteration the dot is already consumed.
        if trailing_dot {
            self.tokens.push(Token {
                value: TokenValue::Punctuation(tokens::Punctuation::Dot),
                loc: self.line.loc_rel(index).unwrap(),
            });
        }
    }

    fn whitespace_handler(&mut self) {
        self.advance_while(|(_, ch)| ch.is_whitespace() && *ch != '\n');
    }

    fn alphabetic_handler(&mut self, current: usize) {
        let count = self.advance_while(|(_, ch)| Lexer::is_identifier(*ch));
        let index = current + count;
        let value = self.source[current..index + 1].to_string();
        let token = match tokens::Keyword::try_from(value.as_str()) {
            Ok(keyword) => TokenValue::Keyword(keyword),
            Err(_) => TokenValue::Literal(tokens::Literal::Identifier(value)),
        };
        self.tokens.push(Token {
            value: token,
            loc: self.line.loc_rel(current).unwrap(),
        });
    }

    fn other_handler(&mut self, current: usize, single: char) {
        if let Ok(token) = tokens::Punctuation::try_from(single) {
            self.tokens.push(Token {
                value: TokenValue::Punctuation(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return;
        }

        let mut two_char_buf = [0u8; 8];

        // for double chars operators
        if let Some((_, ch)) = self.peek() {
            let double_slice = util::to_str(&mut two_char_buf, &[single, *ch]);
            if let Ok(token) = tokens::Operator::try_from(double_slice) {
                self.tokens.push(Token {
                    value: TokenValue::Operator(token),
                    loc: self.line.loc_rel(current).unwrap(),
                });
                let _ = self.advance();
                return;
            }
        }

        // for single chars operators
        let single_slice = util::to_str(&mut two_char_buf, &[single]);
        if let Ok(token) = tokens::Operator::try_from(single_slice) {
            self.tokens.push(Token {
                value: TokenValue::Operator(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return;
        }

        self.errors.push(LexError::UnknownToken(
            self.line.loc_rel(current).unwrap(),
            single,
            single as u32,
        ));
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
        while let Some(true) = self.peek().map(&pred) {
            count += 1;
            let _ = self.advance();
        }
        count
    }

    fn if_next_is(&mut self, ch: char) -> bool {
        match self.chars.peek() {
            Some((_, c)) if *c == ch => true,
            _ => false,
        }
    }

    fn is_identifier(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }
}

#[derive(Debug, Clone)]
struct LineLocation {
    pub index: usize,
    pub start: usize,
}

impl LineLocation {
    pub fn col_rel(&self, index: usize) -> Option<usize> {
        match index {
            i if i < self.start => None,
            _ => Some(index - self.start + 1), // 1-indexed
        }
    }

    pub fn loc_rel(&self, index: usize) -> Option<Location> {
        match self.col_rel(index) {
            Some(column) => Some(Location {
                line: self.index,
                column,
            }),
            None => None,
        }
    }
}
