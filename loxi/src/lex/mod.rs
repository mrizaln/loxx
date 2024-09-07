use std::iter::Peekable;
use std::str::CharIndices;

use thiserror::Error;

pub mod token;

use self::token::{tokens, Location, Token, TokenValue};

#[derive(Debug, Error)]
pub enum LexError {
    #[error("{0} Unknown token {1:?} ({2:#x})")]
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
        while let Some((i, ch)) = self.chars.next() {
            let skip_to = self.scan_token(i, ch);

            let mut diff = skip_to - i - 1;
            while diff > 0 {
                self.chars.next();
                diff -= 1;
            }

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

    fn scan_token(&mut self, current: usize, single: char) -> usize {
        match single {
            '\n' => self.newline_handler(current),
            '/' => self.slash_handler(current),
            '"' => self.string_handler(current),
            c if c.is_digit(10) => self.number_handler(current),
            c if c.is_whitespace() => self.whitespace_handler(current),
            c if c.is_alphabetic() => self.alphabetic_handler(current),
            _ => self.other_handler(current, single),
        }
    }

    fn newline_handler(&mut self, current: usize) -> usize {
        println!("start: {}, current: {}", self.line.start, current);
        self.lines.push(&self.source[self.line.start..current]);

        self.line.index += 1;
        self.line.start = current + 1;

        current + 1
    }

    fn slash_handler(&mut self, current: usize) -> usize {
        // might be comment
        match self.chars.peek() {
            Some((i, '/')) => {
                let index = self.source[*i + 1..]
                    .find('\n')
                    .map(|i| i + current)
                    .unwrap_or(self.source.len());
                return index;
            }
            _ => (),
        }

        self.tokens.push(Token {
            value: TokenValue::Operator(tokens::Operator::Slash),
            loc: self.line.loc_rel(current).unwrap(),
        });
        current + 1
    }

    fn string_handler(&mut self, current: usize) -> usize {
        let start_line = self.line.clone();

        let mut index = None;
        for (i, ch) in self.source[current + 1..].char_indices() {
            if ch == '"' {
                index = Some(current + 1 + i);
                break;
            } else if ch == '\n' {
                self.newline_handler(current + 1 + i);
            }
        }

        match index {
            Some(idx) => {
                let value = self.source[current + 1..idx - 1].to_string();
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::String(value)),
                    loc: start_line.loc_rel(current).unwrap(),
                });
                idx + 1
            }
            None => {
                self.errors.push(LexError::UnterminatedString(
                    start_line.loc_rel(current).unwrap(),
                ));

                self.source.len()
            }
        }
    }

    // TODO: add checks on number of dots encountered
    // TODO: check whether at the end of the number an identifier exist, error if yes
    fn number_handler(&mut self, current: usize) -> usize {
        let index = self.source[current..]
            .find(|c: char| !c.is_digit(10) && c != '.')
            .map(|i| i + current)
            .unwrap_or(self.source.len());

        match self.source[current..index].parse::<f64>() {
            Ok(value) => {
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::Number(value)),
                    loc: self.line.loc_rel(current).unwrap(),
                });
            }
            Err(_err) => {
                self.errors.push(LexError::UnableToParseNumber(
                    self.line.loc_rel(current).unwrap(),
                    self.source[current..index].to_string(),
                ));
            }
        }
        index
    }

    fn whitespace_handler(&mut self, current: usize) -> usize {
        let index = self.source[current..]
            .find(|c: char| !c.is_whitespace() || c == '\n') // ignore newline
            .map(|i| i + current)
            .unwrap_or(self.source.len());
        index
    }

    fn alphabetic_handler(&mut self, current: usize) -> usize {
        let index = self.source[current..]
            .find(|c: char| !Lexer::is_identifier(c))
            .map(|i| i + current)
            .unwrap_or(self.source.len());
        let value = self.source[current..index].to_string();
        let token = match tokens::Keyword::try_from(value.as_str()) {
            Ok(keyword) => TokenValue::Keyword(keyword),
            Err(_) => TokenValue::Literal(tokens::Literal::Identifier(value)),
        };
        self.tokens.push(Token {
            value: token,
            loc: self.line.loc_rel(current).unwrap(),
        });
        index
    }

    fn other_handler(&mut self, current: usize, single: char) -> usize {
        if let Ok(token) = tokens::Punctuation::try_from(single) {
            self.tokens.push(Token {
                value: TokenValue::Punctuation(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return current + 1;
        }

        let mut two_char_buf = [0u8; 8];

        // for double chars operators
        match self.chars.peek() {
            Some((_, ch)) => {
                let double_slice = to_str(&mut two_char_buf, single, Some(*ch));
                if let Ok(token) = tokens::Operator::try_from(double_slice) {
                    self.tokens.push(Token {
                        value: TokenValue::Operator(token),
                        loc: self.line.loc_rel(current).unwrap(),
                    });
                    return current + 2;
                }
            }
            _ => (),
        }

        // for single chars operators
        let single_slice = to_str(&mut two_char_buf, single, None);
        if let Ok(token) = tokens::Operator::try_from(single_slice) {
            self.tokens.push(Token {
                value: TokenValue::Operator(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return current + 1;
        }

        self.errors.push(LexError::UnknownToken(
            self.line.loc_rel(current).unwrap(),
            single,
            single as u32,
        ));

        current + 1
    }

    fn is_identifier(c: char) -> bool {
        c.is_alphabetic() || c == '_'
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

// inspired by: https://stackoverflow.com/a/78486719
fn to_str(buf: &mut [u8; 8], left: char, right: Option<char>) -> &str {
    let mut pos = 0;
    pos += left.encode_utf8(&mut buf[pos..]).len();
    if let Some(right) = right {
        pos += right.encode_utf8(&mut buf[pos..]).len();
    }
    std::str::from_utf8(&buf[..pos]).expect("The two chars must be valid utf8")
}
