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
    lines: Vec<&'a str>,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
    line: LineLocation,
    start: usize,
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
            lines: Vec::new(),
            tokens: Vec::new(),
            errors: Vec::new(),
            line: LineLocation {
                index: 1,
                start: 0,
                end: 0,
            },
            start: 0,
        }
    }

    pub fn scan(mut self) -> ScanResult<'a> {
        while self.start < self.source.len() {
            self.start = self.scan_token(self.start);
            self.line.end = self.start;

            // simple debugging to see whether my code is stalling, or just slow :D
            print!(
                "\r{} chars out of {} scanned",
                self.start,
                self.source.len()
            );
        }

        self.tokens.push(Token {
            value: TokenValue::Eof,
            loc: self.line.loc_rel(self.start).unwrap(),
        });

        ScanResult {
            lines: self.lines,
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn scan_token(&mut self, current: usize) -> usize {
        let single = self.source.chars().nth(current).unwrap();
        match single {
            '\n' => self.newline_handler(current),
            '/' => self.slash_handler(current),
            '"' => self.string_handler(current),
            c if c.is_digit(10) => self.number_handler(current),
            c if c.is_whitespace() => self.whitespace_handler(current),
            c if c.is_alphabetic() => self.alphabetic_handler(current),
            _ => self.other_handler(current),
        }
    }

    fn newline_handler(&mut self, current: usize) -> usize {
        let LineLocation {
            ref mut index,
            ref mut start,
            ref mut end,
        } = self.line;

        *end = current;
        self.lines.push(&self.source[*start..*end]);

        *index += 1;
        *start = current + 1;

        current + 1
    }

    fn slash_handler(&mut self, current: usize) -> usize {
        // might be comment
        let double_slice = &self.source[current..current + 2];
        if double_slice == "//" {
            let index = self.source[current..]
                .find('\n')
                .map(|i| i + current)
                .unwrap_or(self.source.len());
            return index;
        }

        self.tokens.push(Token {
            value: TokenValue::Operator(tokens::Operator::Slash),
            loc: self.line.loc_rel(current).unwrap(),
        });
        current + 1
    }

    fn string_handler(&mut self, current: usize) -> usize {
        let index = self.source[current + 1..]
            .find('"')
            .map(|i| i + current + 1);

        match index {
            Some(idx) => {
                let value = self.source[current + 1..idx - 1].to_string();
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::String(value)),
                    loc: self.line.loc_rel(current).unwrap(),
                });
                idx + 1
            }
            None => {
                self.errors.push(LexError::UnterminatedString(
                    self.line.loc_rel(current).unwrap(),
                ));

                // guesstimate end of statement
                let end = self.source[current..]
                    .find(|c: char| c == '\n')
                    .map(|i| i + current)
                    .unwrap_or(self.source.len());
                end
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

    fn other_handler(&mut self, current: usize) -> usize {
        let single = self.source.chars().nth(current).unwrap();

        if let Ok(token) = tokens::Punctuation::try_from(single) {
            self.tokens.push(Token {
                value: TokenValue::Punctuation(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return current + 1;
        }

        // for double cchars operators
        let double_slice = &self.source[current..current + 2];
        if let Ok(token) = tokens::Operator::try_from(double_slice) {
            self.tokens.push(Token {
                value: TokenValue::Operator(token),
                loc: self.line.loc_rel(current).unwrap(),
            });
            return current + 2;
        }

        // for single chars operators
        let single_slice = &self.source[current..current + 1];
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

#[derive(Debug)]
struct LineLocation {
    pub index: usize,
    pub start: usize,
    pub end: usize,
}

impl LineLocation {
    pub fn col_rel(&self, index: usize) -> Option<usize> {
        match index {
            i if i < self.start => None,
            i if i > self.end => None,
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
