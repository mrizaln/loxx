use thiserror::Error;

pub mod token;

use self::token::{tokens, Location, Token, TokenValue};

#[derive(Debug, Error)]
pub enum LexError {
    #[error("Unknown token {char:?} ({value:#x}) [line {line}]")]
    UnknownToken { char: char, value: u32, line: usize },
    #[error("Unterminated String at line {0}")]
    UnterminatedString(usize),
    #[error("Unable to parse Number {0} [line {1}]")]
    UnableToParseNumber(String, usize),
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(program: &'a str) -> Self {
        Lexer {
            source: program,
            tokens: Vec::new(),
            start: 0,
            line: 1, // 1-indexed
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, LexError> {
        while self.start < self.source.len() {
            self.start = self.scan_token(self.start)?;
        }

        self.tokens.push(Token {
            value: TokenValue::Eof,
            loc: Location { line: self.line },
        });

        Ok(self.tokens)
    }

    fn scan_token(&mut self, start: usize) -> Result<usize, LexError> {
        let current = start;
        let single = self.source.chars().nth(current).unwrap();

        match single {
            '\n' => {
                self.line += 1;
                Ok(current + 1)
            }
            '/' => {
                // might be comment
                let double_slice = &self.source[current..current + 2];
                if double_slice == "//" {
                    let index = self.source[current..]
                        .find('\n')
                        .map(|i| i + current)
                        .unwrap_or(self.source.len());
                    return Ok(index);
                }

                // for double chars operators
                if let Ok(token) = tokens::Operator::try_from(double_slice) {
                    self.tokens.push(Token {
                        value: TokenValue::Operator(token),
                        loc: Location { line: self.line },
                    });
                    return Ok(current + 2);
                }

                self.tokens.push(Token {
                    value: TokenValue::Operator(tokens::Operator::Slash),
                    loc: Location { line: self.line },
                });
                Ok(current + 1)
            }
            '"' => {
                let index = self.source[current + 1..]
                    .find('"')
                    .map(|i| i + current + 1)
                    .ok_or(LexError::UnterminatedString(self.line))?;
                let value = self.source[current + 1..index - 1].to_string();
                self.tokens.push(Token {
                    value: TokenValue::Literal(tokens::Literal::String(value)),
                    loc: Location { line: self.line },
                });
                Ok(index + 1)
            }
            c if c.is_digit(10) => {
                // TODO: add checks on number of dots encountered
                // TODO: check whether at the end of the number an identifier exist, error if yes
                let index = self.source[current..]
                    .find(|c: char| !c.is_digit(10) && c != '.')
                    .map(|i| i + current)
                    .unwrap_or(self.source.len());
                match self.source[current..index].parse::<f64>() {
                    Ok(value) => {
                        self.tokens.push(Token {
                            value: TokenValue::Literal(tokens::Literal::Number(value)),
                            loc: Location { line: self.line },
                        });
                        Ok(index)
                    }
                    Err(err) => {
                        println!("{err}");
                        Err(LexError::UnableToParseNumber(
                            self.source[current..index].to_string(),
                            self.line,
                        ))
                    }
                }
            }
            c if c.is_whitespace() => Ok(current + 1),
            c if c.is_alphabetic() => {
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
                    loc: Location { line: self.line },
                });
                Ok(index)
            }
            _ => {
                if let Ok(token) = tokens::Punctuation::try_from(single) {
                    self.tokens.push(Token {
                        value: TokenValue::Punctuation(token),
                        loc: Location { line: self.line },
                    });
                    return Ok(current + 1);
                }

                // for single chars operators
                let single_slice = &self.source[current..current + 1];
                if let Ok(token) = tokens::Operator::try_from(single_slice) {
                    self.tokens.push(Token {
                        value: TokenValue::Operator(token),
                        loc: Location { line: self.line },
                    });
                    return Ok(current + 1);
                }

                Err(LexError::UnknownToken {
                    char: single,
                    value: single as u32,
                    line: self.line,
                })
            }
        }
    }

    fn is_identifier(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }
}
