use std::iter::Peekable;
use std::ops::Deref;
use std::slice::Iter;

use crate::lex;
use crate::util::{Location, TokLoc};

use self::expr::Expr;

pub mod expr;
mod test;
pub mod token;

///! Lox Grammar (unfinished)
///  ------------------------
/// expression -> equality ;
/// equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term       -> factor ( ( "-" | "+" ) factor )* ;
/// factor     -> unary ( ( "/" | "*" ) unary )* ;
/// unary      -> ( "!" | "-" ) unary | primary ;
/// primary    -> NUMBER | STRING | "true" | "false" | "nil" | grouping ;
/// grouping   -> "(" expression ")"

pub enum ParseError {
    SyntaxError {
        expect: &'static str,
        real: &'static str,
        loc: Location,
    },
    EndOfFile,
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, lex::Token>>,
    errors: Vec<ParseError>,
    panic_mode: bool,
}

pub type ParseResult = Result<Box<Expr>, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<lex::Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            errors: Vec::new(),
            panic_mode: false,
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        self.expression()
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    fn binary<F1, F2>(&mut self, curr: F1, inner: F2) -> ParseResult
    where
        F1: Fn(&lex::Token) -> Option<TokLoc<token::BinaryOp>>,
        F2: Fn(&mut Self) -> ParseResult,
    {
        let mut expr = inner(self)?;

        while let Some(op) = self.peek().and_then(&curr) {
            self.advance();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator: op,
                right: inner(self)?,
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult {
        self.binary(conv::to_equality, Self::comparison)
    }

    fn comparison(&mut self) -> ParseResult {
        self.binary(conv::to_comparison, Self::term)
    }

    fn term(&mut self) -> ParseResult {
        self.binary(conv::to_term, Self::factor)
    }

    fn factor(&mut self) -> ParseResult {
        self.binary(conv::to_factor, Self::unary)
    }

    fn unary(&mut self) -> ParseResult {
        if let Some(op) = self.peek().and_then(conv::to_unary) {
            self.advance();
            Ok(Box::new(Expr::Unary {
                operator: op,
                right: self.unary()?,
            }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseResult {
        let curr = self.advance().ok_or(ParseError::EndOfFile)?;
        let loc = curr.loc();

        enum Lit {
            Yes(token::Literal),
            No(&'static str),
        }

        let lit = match curr {
            lex::Token::Keyword(TokLoc { tok, .. }) => match tok {
                lex::token::Keyword::True => todo!(),
                lex::token::Keyword::False => todo!(),
                lex::token::Keyword::Nil => todo!(),
                _ => Lit::No(tok.into()),
            },
            lex::Token::Literal(TokLoc { tok, .. }) => match tok {
                lex::token::Literal::String(str) => Lit::Yes(token::Literal::String(str.clone())), // TODO: move instead
                lex::token::Literal::Number(num) => Lit::Yes(token::Literal::Number(*num)),
                lex::token::Literal::Identifier(_) => unimplemented!(), // TODO: implement
            },
            lex::Token::Punctuation(TokLoc { tok, .. }) => match tok {
                lex::token::Punctuation::ParenLeft => {
                    let expr = self.expression()?;
                    match self.peek() {
                        Some(lex::Token::Punctuation(TokLoc {
                            tok: lex::token::Punctuation::ParenRight,
                            loc: _,
                        })) => {
                            return Ok(Box::new(Expr::Grouping { expr }));
                        }
                        _ => Lit::No((&lex::token::Punctuation::ParenRight).into()),
                    }
                }
                _ => Lit::No(curr.static_str()),
            },
            _ => Lit::No(curr.static_str()),
        };

        match lit {
            Lit::Yes(lit) => Ok(Box::new(Expr::Literal {
                value: TokLoc { tok: lit, loc },
            })),
            Lit::No(str) => Err(ParseError::SyntaxError {
                expect: "<literal>",
                real: str,
                loc,
            }),
        }
    }

    fn peek(&mut self) -> Option<&lex::Token> {
        self.tokens.peek().map(&Deref::deref)
    }

    fn advance(&mut self) -> Option<&lex::Token> {
        self.tokens.next()
    }

    fn advance_while<F>(&mut self, pred: F) -> usize
    where
        F: Fn(&lex::Token) -> bool,
    {
        let mut count = 0;
        while let Some(true) = self.peek().map(&pred) {
            count += 1;
        }
        count
    }
}

mod conv {
    use super::*;

    pub fn to_equality(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            lex::Token::Operator(TokLoc { tok, .. }) => match tok {
                lex::token::Operator::BangEqual => token::BinaryOp::NotEqual,
                lex::token::Operator::EqualEqual => token::BinaryOp::Equal,
                _ => return None,
            },
            _ => return None,
        };

        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_comparison(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            lex::Token::Operator(TokLoc { tok, .. }) => match tok {
                lex::token::Operator::Greater => token::BinaryOp::Greater,
                lex::token::Operator::GreaterEqual => token::BinaryOp::GreaterEq,
                lex::token::Operator::Less => token::BinaryOp::Less,
                lex::token::Operator::LessEqual => token::BinaryOp::LessEq,
                _ => return None,
            },
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_term(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            lex::Token::Operator(TokLoc { tok, .. }) => match tok {
                lex::token::Operator::Plus => token::BinaryOp::Add,
                lex::token::Operator::Minus => token::BinaryOp::Sub,
                _ => return None,
            },
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_factor(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            lex::Token::Operator(TokLoc { tok, .. }) => match tok {
                lex::token::Operator::Star => token::BinaryOp::Mul,
                lex::token::Operator::Slash => token::BinaryOp::Div,
                _ => return None,
            },
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_unary(tok: &lex::Token) -> Option<TokLoc<token::UnaryOp>> {
        let new_tok = match tok {
            lex::Token::Operator(TokLoc { tok, .. }) => match tok {
                lex::token::Operator::Bang => token::UnaryOp::Not,
                lex::token::Operator::Minus => token::UnaryOp::Minus,
                _ => return None,
            },
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }
}
