use core::panic;
use std::iter::Peekable;
use std::ops::Deref;
use std::slice::Iter;

use thiserror::Error;

use crate::lex;
use crate::util::{Location, TokLoc};

use expr::Expr;
use stmt::Stmt;

pub mod expr;
pub mod stmt;
mod test;
pub mod token;

///! Lox Grammar (unfinished)
///  ------------------------
/// program    -> statement* EOF ;
/// statement  -> expr_stmt | print_stmt ;
/// expr_stmt  -> expression ";" ;
/// print_stmt -> "print" expression ";" ;
/// expression -> equality ;
/// equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term       -> factor ( ( "-" | "+" ) factor )* ;
/// factor     -> unary ( ( "/" | "*" ) unary )* ;
/// unary      -> ( "!" | "-" ) unary | primary ;
/// primary    -> NUMBER | STRING | "true" | "false" | "nil" | grouping ;
/// grouping   -> "(" expression ")"

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("{loc} SyntaxError: Expect '{expect}', got '{real}'")]
    SyntaxError {
        expect: &'static str,
        real: &'static str,
        loc: Location,
    },

    #[error("ParseError: Unexpected end of file '<eof>'")]
    EndOfFile(Location),

    #[error("SyntaxErrror: Empty expression is not allowed")]
    EmptyExpr(Location),
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, lex::Token>>,
}

pub struct Program {
    pub statements: Vec<Stmt>,
}

pub type ExprResult = Result<Box<Expr>, ParseError>;
pub type StmtResult = Result<Stmt, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<lex::Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Program, ParseError> {
        let mut program = Program {
            statements: Vec::new(),
        };
        while let Some(_) = self.peek() {
            match self.statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => match err {
                    ParseError::EndOfFile(_) => break,
                    _ => return Err(err),
                },
            }
        }
        Ok(program)
    }

    fn synchronize(&mut self) {
        while let Some(tok) = self.peek() {
            match tok {
                lex::Token::Keyword(TokLoc { tok, .. }) => match tok {
                    lex::token::Keyword::Class => break,
                    lex::token::Keyword::If => break,
                    lex::token::Keyword::Else => unimplemented!(),
                    lex::token::Keyword::For => break,
                    lex::token::Keyword::While => break,
                    lex::token::Keyword::Fun => break,
                    lex::token::Keyword::Print => break,
                    lex::token::Keyword::Return => break,
                    lex::token::Keyword::Var => break,
                    _ => (),
                },
                _ => (),
            }
            self.advance();
        }
    }

    fn statement(&mut self) -> StmtResult {
        match self.peek() {
            Some(tok) => match tok {
                lex::Token::Keyword(TokLoc {
                    tok: lex::token::Keyword::Print,
                    ..
                }) => {
                    let loc = self.advance().unwrap().loc();
                    let expr = match self.expression_statement() {
                        Ok(expr) => match expr {
                            Stmt::Expr(expr) => expr,
                            _ => panic!("Unexpected statement"),
                        },
                        Err(err) => match err {
                            ParseError::EmptyExpr(loc) => Err(ParseError::SyntaxError {
                                expect: "<expression>",
                                real: "<empty>",
                                loc,
                            }),
                            _ => Err(err),
                        }?,
                    };
                    return Ok(Stmt::Print { loc, expr });
                }
                _ => self.expression_statement(),
            },
            None => panic!("Unexpected end of file"),
        }
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;
        match self.peek() {
            Some(lex::Token::Punctuation(TokLoc {
                tok: lex::token::Punctuation::Semicolon,
                loc: _,
            })) => {
                self.advance();
                return Ok(Stmt::Expr(*expr));
            }
            Some(tok) => Err(ParseError::SyntaxError {
                expect: (&lex::token::Punctuation::Semicolon).into(),
                real: tok.static_str(),
                loc: tok.loc(),
            }),
            None => panic!("Unexpected end of file"),
        }
    }

    fn expression(&mut self) -> ExprResult {
        self.equality()
    }

    fn binary<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&lex::Token) -> Option<TokLoc<token::BinaryOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;

        while let Some(op) = self.peek().and_then(&curr) {
            self.advance();
            expr = Box::new(Expr::Binary {
                left: expr,
                operator: op,
                right: match inner(self) {
                    Ok(expr) => expr,
                    Err(err) => match err {
                        ParseError::EndOfFile(loc) => Err(ParseError::SyntaxError {
                            expect: "<expression>",
                            real: "<eof>",
                            loc,
                        })?,
                        ParseError::EmptyExpr(loc) => Err(ParseError::SyntaxError {
                            expect: "<expression>",
                            real: "<empty>",
                            loc,
                        })?,
                        _ => return Err(err),
                    },
                },
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ExprResult {
        self.binary(conv::to_equality, Self::comparison)
    }

    fn comparison(&mut self) -> ExprResult {
        self.binary(conv::to_comparison, Self::term)
    }

    fn term(&mut self) -> ExprResult {
        self.binary(conv::to_term, Self::factor)
    }

    fn factor(&mut self) -> ExprResult {
        self.binary(conv::to_factor, Self::unary)
    }

    fn unary(&mut self) -> ExprResult {
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

    fn primary(&mut self) -> ExprResult {
        let curr = self.advance().expect("Unexpected end of file");
        let loc = curr.loc();

        enum Lit {
            Yes(token::Literal),
            No(&'static str),
        }

        let lit = match curr {
            lex::Token::Keyword(TokLoc { tok, .. }) => match tok {
                lex::token::Keyword::True => Lit::Yes(token::Literal::True),
                lex::token::Keyword::False => Lit::Yes(token::Literal::False),
                lex::token::Keyword::Nil => Lit::Yes(token::Literal::Nil),
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
                            self.advance();
                            return Ok(Box::new(Expr::Grouping { expr }));
                        }
                        Some(tok) => Lit::No(tok.static_str()),
                        None => return Err(ParseError::EndOfFile(loc)),
                    }
                }
                _ => return Err(ParseError::EmptyExpr(loc)),
            },
            lex::Token::Eof(_) => return Err(ParseError::EndOfFile(loc)),
            _ => return Err(ParseError::EmptyExpr(loc)),
        };

        match lit {
            Lit::Yes(lit) => Ok(Box::new(Expr::Literal {
                value: TokLoc { tok: lit, loc },
            })),
            Lit::No(str) => Err(ParseError::SyntaxError {
                expect: "<expression>",
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
