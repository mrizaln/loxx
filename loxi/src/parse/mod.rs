use std::iter::Peekable;
use std::ops::Deref;
use std::slice::Iter;

use thiserror::Error;

use crate::lex::{self, token as ltok};
use crate::util::{Location, TokLoc};

use expr::{
    macros::{group_expr, ref_expr, val_expr},
    Expr, RefExpr, ValExpr,
};
use stmt::Stmt;

use macros::{consume_no_eof, is_tok, syntax_error};

pub mod expr;
pub mod stmt;
mod test;
pub mod token;

///! Lox Grammar (unfinished)
///  ------------------------
/// program     -> declaration* EOF ;
/// declaration -> var_decl | statement ;
/// var_decl    -> "var" IDENTIFIER ( "=" expression)? ";" ;
/// statement   -> expr_stmt | print_stmt ;
/// block       -> "{" declaration* "}"
/// expr_stmt   -> expression ";" ;
/// print_stmt  -> "print" expression ";" ;
/// expression  -> assignment ;
/// assignment  -> IDENTIFIER "=" assignment | equality ;
/// equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term        -> factor ( ( "-" | "+" ) factor )* ;
/// factor      -> unary ( ( "/" | "*" ) unary )* ;
/// unary       -> ( "!" | "-" ) unary | primary ;
/// primary     -> NUMBER | STRING | "true" | "false" | "nil" | grouping | IDENTIFIER ;
/// grouping    -> "(" expression ")"

// TODO: Add more variation of the SyntaxError
#[derive(Debug, Error)]
#[error("{loc} SyntaxError: Expect '{expect}', got '{real}'")]
pub struct SyntaxError {
    pub expect: &'static str,
    pub real: &'static str,
    pub loc: Location,
}

pub enum ParseError {
    SyntaxError(SyntaxError),
    EndOfFile(Location),
}

impl ParseError {
    pub fn loc(&self) -> Location {
        match self {
            ParseError::SyntaxError(SyntaxError { loc, .. }) => *loc,
            ParseError::EndOfFile(loc) => *loc,
        }
    }

    /// Convert the variant to ParseError::SyntaxError if it is not already
    pub fn syntax_err(self, expect: &'static str) -> Self {
        match self {
            ParseError::SyntaxError(_) => self,
            ParseError::EndOfFile(loc) => syntax_error!(expect, "<eof>", loc),
        }
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, lex::Token>>,
    eof: Location,
}

pub struct Program {
    pub statements: Vec<Stmt>,
}

pub type ExprResult = Result<Box<Expr>, ParseError>;
pub type StmtResult = Result<Stmt, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [lex::Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            eof: tokens.last().expect("The slice must not be empty").loc(),
        }
    }

    pub fn parse(mut self) -> Result<Program, Vec<SyntaxError>> {
        let mut program = Program {
            statements: Vec::new(),
        };
        let mut errors = Vec::new();

        loop {
            match self.declaration() {
                Ok(stmt) => program.statements.push(stmt),
                Err(err) => match err {
                    ParseError::EndOfFile(_) => break,
                    ParseError::SyntaxError(err) => errors.push(err),
                },
            }
        }

        match errors.is_empty() {
            true => Ok(program),
            false => Err(errors),
        }
    }

    fn synchronize(&mut self) {
        while let Ok(tok) = self.peek() {
            match tok {
                lex::Token::Keyword(TokLoc { tok, .. }) => match tok {
                    ltok::Keyword::Class => break,
                    ltok::Keyword::If => break,
                    ltok::Keyword::Else => break,
                    ltok::Keyword::For => break,
                    ltok::Keyword::While => break,
                    ltok::Keyword::Fun => break,
                    ltok::Keyword::Print => break,
                    ltok::Keyword::Return => break,
                    ltok::Keyword::Var => break,
                    _ => (),
                },
                _ => (),
            }
            self.advance();
        }
    }

    fn declaration(&mut self) -> StmtResult {
        match self.peek()? {
            lex::Token::Keyword(TokLoc {
                tok: ltok::Keyword::Var,
                ..
            }) => {
                self.advance();
                self.var_declaration()
            }
            _ => self.statement(),
        }
        .map_err(|err| {
            self.synchronize();
            err
        })
    }

    fn var_declaration(&mut self) -> StmtResult {
        let (name, loc) = consume_no_eof! {
            self as ["<identifier>"]
            if   lex::Token::Literal(TokLoc { tok: ltok::Literal::Identifier(name), loc }),
            then (name.clone(), loc.clone()),       // TODO: move instead
        }?;
        self.advance(); // separate advance from consume! because both borrows self

        let init = consume_no_eof! {
            self as ["<expression>"]
            if   is_tok!(Operator::Equal),
            then {
                self.advance();
                Some(*(self.expression())?)
            },
            else None,
        }?;

        consume_no_eof! {
            self as [";"]
            if   is_tok!(Punctuation::Semicolon),
            then self.advance(),
        }?;

        Ok(Stmt::Var { loc, name, init })
    }

    fn statement(&mut self) -> StmtResult {
        match self.peek()? {
            is_tok!(Keyword::Print) => {
                let loc = self.advance().unwrap().loc();
                let expr = match self.expression_statement() {
                    Ok(Stmt::Expr { expr }) => Ok(expr),
                    Ok(_) => unreachable!(),
                    Err(ParseError::EndOfFile(loc)) => {
                        Err(syntax_error!("<expression>", "<eof", loc))
                    }
                    Err(err) => Err(err),
                }?;
                Ok(Stmt::Print { loc, expr })
            }
            is_tok!(Punctuation::BraceLeft) => {
                let loc = self.advance().unwrap().loc();
                self.block(loc)
            }
            _ => self.expression_statement(),
        }
    }

    fn block(&mut self, _start: Location) -> StmtResult {
        let mut statements = Vec::new();

        loop {
            match self.peek().map_err(|e| e.syntax_err("<statement>"))? {
                is_tok!(Punctuation::BraceRight) => break,
                _ => statements.push(self.declaration()?),
            }
        }

        // TODO: tell the location of the starting brace on the error message
        consume_no_eof! {
            self as ["}"]
            if   is_tok!(Punctuation::BraceRight),
            then self.advance(),
        }?;

        Ok(Stmt::Block { statements })
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression()?;

        match self.peek()? {
            is_tok!(Punctuation::Semicolon) => {
                self.advance();
                return Ok(Stmt::Expr { expr: *expr });
            }
            tok => Err(syntax_error!(
                (&ltok::Punctuation::Semicolon).into(),
                tok.static_str(),
                tok.loc()
            )),
        }
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn binary<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&lex::Token) -> Option<TokLoc<token::BinaryOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;

        while let Some(op) = curr(self.peek()?) {
            self.advance();
            expr = Box::new(val_expr!(Binary {
                left: expr,
                operator: op,
                right: inner(self).map_err(|err| err.syntax_err("<expression>"))?,
            }));
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.equality()?;

        match self.peek()? {
            is_tok!(Operator::Equal) => {
                let loc = self.advance().unwrap().loc();
                let value = self.assignment()?;

                match *expr {
                    Expr::RefExpr(lvalue) => match lvalue {
                        RefExpr::Variable { var } => {
                            Ok(Box::new(ref_expr!(Assignment { var, value })))
                        }
                        RefExpr::Assignment { .. } => unreachable!(),
                        RefExpr::Grouping { .. } => Err(syntax_error!("<lvalue>", "<group>", loc)),
                    },
                    Expr::ValExpr(_) => Err(syntax_error!("<lvalue>", "<rvalue>", loc)),
                }
            }
            _ => Ok(expr),
        }
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
        if let Some(op) = conv::to_unary(self.peek()?) {
            self.advance();
            Ok(Box::new(val_expr!(Unary {
                operator: op,
                right: self.unary()?,
            })))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ExprResult {
        let curr = self.advance().expect("Unexpected end of file");
        let loc = curr.loc();

        enum Lit {
            Lit(token::Literal),
            Var(token::Variable),
            No(&'static str),
        }

        let lit = match curr {
            lex::Token::Keyword(TokLoc { tok, .. }) => match tok {
                ltok::Keyword::True => Lit::Lit(token::Literal::True),
                ltok::Keyword::False => Lit::Lit(token::Literal::False),
                ltok::Keyword::Nil => Lit::Lit(token::Literal::Nil),
                _ => Lit::No(tok.into()),
            },
            lex::Token::Literal(TokLoc { tok, .. }) => match tok {
                ltok::Literal::String(str) => Lit::Lit(token::Literal::String(str.clone())), // TODO: move instead
                ltok::Literal::Number(num) => Lit::Lit(token::Literal::Number(*num)),
                ltok::Literal::Identifier(name) => Lit::Var(token::Variable { name: name.clone() }),
            },
            lex::Token::Punctuation(TokLoc { tok, .. }) => match tok {
                ltok::Punctuation::ParenLeft => {
                    let expr = self.expression()?;
                    match self.peek()? {
                        lex::Token::Punctuation(TokLoc {
                            tok: ltok::Punctuation::ParenRight,
                            ..
                        }) => {
                            self.advance();
                            return Ok(Box::new(group_expr!(*expr)));
                        }
                        tok => Lit::No(tok.static_str()),
                    }
                }
                _ => Lit::No(tok.into()),
            },
            lex::Token::Eof(_) => return Err(ParseError::EndOfFile(loc)),
            _ => Lit::No(curr.static_str()),
        };

        match lit {
            Lit::Lit(lit) => Ok(Box::new(val_expr!(Literal {
                value: TokLoc { tok: lit, loc },
            }))),
            Lit::Var(var) => Ok(Box::new(ref_expr!(Variable {
                var: TokLoc { tok: var, loc },
            }))),
            Lit::No(str) => Err(syntax_error!("<expression>", str, loc)),
        }
    }

    fn peek(&mut self) -> Result<&lex::Token, ParseError> {
        match self.tokens.peek().map(&Deref::deref) {
            Some(lex::Token::Eof(_)) => Err(ParseError::EndOfFile(self.eof)),
            None => Err(ParseError::EndOfFile(self.eof)),
            Some(tok) => Ok(tok),
        }
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
                ltok::Operator::BangEqual => token::BinaryOp::NotEqual,
                ltok::Operator::EqualEqual => token::BinaryOp::Equal,
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
                ltok::Operator::Greater => token::BinaryOp::Greater,
                ltok::Operator::GreaterEqual => token::BinaryOp::GreaterEq,
                ltok::Operator::Less => token::BinaryOp::Less,
                ltok::Operator::LessEqual => token::BinaryOp::LessEq,
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
                ltok::Operator::Plus => token::BinaryOp::Add,
                ltok::Operator::Minus => token::BinaryOp::Sub,
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
                ltok::Operator::Star => token::BinaryOp::Mul,
                ltok::Operator::Slash => token::BinaryOp::Div,
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
                ltok::Operator::Bang => token::UnaryOp::Not,
                ltok::Operator::Minus => token::UnaryOp::Minus,
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

mod macros {
    macro_rules! syntax_error {
        ($expect:expr, $real:expr, $loc:expr) => {
            ParseError::SyntaxError(SyntaxError {
                expect: $expect,
                real: $real,
                loc: $loc,
            })
        };
    }

    macro_rules! consume_no_eof {
        ($self:ident as [$name:expr] if $tok:pat, then $xpr:expr,) => {
            match $self.peek() {
                Ok($tok) => Ok($xpr),
                Ok(tok) => Err(syntax_error!($name, tok.static_str(), tok.loc())),
                Err(ParseError::EndOfFile(loc)) => Err(syntax_error!($name, "<eof>", loc)),
                Err(err) => Err(err),
            }
        };
        ($self:ident as [$name:expr] if $tok:pat, then $xpr1:expr, else $xpr2:expr,) => {
            match $self.peek() {
                Ok($tok) => Ok($xpr1),
                Ok(_) => Ok($xpr2),
                Err(ParseError::EndOfFile(loc)) => Err(syntax_error!($name, "<eof>", loc)),
                Err(err) => Err(err),
            }
        };
    }

    macro_rules! ltokl {
        ($type:ident::$name:ident) => {
            lex::Token::$type(TokLoc {
                tok: ltok::$type::$name,
                ..
            })
        };
    }

    pub(crate) use consume_no_eof;
    pub(crate) use ltokl as is_tok;
    pub(crate) use syntax_error;
}
