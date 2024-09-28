use std::collections::VecDeque;
use thiserror::Error;

use crate::lex::{self, token as ltok};
use crate::util::{Location, TokLoc};

use expr::{
    macros::{group_expr, ref_expr, val_expr},
    Expr, RefExpr, ValExpr,
};
use stmt::Stmt;

use macros::{is_tok, missing_delim, peek_no_eof, syntax_error};

pub mod expr;
pub mod stmt;
mod test;
pub mod token;

///! Lox Grammar (unfinished)
///  ------------------------
/// program     -> declaration* EOF ;
/// declaration -> var_decl | statement ;
/// var_decl    -> "var" IDENTIFIER ( "=" expression)? ";" ;
/// statement   -> expr_stmt | if_stmt | print_stmt | while_stmt | block ;
/// block       -> "{" declaration* "}"
/// expr_stmt   -> expression ";" ;
/// if_stmt     -> "if" "(" expression ")" statement ( "else" statement )? ;
/// print_stmt  -> "print" expression ";" ;
/// while_stmt  -> "while" "(" expression ")" statement ;
/// expression  -> assignment ;
/// assignment  -> IDENTIFIER "=" assignment | logical_or ;
/// logical_or  -> logical_and ( "or" logical_and )* ;
/// logical_and -> equality ( "and" equality )* ;
/// equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term        -> factor ( ( "-" | "+" ) factor )* ;
/// factor      -> unary ( ( "/" | "*" ) unary )* ;
/// unary       -> ( "!" | "-" ) unary | primary ;
/// primary     -> NUMBER | STRING | "true" | "false" | "nil" | grouping | IDENTIFIER ;
/// grouping    -> "(" expression ")"

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("{loc} SyntaxError: Expect '{expect}', got '{real}'")]
    Expect {
        expect: &'static str,
        real: &'static str,
        loc: Location,
    },

    #[error("{start} SyntaxError: Missing closing delimiter '{delim}'")]
    MissingDelim {
        delim: &'static str,
        start: Location,
    },
}

impl SyntaxError {
    pub fn loc(&self) -> Location {
        match self {
            SyntaxError::Expect { loc, .. } => *loc,
            SyntaxError::MissingDelim { start, .. } => *start,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(SyntaxError),
    EndOfFile(Location),
}

impl ParseError {
    /// Convert the variant to ParseError::SyntaxError(SyntaxError::Expect)
    pub fn syntax_err(self, expect: &'static str) -> Self {
        match self {
            ParseError::EndOfFile(loc) => syntax_error!(expect, "<eof>", loc),
            _ => self,
        }
    }

    /// convert the variant to ParseError::SyntaxError(SyntaxError::MissingDelim)
    pub fn missing_delim(self, delim: &'static str, start: Location) -> Self {
        match self {
            ParseError::EndOfFile(_) => {
                ParseError::SyntaxError(SyntaxError::MissingDelim { start, delim })
            }
            _ => self,
        }
    }
}

pub struct Parser {
    tokens: VecDeque<lex::Token>,
    errors: Vec<SyntaxError>,
    current: Option<lex::Token>,
}

pub struct Program {
    pub statements: Vec<Stmt>,
}

pub type ExprResult = Result<Box<Expr>, ParseError>;
pub type StmtResult = Result<Stmt, ParseError>;

impl Parser {
    pub fn new() -> Self {
        Self {
            tokens: VecDeque::new(),
            errors: Vec::new(),
            current: None,
        }
    }

    pub fn parse(&mut self, tokens: Vec<lex::Token>) -> Result<Program, Vec<SyntaxError>> {
        self.tokens = VecDeque::from(tokens);

        let mut program = Program {
            statements: Vec::new(),
        };

        while let Some(stmt) = self.declaration() {
            program.statements.push(stmt);
        }

        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn synchronize(&mut self) {
        // discard anything that previously produces error
        self.advance();

        while let Ok(tok) = self.peek() {
            match tok {
                is_tok!(Keyword::Class) => break,
                is_tok!(Keyword::If) => break,
                is_tok!(Keyword::Else) => break,
                is_tok!(Keyword::For) => break,
                is_tok!(Keyword::While) => break,
                is_tok!(Keyword::Fun) => break,
                is_tok!(Keyword::Print) => break,
                is_tok!(Keyword::Return) => break,
                is_tok!(Keyword::Var) => break,
                _ => (),
            }
            self.advance();
        }
    }

    /// this function only returns Err when it reaches EOF
    fn declaration(&mut self) -> Option<Stmt> {
        if let Ok(tok) = self.peek() {
            let stmt = match tok {
                is_tok!(Keyword::Var) => {
                    self.advance();
                    self.var_declaration()
                }
                _ => self.statement(),
            };
            match stmt {
                Ok(stmt) => Some(stmt),
                Err(err) => {
                    match err {
                        ParseError::SyntaxError(err) => self.errors.push(err),
                        ParseError::EndOfFile(_) => (),
                    };
                    self.synchronize();
                    None
                }
            }
        } else {
            None
        }
    }

    fn var_declaration(&mut self) -> StmtResult {
        // TODO: add edge case check when the variable initializer has identifier with the same name
        //       and make it a SyntaxError

        let (name, loc) = peek_no_eof! { self as ["<identifier>"]
            if is_tok!(Literal::Identifier(name, loc)) => (name.clone(), loc.clone()),
        }?;
        self.advance();

        let init = peek_no_eof! { self as ["; or ="]
            if is_tok!(Operator::Equal) => {
                self.advance();
                Some(*self.expression().map_err(|err|err.syntax_err("<expression>"))?)
            },
            else tok => match tok {
                is_tok!(Punctuation::Semicolon) => None,
                _ => Err(syntax_error!("; or =", tok.static_str(), tok.loc()))?,
            },
        }?;

        let _ = peek_no_eof! { self as [";"]
            if is_tok!(Punctuation::Semicolon) => self.advance(),
        }?;

        Ok(Stmt::Var { loc, name, init })
    }

    fn statement(&mut self) -> StmtResult {
        match self.peek()? {
            is_tok!(Keyword::Print) => {
                let loc = self.advance().unwrap().loc();
                self.print_statement(loc)
            }
            is_tok!(Punctuation::BraceLeft) => {
                let loc = self.advance().unwrap().loc();
                self.block(loc)
            }
            is_tok!(Keyword::If) => {
                let loc = self.advance().unwrap().loc();
                self.if_statement(loc)
            }
            is_tok!(Keyword::While) => {
                let loc = self.advance().unwrap().loc();
                self.while_statement(loc)
            }
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self, loc: Location) -> StmtResult {
        let expr = match self.expression_statement()? {
            Stmt::Expr { expr } => expr,
            _ => panic!("Should be expr"),
        };
        Ok(Stmt::Print { loc, expr })
    }

    fn while_statement(&mut self, loc: Location) -> StmtResult {
        peek_no_eof! { self as ["("] if is_tok!(Punctuation::ParenLeft) => self.advance(), }?;
        let condition = self.expression()?;
        peek_no_eof! { self as [")"] if is_tok!(Punctuation::ParenRight) => self.advance(), }?;
        let body = self.statement()?;

        Ok(Stmt::While {
            loc,
            condition: *condition,
            body: Box::new(body),
        })
    }

    fn block(&mut self, start: Location) -> StmtResult {
        let mut statements = Vec::new();

        // this loop can only stop if EndOfFile or BraceRight encountered
        while let Ok(tok) = self.peek() {
            match tok {
                is_tok!(Punctuation::BraceRight) => break,
                _ => match self.declaration() {
                    Some(decl) => statements.push(decl),
                    None => continue,
                },
            }
        }

        if let Ok(is_tok!(Punctuation::BraceRight)) = self.peek() {
            self.advance();
            Ok(Stmt::Block { statements })
        } else {
            Err(missing_delim!("}", start))
        }
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression().map_err(|e| e.syntax_err(";"))?;
        let _ = peek_no_eof! { self as [";"]
            if is_tok!(Punctuation::Semicolon) => self.advance(),
        }?;
        Ok(Stmt::Expr { expr: *expr })
    }

    fn if_statement(&mut self, loc: Location) -> StmtResult {
        peek_no_eof! { self as ["("] if is_tok!(Punctuation::ParenLeft) => self.advance(), }?;
        let condition = self.expression()?;
        peek_no_eof! { self as [")"] if is_tok!(Punctuation::ParenRight) => self.advance(), }?;

        let then = self.statement()?;
        let otherwise = match self.peek() {
            Ok(is_tok!(Keyword::Else)) => {
                self.advance();
                Some(self.statement()?)
            }
            _ => None,
        };

        Ok(Stmt::If {
            loc,
            condition: *condition,
            then: Box::new(then),
            otherwise: otherwise.map(|v| Box::new(v)),
        })
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.logical_or()?;

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

    fn logical_or(&mut self) -> ExprResult {
        self.logical(
            |tok| conv::to_logical(tok, token::LogicalOp::Or),
            Self::logical_and,
        )
    }

    fn logical_and(&mut self) -> ExprResult {
        self.logical(
            |tok| conv::to_logical(tok, token::LogicalOp::And),
            Self::equality,
        )
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

        let lit = |lit| val_expr! { Literal { value: TokLoc { tok: lit, loc } } };
        let var = |name| ref_expr! { Variable { var: TokLoc { tok: token::Variable { name: name }, loc } } };

        type Lit = token::Literal;

        let expr = match curr {
            is_tok!(Keyword::True) => lit(Lit::True),
            is_tok!(Keyword::False) => lit(Lit::False),
            is_tok!(Keyword::Nil) => lit(Lit::Nil),

            is_tok!(Literal::String(str, _)) => lit(Lit::String(str.clone())),
            is_tok!(Literal::Number(num, _)) => lit(Lit::Number(*num)),
            is_tok!(Literal::Identifier(name, _)) => var(name.clone()),

            is_tok!(Punctuation::ParenLeft) => {
                let expr = self.expression().map_err(|e| e.missing_delim(")", loc))?;
                match self.peek().map_err(|e| e.missing_delim(")", loc))? {
                    is_tok!(Punctuation::ParenRight) => self.advance(),
                    _ => Err(missing_delim!(")", loc))?,
                };
                group_expr!(*expr)
            }

            lex::Token::Eof(_) => return Err(ParseError::EndOfFile(loc)),
            _ => return Err(syntax_error!("<expression", curr.static_str(), loc)),
        };

        Ok(Box::new(expr))
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
                right: inner(self).map_err(|e| e.syntax_err("<expression>"))?,
            }));
        }

        Ok(expr)
    }

    fn logical<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&lex::Token) -> Option<TokLoc<token::LogicalOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;

        while let Some(kind) = curr(self.peek()?) {
            self.advance();
            expr = Box::new(val_expr!(Logical {
                left: expr,
                kind: kind,
                right: inner(self).map_err(|e| e.syntax_err("<expression>"))?,
            }));
        }

        Ok(expr)
    }

    fn peek(&mut self) -> Result<&lex::Token, ParseError> {
        match self.tokens.front() {
            // out of bound read is considered as EOF at invalid location [0:0]
            None => Err(ParseError::EndOfFile(Location::default())),
            Some(lex::Token::Eof(loc)) => Err(ParseError::EndOfFile(*loc)),
            Some(tok) => Ok(tok),
        }
    }

    fn advance(&mut self) -> Option<&lex::Token> {
        self.current = self.tokens.pop_front();
        self.current.as_ref()
    }
}

mod conv {
    use super::*;

    pub fn to_equality(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::BangEqual) => token::BinaryOp::NotEqual,
            is_tok!(Operator::EqualEqual) => token::BinaryOp::Equal,
            _ => return None,
        };

        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_comparison(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Greater) => token::BinaryOp::Greater,
            is_tok!(Operator::GreaterEqual) => token::BinaryOp::GreaterEq,
            is_tok!(Operator::Less) => token::BinaryOp::Less,
            is_tok!(Operator::LessEqual) => token::BinaryOp::LessEq,
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_term(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Plus) => token::BinaryOp::Add,
            is_tok!(Operator::Minus) => token::BinaryOp::Sub,
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_factor(tok: &lex::Token) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Star) => token::BinaryOp::Mul,
            is_tok!(Operator::Slash) => token::BinaryOp::Div,
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_unary(tok: &lex::Token) -> Option<TokLoc<token::UnaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Bang) => token::UnaryOp::Not,
            is_tok!(Operator::Minus) => token::UnaryOp::Minus,
            _ => return None,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_logical(
        tok: &lex::Token,
        kind: token::LogicalOp,
    ) -> Option<TokLoc<token::LogicalOp>> {
        let new_tok = match tok {
            is_tok!(Keyword::And) => token::LogicalOp::And,
            is_tok!(Keyword::Or) => token::LogicalOp::Or,
            _ => return None,
        };

        match new_tok == kind {
            true => Some(TokLoc {
                tok: new_tok,
                loc: tok.loc(),
            }),
            false => None,
        }
    }
}

mod macros {
    /// convenience macro for creating a `ParseError::SyntaxError`
    macro_rules! syntax_error {
        ($expect:expr, $real:expr, $loc:expr) => {
            ParseError::SyntaxError(SyntaxError::Expect {
                expect: $expect,
                real: $real,
                loc: $loc,
            })
        };
    }

    macro_rules! missing_delim {
        ($delim:expr, $start:expr) => {
            ParseError::SyntaxError(SyntaxError::MissingDelim {
                start: $start,
                delim: $delim,
            })
        };
    }

    /// peek `$self` for the next token, if it is not the expected token (`$tok`) or the end of
    /// file, return a `ParseError::SyntaxError`
    macro_rules! peek_no_eof {
        ($self:ident as [$name:expr] if $tok:pat => $xpr:expr,) => {
            match $self.peek() {
                Ok($tok) => Ok($xpr),
                Ok(tok) => Err(syntax_error!($name, tok.static_str(), tok.loc())),
                Err(err) => Err(err.syntax_err($name)),
            }
        };
        ($self:ident as [$name:expr] if $tok:pat => $xpr1:expr, else $other:ident => $xpr2:expr,) => {
            match $self.peek() {
                Ok($tok) => Ok($xpr1),
                Ok($other) => Ok($xpr2),
                Err(err) => Err(err.syntax_err($name)),
            }
        };
    }

    /// convenience macro for creating a `lex::Token::$name(TokLoc { tok: ltok::$name::$tok, .. })`
    macro_rules! ltokl {
        ($type:ident::$name:ident) => {
            lex::Token::$type(TokLoc {
                tok: ltok::$type::$name,
                ..
            })
        };
        ($type:ident::$name:ident($tok:tt, $loc:tt)) => {
            lex::Token::$type(TokLoc {
                tok: ltok::$type::$name($tok),
                loc: $loc,
            })
        };
    }

    pub(crate) use ltokl as is_tok;
    pub(crate) use missing_delim;
    pub(crate) use peek_no_eof;
    pub(crate) use syntax_error;
}
