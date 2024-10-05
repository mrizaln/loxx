//! Lox Grammar (unfinished)
//!  ------------------------
//! program     -> declaration* EOF ;
//!
//! declaration -> class_decl
//!                 | fun_decl
//!                 | var_decl
//!                 | statement ;
//!
//! class_decl  -> "class" IDENTIFIER "{" function* "}" ;
//!
//! fun_decl    -> "fun" function ;
//!
//! function    -> IDENTIFIER "(" parameters? ")" block ;
//!
//! parameters  -> IDENTIFIER ( "," IDENTIFIER )* ;
//!
//! var_decl    -> "var" IDENTIFIER ( "=" expression)? ";" ;
//!
//! statement   -> expr_stmt
//!                 | for_stmt
//!                 | if_stmt
//!                 | print_stmt
//!                 | return_stmt
//!                 | while_stmt
//!                 | block ;
//!
//! block       -> "{" declaration* "}"
//!
//! expr_stmt   -> expression ";" ;
//!
//! if_stmt     -> "if" "(" expression ")" statement ( "else" statement )? ;
//!
//! print_stmt  -> "print" expression ";" ;
//!
//! while_stmt  -> "while" "(" expression ")" statement ;
//!
//! for_stmt    -> "for" "(" (var_decl | expr_stmt | ";")
//!                 expression? ";"
//!                 expression? ")" statement ;
//!
//! return_stmt -> "return" expression? ";" ;
//!
//! expression  -> assignment ;
//!
//! assignment  -> (call "." )? IDENTIFIER "=" assignment
//!                 | logical_or ;
//!
//! logical_or  -> logical_and ( "or" logical_and )* ;
//!
//! logical_and -> equality ( "and" equality )* ;
//!
//! equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
//!
//! comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//!
//! term        -> factor ( ( "-" | "+" ) factor )* ;
//!
//! factor      -> unary ( ( "/" | "*" ) unary )* ;
//!
//! unary       -> ( "!" | "-" ) unary | call ;
//!
//! call        -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
//!
//! arguments   -> expression ( "," expression )* ;
//!
//! primary     -> NUMBER
//!                 | STRING
//!                 | "true"
//!                 | "false"
//!                 | "nil"
//!                 | grouping
//!                 | IDENTIFIER ;
//!
//! grouping    -> "(" expression ")"

use std::collections::VecDeque;
use std::fmt::Display;
use thiserror::Error;

use crate::interp::interner::{Interner, Key};
use crate::lex::{self, token as ltok};
use crate::util::{Location, TokLoc};

use expr::{Expr, RefExpr};
use stmt::Stmt;

use macros::{is_tok, missing_delim, peek_no_eof, syntax_error};

use self::stmt::StmtFunction;

pub mod expr;
pub mod stmt;
pub mod token;

#[cfg(test)]
mod test;

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

    #[error("{loc} SyntaxError: Number of arguments exceed language limit ({num} exceed {limit})")]
    TooManyArguments {
        num: usize,
        limit: usize,
        loc: Location,
    },
}

impl SyntaxError {
    pub fn loc(&self) -> Location {
        match self {
            SyntaxError::Expect { loc, .. } => *loc,
            SyntaxError::MissingDelim { start, .. } => *start,
            SyntaxError::TooManyArguments { loc, .. } => *loc,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(SyntaxError),
    EndOfFile(Location),
}

impl ParseError {
    pub fn too_many_args(num: usize, loc: Location) -> ParseError {
        ParseError::SyntaxError(SyntaxError::TooManyArguments {
            num,
            limit: Expr::MAX_FUNC_ARGS,
            loc,
        })
    }

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

pub struct DisplayedProgram<'a, 'b> {
    program: &'a Program,
    interner: &'b Interner,
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
                is_tok!(Keyword::Fun) => {
                    let loc = self.advance().unwrap().loc();
                    self.function_declaration(loc)
                        .map(|func| Stmt::Function { func })
                }
                is_tok!(Keyword::Class) => {
                    let loc = self.advance().unwrap().loc();
                    self.class_declaration(loc)
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

    fn class_declaration(&mut self, loc: Location) -> StmtResult {
        let name = peek_no_eof! { self as ["<identifier>"]
            if is_tok!(Literal::Identifier(str, _)) => *str,
        }?;
        self.advance();
        peek_no_eof! { self as ["{"] if is_tok!(Punctuation::BraceLeft) => self.advance(), }?;

        let mut methods = Vec::new();
        loop {
            match self.peek() {
                Err(err) => Err(err.syntax_err("fun or }"))?,
                Ok(is_tok!(Punctuation::BraceRight)) => break,
                Ok(tok) => {
                    let loc = tok.loc();
                    methods.push(self.function_declaration(loc)?);
                }
            };
        }

        peek_no_eof! { self as ["}"] if is_tok!(Punctuation::BraceRight) => self.advance(), }?;

        let methods = methods.into_boxed_slice();
        Ok(Stmt::Class { loc, name, methods })
    }

    fn function_declaration(&mut self, loc: Location) -> Result<StmtFunction, ParseError> {
        let name = peek_no_eof! { self as ["<identifier>"]
            if is_tok!(Literal::Identifier(name, _)) => *name,
        }?;
        self.advance();

        peek_no_eof! { self as ["("] if is_tok!(Punctuation::ParenLeft) => self.advance(), }?;

        let mut params = Vec::<Key>::new();

        match self.peek() {
            Ok(is_tok!(Punctuation::ParenRight)) => {
                self.advance();
            }
            Ok(is_tok!(Literal::Identifier(_, _))) => loop {
                match self.peek() {
                    Ok(is_tok!(Literal::Identifier(name, _))) => {
                        params.push(*name);
                        self.advance();
                    }
                    Ok(tok) => Err(syntax_error!(
                        "<identifier or )",
                        tok.static_str(),
                        tok.loc()
                    ))?,
                    Err(err) => Err(err.syntax_err("<identifier> or )"))?,
                };
                match self.peek() {
                    Ok(is_tok!(Punctuation::ParenRight)) => {
                        self.advance();
                        break;
                    }
                    Ok(is_tok!(Punctuation::Comma)) => {
                        self.advance();
                        continue;
                    }
                    Ok(tok) => Err(syntax_error!(", or )", tok.static_str(), tok.loc()))?,
                    Err(err) => Err(err.syntax_err(", or )"))?,
                }
            },
            Ok(tok) => Err(syntax_error!(
                "<identifier> or )",
                tok.static_str(),
                tok.loc()
            ))?,
            Err(err) => Err(err.syntax_err("<identifier> or )"))?,
        }

        if params.len() >= Expr::MAX_FUNC_ARGS {
            Err(ParseError::too_many_args(params.len(), loc))?;
        }

        let body = peek_no_eof! { self as ["{"]
            if is_tok!(Punctuation::BraceLeft) => {
                let loc = self.advance().unwrap().loc();
                self.block(loc)?
            },
        }?;

        let body = match body {
            Stmt::Block { statements } => statements.into_boxed_slice(),
            _ => unreachable!(),
        };

        Ok(StmtFunction::new(
            name,
            params.into_boxed_slice(),
            body,
            loc,
        ))
    }

    fn var_declaration(&mut self) -> StmtResult {
        let (name, loc) = peek_no_eof! { self as ["<identifier>"]
            if is_tok!(Literal::Identifier(name, loc)) => (*name, *loc),
        }?;
        self.advance();

        let init = peek_no_eof! { self as ["; or ="]
            if is_tok!(Operator::Equal) => {
                self.advance();
                Some(self.expression().map_err(|err|err.syntax_err("<expression>"))?)
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
            is_tok!(Keyword::For) => {
                let loc = self.advance().unwrap().loc();
                self.for_statement(loc)
            }
            is_tok!(Keyword::Return) => {
                let loc = self.advance().unwrap().loc();
                self.return_statement(loc)
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
            condition,
            body: body.boxed(),
        })
    }

    fn for_statement(&mut self, loc: Location) -> StmtResult {
        peek_no_eof! { self as ["("] if is_tok!(Punctuation::ParenLeft) => self.advance(), }?;

        let init = match self
            .peek()
            .map_err(|err| err.syntax_err("<var_stmt> or <expr_stmt"))?
        {
            is_tok!(Punctuation::Semicolon) => {
                self.advance();
                None
            }
            is_tok!(Keyword::Var) => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => Some(self.expression_statement()?),
        };

        let condition = peek_no_eof! { self as ["<expression>"]
            if is_tok!(Punctuation::Semicolon) => None,
            else _ => Some(self.expression()?),
        }?;
        peek_no_eof! { self as [";"] if is_tok!(Punctuation::Semicolon) => self.advance(), }?;

        let increment = peek_no_eof! { self as [")"]
            if is_tok!(Punctuation::ParenRight) => None,
            else _ => Some(self.expression()?),
        }?;
        peek_no_eof! { self as [")"] if is_tok!(Punctuation::ParenRight) => self.advance(), }?;

        // desugar the for loop into a while loop

        let condition = match condition {
            Some(expr) => expr,
            None => Expr::literal(TokLoc::new(token::Literal::True, loc)).boxed(),
        };

        let body = match increment {
            None => self.statement()?,
            Some(expr) => Stmt::Block {
                statements: vec![self.statement()?, Stmt::Expr { expr }],
            },
        };

        let while_stmt = Stmt::While {
            loc,
            condition,
            body: body.boxed(),
        };

        match init {
            Some(stmt) => Ok(Stmt::Block {
                statements: vec![stmt, while_stmt],
            }),
            None => Ok(while_stmt),
        }
    }

    fn return_statement(&mut self, loc: Location) -> StmtResult {
        match self.peek() {
            Ok(tok) => {
                let value = match tok {
                    is_tok!(Punctuation::Semicolon) => None,
                    _ => Some(self.expression()?),
                };
                let _ = peek_no_eof! { self as [";"]
                    if is_tok!(Punctuation::Semicolon) => self.advance(),
                }?;
                Ok(Stmt::Return { loc, value })
            }
            _ => Err(syntax_error!("<expression> or ;", "<eof>", loc)),
        }
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
        Ok(Stmt::Expr { expr })
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
            condition,
            then: then.boxed(),
            otherwise: otherwise.map(Stmt::boxed),
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
                    Expr::RefExpr(lvalue, _) => match lvalue {
                        RefExpr::Variable { var } => Ok(Expr::assignment(var, value).boxed()),
                        RefExpr::Get { object, prop } => Ok(Expr::set(object, prop, value).boxed()),
                        RefExpr::Grouping { .. } => Err(syntax_error!("<lvalue>", "<group>", loc)),

                        // TODO: use better error message
                        RefExpr::This { loc } => Err(syntax_error!("<lvalue>", "<this keyword>", loc)),

                        // RefExpr::Grouping should protect these cases
                        RefExpr::Assignment { .. } => unreachable!(),
                        RefExpr::Set { .. } => unreachable!(),
                    },
                    Expr::ValExpr(_, _) => Err(syntax_error!("<lvalue>", "<rvalue>", loc)),
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
            Ok(Expr::unary(op, self.unary()?).boxed())
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;

        loop {
            match self.peek()? {
                is_tok!(Punctuation::ParenLeft) => {
                    let loc = self.advance().unwrap().loc();
                    expr = self.finish_call(loc, expr)?;
                    Ok(())
                }
                is_tok!(Punctuation::Dot) => {
                    let loc = self.advance().unwrap().loc();
                    let name = peek_no_eof! { self as ["<identifier>"]
                        if is_tok!(Literal::Identifier(name, _)) => *name,
                    }?;
                    self.advance();
                    let tok = TokLoc::new(token::DotProp { name }, loc);
                    expr = Expr::get(expr, tok).boxed();
                    Ok(())
                }
                _ => break,
            }?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, loc: Location, callee: Box<Expr>) -> ExprResult {
        // zero argument
        if let is_tok!(Punctuation::ParenRight) = self.peek()? {
            self.advance();
            return Ok(Expr::call(callee, Box::new([]), loc).boxed());
        }

        // one or more arguments
        let mut arguments = Vec::new();
        loop {
            arguments.push(*self.expression()?);
            match self.peek()? {
                is_tok!(Punctuation::Comma) => {
                    self.advance();
                    continue;
                }
                _ => break,
            }
        }

        peek_no_eof! { self as [")"] if is_tok!(Punctuation::ParenRight) => self.advance(), }?;

        // NOTE: in the lox book, this arguments number check is done using >= instead of >
        if arguments.len() >= Expr::MAX_FUNC_ARGS {
            Err(ParseError::too_many_args(arguments.len(), loc))
        } else {
            Ok(Expr::call(callee, arguments.into_boxed_slice(), loc).boxed())
        }
    }

    fn primary(&mut self) -> ExprResult {
        let curr = self.advance().expect("Unexpected end of file");
        let loc = curr.loc();

        let lit = |lit| Expr::literal(TokLoc::new(lit, loc));
        let var = |name| Expr::variable(TokLoc::new(token::Variable { name }, loc));

        type Lit = token::Literal;

        let expr = match curr {
            is_tok!(Keyword::True) => lit(Lit::True),
            is_tok!(Keyword::False) => lit(Lit::False),
            is_tok!(Keyword::Nil) => lit(Lit::Nil),
            is_tok!(Keyword::This) => Expr::this(loc),

            is_tok!(Literal::String(str, _)) => lit(Lit::String(*str)),
            is_tok!(Literal::Number(num, _)) => lit(Lit::Number(*num)),
            is_tok!(Literal::Identifier(name, _)) => var(*name),

            is_tok!(Punctuation::ParenLeft) => {
                let expr = self.expression().map_err(|e| e.missing_delim(")", loc))?;
                match self.peek().map_err(|e| e.missing_delim(")", loc))? {
                    is_tok!(Punctuation::ParenRight) => self.advance(),
                    _ => Err(missing_delim!(")", loc))?,
                };
                match *expr {
                    Expr::ValExpr(expr, id) => Expr::group_val(Box::new(expr), loc, id),
                    Expr::RefExpr(expr, id) => Expr::group_ref(Box::new(expr), loc, id),
                }
            }

            lex::Token::Eof(_) => return Err(ParseError::EndOfFile(loc)),
            _ => return Err(syntax_error!("<expression>", curr.static_str(), loc)),
        };

        Ok(expr.boxed())
    }

    fn binary<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&lex::Token) -> Option<TokLoc<token::BinaryOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;
        while let Some(op) = curr(self.peek()?) {
            self.advance();
            let right = inner(self).map_err(|e| e.syntax_err("<expression>"))?;
            expr = Expr::binary(expr, op, right).boxed();
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
            let right = inner(self).map_err(|e| e.syntax_err("<expression>"))?;
            expr = Expr::logical(expr, kind, right).boxed();
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

impl Program {
    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedProgram<'a, 'b> {
        DisplayedProgram {
            program: self,
            interner,
        }
    }
}

impl Display for DisplayedProgram<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.program.statements {
            writeln!(f, "{}", stmt.display(self.interner))?;
        }
        Ok(())
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
        ($self:ident as [$name:expr] if $tok:pat => $xpr1:expr, else $other:tt => $xpr2:expr,) => {
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
