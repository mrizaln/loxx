//! Lox Grammar (unfinished)
//!  ------------------------
//! program     -> declaration* EOF ;
//!
//! declaration -> class_decl
//!                 | fun_decl
//!                 | var_decl
//!                 | statement ;
//!
//! class_decl  -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
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
//!                 | debug_stmt        (enabled if features flag "debug" activated)
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
//! debug_stmt  -> "debug" expression ";" ;
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
//!                 | IDENTIFIER
//!                 | "super" "." IDENTIFIER ;
//!
//! grouping    -> "(" expression ")"

use std::collections::VecDeque;
use std::fmt::Display;

use crate::interp::interner::{Interner, Key};
use crate::lex::token::{self as ltok, Token as LToken};
use crate::util::{Location, LoxToken, TokLoc};

use self::ast::Ast;
use self::error::{ParseError, ParseResultExt, SyntaxError};
use self::expr::{Expr, ExprId, RefExpr};
use self::stmt::{Stmt, StmtFunction, StmtFunctionId, StmtId};

use self::macros::{consume_identifier, consume_punctuation, is_tok, peek_no_eof};

pub mod ast;
pub mod error;
pub mod expr;
pub mod stmt;
pub mod token;

#[cfg(test)]
mod test;

type ExprResult = Result<ExprId, ParseError>;
type StmtResult = Result<StmtId, SyntaxError>;

pub enum Mode {
    Script,
    Repl,
}

pub struct Parser<'a> {
    tokens: VecDeque<LToken>,
    errors: Vec<SyntaxError>,
    current: Option<LToken>,
    ast: &'a mut Ast,
    mode: Mode,
}

pub struct Program {
    pub statements: Vec<StmtId>,
}

pub struct DisplayedProgram<'a, 'b, 'c> {
    program: &'a Program,
    interner: &'b Interner,
    ast: &'c Ast,
}

impl Parser<'_> {
    pub fn new(ast: &mut Ast, mode: Mode) -> Parser {
        Parser {
            tokens: VecDeque::new(),
            errors: Vec::new(),
            current: None,
            ast,
            mode,
        }
    }

    pub fn parse(&mut self, tokens: Vec<LToken>) -> Result<Program, Vec<SyntaxError>> {
        self.tokens = VecDeque::from(tokens);

        let mut statements = Vec::new();

        while let Some(stmt) = self.declaration() {
            statements.push(stmt);
        }

        if self.errors.is_empty() {
            Ok(Program { statements })
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
    fn declaration(&mut self) -> Option<StmtId> {
        if let Ok(tok) = self.peek() {
            let stmt = match tok {
                is_tok!(Keyword::Var) => {
                    self.advance();
                    self.var_declaration()
                }
                is_tok!(Keyword::Fun) => {
                    let loc = self.advance().unwrap().loc();
                    self.function_declaration(loc)
                        .map(|fun| self.ast.add_stmt(Stmt::function(fun), loc))
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
                    self.errors.push(err);
                    self.synchronize();
                    None
                }
            }
        } else {
            None
        }
    }

    fn class_declaration(&mut self, loc: Location) -> StmtResult {
        let (name, _) = consume_identifier!(self)?;

        let base = match self.peek_no_eof("< or {")? {
            is_tok!(Operator::Less) => {
                let loc = self.advance().unwrap().loc();
                let (base, _) = consume_identifier!(self)?;
                let variable = Expr::variable(token::Variable { name: base });
                Ok(Some(self.ast.add_expr(variable, loc)))
            }
            is_tok!(Punctuation::BraceLeft) => Ok(None),
            tok => Err(SyntaxError::expect("< or {", tok.static_str(), tok.loc())),
        }?;

        consume_punctuation!(self, BraceLeft)?;
        let mut methods = Vec::new();
        loop {
            match self.peek_no_eof("<fun> or }")? {
                is_tok!(Punctuation::BraceRight) => break,
                tok => {
                    let loc = tok.loc();
                    let method = self.function_declaration(loc)?;
                    methods.push(method);
                }
            };
        }
        consume_punctuation!(self, BraceRight)?;

        let class = Stmt::class(name, base, methods.into_boxed_slice());
        Ok(self.ast.add_stmt(class, loc))
    }

    fn function_declaration(&mut self, loc: Location) -> Result<StmtFunctionId, SyntaxError> {
        let (name, _) = consume_identifier!(self)?;
        consume_punctuation!(self, ParenLeft)?;
        let mut params = Vec::<(Key, Location)>::new();

        peek_no_eof!(self, "<identifier> or )" => {
            is_tok!(Punctuation::ParenRight) => {
                self.advance();
            },
            is_tok!(Literal::Identifier(_, _)) => loop {
                let (name, loc) = consume_identifier!(self)?;
                params.push((name, loc));

                // I can't use `peek_no_eof!` here since this block contains `break` and `continue`
                match self.peek_no_eof(", or )")? {
                    is_tok!(Punctuation::ParenRight) => {
                        self.advance();
                        break;
                    },
                    is_tok!(Punctuation::Comma) => {
                        self.advance();
                        continue;
                    },
                    // so, this line is inevitable
                    tok => Err(SyntaxError::expect(", or )", tok.static_str(), tok.loc()))?,
                };
            },
        })?;

        if params.len() >= Expr::MAX_FUNC_ARGS {
            Err(SyntaxError::too_many_args(params.len(), loc))?;
        }

        let body_loc = consume_punctuation!(self, BraceLeft)?;
        let body = self.block(body_loc)?;

        let fun = StmtFunction::new(name, params.into_boxed_slice(), body);
        Ok(self.ast.add_func(fun, loc))
    }

    fn var_declaration(&mut self) -> StmtResult {
        let (name, loc) = consume_identifier!(self)?;

        let init = match self.peek_no_eof("; or =")? {
            is_tok!(Operator::Equal) => {
                self.advance();
                Some(self.expression().map_syntax_err("<expression>")?)
            }
            tok => match tok {
                is_tok!(Punctuation::Semicolon) => None,
                _ => Err(SyntaxError::expect("; or =", tok.static_str(), tok.loc()))?,
            },
        };

        consume_punctuation!(self, Semicolon)?;

        let stmt = Stmt::var(name, init);
        Ok(self.ast.add_stmt(stmt, loc))
    }

    fn statement(&mut self) -> StmtResult {
        match self.peek_no_eof("<statement>")? {
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

            #[cfg(feature = "debug")]
            is_tok!(Keyword::Debug) => {
                let loc = self.advance().unwrap().loc();
                self.debug_statement(loc)
            }

            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self, loc: Location) -> StmtResult {
        let expr = self.expression().map_syntax_err("<expression>")?;
        consume_punctuation!(self, Semicolon)?;
        let stmt = Stmt::print(expr);
        Ok(self.ast.add_stmt(stmt, loc))
    }

    #[cfg(feature = "debug")]
    fn debug_statement(&mut self, loc: Location) -> StmtResult {
        let expr = self.expression().map_syntax_err("<expression>")?;
        consume_punctuation!(self, Semicolon)?;
        let stmt = Stmt::debug(expr);
        Ok(self.ast.add_stmt(stmt, loc))
    }

    fn while_statement(&mut self, loc: Location) -> StmtResult {
        consume_punctuation!(self, ParenLeft)?;
        let condition = self.expression().map_syntax_err("<expression>")?;
        consume_punctuation!(self, ParenRight)?;
        let body = self.statement()?;

        let stmt = Stmt::while_(condition, body);
        Ok(self.ast.add_stmt(stmt, loc))
    }

    fn for_statement(&mut self, loc: Location) -> StmtResult {
        consume_punctuation!(self, ParenLeft)?;

        let init = match self.peek_no_eof("<var_stmt> or <expr_stmt")? {
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

        let condition = match self.peek_no_eof("<expression>")? {
            is_tok!(Punctuation::Semicolon) => None,
            _ => Some(self.expression().map_syntax_err("<expression>")?),
        };
        consume_punctuation!(self, Semicolon)?;

        let increment = match self.peek_no_eof(")")? {
            is_tok!(Punctuation::ParenRight) => None,
            _ => Some(self.expression().map_syntax_err("<expression>")?),
        };
        consume_punctuation!(self, ParenRight)?;

        // desugar the for loop into a while loop

        let condition = match condition {
            Some(expr) => expr,
            None => self.ast.add_expr(Expr::literal(token::Literal::True), loc),
        };

        let body = match increment {
            None => self.statement()?,
            Some(expr) => {
                let expr = self.ast.add_stmt(Stmt::expr(expr), loc);
                let block = Stmt::block(Box::new([self.statement()?, expr]));
                self.ast.add_stmt(block, loc)
            }
        };

        let while_ = Stmt::while_(condition, body);
        let while_ = self.ast.add_stmt(while_, loc);

        match init {
            Some(stmt) => Ok({
                let block = Stmt::block(Box::new([stmt, while_]));
                self.ast.add_stmt(block, loc)
            }),
            None => Ok(while_),
        }
    }

    fn return_statement(&mut self, loc: Location) -> StmtResult {
        let value = match self.peek_no_eof("<expression> or ;")? {
            is_tok!(Punctuation::Semicolon) => None,
            _ => Some(self.expression().map_syntax_err("<expression>")?),
        };
        consume_punctuation!(self, Semicolon)?;

        let return_ = Stmt::return_(value);
        Ok(self.ast.add_stmt(return_, loc))
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
            let block = Stmt::block(statements.into_boxed_slice());
            Ok(self.ast.add_stmt(block, start))
        } else {
            Err(SyntaxError::missing_delim("}", start))
        }
    }

    fn expression_statement(&mut self) -> StmtResult {
        let expr = self.expression().map_syntax_err("<expression>")?;
        match self.peek() {
            Ok(is_tok!(Punctuation::Semicolon)) => {
                self.advance();
                let stmt = Stmt::expr(expr);
                let loc = self.ast.get_expr(&expr).loc;
                Ok(self.ast.add_stmt(stmt, loc))
            }
            Ok(tok) => Err(SyntaxError::expect(";", tok.static_str(), tok.loc())),
            Err(err) => match self.mode {
                Mode::Script => Err(err.as_syntax_err(";")),
                Mode::Repl => {
                    let stmt = Stmt::print(expr);
                    let loc = self.ast.get_expr(&expr).loc;
                    Ok(self.ast.add_stmt(stmt, loc))
                }
            },
        }
    }

    fn if_statement(&mut self, loc: Location) -> StmtResult {
        consume_punctuation!(self, ParenLeft)?;
        let condition = self.expression().map_syntax_err("<expression>")?;
        consume_punctuation!(self, ParenRight)?;

        let then = self.statement()?;
        let otherwise = match self.peek() {
            Ok(is_tok!(Keyword::Else)) => {
                self.advance();
                Some(self.statement()?)
            }
            _ => None,
        };

        let if_ = match otherwise {
            Some(otherwise) => Stmt::if_else(condition, then, otherwise),
            None => Stmt::if_(condition, then),
        };
        Ok(self.ast.add_stmt(if_, loc))
    }

    fn expression(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        let expr = self.logical_or()?;

        match self.peek() {
            Ok(is_tok!(Operator::Equal)) => {
                let loc = self.advance().unwrap().loc();
                let value = self.assignment()?;

                let expr = &self.ast.get_expr(&expr).expr;
                let expr = match expr {
                    Expr::RefExpr(lvalue) => match lvalue {
                        RefExpr::Variable { var } => Expr::assignment(var.clone(), value),
                        RefExpr::Get { object, prop } => Expr::set(*object, prop.clone(), value),
                        RefExpr::Grouping { .. } => {
                            Err(SyntaxError::expect("<lvalue>", "<group>", loc))?
                        }

                        // TODO: use better error message
                        RefExpr::This { .. } => {
                            Err(SyntaxError::expect("<lvalue>", "<this keyword>", loc))?
                        }
                        RefExpr::Super { .. } => {
                            Err(SyntaxError::expect("<lvalue>", "<super keyword>", loc))?
                        }

                        // RefExpr::Grouping should protect these cases
                        RefExpr::Assignment { .. } => unreachable!(),
                        RefExpr::Set { .. } => unreachable!(),
                    },
                    Expr::ValExpr(_) => Err(SyntaxError::expect("<lvalue>", "<rvalue>", loc))?,
                };
                Ok(self.ast.add_expr(expr, loc))
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
            let expr = Expr::unary(op.tok, self.unary()?);
            Ok(self.ast.add_expr(expr, op.loc))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ExprResult {
        let mut expr = self.primary()?;
        loop {
            match self.peek() {
                Ok(is_tok!(Punctuation::ParenLeft)) => {
                    let loc = self.advance().unwrap().loc();
                    expr = self.finish_call(loc, expr)?;
                }
                Ok(is_tok!(Punctuation::Dot)) => {
                    let loc = self.advance().unwrap().loc();
                    let (name, _) = consume_identifier!(self)?;
                    let tok = TokLoc::new(token::DotProp { name }, loc);
                    let get = Expr::get(expr, tok.tok);
                    expr = self.ast.add_expr(get, tok.loc);
                }
                _ => break,
            };
        }
        Ok(expr)
    }

    fn finish_call(&mut self, loc: Location, callee: ExprId) -> ExprResult {
        // zero argument
        if let is_tok!(Punctuation::ParenRight) = self.peek()? {
            self.advance();
            let call = Expr::call(callee, Box::new([]));
            return Ok(self.ast.add_expr(call, loc));
        }

        // one or more arguments
        let mut arguments = Vec::new();
        loop {
            arguments.push(self.expression()?);
            match self.peek()? {
                is_tok!(Punctuation::Comma) => {
                    self.advance();
                    continue;
                }
                _ => break,
            }
        }

        consume_punctuation!(self, ParenRight)?;

        // NOTE: in the lox book, this arguments number check is done using >= instead of >
        if arguments.len() >= Expr::MAX_FUNC_ARGS {
            Err(ParseError::too_many_args(arguments.len(), loc))
        } else {
            let call = Expr::call(callee, arguments.into_boxed_slice());
            Ok(self.ast.add_expr(call, loc))
        }
    }

    fn primary(&mut self) -> ExprResult {
        let curr = self.advance().expect("Unexpected end of file");
        let mut loc = curr.loc();

        let lit = Expr::literal;
        let var = |name| Expr::variable(token::Variable { name });

        type Lit = token::Literal;

        let expr = match curr {
            is_tok!(Keyword::True) => lit(Lit::True),
            is_tok!(Keyword::False) => lit(Lit::False),
            is_tok!(Keyword::Nil) => lit(Lit::Nil),
            is_tok!(Keyword::This) => Expr::this(),
            is_tok!(Keyword::Super) => {
                loc = consume_punctuation!(self, Dot)?;
                let (name, _) = consume_identifier!(self)?;
                let prop = token::DotProp { name };
                Expr::super_(prop)
            }

            is_tok!(Literal::String(str, _)) => lit(Lit::String(*str)),
            is_tok!(Literal::Number(num, _)) => lit(Lit::Number(*num)),
            is_tok!(Literal::Identifier(name, _)) => var(*name),

            is_tok!(Punctuation::ParenLeft) => {
                let expr = self.expression().map_err(|e| e.missing_delim(")", loc))?;
                match self.peek().map_err(|e| e.missing_delim(")", loc))? {
                    is_tok!(Punctuation::ParenRight) => self.advance(),
                    _ => Err(SyntaxError::missing_delim(")", loc))?,
                };
                match self.ast.get_expr(&expr).expr {
                    Expr::ValExpr(_) => Expr::group_val(expr),
                    Expr::RefExpr(_) => Expr::group_ref(expr),
                }
            }

            LToken::Eof(_) => return Err(ParseError::EndOfFile(loc)),
            _ => Err(SyntaxError::expect("<expression>", curr.static_str(), loc))?,
        };

        Ok(self.ast.add_expr(expr, loc))
    }

    fn binary<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&LToken) -> Option<TokLoc<token::BinaryOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;
        while let Some(op) = self.peek().ok().and_then(&curr) {
            self.advance();
            let right = inner(self).map_syntax_err("<expression>")?;
            let binary = Expr::binary(expr, op.tok, right);
            expr = self.ast.add_expr(binary, op.loc);
        }
        Ok(expr)
    }

    fn logical<F1, F2>(&mut self, curr: F1, inner: F2) -> ExprResult
    where
        F1: Fn(&LToken) -> Option<TokLoc<token::LogicalOp>>,
        F2: Fn(&mut Self) -> ExprResult,
    {
        let mut expr = inner(self)?;
        while let Some(kind) = self.peek().ok().and_then(&curr) {
            self.advance();
            let right = inner(self).map_syntax_err("<expression>")?;
            let logical = Expr::logical(expr, kind.tok, right);
            expr = self.ast.add_expr(logical, kind.loc);
        }
        Ok(expr)
    }

    fn peek(&mut self) -> Result<&LToken, ParseError> {
        match self.tokens.front() {
            // out of bound read is considered as EOF at invalid location [0:0]
            None => Err(ParseError::EndOfFile(Location::default())),
            Some(LToken::Eof(loc)) => Err(ParseError::EndOfFile(*loc)),
            Some(tok) => Ok(tok),
        }
    }

    fn peek_no_eof(&mut self, expect: &'static str) -> Result<&LToken, SyntaxError> {
        self.peek().map_syntax_err(expect)
    }

    fn advance(&mut self) -> Option<&LToken> {
        self.current = self.tokens.pop_front();
        self.current.as_ref()
    }
}

impl Program {
    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedProgram<'a, 'b, 'c> {
        DisplayedProgram {
            program: self,
            interner,
            ast,
        }
    }
}

impl Display for DisplayedProgram<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.program.statements {
            let stmt = &self.ast.get_stmt(stmt).stmt;
            writeln!(f, "{}", stmt.display(self.interner, self.ast))?;
        }
        Ok(())
    }
}

mod conv {
    use super::*;

    pub fn to_equality(tok: &LToken) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::BangEqual) => token::BinaryOp::NotEqual,
            is_tok!(Operator::EqualEqual) => token::BinaryOp::Equal,
            _ => None?,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_comparison(tok: &LToken) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Greater) => token::BinaryOp::Greater,
            is_tok!(Operator::GreaterEqual) => token::BinaryOp::GreaterEq,
            is_tok!(Operator::Less) => token::BinaryOp::Less,
            is_tok!(Operator::LessEqual) => token::BinaryOp::LessEq,
            _ => None?,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_term(tok: &LToken) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Plus) => token::BinaryOp::Add,
            is_tok!(Operator::Minus) => token::BinaryOp::Sub,
            _ => None?,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_factor(tok: &LToken) -> Option<TokLoc<token::BinaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Star) => token::BinaryOp::Mul,
            is_tok!(Operator::Slash) => token::BinaryOp::Div,
            _ => None?,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_unary(tok: &LToken) -> Option<TokLoc<token::UnaryOp>> {
        let new_tok = match tok {
            is_tok!(Operator::Bang) => token::UnaryOp::Not,
            is_tok!(Operator::Minus) => token::UnaryOp::Minus,
            _ => None?,
        };
        Some(TokLoc {
            tok: new_tok,
            loc: tok.loc(),
        })
    }

    pub fn to_logical(tok: &LToken, kind: token::LogicalOp) -> Option<TokLoc<token::LogicalOp>> {
        let new_tok = match tok {
            is_tok!(Keyword::And) => token::LogicalOp::And,
            is_tok!(Keyword::Or) => token::LogicalOp::Or,
            _ => None?,
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
    /// peek `$self` for the next token, if it is not the expected token (`$tok`) or the end of
    /// file, return a `ParseError::SyntaxError`
    macro_rules! peek_no_eof {
        // multiple pattern
        ($self:ident, $expect:expr => { $($tok:pat => $xpr:expr,)+ }) => {
            match $self.peek() {
                $(Ok($tok) => Ok($xpr),)+
                Ok(tok) => Err(SyntaxError::expect($expect, tok.static_str(), tok.loc())),
                Err(err) => Err(err.as_syntax_err($expect))
            }
        };
        // single pattern, no braces
        ($self:ident, $expect:expr => $tok:pat => $xpr:expr) => {
            peek_no_eof!($self, $expect => { $tok => $xpr, })
        };

    }

    /// consume the next token if it is the expected punctuation, return the location of said
    /// punctuation otherwise return an error
    macro_rules! consume_punctuation {
        ($self:ident, $punc:ident) => {
            peek_no_eof! {
                $self,
                ltok::Punctuation::$punc.as_str() => is_tok!(Punctuation::$punc) => $self.advance().unwrap().loc()
            }
        };
    }

    /// consume the next token if it is an identifier, return the name and location of said
    /// identifier otherwise return an error
    macro_rules! consume_identifier {
        ($self:ident) => {
            peek_no_eof!($self, "<identifier>" => {
                is_tok!(Literal::Identifier(name, loc)) => {
                    let res = (*name, *loc);
                    $self.advance();
                    res
                },
            })
        }
    }

    /// convenience macro for creating a `LToken::$name(TokLoc { tok: ltok::$name::$tok, .. })`
    macro_rules! is_tok {
        ($type:ident::$name:ident) => {
            LToken::$type(TokLoc {
                tok: ltok::$type::$name,
                ..
            })
        };
        ($type:ident::$name:ident($tok:tt, $loc:tt)) => {
            LToken::$type(TokLoc {
                tok: ltok::$type::$name($tok),
                loc: $loc,
            })
        };
    }

    pub(crate) use consume_identifier;
    pub(crate) use consume_punctuation;
    pub(crate) use is_tok;
    pub(crate) use peek_no_eof;
}
