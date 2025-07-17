#![allow(dead_code)]

use std::mem;

use loxi::interner::{Interner, Key};
use loxi::parse::ast::Ast;
use loxi::parse::expr::{Expr, ExprId, ExprL, RefExpr, ValExpr};
use loxi::parse::stmt::{Stmt, StmtFunctionId, StmtId, StmtL};
use loxi::parse::token::{self, Literal};
use loxi::parse::Program;
use loxi::resolve::ResolveMap;
use loxi::util::Loc;
use thiserror::Error;

use crate::bytecode::{Address, BinOp, Bytecode, ConstKind, JumpOp, LoadOp, Offset, UnaryOp};
use crate::metadata::{Metadata, VarId};

pub struct Compiler<'a> {
    bytecode: Bytecode,
    resolve_map: ResolveMap,
    debug_info: Metadata,
    variables: Vec<(Key, VarId)>,
    stack_depth: u32,
    ast: &'a Ast,
    interner: &'a Interner,
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("{0} CompileError: error")]
    Error(Loc),
}

impl<'a> Compiler<'a> {
    pub fn new(resolve_map: ResolveMap, ast: &'a Ast, interner: &'a Interner) -> Self {
        Self {
            bytecode: Bytecode::new(),
            resolve_map,
            debug_info: Metadata::default(),
            variables: Vec::default(),
            stack_depth: 0,
            ast,
            interner,
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<Bytecode, CompileError> {
        for stmt in program.statements.iter() {
            self.compile_stmt(stmt)?
        }
        Ok(mem::take(&mut self.bytecode))
    }

    fn compile_stmt(&mut self, id: &StmtId) -> Result<(), CompileError> {
        let StmtL { stmt, loc } = self.ast.get_stmt(id);
        match stmt {
            Stmt::Expr { expr } => self.compile_expr(expr).map(|_| {
                self.decr_depth(1);
                self.bytecode.emit_pop();
            }),
            Stmt::Print { expr } => self.compile_print(expr),
            Stmt::Var { name, init } => self.compile_var(name, loc, init),
            Stmt::Block { statements } => self.compile_block(statements),
            Stmt::If {
                condition,
                then,
                otherwise,
            } => self.compile_if(condition, then, otherwise),
            Stmt::While { condition, body } => self.compile_while(condition, body),
            Stmt::Function { func } => self.compile_function(func),
            Stmt::Return { value } => self.compile_return(value),
            Stmt::Class {
                name,
                base,
                methods,
            } => self.compile_class(name, base, methods),

            #[cfg(feature = "debug")]
            Stmt::Debug { expr } => self.compile_debug(expr),
        }
    }

    fn compile_expr(&mut self, expr: &ExprId) -> Result<(), CompileError> {
        let ExprL { expr, loc } = self.ast.get_expr(expr);
        match expr {
            Expr::ValExpr(val_expr) => self.compile_val_expr(val_expr, loc),
            Expr::RefExpr(ref_expr) => self.compile_ref_expr(ref_expr),
        }
    }

    fn compile_val_expr(&mut self, expr: &ValExpr, loc: &Loc) -> Result<(), CompileError> {
        match expr {
            ValExpr::Literal { value } => {
                match value {
                    Literal::Number(num) => {
                        let addr = self.bytecode.create_consant_num(*num);
                        self.bytecode.emit_constant(ConstKind::Number(addr));
                    }
                    Literal::String(key) => {
                        let addr = self.bytecode.create_constant_string(*key, self.interner);
                        self.bytecode.emit_constant(ConstKind::String(addr));
                    }
                    Literal::True => _ = self.bytecode.emit_constant(ConstKind::Bool(true)),
                    Literal::False => _ = self.bytecode.emit_constant(ConstKind::Bool(false)),
                    Literal::Nil => _ = self.bytecode.emit_constant(ConstKind::Nil),
                };
                self.incr_depth(1);
            }
            ValExpr::Unary { operator, right } => {
                self.compile_expr(right)?;
                let op = match operator {
                    token::UnaryOp::Minus => UnaryOp::Negate,
                    token::UnaryOp::Not => UnaryOp::Not,
                };
                let addr = self.bytecode.emit_unary(op);
                self.debug_info.push_expr(loc, operator.into(), addr);
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                let op = match operator {
                    token::BinaryOp::Equal => BinOp::Equal,
                    token::BinaryOp::NotEqual => BinOp::NotEqual,
                    token::BinaryOp::Less => BinOp::Less,
                    token::BinaryOp::LessEq => BinOp::LessEq,
                    token::BinaryOp::Greater => BinOp::Greater,
                    token::BinaryOp::GreaterEq => BinOp::GreaterEq,
                    token::BinaryOp::Add => BinOp::Add,
                    token::BinaryOp::Sub => BinOp::Sub,
                    token::BinaryOp::Mul => BinOp::Mul,
                    token::BinaryOp::Div => BinOp::Div,
                };
                let addr = self.bytecode.emit_binary(op);
                self.debug_info.push_expr(loc, operator.into(), addr);
                self.decr_depth(1);
            }
            ValExpr::Grouping { expr } => self.compile_expr(expr)?,
            ValExpr::Logical { left, kind, right } => match kind {
                token::LogicalOp::And => {
                    self.compile_expr(left)?;
                    let op = JumpOp::OnFalse(Address::default());
                    let (addr, patch) = self.bytecode.emit_jump(op);
                    self.debug_info.push_expr(loc, kind.into(), addr);

                    self.decr_depth(1);
                    self.bytecode.emit_pop();

                    self.compile_expr(right)?;
                    self.bytecode.patch_jump(patch);
                }
                token::LogicalOp::Or => {
                    self.compile_expr(left)?;
                    let op = JumpOp::OnTrue(Address::default());
                    let (addr, patch) = self.bytecode.emit_jump(op);
                    self.debug_info.push_expr(loc, kind.into(), addr);

                    self.decr_depth(1);
                    self.bytecode.emit_pop();

                    self.compile_expr(right)?;
                    self.bytecode.patch_jump(patch);
                }
            },
            ValExpr::Call { callee, args } => todo!(),
        };
        Ok(())
    }

    fn compile_ref_expr(&mut self, expr: &RefExpr) -> Result<(), CompileError> {
        match expr {
            RefExpr::Variable { var } => {
                let offset = self.get_variable(var.name).unwrap();
                self.bytecode.emit_load(LoadOp::Local(offset));
                self.incr_depth(1);
            }
            RefExpr::Grouping { expr } => self.compile_expr(expr)?,
            RefExpr::Assignment { var, value } => todo!(),
            RefExpr::Get { object, prop } => todo!(),
            RefExpr::Set {
                object,
                prop,
                value,
            } => todo!(),
            RefExpr::Super { prop } => todo!(),
            RefExpr::This => todo!(),
        };
        Ok(())
    }

    fn compile_print(&mut self, expr: &ExprId) -> Result<(), CompileError> {
        self.compile_expr(expr)?;
        self.bytecode.emit_print();
        self.decr_depth(1);
        Ok(())
    }

    fn compile_var(
        &mut self,
        name: &Key,
        loc: &Loc,
        init: &Option<ExprId>,
    ) -> Result<(), CompileError> {
        match init {
            Some(expr) => self.compile_expr(expr)?,
            None => {
                self.bytecode.emit_constant(ConstKind::Nil);
                self.incr_depth(1);
            }
        }
        let offset = self.current_depth();
        let id = self.debug_info.push_var(
            loc,
            self.interner.resolve(*name),
            offset,
            self.bytecode.current_address(),
            0,
        );
        self.variables.push((*name, id));
        Ok(())
    }

    fn compile_block(&mut self, statements: &[StmtId]) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_if(
        &mut self,
        condition: &ExprId,
        then: &StmtId,
        otherwise: &Option<StmtId>,
    ) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_while(&mut self, condition: &ExprId, body: &StmtId) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_function(&mut self, func: &StmtFunctionId) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_return(&mut self, value: &Option<ExprId>) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_class(
        &mut self,
        name: &Key,
        base: &Option<ExprId>,
        methods: &[StmtFunctionId],
    ) -> Result<(), CompileError> {
        todo!()
    }

    #[cfg(feature = "debug")]
    fn compile_debug(&mut self, expr: &ExprId) -> Result<(), CompileError> {
        todo!()
    }

    fn get_variable(&self, key: Key) -> Option<Offset> {
        let var = self.variables.iter().rev().find(|v| v.0 == key);
        var.map(|v| self.debug_info.get_var(v.1)).map(|v| v.offset)
    }

    fn current_depth(&self) -> Offset {
        Offset(self.stack_depth)
    }

    fn incr_depth(&mut self, amount: u32) -> Offset {
        self.stack_depth += amount;
        Offset(self.stack_depth)
    }

    fn decr_depth(&mut self, amount: u32) -> Offset {
        self.stack_depth -= amount;
        Offset(self.stack_depth)
    }
}
