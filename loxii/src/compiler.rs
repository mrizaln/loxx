use std::mem;

use loxi::interner::{Interner, Key};
use loxi::parse::ast::Ast;
use loxi::parse::expr::{Expr, ExprId, ExprL, RefExpr, ValExpr};
use loxi::parse::stmt::{Stmt, StmtFunctionId, StmtId, StmtL};
use loxi::parse::token::{self, Literal};
use loxi::parse::Program;
use loxi::resolve::ResolveMap;
use loxi::util::Loc;
use rustc_hash::FxHashMap;

use crate::bytecode::{BinOp, Bytecode, BytecodeReader, Offset, UnaryOp};
use crate::value::Constant;

pub struct Compiler<'a> {
    bytecode: Bytecode,
    variables: FxHashMap<(Key, Loc), u16>,
    local_depth: u16,
    resolve_map: ResolveMap,
    ast: &'a Ast,
    interner: &'a Interner,
}

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("{0} SyntaxError: Variable is used in its own initializer")]
    VariableInInitializer(Loc),
}

impl<'a> Compiler<'a> {
    pub fn new(resolve_map: ResolveMap, ast: &'a Ast, interner: &'a Interner) -> Self {
        Self {
            bytecode: Bytecode::new(),
            variables: FxHashMap::default(),
            local_depth: 0,
            resolve_map,
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
                self.local_depth -= 1;
                self.bytecode.emit_pop();
            }),
            Stmt::Print { expr } => self.compile_print(expr),
            Stmt::Var { name, init } => self.compile_var(name, init),
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

    fn compile_expr(&mut self, expr: &ExprId) -> Result<Offset, CompileError> {
        let ExprL { expr, loc } = self.ast.get_expr(expr);
        match expr {
            Expr::ValExpr(val_expr) => self.compile_val_expr(val_expr, loc),
            Expr::RefExpr(ref_expr) => self.compile_ref_expr(ref_expr),
        }
    }

    fn compile_val_expr(&mut self, expr: &ValExpr, loc: &Loc) -> Result<Offset, CompileError> {
        match expr {
            ValExpr::Literal { value } => {
                let addr = self.bytecode.create_constant(match value {
                    Literal::Number(num) => Constant::Number(*num),
                    Literal::String(key) => Constant::String(self.interner.resolve(*key)),
                    Literal::True => Constant::Bool(true),
                    Literal::False => Constant::Bool(false),
                    Literal::Nil => Constant::Nil,
                });
                self.bytecode.emit_constant(addr);
                self.local_depth += 1;
                Ok(Offset(self.local_depth))
            }
            ValExpr::Unary { operator, right } => {
                let right = self.compile_expr(right)?;
                let op = match operator {
                    token::UnaryOp::Minus => UnaryOp::Negate(right),
                    token::UnaryOp::Not => UnaryOp::Not(right),
                };
                self.bytecode.emit_unary(op);
                Ok(Offset(self.local_depth))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.compile_expr(left)?;
                let right = self.compile_expr(right)?;
                let op = match operator {
                    token::BinaryOp::Equal => BinOp::Equal(left, right),
                    token::BinaryOp::NotEqual => BinOp::NotEqual(left, right),
                    token::BinaryOp::Less => BinOp::Less(left, right),
                    token::BinaryOp::LessEq => BinOp::LessEq(left, right),
                    token::BinaryOp::Greater => BinOp::Greater(left, right),
                    token::BinaryOp::GreaterEq => BinOp::GreaterEq(left, right),
                    token::BinaryOp::Add => BinOp::Add(left, right),
                    token::BinaryOp::Sub => BinOp::Sub(left, right),
                    token::BinaryOp::Mul => BinOp::Mul(left, right),
                    token::BinaryOp::Div => BinOp::Div(left, right),
                };
                self.bytecode.emit_binary(op);
                self.local_depth -= 1;
                Ok(Offset(self.local_depth))
            }
            ValExpr::Grouping { expr } => self.compile_expr(expr),
            ValExpr::Logical { left, kind, right } => todo!(),
            ValExpr::Call { callee, args } => todo!(),
        }
    }

    fn compile_ref_expr(&mut self, expr: &RefExpr) -> Result<Offset, CompileError> {
        match expr {
            RefExpr::Variable { var } => todo!(),
            RefExpr::Grouping { expr } => todo!(),
            RefExpr::Assignment { var, value } => todo!(),
            RefExpr::Get { object, prop } => todo!(),
            RefExpr::Set {
                object,
                prop,
                value,
            } => todo!(),
            RefExpr::Super { prop } => todo!(),
            RefExpr::This => todo!(),
        }
    }

    fn compile_print(&mut self, expr: &ExprId) -> Result<(), CompileError> {
        todo!()
    }

    fn compile_var(&mut self, name: &Key, init: &Option<ExprId>) -> Result<(), CompileError> {
        todo!()
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
}
