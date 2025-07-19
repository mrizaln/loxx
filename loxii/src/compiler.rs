#![allow(dead_code)]

use loxi::interner::{Interner, Key};
use loxi::parse::ast::Ast;
use loxi::parse::expr::{Expr, ExprId, ExprL, RefExpr, ValExpr};
use loxi::parse::stmt::{Stmt, StmtFunctionId, StmtFunctionL, StmtId, StmtL};
use loxi::parse::token::{self, Literal};
use loxi::parse::Program;
use loxi::resolve::scope::Hoists;
use loxi::resolve::ResolveMap;
use loxi::util::Loc;
use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::bytecode::{
    BinOp, Bytecode, BytecodeBuilder, ConstKind, InstrAddr, JumpDest, JumpKind, JumpOp, LoadOp,
    StackIdx, StoreOp, UnaryOp,
};

pub struct Compiler<'a> {
    bytecode: BytecodeBuilder,
    resolve_map: ResolveMap,
    globals: FxHashMap<Key, StackIdx>,
    locals: Vec<(Key, StackIdx)>,
    scopes: Vec<u32>,
    current_hoists: Option<Hoists>,
    stack_depth: u32,
    ast: &'a Ast,
    interner: &'a Interner,
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("{0} CompileError: error")]
    Error(Loc),
}

enum VarKind {
    Local,
    Global,
    Upvalue,
}

impl<'a> Compiler<'a> {
    pub fn new(resolve_map: ResolveMap, ast: &'a Ast, interner: &'a Interner) -> Self {
        Self {
            bytecode: BytecodeBuilder::new(),
            resolve_map,
            globals: FxHashMap::default(),
            locals: Vec::default(),
            scopes: Vec::default(),
            current_hoists: None,
            stack_depth: 0,
            ast,
            interner,
        }
    }

    pub fn compile(mut self, program: &Program) -> Result<Bytecode, CompileError> {
        let global = self.interner.key_global;
        let name = self.bytecode.create_constant_string(global, self.interner);
        self.bytecode.start_chunk(name, Loc::default());

        let globals = self.resolve_map.globals().to_owned();
        for (i, (name, _)) in globals.iter().enumerate() {
            self.globals.insert(*name, StackIdx(i as u32 + 1));
            self.bytecode.emit_constant(ConstKind::Nil);
            self.incr_depth(1);
        }

        for stmt in program.statements.iter() {
            self.compile_stmt(stmt)?
        }

        self.bytecode.end_chunk();
        Ok(self.bytecode.finish())
    }

    fn compile_stmt(&mut self, id: &StmtId) -> Result<(), CompileError> {
        let StmtL { stmt, loc } = self.ast.get_stmt(id);
        match stmt {
            Stmt::Expr { expr } => self.compile_expr(expr).map(|_| {
                self.bytecode.emit_pop(1);
                self.decr_depth(1);
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
            Stmt::Function { func } => self.compile_function(func, loc),
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

    fn compile_expr(&mut self, id: &ExprId) -> Result<(), CompileError> {
        let ExprL { expr, loc } = self.ast.get_expr(id);
        match expr {
            Expr::ValExpr(val_expr) => self.compile_val_expr(val_expr, loc),
            Expr::RefExpr(ref_expr) => self.compile_ref_expr(id, ref_expr),
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
                self.bytecode.emit_unary(op);
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
                self.bytecode.emit_binary(op);
                self.decr_depth(1);
            }
            ValExpr::Grouping { expr } => self.compile_expr(expr)?,
            ValExpr::Logical { left, kind, right } => match kind {
                token::LogicalOp::And => {
                    self.compile_expr(left)?;
                    let (_, patch) = self.bytecode.emit_jump(JumpKind::OnFalse);

                    self.bytecode.emit_pop(1);
                    self.decr_depth(1);

                    self.compile_expr(right)?;
                    self.bytecode.patch_jump(patch, JumpDest::Current);
                }
                token::LogicalOp::Or => {
                    self.compile_expr(left)?;
                    let (_, patch) = self.bytecode.emit_jump(JumpKind::OnTrue);

                    self.bytecode.emit_pop(1);
                    self.decr_depth(1);

                    self.compile_expr(right)?;
                    self.bytecode.patch_jump(patch, JumpDest::Current);
                }
            },
            ValExpr::Call { callee, args } => todo!(),
        };
        Ok(())
    }

    fn compile_ref_expr(&mut self, id: &ExprId, expr: &RefExpr) -> Result<(), CompileError> {
        match expr {
            RefExpr::Variable { var } => {
                let (off, kind) = self.get_variable(id, &var.name).unwrap();
                let op = match kind {
                    VarKind::Local => LoadOp::Local(off),
                    VarKind::Global => LoadOp::Global(off),
                    VarKind::Upvalue => LoadOp::Upvalue(off),
                };
                self.bytecode.emit_load(op);
                self.incr_depth(1);
            }
            RefExpr::Grouping { expr } => self.compile_expr(expr)?,
            RefExpr::Assignment { var, value } => self.compile_assignment(&var.name, id, value)?,
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

    fn compile_assignment(
        &mut self,
        name: &Key,
        id: &ExprId,
        value: &ExprId,
    ) -> Result<(), CompileError> {
        self.compile_expr(value)?;
        let (off, kind) = self.get_variable(id, name).unwrap();
        let op = match kind {
            VarKind::Local => StoreOp::Local(off),
            VarKind::Global => StoreOp::Global(off),
            VarKind::Upvalue => StoreOp::Upvalue(off),
        };
        self.bytecode.emit_store(op);
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

        if self.scopes.is_empty() {
            let offset = self.get_global(name).unwrap();
            self.bytecode.emit_store(StoreOp::Global(offset));
            self.bytecode.emit_pop(1);
            self.decr_depth(1);
        } else {
            let offset = self.current_depth();
            self.locals.push((*name, offset));
        }

        Ok(())
    }

    fn compile_block(&mut self, statements: &[StmtId]) -> Result<(), CompileError> {
        self.new_scope();
        for stmt in statements.iter() {
            self.compile_stmt(stmt)?;
        }
        self.drop_scope();
        Ok(())
    }

    fn compile_if(
        &mut self,
        condition: &ExprId,
        then: &StmtId,
        otherwise: &Option<StmtId>,
    ) -> Result<(), CompileError> {
        self.compile_expr(condition)?;

        let (_, patch_cond) = self.bytecode.emit_jump(JumpKind::OnFalse);
        self.compile_stmt(then)?;

        match otherwise {
            Some(stmt) => {
                let (_, patch_then) = self.bytecode.emit_jump(JumpKind::Unconditional);
                self.bytecode.patch_jump(patch_cond, JumpDest::Current);
                self.compile_stmt(stmt)?;
                self.bytecode.patch_jump(patch_then, JumpDest::Current);
            }
            _ => {
                self.bytecode.patch_jump(patch_cond, JumpDest::Current);
            }
        }

        self.bytecode.emit_pop(1);
        self.decr_depth(1);

        Ok(())
    }

    fn compile_while(&mut self, condition: &ExprId, body: &StmtId) -> Result<(), CompileError> {
        let start = self.bytecode.current_address();

        self.compile_expr(condition)?;
        let (_, patch) = self.bytecode.emit_jump(JumpKind::OnFalse);

        self.bytecode.emit_pop(1); // drop condition
        self.decr_depth(1);
        self.compile_stmt(body)?;

        let (_, end_patch) = self.bytecode.emit_jump(JumpKind::Unconditional);
        self.bytecode.patch_jump(end_patch, JumpDest::Addr(start));

        self.bytecode.patch_jump(patch, JumpDest::Current);
        self.bytecode.emit_pop(1); // drop condition

        Ok(())
    }

    fn compile_function(&mut self, func: &StmtFunctionId, loc: &Loc) -> Result<(), CompileError> {
        let StmtFunctionL { func, loc } = self.ast.get_func(func);
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

    fn new_scope(&mut self) {
        self.scopes.push(self.stack_depth);
    }

    fn drop_scope(&mut self) {
        let until = self.scopes.pop().expect("scopes must not empty");
        let delta = self.stack_depth - until;
        if delta > 0 {
            self.decr_depth(delta);
            self.bytecode.emit_pop(delta);
            self.locals.truncate(self.locals.len() - delta as usize);
        }
    }

    fn get_variable(&self, id: &ExprId, key: &Key) -> Option<(StackIdx, VarKind)> {
        match self.resolve_map.distance(*id) {
            None => self.get_global(key).map(|v| (v, VarKind::Global)),
            Some(_) => self.get_local(key).map(|v| (v, VarKind::Local)), // TODO: check upvalue
        }
    }

    fn get_global(&self, key: &Key) -> Option<StackIdx> {
        self.globals.get(key).cloned()
    }

    fn get_local(&self, key: &Key) -> Option<StackIdx> {
        let var = self.locals.iter().rev().find(|v| v.0 == *key);
        var.map(|v| v.1)
    }

    fn current_depth(&self) -> StackIdx {
        StackIdx(self.stack_depth)
    }

    fn incr_depth(&mut self, amount: u32) -> StackIdx {
        self.stack_depth += amount;
        StackIdx(self.stack_depth)
    }

    fn decr_depth(&mut self, amount: u32) -> StackIdx {
        self.stack_depth -= amount;
        StackIdx(self.stack_depth)
    }
}
