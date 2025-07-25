use crate::util::Loc;

use super::expr::{Expr, ExprId, ExprL};
use super::stmt::{Stmt, StmtFunction, StmtFunctionId, StmtFunctionL, StmtId, StmtL};

pub struct Ast {
    pub expressions: Vec<ExprL>,
    pub statements: Vec<StmtL>,
    pub functions: Vec<StmtFunctionL>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            expressions: Vec::new(),
            statements: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn add_expr(&mut self, expr: Expr, loc: Loc) -> ExprId {
        let id = self.expressions.len();
        self.expressions.push(ExprL { expr, loc });
        ExprId::new(id)
    }

    pub fn add_stmt(&mut self, stmt: Stmt, loc: Loc) -> StmtId {
        let id = self.statements.len();
        self.statements.push(StmtL { stmt, loc });
        StmtId::new(id)
    }

    pub fn add_func(&mut self, func: StmtFunction, loc: Loc) -> StmtFunctionId {
        let id = self.functions.len();
        self.functions.push(StmtFunctionL { func, loc });
        StmtFunctionId::new(id)
    }

    pub fn get_expr(&self, id: &ExprId) -> &ExprL {
        &self.expressions[id.inner()]
    }

    pub fn get_stmt(&self, id: &StmtId) -> &StmtL {
        &self.statements[id.inner()]
    }

    pub fn get_func(&self, id: &StmtFunctionId) -> &StmtFunctionL {
        &self.functions[id.inner()]
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}
