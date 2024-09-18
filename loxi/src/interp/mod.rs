use thiserror::Error;

use crate::parse::{stmt::Stmt, token, Program};
use crate::util::Location;

pub mod object;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{0} RuntimeError: Invalid binary operation '{1}' between '{2}' and '{3}'")]
    InvalidBinaryOp(Location, token::BinaryOp, &'static str, &'static str),

    #[error("{0} RuntimeError: Invalid unary operation '{1}' on '{2}'")]
    InvalidUnaryOp(Location, token::UnaryOp, &'static str),
}

pub struct Interpreter {
    // program context live here
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), RuntimeError> {
        for stmt in program.statements.into_iter() {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => {
                expr.eval()?;
            }
            Stmt::Print { expr, .. } => {
                let expr = expr.eval()?;
                println!("{expr}");
            }
            Stmt::Var { loc, name, init } => todo!(),
        };
        Ok(())
    }
}
