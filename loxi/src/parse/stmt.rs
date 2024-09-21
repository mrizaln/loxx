use std::fmt::{Debug, Display};

use crate::interp::env::Env;
use crate::interp::object::Value;
use crate::interp::RuntimeError;
use crate::util::Location;

use super::expr::{
    macros::{eval_cloned, eval_unit},
    Expr,
};

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Expr {
        expr: Expr,
    },
    Print {
        loc: Location,
        expr: Expr,
    },
    Var {
        loc: Location,
        name: String,
        init: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
}

impl Stmt {
    pub fn execute(self, env: &mut Env) -> Result<(), RuntimeError> {
        match self {
            Stmt::Expr { expr } => eval_unit!(expr, env)?,
            Stmt::Print { expr, .. } => {
                // eval_ref! macro could be defined, but the error handling must be built-in into
                // the macro because it relies on lifetime extension of the Value where the Error
                // must be thrown from the match arms. I want the eval_ref! macro to be able to not
                // handle the error and return it as is so user can handle it themselves. eval_unit!
                // and eval_cloned! both do that and I want symmetry.
                match expr {
                    Expr::ValExpr(expr) => println!("{}", expr.eval(env)?),
                    Expr::RefExpr(expr) => println!("{}", expr.eval(env)?),
                };
            }
            Stmt::Var { loc: _, name, init } => {
                //
                // there are two ways to implement this if the init is a RefExpr:
                // - clone the init         -> the variable then becomes separate entity
                // - reference the init     -> the variable then becomes an alias
                // at this point, I don't know how Lox handle this thing, maybe in future chapters.
                // if it was the latter, then I'll be damned, I have to implement the garbage
                // collector very early on... or at least a system that can track the entities and
                // their references. for now, I'll just clone the init.
                //
                //      -- 2024/09/19 02:57 [chapter 8.2: global variables]

                let value = match init {
                    Some(expr) => eval_cloned!(expr, env)?,
                    None => Value::Nil,
                };

                // TODO: add location metadata
                env.define(name, value);
            }
            Stmt::Block { statements } => {
                let mut new_env = env.child();
                for stmt in statements {
                    stmt.execute(&mut new_env)?;
                }
            }
        };
        Ok(())
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Display::fmt(&expr, f),
            Stmt::Print { expr, .. } => write!(f, "(print {expr})"),
            Stmt::Var { name, init, .. } => match init {
                Some(val) => write!(f, "(var {name} {val})"),
                None => write!(f, "(var {name} nil)"),
            },
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {}", stmt)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Debug::fmt(&expr, f),
            Stmt::Print { loc, expr } => write!(f, "(print{loc} {expr:?})"),
            Stmt::Var { loc, name, init } => match init {
                Some(val) => write!(f, "(var{loc} {name} {val:?})"),
                None => write!(f, "(var{loc} {name} nil)"),
            },
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {:?}", stmt)?;
                }
                write!(f, ")")
            }
        }
    }
}
