use std::fmt::{Debug, Display};

use lasso::{Rodeo, Spur};

use crate::interp::env::Env;
use crate::interp::value::Value;
use crate::interp::RuntimeError;
use crate::util::Location;

use super::expr::Expr;

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
        name: Spur,
        init: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        loc: Location,
        condition: Expr,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    While {
        loc: Location,
        condition: Expr,
        body: Box<Stmt>,
    },
}

impl Stmt {
    pub fn execute(&self, env: &mut Env, arena: &Rodeo) -> Result<(), RuntimeError> {
        match self {
            Stmt::Expr { expr } => expr.eval_unit(env, arena)?,
            Stmt::Print { expr, .. } => expr.eval_fn(env, arena, |v| println!("{}", v))?,
            Stmt::Var { name, init, .. } => {
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
                    Some(expr) => expr.eval_cloned(env, arena)?,
                    None => Value::Nil,
                };

                // TODO: add location metadata
                env.define(name.clone(), value);
            }
            Stmt::Block { statements } => {
                let mut new_env = env.child();
                for stmt in statements {
                    stmt.execute(&mut new_env, arena)?;
                }
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => match condition.eval_fn(env, arena, |v| v.truthiness())? {
                true => then.execute(env, arena)?,
                false => {
                    if let Some(stmt) = otherwise {
                        stmt.execute(env, arena)?
                    }
                }
            },
            Stmt::While {
                condition, body, ..
            } => {
                while condition.eval_fn(env, arena, |v| v.truthiness())? {
                    body.execute(env, arena)?
                }
            }
        };

        Ok(())
    }
}

// TODO: remove these impl for Display and Debug then replace with proper conversion from Spur

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Display::fmt(&expr, f),
            Stmt::Print { expr, .. } => write!(f, "(print {expr})"),
            Stmt::Var { name, init, .. } => match init {
                Some(val) => write!(f, "(var spur|{}| {val})", name.into_inner()),
                None => write!(f, "(var spur|{}| nil)", name.into_inner()),
            },
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {}", stmt)?;
                }
                write!(f, ")")
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => match otherwise {
                Some(other) => write!(f, "(if-else {condition} {then} {other})"),
                None => write!(f, "(if {condition} {then})"),
            },
            Stmt::While {
                condition, body, ..
            } => write!(f, "(while {condition} {body})"),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr { expr } => Debug::fmt(&expr, f),
            Stmt::Print { loc, expr } => write!(f, "(print{loc} {expr:?})"),
            Stmt::Var { loc, name, init } => match init {
                Some(val) => write!(f, "(var{loc} spur|{}| {val:?})", name.into_inner()),
                None => write!(f, "(var{loc} spur|{}| nil)", name.into_inner()),
            },
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {:?}", stmt)?;
                }
                write!(f, ")")
            }
            Stmt::If {
                loc,
                condition,
                then,
                otherwise,
            } => match otherwise {
                Some(other) => write!(f, "(if-else{loc} {condition:?} {then:?} {other:?})"),
                None => write!(f, "(if{loc} {condition:?} {then:?})"),
            },
            Stmt::While {
                loc,
                condition,
                body,
            } => write!(f, "(while{loc} {condition:?} {body:?})"),
        }
    }
}
