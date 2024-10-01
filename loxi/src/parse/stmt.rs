use std::fmt::{Debug, Display};

use lasso::{Rodeo, Spur};

use crate::interp::{env::Env, function::Function, value::Value, RuntimeError};
use crate::util::Location;

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>,
    },
    Print {
        loc: Location,
        expr: Box<Expr>,
    },
    Var {
        loc: Location,
        name: Spur,
        init: Option<Box<Expr>>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        loc: Location,
        condition: Box<Expr>,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    While {
        loc: Location,
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Function {
        func: Box<Function>,
    },
    Return {
        loc: Location,
        value: Option<Box<Expr>>,
    },
}

/// A wrapper for `Stmt` that can be displayed. This is necessary to "pass" arena as addtional
/// argument to `Display::fmt` method.
pub struct DisplayedStmt<'a, 'b> {
    stmt: &'a Stmt,
    arena: &'b Rodeo,
}

pub enum Unwind {
    None,
    Return(Value, Location),
}

impl Stmt {
    // TODO: handle return statement
    pub fn execute(&self, env: &mut Env, arena: &Rodeo) -> Result<Unwind, RuntimeError> {
        match self {
            Stmt::Expr { expr } => {
                expr.eval_unit(env, arena)?;
                Ok(Unwind::None)
            }
            Stmt::Print { expr, .. } => {
                expr.eval_fn(env, arena, |v| println!("{}", v.display(arena)))?;
                Ok(Unwind::None)
            }
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
                    None => Value::nil(),
                };

                // TODO: add location metadata
                env.define(name.clone(), value);
                Ok(Unwind::None)
            }
            Stmt::Block { statements } => {
                let mut new_env = env.child();
                for stmt in statements {
                    if let Unwind::Return(value, loc) = stmt.execute(&mut new_env, arena)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => match condition.eval_fn(env, arena, |v| v.truthiness())? {
                true => {
                    if let Unwind::Return(value, loc) = then.execute(env, arena)? {
                        Ok(Unwind::Return(value, loc))
                    } else {
                        Ok(Unwind::None)
                    }
                }
                false => {
                    if let Some(stmt) = otherwise {
                        Ok(stmt.execute(env, arena)?)
                    } else {
                        Ok(Unwind::None)
                    }
                }
            },
            Stmt::While {
                condition, body, ..
            } => {
                while condition.eval_fn(env, arena, |v| v.truthiness())? {
                    if let Unwind::Return(value, loc) = body.execute(env, arena)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            // should I really clone here?
            Stmt::Function { func } => {
                env.define(func.name, Value::function(*func.clone()));
                Ok(Unwind::None)
            }
            Stmt::Return { value, loc } => {
                let value = match value {
                    Some(expr) => expr.eval_cloned(env, arena)?,
                    None => Value::nil(),
                };
                Ok(Unwind::Return(value, *loc))
            }
        }
    }

    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedStmt<'a, 'b> {
        DisplayedStmt { stmt: self, arena }
    }
}

impl Display for DisplayedStmt<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.stmt {
            Stmt::Expr { expr } => Display::fmt(&expr.display(arena), f),
            Stmt::Print { expr, .. } => write!(f, "(print {})", expr.display(arena)),
            Stmt::Var { name, init, .. } => {
                let name = arena.resolve(name);
                match init {
                    Some(val) => write!(f, "(var {} {})", name, val.display(arena)),
                    None => write!(f, "(var {} nil)", name),
                }
            }
            Stmt::Block { statements } => {
                write!(f, "(block")?;
                for stmt in statements {
                    write!(f, " {}", stmt.display(arena))?;
                }
                write!(f, ")")
            }
            Stmt::If {
                condition,
                then,
                otherwise,
                ..
            } => {
                let condition = condition.display(arena);
                let then = then.display(arena);
                match otherwise {
                    Some(other) => {
                        let other = other.display(arena);
                        write!(f, "(if-else {condition} {then} {other})")
                    }
                    None => {
                        write!(f, "(if {condition} {then})")
                    }
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                let condition = condition.display(arena);
                let body = body.display(arena);
                write!(f, "(while {condition} {body})")
            }
            Stmt::Function { func } => {
                write!(f, "(fun (")?;
                for param in &func.params {
                    write!(f, " {}", arena.resolve(param))?;
                }
                write!(f, ")")?;
                for stmt in &func.body {
                    write!(f, " {}", stmt.display(arena))?;
                }
                write!(f, ")")
            }
            Stmt::Return { value, .. } => match value {
                Some(val) => write!(f, "(return {})", val.display(arena)),
                None => write!(f, "(return nil)"),
            },
        }
    }
}

impl Unwind {
    pub fn expect_none(self) {
        match self {
            Unwind::None => {}
            _ => panic!("expected no value"),
        }
    }
}
