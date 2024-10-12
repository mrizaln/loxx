use std::rc::Rc;

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::parse::expr::{Expr, ExprId, RefExpr, ValExpr};
use crate::parse::{stmt::Stmt, stmt::Unwind, token, Program};
use crate::resolve::ResolveMap;
use crate::util::{Location, TokLoc};

use self::class::{Class, Property};
use self::env::DynamicEnv;
use self::function::{Kind, Native, UserDefined};
use self::interner::{Interner, Key};
use self::value::Value;

pub mod class;
pub mod env;
pub mod function;
pub mod interner;
pub mod value;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{0} RuntimeError: Invalid binary operation '{1}' between '{2}' and '{3}'")]
    InvalidBinaryOp(Location, token::BinaryOp, &'static str, &'static str),

    #[error("{0} RuntimeError: Invalid unary operation '{1}' on '{2}'")]
    InvalidUnaryOp(Location, token::UnaryOp, &'static str),

    #[error("{0} RuntimeError: Trying to access undefined variable: '{1}'")]
    UndefinedVariable(Location, String),

    #[error("{0}")]
    FunctionError(#[from] function::FunctionError),

    #[error("{0} RuntimeError: Trying to access a property on a non-instance object")]
    InvalidPropertyAccess(Location),

    #[error("{0} RuntimeError: Trying to access an undefined property")]
    UndefinedProperty(Location),

    #[error("{0} RuntimeError: Not a function or a callable object")]
    NotCallable(Location),

    #[error("{0} RuntimeError: Inherit target must be a class")]
    InheritNonClass(Location),
}

impl RuntimeError {
    pub fn loc(&self) -> Location {
        match self {
            RuntimeError::InvalidBinaryOp(loc, _, _, _) => *loc,
            RuntimeError::InvalidUnaryOp(loc, _, _) => *loc,
            RuntimeError::UndefinedVariable(loc, _) => *loc,
            RuntimeError::FunctionError(err) => err.loc(),
            RuntimeError::NotCallable(loc) => *loc,
            RuntimeError::InvalidPropertyAccess(loc) => *loc,
            RuntimeError::UndefinedProperty(loc) => *loc,
            RuntimeError::InheritNonClass(loc) => *loc,
        }
    }
}

pub struct Interpreter {
    dyn_env: DynamicEnv,
    interner: Interner,
    resolve_map: ResolveMap,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            dyn_env: DynamicEnv::new_with_global(),
            interner: Interner::new(),
            resolve_map: ResolveMap::default(),
        };
        interp.populate_env();
        interp
    }

    pub fn interner(&mut self) -> &mut Interner {
        &mut self.interner
    }

    pub fn interpret(
        &mut self,
        program: Program,
        mut resolve_map: ResolveMap,
    ) -> Result<(), RuntimeError> {
        std::mem::swap(&mut self.resolve_map, &mut resolve_map);
        for stmt in program.statements.iter() {
            match self.execute(stmt)? {
                Unwind::None => (),
                Unwind::Return(_, _) => {
                    unreachable!("stray return detection should have been handled in Resolver!")
                }
            }
        }
        Ok(())
    }

    fn populate_env(&mut self) {
        let name = self.interner.get_or_intern("clock");
        let clock = Native::new(name, Box::new([]), native_functions::clock);
        self.dyn_env.define(name, Value::native_function(clock));
    }

    fn execute(&self, stmt: &Stmt) -> Result<Unwind, RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => {
                self.eval(expr)?;
                Ok(Unwind::None)
            }
            Stmt::Print { expr, .. } => {
                let value = self.eval(expr)?;
                println!("{}", value.display(&self.interner));
                Ok(Unwind::None)
            }
            Stmt::Var { name, init, .. } => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };

                // TODO: add location metadata
                self.dyn_env.define(*name, value);
                Ok(Unwind::None)
            }
            Stmt::Block { statements } => {
                let _local = self.dyn_env.create_scope();
                for stmt in statements {
                    if let Unwind::Return(value, loc) = self.execute(stmt)? {
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
            } => match self.eval(condition)?.truthiness() {
                true => {
                    if let Unwind::Return(value, loc) = self.execute(then)? {
                        Ok(Unwind::Return(value, loc))
                    } else {
                        Ok(Unwind::None)
                    }
                }
                false => {
                    if let Some(stmt) = otherwise {
                        Ok(self.execute(stmt)?)
                    } else {
                        Ok(Unwind::None)
                    }
                }
            },
            Stmt::While {
                condition, body, ..
            } => {
                while self.eval(condition)?.truthiness() {
                    if let Unwind::Return(value, loc) = self.execute(body)? {
                        return Ok(Unwind::Return(value, loc));
                    }
                }
                Ok(Unwind::None)
            }
            Stmt::Function { func } => {
                self.dyn_env.define(
                    func.name,
                    Value::function(UserDefined::new(
                        Rc::clone(func),
                        self.dyn_env.current(),
                        Kind::Function,
                    )),
                );
                Ok(Unwind::None)
            }
            Stmt::Return { value, loc } => {
                let value = match value {
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };
                Ok(Unwind::Return(value, *loc))
            }
            Stmt::Class {
                loc,
                name,
                base,
                methods,
            } => {
                let base = match base {
                    Some(expr) => match expr {
                        Expr::RefExpr(RefExpr::Variable { var }, _) => match self.eval(expr)? {
                            Value::Class(class) => Some(class),
                            _ => Err(RuntimeError::InheritNonClass(var.loc))?,
                        },
                        _ => unreachable!("base should be a RefExpr::Variable"),
                    },
                    None => None,
                };

                // is this really necessary?
                self.dyn_env.define(*name, Value::Nil);

                let super_scope = match &base {
                    Some(base) => {
                        let guard = self.dyn_env.create_scope();
                        self.dyn_env
                            .define(self.interner.key_super, Value::Class(Rc::clone(base)));
                        Some(guard)
                    }
                    None => None,
                };

                let mut methods_map = FxHashMap::default();
                let mut constructor = None;

                for method in methods.into_iter() {
                    let env = self.dyn_env.current();
                    let func = |kind| UserDefined::new(Rc::clone(method), env, kind);

                    if method.name == self.interner.key_init {
                        match &constructor {
                            None => constructor = Some(func(Kind::Constructor)),
                            Some(_) => unreachable!(
                                "duplicate declaration should have been handled in Resolver!"
                            ),
                        }
                    } else {
                        methods_map.insert(method.name, func(Kind::Function));
                    }
                }

                drop(super_scope);

                let value = Value::class(Class::new(*name, base, constructor, methods_map, *loc));
                self.dyn_env.modify_at(*name, 0, |v| *v = value);

                Ok(Unwind::None)
            }
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::ValExpr(expr, _id) => self.eval_val(expr),
            Expr::RefExpr(expr, id) => self.eval_ref(expr, *id),
        }
    }

    fn eval_val(&self, expr: &ValExpr) -> Result<Value, RuntimeError> {
        match expr {
            ValExpr::Literal { value } => match &value.tok {
                token::Literal::Number(num) => Ok(Value::number(*num)),
                token::Literal::String(str) => Ok(Value::string_literal(*str)),
                token::Literal::True => Ok(Value::bool(true)),
                token::Literal::False => Ok(Value::bool(false)),
                token::Literal::Nil => Ok(Value::nil()),
            },
            ValExpr::Grouping { expr, .. } => self.eval_val(expr),
            ValExpr::Unary { operator, right } => {
                let value = self.eval(right)?;
                match operator.tok {
                    token::UnaryOp::Minus => value.minus(),
                    token::UnaryOp::Not => Ok(value.not()),
                }
                .map_err(|err| match err {
                    value::InvalidOp::Unary(s) => {
                        RuntimeError::InvalidUnaryOp(operator.loc, operator.tok.clone(), s)
                    }
                    _ => unreachable!("UnaryOp should only return Unary variant of InvalidOp"),
                })
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let lhs = self.eval(left)?;
                let rhs = self.eval(right)?;

                match operator.tok {
                    token::BinaryOp::Add => lhs.add(rhs, &self.interner),
                    token::BinaryOp::Sub => lhs.sub(rhs),
                    token::BinaryOp::Mul => lhs.mul(rhs),
                    token::BinaryOp::Div => lhs.div(rhs),
                    token::BinaryOp::Equal => Ok(lhs.eq(&rhs, &self.interner)),
                    token::BinaryOp::NotEqual => Ok(lhs.neq(&rhs, &self.interner)),
                    token::BinaryOp::Less => lhs.lt(&rhs),
                    token::BinaryOp::LessEq => lhs.le(&rhs),
                    token::BinaryOp::Greater => lhs.gt(&rhs),
                    token::BinaryOp::GreaterEq => lhs.ge(&rhs),
                }
                .map_err(|err| match err {
                    value::InvalidOp::Binary(l, r) => {
                        RuntimeError::InvalidBinaryOp(operator.loc, operator.tok.clone(), l, r)
                    }
                    _ => unreachable!("BinaryOp should only return Binary variant of InvalidOp"),
                })
            }
            ValExpr::Logical { left, kind, right } => {
                let lhs = self.eval(left)?;
                match (&kind.tok, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => Ok(lhs),
                    (token::LogicalOp::Or, true) => Ok(lhs),
                    (_, _) => self.eval(right),
                }
            }
            ValExpr::Call { callee, loc, args } => {
                let callee = self.eval(callee)?;

                match callee {
                    Value::Function(func) => {
                        let args = args
                            .into_iter()
                            .map(|a| self.eval(a))
                            .collect::<Result<Box<[_]>, _>>()?;
                        match func.as_ref() {
                            function::Function::Native(func) => func.call(args),
                            function::Function::UserDefined(func) => func.call(args, self),
                        }
                    }
                    Value::Class(class) => {
                        let args = args
                            .into_iter()
                            .map(|a| self.eval(a))
                            .collect::<Result<Box<[_]>, _>>()?;
                        let instance = class.construct(args, *loc, self)?;
                        Ok(Value::Instance(instance))
                    }
                    _ => Err(RuntimeError::NotCallable(*loc)),
                }
            }
        }
    }

    fn eval_ref(&self, expr: &RefExpr, id: ExprId) -> Result<Value, RuntimeError> {
        match expr {
            RefExpr::Variable {
                var: TokLoc { tok, loc },
            } => self.lookup_var(id, tok.name).ok_or_else(|| {
                let var_name = self.interner.resolve(tok.name);
                RuntimeError::UndefinedVariable(*loc, var_name.to_string())
            }),
            RefExpr::Grouping { expr, .. } => self.eval_ref(expr, id),
            RefExpr::Assignment { var, value } => {
                let value = self.eval(value)?;
                match self.modify_var(id, var.tok.name, |v| *v = value) {
                    Some(_) => Ok(self.lookup_var(id, var.tok.name).unwrap()),
                    None => Err(RuntimeError::UndefinedVariable(
                        var.loc,
                        self.interner.resolve(var.tok.name).to_owned(),
                    )),
                }
            }
            RefExpr::Get { object, prop } => match self.eval(object)? {
                Value::Instance(instance) => match instance.get(prop.tok.name, &self.interner) {
                    None => Err(RuntimeError::UndefinedProperty(prop.loc)),
                    Some(prop) => match prop {
                        Property::Field(value) => Ok(value),
                        Property::Method(func) => Ok(Value::function(func)), // TODO: edit
                    },
                },
                _ => Err(RuntimeError::InvalidPropertyAccess(prop.loc)),
            },
            RefExpr::Set {
                object,
                prop,
                value,
            } => match self.eval(object)? {
                Value::Instance(instance) => {
                    let value = self.eval(value)?;
                    instance.set(prop.tok.name, value.clone());
                    Ok(value)
                }
                _ => Err(RuntimeError::InvalidPropertyAccess(prop.loc)),
            },
            RefExpr::This { .. } => {
                let this = self.interner.key_this;
                match self.lookup_var(id, this) {
                    Some(value) => Ok(value),
                    None => unreachable!(
                        "stray this keyword detection should have been handled in Resolver!"
                    ),
                }
            }
            RefExpr::Super { prop } => {
                let distance = match self.resolve_map.distance(id) {
                    Some(dist) => dist,
                    None => panic!("super should be inside resolve_map: {}", self.resolve_map),
                };

                let superr = self.interner.key_super;
                let base = match self.dyn_env.get_at(superr, distance) {
                    Some(Value::Class(class)) => class,
                    _ => unreachable!("super should be available as class here "),
                };

                // NOTE: the layout of the Env makes sure "this" available below current scope
                let this = self.interner.key_this;
                let instance = match self.dyn_env.get_at(this, distance - 1) {
                    Some(Value::Instance(instance)) => instance,
                    _ => unreachable!("this should be available as instance here"),
                };

                let find_func = |name: Key| match name {
                    x if x == self.interner.key_init => base.find_ctor(),
                    _ => base.find_method(name),
                };

                match find_func(prop.tok.name) {
                    None => Err(RuntimeError::UndefinedProperty(prop.loc)),
                    Some(method) => Ok(Value::function(method.bind(instance, &self.interner))),
                }
            }
        }
    }

    fn lookup_var(&self, expr_id: ExprId, key: Key) -> Option<Value> {
        match self.resolve_map.distance(expr_id) {
            Some(distance) => self.dyn_env.get_at(key, distance),
            None => self.dyn_env.get_global(key),
        }
    }

    fn modify_var<F, R>(&self, expr_id: ExprId, key: Key, f: F) -> Option<R>
    where
        F: FnOnce(&mut Value) -> R,
    {
        match self.resolve_map.distance(expr_id) {
            Some(distance) => self.dyn_env.modify_at(key, distance, f),
            None => self.dyn_env.modify_global(key, f),
        }
    }
}

mod native_functions {
    use super::*;

    pub fn clock(_args: Box<[Value]>) -> Result<Value, RuntimeError> {
        let now = std::time::SystemTime::now();
        let seconds = now
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64();
        Ok(Value::number(seconds))
    }
}
