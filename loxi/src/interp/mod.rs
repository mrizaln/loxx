use std::rc::Rc;

use rustc_hash::FxHashMap;
use thiserror::Error;

use crate::native_fn;
use crate::parse::ast::Ast;
use crate::parse::expr::{Expr, ExprId, ExprL, RefExpr, ValExpr};
use crate::parse::stmt::{Stmt, StmtFunctionL, StmtId, StmtL, Unwind};
use crate::parse::{token, Program};
use crate::resolve::ResolveMap;
use crate::util::Location;

use self::class::{Class, Property};
use self::env::DynamicEnv;
use self::function::{Kind, Native, UserDefined};
use self::interner::{Interner, Key};
use self::value::{Value, ValueGen};

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

pub struct Interpreter {
    dyn_env: DynamicEnv,
    interner: Interner,
    resolve_map: ResolveMap,
    ast: Ast,
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

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            dyn_env: DynamicEnv::with_global(),
            interner: Interner::new(),
            resolve_map: ResolveMap::default(),
            ast: Ast::new(),
        };
        interp.populate_env();
        interp
    }

    pub fn interner_and_ast_mut(&mut self) -> (&mut Interner, &mut Ast) {
        (&mut self.interner, &mut self.ast)
    }

    pub fn interner_and_ast(&self) -> (&Interner, &Ast) {
        (&self.interner, &self.ast)
    }

    pub fn interpret(
        &mut self,
        program: Program,
        mut resolve_map: ResolveMap,
    ) -> Result<(), RuntimeError> {
        std::mem::swap(&mut self.resolve_map, &mut resolve_map);
        for stmt in program.statements.iter() {
            if let Unwind::Return(_) = self.execute(stmt)? {
                unreachable!("stray return detection should have been handled in Resolver!")
            }
        }
        Ok(())
    }

    fn populate_env(&mut self) {
        let mut define = |name: &str, args, body| {
            let name = self.interner.get_or_intern(name);
            let func = Native::new(name, args, body);
            self.dyn_env.define(name, Value::native_function(func));
        };

        define("clock", 0, native_fn::clock);

        #[cfg(feature = "loxlox")]
        {
            define("getc", 0, native_fn::loxlox::getc);
            define("chr", 1, native_fn::loxlox::chr);
            define("exit", 1, native_fn::loxlox::exit);
            define("print_error", 1, native_fn::loxlox::print_error);
        }
    }

    fn execute(&self, stmt: &StmtId) -> Result<Unwind, RuntimeError> {
        let StmtL { stmt, loc } = self.ast.get_stmt(stmt);
        match stmt {
            Stmt::Expr { expr } => {
                self.eval(expr)?;
                Ok(Unwind::None)
            }
            Stmt::Print { expr, .. } => {
                let value = self.eval(expr)?;
                println!("{}", value.display(&self.interner, &self.ast));
                Ok(Unwind::None)
            }
            Stmt::Var { name, init, .. } => {
                let value = match init {
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };

                self.dyn_env.define(*name, value);
                Ok(Unwind::None)
            }
            Stmt::Block { statements } => {
                let _local = self.dyn_env.create_scope();
                for stmt in statements {
                    if let Unwind::Return(value) = self.execute(stmt)? {
                        return Ok(Unwind::Return(value));
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
                    if let Unwind::Return(value) = self.execute(then)? {
                        Ok(Unwind::Return(value))
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
                    if let Unwind::Return(value) = self.execute(body)? {
                        return Ok(Unwind::Return(value));
                    }
                }
                Ok(Unwind::None)
            }
            Stmt::Function { func: id } => {
                let StmtFunctionL { func, .. } = self.ast.get_func(id);
                self.dyn_env.define(
                    func.name,
                    Value::function(UserDefined::new(
                        *id,
                        self.dyn_env.current(),
                        Kind::Function,
                    )),
                );
                Ok(Unwind::None)
            }
            Stmt::Return { value } => {
                let value = match value {
                    Some(expr) => self.eval(expr)?,
                    None => Value::nil(),
                };
                Ok(Unwind::Return(value))
            }
            Stmt::Class {
                name,
                base,
                methods,
            } => {
                let base = match base {
                    Some(expr_id) => {
                        let ExprL { expr, loc } = self.ast.get_expr(expr_id);
                        match expr {
                            Expr::RefExpr(RefExpr::Variable { .. }) => match self.eval(expr_id)? {
                                Value::Class(class) => Some(class),
                                _ => Err(RuntimeError::InheritNonClass(*loc))?,
                            },
                            _ => unreachable!("base should be a RefExpr::Variable"),
                        }
                    }
                    None => None,
                };

                // is this really necessary?
                self.dyn_env.define(*name, Value::Nil);

                let super_scope = match &base {
                    Some(base) => {
                        let guard = self.dyn_env.create_scope();
                        let superr = self.interner.key_super;
                        self.dyn_env.define(superr, Value::Class(Rc::clone(base)));
                        Some(guard)
                    }
                    None => None,
                };

                let mut methods_map = FxHashMap::default();
                let mut constructor = None;

                for method in methods.into_iter() {
                    let env = self.dyn_env.current();
                    let create_func = |kind| UserDefined::new(*method, env, kind);

                    let StmtFunctionL { func, .. } = self.ast.get_func(method);
                    if func.name == self.interner.key_init {
                        match &constructor {
                            None => constructor = Some(create_func(Kind::Constructor)),
                            Some(_) => unreachable!(
                                "duplicate declaration should have been handled in Resolver!"
                            ),
                        }
                    } else {
                        methods_map.insert(func.name, create_func(Kind::Function));
                    }
                }

                drop(super_scope);

                let value = Value::class(Class::new(*name, base, constructor, methods_map, *loc));
                self.dyn_env.modify_at(*name, 0, |v| *v = value);

                Ok(Unwind::None)
            }

            #[cfg(feature = "debug")]
            Stmt::Debug { expr } => {
                let value = self.eval(expr)?;
                eprintln!("{}", value.display(&self.interner, &self.ast));
                Ok(Unwind::None)
            }
        }
    }

    pub fn eval(&self, expr_id: &ExprId) -> Result<Value, RuntimeError> {
        let ExprL { expr, loc } = self.ast.get_expr(expr_id);
        match expr {
            Expr::ValExpr(expr) => self.eval_val(expr, *loc),
            Expr::RefExpr(expr) => self.eval_ref(expr, *expr_id, *loc),
        }
    }

    fn eval_val(&self, expr: &ValExpr, loc: Location) -> Result<Value, RuntimeError> {
        match expr {
            ValExpr::Literal { value } => match value {
                token::Literal::Number(num) => Ok(Value::number(*num)),
                token::Literal::String(str) => Ok(Value::string_literal(*str)),
                token::Literal::True => Ok(Value::bool(true)),
                token::Literal::False => Ok(Value::bool(false)),
                token::Literal::Nil => Ok(Value::nil()),
            },
            ValExpr::Grouping { expr, .. } => self.eval(expr),
            ValExpr::Unary { operator, right } => {
                let value = self.eval(right)?;
                match operator {
                    token::UnaryOp::Minus => value.minus(),
                    token::UnaryOp::Not => Ok(value.not()),
                }
                .map_err(|err| match err {
                    value::InvalidOp::Unary(s) => {
                        RuntimeError::InvalidUnaryOp(loc, operator.clone(), s)
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

                match operator {
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
                        RuntimeError::InvalidBinaryOp(loc, operator.clone(), l, r)
                    }
                    _ => unreachable!("BinaryOp should only return Binary variant of InvalidOp"),
                })
            }
            ValExpr::Logical { left, kind, right } => {
                let lhs = self.eval(left)?;
                match (&kind, lhs.truthiness()) {
                    (token::LogicalOp::And, false) => Ok(lhs),
                    (token::LogicalOp::Or, true) => Ok(lhs),
                    (_, _) => self.eval(right),
                }
            }
            ValExpr::Call { callee, args } => {
                let callee = self.eval(callee)?;

                let mut gen = |expr_id| self.eval(&expr_id);
                let args = ValueGen::new(args, &mut gen);

                match callee {
                    Value::Function(func) => match func.as_ref() {
                        function::Function::Native(func) => func.call(self, args, loc),
                        function::Function::UserDefined(func) => func.call(self, args, loc),
                    },
                    Value::Class(class) => {
                        let instance = class.construct(loc, self, args)?;
                        Ok(Value::Instance(instance))
                    }
                    _ => Err(RuntimeError::NotCallable(loc)),
                }
            }
        }
    }

    fn eval_ref(&self, expr: &RefExpr, id: ExprId, loc: Location) -> Result<Value, RuntimeError> {
        match expr {
            RefExpr::Variable { var } => self.lookup_var(id, var.name).ok_or_else(|| {
                let var_name = self.interner.resolve(var.name);
                RuntimeError::UndefinedVariable(loc, var_name.to_string())
            }),
            RefExpr::Grouping { expr, .. } => self.eval(expr),
            RefExpr::Assignment { var, value } => {
                let value = self.eval(value)?;
                match self.modify_var(id, var.name, |v| *v = value) {
                    Some(_) => Ok(self.lookup_var(id, var.name).unwrap()),
                    None => Err(RuntimeError::UndefinedVariable(
                        loc,
                        self.interner.resolve(var.name).to_owned(),
                    )),
                }
            }
            RefExpr::Get { object, prop } => match self.eval(object)? {
                Value::Instance(instance) => match instance.get(prop.name, &self.interner) {
                    None => Err(RuntimeError::UndefinedProperty(loc)),
                    Some(prop) => match prop {
                        Property::Field(value) => Ok(value),
                        Property::Method(func) => Ok(Value::function(func)),
                    },
                },
                _ => Err(RuntimeError::InvalidPropertyAccess(loc)),
            },
            RefExpr::Set {
                object,
                prop,
                value,
            } => match self.eval(object)? {
                Value::Instance(instance) => {
                    let value = self.eval(value)?;
                    instance.set(prop.name, value.clone());
                    Ok(value)
                }
                _ => Err(RuntimeError::InvalidPropertyAccess(loc)),
            },
            RefExpr::This => {
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

                match find_func(prop.name) {
                    None => Err(RuntimeError::UndefinedProperty(loc)),
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

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
