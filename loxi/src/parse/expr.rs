use std::fmt::Display;

use lasso::Rodeo;

use super::token;
use crate::util::{Location, TokLoc};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    ValExpr(ValExpr),
    RefExpr(RefExpr),
}

/// Expression that produces `Value`
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ValExpr {
    Literal {
        value: TokLoc<token::Literal>,
    },
    Unary {
        operator: TokLoc<token::UnaryOp>,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: TokLoc<token::BinaryOp>,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<ValExpr>,
    },
    Logical {
        left: Box<Expr>,
        kind: TokLoc<token::LogicalOp>,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Box<[Expr]>,
        loc: Location,
    },
}

/// Expression that produces a reference to `Value`
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RefExpr {
    Variable {
        var: TokLoc<token::Variable>,
    },
    Grouping {
        expr: Box<RefExpr>,
    },
    Assignment {
        var: TokLoc<token::Variable>,
        value: Box<Expr>,
    },
}

pub struct DisplayedExpr<'a, 'b> {
    expr: &'a Expr,
    arena: &'b Rodeo,
}

pub struct DisplayedValExpr<'a, 'b> {
    expr: &'a ValExpr,
    arena: &'b Rodeo,
}

pub struct DisplayedRefExpr<'a, 'b> {
    expr: &'a RefExpr,
    arena: &'b Rodeo,
}

impl Expr {
    /// Lox has maximum number of arguments for its functions, because the original implementation
    /// is in Java and it's limited there to 255 arguments only.
    pub const MAX_FUNC_ARGS: usize = 255;

    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedExpr<'a, 'b> {
        DisplayedExpr { expr: self, arena }
    }
}

impl ValExpr {
    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedValExpr<'a, 'b> {
        DisplayedValExpr { expr: self, arena }
    }
}

impl RefExpr {
    pub fn display<'a, 'b>(&'a self, arena: &'b Rodeo) -> DisplayedRefExpr<'a, 'b> {
        DisplayedRefExpr { expr: self, arena }
    }
}

impl Display for DisplayedExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            Expr::ValExpr(expr) => write!(f, "{}", expr.display(arena)),
            Expr::RefExpr(expr) => write!(f, "{}", expr.display(arena)),
        }
    }
}

impl Display for DisplayedValExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            ValExpr::Literal { value } => {
                write!(f, "{}", value.tok.display(arena))
            }
            ValExpr::Unary { operator, right } => {
                let op: &str = (&operator.tok).into();
                write!(f, "({op} {})", right.display(arena))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let op: &str = (&operator.tok).into();
                write!(f, "({op} {} {})", left.display(arena), right.display(arena))
            }
            ValExpr::Grouping { expr } => {
                write!(f, "(group {})", expr.display(arena))
            }
            ValExpr::Logical { left, kind, right } => {
                let op: &str = (&kind.tok).into();
                write!(f, "({op} {} {})", left.display(arena), right.display(arena))
            }
            ValExpr::Call { callee, args, .. } => {
                write!(f, "(call {} ", callee.display(arena))?;
                write!(f, "(args")?;
                for expr in args {
                    write!(f, " {}", expr.display(arena))?;
                }
                write!(f, "))")
            }
        }
    }
}

impl Display for DisplayedRefExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arena = self.arena;
        match self.expr {
            RefExpr::Variable { var } => write!(f, "(var {})", arena.resolve(&var.tok.name)),
            RefExpr::Grouping { expr } => write!(f, "{}", expr.display(arena)),
            RefExpr::Assignment { var, value } => {
                let name = arena.resolve(&var.tok.name);
                write!(f, "(= {} {})", name, value.display(arena))
            }
        }
    }
}

pub mod macros {
    macro_rules! val_expr {
        ($kind:tt $xpr:tt) => {
            Expr::ValExpr(ValExpr::$kind $xpr)
        };
    }

    macro_rules! ref_expr {
        ($kind:tt $xpr:tt) => {
            Expr::RefExpr(RefExpr::$kind $xpr)
        };
    }

    macro_rules! group_expr {
        ($xpr:expr) => {
            match $xpr {
                Expr::ValExpr(expr) => val_expr!(Grouping {
                    expr: Box::new(expr)
                }),
                Expr::RefExpr(expr) => ref_expr!(Grouping {
                    expr: Box::new(expr)
                }),
            }
        };
    }

    pub(crate) use group_expr;
    pub(crate) use ref_expr;
    pub(crate) use val_expr;
}
