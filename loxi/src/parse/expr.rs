use std::fmt::Display;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::token;
use crate::interp::interner::Interner;
use crate::util::{Location, LoxToken, TokLoc};

use macros::*;

#[derive(Debug, Clone)]
pub enum Expr {
    ValExpr(ValExpr, ExprId),
    RefExpr(RefExpr, ExprId),
}

/// ExprId is used to identify an expression, it's ignored in comparison and ordering.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId {
    id: usize,
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
        loc: Location,
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
        loc: Location,
    },
    Assignment {
        var: TokLoc<token::Variable>,
        value: Box<Expr>,
    },
    Get {
        object: Box<Expr>,
        prop: TokLoc<token::DotProp>,
    },
    Set {
        object: Box<Expr>,
        prop: TokLoc<token::DotProp>,
        value: Box<Expr>,
    },
    This {
        loc: Location,
    },
    Super {
        prop: TokLoc<token::DotProp>,
    },
}

pub struct DisplayedExpr<'a, 'b> {
    expr: &'a Expr,
    interner: &'b Interner,
}

pub struct DisplayedValExpr<'a, 'b> {
    expr: &'a ValExpr,
    interner: &'b Interner,
}

pub struct DisplayedRefExpr<'a, 'b> {
    expr: &'a RefExpr,
    interner: &'b Interner,
}

impl Expr {
    /// Lox has maximum number of arguments for its functions, because the original implementation
    /// is in Java and it's limited there to 255 arguments only.
    pub const MAX_FUNC_ARGS: usize = 255;

    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedExpr<'a, 'b> {
        DisplayedExpr {
            expr: self,
            interner,
        }
    }

    pub fn literal(value: TokLoc<token::Literal>) -> Self {
        val_expr!(Literal { value })
    }

    pub fn unary(operator: TokLoc<token::UnaryOp>, right: Box<Expr>) -> Self {
        val_expr!(Unary { operator, right })
    }

    pub fn binary(left: Box<Expr>, operator: TokLoc<token::BinaryOp>, right: Box<Expr>) -> Self {
        val_expr!(Binary {
            left,
            operator,
            right
        })
    }

    // group inherits its inner ExprId
    pub fn group_val(expr: Box<ValExpr>, loc: Location, id: ExprId) -> Self {
        Expr::ValExpr(ValExpr::Grouping { expr, loc }, id)
    }

    pub fn logical(left: Box<Expr>, kind: TokLoc<token::LogicalOp>, right: Box<Expr>) -> Self {
        val_expr!(Logical { left, kind, right })
    }

    pub fn call(callee: Box<Expr>, args: Box<[Expr]>, loc: Location) -> Self {
        val_expr!(Call { callee, args, loc })
    }

    pub fn variable(var: TokLoc<token::Variable>) -> Self {
        ref_expr!(Variable { var })
    }

    // groups inherits its inner ExprId
    pub fn group_ref(expr: Box<RefExpr>, loc: Location, id: ExprId) -> Self {
        Expr::RefExpr(RefExpr::Grouping { expr, loc }, id)
    }

    pub fn assignment(var: TokLoc<token::Variable>, value: Box<Expr>) -> Self {
        ref_expr!(Assignment { var, value })
    }

    pub fn get(object: Box<Expr>, prop: TokLoc<token::DotProp>) -> Self {
        Expr::RefExpr(RefExpr::Get { object, prop }, ExprId::new())
    }

    pub fn set(object: Box<Expr>, prop: TokLoc<token::DotProp>, value: Box<Expr>) -> Self {
        Expr::RefExpr(
            RefExpr::Set {
                object,
                prop,
                value,
            },
            ExprId::new(),
        )
    }

    pub fn this(loc: Location) -> Self {
        Expr::RefExpr(RefExpr::This { loc }, ExprId::new())
    }

    pub fn superr(prop: TokLoc<token::DotProp>) -> Self {
        Expr::RefExpr(RefExpr::Super { prop }, ExprId::new())
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::ValExpr(lhs, _), Expr::ValExpr(rhs, _)) => lhs == rhs,
            (Expr::RefExpr(lhs, _), Expr::RefExpr(rhs, _)) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Expr::ValExpr(lhs, _), Expr::ValExpr(rhs, _)) => lhs.partial_cmp(rhs),
            (Expr::RefExpr(lhs, _), Expr::RefExpr(rhs, _)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl ExprId {
    pub fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(1);
        Self {
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
        }
    }
}

impl ValExpr {
    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedValExpr<'a, 'b> {
        DisplayedValExpr {
            expr: self,
            interner,
        }
    }
}

impl RefExpr {
    pub fn display<'a, 'b>(&'a self, interner: &'b Interner) -> DisplayedRefExpr<'a, 'b> {
        DisplayedRefExpr {
            expr: self,
            interner,
        }
    }
}

impl Display for DisplayedExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.expr {
            Expr::ValExpr(expr, _) => write!(f, "{}", expr.display(interner)),
            Expr::RefExpr(expr, _) => write!(f, "{}", expr.display(interner)),
        }
    }
}

impl Display for DisplayedValExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.expr {
            ValExpr::Literal { value } => {
                write!(f, "{}", value.tok.display(interner))
            }
            ValExpr::Unary { operator, right } => {
                let op = operator.tok.as_str();
                write!(f, "({op} {})", right.display(interner))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                write!(
                    f,
                    "({} {} {})",
                    operator.tok.as_str(),
                    left.display(interner),
                    right.display(interner)
                )
            }
            ValExpr::Grouping { expr, .. } => {
                write!(f, "(group {})", expr.display(interner))
            }
            ValExpr::Logical { left, kind, right } => {
                write!(
                    f,
                    "({} {} {})",
                    kind.tok.as_str(),
                    left.display(interner),
                    right.display(interner)
                )
            }
            ValExpr::Call { callee, args, .. } => {
                write!(f, "(call {} ", callee.display(interner))?;
                write!(f, "(args")?;
                for expr in args {
                    write!(f, " {}", expr.display(interner))?;
                }
                write!(f, "))")
            }
        }
    }
}

impl Display for DisplayedRefExpr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        match self.expr {
            RefExpr::Variable { var } => write!(f, "(var {})", interner.resolve(var.tok.name)),
            RefExpr::Grouping { expr, .. } => write!(f, "{}", expr.display(interner)),
            RefExpr::Assignment { var, value } => {
                let name = interner.resolve(var.tok.name);
                write!(f, "(= {} {})", name, value.display(interner))
            }
            RefExpr::Get { object, prop } => {
                write!(
                    f,
                    "(get {} {})",
                    object.display(interner),
                    interner.resolve(prop.tok.name)
                )
            }
            RefExpr::Set { object, value, .. } => write!(
                f,
                "(set {} {})",
                object.display(interner),
                value.display(interner)
            ),
            RefExpr::This { .. } => write!(f, "(this)"),
            RefExpr::Super { prop, .. } => {
                write!(f, "(super {})", interner.resolve(prop.tok.name))
            }
        }
    }
}

mod macros {
    macro_rules! val_expr {
        ($kind:tt $xpr:tt) => {
            Expr::ValExpr(ValExpr::$kind $xpr, ExprId::new())
        };
    }

    macro_rules! ref_expr {
        ($kind:tt $xpr:tt) => {
            Expr::RefExpr(RefExpr::$kind $xpr, ExprId::new())
        };
    }

    pub(crate) use ref_expr;
    pub(crate) use val_expr;
}
