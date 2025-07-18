use std::fmt::Display;

use crate::interp::interner::Interner;
use crate::util::{Loc, LoxToken};

use super::ast::Ast;
use super::token;

use self::macros::{ref_expr, val_expr};

pub struct ExprL {
    pub expr: Expr,
    pub loc: Loc,
}

pub enum Expr {
    ValExpr(ValExpr),
    RefExpr(RefExpr),
}

pub enum ValExpr {
    Literal {
        value: token::Literal,
    },
    Unary {
        operator: token::UnaryOp,
        right: ExprId,
    },
    Binary {
        left: ExprId,
        operator: token::BinaryOp,
        right: ExprId,
    },
    Grouping {
        expr: ExprId,
    },
    Logical {
        left: ExprId,
        kind: token::LogicalOp,
        right: ExprId,
    },
    Call {
        callee: ExprId,
        args: Box<[ExprId]>,
    },
}

pub enum RefExpr {
    Variable {
        var: token::Variable,
    },
    Grouping {
        expr: ExprId,
    },
    Assignment {
        var: token::Variable,
        value: ExprId,
    },
    Get {
        object: ExprId,
        prop: token::DotProp,
    },
    Set {
        object: ExprId,
        prop: token::DotProp,
        value: ExprId,
    },
    Super {
        prop: token::DotProp,
    },
    This,
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct ExprId {
    id: usize,
}

pub struct DisplayedExpr<'a, 'b, 'c> {
    expr: &'a Expr,
    interner: &'b Interner,
    ast: &'c Ast,
}

pub struct DisplayedValExpr<'a, 'b, 'c> {
    expr: &'a ValExpr,
    interner: &'b Interner,
    ast: &'c Ast,
}

pub struct DisplayedRefExpr<'a, 'b, 'c> {
    expr: &'a RefExpr,
    interner: &'b Interner,
    ast: &'c Ast,
}

impl Expr {
    /// Lox has maximum number of arguments for its functions, because the original implementation
    /// is in Java and it's limited there to 255 arguments only.
    pub const MAX_FUNC_ARGS: usize = 255;

    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedExpr<'a, 'b, 'c> {
        DisplayedExpr {
            expr: self,
            interner,
            ast,
        }
    }

    pub fn literal(value: token::Literal) -> Self {
        val_expr!(Literal { value })
    }

    pub fn unary(operator: token::UnaryOp, right: ExprId) -> Self {
        val_expr!(Unary { operator, right })
    }

    pub fn binary(left: ExprId, operator: token::BinaryOp, right: ExprId) -> Self {
        val_expr!(Binary {
            left,
            operator,
            right
        })
    }

    pub fn group_val(expr: ExprId) -> Self {
        Expr::ValExpr(ValExpr::Grouping { expr })
    }

    pub fn logical(left: ExprId, kind: token::LogicalOp, right: ExprId) -> Self {
        val_expr!(Logical { left, kind, right })
    }

    pub fn call(callee: ExprId, args: Box<[ExprId]>) -> Self {
        val_expr!(Call { callee, args })
    }

    pub fn variable(var: token::Variable) -> Self {
        ref_expr!(Variable { var })
    }

    pub fn group_ref(expr: ExprId) -> Self {
        Expr::RefExpr(RefExpr::Grouping { expr })
    }

    pub fn assignment(var: token::Variable, value: ExprId) -> Self {
        ref_expr!(Assignment { var, value })
    }

    pub fn get(object: ExprId, prop: token::DotProp) -> Self {
        Expr::RefExpr(RefExpr::Get { object, prop })
    }

    pub fn set(object: ExprId, prop: token::DotProp, value: ExprId) -> Self {
        Expr::RefExpr(RefExpr::Set {
            object,
            prop,
            value,
        })
    }

    pub fn this() -> Self {
        Expr::RefExpr(RefExpr::This)
    }

    pub fn super_(prop: token::DotProp) -> Self {
        Expr::RefExpr(RefExpr::Super { prop })
    }
}

impl ExprId {
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn inner(&self) -> usize {
        self.id
    }
}

impl ValExpr {
    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedValExpr<'a, 'b, 'c> {
        DisplayedValExpr {
            expr: self,
            interner,
            ast,
        }
    }
}

impl RefExpr {
    pub fn display<'a, 'b, 'c>(
        &'a self,
        interner: &'b Interner,
        ast: &'c Ast,
    ) -> DisplayedRefExpr<'a, 'b, 'c> {
        DisplayedRefExpr {
            expr: self,
            interner,
            ast,
        }
    }
}

impl Display for DisplayedExpr<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        let ast = self.ast;

        match self.expr {
            Expr::ValExpr(expr) => write!(f, "{}", expr.display(interner, ast)),
            Expr::RefExpr(expr) => write!(f, "{}", expr.display(interner, ast)),
        }
    }
}

impl Display for DisplayedValExpr<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        let ast = self.ast;

        match self.expr {
            ValExpr::Literal { value } => {
                write!(f, "{}", value.display(interner))
            }
            ValExpr::Unary { operator, right } => {
                let op = operator.as_str();
                let rhs = &ast.get_expr(right).expr;
                write!(f, "({op} {})", rhs.display(interner, ast))
            }
            ValExpr::Binary {
                left,
                operator,
                right,
            } => {
                let op = operator.as_str();
                let lhs = &ast.get_expr(left).expr;
                let rhs = &ast.get_expr(right).expr;
                write!(
                    f,
                    "({op} {} {})",
                    lhs.display(interner, ast),
                    rhs.display(interner, ast)
                )
            }
            ValExpr::Grouping { expr, .. } => {
                let expr = &ast.get_expr(expr).expr;
                write!(f, "(group {})", expr.display(interner, ast))
            }
            ValExpr::Logical { left, kind, right } => {
                let kind = kind.as_str();
                let lhs = &ast.get_expr(left).expr;
                let rhs = &ast.get_expr(right).expr;
                write!(
                    f,
                    "({kind} {} {})",
                    lhs.display(interner, ast),
                    rhs.display(interner, ast)
                )
            }
            ValExpr::Call { callee, args, .. } => {
                let callee = &ast.get_expr(callee).expr;
                write!(f, "(call {} ", callee.display(interner, ast))?;
                write!(f, "(args")?;
                for expr in args {
                    let expr = &ast.get_expr(expr).expr;
                    write!(f, " {}", expr.display(interner, ast))?;
                }
                write!(f, "))")
            }
        }
    }
}

impl Display for DisplayedRefExpr<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;
        let ast = self.ast;

        match self.expr {
            RefExpr::Variable { var } => {
                let name = interner.resolve(var.name);
                write!(f, "(var {name})")
            }
            RefExpr::Grouping { expr, .. } => {
                let expr = &self.ast.get_expr(expr).expr;
                write!(f, "{}", expr.display(interner, ast))
            }
            RefExpr::Assignment { var, value } => {
                let name = interner.resolve(var.name);
                let value = &ast.get_expr(value).expr;
                write!(f, "(= {name} {})", value.display(interner, ast))
            }
            RefExpr::Get { object, prop } => {
                let object = &ast.get_expr(object).expr;
                let prop = interner.resolve(prop.name);
                write!(f, "(get {} {prop})", object.display(interner, ast),)
            }
            RefExpr::Set { object, value, .. } => {
                let object = &ast.get_expr(object).expr;
                let value = &ast.get_expr(value).expr;
                write!(
                    f,
                    "(set {} {})",
                    object.display(interner, ast),
                    value.display(interner, ast)
                )
            }
            RefExpr::Super { prop, .. } => {
                let name = interner.resolve(prop.name);
                write!(f, "(super {name})")
            }
            RefExpr::This => write!(f, "(this)"),
        }
    }
}

mod macros {
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

    pub(crate) use ref_expr;
    pub(crate) use val_expr;
}
