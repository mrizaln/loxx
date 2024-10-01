#![cfg(test)]

use std::ops::Deref;

use indoc::indoc;

use crate::interp::interner::Interner;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::util::{Location, TokLoc};

use super::{expr::macros::*, expr::*, stmt::*, token};

macro_rules! tokl {
    ([$loc:expr] -> $type:ident::$var:ident) => {
        val_expr!($type {
            value: TokLoc {
                tok: token::$type::$var,
                loc: $loc,
            },
        })
    };
    ([$loc:expr] -> $type:ident::$var:ident = $value:expr) => {
        val_expr!($type {
            value: TokLoc {
                tok: token::$type::$var($value),
                loc: $loc,
            },
        })
    };
}

static EXPRESSION: &str = indoc! { r#"
    1 * (2 - 3) < 4 == false;
"# };

fn get_reference_ast() -> Expr {
    // given an expression below
    //      1 * (2 - 3) < 4 == false
    //      1   5   9   13  17  21
    // it should produce
    //      (== (< (* 1 ('group' (- 2 3))) 4) false)

    let loc = |l, c| Location { line: l, column: c };

    let lit1 = tokl! { [loc(1, 1)] -> Literal::Number = 1.0 };
    let lit2 = tokl! { [loc(1, 6)] -> Literal::Number = 2.0 };
    let lit3 = tokl! { [loc(1, 10)] -> Literal::Number = 3.0 };
    let lit4 = tokl! { [loc(1, 15)] -> Literal::Number = 4.0 };
    let litf = tokl! { [loc(1, 20)] -> Literal::False };

    let eqeq = TokLoc {
        tok: token::BinaryOp::Equal,
        loc: loc(1, 17),
    };
    let min = TokLoc {
        tok: token::BinaryOp::Sub,
        loc: loc(1, 8),
    };
    let star = TokLoc {
        tok: token::BinaryOp::Mul,
        loc: loc(1, 3),
    };
    let lt = TokLoc {
        tok: token::BinaryOp::Less,
        loc: loc(1, 13),
    };

    let bin_min = ValExpr::Binary {
        left: Box::new(lit2),
        operator: min,
        right: Box::new(lit3),
    };

    let grp1 = val_expr!(Grouping {
        expr: Box::new(bin_min),
    });

    let bin_1_grp1 = val_expr!(Binary {
        left: Box::new(lit1),
        operator: star,
        right: Box::new(grp1),
    });

    let bin_lt = val_expr!(Binary {
        left: Box::new(bin_1_grp1),
        operator: lt,
        right: Box::new(lit4),
    });

    val_expr!(Binary {
        left: Box::new(bin_lt),
        operator: eqeq,
        right: Box::new(litf),
    })
}

#[test]
fn print_expr_tree() {
    let expr = get_reference_ast();
    let interner = Interner::new();

    // the tokens inside the AST should not need interner here, so an empty one suffice

    println!("{}", expr.display(&interner));

    let result = "(== (< (* 1 (group (- 2 3))) 4) false)";
    assert_eq!(result, format!("{}", expr.display(&interner)))
}

#[test]
fn parse_to_a_correct_ast() {
    let mut interner = Interner::new();
    let lexer = Lexer::new(EXPRESSION, &mut interner);
    let result = lexer.scan();

    assert!(result.errors.is_empty());

    let mut parser = Parser::new();
    let expr = parser.parse(result.tokens);

    assert!(expr.is_ok());

    match expr.unwrap().statements.first().unwrap() {
        Stmt::Expr { expr } => assert_eq!(expr.deref(), &get_reference_ast()),
        _ => unreachable!(),
    };
}
