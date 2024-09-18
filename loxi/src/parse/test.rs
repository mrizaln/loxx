#![cfg(test)]

use indoc::indoc;

use crate::lex::Lexer;
use crate::parse::Parser;
use crate::util::{Location, TokLoc};

use super::{expr::*, stmt::*, token};

macro_rules! tokl {
    ([$loc:expr] -> $type:ident::$var:ident) => {
        Expr::$type {
            value: TokLoc {
                tok: token::$type::$var,
                loc: $loc,
            },
        }
    };
    ([$loc:expr] -> $type:ident::$var:ident = $value:expr) => {
        Expr::$type {
            value: TokLoc {
                tok: token::$type::$var($value),
                loc: $loc,
            },
        }
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

    let bin_min = Expr::Binary {
        left: Box::new(lit2),
        operator: min,
        right: Box::new(lit3),
    };

    let grp1 = Expr::Grouping {
        expr: Box::new(bin_min),
    };

    let bin_1_grp1 = Expr::Binary {
        left: Box::new(lit1),
        operator: star,
        right: Box::new(grp1),
    };

    let bin_lt = Expr::Binary {
        left: Box::new(bin_1_grp1),
        operator: lt,
        right: Box::new(lit4),
    };

    let bin_eqeq = Expr::Binary {
        left: Box::new(bin_lt),
        operator: eqeq,
        right: Box::new(litf),
    };

    bin_eqeq
}

#[test]
fn print_expr_tree() {
    let expr = get_reference_ast();
    println!("{expr}");

    let result = "(== (< (* 1 ('group' (- 2 3))) 4) false)";
    assert_eq!(result, format!("{expr}"))
}

#[test]
fn parse_to_a_correct_ast() {
    let lexer = Lexer::new(EXPRESSION);
    let result = lexer.scan();

    assert!(result.errors.is_empty());

    let parser = Parser::new(&result.tokens);
    let expr = parser.parse();

    assert!(matches!(expr, Ok(_)));

    match expr.unwrap().statements.first().unwrap() {
        Stmt::Expr { expr } => assert_eq!(*expr, get_reference_ast()),
        _ => assert!(false),
    };
}
