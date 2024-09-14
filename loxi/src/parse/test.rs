#![cfg(test)]

use super::{expr::*, token};

#[test]
fn print_expr_tree() {
    // given an expression below
    //      (1 * (2 - 3) < 4) == false
    // it should produce
    //      (== (< (* 1 ('group' (- 2 3))) 4) false)

    let lit1 = Expr::Literal {
        value: token::Literal::Number(1.0),
    };
    let lit2 = Expr::Literal {
        value: token::Literal::Number(2.0),
    };
    let lit3 = Expr::Literal {
        value: token::Literal::Number(3.0),
    };
    let lit4 = Expr::Literal {
        value: token::Literal::Number(4.0),
    };
    let litf = Expr::Literal {
        value: token::Literal::False,
    };

    let eqeq = token::BinaryOp::Equal;
    let min = token::BinaryOp::Minus;
    let star = token::BinaryOp::Mul;
    let lt = token::BinaryOp::Less;

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
    println!("{bin_eqeq}");

    let result = "(== (< (* 1 ('group' (- 2 3))) 4) false)";
    assert_eq!(result, format!("{bin_eqeq}"))
}
