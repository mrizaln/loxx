use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::interp::interner::Interner;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::util::{Location, TokLoc};

use super::{expr::*, stmt::*, token};

static EXPRESSION: &str = indoc! { r#"
    1 * (2 - 3) < 4 == false;
"# };

fn get_reference_ast() -> Expr {
    // given an expression below
    //      1 * (2 - 3) < 4 == false
    //      1   5   9   13  17  21
    // it should produce
    //      (== (< (* 1 ('group' (- 2 3))) 4) false)

    let loc = Location::new;

    let lit1 = Expr::literal(TokLoc::new(token::Literal::Number(1.0), loc(1, 1)));
    let lit2 = Expr::literal(TokLoc::new(token::Literal::Number(2.0), loc(1, 6)));
    let lit3 = Expr::literal(TokLoc::new(token::Literal::Number(3.0), loc(1, 10)));
    let lit4 = Expr::literal(TokLoc::new(token::Literal::Number(4.0), loc(1, 15)));
    let litf = Expr::literal(TokLoc::new(token::Literal::False, loc(1, 20)));

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
        left: lit2.boxed(),
        operator: min,
        right: lit3.boxed(),
    };

    let grp1 = Expr::group_val(Box::new(bin_min), loc(1, 5));
    let bin_1_grp1 = Expr::binary(lit1.boxed(), star, grp1.boxed());
    let bin_lt = Expr::binary(bin_1_grp1.boxed(), lt, lit4.boxed());

    Expr::binary(bin_lt.boxed(), eqeq, litf.boxed())
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
    let result = parser.parse(result.tokens);

    assert!(result.is_ok());

    let reference_ast = get_reference_ast();

    println!("expect: {}", reference_ast.display(&interner));
    println!("result: {}", result.as_ref().unwrap().display(&interner));

    match result.unwrap().statements.first().unwrap() {
        Stmt::Expr { expr } => assert_eq!(expr.as_ref(), &reference_ast),
        _ => unreachable!(),
    };
}
