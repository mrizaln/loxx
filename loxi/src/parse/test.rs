use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::interp::interner::Interner;
use crate::lex::Lexer;
use crate::parse::ast::Ast;
use crate::parse::Parser;
use crate::util::Location;

use super::{expr::*, stmt::*, token};

static EXPRESSION: &str = indoc! { r#"
    1 * (2 - 3) < 4 == false;
"# };

fn get_reference_ast(ast: &mut Ast) -> ExprId {
    // given an expression below
    //      1 * (2 - 3) < 4 == false
    //      1   5   9   13  17  21
    // it should produce
    //      (== (< (* 1 ('group' (- 2 3))) 4) false)

    type Lit = token::Literal;
    let lit = Expr::literal;
    let loc = Location::new;

    let lit2 = ast.add_expr(lit(Lit::Number(2.0)), loc(1, 6));
    let lit3 = ast.add_expr(lit(Lit::Number(3.0)), loc(1, 10));
    let bin_sub = Expr::binary(lit2, token::BinaryOp::Sub, lit3);
    let bin_sub = ast.add_expr(bin_sub, loc(1, 8));
    let grp_sub = ast.add_expr(Expr::group_val(bin_sub), loc(1, 5));

    let lit1 = ast.add_expr(lit(Lit::Number(1.0)), loc(1, 1));
    let bin_mul = Expr::binary(lit1, token::BinaryOp::Mul, grp_sub);
    let bin_mul = ast.add_expr(bin_mul, loc(1, 3));

    let lit4 = ast.add_expr(lit(Lit::Number(4.0)), loc(1, 15));
    let litf = ast.add_expr(lit(Lit::False), loc(1, 20));

    let bin_lt = Expr::binary(bin_mul, token::BinaryOp::Less, lit4);
    let bin_lt = ast.add_expr(bin_lt, loc(1, 13));

    let bin_eq = Expr::binary(bin_lt, token::BinaryOp::Equal, litf);
    ast.add_expr(bin_eq, loc(1, 17))
}

#[test]
fn print_expr_tree() {
    let mut ast = Ast::new();
    let interner = Interner::new();
    let expr_id = get_reference_ast(&mut ast);

    let ExprL { expr, .. } = ast.get_expr(&expr_id);
    println!("{}", expr.display(&interner, &ast));

    let result = "(== (< (* 1 (group (- 2 3))) 4) false)";
    assert_eq!(result, format!("{}", expr.display(&interner, &ast)))
}

#[test]
fn parse_to_a_correct_ast() {
    let mut interner = Interner::new();
    let mut ast = Ast::new();
    let lexer = Lexer::new(EXPRESSION, &mut interner);
    let result = lexer.scan();

    assert!(result.errors.is_empty());

    let mut parser = Parser::new(&mut ast);
    let result = parser.parse(result.tokens);

    assert!(result.is_ok());
    let result = ast.get_stmt(result.as_ref().unwrap().statements.first().unwrap());

    let mut reference_ast = Ast::new();
    let expr_id = get_reference_ast(&mut reference_ast);
    let expect = reference_ast.get_expr(&expr_id);

    match &result.stmt {
        Stmt::Expr { expr } => {
            let result = ast.get_expr(expr).expr.display(&interner, &ast);
            let expect = expect.expr.display(&interner, &reference_ast);

            // Because of me using arena for the AST, the resulting AST itself (Stmt, Expr) can't
            // be compared directly. This test relies on correct implementation of `DisplayedExpr`
            // on representing the AST as a string.
            assert_eq!(expect.to_string(), result.to_string());
            println!("expect: {expect}");
            println!("result: {result}");
        }
        _ => unreachable!(),
    };
}
