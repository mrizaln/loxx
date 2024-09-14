#![cfg(test)]

use indoc::indoc;

use super::*;

static HELLO_WORLD: &str = indoc! { r#"
    var hello = "Hello world!";
    print hello;
"# };

#[test]
fn hello_test() {
    let lexer = Lexer::new(HELLO_WORLD);
    let result = lexer.scan();

    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.lines.len(), 2);
    assert_eq!(result.tokens.len(), 9);

    let loc = |l, c| Location { line: l, column: c };

    let tokens = vec![
        tok! { [loc(1,1)]  -> Keyword::Var },
        tok! { [loc(1,5)]  -> Literal::Identifier = "hello".into() },
        tok! { [loc(1,11)] -> Operator::Equal },
        tok! { [loc(1,13)] -> Literal::String = "Hello world!".into() },
        tok! { [loc(1,27)] -> Punctuation::Semicolon },
        tok! { [loc(2,1)]  -> Keyword::Print },
        tok! { [loc(2,7)]  -> Literal::Identifier = "hello".into() },
        tok! { [loc(2,12)] -> Punctuation::Semicolon },
        Token::Eof(Location { line: 3, column: 1 }),
    ];

    for (t1, t2) in result.tokens.iter().zip(tokens) {
        assert_eq!(*t1, t2);
    }
}
