#![cfg(test)]

use indoc::indoc;

use super::*;

static HELLO_WORLD: &str = indoc! { r#"
    var hello = "Hello world!";
    print hello;
"# };

#[test]
fn hello_test() {
    let mut lex_arena = Rodeo::default();
    let lexer = Lexer::new(HELLO_WORLD, &mut lex_arena);
    let result = lexer.scan();

    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.lines.len(), 2);
    assert_eq!(result.tokens.len(), 9);

    let loc = |l, c| Location { line: l, column: c };
    let lit = util::generate_string_literal_identifier;
    let mut intern = |str| lex_arena.get_or_intern(str);

    let tokens = vec![
        tok! { [loc(1,1)]  -> Keyword::Var },
        tok! { [loc(1,5)]  -> Literal::Identifier = intern("hello") },
        tok! { [loc(1,11)] -> Operator::Equal },
        tok! { [loc(1,13)] -> Literal::String = intern(&lit("Hello world!")) },
        tok! { [loc(1,27)] -> Punctuation::Semicolon },
        tok! { [loc(2,1)]  -> Keyword::Print },
        tok! { [loc(2,7)]  -> Literal::Identifier = intern("hello") },
        tok! { [loc(2,12)] -> Punctuation::Semicolon },
        Token::Eof(Location { line: 3, column: 1 }),
    ];

    for str in lex_arena.strings() {
        println!("{str}")
    }

    // TODO: check again whether the string has different key
    for (t1, t2) in result.tokens.iter().zip(tokens) {
        assert_eq!(*t1, t2);
    }
}
