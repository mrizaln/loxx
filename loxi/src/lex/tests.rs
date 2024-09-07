#![cfg(test)]

use indoc::indoc;

use super::*;

static HELLO_WORLD: &str = indoc! { r#"
    var hello = "Hello world!";
    print hello;
"# };

macro_rules! tok {
    {[$line:expr,$col:expr] -> $type:ident} => {
        Token {
            value: TokenValue::$type,
            loc: Location { line: $line, column: $col },
        }
    };
    {[$line:expr,$col:expr] -> $type:ident::$value:ident} => {
        Token {
            value: TokenValue::$type(tokens::$type::$value),
            loc: Location { line: $line, column: $col },
        }
    };
    {[$line:expr,$col:expr] -> $type:ident::$name:ident = $value:expr} => {
        Token {
            value: TokenValue::$type(tokens::$type::$name($value)),
            loc: Location { line: $line, column: $col },
        }
    };
}

#[test]
fn hello_test() {
    let lexer = Lexer::new(HELLO_WORLD);
    let result = lexer.scan();

    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.lines.len(), 2);
    assert_eq!(result.tokens.len(), 9);

    let tokens = vec![
        tok! { [1,1]  -> Keyword::Var },
        tok! { [1,5]  -> Literal::Identifier = "hello".into() },
        tok! { [1,11] -> Operator::Equal },
        tok! { [1,13] -> Literal::String = "Hello world!".into() },
        tok! { [1,27] -> Punctuation::Semicolon },
        tok! { [2,1]  -> Keyword::Print },
        tok! { [2,7]  -> Literal::Identifier = "hello".into() },
        tok! { [2,12] -> Punctuation::Semicolon },
        tok! { [3,1]  -> Eof},
    ];

    for (t1, t2) in result.tokens.iter().zip(tokens) {
        assert_eq!(*t1, t2);
    }
}
