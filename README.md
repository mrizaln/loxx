# loxx

Lox interpreter written in Rust (Lox is a scripting language created by Robert Nystrom for [this book](https://craftinginterpreters.com/))

This repository contains two packages that corresponds to the following in the book

- `loxi ` &rarr; `jlox`: a tree-walk interpreter (**implementation complete**)
- `loxii` &rarr; `clox`: a bytecode interpreter (not implemented yet)

### TODO

These are from the challenges, I guess I'll add it as extension for the language and using cargo feature flag to enable it

- Add support for C-style block comments (`/* ... */`)
- Implement bitwise, shift, modulo, and conditional operators
- Add support for comma expressions (like in C)
- Add support for C-style conditional or "ternary" operator `?:`
- Add option to raise a runtime error on division by zero
- Make it a runtime error to access a variable that has not been initialized or been assigned
- Add support for `break` and `continue` statements in a loop
- Add anonymous function support
- Extend the resolver to raise an error if a local variable is never used
- Implement static methods (use `class` to mark a method as static)
- Support "getter" and "setter" for class members (see the book on challenges section on classes chapter for the syntax)
- Add support for string interpolation

Other challenges but lower priority

> Reasons for lower priority including but not limited to
>
> - Implementing these as features will make programming more error-prone

- Allow ordering with different type (e.g. `3 < "pancake"`)
- For `+` operator, if either of the operand is a string, convert the other operand to string.

## Building

```sh
cargo build --bin loxi              # or loxii
cargo run --bin loxi -- --help      # opens help
```

## Testing

The tests is performed using a python script

```sh
./test.py <interpreter> -c <chapter>
```

Use the `-h` flag to see how to use it.

## Extension

> enable using cargo `--features` flag

- `unicode`: allow non-whitespace unicode as identifier.
