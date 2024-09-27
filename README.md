# loxx

Lox interpreter written in Rust (Lox is a scripting language created by Robert Nystrom for [this book](https://craftinginterpreters.com/))

This repository contains two packages that corresponds to the following in the book

- `loxi ` &rarr; `jlox`: a tree-walk interpreter (still under construction)
- `loxii` &rarr; `clox`: a bytecode interpreter (not implemented yet)

### TODO

> These are from the challenges, I guess I'll add it as extension for the language and using cargo feature flag to enable it

- Add support for C-style block comments (`/* ... */`)
- Implement bitwise, shift, modulo, and conditional operators
- Add support for comma expressions (like in C)
- Add support for C-style conditional or "ternary" operator `?:`

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
