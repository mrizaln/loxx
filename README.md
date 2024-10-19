# loxx

Lox interpreter written in Rust (Lox is a scripting language created by Robert Nystrom for [this book](https://craftinginterpreters.com/))

This repository contains two packages that corresponds to the following in the book

- `loxi ` &rarr; `jlox`: a tree-walk interpreter (**implementation complete**)
- `loxii` &rarr; `clox`: a bytecode interpreter (not implemented yet)

## Building

```sh
cargo build --bin loxi              # or loxii
cargo run --bin loxi -- --help      # opens help
```

## Testing

The tests are performed using a python script

```sh
./test.py <interpreter> -c <chapter>
```

Use the `-h` flag to see how to use it.

## Benchmarks

### Running

You can run the benchmarks yourself using the python script

```sh
./benchmark.py <interpreter> <repeat>
```

Use the `-h` flag to see how to use it.

> You can add other interpreters to the script by adding more enumeration to `Interpreter` class yourself.
> That is what I do to add and benchmark `jlox` below.

### Result

The benchmarks are performed on an **Intel Core i5-10500H machine with the frequency locked at 2.5GHz**. Each benchmark are repeated 5 times and then averaged to get better result.

#### Tree-walk interpreter

|                       |        loxi |        jlox |         diff |
| --------------------- | ----------: | ----------: | -----------: |
| `binary_trees.lox`    | 14.443941 s |  9.880800 s |   _+46.18 %_ |
| `equality.lox`        |  9.941284 s |  5.494600 s |   _+80.93 %_ |
| `fib.lox`             | 12.876985 s |  6.060200 s |  _+112.48 %_ |
| `instantiation.lox`   |  3.923048 s |  2.008800 s |   _+95.29 %_ |
| `invocation.lox`      |  4.071392 s |  1.759000 s |  _+131.46 %_ |
| `method_call.lox`     |  2.020660 s |  2.427200 s | **-16.75 %** |
| `properties.lox`      |  5.592753 s |  6.464400 s | **-13.48 %** |
| `string_equality.lox` |  3.659577 s |  6.170000 s | **-40.69 %** |
| `trees.lox`           | 27.242201 s | 32.009400 s | **-14.89 %** |
| `zoo.lox`             |  3.839772 s |  6.017600 s | **-36.20 %** |
| `zoo_batch.lox`       | 10.007977 s | 10.014200 s |  **-0.06 %** |

#### Bytecode interpreter

> **Not implemented yet**

## Extension

> enable using cargo `--features` flag

- `unicode`: allow non-whitespace unicode as identifier.
- `loxlox`: provides the following functions that facilitate [LoxLox](https://github.com/benhoyt/loxlox)
  - `getc()`: get char from `stdin` as number (currently not handling `utf8` properly)
  - `chr(ch)`: turn char code number into `string` (currently not handling `utf8` properly, if they are encountered `chr` will return `nil` instead)
  - `exit(status)`: exit with given status code
  - `print_error(msg)`: print message to `stderr` (I modify it so it can print any kind of Lox [`Value`](./loxi/src/interp/value.rs))

## TODO

These are from the challenges, I guess I'll add it as extension for the language and using cargo feature flag to enable it

- [ ] Add support for C-style block comments (`/* ... */`)
- [ ] Implement bitwise, shift, modulo, and conditional operators
- [ ] Add support for comma expressions (like in C)
- [ ] Add support for C-style conditional or "ternary" operator `?:`
- [ ] Add option to raise a runtime error on division by zero
- [ ] Make it a runtime error to access a variable that has not been initialized or been assigned
- [ ] Add support for `break` and `continue` statements in a loop
- [ ] Add anonymous function support
- [ ] Extend the resolver to raise an error if a local variable is never used
- [ ] Implement static methods (use `class` to mark a method as static)
- [ ] Support "getter" and "setter" for class members (see the book on challenges section on classes chapter for the syntax)
- [ ] Add support for string interpolation

Other challenges but lower priority

> Reasons for lower priority including but not limited to
>
> - Implementing these as features will make programming more error-prone

- [ ] Allow ordering with different type (e.g. `3 < "pancake"`)
- [ ] For `+` operator, if either of the operand is a string, convert the other operand to string.
