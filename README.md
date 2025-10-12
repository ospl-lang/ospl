# OSPL

A programming language with a Rust-based interpreter.

## Building

```bash
cargo build
```

## Running Programs

```bash
./target/debug/ospl run <file.ospl>
./target/debug/ospl run test.ospl
```

Alternative commands:
- `exe` or `r` - run a file with statements
- `run-expr` - run a file as an expression
- `run-stmt` - run a file as a single statement

## Syntax

Comments:
- `#` for line comments
- `*****` multiline `*****` for block comments

Variables:
```ospl
def x = 10;
x = 20;  # reassignment
```

Functions:
```ospl
def add = fn((a, b), c) {
    return a + b + c;
};
```

Control flow:
```ospl
if condition {
    # code
} else if other {
    # code
} else {
    # code
};

select value {
    case (pattern) {
        # code
    }
    case (_) {
        # default case
    }
};

loop {
    # infinite loop
};
```

## VS Code Extension

Install the `.vsix` file for syntax highlighting.

or compile it with:
```
cd vscode/ospl-vscode && npx vsce package | cat
```
