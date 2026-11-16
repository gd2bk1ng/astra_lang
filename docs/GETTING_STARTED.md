# Getting Started with SyntraLang

SyntraLang is an early-stage Rust library for lexical analysis, parsing, borrowed syntax trees, and initial semantic verification. This guide gets the repository building locally; it does not install a standalone language toolchain or interpreter.

## 1. Prerequisites

- Git.
- Rust 1.85 or newer, with Cargo. The crate uses the Rust 2024 edition.
- A supported host platform for the Rust toolchain.

Install Rust using the official instructions at https://www.rust-lang.org/tools/install. The repository's `rust-toolchain.toml` selects the stable channel and requests rustfmt and Clippy.

## 2. Clone the repository

```bash
git clone https://github.com/gd2bk1ng/syntra_lang.git
cd syntra_lang
```

The canonical integration branch is `main`.

## 3. Verify the checkout

Run the same core checks used by the repository CI workflow:

```bash
cargo fmt --all -- --check
cargo check --all-targets
cargo test --all-targets
cargo clippy --all-targets --all-features -- -D warnings
```

A successful run on your machine is local verification evidence for that checkout and toolchain. CI results are recorded separately on GitHub.

## 4. Use the library

The current crate exposes lexer, parser, AST, diagnostics, semantic verification, and an invocation-coordination foundation.

```rust
use syntra_lang::{Lexer, Parser};

fn main() {
    let source = "fn answer() -> Int<64> { return 42; }";

    let tokens = Lexer::new(source)
        .tokenize()
        .expect("valid source should tokenize");

    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse_program()
        .expect("valid source should parse");

    println!("parsed {} declaration(s)", program.declarations.len());
}
```

The returned token vector and AST borrow source slices. Keep the source alive for as long as those values are used.

## 5. Run semantic verification

The initial verifier checks only the semantic rules represented by the current AST and verifier implementation:

```rust
use syntra_lang::{Lexer, Parser, SemanticVerifier};

fn main() {
    let source = "fn answer() -> Int<64> { return 42; }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let report = SemanticVerifier::new(source).verify_program(&program);
    for diagnostic in &report.diagnostics {
        println!("{}", diagnostic.render());
    }
    assert!(report.is_success());
}
```

A successful semantic report is not a proof of program safety and does not execute the program.

## 6. Explore the tests

Integration tests document the current public contracts. Useful starting points include:

- `tests/lexer_contract.rs`
- `tests/parser_contract.rs`
- `tests/parser_hardening.rs`
- `tests/cognitive_parser_fixtures.rs`
- `tests/semantic_fixtures.rs`
- `tests/invariant_hardening.rs`

Unit tests also live alongside implementation modules.

## 7. Where to go next

- Read the [Language Reference](LANGUAGE_REFERENCE.md) before writing examples.
- Read [Architecture](ARCHITECTURE.md) before changing module boundaries.
- Check [Implementation Status](IMPLEMENTATION_STATUS.md) before relying on a feature.
- Follow [Contributing](../CONTRIBUTING.md) for changes and tests.

## Current limitations

There is no complete compiler, interpreter, executable backend, command-line compiler, LSP server, package manager, or production runtime capability-enforcement system yet. SyntraLang is pre-alpha and its syntax/API may change.
