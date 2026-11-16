# Developer Guide

## Build

```bash
git clone https://github.com/gd2bk1ng/syntra_lang.git
cd syntra_lang
cargo fmt --all -- --check
cargo check --all-targets
cargo test --all-targets
cargo clippy --all-targets --all-features -- -D warnings
```

Rust 1.85 or newer is required for the Rust 2024 edition; dependencies may require a newer version.

## Current code layers

- `lexer.rs`: lexical scanning and source spans.
- `tokens.rs`: token model.
- `ast.rs`: borrowed syntax tree.
- `parser.rs`: recursive-descent parsing and recovery.
- `diagnostics.rs`: source-aware diagnostics.
- `verify.rs`: initial semantic checks.
- `engine/identity.rs`: identity types.
- `engine/runtime.rs`: invocation registry and lifecycle coordinator.

## Contribution workflow

Create a focused branch from `main`, document the invariant or behavior being changed, add positive and adversarial tests, run the required Cargo checks, update documentation, and open a pull request.

## Important boundaries

There is no complete interpreter or capability-enforcing runtime. The semantic capability ledger is not a runtime authority ledger. Governance and approval syntax does not authenticate or authorize an operation.

Read the full [Contributing Guide](../CONTRIBUTING.md) and [Architecture](../docs/ARCHITECTURE.md) before changing security-sensitive code.
