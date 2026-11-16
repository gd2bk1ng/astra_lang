# SyntraLang Implementation Status

Last reconciled with repository source on 2026-11-16. This is a source review, not a claim that the current branch has passed compilation or tests in every environment.

## Status legend

- **Implemented foundation:** code and tests exist for a bounded contract.
- **Partial / experimental:** some behavior exists, but the feature is incomplete or not production-ready.
- **Planned:** design direction only; no complete implementation.
- **Not verified here:** no fresh build/test execution is claimed by this documentation change.

## Component matrix

| Component | Status | Boundary |
|---|---|---|
| Unicode-aware lexer | Implemented foundation | Tokenizes identifiers, literals, comments, punctuation/operators, and reports malformed input |
| Borrowed tokens and spans | Implemented foundation | Source lexemes are borrowed; token vector allocates |
| Recursive-descent parser | Partial | Functions, rules, intents, goals, effects, uncertainty, governance, and a subset of statements/expressions |
| Borrowed AST | Implemented foundation | Source-bearing AST values borrow from source; collections allocate |
| Parser recovery | Partial | Conservative synchronization; does not guarantee intended repair |
| Source-aware diagnostics | Implemented foundation | Parse/semantic diagnostics include source spans and rendering |
| AST invariants | Partial | Validation helpers and adversarial tests exist; continued hardening required |
| Semantic verifier | Partial / experimental | Initial callable, type, return-flow, epistemic, effect, capability-leakage, and governance checks |
| Semantic capability ledger | Experimental foundation | Name-based bookkeeping in the verifier; not runtime authority |
| Invocation coordinator | Partial runtime foundation | Registry-qualified IDs and invocation lifecycle; not a language execution engine |
| Capability owner identity type | Identity scaffold | Type exists; authoritative ownership/bootstrap and ledger integration are not complete |
| Capability transfer/consume runtime | Planned | No complete production runtime ledger or transfer protocol on current branch |
| Cognitive IR | Planned | Not implemented |
| Interpreter/compiler backend | Planned | Not implemented |
| CLI, LSP, IDE integration | Planned | Not implemented |
| Standard library/package manager | Planned | Not implemented |
| Syntra Kernel integration | Planned | Future integration target |

## Important distinctions

- **Parsed** does not mean semantically valid.
- **Semantically accepted** does not mean authorized to access a host resource.
- **Capability syntax** does not create a runtime capability.
- **An approval label** is not an authenticated approval.
- **Zero-copy source fields** does not mean zero allocation.
- **Committed code** does not mean compiled or tested.

## Release-readiness gaps

Before a stable release, the project still needs:
- a reviewed and tested language grammar;
- stronger parser/AST invariants and fuzz/property testing;
- a reconciled and sound semantic contract;
- a complete runtime capability ownership/transfer/consumption model before exposing host effects;
- build and CI evidence on supported toolchains/platforms;
- a deliberate MSRV and dependency/lockfile policy;
- license text files matching package metadata;
- versioned API/grammar compatibility policy;
- release notes and reproducible release procedure.

## Verification record for this documentation branch

The documentation update itself has not been compiled or tested by the GitHub file-editing workflow. The corrected CI workflow will run on pushes to `main` and pull requests targeting `main`; the feature branch should be validated through its pull request or a manual workflow run. The maintainer's local Rust rig remains an additional verification environment.


## CI observation from documentation PR

GitHub Actions run 198 for PR #5 reached the formatting step and **failed at `cargo fmt --all -- --check`**. The output reports formatting differences across pre-existing Rust source and test files, including `src/ast.rs`, `src/diagnostics.rs`, `src/engine/runtime.rs`, `src/lexer.rs`, `src/parser.rs`, `src/verify.rs`, and multiple integration tests.

Because the format step failed, `cargo check`, `cargo test`, and Clippy were skipped in that run. This is a formatting-gate failure, not evidence of a compilation or test failure; compilation and tests remain unverified by this run. A separate formatting-only cleanup followed by a fresh full CI run is required to establish a green baseline.
