# SyntraLang

<p align="center">
  <strong>An intent-oriented language foundation for inspectable cognitive systems.</strong>
</p>

<p align="center">
  <a href="https://github.com/gd2bk1ng/syntra_lang/actions/workflows/ci.yml"><img alt="CI" src="https://github.com/gd2bk1ng/syntra_lang/actions/workflows/ci.yml/badge.svg?branch=main"></a>
  <img alt="Rust edition" src="https://img.shields.io/badge/Rust-2024-orange?logo=rust">
  <img alt="Version" src="https://img.shields.io/badge/version-0.1.0-informational">
  <img alt="Status" src="https://img.shields.io/badge/status-pre--alpha-yellow">
  <a href="https://github.com/gd2bk1ng/syntra_lang"><img alt="GitHub repository" src="https://img.shields.io/badge/GitHub-syntra__lang-181717?logo=github"></a>
</p>

SyntraLang is an independent language and compiler-foundation project exploring how programs can represent **intent, goals, effects, uncertainty, governance, and ordinary computation** in inspectable structures.

It is designed to be useful independently while leaving room for future integration with the Syntra ecosystem, including Syntra Kernel. The project is early-stage software: its lexer, an initial borrowed AST/parser, source-aware diagnostics, a first semantic-verification pass, and an invocation coordinator are present. It is **not yet a complete general-purpose language, production compiler, or secure execution runtime**.

> **Project status:** pre-alpha / active research and engineering. APIs and syntax may change before 1.0. Do not use this project as a security boundary or to execute untrusted programs.

## Why SyntraLang?

Most programming languages express computation primarily through values, types, and control flow. SyntraLang is exploring an additional layer: making the purpose and constraints of cognitive or agent-oriented work explicit and inspectable.

The long-term design separates:

- **Intent:** what a system is trying to accomplish.
- **Goals:** outcomes and conditions it is trying to satisfy.
- **Knowledge and uncertainty:** what is known, assumed, observed, or uncertain.
- **Effects and capabilities:** what an operation may change or access.
- **Governance:** policy, authorization, approval, and audit requirements.
- **Verification:** whether source structures satisfy defined rules.

These are design directions, not a claim that every concept already has complete runtime semantics.

## What exists today

| Area | Current state |
|---|---|
| Lexer | Implemented foundation: Unicode XID identifiers, literals, operators, comments, borrowed lexemes, spans, and malformed-input errors |
| Parser and AST | Initial recursive-descent parser for functions, rules, intents, goals, effects, uncertainty, governance, blocks, and a subset of expressions/statements |
| Parser recovery | Fail-fast and recovering entry points with conservative synchronization |
| Diagnostics | Source-aware parser/semantic diagnostics with byte spans and rendered context |
| Semantic verifier | Initial checks for supported types, callable declarations/calls, return flow, epistemic confidence, selected effects/capability rules, and governance constraints |
| Invocation engine | Coordinator-owned invocation lifecycle with registry-qualified invocation identities |
| Capability runtime | Not yet implemented; the semantic capability ledger is not a runtime authority system |
| Compiler/backend/VM | Not yet implemented |
| Cognitive IR, CLI, LSP, package manager | Planned; not implemented |

See [Implementation Status](docs/IMPLEMENTATION_STATUS.md) for precise boundaries and [Roadmap](docs/ROADMAP.md) for planned work.

## Quick start

### Requirements

- Rust toolchain with **Rust 1.85 or newer** (the minimum needed for the Rust 2024 edition; dependencies may require a newer toolchain).
- Cargo, included with the Rust toolchain.
- Git.

Clone and check the project:

```bash
git clone https://github.com/gd2bk1ng/syntra_lang.git
cd syntra_lang
cargo fmt --all -- --check
cargo check --all-targets
cargo test --all-targets
cargo clippy --all-targets --all-features -- -D warnings
```

### Parse a small SyntraLang program

The current public library API can tokenize and parse the implemented syntax subset:

```rust
use syntra_lang::{Declaration, Lexer, Parser};

fn main() {
    let source = r#"
        fn add(x: Int<64>, y: Int<64>) -> Int<64> {
            return x + y;
        }

        intent deployment {
            motive "ship a verified change"
            goal release {
                description "publish the release"
                priority 10
                condition ready
                target deploy()
            }
            effects { Network, IO }
            governance {
                requires operator
                approval reviewer
                policy "release-policy"
                constraint risk <= 0.2
            }
        }
    "#;

    let tokens = Lexer::new(source)
        .tokenize()
        .expect("source should tokenize");

    let mut parser = Parser::new(&tokens);
    let program = parser
        .parse_program()
        .expect("source should parse");

    assert!(matches!(
        program.declarations.first(),
        Some(Declaration::Function(_))
    ));
}
```

This example demonstrates parsing, not execution or authorization. A parsed `approval reviewer` field is data in the AST; it does not grant or prove approval.

For a step-by-step guide, see [Getting Started](docs/GETTING_STARTED.md). For exact supported forms, see the [Language Reference](docs/LANGUAGE_REFERENCE.md).

## Architecture

The current pipeline is:

```text
Source (&str)
   |
   v
Lexer -----> borrowed Tokens + source Spans
   |
   v
Parser ----> borrowed AST
   |
   v
SemanticVerifier ----> source-aware SemanticReport

RuntimeCoordinator ----> registry-qualified invocation lifecycle
                          (separate from parsing and verification)
```

The parser constructs syntax; the semantic verifier checks the subset of meaning currently represented by the AST. The invocation coordinator is a separate runtime foundation. There is no complete compiler, interpreter, or capability-enforcing execution backend yet.

Read [Architecture](docs/ARCHITECTURE.md) and [Language Contract](docs/SYNTRALANG_LANGUAGE_CONTRACT.md) for design details.

## Documentation

- [Documentation index](docs/README.md)
- [Getting started](docs/GETTING_STARTED.md)
- [Implemented language reference](docs/LANGUAGE_REFERENCE.md)
- [Architecture and module boundaries](docs/ARCHITECTURE.md)
- [Implementation status](docs/IMPLEMENTATION_STATUS.md)
- [Language contract and long-term concepts](docs/SYNTRALANG_LANGUAGE_CONTRACT.md)
- [Lexical contract matrix](docs/LEXICAL_CONTRACT_MATRIX.md)
- [Borrowed-token architecture](docs/BORROWED_TOKEN_ARCHITECTURE.md)
- [Cognitive syntax and AST](docs/COGNITIVE_SYNTAX_STAGING.md)
- [Parser design](docs/PARSER_COGNITIVE_PRIMITIVES_SPEC.md)
- [Grammar draft](syntra_grammar.ebnf)
- [Roadmap](docs/ROADMAP.md)
- [FAQ](docs/FAQ.md)
- [Social and project introduction kit](docs/SOCIAL_MEDIA_KIT.md)
- [Contributing](CONTRIBUTING.md)
- [Security policy](SECURITY.md)
- [Support](SUPPORT.md)
- [Changelog](CHANGELOG.md)

## Project principles

1. **Correctness before feature count.** Every language feature needs explicit invariants and tests.
2. **Syntax is not authority.** Parsing a capability, policy, or approval declaration must never itself grant permission.
3. **Verification is separate from execution.** Static checks do not replace runtime enforcement.
4. **Preserve provenance.** Source spans and original lexemes should remain traceable.
5. **Make ownership explicit.** Borrowed source data and runtime authority identities must not be conflated.
6. **Document evidence honestly.** Implemented, designed, planned, compiled, and tested are different statuses.
7. **Keep the language independent.** SyntraLang should remain useful outside Syntra Kernel.

## Development status and contribution

The canonical development branch is `main`. Changes should be made on focused branches and reviewed before merging. The project is not yet at a stable 1.0 compatibility promise.

Contributions are welcome, especially tests, grammar reviews, diagnostics, documentation, and carefully scoped implementation work. Please read [CONTRIBUTING.md](CONTRIBUTING.md) first.

## Community and project links

- Source code: https://github.com/gd2bk1ng/syntra_lang
- Issues and bug reports: https://github.com/gd2bk1ng/syntra_lang/issues
- Pull requests: https://github.com/gd2bk1ng/syntra_lang/pulls
- Releases: https://github.com/gd2bk1ng/syntra_lang/releases

See [Social Media Kit](docs/SOCIAL_MEDIA_KIT.md) for short descriptions, announcement copy, and project tags.

## Historical note

SyntraLang grew out of earlier Astra language experiments. Historical material is retained separately for research and recovery; the active language identity, specification, and development direction are SyntraLang.

## License

The package metadata declares `MIT OR Apache-2.0`. The repository should include the corresponding license texts before a formal release; see the release-readiness checklist in [Implementation Status](docs/IMPLEMENTATION_STATUS.md).
