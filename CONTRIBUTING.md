# Contributing to SyntraLang

Thank you for helping improve SyntraLang. This is a pre-alpha language project; careful review of contracts, edge cases, and documentation is especially valuable.

## Before starting

Read the [README](README.md), [Architecture](docs/ARCHITECTURE.md), and [Implementation Status](docs/IMPLEMENTATION_STATUS.md). Search existing issues and pull requests. For substantial language or architecture changes, discuss the design before implementation.

## Development workflow

- Base work on `main`.
- Use a focused branch for each coherent change.
- Keep unrelated refactors and formatting changes out of feature pull requests.
- Do not rewrite shared history without coordination.
- Include tests and update documentation when behavior or public APIs change.

## Required checks

```bash
cargo fmt --all -- --check
cargo check --all-targets
cargo test --all-targets
cargo clippy --all-targets --all-features -- -D warnings
```

Report the exact commands, Rust version, operating system, and relevant output. Never claim a check passed unless it was actually run.

## Testing expectations

Add positive, negative, boundary, and adversarial tests as appropriate. Parser changes need malformed-input, recovery-progress, and source-span coverage. Runtime authority changes need state-transition, foreign-identity, failed-preflight atomicity, and lifecycle tests.

## Security and architecture rules

- Keep lexer, parser, semantic verifier, and runtime responsibilities separate.
- Source-level governance or approval labels must never grant authority.
- The verifier's name-based semantic capability ledger is not a runtime authority ledger.
- Keep identity minting behind an explicit trusted boundary.
- Document invariants before changing security-sensitive state transitions.
- Preserve source provenance and borrowed lifetime guarantees.

## Documentation

Update the language reference, architecture, implementation status, and roadmap when a change affects syntax, public API, or project status. Examples must be accepted by the current parser; planned features must be labelled as planned.

## Pull request checklist

- [ ] One clear purpose and a focused diff.
- [ ] Invariants and trust boundaries documented.
- [ ] Positive, negative, and relevant adversarial tests added.
- [ ] Formatting, check, test, and Clippy run, or limitations stated.
- [ ] Documentation and public API references updated.
- [ ] No secrets, unrelated files, or generated build artifacts included.
- [ ] All claims match actual verification evidence.

## Bug reports

Use https://github.com/gd2bk1ng/syntra_lang/issues. Include a minimal reproduction, expected and actual behavior, toolchain/platform details, and relevant command output. Remove credentials, private data, and secrets before posting.
