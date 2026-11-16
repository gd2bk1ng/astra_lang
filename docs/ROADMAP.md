# SyntraLang Roadmap

This roadmap is directional, not a delivery-date promise. Work is accepted when its invariants, implementation, tests, and documentation are reviewed.

## Guiding order

Correctness and clear contracts come before syntax expansion, performance work, or ecosystem marketing.

## Stage 0 — Repository and engineering foundation

- [x] Establish SyntraLang identity and canonical `main` branch.
- [x] Preserve Astra-origin history separately.
- [x] Establish Rust crate metadata and a CI workflow.
- [ ] Protect `main` and require CI/review before merging.
- [ ] Add explicit license text files and a release policy.
- [ ] Define MSRV, dependency update, and lockfile policy.

## Stage 1 — Lexical and source model

- [x] Unicode-aware identifier scanning.
- [x] Strict malformed-input handling.
- [x] Borrowed token lexemes and source spans.
- [x] Numeric/string lexical contract and regression tests.
- [ ] Expand property-based/fuzz coverage.
- [ ] Benchmark lexer throughput and allocations.

## Stage 2 — Parser and AST

- [x] Borrowed token cursor and AST substrate.
- [x] Initial recursive-descent parser for ordinary declarations and expressions.
- [x] Structured intent, goal, effects, uncertainty, and governance AST nodes.
- [x] Fail-fast and recovering parse modes.
- [x] Source-aware diagnostics.
- [ ] Freeze a versioned implemented grammar.
- [ ] Expand AST invariant validation and malformed-input tests.
- [ ] Add fuzz/property tests and parser corpus.

## Stage 3 — Semantic model

- [x] Initial semantic verifier and diagnostic codes.
- [x] Initial callable registry, basic type inference/checks, return-flow checks.
- [x] Initial epistemic confidence and selected effect/governance checks.
- [ ] Audit and repair semantic soundness gaps.
- [ ] Define scopes, binding identity, effect contracts, and capability typing precisely.
- [ ] Separate syntax declarations from authenticated runtime policy.
- [ ] Build conformance fixtures for every semantic rule.

## Stage 4 — Runtime authority

- [x] Coordinator-owned invocation lifecycle.
- [x] Registry-qualified invocation identities.
- [ ] Define trusted bootstrap and owner/invocation association.
- [ ] Implement a versioned capability ledger with unforgeable authority boundaries.
- [ ] Implement atomic transfer/consume preflight and commit.
- [ ] Validate nested return/abandonment chronology and descendant lifecycle.
- [ ] Add adversarial state-machine/property tests.
- [ ] Keep semantic verification decoupled from runtime enforcement.

## Stage 5 — Intermediate representation and execution

- [ ] Specify Cognitive IR and semantic preservation requirements.
- [ ] Define a minimal interpreter or backend contract.
- [ ] Establish host capability adapter boundary.
- [ ] Add deterministic execution tests and resource limits.
- [ ] Do not enable untrusted host effects until runtime authority is independently reviewed.

## Stage 6 — Developer ecosystem

- [ ] Stable public API and versioning policy.
- [ ] CLI and source formatter.
- [ ] Language Server Protocol support.
- [ ] Editor integrations and syntax highlighting.
- [ ] Standard library and package/distribution story.
- [ ] Examples, tutorials, and migration guides.

## Stage 7 — Syntra ecosystem integration

- [ ] Define integration contract with Syntra Kernel.
- [ ] Keep SyntraLang usable as an independent crate.
- [ ] Add integration tests across process and platform boundaries.
- [ ] Document compatibility between language, IR, runtime, and kernel versions.

## Completion criteria

A stage is not complete because its checklist is mostly filled. It is complete only when the acceptance criteria, negative/adversarial tests, documentation, and verification evidence are present.
