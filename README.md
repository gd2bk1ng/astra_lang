# SyntraLang

**SyntraLang** is an independent, intent-oriented language foundation for AI,
symbolic reasoning, cognitive systems, and Syntra ecosystem tooling.

SyntraLang is the successor to the historical Astra language work. The original
Astra repository is retained as historical source material; this branch is the
active SyntraLang development line.

## Current milestone: lexical foundation

The lexer is being rebuilt as a strict, deterministic compiler component.

It provides:

- grammar-aligned punctuation and operator recognition
- Unicode identifiers
- integer, floating-point, string, and boolean literals
- line and block comments
- stable byte offsets with line/column positions
- exact raw lexeme preservation
- scalar numeric values with raw source spelling; string contents remain borrowed and undecoded
- deterministic lexical diagnostics
- malformed-input rejection
- no silent character loss
- focused unit tests for valid and invalid input
- a public library API for downstream AI, compiler, IDE, and kernel projects

## Library use

\`\`\`rust
use syntra_lang::{Lexer, TokenKind};

let tokens = Lexer::new("intent syntra { motive \"reason\" }")
    .tokenize()
    .expect("valid Syntra source");

assert!(matches!(
    tokens.last().map(|token| &token.kind),
    Some(TokenKind::Eof)
));
\`\`\`

## Design principles

SyntraLang is intended to be reusable independently of Syntra Kernel.

The lexical layer therefore prioritizes:

1. deterministic behavior
2. explicit syntax
3. source-aware tokens
4. strict malformed-input handling
5. stable diagnostics
6. Unicode support
7. independent testability
8. a small, predictable public API

The lexer does **not** decide whether an identifier is a keyword. Syntactic roles
such as \`intent\`, \`fn\`, \`rule\`, and \`modify\` remain parser concerns. This
keeps the token contract stable and avoids the historical lexer/parser mismatch
where one layer treated a word as a dedicated token and another expected an
identifier.

## Language contract

The semantic direction of SyntraLang is documented in
`docs/SYNTRALANG_LANGUAGE_CONTRACT.md`. It defines the long-term language
concepts and architectural boundaries without pretending that unimplemented
features already exist.

## Historical continuity

The historical Astra implementation is preserved for recovery and research.
Its grammar, parser, AST, type-system experiments, intent model, and tests are
being audited and reconciled rather than discarded.

The active development branch is:

\`syntra_lang\`

## Semantic Verification Milestone

The borrowed AST now has an initial semantic verification layer exposed as `syntra_lang::SemanticVerifier`.

Implemented foundations:
- borrowed `Program<'src>` traversal
- epistemic confidence bounds and deterministic-gate checks
- explicit semantic types for probabilistic values and capabilities
- affine capability ledger foundation
- effect/capability checks for known side-effect operations
- governance numeric contradiction detection
- source-aware semantic diagnostics through `DiagnosticEngine`
- semantic conformance fixtures for epistemic resolution, capability leakage, self-modification, governance contradiction, and confidence bounds

Important staging boundary:
- the current AST does not yet encode probabilistic variable declarations or function-local effect/capability clauses
- therefore this verifier implements only the semantic rules expressible by the current syntax
- unrestricted self-modification, authorization proof, and runtime capability enforcement remain outside this milestone

Semantic verification is intentionally separate from parsing: parser provenance remains authoritative for source structure, while the verifier determines whether that structure is semantically admissible.

## Roadmap

### Phase 1 — Lexical contract
- complete lexer
- token model
- diagnostics
- lexical fixtures
- lexical contract matrix
- numeric edge-case regression suite
- CI verification

### Phase 2 — Syntax
- borrowed-token architecture — implemented
- parser cursor — implemented
- borrowed AST layout — implemented
- initial recursive-descent parser — implemented
- precedence and nested-block parser fixtures — implemented
- parser recovery boundaries — implemented
- source-aware diagnostics engine — implemented
- grammar conformance fixtures — in progress
- cognitive primitive parser specification — staged
- borrowed cognitive AST substrate — implemented
- cognitive parser integration — next
- full cognitive AST parsing — gated on parser verification

### Phase 3 — Semantics
- type system
- effects
- symbolic constructs
- intent semantics

### Phase 4 — Integration
- stable IR
- SDK/tooling
- APC integrations
- Syntra Kernel integration

### Phase 5 — Ecosystem
- LSP/IDE support
- documentation
- examples
- compatibility/versioning
- third-party adoption

## License

MIT OR Apache-2.0


## Parser hardening

The parser now has two explicit operating modes:

- `parse_program()` — fail-fast parsing for compiler pipelines that want the first syntax error;
- `parse_program_recovering()` — best-effort parsing that records multiple syntax errors and synchronizes on semicolons or closing braces.

Diagnostics preserve the original borrowed source slice and report:

- line and column;
- byte start/end offsets;
- source-line context;
- a stable diagnostic code;
- a rendered caret span.

The recovery layer is deliberately conservative. It establishes deterministic
progress without pretending that malformed syntax can always be repaired.

## Cognitive syntax staging

The first cognitive syntax hooks are documented in
`docs/COGNITIVE_SYNTAX_STAGING.md`.

The staged concepts are:

- `intent`
- `goal`
- `effects`
- `uncertainty`
- `governance`

These remain parser/AST extension points until the ordinary syntax substrate
passes its verification gates. Cognitive semantics and governance verification
remain separate from parsing.
