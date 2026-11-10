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
- decoded literal values
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
- borrowed-token architecture
- Syntra parser
- cognitive primitive parser specification
- unified AST
- grammar conformance tests

### Phase 2 — Syntax
- Syntra parser
- unified AST
- grammar conformance tests

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
