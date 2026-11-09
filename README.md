# SyntraLang

**SyntraLang** is the language foundation for intent-oriented, symbolic, cognitive,
and AI systems in the Syntra ecosystem.

This repository is the continuation of the original Astra language work. Historical
Astra source is preserved as the foundation while the active language identity and
public API move forward as **SyntraLang**.

## Current milestone: lexical foundation

The lexer is being rebuilt as a strict, deterministic compiler component.

It provides:

- complete punctuation and operator recognition required by the current grammar
- identifiers and Unicode identifiers
- integer, floating-point, string, and boolean literals
- line and block comments
- stable byte spans with line/column positions
- deterministic lexical diagnostics
- malformed-input rejection
- no silent character loss
- unit tests for the lexical contract
- a public library API for downstream AI and compiler projects

## Library use

```rust
use syntra_lang::{Lexer, TokenKind};

let tokens = Lexer::new("intent syntra { motive \"reason\" }")
    .tokenize()
    .expect("valid Syntra source");

assert!(matches!(tokens.last().map(|t| &t.kind), Some(TokenKind::Eof)));
```

## Design principle

SyntraLang is intended to become a reusable language infrastructure layer.

The lexer must therefore remain:

1. deterministic
2. explicit
3. lossless with respect to recognized source semantics
4. strict about malformed input
5. independently testable
6. stable enough for compiler, IDE, AI-agent, and Syntra Kernel consumers

Advanced AI semantics, symbolic reasoning, cognition, self-modification, and runtime
behavior belong to later compiler/runtime layers. The lexer establishes the trusted
front door for those systems.

## Historical continuity

The original Astra implementation is not discarded. Existing grammar, parser, AST,
type-system, and research concepts are being recovered and reconciled rather than
rewritten blindly.

The active development branch for this recovery is:

`syntra_lang`

## License

MIT OR Apache-2.0
