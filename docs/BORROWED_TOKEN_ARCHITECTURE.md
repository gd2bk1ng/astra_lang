# SyntraLang Borrowed Token and AST Architecture

Status: **borrowed lexical and AST foundations implemented; allocation-free pipeline not claimed**

## 1. Purpose

SyntraLang keeps source text as the owner of spelling and provenance. Tokens and AST source fields borrow from the original input where practical.

Current pipeline:

```text
&'src str -> Lexer<'src> -> Vec<Token<'src>>
                         -> Parser<'tokens, 'src> -> Program<'src>
```

## 2. Token contract

The public token model carries:
- a token kind;
- a source span with byte offsets and line/column coordinates;
- the exact borrowed source lexeme.

Identifiers and string literal content borrow from the source. Numeric tokens carry parsed scalar values while retaining the original lexeme.

## 3. String and numeric boundaries

String lexing validates supported escapes and preserves raw contents. It does not currently decode the string into an owned runtime value.

The current numeric subset is decimal integer and finite decimal float. Exponent notation and alternate bases are not part of the current contract. Numeric overflow and malformed separator placement are errors.

## 4. Parser and AST lifetimes

The parser uses two distinct lifetimes:
- `'tokens`: the borrow of the token slice;
- `'src`: the lifetime of source slices inside tokens.

AST names, raw string contents, and other source-bearing fields borrow from `'src`. AST collections use `Vec`, so parsing still allocates collection storage.

## 5. What zero-copy means here

The current guarantee is **zero-copy source lexemes and source-bearing AST fields where represented as borrowed slices**. It does not mean:
- no heap allocations;
- streaming parsing;
- an allocation-free AST;
- `no_std`;
- no owned semantic values in future phases.

## 6. Invariants

1. A borrowed token cannot outlive its source.
2. Original lexemes remain available for diagnostics and tooling.
3. Spans refer to byte positions in the original source buffer.
4. Lexer classification does not encode cognitive semantics.
5. Parser and AST lifetimes remain tied to source provenance.
6. Any future owned conversion is explicit.
7. Performance claims require benchmarks and allocation measurements.

## 7. Implemented parser boundary

The initial parser recognizes top-level functions, rules, intents, and `@grad` functions; typed parameters and generic type references; blocks; let/return/backtrack/annotation/expression statements; arithmetic, comparison, logical and call expressions; symbolic and self-modification expressions; and structured goal/effects/uncertainty/governance intent members.

The parser offers fail-fast parsing and a recovering mode. Recovery is conservative and does not imply that arbitrary malformed input can be repaired.

## 8. Future work

- benchmark tokenization and parsing;
- measure allocations rather than infer them;
- evaluate iterator/streaming interfaces;
- strengthen AST invariants and source mapping;
- evaluate a `no_std` boundary only after API and diagnostics contracts are stable;
- add property-based and fuzz testing.
