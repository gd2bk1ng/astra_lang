# SyntraLang Lexical Contract Matrix

Status: **implemented lexical foundation; contract remains version 0.1 and may evolve**

This document records the current lexer contract and the decisions made while reconciling earlier Astra experiments with SyntraLang.

## 1. Token architecture

The lexer recognizes lexical structure, not cognitive meaning. Words such as `intent`, `fn`, `rule`, `goal`, `effects`, `uncertainty`, and `governance` are emitted as identifiers. The parser assigns grammatical roles.

This keeps the token model small and avoids coupling the lexer to every evolving language concept.

## 2. Current lexical behavior

| Area | Current contract |
|---|---|
| Identifiers | Unicode XID start/continue; underscore is accepted at the beginning |
| Boolean literals | `true`, `false` |
| Integers | Decimal `i64` values; overflow is rejected |
| Floats | Decimal finite `f64` values; exponent notation is not specified |
| Numeric separators | Underscore separators are restricted; malformed placements are rejected |
| Strings | Double-quoted; raw source content is borrowed; supported escapes are validated but not decoded into an owned string |
| Comments | `//` line comments and `/* ... */` block comments |
| Spans | Byte offsets plus line/column coordinates |
| Unknown characters | Hard lexical errors; input is not silently skipped |
| EOF | Explicit EOF token |

Examples:
- Valid: `42`, `1_000`, `1.0`, `.5`, `"hello\nworld"`, `東京`.
- Invalid: `1.`, `1__2`, `1_.2`, `1._2`, unterminated strings/comments, unsupported escapes, and unsupported characters.

## 3. Source ownership

`Token<'src>` retains source lexemes as borrowed slices. Numeric tokens additionally carry parsed scalar values. `Lexer::tokenize()` returns a `Vec<Token<'src>>`, so the token contents are zero-copy but the token collection allocates.

This is **not** a zero-allocation lexer, zero-allocation parser, or `no_std` implementation.

## 4. Historical reconciliation

Earlier Astra prototypes used dedicated keyword-like token variants and broader or inconsistent numeric/string rules. SyntraLang keeps lexical words as identifiers and places keyword recognition in the parser. Resolved mismatches should have regression tests at the lexical boundary.

## 5. Regression policy

Every lexical contract change should include:
1. valid-input fixtures;
2. malformed-input fixtures;
3. exact source-span assertions where relevant;
4. parser conformance tests when syntax is affected;
5. semantic tests when meaning is affected.

The implementation and tests are authoritative. This matrix should be updated in the same change as any contract change.
