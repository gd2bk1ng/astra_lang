# SyntraLang Parser Architecture for Cognitive Primitives

Status: **initial recursive-descent parser implemented; full grammar and semantic coverage remain incomplete**

## 1. Responsibility

The parser converts the lexer's token stream into a borrowed AST. It is responsible for grammatical structure and source spans. It does not establish semantic safety, authorization, capability ownership, or execution behavior.

Pipeline:

```text
source -> lexer -> borrowed tokens -> parser -> borrowed AST -> semantic verifier
```

## 2. Lexical vocabulary

The lexer intentionally emits words such as `fn`, `intent`, `goal`, `effects`, `uncertainty`, and `governance` as identifiers. The parser recognizes their grammatical roles. Do not add dedicated lexer keywords without an explicit contract review.

## 3. Implemented top-level forms

- `fn` function declarations;
- `@grad fn` annotated gradient-function declarations;
- `rule` declarations;
- `intent` declarations.

Functions and rules support named parameters with type references, optional return type references, and blocks.

## 4. Cognitive intent forms

Intent members currently include generic identifier/string and identifier/block members, plus structured:
- `goal` with description, priority, condition, and target;
- `effects` with a comma-separated identifier list;
- `uncertainty` with confidence, assumption, evidence, and source;
- `governance` with requires, approval, policy, and constraint.

These structures are syntax nodes, not complete semantic models.

## 5. Statements and expressions

The implemented AST includes let bindings, return statements, backtrack blocks, annotations, expression statements, identifiers, literals, calls, unary and binary expressions, blocks, symbolic expressions, and self-modification expressions.

See [Language Reference](LANGUAGE_REFERENCE.md) and [implemented grammar](../syntra_grammar.ebnf) for the current subset. Exploratory constructs not represented in the AST are not implemented merely because they appeared in an earlier grammar proposal.

## 6. Error handling

- `parse_program()` is fail-fast and returns the first parse error.
- `parse_program_recovering()` returns a partial program and collected parse errors.
- Recovery synchronizes conservatively at declaration/block boundaries.
- `DiagnosticEngine` maps parser errors to source-aware diagnostics with stable codes and byte spans.

Recovery guarantees progress; it does not promise to reconstruct the programmer's intended syntax.

## 7. Acceptance criteria for future parser work

- grammar fixtures for valid and invalid forms;
- precise byte-span and line/column assertions;
- deterministic behavior;
- no-progress and malformed nesting tests;
- borrowed-lifetime tests;
- AST invariant validation;
- property-based and fuzz testing;
- documented compatibility decisions.

## 8. Future candidates

Imports/modules, match/pattern syntax, lambdas, richer type syntax, function effect clauses, and additional cognitive primitives are not part of the current implemented grammar. They require design, AST representation, tests, and semantic contracts before being advertised as supported.
