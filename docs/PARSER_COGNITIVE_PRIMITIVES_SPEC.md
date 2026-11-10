# SyntraLang Parser Architecture for Cognitive Primitives

Status: **Phase 2 design specification — draft**

## 1. Purpose

This document defines how the future SyntraLang parser should recognize
cognitive vocabulary without turning the lexer into a semantic keyword registry
or weakening source ownership guarantees.

The parser consumes the stable lexical token space established by the Phase 1
contract.

## 2. Parser boundary

The intended pipeline is:

`source buffer -> lexer -> lexical tokens -> parser -> syntax AST -> semantic verifier`

The parser is responsible for grammatical structure.

The semantic verifier is responsible for meaning, type/effect constraints,
capabilities, uncertainty rules, governance, and authorization.

The parser must not attempt to prove semantic safety.

## 3. Cognitive vocabulary

Initial parser-level vocabulary includes:

- `intent`
- `goal`
- `knowledge`
- `context`
- `relationship`
- `capability`
- `effect`
- `effects`
- `uncertainty`
- `memory`
- `observation`
- `action`
- `reflection`
- `verification`
- `governance`
- `learning`
- `approval`

These remain lexical identifiers.

A parser helper such as `at_word("intent")` may recognize the role without
requiring a dedicated `TokenKind::Intent`.

This preserves the keyword-light lexical contract and prevents the historical
lexer/parser mismatch from returning.

## 4. Intent grammar

The initial parser target is an intent declaration of the form:

`intent Identifier { IntentMember* }`

An intent member may eventually include:

- goal declarations;
- observations;
- knowledge/context requirements;
- relationships;
- actions;
- effects;
- uncertainty declarations;
- verification requirements;
- governance constraints;
- human approval requirements.

The exact member grammar must be finalized before implementation.

## 5. Goal grammar

Goals should be parsed as structured declarations rather than arbitrary
strings when their semantic model is finalized.

Conceptually:

`goal Identifier [priority Expression] [target Expression]`

The parser should preserve the source structure needed for later planning and
verification without deciding whether a goal is achievable.

## 6. Effects

Effects should remain syntactically explicit.

Function or action declarations may eventually contain an effect clause such as:

`effects { Pure, IO, Network }`

The parser records the declared effect set. The semantic verifier determines
whether the implementation actually satisfies the declaration.

## 7. Governance

Governance clauses should be represented structurally so that downstream
verification can inspect:

- required capabilities;
- prohibited effects;
- approval requirements;
- resource boundaries;
- audit/provenance requirements.

The parser does not grant authority. It only constructs the declaration.

## 8. Uncertainty

Uncertainty syntax should preserve both the underlying value/type and its
confidence or probability expression.

A future syntax may resemble:

`uncertain Observation [confidence >= threshold]`

but this exact syntax is intentionally not yet committed.

The parser architecture must therefore allow uncertainty metadata to be
attached to expressions/declarations without baking one probability model into
the lexer.

## 9. Zero-copy design target

The lexical layer now provides the required borrowed-token boundary. Tokens are `Token<'a>` values whose source-bearing fields borrow directly from the lexer input.

The long-term parser target is source-borrowing AST data:

`AST<'a>`

where identifiers, strings, and source slices borrow from the original input
buffer.

The parser should avoid cloning source text solely to construct syntax nodes.

The current token vector is still heap-backed because `tokenize()` returns `Vec<Token<'a>>`, but token contents themselves are now zero-copy. Therefore a zero-allocation lexer/parser pipeline cannot honestly be claimed yet.

The migration should happen deliberately:

1. define borrowed token representation; **done**
2. preserve source spans; **done**
3. establish lifetime invariants; **done**
4. benchmark allocation behavior;
5. migrate parser/AST;
6. only then consider a `no_std` core boundary.

## 10. no_std boundary

The future core should separate language logic from operating-system facilities.

Candidate boundary:

`syntra-core-lex`
`syntra-ast`
`syntra-verify`

These should avoid depending on OS services where practical.

The current crate still uses `std`; the no_std migration is therefore a
future architectural task, not a property of the current implementation.

## 11. Capability and linearity

Capability syntax can be parsed without enforcing capability consumption.

Affine/linear guarantees belong in the semantic/type system, where ownership,
flow, duplication, and consumption can be analyzed.

The parser must preserve capability-bearing syntax and source provenance so the
verifier can perform those checks later.

## 12. Error recovery

Parser diagnostics should retain:

- token span;
- source lexeme;
- expected syntactic construct;
- actual construct;
- recovery location;
- enough context to explain the cognitive construct being parsed.

Recovery must never silently discard unknown semantic content.

## 13. AST direction

The future AST should support both ordinary programming constructs and cognitive
constructs, for example:

`Function`
`Intent`
`Goal`
`Knowledge`
`Observation`
`Action`
`Rule`
`EffectClause`
`GovernanceClause`
`UncertaintyAnnotation`
`VerificationRequirement`

These are architectural targets. They should only become stable public API
after representative syntax and semantic tests exist.

## 14. Parser acceptance criteria

Before calling the parser production-ready, we should have:

- grammar conformance fixtures;
- valid and invalid cognitive primitive fixtures;
- source-span diagnostics;
- deterministic parsing;
- error recovery tests;
- ownership/lifetime tests;
- allocation measurements;
- compatibility/versioning policy;
- documented AST invariants.

## 15. Implementation order

1. Define borrowed token representation.
2. Freeze lexical contract.
3. Implement parser cursor/diagnostic infrastructure.
4. Implement ordinary declarations and expressions.
5. Implement intent and goal syntax.
6. Implement effects and governance syntax.
7. Implement uncertainty syntax after its semantic contract is defined.
8. Add AST invariants and conformance fixtures.
9. Prepare semantic verifier interfaces.
