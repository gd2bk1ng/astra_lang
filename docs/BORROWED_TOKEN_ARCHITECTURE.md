# SyntraLang Borrowed Token Architecture

Status: **Phase 1 architecture — implemented**

## 1. Purpose

SyntraLang now establishes the ownership boundary required for a zero-copy
parser.

The lexical pipeline is:

`&'a str -> Lexer<'a> -> Token<'a> -> Parser<'a> -> AST<'a>`

The source buffer is the owner of source text. Tokens borrow from that buffer
and do not duplicate identifiers, string contents, or source lexemes.

## 2. Lifetime contract

Every token carries the lifetime `'a` of the source passed to `Lexer::new`.

A `Token<'a>`:

- cannot outlive its source buffer;
- never owns its source lexeme;
- retains byte offsets through `Span`;
- retains exact source spelling through `&'a str`;
- may be copied cheaply because it contains only scalar values and references.

This makes the parser lifetime boundary explicit before AST construction begins.

## 3. Token representation

The current representation is:

`Token<'a> { kind: TokenKind<'a>, span: Span, lexeme: &'a str }`

Borrowed semantic-bearing token variants include:

- `Identifier(&'a str)`
- `StringLiteral { raw: &'a str }`

Numeric variants store parsed scalar values:

- `IntLiteral(i64)`
- `FloatLiteral(f64)`

Their exact source spelling remains available through `lexeme`.

## 4. String ownership rule

String literals no longer allocate a decoded `String` during lexing.

The lexer stores the raw source contents between the quotes. Escape decoding is
deliberately deferred to a later value/semantic layer.

This prevents the lexer from becoming a hidden allocation boundary and keeps
source provenance intact.

## 5. Numeric ownership rule

Decimal numeric parsing no longer constructs temporary strings merely to remove
separator characters.

The lexer validates the source slice directly and produces the scalar value.
The original spelling remains borrowed through `Token::lexeme`.

The current decimal contract remains intentionally narrow:

- `1.0` — valid
- `.5` — valid
- `1.` — invalid
- `1_.2` — invalid
- `1._2` — invalid
- `1__2` — invalid
- `1.2.3` — invalid

Future numeric extensions must be specified in the lexical contract before
implementation.

## 6. What remains allocated

This migration establishes **zero-copy token contents**, not yet a
zero-allocation lexer pipeline.

`Lexer::tokenize()` still returns `Vec<Token<'a>>`, so the token collection
allocates.

The lexer also currently lives in the `std` environment.

The next optimization stages are therefore separate:

1. borrowed token contents — **implemented**;
2. parser cursor over borrowed tokens — next;
3. allocation measurement and hot-path profiling;
4. optional streaming/token iterator API;
5. `no_std` core boundary;
6. allocation-free parsing where the final AST representation permits it.

We must not collapse these into one unverified "zero allocation" claim.

## 7. Parser contract

The future parser should accept borrowed tokens without cloning source text.

Target shape:

`Parser<'a> { source: &'a str, tokens: &'a [Token<'a>] }`

or, for streaming designs, a cursor over a borrowed token producer.

The parser may construct owned semantic values only when the language contract
requires ownership. Syntax nodes representing source identifiers, literals,
annotations, and provenance should borrow whenever practical.

## 8. AST contract

The intended AST boundary is:

`AST<'a>`

The AST lifetime must originate from the same source buffer as its syntax
tokens.

This means a parser cannot accidentally create an AST that outlives its source
without explicitly introducing an ownership/conversion boundary.

## 9. no_std boundary

The current implementation is still `std`.

The borrowed token API is deliberately designed so that the lexical data model
does not require operating-system services. A future `syntra-core-lex` crate
can move this contract into `no_std` after the API and diagnostics boundary
are proven.

The migration must not introduce a second incompatible token model.

## 10. Compatibility policy

This is a deliberate API transition from:

`Token { lexeme: String }`

to:

`Token<'a> { lexeme: &'a str }`

The old ownership model is not preserved as the canonical core representation.
If an owned representation is needed for long-lived tooling, persistence, or
FFI, it should be an explicit conversion at the boundary rather than the
default lexer representation.

## 11. Invariants

The following invariants are now architectural requirements:

1. No token owns a copy of source text.
2. Every borrowed token is tied to one source lifetime.
3. Exact source spelling remains recoverable.
4. Spans refer to the original source buffer.
5. Lexical classification does not encode cognitive semantics.
6. Escape decoding does not force lexer allocation.
7. Parser and AST lifetimes must remain compatible with the lexical lifetime.
8. Any owned conversion must be explicit.

## 12. Verification requirements

Before the parser is considered production-ready, add tests for:

- token lifetime compilation constraints;
- identifiers borrowing the original source;
- string raw slices borrowing the original source;
- numeric source spelling and scalar value agreeing;
- parser AST lifetime propagation;
- allocation counts;
- parser behavior after source-buffer mutation is impossible by construction;
- owned conversion boundaries, if introduced.


## 13. Parser cursor and AST boundary

The Phase 2 parser now uses a two-lifetime cursor:

`TokenCursor<'tokens, 'src>`

where:

- `'tokens` is the borrow of the token slice;
- `'src` is the lifetime of source text inside those tokens.

This prevents the parser from taking ownership of the lexical stream.

The syntax tree is correspondingly parameterized as `AST<'src>` through
`Program<'src>` and its child nodes. Identifier names, string contents, and
other source-bearing values borrow directly from the original source buffer.

Collections such as declaration and statement lists still use `Vec`. The
current milestone therefore provides zero-copy **source data**, not a claim of
zero heap allocation.

## 14. Parser implementation status

The initial recursive-descent infrastructure is now present for:

- top-level `fn`, `intent`, and `rule` declarations;
- `@grad fn` declaration recognition;
- typed parameter lists;
- generic type references;
- structural blocks;
- `let`, `return`, and `backtrack` statements;
- expression statements;
- unary and binary expressions;
- function calls;
- symbolic expressions;
- self-modification syntax;
- borrowed literals;
- source-aware parse errors.

This is intentionally a parser foundation, not the completed language grammar.

Cognitive semantics remain outside the parser and belong to the semantic
verification layer.

## 15. Next parser hardening

Before expanding the cognitive grammar, the parser should receive:

1. grammar conformance fixtures;
2. diagnostic recovery infrastructure;
3. match/pattern syntax;
4. effect clauses;
5. full type grammar;
6. intent member grammar beyond the initial string/block forms;
7. allocation measurements;
8. parser fuzz/property testing;
9. AST invariants and versioning policy.

