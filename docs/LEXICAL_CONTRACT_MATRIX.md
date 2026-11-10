# SyntraLang Lexical Contract Matrix

Status: **Phase 1 — working contract**

This matrix reconciles recovered Astra syntax with the current SyntraLang
lexical contract. It is a design and regression artifact, not a claim that the
parser or semantic layer is complete.

## 1. Token architecture

SyntraLang currently uses a **lexical token space** with source spans and exact
source lexemes. Parser-level words such as `intent`, `fn`, `rule`,
`goal`, `governance`, `effects`, and `uncertainty` are intentionally
represented as identifiers at the lexer boundary.

This is deliberate: the lexer recognizes lexical structure; the parser assigns
syntactic/semantic roles.

The term **subspace token** is therefore treated here as a future architectural
concept: a parser/semantic classification layered over the stable lexical token
space, rather than a second incompatible lexer token universe.

## 2. Historical construct matrix

| Historical Astra construct | Expected lexical sequence | Current Syntra lexical tokens | Mismatch / risk | Final Syntra contract | Regression case |
|---|---|---|---|---|---|
| `intent name { ... }` | identifier, identifier, `{`, ... | Identifier("intent"), Identifier(name), LBrace | Historical lexer emitted dedicated Intent token | Keep lexical words as identifiers; parser recognizes declaration role | `intent syntra { }` |
| `motive "text"` | identifier, string | Identifier("motive"), StringLiteral | Historical lexer emitted Motive token | Parser-owned keyword role | `intent x { motive "reason" }` |
| `action "text"` | identifier, string | Identifier("action"), StringLiteral | Historical lexer emitted Action token | Parser-owned keyword role | `intent x { action "act" }` |
| `fn name(...)` | identifier, identifier, `(`, ... | Identifier("fn"), Identifier(name), LParen | Historical parser expected keyword-like identifiers | Stable identifier token contract | `fn main() {}` |
| `@grad` | `@`, identifier | At, Identifier("grad") | Historical parser/lexer disagreed about annotation representation | Always lex as two tokens; parser combines them | `@grad fn f() {}` |
| `rule name(...)` | identifier, identifier, LParen | Identifier("rule"), Identifier(name), LParen | None after keyword-light decision | Parser assigns RuleDecl role | `rule r() {}` |
| effect annotation | `{`, identifier list, `}` | LBrace, Identifier, Comma, RBrace | Braces also serve blocks | Context-sensitive parser interpretation | `fn f() {Pure, IO} {}` |
| return arrow | `-`, `>` | Arrow | Historical grammar and token APIs diverged | Arrow is a dedicated lexical operator | `fn f() -> Int<64> {}` |
| fat arrow | `=`, `>` | ArrowFat | Required by match grammar | Dedicated lexical operator | `x => y;` |
| type constructors | identifiers plus angle brackets | Identifier, LessThan, GreaterThan | No dedicated type keyword tokens | Parser owns type constructor interpretation | `Cap<Int<64>>` |
| symbolic expression | identifier, parens | Identifier("symbolic"), LParen, ... | Historical parser treats word semantically | Parser-level symbolic construct | `symbolic(x)` |
| self-modification | identifier, parens | Identifier("modify"), LParen, ... | Governance semantics are not lexical | Parser recognizes syntax; verifier governs legality | `modify(target, patch)` |
| backtrack | identifier, block | Identifier("backtrack"), LBrace | Historical parser uses keyword-like identifier | Parser-owned statement role | `backtrack {}` |
| annotation | `::`, identifier, optional args | DoubleColon, Identifier, ... | Legacy parser/lexer versions differed | Stable operator token + parser semantics | `::trace` |
| boolean literal | `true`/`false` | BoolLiteral | Dedicated literal is appropriate | Keep literal distinction | `true false` |
| integer | decimal digits | IntLiteral | Historical grammar permitted signs lexically | Sign is unary Minus in Syntra | `-42` |
| float | decimal digits + dot | FloatLiteral | Ambiguous `1.` must not be silently accepted | Require digits after decimal point | `1.0`, `.5`, reject `1.` |
| numeric separators | digits with separators | IntLiteral/FloatLiteral | Legacy grammar did not define separator placement | Separator only between digits | reject `1__2`, `1_.2`, `1._2` |
| string | quoted source | StringLiteral with raw lexeme | Legacy lexer lacked strict escape/error handling | Decode supported escapes; preserve raw lexeme | `"a\\n"`, reject invalid escape |
| unknown character | any unsupported source char | LexError | Legacy lexer silently skipped unknown input | Unknown input is a hard lexical error | reject `$` |
| Unicode identifier | XID identifier | Identifier | Legacy lexer used broad alphabetic checks | Unicode XID_Start/XID_Continue | `東京`, combining-mark case |

## 3. Cognitive keyword policy

The following words are reserved **semantic vocabulary**, not lexer keywords at
this stage:

- `intent`
- `goal`
- `knowledge`
- `context`
- `relationship`
- `rule`
- `capability`
- `effect`
- `uncertainty`
- `memory`
- `observation`
- `action`
- `reflection`
- `verification`
- `governance`
- `learning`
- `approval`

The parser will establish where these words have special grammatical meaning.
This keeps lexical compatibility stable and allows the semantic vocabulary to
grow without continually expanding the low-level token enum.

## 4. Zero-copy and no_std status

The long-term architecture calls for source-borrowing tokens over `&'a [u8]`
or an equivalent source buffer, with parsing performed without runtime heap
allocation where practical.

The **current implementation does not yet satisfy that invariant**. Current
`TokenKind` variants own `String` values and `Vec<Token>` is returned by
the lexer. This matrix therefore records zero-copy/no_std as a target
architecture, not an achieved property.

Before changing the public token API, we should establish the exact ownership
contract and benchmark its effect.

## 5. SIMD and affine capability status

SIMD acceleration is not currently required for correctness and should not
dictate the public lexer contract. A future optimized backend may use SIMD
scanning while preserving identical token boundaries and diagnostics.

Affine/linear capability enforcement is a semantic/type-system concern. The
lexer should preserve capability-related syntax faithfully; it should not claim
to enforce resource consumption rules that require semantic information.

## 6. Regression policy

Every row representing a resolved historical mismatch should eventually have:

1. a positive lexical fixture where applicable;
2. a malformed-input fixture where applicable;
3. a parser conformance test once the parser exists;
4. a semantic verification test when the construct acquires meaning.

A lexer regression must fail at the lexical boundary rather than being silently
reinterpreted downstream.
