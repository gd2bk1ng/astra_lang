# SyntraLang Architecture

Status: **current crate architecture, pre-alpha**

## 1. Design goal

Keep lexical structure, syntax, semantic verification, runtime coordination, and future execution backends separate. This prevents parser conveniences or source annotations from becoming accidental security authority.

## 2. Current source pipeline

```text
UTF-8 source
   |
   v
lexer.rs --------------------> Vec<Token<'src>>
   |                           borrowed lexemes + Span
   v
parser.rs -------------------> Program<'src>
   |                           borrowed AST from ast.rs
   v
verify.rs ------------------> SemanticReport
                               source-aware diagnostics
```

The lexer and parser are source-oriented and borrow where possible. Collections such as tokens, declarations, and statements still allocate.

## 3. Main modules

| Module | Responsibility |
|---|---|
| `tokens` | Token kinds, token values, and source spans |
| `lexer` | Unicode-aware scanning, literals, comments, operators, and lexical errors |
| `ast` | Borrowed syntax tree data structures |
| `parser` | Recursive-descent parser, token cursor, fail-fast and recovery modes |
| `diagnostics` | Source-aware parse and semantic diagnostics |
| `invariants` | AST/source invariant validation helpers |
| `epistemic` | Epistemic binding/environment primitives |
| `verify` | Initial static semantic checks |
| `engine::identity` | Strongly typed capability-kind and capability-owner identity types |
| `engine::runtime` | Registry-qualified invocation IDs and invocation lifecycle coordination |

## 4. Trust boundaries

### Lexer and parser

They recognize and preserve source structure. They do not authenticate users, authorize host operations, or execute code.

### Semantic verifier

The verifier checks a subset of semantic constraints expressible by the current AST. It is not a formal proof engine, complete type checker, sandbox, or runtime policy enforcement layer.

### Invocation coordinator

The current runtime coordinator allocates registry-qualified invocation IDs and controls invocation lifecycle transitions. It does not yet associate invocations with authoritative capability owners or execute language programs.

### Capability identities

The crate contains capability-kind and owner-ID types. These types alone do not constitute a capability ledger, transfer protocol, authorization mechanism, or runtime enforcement system.

## 5. Invariants worth preserving

- An invocation identity is bound to the registry that minted it.
- Foreign-registry invocation IDs must not alias local records.
- Parent/child invocation relationships are validated before lifecycle mutation.
- Terminal invocation states are irreversible under the current lifecycle model.
- Borrowed source data cannot outlive its source buffer.
- Source spans remain byte-based and traceable to the original input.
- Parsing governance or approval syntax never grants authority.
- Semantic capability bookkeeping must remain distinct from future runtime capability enforcement.

## 6. Planned pipeline

```text
Source -> Lexer -> Parser -> AST -> Semantic model
                                   |
                                   v
                             Cognitive IR
                                   |
                                   v
                         Verified runtime boundary
                                   |
                                   v
                         Backend / host integration
```

Cognitive IR, execution backend, capability ledger/transfer runtime, and host integration are not implemented as a complete path today.

## 7. Engineering rule

Any change crossing a trust boundary needs:
1. a written invariant;
2. a clear authority owner;
3. preflight/atomicity analysis where state changes;
4. adversarial tests;
5. source/API documentation;
6. local and CI verification evidence.
