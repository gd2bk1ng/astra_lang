# SyntraLang Language Reference

Status: **reference for the implemented pre-alpha parser subset**

This is a practical guide to forms represented by the current lexer, parser, and AST. It is not a stable 1.0 language specification. For exact grammar productions, see [syntra_grammar.ebnf](../syntra_grammar.ebnf).

## 1. Source model

SyntraLang source is UTF-8 text. The lexer returns source-borrowing tokens with byte spans, line/column positions, and original lexemes. Words that act as syntax markers are generally lexed as identifiers; the parser assigns their role.

## 2. Top-level declarations

### Functions

```syntra
fn add(left: Int<64>, right: Int<64>) -> Int<64> {
    return left + right;
}
```

Parameters have names and type references. A return type is optional. The current parser recognizes generic type-reference syntax such as `Int<64>`; this does not imply a complete type system.

### Gradient annotation

```syntra
@grad fn objective(x: Float<64>) -> Float<64> {
    return x * x;
}
```

The parser records the `grad` marker on the function AST. It does not implement automatic differentiation.

### Rules

```syntra
rule within_limit(value: Float<64>) -> Bool {
    return value <= 1.0;
}
```

Rules share the callable verification namespace with functions in the current verifier.

## 3. Intent declarations

```syntra
intent mission {
    motive "deliver a reliable release"
    action "prepare and validate the release"
    context {
        environment "staging"
    }
}
```

Generic intent members use an identifier followed by a string literal or a block.

## 4. Goals

```syntra
goal deploy {
    description "publish the approved build"
    priority 10
    condition tests_passed && review_complete
    target deploy()
}
```

Supported members are:
- `description` followed by a string literal;
- `priority` followed by an expression;
- `condition` followed by an expression;
- `target` followed by an expression.

These fields are parsed into dedicated AST variants. Their complete planning semantics are not implemented.

## 5. Effects

```syntra
effects { Pure, Network, IO }
```

The parser stores comma-separated effect identifiers. This is descriptive syntax, not a grant of operating-system permission or runtime authority.

## 6. Uncertainty

```syntra
uncertainty {
    confidence 0.95
    assumption "the staging environment matches production"
    evidence evaluate_release()
    source "release telemetry"
}
```

The AST preserves confidence, assumption, evidence, and source members. The numerical and epistemic model remains partial.

## 7. Governance

```syntra
governance {
    requires operator
    approval reviewer
    policy "production-release"
    constraint risk <= 0.2
}
```

The parser preserves required identifiers, approval labels, policy strings, and constraint expressions. A source-level `approval` member is not an authenticated approval event.

## 8. Statements

The current AST includes:
- `let name [= expression];`
- `return expression;`
- `backtrack { ... }`
- `::annotation` with optional expression arguments;
- expression statements terminated by semicolons.

Example:

```syntra
fn calculate(x: Int<64>) -> Int<64> {
    let doubled = x * 2;
    ::trace(doubled);
    return doubled;
}
```

## 9. Expressions

The current expression model includes:
- identifiers and integer, float, string, and boolean literals;
- function calls;
- unary negation and logical not;
- arithmetic, comparison, equality, and logical binary operators;
- block expressions;
- `symbolic(name)`;
- `modify(target, patch)`.

The presence of a `modify` AST node does not authorize code modification. A runtime and governance design would be required before such behavior could be safely executed.

## 10. Lexical rules

- Unicode XID identifiers, with underscore accepted as the initial character.
- Decimal integer and finite decimal float literals.
- `.5` and `1.0` are valid examples; `1.` is invalid.
- Numeric separators are restricted to supported positions.
- Double-quoted strings validate the escapes `\\n`, `\\r`, `\\t`, `\\0`, `\\\\`, `\\"`, and `\\/`.
- `//` line comments and `/* ... */` block comments.
- Unknown characters and malformed literals produce lexical errors.

The lexer validates string escapes but preserves raw string contents; it does not currently provide a fully decoded runtime string value.

## 11. Not yet part of the implemented language

Do not assume support for imports/modules, match expressions, pattern matching, lambdas, complete effect clauses on function declarations, a full type system, a standard library, macros, a compiler backend, or execution semantics. These require separate implementation and tests.
