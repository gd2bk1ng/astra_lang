# SyntraLang Cognitive Syntax

Status: **initial cognitive AST and parser forms implemented; semantics remain partial**

This document describes the cognitive-oriented syntax currently represented in the borrowed AST and recognized by the parser. It does not claim a complete cognitive runtime, planner, authorization system, or probabilistic type system.

## 1. Design boundary

The lexer remains keyword-light. Cognitive words are identifiers at the lexical layer. The parser recognizes selected structural forms and produces borrowed AST nodes. Semantic verification is a separate pass.

## 2. Intent

```syntra
intent mission {
    motive "deliver safely"
    action "prepare a release"
}
```

An intent is a named declaration containing identifier/string or identifier/block members and structured cognitive blocks.

## 3. Goals

```syntra
goal deploy {
    description "production release"
    priority 10
    condition ready && score >= 0.8
    target deploy()
}
```

The AST has dedicated goal members for description, priority, condition, and target. Parsing preserves structure; it does not prove achievability.

## 4. Effects

```syntra
effects { Pure, Network, IO }
```

The parser stores effect names as borrowed identifiers. This syntax is not a runtime permission grant, and semantic effect coverage is incomplete.

## 5. Uncertainty

```syntra
uncertainty {
    confidence 0.95
    assumption "staging matches production"
    evidence assess()
    source "telemetry"
}
```

The AST distinguishes confidence, assumption, evidence, and source. Calibration, evidence quality, and a complete probabilistic type system remain future work.

## 6. Governance

```syntra
governance {
    requires operator
    approval reviewer
    policy "production-change"
    constraint risk <= 0.2
}
```

Governance nodes preserve requirements, approval labels, policy strings, and constraint expressions. A parsed approval label is not evidence that an authorized human or service approved an operation.

## 7. Safety boundary

The parser only records source structure. It does not:
- authenticate an operator;
- issue or validate approval credentials;
- grant runtime capabilities;
- enforce host permissions;
- execute an action;
- prove a policy is safe.

Those guarantees require separate verified semantic and runtime components.

## 8. Parser acceptance and remaining work

The repository contains cognitive parser fixtures covering structured intent members, source spans, and recovery from malformed cognitive syntax. Remaining work includes broader grammar conformance, AST invariant coverage, semantic rules with precise contracts, and fuzz/property testing.
