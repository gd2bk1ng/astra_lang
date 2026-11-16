# SyntraLang Language Contract

Status: **foundational design contract — evolving; not a complete language specification**

This document describes the intended semantic direction of SyntraLang and the constraints that should guide implementation. It is not a claim that all concepts below have complete syntax, static semantics, or runtime behavior today.

## 1. Purpose

SyntraLang explores a language model for programs and systems that need to make purpose, knowledge, uncertainty, effects, authority, and verification inspectable alongside ordinary computation.

The language should be usable independently of Syntra Kernel and the wider Syntra ecosystem.

## 2. Core concepts

- **Intent:** a named declaration describing purpose and associated structured members.
- **Goal:** a named outcome with descriptive, priority, condition, and target fields.
- **Knowledge and context:** information available to a computation, including its scope and provenance. These are design concepts; complete dedicated syntax is not yet implemented.
- **Relationships:** explicit connections among entities, concepts, events, and states. Planned.
- **Rules:** named callable-like declarations currently parsed alongside functions.
- **Capabilities:** authority to perform a defined class of operation. The source vocabulary and initial semantic checks are experimental; a secure runtime capability system is not yet complete.
- **Effects:** declarations of operation classes such as pure computation, I/O, or network access. Current intent syntax preserves effect names; verification coverage is partial.
- **Uncertainty:** structured confidence, assumption, evidence, and source members. The AST and parser preserve this structure; calibration and a complete probabilistic type system are future work.
- **Memory and observation:** concepts for persistent state and received information, including provenance, lifetime, and authorization. Planned.
- **Action:** an intentional interaction with an environment or resource. Planned as a complete semantic/runtime construct.
- **Reflection:** inspectable examination of state, reasoning, or execution history. Planned; it does not imply consciousness.
- **Verification:** checks that source structures satisfy supported constraints. A first semantic-verification pass exists; it is not a proof system.
- **Governance:** policy, approval metadata, constraints, and audit requirements. The parser preserves selected governance members; an approval label is never proof of authorization.
- **Learning and adaptation:** controlled changes based on experience. Future design; it must not imply unrestricted code modification.

## 3. Architectural separation

The language is designed around distinct responsibilities:

1. Source text and lexical structure.
2. Grammatical structure and borrowed AST.
3. Static semantic verification.
4. Lowered/intermediate representation.
5. Runtime authority and effect enforcement.
6. Backend execution and host integration.

Parsing must not grant authority. Static verification must not be presented as runtime enforcement. Runtime policy must not trust a source-level label such as `approved` without an authoritative external event and verifiable provenance.

## 4. Long-term compilation direction

```text
SyntraLang source
      |
      v
Lexer -> Tokens -> Parser -> AST
                           |
                           v
                   Semantic verification
                           |
                           v
                  Cognitive Intermediate IR
                           |
                           v
                 Runtime / backend / host
```

The Cognitive IR and complete backend are future targets, not implemented components of the current crate.

## 5. Design constraints

SyntraLang should prioritize:

- deterministic behavior where possible;
- explicit semantics over implicit or magical behavior;
- source spans and recoverable provenance;
- clear ownership and lifetime boundaries;
- capability-aware effects;
- explicit uncertainty rather than silently treating every value as certain;
- safe defaults and least authority;
- auditable governance;
- interoperability and portability;
- deliberate compatibility and versioning;
- independent testability of each layer.

## 6. Historical continuity

Earlier Astra work is historical research material. Concepts should be evaluated individually: preserve sound ideas, redesign fragile ones, reject ideas that conflict with this contract, and retain old code only when it provides useful evidence or regression material. Historical implementation details are not automatically normative for SyntraLang.

## 7. Current implementation boundary

Implemented foundations include a Unicode-aware borrowed lexer, an initial recursive-descent parser and borrowed AST, parser recovery, source-aware diagnostics, an initial semantic verifier, and a registry-qualified invocation coordinator.

Not yet implemented as complete language/runtime features: a stable formal 1.0 grammar, a full type/effect system, Cognitive IR, interpreter/compiler backend, secure capability ledger and transfer runtime, standard library, CLI, LSP, package manager, or production-grade governance/authorization.

See [Implementation Status](IMPLEMENTATION_STATUS.md) for the detailed matrix.
