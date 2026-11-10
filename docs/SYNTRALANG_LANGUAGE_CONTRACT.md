# SyntraLang Language Contract

Status: **Foundational design specification — evolving**

This document defines the semantic direction of SyntraLang. It is intentionally
more stable than any individual parser or runtime implementation. Syntax may
change while these concepts are refined.

## 1. Purpose

SyntraLang is designed to represent programs that do more than calculate values.
It should eventually provide first-class language concepts for describing
intelligence-oriented systems: what they intend to do, what they know, what they
observe, what they are permitted to do, how they reason, and how their actions
can be verified.

This is a design goal, not a claim that the current implementation already
provides these capabilities.

## 2. Core concepts

### Intent

**Intent** describes what a system is trying to accomplish.

An intent should eventually be representable independently from the mechanics
used to execute it.

### Goals

**Goals** describe desired outcomes, priorities, or target states.

Goals may eventually participate in planning, evaluation, prioritization, and
verification.

### Knowledge

**Knowledge** represents structured information that a system can use when
reasoning or acting.

The language should distinguish knowledge from transient computation where that
distinction is semantically useful.

### Context

**Context** describes the information relevant to a computation, decision, or
intent at a particular point in time.

Context should be explicit enough to support reproducible reasoning and
diagnostics.

### Relationships

**Relationships** represent connections among entities, concepts, events, states,
and other semantic objects.

This provides a foundation for graph-oriented and relational reasoning without
requiring every relationship to be encoded as an ordinary function call.

### Rules

**Rules** represent explicit constraints, conditions, transformations, or
reasoning relationships.

Rules should remain inspectable and verifiable rather than being hidden inside
opaque runtime behavior.

### Capabilities

**Capabilities** describe what an agent, function, module, or runtime component
is permitted to do.

Capability design will be tied to governance and effect tracking.

### Effects

**Effects** describe observable changes or interactions caused by an operation.

Potential effect classes include pure computation, I/O, network access, and
self-modification. The current grammar already contains an experimental effect
vocabulary; its semantics are not yet finalized.

### Uncertainty

**Uncertainty** represents incomplete knowledge, confidence, probability, or
other non-binary states of belief.

SyntraLang should not force cognitive systems to represent every conclusion as
simply true or false.

### Memory

**Memory** represents persistent or contextual information available to a
cognitive system.

Memory semantics must eventually distinguish scope, lifetime, provenance,
mutability, and authorization.

### Observation

**Observation** represents information received from an external or internal
source.

Observations should preserve enough provenance and context to support later
verification.

### Action

**Action** represents an intentional interaction with an environment, system,
resource, or other agent.

Actions should eventually expose their required capabilities and expected
effects.

### Reflection

**Reflection** represents examination of a system's own state, reasoning,
execution history, or performance.

Reflection does not imply consciousness. It is a language-level representation
of introspective or metacognitive operations.

### Verification

**Verification** represents checking whether a conclusion, state, or action
satisfies defined constraints or evidence requirements.

Verification should be composable with intents, rules, capabilities, and effects.

### Governance

**Governance** represents permissions, authorization, safety boundaries,
approval requirements, policy, auditability, and related controls.

Governance must be treated as a semantic concern rather than an optional UI
feature.

### Learning and adaptation

**Learning/adaptation** represents controlled changes in behavior, parameters,
knowledge, or strategies based on experience or new information.

Learning must not implicitly grant unrestricted authority to modify executable
code or bypass governance.

### Human intent and approval

**Human intent and approval** should be representable explicitly.

Human approval must eventually be modeled as an authoritative event with
defined scope, provenance, and expiry or revocation semantics where
appropriate. A label such as `approved` must never be treated as proof of
authorization by itself.

## 3. Architectural principle

The language should preserve a separation between:

1. **what is intended**,
2. **what is known or observed**,
3. **what reasoning is performed**,
4. **what actions are requested**,
5. **what capabilities permit those actions**,
6. **what effects may occur**, and
7. **what governance and verification constrain the system**.

This separation is intended to make cognitive programs inspectable, testable,
portable, and interoperable.

## 4. Compilation direction

The long-term compilation model is expected to evolve toward:

`SyntraLang source -> AST -> semantic model -> Cognitive IR -> runtime/backend`

The Cognitive IR is a future design target. It is not implemented by the
current lexical milestone.

The IR should preserve semantic information that would otherwise disappear
during ordinary lowering.

## 5. Design constraints

SyntraLang should prioritize:

- deterministic behavior where determinism is possible;
- explicit semantics over magical behavior;
- machine-readable source structure;
- strong diagnostics and source spans;
- capability-aware effects;
- explicit uncertainty;
- provenance and auditability;
- safe defaults;
- interoperability;
- portability;
- backwards compatibility through deliberate versioning;
- independent implementation and testing of language layers.

## 6. Historical continuity

The historical Astra implementation is a research and recovery artifact for
SyntraLang. Existing Astra concepts should be evaluated individually:

- preserve ideas that remain sound;
- redesign ideas whose implementation was fragile;
- reject ideas that no longer fit the language contract;
- retain historical code when it provides useful experiments or regression
  material.

SyntraLang is therefore an evolution of the Astra work, not a claim that every
historical implementation detail remains correct.

## 7. Current implementation status

At the current milestone:

- lexical infrastructure exists;
- token spans and source lexemes exist;
- strict malformed-input handling exists;
- Unicode XID-based identifiers exist;
- the formal grammar is still a draft;
- parser/AST integration is not yet complete;
- semantic implementations for the concepts above are not yet complete;
- Cognitive IR is not yet implemented;
- governance and self-modification remain research/design concerns.

The contract will be updated as implementation and testing establish stronger
evidence.
