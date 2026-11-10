# SyntraLang Cognitive Syntax Staging

Status: **syntax staging specification — not yet implemented**

This document defines the first cognitive-syntax extension points that will sit on
top of the hardened ordinary syntax substrate. It does not claim that the
semantic model, verifier, runtime, or governance system exists yet.

## 1. Design boundary

Cognitive syntax is ordinary SyntraLang syntax with domain-specific structural
forms. The lexer remains keyword-light. The parser recognizes the structural
forms and produces source-borrowing AST nodes. Semantic verification remains a
separate phase.

The first staged concepts are:

- `intent`
- `goal`
- `effects`
- `uncertainty`
- `governance`

These are deliberately introduced as composable blocks rather than as a second
language embedded beside the ordinary grammar.

## 2. Intent

The existing declaration remains the outer cognitive container:

~~~ebnf
IntentDecl ::= "intent" Identifier "{" { IntentMember } "}"
~~~

The staged member grammar is:

~~~ebnf
IntentMember ::= IntentField
               | GoalBlock
               | EffectsBlock
               | UncertaintyBlock
               | GovernanceBlock

IntentField ::= "motive" StringLiteral
              | "action" StringLiteral
              | Identifier StringLiteral
              | Identifier Block
~~~

This preserves the existing parser foundation while reserving explicit
structural forms for the cognitive layer.

## 3. Goal

Goals describe an intended outcome without assigning semantic truth to the
parser.

~~~ebnf
GoalBlock ::= "goal" Identifier "{" { GoalMember } "}"
GoalMember ::= "description" StringLiteral
             | "priority" Expression
             | "condition" Expression
             | "target" Expression
~~~

The parser's responsibility is structural validity and source provenance.
Priority, condition, and target semantics belong to later verification.

## 4. Effects

Effects declare the capabilities or side-effect classes associated with a
construct.

~~~ebnf
EffectsBlock ::= "effects" "{" EffectItem { "," EffectItem } "}"
EffectItem ::= Identifier
             | "Pure"
             | "IO"
             | "Network"
             | "SelfModify"
~~~

This is compatible with the existing effect vocabulary in the active grammar
draft. The parser should preserve each effect name as borrowed source text.

## 5. Uncertainty

Uncertainty is represented structurally first. It must not be treated as a
boolean confidence flag by the parser.

~~~ebnf
UncertaintyBlock ::= "uncertainty" "{" { UncertaintyMember } "}"
UncertaintyMember ::= "confidence" Expression
                    | "assumption" StringLiteral
                    | "evidence" Expression
                    | "source" StringLiteral
~~~

Numerical meaning, calibration, evidence quality, and epistemic rules belong to
the semantic layer.

## 6. Governance

Governance provides a syntax boundary for authority and approval metadata.

~~~ebnf
GovernanceBlock ::= "governance" "{" { GovernanceMember } "}"
GovernanceMember ::= "requires" Identifier
                   | "approval" Identifier
                   | "policy" StringLiteral
                   | "constraint" Expression
~~~

The parser must never interpret an `approval` field as proof that approval was
actually granted. Verification and runtime policy must establish that boundary.

## 7. Staged AST boundary

The first implementation should extend the borrowed AST rather than introduce
an owned cognitive tree.

The intended shape is conceptually:

~~~text
Program<'src>
  -> Declaration<'src>
      -> Intent<'src>
          -> IntentMember<'src>
              -> Goal<'src>
              -> Effects<'src>
              -> Uncertainty<'src>
              -> Governance<'src>
~~~

All source-bearing names, strings, and expressions continue to borrow from the
original source buffer.

## 8. Implementation gate

Before these forms are enabled in the production parser, the base parser must
satisfy:

1. precedence fixtures pass;
2. nested-block fixtures pass;
3. invalid-control-flow fixtures pass;
4. recovery terminates at defined synchronization boundaries;
5. diagnostics report exact byte ranges and line/column positions;
6. malformed input cannot cause parser panics or no-progress loops;
7. grammar conformance fixtures are established;
8. CI verifies the implementation.

Only after these gates should cognitive AST nodes be added.

## 9. Semantic separation

The parser establishes **what was written**.

The semantic verifier will establish:

- what the cognitive construct means;
- whether its types and effects are valid;
- whether uncertainty claims are well-formed;
- whether governance constraints are satisfied;
- whether an action is authorized.

This separation is a core SyntraLang architectural invariant.
