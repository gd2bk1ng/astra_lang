//! Semantic verification for the borrowed SyntraLang AST.
//!
//! This module is intentionally separate from parsing. The parser answers
//! "what source structure was written?"; this pass answers "is that structure
//! semantically admissible under the currently implemented verifier rules?".
//!
//! The current AST does not yet encode probabilistic variable declarations or
//! function-local effect/capability clauses. The verifier therefore implements
//! the strongest checks expressible today and exposes explicit capability and
//! epistemic state types for the next syntax/IR expansion.

use crate::ast::*;
use crate::diagnostics::{Diagnostic, DiagnosticEngine};
use crate::tokens::Span;
use std::collections::{BTreeMap, BTreeSet};

const CODE_EPISTEMIC_RANGE: &str = "SYN2001";
const CODE_EPISTEMIC_GATE: &str = "SYN2002";
const CODE_EFFECT_DUPLICATE: &str = "SYN2101";
const CODE_EFFECT_REQUIRED: &str = "SYN2102";
const CODE_CAPABILITY_LEAK: &str = "SYN2103";
const CODE_GOVERNANCE_CONTRADICTION: &str = "SYN2201";
const CODE_GOVERNANCE_EMPTY: &str = "SYN2202";

/// The semantic confidence attached to a value.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ConfidenceState {
    Deterministic,
    Probabilistic { confidence: f64 },
    Unknown,
}

impl ConfidenceState {
    fn confidence(self) -> Option<f64> {
        match self {
            Self::Deterministic => Some(1.0),
            Self::Probabilistic { confidence } => Some(confidence),
            Self::Unknown => None,
        }
    }
}

/// Semantic types used by the first verifier layer.
///
/// Probabilistic is deliberately distinct from its underlying value type so
/// deterministic consumers cannot silently erase epistemic provenance.
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType<'a> {
    Unknown,
    Boolean,
    Integer,
    Float,
    String,
    Probabilistic {
        value: Box<SemanticType<'a>>,
        confidence: f64,
    },
    Capability(&'a str),
    Unit,
}

/// A capability/effect declaration visible to the current verification scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Capability<'a> {
    pub name: &'a str,
}

/// Tracks affine capability state. The current AST does not yet expose
/// capability-token declarations, so this ledger is an explicit foundation
/// for the upcoming capability-aware syntax/IR.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CapabilityLedger<'a> {
    available: BTreeSet<&'a str>,
    consumed: BTreeSet<&'a str>,
}

impl<'a> CapabilityLedger<'a> {
    pub fn grant(&mut self, capability: Capability<'a>) -> bool {
        self.available.insert(capability.name)
    }

    pub fn consume(&mut self, capability: Capability<'a>) -> Result<(), CapabilityError<'a>> {
        if !self.available.remove(capability.name) {
            if self.consumed.contains(capability.name) {
                return Err(CapabilityError::AlreadyConsumed(capability.name));
            }
            return Err(CapabilityError::Unavailable(capability.name));
        }
        self.consumed.insert(capability.name);
        Ok(())
    }

    pub fn available(&self, name: &str) -> bool {
        self.available.contains(name)
    }

    pub fn consumed(&self, name: &str) -> bool {
        self.consumed.contains(name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityError<'a> {
    Unavailable(&'a str),
    AlreadyConsumed(&'a str),
}

/// Result of semantic verification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticReport<'a> {
    pub diagnostics: Vec<Diagnostic<'a>>,
}

impl<'a> SemanticReport<'a> {
    pub fn is_success(&self) -> bool {
        self.diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != crate::diagnostics::DiagnosticSeverity::Error)
    }

    pub fn errors(&self) -> impl Iterator<Item = &Diagnostic<'a>> {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity == crate::diagnostics::DiagnosticSeverity::Error)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct NumericConstraint {
    lower: Option<f64>,
    upper: Option<f64>,
}

/// Semantic verifier over a borrowed SyntraLang program.
#[derive(Debug)]
pub struct SemanticVerifier<'src> {
    source: &'src str,
    diagnostics: Vec<Diagnostic<'src>>,
    declared_effects: BTreeSet<&'src str>,
    epistemic_confidence: Option<f64>,
}

impl<'src> SemanticVerifier<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            declared_effects: BTreeSet::new(),
            epistemic_confidence: None,
        }
    }

    pub fn verify_program(mut self, program: &Program<'src>) -> SemanticReport<'src> {
        for declaration in &program.declarations {
            match declaration {
                Declaration::Function(function) => self.verify_function(function),
                Declaration::Intent(intent) => self.verify_intent(intent),
                Declaration::Rule(rule) => self.verify_rule(rule),
            }
        }

        SemanticReport {
            diagnostics: self.diagnostics,
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic<'src>] {
        &self.diagnostics
    }

    fn verify_function(&mut self, function: &Function<'src>) {
        self.declared_effects.clear();
        self.epistemic_confidence = None;
        self.verify_block(&function.body);
    }

    fn verify_rule(&mut self, rule: &Rule<'src>) {
        self.declared_effects.clear();
        self.epistemic_confidence = None;
        self.verify_block(&rule.body);
    }

    fn verify_intent(&mut self, intent: &Intent<'src>) {
        let previous_effects = std::mem::take(&mut self.declared_effects);
        let previous_confidence = self.epistemic_confidence.take();

        for member in &intent.members {
            if let IntentValue::Effects(effects) = &member.value {
                for item in &effects.items {
                    if !self.declared_effects.insert(item.name) {
                        self.error(
                            CODE_EFFECT_DUPLICATE,
                            format!("effect '{}' is declared more than once", item.name),
                            item.span,
                        );
                    }
                }
            }
        }

        // Establish epistemic facts before checking consumers, regardless of
        // declaration order inside the intent.
        for member in &intent.members {
            if let IntentValue::Uncertainty(uncertainty) = &member.value {
                self.verify_uncertainty(uncertainty);
            }
        }

        for member in &intent.members {
            match &member.value {
                IntentValue::Goal(goal) => self.verify_goal(goal),
                IntentValue::Effects(_) | IntentValue::Uncertainty(_) => {}
                IntentValue::Governance(governance) => self.verify_governance(governance),
                IntentValue::String(_) | IntentValue::Block(_) => {}
            }
        }

        self.declared_effects = previous_effects;
        self.epistemic_confidence = previous_confidence;
    }

    fn verify_goal(&mut self, goal: &Goal<'src>) {
        for member in &goal.members {
            match member {
                GoalMember::Description { value, span } => {
                    if value.trim().is_empty() {
                        self.error(CODE_GOVERNANCE_EMPTY, "goal description cannot be empty", *span);
                    }
                }
                GoalMember::Priority { value, .. } => {
                    let _ = self.infer_expression(value);
                }
                GoalMember::Condition { value, span } => {
                    let state = self.infer_expression(value);
                    self.verify_deterministic_gate(state, *span);
                }
                GoalMember::Target { value, span } => {
                    let state = self.infer_expression(value);
                    self.verify_deterministic_gate(state, *span);
                }
            }
        }
    }

    fn verify_uncertainty(&mut self, uncertainty: &Uncertainty<'src>) {
        for member in &uncertainty.members {
            match member {
                UncertaintyMember::Confidence { value, span } => {
                    match self.literal_number(value) {
                        Some(confidence) if (0.0..=1.0).contains(&confidence) => {
                            self.epistemic_confidence = Some(confidence);
                        }
                        Some(confidence) => self.error(
                            CODE_EPISTEMIC_RANGE,
                            format!("confidence must be within [0, 1], found {confidence}"),
                            *span,
                        ),
                        None => {
                            self.error(
                                CODE_EPISTEMIC_RANGE,
                                "confidence must be statically evaluable to a numeric literal",
                                *span,
                            );
                        }
                    }
                }
                UncertaintyMember::Assumption { value, span }
                | UncertaintyMember::Source { value, span } => {
                    if value.trim().is_empty() {
                        self.error(CODE_GOVERNANCE_EMPTY, "uncertainty metadata cannot be empty", *span);
                    }
                }
                UncertaintyMember::Evidence { value, .. } => {
                    let _ = self.infer_expression(value);
                }
            }
        }
    }

    fn verify_governance(&mut self, governance: &Governance<'src>) {
        let mut constraints: BTreeMap<&'src str, NumericConstraint> = BTreeMap::new();

        for member in &governance.members {
            match member {
                GovernanceMember::Requires { value, span } => {
                    if value.trim().is_empty() {
                        self.error(CODE_GOVERNANCE_EMPTY, "governance requires value cannot be empty", *span);
                    }
                }
                GovernanceMember::Approval { value, span } => {
                    if value.trim().is_empty() {
                        self.error(CODE_GOVERNANCE_EMPTY, "governance approval value cannot be empty", *span);
                    }
                }
                GovernanceMember::Policy { value, span } => {
                    if value.trim().is_empty() {
                        self.error(CODE_GOVERNANCE_EMPTY, "governance policy cannot be empty", *span);
                    }
                }
                GovernanceMember::Constraint { value, span } => {
                    if let Some((name, constraint)) = self.numeric_constraint(value) {
                        let entry = constraints.entry(name).or_insert(NumericConstraint {
                            lower: None,
                            upper: None,
                        });
                        entry.lower = match constraint.lower {
                            Some(bound) => Some(entry.lower.map_or(bound, |old| old.max(bound))),
                            None => entry.lower,
                        };
                        entry.upper = match constraint.upper {
                            Some(bound) => Some(entry.upper.map_or(bound, |old| old.min(bound))),
                            None => entry.upper,
                        };

                        if let (Some(lower), Some(upper)) = (entry.lower, entry.upper)
                            && lower > upper
                        {
                            self.error(
                                CODE_GOVERNANCE_CONTRADICTION,
                                format!("governance constraints on '{name}' are contradictory"),
                                *span,
                            );
                        }
                    } else {
                        let _ = self.infer_expression(value);
                    }
                }
            }
        }
    }

    fn verify_block(&mut self, block: &Block<'src>) {
        for statement in &block.statements {
            match statement {
                Statement::Let { value, .. } => {
                    if let Some(value) = value {
                        let _ = self.infer_expression(value);
                    }
                }
                Statement::Return { value, .. }
                | Statement::Expression { expression: value, .. } => {
                    let _ = self.infer_expression(value);
                }
                Statement::Backtrack { body, .. } => self.verify_block(body),
                Statement::Annotation(annotation) => {
                    for argument in &annotation.arguments {
                        let _ = self.infer_expression(argument);
                    }
                }
            }
        }
    }

    fn infer_expression(&mut self, expression: &Expression<'src>) -> ConfidenceState {
        match expression {
            Expression::Identifier(_, _) => match self.epistemic_confidence {
                Some(confidence) if confidence < 1.0 => {
                    ConfidenceState::Probabilistic { confidence }
                }
                Some(_) => ConfidenceState::Deterministic,
                None => ConfidenceState::Unknown,
            },
            Expression::Literal(_, _) => ConfidenceState::Deterministic,
            Expression::Call {
                callee,
                arguments,
                span,
            } => {
                let callee_name = match callee.as_ref() {
                    Expression::Identifier(name, _) => Some(*name),
                    _ => None,
                };

                let mut argument_state = None;
                for argument in arguments {
                    let state = self.infer_expression(argument);
                    argument_state = Some(match argument_state {
                        Some(previous) => combine_epistemic(previous, state),
                        None => state,
                    });
                }

                if let Some(name) = callee_name {
                    self.verify_effect_for_operation(name, *span);

                    if is_resolution_operation(name) {
                        return ConfidenceState::Deterministic;
                    }

                    if is_deterministic_gate(name) {
                        if let Some(ConfidenceState::Probabilistic { confidence }) = argument_state
                            && confidence < 1.0
                        {
                            self.error(
                                CODE_EPISTEMIC_GATE,
                                format!(
                                    "probabilistic argument with confidence {confidence:.3} cannot enter deterministic gate '{name}' without explicit resolution"
                                ),
                                *span,
                            );
                        }
                        return ConfidenceState::Deterministic;
                    }
                }

                argument_state.unwrap_or(ConfidenceState::Unknown)
            }
            Expression::Unary { operand, .. } => self.infer_expression(operand),
            Expression::Binary { left, right, .. } => {
                let left_state = self.infer_expression(left);
                let right_state = self.infer_expression(right);
                combine_epistemic(left_state, right_state)
            }
            Expression::Block(block) => {
                self.verify_block(block);
                ConfidenceState::Unknown
            }
            Expression::Symbolic { .. } => ConfidenceState::Unknown,
            Expression::SelfModify { target, patch, span } => {
                let _ = self.infer_expression(patch);
                self.verify_effect_for_operation("SelfModify", *span);
                if !self.declared_effects.contains("SelfModify") {
                    self.error(
                        CODE_CAPABILITY_LEAK,
                        format!("self-modification of '{target}' requires the declared SelfModify effect"),
                        *span,
                    );
                }
                ConfidenceState::Unknown
            }
        }
    }

    fn verify_effect_for_operation(&mut self, operation: &str, span: Span) {
        let required = match operation {
            "allocate_memory" => Some("AllocateMemory"),
            "dispatch_network_command" => Some("DispatchNetworkCommand"),
            "write_file" => Some("WriteFile"),
            "read_file" => Some("ReadFile"),
            "spawn_process" => Some("SpawnProcess"),
            "SelfModify" => Some("SelfModify"),
            _ => None,
        };

        let Some(required) = required else {
            return;
        };

        if !self.declared_effects.contains(required) {
            self.error(
                CODE_EFFECT_REQUIRED,
                format!(
                    "operation '{operation}' requires effect '{required}', but that effect is not declared in the current semantic scope"
                ),
                span,
            );
        }
    }

    fn verify_deterministic_gate(&mut self, state: ConfidenceState, span: Span) {
        if let ConfidenceState::Probabilistic { confidence } = state
            && confidence < 1.0
        {
            self.error(
                CODE_EPISTEMIC_GATE,
                format!(
                    "probabilistic value with confidence {confidence:.3} cannot enter a deterministic gate without explicit resolution"
                ),
                span,
            );
        }
    }

    fn literal_number(&self, expression: &Expression<'src>) -> Option<f64> {
        match expression {
            Expression::Literal(Literal::Integer(value), _) => Some(*value as f64),
            Expression::Literal(Literal::Float(value), _) => Some(*value),
            _ => None,
        }
    }

    fn numeric_constraint(
        &self,
        expression: &Expression<'src>,
    ) -> Option<(&'src str, NumericConstraint)> {
        let Expression::Binary {
            left,
            operator,
            right,
            ..
        } = expression
        else {
            return None;
        };

        let Expression::Identifier(name, _) = left.as_ref() else {
            return None;
        };
        let value = self.literal_number(right)?;

        match operator {
            BinaryOp::Greater | BinaryOp::GreaterEqual => {
                Some((*name, NumericConstraint { lower: Some(value), upper: None }))
            }
            BinaryOp::Less | BinaryOp::LessEqual => {
                Some((*name, NumericConstraint { lower: None, upper: Some(value) }))
            }
            BinaryOp::Equal => Some((
                *name,
                NumericConstraint {
                    lower: Some(value),
                    upper: Some(value),
                },
            )),
            _ => None,
        }
    }

    fn error(&mut self, code: &'static str, message: impl Into<String>, span: Span) {
        self.diagnostics.push(DiagnosticEngine::semantic_error(
            self.source,
            code,
            message,
            span,
        ));
    }
}

fn combine_epistemic(left: ConfidenceState, right: ConfidenceState) -> ConfidenceState {
    match (left, right) {
        (
            ConfidenceState::Probabilistic { confidence: left },
            ConfidenceState::Probabilistic { confidence: right },
        ) => ConfidenceState::Probabilistic {
            confidence: left.min(right),
        },
        (ConfidenceState::Probabilistic { confidence }, ConfidenceState::Deterministic)
        | (ConfidenceState::Deterministic, ConfidenceState::Probabilistic { confidence }) => {
            ConfidenceState::Probabilistic { confidence }
        }
        (ConfidenceState::Unknown, _) | (_, ConfidenceState::Unknown) => ConfidenceState::Unknown,
        _ => ConfidenceState::Deterministic,
    }
}

fn is_resolution_operation(name: &str) -> bool {
    matches!(name, "resolve" | "resolve_confidence" | "assert_confidence")
}

fn is_deterministic_gate(name: &str) -> bool {
    matches!(name, "deterministic_gate" | "control_gate" | "assert")
}
