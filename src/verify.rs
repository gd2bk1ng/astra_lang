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
use crate::epistemic::{EpistemicBinding, EpistemicEnvironment, EpistemicState};
use crate::tokens::Span;
use std::collections::{BTreeMap, BTreeSet};

const CODE_EPISTEMIC_RANGE: &str = "SYN2001";
const CODE_EPISTEMIC_GATE: &str = "SYN2002";
const CODE_EPISTEMIC_BINDING_DUPLICATE: &str = "SYN2003";
const CODE_EFFECT_DUPLICATE: &str = "SYN2101";
const CODE_EFFECT_REQUIRED: &str = "SYN2102";
const CODE_CAPABILITY_LEAK: &str = "SYN2103";
const CODE_GOVERNANCE_CONTRADICTION: &str = "SYN2201";
const CODE_GOVERNANCE_EMPTY: &str = "SYN2202";
const CODE_TYPE_MISMATCH: &str = "SYN2301";
const CODE_CALLABLE_DUPLICATE: &str = "SYN2401";
const CODE_CALL_ARITY: &str = "SYN2402";
const CODE_CALL_ARGUMENT_TYPE: &str = "SYN2403";
const CODE_RETURN_TYPE_MISMATCH: &str = "SYN2501";
const CODE_MISSING_RETURN: &str = "SYN2502";

/// The semantic confidence attached to a value.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ConfidenceState {
    Deterministic,
    Probabilistic { confidence: f64 },
    Unknown,
}


/// Separates an expression's syntactic type from whether that type is safe
/// to expose to downstream semantic checks.
#[derive(Debug, Clone, PartialEq)]
struct ExpressionValidation<'src> {
    inferred_type: SemanticType<'src>,
    is_valid: bool,
}

impl<'src> ExpressionValidation<'src> {
    fn validated_type(&self) -> SemanticType<'src> {
        if self.is_valid {
            self.inferred_type.clone()
        } else {
            SemanticType::Unknown
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

#[derive(Debug, Clone)]
struct CallableParameter<'src> {
    name: &'src str,
    type_name: &'src str,
    span: Span,
}

#[derive(Debug, Clone)]
struct CallableSignature<'src> {
    name: &'src str,
    parameters: Vec<CallableParameter<'src>>,
    return_type: Option<&'src str>,
    declaration_span: Span,
    declaration_kind: &'static str,
}

#[derive(Debug, Default)]
struct CallableRegistry<'src> {
    signatures: BTreeMap<&'src str, CallableSignature<'src>>,
}

impl<'src> CallableRegistry<'src> {
    fn register(&mut self, signature: CallableSignature<'src>) -> bool {
        use std::collections::btree_map::Entry;

        match self.signatures.entry(signature.name) {
            Entry::Vacant(entry) => {
                entry.insert(signature);
                true
            }
            Entry::Occupied(_) => false,
        }
    }

    fn get(&self, name: &str) -> Option<&CallableSignature<'src>> {
        self.signatures.get(name)
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
    epistemic_environment: EpistemicEnvironment<'src>,
    callable_registry: CallableRegistry<'src>,
    expected_return_type: Option<SemanticType<'src>>,
}

impl<'src> SemanticVerifier<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            declared_effects: BTreeSet::new(),
            epistemic_confidence: None,
            epistemic_environment: EpistemicEnvironment::new(),
            callable_registry: CallableRegistry::default(),
            expected_return_type: None,
        }
    }

    pub fn verify_program(mut self, program: &Program<'src>) -> SemanticReport<'src> {
        // Pass 1: collect every top-level callable before checking any body.
        // This makes call resolution independent of source declaration order.
        self.collect_callable_declarations(program);

        // Pass 2: verify declaration bodies against the completed registry.
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

    fn collect_callable_declarations(&mut self, program: &Program<'src>) {
        for declaration in &program.declarations {
            let signature = match declaration {
                Declaration::Function(function) => Some(CallableSignature {
                    name: function.name,
                    parameters: function
                        .params
                        .iter()
                        .map(|parameter| CallableParameter {
                            name: parameter.name,
                            type_name: parameter.ty.name,
                            span: parameter.span,
                        })
                        .collect(),
                    return_type: function.return_type.as_ref().map(|ty| ty.name),
                    declaration_span: function.span,
                    declaration_kind: "function",
                }),
                Declaration::Rule(rule) => Some(CallableSignature {
                    name: rule.name,
                    parameters: rule
                        .params
                        .iter()
                        .map(|parameter| CallableParameter {
                            name: parameter.name,
                            type_name: parameter.ty.name,
                            span: parameter.span,
                        })
                        .collect(),
                    return_type: rule.return_type.as_ref().map(|ty| ty.name),
                    declaration_span: rule.span,
                    declaration_kind: "rule",
                }),
                Declaration::Intent(_) => None,
            };

            if let Some(signature) = signature {
                let name = signature.name;
                let kind = signature.declaration_kind;
                let span = signature.declaration_span;
                if !self.callable_registry.register(signature) {
                    self.error(
                        CODE_CALLABLE_DUPLICATE,
                        format!("callable '{name}' is declared more than once; functions and rules share one top-level namespace (duplicate {kind})"),
                        span,
                    );
                }
            }
        }
    }

    fn verify_function(&mut self, function: &Function<'src>) {
        self.declared_effects.clear();
        self.epistemic_confidence = None;
        self.epistemic_environment = EpistemicEnvironment::new();

        let previous_return_type = self.expected_return_type.take();
        self.expected_return_type = function
            .return_type
            .as_ref()
            .map(|type_ref| semantic_type_from_name(type_ref.name));

        for parameter in &function.params {
            self.register_parameter(parameter);
        }
        self.verify_block(&function.body);

        if let Some(expected_type) = function
            .return_type
            .as_ref()
            .map(|type_ref| semantic_type_from_name(type_ref.name))
            .filter(requires_explicit_return)
            && analyze_block_flow(&function.body) == FlowOutcome::FallsThrough
        {
            self.error(
                CODE_MISSING_RETURN,
                format!(
                    "callable declared to return {expected_type:?} but may fall through without a return"
                ),
                function.body.span,
            );
        }

        self.expected_return_type = previous_return_type;
        self.epistemic_environment = EpistemicEnvironment::new();
    }

    fn verify_rule(&mut self, rule: &Rule<'src>) {
        self.declared_effects.clear();
        self.epistemic_confidence = None;
        self.epistemic_environment = EpistemicEnvironment::new();

        let previous_return_type = self.expected_return_type.take();
        self.expected_return_type = rule
            .return_type
            .as_ref()
            .map(|type_ref| semantic_type_from_name(type_ref.name));

        for parameter in &rule.params {
            self.register_parameter(parameter);
        }
        self.verify_block(&rule.body);

        if let Some(expected_type) = rule
            .return_type
            .as_ref()
            .map(|type_ref| semantic_type_from_name(type_ref.name))
            .filter(requires_explicit_return)
            && analyze_block_flow(&rule.body) == FlowOutcome::FallsThrough
        {
            self.error(
                CODE_MISSING_RETURN,
                format!(
                    "callable declared to return {expected_type:?} but may fall through without a return"
                ),
                rule.body.span,
            );
        }

        self.expected_return_type = previous_return_type;
        self.epistemic_environment = EpistemicEnvironment::new();
    }

    fn register_parameter(&mut self, parameter: &Parameter<'src>) {
        let binding = EpistemicBinding::new(
            parameter.name,
            parameter.ty.name,
            EpistemicState::Uncertain,
            parameter.span,
        );

        if self
            .epistemic_environment
            .declare(parameter.name, binding)
            .is_err()
        {
            self.error(
                CODE_EPISTEMIC_BINDING_DUPLICATE,
                format!(
                    "parameter '{}' is already declared in this function or rule scope",
                    parameter.name
                ),
                parameter.span,
            );
        }
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
                IntentValue::Block(block) => {
                    // Intent-owned blocks form their own lexical scope while
                    // inheriting the intent's declared effects and confidence.
                    self.epistemic_environment.enter_scope();
                    self.verify_block(block);
                    self.epistemic_environment.exit_scope();
                }
                IntentValue::String(_) => {}
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
                    self.validate_expression_types(value);
                    let _ = self.infer_expression(value);
                }
                GoalMember::Condition { value, span } => {
                    self.validate_expression_types(value);
                    let state = self.infer_expression(value);
                    self.verify_deterministic_gate(state, *span);
                }
                GoalMember::Target { value, span } => {
                    self.validate_expression_types(value);
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
                    self.validate_expression_types(value);
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
                    self.validate_expression_types(value);
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
                Statement::Let { name, value, span } => {
                    // Initializers are checked before declaration, so the new
                    // binding cannot resolve to itself during initialization.
                    let inferred_type = if let Some(value) = value {
                        let validation = self.validate_expression_types(value);
                        let _ = self.infer_expression(value);
                        validation.validated_type()
                    } else {
                        SemanticType::Unknown
                    };

                    self.register_local_binding(name, inferred_type, *span);
                }
                Statement::Return { value, .. } => {
                    let validation = self.validate_expression_types(value);
                    self.validate_return_type(value, validation.is_valid);
                    let _ = self.infer_expression(value);
                }
                Statement::Expression { expression: value, .. } => {
                    let _ = self.validate_expression_types(value);
                    let _ = self.infer_expression(value);
                }
                Statement::Backtrack { body, .. } => {
                    self.epistemic_environment.enter_scope();
                    self.verify_block(body);
                    self.epistemic_environment.exit_scope();
                }
                Statement::Annotation(annotation) => {
                    for argument in &annotation.arguments {
                        self.validate_expression_types(argument);
                        let _ = self.infer_expression(argument);
                    }
                }
            }
        }
    }

    fn validate_return_type(&mut self, value: &Expression<'src>, expression_is_valid: bool) {
        let Some(expected_type) = self.expected_return_type.clone() else {
            // A callable without a declared return type has no return contract
            // to enforce. The existing expression semantics remain unchanged.
            return;
        };

        if !expression_is_valid {
            return;
        }
        let actual_type = self.infer_expression_type(value);

        // Unknown or unsupported types cannot establish a mismatch. Preserve
        // the verifier's existing cascade-suppression contract.
        if actual_type == SemanticType::Unknown || expected_type == SemanticType::Unknown {
            return;
        }

        if !semantic_types_compatible(&expected_type, &actual_type) {
            self.error(
                CODE_RETURN_TYPE_MISMATCH,
                format!(
                    "return expression has type {actual_type:?}; expected {expected_type:?}"
                ),
                expression_span(value),
            );
        }
    }

    fn register_local_binding(
        &mut self,
        name: &'src str,
        inferred_type: SemanticType<'src>,
        span: Span,
    ) {
        let binding = EpistemicBinding::new(
            name,
            semantic_type_name(&inferred_type),
            EpistemicState::Uncertain,
            span,
        );

        if self.epistemic_environment.declare(name, binding).is_err() {
            self.error(
                CODE_EPISTEMIC_BINDING_DUPLICATE,
                format!("local binding '{name}' is already declared in this scope"),
                span,
            );
        }
    }

    fn infer_expression(&mut self, expression: &Expression<'src>) -> ConfidenceState {
        match expression {
            Expression::Identifier(name, _) => {
                // Resolve the active epistemic binding independently of confidence.
                // Unresolved identifiers remain compatible with existing verifier behavior.
                let _binding = self.epistemic_environment.lookup(name);

                match self.epistemic_confidence {
                    Some(confidence) if confidence < 1.0 => {
                        ConfidenceState::Probabilistic { confidence }
                    }
                    Some(_) => ConfidenceState::Deterministic,
                    None => ConfidenceState::Unknown,
                }
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
                self.epistemic_environment.enter_scope();
                self.verify_block(block);
                self.epistemic_environment.exit_scope();
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

    /// Infer the ordinary value type of an expression without changing
    /// confidence or epistemic state.
    ///
    /// This pass is intentionally conservative: unsupported constructs and
    /// unknown identifiers produce `SemanticType::Unknown`.
    fn infer_expression_type(&self, expression: &Expression<'src>) -> SemanticType<'src> {
        match expression {
            Expression::Literal(literal, _) => match literal {
                Literal::Integer(_) => SemanticType::Integer,
                Literal::Float(_) => SemanticType::Float,
                Literal::String(_) => SemanticType::String,
                Literal::Boolean(_) => SemanticType::Boolean,
            },
            Expression::Identifier(name, _) => self
                .epistemic_environment
                .lookup(name)
                .map(|binding| semantic_type_from_name(binding.ty.type_name))
                .unwrap_or(SemanticType::Unknown),
            Expression::Unary { operator, operand, .. } => {
                let operand_type = self.infer_expression_type(operand);
                match operator {
                    UnaryOp::Negate => match operand_type {
                        SemanticType::Integer => SemanticType::Integer,
                        SemanticType::Float => SemanticType::Float,
                        _ => SemanticType::Unknown,
                    },
                    UnaryOp::Not => SemanticType::Boolean,
                }
            }
            Expression::Binary { left, operator, right, .. } => {
                let left_type = self.infer_expression_type(left);
                let right_type = self.infer_expression_type(right);

                match operator {
                    BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual
                    | BinaryOp::And
                    | BinaryOp::Or => SemanticType::Boolean,
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide => match (left_type, right_type) {
                        (SemanticType::Integer, SemanticType::Integer) => SemanticType::Integer,
                        (SemanticType::Integer, SemanticType::Float)
                        | (SemanticType::Float, SemanticType::Integer)
                        | (SemanticType::Float, SemanticType::Float) => SemanticType::Float,
                        _ => SemanticType::Unknown,
                    },
                }
            }
            Expression::Call { callee, .. } => {
                let Expression::Identifier(name, _) = callee.as_ref() else {
                    return SemanticType::Unknown;
                };

                self.callable_registry
                    .get(name)
                    .and_then(|signature| signature.return_type)
                    .map(semantic_type_from_name)
                    .unwrap_or(SemanticType::Unknown)
            }
            Expression::Block(_)
            | Expression::Symbolic { .. }
            | Expression::SelfModify { .. } => SemanticType::Unknown,
        }
    }

    /// Validate an expression and retain its syntactic type separately from
    /// the type safe for downstream semantic checks.
    fn validate_expression_types(
        &mut self,
        expression: &Expression<'src>,
    ) -> ExpressionValidation<'src> {
        let inferred_type = self.infer_expression_type(expression);
        let mut is_valid = true;

        match expression {
            Expression::Unary {
                operator,
                operand,
                span,
            } => {
                let operand_validation = self.validate_expression_types(operand);
                let operand_type = operand_validation.validated_type();
                if operand_type != SemanticType::Unknown {
                    let compatible = match operator {
                        UnaryOp::Negate => is_numeric_type(&operand_type),
                        UnaryOp::Not => operand_type == SemanticType::Boolean,
                    };
                    if !compatible {
                        let operator_name = match operator {
                            UnaryOp::Negate => "-",
                            UnaryOp::Not => "!",
                        };
                        self.error(
                            CODE_TYPE_MISMATCH,
                            format!(
                                "operator '{operator_name}' is not compatible with operand type {operand_type:?}"
                            ),
                            *span,
                        );
                        is_valid = false;
                    }
                }
                is_valid &= operand_validation.is_valid;
            }
            Expression::Binary {
                left,
                operator,
                right,
                span,
            } => {
                let left_validation = self.validate_expression_types(left);
                let right_validation = self.validate_expression_types(right);
                let left_type = left_validation.validated_type();
                let right_type = right_validation.validated_type();

                if left_type != SemanticType::Unknown && right_type != SemanticType::Unknown {
                    let compatible = match operator {
                        BinaryOp::Add
                        | BinaryOp::Subtract
                        | BinaryOp::Multiply
                        | BinaryOp::Divide => {
                            is_numeric_type(&left_type) && is_numeric_type(&right_type)
                        }
                        BinaryOp::And | BinaryOp::Or => {
                            left_type == SemanticType::Boolean
                                && right_type == SemanticType::Boolean
                        }
                        BinaryOp::Equal | BinaryOp::NotEqual => {
                            left_type == right_type
                                || (is_numeric_type(&left_type) && is_numeric_type(&right_type))
                        }
                        BinaryOp::Less
                        | BinaryOp::LessEqual
                        | BinaryOp::Greater
                        | BinaryOp::GreaterEqual => {
                            is_numeric_type(&left_type) && is_numeric_type(&right_type)
                        }
                    };

                    if !compatible {
                        self.error(
                            CODE_TYPE_MISMATCH,
                            format!(
                                "operator '{}' is not compatible with operand types {left_type:?} and {right_type:?}",
                                binary_operator_name(*operator)
                            ),
                            *span,
                        );
                        is_valid = false;
                    }
                }
                is_valid &= left_validation.is_valid && right_validation.is_valid;
            }
            Expression::Call {
                callee,
                arguments,
                span,
            } => {
                let callee_validation = self.validate_expression_types(callee);
                let mut argument_validations = Vec::with_capacity(arguments.len());
                for argument in arguments {
                    argument_validations.push(self.validate_expression_types(argument));
                }
                is_valid &= callee_validation.is_valid
                    && argument_validations.iter().all(|result| result.is_valid);

                let Expression::Identifier(name, _) = callee.as_ref() else {
                    return ExpressionValidation { inferred_type, is_valid };
                };
                let Some(signature) = self.callable_registry.get(name).cloned() else {
                    return ExpressionValidation { inferred_type, is_valid };
                };

                if arguments.len() != signature.parameters.len() {
                    self.error(
                        CODE_CALL_ARITY,
                        format!(
                            "call to '{}' supplies {} argument(s), but its {} declaration at {}:{} requires {}",
                            signature.name,
                            arguments.len(),
                            signature.declaration_kind,
                            signature.declaration_span.line,
                            signature.declaration_span.column,
                            signature.parameters.len()
                        ),
                        *span,
                    );
                    is_valid = false;
                }

                for ((argument, validation), parameter) in arguments
                    .iter()
                    .zip(&argument_validations)
                    .zip(&signature.parameters)
                {
                    let actual_type = validation.validated_type();
                    let expected_type = semantic_type_from_name(parameter.type_name);

                    if actual_type == SemanticType::Unknown
                        || expected_type == SemanticType::Unknown
                    {
                        continue;
                    }

                    if !semantic_types_compatible(&expected_type, &actual_type) {
                        self.error(
                            CODE_CALL_ARGUMENT_TYPE,
                            format!(
                                "argument for parameter '{}' (declared at {}:{}) in call to '{}' has type {actual_type:?}; expected {expected_type:?}",
                                parameter.name,
                                parameter.span.line,
                                parameter.span.column,
                                signature.name
                            ),
                            expression_span(argument),
                        );
                        is_valid = false;
                    }
                }
            }
            Expression::SelfModify { patch, .. } => {
                let validation = self.validate_expression_types(patch);
                is_valid &= validation.is_valid;
            }
            // Expression blocks are visited by infer_expression(), which
            // enters their lexical scope and invokes verify_block().
            Expression::Block(_) => {}
            Expression::Identifier(_, _)
            | Expression::Literal(_, _)
            | Expression::Symbolic { .. } => {}
        }

        ExpressionValidation { inferred_type, is_valid }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FlowOutcome {
    FallsThrough,
    Returns,
}

/// Analyze sequential block flow without mutating verifier state.
/// Backtrack is deliberately conservative: its body cannot guarantee that
/// the enclosing sequence returns because its execution semantics are not
/// yet specified as unconditional.
fn analyze_block_flow(block: &Block<'_>) -> FlowOutcome {
    for statement in &block.statements {
        if analyze_statement_flow(statement) == FlowOutcome::Returns {
            return FlowOutcome::Returns;
        }
    }

    FlowOutcome::FallsThrough
}

fn analyze_statement_flow(statement: &Statement<'_>) -> FlowOutcome {
    match statement {
        Statement::Return { .. } => FlowOutcome::Returns,
        Statement::Let {
            value: Some(expression),
            ..
        }
        | Statement::Expression { expression, .. } => analyze_expression_flow(expression),
        Statement::Let { value: None, .. }
        | Statement::Backtrack { .. }
        | Statement::Annotation(_) => FlowOutcome::FallsThrough,
    }
}

/// Expression blocks propagate nonlocal returns through their containing
/// expression. Other expression children are visited in evaluation order.
fn analyze_expression_flow(expression: &Expression<'_>) -> FlowOutcome {
    match expression {
        Expression::Block(block) => analyze_block_flow(block),
        Expression::Call {
            callee, arguments, ..
        } => {
            if analyze_expression_flow(callee) == FlowOutcome::Returns {
                return FlowOutcome::Returns;
            }
            for argument in arguments {
                if analyze_expression_flow(argument) == FlowOutcome::Returns {
                    return FlowOutcome::Returns;
                }
            }
            FlowOutcome::FallsThrough
        }
        Expression::Unary { operand, .. } => analyze_expression_flow(operand),
        Expression::Binary {
            left,
            operator,
            right,
            ..
        } => {
            if analyze_expression_flow(left) == FlowOutcome::Returns {
                return FlowOutcome::Returns;
            }

            // Logical operators may short-circuit. A return nested in the
            // right operand is therefore not guaranteed to execute and cannot
            // establish a return for the enclosing callable.
            if matches!(operator, BinaryOp::And | BinaryOp::Or) {
                return FlowOutcome::FallsThrough;
            }

            analyze_expression_flow(right)
        }
        Expression::SelfModify { patch, .. } => analyze_expression_flow(patch),
        Expression::Identifier(_, _)
        | Expression::Literal(_, _)
        | Expression::Symbolic { .. } => FlowOutcome::FallsThrough,
    }
}

fn requires_explicit_return(ty: &SemanticType<'_>) -> bool {
    matches!(
        ty,
        SemanticType::Integer
            | SemanticType::Float
            | SemanticType::Boolean
            | SemanticType::String
    )
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

fn semantic_types_compatible(expected: &SemanticType<'_>, actual: &SemanticType<'_>) -> bool {
    expected == actual || (is_numeric_type(expected) && is_numeric_type(actual))
}

fn expression_span(expression: &Expression<'_>) -> Span {
    match expression {
        Expression::Identifier(_, span)
        | Expression::Literal(_, span)
        | Expression::Call { span, .. }
        | Expression::Unary { span, .. }
        | Expression::Binary { span, .. }
        | Expression::Symbolic { span, .. }
        | Expression::SelfModify { span, .. } => *span,
        Expression::Block(block) => block.span,
    }
}

fn is_numeric_type(semantic_type: &SemanticType<'_>) -> bool {
    matches!(semantic_type, SemanticType::Integer | SemanticType::Float)
}

fn binary_operator_name(operator: BinaryOp) -> &'static str {
    match operator {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

fn is_resolution_operation(name: &str) -> bool {
    matches!(name, "resolve" | "resolve_confidence" | "assert_confidence")
}

fn is_deterministic_gate(name: &str) -> bool {
    matches!(name, "deterministic_gate" | "control_gate" | "assert")
}

/// Translate an inferred semantic type into the epistemic binding type-name
/// vocabulary. Complex or unsupported inferred types remain unknown.
fn semantic_type_name<'src>(semantic_type: &SemanticType<'src>) -> &'src str {
    match semantic_type {
        SemanticType::Boolean => "Boolean",
        SemanticType::Integer => "Integer",
        SemanticType::Float => "Float",
        SemanticType::String => "String",
        SemanticType::Capability(name) => name,
        SemanticType::Unit => "Unit",
        SemanticType::Unknown | SemanticType::Probabilistic { .. } => "Unknown",
    }
}

/// Translate a declared binding type into the verifier's currently supported
/// value-type vocabulary. User-defined or unsupported names remain unknown.
fn semantic_type_from_name(type_name: &str) -> SemanticType<'_> {
    match type_name {
        "Integer" | "Int" | "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => {
            SemanticType::Integer
        }
        "Float" | "f32" | "f64" | "Double" => SemanticType::Float,
        "String" | "str" => SemanticType::String,
        "Boolean" | "Bool" | "bool" => SemanticType::Boolean,
        "Unit" | "()" => SemanticType::Unit,
        _ => SemanticType::Unknown,
    }
}

#[cfg(test)]
mod expression_type_tests {
    use super::*;

    fn verifier() -> SemanticVerifier<'static> {
        SemanticVerifier::new("")
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn literal_expression_types_are_inferred_without_environment_mutation() {
        let verifier = verifier();
        assert_eq!(
            verifier.infer_expression_type(&Expression::Literal(Literal::Integer(7), span())),
            SemanticType::Integer
        );
        assert_eq!(
            verifier.infer_expression_type(&Expression::Literal(Literal::Float(1.5), span())),
            SemanticType::Float
        );
        assert_eq!(
            verifier.infer_expression_type(&Expression::Literal(Literal::String("x"), span())),
            SemanticType::String
        );
        assert_eq!(
            verifier.infer_expression_type(&Expression::Literal(Literal::Boolean(true), span())),
            SemanticType::Boolean
        );
        assert!(verifier.epistemic_environment.lookup("x").is_none());
    }

    #[test]
    fn identifier_types_come_from_existing_binding_metadata() {
        let mut verifier = verifier();
        let binding = EpistemicBinding::new(
            "count",
            "Integer",
            EpistemicState::Uncertain,
            span(),
        );
        verifier
            .epistemic_environment
            .declare("count", binding)
            .expect("test binding declaration succeeds");

        assert_eq!(
            verifier.infer_expression_type(&Expression::Identifier("count", span())),
            SemanticType::Integer
        );
        assert_eq!(
            verifier
                .epistemic_environment
                .lookup("count")
                .expect("binding remains present")
                .epistemic_state,
            EpistemicState::Uncertain
        );
        assert_eq!(
            verifier.infer_expression_type(&Expression::Identifier("missing", span())),
            SemanticType::Unknown
        );
    }

    #[test]
    fn arithmetic_and_operator_types_are_derived_conservatively() {
        let verifier = verifier();
        let integer = Expression::Literal(Literal::Integer(1), span());
        let float = Expression::Literal(Literal::Float(2.0), span());
        let boolean = Expression::Literal(Literal::Boolean(true), span());

        let integer_plus_integer = Expression::Binary {
            left: Box::new(integer),
            operator: BinaryOp::Add,
            right: Box::new(Expression::Literal(Literal::Integer(2), span())),
            span: span(),
        };
        assert_eq!(
            verifier.infer_expression_type(&integer_plus_integer),
            SemanticType::Integer
        );

        let integer_plus_float = Expression::Binary {
            left: Box::new(Expression::Literal(Literal::Integer(1), span())),
            operator: BinaryOp::Add,
            right: Box::new(float),
            span: span(),
        };
        assert_eq!(
            verifier.infer_expression_type(&integer_plus_float),
            SemanticType::Float
        );

        let comparison = Expression::Binary {
            left: Box::new(Expression::Literal(Literal::Integer(1), span())),
            operator: BinaryOp::Less,
            right: Box::new(Expression::Literal(Literal::Integer(2), span())),
            span: span(),
        };
        assert_eq!(
            verifier.infer_expression_type(&comparison),
            SemanticType::Boolean
        );

        let logical_not = Expression::Unary {
            operator: UnaryOp::Not,
            operand: Box::new(boolean),
            span: span(),
        };
        assert_eq!(
            verifier.infer_expression_type(&logical_not),
            SemanticType::Boolean
        );
    }

    #[test]
    fn unsupported_expression_types_remain_unknown() {
        let verifier = verifier();
        let call = Expression::Call {
            callee: Box::new(Expression::Identifier("compute", span())),
            arguments: Vec::new(),
            span: span(),
        };
        assert_eq!(
            verifier.infer_expression_type(&call),
            SemanticType::Unknown
        );
        assert_eq!(
            semantic_type_from_name("CustomRecord"),
            SemanticType::Unknown
        );
    }

    #[test]
    fn let_binding_registration_propagates_inferred_type() {
        let mut verifier = verifier();
        let block = Block {
            statements: vec![Statement::Let {
                name: "count",
                value: Some(Expression::Literal(Literal::Integer(7), span())),
                span: span(),
            }],
            span: span(),
        };

        verifier.verify_block(&block);

        let binding = verifier
            .epistemic_environment
            .lookup("count")
            .expect("local binding is registered");
        assert_eq!(binding.ty.type_name, "Integer");
        assert_eq!(binding.epistemic_state, EpistemicState::Uncertain);
    }

    #[test]
    fn unsupported_local_binding_type_remains_unknown() {
        let mut verifier = verifier();

        verifier.register_local_binding(
            "value",
            SemanticType::Unknown,
            span(),
        );

        assert_eq!(
            verifier
                .epistemic_environment
                .lookup("value")
                .expect("local binding is registered")
                .ty
                .type_name,
            "Unknown"
        );
    }
}


#[cfg(test)]
mod adversarial_type_propagation_tests {
    use super::*;

    fn sample_verifier() -> SemanticVerifier<'static> {
        SemanticVerifier::new("")
    }

    fn span() -> Span {
        Span::new(0, 0)
    }

    #[test]
    fn adversarial_literal_and_unknown_propagation() {
        let v = sample_verifier();

        assert_eq!(
            v.infer_expression_type(&Expression::Literal(Literal::Integer(10), span())),
            SemanticType::Integer
        );
        assert_eq!(
            v.infer_expression_type(&Expression::Literal(Literal::Float(123.456), span())),
            SemanticType::Float
        );
        assert_eq!(
            v.infer_expression_type(&Expression::Literal(Literal::String("test"), span())),
            SemanticType::String
        );
        assert_eq!(
            v.infer_expression_type(&Expression::Literal(Literal::Boolean(true), span())),
            SemanticType::Boolean
        );

        assert_eq!(
            v.infer_expression_type(&Expression::Identifier("unknown", span())),
            SemanticType::Unknown
        );
        assert_eq!(
            v.infer_expression_type(&Expression::Symbolic {
                identifier: "sym",
                span: span(),
            }),
            SemanticType::Unknown
        );
    }

    #[test]
    fn adversarial_nested_and_incompatible_arithmetic() {
        let v = sample_verifier();

        let mixed_add = Expression::Binary {
            left: Box::new(Expression::Literal(Literal::Integer(1), span())),
            operator: BinaryOp::Add,
            right: Box::new(Expression::Literal(Literal::Float(2.5), span())),
            span: span(),
        };
        assert_eq!(
            v.infer_expression_type(&mixed_add),
            SemanticType::Float
        );

        let incompatible = Expression::Binary {
            left: Box::new(Expression::Literal(Literal::Integer(1), span())),
            operator: BinaryOp::Add,
            right: Box::new(Expression::Literal(Literal::String("text"), span())),
            span: span(),
        };
        assert_eq!(
            v.infer_expression_type(&incompatible),
            SemanticType::Unknown
        );

        let logical_not = Expression::Unary {
            operator: UnaryOp::Not,
            operand: Box::new(Expression::Identifier("unknown", span())),
            span: span(),
        };
        assert_eq!(
            v.infer_expression_type(&logical_not),
            SemanticType::Boolean
        );
    }

    #[test]
    fn adversarial_epistemic_independence_and_shadowing() {
        let mut v = sample_verifier();

        v.register_local_binding("x", SemanticType::Integer, span());
        let binding = v
            .epistemic_environment
            .lookup("x")
            .expect("outer binding is registered");

        assert_eq!(binding.ty.type_name, "Integer");
        assert_eq!(binding.ty.state, EpistemicState::Uncertain);
        assert_eq!(binding.epistemic_state, EpistemicState::Uncertain);

        v.epistemic_environment.enter_scope();
        v.register_local_binding("x", SemanticType::Float, span());
        assert_eq!(
            v.epistemic_environment
                .lookup("x")
                .expect("inner binding is registered")
                .ty
                .type_name,
            "Float"
        );

        v.epistemic_environment.exit_scope();
        assert_eq!(
            v.epistemic_environment
                .lookup("x")
                .expect("outer binding remains visible")
                .ty
                .type_name,
            "Integer"
        );
        assert_eq!(
            v.epistemic_environment
                .lookup("x")
                .expect("outer binding state remains unchanged")
                .epistemic_state,
            EpistemicState::Uncertain
        );
    }
}
