use crate::ast::*;
use crate::tokens::Span;
use std::borrow::Cow;

/// Structural categories reported by AST invariant validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstInvariantKind {
    InvalidSpanBounds,
    Utf8BoundaryViolation,
    ContainmentViolation,
    SiblingOrderViolation,
    EmptyNodeSpan,
}

/// A single structural invariant violation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstInvariantError<'a> {
    pub kind: AstInvariantKind,
    pub span: Span,
    pub message: Cow<'a, str>,
}

/// Aggregate result of AST structural invariant validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstInvariantReport<'a> {
    pub errors: Vec<AstInvariantError<'a>>,
}

/// Validate the structural integrity of a parsed program against its source.
///
/// This layer is deliberately syntax-structural: it verifies spans,
/// containment, ordering, and UTF-8 safety, but does not perform semantic
/// verification.
pub fn validate_program_invariants<'src>(
    program: &Program<'src>,
    source: &'src str,
) -> Result<(), AstInvariantReport<'src>> {
    let mut validator = Validator { source, errors: Vec::new() };

    validator.check_span(program.span, "program", false);

    let mut previous = None;
    for declaration in &program.declarations {
        let span = declaration.span();
        validator.check_span(span, "declaration", true);
        validator.check_containment(program.span, span, "declaration", "program");
        validator.check_sibling_order(previous, span, "declarations");
        validator.validate_declaration(declaration);
        previous = Some(span);
    }

    validator.finish()
}

struct Validator<'src> {
    source: &'src str,
    errors: Vec<AstInvariantError<'src>>,
}

impl<'src> Validator<'src> {
    fn finish(self) -> Result<(), AstInvariantReport<'src>> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(AstInvariantReport { errors: self.errors })
        }
    }

    fn error(&mut self, kind: AstInvariantKind, span: Span, message: impl Into<Cow<'src, str>>) {
        self.errors.push(AstInvariantError { kind, span, message: message.into() });
    }

    fn check_span(&mut self, span: Span, label: &str, non_empty: bool) {
        let len = self.source.len();

        if span.start > span.end || span.end > len {
            self.error(
                AstInvariantKind::InvalidSpanBounds,
                span,
                format!(
                    "{label} span {}..{} is outside source bounds 0..{}",
                    span.start, span.end, len
                ),
            );
            return;
        }

        if !self.source.is_char_boundary(span.start) || !self.source.is_char_boundary(span.end) {
            self.error(
                AstInvariantKind::Utf8BoundaryViolation,
                span,
                format!("{label} span {}..{} is not aligned to UTF-8 character boundaries", span.start, span.end),
            );
        }

        if non_empty && span.start == span.end {
            self.error(
                AstInvariantKind::EmptyNodeSpan,
                span,
                format!("{label} has a degenerate empty span"),
            );
        }
    }

    fn check_containment(&mut self, parent: Span, child: Span, child_label: &str, parent_label: &str) {
        if child.start < parent.start || child.end > parent.end {
            self.error(
                AstInvariantKind::ContainmentViolation,
                child,
                format!(
                    "{child_label} span {}..{} is not contained in {parent_label} span {}..{}",
                    child.start, child.end, parent.start, parent.end
                ),
            );
        }
    }

    fn check_sibling_order(&mut self, previous: Option<Span>, current: Span, label: &str) {
        if let Some(previous) = previous {
            if previous.end > current.start {
                self.error(
                    AstInvariantKind::SiblingOrderViolation,
                    current,
                    format!(
                        "{label} overlap or reverse order: previous span {}..{} precedes current span {}..{}",
                        previous.start, previous.end, current.start, current.end
                    ),
                );
            }
        }
    }

    fn validate_declaration(&mut self, declaration: &Declaration<'src>) {
        match declaration {
            Declaration::Function(function) => self.validate_function(function),
            Declaration::Intent(intent) => self.validate_intent(intent),
            Declaration::Rule(rule) => self.validate_rule(rule),
        }
    }

    fn validate_function(&mut self, function: &Function<'src>) {
        let parent = function.span;
        let mut previous = None;

        for parameter in &function.params {
            self.validate_parameter(parameter);
            self.check_containment(parent, parameter.span, "parameter", "function");
            self.check_sibling_order(previous, parameter.span, "function parameters");
            previous = Some(parameter.span);
        }

        if let Some(return_type) = &function.return_type {
            self.validate_type_ref(return_type);
            self.check_containment(parent, return_type.span, "return type", "function");
            self.check_sibling_order(previous, return_type.span, "function signature members");
            previous = Some(return_type.span);
        }

        self.validate_block(&function.body);
        self.check_containment(parent, function.body.span, "function body", "function");
        self.check_sibling_order(previous, function.body.span, "function body");
    }

    fn validate_rule(&mut self, rule: &Rule<'src>) {
        let parent = rule.span;
        let mut previous = None;

        for parameter in &rule.params {
            self.validate_parameter(parameter);
            self.check_containment(parent, parameter.span, "parameter", "rule");
            self.check_sibling_order(previous, parameter.span, "rule parameters");
            previous = Some(parameter.span);
        }

        if let Some(return_type) = &rule.return_type {
            self.validate_type_ref(return_type);
            self.check_containment(parent, return_type.span, "return type", "rule");
            self.check_sibling_order(previous, return_type.span, "rule signature members");
            previous = Some(return_type.span);
        }

        self.validate_block(&rule.body);
        self.check_containment(parent, rule.body.span, "rule body", "rule");
        self.check_sibling_order(previous, rule.body.span, "rule body");
    }

    fn validate_parameter(&mut self, parameter: &Parameter<'src>) {
        self.check_span(parameter.span, "parameter", true);
        self.validate_type_ref(&parameter.ty);
        self.check_containment(parameter.span, parameter.ty.span, "parameter type", "parameter");
    }

    fn validate_type_ref(&mut self, type_ref: &TypeRef<'src>) {
        self.check_span(type_ref.span, "type", true);
        let parent = type_ref.span;
        let mut previous = None;

        for argument in &type_ref.arguments {
            self.validate_type_ref(argument);
            self.check_containment(parent, argument.span, "type argument", "type");
            self.check_sibling_order(previous, argument.span, "type arguments");
            previous = Some(argument.span);
        }
    }

    fn validate_intent(&mut self, intent: &Intent<'src>) {
        self.check_span(intent.span, "intent", true);
        let parent = intent.span;
        let mut previous = None;

        for member in &intent.members {
            self.check_span(member.span, "intent member", true);
            self.check_containment(parent, member.span, "intent member", "intent");
            self.check_sibling_order(previous, member.span, "intent members");
            self.validate_intent_value(&member.value, member.span);
            previous = Some(member.span);
        }
    }

    fn validate_intent_value(&mut self, value: &IntentValue<'src>, parent_span: Span) {
        match value {
            IntentValue::String(_) => {}
            IntentValue::Block(block) => {
                self.validate_block(block);
                self.check_containment(parent_span, block.span, "intent block", "intent member");
            }
            IntentValue::Goal(goal) => {
                self.validate_goal(goal);
                self.check_containment(parent_span, goal.span, "goal", "intent member");
            }
            IntentValue::Effects(effects) => {
                self.validate_effects(effects);
                self.check_containment(parent_span, effects.span, "effects", "intent member");
            }
            IntentValue::Uncertainty(uncertainty) => {
                self.validate_uncertainty(uncertainty);
                self.check_containment(parent_span, uncertainty.span, "uncertainty", "intent member");
            }
            IntentValue::Governance(governance) => {
                self.validate_governance(governance);
                self.check_containment(parent_span, governance.span, "governance", "intent member");
            }
        }
    }

    fn validate_goal(&mut self, goal: &Goal<'src>) {
        self.check_span(goal.span, "goal", true);
        let parent = goal.span;
        let mut previous = None;

        for member in &goal.members {
            let span = goal_member_span(member);
            self.check_span(span, "goal member", true);
            self.check_containment(parent, span, "goal member", "goal");
            self.check_sibling_order(previous, span, "goal members");
            match member {
                GoalMember::Description { .. } => {}
                GoalMember::Priority { value, .. }
                | GoalMember::Condition { value, .. }
                | GoalMember::Target { value, .. } => {
                    self.validate_expression(value);
                    self.check_containment(span, value.span(), "goal expression", "goal member");
                }
            }
            previous = Some(span);
        }
    }

    fn validate_effects(&mut self, effects: &Effects<'src>) {
        self.check_span(effects.span, "effects", true);
        let parent = effects.span;
        let mut previous = None;

        for item in &effects.items {
            self.check_span(item.span, "effect item", true);
            self.check_containment(parent, item.span, "effect item", "effects");
            self.check_sibling_order(previous, item.span, "effect items");
            previous = Some(item.span);
        }
    }

    fn validate_uncertainty(&mut self, uncertainty: &Uncertainty<'src>) {
        self.check_span(uncertainty.span, "uncertainty", true);
        let parent = uncertainty.span;
        let mut previous = None;

        for member in &uncertainty.members {
            let span = uncertainty_member_span(member);
            self.check_span(span, "uncertainty member", true);
            self.check_containment(parent, span, "uncertainty member", "uncertainty");
            self.check_sibling_order(previous, span, "uncertainty members");
            match member {
                UncertaintyMember::Confidence { value, .. }
                | UncertaintyMember::Evidence { value, .. } => {
                    self.validate_expression(value);
                    self.check_containment(span, value.span(), "uncertainty expression", "uncertainty member");
                }
                UncertaintyMember::Assumption { .. } | UncertaintyMember::Source { .. } => {}
            }
            previous = Some(span);
        }
    }

    fn validate_governance(&mut self, governance: &Governance<'src>) {
        self.check_span(governance.span, "governance", true);
        let parent = governance.span;
        let mut previous = None;

        for member in &governance.members {
            let span = governance_member_span(member);
            self.check_span(span, "governance member", true);
            self.check_containment(parent, span, "governance member", "governance");
            self.check_sibling_order(previous, span, "governance members");
            if let GovernanceMember::Constraint { value, .. } = member {
                self.validate_expression(value);
                self.check_containment(span, value.span(), "governance expression", "governance member");
            }
            previous = Some(span);
        }
    }

    fn validate_block(&mut self, block: &Block<'src>) {
        self.check_span(block.span, "block", true);
        let parent = block.span;
        let mut previous = None;

        for statement in &block.statements {
            let span = statement.span();
            self.check_span(span, "statement", true);
            self.check_containment(parent, span, "statement", "block");
            self.check_sibling_order(previous, span, "statements");
            self.validate_statement(statement);
            previous = Some(span);
        }
    }

    fn validate_statement(&mut self, statement: &Statement<'src>) {
        match statement {
            Statement::Let { value, span, .. } => {
                if let Some(value) = value {
                    self.validate_expression(value);
                    self.check_containment(*span, value.span(), "let expression", "let statement");
                }
            }
            Statement::Return { value, span } => {
                self.validate_expression(value);
                self.check_containment(*span, value.span(), "return expression", "return statement");
            }
            Statement::Expression { expression, span } => {
                self.validate_expression(expression);
                self.check_containment(*span, expression.span(), "expression", "expression statement");
            }
            Statement::Backtrack { body, span } => {
                self.validate_block(body);
                self.check_containment(*span, body.span, "backtrack body", "backtrack statement");
            }
            Statement::Annotation(annotation) => self.validate_annotation(annotation),
        }
    }

    fn validate_annotation(&mut self, annotation: &Annotation<'src>) {
        self.check_span(annotation.span, "annotation", true);
        let parent = annotation.span;
        let mut previous = None;

        for argument in &annotation.arguments {
            self.validate_expression(argument);
            self.check_containment(parent, argument.span(), "annotation argument", "annotation");
            self.check_sibling_order(previous, argument.span(), "annotation arguments");
            previous = Some(argument.span());
        }
    }

    fn validate_expression(&mut self, expression: &Expression<'src>) {
        let span = expression.span();
        self.check_span(span, "expression", true);

        match expression {
            Expression::Identifier(_, _)
            | Expression::Literal(_, _)
            | Expression::Symbolic { .. } => {}

            Expression::Call { callee, arguments, .. } => {
                self.validate_expression(callee);
                self.check_containment(span, callee.span(), "call callee", "call");
                let mut previous = Some(callee.span());
                for argument in arguments {
                    self.validate_expression(argument);
                    self.check_containment(span, argument.span(), "call argument", "call");
                    self.check_sibling_order(previous, argument.span(), "call arguments");
                    previous = Some(argument.span());
                }
            }

            Expression::Unary { operand, .. } => {
                self.validate_expression(operand);
                self.check_containment(span, operand.span(), "unary operand", "unary expression");
            }

            Expression::Binary { left, right, .. } => {
                self.validate_expression(left);
                self.validate_expression(right);
                self.check_containment(span, left.span(), "binary left operand", "binary expression");
                self.check_containment(span, right.span(), "binary right operand", "binary expression");
                self.check_sibling_order(Some(left.span()), right.span(), "binary operands");
            }

            Expression::Block(block) => {
                self.validate_block(block);
                self.check_containment(span, block.span, "expression block", "expression");
            }

            Expression::SelfModify { patch, .. } => {
                self.validate_expression(patch);
                self.check_containment(span, patch.span(), "self-modify patch", "self-modify expression");
            }
        }
    }
}

trait AstSpan {
    fn span(&self) -> Span;
}

impl AstSpan for Declaration<'_> {
    fn span(&self) -> Span {
        match self {
            Declaration::Function(value) => value.span,
            Declaration::Intent(value) => value.span,
            Declaration::Rule(value) => value.span,
        }
    }
}

impl AstSpan for Statement<'_> {
    fn span(&self) -> Span {
        match self {
            Statement::Let { span, .. }
            | Statement::Return { span, .. }
            | Statement::Expression { span, .. }
            | Statement::Backtrack { span, .. } => *span,
            Statement::Annotation(annotation) => annotation.span,
        }
    }
}

impl AstSpan for Expression<'_> {
    fn span(&self) -> Span {
        match self {
            Expression::Identifier(_, span)
            | Expression::Literal(_, span)
            | Expression::Symbolic { span, .. }
            | Expression::SelfModify { span, .. }
            | Expression::Unary { span, .. }
            | Expression::Binary { span, .. }
            | Expression::Call { span, .. } => *span,
            Expression::Block(block) => block.span,
        }
    }
}

fn goal_member_span(member: &GoalMember<'_>) -> Span {
    match member {
        GoalMember::Description { span, .. }
        | GoalMember::Priority { span, .. }
        | GoalMember::Condition { span, .. }
        | GoalMember::Target { span, .. } => *span,
    }
}

fn uncertainty_member_span(member: &UncertaintyMember<'_>) -> Span {
    match member {
        UncertaintyMember::Confidence { span, .. }
        | UncertaintyMember::Assumption { span, .. }
        | UncertaintyMember::Evidence { span, .. }
        | UncertaintyMember::Source { span, .. } => *span,
    }
}

fn governance_member_span(member: &GovernanceMember<'_>) -> Span {
    match member {
        GovernanceMember::Requires { span, .. }
        | GovernanceMember::Approval { span, .. }
        | GovernanceMember::Policy { span, .. }
        | GovernanceMember::Constraint { span, .. } => *span,
    }
}
