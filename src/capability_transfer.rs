//! Static capability-transfer precommit planning.
//!
//! This module resolves explicit move arguments against host-provisioned source
//! bindings and produces immutable plans. It never mutates the authoritative
//! capability ledger. Applying a plan is an execution-context responsibility.

use crate::ast::Expression;
use crate::tokens::Span;
use crate::verify::{
    CapabilityEnvironment, CapabilityError, CapabilityId, CapabilityInstanceId,
    CapabilityLedger, CapabilityOwnerId, CapabilityState,
};

/// One capability parameter in its original positional slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityParameterSpec<'a> {
    pub name: &'a str,
    pub kind: CapabilityId,
    pub declaration_span: Span,
}

/// A statically resolved transfer item. This is a description, not ownership.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityTransferItem<'a> {
    pub source_name: &'a str,
    pub instance: CapabilityInstanceId,
    pub source_owner: CapabilityOwnerId,
    pub parameter_name: &'a str,
    pub required_kind: CapabilityId,
    pub move_span: Span,
    pub parameter_span: Span,
}

/// Immutable result of static capability precommit validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapabilityTransferPlan<'a> {
    callee: &'a str,
    caller_owner: CapabilityOwnerId,
    call_span: Span,
    items: Vec<CapabilityTransferItem<'a>>,
}

impl<'a> CapabilityTransferPlan<'a> {
    pub const fn callee(&self) -> &'a str { self.callee }
    pub const fn caller_owner(&self) -> CapabilityOwnerId { self.caller_owner }
    pub const fn call_span(&self) -> Span { self.call_span }
    pub fn items(&self) -> &[CapabilityTransferItem<'a>] { &self.items }
    pub fn instances(&self) -> Vec<CapabilityInstanceId> {
        self.items.iter().map(|item| item.instance).collect()
    }
}

/// Static failures are returned to the verifier for diagnostic rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityPrecommitError<'a> {
    MissingCapabilityArgument {
        parameter: &'a str,
        parameter_span: Span,
        call_span: Span,
    },
    MoveRequired {
        parameter: &'a str,
        required_kind: CapabilityId,
        parameter_span: Span,
        argument_span: Span,
    },
    MoveTargetMustBeIdentifier { argument_span: Span },
    UnboundSource {
        source_name: &'a str,
        argument_span: Span,
    },
    WrongOwner {
        source_name: &'a str,
        expected: CapabilityOwnerId,
        actual: CapabilityOwnerId,
        argument_span: Span,
    },
    Unavailable {
        source_name: &'a str,
        state: CapabilityState,
        argument_span: Span,
    },
    KindMismatch {
        source_name: &'a str,
        actual: CapabilityId,
        required: CapabilityId,
        argument_span: Span,
    },
    DuplicateInstance {
        source_name: &'a str,
        first_argument_span: Span,
        duplicate_argument_span: Span,
    },
    Ledger(CapabilityError),
}

/// Validate positional capability arguments without changing ledger or bindings.
///
/// The caller supplies capability parameters in declaration order. Value
/// parameters are omitted from this slice but retain their original positions
/// through each spec's `parameter_index`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IndexedCapabilityParameter<'a> {
    pub parameter_index: usize,
    pub spec: CapabilityParameterSpec<'a>,
}

pub fn build_static_transfer_plan<'a>(
    callee: &'a str,
    caller_owner: CapabilityOwnerId,
    call_span: Span,
    arguments: &[Expression<'a>],
    parameters: &[IndexedCapabilityParameter<'a>],
    environment: &CapabilityEnvironment<'a>,
    ledger: &CapabilityLedger,
) -> Result<CapabilityTransferPlan<'a>, Vec<CapabilityPrecommitError<'a>>> {
    let mut errors = Vec::new();
    let mut items = Vec::new();
    let mut seen = std::collections::BTreeMap::<CapabilityInstanceId, Span>::new();

    for indexed in parameters {
        let spec = indexed.spec;
        let Some(argument) = arguments.get(indexed.parameter_index) else {
            errors.push(CapabilityPrecommitError::MissingCapabilityArgument {
                parameter: spec.name,
                parameter_span: spec.declaration_span,
                call_span,
            });
            continue;
        };

        let (target, move_span) = match argument {
            Expression::Move { value, span } => match value.as_ref() {
                Expression::Identifier(name, _) => (*name, *span),
                _ => {
                    errors.push(CapabilityPrecommitError::MoveTargetMustBeIdentifier {
                        argument_span: *span,
                    });
                    continue;
                }
            },
            other => {
                errors.push(CapabilityPrecommitError::MoveRequired {
                    parameter: spec.name,
                    required_kind: spec.kind,
                    parameter_span: spec.declaration_span,
                    argument_span: expression_span(other),
                });
                continue;
            }
        };

        let Some(binding) = environment.lookup(target) else {
            errors.push(CapabilityPrecommitError::UnboundSource {
                source_name: target,
                argument_span: move_span,
            });
            continue;
        };

        if binding.owner() != caller_owner || binding.owner() != environment.active_owner() {
            errors.push(CapabilityPrecommitError::WrongOwner {
                source_name: target,
                expected: caller_owner,
                actual: binding.owner(),
                argument_span: move_span,
            });
            continue;
        }

        let instance = match ledger.instance(binding.instance()) {
            Ok(instance) => instance,
            Err(error) => {
                errors.push(CapabilityPrecommitError::Ledger(error));
                continue;
            }
        };

        if instance.owner() != caller_owner {
            errors.push(CapabilityPrecommitError::WrongOwner {
                source_name: target,
                expected: caller_owner,
                actual: instance.owner(),
                argument_span: move_span,
            });
            continue;
        }
        if instance.state() != CapabilityState::Available {
            errors.push(CapabilityPrecommitError::Unavailable {
                source_name: target,
                state: instance.state(),
                argument_span: move_span,
            });
            continue;
        }
        if instance.kind() != spec.kind {
            errors.push(CapabilityPrecommitError::KindMismatch {
                source_name: target,
                actual: instance.kind(),
                required: spec.kind,
                argument_span: move_span,
            });
            continue;
        }

        if let Some(first_argument_span) = seen.insert(binding.instance(), move_span) {
            errors.push(CapabilityPrecommitError::DuplicateInstance {
                source_name: target,
                first_argument_span,
                duplicate_argument_span: move_span,
            });
            continue;
        }

        items.push(CapabilityTransferItem {
            source_name: target,
            instance: binding.instance(),
            source_owner: caller_owner,
            parameter_name: spec.name,
            required_kind: spec.kind,
            move_span,
            parameter_span: spec.declaration_span,
        });
    }

    if errors.is_empty() {
        Ok(CapabilityTransferPlan {
            callee,
            caller_owner,
            call_span,
            items,
        })
    } else {
        Err(errors)
    }
}

fn expression_span(expression: &Expression<'_>) -> Span {
    match expression {
        Expression::Identifier(_, span)
        | Expression::Literal(_, span)
        | Expression::Move { span, .. }
        | Expression::Unary { span, .. }
        | Expression::Binary { span, .. }
        | Expression::Call { span, .. }
        | Expression::Symbolic { span, .. }
        | Expression::SelfModify { span, .. } => *span,
        Expression::Block(block) => block.span,
    }
}
