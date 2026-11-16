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
    Ledger { error: CapabilityError, argument_span: Span },
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
                errors.push(CapabilityPrecommitError::Ledger {
                    error,
                    argument_span: move_span,
                });
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::verify::SemanticVerifier;

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    fn move_arg(name: &'static str, start: usize) -> Expression<'static> {
        Expression::Move {
            value: Box::new(Expression::Identifier(name, span(start + 5, start + 6))),
            span: span(start, start + 6),
        }
    }

    #[test]
    fn static_plan_resolves_existing_binding_without_mutating_ledger() {
        let mut verifier = SemanticVerifier::new("");
        let root = verifier.capability_ledger().root_owner();
        let instance = verifier.capability_ledger_mut()
            .provision(CapabilityId::ReadFile, root, span(0, 4)).unwrap();
        verifier.bind_root_capability("reader", instance, span(0, 6)).unwrap();

        let before = verifier.capability_ledger().instance(instance).unwrap().state();
        let arguments = vec![move_arg("reader", 20)];
        let parameters = [IndexedCapabilityParameter {
            parameter_index: 0,
            spec: CapabilityParameterSpec {
                name: "file_access",
                kind: CapabilityId::ReadFile,
                declaration_span: span(5, 25),
            },
        }];

        let plan = build_static_transfer_plan(
            "read_document", root, span(15, 30), &arguments, &parameters,
            verifier.capability_environment(), verifier.capability_ledger(),
        ).unwrap();

        assert_eq!(plan.callee(), "read_document");
        assert_eq!(plan.items().len(), 1);
        assert_eq!(plan.items()[0].instance, instance);
        assert_eq!(verifier.capability_ledger().instance(instance).unwrap().state(), before);
        assert_eq!(verifier.capability_ledger().instance(instance).unwrap().owner(), root);
    }

    #[test]
    fn static_plan_rejects_wrong_kind_and_duplicate_instance() {
        let mut verifier = SemanticVerifier::new("");
        let root = verifier.capability_ledger().root_owner();
        let instance = verifier.capability_ledger_mut()
            .provision(CapabilityId::ReadFile, root, span(0, 4)).unwrap();
        verifier.bind_root_capability("reader", instance, span(0, 6)).unwrap();

        let arguments = vec![move_arg("reader", 20), move_arg("reader", 40)];
        let parameters = [
            IndexedCapabilityParameter {
                parameter_index: 0,
                spec: CapabilityParameterSpec { name: "first", kind: CapabilityId::ReadFile, declaration_span: span(1, 8) },
            },
            IndexedCapabilityParameter {
                parameter_index: 1,
                spec: CapabilityParameterSpec { name: "second", kind: CapabilityId::ReadFile, declaration_span: span(9, 16) },
            },
        ];

        let errors = build_static_transfer_plan(
            "combine", root, span(15, 50), &arguments, &parameters,
            verifier.capability_environment(), verifier.capability_ledger(),
        ).unwrap_err();

        assert!(errors.iter().any(|error| matches!(error, CapabilityPrecommitError::DuplicateInstance { .. })));
        assert_eq!(verifier.capability_ledger().instance(instance).unwrap().state(), CapabilityState::Available);
    }

    #[test]
    fn static_plan_reports_missing_capability_parameter() {
        let verifier = SemanticVerifier::new("");
        let root = verifier.capability_ledger().root_owner();
        let parameters = [IndexedCapabilityParameter {
            parameter_index: 0,
            spec: CapabilityParameterSpec { name: "authority", kind: CapabilityId::SelfModify, declaration_span: span(4, 20) },
        }];

        let errors = build_static_transfer_plan(
            "mutate", root, span(0, 8), &[], &parameters,
            verifier.capability_environment(), verifier.capability_ledger(),
        ).unwrap_err();

        assert!(matches!(errors.as_slice(), [CapabilityPrecommitError::MissingCapabilityArgument { .. }]));
    }
}

#[cfg(test)]
mod call_site_integration_tests {
    use super::*;
    use crate::{Lexer, Parser, SemanticVerifier};

    fn parse(source: &str) -> (Vec<crate::Token<'_>>, crate::Program<'_>) {
        let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
        let program = Parser::new(&tokens).parse_program().expect("parsing succeeds");
        (tokens, program)
    }

    #[test]
    fn verifier_emits_immutable_plan_for_concrete_move_argument() {
        let source = concat!(
            "fn worker(file: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(move reader); } }",
        );
        let (_tokens, program) = parse(source);
        let mut ledger = CapabilityLedger::new();
        let root = ledger.root_owner();
        let instance = ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut verifier = SemanticVerifier::with_capability_ledger(source, ledger);
        verifier.bind_root_capability("reader", instance, Span::new(0, 0)).unwrap();

        let report = verifier.verify_program(&program);
        assert!(report.is_success(), "unexpected diagnostics: {:?}", report.diagnostics);
        assert_eq!(report.capability_transfer_plans().len(), 1);
        assert_eq!(report.capability_transfer_plans()[0].items()[0].instance, instance);
        // The report contains a plan, not an applied transfer. The direct
        // builder tests separately assert that ledger state remains unchanged.
    }

    #[test]
    fn omitted_capability_argument_reports_arity_and_capability_slot() {
        let source = concat!(
            "fn worker(value: Integer, file: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(1); } }",
        );
        let (_tokens, program) = parse(source);
        let report = SemanticVerifier::new(source).verify_program(&program);

        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2402"));
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2116"));
        assert!(report.capability_transfer_plans().is_empty());
    }

    #[test]
    fn callable_capability_parameters_are_symbolic_inside_callable_bodies() {
        let source = concat!(
            "fn nested(file: Cap<ReadFile>) { }\n",
            "fn outer(file: Cap<ReadFile>) { nested(move file); }",
        );
        let (_tokens, program) = parse(source);
        let report = SemanticVerifier::new(source).verify_program(&program);

        assert!(report.is_success(), "unexpected diagnostics: {:?}", report.diagnostics);
        assert!(report.capability_transfer_plans().is_empty());
    }

    #[test]
    fn denied_effect_prevents_static_transfer_plan_publication() {
        let source = concat!(
            "fn worker(file: Cap<ReadFile>) effects { ReadFile } { }\n",
            "intent task { workspace { worker(move reader); } }",
        );
        let (_tokens, program) = parse(source);
        let mut ledger = CapabilityLedger::new();
        let root = ledger.root_owner();
        let instance = ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut verifier = SemanticVerifier::with_capability_ledger(source, ledger);
        verifier.bind_root_capability("reader", instance, Span::new(0, 0)).unwrap();

        let report = verifier.verify_program(&program);
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2105"));
        assert!(report.capability_transfer_plans().is_empty());
    }
}

#[cfg(test)]
mod diagnostic_mapping_tests {
    use super::*;
    use crate::{Lexer, Parser, SemanticVerifier};

    fn program(source: &str) -> (Vec<crate::Token<'_>>, crate::Program<'_>) {
        let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
        let program = Parser::new(&tokens).parse_program().expect("parsing succeeds");
        (tokens, program)
    }

    fn report_with_instance<'a>(
        source: &'a str,
        instance_kind: CapabilityId,
        bind_name: &'static str,
    ) -> crate::SemanticReport<'a> {
        let (_tokens, ast) = program(source);
        let mut ledger = CapabilityLedger::new();
        let root = ledger.root_owner();
        let instance = ledger.provision(instance_kind, root, Span::new(0, 0)).unwrap();
        let mut verifier = SemanticVerifier::with_capability_ledger(source, ledger);
        verifier.bind_root_capability(bind_name, instance, Span::new(0, 0)).unwrap();
        verifier.verify_program(&ast)
    }

    #[test]
    fn call_site_maps_missing_move_and_kind_mismatch() {
        let missing_move_source = concat!(
            "fn worker(file: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(reader); } }",
        );
        let (_tokens, ast) = program(missing_move_source);
        let mut ledger = CapabilityLedger::new();
        let root = ledger.root_owner();
        let instance = ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut verifier = SemanticVerifier::with_capability_ledger(missing_move_source, ledger);
        verifier.bind_root_capability("reader", instance, Span::new(0, 0)).unwrap();
        let report = verifier.verify_program(&ast);
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2110"));

        let mismatch_source = concat!(
            "fn worker(file: Cap<WriteFile>) { }\n",
            "intent task { workspace { worker(move reader); } }",
        );
        let report = report_with_instance(
            mismatch_source,
            CapabilityId::ReadFile,
            "reader",
        );
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2111"));
    }

    #[test]
    fn call_site_maps_unbound_and_duplicate_moves() {
        let unbound_source = concat!(
            "fn worker(file: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(move missing); } }",
        );
        let (_tokens, ast) = program(unbound_source);
        let report = SemanticVerifier::new(unbound_source).verify_program(&ast);
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2112"));

        let duplicate_source = concat!(
            "fn worker(first: Cap<ReadFile>, second: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(move reader, move reader); } }",
        );
        let (_tokens, ast) = program(duplicate_source);
        let mut ledger = CapabilityLedger::new();
        let root = ledger.root_owner();
        let instance = ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut verifier = SemanticVerifier::with_capability_ledger(duplicate_source, ledger);
        verifier.bind_root_capability("reader", instance, Span::new(0, 0)).unwrap();
        let report = verifier.verify_program(&ast);
        assert!(report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2114"));
    }

    #[test]
    fn call_site_maps_consumed_and_in_transit_authority() {
        let source = concat!(
            "fn worker(file: Cap<ReadFile>) { }\n",
            "intent task { workspace { worker(move reader); } }",
        );
        let (_tokens, ast) = program(source);

        let mut consumed_ledger = CapabilityLedger::new();
        let root = consumed_ledger.root_owner();
        let consumed = consumed_ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut consumed_verifier = SemanticVerifier::with_capability_ledger(source, consumed_ledger);
        consumed_verifier.bind_root_capability("reader", consumed, Span::new(0, 0)).unwrap();
        consumed_verifier.capability_ledger_mut().enter_transit(root, consumed, 77).unwrap();
        consumed_verifier.capability_ledger_mut().consume(root, consumed, 77, Span::new(10, 20)).unwrap();
        let consumed_report = consumed_verifier.verify_program(&ast);
        assert!(consumed_report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2113"));

        let mut transit_ledger = CapabilityLedger::new();
        let root = transit_ledger.root_owner();
        let transit = transit_ledger.provision(CapabilityId::ReadFile, root, Span::new(0, 0)).unwrap();
        let mut transit_verifier = SemanticVerifier::with_capability_ledger(source, transit_ledger);
        transit_verifier.bind_root_capability("reader", transit, Span::new(0, 0)).unwrap();
        transit_verifier.capability_ledger_mut().enter_transit(root, transit, 78).unwrap();
        let transit_report = transit_verifier.verify_program(&ast);
        assert!(transit_report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2115"));
    }
}
