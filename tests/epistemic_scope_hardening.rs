use syntra_lang::{
    EpistemicBinding, EpistemicEnvironment, EpistemicState, EpistemicTransitionContract,
    EpistemicTransitionError, EpistemicType, Span,
};

fn binding(name: &'static str, state: EpistemicState) -> EpistemicBinding<'static> {
    EpistemicBinding::new(name, "Signal", state, Span::new(0, 6))
}

fn observed_to_resolved() -> EpistemicTransitionContract {
    EpistemicTransitionContract::new(
        [EpistemicState::Observed],
        [EpistemicState::Resolved],
    )
}

#[test]
fn default_environment_starts_with_a_usable_root_scope() {
    let mut environment = EpistemicEnvironment::default();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("default environment has a root scope");
    assert!(environment.lookup("signal").is_some());
}

#[test]
fn basic_binding_and_contract_checked_state_tracking() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("initial declaration succeeds");

    assert_eq!(
        environment.lookup("signal").expect("binding exists").epistemic_state,
        EpistemicState::Observed
    );
    assert_eq!(
        environment.lookup("signal").expect("binding exists").ty,
        EpistemicType::new("Signal", EpistemicState::Observed)
    );

    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("contract installation succeeds");
    environment
        .transition_state("signal", EpistemicState::Resolved)
        .expect("contract permits transition");

    let signal = environment.lookup("signal").expect("binding exists");
    assert_eq!(signal.epistemic_state, EpistemicState::Resolved);
    assert_eq!(signal.ty.state, EpistemicState::Resolved);
}

#[test]
fn lexical_scope_nesting_and_shadowing_are_isolated() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("outer declaration succeeds");

    environment.enter_scope();
    environment
        .declare("signal", binding("signal", EpistemicState::Uncertain))
        .expect("inner shadowing declaration succeeds");

    assert_eq!(
        environment.lookup("signal").expect("inner binding exists").epistemic_state,
        EpistemicState::Uncertain
    );

    environment.exit_scope();

    assert_eq!(
        environment.lookup("signal").expect("outer binding is restored").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn scope_exit_discards_inner_bindings_and_local_contract_refinements() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("outer declaration succeeds");

    environment.enter_scope();
    environment
        .declare("local", binding("local", EpistemicState::Uncertain))
        .expect("local declaration succeeds");
    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("inner contract installation succeeds");
    environment
        .transition_state("signal", EpistemicState::Resolved)
        .expect("inner transition succeeds");

    assert_eq!(
        environment.lookup("signal").expect("refined binding exists").epistemic_state,
        EpistemicState::Resolved
    );
    assert!(environment.lookup("local").is_some());

    environment.exit_scope();

    assert_eq!(
        environment.lookup("signal").expect("outer binding survives").epistemic_state,
        EpistemicState::Observed
    );
    assert!(environment.lookup("local").is_none());
}

#[test]
fn nested_scope_transition_cannot_leak_into_parent() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("belief", binding("belief", EpistemicState::Uncertain))
        .expect("outer declaration succeeds");

    environment.enter_scope();
    environment
        .set_transition_contract(
            "belief",
            EpistemicTransitionContract::new(
                [EpistemicState::Uncertain],
                [EpistemicState::Known],
            ),
        )
        .expect("inner contract installation succeeds");
    environment
        .transition_state("belief", EpistemicState::Known)
        .expect("inner transition succeeds");

    assert_eq!(
        environment.lookup("belief").expect("refined binding exists").epistemic_state,
        EpistemicState::Known
    );

    environment.exit_scope();

    assert_eq!(
        environment.lookup("belief").expect("parent binding survives").epistemic_state,
        EpistemicState::Uncertain
    );
}

#[test]
fn inherited_binding_cannot_be_transitioned_without_a_local_shadow_or_contract() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("outer declaration succeeds");
    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("outer contract installation succeeds");

    environment.enter_scope();
    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::ScopeMutationRequiresLocalBinding)
    );
    assert_eq!(
        environment.lookup("signal").expect("inherited binding remains visible").epistemic_state,
        EpistemicState::Observed
    );
    environment.exit_scope();

    assert_eq!(
        environment.lookup("signal").expect("parent binding remains").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn local_contract_installation_creates_an_isolated_shadow_for_transition() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("outer declaration succeeds");
    environment.enter_scope();

    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("local contract installation explicitly creates a local overlay");
    environment
        .transition_state("signal", EpistemicState::Resolved)
        .expect("transition is allowed on the local overlay");

    assert_eq!(
        environment.lookup("signal").expect("local overlay is visible").epistemic_state,
        EpistemicState::Resolved
    );
    environment.exit_scope();
    assert_eq!(
        environment.lookup("signal").expect("parent binding is restored").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn explicit_local_shadow_declaration_allows_only_local_transition() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("outer declaration succeeds");
    environment.enter_scope();
    environment
        .declare("signal", binding("signal", EpistemicState::Uncertain))
        .expect("explicit local shadow succeeds");

    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::ContractNotDefined)
    );
    assert_eq!(
        environment.lookup("signal").expect("local shadow remains").epistemic_state,
        EpistemicState::Uncertain
    );
    environment.exit_scope();
    assert_eq!(
        environment.lookup("signal").expect("parent binding remains").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn transition_without_contract_is_rejected_without_mutation() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("declaration succeeds");

    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::ContractNotDefined)
    );
    assert_eq!(
        environment.lookup("signal").expect("binding remains").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn source_state_not_admitted_by_contract_is_rejected() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Uncertain))
        .expect("declaration succeeds");
    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("contract installation succeeds");

    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::SourceStateNotAllowed)
    );
    assert_eq!(
        environment.lookup("signal").expect("binding remains").epistemic_state,
        EpistemicState::Uncertain
    );
}

#[test]
fn target_state_not_admitted_by_contract_is_rejected() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("declaration succeeds");
    environment
        .set_transition_contract(
            "signal",
            EpistemicTransitionContract::new(
                [EpistemicState::Observed],
                [EpistemicState::Known],
            ),
        )
        .expect("contract installation succeeds");

    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::TargetStateNotAllowed)
    );
    assert_eq!(
        environment.lookup("signal").expect("binding remains").epistemic_state,
        EpistemicState::Observed
    );
}

#[test]
fn compatibility_update_state_api_cannot_bypass_contracts() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("declaration succeeds");

    assert_eq!(
        environment.update_state("signal", EpistemicState::Known),
        Err(EpistemicTransitionError::ContractNotDefined)
    );

    environment
        .set_transition_contract("signal", observed_to_resolved())
        .expect("contract installation succeeds");
    assert_eq!(
        environment.update_state("signal", EpistemicState::Resolved),
        Ok(())
    );
}

#[test]
fn missing_bindings_and_duplicate_current_scope_declarations_are_rejected() {
    let mut environment = EpistemicEnvironment::new();

    assert!(environment.lookup("missing").is_none());
    assert_eq!(
        environment.update_state("missing", EpistemicState::Known),
        Err(EpistemicTransitionError::BindingNotFound)
    );

    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("initial declaration succeeds");

    assert!(environment
        .declare("signal", binding("signal", EpistemicState::Known))
        .is_err());

    environment.enter_scope();
    environment
        .declare("signal", binding("signal", EpistemicState::Known))
        .expect("shadowing in a distinct scope is allowed");
}
