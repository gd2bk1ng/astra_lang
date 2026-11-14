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


#[test]
fn root_scope_exit_is_a_no_op_and_environment_remains_usable() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("root", binding("root", EpistemicState::Observed))
        .expect("root declaration succeeds");

    for _ in 0..8 {
        environment.exit_scope();
    }

    assert_eq!(
        environment.lookup("root").expect("root survives exit attempts").epistemic_state,
        EpistemicState::Observed
    );
    environment
        .declare("still_root", binding("still_root", EpistemicState::Known))
        .expect("root remains usable");
}

#[test]
fn deeply_nested_shadowing_restores_each_parent_in_reverse_order() {
    const DEPTH: usize = 128;
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("root declaration succeeds");

    let states = [
        EpistemicState::Observed,
        EpistemicState::Inferred,
        EpistemicState::Assumed,
        EpistemicState::Uncertain,
        EpistemicState::Resolved,
        EpistemicState::Known,
    ];

    for depth in 0..DEPTH {
        environment.enter_scope();
        let state = states[depth % states.len()];
        environment
            .declare("signal", binding("signal", state))
            .expect("each nested scope may shadow its parent");
        assert_eq!(
            environment.lookup("signal").expect("innermost binding exists").epistemic_state,
            state
        );
    }

    for depth in (0..DEPTH).rev() {
        environment.exit_scope();
        let expected = if depth == 0 {
            EpistemicState::Observed
        } else {
            states[(depth - 1) % states.len()]
        };
        assert_eq!(
            environment.lookup("signal").expect("parent binding is restored").epistemic_state,
            expected
        );
    }
}

#[test]
fn repeated_scope_reentry_does_not_retain_stale_local_contracts() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("root declaration succeeds");

    for _ in 0..64 {
        environment.enter_scope();
        environment
            .set_transition_contract("signal", observed_to_resolved())
            .expect("scope-local contract installs");
        environment
            .transition_state("signal", EpistemicState::Resolved)
            .expect("local transition succeeds");
        environment.exit_scope();

        let parent = environment.lookup("signal").expect("parent binding remains");
        assert_eq!(parent.epistemic_state, EpistemicState::Observed);
        assert_eq!(parent.ty.state, EpistemicState::Observed);
        assert!(parent.contract.transition.is_none());
    }
}

#[test]
fn nested_contract_replacement_restores_outer_scope_contract_after_exit() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("belief", binding("belief", EpistemicState::Uncertain))
        .expect("root declaration succeeds");

    let outer_contract = EpistemicTransitionContract::new(
        [EpistemicState::Uncertain],
        [EpistemicState::Known],
    );
    environment.enter_scope();
    environment
        .set_transition_contract("belief", outer_contract)
        .expect("outer local contract installs");
    environment
        .transition_state("belief", EpistemicState::Known)
        .expect("outer local transition succeeds");

    environment.enter_scope();
    environment
        .set_transition_contract(
            "belief",
            EpistemicTransitionContract::new(
                [EpistemicState::Known],
                [EpistemicState::Resolved],
            ),
        )
        .expect("nested contract replaces only the nested overlay");
    environment
        .transition_state("belief", EpistemicState::Resolved)
        .expect("nested transition succeeds");
    assert_eq!(
        environment.lookup("belief").expect("nested overlay is visible").epistemic_state,
        EpistemicState::Resolved
    );

    environment.exit_scope();
    let restored_outer = environment
        .lookup("belief")
        .expect("outer overlay is restored");
    assert_eq!(restored_outer.epistemic_state, EpistemicState::Known);
    assert_eq!(restored_outer.ty.state, EpistemicState::Known);
    assert_eq!(
        restored_outer.contract.transition,
        Some(EpistemicTransitionContract::new(
            [EpistemicState::Uncertain],
            [EpistemicState::Known],
        )),
        "the outer scope's original transition contract must be restored"
    );

    // The restored contract is intentionally not reusable from Known:
    // its only admitted source is Uncertain. This proves the nested
    // replacement did not overwrite the outer contract.
    assert_eq!(
        environment.transition_state("belief", EpistemicState::Resolved),
        Err(EpistemicTransitionError::SourceStateNotAllowed)
    );
    assert_eq!(
        environment.lookup("belief").expect("outer overlay remains").epistemic_state,
        EpistemicState::Known
    );

    environment.exit_scope();
    assert_eq!(
        environment.lookup("belief").expect("root binding is restored").epistemic_state,
        EpistemicState::Uncertain
    );
    assert!(environment
        .lookup("belief")
        .expect("root binding remains")
        .contract
        .transition
        .is_none());
}

#[test]
fn empty_transition_contract_rejects_every_transition_without_state_drift() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("declaration succeeds");
    environment
        .set_transition_contract(
            "signal",
            EpistemicTransitionContract::new([], []),
        )
        .expect("empty contract is representable");

    assert_eq!(
        environment.transition_state("signal", EpistemicState::Resolved),
        Err(EpistemicTransitionError::SourceStateNotAllowed)
    );
    let current = environment.lookup("signal").expect("binding remains");
    assert_eq!(current.epistemic_state, EpistemicState::Observed);
    assert_eq!(current.ty.state, EpistemicState::Observed);
}

#[test]
fn duplicate_contract_entries_do_not_change_transition_semantics() {
    let mut environment = EpistemicEnvironment::new();
    environment
        .declare("signal", binding("signal", EpistemicState::Observed))
        .expect("declaration succeeds");
    environment
        .set_transition_contract(
            "signal",
            EpistemicTransitionContract::new(
                [
                    EpistemicState::Observed,
                    EpistemicState::Observed,
                ],
                [
                    EpistemicState::Resolved,
                    EpistemicState::Resolved,
                ],
            ),
        )
        .expect("duplicate entries are accepted as redundant metadata");
    environment
        .transition_state("signal", EpistemicState::Resolved)
        .expect("duplicate allow-list entries still permit the transition");

    let current = environment.lookup("signal").expect("binding remains");
    assert_eq!(current.epistemic_state, EpistemicState::Resolved);
    assert_eq!(current.ty.state, EpistemicState::Resolved);
}

#[test]
fn rejected_transitions_never_diverge_binding_and_type_states() {
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
        .expect("contract installs");

    for attempted_state in [
        EpistemicState::Resolved,
        EpistemicState::Inferred,
        EpistemicState::Assumed,
        EpistemicState::Uncertain,
    ] {
        assert!(environment.transition_state("signal", attempted_state).is_err());
        let current = environment.lookup("signal").expect("binding remains");
        assert_eq!(current.epistemic_state, current.ty.state);
        assert_eq!(current.epistemic_state, EpistemicState::Observed);
    }
}
