use syntra_lang::{
    EpistemicBinding, EpistemicEnvironment, EpistemicState, EpistemicType, Span,
};

fn binding(name: &'static str, state: EpistemicState) -> EpistemicBinding<'static> {
    EpistemicBinding::new(name, "Signal", state, Span { start: 0, end: 6 })
}

#[test]
fn basic_binding_and_state_tracking() {
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
        .update_state("signal", EpistemicState::Resolved)
        .expect("state refinement succeeds");

    assert_eq!(
        environment.lookup("signal").expect("binding exists").epistemic_state,
        EpistemicState::Resolved
    );
    assert_eq!(
        environment.lookup("signal").expect("binding exists").ty.state,
        EpistemicState::Resolved
    );
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
fn scope_exit_discards_inner_bindings_and_local_refinements() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("signal", binding("signal", EpistemicState::Uncertain))
        .expect("outer declaration succeeds");

    environment.enter_scope();
    environment
        .declare("local", binding("local", EpistemicState::Observed))
        .expect("local declaration succeeds");

    environment
        .update_state("signal", EpistemicState::Resolved)
        .expect("local refinement succeeds");

    assert_eq!(
        environment.lookup("signal").expect("refined binding exists").epistemic_state,
        EpistemicState::Resolved
    );
    assert!(environment.lookup("local").is_some());

    environment.exit_scope();

    assert_eq!(
        environment.lookup("signal").expect("outer binding survives").epistemic_state,
        EpistemicState::Uncertain
    );
    assert!(environment.lookup("local").is_none());
}

#[test]
fn nested_scope_modification_cannot_leak_into_parent() {
    let mut environment = EpistemicEnvironment::new();

    environment
        .declare("belief", binding("belief", EpistemicState::Uncertain))
        .expect("outer declaration succeeds");

    environment.enter_scope();
    environment
        .update_state("belief", EpistemicState::Known)
        .expect("inner refinement succeeds");

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
fn missing_bindings_and_duplicate_current_scope_declarations_are_rejected() {
    let mut environment = EpistemicEnvironment::new();

    assert!(environment.lookup("missing").is_none());
    assert!(environment.update_state("missing", EpistemicState::Known).is_err());

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
