use syntra_lang::{
    Declaration, DiagnosticEngine, Expression, Lexer, Parser, SemanticVerifier, Statement,
};

fn verify(source: &str) -> syntra_lang::SemanticReport<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");
    SemanticVerifier::new(source).verify_program(&program)
}

#[test]
fn valid_epistemic_resolution_and_declared_effects_pass() {
    let source = concat!(
        "intent mission {\n",
        "  effects { DispatchNetworkCommand }\n",
        "  uncertainty {\n",
        "    confidence 0.95\n",
        "    assumption \"telemetry is calibrated\"\n",
        "    evidence assess(threshold(0.8))\n",
        "    source \"telemetry\"\n",
        "  }\n",
        "  goal deploy {\n",
        "    description \"controlled deployment\"\n",
        "    condition assert_confidence(signal)\n",
        "    target dispatch_network_command(resolve(signal))\n",
        "  }\n",
        "}\n",
    );

    let report = verify(source);

    assert!(
        report.is_success(),
        "unexpected semantic diagnostics: {:?}",
        report.diagnostics
    );
}



#[test]
fn epistemic_downgrade_into_deterministic_gate_is_rejected() {
    let source = concat!(
        "intent mission {\n",
        "  uncertainty { confidence 0.95 }\n",
        "  goal deploy {\n",
        "    condition control_gate(signal)\n",
        "  }\n",
        "}\n",
    );

    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2002")
        .expect("missing epistemic gate diagnostic");

    assert!(diagnostic.message.contains("confidence 0.950"));
    assert!(diagnostic.message.contains("control_gate"));
}

#[test]
fn capability_leakage_is_rejected_with_source_span() {
    let source = "fn unsafe_action() { dispatch_network_command(); }";
    let report = verify(source);

    assert!(!report.is_success());

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2102")
        .expect("missing effect diagnostic");

    assert_eq!(
        &source[diagnostic.span.start..diagnostic.span.end],
        "dispatch_network_command()"
    );
    assert!(diagnostic.render().contains("dispatch_network_command"));
    assert!(diagnostic.render().contains("bytes"));
}

#[test]
fn self_modification_requires_explicit_capability() {
    let source = "fn mutate() { modify(target, patch); }";
    let report = verify(source);

    assert!(!report.is_success());
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "SYN2103")
    );
}

#[test]
fn contradictory_governance_constraints_are_rejected() {
    let source = concat!(
        "intent release {\n",
        "  governance {\n",
        "    constraint risk >= 0.9\n",
        "    constraint risk <= 0.8\n",
        "  }\n",
        "}\n",
    );

    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2201")
        .expect("missing governance contradiction");

    assert_eq!(
        &source[diagnostic.span.start..diagnostic.span.end],
        "constraint risk <= 0.8"
    );
}

#[test]
fn invalid_confidence_is_rejected() {
    let source = "intent x { uncertainty { confidence 1.2 } }";
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2001")
        .expect("missing confidence diagnostic");

    assert!(diagnostic.message.contains("[0, 1]"));
    assert!(diagnostic.span.start < diagnostic.span.end);
}

#[test]
fn semantic_diagnostics_use_existing_engine_rendering() {
    let source = "fn x() { dispatch_network_command(); }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2102")
        .expect("missing semantic diagnostic");

    let rendered = DiagnosticEngine::semantic_error(
        source,
        diagnostic.code,
        diagnostic.message.clone(),
        diagnostic.span,
    )
    .render();

    assert!(rendered.contains("error[SYN2102]"));
    assert!(rendered.contains("bytes"));
}

#[test]
fn verifier_preserves_borrowed_ast_source_provenance() {
    let source = "intent x { effects { Network } }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let Declaration::Intent(intent) = &program.declarations[0] else {
        panic!("expected intent");
    };

    let report = SemanticVerifier::new(source).verify_program(&program);
    assert!(report.is_success());
    assert_eq!(&source[intent.span.start..intent.span.end], source);
}


#[test]
fn duplicate_local_bindings_are_rejected_with_the_declaration_span() {
    let source = "fn sample() { let signal = 1; let signal = 2; }";
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2003")
        .expect("duplicate local binding must be diagnosed");

    assert_eq!(
        &source[diagnostic.span.start..diagnostic.span.end],
        "let signal = 2;"
    );
    assert!(diagnostic.message.contains("signal"));
}

#[test]
fn uninitialized_local_bindings_are_registered_without_inventing_a_type() {
    let source = "fn sample() { let signal; return signal; }";
    let report = verify(source);

    assert!(
        report.is_success(),
        "unexpected semantic diagnostics: {:?}",
        report.diagnostics
    );
}

#[test]
fn parser_nests_backtrack_local_inside_backtrack_body() {
    let source = concat!(
        "fn sample() {\n",
        "  let signal = 1;\n",
        "  backtrack { let signal = signal; }\n",
        "  let signal = 2;\n",
        "}\n",
    );

    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function declaration");
    };

    assert_eq!(function.body.statements.len(), 3);

    let Statement::Backtrack { body, .. } = &function.body.statements[1] else {
        panic!("expected backtrack statement in the outer function body");
    };

    assert_eq!(body.statements.len(), 1);

    let Statement::Let {
        name,
        value: Some(value),
        ..
    } = &body.statements[0]
    else {
        panic!("expected inner let statement inside the backtrack body");
    };

    assert_eq!(*name, "signal");

    let Expression::Identifier(inner_name, _) = value else {
        panic!("expected inner initializer to be an identifier");
    };

    assert_eq!(*inner_name, "signal");
}


#[test]
fn nested_backtrack_scope_allows_local_shadowing() {
    let source = concat!(
        "fn sample() {\n",
        "  let signal = 1;\n",
        "  backtrack { let signal = signal; }\n",
        "  let signal = 2;\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "nested scope must not collide with or leak into its parent: {:?}",
        report.diagnostics
    );
}
