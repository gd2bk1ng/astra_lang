use syntra_lang::{
    validate_program_invariants, Declaration, DiagnosticEngine, Expression, Lexer, Parser,
    SemanticReport, SemanticVerifier, Statement,
};

fn verify(source: &str) -> SemanticReport<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");
    let validated =
        validate_program_invariants(program, source).expect("parsed fixture satisfies invariants");

    match SemanticVerifier::new(source).verify(validated) {
        Ok(verified) => verified.report().clone(),
        Err(diagnostics) => SemanticReport { diagnostics },
    }
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
fn unbound_confidence_metadata_does_not_leak_into_unrelated_identifiers() {
    let source = concat!(
        "intent mission {\n",
        "  uncertainty { confidence 0.95 }\n",
        "  goal deploy {\n",
        "    condition control_gate(signal)\n",
        "  }\n",
        "}\n",
    );

    let report = verify(source);

    assert!(
        report.is_success(),
        "unassociated confidence metadata must not become verifier-global state: {:?}",
        report.diagnostics
    );
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

    let validated =
        validate_program_invariants(program, source).expect("parsed fixture satisfies invariants");
    let report = SemanticVerifier::new(source)
        .verify(validated)
        .expect("valid fixture should produce a verified program");
    assert!(report.report().is_success());
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
        "  let result = signal;\n",
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
        "  let result = signal;\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "nested scope must not collide with or leak into its parent: {:?}",
        report.diagnostics
    );
}

#[test]
fn deeply_nested_backtrack_shadowing_restores_each_outer_binding() {
    let source = concat!(
        "fn sample() {\n",
        "  let signal = 1;\n",
        "  backtrack {\n",
        "    let signal = signal;\n",
        "    backtrack { let signal = signal; }\n",
        "    let inner_result = signal;\n",
        "  }\n",
        "  let result = signal;\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "nested bindings must resolve without colliding or leaking: {:?}",
        report.diagnostics
    );
}

#[test]
fn unresolved_identifiers_remain_compatible_with_existing_semantics() {
    let source = "fn sample() { return unresolved_signal; }";
    let report = verify(source);

    assert!(
        report.is_success(),
        "identifier lookup must not introduce unresolved-name errors: {:?}",
        report.diagnostics
    );
}

#[test]
fn type_compatibility_rejects_non_boolean_logical_operands() {
    let source = "fn sample() { return true && 1; }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2301")
        .expect("logical operand mismatch must be diagnosed");

    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "true && 1");
    assert!(diagnostic.message.contains("Boolean"));
    assert!(diagnostic.message.contains("Integer"));
}

#[test]
fn type_compatibility_rejects_non_numeric_relational_operands() {
    let source = "fn sample() { return 1 < \"text\"; }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "relational comparison with a string must be rejected: {:?}",
        report.diagnostics
    );
}

#[test]
fn type_compatibility_allows_numeric_promotion_and_cross_type_equality() {
    let source = concat!(
        "fn sample() {\n",
        "  let sum = 1 + 2.5;\n",
        "  let equal = 1 == 1.0;\n",
        "  let ordered = 1.0 >= 1;\n",
        "  let logical = true || false;\n",
        "  let string_equal = \"left\" == \"right\";\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "numeric promotion and cross-type equality should pass: {:?}",
        report.diagnostics
    );
}

#[test]
fn type_compatibility_suppresses_cascades_for_unknown_operands() {
    let source = "fn sample() { return unresolved && 1; }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "unknown operands must suppress compatibility cascades: {:?}",
        report.diagnostics
    );
}

#[test]
fn type_compatibility_checks_unary_not_and_negation() {
    let source = concat!(
        "fn sample() {\n",
        "  let invalid_not = !1;\n",
        "  let invalid_negate = -true;\n",
        "}\n",
    );
    let report = verify(source);

    assert_eq!(
        report.diagnostics.iter().filter(|diagnostic| diagnostic.code == "SYN2301").count(),
        2
    );
}

#[test]
fn type_compatibility_rejects_incompatible_arithmetic_and_equality() {
    let source = concat!(
        "fn sample() {\n",
        "  let invalid_sum = 1 + \"text\";\n",
        "  let invalid_equal = true == \"true\";\n",
        "}\n",
    );
    let report = verify(source);

    assert_eq!(
        report.diagnostics.iter().filter(|diagnostic| diagnostic.code == "SYN2301").count(),
        2,
        "each known incompatible operation should produce one diagnostic: {:?}",
        report.diagnostics
    );
}

#[test]
fn multiline_binary_type_diagnostic_preserves_expression_origin() {
    let source = concat!(
        "fn sample() {\n",
        "  return true &&\n",
        "    1;\n",
        "}\n",
    );
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2301")
        .expect("multiline logical mismatch must be diagnosed");

    assert_eq!(diagnostic.span.line, 2);
    assert_eq!(diagnostic.span.column, 10);
    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("true &&\n    1")
    );
    assert!(source.is_char_boundary(diagnostic.span.start));
    assert!(source.is_char_boundary(diagnostic.span.end));
}

#[test]
fn multiline_unary_type_diagnostic_preserves_operator_origin() {
    let source = concat!(
        "fn sample() {\n",
        "  return\n",
        "    -true;\n",
        "}\n",
    );
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2301")
        .expect("invalid unary negation must be diagnosed");

    assert_eq!(diagnostic.span.line, 3);
    assert_eq!(diagnostic.span.column, 5);
    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("-true")
    );
}

#[test]
fn multiline_call_expression_span_preserves_callee_origin() {
    let source = concat!(
        "fn sample() {\n",
        "  return\n",
        "    inspect(\n",
        "      1\n",
        "    );\n",
        "}\n",
    );
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function declaration");
    };
    let Statement::Return {
        value: Expression::Call { span, .. },
        ..
    } = &function.body.statements[0]
    else {
        panic!("expected a call expression in the return statement");
    };

    assert_eq!(span.line, 3);
    assert_eq!(span.column, 5);
    assert_eq!(
        source.get(span.start..span.end),
        Some("inspect(\n      1\n    )")
    );
}

#[test]
fn unicode_before_expression_keeps_character_column_and_utf8_safe_offsets() {
    let source = "fn sample() { let café = 1; return true && 1; }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2301")
        .expect("logical mismatch must be diagnosed");

    let line_prefix = source
        .get(..diagnostic.span.start)
        .expect("diagnostic start must be a UTF-8 boundary");
    let expected_column = line_prefix.chars().count() + 1;

    assert_eq!(diagnostic.span.line, 1);
    assert_eq!(diagnostic.span.column, expected_column);
    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("true && 1")
    );
    assert!(source.is_char_boundary(diagnostic.span.end));
}

#[test]
fn nested_call_diagnostics_are_unique_stable_and_source_bounded() {
    let source = concat!(
        "fn sample() {\n",
        "  return inspect(\n",
        "    true && 1\n",
        "  );\n",
        "}\n",
    );
    let first = verify(source);
    let second = verify(source);

    assert_eq!(first.diagnostics, second.diagnostics);
    let mismatches: Vec<_> = first
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2301")
        .collect();
    assert_eq!(mismatches.len(), 1, "nested operator should be diagnosed once");

    let span = mismatches[0].span;
    assert!(span.start <= span.end);
    assert!(span.end <= source.len());
    assert!(source.is_char_boundary(span.start));
    assert!(source.is_char_boundary(span.end));
    assert_eq!(span.line, 3);
    assert_eq!(span.column, 5);
    assert_eq!(source.get(span.start..span.end), Some("true && 1"));
}


#[test]
fn intent_owned_blocks_validate_operator_compatibility() {
    let source = concat!(
        "intent mission {\n",
        "  workspace {\n",
        "    let invalid_logical = true && 1;\n",
        "    return -true;\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    let mismatches: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2301")
        .collect();
    assert_eq!(
        mismatches.len(),
        2,
        "each incompatible operator inside an intent block must be checked once: {:?}",
        report.diagnostics
    );
    assert!(mismatches.iter().any(|diagnostic| {
        source.get(diagnostic.span.start..diagnostic.span.end) == Some("true && 1")
    }));
    assert!(mismatches.iter().any(|diagnostic| {
        source.get(diagnostic.span.start..diagnostic.span.end) == Some("-true")
    }));
}

#[test]
fn nested_expression_blocks_inside_intent_blocks_are_verified_once() {
    let source = concat!(
        "intent mission {\n",
        "  workspace {\n",
        "    let result = { let invalid = true && 1; };\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    let mismatches: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2301")
        .collect();
    assert_eq!(
        mismatches.len(),
        1,
        "nested expression block must be traversed exactly once: {:?}",
        report.diagnostics
    );
    assert_eq!(
        source.get(mismatches[0].span.start..mismatches[0].span.end),
        Some("true && 1")
    );
}

#[test]
fn intent_owned_blocks_isolate_bindings_and_preserve_nested_shadowing() {
    let source = concat!(
        "intent mission {\n",
        "  workspace {\n",
        "    let signal = 1;\n",
        "    backtrack { let signal = true; }\n",
        "    let result = signal;\n",
        "  }\n",
        "  another_workspace { let signal = 2; }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "intent blocks must isolate sibling bindings and preserve nested shadowing: {:?}",
        report.diagnostics
    );
}

#[test]
fn duplicate_bindings_inside_intent_owned_blocks_are_rejected() {
    let source = concat!(
        "intent mission {\n",
        "  workspace {\n",
        "    let signal = 1;\n",
        "    let signal = 2;\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    let duplicates: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2003")
        .collect();
    assert_eq!(
        duplicates.len(),
        1,
        "duplicate local binding inside intent block must be diagnosed once: {:?}",
        report.diagnostics
    );
    assert_eq!(
        source.get(duplicates[0].span.start..duplicates[0].span.end),
        Some("let signal = 2;")
    );
}

#[test]
fn intent_owned_blocks_inherit_declared_effects() {
    let source = concat!(
        "intent mission {\n",
        "  effects { DispatchNetworkCommand }\n",
        "  workspace { dispatch_network_command(); }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "intent-owned blocks must inherit the enclosing intent's declared effects: {:?}",
        report.diagnostics
    );
}


#[test]
fn callable_registry_resolves_forward_function_references_and_return_types() {
    let source = concat!(
        "fn caller() {\n",
        "  let result = later(1);\n",
        "  let sum = result + 1.0;\n",
        "}\n",
        "fn later(value: Integer) -> Float { return 2.5; }\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "forward callable resolution and return-type propagation should succeed: {:?}",
        report.diagnostics
    );
}

#[test]
fn callable_registry_resolves_rules_as_callables() {
    let source = concat!(
        "fn caller() { let result = calculate(2); let sum = result + 1; }\n",
        "rule calculate(value: Integer) -> Integer { return value; }\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "rule declarations should participate in callable resolution: {:?}",
        report.diagnostics
    );
}

#[test]
fn callable_registry_rejects_duplicate_function_and_rule_names() {
    let source = concat!(
        "fn shared(value: Integer) { }\n",
        "rule shared(value: Float) { }\n",
    );
    let report = verify(source);
    let duplicates: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2401")
        .collect();

    assert_eq!(duplicates.len(), 1, "cross-kind duplicate must be reported once");
    assert_eq!(
        source.get(duplicates[0].span.start..duplicates[0].span.end),
        Some("rule shared(value: Float) { }")
    );
    assert!(duplicates[0].message.contains("shared"));
}

#[test]
fn callable_arity_mismatch_reports_the_call_expression_span() {
    let source = concat!(
        "fn caller() { target(1); }\n",
        "fn target(value: Integer, enabled: Boolean) { }\n",
    );
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2402")
        .expect("call with missing argument must be diagnosed");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("target(1)")
    );
    assert!(diagnostic.message.contains("1 argument"));
    assert!(diagnostic.message.contains("requires 2"));
}

#[test]
fn callable_argument_type_mismatch_highlights_the_argument() {
    let source = concat!(
        "fn caller() { target(\"wrong\"); }\n",
        "fn target(value: Integer) { }\n",
    );
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2403")
        .expect("incompatible call argument must be diagnosed");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("\"wrong\"")
    );
    assert!(diagnostic.message.contains("value"));
    assert!(diagnostic.message.contains("Integer"));
    assert!(diagnostic.message.contains("String"));
}

#[test]
fn callable_argument_types_allow_existing_numeric_compatibility() {
    let source = concat!(
        "fn caller() { accept_integer(1.5); accept_float(1); }\n",
        "fn accept_integer(value: Float) { }\n",
        "fn accept_float(value: Float) { }\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "numeric argument compatibility should permit Integer/Float interchange: {:?}",
        report.diagnostics
    );
}

#[test]
fn callable_validation_suppresses_unknown_argument_type_cascades() {
    let source = concat!(
        "fn caller() { accept(unresolved_value); }\n",
        "fn accept(value: CustomRecord) { }\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "unknown argument and user-defined parameter types must remain conservative: {:?}",
        report.diagnostics
    );
}

#[test]
fn callable_return_type_unknown_when_omitted_or_user_defined() {
    let source = concat!(
        "fn caller() {\n",
        "  let omitted = no_return_type();\n",
        "  let custom = custom_return_type();\n",
        "  let safe = omitted + 1;\n",
        "  let also_safe = custom + 1;\n",
        "}\n",
        "fn no_return_type() { }\n",
        "fn custom_return_type() -> CustomRecord { }\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "omitted and unsupported return types should remain Unknown without cascades: {:?}",
        report.diagnostics
    );
}


#[test]
fn callable_arity_mismatch_rejects_extra_arguments() {
    let source = concat!(
        "fn caller() { target(1, true); }\n",
        "fn target(value: Integer) { }\n",
    );
    let report = verify(source);
    let diagnostics: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2402")
        .collect();

    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        source.get(diagnostics[0].span.start..diagnostics[0].span.end),
        Some("target(1, true)")
    );
    assert!(diagnostics[0].message.contains("2 argument"));
    assert!(diagnostics[0].message.contains("requires 1"));
}

#[test]
fn return_contract_rejects_function_value_type_mismatch() {
    let source = "fn calculate() -> Integer { return \"wrong\"; }";
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2501")
        .expect("function return type mismatch must be diagnosed");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("\"wrong\"")
    );
    assert!(diagnostic.message.contains("String"));
    assert!(diagnostic.message.contains("Integer"));
}

#[test]
fn return_contract_rejects_rule_value_type_mismatch() {
    let source = "rule calculate() -> Boolean { return 1; }";
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2501")
        .expect("rule return type mismatch must be diagnosed");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("1")
    );
    assert!(diagnostic.message.contains("Integer"));
    assert!(diagnostic.message.contains("Boolean"));
}

#[test]
fn return_contract_allows_existing_numeric_compatibility() {
    let source = concat!(
        "fn calculate() -> Float {\n",
        "  return 1;\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "Integer should be compatible with a declared Float return type: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_contract_suppresses_unknown_type_cascades() {
    let source = "fn calculate() -> Integer { return unresolved_signal; }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2501"),
        "unknown return types must suppress speculative mismatch diagnostics: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_contract_is_inherited_by_nested_backtrack_blocks() {
    let source = concat!(
        "fn calculate() -> Integer {\n",
        "  backtrack {\n",
        "    return \"wrong\";\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    let diagnostics: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2501")
        .collect();

    assert_eq!(
        diagnostics.len(),
        1,
        "nested backtrack return must be checked exactly once: {:?}",
        report.diagnostics
    );
    assert_eq!(
        source.get(diagnostics[0].span.start..diagnostics[0].span.end),
        Some("\"wrong\"")
    );
}

#[test]
fn return_contract_is_inherited_by_nested_expression_blocks() {
    let source = concat!(
        "fn calculate() -> Integer {\n",
        "  return {\n",
        "    return \"wrong\";\n",
        "  };\n",
        "}\n",
    );
    let report = verify(source);

    let diagnostics: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2501")
        .collect();

    assert_eq!(
        diagnostics.len(),
        1,
        "nested expression-block return must be checked exactly once: {:?}",
        report.diagnostics
    );
    assert_eq!(
        source.get(diagnostics[0].span.start..diagnostics[0].span.end),
        Some("\"wrong\"")
    );
}

#[test]
fn return_path_analysis_accepts_guaranteed_function_return() {
    let source = "fn compute() -> Int { return 42; }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "a concrete callable with an explicit return must not be reported as falling through: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_path_analysis_rejects_function_fallthrough_for_concrete_types() {
    let source = "fn compute() -> Int { let x = 1; }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2502")
        .expect("concrete non-Unit function that falls through must be diagnosed");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("{ let x = 1; }")
    );
}

#[test]
fn return_path_analysis_rejects_rule_fallthrough_for_concrete_types() {
    let source = "rule compute() -> String { let x = 1; }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "concrete non-Unit rules must satisfy the return-path contract: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_path_analysis_allows_unit_omitted_and_unknown_return_types() {
    let sources = [
        "fn log_msg() -> Unit { let x = 1; }",
        "fn log_msg() { let x = 1; }",
        "fn log_msg() -> CustomRecord { let x = 1; }",
    ];

    for source in sources {
        let report = verify(source);
        assert!(
            !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
            "Unit, omitted, and unsupported return types must not require an explicit return: {source}: {:?}",
            report.diagnostics
        );
    }
}

#[test]
fn return_path_analysis_propagates_nonlocal_return_through_nested_expression_blocks() {
    let source = "fn compute() -> Int { let x = { return 10; }; }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "a return inside a nested expression block exits the callable: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_path_analysis_treats_backtrack_conservatively_as_fallthrough() {
    let source = "fn compute() -> Int { backtrack { return 0; } }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "a return inside Backtrack cannot guarantee that the callable returns: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_path_analysis_rejects_empty_concrete_callable_body() {
    let source = "fn compute() -> Boolean { }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "an empty concrete non-Unit callable must be diagnosed"
    );
}

#[test]
fn return_path_analysis_accepts_return_after_sequential_statements() {
    let source = "fn compute() -> Int { let value = 1; value; return value; }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "sequential statements preceding a return must not imply fallthrough: {:?}",
        report.diagnostics
    );
}

#[test]
fn return_path_analysis_rejects_fallthrough_from_nested_expression_block() {
    let source = "fn compute() -> Int { let value = { let inner = 1; }; }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "a nested expression block that falls through does not satisfy the callable return contract: {:?}",
        report.diagnostics
    );
}

