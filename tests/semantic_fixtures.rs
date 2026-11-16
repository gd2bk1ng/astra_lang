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
fn unknown_effects_are_rejected_at_the_identifier_span() {
    let source = "intent x { effects { Network } }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2104")
        .expect("unknown effect must be diagnosed");

    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "Network");
    assert_eq!(diagnostic.message, "unknown effect 'Network'");
}

#[test]
fn repeated_unknown_effects_each_report_unknown_not_duplicate() {
    let source = "intent x { effects { Network, Network } }";
    let report = verify(source);

    assert_eq!(
        report
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.code == "SYN2104")
            .count(),
        2
    );
    assert!(
        !report
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "SYN2101"),
        "unresolved names must not enter the canonical duplicate set: {:?}",
        report.diagnostics
    );
}

#[test]
fn duplicate_effects_are_detected_by_canonical_identity() {
    let source = "intent x { effects { WriteFile, WriteFile } }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2101")
        .expect("duplicate known effect must be diagnosed");

    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "WriteFile");
    assert_eq!(
        diagnostic.message,
        "effect 'WriteFile' is declared more than once"
    );
}

#[test]
fn effect_names_are_case_sensitive() {
    let source = "intent x { effects { writefile } }";
    let report = verify(source);
    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2104")
        .expect("incorrectly cased effect must be unknown");

    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "writefile");
}

#[test]
fn known_effect_declaration_remains_position_independent() {
    let source = concat!(
        "intent x {\n",
        "  workspace { read_file(); }\n",
        "  effects { ReadFile }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        !report
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code == "SYN2102"),
        "all intent effects must be collected before checking consumers: {:?}",
        report.diagnostics
    );
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
    let source = "intent x { effects { ReadFile } }";
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

#[test]
fn return_path_analysis_does_not_assume_short_circuit_and_rhs_executes() {
    let sources = [
        "fn compute() -> Boolean { let result = false && { return true; }; }",
        "fn compute() -> Boolean { let result = true || { return false; }; }",
    ];

    for source in sources {
        let report = verify(source);
        assert!(
            report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
            "a return nested in a logical RHS is not guaranteed to execute: {source}: {:?}",
            report.diagnostics
        );
    }
}

#[test]
fn invalid_initializer_exposes_unknown_to_downstream_callable_validation() {
    let source = concat!(
        "fn consume(value: String) { }\n",
        "fn caller() {\n",
        "  let invalid = true && 1;\n",
        "  consume(invalid);\n",
        "}\n",
    );
    let report = verify(source);

    assert_eq!(
        report.diagnostics.iter().filter(|diagnostic| diagnostic.code == "SYN2301").count(),
        1,
        "the invalid logical expression should retain its primary operator diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2403"),
        "an invalid initializer must not cause a downstream callable argument cascade: {:?}",
        report.diagnostics
    );
}

#[test]
fn genuinely_unknown_initializer_remains_distinct_from_invalid_expression() {
    let source = concat!(
        "fn consume(value: String) { }\n",
        "fn caller() {\n",
        "  let unresolved = unknown_signal;\n",
        "  consume(unresolved);\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "SYN2301" || diagnostic.code == "SYN2403"
        }),
        "genuine unknown types must remain permissive and suppress speculative diagnostics: {:?}",
        report.diagnostics
    );
}

#[test]
fn lexical_value_binding_shadows_global_callable_and_reports_non_callable_callee() {
    let source = concat!(
        "fn calculate(value: Integer) -> Integer { return value; }\n",
        "fn caller() {\n",
        "  let calculate = 10;\n",
        "  calculate(2);\n",
        "}\n",
    );
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2404")
        .expect("a local value must shadow the global callable");
    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("calculate")
    );
    assert!(
        !report.diagnostics.iter().any(|diagnostic| {
            diagnostic.code == "SYN2402" || diagnostic.code == "SYN2403"
        }),
        "the shadowed global signature must not be used for call validation: {:?}",
        report.diagnostics
    );
}

#[test]
fn lexical_parameter_binding_shadows_global_callable() {
    let source = concat!(
        "fn calculate(value: Integer) -> Integer { return value; }\n",
        "fn caller(calculate: Integer) { calculate(); }\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2404"),
        "a parameter binding must take precedence over a global callable: {:?}",
        report.diagnostics
    );
}

#[test]
fn user_callable_declarations_cannot_redefine_reserved_builtins() {
    let source = concat!(
        "fn resolve() { }\n",
        "rule assert() { }\n",
        "fn dispatch_network_command() { }\n",
        "fn SelfModify() { }\n",
    );
    let report = verify(source);

    let reserved: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2405")
        .collect();
    assert_eq!(
        reserved.len(),
        4,
        "every user declaration colliding with a reserved built-in must be rejected: {:?}",
        report.diagnostics
    );
}

#[test]
fn local_value_can_shadow_builtin_name_but_cannot_be_called() {
    let source = "fn caller() { let resolve = 1; resolve(2); }";
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2404"),
        "lexical value precedence applies to built-in names as well: {:?}",
        report.diagnostics
    );
}

#[test]
fn invalid_goal_condition_does_not_reach_deterministic_gate() {
    let source = concat!(
        "intent mission {\n",
        "  uncertainty { confidence 0.95 }\n",
        "  goal deploy {\n",
        "    condition !signal + true\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "the invalid condition must retain its type diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2002"),
        "an invalid condition must not feed a poisoned confidence state into deterministic-gate enforcement: {:?}",
        report.diagnostics
    );
}

#[test]
fn invalid_goal_target_does_not_reach_deterministic_gate() {
    let source = concat!(
        "intent mission {\n",
        "  uncertainty { confidence 0.95 }\n",
        "  goal deploy {\n",
        "    target !signal + true\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "the invalid target must retain its type diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2002"),
        "an invalid target must not feed a poisoned confidence state into deterministic-gate enforcement: {:?}",
        report.diagnostics
    );
}

#[test]
fn invalid_governance_constraint_does_not_create_false_contradiction() {
    let source = concat!(
        "intent release {\n",
        "  governance {\n",
        "    constraint risk >= 0.9\n",
        "    constraint risk <= 0.8 + true\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "the invalid constraint must retain its type diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2201"),
        "an invalid constraint must not contribute poisoned bounds to the contradiction ledger: {:?}",
        report.diagnostics
    );
}

#[test]
fn invalid_goal_expression_still_reports_independent_effect_diagnostic() {
    let source = concat!(
        "intent mission {\n",
        "  goal deploy {\n",
        "    condition 1 + true + dispatch_network_command()\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2301"),
        "the expression must retain its independent type diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2102"),
        "static effect inspection must still report the undeclared operation in an invalid expression: {:?}",
        report.diagnostics
    );
}

#[test]
fn multiple_effect_blocks_aggregate_into_one_intent_contract() {
    let source = concat!(
        "intent mission {\n",
        "  effects { ReadFile }\n",
        "  effects { DispatchNetworkCommand }\n",
        "  workspace { read_file(); dispatch_network_command(); }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "all valid effect blocks must contribute to the unified intent contract: {:?}",
        report.diagnostics
    );
}

#[test]
fn nested_backtrack_and_expression_blocks_inherit_intent_effects() {
    let source = concat!(
        "intent mission {\n",
        "  effects { DispatchNetworkCommand }\n",
        "  workspace {\n",
        "    backtrack { dispatch_network_command(); }\n",
        "    { dispatch_network_command(); };\n",
        "  }\n",
        "}\n",
    );
    let report = verify(source);

    assert!(
        report.is_success(),
        "nested Backtrack and expression blocks must inherit the intent effect contract: {:?}",
        report.diagnostics
    );
}

#[test]
fn callable_bodies_do_not_inherit_ambient_intent_effects() {
    let source = concat!(
        "intent mission { effects { DispatchNetworkCommand } }\n",
        "fn isolated_function() { dispatch_network_command(); }\n",
        "rule isolated_rule() { dispatch_network_command(); }\n",
    );
    let report = verify(source);
    let missing_effects = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2102")
        .count();

    assert_eq!(
        missing_effects, 2,
        "both function and rule bodies must be isolated from intent effects: {:?}",
        report.diagnostics
    );
}


#[test]
fn callable_function_effect_clause_reports_unknown_and_duplicate_effects() {
    let source = "fn reader() effects { ReadFile, UnknownEffect, ReadFile, UnknownEffect } { }";
    let report = verify(source);
    let unknown: Vec<_> = report.diagnostics.iter().filter(|d| d.code == "SYN2104").collect();
    let duplicate: Vec<_> = report.diagnostics.iter().filter(|d| d.code == "SYN2101").collect();

    assert_eq!(unknown.len(), 2, "each unresolved item must be diagnosed: {:?}", report.diagnostics);
    assert_eq!(duplicate.len(), 1, "only repeated known identities are duplicates: {:?}", report.diagnostics);
    assert_eq!(&source[unknown[0].span.start..unknown[0].span.end], "UnknownEffect");
    assert_eq!(&source[duplicate[0].span.start..duplicate[0].span.end], "ReadFile");
    assert_eq!(duplicate[0].message, "effect 'ReadFile' is declared more than once");
}

#[test]
fn callable_rule_effect_clause_uses_same_resolution_contract() {
    let source = "rule sender() effects { DispatchNetworkCommand, DispatchNetworkCommand, dispatchnetworkcommand } { }";
    let report = verify(source);
    let unknown: Vec<_> = report.diagnostics.iter().filter(|d| d.code == "SYN2104").collect();
    let duplicate: Vec<_> = report.diagnostics.iter().filter(|d| d.code == "SYN2101").collect();

    assert_eq!(unknown.len(), 1, "case mismatch must be unknown: {:?}", report.diagnostics);
    assert_eq!(duplicate.len(), 1, "known canonical identity must be deduplicated: {:?}", report.diagnostics);
    assert_eq!(&source[unknown[0].span.start..unknown[0].span.end], "dispatchnetworkcommand");
    assert_eq!(&source[duplicate[0].span.start..duplicate[0].span.end], "DispatchNetworkCommand");
}

#[test]
fn callable_without_effect_clause_has_no_effect_declaration_diagnostics() {
    let source = "fn plain() { } rule also_plain() { }";
    let report = verify(source);
    assert!(
        !report.diagnostics.iter().any(|d| d.code == "SYN2101" || d.code == "SYN2104"),
        "omitted clauses must remain valid empty contracts: {:?}",
        report.diagnostics
    );
}

#[test]
fn duplicate_callable_declaration_effects_are_still_validated() {
    let source = concat!(
        "fn worker() effects { ReadFile } { }\n",
        "rule worker() effects { UnknownEffect, WriteFile } { }\n",
    );
    let report = verify(source);

    assert_eq!(
        report.diagnostics.iter().filter(|d| d.code == "SYN2104").count(),
        1,
        "effect clauses on rejected duplicate declarations must still be checked: {:?}",
        report.diagnostics
    );
    assert_eq!(
        report.diagnostics.iter().filter(|d| d.code == "SYN2401").count(),
        1
    );
}


#[test]
fn callable_body_activates_its_declared_effect_contract() {
    let source = "fn reader() effects { ReadFile } { read_file(); }";
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2102"),
        "a callable must be allowed to exercise its own declared effects: {:?}",
        report.diagnostics
    );
    assert!(report.is_success(), "unexpected diagnostics: {:?}", report.diagnostics);
}

#[test]
fn call_site_requires_callee_effect_contract_in_caller_scope() {
    let source = concat!(
        "fn reader() effects { ReadFile } { read_file(); }\n",
        "fn caller() { reader(); }\n",
    );
    let report = verify(source);

    let diagnostic = report
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code == "SYN2105")
        .expect("missing callable effect permission diagnostic");

    assert_eq!(
        source.get(diagnostic.span.start..diagnostic.span.end),
        Some("reader()")
    );
    assert_eq!(
        diagnostic.message,
        "call to 'reader' requires effect 'ReadFile', but that effect is not declared in the current semantic scope"
    );
}

#[test]
fn call_site_accepts_callee_effects_declared_by_caller() {
    let source = concat!(
        "fn reader() effects { ReadFile } { read_file(); }\n",
        "fn caller() effects { ReadFile } { reader(); }\n",
    );
    let report = verify(source);

    assert!(
        !report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2105"),
        "caller permissions must satisfy the callee contract: {:?}",
        report.diagnostics
    );
}

#[test]
fn call_site_reports_missing_effects_in_deterministic_canonical_order() {
    let source = concat!(
        "fn worker() effects { WriteFile, ReadFile } { }\n",
        "fn caller() { worker(); }\n",
    );
    let report = verify(source);

    let diagnostics: Vec<_> = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2105")
        .collect();

    assert_eq!(
        diagnostics.len(),
        2,
        "each missing callable permission must be reported: {:?}",
        report.diagnostics
    );
    assert_eq!(
        diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect::<Vec<_>>(),
        vec![
            "call to 'worker' requires effect 'WriteFile', but that effect is not declared in the current semantic scope",
            "call to 'worker' requires effect 'ReadFile', but that effect is not declared in the current semantic scope",
        ]
    );
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| source.get(diagnostic.span.start..diagnostic.span.end) == Some("worker()")),
        "every SYN2105 must span the complete call expression: {:?}",
        diagnostics
    );
}

#[test]
fn call_site_effect_diagnostic_is_independent_of_argument_validation() {
    let source = concat!(
        "fn worker(value: Integer) effects { ReadFile } { }\n",
        "fn caller() { worker(\"wrong\"); }\n",
    );
    let report = verify(source);

    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2403"),
        "the invalid argument must retain its type diagnostic: {:?}",
        report.diagnostics
    );
    assert!(
        report.diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2105"),
        "missing callable effect permission must remain independently reportable: {:?}",
        report.diagnostics
    );

    let first_relevant = report
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == "SYN2403" || diagnostic.code == "SYN2105")
        .map(|diagnostic| diagnostic.code)
        .collect::<Vec<_>>();
    assert_eq!(first_relevant, vec!["SYN2403", "SYN2105"]);
}


#[test]
fn capability_parameters_are_canonicalized_and_not_value_bindings() {
    let source = concat!("fn worker(authority: Cap<WriteFile>) { }\n", "fn caller() { worker(move authority); }\n");
    let report = verify(source);
    assert!(!report.diagnostics.iter().any(|d| d.code == "SYN2117"), "known kind must resolve: {:?}", report.diagnostics);
    assert!(!report.diagnostics.iter().any(|d| d.code == "SYN2110"), "explicit move accepted: {:?}", report.diagnostics);
    assert!(!report.diagnostics.iter().any(|d| d.code == "SYN2404"), "capability param is not an ordinary value binding: {:?}", report.diagnostics);
}

#[test]
fn unknown_capability_parameter_kind_is_rejected_without_value_fallback() {
    let source = "fn worker(authority: Cap<UnknownAuthority>) { }";
    let report = verify(source);
    let diagnostic = report.diagnostics.iter().find(|d| d.code == "SYN2117").expect("unknown kind rejected");
    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "Cap<UnknownAuthority>");
}

#[test]
fn capability_call_argument_requires_explicit_move_syntax() {
    let source = concat!("fn worker(authority: Cap<WriteFile>) { }\n", "fn caller() { worker(authority); }\n");
    let report = verify(source);
    let diagnostic = report.diagnostics.iter().find(|d| d.code == "SYN2110").expect("explicit move required");
    assert_eq!(&source[diagnostic.span.start..diagnostic.span.end], "authority");
}

#[test]
fn capability_move_syntax_does_not_mutate_authority_ledger() {
    let source = concat!("fn worker(authority: Cap<WriteFile>) { }\n", "fn caller() { worker(move authority); }\n");
    let report = verify(source);
    assert!(!report.diagnostics.iter().any(|d| ["SYN2112","SYN2113","SYN2115","SYN2118","SYN2119"].contains(&d.code)),
        "ownership lookup and mutation are deferred: {:?}", report.diagnostics);
}
