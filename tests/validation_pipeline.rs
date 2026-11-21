use syntra_lang::{
    validate_program_invariants, AstInvariantKind, Lexer, Parser, SemanticVerifier, Span,
    ValidatedProgram, VerifiedProgram, Program,
};

fn parse(source: &str) -> Program<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    parser.parse_program().expect("parsing succeeds")
}

fn verify(source: &str) -> Result<VerifiedProgram<'_>, Vec<syntra_lang::Diagnostic<'_>>> {
    let program = parse(source);
    let validated =
        validate_program_invariants(program, source).expect("fixture satisfies AST invariants");
    SemanticVerifier::new(source).verify(validated)
}

#[test]
fn valid_program_crosses_validation_and_verification_boundaries() {
    let source = "fn compute() -> Int { return 42; }";
    let program = parse(source);
    let validated =
        validate_program_invariants(program, source).expect("valid program must validate");

    assert_eq!(validated.source(), source);
    assert_eq!(validated.program().declarations.len(), 1);

    let verified = SemanticVerifier::new(source)
        .verify(validated)
        .expect("valid program must verify");

    assert_eq!(verified.source(), source);
    assert_eq!(verified.program().declarations.len(), 1);
    assert!(verified.report().is_success());
    assert_eq!(
        verified.validated_program().source(),
        verified.source()
    );
}

#[test]
fn invalid_and_inverted_spans_are_rejected_before_verification() {
    let source = "fn f() {}";

    let invalid = Program {
        declarations: vec![],
        span: Span::new(0, source.len() + 1),
    };
    let report =
        validate_program_invariants(invalid, source).expect_err("out-of-bounds span must fail");
    assert!(report
        .errors
        .iter()
        .any(|error| error.kind == AstInvariantKind::InvalidSpanBounds));

    let inverted = Program {
        declarations: vec![],
        span: Span::new(5, 2),
    };
    let report =
        validate_program_invariants(inverted, source).expect_err("inverted span must fail");
    assert!(report
        .errors
        .iter()
        .any(|error| error.kind == AstInvariantKind::InvalidSpanBounds));
}

#[test]
fn utf8_split_spans_are_rejected_before_verification() {
    let source = "é";
    let program = Program {
        declarations: vec![],
        span: Span::new(0, 1),
    };

    let report =
        validate_program_invariants(program, source).expect_err("UTF-8 split span must fail");
    assert!(report
        .errors
        .iter()
        .any(|error| error.kind == AstInvariantKind::Utf8BoundaryViolation));
}

#[test]
fn semantic_failure_does_not_produce_a_verified_program() {
    let source = "fn compute() -> Int { return true; }";

    let result = verify(source);
    assert!(result.is_err(), "semantic failure must not produce VerifiedProgram");

    let diagnostics = result.expect_err("semantic verification must fail");
    assert!(diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2501"));
}

#[test]
fn short_circuit_rhs_return_does_not_guarantee_callable_return() {
    let source = "fn compute() -> Int { let value = true && { return 10; }; }";

    let diagnostics = verify(source).expect_err("short-circuit RHS return must not guarantee return");
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2502"),
        "expected missing-return diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn strict_equal_endpoint_governance_bounds_are_contradictory() {
    let source = concat!(
        "intent release {\n",
        "  governance {\n",
        "    constraint risk > 0.8\n",
        "    constraint risk <= 0.8\n",
        "  }\n",
        "}\n",
    );

    let diagnostics = verify(source).expect_err("strict/inclusive equal endpoints must conflict");
    assert!(
        diagnostics.iter().any(|diagnostic| diagnostic.code == "SYN2201"),
        "expected governance contradiction, got: {diagnostics:?}"
    );
}

#[test]
fn inclusive_equal_endpoint_governance_bounds_are_not_contradictory() {
    let source = concat!(
        "intent release {\n",
        "  governance {\n",
        "    constraint risk >= 0.8\n",
        "    constraint risk <= 0.8\n",
        "  }\n",
        "}\n",
    );

    let verified = verify(source).expect("inclusive equal endpoints describe a valid singleton range");
    assert!(verified.report().is_success());
}

#[test]
fn verified_program_has_read_only_boundary_accessors() {
    let source = "fn compute() { return 1; }";
    let verified = verify(source).expect("valid program must verify");

    let program: &syntra_lang::Program<'_> = verified.program();
    let validated: &ValidatedProgram<'_> = verified.validated_program();

    assert_eq!(program, validated.program());
    assert_eq!(validated.source(), source);
    assert!(verified.report().is_success());

    // The public API exposes only shared references to the AST. There is no
    // public constructor, mutable accessor, DerefMut, or AsMut implementation.
}
