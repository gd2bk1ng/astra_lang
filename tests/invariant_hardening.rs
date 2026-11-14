use syntra_lang::{
    validate_program_invariants, AstInvariantKind, Block, Declaration, Function, Lexer, Literal,
    Program, Span, Statement,
};

fn parse(source: &str) -> Program<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = syntra_lang::Parser::new(&tokens);
    parser.parse_program().expect("parsing succeeds")
}

fn kinds(report: &syntra_lang::AstInvariantReport<'_>) -> Vec<AstInvariantKind> {
    report.errors.iter().map(|error| error.kind).collect()
}

#[test]
fn valid_ast_passes_invariant_validation() {
    let source = concat!(
        "fn f(value: Map<String, Vec<Int<64>>>) -> Result {\n",
        "  let x = 1 + 2 * 3;\n",
        "  backtrack { return x; }\n",
        "  return x;\n",
        "}\n",
        "intent mission {\n",
        "  goal deploy {\n",
        "    description \"controlled deployment\"\n",
        "    priority 1\n",
        "    condition assert_confidence(signal)\n",
        "    target dispatch_network_command(resolve(signal))\n",
        "  }\n",
        "  effects { DispatchNetworkCommand }\n",
        "  uncertainty { confidence 0.95; assumption \"telemetry\" }\n",
        "  governance { requires operator; constraint risk <= 0.8 }\n",
        "}\n",
    );

    let program = parse(source);
    assert!(validate_program_invariants(&program, source).is_ok());
}

#[test]
fn catches_inverted_and_out_of_bounds_spans() {
    let source = "fn f() { return 1; }";
    let program = Program {
        declarations: vec![],
        span: Span::new(20, 10),
    };

    let report = validate_program_invariants(&program, source).expect_err("invalid span must fail");
    assert_eq!(report.errors.len(), 1);
    assert_eq!(report.errors[0].kind, AstInvariantKind::InvalidSpanBounds);

    let program = Program {
        declarations: vec![],
        span: Span::new(0, source.len() + 1),
    };

    let report = validate_program_invariants(&program, source).expect_err("out-of-bounds span must fail");
    assert_eq!(report.errors.len(), 1);
    assert_eq!(report.errors[0].kind, AstInvariantKind::InvalidSpanBounds);
}

#[test]
fn catches_utf8_mid_character_spans() {
    let source = "é";
    let program = Program {
        declarations: vec![],
        span: Span::new(0, 1),
    };

    let report = validate_program_invariants(&program, source).expect_err("mid-character span must fail");
    assert_eq!(report.errors.len(), 1);
    assert_eq!(report.errors[0].kind, AstInvariantKind::Utf8BoundaryViolation);
}

#[test]
fn catches_child_containment_violation() {
    let source = "fn f() { return 1; }";
    let body = Block {
        statements: vec![],
        span: Span::new(9, 20),
    };
    let function = Function {
        name: "f",
        params: vec![],
        return_type: None,
        body,
        span: Span::new(0, 8),
        grad: false,
    };
    let program = Program {
        declarations: vec![Declaration::Function(function)],
        span: Span::new(0, source.len()),
    };

    let report = validate_program_invariants(&program, source).expect_err("child outside parent must fail");
    assert!(kinds(&report).contains(&AstInvariantKind::ContainmentViolation));
}

#[test]
fn catches_overlapping_declarations() {
    let source = "fn a() {} fn b() {}";
    let first = Function {
        name: "a",
        params: vec![],
        return_type: None,
        body: Block { statements: vec![], span: Span::new(8, 10) },
        span: Span::new(0, 10),
        grad: false,
    };
    let second = Function {
        name: "b",
        params: vec![],
        return_type: None,
        body: Block { statements: vec![], span: Span::new(18, 20) },
        span: Span::new(9, 20),
        grad: false,
    };
    let program = Program {
        declarations: vec![Declaration::Function(first), Declaration::Function(second)],
        span: Span::new(0, source.len()),
    };

    let report = validate_program_invariants(&program, source).expect_err("overlap must fail");
    assert!(kinds(&report).contains(&AstInvariantKind::SiblingOrderViolation));
}

#[test]
fn catches_empty_non_empty_construct_span() {
    let source = "fn f() {}";
    let function = Function {
        name: "f",
        params: vec![],
        return_type: None,
        body: Block { statements: vec![], span: Span::new(7, 9) },
        span: Span::new(0, 0),
        grad: false,
    };
    let program = Program {
        declarations: vec![Declaration::Function(function)],
        span: Span::new(0, source.len()),
    };

    let report = validate_program_invariants(&program, source).expect_err("empty function span must fail");
    assert!(kinds(&report).contains(&AstInvariantKind::EmptyNodeSpan));
}

#[test]
fn catches_nested_statement_ordering() {
    let source = "fn f() { return 1; return 2; }";
    let statements = vec![
        Statement::Return { value: syntra_lang::Expression::Literal(Literal::Integer(1), Span::new(17, 18)), span: Span::new(10, 19) },
        Statement::Return { value: syntra_lang::Expression::Literal(Literal::Integer(2), Span::new(27, 28)), span: Span::new(18, 29) },
    ];
    let function = Function {
        name: "f",
        params: vec![],
        return_type: None,
        body: Block { statements, span: Span::new(8, 31) },
        span: Span::new(0, source.len()),
        grad: false,
    };
    let program = Program { declarations: vec![Declaration::Function(function)], span: Span::new(0, source.len()) };

    let report = validate_program_invariants(&program, source).expect_err("overlapping statements must fail");
    assert!(kinds(&report).contains(&AstInvariantKind::SiblingOrderViolation));
}
