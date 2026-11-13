use syntra_lang::{
    BinaryOp, Declaration, DiagnosticEngine, Expression, Lexer, Literal, Parser, Statement,
};

fn parse(source: &str) -> syntra_lang::Program<'_> {
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    parser.parse_program().expect("parsing succeeds")
}

#[test]
fn expression_precedence_is_structural() {
    let program = parse("fn f() { return 1 + 2 * 3; }");
    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function");
    };

    let Statement::Return { value, .. } = &function.body.statements[0] else {
        panic!("expected return");
    };

    let Expression::Binary { left, operator, right, .. } = value else {
        panic!("expected binary expression");
    };

    assert_eq!(*operator, BinaryOp::Add);
    assert!(matches!(**left, Expression::Literal(Literal::Integer(1), _)));

    let Expression::Binary { operator, .. } = &**right else {
        panic!("expected multiplication on the right");
    };
    assert_eq!(*operator, BinaryOp::Multiply);
}

#[test]
fn nested_blocks_preserve_tree_structure() {
    let program = parse(
        "fn f() { let x = { let y = 1; return y; }; return x; }",
    );
    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function");
    };

    let Statement::Let { value: Some(Expression::Block(block)), .. } =
        &function.body.statements[0]
    else {
        panic!("expected block-valued let");
    };

    assert_eq!(block.statements.len(), 2);
    assert!(matches!(block.statements[0], Statement::Let { .. }));
    assert!(matches!(block.statements[1], Statement::Return { .. }));
}

#[test]
fn malformed_control_flow_reports_without_panicking() {
    let source = "fn bad() { return; backtrack; }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let report = parser.parse_program_recovering();

    assert_eq!(report.errors.len(), 3);
    assert_eq!(report.program.declarations.len(), 0);

    let diagnostics = DiagnosticEngine::parse_errors(source, report.errors);
    assert_eq!(diagnostics.len(), 4);
    assert!(diagnostics[0].render().contains("return;"));
    assert!(diagnostics[1].render().contains("backtrack"));
}

#[test]
fn recovery_resynchronizes_at_statement_and_block_boundaries() {
    let source = concat!(
        "fn broken() { return 1 }\n",
        "fn good() { return 2; }\n",
    );
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let report = parser.parse_program_recovering();

    assert_eq!(report.errors.len(), 1);
    assert_eq!(report.program.declarations.len(), 1);

    let Declaration::Function(function) = &report.program.declarations[0] else {
        panic!("expected recovered function");
    };
    assert_eq!(function.name, "good");
}

#[test]
fn invalid_expression_reports_exact_token_span() {
    let source = "fn f() { return 1 + ; }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let error = parser.parse_program().expect_err("input must be rejected");
    let diagnostic = DiagnosticEngine::parse_error(source, error);

    assert_eq!(diagnostic.span.line, 1);
    assert_eq!(diagnostic.span.column, 21);
    assert_eq!(source.get(diagnostic.span.start..diagnostic.span.end), Some(";"));
    assert!(diagnostic.render().contains("bytes 20..21"));
}

#[test]
fn deeply_nested_expression_preserves_precedence_structure() {
    let program = parse(
        "fn f() { return 1 + 2 * 3 - 4 / 5 + 6 == 7 && true; }",
    );
    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function");
    };
    let Statement::Return { value, .. } = &function.body.statements[0] else {
        panic!("expected return");
    };

    let Expression::Binary { operator, left, right, .. } = value else {
        panic!("expected top-level binary expression");
    };
    assert_eq!(*operator, BinaryOp::And);

    let Expression::Binary { operator, .. } = &**left else {
        panic!("expected equality expression on left");
    };
    assert_eq!(*operator, BinaryOp::Equal);
    assert!(matches!(**right, Expression::Literal(Literal::Boolean(true), _)));
}

#[test]
fn ambiguous_operator_chaining_follows_precedence_levels() {
    let program = parse("fn f() { return 1 + 2 < 4 && 3 * 2 == 6; }");
    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function");
    };
    let Statement::Return { value, .. } = &function.body.statements[0] else {
        panic!("expected return");
    };

    let Expression::Binary { operator, left, right, .. } = value else {
        panic!("expected binary expression");
    };
    assert_eq!(*operator, BinaryOp::And);

    let Expression::Binary { operator, .. } = &**left else {
        panic!("expected comparison on left");
    };
    assert_eq!(*operator, BinaryOp::Less);

    let Expression::Binary { operator, .. } = &**right else {
        panic!("expected equality on right");
    };
    assert_eq!(*operator, BinaryOp::Equal);
}

#[test]
fn nested_generic_type_arguments_preserve_borrowed_names() {
    let program = parse(
        "fn f(value: Map<String, Vec<Int<64>>>) { return value; }",
    );
    let Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function");
    };

    let ty = &function.params[0].ty;
    assert_eq!(ty.name, "Map");
    assert_eq!(ty.arguments.len(), 2);
    assert_eq!(ty.arguments[0].name, "String");
    assert_eq!(ty.arguments[1].name, "Vec");
    assert_eq!(ty.arguments[1].arguments[0].name, "Int");
    assert_eq!(ty.arguments[1].arguments[0].arguments[0].name, "64");
}

#[test]
fn recovering_parser_collects_distinct_errors_across_multiple_declarations() {
    let source = concat!(
        "fn first() { return ; }\n",
        "fn second() { return ; }\n",
        "fn third() { return ; }\n",
        "fn good() { return 4; }\n",
    );
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let report = parser.parse_program_recovering();

    assert_eq!(report.errors.len(), 6);
    assert_eq!(report.program.declarations.len(), 1);

    let diagnostics = DiagnosticEngine::parse_errors(source, report.errors);
    assert_eq!(diagnostics.len(), 6);
    assert!(diagnostics[0].span.start < diagnostics[1].span.start);
    assert!(diagnostics[1].span.start < diagnostics[2].span.start);
    assert!(diagnostics.iter().all(|diagnostic| {
        diagnostic.span.start <= diagnostic.span.end
            && diagnostic.span.end <= source.len()
    }));
}

#[test]
fn diagnostics_remain_source_bounded_for_multiline_recovery() {
    let source = concat!(
        "fn first() {\n",
        "  return ;\n",
        "}\n",
        "fn second() {\n",
        "  return 1 + ;\n",
        "}\n",
    );
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);

    let report = parser.parse_program_recovering();
    let diagnostics = DiagnosticEngine::parse_errors(source, report.errors);

    assert_eq!(diagnostics.len(), 4);
    assert_eq!(diagnostics[0].span.line, 2);
    assert_eq!(diagnostics[1].span.line, 5);
    assert!(diagnostics[0].span.end <= source.len());
    assert!(diagnostics[1].span.end <= source.len());
    assert!(diagnostics[0].render().contains("bytes"));
    assert!(diagnostics[1].render().contains("bytes"));
}
