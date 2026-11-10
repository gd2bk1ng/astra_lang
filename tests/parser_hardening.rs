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

    assert_eq!(report.errors.len(), 2);
    assert_eq!(report.program.declarations.len(), 0);

    let diagnostics = DiagnosticEngine::parse_errors(source, report.errors);
    assert_eq!(diagnostics.len(), 2);
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
