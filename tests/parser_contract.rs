use syntra_lang::{Lexer, Parser};

#[test]
fn parser_builds_borrowed_function_ast() {
    let source = String::from(
        "fn add(x: Int<64>, y: Int<64>) -> Int<64> { return x + y; }",
    );
    let tokens = Lexer::new(&source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    assert_eq!(program.declarations.len(), 1);

    let syntra_lang::Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function declaration");
    };

    assert_eq!(function.name, "add");
    assert_eq!(function.params.len(), 2);
    assert_eq!(function.params[0].name, "x");
    assert_eq!(function.params[0].ty.name, "Int");
    assert_eq!(function.params[0].ty.arguments[0].name, "64");

    assert!(std::ptr::eq(function.name.as_ptr(), source["fn ".len()..].as_ptr()));

    let syntra_lang::Statement::Return { value, .. } = &function.body.statements[0] else {
        panic!("expected return statement");
    };

    assert!(matches!(value, syntra_lang::Expression::Binary { .. }));
}

#[test]
fn parser_preserves_intent_member_source() {
    let source = String::from(
        r#"intent syntra { motive "reason" action "act" }"#,
    );
    let tokens = Lexer::new(&source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let syntra_lang::Declaration::Intent(intent) = &program.declarations[0] else {
        panic!("expected intent declaration");
    };

    assert_eq!(intent.name, "syntra");
    assert_eq!(intent.members.len(), 2);
    assert_eq!(intent.members[0].name, "motive");

    let syntra_lang::IntentValue::String(value) = intent.members[0].value else {
        panic!("expected string intent member");
    };

    assert_eq!(value, "reason");
    assert!(std::ptr::eq(value.as_ptr(), source["intent syntra { motive "".len()..].as_ptr()));
}

#[test]
fn parser_cursor_never_owns_token_storage() {
    let source = String::from("fn f() {}");
    let tokens = Lexer::new(&source).tokenize().expect("lexing succeeds");
    let cursor = syntra_lang::TokenCursor::new(&tokens);

    assert_eq!(cursor.position(), 0);
    assert_eq!(cursor.current().expect("first token").lexeme, "fn");
    assert_eq!(cursor.peek(1).expect("second token").lexeme, "f");
    assert_eq!(cursor.remaining().len(), tokens.len());
}
