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
    let syntra_lang::ParameterKind::Value { ty } = &function.params[0].kind else { panic!("expected ordinary value parameter"); };
    assert_eq!(ty.name, "Int");
    assert_eq!(ty.arguments[0].name, "64");
    assert!(function.effects.is_none());

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
    assert!(std::ptr::eq(value.as_ptr(), source["intent syntra { motive \"".len()..].as_ptr()));
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


#[test]
fn parser_accepts_function_effects_before_return_type() {
    let source = "fn read_configuration(path: String) effects { ReadFile, WriteFile } -> String { return path; }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let syntra_lang::Declaration::Function(function) = &program.declarations[0] else {
        panic!("expected function declaration");
    };
    let effects = function.effects.as_ref().expect("function effects parsed");
    assert_eq!(effects.items.iter().map(|item| item.name).collect::<Vec<_>>(), ["ReadFile", "WriteFile"]);
    assert_eq!(function.return_type.as_ref().expect("return type parsed").name, "String");
    assert_eq!(&source[effects.span.start..effects.span.end], "effects { ReadFile, WriteFile }");
}

#[test]
fn parser_accepts_rule_effects_without_return_type() {
    let source = "rule synchronize() effects { DispatchNetworkCommand } { dispatch_network_command(); }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let syntra_lang::Declaration::Rule(rule) = &program.declarations[0] else {
        panic!("expected rule declaration");
    };
    let effects = rule.effects.as_ref().expect("rule effects parsed");
    assert_eq!(effects.items.len(), 1);
    assert_eq!(effects.items[0].name, "DispatchNetworkCommand");
    assert!(rule.return_type.is_none());
    assert_eq!(&source[effects.span.start..effects.span.end], "effects { DispatchNetworkCommand }");
}

#[test]
fn parser_preserves_callable_without_effect_clause() {
    let source = "rule plain() {}";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");

    let syntra_lang::Declaration::Rule(rule) = &program.declarations[0] else {
        panic!("expected rule declaration");
    };
    assert!(rule.effects.is_none());
}


#[test]
fn parser_preserves_capability_parameter_syntax_and_order() {
    let source = "fn update(path: String, file: Cap<WriteFile>, reader: Cap<ReadFile>) { }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");
    let syntra_lang::Declaration::Function(function) = &program.declarations[0] else { panic!("expected function"); };
    assert_eq!(function.params.len(), 3);
    assert!(matches!(&function.params[0].kind, syntra_lang::ParameterKind::Value { .. }));
    let syntra_lang::ParameterKind::Capability { capability } = &function.params[1].kind else { panic!("expected capability parameter"); };
    assert_eq!(capability.capability_name, "WriteFile");
    assert_eq!(&source[capability.span.start..capability.span.end], "Cap<WriteFile>");
    let syntra_lang::ParameterKind::Capability { capability } = &function.params[2].kind else { panic!("expected capability parameter"); };
    assert_eq!(capability.capability_name, "ReadFile");
    assert!(function.params[0].span.end <= function.params[1].span.start);
    assert!(function.params[1].span.end <= function.params[2].span.start);
}

#[test]
fn parser_accepts_capability_parameters_on_rules() {
    let source = "rule synchronize(network: Cap<DispatchNetworkCommand>) { }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");
    let syntra_lang::Declaration::Rule(rule) = &program.declarations[0] else { panic!("expected rule"); };
    assert!(matches!(&rule.params[0].kind, syntra_lang::ParameterKind::Capability { .. }));
}

#[test]
fn parser_preserves_explicit_move_as_direct_identifier() {
    let source = "fn caller() { worker(move authority); }";
    let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("parsing succeeds");
    let syntra_lang::Declaration::Function(function) = &program.declarations[0] else { panic!("expected function"); };
    let syntra_lang::Statement::Expression { expression, .. } = &function.body.statements[0] else { panic!("expected expression statement"); };
    let syntra_lang::Expression::Call { arguments, .. } = expression else { panic!("expected call"); };
    let syntra_lang::Expression::Move { value, span } = &arguments[0] else { panic!("expected move"); };
    assert!(matches!(value.as_ref(), syntra_lang::Expression::Identifier("authority", _)));
    assert_eq!(&source[span.start..span.end], "move authority");
}

#[test]
fn parser_rejects_non_identifier_move_targets() {
    for source in [
        "fn caller() { worker(move authority()); }",
        "fn caller() { worker(move (authority)); }",
        "fn caller() { worker(move authority.field); }",
    ] {
        let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
        let mut parser = Parser::new(&tokens);
        assert!(parser.parse_program().is_err(), "must reject: {source}");
    }
}


#[test]
fn parser_rejects_malformed_capability_parameter_types() {
    for source in [
        "fn invalid(authority: Cap<>) { }",
        "fn invalid(authority: Cap<ReadFile, WriteFile>) { }",
        "fn invalid(authority: Cap<Cap<ReadFile>>) { }",
    ] {
        let tokens = Lexer::new(source).tokenize().expect("lexing succeeds");
        let mut parser = Parser::new(&tokens);
        assert!(parser.parse_program().is_err(), "must reject: {source}");
    }
}
