use syntra_lang::{Lexer, TokenKind};

#[test]
fn public_lexer_api_handles_intent_source() {
    let tokens = Lexer::new(
        r#"intent syntra { motive "reason" action "understand" }"#,
    )
    .tokenize()
    .expect("source should lex");

    assert!(matches!(
        tokens.first().map(|token| &token.kind),
        Some(TokenKind::Identifier(name)) if *name == "intent"
    ));
    assert!(matches!(
        tokens.last().map(|token| &token.kind),
        Some(TokenKind::Eof)
    ));
}

#[test]
fn public_tokens_preserve_source_spelling() {
    let tokens = Lexer::new(r#"value = 001_002"#)
        .tokenize()
        .expect("source should lex");

    let number = tokens
        .iter()
        .find(|token| matches!(token.kind, TokenKind::IntLiteral(_)))
        .expect("integer token");

    assert_eq!(number.lexeme, "001_002");
    assert_eq!(number.kind, TokenKind::IntLiteral(1002));
}

#[test]
fn public_lexer_rejects_unknown_characters() {
    let error = Lexer::new("intent $").tokenize().unwrap_err();

    assert!(matches!(
        error,
        syntra_lang::LexError::UnexpectedCharacter { ch: '$', .. }
    ));
}

#[test]
fn public_numeric_contract_accepts_decimal_floats() {
    let tokens = Lexer::new("1.0 .5")
        .tokenize()
        .expect("valid decimal floats should lex");

    assert_eq!(tokens[0].kind, TokenKind::FloatLiteral(1.0));
    assert_eq!(tokens[1].kind, TokenKind::FloatLiteral(0.5));
}

#[test]
fn public_numeric_contract_rejects_malformed_decimal_floats() {
    for source in ["1.", "1_.2", "1__2", "1.2.3"] {
        assert!(
            matches!(
                Lexer::new(source).tokenize(),
                Err(syntra_lang::LexError::InvalidNumber { .. })
            ),
            "expected invalid number for {source:?}"
        );
    }
}

#[test]
fn public_tokens_borrow_original_source() {
    let source = String::from("intent syntra");
    let tokens = Lexer::new(&source)
        .tokenize()
        .expect("source should lex");

    assert!(std::ptr::eq(tokens[0].lexeme.as_ptr(), source.as_ptr()));
    assert_eq!(tokens[0].lexeme, "intent");

    assert!(std::ptr::eq(
        tokens[1].lexeme.as_ptr(),
        source["intent ".len()..].as_ptr()
    ));
}

#[test]
fn public_string_tokens_preserve_raw_source_without_decoding() {
    let source = String::from(r#"value "a\nb""#);
    let tokens = Lexer::new(&source)
        .tokenize()
        .expect("source should lex");

    let string = tokens
        .iter()
        .find(|token| matches!(token.kind, TokenKind::StringLiteral { .. }))
        .expect("string token");

    assert_eq!(
        string.kind,
        TokenKind::StringLiteral { raw: r#"a\nb"# }
    );
    assert_eq!(string.lexeme, r#""a\nb""#);
}
