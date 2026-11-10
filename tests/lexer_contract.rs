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
        Some(TokenKind::Identifier(name)) if name == "intent"
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
