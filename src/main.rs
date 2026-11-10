use syntra_lang::{Lexer, TokenKind};

fn main() {
    let source = r#"
        intent syntra {
            motive "reason"
            action "understand"
        }
    "#;

    let tokens = Lexer::new(source)
        .tokenize()
        .expect("source should lex successfully");

    for token in tokens {
        println!("{:?} {:?} {:?}", token.kind, token.span, token.lexeme);

        if token.kind == TokenKind::Eof {
            break;
        }
    }
}
