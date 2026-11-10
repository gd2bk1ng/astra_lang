use crate::tokens::{Span, Token, TokenKind};
use std::fmt;
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { ch: char, span: Span },
    UnterminatedString { span: Span },
    InvalidEscape { escape: String, span: Span },
    UnterminatedBlockComment { span: Span },
    InvalidNumber { lexeme: String, span: Span },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter { ch, span } =>
                write!(f, "unexpected character {ch:?} at {}:{}", span.line, span.column),
            Self::UnterminatedString { span } =>
                write!(f, "unterminated string at {}:{}", span.line, span.column),
            Self::InvalidEscape { escape, span } =>
                write!(f, "invalid escape {escape:?} at {}:{}", span.line, span.column),
            Self::UnterminatedBlockComment { span } =>
                write!(f, "unterminated block comment at {}:{}", span.line, span.column),
            Self::InvalidNumber { lexeme, span } =>
                write!(f, "invalid number {lexeme:?} at {}:{}", span.line, span.column),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: std::str::CharIndices<'a>,
    peeked: Option<(usize, char)>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.char_indices();
        Self {
            peeked: chars.next(),
            input,
            chars,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            let token = self.next_token()?;
            let eof = token.kind == TokenKind::Eof;
            out.push(token);
            if eof {
                return Ok(out);
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_space_and_comments()?;

        let (start, ch) = match self.peeked {
            Some(v) => v,
            None => {
                let span = Span::with_pos(
                    self.input.len(),
                    self.input.len(),
                    self.line,
                    self.column,
                );
                return Ok(Token::with_lexeme(TokenKind::Eof, span, ""));
            }
        };

        let line = self.line;
        let column = self.column;

        if is_ident_start(ch) {
            return Ok(self.lex_identifier(start, line, column));
        }

        if ch.is_ascii_digit() || (ch == '.' && self.next_is_digit()) {
            return self.lex_number(start, line, column);
        }

        if ch == '"' {
            return self.lex_string(start, line, column);
        }

        let kind = match ch {
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => {
                if self.next_is(':') {
                    self.bump();
                    TokenKind::DoubleColon
                } else {
                    TokenKind::Colon
                }
            }
            '|' => {
                if self.next_is('|') {
                    self.bump();
                    TokenKind::OrOr
                } else {
                    TokenKind::Pipe
                }
            }
            '@' => TokenKind::At,
            '+' => TokenKind::Plus,
            '-' => {
                if self.next_is('>') {
                    self.bump();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '!' => {
                if self.next_is('=') {
                    self.bump();
                    TokenKind::NotEqual
                } else {
                    TokenKind::Bang
                }
            }
            '=' => {
                if self.next_is('=') {
                    self.bump();
                    TokenKind::EqualEqual
                } else if self.next_is('>') {
                    self.bump();
                    TokenKind::ArrowFat
                } else {
                    TokenKind::Equal
                }
            }
            '<' => {
                if self.next_is('=') {
                    self.bump();
                    TokenKind::LessEqual
                } else {
                    TokenKind::LessThan
                }
            }
            '>' => {
                if self.next_is('=') {
                    self.bump();
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            '&' => {
                if self.next_is('&') {
                    self.bump();
                    TokenKind::AndAnd
                } else {
                    return Err(LexError::UnexpectedCharacter {
                        ch,
                        span: self.make_span(start, start + ch.len_utf8(), line, column),
                    });
                }
            }
            '_' => TokenKind::Underscore,
            _ => {
                return Err(LexError::UnexpectedCharacter {
                    ch,
                    span: self.make_span(start, start + ch.len_utf8(), line, column),
                })
            }
        };

        let end = self.bump()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(start + ch.len_utf8());

        Ok(self.token_from_source(kind, start, end, line, column))
    }

    fn token_from_source(
        &self,
        kind: TokenKind,
        start: usize,
        end: usize,
        line: usize,
        column: usize,
    ) -> Token {
        Token::with_lexeme(
            kind,
            self.make_span(start, end, line, column),
            &self.input[start..end],
        )
    }

    fn skip_space_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            while self.peek().is_some_and(char::is_whitespace) {
                self.bump();
            }

            if self.peek() != Some('/') {
                return Ok(());
            }

            if self.next_is('/') {
                self.bump();
                self.bump();

                while let Some(c) = self.peek() {
                    self.bump();
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }

            if self.next_is('*') {
                let start = self.offset();
                let line = self.line;
                let column = self.column;

                self.bump();
                self.bump();

                loop {
                    match self.peek() {
                        None => {
                            return Err(LexError::UnterminatedBlockComment {
                                span: self.make_span(start, self.input.len(), line, column),
                            })
                        }
                        Some('*') if self.next_is('/') => {
                            self.bump();
                            self.bump();
                            break;
                        }
                        Some(_) => {
                            self.bump();
                        }
                    }
                }

                continue;
            }

            return Ok(());
        }
    }

    fn lex_identifier(&mut self, start: usize, line: usize, column: usize) -> Token {
        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
                self.bump();
            } else {
                break;
            }
        }

        let end = self.offset();
        let raw = &self.input[start..end];

        let kind = match raw {
            "true" => TokenKind::BoolLiteral(true),
            "false" => TokenKind::BoolLiteral(false),
            "_" => TokenKind::Underscore,
            _ => TokenKind::Identifier(raw.to_owned()),
        };

        Token::with_lexeme(
            kind,
            self.make_span(start, end, line, column),
            raw,
        )
    }

    fn lex_number(
        &mut self,
        start: usize,
        line: usize,
        column: usize,
    ) -> Result<Token, LexError> {
        let mut text = String::new();
        let mut has_dot = false;
        let mut has_digits = false;

        if self.peek() == Some('.') {
            has_dot = true;
            text.push('.');
            self.bump();
        }

        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {
                    has_digits = true;
                    text.push(c);
                    self.bump();
                }
                '_' => {
                    text.push(c);
                    self.bump();
                }
                '.' if !has_dot => {
                    has_dot = true;
                    text.push('.');
                    self.bump();
                }
                '.' => break,
                _ => break,
            }
        }

        // Decimal floats require digits on both sides of the decimal point.
        // This keeps the lexical contract unambiguous: "1." is not a float,
        // and the trailing dot remains available to a future punctuation rule.
        if !has_digits || text.ends_with('_') || text.ends_with('.') || text.contains("__") {
            return Err(LexError::InvalidNumber {
                lexeme: text,
                span: self.make_span(start, self.offset(), line, column),
            });
        }

        // A second dot followed by a digit is almost certainly a malformed
        // numeric literal, rather than two valid tokens.
        if self.peek() == Some('.') && self.next_is_digit() {
            text.push('.');
            self.bump();

            while let Some(c) = self.peek() {
                if c.is_ascii_digit() || c == '_' {
                    text.push(c);
                    self.bump();
                } else {
                    break;
                }
            }

            return Err(LexError::InvalidNumber {
                lexeme: text,
                span: self.make_span(start, self.offset(), line, column),
            });
        }

        let clean = text.replace('_', "");

        let kind = if has_dot {
            match clean.parse::<f64>() {
                Ok(value) if value.is_finite() => TokenKind::FloatLiteral(value),
                _ => {
                    return Err(LexError::InvalidNumber {
                        lexeme: text,
                        span: self.make_span(start, self.offset(), line, column),
                    })
                }
            }
        } else {
            match clean.parse::<i64>() {
                Ok(value) => TokenKind::IntLiteral(value),
                Err(_) => {
                    return Err(LexError::InvalidNumber {
                        lexeme: text,
                        span: self.make_span(start, self.offset(), line, column),
                    })
                }
            }
        };

        Ok(self.token_from_source(
            kind,
            start,
            self.offset(),
            line,
            column,
        ))
    }

    fn lex_string(
        &mut self,
        start: usize,
        line: usize,
        column: usize,
    ) -> Result<Token, LexError> {
        self.bump(); // opening quote
        let mut value = String::new();

        loop {
            match self.peek() {
                None => {
                    return Err(LexError::UnterminatedString {
                        span: self.make_span(start, self.input.len(), line, column),
                    })
                }
                Some('"') => {
                    self.bump();
                    break;
                }
                Some('\\') => {
                    let escape_start = self.offset();
                    self.bump();

                    let escaped = match self.peek() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('0') => '\0',
                        Some('\\') => '\\',
                        Some('"') => '"',
                        Some('/') => '/',
                        Some(other) => {
                            return Err(LexError::InvalidEscape {
                                escape: format!("\\{other}"),
                                span: self.make_span(
                                    escape_start,
                                    self.offset() + other.len_utf8(),
                                    line,
                                    column,
                                ),
                            })
                        }
                        None => {
                            return Err(LexError::UnterminatedString {
                                span: self.make_span(
                                    start,
                                    self.input.len(),
                                    line,
                                    column,
                                ),
                            })
                        }
                    };

                    self.bump();
                    value.push(escaped);
                }
                Some(c) => {
                    value.push(c);
                    self.bump();
                }
            }
        }

        Ok(self.token_from_source(
            TokenKind::StringLiteral(value),
            start,
            self.offset(),
            line,
            column,
        ))
    }

    fn peek(&self) -> Option<char> {
        self.peeked.map(|(_, c)| c)
    }

    fn next_is(&self, expected: char) -> bool {
        self.peeked
            .map(|(i, _)| self.input[i..].chars().nth(1) == Some(expected))
            .unwrap_or(false)
    }

    fn next_is_digit(&self) -> bool {
        self.peeked
            .map(|(i, _)| {
                self.input[i..]
                    .chars()
                    .nth(1)
                    .is_some_and(|c| c.is_ascii_digit())
            })
            .unwrap_or(false)
    }

    fn offset(&self) -> usize {
        self.peeked
            .map(|(i, _)| i)
            .unwrap_or(self.input.len())
    }

    fn make_span(&self, start: usize, end: usize, line: usize, column: usize) -> Span {
        Span::with_pos(start, end, line, column)
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let current = self.peeked;
        self.peeked = self.chars.next();

        if let Some((_, c)) = current {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        current
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || is_xid_start(c)
}

fn is_ident_continue(c: char) -> bool {
    is_xid_continue(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(src: &str) -> Vec<TokenKind> {
        Lexer::new(src)
            .tokenize()
            .unwrap()
            .into_iter()
            .map(|token| token.kind)
            .collect()
    }

    #[test]
    fn full_punctuation() {
        assert_eq!(
            kinds("{}()[],:;|::@->=>= == != < <= > >= + - * / ! && || _"),
            vec![
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::Comma,
                TokenKind::Colon,
                TokenKind::Semicolon,
                TokenKind::Pipe,
                TokenKind::DoubleColon,
                TokenKind::At,
                TokenKind::Arrow,
                TokenKind::ArrowFat,
                TokenKind::Equal,
                TokenKind::EqualEqual,
                TokenKind::NotEqual,
                TokenKind::LessThan,
                TokenKind::LessEqual,
                TokenKind::GreaterThan,
                TokenKind::GreaterEqual,
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Bang,
                TokenKind::AndAnd,
                TokenKind::OrOr,
                TokenKind::Underscore,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn literals_and_unicode_identifiers() {
        assert_eq!(
            kinds(r#"intent 東京 { value 42 other 3.14 text "hello\nworld" true false }"#),
            vec![
                TokenKind::Identifier("intent".into()),
                TokenKind::Identifier("東京".into()),
                TokenKind::LBrace,
                TokenKind::Identifier("value".into()),
                TokenKind::IntLiteral(42),
                TokenKind::Identifier("other".into()),
                TokenKind::FloatLiteral("3.14".parse::<f64>().unwrap()),
                TokenKind::Identifier("text".into()),
                TokenKind::StringLiteral("hello\nworld".into()),
                TokenKind::BoolLiteral(true),
                TokenKind::BoolLiteral(false),
                TokenKind::RBrace,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn supports_combining_marks_in_identifiers() {
        assert_eq!(
            kinds("é"),
            vec![TokenKind::Identifier("é".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn preserves_raw_lexemes() {
        let tokens = Lexer::new(r#"x = 001_002; text = "a\nb";"#)
            .tokenize()
            .unwrap();

        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[2].lexeme, "001_002");
        assert_eq!(tokens[6].lexeme, r#""a\nb""#);
    }

    #[test]
    fn separates_annotation_prefix() {
        let tokens = Lexer::new("@grad fn loss(x: Float<64>) -> Float<64>")
            .tokenize()
            .unwrap();

        assert_eq!(tokens[0].kind, TokenKind::At);
        assert_eq!(tokens[1].kind, TokenKind::Identifier("grad".into()));
        assert_eq!(tokens[2].kind, TokenKind::Identifier("fn".into()));
    }

    #[test]
    fn handles_negative_numbers_as_unary_minus() {
        let tokens = Lexer::new("-42 -3.14")
            .tokenize()
            .unwrap();

        assert_eq!(tokens[0].kind, TokenKind::Minus);
        assert_eq!(tokens[1].kind, TokenKind::IntLiteral(42));
        assert_eq!(tokens[2].kind, TokenKind::Minus);
        assert_eq!(tokens[3].kind, TokenKind::FloatLiteral("3.14".parse::<f64>().unwrap()));
    }

    #[test]
    fn rejects_unknown_input() {
        assert!(matches!(
            Lexer::new("fn $").tokenize(),
            Err(LexError::UnexpectedCharacter { ch: '$', .. })
        ));
    }

    #[test]
    fn rejects_unterminated_string() {
        assert!(matches!(
            Lexer::new(r#"intent x { motive "oops }"#).tokenize(),
            Err(LexError::UnterminatedString { .. })
        ));
    }

    #[test]
    fn rejects_unterminated_block_comment() {
        assert!(matches!(
            Lexer::new("/* never closes").tokenize(),
            Err(LexError::UnterminatedBlockComment { .. })
        ));
    }

    #[test]
    fn rejects_malformed_numbers() {
        assert_eq!(kinds("1.0 .5"), vec![
            TokenKind::FloatLiteral(1.0),
            TokenKind::FloatLiteral(0.5),
            TokenKind::Eof,
        ]);

        assert!(matches!(
            Lexer::new("1.").tokenize(),
            Err(LexError::InvalidNumber { .. })
        ));

        assert!(matches!(
            Lexer::new("1_.2").tokenize(),
            Err(LexError::InvalidNumber { .. })
        ));

        assert!(matches!(
            Lexer::new("1._2").tokenize(),
            Err(LexError::InvalidNumber { .. })
        ));

        assert!(matches!(
            Lexer::new("1__2").tokenize(),
            Err(LexError::InvalidNumber { .. })
        ));

        assert!(matches!(
            Lexer::new("1.2.3").tokenize(),
            Err(LexError::InvalidNumber { .. })
        ));
    }

    #[test]
    fn tracks_positions() {
        let tokens = Lexer::new("fn main() {\n  return 42;\n}")
            .tokenize()
            .unwrap();

        assert_eq!((tokens[0].span.line, tokens[0].span.column), (1, 1));
        assert_eq!((tokens[5].span.line, tokens[5].span.column), (2, 3));
    }
}
