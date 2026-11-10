use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            line: 1,
            column: 1,
        }
    }

    pub fn with_pos(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenKind {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,
    Pipe,
    DoubleColon,
    At,
    Arrow,
    ArrowFat,
    Equal,
    EqualEqual,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    AndAnd,
    OrOr,
    Underscore,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,

    /// Exact source slice for this token.
    ///
    /// Literal token kinds may contain a normalized/decoded value while this
    /// field preserves the original spelling for diagnostics, tooling, and
    /// source-aware AI systems.
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
            lexeme: String::new(),
        }
    }

    pub fn with_lexeme(kind: TokenKind, span: Span, lexeme: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            lexeme: lexeme.into(),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        match self {
            Identifier(v) => write!(f, "identifier({v})"),
            IntLiteral(v) => write!(f, "integer({v})"),
            FloatLiteral(v) => write!(f, "float({v})"),
            StringLiteral(_) => write!(f, "string"),
            BoolLiteral(v) => write!(f, "boolean({v})"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),
            Pipe => write!(f, "|"),
            DoubleColon => write!(f, "::"),
            At => write!(f, "@"),
            Arrow => write!(f, "->"),
            ArrowFat => write!(f, "=>"),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            NotEqual => write!(f, "!="),
            LessThan => write!(f, "<"),
            LessEqual => write!(f, "<="),
            GreaterThan => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Bang => write!(f, "!"),
            AndAnd => write!(f, "&&"),
            OrOr => write!(f, "||"),
            Underscore => write!(f, "_"),
            Eof => write!(f, "EOF"),
        }
    }
}
