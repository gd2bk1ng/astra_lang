use serde::{Deserialize, Serialize};
use std::fmt;

/// Byte offsets and human-readable source position for a token.
///
/// Offsets are always measured in the original UTF-8 source buffer. A borrowed
/// token therefore never needs to duplicate source text to retain provenance.
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

/// Lexical token kind.
///
/// The token kind borrows source text for constructs whose spelling is
/// semantically relevant. Numeric values remain immediate scalar values; their
/// exact source spelling is retained by Token::lexeme.
///
/// String literals intentionally preserve their raw source contents rather
/// than allocating a decoded String. Escape decoding belongs to a later
/// semantic/value layer.
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral { raw: &'a str },
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

/// A zero-copy lexical token borrowing directly from the input source.
///
/// # Lifetime contract
///
/// 'a is the lifetime of the source buffer supplied to the lexer. The token
/// cannot outlive that source buffer, and it never owns a copy of its lexeme.
/// This is the ownership boundary the parser and future AST<'a> will build
/// upon.
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,

    /// Exact source spelling for this token.
    pub lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span, lexeme: &'a str) -> Self {
        Self { kind, span, lexeme }
    }
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        match self {
            Identifier(v) => write!(f, "identifier({v})"),
            IntLiteral(v) => write!(f, "integer({v})"),
            FloatLiteral(v) => write!(f, "float({v})"),
            StringLiteral { .. } => write!(f, "string"),
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
