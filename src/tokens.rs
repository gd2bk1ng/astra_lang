// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: tokens.rs
//
//  Description:
//      Token kinds, token container, and source Span used by the lexer.
//      Parser and other passes consume Vec<Token> produced by the lexer.
//      This file intentionally keeps TokenKind exhaustive for operators,
//      punctuation, keywords and literal kinds required by ARC v0.1 and the
//      Astra Manifesto (annotations, rule/actor/backtrack, Arrow, DoubleColon).
//
//  Author:      Alex Roussinov
//  Created:     2025-12-05
//  Copyright (c) 2025 Alex Roussinov
//
//  License:
//      Dual licensed under the MIT and Apache 2.0 licenses.
//      See LICENSE-MIT and LICENSE-APACHE at the repository root for details.
// =============================================================================

use std::fmt;

/// Source-span used while lexing and parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: Option<usize>,
    pub column: Option<usize>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end, line: None, column: None }
    }

    pub fn with_pos(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self { start, end, line: Some(line), column: Some(column) }
    }
}

/// Token kinds for Astra language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Intent,
    Motive,
    Action,

    Identifier(String),
    StringLiteral(String),

    LBrace,
    RBrace,

    Eof,
}

/// Token produced by the lexer: a kind plus source span.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self {
            Intent => write!(f, "intent"),
            Motive => write!(f, "motive"),
            Action => write!(f, "action"),
            Identifier(name) => write!(f, "Identifier({})", name),
            StringLiteral(s) => write!(f, "StringLiteral(\"{}\")", s),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Eof => write!(f, "EOF"),
        }
    }
}
