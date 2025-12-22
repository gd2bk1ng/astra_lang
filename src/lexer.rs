// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: lexer.rs
//
//  Description:
//      Lexer implementation for Astra language.
//      Converts source code string into a stream of tokens with spans.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
//  Copyright (c) 2025 Alex Roussinov
//
//  License:
//      Dual licensed under the MIT and Apache 2.0 licenses.
//      See LICENSE-MIT and LICENSE-APACHE at the repository root for details.
// =============================================================================

use crate::tokens::{Token, TokenKind, Span};

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
        let peeked = chars.next();
        Lexer {
            input,
            chars,
            peeked,
            line: 1,
            column: 0,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        let current = self.peeked;
        self.peeked = self.chars.next();

        if let Some((_, ch)) = current {
            if ch == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        current
    }

    fn peek(&self) -> Option<char> {
        self.peeked.map(|(_, ch)| ch)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let (start_idx, ch) = match self.peeked {
            Some(c) => c,
            None => {
                return Some(Token::new(
                    TokenKind::Eof,
                    Span::new(self.input.len(), self.input.len()),
                ))
            }
        };

        // Single-char tokens
        if ch == '{' {
            self.bump();
            return Some(Token::new(TokenKind::LBrace, Span::new(start_idx, start_idx + 1)));
        }
        if ch == '}' {
            self.bump();
            return Some(Token::new(TokenKind::RBrace, Span::new(start_idx, start_idx + 1)));
        }

        // String literal
        if ch == '"' {
            return Some(self.lex_string());
        }

        // Identifier or keyword
        if ch.is_alphabetic() || ch == '_' {
            return Some(self.lex_identifier_or_keyword());
        }

        // Unknown char, skip and try next
        self.bump();
        self.next_token()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn lex_string(&mut self) -> Token {
        let (start_idx, _) = self.bump().unwrap(); // consume opening quote
        let mut string_content = String::new();

        while let Some((_, ch)) = self.peeked {
            if ch == '"' {
                self.bump(); // consume closing quote
                break;
            } else {
                string_content.push(ch);
                self.bump();
            }
        }

        let span = Span::new(start_idx, self.peeked.map(|(i, _)| i).unwrap_or(self.input.len()));
        Token::new(TokenKind::StringLiteral(string_content), span)
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let (start_idx, _) = self.peeked.unwrap();
        let mut ident = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.bump();
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "intent" => TokenKind::Intent,
            "motive" => TokenKind::Motive,
            "action" => TokenKind::Action,
            _ => TokenKind::Identifier(ident),
        };

        let span = Span::new(start_idx, self.peeked.map(|(i, _)| i).unwrap_or(self.input.len()));
        Token::new(kind, span)
    }
}
