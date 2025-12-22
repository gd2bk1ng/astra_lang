// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: parser.rs
//
//  Description:
//      Parser for Astra language that converts token stream into AST nodes.
//      Currently supports parsing a single `intent` block with optional motive and action.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
// =============================================================================

use crate::tokens::{Token, TokenKind};
use crate::ast::IntentBlock;

/// Parser structure holding tokens and current position.
pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

/// Possible parse errors.
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(TokenKind),
    MissingField(&'static str),
}

impl<'a> Parser<'a> {
    /// Creates a new parser instance from a slice of tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// Peeks at the current token without consuming it.
    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    /// Advances the current position by one.
    fn bump(&mut self) {
        self.pos += 1;
    }

    /// Expects the current token to be `expected`, consumes it, or errors.
    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        match self.peek() {
            Some(token) if token.kind == expected => {
                self.bump();
                Ok(())
            }
            Some(token) => Err(ParseError::UnexpectedToken(token.kind.clone())),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Expects the current token to be an Identifier and returns its string.
    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(token) => {
                if let TokenKind::Identifier(name) = &token.kind {
                    self.bump();
                    Ok(name.clone())
                } else {
                    Err(ParseError::UnexpectedToken(token.kind.clone()))
                }
            }
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Expects the current token to be a StringLiteral and returns its string.
    fn expect_string_literal(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(token) => {
                if let TokenKind::StringLiteral(value) = &token.kind {
                    self.bump();
                    Ok(value.clone())
                } else {
                    Err(ParseError::UnexpectedToken(token.kind.clone()))
                }
            }
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Parses a single `intent` block with optional `motive` and `action`.
    pub fn parse_intent_block(&mut self) -> Result<IntentBlock, ParseError> {
        // Expect 'intent' keyword
        self.expect(TokenKind::Intent)?;

        // Expect identifier (name)
        let name = self.expect_identifier()?;

        // Expect '{'
        self.expect(TokenKind::LBrace)?;

        // Initialize optional fields
        let mut motive: Option<String> = None;
        let mut action: Option<String> = None;

        // Parse fields inside block until '}'
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Motive => {
                    self.bump();
                    let val = self.expect_string_literal()?;
                    motive = Some(val);
                }
                TokenKind::Action => {
                    self.bump();
                    let val = self.expect_string_literal()?;
                    action = Some(val);
                }
                TokenKind::RBrace => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::UnexpectedToken(token.kind.clone())),
            }
        }

        // Validate required fields
        if motive.is_none() {
            return Err(ParseError::MissingField("motive"));
        }
        if action.is_none() {
            return Err(ParseError::MissingField("action"));
        }

        Ok(IntentBlock { name, motive, action })
    }
}
