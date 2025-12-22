// =============================================================================
// Astra Reference Compiler (ARC)
// File: parser.rs
//
// Description:
//     Full parser implementation for key Astra language constructs,
//     including functions, intents, rules, and expressions.
//
// Author: Alex Roussinov
// Created: 2025-12-22
// =============================================================================

use crate::tokens::{Token, TokenKind};
use std::fmt;

#[derive(Debug, Clone)]
pub enum AstNode {
    Function(FunctionDecl),
    Intent(IntentDecl),
    Rule(RuleDecl),
    // Extend with other top-level declarations as needed
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub is_grad: bool,
    pub name: String,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
    pub effects: Vec<String>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct IntentDecl {
    pub name: String,
    pub motive: Option<String>,
    pub action: Option<String>,
    // Extend with other intent fields as needed
}

#[derive(Debug, Clone)]
pub struct RuleDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Simple(String),
    // Extend with full type system as needed
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    LetBinding { name: String, expr: Option<Expression> },
    Return(Expression),
    // Extend as needed
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    FunctionCall { callee: Box<Expression>, args: Vec<Expression> },
    Block(Block),
    // Extend with other expressions (match, lambda, unary, binary, etc.)
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(TokenKind),
    ExpectedToken(String),
    Custom(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::UnexpectedToken(tok) => write!(f, "Unexpected token: {:?}", tok),
            ParseError::ExpectedToken(tok) => write!(f, "Expected token: {}", tok),
            ParseError::Custom(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    fn bump(&mut self) -> Option<&'a Token> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &TokenKind) -> Result<(), ParseError> {
        match self.peek() {
            Some(token) if &token.kind == expected => {
                self.bump();
                Ok(())
            }
            Some(token) => Err(ParseError::UnexpectedToken(token.kind.clone())),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    Ok(name.clone())
                }
                _ => Err(ParseError::ExpectedToken("identifier".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<AstNode>, ParseError> {
        let mut nodes = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Eof {
                break;
            }
            let node = self.parse_top_level_decl()?;
            nodes.push(node);
        }
        Ok(nodes)
    }

    fn parse_top_level_decl(&mut self) -> Result<AstNode, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(s) if s == "fn" => {
                    let func = self.parse_function_decl()?;
                    Ok(AstNode::Function(func))
                }
                TokenKind::Identifier(s) if s == "intent" => {
                    let intent = self.parse_intent_decl()?;
                    Ok(AstNode::Intent(intent))
                }
                TokenKind::Identifier(s) if s == "rule" => {
                    let rule = self.parse_rule_decl()?;
                    Ok(AstNode::Rule(rule))
                }
                _ => Err(ParseError::UnexpectedToken(token.kind.clone())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_function_decl(&mut self) -> Result<FunctionDecl, ParseError> {
        // Optional @grad annotation
        let mut is_grad = false;
        if let Some(token) = self.peek() {
            if let TokenKind::Identifier(s) = &token.kind {
                if s == "@grad" {
                    is_grad = true;
                    self.bump();
                }
            }
        }

        // Expect fn
        self.expect(&TokenKind::Identifier("fn".into()))?;

        let name = self.expect_identifier()?;

        self.expect(&TokenKind::LParen)?;

        let params = self.parse_param_list()?;

        self.expect(&TokenKind::RParen)?;

        // Optional return type
        let ret_type = if let Some(Token { kind: TokenKind::Arrow, .. }) = self.peek() {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Optional effect annotation
        let effects = if let Some(Token { kind: TokenKind::LBrace, .. }) = self.peek() {
            // For simplicity, not parsing effect annotations here
            Vec::new()
        } else {
            Vec::new()
        };

        let body = self.parse_block()?;

        Ok(FunctionDecl {
            is_grad,
            name,
            params,
            ret_type,
            effects,
            body,
        })
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        loop {
            if let Some(Token { kind: TokenKind::RParen, .. }) = self.peek() {
                break;
            }
            let name = self.expect_identifier()?;
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;
            params.push(Param { name, ty });

            if let Some(Token { kind: TokenKind::Comma, .. }) = self.peek() {
                self.bump();
            } else {
                break;
            }
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    Ok(Type::Simple(name.clone()))
                }
                _ => Err(ParseError::ExpectedToken("type".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RBrace {
                self.bump();
                break;
            }
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // For simplicity parse expression statements or let bindings
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(s) if s == "let" => {
                    self.bump();
                    let name = self.expect_identifier()?;
                    let expr = if let Some(Token { kind: TokenKind::Equal, .. }) = self.peek() {
                        self.bump();
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::LetBinding { name, expr })
                }
                TokenKind::Identifier(s) if s == "return" => {
                    self.bump();
                    let expr = self.parse_expression()?;
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Return(expr))
                }
                _ => {
                    let expr = self.parse_expression()?;
                    self.expect(&TokenKind::Semicolon)?;
                    Ok(Statement::Expr(expr))
                }
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // For simplicity parse identifiers and literals and function calls
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    // Check for function call
                    if let Some(Token { kind: TokenKind::LParen, .. }) = self.peek() {
                        self.bump();
                        let args = self.parse_arg_list()?;
                        self.expect(&TokenKind::RParen)?;
                        Ok(Expression::FunctionCall {
                            callee: Box::new(Expression::Identifier(name.clone())),
                            args,
                        })
                    } else {
                        Ok(Expression::Identifier(name.clone()))
                    }
                }
                TokenKind::IntLiteral(n) => {
                    self.bump();
                    Ok(Expression::IntLiteral(*n))
                }
                TokenKind::FloatLiteral(f) => {
                    self.bump();
                    Ok(Expression::FloatLiteral(*f))
                }
                TokenKind::StringLiteral(s) => {
                    self.bump();
                    Ok(Expression::StringLiteral(s.clone()))
                }
                TokenKind::BoolLiteral(b) => {
                    self.bump();
                    Ok(Expression::BoolLiteral(*b))
                }
                TokenKind::LBrace => {
                    let block = self.parse_block()?;
                    Ok(Expression::Block(block))
                }
                _ => Err(ParseError::UnexpectedToken(token.kind.clone())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();
        loop {
            if let Some(Token { kind: TokenKind::RParen, .. }) = self.peek() {
                break;
            }
            let expr = self.parse_expression()?;
            args.push(expr);
            if let Some(Token { kind: TokenKind::Comma, .. }) = self.peek() {
                self.bump();
            } else {
                break;
            }
        }
        Ok(args)
    }

    fn parse_intent_decl(&mut self) -> Result<IntentDecl, ParseError> {
        self.expect(&TokenKind::Identifier("intent".into()))?;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;

        let mut motive = None;
        let mut action = None;

        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Identifier(s) if s == "motive" => {
                    self.bump();
                    if let Some(Token { kind: TokenKind::StringLiteral(val), .. }) = self.bump() {
                        motive = Some(val.clone());
                    } else {
                        return Err(ParseError::ExpectedToken("string literal".into()));
                    }
                }
                TokenKind::Identifier(s) if s == "action" => {
                    self.bump();
                    if let Some(Token { kind: TokenKind::StringLiteral(val), .. }) = self.bump() {
                        action = Some(val.clone());
                    } else {
                        return Err(ParseError::ExpectedToken("string literal".into()));
                    }
                }
                TokenKind::RBrace => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::UnexpectedToken(token.kind.clone())),
            }
        }

        Ok(IntentDecl { name, motive, action })
    }

    fn parse_rule_decl(&mut self) -> Result<RuleDecl, ParseError> {
        self.expect(&TokenKind::Identifier("rule".into()))?;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;
        let ret_type = if let Some(Token { kind: TokenKind::Arrow, .. }) = self.peek() {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        Ok(RuleDecl { name, params, ret_type, body })
    }
}
