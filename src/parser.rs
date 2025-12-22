// =============================================================================
// Astra Reference Compiler (ARC)
// File: parser.rs
//
// Extended parser implementation supporting:
// - Full type parsing (linear, capability, dependent, symbolic, differentiable)
// - Effect annotations on functions
// - Pattern matching expressions and patterns
// - Symbolic expressions
// - Backtracking blocks
// - Basic error recovery and diagnostics
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
    // Add more as needed
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
    // Add more intent fields as needed
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

    Mut(Box<Type>),
    Ref(Box<Type>),
    Cap(Box<Type>),
    Tensor(Shape, DType),
    Symbolic(String),
    Grad(Box<Type>),
    DepType {
        var: String,
        var_type: Box<Type>,
        prop: Expression,
    },

    Unit,
    Bool,
    Int(u32),
    Float(u32),
    String,
}

#[derive(Debug, Clone)]
pub struct Shape(pub Vec<ShapeDim>);

#[derive(Debug, Clone)]
pub enum ShapeDim {
    Number(u32),
    Identifier(String),
}

#[derive(Debug, Clone)]
pub enum DType {
    F32,
    F64,
    I32,
    I64,
    Bool,
    String,
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
    Backtrack(Block),
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
    Match { expr: Box<Expression>, arms: Vec<MatchArm> },
    Symbolic(String),
    SelfModify { target: String, patch: Box<Expression> },
    // Extend as needed
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    Wildcard,
    Literal(Expression),
    Tuple(Vec<Pattern>),
    Constructor { name: String, args: Vec<Pattern> },
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
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
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
            Some(token) => {
                let err = ParseError::UnexpectedToken(token.kind.clone());
                self.errors.push(err.clone());
                Err(err)
            }
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    Ok(name.clone())
                }
                _ => {
                    let err = ParseError::ExpectedToken("identifier".into());
                    self.errors.push(err.clone());
                    Err(err)
                }
            },
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    pub fn parse_program(&mut self) -> Vec<AstNode> {
        let mut nodes = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Eof {
                break;
            }
            match self.parse_top_level_decl() {
                Ok(node) => nodes.push(node),
                Err(_) => self.recover_top_level(),
            }
        }
        nodes
    }

    fn recover_top_level(&mut self) {
        // Simple recovery: skip tokens until next top-level keyword or EOF
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Identifier(s) if s == "fn" || s == "intent" || s == "rule" => break,
                TokenKind::Eof => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn parse_top_level_decl(&mut self) -> Result<AstNode, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(s) if s == "fn" || s == "@grad" => {
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
                _ => {
                    let err = ParseError::UnexpectedToken(token.kind.clone());
                    self.errors.push(err.clone());
                    Err(err)
                }
            },
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
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

        // Optional effect annotation: { Pure, IO, ... }
        let effects = if let Some(Token { kind: TokenKind::LBrace, .. }) = self.peek() {
            self.parse_effect_annotation()?
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
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RParen {
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
                    // Handle type constructors like Mut<T>, Cap<T>, etc.
                    let type_name = name.clone();
                    self.bump();

                    match type_name.as_str() {
                        "Mut" | "Ref" | "Cap" | "Grad" => {
                            self.expect(&TokenKind::LessThan)?;
                            let inner = self.parse_type()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            let ty = match type_name.as_str() {
                                "Mut" => Type::Mut(Box::new(inner)),
                                "Ref" => Type::Ref(Box::new(inner)),
                                "Cap" => Type::Cap(Box::new(inner)),
                                "Grad" => Type::Grad(Box::new(inner)),
                                _ => unreachable!(),
                            };
                            Ok(ty)
                        }
                        "Tensor" => {
                            self.expect(&TokenKind::LessThan)?;
                            let shape = self.parse_shape()?;
                            self.expect(&TokenKind::Comma)?;
                            let dtype = self.parse_dtype()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            Ok(Type::Tensor(shape, dtype))
                        }
                        "DepType" => {
                            self.expect(&TokenKind::LessThan)?;
                            let var = self.expect_identifier()?;
                            self.expect(&TokenKind::Colon)?;
                            let var_type = self.parse_type()?;
                            self.expect(&TokenKind::Pipe)?;
                            let prop = self.parse_expression()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            Ok(Type::DepType {
                                var,
                                var_type: Box::new(var_type),
                                prop,
                            })
                        }
                        "Symbolic" => {
                            self.expect(&TokenKind::LessThan)?;
                            let prop_expr = self.parse_expression()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            if let Expression::Identifier(prop_name) = prop_expr {
                                Ok(Type::Symbolic(prop_name))
                            } else {
                                // For simplicity, symbolic prop as string from identifier only
                                Err(ParseError::Custom(
                                    "Symbolic prop must be identifier".into(),
                                ))
                            }
                        }
                        "Unit" => Ok(Type::Unit),
                        "Bool" => Ok(Type::Bool),
                        "String" => Ok(Type::String),
                        "Int" | "Float" => {
                            self.expect(&TokenKind::LessThan)?;
                            let size = self.expect_int_literal()? as u32;
                            self.expect(&TokenKind::GreaterThan)?;
                            if type_name == "Int" {
                                Ok(Type::Int(size))
                            } else {
                                Ok(Type::Float(size))
                            }
                        }
                        _ => Ok(Type::Simple(type_name)),
                    }
                }
                _ => Err(ParseError::ExpectedToken("type".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn expect_int_literal(&mut self) -> Result<i64, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::IntLiteral(n) => {
                    self.bump();
                    Ok(*n)
                }
                _ => {
                    let err = ParseError::ExpectedToken("integer literal".into());
                    self.errors.push(err.clone());
                    Err(err)
                }
            },
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    fn parse_shape(&mut self) -> Result<Shape, ParseError> {
        self.expect(&TokenKind::LBracket)?;
        let mut dims = Vec::new();
        loop {
            match self.peek() {
                Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                    dims.push(ShapeDim::Number(*n as u32));
                    self.bump();
                }
                Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                    dims.push(ShapeDim::Identifier(name.clone()));
                    self.bump();
                }
                _ => {
                    return Err(ParseError::ExpectedToken("shape dimension".into()));
                }
            }
            match self.peek() {
                Some(Token { kind: TokenKind::Comma, .. }) => {
                    self.bump();
                }
                Some(Token { kind: TokenKind::RBracket, .. }) => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::ExpectedToken("',' or ']'".into())),
            }
        }
        Ok(Shape(dims))
    }

    fn parse_dtype(&mut self) -> Result<DType, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    match name.as_str() {
                        "f32" => Ok(DType::F32),
                        "f64" => Ok(DType::F64),
                        "i32" => Ok(DType::I32),
                        "i64" => Ok(DType::I64),
                        "bool" => Ok(DType::Bool),
                        "string" => Ok(DType::String),
                        _ => Err(ParseError::Custom(format!("Unknown dtype: {}", name))),
                    }
                }
                _ => Err(ParseError::ExpectedToken("dtype".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_effect_annotation(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect(&TokenKind::LBrace)?;
        let mut effects = Vec::new();
        loop {
            match self.peek() {
                Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                    effects.push(name.clone());
                    self.bump();
                }
                Some(Token { kind: TokenKind::Comma, .. }) => {
                    self.bump();
                }
                Some(Token { kind: TokenKind::RBrace, .. }) => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::ExpectedToken("effect name or '}'".into())),
            }
        }
        Ok(effects)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RBrace {
                self.bump();
                break;
            }
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.recover_statement();
                }
            }
        }
        Ok(Block { statements })
    }

    fn recover_statement(&mut self) {
        // Skip tokens until semicolon or block end
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Semicolon || token.kind == TokenKind::RBrace {
                if token.kind == TokenKind::Semicolon {
                    self.bump();
                }
                break;
            }
            self.bump();
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
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
                TokenKind::Identifier(s) if s == "backtrack" => {
                    self.bump();
                    let block = self.parse_block()?;
                    Ok(Statement::Backtrack(block))
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
                    } else if name == "symbolic" {
                        // symbolic(expr)
                        self.expect(&TokenKind::LParen)?;
                        let sym_name = self.expect_identifier()?;
                        self.expect(&TokenKind::RParen)?;
                        Ok(Expression::Symbolic(sym_name))
                    } else if name == "modify" {
                        // modify(target, patch)
                        self.expect(&TokenKind::LParen)?;
                        let target = self.expect_identifier()?;
                        self.expect(&TokenKind::Comma)?;
                        let patch = self.parse_expression()?;
                        self.expect(&TokenKind::RParen)?;
                        Ok(Expression::SelfModify {
                            target,
                            patch: Box::new(patch),
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
                TokenKind::Identifier(s) if s == "match" => {
                    self.bump();
                    let expr = self.parse_expression()?;
                    self.expect(&TokenKind::LBrace)?;
                    let mut arms = Vec::new();
                    while let Some(token) = self.peek() {
                        if token.kind == TokenKind::RBrace {
                            self.bump();
                            break;
                        }
                        let pat = self.parse_pattern()?;
                        self.expect(&TokenKind::ArrowFat)?; // => token
                        let arm_expr = self.parse_expression()?;
                        self.expect(&TokenKind::Semicolon)?;
                        arms.push(MatchArm {
                            pattern: pat,
                            expr: arm_expr,
                        });
                    }
                    Ok(Expression::Match {
                        expr: Box::new(expr),
                        arms,
                    })
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    // Check for constructor pattern with args
                    if let Some(Token { kind: TokenKind::LParen, .. }) = self.peek() {
                        self.bump();
                        let mut args = Vec::new();
                        while let Some(token) = self.peek() {
                            if token.kind == TokenKind::RParen {
                                self.bump();
                                break;
                            }
                            let pat = self.parse_pattern()?;
                            args.push(pat);
                            if let Some(Token { kind: TokenKind::Comma, .. }) = self.peek() {
                                self.bump();
                            } else {
                                // Expecting RParen next
                                continue;
                            }
                        }
                        Ok(Pattern::Constructor {
                            name: name.clone(),
                            args,
                        })
                    } else {
                        Ok(Pattern::Identifier(name.clone()))
                    }
                }
                TokenKind::Underscore => {
                    self.bump();
                    Ok(Pattern::Wildcard)
                }
                TokenKind::IntLiteral(n) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::IntLiteral(*n)))
                }
                TokenKind::StringLiteral(s) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::StringLiteral(s.clone())))
                }
                TokenKind::BoolLiteral(b) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::BoolLiteral(*b)))
                }
                TokenKind::LParen => {
                    self.bump();
                    let mut pats = Vec::new();
                    while let Some(token) =






                        fn recover_top_level(&mut self) {
        // Simple recovery: skip tokens until next top-level keyword or EOF
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Identifier(s) if s == "fn" || s == "intent" || s == "rule" => break,
                TokenKind::Eof => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn parse_top_level_decl(&mut self) -> Result<AstNode, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(s) if s == "@grad" || s == "fn" => {
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
                _ => {
                    let err = ParseError::UnexpectedToken(token.kind.clone());
                    self.errors.push(err.clone());
                    Err(err)
                }
            },
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
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

        // Optional effect annotation: { Pure, IO, ... }
        let effects = if let Some(Token { kind: TokenKind::LBrace, .. }) = self.peek() {
            self.parse_effect_annotation()?
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
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RParen {
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
                    let type_name = name.clone();
                    self.bump();

                    match type_name.as_str() {
                        "Mut" | "Ref" | "Cap" | "Grad" => {
                            self.expect(&TokenKind::LessThan)?;
                            let inner = self.parse_type()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            let ty = match type_name.as_str() {
                                "Mut" => Type::Mut(Box::new(inner)),
                                "Ref" => Type::Ref(Box::new(inner)),
                                "Cap" => Type::Cap(Box::new(inner)),
                                "Grad" => Type::Grad(Box::new(inner)),
                                _ => unreachable!(),
                            };
                            Ok(ty)
                        }
                        "Tensor" => {
                            self.expect(&TokenKind::LessThan)?;
                            let shape = self.parse_shape()?;
                            self.expect(&TokenKind::Comma)?;
                            let dtype = self.parse_dtype()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            Ok(Type::Tensor(shape, dtype))
                        }
                        "DepType" => {
                            self.expect(&TokenKind::LessThan)?;
                            let var = self.expect_identifier()?;
                            self.expect(&TokenKind::Colon)?;
                            let var_type = self.parse_type()?;
                            self.expect(&TokenKind::Pipe)?;
                            let prop = self.parse_expression()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            Ok(Type::DepType {
                                var,
                                var_type: Box::new(var_type),
                                prop,
                            })
                        }
                        "Symbolic" => {
                            self.expect(&TokenKind::LessThan)?;
                            let prop_expr = self.parse_expression()?;
                            self.expect(&TokenKind::GreaterThan)?;
                            if let Expression::Identifier(prop_name) = prop_expr {
                                Ok(Type::Symbolic(prop_name))
                            } else {
                                Err(ParseError::Custom(
                                    "Symbolic prop must be identifier".into(),
                                ))
                            }
                        }
                        "Unit" => Ok(Type::Unit),
                        "Bool" => Ok(Type::Bool),
                        "String" => Ok(Type::String),
                        "Int" | "Float" => {
                            self.expect(&TokenKind::LessThan)?;
                            let size = self.expect_int_literal()? as u32;
                            self.expect(&TokenKind::GreaterThan)?;
                            if type_name == "Int" {
                                Ok(Type::Int(size))
                            } else {
                                Ok(Type::Float(size))
                            }
                        }
                        _ => Ok(Type::Simple(type_name)),
                    }
                }
                _ => Err(ParseError::ExpectedToken("type".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn expect_int_literal(&mut self) -> Result<i64, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::IntLiteral(n) => {
                    self.bump();
                    Ok(*n)
                }
                _ => {
                    let err = ParseError::ExpectedToken("integer literal".into());
                    self.errors.push(err.clone());
                    Err(err)
                }
            },
            None => {
                let err = ParseError::UnexpectedEof;
                self.errors.push(err.clone());
                Err(err)
            }
        }
    }

    fn parse_shape(&mut self) -> Result<Shape, ParseError> {
        self.expect(&TokenKind::LBracket)?;
        let mut dims = Vec::new();
        loop {
            match self.peek() {
                Some(Token { kind: TokenKind::IntLiteral(n), .. }) => {
                    dims.push(ShapeDim::Number(*n as u32));
                    self.bump();
                }
                Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                    dims.push(ShapeDim::Identifier(name.clone()));
                    self.bump();
                }
                _ => {
                    return Err(ParseError::ExpectedToken("shape dimension".into()));
                }
            }
            match self.peek() {
                Some(Token { kind: TokenKind::Comma, .. }) => {
                    self.bump();
                }
                Some(Token { kind: TokenKind::RBracket, .. }) => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::ExpectedToken("',' or ']'".into())),
            }
        }
        Ok(Shape(dims))
    }

    fn parse_dtype(&mut self) -> Result<DType, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    match name.as_str() {
                        "f32" => Ok(DType::F32),
                        "f64" => Ok(DType::F64),
                        "i32" => Ok(DType::I32),
                        "i64" => Ok(DType::I64),
                        "bool" => Ok(DType::Bool),
                        "string" => Ok(DType::String),
                        _ => Err(ParseError::Custom(format!("Unknown dtype: {}", name))),
                    }
                }
                _ => Err(ParseError::ExpectedToken("dtype".into())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_effect_annotation(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect(&TokenKind::LBrace)?;
        let mut effects = Vec::new();
        loop {
            match self.peek() {
                Some(Token { kind: TokenKind::Identifier(name), .. }) => {
                    effects.push(name.clone());
                    self.bump();
                }
                Some(Token { kind: TokenKind::Comma, .. }) => {
                    self.bump();
                }
                Some(Token { kind: TokenKind::RBrace, .. }) => {
                    self.bump();
                    break;
                }
                _ => return Err(ParseError::ExpectedToken("effect name or '}'".into())),
            }
        }
        Ok(effects)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RBrace {
                self.bump();
                break;
            }
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.errors.push(err);
                    self.recover_statement();
                }
            }
        }
        Ok(Block { statements })
    }

    fn recover_statement(&mut self) {
        // Skip tokens until semicolon or block end
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Semicolon || token.kind == TokenKind::RBrace {
                if token.kind == TokenKind::Semicolon {
                    self.bump();
                }
                break;
            }
            self.bump();
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
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
                TokenKind::Identifier(s) if s == "backtrack" => {
                    self.bump();
                    let block = self.parse_block()?;
                    Ok(Statement::Backtrack(block))
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
                    } else if name == "symbolic" {
                        self.expect(&TokenKind::LParen)?;
                        let sym_name = self.expect_identifier()?;
                        self.expect(&TokenKind::RParen)?;
                        Ok(Expression::Symbolic(sym_name))
                    } else if name == "modify" {
                        self.expect(&TokenKind::LParen)?;
                        let target = self.expect_identifier()?;
                        self.expect(&TokenKind::Comma)?;
                        let patch = self.parse_expression()?;
                        self.expect(&TokenKind::RParen)?;
                        Ok(Expression::SelfModify {
                            target,
                            patch: Box::new(patch),
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
                TokenKind::Identifier(s) if s == "match" => {
                    self.bump();
                    let expr = self.parse_expression()?;
                    self.expect(&TokenKind::LBrace)?;
                    let mut arms = Vec::new();
                    while let Some(token) = self.peek() {
                        if token.kind == TokenKind::RBrace {
                            self.bump();
                            break;
                        }
                        let pat = self.parse_pattern()?;
                        self.expect(&TokenKind::ArrowFat)?;
                        let arm_expr = self.parse_expression()?;
                        self.expect(&TokenKind::Semicolon)?;
                        arms.push(MatchArm {
                            pattern: pat,
                            expr: arm_expr,
                        });
                    }
                    Ok(Expression::Match {
                        expr: Box::new(expr),
                        arms,
                    })
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => {
                    self.bump();
                    if let Some(Token { kind: TokenKind::LParen, .. }) = self.peek() {
                        self.bump();
                        let mut args = Vec::new();
                        while let Some(token) = self.peek() {
                            if token.kind == TokenKind::RParen {
                                self.bump();
                                break;
                            }
                            let pat = self.parse_pattern()?;
                            args.push(pat);
                            if let Some(Token { kind: TokenKind::Comma, .. }) = self.peek() {
                                self.bump();
                            } else {
                                continue;
                            }
                        }
                        Ok(Pattern::Constructor {
                            name: name.clone(),
                            args,
                        })
                    } else {
                        Ok(Pattern::Identifier(name.clone()))
                    }
                }
                TokenKind::Underscore => {
                    self.bump();
                    Ok(Pattern::Wildcard)
                }
                TokenKind::IntLiteral(n) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::IntLiteral(*n)))
                }
                TokenKind::StringLiteral(s) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::StringLiteral(s.clone())))
                }
                TokenKind::BoolLiteral(b) => {
                    self.bump();
                    Ok(Pattern::Literal(Expression::BoolLiteral(*b)))
                }
                TokenKind::LParen => {
                    self.bump();
                    let mut pats = Vec::new();
                    while let Some(token) = self.peek() {
                        if token.kind == TokenKind::RParen {
                            self.bump();
                            break;
                        }
                        let pat = self.parse_pattern()?;
                        pats.push(pat);
                        if let Some(Token { kind: TokenKind::Comma, .. }) = self.peek() {
                            self.bump();
                        }
                    }
                    Ok(Pattern::Tuple(pats))
                }
                _ => Err(ParseError::UnexpectedToken(token.kind.clone())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
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

                    
