use crate::ast::*;
use crate::tokens::{Span, Token, TokenKind};

#[derive(Debug, Clone, Copy)]
pub struct TokenCursor<'tokens, 'src> {
    tokens: &'tokens [Token<'src>],
    position: usize,
}

impl<'tokens, 'src> TokenCursor<'tokens, 'src> {
    pub fn new(tokens: &'tokens [Token<'src>]) -> Self { Self { tokens, position: 0 } }
    pub fn position(&self) -> usize { self.position }
    pub fn current(&self) -> Option<&'tokens Token<'src>> { self.tokens.get(self.position) }
    pub fn peek(&self, distance: usize) -> Option<&'tokens Token<'src>> { self.tokens.get(self.position.saturating_add(distance)) }
    pub fn bump(&mut self) -> Option<&'tokens Token<'src>> { let t = self.current(); if t.is_some() { self.position += 1; } t }
    pub fn is_eof(&self) -> bool { self.current().map(|t| matches!(t.kind, TokenKind::Eof)).unwrap_or(true) }
    pub fn remaining(&self) -> &'tokens [Token<'src>] { self.tokens.get(self.position..).unwrap_or(&[]) }
    pub fn previous(&self) -> Option<&'tokens Token<'src>> { self.position.checked_sub(1).and_then(|i| self.tokens.get(i)) }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedEof { expected: &'static str, span: Span },
    UnexpectedToken { expected: &'static str, found: TokenKind<'a>, span: Span },
}

impl<'a> ParseError<'a> {
    fn unexpected(expected: &'static str, token: &Token<'a>) -> Self {
        Self::UnexpectedToken { expected, found: token.kind, span: token.span }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseReport<'a> {
    pub program: Program<'a>,
    pub errors: Vec<ParseError<'a>>,
}

impl<'a> ParseReport<'a> {
    pub fn is_success(&self) -> bool {
        self.errors.is_empty()
    }
}

pub struct Parser<'tokens, 'src> { cursor: TokenCursor<'tokens, 'src> }

impl<'tokens, 'src> Parser<'tokens, 'src> {
    pub fn new(tokens: &'tokens [Token<'src>]) -> Self { Self { cursor: TokenCursor::new(tokens) } }
    pub fn cursor(&self) -> TokenCursor<'tokens, 'src> { self.cursor }

    pub fn parse_program(&mut self) -> Result<Program<'src>, ParseError<'src>> {
        let start = self.current_span().map(|s| s.start).unwrap_or(0);
        let start_position = self.current_span().map(|s| (s.line, s.column)).unwrap_or((1, 1));
        let mut declarations = Vec::new();
        while !self.cursor.is_eof() {
            declarations.push(self.parse_declaration()?);
        }
        let end = self.current_span().map(|s| s.end).unwrap_or(start);
        Ok(Program {
            declarations,
            span: Span::with_pos(start, end, start_position.0, start_position.1),
        })
    }

    /// Parse as much of the program as possible, recovering at stable syntax
    /// boundaries instead of aborting after the first malformed declaration.
    ///
    /// Recovery consumes through a semicolon or a closing brace. This keeps
    /// malformed input from trapping the parser in a no-progress loop while
    /// preserving the normal fail-fast `parse_program` API.
    pub fn parse_program_recovering(&mut self) -> ParseReport<'src> {
        let start = self.current_span().map(|s| s.start).unwrap_or(0);
        let start_position = self.current_span().map(|s| (s.line, s.column)).unwrap_or((1, 1));
        let mut declarations = Vec::new();
        let mut errors = Vec::new();

        while !self.cursor.is_eof() {
            match self.parse_declaration() {
                Ok(declaration) => declarations.push(declaration),
                Err(error) => {
                    errors.push(error);
                    self.synchronize();
                }
            }
        }

        let end = self.current_span().map(|s| s.end).unwrap_or(start);
        ParseReport {
            program: Program {
                declarations,
                span: Span::with_pos(start, end, start_position.0, start_position.1),
            },
            errors,
        }
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.current() {
            match token.kind {
                TokenKind::Semicolon | TokenKind::RBrace => {
                    self.bump();
                    break;
                }
                TokenKind::Eof => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration<'src>, ParseError<'src>> {
        if self.at_word("fn") { return Ok(Declaration::Function(self.parse_function(false)?)); }
        if self.at(TokenKind::At) && self.peek_word(1, "grad") { self.bump(); self.expect_word("grad")?; return Ok(Declaration::Function(self.parse_function(true)?)); }
        if self.at_word("intent") { return Ok(Declaration::Intent(self.parse_intent()?)); }
        if self.at_word("rule") { return Ok(Declaration::Rule(self.parse_rule()?)); }
        let t = self.current().ok_or(ParseError::UnexpectedEof { expected: "declaration", span: Span::new(0,0) })?;
        Err(ParseError::unexpected("fn, intent, rule, or @grad", t))
    }

    fn parse_function(&mut self, grad: bool) -> Result<Function<'src>, ParseError<'src>> {
        let start = self.expect_word("fn")?.span.start;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::LParen, "(")?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen, ")")?;
        let return_type = if self.at(TokenKind::Arrow) { self.bump(); Some(self.parse_type()?) } else { None };
        let body = self.parse_block()?;
        Ok(Function { name, params, return_type, body, span: Span::with_pos(start, body.span.end, 1, 1), grad })
    }

    fn parse_rule(&mut self) -> Result<Rule<'src>, ParseError<'src>> {
        let start = self.expect_word("rule")?.span.start;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::LParen, "(")?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen, ")")?;
        let return_type = if self.at(TokenKind::Arrow) { self.bump(); Some(self.parse_type()?) } else { None };
        let body = self.parse_block()?;
        Ok(Rule { name, params, return_type, body, span: Span::with_pos(start, body.span.end, 1, 1) })
    }

    fn parse_intent(&mut self) -> Result<Intent<'src>, ParseError<'src>> {
        let start = self.expect_word("intent")?.span.start;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::LBrace, "{")?;
        let mut members = Vec::new();
        while !self.at(TokenKind::RBrace) {
            let member_start = self.current_span().map(|s| s.start).unwrap_or(start);
            let member_name = self.expect_identifier()?;
            let value = match self.current().map(|t| t.kind) {
                Some(TokenKind::StringLiteral { raw }) => { self.bump(); IntentValue::String(raw) }
                Some(TokenKind::LBrace) => IntentValue::Block(self.parse_block()?),
                _ => { let t = self.current().ok_or(ParseError::UnexpectedEof { expected: "intent member", span: Span::new(member_start, member_start) })?; return Err(ParseError::unexpected("string literal or block", t)); }
            };
            let end = match &value { IntentValue::String(_) => self.previous_end().unwrap_or(member_start), IntentValue::Block(b) => b.span.end };
            members.push(IntentMember { name: member_name, value, span: Span::with_pos(member_start, end, 1, 1) });
        }
        let close = self.expect(TokenKind::RBrace, "}")?;
        Ok(Intent { name, members, span: Span::with_pos(start, close.span.end, 1, 1) })
    }

    fn parse_params(&mut self) -> Result<Vec<Parameter<'src>>, ParseError<'src>> {
        let mut params = Vec::new();
        if self.at(TokenKind::RParen) { return Ok(params); }
        loop {
            let start = self.current_span().map(|s| s.start).unwrap_or(0);
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon, ":")?;
            let ty = self.parse_type()?;
            let end = ty.span.end;
            params.push(Parameter { name, ty, span: Span::with_pos(start, end, 1, 1) });
            if self.at(TokenKind::Comma) { self.bump(); if self.at(TokenKind::RParen) { break; } } else { break; }
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<TypeRef<'src>, ParseError<'src>> {
        let start = self.current_span().map(|s| s.start).unwrap_or(0);
        let name = match self.current() {
            Some(token) => match token.kind {
                TokenKind::Identifier(name) => { self.bump(); name }
                TokenKind::IntLiteral(_) => { let lexeme = token.lexeme; self.bump(); lexeme }
                TokenKind::FloatLiteral(_) => { let lexeme = token.lexeme; self.bump(); lexeme }
                _ => return Err(ParseError::unexpected("type or type argument", token)),
            },
            None => return Err(ParseError::UnexpectedEof { expected: "type", span: Span::new(0, 0) }),
        };
        let mut arguments = Vec::new();
        if self.at(TokenKind::LessThan) {
            self.bump();
            if !self.at(TokenKind::GreaterThan) {
                loop { arguments.push(self.parse_type()?); if self.at(TokenKind::Comma) { self.bump(); continue; } break; }
            }
            self.expect(TokenKind::GreaterThan, ">")?;
        }
        let end = self.previous_end().unwrap_or(start);
        Ok(TypeRef { name, arguments, span: Span::with_pos(start, end, 1, 1) })
    }

    fn parse_block(&mut self) -> Result<Block<'src>, ParseError<'src>> {
        let open = self.expect(TokenKind::LBrace, "{")?;
        let mut statements = Vec::new();
        while !self.at(TokenKind::RBrace) {
            if self.cursor.is_eof() { return Err(ParseError::UnexpectedEof { expected: "}", span: open.span }); }
            statements.push(self.parse_statement()?);
        }
        let close = self.expect(TokenKind::RBrace, "}")?;
        Ok(Block { statements, span: Span::with_pos(open.span.start, close.span.end, open.span.line, open.span.column) })
    }

    fn parse_statement(&mut self) -> Result<Statement<'src>, ParseError<'src>> {
        if self.at_word("let") {
            let start = self.expect_word("let")?.span.start; let name = self.expect_identifier()?;
            let value = if self.at(TokenKind::Equal) { self.bump(); Some(self.parse_expression()?) } else { None };
            let semi = self.expect(TokenKind::Semicolon, ";")?;
            return Ok(Statement::Let { name, value, span: Span::with_pos(start, semi.span.end, 1, 1) });
        }
        if self.at_word("return") {
            let start = self.expect_word("return")?.span.start; let value = self.parse_expression()?;
            let semi = self.expect(TokenKind::Semicolon, ";")?;
            return Ok(Statement::Return { value, span: Span::with_pos(start, semi.span.end, 1, 1) });
        }
        if self.at_word("backtrack") {
            let start = self.expect_word("backtrack")?.span.start; let body = self.parse_block()?;
            return Ok(Statement::Backtrack { span: Span::with_pos(start, body.span.end, 1, 1), body });
        }
        let start = self.current_span().map(|s| s.start).unwrap_or(0); let expression = self.parse_expression()?;
        let semi = self.expect(TokenKind::Semicolon, ";")?;
        Ok(Statement::Expression { expression, span: Span::with_pos(start, semi.span.end, 1, 1) })
    }

    fn parse_expression(&mut self) -> Result<Expression<'src>, ParseError<'src>> { self.parse_binary(0) }

    fn parse_binary(&mut self, min: u8) -> Result<Expression<'src>, ParseError<'src>> {
        let mut left = self.parse_unary()?;
        loop {
            let Some((op, precedence)) = self.binary_operator() else { break };
            if precedence < min { break; }
            self.bump(); let right = self.parse_binary(precedence + 1)?;
            let span = Span::with_pos(left.span().start, right.span().end, 1, 1);
            left = Expression::Binary { left: Box::new(left), operator: op, right: Box::new(right), span };
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let op = match self.current().map(|t| t.kind) { Some(TokenKind::Minus) => Some(UnaryOp::Negate), Some(TokenKind::Bang) => Some(UnaryOp::Not), _ => None };
        if let Some(operator) = op { let start = self.current_span().unwrap().start; self.bump(); let operand = self.parse_unary()?; let span = Span::with_pos(start, operand.span().end, 1, 1); return Ok(Expression::Unary { operator, operand: Box::new(operand), span }); }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let mut expression = self.parse_primary()?;
        while self.at(TokenKind::LParen) {
            self.bump(); let mut arguments = Vec::new();
            if !self.at(TokenKind::RParen) { loop { arguments.push(self.parse_expression()?); if self.at(TokenKind::Comma) { self.bump(); continue; } break; } }
            let close = self.expect(TokenKind::RParen, ")")?; let span = Span::with_pos(expression.span().start, close.span.end, 1, 1);
            expression = Expression::Call { callee: Box::new(expression), arguments, span };
        }
        Ok(expression)
    }

    fn parse_primary(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let token = self.current().ok_or(ParseError::UnexpectedEof { expected: "expression", span: Span::new(0,0) })?;
        match token.kind {
            TokenKind::Identifier(name) if name == "symbolic" && self.peek_kind(1) == Some(TokenKind::LParen) => {
                let start = token.span.start; self.bump(); self.expect(TokenKind::LParen, "(")?; let id = self.expect_identifier()?; let close = self.expect(TokenKind::RParen, ")")?;
                Ok(Expression::Symbolic { identifier: id, span: Span::with_pos(start, close.span.end, 1, 1) })
            }
            TokenKind::Identifier(name) if name == "modify" && self.peek_kind(1) == Some(TokenKind::LParen) => {
                let start = token.span.start; self.bump(); self.expect(TokenKind::LParen, "(")?; let target = self.expect_identifier()?; self.expect(TokenKind::Comma, ",")?; let patch = self.parse_expression()?; let close = self.expect(TokenKind::RParen, ")")?;
                Ok(Expression::SelfModify { target, patch: Box::new(patch), span: Span::with_pos(start, close.span.end, 1, 1) })
            }
            TokenKind::Identifier(name) => { let span = token.span; self.bump(); Ok(Expression::Identifier(name, span)) }
            TokenKind::IntLiteral(v) => { let span = token.span; self.bump(); Ok(Expression::Literal(Literal::Integer(v), span)) }
            TokenKind::FloatLiteral(v) => { let span = token.span; self.bump(); Ok(Expression::Literal(Literal::Float(v), span)) }
            TokenKind::StringLiteral { raw } => { let span = token.span; self.bump(); Ok(Expression::Literal(Literal::String(raw), span)) }
            TokenKind::BoolLiteral(v) => { let span = token.span; self.bump(); Ok(Expression::Literal(Literal::Boolean(v), span)) }
            TokenKind::LBrace => Ok(Expression::Block(self.parse_block()?)),
            _ => Err(ParseError::unexpected("expression", token)),
        }
    }

    fn binary_operator(&self) -> Option<(BinaryOp, u8)> {
        match self.current().map(|t| t.kind) {
            Some(TokenKind::OrOr) => Some((BinaryOp::Or,1)), Some(TokenKind::AndAnd) => Some((BinaryOp::And,2)),
            Some(TokenKind::EqualEqual) => Some((BinaryOp::Equal,3)), Some(TokenKind::NotEqual) => Some((BinaryOp::NotEqual,3)),
            Some(TokenKind::LessThan) => Some((BinaryOp::Less,4)), Some(TokenKind::LessEqual) => Some((BinaryOp::LessEqual,4)),
            Some(TokenKind::GreaterThan) => Some((BinaryOp::Greater,4)), Some(TokenKind::GreaterEqual) => Some((BinaryOp::GreaterEqual,4)),
            Some(TokenKind::Plus) => Some((BinaryOp::Add,5)), Some(TokenKind::Minus) => Some((BinaryOp::Subtract,5)),
            Some(TokenKind::Star) => Some((BinaryOp::Multiply,6)), Some(TokenKind::Slash) => Some((BinaryOp::Divide,6)), _ => None,
        }
    }

    fn expect_identifier(&mut self) -> Result<&'src str, ParseError<'src>> {
        let t = self.current().ok_or(ParseError::UnexpectedEof { expected: "identifier", span: Span::new(0,0) })?;
        match t.kind { TokenKind::Identifier(name) => { self.bump(); Ok(name) }, _ => Err(ParseError::unexpected("identifier", t)) }
    }
    fn expect_word(&mut self, word: &'static str) -> Result<&'tokens Token<'src>, ParseError<'src>> {
        let t = self.current().ok_or(ParseError::UnexpectedEof { expected: word, span: Span::new(0,0) })?;
        if matches!(t.kind, TokenKind::Identifier(v) if v == word) { return Ok(self.bump().unwrap()); }
        Err(ParseError::unexpected(word, t))
    }
    fn expect(&mut self, kind: TokenKind<'src>, expected: &'static str) -> Result<&'tokens Token<'src>, ParseError<'src>> {
        let t = self.current().ok_or(ParseError::UnexpectedEof { expected, span: Span::new(0,0) })?;
        if t.kind == kind { return Ok(self.bump().unwrap()); } Err(ParseError::unexpected(expected, t))
    }
    fn at(&self, kind: TokenKind<'src>) -> bool { self.current().is_some_and(|t| t.kind == kind) }
    fn at_word(&self, word: &str) -> bool { self.current().is_some_and(|t| matches!(t.kind, TokenKind::Identifier(v) if v == word)) }
    fn peek_word(&self, n: usize, word: &str) -> bool { self.peek(n).is_some_and(|t| matches!(t.kind, TokenKind::Identifier(v) if v == word)) }
    fn peek_kind(&self, n: usize) -> Option<TokenKind<'src>> { self.peek(n).map(|t| t.kind) }
    fn current_span(&self) -> Option<Span> { self.current().map(|t| t.span) }
    fn previous_end(&self) -> Option<usize> { self.cursor.previous().map(|t| t.span.end) }
    fn bump(&mut self) -> Option<&'tokens Token<'src>> { self.cursor.bump() }

    fn current(&self) -> Option<&'tokens Token<'src>> { self.cursor.current() }
}

trait ExpressionSpan { fn span(&self) -> Span; }
impl ExpressionSpan for Expression<'_> {
    fn span(&self) -> Span {
        match self {
            Expression::Identifier(_, s) | Expression::Literal(_, s) | Expression::Symbolic { span: s, .. } | Expression::SelfModify { span: s, .. } | Expression::Unary { span: s, .. } | Expression::Binary { span: s, .. } | Expression::Call { span: s, .. } => *s,
            Expression::Block(b) => b.span,
        }
    }
}
