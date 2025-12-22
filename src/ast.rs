// =============================================================================
// Astra Reference Compiler (ARC)
// File: ast.rs
//
// Description:
//     Abstract Syntax Tree (AST) definitions for the Astra language.
//     Represents parsed source code constructs in a structured, typed form.
//
//     AST is syntax-agnostic but includes optional type annotations for later phases.
//
// Intent:
//     - Optimize for clarity and maintainability.
//     - Provide a foundation for safe, verifiable compilation and analysis.
//     - Support temporal reasoning and intent-driven constructs.
//
// Author: Alex Roussinov
// Created: 2025-12-22
// =============================================================================

use crate::types::Type;

/// Top-level AST node representing a program item.
#[derive(Debug, Clone)]
pub enum AstNode {
    Function(FunctionDecl),
    Intent(IntentDecl),
    Rule(RuleDecl),
    // Extend as needed for other top-level constructs
}

/// Function declaration node.
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    /// Optional differentiable function marker (`@grad`)
    pub is_grad: bool,
    /// Function name
    pub name: String,
    /// Parameters with optional type annotations
    pub params: Vec<Param>,
    /// Optional return type annotation
    pub ret_type: Option<Type>,
    /// Effects declared on the function (e.g., IO, SelfModify)
    pub effects: Vec<String>,
    /// Function body block
    pub body: Block,
}

/// Intent declaration node (Astra intent layer).
#[derive(Debug, Clone)]
pub struct IntentDecl {
    /// Intent name
    pub name: String,
    /// Optional motive description
    pub motive: Option<String>,
    /// Optional action description
    pub action: Option<String>,
    // Extend with additional intent fields as needed
}

/// Rule declaration node (logic programming).
#[derive(Debug, Clone)]
pub struct RuleDecl {
    /// Rule name
    pub name: String,
    /// Parameters with optional type annotations
    pub params: Vec<Param>,
    /// Optional return type annotation
    pub ret_type: Option<Type>,
    /// Rule body block
    pub body: Block,
}

/// Function or rule parameter.
#[derive(Debug, Clone)]
pub struct Param {
    /// Parameter name
    pub name: String,
    /// Optional type annotation
    pub ty: Option<Type>,
}

/// Block of statements.
#[derive(Debug, Clone)]
pub struct Block {
    /// List of statements in the block
    pub statements: Vec<Statement>,
}

/// Statements within blocks.
#[derive(Debug, Clone)]
pub enum Statement {
    /// Expression statement
    Expr(Expression),
    /// Let binding with optional initializer
    LetBinding { name: String, expr: Option<Expression> },
    /// Return statement
    Return(Expression),
    /// Backtracking block for symbolic search
    Backtrack(Block),
    // Extend with other statement types as needed
}

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),

    /// Function call: callee(args...)
    FunctionCall { callee: Box<Expression>, args: Vec<Expression> },

    /// Code block expression
    Block(Block),

    /// Pattern matching expression: match expr { arms }
    Match { expr: Box<Expression>, arms: Vec<MatchArm> },

    /// Symbolic expression (logic variable or constraint)
    Symbolic(String),

    /// Self-modification expression: modify(target, patch)
    SelfModify { target: String, patch: Box<Expression> },

    // Extend with unary, binary, lambda, etc., as needed
}

/// Match arm: pattern => expression;
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expression,
}

/// Patterns for match expressions.
#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    Wildcard,
    Literal(Expression),
    Tuple(Vec<Pattern>),
    Constructor { name: String, args: Vec<Pattern> },
}
