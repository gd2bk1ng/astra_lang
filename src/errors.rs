// =============================================================================
// Astra Reference Compiler (ARC)
// File: errors.rs
//
// Description:
//     Error types and diagnostic utilities for parsing and type checking.
//
// Intent:
//     - Provide clear, actionable error messages.
//     - Support extensible error categories for future tooling.
//
// Author: Alex Roussinov
// Created: 2025-12-22
// =============================================================================

use std::fmt;

/// Represents a span in source code (optional).
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// Parsing errors.
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(String),
    ExpectedToken(String),
    Custom(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::UnexpectedToken(tok) => write!(f, "Unexpected token: {}", tok),
            ParseError::ExpectedToken(tok) => write!(f, "Expected token: {}", tok),
            ParseError::Custom(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

/// Type checking errors.
#[derive(Debug)]
pub enum TypeError {
    Mismatch {
        expected: crate::types::Type,
        found: crate::types::Type,
        span: Option<Span>,
    },
    LinearUseError {
        var: String,
        span: Option<Span>,
    },
    UndefinedVariable {
        var: String,
        span: Option<Span>,
    },
    EffectMismatch {
        expected: Vec<String>,
        found: Vec<String>,
        span: Option<Span>,
    },
    DependentConstraintFailed {
        var: String,
        prop: crate::ast::Expression,
        span: Option<Span>,
    },
    Other(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::Mismatch { expected, found, .. } => {
                write!(f, "Type mismatch: expected {:?}, found {:?}", expected, found)
            }
            TypeError::LinearUseError { var, .. } => {
                write!(f, "Linear variable '{}' used more than once", var)
            }
            TypeError::UndefinedVariable { var, .. } => {
                write!(f, "Undefined variable '{}'", var)
            }
            TypeError::EffectMismatch { expected, found, .. } => {
                write!(
                    f,
                    "Effect mismatch: expected {:?}, found {:?}",
                    expected, found
                )
            }
            TypeError::DependentConstraintFailed { var, prop, .. } => {
                write!(f, "Dependent constraint failed on '{}': {:?}", var, prop)
            }
            TypeError::Other(msg) => write!(f, "Type error: {}", msg),
        }
    }
}
