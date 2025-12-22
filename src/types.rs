// =============================================================================
// Astra Reference Compiler (ARC)
// File: types.rs
//
// Description:
//     Core type system data structures for Astra language.
//     Supports linear types, capability tokens, dependent types, symbolic and differentiable types.
//
// Intent:
//     - Represent rich, hybrid types for safe and expressive AGI programming.
//     - Facilitate reasoning about ownership, effects, and temporal constraints.
//
// Author: Alex Roussinov
// Created: 2025-12-22
// =============================================================================

use crate::ast::Expression;

/// Astra type enumeration capturing all language-level types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Int(u32),   // Bit width (e.g., 32, 64)
    Float(u32), // Bit width
    String,

    /// Tensor type with shape and element data type
    Tensor(Shape, DType),

    /// Linear mutable ownership type
    Mut(Box<Type>),

    /// Shared immutable reference type
    Ref(Box<Type>),

    /// Capability token type (for effect control)
    Cap(Box<Type>),

    /// Symbolic logic variable or constraint wrapper
    Symbolic(String),

    /// Differentiable wrapper type
    Grad(Box<Type>),

    /// Dependent (light) type with variable, its type, and proposition constraint
    DepType {
        var: String,
        var_type: Box<Type>,
        prop: Expression, // Logical proposition expressing constraint
    },

    /// Named or user-defined type
    Simple(String),
}

/// Tensor shape: list of dimensions (numbers or identifiers)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Shape(pub Vec<ShapeDim>);

/// Dimension element of a tensor shape
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ShapeDim {
    Number(u32),
    Identifier(String),
}

/// Tensor element data types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DType {
    F32,
    F64,
    I32,
    I64,
    Bool,
    String,
}
