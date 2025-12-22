// =============================================================================
// Astra Reference Compiler (ARC)
// File: type_checker.rs
//
// Description:
//     Type checking implementation for Astra language.
//     Enforces linear ownership, capability tokens, dependent types, and effect checking.
//
// Intent:
//     - Prioritize safety by enforcing ownership and effect rules.
//     - Provide clear error diagnostics for failed checks.
//     - Support temporal and intent-driven reasoning.
//
// Author: Alex Roussinov
// Created: 2025-12-22
// =============================================================================

use crate::ast::*;
use crate::types::*;
use crate::errors::*;
use std::collections::{HashMap, HashSet};

/// Typing context managing unrestricted and linear bindings.
pub struct TypeContext {
    /// Unrestricted context (shared refs, capabilities, type vars)
    unrestricted: HashMap<String, Type>,

    /// Linear context (exclusive mutable ownership)
    linear: HashMap<String, Type>,

    /// Track used linear variables to prevent double use
    used_linear: HashSet<String>,
}

impl TypeContext {
    /// Create a new empty typing context.
    pub fn new() -> Self {
        TypeContext {
            unrestricted: HashMap::new(),
            linear: HashMap::new(),
            used_linear: HashSet::new(),
        }
    }

    /// Lookup variable type with linear usage enforcement.
    pub fn lookup(&mut self, name: &str) -> Result<Type> {
        if let Some(ty) = self.unrestricted.get(name) {
            Ok(ty.clone())
        } else if let Some(ty) = self.linear.get(name) {
            if self.used_linear.contains(name) {
                Err(TypeError::LinearUseError {
                    var: name.to_string(),
                    span: None,
                })
            } else {
                self.used_linear.insert(name.to_string());
                Ok(ty.clone())
            }
        } else {
            Err(TypeError::UndefinedVariable {
                var: name.to_string(),
                span: None,
            })
        }
    }

    /// Add a variable binding to the context.
    pub fn add(&mut self, name: String, ty: Type) {
        match &ty {
            Type::Mut(_) => {
                self.linear.insert(name, ty);
            }
            _ => {
                self.unrestricted.insert(name, ty);
            }
        }
    }

    /// Type check an expression.
    pub fn check_expr(&mut self, expr: &Expression) -> Result<Type> {
        match expr {
            Expression::Identifier(name) => self.lookup(name),

            Expression::IntLiteral(_) => Ok(Type::Int(64)),
            Expression::FloatLiteral(_) => Ok(Type::Float(64)),
            Expression::BoolLiteral(_) => Ok(Type::Bool),
            Expression::StringLiteral(_) => Ok(Type::String),

            Expression::FunctionCall { callee, args } => {
                let callee_ty = self.check_expr(callee)?;
                // Simplified: extend for full function type checking
                match callee_ty {
                    Type::Simple(ref s) if s == "fn" => Ok(Type::Unit),
                    _ => Err(TypeError::Mismatch {
                        expected: Type::Simple("function".into()),
                        found: callee_ty,
                        span: None,
                    }),
                }
            }

            Expression::Block(block) => {
                let mut ty = Type::Unit;
                for stmt in &block.statements {
                    ty = self.check_statement(stmt)?;
                }
                Ok(ty)
            }

            Expression::Match { expr, arms } => {
                let scrutinee_ty = self.check_expr(expr)?;
                let mut arm_types = Vec::new();
                for arm in arms {
                    // TODO: pattern type checking
                    let arm_ty = self.check_expr(&arm.expr)?;
                    arm_types.push(arm_ty);
                }
                if arm_types.windows(2).all(|w| w[0] == w[1]) {
                    Ok(arm_types[0].clone())
                } else {
                    Err(TypeError::Other("Match arms have differing types".into()))
                }
            }

            Expression::Symbolic(_) => Ok(Type::Symbolic("".into())),

            Expression::SelfModify { .. } => {
                // TODO: Check capability token presence
                Ok(Type::Unit)
            }

            _ => Err(TypeError::Other("Expression type checking not implemented".into())),
        }
    }

    /// Type check a statement.
    pub fn check_statement(&mut self, stmt: &Statement) -> Result<Type> {
        match stmt {
            Statement::Expr(expr) => self.check_expr(expr),
            Statement::LetBinding { name, expr } => {
                if let Some(e) = expr {
                    let ty = self.check_expr(e)?;
                    self.add(name.clone(), ty);
                    Ok(Type::Unit)
                } else {
                    Err(TypeError::Other(
                        "Let binding without initializer not supported".into(),
                    ))
                }
            }
            Statement::Return(expr) => self.check_expr(expr),
            Statement::Backtrack(block) => {
                for stmt in &block.statements {
                    self.check_statement(stmt)?;
                }
                Ok(Type::Unit)
            }
        }
    }

    /// Type check a function declaration.
    pub fn check_function(&mut self, func: &FunctionDecl) -> Result<()> {
        let mut local_ctx = TypeContext::new();

        for param in &func.params {
            if let Some(ty) = &param.ty {
                local_ctx.add(param.name.clone(), ty.clone());
            } else {
                return Err(TypeError::Other(format!(
                    "Parameter '{}' missing type annotation",
                    param.name
                )));
            }
        }

        let ret_ty = func.ret_type.clone().unwrap_or(Type::Unit);

        let body_ty = local_ctx.check_block(&func.body)?;

        if body_ty != ret_ty {
            return Err(TypeError::Mismatch {
                expected: ret_ty,
                found: body_ty,
                span: None,
            });
        }

        // TODO: Effect checking here

        Ok(())
    }

    /// Type check a block of statements.
    pub fn check_block(&mut self, block: &Block) -> Result<Type> {
        let mut ty = Type::Unit;
        for stmt in &block.statements {
            ty = self.check_statement(stmt)?;
        }
        Ok(ty)
    }
}
