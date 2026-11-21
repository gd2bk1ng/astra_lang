//! Structurally validated SyntraLang programs.
//!
//! `ValidatedProgram` is the type-level boundary between the raw AST and
//! semantic verification. It can only be constructed inside the crate after
//! structural AST invariant validation has completed successfully.

use crate::ast::Program;

/// A SyntraLang program that has passed structural AST invariant validation.
///
/// The contained AST and source are immutable through this API. Construction
/// is restricted to the crate so callers cannot manufacture a validated
/// program without first passing the structural validator.
#[derive(Debug, PartialEq)]
pub struct ValidatedProgram<'src> {
    program: Program<'src>,
    source: &'src str,
}

impl<'src> ValidatedProgram<'src> {
    pub(crate) fn new(program: Program<'src>, source: &'src str) -> Self {
        Self { program, source }
    }

    pub fn program(&self) -> &Program<'src> {
        &self.program
    }

    pub fn source(&self) -> &'src str {
        self.source
    }
}
