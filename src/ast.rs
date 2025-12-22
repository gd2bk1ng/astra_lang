// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: ast.rs
//
//  Description:
//      Abstract Syntax Tree (AST) node definitions for Astra language constructs.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
// =============================================================================

/// Represents an `intent` block with optional `motive` and `action`.
#[derive(Debug)]
pub struct IntentBlock {
    /// The name of the intent, e.g. "astra"
    pub name: String,
    /// Optional motive string, e.g. "performance"
    pub motive: Option<String>,
    /// Optional action string, e.g. "optimize"
    pub action: Option<String>,
}
