//! SyntraLang library.

//!
//! The public library surface exposes the lexical foundation independently
//! from the compiler, runtime, AI tooling, and Syntra Kernel layers.

pub mod ast;
pub mod diagnostics;
pub mod engine;
pub mod epistemic;
pub mod invariants;
pub mod lexer;
pub mod parser;
pub mod tokens;
pub mod verify;
pub mod validated;

// The lexer and token model now form the borrowed source boundary. Parser and
// AST modules should build on these lifetime contracts rather than introduce
// a second owned token representation.

pub use ast::*;
pub use engine::identity::{CapabilityId, CapabilityOwnerId};
pub use epistemic::*;
pub use invariants::*;
pub use lexer::{LexError, Lexer};
pub use diagnostics::{Diagnostic, DiagnosticEngine, DiagnosticSeverity};
pub use parser::{ParseError, ParseReport, Parser, TokenCursor};
pub use tokens::{Span, Token, TokenKind};
pub use verify::{
    Capability, CapabilityError, CapabilityLedger, SemanticReport,
    SemanticType, SemanticVerifier, VerifiedProgram,
};
pub use validated::ValidatedProgram;
