//! SyntraLang library.
//!
//! The public library surface exposes the lexical foundation independently
//! from the compiler, runtime, AI tooling, and Syntra Kernel layers.

pub mod lexer;
pub mod tokens;

pub use lexer::{LexError, Lexer};
pub use tokens::{Span, Token, TokenKind};
