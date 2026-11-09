//! SyntraLang library.
//!
//! The library surface is intentionally small and stable: lexical and token
//! infrastructure can be consumed independently by compilers, AI tooling,
//! IDEs, agents, and Syntra Kernel components.

pub mod lexer;
pub mod tokens;

pub use lexer::{LexError, Lexer};
pub use tokens::{Span, Token, TokenKind};
