//! SyntraLang library.
//!
//! The public library surface exposes the lexical foundation independently
//! from the compiler, runtime, AI tooling, and Syntra Kernel layers.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod tokens;

// The lexer and token model now form the borrowed source boundary. Parser and
// AST modules should build on these lifetime contracts rather than introduce a
// second owned token representation.

pub use ast::*;
pub use lexer::{LexError, Lexer};
pub use parser::{ParseError, Parser, TokenCursor};
pub use tokens::{Span, Token, TokenKind};
