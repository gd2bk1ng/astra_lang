//! Runtime engine: invocation coordination and affine capability authority.
//!
//! The engine is kept separate from parsing and semantic verification. Its
//! submodules own runtime identities, invocation lifecycle, and capability
//! ledger transitions.

pub mod identity;
