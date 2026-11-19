//! Strongly typed identities shared by the capability runtime and invocation engine.
//!
//! These identifiers deliberately do not expose public constructors. Authority
//! identifiers are minted by the engine's bootstrap/registry paths; callers may
//! inspect stable numeric values but cannot manufacture an identity by casting.

/// Semantic kind of authority carried by a runtime capability instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CapabilityId {
    AllocateMemory,
    DispatchNetworkCommand,
    WriteFile,
    ReadFile,
    SpawnProcess,
    SelfModify,
}

impl CapabilityId {
    /// Canonical source-language spelling for this capability kind.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::AllocateMemory => "AllocateMemory",
            Self::DispatchNetworkCommand => "DispatchNetworkCommand",
            Self::WriteFile => "WriteFile",
            Self::ReadFile => "ReadFile",
            Self::SpawnProcess => "SpawnProcess",
            Self::SelfModify => "SelfModify",
        }
    }
}

/// Runtime principal that owns or receives capability instances.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CapabilityOwnerId(u64);

impl CapabilityOwnerId {
    /// Return the stable numeric representation without exposing construction.
    pub const fn value(self) -> u64 {
        self.0
    }
}
