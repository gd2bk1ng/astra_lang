//! Runtime engine: invocation coordination and affine capability authority.
//!
//! The engine is kept separate from parsing and semantic verification. Its
//! submodules own runtime identities, invocation lifecycle, and capability
//! ledger transitions.

pub mod capability_runtime;
pub mod identity;
pub mod runtime;

pub use capability_runtime::{
    CapabilityInstanceId, CapabilityInstanceRecord, CapabilityInstanceState,
    CapabilityLedger, CapabilityReturn, CapabilityRuntimeError, CapabilityTransferRecord,
    CapabilityTransferRequest, CapabilityTransferState, TransferId,
};
pub use runtime::{
    InvocationId, InvocationState, RegistryId, RuntimeCoordinator,
    RuntimeError, RuntimeInvocationRecord,
};
