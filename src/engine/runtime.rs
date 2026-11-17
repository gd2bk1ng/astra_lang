//! Invocation registry and coordinator boundary for the runtime engine.
//!
//! The registry owns invocation identities and lifecycle state. Mutation is
//! deliberately private: callers interact through RuntimeCoordinator, which
//! holds the unforgeable coordinator privilege token.

use std::collections::BTreeMap;

use super::capability_runtime::{
    CapabilityInstanceId, CapabilityLedger, CapabilityRuntimeError, CapabilityTransferRequest,
    CapabilityTransferRecord, CapabilityReturn,
};
use super::identity::CapabilityOwnerId;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_REGISTRY_ID: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegistryId(u64);

impl RegistryId {
    pub const fn value(self) -> u64 { self.0 }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvocationId {
    registry: RegistryId,
    sequence: u64,
}

impl InvocationId {
    #[cfg(test)]
    pub(crate) const fn new_for_test(sequence: u64) -> Self {
        Self {
            registry: RegistryId(0),
            sequence,
        }
    }

    /// Return the registry-qualified sequence number.
    pub const fn value(self) -> u64 { self.sequence }

    /// Return the registry that minted this invocation identity.
    pub const fn registry_id(self) -> RegistryId { self.registry }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvocationState {
    Prepared,
    Committed,
    Executing,
    Returned,
    Abandoned,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    RegistryIdExhausted,
    InvocationIdExhausted,
    RegistryVersionExhausted,
    OwnerIdExhausted,
    UnknownInvocation(InvocationId),
    ForeignInvocation { expected_registry: RegistryId, actual_registry: RegistryId },
    InvalidTransition { from: InvocationState, to: InvocationState },
    ParentNotExecuting(InvocationId),
    ParentIdNotLessThanChild { parent: InvocationId, child: InvocationId },
    NonTerminalDescendant(InvocationId),
    RegistryInvariantViolation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeInvocationRecord {
    id: InvocationId,
    parent: Option<InvocationId>,
    owner: CapabilityOwnerId,
    state: InvocationState,
}

impl RuntimeInvocationRecord {
    pub const fn id(&self) -> InvocationId { self.id }
    pub const fn parent(&self) -> Option<InvocationId> { self.parent }
    pub const fn owner(&self) -> CapabilityOwnerId { self.owner }
    pub const fn state(&self) -> InvocationState { self.state }
}

#[derive(Debug)]
struct CoordinatorPrivilegeToken {
    _private: (),
}

impl CoordinatorPrivilegeToken {
    fn new() -> Self { Self { _private: () } }
}

#[derive(Debug)]
struct RuntimeInvocationRegistry {
    id: RegistryId,
    next_invocation_id: Option<u64>,
    version: u64,
    records: BTreeMap<InvocationId, RuntimeInvocationRecord>,
}

impl RuntimeInvocationRegistry {
    fn new() -> Result<Self, RuntimeError> {
        let id = NEXT_REGISTRY_ID
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| next.checked_add(1))
            .map_err(|_| RuntimeError::RegistryIdExhausted)?;
        Ok(Self {
            id: RegistryId(id),
            next_invocation_id: Some(1),
            version: 0,
            records: BTreeMap::new(),
        })
    }

    fn allocate_id(&mut self) -> Result<InvocationId, RuntimeError> {
        let value = self.next_invocation_id.ok_or(RuntimeError::InvocationIdExhausted)?;
        // Preserve the full u64 identity space: None means the maximum ID was
        // already issued and no further invocation can be allocated.
        self.next_invocation_id = value.checked_add(1);
        Ok(InvocationId { registry: self.id, sequence: value })
    }

    fn prepare(
        &mut self,
        _authority: &CoordinatorPrivilegeToken,
        parent: Option<InvocationId>,
        owner: CapabilityOwnerId,
    ) -> Result<InvocationId, RuntimeError> {
        self.validate_structure()?;
        let next_version = self.version.checked_add(1)
            .ok_or(RuntimeError::RegistryVersionExhausted)?;
        if let Some(parent_id) = parent {
            if parent_id.registry != self.id {
                return Err(RuntimeError::ForeignInvocation {
                    expected_registry: self.id,
                    actual_registry: parent_id.registry,
                });
            }
            let parent_record = self.records.get(&parent_id)
                .ok_or(RuntimeError::UnknownInvocation(parent_id))?;
            if parent_record.state != InvocationState::Executing {
                return Err(RuntimeError::ParentNotExecuting(parent_id));
            }
        }

        let id = self.allocate_id()?;
        if let Some(parent_id) = parent {
            if parent_id >= id {
                return Err(RuntimeError::ParentIdNotLessThanChild { parent: parent_id, child: id });
            }
        }

        self.records.insert(id, RuntimeInvocationRecord {
            id,
            parent,
            owner,
            state: InvocationState::Prepared,
        });
        self.version = next_version;
        Ok(id)
    }

    fn transition(
        &mut self,
        _authority: &CoordinatorPrivilegeToken,
        id: InvocationId,
        to: InvocationState,
    ) -> Result<(), RuntimeError> {
        self.validate_structure()?;
        if id.registry != self.id {
            return Err(RuntimeError::ForeignInvocation {
                expected_registry: self.id,
                actual_registry: id.registry,
            });
        }
        let record = self.records.get(&id).ok_or(RuntimeError::UnknownInvocation(id))?;
        let from = record.state;
        let legal = matches!(
            (from, to),
            (InvocationState::Prepared, InvocationState::Committed)
                | (InvocationState::Committed, InvocationState::Executing)
                | (InvocationState::Executing, InvocationState::Returned)
                | (InvocationState::Executing, InvocationState::Abandoned)
        );
        if !legal {
            return Err(RuntimeError::InvalidTransition { from, to });
        }

        if matches!(to, InvocationState::Returned | InvocationState::Abandoned) {
            if let Some(descendant) = self.first_nonterminal_descendant(id) {
                return Err(RuntimeError::NonTerminalDescendant(descendant));
            }
        }
        let next_version = self.version.checked_add(1)
            .ok_or(RuntimeError::RegistryVersionExhausted)?;

        let Some(record) = self.records.get_mut(&id) else {
            return Err(RuntimeError::UnknownInvocation(id));
        };
        record.state = to;
        self.version = next_version;
        Ok(())
    }


    /// Check the registry's complete parent graph before any lifecycle mutation.
    /// Public APIs maintain these invariants; the full scan also makes future
    /// staged/replacement-state integration fail closed on malformed ancestry.
    fn validate_structure(&self) -> Result<(), RuntimeError> {
        for (key, record) in &self.records {
            if *key != record.id
                || key.registry != self.id
                || record.id.registry != self.id
            {
                return Err(RuntimeError::RegistryInvariantViolation);
            }

            let mut visited = std::collections::BTreeSet::new();
            visited.insert(record.id);
            let mut cursor = record.parent;
            while let Some(parent_id) = cursor {
                if parent_id.registry != self.id
                    || parent_id >= record.id
                    || !visited.insert(parent_id)
                {
                    return Err(RuntimeError::RegistryInvariantViolation);
                }
                let Some(parent) = self.records.get(&parent_id) else {
                    return Err(RuntimeError::RegistryInvariantViolation);
                };
                let parent_terminal = matches!(
                    parent.state,
                    InvocationState::Returned | InvocationState::Abandoned
                );
                let record_terminal = matches!(
                    record.state,
                    InvocationState::Returned | InvocationState::Abandoned
                );
                if (parent.state != InvocationState::Executing && !parent_terminal)
                    || (parent_terminal && !record_terminal)
                {
                    return Err(RuntimeError::RegistryInvariantViolation);
                }
                cursor = parent.parent;
            }
        }
        Ok(())
    }

    fn first_nonterminal_descendant(&self, ancestor: InvocationId) -> Option<InvocationId> {
        self.records.values().find_map(|candidate| {
            if candidate.state == InvocationState::Returned || candidate.state == InvocationState::Abandoned {
                return None;
            }
            let mut cursor = candidate.parent;
            while let Some(parent) = cursor {
                if parent == ancestor { return Some(candidate.id); }
                cursor = self.records.get(&parent).and_then(|record| record.parent);
            }
            None
        })
    }
}

/// Sole public authority for creating invocations and advancing their lifecycle.
#[derive(Debug)]
pub struct RuntimeCoordinator {
    registry: RuntimeInvocationRegistry,
    authority: CoordinatorPrivilegeToken,
    capability_ledger: CapabilityLedger,
}

impl RuntimeCoordinator {
    pub fn new() -> Result<Self, RuntimeError> {
        Ok(Self {
            registry: RuntimeInvocationRegistry::new()?,
            authority: CoordinatorPrivilegeToken::new(),
            capability_ledger: CapabilityLedger::new(),
        })
    }

    pub const fn registry_id(&self) -> RegistryId { self.registry.id }

    pub fn prepare_root(&mut self) -> Result<InvocationId, RuntimeError> {
        self.registry.validate_structure()?;
        if self.registry.next_invocation_id.is_none() {
            return Err(RuntimeError::InvocationIdExhausted);
        }
        if self.registry.version.checked_add(1).is_none() {
            return Err(RuntimeError::RegistryVersionExhausted);
        }

        let owner = CapabilityOwnerId::mint().ok_or(RuntimeError::OwnerIdExhausted)?;
        self.registry.prepare(&self.authority, None, owner)
    }

    pub fn prepare_child(&mut self, parent: InvocationId) -> Result<InvocationId, RuntimeError> {
        if parent.registry != self.registry.id {
            return Err(RuntimeError::ForeignInvocation {
                expected_registry: self.registry.id,
                actual_registry: parent.registry,
            });
        }

        let owner = self
            .registry
            .records
            .get(&parent)
            .ok_or(RuntimeError::UnknownInvocation(parent))?
            .owner;
        self.registry.prepare(&self.authority, Some(parent), owner)
    }

    pub fn commit(&mut self, id: InvocationId) -> Result<(), RuntimeError> {
        self.registry.transition(&self.authority, id, InvocationState::Committed)
    }

    pub fn begin_execution(&mut self, id: InvocationId) -> Result<(), RuntimeError> {
        self.registry.transition(&self.authority, id, InvocationState::Executing)
    }

    pub fn return_invocation(&mut self, id: InvocationId) -> Result<(), RuntimeError> {
        self.registry.transition(&self.authority, id, InvocationState::Returned)
    }

    pub fn abandon_invocation(&mut self, id: InvocationId) -> Result<(), RuntimeError> {
        self.registry.transition(&self.authority, id, InvocationState::Abandoned)
    }

    pub fn invocation(&self, id: InvocationId) -> Option<&RuntimeInvocationRecord> {
        self.registry.records.get(&id)
    }

    pub fn capability_instance(
        &self,
        id: CapabilityInstanceId,
    ) -> Option<&super::capability_runtime::CapabilityInstanceRecord> {
        self.capability_ledger.instance(id)
    }

    pub fn capability_transfer(
        &self,
        id: super::capability_runtime::TransferId,
    ) -> Option<&CapabilityTransferRecord> {
        self.capability_ledger.transfer(id)
    }

    pub fn capability_ledger_version(&self) -> u64 {
        self.capability_ledger.version()
    }

    pub fn grant_capability(
        &mut self,
        invocation: InvocationId,
        kind: super::identity::CapabilityId,
    ) -> Result<CapabilityInstanceId, CapabilityRuntimeError> {
        let owner = self
            .registry
            .records
            .get(&invocation)
            .filter(|record| record.state == InvocationState::Executing)
            .map(|record| record.owner)
            .ok_or(CapabilityRuntimeError::InvalidInvocationState(invocation))?;
        self.capability_ledger.grant(owner, kind)
    }

    pub fn transfer_batch(
        &mut self,
        source: InvocationId,
        requests: &[CapabilityTransferRequest],
    ) -> Result<Vec<super::capability_runtime::TransferId>, CapabilityRuntimeError> {
        let snapshot = self
            .registry
            .records
            .iter()
            .map(|(&id, record)| (id, (record.owner, record.state)))
            .collect::<BTreeMap<_, _>>();

        self.capability_ledger
            .transfer_batch(source, requests, |id| snapshot.get(&id).copied())
    }

    pub fn consume_capability(
        &mut self,
        invocation: InvocationId,
        capability: CapabilityInstanceId,
    ) -> Result<(), CapabilityRuntimeError> {
        let snapshot = self
            .registry
            .records
            .iter()
            .map(|(&id, record)| (id, record.owner))
            .collect::<BTreeMap<_, _>>();

        self.capability_ledger
            .consume(invocation, capability, |id| snapshot.get(&id).copied())
    }

    pub fn return_capability_transfer(
        &mut self,
        transfer: super::capability_runtime::TransferId,
    ) -> Result<CapabilityReturn, CapabilityRuntimeError> {
        let snapshot = self
            .registry
            .records
            .iter()
            .map(|(&id, record)| (id, record.state))
            .collect::<BTreeMap<_, _>>();

        self.capability_ledger
            .return_transfer(transfer, |id| snapshot.get(&id).copied())
    }

    pub fn invocations(&self) -> impl Iterator<Item = &RuntimeInvocationRecord> {
        self.registry.records.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn executing_root(coordinator: &mut RuntimeCoordinator) -> InvocationId {
        let root = coordinator.prepare_root().unwrap();
        coordinator.commit(root).unwrap();
        coordinator.begin_execution(root).unwrap();
        root
    }

    #[test]
    fn lifecycle_follows_only_the_declared_forward_edges() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = coordinator.prepare_root().unwrap();
        assert!(matches!(
            coordinator.begin_execution(root),
            Err(RuntimeError::InvalidTransition { .. })
        ));
        coordinator.commit(root).unwrap();
        coordinator.begin_execution(root).unwrap();
        coordinator.return_invocation(root).unwrap();
        assert_eq!(coordinator.invocation(root).unwrap().state(), InvocationState::Returned);
        assert!(matches!(
            coordinator.abandon_invocation(root),
            Err(RuntimeError::InvalidTransition { .. })
        ));
    }

    #[test]
    fn child_ids_are_monotonic_and_parent_must_be_executing() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = coordinator.prepare_root().unwrap();
        assert!(matches!(
            coordinator.prepare_child(root),
            Err(RuntimeError::ParentNotExecuting(_))
        ));
        coordinator.commit(root).unwrap();
        coordinator.begin_execution(root).unwrap();
        let child = coordinator.prepare_child(root).unwrap();
        assert!(child.value() > root.value());
        assert_eq!(coordinator.invocation(child).unwrap().parent(), Some(root));
    }


    #[test]
    fn lifecycle_mutation_rejects_forged_or_missing_ancestry() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        let child = coordinator.prepare_child(root).unwrap();

        coordinator.registry.records.get_mut(&child).unwrap().parent =
            Some(InvocationId { registry: coordinator.registry.id, sequence: u64::MAX });

        assert_eq!(
            coordinator.commit(child),
            Err(RuntimeError::RegistryInvariantViolation)
        );
        assert_eq!(
            coordinator.invocation(child).unwrap().state(),
            InvocationState::Prepared
        );
    }

    #[test]
    fn parent_cannot_terminate_before_nonterminal_descendants() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        let child = coordinator.prepare_child(root).unwrap();
        coordinator.commit(child).unwrap();
        coordinator.begin_execution(child).unwrap();
        assert_eq!(
            coordinator.return_invocation(root),
            Err(RuntimeError::NonTerminalDescendant(child))
        );
        coordinator.return_invocation(child).unwrap();
        coordinator.return_invocation(root).unwrap();
    }


    #[test]
    fn invocation_allocator_issues_maximum_id_once_then_exhausts() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        coordinator.registry.next_invocation_id = Some(u64::MAX);

        let final_id = coordinator.prepare_root().unwrap();
        assert_eq!(final_id.value(), u64::MAX);
        assert_eq!(
            coordinator.prepare_root(),
            Err(RuntimeError::InvocationIdExhausted)
        );
        assert_eq!(coordinator.invocations().count(), 1);
    }

    #[test]
    fn invocation_ids_are_bound_to_the_registry_that_minted_them() {
        let mut first = RuntimeCoordinator::new().unwrap();
        let mut second = RuntimeCoordinator::new().unwrap();

        let first_root = first.prepare_root().unwrap();
        let second_root = second.prepare_root().unwrap();

        // Each registry starts its local sequence at one. The registry
        // component prevents these otherwise-colliding IDs from aliasing.
        assert_eq!(first_root.value(), second_root.value());
        assert_ne!(first_root.registry_id(), second_root.registry_id());
        assert_ne!(first_root, second_root);

        assert!(matches!(
            second.commit(first_root),
            Err(RuntimeError::ForeignInvocation {
                expected_registry,
                actual_registry,
            }) if expected_registry == second.registry_id()
                && actual_registry == first.registry_id()
        ));
        assert!(matches!(
            second.prepare_child(first_root),
            Err(RuntimeError::ForeignInvocation { .. })
        ));
        assert!(second.invocation(first_root).is_none());

        // A foreign ID must not mutate the receiving registry's own record.
        assert_eq!(
            second.invocation(second_root).unwrap().state(),
            InvocationState::Prepared
        );
    }


    #[test]
    fn registry_version_advances_only_after_successful_mutations() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        assert_eq!(coordinator.registry.version, 0);

        let root = coordinator.prepare_root().unwrap();
        assert_eq!(coordinator.registry.version, 1);

        assert!(matches!(
            coordinator.begin_execution(root),
            Err(RuntimeError::InvalidTransition { .. })
        ));
        assert_eq!(coordinator.registry.version, 1);

        coordinator.commit(root).unwrap();
        assert_eq!(coordinator.registry.version, 2);
        coordinator.begin_execution(root).unwrap();
        assert_eq!(coordinator.registry.version, 3);
        coordinator.return_invocation(root).unwrap();
        assert_eq!(coordinator.registry.version, 4);
    }

    #[test]
    fn terminal_parent_cannot_have_a_nonterminal_child() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        let child = coordinator.prepare_child(root).unwrap();
        coordinator.commit(child).unwrap();
        coordinator.begin_execution(child).unwrap();

        // Simulate corrupted internal state. Every public mutation must reject
        // the malformed snapshot before changing the child.
        coordinator.registry.records.get_mut(&root).unwrap().state =
            InvocationState::Returned;

        assert_eq!(
            coordinator.return_invocation(child),
            Err(RuntimeError::RegistryInvariantViolation)
        );
        assert_eq!(
            coordinator.invocation(child).unwrap().state(),
            InvocationState::Executing
        );
    }

    #[test]
    fn invocation_owners_are_engine_minted_and_child_inherits_parent_authority() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        let child = coordinator.prepare_child(root).unwrap();

        assert_ne!(coordinator.invocation(root).unwrap().owner(),
                   CapabilityOwnerId::new_for_test(0));
        assert_eq!(
            coordinator.invocation(child).unwrap().owner(),
            coordinator.invocation(root).unwrap().owner()
        );
    }

    #[test]
    fn coordinator_controls_atomic_capability_transfer_and_consumption() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        let child = coordinator.prepare_child(root).unwrap();
        coordinator.commit(child).unwrap();
        coordinator.begin_execution(child).unwrap();

        let capability = coordinator
            .grant_capability(root, super::super::identity::CapabilityId::ReadFile)
            .unwrap();

        let transfer = coordinator
            .transfer_batch(
                root,
                &[CapabilityTransferRequest::new(capability, child)],
            )
            .unwrap();

        assert_eq!(transfer.len(), 1);
        assert!(matches!(
            coordinator.capability_instance(capability).unwrap().state(),
            super::super::capability_runtime::CapabilityInstanceState::InTransit { .. }
        ));

        coordinator.consume_capability(child, capability).unwrap();
        assert_eq!(
            coordinator.capability_instance(capability).unwrap().state(),
            super::super::capability_runtime::CapabilityInstanceState::Consumed
        );
        assert_eq!(
            coordinator.consume_capability(child, capability),
            Err(CapabilityRuntimeError::CapabilityNotAvailable(capability))
        );
    }

    #[test]
    fn abandoned_invocation_is_terminal_and_irreversible() {
        let mut coordinator = RuntimeCoordinator::new().unwrap();
        let root = executing_root(&mut coordinator);
        coordinator.abandon_invocation(root).unwrap();
        assert!(matches!(
            coordinator.return_invocation(root),
            Err(RuntimeError::InvalidTransition { .. })
        ));
    }
}
