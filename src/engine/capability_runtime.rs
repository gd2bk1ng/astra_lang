//! Versioned production capability ledger.
//!
//! This module is deliberately independent from `crate::verify::CapabilityLedger`.
//! The verifier ledger answers a semantic question ("was this capability kind
//! declared?"). This ledger answers a runtime question ("which unique instance
//! exists, who owns it, and what irreversible state transitions occurred?").

use std::collections::{BTreeMap, BTreeSet};

use super::identity::{CapabilityId, CapabilityOwnerId};
use super::runtime::{InvocationId, InvocationState};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CapabilityInstanceId(u64);

impl CapabilityInstanceId {
    pub const fn value(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TransferId(u64);

impl TransferId {
    pub const fn value(self) -> u64 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityInstanceState {
    Available,
    InTransit { transfer: TransferId },
    Consumed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityTransferState {
    Active,
    Returned,
    Consumed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityInstanceRecord {
    id: CapabilityInstanceId,
    kind: CapabilityId,
    owner: CapabilityOwnerId,
    state: CapabilityInstanceState,
}

impl CapabilityInstanceRecord {
    pub const fn id(self) -> CapabilityInstanceId {
        self.id
    }

    pub const fn kind(self) -> CapabilityId {
        self.kind
    }

    pub const fn owner(self) -> CapabilityOwnerId {
        self.owner
    }

    pub const fn state(self) -> CapabilityInstanceState {
        self.state
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityTransferRecord {
    id: TransferId,
    capability: CapabilityInstanceId,
    source_invocation: InvocationId,
    destination_invocation: InvocationId,
    source_owner: CapabilityOwnerId,
    destination_owner: CapabilityOwnerId,
    parent_transfer: Option<TransferId>,
    state: CapabilityTransferState,
    returned_at: Option<u64>,
}

impl CapabilityTransferRecord {
    pub const fn id(self) -> TransferId {
        self.id
    }

    pub const fn capability(self) -> CapabilityInstanceId {
        self.capability
    }

    pub const fn source_invocation(self) -> InvocationId {
        self.source_invocation
    }

    pub const fn destination_invocation(self) -> InvocationId {
        self.destination_invocation
    }

    pub const fn source_owner(self) -> CapabilityOwnerId {
        self.source_owner
    }

    pub const fn destination_owner(self) -> CapabilityOwnerId {
        self.destination_owner
    }

    pub const fn parent_transfer(self) -> Option<TransferId> {
        self.parent_transfer
    }

    pub const fn state(self) -> CapabilityTransferState {
        self.state
    }

    pub const fn returned_at(self) -> Option<u64> {
        self.returned_at
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CapabilityRuntimeError {
    InstanceIdExhausted,
    TransferIdExhausted,
    VersionExhausted,
    UnknownCapability(CapabilityInstanceId),
    UnknownTransfer(TransferId),
    DuplicateCapability(CapabilityInstanceId),
    CapabilityNotOwned {
        capability: CapabilityInstanceId,
        expected: CapabilityOwnerId,
        actual: CapabilityOwnerId,
    },
    CapabilityNotAvailable(CapabilityInstanceId),
    InvalidInvocationState(InvocationId),
    SameInvocationTransfer(InvocationId),
    InvalidTransferState(TransferId),
    RegistryInvariantViolation,
    ReturnSequenceExhausted,
    ReturnSequenceMismatch,
    DuplicateReturnSequence(u64),
    NonMonotonicReturnSequence,
    ChildReturnedAfterParent,
    StaleTerminationPlan { expected_version: u64, actual_version: u64 },
    InvalidTerminationTransfer(TransferId),
    InvalidTerminationAncestry(TransferId),
    ReturnSequenceOverflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityTransferRequest {
    capability: CapabilityInstanceId,
    destination: InvocationId,
}

impl CapabilityTransferRequest {
    pub const fn new(capability: CapabilityInstanceId, destination: InvocationId) -> Self {
        Self {
            capability,
            destination,
        }
    }

    pub const fn capability(self) -> CapabilityInstanceId {
        self.capability
    }

    pub const fn destination(self) -> InvocationId {
        self.destination
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapabilityReturn {
    transfer: TransferId,
    returned_at: u64,
}

impl CapabilityReturn {
    pub const fn transfer(self) -> TransferId {
        self.transfer
    }

    pub const fn returned_at(self) -> u64 {
        self.returned_at
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CapabilityTerminationPlan {
    expected_version: u64,
    invocation: InvocationId,
    transfers: Vec<TransferId>,
}

impl CapabilityTerminationPlan {
    pub(crate) const fn expected_version(&self) -> u64 {
        self.expected_version
    }

    pub(crate) const fn invocation(&self) -> InvocationId {
        self.invocation
    }

    pub(crate) fn transfers(&self) -> &[TransferId] {
        &self.transfers
    }
}

#[derive(Debug, Clone)]
struct PlannedTransfer {
    id: TransferId,
    capability: CapabilityInstanceId,
    source_invocation: InvocationId,
    destination_invocation: InvocationId,
    source_owner: CapabilityOwnerId,
    destination_owner: CapabilityOwnerId,
    parent_transfer: Option<TransferId>,
}

#[derive(Debug)]
pub struct CapabilityLedger {
    version: u64,
    next_instance_id: Option<u64>,
    next_transfer_id: Option<u64>,
    next_return_sequence: Option<u64>,
    instances: BTreeMap<CapabilityInstanceId, CapabilityInstanceRecord>,
    transfers: BTreeMap<TransferId, CapabilityTransferRecord>,
}

impl CapabilityLedger {
    pub(crate) fn new() -> Self {
        Self {
            version: 0,
            next_instance_id: Some(1),
            next_transfer_id: Some(1),
            next_return_sequence: Some(1),
            instances: BTreeMap::new(),
            transfers: BTreeMap::new(),
        }
    }

    pub const fn version(&self) -> u64 {
        self.version
    }

    pub fn instance(&self, id: CapabilityInstanceId) -> Option<&CapabilityInstanceRecord> {
        self.instances.get(&id)
    }

    pub fn transfer(&self, id: TransferId) -> Option<&CapabilityTransferRecord> {
        self.transfers.get(&id)
    }

    pub fn instances(&self) -> impl Iterator<Item = &CapabilityInstanceRecord> {
        self.instances.values()
    }

    pub fn transfers(&self) -> impl Iterator<Item = &CapabilityTransferRecord> {
        self.transfers.values()
    }

    /// Mint one capability instance for a coordinator-resolved owner.
    ///
    /// The owner identity is supplied by the engine, not manufactured by the
    /// caller. This method is crate-private so bootstrap/grant authority stays
    /// inside the engine boundary.
    pub(crate) fn grant(
        &mut self,
        owner: CapabilityOwnerId,
        kind: CapabilityId,
    ) -> Result<CapabilityInstanceId, CapabilityRuntimeError> {
        self.validate_structure()?;

        let next_version = self.next_version()?;
        let sequence = self
            .next_instance_id
            .ok_or(CapabilityRuntimeError::InstanceIdExhausted)?;
        let next_instance_id = sequence.checked_add(1);

        let id = CapabilityInstanceId(sequence);
        self.instances.insert(
            id,
            CapabilityInstanceRecord {
                id,
                kind,
                owner,
                state: CapabilityInstanceState::Available,
            },
        );
        self.next_instance_id = next_instance_id;
        self.version = next_version;
        Ok(id)
    }

    /// Preflight every requested transfer before changing any ledger state.
    ///
    /// Destination owners/states are resolved by the coordinator. The ledger
    /// never accepts a caller-supplied owner identity as transfer authority.
    pub(crate) fn transfer_batch<F>(
        &mut self,
        source_invocation: InvocationId,
        requests: &[CapabilityTransferRequest],
        resolve_invocation: F,
    ) -> Result<Vec<TransferId>, CapabilityRuntimeError>
    where
        F: Fn(InvocationId) -> Option<(CapabilityOwnerId, InvocationState)>,
    {
        self.validate_structure()?;

        let Some((source_owner, source_state)) = resolve_invocation(source_invocation) else {
            return Err(CapabilityRuntimeError::InvalidInvocationState(source_invocation));
        };
        if source_state != InvocationState::Executing {
            return Err(CapabilityRuntimeError::InvalidInvocationState(source_invocation));
        }

        let mut seen = BTreeSet::new();
        let mut planned = Vec::with_capacity(requests.len());
        let mut next_transfer_id = self.next_transfer_id;

        for request in requests {
            if !seen.insert(request.capability) {
                return Err(CapabilityRuntimeError::DuplicateCapability(request.capability));
            }

            let instance = self
                .instances
                .get(&request.capability)
                .ok_or(CapabilityRuntimeError::UnknownCapability(request.capability))?;

            if instance.owner != source_owner {
                return Err(CapabilityRuntimeError::CapabilityNotOwned {
                    capability: request.capability,
                    expected: source_owner,
                    actual: instance.owner,
                });
            }

            if matches!(instance.state, CapabilityInstanceState::Consumed) {
                return Err(CapabilityRuntimeError::CapabilityNotAvailable(
                    request.capability,
                ));
            }

            if request.destination == source_invocation {
                return Err(CapabilityRuntimeError::SameInvocationTransfer(
                    source_invocation,
                ));
            }

            let Some((destination_owner, destination_state)) =
                resolve_invocation(request.destination)
            else {
                return Err(CapabilityRuntimeError::InvalidInvocationState(
                    request.destination,
                ));
            };
            if destination_state != InvocationState::Executing {
                return Err(CapabilityRuntimeError::InvalidInvocationState(
                    request.destination,
                ));
            }

            let transfer_sequence = next_transfer_id
                .ok_or(CapabilityRuntimeError::TransferIdExhausted)?;
            next_transfer_id = transfer_sequence.checked_add(1);

            let parent_transfer = match instance.state {
                CapabilityInstanceState::Available => None,
                CapabilityInstanceState::InTransit { transfer } => {
                    let parent = self
                        .transfers
                        .get(&transfer)
                        .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                    if parent.state != CapabilityTransferState::Active
                        || parent.destination_invocation != source_invocation
                        || parent.destination_owner != source_owner
                    {
                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                    }
                    Some(transfer)
                }
                CapabilityInstanceState::Consumed => {
                    return Err(CapabilityRuntimeError::CapabilityNotAvailable(
                        request.capability,
                    ))
                }
            };

            planned.push(PlannedTransfer {
                id: TransferId(transfer_sequence),
                capability: request.capability,
                source_invocation,
                destination_invocation: request.destination,
                source_owner,
                destination_owner,
                parent_transfer,
            });
        }

        if requests.is_empty() {
            return Ok(Vec::new());
        }

        let next_version = self.next_version()?;

        for plan in &planned {
            let instance = self
                .instances
                .get_mut(&plan.capability)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
            instance.owner = plan.destination_owner;
            instance.state = CapabilityInstanceState::InTransit { transfer: plan.id };

            self.transfers.insert(
                plan.id,
                CapabilityTransferRecord {
                    id: plan.id,
                    capability: plan.capability,
                    source_invocation: plan.source_invocation,
                    destination_invocation: plan.destination_invocation,
                    source_owner: plan.source_owner,
                    destination_owner: plan.destination_owner,
                    parent_transfer: plan.parent_transfer,
                    state: CapabilityTransferState::Active,
                    returned_at: None,
                },
            );
        }

        self.next_transfer_id = next_transfer_id;
        self.version = next_version;
        self.validate_structure()?;
        Ok(planned.into_iter().map(|plan| plan.id).collect())
    }

    /// Irreversibly consume an available capability instance.
    pub(crate) fn consume<F>(
        &mut self,
        invocation: InvocationId,
        capability: CapabilityInstanceId,
        resolve_invocation: F,
    ) -> Result<(), CapabilityRuntimeError>
    where
        F: Fn(InvocationId) -> Option<CapabilityOwnerId>,
    {
        self.validate_structure()?;

        let owner = resolve_invocation(invocation)
            .ok_or(CapabilityRuntimeError::InvalidInvocationState(invocation))?;

        let instance = self
            .instances
            .get(&capability)
            .ok_or(CapabilityRuntimeError::UnknownCapability(capability))?;

        if instance.owner != owner {
            return Err(CapabilityRuntimeError::CapabilityNotOwned {
                capability,
                expected: owner,
                actual: instance.owner,
            });
        }

        let active_transfer = match instance.state {
            CapabilityInstanceState::Available => None,
            CapabilityInstanceState::InTransit { transfer } => Some(transfer),
            CapabilityInstanceState::Consumed => {
                return Err(CapabilityRuntimeError::CapabilityNotAvailable(capability))
            }
        };

        if let Some(transfer) = active_transfer {
            let transfer_record = self
                .transfers
                .get(&transfer)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
            if transfer_record.state != CapabilityTransferState::Active
                || transfer_record.destination_invocation != invocation
                || transfer_record.destination_owner != owner
            {
                return Err(CapabilityRuntimeError::CapabilityNotAvailable(capability));
            }
        }

        let next_version = self.next_version()?;
        let instance = self
            .instances
            .get_mut(&capability)
            .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
        instance.state = CapabilityInstanceState::Consumed;

        if let Some(transfer) = active_transfer {
            let transfer_record = self
                .transfers
                .get_mut(&transfer)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
            transfer_record.state = CapabilityTransferState::Consumed;
        }

        self.version = next_version;

        self.validate_structure()?;
        Ok(())
    }

    /// Stage termination of one executing invocation.
    ///
    /// The plan captures the ledger version and the exact active incoming
    /// transfer set. No return sequence is allocated during planning. A plan
    /// therefore becomes stale as soon as any other ledger mutation occurs.
    pub(crate) fn plan_termination<F>(
        &self,
        invocation: InvocationId,
        resolve_invocation: F,
    ) -> Result<CapabilityTerminationPlan, CapabilityRuntimeError>
    where
        F: Fn(InvocationId) -> Option<InvocationState>,
    {
        self.validate_structure()?;

        let state = resolve_invocation(invocation)
            .ok_or(CapabilityRuntimeError::InvalidInvocationState(invocation))?;
        if state != InvocationState::Executing {
            return Err(CapabilityRuntimeError::InvalidInvocationState(invocation));
        }

        let transfers = self
            .transfers
            .values()
            .filter(|record| {
                record.destination_invocation == invocation
                    && record.state == CapabilityTransferState::Active
            })
            .map(|record| record.id)
            .collect();

        Ok(CapabilityTerminationPlan {
            expected_version: self.version,
            invocation,
            transfers,
        })
    }

    /// Commit a previously staged termination.
    ///
    /// Every failure is detected before the first mutation. Return sequence
    /// numbers are allocated privately here, in transfer-ID order, and only
    /// for transfers whose capability remains in transit. Consumed capabilities
    /// close their active transfer without receiving a return sequence.
    pub(crate) fn commit_termination<F>(
        &mut self,
        plan: &CapabilityTerminationPlan,
        resolve_invocation: F,
    ) -> Result<Vec<CapabilityReturn>, CapabilityRuntimeError>
    where
        F: Fn(InvocationId) -> Option<InvocationState>,
    {
        self.validate_structure()?;

        if self.version != plan.expected_version {
            return Err(CapabilityRuntimeError::StaleTerminationPlan {
                expected_version: plan.expected_version,
                actual_version: self.version,
            });
        }

        let state = resolve_invocation(plan.invocation)
            .ok_or(CapabilityRuntimeError::InvalidInvocationState(plan.invocation))?;
        if state != InvocationState::Executing {
            return Err(CapabilityRuntimeError::InvalidInvocationState(plan.invocation));
        }

        let next_version = self.next_version()?;
        let mut next_return_sequence = self.next_return_sequence;
        let mut staged_returns = Vec::new();

        for transfer_id in &plan.transfers {
            let record = self
                .transfers
                .get(transfer_id)
                .ok_or(CapabilityRuntimeError::InvalidTerminationTransfer(*transfer_id))?;
            if record.state != CapabilityTransferState::Active
                || record.destination_invocation != plan.invocation
            {
                return Err(CapabilityRuntimeError::InvalidTerminationTransfer(*transfer_id));
            }

            let instance = self
                .instances
                .get(&record.capability)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;

            match instance.state {
                CapabilityInstanceState::InTransit { transfer } if transfer == *transfer_id => {
                    if let Some(parent_transfer) = record.parent_transfer {
                        let parent = self
                            .transfers
                            .get(&parent_transfer)
                            .ok_or(CapabilityRuntimeError::InvalidTerminationAncestry(
                                *transfer_id,
                            ))?;
                        if parent.state != CapabilityTransferState::Active
                            || parent.destination_invocation != record.source_invocation
                            || parent.capability != record.capability
                            || parent.destination_owner != record.source_owner
                        {
                            return Err(CapabilityRuntimeError::InvalidTerminationAncestry(
                                *transfer_id,
                            ));
                        }
                    }

                    let sequence = next_return_sequence
                        .ok_or(CapabilityRuntimeError::ReturnSequenceExhausted)?;
                    next_return_sequence = sequence.checked_add(1);
                    staged_returns.push(CapabilityReturn {
                        transfer: *transfer_id,
                        returned_at: sequence,
                    });
                }
                CapabilityInstanceState::Consumed => {}
                _ => {
                    return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                }
            }
        }

        for staged in &staged_returns {
            let record = self
                .transfers
                .get(&staged.transfer)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
            if let Some(parent_transfer) = record.parent_transfer {
                if let Some(parent) = self.transfers.get(&parent_transfer) {
                    if parent.state == CapabilityTransferState::Returned {
                        let parent_sequence = parent
                            .returned_at
                            .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                        if staged.returned_at >= parent_sequence {
                            return Err(CapabilityRuntimeError::ChildReturnedAfterParent);
                        }
                    } else if parent.state != CapabilityTransferState::Active {
                        return Err(CapabilityRuntimeError::InvalidTerminationAncestry(
                            staged.transfer,
                        ));
                    }
                } else {
                    return Err(CapabilityRuntimeError::InvalidTerminationAncestry(
                        staged.transfer,
                    ));
                }
            }
        }

        for transfer_id in &plan.transfers {
            let record = *self
                .transfers
                .get(transfer_id)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
            let instance = self
                .instances
                .get_mut(&record.capability)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;

            if let Some(staged) = staged_returns.iter().find(|item| item.transfer == *transfer_id) {
                if let Some(parent_transfer) = record.parent_transfer {
                    instance.owner = record.source_owner;
                    instance.state = CapabilityInstanceState::InTransit {
                        transfer: parent_transfer,
                    };
                } else {
                    instance.owner = record.source_owner;
                    instance.state = CapabilityInstanceState::Available;
                }

                let record = self
                    .transfers
                    .get_mut(transfer_id)
                    .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                record.state = CapabilityTransferState::Returned;
                record.returned_at = Some(staged.returned_at);
            } else {
                instance.state = CapabilityInstanceState::Consumed;
                let record = self
                    .transfers
                    .get_mut(transfer_id)
                    .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                record.state = CapabilityTransferState::Consumed;
            }
        }

        self.next_return_sequence = next_return_sequence;
        self.version = next_version;
        debug_assert!(self.validate_structure().is_ok());
        Ok(staged_returns)
    }

    /// Return one transfer through the same staged termination machinery.
    /// This compatibility boundary terminates the transfer's destination
    /// invocation and therefore may reconcile additional incoming transfers.
    pub(crate) fn return_transfer<F>(
        &mut self,
        transfer: TransferId,
        resolve_invocation: F,
    ) -> Result<CapabilityReturn, CapabilityRuntimeError>
    where
        F: Fn(InvocationId) -> Option<InvocationState> + Copy,
    {
        let record = self
            .transfers
            .get(&transfer)
            .ok_or(CapabilityRuntimeError::UnknownTransfer(transfer))?;
        let plan = self.plan_termination(record.destination_invocation, resolve_invocation)?;
        let returns = self.commit_termination(&plan, resolve_invocation)?;
        returns
            .into_iter()
            .find(|item| item.transfer == transfer)
            .ok_or(CapabilityRuntimeError::InvalidTerminationTransfer(transfer))
    }

    /// Validate all internal ownership, transfer, ancestry, and sequence invariants.
    pub(crate) fn validate_structure(&self) -> Result<(), CapabilityRuntimeError> {
        let mut seen_return_sequences = BTreeSet::new();
        let mut max_return_sequence = None;

        for (id, instance) in &self.instances {
            if *id != instance.id {
                return Err(CapabilityRuntimeError::RegistryInvariantViolation);
            }

            if let CapabilityInstanceState::InTransit { transfer } = instance.state {
                let record = self
                    .transfers
                    .get(&transfer)
                    .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                if record.capability != *id || record.state != CapabilityTransferState::Active {
                    return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                }
                if instance.owner != record.destination_owner {
                    return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                }
            }
        }

        for (id, transfer) in &self.transfers {
            if *id != transfer.id {
                return Err(CapabilityRuntimeError::RegistryInvariantViolation);
            }

            let instance = self
                .instances
                .get(&transfer.capability)
                .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;

            if let Some(parent_id) = transfer.parent_transfer {
                let parent = self
                    .transfers
                    .get(&parent_id)
                    .ok_or(CapabilityRuntimeError::InvalidTerminationAncestry(*id))?;
                if parent.capability != transfer.capability
                    || parent.destination_invocation != transfer.source_invocation
                    || parent.destination_owner != transfer.source_owner
                {
                    return Err(CapabilityRuntimeError::InvalidTerminationAncestry(*id));
                }
                if parent_id >= *id {
                    return Err(CapabilityRuntimeError::InvalidTerminationAncestry(*id));
                }
            }

            match transfer.state {
                CapabilityTransferState::Active => {
                    if instance.state != (CapabilityInstanceState::InTransit { transfer: *id }) {
                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                    }
                    if transfer.returned_at.is_some() {
                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                    }
                    if let Some(parent_id) = transfer.parent_transfer {
                        let parent = self
                            .transfers
                            .get(&parent_id)
                            .ok_or(CapabilityRuntimeError::InvalidTerminationAncestry(*id))?;
                        if parent.state != CapabilityTransferState::Active {
                            return Err(CapabilityRuntimeError::InvalidTerminationAncestry(*id));
                        }
                    }
                }
                CapabilityTransferState::Returned => {
                    let sequence = transfer
                        .returned_at
                        .ok_or(CapabilityRuntimeError::RegistryInvariantViolation)?;
                    if !seen_return_sequences.insert(sequence) {
                        return Err(CapabilityRuntimeError::DuplicateReturnSequence(sequence));
                    }
                    max_return_sequence = Some(
                        max_return_sequence.map_or(sequence, |max| max.max(sequence)),
                    );

                    match transfer.parent_transfer {
                        None => {
                            if instance.state != CapabilityInstanceState::Available
                                || instance.owner != transfer.source_owner
                            {
                                return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                            }
                        }
                        Some(parent_id) => {
                            let parent = self
                                .transfers
                                .get(&parent_id)
                                .ok_or(CapabilityRuntimeError::InvalidTerminationAncestry(*id))?;
                            match parent.state {
                                CapabilityTransferState::Active => {
                                    if instance.state
                                        != (CapabilityInstanceState::InTransit {
                                            transfer: parent_id,
                                        })
                                        || instance.owner != parent.destination_owner
                                    {
                                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                                    }
                                }
                                CapabilityTransferState::Returned => {
                                    let parent_sequence = parent.returned_at.ok_or(
                                        CapabilityRuntimeError::RegistryInvariantViolation,
                                    )?;
                                    if sequence >= parent_sequence {
                                        return Err(CapabilityRuntimeError::ChildReturnedAfterParent);
                                    }
                                    if instance.state != CapabilityInstanceState::Available
                                        || instance.owner != transfer.source_owner
                                    {
                                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                                    }
                                }
                                CapabilityTransferState::Consumed => {
                                    return Err(CapabilityRuntimeError::InvalidTerminationAncestry(
                                        *id,
                                    ));
                                }
                            }
                        }
                    }
                }
                CapabilityTransferState::Consumed => {
                    if transfer.returned_at.is_some()
                        || instance.state != CapabilityInstanceState::Consumed
                    {
                        return Err(CapabilityRuntimeError::RegistryInvariantViolation);
                    }
                }
            }
        }

        match (max_return_sequence, self.next_return_sequence) {
            (None, Some(1)) => {}
            (Some(max), Some(next)) if max < u64::MAX && next == max + 1 => {}
            (Some(u64::MAX), None) => {}
            _ => return Err(CapabilityRuntimeError::RegistryInvariantViolation),
        }

        Ok(())
    }

    fn next_version(&self) -> Result<u64, CapabilityRuntimeError> {
        self.version
            .checked_add(1)
            .ok_or(CapabilityRuntimeError::VersionExhausted)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ids() -> (InvocationId, InvocationId, CapabilityOwnerId, CapabilityOwnerId) {
        let root = InvocationId::new_for_test(1);
        let child = InvocationId::new_for_test(2);
        let root_owner = CapabilityOwnerId::new_for_test(1);
        let child_owner = CapabilityOwnerId::new_for_test(2);
        (root, child, root_owner, child_owner)
    }

    #[test]
    fn capability_kind_and_instance_identity_are_distinct() {
        let (_, _, root_owner, _) = ids();
        let mut ledger = CapabilityLedger::new();
        let first = ledger.grant(root_owner, CapabilityId::ReadFile).unwrap();
        let second = ledger.grant(root_owner, CapabilityId::ReadFile).unwrap();

        assert_ne!(first, second);
        assert_eq!(
            ledger.instance(first).unwrap().kind(),
            CapabilityId::ReadFile
        );
        assert_eq!(
            ledger.instance(second).unwrap().kind(),
            CapabilityId::ReadFile
        );
    }

    #[test]
    fn transfer_batch_preflights_all_entries_before_mutation() {
        let (root, child, root_owner, child_owner) = ids();
        let mut ledger = CapabilityLedger::new();
        let first = ledger.grant(root_owner, CapabilityId::ReadFile).unwrap();
        let second = ledger.grant(root_owner, CapabilityId::WriteFile).unwrap();
        let version = ledger.version();

        let result = ledger.transfer_batch(
            root,
            &[
                CapabilityTransferRequest::new(first, child),
                CapabilityTransferRequest::new(second, root),
            ],
            |invocation| match invocation {
                id if id == root => Some((root_owner, InvocationState::Executing)),
                id if id == child => Some((child_owner, InvocationState::Executing)),
                _ => None,
            },
        );

        assert_eq!(
            result,
            Err(CapabilityRuntimeError::SameInvocationTransfer(root))
        );
        assert_eq!(ledger.version(), version);
        assert_eq!(
            ledger.instance(first).unwrap().state(),
            CapabilityInstanceState::Available
        );
        assert_eq!(
            ledger.instance(second).unwrap().state(),
            CapabilityInstanceState::Available
        );
        assert!(ledger.transfers().next().is_none());
    }

    #[test]
    fn consumed_capability_cannot_be_consumed_again() {
        let (root, _, root_owner, _) = ids();
        let mut ledger = CapabilityLedger::new();
        let capability = ledger.grant(root_owner, CapabilityId::ReadFile).unwrap();

        ledger
            .consume(root, capability, |id| (id == root).then_some(root_owner))
            .unwrap();

        assert_eq!(
            ledger.consume(root, capability, |id| (id == root).then_some(root_owner)),
            Err(CapabilityRuntimeError::CapabilityNotAvailable(capability))
        );
        assert_eq!(
            ledger.instance(capability).unwrap().state(),
            CapabilityInstanceState::Consumed
        );
    }
}
