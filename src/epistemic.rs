//! Foundational epistemic semantics for SyntraLang.
//!
//! P2 models epistemic state separately from ordinary value types, tracks
//! bindings in lexical scopes, and requires explicit transition contracts
//! before a binding may change epistemic state.

use crate::tokens::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EpistemicState {
    Observed,
    Inferred,
    Assumed,
    Uncertain,
    Resolved,
    Known,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EpistemicType<'src> {
    pub type_name: &'src str,
    pub state: EpistemicState,
}

impl<'src> EpistemicType<'src> {
    pub fn new(type_name: &'src str, state: EpistemicState) -> Self {
        Self { type_name, state }
    }
}

/// An explicit allow-list for epistemic state transitions.
///
/// A transition is permitted only when its current state appears in
/// `allowed_from` and its requested state appears in `allowed_to`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EpistemicTransitionContract {
    pub allowed_from: Vec<EpistemicState>,
    pub allowed_to: Vec<EpistemicState>,
}

impl EpistemicTransitionContract {
    pub fn new(
        allowed_from: impl IntoIterator<Item = EpistemicState>,
        allowed_to: impl IntoIterator<Item = EpistemicState>,
    ) -> Self {
        Self {
            allowed_from: allowed_from.into_iter().collect(),
            allowed_to: allowed_to.into_iter().collect(),
        }
    }

    fn permits(&self, from: EpistemicState, to: EpistemicState) -> bool {
        self.allowed_from.contains(&from) && self.allowed_to.contains(&to)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ContractMetadata<'src> {
    pub name: Option<&'src str>,
    pub requires: Vec<&'src str>,
    pub guarantees: Vec<&'src str>,
    pub transition: Option<EpistemicTransitionContract>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EpistemicBinding<'src> {
    pub name: &'src str,
    pub ty: EpistemicType<'src>,
    pub epistemic_state: EpistemicState,
    pub contract: ContractMetadata<'src>,
    pub span: Span,
}

impl<'src> EpistemicBinding<'src> {
    pub fn new(name: &'src str, type_name: &'src str, state: EpistemicState, span: Span) -> Self {
        Self {
            name,
            ty: EpistemicType::new(type_name, state),
            epistemic_state: state,
            contract: ContractMetadata::default(),
            span,
        }
    }

    fn update_state_unchecked(&mut self, state: EpistemicState) {
        self.epistemic_state = state;
        self.ty.state = state;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EpistemicTransitionError {
    BindingNotFound,
    ContractNotDefined,
    SourceStateNotAllowed,
    TargetStateNotAllowed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EpistemicEnvironment<'src> {
    scopes: Vec<HashMap<&'src str, EpistemicBinding<'src>>>,
}

impl<'src> Default for EpistemicEnvironment<'src> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'src> EpistemicEnvironment<'src> {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn declare(
        &mut self,
        name: &'src str,
        binding: EpistemicBinding<'src>,
    ) -> Result<(), String> {
        let scope = self
            .scopes
            .last_mut()
            .expect("EpistemicEnvironment always contains a root scope");

        if scope.contains_key(name) {
            return Err(format!("binding '{name}' is already declared in the current scope"));
        }
        if binding.name != name {
            return Err(format!(
                "binding name mismatch: declaration uses '{name}', binding contains '{}'",
                binding.name
            ));
        }

        scope.insert(name, binding);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&EpistemicBinding<'src>> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// Attach or replace the transition contract visible to this scope.
    ///
    /// Inherited bindings are copied into the current scope before mutation,
    /// so contract changes cannot leak into an enclosing scope.
    pub fn set_transition_contract(
        &mut self,
        name: &'src str,
        contract: EpistemicTransitionContract,
    ) -> Result<(), EpistemicTransitionError> {
        let binding = self.binding_mut_in_current_scope(name)?;
        binding.contract.transition = Some(contract);
        Ok(())
    }

    /// Check and apply a state transition atomically.
    ///
    /// No state is changed when the binding has no contract or when either
    /// endpoint is not admitted by the contract.
    pub fn transition_state(
        &mut self,
        name: &'src str,
        new_state: EpistemicState,
    ) -> Result<(), EpistemicTransitionError> {
        let binding = self.binding_mut_in_current_scope(name)?;
        let contract = binding
            .contract
            .transition
            .as_ref()
            .ok_or(EpistemicTransitionError::ContractNotDefined)?;
        let old_state = binding.epistemic_state;

        if contract.permits(old_state, new_state) {
            binding.update_state_unchecked(new_state);
            return Ok(());
        }

        if !contract.allowed_from.contains(&old_state) {
            return Err(EpistemicTransitionError::SourceStateNotAllowed);
        }
        Err(EpistemicTransitionError::TargetStateNotAllowed)
    }

    /// Compatibility spelling for callers that previously used update_state.
    /// It now enforces the same explicit transition contract as transition_state.
    pub fn update_state(
        &mut self,
        name: &'src str,
        new_state: EpistemicState,
    ) -> Result<(), EpistemicTransitionError> {
        self.transition_state(name, new_state)
    }

    fn binding_mut_in_current_scope(
        &mut self,
        name: &'src str,
    ) -> Result<&mut EpistemicBinding<'src>, EpistemicTransitionError> {
        let current_index = self.scopes.len() - 1;

        if !self.scopes[current_index].contains_key(name) {
            let inherited = self
                .scopes
                .iter()
                .rev()
                .skip(1)
                .find_map(|scope| scope.get(name))
                .cloned();

            if let Some(binding) = inherited {
                self.scopes[current_index].insert(name, binding);
            } else {
                return Err(EpistemicTransitionError::BindingNotFound);
            }
        }

        Ok(self.scopes[current_index]
            .get_mut(name)
            .expect("binding was inserted or already existed"))
    }
}
