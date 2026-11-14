//! Foundational epistemic semantics for SyntraLang.
//!
//! P2 introduces explicit epistemic states, a type wrapper that keeps the
//! underlying type separate from its epistemic status, and a lexically scoped
//! environment. Scope-local declarations and state refinements are discarded
//! together when a scope exits.

use crate::tokens::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ContractMetadata<'src> {
    pub name: Option<&'src str>,
    pub requires: Vec<&'src str>,
    pub guarantees: Vec<&'src str>,
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

    fn update_state(&mut self, state: EpistemicState) {
        self.epistemic_state = state;
        self.ty.state = state;
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct EpistemicEnvironment<'src> {
    scopes: Vec<HashMap<&'src str, EpistemicBinding<'src>>>,
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

    pub fn update_state(
        &mut self,
        name: &str,
        new_state: EpistemicState,
    ) -> Result<(), String> {
        if let Some(binding) = self
            .scopes
            .last_mut()
            .expect("EpistemicEnvironment always contains a root scope")
            .get_mut(name)
        {
            binding.update_state(new_state);
            return Ok(());
        }

        let inherited = self
            .scopes
            .iter()
            .rev()
            .skip(1)
            .find_map(|scope| scope.get(name))
            .cloned();

        let Some(mut binding) = inherited else {
            return Err(format!("binding '{name}' is not declared in the current scope"));
        };

        binding.update_state(new_state);
        self.scopes
            .last_mut()
            .expect("EpistemicEnvironment always contains a root scope")
            .insert(name, binding);
        Ok(())
    }
}
