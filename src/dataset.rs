// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: dataset.rs
//
//  Description:
//      Hierarchical dataset structure for Astra micro intents,
//      organized by domain keys mapping to lists of granular, actionable micro intents.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
// =============================================================================

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A micro intent represents a fine-grained human intention or goal.
#[derive(Debug, Serialize, Deserialize)]
pub struct MicroIntent {
    /// Unique ID for the micro intent (optional, e.g., 101, 102, etc.)
    pub id: Option<u32>,
    /// The text description of the micro intent.
    pub description: String,
}

/// The dataset maps domain keys (e.g., "Parenting") to lists of micro intents.
#[derive(Debug, Serialize, Deserialize)]
pub struct IntentDataset {
    pub domains: HashMap<String, Vec<MicroIntent>>,
}

impl IntentDataset {
    /// Creates a new empty dataset.
    pub fn new() -> Self {
        IntentDataset {
            domains: HashMap::new(),
        }
    }

    /// Adds a micro intent to a domain.
    pub fn add_intent(&mut self, domain: &str, id: Option<u32>, description: &str) {
        let intents = self.domains.entry(domain.to_string()).or_default();
        intents.push(MicroIntent {
            id,
            description: description.to_string(),
        });
    }
}
