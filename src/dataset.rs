// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: dataset.rs
//
//  Description:
//      Hierarchical dataset structure for Astra micro intents,
//      with JSON load/save support for easy updates and integration.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
// =============================================================================

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::Path;

#[derive(Debug, Serialize, Deserialize)]
pub struct MicroIntent {
    pub id: Option<u32>,
    pub description: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IntentDataset {
    pub domains: HashMap<String, Vec<MicroIntent>>,
}

impl IntentDataset {
    pub fn new() -> Self {
        IntentDataset {
            domains: HashMap::new(),
        }
    }

    pub fn add_intent(&mut self, domain: &str, id: Option<u32>, description: &str) {
        let intents = self.domains.entry(domain.to_string()).or_default();
        intents.push(MicroIntent {
            id,
            description: description.to_string(),
        });
    }

    /// Loads an IntentDataset from a JSON file at the given path.
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let data = fs::read_to_string(path)?;
        let dataset = serde_json::from_str(&data)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        Ok(dataset)
    }

    /// Saves the IntentDataset to a JSON file at the given path.
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        let mut file = fs::File::create(path)?;
        file.write_all(json.as_bytes())?;
        Ok(())
    }
}
