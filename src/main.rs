// =============================================================================
//  Astra Reference Compiler (ARC)
//  File: main.rs
//
//  Description:
//      Entry point for the Astra language compiler and runtime.
//      Astra is a purpose-built hybrid programming language designed to enable 
//      the development of next-generation Artificial General Intelligence (AGI) 
//      systems. It combines symbolic reasoning, neural computation, and 
//      differentiable programming with a safety-first, modular design.
//
//      This file initializes the compiler pipeline by lexing source code into tokens,
//      parsing those tokens into an Abstract Syntax Tree (AST), and preparing for 
//      semantic analysis and execution.
//
//  Vision & Goals:
//      Astra aims to provide a rich, extensible framework for structuring and 
//      dynamically expanding thousands to hundreds of thousands of micro-intents — 
//      fine-grained units of meaning and action critical for versatile, human-like 
//      understanding and problem-solving across domains.
//
//      The language supports hierarchical, parameterized intents, active learning, 
//      and modular intent management to reduce complexity and enable efficient 
//      transfer learning, autonomous goal-setting, and self-reflection.
//
//      Astra’s core design principles include:
//        - Safety and verifiability akin to Rust’s guarantees.
//        - Expressivity and readability inspired by Python, Julia, and Rust.
//        - Integration of symbolic logic, differentiable programming, and meta-programming.
//        - Native compilation to MLIR for performance and hardware flexibility.
//        - Support for autonomy, self-awareness, and alignment with human values.
//
//  Author:      Alex Roussinov
//  Created:     2025-12-22
//  Copyright (c) 2025 Alex Roussinov
//
//  License:
//      Dual licensed under the MIT and Apache 2.0 licenses.
//      See LICENSE-MIT and LICENSE-APACHE at the repository root for details.
// =============================================================================

mod tokens;
mod lexer;
mod dataset;

use lexer::Lexer;
use tokens::TokenKind;
use dataset::IntentDataset;

fn main() {
    // Part 1: Lexing example
    let source = r#"
        intent astra {
            motive "performance"
            action "optimize"
        }
    "#;

    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next_token().expect("Lexer returned None unexpectedly");
        tokens.push(token.clone());
        if token.kind == TokenKind::Eof {
            break;
        }
    }

    println!("Tokens:");
    for token in &tokens {
        println!("{:?} at {:?}", token.kind, token.span);
    }

    // Part 2: Dataset example
    let mut dataset = IntentDataset::new();

    // Parenting domain (IDs 101-200)
    dataset.add_intent("Parenting", Some(101), "Monitor child's screen time usage");
    dataset.add_intent("Parenting", Some(102), "Find age-appropriate educational games");
    dataset.add_intent("Parenting", Some(103), "Schedule pediatrician visits");
    dataset.add_intent("Parenting", Some(104), "Search for healthy toddler snacks");
    dataset.add_intent("Parenting", Some(105), "Teach child to tie shoelaces");
    // ... add more parenting intents as needed

    // Career Development domain (IDs 201-300)
    dataset.add_intent("Career Development", Some(201), "Update resume with recent experience");
    dataset.add_intent("Career Development", Some(202), "Search for job openings in sustainability");
    dataset.add_intent("Career Development", Some(203), "Find tips for writing cover letters");
    dataset.add_intent("Career Development", Some(204), "Look for interview preparation techniques");
    dataset.add_intent("Career Development", Some(205), "Search for online courses to improve skills");
    // ... add more career development intents as needed

    // Serialize to JSON and print (for export or inspection)
    let json = serde_json::to_string_pretty(&dataset).unwrap();
    println!("\nSerialized Intent Dataset JSON:\n{}", json);
}

