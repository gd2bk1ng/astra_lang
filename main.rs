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
//  Created:     2025-12-XX
//  Copyright (c) 2025 Alex Roussinov
//
//  License:
//      Dual licensed under the MIT and Apache 2.0 licenses.
//      See LICENSE-MIT and LICENSE-APACHE at the repository root for details.
// =============================================================================

mod tokens;
mod lexer;

use lexer::Lexer;
use tokens::TokenKind;

fn main() {
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
    for token in tokens {
        println!("{:?} at {:?}", token.kind, token.span);
    }
}
