//! # Cirq — High-Performance Register-Based VM Interpreter
//!
//! Cirq is a blazingly fast interpreter built in Rust, prioritizing raw
//! performance above all. It features a register-based virtual machine
//! with ARC (Automated Reference Counting) memory management — zero GC.
//!
//! ## Architecture
//! Source → Lexer → Parser → AST → Compiler → Bytecode → VM → Output
//!
//! ## Key Features
//! - Register-based VM with compact instruction encoding.
//! - ARC memory via `Rc` — deterministic, zero-pause, no garbage collector.
//! - `FxHashMap` for fastest string-keyed global lookups.
//! - `memchr` SIMD-accelerated scanning in the lexer.
//! - Trait-based builtin module system (e.g., `io` module).

pub mod ast;
pub mod builtin;
pub mod compiler;
pub mod error;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod token;
pub mod value;
pub mod vm;

#[cfg(test)]
mod tests;
