//! # Cirq CLI Entry Point
//!
//! Reads a `.cq` source file, pipes it through the full interpreter
//! pipeline (lex → parse → compile → execute), and reports errors
//! with source location information.

use std::env;
use std::process;

use cirq::builtin::{BuiltinModule, IoModule};
use cirq::compiler::Compiler;
use cirq::lexer::Lexer;
use cirq::parser::Parser;
use cirq::vm::Vm;

// -----------------------------------------------------------------------------
// MAIN ENTRY POINT
// -----------------------------------------------------------------------------

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: cirq <file.cq>");
        process::exit(1);
    }

    let path = &args[1];
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading '{}': {}", path, e);
            process::exit(1);
        }
    };

    if let Err(e) = run(&source) {
        eprintln!("{}", e);
        process::exit(1);
    }
}

/// Runs the full interpreter pipeline on the given source string.
fn run(source: &str) -> Result<(), cirq::error::CirqError> {
    // Lexing
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    // Parsing
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    // Compilation
    let compiler = Compiler::new();
    let program = compiler.compile(&ast)?;

    // Execution
    let mut vm = Vm::new();

    // Register builtin modules
    let io_module = IoModule;
    vm.register_module(io_module.name(), io_module.build());

    vm.execute(program)?;

    Ok(())
}
