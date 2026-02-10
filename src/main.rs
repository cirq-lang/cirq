use std::env;
use std::process;

use cirq::builtin::{BuiltinModule, IoModule, MathModule};
use cirq::compiler::Compiler;
use cirq::lexer::Lexer;
use cirq::parser::Parser;
use cirq::vm::Vm;
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

fn run(source: &str) -> Result<(), cirq::error::CirqError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let compiler = Compiler::new();
    let program = compiler.compile(&ast)?;

    let mut vm = Vm::new();

    let io_module = IoModule;
    vm.register_module(io_module.name(), io_module.build());
    let math_module = MathModule;
    vm.register_module(math_module.name(), math_module.build());

    vm.execute(program)?;

    Ok(())
}
