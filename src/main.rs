mod repl;

use clap::Parser;
use std::process;

use cirq::builtin::{self, BuiltinModule, EnvModule, IoModule, MathModule, TimeModule};
use cirq::compiler::Compiler;
use cirq::lexer::Lexer;
use cirq::parser::Parser as CirqParser;
use cirq::vm::Vm;

#[derive(Parser)]
#[command(name = "cirq")]
#[command(about = "A simple interpreter for the Cirq language", long_about = None)]
struct Cli {
    file: Option<String>,

    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();

    match cli.file {
        Some(file) => run_file(&file, cli.args),
        None => repl::start(&mut create_vm()),
    }
}

fn run_file(path: &str, extra_args: Vec<String>) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading '{}': {}", path, e);
            process::exit(1);
        }
    };

    let mut script_args = extra_args;
    script_args.insert(0, path.to_string());
    builtin::set_script_args(script_args);

    if let Err(e) = run(&source) {
        eprintln!("{}", cirq::error::format_error(&e, &source, path));
        process::exit(1);
    }
}

fn create_vm() -> Vm {
    let mut vm = Vm::new();

    let io_module = IoModule;
    vm.register_module(io_module.name(), io_module.build());
    let math_module = MathModule;
    vm.register_module(math_module.name(), math_module.build());
    let time_module = TimeModule;
    vm.register_module(time_module.name(), time_module.build());
    let env_module = EnvModule;
    vm.register_module(env_module.name(), env_module.build());

    vm
}

fn run(source: &str) -> Result<(), cirq::error::CirqError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let mut parser = CirqParser::new(tokens);
    let ast = parser.parse()?;

    let compiler = Compiler::new();
    let program = compiler.compile(&ast)?;

    let mut vm = create_vm();
    vm.execute(program)?;
    Ok(())
}
