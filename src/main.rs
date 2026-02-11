use clap::Parser;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::process;

use cirq::builtin::{self, BuiltinModule, EnvModule, IoModule, MathModule, TimeModule};
use cirq::compiler::Compiler;
use cirq::lexer::Lexer;
use cirq::parser::Parser as CirqParser;
use cirq::value::Value;
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
        None => repl(),
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

fn repl() {
    let mut editor = match DefaultEditor::new() {
        Ok(ed) => ed,
        Err(e) => {
            eprintln!("failed to initialize REPL: {}", e);
            process::exit(1);
        }
    };

    let mut vm = create_vm();

    loop {
        let input = match read_input(&mut editor) {
            Some(line) => line,
            None => break, // EOF or .exit
        };

        if input.trim().is_empty() {
            continue;
        }

        match compile_repl_source(&input) {
            Ok(program) => match vm.execute(program) {
                Ok(val) => {
                    if !matches!(val, Value::Null) {
                        println!("{}", val.to_display_string());
                    }
                }
                Err(e) => {
                    eprintln!("{}", cirq::error::format_error(&e, &input, "<repl>"));
                }
            },
            Err(e) => {
                eprintln!("{}", cirq::error::format_error(&e, &input, "<repl>"));
            }
        }
    }
}

fn read_input(editor: &mut DefaultEditor) -> Option<String> {
    let first_line = match editor.readline(">> ") {
        Ok(line) => line,
        Err(ReadlineError::Eof | ReadlineError::Interrupted) => return None,
        Err(e) => {
            eprintln!("readline error: {}", e);
            return None;
        }
    };

    if first_line.trim() == ".exit" {
        return None;
    }

    let mut buffer = first_line;

    while needs_continuation(&buffer) {
        match editor.readline(".. ") {
            Ok(line) => {
                buffer.push('\n');
                buffer.push_str(&line);
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(e) => {
                eprintln!("readline error: {}", e);
                break;
            }
        }
    }

    let _ = editor.add_history_entry(&buffer);
    Some(buffer)
}

fn needs_continuation(input: &str) -> bool {
    let mut braces: i32 = 0;
    let mut parens: i32 = 0;
    let mut brackets: i32 = 0;
    let mut in_string = false;
    let mut string_quote: u8 = 0;
    let mut prev_byte: u8 = 0;

    for &byte in input.as_bytes() {
        if in_string {
            if byte == string_quote && prev_byte != b'\\' {
                in_string = false;
            }
            prev_byte = byte;
            continue;
        }

        match byte {
            b'"' | b'\'' => {
                in_string = true;
                string_quote = byte;
            }
            b'{' => braces += 1,
            b'}' => braces -= 1,
            b'(' => parens += 1,
            b')' => parens -= 1,
            b'[' => brackets += 1,
            b']' => brackets -= 1,
            _ => {}
        }

        prev_byte = byte;
    }

    braces > 0 || parens > 0 || brackets > 0
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

fn compile_source(source: &str) -> Result<cirq::opcode::CompiledFunction, cirq::error::CirqError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let mut parser = CirqParser::new(tokens);
    let ast = parser.parse()?;

    let compiler = Compiler::new();
    compiler.compile(&ast)
}

fn compile_repl_source(
    source: &str,
) -> Result<cirq::opcode::CompiledFunction, cirq::error::CirqError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;

    let mut parser = CirqParser::new(tokens);
    let ast = parser.parse()?;

    let compiler = Compiler::new();
    compiler.compile_repl(&ast)
}

fn run(source: &str) -> Result<(), cirq::error::CirqError> {
    let program = compile_source(source)?;
    let mut vm = create_vm();
    vm.execute(program)?;
    Ok(())
}
