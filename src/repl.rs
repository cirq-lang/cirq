use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

use cirq::compiler::Compiler;
use cirq::lexer::Lexer;
use cirq::parser::Parser as CirqParser;
use cirq::value::Value;
use cirq::vm::Vm;

pub fn start(vm: &mut Vm) {
    let mut editor = match DefaultEditor::new() {
        Ok(ed) => ed,
        Err(e) => {
            eprintln!("failed to initialize REPL: {}", e);
            std::process::exit(1);
        }
    };

    loop {
        let input = match read_input(&mut editor) {
            Some(line) => line,
            None => break,
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
