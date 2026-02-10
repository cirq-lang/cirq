//! # Builtin Module
//!
//! Trait-based system for registering builtin modules into the Cirq VM.
//! Each builtin module implements [`BuiltinModule`] and exposes named
//! native functions with fixed arity — no variadic, no optional params.
//!
//! ## Provided Modules
//! - `io` — Standard I/O operations (print, read, write files).

use crate::value::{Module, NativeFn, Value};

use std::io::{self, BufRead, Read, Write};
use std::rc::Rc;

// -----------------------------------------------------------------------------
// BUILTIN MODULE TRAIT
// -----------------------------------------------------------------------------

/// Trait for defining builtin modules that can be registered into the VM.
///
/// Implementors provide a module name and a list of `(name, arity, fn)`
/// tuples that become native function values in the module.
pub trait BuiltinModule {
    /// Returns the module name (e.g., `"io"`).
    fn name(&self) -> &'static str;

    /// Returns a list of `(function_name, arity, function_pointer)` tuples.
    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)>;

    /// Builds a [`Module`] value from this builtin definition.
    fn build(&self) -> Module {
        let members = self
            .functions()
            .into_iter()
            .map(|(name, arity, func)| (name.to_string(), Value::NativeFun { name, arity, func }))
            .collect();

        Module {
            name: self.name().to_string(),
            members,
        }
    }
}

// -----------------------------------------------------------------------------
// IO MODULE
// -----------------------------------------------------------------------------

/// The `io` builtin module providing standard I/O operations.
///
/// All functions have fixed arity with no optional or variadic parameters.
/// File I/O operations automatically manage memory — no manual cleanup.
pub struct IoModule;

impl BuiltinModule for IoModule {
    fn name(&self) -> &'static str {
        "io"
    }

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)> {
        vec![
            ("print", 1, io_print as NativeFn),
            ("printn", 1, io_printn as NativeFn),
            ("eprint", 1, io_eprint as NativeFn),
            ("eprintn", 1, io_eprintn as NativeFn),
            ("input", 1, io_input as NativeFn),
            ("read", 1, io_read as NativeFn),
            ("readline", 1, io_readline as NativeFn),
            ("readlines", 1, io_readlines as NativeFn),
            ("write", 2, io_write as NativeFn),
        ]
    }
}

// -----------------------------------------------------------------------------
// IO FUNCTION IMPLEMENTATIONS
// -----------------------------------------------------------------------------

/// Prints a value to stdout without a trailing newline.
fn io_print(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    print!("{}", text);
    io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(Value::Null)
}

/// Prints a value to stdout with a trailing newline.
fn io_printn(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    println!("{}", text);
    Ok(Value::Null)
}

/// Prints a value to stderr without a trailing newline.
fn io_eprint(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    eprint!("{}", text);
    io::stderr().flush().map_err(|e| e.to_string())?;
    Ok(Value::Null)
}

/// Prints a value to stderr with a trailing newline.
fn io_eprintn(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    eprintln!("{}", text);
    Ok(Value::Null)
}

/// Prints a prompt to stdout, then reads a line from stdin.
fn io_input(args: &[Value]) -> Result<Value, String> {
    let prompt = args[0].to_display_string();
    print!("{}", prompt);
    io::stdout().flush().map_err(|e| e.to_string())?;

    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;

    // Strip trailing newline
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }

    Ok(Value::Str(Rc::new(line)))
}

/// Reads the entire contents of a file into a string.
fn io_read(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let mut file = std::fs::File::open(&path).map_err(|e| format!("{}: {}", path, e))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|e| format!("{}: {}", path, e))?;
    Ok(Value::Str(Rc::new(contents)))
}

/// Reads the first line of a file.
fn io_readline(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let file = std::fs::File::open(&path).map_err(|e| format!("{}: {}", path, e))?;
    let reader = io::BufReader::new(file);
    let mut line = String::new();
    reader
        .take(1024 * 1024) // safety limit: 1MB for a single line
        .read_line(&mut line)
        .map_err(|e| format!("{}: {}", path, e))?;

    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }

    Ok(Value::Str(Rc::new(line)))
}

/// Reads all lines of a file, returning an array of strings.
fn io_readlines(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let file = std::fs::File::open(&path).map_err(|e| format!("{}: {}", path, e))?;
    let reader = io::BufReader::new(file);
    let mut lines = Vec::new();

    for line_result in reader.lines() {
        let line = line_result.map_err(|e| format!("{}: {}", path, e))?;
        lines.push(Value::Str(Rc::new(line)));
    }

    Ok(Value::Array(Rc::new(std::cell::RefCell::new(lines))))
}

/// Writes a string to a file, creating or overwriting it.
fn io_write(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let content = args[1].to_display_string();
    std::fs::write(&path, &content).map_err(|e| format!("{}: {}", path, e))?;
    Ok(Value::Null)
}
