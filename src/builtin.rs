use crate::value::{Module, NativeFn, Value};

use std::io::{self, BufRead, Read, Write};
use std::rc::Rc;
pub trait BuiltinModule {
    fn name(&self) -> &'static str;

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)>;

    fn constants(&self) -> Vec<(&'static str, Value)> {
        Vec::new()
    }

    fn build(&self) -> Module {
        let mut members: Vec<(String, Value)> = self
            .constants()
            .into_iter()
            .map(|(name, val)| (name.to_string(), val))
            .collect();

        members.extend(
            self.functions().into_iter().map(|(name, arity, func)| {
                (name.to_string(), Value::NativeFun { name, arity, func })
            }),
        );

        Module {
            name: self.name().to_string(),
            members,
        }
    }
}
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
fn io_print(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    print!("{}", text);
    io::stdout().flush().map_err(|e| e.to_string())?;
    Ok(Value::Null)
}

fn io_printn(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    println!("{}", text);
    Ok(Value::Null)
}

fn io_eprint(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    eprint!("{}", text);
    io::stderr().flush().map_err(|e| e.to_string())?;
    Ok(Value::Null)
}

fn io_eprintn(args: &[Value]) -> Result<Value, String> {
    let text = args[0].to_display_string();
    eprintln!("{}", text);
    Ok(Value::Null)
}

fn io_input(args: &[Value]) -> Result<Value, String> {
    let prompt = args[0].to_display_string();
    print!("{}", prompt);
    io::stdout().flush().map_err(|e| e.to_string())?;

    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;

    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }

    Ok(Value::Str(Rc::new(line)))
}

fn io_read(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let mut file = std::fs::File::open(&path).map_err(|e| format!("{}: {}", path, e))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|e| format!("{}: {}", path, e))?;
    Ok(Value::Str(Rc::new(contents)))
}

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

fn io_write(args: &[Value]) -> Result<Value, String> {
    let path = args[0].to_display_string();
    let content = args[1].to_display_string();
    std::fs::write(&path, &content).map_err(|e| format!("{}: {}", path, e))?;
    Ok(Value::Null)
}
pub struct MathModule;

impl BuiltinModule for MathModule {
    fn name(&self) -> &'static str {
        "math"
    }

    fn constants(&self) -> Vec<(&'static str, Value)> {
        vec![
            ("PI", Value::Num(std::f64::consts::PI)),
            ("E", Value::Num(std::f64::consts::E)),
        ]
    }

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)> {
        vec![
            ("sin", 1, math_sin as NativeFn),
            ("cos", 1, math_cos as NativeFn),
            ("tan", 1, math_tan as NativeFn),
            ("asin", 1, math_asin as NativeFn),
            ("acos", 1, math_acos as NativeFn),
            ("atan", 1, math_atan as NativeFn),
            ("atan2", 2, math_atan2 as NativeFn),
            ("sqrt", 1, math_sqrt as NativeFn),
            ("cbrt", 1, math_cbrt as NativeFn),
            ("pow", 2, math_pow as NativeFn),
            ("log", 1, math_log as NativeFn),
            ("log2", 1, math_log2 as NativeFn),
            ("log10", 1, math_log10 as NativeFn),
            ("exp", 1, math_exp as NativeFn),
            ("min", 2, math_min as NativeFn),
            ("max", 2, math_max as NativeFn),
            ("clamp", 3, math_clamp as NativeFn),
            ("random", 0, math_random as NativeFn),
            ("sign", 1, math_sign as NativeFn),
        ]
    }
}
fn expect_num(args: &[Value], idx: usize, fn_name: &str) -> Result<f64, String> {
    match &args[idx] {
        Value::Num(n) => Ok(*n),
        other => Err(format!(
            "math.{} expected number, got {}",
            fn_name,
            other.type_name()
        )),
    }
}

fn math_sin(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "sin")?.sin()))
}

fn math_cos(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "cos")?.cos()))
}

fn math_tan(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "tan")?.tan()))
}

fn math_asin(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "asin")?.asin()))
}

fn math_acos(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "acos")?.acos()))
}

fn math_atan(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "atan")?.atan()))
}

fn math_atan2(args: &[Value]) -> Result<Value, String> {
    let y = expect_num(args, 0, "atan2")?;
    let x = expect_num(args, 1, "atan2")?;
    Ok(Value::Num(y.atan2(x)))
}

fn math_sqrt(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "sqrt")?.sqrt()))
}

fn math_cbrt(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "cbrt")?.cbrt()))
}

fn math_pow(args: &[Value]) -> Result<Value, String> {
    let base = expect_num(args, 0, "pow")?;
    let exp = expect_num(args, 1, "pow")?;
    Ok(Value::Num(base.powf(exp)))
}

fn math_log(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "log")?.ln()))
}

fn math_log2(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "log2")?.log2()))
}

fn math_log10(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "log10")?.log10()))
}

fn math_exp(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(expect_num(args, 0, "exp")?.exp()))
}

fn math_min(args: &[Value]) -> Result<Value, String> {
    let a = expect_num(args, 0, "min")?;
    let b = expect_num(args, 1, "min")?;
    Ok(Value::Num(a.min(b)))
}

fn math_max(args: &[Value]) -> Result<Value, String> {
    let a = expect_num(args, 0, "max")?;
    let b = expect_num(args, 1, "max")?;
    Ok(Value::Num(a.max(b)))
}

fn math_clamp(args: &[Value]) -> Result<Value, String> {
    let x = expect_num(args, 0, "clamp")?;
    let lo = expect_num(args, 1, "clamp")?;
    let hi = expect_num(args, 2, "clamp")?;
    Ok(Value::Num(x.clamp(lo, hi)))
}

fn math_random(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(rand::random::<f64>()))
}

fn math_sign(args: &[Value]) -> Result<Value, String> {
    let n = expect_num(args, 0, "sign")?;
    let result = if n > 0.0 {
        1.0
    } else if n < 0.0 {
        -1.0
    } else {
        0.0
    };
    Ok(Value::Num(result))
}
