use crate::value::{Class, Instance, Module, NativeFn, Value};

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::io::{self, BufRead, Read, Write};
use std::rc::Rc;

pub struct NativeClassDef {
    pub name: &'static str,
    pub constructor: Option<(u8, NativeFn)>,
    pub methods: Vec<(&'static str, u8, NativeFn)>,
}

pub trait BuiltinModule {
    fn name(&self) -> &'static str;

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)>;

    fn constants(&self) -> Vec<(&'static str, Value)> {
        Vec::new()
    }

    fn classes(&self) -> Vec<NativeClassDef> {
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

        for class_def in self.classes() {
            let mut methods: FxHashMap<String, Value> = FxHashMap::default();

            if let Some((arity, func)) = class_def.constructor {
                methods.insert(
                    "init".to_string(),
                    Value::NativeFun {
                        name: "init",
                        arity,
                        func,
                    },
                );
            }

            for (name, arity, func) in class_def.methods {
                methods.insert(name.to_string(), Value::NativeFun { name, arity, func });
            }

            let class = Class {
                name: class_def.name.to_string(),
                methods,
            };
            members.push((class_def.name.to_string(), Value::Class(Rc::new(class))));
        }

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

pub struct TimeModule;

impl BuiltinModule for TimeModule {
    fn name(&self) -> &'static str {
        "time"
    }

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)> {
        vec![
            ("now", 0, time_now as NativeFn),
            ("from_timestamp", 1, time_from_timestamp as NativeFn),
            ("from_date", 3, time_from_date as NativeFn),
            ("from_datetime", 6, time_from_datetime as NativeFn),
            ("clock", 0, time_clock as NativeFn),
            ("elapsed", 1, time_elapsed as NativeFn),
            ("sleep", 1, time_sleep as NativeFn),
            ("sleep_ms", 1, time_sleep_ms as NativeFn),
        ]
    }

    fn classes(&self) -> Vec<NativeClassDef> {
        vec![NativeClassDef {
            name: "DateTime",
            constructor: Some((1, dt_init as NativeFn)),
            methods: vec![
                ("to_iso", 0, dt_to_iso as NativeFn),
                ("to_date", 0, dt_to_date as NativeFn),
                ("to_clock", 0, dt_to_clock as NativeFn),
                ("add_days", 1, dt_add_days as NativeFn),
                ("add_hours", 1, dt_add_hours as NativeFn),
                ("add_secs", 1, dt_add_secs as NativeFn),
                ("diff", 1, dt_diff as NativeFn),
                ("is_leap", 0, dt_is_leap as NativeFn),
                ("days_in_month", 0, dt_days_in_month as NativeFn),
            ],
        }]
    }
}

fn build_datetime_instance(ts: f64) -> Value {
    use chrono::{DateTime, Datelike, TimeZone, Timelike, Utc};

    let secs = ts as i64;
    let nanos = ((ts - secs as f64) * 1_000_000_000.0) as u32;
    let dt: DateTime<Utc> = Utc
        .timestamp_opt(secs, nanos)
        .single()
        .unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());

    let class = Class {
        name: "DateTime".to_string(),
        methods: {
            let mut m: FxHashMap<String, Value> = FxHashMap::default();
            m.insert(
                "init".to_string(),
                Value::NativeFun {
                    name: "init",
                    arity: 1,
                    func: dt_init,
                },
            );
            for (name, arity, func) in [
                ("to_iso", 0u8, dt_to_iso as NativeFn),
                ("to_date", 0, dt_to_date as NativeFn),
                ("to_clock", 0, dt_to_clock as NativeFn),
                ("add_days", 1, dt_add_days as NativeFn),
                ("add_hours", 1, dt_add_hours as NativeFn),
                ("add_secs", 1, dt_add_secs as NativeFn),
                ("diff", 1, dt_diff as NativeFn),
                ("is_leap", 0, dt_is_leap as NativeFn),
                ("days_in_month", 0, dt_days_in_month as NativeFn),
            ] {
                m.insert(name.to_string(), Value::NativeFun { name, arity, func });
            }
            m
        },
    };

    let mut fields: FxHashMap<String, Value> = FxHashMap::default();
    fields.insert("timestamp".to_string(), Value::Num(ts));
    fields.insert("year".to_string(), Value::Num(dt.year() as f64));
    fields.insert("month".to_string(), Value::Num(dt.month() as f64));
    fields.insert("day".to_string(), Value::Num(dt.day() as f64));
    fields.insert("hour".to_string(), Value::Num(dt.hour() as f64));
    fields.insert("minute".to_string(), Value::Num(dt.minute() as f64));
    fields.insert("second".to_string(), Value::Num(dt.second() as f64));
    fields.insert(
        "weekday".to_string(),
        Value::Num(dt.weekday().num_days_from_monday() as f64),
    );
    fields.insert("yearday".to_string(), Value::Num(dt.ordinal() as f64));

    let instance = Instance {
        class: Rc::new(class),
        fields,
    };
    Value::Instance(Rc::new(RefCell::new(instance)))
}

fn get_timestamp(inst: &Instance) -> f64 {
    match inst.fields.get("timestamp") {
        Some(Value::Num(n)) => *n,
        _ => 0.0,
    }
}

fn time_now(_args: &[Value]) -> Result<Value, String> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| e.to_string())?
        .as_secs_f64();
    Ok(build_datetime_instance(ts))
}

fn time_from_timestamp(args: &[Value]) -> Result<Value, String> {
    let ts = expect_num(args, 0, "from_timestamp")?;
    Ok(build_datetime_instance(ts))
}

fn time_from_date(args: &[Value]) -> Result<Value, String> {
    use chrono::{TimeZone, Utc};

    let y = expect_num(args, 0, "from_date")? as i32;
    let m = expect_num(args, 1, "from_date")? as u32;
    let d = expect_num(args, 2, "from_date")? as u32;

    let dt = Utc
        .with_ymd_and_hms(y, m, d, 0, 0, 0)
        .single()
        .ok_or_else(|| format!("invalid date: {}-{}-{}", y, m, d))?;
    Ok(build_datetime_instance(dt.timestamp() as f64))
}

fn time_from_datetime(args: &[Value]) -> Result<Value, String> {
    use chrono::{TimeZone, Utc};

    let y = expect_num(args, 0, "from_datetime")? as i32;
    let m = expect_num(args, 1, "from_datetime")? as u32;
    let d = expect_num(args, 2, "from_datetime")? as u32;
    let h = expect_num(args, 3, "from_datetime")? as u32;
    let min = expect_num(args, 4, "from_datetime")? as u32;
    let s = expect_num(args, 5, "from_datetime")? as u32;

    let dt = Utc
        .with_ymd_and_hms(y, m, d, h, min, s)
        .single()
        .ok_or_else(|| format!("invalid datetime: {}-{}-{} {}:{}:{}", y, m, d, h, min, s))?;
    Ok(build_datetime_instance(dt.timestamp() as f64))
}

fn time_clock(_args: &[Value]) -> Result<Value, String> {
    use std::time::Instant;

    static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
    let start = START.get_or_init(Instant::now);
    Ok(Value::Num(start.elapsed().as_secs_f64()))
}

fn time_elapsed(args: &[Value]) -> Result<Value, String> {
    use std::time::Instant;

    static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
    let start = START.get_or_init(Instant::now);
    let prev = expect_num(args, 0, "elapsed")?;
    let now = start.elapsed().as_secs_f64();
    Ok(Value::Num(now - prev))
}

fn time_sleep(args: &[Value]) -> Result<Value, String> {
    let secs = expect_num(args, 0, "sleep")?;
    if secs < 0.0 {
        return Err("sleep duration must be non-negative".to_string());
    }
    std::thread::sleep(std::time::Duration::from_secs_f64(secs));
    Ok(Value::Null)
}

fn time_sleep_ms(args: &[Value]) -> Result<Value, String> {
    let ms = expect_num(args, 0, "sleep_ms")?;
    if ms < 0.0 {
        return Err("sleep duration must be non-negative".to_string());
    }
    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    Ok(Value::Null)
}

fn dt_init(args: &[Value]) -> Result<Value, String> {
    let ts = expect_num(args, 1, "DateTime.init")?;
    if let Value::Instance(inst) = &args[0] {
        use chrono::{DateTime, Datelike, TimeZone, Timelike, Utc};

        let secs = ts as i64;
        let nanos = ((ts - secs as f64) * 1_000_000_000.0) as u32;
        let dt: DateTime<Utc> = Utc
            .timestamp_opt(secs, nanos)
            .single()
            .unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());

        let mut inst = inst.borrow_mut();
        inst.fields.insert("timestamp".to_string(), Value::Num(ts));
        inst.fields
            .insert("year".to_string(), Value::Num(dt.year() as f64));
        inst.fields
            .insert("month".to_string(), Value::Num(dt.month() as f64));
        inst.fields
            .insert("day".to_string(), Value::Num(dt.day() as f64));
        inst.fields
            .insert("hour".to_string(), Value::Num(dt.hour() as f64));
        inst.fields
            .insert("minute".to_string(), Value::Num(dt.minute() as f64));
        inst.fields
            .insert("second".to_string(), Value::Num(dt.second() as f64));
        inst.fields.insert(
            "weekday".to_string(),
            Value::Num(dt.weekday().num_days_from_monday() as f64),
        );
        inst.fields
            .insert("yearday".to_string(), Value::Num(dt.ordinal() as f64));
    }
    Ok(Value::Null)
}

fn dt_to_iso(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        use chrono::{TimeZone, Utc};
        let ts = get_timestamp(&inst.borrow());
        let dt = Utc
            .timestamp_opt(ts as i64, 0)
            .single()
            .unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
        Ok(Value::Str(Rc::new(
            dt.format("%Y-%m-%dT%H:%M:%SZ").to_string(),
        )))
    } else {
        Err("to_iso requires a DateTime instance".to_string())
    }
}

fn dt_to_date(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        use chrono::{TimeZone, Utc};
        let ts = get_timestamp(&inst.borrow());
        let dt = Utc
            .timestamp_opt(ts as i64, 0)
            .single()
            .unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
        Ok(Value::Str(Rc::new(dt.format("%Y-%m-%d").to_string())))
    } else {
        Err("to_date requires a DateTime instance".to_string())
    }
}

fn dt_to_clock(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        use chrono::{TimeZone, Utc};
        let ts = get_timestamp(&inst.borrow());
        let dt = Utc
            .timestamp_opt(ts as i64, 0)
            .single()
            .unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
        Ok(Value::Str(Rc::new(dt.format("%H:%M:%S").to_string())))
    } else {
        Err("to_clock requires a DateTime instance".to_string())
    }
}

fn dt_add_days(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        let n = expect_num(args, 1, "add_days")?;
        let ts = get_timestamp(&inst.borrow());
        Ok(build_datetime_instance(ts + n * 86400.0))
    } else {
        Err("add_days requires a DateTime instance".to_string())
    }
}

fn dt_add_hours(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        let n = expect_num(args, 1, "add_hours")?;
        let ts = get_timestamp(&inst.borrow());
        Ok(build_datetime_instance(ts + n * 3600.0))
    } else {
        Err("add_hours requires a DateTime instance".to_string())
    }
}

fn dt_add_secs(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        let n = expect_num(args, 1, "add_secs")?;
        let ts = get_timestamp(&inst.borrow());
        Ok(build_datetime_instance(ts + n))
    } else {
        Err("add_secs requires a DateTime instance".to_string())
    }
}

fn dt_diff(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst_a) = &args[0] {
        if let Value::Instance(inst_b) = &args[1] {
            let ts_a = get_timestamp(&inst_a.borrow());
            let ts_b = get_timestamp(&inst_b.borrow());
            Ok(Value::Num(ts_a - ts_b))
        } else {
            Err("diff expects a DateTime argument".to_string())
        }
    } else {
        Err("diff requires a DateTime instance".to_string())
    }
}

fn dt_is_leap(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        let year = match inst.borrow().fields.get("year") {
            Some(Value::Num(n)) => *n as i32,
            _ => return Err("missing year field".to_string()),
        };
        let leap = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
        Ok(Value::Bool(leap))
    } else {
        Err("is_leap requires a DateTime instance".to_string())
    }
}

fn dt_days_in_month(args: &[Value]) -> Result<Value, String> {
    if let Value::Instance(inst) = &args[0] {
        let inst_ref = inst.borrow();
        let year = match inst_ref.fields.get("year") {
            Some(Value::Num(n)) => *n as i32,
            _ => return Err("missing year field".to_string()),
        };
        let month = match inst_ref.fields.get("month") {
            Some(Value::Num(n)) => *n as u32,
            _ => return Err("missing month field".to_string()),
        };
        let days = match month {
            1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
            4 | 6 | 9 | 11 => 30,
            2 => {
                if (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0) {
                    29
                } else {
                    28
                }
            }
            _ => return Err(format!("invalid month: {}", month)),
        };
        Ok(Value::Num(days as f64))
    } else {
        Err("days_in_month requires a DateTime instance".to_string())
    }
}

pub struct EnvModule;

impl BuiltinModule for EnvModule {
    fn name(&self) -> &'static str {
        "env"
    }

    fn functions(&self) -> Vec<(&'static str, u8, NativeFn)> {
        vec![
            ("get", 1, env_get as NativeFn),
            ("set", 2, env_set as NativeFn),
            ("remove", 1, env_remove as NativeFn),
            ("has", 1, env_has as NativeFn),
            ("vars", 0, env_vars as NativeFn),
            ("args", 0, env_args as NativeFn),
            ("cwd", 0, env_cwd as NativeFn),
            ("home", 0, env_home as NativeFn),
            ("os", 0, env_os as NativeFn),
            ("arch", 0, env_arch as NativeFn),
            ("pid", 0, env_pid as NativeFn),
            ("exit", 1, env_exit as NativeFn),
        ]
    }
}

fn env_get(args: &[Value]) -> Result<Value, String> {
    let key = args[0].to_display_string();
    match std::env::var(&key) {
        Ok(val) => Ok(Value::Str(Rc::new(val))),
        Err(_) => Ok(Value::Null),
    }
}

fn env_set(args: &[Value]) -> Result<Value, String> {
    let key = args[0].to_display_string();
    let val = args[1].to_display_string();
    unsafe {
        std::env::set_var(&key, &val);
    }
    Ok(Value::Null)
}

fn env_remove(args: &[Value]) -> Result<Value, String> {
    let key = args[0].to_display_string();
    unsafe {
        std::env::remove_var(&key);
    }
    Ok(Value::Null)
}

fn env_has(args: &[Value]) -> Result<Value, String> {
    let key = args[0].to_display_string();
    Ok(Value::Bool(std::env::var(&key).is_ok()))
}

fn env_vars(_args: &[Value]) -> Result<Value, String> {
    let pairs: Vec<Value> = std::env::vars()
        .map(|(k, v)| {
            Value::Array(Rc::new(RefCell::new(vec![
                Value::Str(Rc::new(k)),
                Value::Str(Rc::new(v)),
            ])))
        })
        .collect();
    Ok(Value::Array(Rc::new(RefCell::new(pairs))))
}

fn env_args(_args: &[Value]) -> Result<Value, String> {
    let args: Vec<Value> = std::env::args().map(|a| Value::Str(Rc::new(a))).collect();
    Ok(Value::Array(Rc::new(RefCell::new(args))))
}

fn env_cwd(_args: &[Value]) -> Result<Value, String> {
    let cwd = std::env::current_dir().map_err(|e| e.to_string())?;
    Ok(Value::Str(Rc::new(cwd.to_string_lossy().into_owned())))
}

fn env_home(_args: &[Value]) -> Result<Value, String> {
    match std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
        Ok(home) => Ok(Value::Str(Rc::new(home))),
        Err(_) => Ok(Value::Null),
    }
}

fn env_os(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Str(Rc::new(std::env::consts::OS.to_string())))
}

fn env_arch(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Str(Rc::new(std::env::consts::ARCH.to_string())))
}

fn env_pid(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Num(std::process::id() as f64))
}

fn env_exit(args: &[Value]) -> Result<Value, String> {
    let code = expect_num(args, 0, "exit")? as i32;
    std::process::exit(code);
}
