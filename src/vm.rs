use crate::error::{CirqError, CirqResult};
use crate::opcode::{CompiledFunction, Instruction};
use crate::value::{Instance, Module, Value};

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
#[derive(Debug)]
struct CallFrame {
    function: Rc<CompiledFunction>,
    ip: usize,
    base: usize,
}
pub struct Vm {
    globals: FxHashMap<String, Value>,
    registers: Vec<Value>,
    frames: Vec<CallFrame>,
    type_methods: FxHashMap<&'static str, FxHashMap<&'static str, Value>>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            globals: FxHashMap::default(),
            registers: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            type_methods: FxHashMap::default(),
        };
        vm.register_builtin_methods();
        vm
    }
    fn register_builtin_methods(&mut self) {
        self.register_num_methods();
        self.register_str_methods();
        self.register_bool_methods();
        self.register_null_methods();
        self.register_array_methods();
    }
    fn register_num_methods(&mut self) {
        self.register_type_method(
            "num",
            "to_str",
            Value::NativeFun {
                name: "to_str",
                arity: 0,
                func: |args| {
                    if let Value::Num(n) = &args[0] {
                        let s = if *n == n.floor() && n.is_finite() {
                            format!("{}", *n as i64)
                        } else {
                            format!("{}", n)
                        };
                        Ok(Value::Str(Rc::new(s)))
                    } else {
                        Err("to_str called on non-num".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "num",
            "to_num",
            Value::NativeFun {
                name: "to_num",
                arity: 0,
                func: |args| Ok(args[0].clone()),
            },
        );

        self.register_type_method(
            "num",
            "abs",
            Value::NativeFun {
                name: "abs",
                arity: 0,
                func: |args| {
                    if let Value::Num(n) = &args[0] {
                        Ok(Value::Num(n.abs()))
                    } else {
                        Err("abs called on non-num".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "num",
            "floor",
            Value::NativeFun {
                name: "floor",
                arity: 0,
                func: |args| {
                    if let Value::Num(n) = &args[0] {
                        Ok(Value::Num(n.floor()))
                    } else {
                        Err("floor called on non-num".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "num",
            "ceil",
            Value::NativeFun {
                name: "ceil",
                arity: 0,
                func: |args| {
                    if let Value::Num(n) = &args[0] {
                        Ok(Value::Num(n.ceil()))
                    } else {
                        Err("ceil called on non-num".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "num",
            "round",
            Value::NativeFun {
                name: "round",
                arity: 0,
                func: |args| {
                    if let Value::Num(n) = &args[0] {
                        Ok(Value::Num(n.round()))
                    } else {
                        Err("round called on non-num".to_string())
                    }
                },
            },
        );
    }
    fn register_str_methods(&mut self) {
        self.register_type_method(
            "str",
            "to_str",
            Value::NativeFun {
                name: "to_str",
                arity: 0,
                func: |args| Ok(args[0].clone()),
            },
        );

        self.register_type_method(
            "str",
            "to_num",
            Value::NativeFun {
                name: "to_num",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        match s.trim().parse::<f64>() {
                            Ok(n) => Ok(Value::Num(n)),
                            Err(_) => Err(format!("cannot convert '{}' to number", s)),
                        }
                    } else {
                        Err("to_num called on non-str".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "len",
            Value::NativeFun {
                name: "len",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        Ok(Value::Num(s.len() as f64))
                    } else {
                        Err("len called on non-str".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "upper",
            Value::NativeFun {
                name: "upper",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        Ok(Value::Str(Rc::new(s.to_uppercase())))
                    } else {
                        Err("upper called on non-str".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "lower",
            Value::NativeFun {
                name: "lower",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        Ok(Value::Str(Rc::new(s.to_lowercase())))
                    } else {
                        Err("lower called on non-str".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "trim",
            Value::NativeFun {
                name: "trim",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        Ok(Value::Str(Rc::new(s.trim().to_string())))
                    } else {
                        Err("trim called on non-str".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "contains",
            Value::NativeFun {
                name: "contains",
                arity: 1,
                func: |args| {
                    if let (Value::Str(s), Value::Str(sub)) = (&args[0], &args[1]) {
                        Ok(Value::Bool(s.contains(sub.as_str())))
                    } else {
                        Err("contains expects a string argument".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "split",
            Value::NativeFun {
                name: "split",
                arity: 1,
                func: |args| {
                    if let (Value::Str(s), Value::Str(sep)) = (&args[0], &args[1]) {
                        let parts: Vec<Value> = s
                            .split(sep.as_str())
                            .map(|p| Value::Str(Rc::new(p.to_string())))
                            .collect();
                        Ok(Value::Array(Rc::new(RefCell::new(parts))))
                    } else {
                        Err("split expects a string separator".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "starts_with",
            Value::NativeFun {
                name: "starts_with",
                arity: 1,
                func: |args| {
                    if let (Value::Str(s), Value::Str(pre)) = (&args[0], &args[1]) {
                        Ok(Value::Bool(s.starts_with(pre.as_str())))
                    } else {
                        Err("starts_with expects a string argument".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "ends_with",
            Value::NativeFun {
                name: "ends_with",
                arity: 1,
                func: |args| {
                    if let (Value::Str(s), Value::Str(suf)) = (&args[0], &args[1]) {
                        Ok(Value::Bool(s.ends_with(suf.as_str())))
                    } else {
                        Err("ends_with expects a string argument".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "replace",
            Value::NativeFun {
                name: "replace",
                arity: 2,
                func: |args| {
                    if let (Value::Str(s), Value::Str(old), Value::Str(new)) =
                        (&args[0], &args[1], &args[2])
                    {
                        Ok(Value::Str(Rc::new(s.replace(old.as_str(), new.as_str()))))
                    } else {
                        Err("replace expects two string arguments".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "slice",
            Value::NativeFun {
                name: "slice",
                arity: 2,
                func: |args| {
                    if let (Value::Str(s), Value::Num(start), Value::Num(end)) =
                        (&args[0], &args[1], &args[2])
                    {
                        let len = s.len() as i64;
                        let a = (*start as i64).max(0).min(len) as usize;
                        let b = (*end as i64).max(0).min(len) as usize;
                        if a <= b {
                            Ok(Value::Str(Rc::new(s[a..b].to_string())))
                        } else {
                            Ok(Value::Str(Rc::new(String::new())))
                        }
                    } else {
                        Err("slice expects two number arguments".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "index_of",
            Value::NativeFun {
                name: "index_of",
                arity: 1,
                func: |args| {
                    if let (Value::Str(s), Value::Str(sub)) = (&args[0], &args[1]) {
                        match s.find(sub.as_str()) {
                            Some(pos) => Ok(Value::Num(pos as f64)),
                            None => Ok(Value::Num(-1.0)),
                        }
                    } else {
                        Err("index_of expects a string argument".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "str",
            "chars",
            Value::NativeFun {
                name: "chars",
                arity: 0,
                func: |args| {
                    if let Value::Str(s) = &args[0] {
                        let chars: Vec<Value> = s
                            .chars()
                            .map(|c| Value::Str(Rc::new(c.to_string())))
                            .collect();
                        Ok(Value::Array(Rc::new(RefCell::new(chars))))
                    } else {
                        Err("chars called on non-str".to_string())
                    }
                },
            },
        );
    }
    fn register_bool_methods(&mut self) {
        self.register_type_method(
            "bool",
            "to_str",
            Value::NativeFun {
                name: "to_str",
                arity: 0,
                func: |args| {
                    if let Value::Bool(b) = &args[0] {
                        Ok(Value::Str(Rc::new(b.to_string())))
                    } else {
                        Err("to_str called on non-bool".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "bool",
            "to_num",
            Value::NativeFun {
                name: "to_num",
                arity: 0,
                func: |args| {
                    if let Value::Bool(b) = &args[0] {
                        Ok(Value::Num(if *b { 1.0 } else { 0.0 }))
                    } else {
                        Err("to_num called on non-bool".to_string())
                    }
                },
            },
        );
    }
    fn register_null_methods(&mut self) {
        self.register_type_method(
            "null",
            "to_str",
            Value::NativeFun {
                name: "to_str",
                arity: 0,
                func: |_| Ok(Value::Str(Rc::new("null".to_string()))),
            },
        );

        self.register_type_method(
            "null",
            "to_num",
            Value::NativeFun {
                name: "to_num",
                arity: 0,
                func: |_| Ok(Value::Num(0.0)),
            },
        );
    }
    fn register_array_methods(&mut self) {
        self.register_type_method(
            "array",
            "push",
            Value::NativeFun {
                name: "push",
                arity: 1,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        arr.borrow_mut().push(args[1].clone());
                        Ok(Value::Null)
                    } else {
                        Err("push called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "pop",
            Value::NativeFun {
                name: "pop",
                arity: 0,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        arr.borrow_mut()
                            .pop()
                            .ok_or_else(|| "pop on empty array".to_string())
                    } else {
                        Err("pop called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "len",
            Value::NativeFun {
                name: "len",
                arity: 0,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        Ok(Value::Num(arr.borrow().len() as f64))
                    } else {
                        Err("len called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "remove",
            Value::NativeFun {
                name: "remove",
                arity: 1,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        if let Value::Num(n) = &args[1] {
                            let idx = *n as usize;
                            let mut elems = arr.borrow_mut();
                            if idx >= elems.len() {
                                return Err(format!(
                                    "remove index {} out of bounds (length {})",
                                    idx,
                                    elems.len()
                                ));
                            }
                            Ok(elems.remove(idx))
                        } else {
                            Err("remove index must be a number".to_string())
                        }
                    } else {
                        Err("remove called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "insert",
            Value::NativeFun {
                name: "insert",
                arity: 2,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        if let Value::Num(n) = &args[1] {
                            let idx = *n as usize;
                            let mut elems = arr.borrow_mut();
                            if idx > elems.len() {
                                return Err(format!(
                                    "insert index {} out of bounds (length {})",
                                    idx,
                                    elems.len()
                                ));
                            }
                            elems.insert(idx, args[2].clone());
                            Ok(Value::Null)
                        } else {
                            Err("insert index must be a number".to_string())
                        }
                    } else {
                        Err("insert called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "contains",
            Value::NativeFun {
                name: "contains",
                arity: 1,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        let found = arr.borrow().iter().any(|v| *v == args[1]);
                        Ok(Value::Bool(found))
                    } else {
                        Err("contains called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "index_of",
            Value::NativeFun {
                name: "index_of",
                arity: 1,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        let pos = arr.borrow().iter().position(|v| *v == args[1]);
                        match pos {
                            Some(i) => Ok(Value::Num(i as f64)),
                            None => Ok(Value::Num(-1.0)),
                        }
                    } else {
                        Err("index_of called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "join",
            Value::NativeFun {
                name: "join",
                arity: 1,
                func: |args| {
                    if let (Value::Array(arr), Value::Str(sep)) = (&args[0], &args[1]) {
                        let parts: Vec<String> =
                            arr.borrow().iter().map(|v| v.to_display_string()).collect();
                        Ok(Value::Str(Rc::new(parts.join(sep.as_str()))))
                    } else {
                        Err("join expects a string separator".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "reverse",
            Value::NativeFun {
                name: "reverse",
                arity: 0,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        arr.borrow_mut().reverse();
                        Ok(Value::Null)
                    } else {
                        Err("reverse called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "slice",
            Value::NativeFun {
                name: "slice",
                arity: 2,
                func: |args| {
                    if let (Value::Array(arr), Value::Num(start), Value::Num(end)) =
                        (&args[0], &args[1], &args[2])
                    {
                        let elems = arr.borrow();
                        let len = elems.len() as i64;
                        let a = (*start as i64).max(0).min(len) as usize;
                        let b = (*end as i64).max(0).min(len) as usize;
                        let sub = if a <= b {
                            elems[a..b].to_vec()
                        } else {
                            Vec::new()
                        };
                        Ok(Value::Array(Rc::new(RefCell::new(sub))))
                    } else {
                        Err("slice expects two number arguments".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "clear",
            Value::NativeFun {
                name: "clear",
                arity: 0,
                func: |args| {
                    if let Value::Array(arr) = &args[0] {
                        arr.borrow_mut().clear();
                        Ok(Value::Null)
                    } else {
                        Err("clear called on non-array".to_string())
                    }
                },
            },
        );

        self.register_type_method(
            "array",
            "to_str",
            Value::NativeFun {
                name: "to_str",
                arity: 0,
                func: |args| Ok(Value::Str(Rc::new(args[0].to_display_string()))),
            },
        );
    }

    pub fn register_type_method(
        &mut self,
        type_name: &'static str,
        method_name: &'static str,
        method_value: Value,
    ) {
        self.type_methods
            .entry(type_name)
            .or_default()
            .insert(method_name, method_value);
    }

    pub fn register_module(&mut self, name: &str, module: Module) {
        self.globals
            .insert(name.to_string(), Value::Module(Rc::new(module)));
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.globals.get(name).cloned()
    }

    pub fn execute(&mut self, main_fn: CompiledFunction) -> CirqResult<Value> {
        let main = Rc::new(main_fn);

        let required = main.local_count as usize + 16;
        self.registers.resize(required.max(256), Value::Null);

        self.frames.push(CallFrame {
            function: main,
            ip: 0,
            base: 0,
        });

        self.run()
    }
    fn run(&mut self) -> CirqResult<Value> {
        loop {
            let frame = self.frames.last_mut().unwrap();
            let instr = frame.function.instructions[frame.ip];
            frame.ip += 1;

            match instr {
                Instruction::LoadConst { dst, idx } => {
                    let base = self.frames.last().unwrap().base;
                    let val = self.frames.last().unwrap().function.constants[idx as usize].clone();
                    self.registers[base + dst as usize] = val;
                }
                Instruction::LoadNull { dst } => {
                    let base = self.frames.last().unwrap().base;
                    self.registers[base + dst as usize] = Value::Null;
                }
                Instruction::LoadTrue { dst } => {
                    let base = self.frames.last().unwrap().base;
                    self.registers[base + dst as usize] = Value::Bool(true);
                }
                Instruction::LoadFalse { dst } => {
                    let base = self.frames.last().unwrap().base;
                    self.registers[base + dst as usize] = Value::Bool(false);
                }
                Instruction::Move { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    let val = self.registers[base + src as usize].clone();
                    self.registers[base + dst as usize] = val;
                }

                Instruction::Add { dst, a, b } => {
                    let base = self.frames.last().unwrap().base;
                    let va = &self.registers[base + a as usize];
                    let vb = &self.registers[base + b as usize];
                    let result = match (va, vb) {
                        (Value::Num(x), Value::Num(y)) => Value::Num(x + y),
                        (Value::Str(x), Value::Str(y)) => {
                            Value::Str(Rc::new(format!("{}{}", x, y)))
                        }
                        _ => {
                            return Err(self.runtime_error(format!(
                                "cannot add {} and {}",
                                va.type_name(),
                                vb.type_name()
                            )));
                        }
                    };
                    self.registers[base + dst as usize] = result;
                }
                Instruction::Sub { dst, a, b } => {
                    self.num_binary_op(dst, a, b, |x, y| x - y, "subtract")?;
                }
                Instruction::Mul { dst, a, b } => {
                    self.num_binary_op(dst, a, b, |x, y| x * y, "multiply")?;
                }
                Instruction::Div { dst, a, b } => {
                    self.num_binary_op(dst, a, b, |x, y| x / y, "divide")?;
                }
                Instruction::Mod { dst, a, b } => {
                    self.num_binary_op(dst, a, b, |x, y| x % y, "modulo")?;
                }
                Instruction::Pow { dst, a, b } => {
                    self.num_binary_op(dst, a, b, |x, y| x.powf(y), "exponentiate")?;
                }
                Instruction::Neg { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    match &self.registers[base + src as usize] {
                        Value::Num(n) => {
                            self.registers[base + dst as usize] = Value::Num(-n);
                        }
                        v => {
                            return Err(
                                self.runtime_error(format!("cannot negate {}", v.type_name()))
                            );
                        }
                    }
                }

                Instruction::BitAnd { dst, a, b } => {
                    self.int_binary_op(dst, a, b, |x, y| x & y, "bitwise AND")?;
                }
                Instruction::BitOr { dst, a, b } => {
                    self.int_binary_op(dst, a, b, |x, y| x | y, "bitwise OR")?;
                }
                Instruction::BitXor { dst, a, b } => {
                    self.int_binary_op(dst, a, b, |x, y| x ^ y, "bitwise XOR")?;
                }
                Instruction::Shl { dst, a, b } => {
                    self.int_binary_op(dst, a, b, |x, y| x << y, "left shift")?;
                }
                Instruction::Shr { dst, a, b } => {
                    self.int_binary_op(dst, a, b, |x, y| x >> y, "right shift")?;
                }
                Instruction::BitNot { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    match &self.registers[base + src as usize] {
                        Value::Num(n) => {
                            let i = *n as i64;
                            self.registers[base + dst as usize] = Value::Num(!i as f64);
                        }
                        v => {
                            return Err(
                                self.runtime_error(format!("cannot bitwise NOT {}", v.type_name()))
                            );
                        }
                    }
                }

                Instruction::Eq { dst, a, b } => {
                    let base = self.frames.last().unwrap().base;
                    let result =
                        self.registers[base + a as usize] == self.registers[base + b as usize];
                    self.registers[base + dst as usize] = Value::Bool(result);
                }
                Instruction::Ne { dst, a, b } => {
                    let base = self.frames.last().unwrap().base;
                    let result =
                        self.registers[base + a as usize] != self.registers[base + b as usize];
                    self.registers[base + dst as usize] = Value::Bool(result);
                }
                Instruction::Lt { dst, a, b } => {
                    self.num_cmp_op(dst, a, b, |x, y| x < y, "<")?;
                }
                Instruction::Gt { dst, a, b } => {
                    self.num_cmp_op(dst, a, b, |x, y| x > y, ">")?;
                }
                Instruction::Le { dst, a, b } => {
                    self.num_cmp_op(dst, a, b, |x, y| x <= y, "<=")?;
                }
                Instruction::Ge { dst, a, b } => {
                    self.num_cmp_op(dst, a, b, |x, y| x >= y, ">=")?;
                }

                Instruction::Not { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    let truthy = self.registers[base + src as usize].is_truthy();
                    self.registers[base + dst as usize] = Value::Bool(!truthy);
                }

                Instruction::Inc { dst } => {
                    let base = self.frames.last().unwrap().base;
                    match &self.registers[base + dst as usize] {
                        Value::Num(n) => {
                            self.registers[base + dst as usize] = Value::Num(n + 1.0);
                        }
                        v => {
                            return Err(
                                self.runtime_error(format!("cannot increment {}", v.type_name()))
                            );
                        }
                    }
                }
                Instruction::Dec { dst } => {
                    let base = self.frames.last().unwrap().base;
                    match &self.registers[base + dst as usize] {
                        Value::Num(n) => {
                            self.registers[base + dst as usize] = Value::Num(n - 1.0);
                        }
                        v => {
                            return Err(
                                self.runtime_error(format!("cannot decrement {}", v.type_name()))
                            );
                        }
                    }
                }

                Instruction::Jump { offset } => {
                    let frame = self.frames.last_mut().unwrap();
                    frame.ip = (frame.ip as i32 + offset) as usize;
                }
                Instruction::JumpIfFalse { src, offset } => {
                    let base = self.frames.last().unwrap().base;
                    if !self.registers[base + src as usize].is_truthy() {
                        let frame = self.frames.last_mut().unwrap();
                        frame.ip = (frame.ip as i32 + offset) as usize;
                    }
                }
                Instruction::JumpIfTrue { src, offset } => {
                    let base = self.frames.last().unwrap().base;
                    if self.registers[base + src as usize].is_truthy() {
                        let frame = self.frames.last_mut().unwrap();
                        frame.ip = (frame.ip as i32 + offset) as usize;
                    }
                }

                Instruction::GetGlobal { dst, name_idx } => {
                    let base = self.frames.last().unwrap().base;
                    let name = &self.frames.last().unwrap().function.names[name_idx as usize];
                    match self.globals.get(name) {
                        Some(val) => {
                            self.registers[base + dst as usize] = val.clone();
                        }
                        None => {
                            return Err(
                                self.runtime_error(format!("undefined variable '{}'", name))
                            );
                        }
                    }
                }
                Instruction::SetGlobal { name_idx, src } => {
                    let base = self.frames.last().unwrap().base;
                    let name =
                        self.frames.last().unwrap().function.names[name_idx as usize].clone();
                    let val = self.registers[base + src as usize].clone();
                    self.globals.insert(name, val);
                }
                Instruction::GetLocal { dst, slot } => {
                    let base = self.frames.last().unwrap().base;
                    let val = self.registers[base + slot as usize].clone();
                    self.registers[base + dst as usize] = val;
                }
                Instruction::SetLocal { slot, src } => {
                    let base = self.frames.last().unwrap().base;
                    let val = self.registers[base + src as usize].clone();
                    self.registers[base + slot as usize] = val;
                }

                Instruction::Call {
                    dst,
                    func_reg,
                    arg_start,
                    arg_count,
                } => {
                    let base = self.frames.last().unwrap().base;
                    let callee = self.registers[base + func_reg as usize].clone();

                    match callee {
                        Value::Fun(func) => {
                            if arg_count != func.param_count {
                                return Err(self.runtime_error(format!(
                                    "function '{}' expects {} arguments, got {}",
                                    func.name, func.param_count, arg_count
                                )));
                            }

                            let new_base = self.registers.len();
                            let frame_size = func.local_count as usize + 16;
                            self.registers.resize(new_base + frame_size, Value::Null);

                            for i in 0..arg_count {
                                self.registers[new_base + i as usize] =
                                    self.registers[base + arg_start as usize + i as usize].clone();
                            }

                            self.frames.push(CallFrame {
                                function: func,
                                ip: 0,
                                base: new_base,
                            });
                        }
                        Value::NativeFun {
                            name, arity, func, ..
                        } => {
                            if arg_count != arity {
                                return Err(self.runtime_error(format!(
                                    "builtin '{}' expects {} arguments, got {}",
                                    name, arity, arg_count
                                )));
                            }

                            let mut args = Vec::with_capacity(arg_count as usize);
                            for i in 0..arg_count {
                                args.push(
                                    self.registers[base + arg_start as usize + i as usize].clone(),
                                );
                            }

                            let result = func(&args).map_err(|e| {
                                CirqError::runtime_no_span(format!("{}: {}", name, e))
                            })?;
                            self.registers[base + dst as usize] = result;
                        }
                        Value::Class(class) => {
                            let instance = Rc::new(RefCell::new(Instance {
                                class: class.clone(),
                                fields: FxHashMap::default(),
                            }));
                            let instance_val = Value::Instance(instance.clone());

                            if let Some(init_method) = class.methods.get("init") {
                                if arg_count != init_method.param_count {
                                    return Err(self.runtime_error(format!(
                                        "'{}' init expects {} arguments, got {}",
                                        class.name, init_method.param_count, arg_count
                                    )));
                                }

                                let new_base = self.registers.len();
                                let frame_size = init_method.local_count as usize + 16;
                                self.registers.resize(new_base + frame_size, Value::Null);

                                self.registers[new_base] = instance_val.clone();
                                for i in 0..arg_count {
                                    self.registers[new_base + 1 + i as usize] = self.registers
                                        [base + arg_start as usize + i as usize]
                                        .clone();
                                }

                                self.frames.push(CallFrame {
                                    function: init_method.clone(),
                                    ip: 0,
                                    base: new_base,
                                });
                            }

                            self.registers[base + dst as usize] = instance_val;
                        }
                        Value::BoundMethod { receiver, method } => match *method {
                            Value::Fun(ref func) => {
                                if arg_count != func.param_count {
                                    return Err(self.runtime_error(format!(
                                        "method '{}' expects {} arguments, got {}",
                                        func.name, func.param_count, arg_count
                                    )));
                                }

                                let new_base = self.registers.len();
                                let frame_size = func.local_count as usize + 16;
                                self.registers.resize(new_base + frame_size, Value::Null);

                                self.registers[new_base] = *receiver;
                                for i in 0..arg_count {
                                    self.registers[new_base + 1 + i as usize] = self.registers
                                        [base + arg_start as usize + i as usize]
                                        .clone();
                                }

                                self.frames.push(CallFrame {
                                    function: func.clone(),
                                    ip: 0,
                                    base: new_base,
                                });
                            }
                            Value::NativeFun {
                                name, arity, func, ..
                            } => {
                                if arg_count != arity {
                                    return Err(self.runtime_error(format!(
                                        "builtin '{}' expects {} arguments, got {}",
                                        name, arity, arg_count
                                    )));
                                }

                                let mut args = Vec::with_capacity(1 + arg_count as usize);
                                args.push(*receiver);
                                for i in 0..arg_count {
                                    args.push(
                                        self.registers[base + arg_start as usize + i as usize]
                                            .clone(),
                                    );
                                }

                                let result = func(&args).map_err(|e| {
                                    CirqError::runtime_no_span(format!("{}: {}", name, e))
                                })?;
                                self.registers[base + dst as usize] = result;
                            }
                            _ => {
                                return Err(self.runtime_error(
                                    "bound method contains non-callable value".to_string(),
                                ));
                            }
                        },
                        other => {
                            return Err(self.runtime_error(format!(
                                "cannot call value of type '{}'",
                                other.type_name()
                            )));
                        }
                    }
                }

                Instruction::Return { src } => {
                    let base = self.frames.last().unwrap().base;
                    let return_val = self.registers[base + src as usize].clone();

                    self.frames.pop();

                    if self.frames.is_empty() {
                        return Ok(return_val);
                    }

                    self.registers.truncate(base);

                    let caller_frame = self.frames.last().unwrap();
                    let call_instr = caller_frame.function.instructions[caller_frame.ip - 1];
                    if let Instruction::Call { dst, .. } = call_instr {
                        let caller_base = caller_frame.base;
                        self.registers[caller_base + dst as usize] = return_val;
                    }
                }

                Instruction::ReturnNull => {
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(Value::Null);
                    }
                    let base = self.frames.last().unwrap().base;
                    let caller_frame = self.frames.last().unwrap();
                    let call_instr = caller_frame.function.instructions[caller_frame.ip - 1];
                    if let Instruction::Call { dst, .. } = call_instr {
                        self.registers[base + dst as usize] = Value::Null;
                    }
                }

                Instruction::NewArray { dst, start, count } => {
                    let base = self.frames.last().unwrap().base;
                    let mut elements = Vec::with_capacity(count as usize);
                    for i in 0..count {
                        elements.push(self.registers[base + start as usize + i as usize].clone());
                    }
                    self.registers[base + dst as usize] =
                        Value::Array(Rc::new(RefCell::new(elements)));
                }
                Instruction::GetIndex { dst, obj, idx } => {
                    let base = self.frames.last().unwrap().base;
                    let array = self.registers[base + obj as usize].clone();
                    let index = self.registers[base + idx as usize].clone();

                    match (&array, &index) {
                        (Value::Array(arr), Value::Num(n)) => {
                            let i = *n as usize;
                            let elems = arr.borrow();
                            if i >= elems.len() {
                                return Err(self.runtime_error(format!(
                                    "index {} out of bounds (length {})",
                                    i,
                                    elems.len()
                                )));
                            }
                            self.registers[base + dst as usize] = elems[i].clone();
                        }
                        _ => {
                            return Err(self.runtime_error(format!(
                                "cannot index {} with {}",
                                array.type_name(),
                                index.type_name()
                            )));
                        }
                    }
                }
                Instruction::SetIndex { obj, idx, val } => {
                    let base = self.frames.last().unwrap().base;
                    let array = self.registers[base + obj as usize].clone();
                    let index = self.registers[base + idx as usize].clone();
                    let value = self.registers[base + val as usize].clone();

                    match (&array, &index) {
                        (Value::Array(arr), Value::Num(n)) => {
                            let i = *n as usize;
                            let mut elems = arr.borrow_mut();
                            if i >= elems.len() {
                                return Err(self.runtime_error(format!(
                                    "index {} out of bounds (length {})",
                                    i,
                                    elems.len()
                                )));
                            }
                            elems[i] = value;
                        }
                        _ => {
                            return Err(self.runtime_error(format!(
                                "cannot index {} with {}",
                                array.type_name(),
                                index.type_name()
                            )));
                        }
                    }
                }

                Instruction::GetMember { dst, obj, name_idx } => {
                    let base = self.frames.last().unwrap().base;
                    let obj_val = self.registers[base + obj as usize].clone();
                    let name = &self.frames.last().unwrap().function.names[name_idx as usize];

                    match &obj_val {
                        Value::Instance(inst) => {
                            let inst_ref = inst.borrow();
                            if let Some(val) = inst_ref.fields.get(name) {
                                self.registers[base + dst as usize] = val.clone();
                            } else if let Some(method) = inst_ref.class.methods.get(name) {
                                self.registers[base + dst as usize] = Value::BoundMethod {
                                    receiver: Box::new(obj_val.clone()),
                                    method: Box::new(Value::Fun(method.clone())),
                                };
                            } else {
                                return Err(self.runtime_error(format!(
                                    "'{}' instance has no field or method '{}'",
                                    inst_ref.class.name, name
                                )));
                            }
                        }
                        Value::Module(m) => match m.get_member(name) {
                            Some(val) => {
                                self.registers[base + dst as usize] = val;
                            }
                            None => {
                                return Err(self.runtime_error(format!(
                                    "module '{}' has no member '{}'",
                                    m.name, name
                                )));
                            }
                        },
                        other => {
                            let type_name = other.type_name();
                            if let Some(methods) = self.type_methods.get(type_name) {
                                if let Some(method_val) = methods.get(name.as_str()) {
                                    self.registers[base + dst as usize] = Value::BoundMethod {
                                        receiver: Box::new(obj_val.clone()),
                                        method: Box::new(method_val.clone()),
                                    };
                                } else {
                                    return Err(self.runtime_error(format!(
                                        "type '{}' has no method '{}'",
                                        type_name, name
                                    )));
                                }
                            } else {
                                return Err(self.runtime_error(format!(
                                    "cannot access member '{}' on {}",
                                    name,
                                    obj_val.type_name()
                                )));
                            }
                        }
                    }
                }

                Instruction::SetMember { obj, name_idx, val } => {
                    let base = self.frames.last().unwrap().base;
                    let obj_val = self.registers[base + obj as usize].clone();
                    let name =
                        self.frames.last().unwrap().function.names[name_idx as usize].clone();
                    let value = self.registers[base + val as usize].clone();

                    match &obj_val {
                        Value::Instance(inst) => {
                            inst.borrow_mut().fields.insert(name, value);
                        }
                        _ => {
                            return Err(self.runtime_error(format!(
                                "cannot set field '{}' on {}",
                                name,
                                obj_val.type_name()
                            )));
                        }
                    }
                }

                Instruction::Concat { dst, start, count } => {
                    let base = self.frames.last().unwrap().base;
                    let mut result = String::new();
                    for i in 0..count {
                        let val = &self.registers[base + start as usize + i as usize];
                        result.push_str(&val.to_display_string());
                    }
                    self.registers[base + dst as usize] = Value::Str(Rc::new(result));
                }
                Instruction::ToString { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    let val = &self.registers[base + src as usize];
                    let s = val.to_display_string();
                    self.registers[base + dst as usize] = Value::Str(Rc::new(s));
                }
            }
        }
    }
    #[inline]
    fn num_binary_op(
        &mut self,
        dst: u8,
        a: u8,
        b: u8,
        op: fn(f64, f64) -> f64,
        name: &str,
    ) -> CirqResult<()> {
        let base = self.frames.last().unwrap().base;
        let va = &self.registers[base + a as usize];
        let vb = &self.registers[base + b as usize];
        match (va, vb) {
            (Value::Num(x), Value::Num(y)) => {
                self.registers[base + dst as usize] = Value::Num(op(*x, *y));
                Ok(())
            }
            _ => Err(self.runtime_error(format!(
                "cannot {} {} and {}",
                name,
                va.type_name(),
                vb.type_name()
            ))),
        }
    }

    #[inline]
    fn int_binary_op(
        &mut self,
        dst: u8,
        a: u8,
        b: u8,
        op: fn(i64, i64) -> i64,
        name: &str,
    ) -> CirqResult<()> {
        let base = self.frames.last().unwrap().base;
        let va = &self.registers[base + a as usize];
        let vb = &self.registers[base + b as usize];
        match (va, vb) {
            (Value::Num(x), Value::Num(y)) => {
                let result = op(*x as i64, *y as i64);
                self.registers[base + dst as usize] = Value::Num(result as f64);
                Ok(())
            }
            _ => Err(self.runtime_error(format!(
                "cannot {} {} and {}",
                name,
                va.type_name(),
                vb.type_name()
            ))),
        }
    }

    #[inline]
    fn num_cmp_op(
        &mut self,
        dst: u8,
        a: u8,
        b: u8,
        op: fn(f64, f64) -> bool,
        sym: &str,
    ) -> CirqResult<()> {
        let base = self.frames.last().unwrap().base;
        let va = &self.registers[base + a as usize];
        let vb = &self.registers[base + b as usize];
        match (va, vb) {
            (Value::Num(x), Value::Num(y)) => {
                self.registers[base + dst as usize] = Value::Bool(op(*x, *y));
                Ok(())
            }
            _ => Err(self.runtime_error(format!(
                "cannot compare ({}) {} and {}",
                sym,
                va.type_name(),
                vb.type_name()
            ))),
        }
    }

    fn runtime_error(&self, message: String) -> CirqError {
        CirqError::runtime_no_span(message)
    }
}
