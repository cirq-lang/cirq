//! # VM Module
//!
//! Register-based virtual machine for executing Cirq bytecode.
//! Uses a tight `match`-based dispatch loop for instruction execution.
//! Globals are stored in an `FxHashMap` for fastest-possible string
//! key lookups.
//!
//! ## Key Design
//! - Pre-allocated register file per call frame.
//! - Call stack via `Vec<CallFrame>` with base pointer tracking.
//! - ARC-based value cloning — cheap refcount bumps, no GC pauses.

use crate::error::{CirqError, CirqResult};
use crate::opcode::{CompiledFunction, Instruction};
use crate::value::{Module, Value};

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

// -----------------------------------------------------------------------------
// CALL FRAME
// -----------------------------------------------------------------------------

/// A single call frame on the VM's call stack.
#[derive(Debug)]
struct CallFrame {
    /// The function being executed.
    function: Rc<CompiledFunction>,
    /// Instruction pointer (index into `function.instructions`).
    ip: usize,
    /// Base register offset in the VM's register file.
    base: usize,
}

// -----------------------------------------------------------------------------
// VM STATE
// -----------------------------------------------------------------------------

/// The Cirq virtual machine. Executes compiled bytecode using a
/// register-based architecture with a call stack.
pub struct Vm {
    /// Global variable storage (name → value).
    globals: FxHashMap<String, Value>,
    /// Flat register file shared across all frames.
    registers: Vec<Value>,
    /// Call stack.
    frames: Vec<CallFrame>,
}

impl Vm {
    /// Creates a new VM instance with the given builtin modules registered.
    pub fn new() -> Self {
        Self {
            globals: FxHashMap::default(),
            registers: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
        }
    }

    /// Registers a module as a global variable.
    pub fn register_module(&mut self, name: &str, module: Module) {
        self.globals
            .insert(name.to_string(), Value::Module(Rc::new(module)));
    }

    /// Executes a compiled top-level function (the main script).
    ///
    /// # Errors
    /// Returns a `CirqError` on runtime errors (type mismatches,
    /// undefined variables, index out of bounds, etc.).
    pub fn execute(&mut self, main_fn: CompiledFunction) -> CirqResult<Value> {
        let main = Rc::new(main_fn);

        // Ensure register file is large enough
        let required = main.local_count as usize + 16;
        self.registers.resize(required.max(256), Value::Null);

        self.frames.push(CallFrame {
            function: main,
            ip: 0,
            base: 0,
        });

        self.run()
    }

    // -------------------------------------------------------------------------
    // MAIN DISPATCH LOOP
    // -------------------------------------------------------------------------

    /// The core execution loop. Fetches and dispatches instructions
    /// until a Return is encountered in the top-level frame.
    fn run(&mut self) -> CirqResult<Value> {
        loop {
            let frame = self.frames.last_mut().unwrap();
            let instr = frame.function.instructions[frame.ip];
            frame.ip += 1;

            match instr {
                // -- Load & Move --
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

                // -- Arithmetic --
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

                // -- Bitwise --
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

                // -- Comparison --
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

                // -- Logical --
                Instruction::Not { dst, src } => {
                    let base = self.frames.last().unwrap().base;
                    let truthy = self.registers[base + src as usize].is_truthy();
                    self.registers[base + dst as usize] = Value::Bool(!truthy);
                }

                // -- Increment / Decrement --
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

                // -- Control Flow --
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

                // -- Variables --
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

                // -- Functions --
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

                            // Set up new frame
                            let new_base = self.registers.len();
                            let frame_size = func.local_count as usize + 16;
                            self.registers.resize(new_base + frame_size, Value::Null);

                            // Copy arguments into new frame's parameter registers
                            for i in 0..arg_count {
                                self.registers[new_base + i as usize] =
                                    self.registers[base + arg_start as usize + i as usize].clone();
                            }

                            self.frames.push(CallFrame {
                                function: func,
                                ip: 0,
                                base: new_base,
                            });

                            // Continue execution in new frame — the return
                            // instruction will pop back and store the result
                            // by inspecting the caller's Call instruction.
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

                    // Pop frame
                    self.frames.pop();

                    if self.frames.is_empty() {
                        // Returning from top-level script
                        return Ok(return_val);
                    }

                    // Truncate register file back to caller's scope
                    self.registers.truncate(base);

                    // Find the Call instruction that invoked us to get dst
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

                // -- Arrays --
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

                // -- Modules --
                Instruction::GetMember { dst, obj, name_idx } => {
                    let base = self.frames.last().unwrap().base;
                    let module_val = self.registers[base + obj as usize].clone();
                    let name = &self.frames.last().unwrap().function.names[name_idx as usize];

                    match &module_val {
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
                        _ => {
                            return Err(self.runtime_error(format!(
                                "cannot access member '{}' on {}",
                                name,
                                module_val.type_name()
                            )));
                        }
                    }
                }

                // -- String Operations --
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

    // -------------------------------------------------------------------------
    // ARITHMETIC HELPERS
    // -------------------------------------------------------------------------

    /// Performs a binary numeric operation (f64 × f64 → f64).
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

    /// Performs a binary integer operation (i64 × i64 → i64, stored as f64).
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

    /// Performs a numeric comparison operation.
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

    /// Creates a runtime error with no span information.
    fn runtime_error(&self, message: String) -> CirqError {
        CirqError::runtime_no_span(message)
    }
}
