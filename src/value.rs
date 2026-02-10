//! # Value Module
//!
//! Runtime value representation for the Cirq interpreter.
//! Uses ARC (Automated Reference Counting) via `Rc` for heap-allocated
//! types — **no garbage collector**. Memory is freed deterministically
//! when the last reference is dropped.
//!
//! ## Key Design
//! - `Num`, `Bool`, `Null` are inline (no heap allocation).
//! - `Str` uses `Rc<String>` for shared ownership with cheap cloning.
//! - `Array` uses `Rc<RefCell<Vec<Value>>>` for mutable shared arrays.
//! - `Fun` and `Module` are reference-counted pointers to compiled data.

use crate::opcode::CompiledFunction;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

// -----------------------------------------------------------------------------
// NATIVE FUNCTION TYPE
// -----------------------------------------------------------------------------

/// Signature for native (builtin) functions.
///
/// Accepts a slice of `Value` arguments and returns a `Value` result.
/// Errors are returned as `Err(String)`.
pub type NativeFn = fn(&[Value]) -> Result<Value, String>;

// -----------------------------------------------------------------------------
// MODULE
// -----------------------------------------------------------------------------

/// A module grouping named functions and native bindings.
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name for display and member access resolution.
    pub name: String,
    /// Named members (functions or values) in this module.
    pub members: Vec<(String, Value)>,
}

impl Module {
    /// Looks up a member by name, returning a clone of its value.
    pub fn get_member(&self, name: &str) -> Option<Value> {
        self.members
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.clone())
    }
}

// -----------------------------------------------------------------------------
// VALUE — Core Runtime Type
// -----------------------------------------------------------------------------

/// The core runtime value type for the Cirq interpreter.
///
/// Designed for minimal overhead:
/// - Primitives (`Num`, `Bool`, `Null`) are stored inline.
/// - Heap types use `Rc` for deterministic ARC memory management.
/// - Clone is cheap: primitives are `Copy`, heap types bump a refcount.
#[derive(Clone)]
pub enum Value {
    /// 64-bit floating-point number (covers both int and float).
    Num(f64),
    /// Boolean value.
    Bool(bool),
    /// The null value.
    Null,
    /// Reference-counted string.
    Str(Rc<String>),
    /// Reference-counted, mutable dynamic array.
    Array(Rc<RefCell<Vec<Value>>>),
    /// Reference-counted compiled function.
    Fun(Rc<CompiledFunction>),
    /// Native (builtin) function pointer with name and arity.
    NativeFun {
        /// Display name for error messages.
        name: &'static str,
        /// Expected number of arguments.
        arity: u8,
        /// The native function pointer.
        func: NativeFn,
    },
    /// Reference-counted module.
    Module(Rc<Module>),
}

impl Value {
    /// Returns `true` if this value is considered "truthy" in boolean context.
    ///
    /// Truthiness rules:
    /// - `Null` → false
    /// - `Bool(b)` → b
    /// - `Num(0.0)` → false, all others → true
    /// - `Str("")` → false, non-empty → true
    /// - Arrays, functions, modules → always true
    #[inline]
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Num(n) => *n != 0.0,
            Value::Str(s) => !s.is_empty(),
            _ => true,
        }
    }

    /// Returns the type name of this value as a static string.
    #[inline]
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Num(_) => "num",
            Value::Bool(_) => "bool",
            Value::Null => "null",
            Value::Str(_) => "str",
            Value::Array(_) => "array",
            Value::Fun(_) => "fun",
            Value::NativeFun { .. } => "builtin",
            Value::Module(_) => "module",
        }
    }

    /// Returns the string representation of this value.
    pub fn to_display_string(&self) -> String {
        match self {
            Value::Num(n) => {
                if n.fract() == 0.0 && n.is_finite() {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }
            Value::Bool(b) => format!("{}", b),
            Value::Null => "null".to_string(),
            Value::Str(s) => s.as_ref().clone(),
            Value::Array(arr) => {
                let elems = arr.borrow();
                let parts: Vec<String> = elems
                    .iter()
                    .map(|v| match v {
                        Value::Str(s) => format!("\"{}\"", s),
                        other => other.to_display_string(),
                    })
                    .collect();
                format!("[{}]", parts.join(", "))
            }
            Value::Fun(f) => format!("<fun {}>", f.name),
            Value::NativeFun { name, .. } => format!("<builtin {}>", name),
            Value::Module(m) => format!("<mod {}>", m.name),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_display_string())
    }
}

// Value equality — used by == and != operators
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Str(a), Value::Str(b)) => a == b,
            _ => false,
        }
    }
}
