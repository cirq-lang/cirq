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
//! - `Class`, `Instance`, `BoundMethod` form the unified OOP system.

use crate::opcode::CompiledFunction;

use rustc_hash::FxHashMap;
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
// CLASS
// -----------------------------------------------------------------------------

/// A class definition, holding its name and compiled methods.
///
/// Methods are stored in a `FxHashMap` for O(1) lookup by name.
/// The `init` method (if present) is called automatically on construction.
#[derive(Debug, Clone)]
pub struct Class {
    /// Class name for display and error messages.
    pub name: String,
    /// Compiled methods keyed by name.
    pub methods: FxHashMap<String, Rc<CompiledFunction>>,
}

// -----------------------------------------------------------------------------
// INSTANCE
// -----------------------------------------------------------------------------

/// A runtime instance of a class.
///
/// Holds a reference to its class (for method lookup) and a mutable
/// field map for per-instance state.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The class this instance was created from.
    pub class: Rc<Class>,
    /// Instance fields, set via `self.field = value` in methods.
    pub fields: FxHashMap<String, Value>,
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
    /// Reference-counted class definition.
    Class(Rc<Class>),
    /// Reference-counted, mutable class instance.
    Instance(Rc<RefCell<Instance>>),
    /// A method bound to a receiver, for unified dispatch.
    ///
    /// Works for both user-defined class methods and builtin type methods.
    /// When called, the receiver is passed as the first argument (slot 0
    /// for `Fun`, prepended to args for `NativeFun`).
    BoundMethod {
        /// The object the method is bound to (instance, array, string, etc.).
        receiver: Box<Value>,
        /// The method to call (`Fun` or `NativeFun`).
        method: Box<Value>,
    },
}

impl Value {
    /// Returns `true` if this value is considered "truthy" in boolean context.
    ///
    /// Truthiness rules:
    /// - `Null` → false
    /// - `Bool(b)` → b
    /// - `Num(0.0)` → false, all others → true
    /// - `Str("")` → false, non-empty → true
    /// - Arrays, functions, modules, classes, instances → always true
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
            Value::Class(_) => "class",
            Value::Instance(_) => "instance",
            Value::BoundMethod { .. } => "bound_method",
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
            Value::Class(c) => format!("<class {}>", c.name),
            Value::Instance(inst) => {
                format!("<{} instance>", inst.borrow().class.name)
            }
            Value::BoundMethod { .. } => "<bound method>".to_string(),
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
