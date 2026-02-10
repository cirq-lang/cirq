use crate::opcode::CompiledFunction;

use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
pub type NativeFn = fn(&[Value]) -> Result<Value, String>;
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub members: Vec<(String, Value)>,
}

impl Module {
    pub fn get_member(&self, name: &str) -> Option<Value> {
        self.members
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.clone())
    }
}
#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub methods: FxHashMap<String, Value>,
}
#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: FxHashMap<String, Value>,
}
#[derive(Clone)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Null,
    Str(Rc<String>),
    Array(Rc<RefCell<Vec<Value>>>),
    Fun(Rc<CompiledFunction>),
    NativeFun {
        name: &'static str,
        arity: u8,
        func: NativeFn,
    },
    Module(Rc<Module>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    BoundMethod {
        receiver: Box<Value>,
        method: Box<Value>,
    },
}

impl Value {
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
