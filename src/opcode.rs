//! # Opcode Module
//!
//! Register-based instruction set for the Cirq virtual machine.
//! Instructions are encoded as Rust enums for type safety during
//! compilation, then dispatched via `match` in the VM's hot loop.
//!
//! ## Design Notes
//! - Every instruction uses register indices (`u8`) for operands.
//! - Builtin calls go through the standard `Call` instruction.
//! - No dedicated opcode for builtins — uniform dispatch model.

// -----------------------------------------------------------------------------
// INSTRUCTION SET
// -----------------------------------------------------------------------------

/// A single register-based VM instruction.
///
/// Register indices are `u8`, allowing up to 256 registers per frame.
/// Constant and name indices are `u16`, supporting 65536 entries.
#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // -- Load & Move --
    /// Loads a constant from the pool into a register.
    LoadConst { dst: u8, idx: u16 },
    /// Loads `null` into a register.
    LoadNull { dst: u8 },
    /// Loads `true` into a register.
    LoadTrue { dst: u8 },
    /// Loads `false` into a register.
    LoadFalse { dst: u8 },
    /// Copies the value from `src` register to `dst` register.
    Move { dst: u8, src: u8 },

    // -- Arithmetic (dst = a op b) --
    /// `dst = a + b`
    Add { dst: u8, a: u8, b: u8 },
    /// `dst = a - b`
    Sub { dst: u8, a: u8, b: u8 },
    /// `dst = a * b`
    Mul { dst: u8, a: u8, b: u8 },
    /// `dst = a / b`
    Div { dst: u8, a: u8, b: u8 },
    /// `dst = a % b`
    Mod { dst: u8, a: u8, b: u8 },
    /// `dst = a ** b`
    Pow { dst: u8, a: u8, b: u8 },
    /// `dst = -src`
    Neg { dst: u8, src: u8 },

    // -- Bitwise --
    /// `dst = a & b`
    BitAnd { dst: u8, a: u8, b: u8 },
    /// `dst = a | b`
    BitOr { dst: u8, a: u8, b: u8 },
    /// `dst = a ^ b`
    BitXor { dst: u8, a: u8, b: u8 },
    /// `dst = a << b`
    Shl { dst: u8, a: u8, b: u8 },
    /// `dst = a >> b`
    Shr { dst: u8, a: u8, b: u8 },
    /// `dst = ~src`
    BitNot { dst: u8, src: u8 },

    // -- Comparison (dst = bool result) --
    /// `dst = (a == b)`
    Eq { dst: u8, a: u8, b: u8 },
    /// `dst = (a != b)`
    Ne { dst: u8, a: u8, b: u8 },
    /// `dst = (a < b)`
    Lt { dst: u8, a: u8, b: u8 },
    /// `dst = (a > b)`
    Gt { dst: u8, a: u8, b: u8 },
    /// `dst = (a <= b)`
    Le { dst: u8, a: u8, b: u8 },
    /// `dst = (a >= b)`
    Ge { dst: u8, a: u8, b: u8 },

    // -- Logical --
    /// `dst = !src`
    Not { dst: u8, src: u8 },

    // -- Increment / Decrement (in-place on register) --
    /// `regs[dst] += 1`
    Inc { dst: u8 },
    /// `regs[dst] -= 1`
    Dec { dst: u8 },

    // -- Control Flow --
    /// Unconditional jump by a signed offset from current IP.
    Jump { offset: i32 },
    /// Jump if register is falsy.
    JumpIfFalse { src: u8, offset: i32 },
    /// Jump if register is truthy.
    JumpIfTrue { src: u8, offset: i32 },

    // -- Variables --
    /// Loads a global variable by name index into a register.
    GetGlobal { dst: u8, name_idx: u16 },
    /// Stores a register value into a global variable by name index.
    SetGlobal { name_idx: u16, src: u8 },
    /// Loads a local variable from a stack slot into a register.
    GetLocal { dst: u8, slot: u8 },
    /// Stores a register value into a local variable stack slot.
    SetLocal { slot: u8, src: u8 },

    // -- Functions --
    /// Calls a function. `func_reg` is the register holding the callable.
    /// `arg_start` is the first register of the arguments, `arg_count`
    /// is how many arguments. Result goes into `dst`.
    Call {
        dst: u8,
        func_reg: u8,
        arg_start: u8,
        arg_count: u8,
    },
    /// Returns a value from the current function.
    Return { src: u8 },
    /// Returns null from the current function (implicit return).
    ReturnNull,

    // -- Arrays --
    /// Creates a new array from consecutive registers.
    NewArray { dst: u8, start: u8, count: u8 },
    /// `dst = array[index]`
    GetIndex { dst: u8, obj: u8, idx: u8 },
    /// `array[index] = val`
    SetIndex { obj: u8, idx: u8, val: u8 },

    // -- Modules --
    /// Loads a member from a module. `mod_reg` is the register holding
    /// the module value, `name_idx` is the member name in the constant pool.
    GetMember { dst: u8, obj: u8, name_idx: u16 },

    // -- String Interpolation --
    /// Concatenates `count` consecutive registers (starting at `start`)
    /// into a single string, storing the result in `dst`.
    Concat { dst: u8, start: u8, count: u8 },
    /// Converts a value to its string representation.
    ToString { dst: u8, src: u8 },
}

// -----------------------------------------------------------------------------
// COMPILED FUNCTION
// -----------------------------------------------------------------------------

/// A compiled function containing its bytecode and metadata.
#[derive(Debug, Clone)]
pub struct CompiledFunction {
    /// The function's name (for debugging and stack traces).
    pub name: String,
    /// The bytecode instruction sequence.
    pub instructions: Vec<Instruction>,
    /// Constant pool for this function.
    pub constants: Vec<crate::value::Value>,
    /// Global name pool — names referenced by `GetGlobal`/`SetGlobal`.
    pub names: Vec<String>,
    /// Number of local variable slots required.
    pub local_count: u8,
    /// Number of parameters this function accepts.
    pub param_count: u8,
}
