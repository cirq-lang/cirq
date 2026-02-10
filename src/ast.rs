//! # AST Module
//!
//! Abstract Syntax Tree node definitions for the Cirq language.
//! The parser produces an AST which the compiler then walks to emit
//! register-based bytecode instructions.
//!
//! ## Design Notes
//! - Nodes use `Box` for heap-allocated children to keep enum sizes small.
//! - Every node carries a [`Span`] for error reporting back to source.

use crate::error::Span;

// -----------------------------------------------------------------------------
// STATEMENTS
// -----------------------------------------------------------------------------

/// A top-level or block-level statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// `var name = expr;` or `var name;`
    VarDecl {
        name: String,
        initializer: Option<Expr>,
        span: Span,
    },
    /// `const name = expr;`
    ConstDecl {
        name: String,
        value: Expr,
        span: Span,
    },
    /// `fun name(params...) { body }`
    FunDecl {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        span: Span,
    },
    /// `mod name { declarations... }`
    ModDecl {
        name: String,
        body: Vec<Stmt>,
        span: Span,
    },
    /// Expression used as a statement (e.g., function calls).
    ExprStmt { expr: Expr, span: Span },
    /// `{ statements... }`
    Block { stmts: Vec<Stmt>, span: Span },
    /// `return expr;` or `return;`
    Return { value: Option<Expr>, span: Span },
    /// `if (cond) { then_body } else { else_body }`
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    /// `while (cond) { body }`
    While {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    /// `for (init; cond; update) { body }`
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
        span: Span,
    },
    /// `break;`
    Break { span: Span },
    /// `continue;`
    Continue { span: Span },
}

// -----------------------------------------------------------------------------
// EXPRESSIONS
// -----------------------------------------------------------------------------

/// An expression node that produces a value.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Numeric literal (f64).
    Number { value: f64, span: Span },
    /// String literal (no interpolation).
    Str { value: String, span: Span },
    /// String interpolation: sequence of string segments and expressions.
    Interpolation {
        parts: Vec<InterpolationPart>,
        span: Span,
    },
    /// `true` or `false`.
    Bool { value: bool, span: Span },
    /// `null`.
    Null { span: Span },
    /// Variable or module name reference.
    Ident { name: String, span: Span },
    /// Binary operation: `left op right`.
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
        span: Span,
    },
    /// Unary prefix operation: `op operand`.
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },
    /// Simple assignment: `target = value`.
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    /// Compound assignment: `target op= value`.
    CompoundAssign {
        target: Box<Expr>,
        op: BinOp,
        value: Box<Expr>,
        span: Span,
    },
    /// Function call: `callee(args...)`.
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    /// Array index read: `array[index]`.
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    /// Member access: `object.member`.
    MemberAccess {
        object: Box<Expr>,
        member: String,
        span: Span,
    },
    /// Array literal: `[elem1, elem2, ...]`.
    Array { elements: Vec<Expr>, span: Span },
    /// Prefix increment/decrement: `++x` or `--x`.
    PreIncDec {
        op: IncDecOp,
        operand: Box<Expr>,
        span: Span,
    },
    /// Postfix increment/decrement: `x++` or `x--`.
    PostIncDec {
        op: IncDecOp,
        operand: Box<Expr>,
        span: Span,
    },
}

// -----------------------------------------------------------------------------
// STRING INTERPOLATION PARTS
// -----------------------------------------------------------------------------

/// A segment within a string interpolation expression.
#[derive(Debug, Clone)]
pub enum InterpolationPart {
    /// A literal string segment between interpolation holes.
    Literal(String),
    /// An expression to be evaluated and converted to string.
    Expr(Expr),
}

// -----------------------------------------------------------------------------
// OPERATORS
// -----------------------------------------------------------------------------

/// Binary operators for arithmetic, comparison, logical, and bitwise ops.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    // Comparison
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    // Logical
    And,
    Or,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Unary prefix operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Arithmetic negation (`-x`).
    Neg,
    /// Logical NOT (`!x`).
    Not,
    /// Bitwise NOT (`~x`).
    BitNot,
    /// Unary plus (`+x`), identity.
    Pos,
}

/// Increment/decrement direction for `++` and `--`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncDecOp {
    /// `++`
    Inc,
    /// `--`
    Dec,
}
