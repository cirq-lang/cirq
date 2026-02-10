//! # Error Module
//!
//! Unified error types for all stages of the Cirq interpreter pipeline.
//! Every error carries source location information (line, column) for
//! precise diagnostics.

use std::fmt;

// -----------------------------------------------------------------------------
// SPAN — Source Location
// -----------------------------------------------------------------------------

/// Represents a position in source code.
///
/// Compact 12-byte representation tracking line, column, and length
/// for precise error reporting and diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// 1-based line number in the source file.
    pub line: u32,
    /// 1-based column number in the source file.
    pub col: u32,
    /// Length of the spanned region in bytes.
    pub len: u32,
}

impl Span {
    /// Creates a new span at the given location.
    #[inline]
    pub fn new(line: u32, col: u32, len: u32) -> Self {
        Self { line, col, len }
    }
}

// -----------------------------------------------------------------------------
// ERROR KIND — Pipeline Stage Classification
// -----------------------------------------------------------------------------

/// Classifies which stage of the pipeline produced the error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    /// Error during lexical analysis (tokenization).
    Lexer,
    /// Error during parsing (syntax analysis).
    Parser,
    /// Error during compilation (AST to bytecode).
    Compiler,
    /// Error during VM execution.
    Runtime,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Lexer => write!(f, "LexerError"),
            ErrorKind::Parser => write!(f, "ParseError"),
            ErrorKind::Compiler => write!(f, "CompileError"),
            ErrorKind::Runtime => write!(f, "RuntimeError"),
        }
    }
}

// -----------------------------------------------------------------------------
// CIRQ ERROR — Unified Error Type
// -----------------------------------------------------------------------------

/// The unified error type for the entire Cirq interpreter.
///
/// Every error carries a classification (`kind`), a human-readable
/// `message`, and an optional `span` pointing to the source location.
#[derive(Debug, Clone)]
pub struct CirqError {
    /// Which pipeline stage produced this error.
    pub kind: ErrorKind,
    /// Human-readable description of what went wrong.
    pub message: String,
    /// Source location where the error occurred, if available.
    pub span: Option<Span>,
}

impl CirqError {
    /// Creates a new error with a source location.
    pub fn new(kind: ErrorKind, message: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            message: message.into(),
            span: Some(span),
        }
    }

    /// Creates a new error without source location information.
    pub fn no_span(kind: ErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            span: None,
        }
    }

    /// Creates a lexer error at the given span.
    #[inline]
    pub fn lexer(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Lexer, message, span)
    }

    /// Creates a parser error at the given span.
    #[inline]
    pub fn parser(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Parser, message, span)
    }

    /// Creates a compiler error at the given span.
    #[inline]
    pub fn compiler(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Compiler, message, span)
    }

    /// Creates a runtime error at the given span.
    #[inline]
    pub fn runtime(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Runtime, message, span)
    }

    /// Creates a runtime error without source location.
    #[inline]
    pub fn runtime_no_span(message: impl Into<String>) -> Self {
        Self::no_span(ErrorKind::Runtime, message)
    }
}

impl fmt::Display for CirqError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.span {
            Some(span) => write!(
                f,
                "{} [line {}:{}]: {}",
                self.kind, span.line, span.col, self.message
            ),
            None => write!(f, "{}: {}", self.kind, self.message),
        }
    }
}

impl std::error::Error for CirqError {}

/// Convenience type alias for Results throughout the Cirq interpreter.
pub type CirqResult<T> = std::result::Result<T, CirqError>;
