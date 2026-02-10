use std::fmt;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub col: u32,
    pub len: u32,
}

impl Span {
    #[inline]
    pub fn new(line: u32, col: u32, len: u32) -> Self {
        Self { line, col, len }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Lexer,
    Parser,
    Compiler,
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
#[derive(Debug, Clone)]
pub struct CirqError {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Option<Span>,
}

impl CirqError {
    pub fn new(kind: ErrorKind, message: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            message: message.into(),
            span: Some(span),
        }
    }

    pub fn no_span(kind: ErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            span: None,
        }
    }

    #[inline]
    pub fn lexer(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Lexer, message, span)
    }

    #[inline]
    pub fn parser(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Parser, message, span)
    }

    #[inline]
    pub fn compiler(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Compiler, message, span)
    }

    #[inline]
    pub fn runtime(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Runtime, message, span)
    }

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

pub type CirqResult<T> = std::result::Result<T, CirqError>;
