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
    SyntaxError,

    TypeError,

    ReferenceError,

    RangeError,

    Error,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::SyntaxError => write!(f, "SyntaxError"),
            ErrorKind::TypeError => write!(f, "TypeError"),
            ErrorKind::ReferenceError => write!(f, "ReferenceError"),
            ErrorKind::RangeError => write!(f, "RangeError"),
            ErrorKind::Error => write!(f, "Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct CirqError {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Option<Span>,
    pub frames: Vec<StackFrame>,
    pub thrown_value: Option<crate::value::Value>,
}

impl CirqError {
    #[inline]
    pub fn syntax(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: ErrorKind::SyntaxError,
            message: message.into(),
            span: Some(span),
            frames: Vec::new(),
            thrown_value: None,
        }
    }

    #[inline]
    pub fn type_error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            kind: ErrorKind::TypeError,
            message: message.into(),
            span,
            frames: Vec::new(),
            thrown_value: None,
        }
    }

    #[inline]
    pub fn reference_error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            kind: ErrorKind::ReferenceError,
            message: message.into(),
            span,
            frames: Vec::new(),
            thrown_value: None,
        }
    }

    #[inline]
    pub fn range_error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            kind: ErrorKind::RangeError,
            message: message.into(),
            span,
            frames: Vec::new(),
            thrown_value: None,
        }
    }

    #[inline]
    pub fn error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            kind: ErrorKind::Error,
            message: message.into(),
            span,
            frames: Vec::new(),
            thrown_value: None,
        }
    }

    pub fn thrown(value: crate::value::Value, span: Option<Span>) -> Self {
        let message = match &value {
            crate::value::Value::Str(s) => s.to_string(),
            crate::value::Value::Instance(inst) => {
                let inst = inst.borrow();
                match inst.fields.get("message") {
                    Some(crate::value::Value::Str(s)) => s.to_string(),
                    _ => inst.class.name.clone(),
                }
            }
            other => other.to_display_string(),
        };
        Self {
            kind: ErrorKind::Error,
            message,
            span,
            frames: Vec::new(),
            thrown_value: Some(value),
        }
    }

    pub fn with_frames(mut self, frames: Vec<StackFrame>) -> Self {
        self.frames = frames;
        self
    }
}

impl fmt::Display for CirqError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)?;

        if let Some(span) = &self.span {
            write!(f, "\n  at line {}:{}", span.line, span.col)?;
        }

        if !self.frames.is_empty() {
            write!(f, "\n")?;
            for frame in &self.frames {
                match &frame.span {
                    Some(s) => write!(
                        f,
                        "\n  at {} (line {}:{})",
                        frame.function_name, s.line, s.col
                    )?,
                    None => write!(f, "\n  at {}", frame.function_name)?,
                }
            }
        }

        Ok(())
    }
}

impl std::error::Error for CirqError {}

pub type CirqResult<T> = std::result::Result<T, CirqError>;

pub fn format_error(error: &CirqError, source: &str, filename: &str) -> String {
    let mut out = String::new();

    out.push_str(&format!("{}: {}", error.kind, error.message));

    if let Some(span) = &error.span {
        out.push_str(&format!(
            "\n\n  at {}:{}:{}\n",
            filename, span.line, span.col
        ));

        let lines: Vec<&str> = source.lines().collect();
        let line_idx = (span.line as usize).saturating_sub(1);

        if line_idx > 0 {
            let prev = line_idx;
            let content = lines.get(prev - 1).unwrap_or(&"");
            out.push_str(&format!("\n  {} | {}", prev, content));
        }

        if let Some(error_line) = lines.get(line_idx) {
            let line_num = span.line;
            out.push_str(&format!("\n> {} | {}", line_num, error_line));

            let gutter_width = format!("  {} | ", line_num).len();
            let col_offset = (span.col as usize).saturating_sub(1);
            let caret_len = (span.len as usize).max(1);
            let padding = " ".repeat(gutter_width + col_offset);
            let carets = "^".repeat(caret_len);
            out.push_str(&format!("\n{}{}", padding, carets));
        }

        if let Some(next_line) = lines.get(line_idx + 1) {
            let next_num = span.line + 1;
            out.push_str(&format!("\n  {} | {}", next_num, next_line));
        }
    }

    if !error.frames.is_empty() {
        out.push_str("\n");
        for frame in &error.frames {
            match &frame.span {
                Some(s) => out.push_str(&format!(
                    "\n  at {} ({}:{}:{})",
                    frame.function_name, filename, s.line, s.col
                )),
                None => out.push_str(&format!("\n  at {}", frame.function_name)),
            }
        }
    }

    out
}
