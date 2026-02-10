//! # Token Module
//!
//! Defines all token types, keywords, and operators for the Cirq language.
//! Tokens are produced by the lexer and consumed by the parser. Each token
//! carries a [`Span`] for source location tracking.

use crate::error::Span;

// -----------------------------------------------------------------------------
// TOKEN KIND — All Lexical Categories
// -----------------------------------------------------------------------------

/// Represents every possible token type in the Cirq language.
///
/// Organized by category: literals, keywords, operators, delimiters, and
/// special tokens for string interpolation.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // -- Literals --
    /// Numeric literal (integer or float, including hex/bin/oct).
    Number(f64),
    /// Plain string literal (no interpolation parts).
    Str(String),
    /// `true`
    True,
    /// `false`
    False,
    /// `null`
    Null,

    // -- String Interpolation --
    /// Opening portion of an interpolated string, before the first `\(`.
    StringStart(String),
    /// Middle portion between two interpolation expressions.
    StringPart(String),
    /// Closing portion after the final interpolation expression.
    StringEnd(String),

    // -- Identifiers & Keywords --
    /// User-defined identifier (variable, function, module name).
    Ident(String),
    /// `var`
    Var,
    /// `const`
    Const,
    /// `fun`
    Fun,
    /// `mod`
    Mod,
    /// `if`
    If,
    /// `else`
    Else,
    /// `while`
    While,
    /// `for`
    For,
    /// `return`
    Return,
    /// `break`
    Break,
    /// `continue`
    Continue,

    // -- Arithmetic Operators --
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `**`
    Power,

    // -- Compound Assignment --
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `%=`
    PercentEq,
    /// `**=`
    PowerEq,

    // -- Comparison Operators --
    /// `==`
    EqEq,
    /// `!=`
    BangEq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,

    // -- Logical Operators --
    /// `&&`
    AmpAmp,
    /// `||`
    PipePipe,
    /// `!`
    Bang,

    // -- Bitwise Operators --
    /// `&`
    Amp,
    /// `|`
    Pipe,
    /// `^`
    Caret,
    /// `~`
    Tilde,
    /// `<<`
    Shl,
    /// `>>`
    Shr,

    // -- Bitwise Compound Assignment --
    /// `&=`
    AmpEq,
    /// `|=`
    PipeEq,
    /// `^=`
    CaretEq,
    /// `<<=`
    ShlEq,
    /// `>>=`
    ShrEq,

    // -- Increment/Decrement --
    /// `++`
    PlusPlus,
    /// `--`
    MinusMinus,

    // -- Assignment --
    /// `=`
    Eq,

    // -- Delimiters --
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,

    // -- Punctuation --
    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `.`
    Dot,

    // -- Special --
    /// End of file marker.
    Eof,
}

// -----------------------------------------------------------------------------
// TOKEN — Token with Source Location
// -----------------------------------------------------------------------------

/// A single token produced by the lexer.
///
/// Pairs a [`TokenKind`] with its [`Span`] in the source code for
/// error reporting and diagnostics.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The classification and payload of this token.
    pub kind: TokenKind,
    /// Source location of this token.
    pub span: Span,
}

impl Token {
    /// Creates a new token with the given kind and span.
    #[inline]
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

// -----------------------------------------------------------------------------
// KEYWORD LOOKUP
// -----------------------------------------------------------------------------

/// Resolves an identifier string to its keyword token kind, if it matches
/// a reserved keyword. Returns `None` for non-keyword identifiers.
///
/// Uses a manually optimized match on string length + first byte to
/// minimize branching for the common case (identifiers, not keywords).
#[inline]
pub fn lookup_keyword(ident: &str) -> Option<TokenKind> {
    match ident {
        "var" => Some(TokenKind::Var),
        "const" => Some(TokenKind::Const),
        "fun" => Some(TokenKind::Fun),
        "mod" => Some(TokenKind::Mod),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "while" => Some(TokenKind::While),
        "for" => Some(TokenKind::For),
        "return" => Some(TokenKind::Return),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "null" => Some(TokenKind::Null),
        "break" => Some(TokenKind::Break),
        "continue" => Some(TokenKind::Continue),
        _ => None,
    }
}
