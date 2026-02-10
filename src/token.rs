use crate::error::Span;
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(f64),
    Str(String),
    True,
    False,
    Null,

    StringStart(String),
    StringPart(String),
    StringEnd(String),

    Ident(String),
    Var,
    Const,
    Fun,
    Mod,
    If,
    Else,
    While,
    For,
    Return,
    Break,
    Continue,
    Class,
    SelfKw,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Power,

    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    PowerEq,

    EqEq,
    BangEq,
    Lt,
    Gt,
    LtEq,
    GtEq,

    AmpAmp,
    PipePipe,
    Bang,

    Amp,
    Pipe,
    Caret,
    Tilde,
    Shl,
    Shr,

    AmpEq,
    PipeEq,
    CaretEq,
    ShlEq,
    ShrEq,

    PlusPlus,
    MinusMinus,

    Eq,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Comma,
    Semicolon,
    Dot,

    Eof,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline]
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
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
        "class" => Some(TokenKind::Class),
        "self" => Some(TokenKind::SelfKw),
        _ => None,
    }
}
