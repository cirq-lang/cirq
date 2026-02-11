use std::fmt;

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
    Super,
    Throw,
    Try,
    Catch,

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
    Colon,
    QuestionQuestion,
    Dot,

    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Number(n) => write!(f, "number '{}'", n),
            TokenKind::Str(s) => write!(f, "string \"{}\"", s),
            TokenKind::True => write!(f, "'true'"),
            TokenKind::False => write!(f, "'false'"),
            TokenKind::Null => write!(f, "'null'"),

            TokenKind::StringStart(s) => write!(f, "string start \"{}\"", s),
            TokenKind::StringPart(s) => write!(f, "string part \"{}\"", s),
            TokenKind::StringEnd(s) => write!(f, "string end \"{}\"", s),

            TokenKind::Ident(name) => write!(f, "identifier '{}'", name),

            TokenKind::Var => write!(f, "'var'"),
            TokenKind::Const => write!(f, "'const'"),
            TokenKind::Fun => write!(f, "'fun'"),
            TokenKind::Mod => write!(f, "'mod'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Else => write!(f, "'else'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::For => write!(f, "'for'"),
            TokenKind::Return => write!(f, "'return'"),
            TokenKind::Break => write!(f, "'break'"),
            TokenKind::Continue => write!(f, "'continue'"),
            TokenKind::Class => write!(f, "'class'"),
            TokenKind::SelfKw => write!(f, "'self'"),
            TokenKind::Super => write!(f, "'super'"),
            TokenKind::Throw => write!(f, "'throw'"),
            TokenKind::Try => write!(f, "'try'"),
            TokenKind::Catch => write!(f, "'catch'"),

            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Percent => write!(f, "'%'"),
            TokenKind::Power => write!(f, "'**'"),

            TokenKind::PlusEq => write!(f, "'+='"),
            TokenKind::MinusEq => write!(f, "'-='"),
            TokenKind::StarEq => write!(f, "'*='"),
            TokenKind::SlashEq => write!(f, "'/='"),
            TokenKind::PercentEq => write!(f, "'%='"),
            TokenKind::PowerEq => write!(f, "'**='"),

            TokenKind::EqEq => write!(f, "'=='"),
            TokenKind::BangEq => write!(f, "'!='"),
            TokenKind::Lt => write!(f, "'<'"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::LtEq => write!(f, "'<='"),
            TokenKind::GtEq => write!(f, "'>='"),

            TokenKind::AmpAmp => write!(f, "'&&'"),
            TokenKind::PipePipe => write!(f, "'||'"),
            TokenKind::Bang => write!(f, "'!'"),

            TokenKind::Amp => write!(f, "'&'"),
            TokenKind::Pipe => write!(f, "'|'"),
            TokenKind::Caret => write!(f, "'^'"),
            TokenKind::Tilde => write!(f, "'~'"),
            TokenKind::Shl => write!(f, "'<<'"),
            TokenKind::Shr => write!(f, "'>>'"),

            TokenKind::AmpEq => write!(f, "'&='"),
            TokenKind::PipeEq => write!(f, "'|='"),
            TokenKind::CaretEq => write!(f, "'^='"),
            TokenKind::ShlEq => write!(f, "'<<='"),
            TokenKind::ShrEq => write!(f, "'>>='"),

            TokenKind::PlusPlus => write!(f, "'++'"),
            TokenKind::MinusMinus => write!(f, "'--'"),

            TokenKind::Eq => write!(f, "'='"),

            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::LBracket => write!(f, "'['"),
            TokenKind::RBracket => write!(f, "']'"),

            TokenKind::Comma => write!(f, "','"),
            TokenKind::Semicolon => write!(f, "';'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::QuestionQuestion => write!(f, "'??'"),
            TokenKind::Dot => write!(f, "'.'"),

            TokenKind::Eof => write!(f, "end of file"),
        }
    }
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
        "super" => Some(TokenKind::Super),
        "throw" => Some(TokenKind::Throw),
        "try" => Some(TokenKind::Try),
        "catch" => Some(TokenKind::Catch),
        _ => None,
    }
}
