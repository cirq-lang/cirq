use crate::error::Span;
#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl {
        name: String,
        initializer: Option<Expr>,
        span: Span,
    },
    ConstDecl {
        name: String,
        value: Expr,
        span: Span,
    },
    FunDecl {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        span: Span,
    },
    ModDecl {
        name: String,
        body: Vec<Stmt>,
        span: Span,
    },
    ExprStmt {
        expr: Expr,
        span: Span,
    },
    Block {
        stmts: Vec<Stmt>,
        span: Span,
    },
    Return {
        value: Option<Expr>,
        span: Span,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
    ClassDecl {
        name: String,
        superclass: Option<String>,
        methods: Vec<MethodDecl>,
        span: Span,
    },
    Throw {
        value: Expr,
        span: Span,
    },
    TryCatch {
        body: Vec<Stmt>,
        catch_name: Option<String>,
        catch_body: Option<Vec<Stmt>>,
        span: Span,
    },
}
#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub span: Span,
}
#[derive(Debug, Clone)]
pub enum Expr {
    Number {
        value: f64,
        span: Span,
    },
    Str {
        value: String,
        span: Span,
    },
    Interpolation {
        parts: Vec<InterpolationPart>,
        span: Span,
    },
    Bool {
        value: bool,
        span: Span,
    },
    Null {
        span: Span,
    },
    Ident {
        name: String,
        span: Span,
    },
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    CompoundAssign {
        target: Box<Expr>,
        op: BinOp,
        value: Box<Expr>,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    MemberAccess {
        object: Box<Expr>,
        member: String,
        span: Span,
    },
    Array {
        elements: Vec<Expr>,
        span: Span,
    },
    PreIncDec {
        op: IncDecOp,
        operand: Box<Expr>,
        span: Span,
    },
    PostIncDec {
        op: IncDecOp,
        operand: Box<Expr>,
        span: Span,
    },
    SelfRef {
        span: Span,
    },
    SuperAccess {
        member: String,
        span: Span,
    },
    TryExpr {
        body: Box<Expr>,
        catch_name: Option<String>,
        catch_body: Option<Vec<Stmt>>,
        span: Span,
    },
    NullCoalesce {
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn get_span(&self) -> Span {
        match self {
            Expr::Number { span, .. }
            | Expr::Str { span, .. }
            | Expr::Interpolation { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Null { span }
            | Expr::Ident { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Assign { span, .. }
            | Expr::CompoundAssign { span, .. }
            | Expr::Call { span, .. }
            | Expr::Index { span, .. }
            | Expr::MemberAccess { span, .. }
            | Expr::Array { span, .. }
            | Expr::PreIncDec { span, .. }
            | Expr::PostIncDec { span, .. }
            | Expr::SelfRef { span }
            | Expr::SuperAccess { span, .. }
            | Expr::TryExpr { span, .. }
            | Expr::NullCoalesce { span, .. } => *span,
        }
    }
}

impl Stmt {
    pub fn get_span(&self) -> Span {
        match self {
            Stmt::VarDecl { span, .. }
            | Stmt::ConstDecl { span, .. }
            | Stmt::FunDecl { span, .. }
            | Stmt::ModDecl { span, .. }
            | Stmt::ExprStmt { span, .. }
            | Stmt::Block { span, .. }
            | Stmt::Return { span, .. }
            | Stmt::If { span, .. }
            | Stmt::While { span, .. }
            | Stmt::For { span, .. }
            | Stmt::Break { span }
            | Stmt::Continue { span }
            | Stmt::ClassDecl { span, .. }
            | Stmt::Throw { span, .. }
            | Stmt::TryCatch { span, .. } => *span,
        }
    }
}
#[derive(Debug, Clone)]
pub enum InterpolationPart {
    Literal(String),
    Expr(Expr),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Pos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncDecOp {
    Inc,
    Dec,
}
