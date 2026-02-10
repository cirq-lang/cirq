//! # Parser Module
//!
//! Recursive descent parser with Pratt-style precedence climbing for
//! expressions. Consumes a flat token stream from the lexer and produces
//! an AST for the compiler.
//!
//! ## Key Features
//! - Pratt parser for correct operator precedence with minimal code.
//! - Zero-copy token references where possible.
//! - Comprehensive error recovery with span-accurate diagnostics.

use crate::ast::*;
use crate::error::{CirqError, CirqResult, Span};
use crate::token::{Token, TokenKind};

// -----------------------------------------------------------------------------
// PRECEDENCE LEVELS
// -----------------------------------------------------------------------------

/// Operator precedence levels, ordered from lowest to highest.
/// Used by the Pratt parser to determine binding strength.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
    Assignment = 1, // = += -= etc.
    Or = 2,         // ||
    And = 3,        // &&
    BitOr = 4,      // |
    BitXor = 5,     // ^
    BitAnd = 6,     // &
    Equality = 7,   // == !=
    Comparison = 8, // < > <= >=
    Shift = 9,      // << >>
    Term = 10,      // + -
    Factor = 11,    // * / %
    Power = 12,     // **
    Unary = 13,     // ! - + ~ ++ --
    Call = 14,      // . () []
}

// -----------------------------------------------------------------------------
// PARSER STATE
// -----------------------------------------------------------------------------

/// The Cirq parser. Turns tokens into an abstract syntax tree.
pub struct Parser {
    /// Token stream produced by the lexer.
    tokens: Vec<Token>,
    /// Current position in the token stream.
    current: usize,
}

impl Parser {
    /// Creates a new parser for the given token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Parses the entire token stream into a list of statements.
    ///
    /// # Errors
    /// Returns a `CirqError` on any syntax error.
    pub fn parse(&mut self) -> CirqResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        Ok(stmts)
    }

    // -------------------------------------------------------------------------
    // DECLARATION PARSING
    // -------------------------------------------------------------------------

    /// Parses a declaration or falls through to a statement.
    fn declaration(&mut self) -> CirqResult<Stmt> {
        match self.peek_kind() {
            TokenKind::Var => self.var_declaration(),
            TokenKind::Const => self.const_declaration(),
            TokenKind::Fun => self.fun_declaration(),
            TokenKind::Mod => self.mod_declaration(),
            _ => self.statement(),
        }
    }

    /// Parses `var name = expr;` or `var name;`
    fn var_declaration(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'var'
        let name = self.expect_ident("expected variable name")?;

        let initializer = if self.match_kind(&TokenKind::Eq) {
            Some(self.expression()?)
        } else {
            None
        };

        self.expect_semicolon()?;
        Ok(Stmt::VarDecl {
            name,
            initializer,
            span,
        })
    }

    /// Parses `const name = expr;`
    fn const_declaration(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'const'
        let name = self.expect_ident("expected constant name")?;
        self.expect_kind(&TokenKind::Eq, "expected '=' after constant name")?;
        let value = self.expression()?;
        self.expect_semicolon()?;
        Ok(Stmt::ConstDecl { name, value, span })
    }

    /// Parses `fun name(params...) { body }`
    fn fun_declaration(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'fun'
        let name = self.expect_ident("expected function name")?;
        let params = self.parse_param_list()?;
        let body = self.parse_block_body()?;
        Ok(Stmt::FunDecl {
            name,
            params,
            body,
            span,
        })
    }

    /// Parses `mod name { declarations... }`
    fn mod_declaration(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'mod'
        let name = self.expect_ident("expected module name")?;
        self.expect_kind(&TokenKind::LBrace, "expected '{' after module name")?;

        let mut body = Vec::new();
        while !self.check_kind(&TokenKind::RBrace) && !self.is_at_end() {
            body.push(self.declaration()?);
        }
        self.expect_kind(&TokenKind::RBrace, "expected '}' after module body")?;

        Ok(Stmt::ModDecl { name, body, span })
    }

    // -------------------------------------------------------------------------
    // STATEMENT PARSING
    // -------------------------------------------------------------------------

    /// Parses a statement (non-declaration).
    fn statement(&mut self) -> CirqResult<Stmt> {
        match self.peek_kind() {
            TokenKind::LBrace => self.block_statement(),
            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::Break => self.break_statement(),
            TokenKind::Continue => self.continue_statement(),
            _ => self.expression_statement(),
        }
    }

    /// Parses `{ statements... }`
    fn block_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.peek().span;
        let stmts = self.parse_block_body()?;
        Ok(Stmt::Block { stmts, span })
    }

    /// Parses `if (cond) { ... } else { ... }`
    fn if_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'if'
        self.expect_kind(&TokenKind::LParen, "expected '(' after 'if'")?;
        let condition = self.expression()?;
        self.expect_kind(&TokenKind::RParen, "expected ')' after condition")?;
        let then_branch = Box::new(self.statement()?);

        let else_branch = if self.match_kind(&TokenKind::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }

    /// Parses `while (cond) { ... }`
    fn while_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'while'
        self.expect_kind(&TokenKind::LParen, "expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.expect_kind(&TokenKind::RParen, "expected ')' after condition")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While {
            condition,
            body,
            span,
        })
    }

    /// Parses `for (init; cond; update) { ... }`
    fn for_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'for'
        self.expect_kind(&TokenKind::LParen, "expected '(' after 'for'")?;

        // Initializer
        let init = if self.match_kind(&TokenKind::Semicolon) {
            None
        } else if self.check_kind(&TokenKind::Var) {
            let decl = self.var_declaration()?;
            Some(Box::new(decl))
        } else {
            let expr = self.expression()?;
            let s = expr.span();
            self.expect_semicolon()?;
            Some(Box::new(Stmt::ExprStmt { expr, span: s }))
        };

        // Condition
        let condition = if self.check_kind(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect_semicolon()?;

        // Update
        let update = if self.check_kind(&TokenKind::RParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect_kind(&TokenKind::RParen, "expected ')' after for clauses")?;

        let body = Box::new(self.statement()?);
        Ok(Stmt::For {
            init,
            condition,
            update,
            body,
            span,
        })
    }

    /// Parses `return expr;` or `return;`
    fn return_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span; // consume 'return'
        let value = if self.check_kind(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect_semicolon()?;
        Ok(Stmt::Return { value, span })
    }

    /// Parses `break;`
    fn break_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span;
        self.expect_semicolon()?;
        Ok(Stmt::Break { span })
    }

    /// Parses `continue;`
    fn continue_statement(&mut self) -> CirqResult<Stmt> {
        let span = self.advance().span;
        self.expect_semicolon()?;
        Ok(Stmt::Continue { span })
    }

    /// Parses an expression statement: `expr;`
    fn expression_statement(&mut self) -> CirqResult<Stmt> {
        let expr = self.expression()?;
        let span = expr.span();
        self.expect_semicolon()?;
        Ok(Stmt::ExprStmt { expr, span })
    }

    // -------------------------------------------------------------------------
    // EXPRESSION PARSING — Pratt / Precedence Climbing
    // -------------------------------------------------------------------------

    /// Parses an expression at the lowest precedence.
    fn expression(&mut self) -> CirqResult<Expr> {
        self.parse_precedence(Precedence::Assignment)
    }

    /// Core Pratt parser: parses expressions with the given minimum
    /// precedence, handling left-to-right and right-to-left associativity.
    fn parse_precedence(&mut self, min_prec: Precedence) -> CirqResult<Expr> {
        // Parse prefix / primary
        let mut left = self.parse_prefix()?;

        // Parse infix and postfix operators
        loop {
            if self.is_at_end() {
                break;
            }

            let prec = self.get_infix_precedence();
            if prec < min_prec {
                break;
            }

            left = self.parse_infix(left, prec)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression (unary, literals, grouping, identifiers).
    fn parse_prefix(&mut self) -> CirqResult<Expr> {
        let token = self.peek().clone();
        match &token.kind {
            // Unary operators
            TokenKind::Minus => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    span: token.span,
                    operand: Box::new(operand),
                })
            }
            TokenKind::Plus => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Pos,
                    span: token.span,
                    operand: Box::new(operand),
                })
            }
            TokenKind::Bang => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    span: token.span,
                    operand: Box::new(operand),
                })
            }
            TokenKind::Tilde => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::BitNot,
                    span: token.span,
                    operand: Box::new(operand),
                })
            }

            // Pre-increment / pre-decrement
            TokenKind::PlusPlus => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::PreIncDec {
                    op: IncDecOp::Inc,
                    operand: Box::new(operand),
                    span: token.span,
                })
            }
            TokenKind::MinusMinus => {
                self.advance();
                let operand = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::PreIncDec {
                    op: IncDecOp::Dec,
                    operand: Box::new(operand),
                    span: token.span,
                })
            }

            // Literals
            TokenKind::Number(_) => {
                let t = self.advance();
                if let TokenKind::Number(v) = t.kind {
                    Ok(Expr::Number {
                        value: v,
                        span: t.span,
                    })
                } else {
                    unreachable!()
                }
            }
            TokenKind::Str(_) => {
                let t = self.advance();
                if let TokenKind::Str(s) = t.kind {
                    Ok(Expr::Str {
                        value: s,
                        span: t.span,
                    })
                } else {
                    unreachable!()
                }
            }
            TokenKind::True => {
                let t = self.advance();
                Ok(Expr::Bool {
                    value: true,
                    span: t.span,
                })
            }
            TokenKind::False => {
                let t = self.advance();
                Ok(Expr::Bool {
                    value: false,
                    span: t.span,
                })
            }
            TokenKind::Null => {
                let t = self.advance();
                Ok(Expr::Null { span: t.span })
            }

            // Identifier
            TokenKind::Ident(_) => {
                let t = self.advance();
                if let TokenKind::Ident(name) = t.kind {
                    Ok(Expr::Ident { name, span: t.span })
                } else {
                    unreachable!()
                }
            }

            // Parenthesized expression
            TokenKind::LParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect_kind(&TokenKind::RParen, "expected ')'")?;
                Ok(expr)
            }

            // Array literal
            TokenKind::LBracket => self.parse_array_literal(),

            // String interpolation
            TokenKind::StringStart(_) => self.parse_string_interpolation(),

            _ => Err(CirqError::parser(
                format!("unexpected token: {:?}", token.kind),
                token.span,
            )),
        }
    }

    /// Parses an infix expression (binary op, assignment, call, index, member).
    fn parse_infix(&mut self, left: Expr, prec: Precedence) -> CirqResult<Expr> {
        let token = self.peek().clone();
        match &token.kind {
            // Assignment
            TokenKind::Eq => {
                self.advance();
                let value = self.parse_precedence(Precedence::Assignment)?;
                Ok(Expr::Assign {
                    span: token.span,
                    target: Box::new(left),
                    value: Box::new(value),
                })
            }

            // Compound assignment
            TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq
            | TokenKind::PercentEq
            | TokenKind::PowerEq
            | TokenKind::AmpEq
            | TokenKind::PipeEq
            | TokenKind::CaretEq
            | TokenKind::ShlEq
            | TokenKind::ShrEq => {
                let t = self.advance();
                let op = compound_to_binop(&t.kind);
                let value = self.parse_precedence(Precedence::Assignment)?;
                Ok(Expr::CompoundAssign {
                    target: Box::new(left),
                    op,
                    value: Box::new(value),
                    span: token.span,
                })
            }

            // Postfix increment/decrement
            TokenKind::PlusPlus => {
                self.advance();
                Ok(Expr::PostIncDec {
                    op: IncDecOp::Inc,
                    operand: Box::new(left),
                    span: token.span,
                })
            }
            TokenKind::MinusMinus => {
                self.advance();
                Ok(Expr::PostIncDec {
                    op: IncDecOp::Dec,
                    operand: Box::new(left),
                    span: token.span,
                })
            }

            // Binary operators
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Power
            | TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LtEq
            | TokenKind::GtEq
            | TokenKind::AmpAmp
            | TokenKind::PipePipe
            | TokenKind::Amp
            | TokenKind::Pipe
            | TokenKind::Caret
            | TokenKind::Shl
            | TokenKind::Shr => {
                let t = self.advance();
                let op = token_to_binop(&t.kind);

                // Right-associative for power
                let next_prec = if t.kind == TokenKind::Power {
                    prec
                } else {
                    Precedence::from_u8(prec as u8 + 1)
                };

                let right = self.parse_precedence(next_prec)?;
                Ok(Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    span: token.span,
                })
            }

            // Function call
            TokenKind::LParen => {
                self.advance();
                let args = self.parse_arg_list()?;
                let span = left.span();
                Ok(Expr::Call {
                    callee: Box::new(left),
                    args,
                    span,
                })
            }

            // Array index
            TokenKind::LBracket => {
                self.advance();
                let index = self.expression()?;
                self.expect_kind(&TokenKind::RBracket, "expected ']'")?;
                let span = left.span();
                Ok(Expr::Index {
                    object: Box::new(left),
                    index: Box::new(index),
                    span,
                })
            }

            // Member access
            TokenKind::Dot => {
                self.advance();
                let member = self.expect_ident("expected member name after '.'")?;
                let span = left.span();
                Ok(Expr::MemberAccess {
                    object: Box::new(left),
                    member,
                    span,
                })
            }

            _ => Ok(left),
        }
    }

    /// Returns the precedence of the current token as an infix operator.
    fn get_infix_precedence(&self) -> Precedence {
        match self.peek_kind() {
            TokenKind::Eq
            | TokenKind::PlusEq
            | TokenKind::MinusEq
            | TokenKind::StarEq
            | TokenKind::SlashEq
            | TokenKind::PercentEq
            | TokenKind::PowerEq
            | TokenKind::AmpEq
            | TokenKind::PipeEq
            | TokenKind::CaretEq
            | TokenKind::ShlEq
            | TokenKind::ShrEq => Precedence::Assignment,
            TokenKind::PipePipe => Precedence::Or,
            TokenKind::AmpAmp => Precedence::And,
            TokenKind::Pipe => Precedence::BitOr,
            TokenKind::Caret => Precedence::BitXor,
            TokenKind::Amp => Precedence::BitAnd,
            TokenKind::EqEq | TokenKind::BangEq => Precedence::Equality,
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq => {
                Precedence::Comparison
            }
            TokenKind::Shl | TokenKind::Shr => Precedence::Shift,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            TokenKind::Power => Precedence::Power,
            TokenKind::PlusPlus | TokenKind::MinusMinus => Precedence::Call,
            TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }

    // -------------------------------------------------------------------------
    // COMPOUND CONSTRUCTS
    // -------------------------------------------------------------------------

    /// Parses an array literal: `[elem1, elem2, ...]` with multiline support.
    fn parse_array_literal(&mut self) -> CirqResult<Expr> {
        let span = self.advance().span; // consume '['
        let mut elements = Vec::new();

        if !self.check_kind(&TokenKind::RBracket) {
            elements.push(self.expression()?);
            while self.match_kind(&TokenKind::Comma) {
                if self.check_kind(&TokenKind::RBracket) {
                    break; // trailing comma
                }
                elements.push(self.expression()?);
            }
        }

        self.expect_kind(&TokenKind::RBracket, "expected ']' after array elements")?;
        Ok(Expr::Array { elements, span })
    }

    /// Parses a string interpolation sequence from `StringStart` through
    /// expression tokens and `StringPart`/`StringEnd` tokens.
    fn parse_string_interpolation(&mut self) -> CirqResult<Expr> {
        let t = self.advance();
        let span = t.span;
        let prefix = if let TokenKind::StringStart(s) = t.kind {
            s
        } else {
            unreachable!()
        };

        let mut parts = Vec::new();
        if !prefix.is_empty() {
            parts.push(InterpolationPart::Literal(prefix));
        }

        loop {
            // Parse the expression between interpolation markers
            if !self.check_kind(&TokenKind::StringPart(String::new())) && !self.check_string_end() {
                let expr = self.expression()?;
                parts.push(InterpolationPart::Expr(expr));
            }

            // Check for StringPart (middle) or StringEnd (final)
            let next = self.peek().clone();
            match &next.kind {
                TokenKind::StringPart(_) => {
                    let t = self.advance();
                    if let TokenKind::StringPart(s) = t.kind {
                        if !s.is_empty() {
                            parts.push(InterpolationPart::Literal(s));
                        }
                    }
                }
                TokenKind::StringEnd(_) => {
                    let t = self.advance();
                    if let TokenKind::StringEnd(s) = t.kind {
                        if !s.is_empty() {
                            parts.push(InterpolationPart::Literal(s));
                        }
                    }
                    break;
                }
                _ => {
                    return Err(CirqError::parser(
                        "unterminated string interpolation",
                        next.span,
                    ));
                }
            }
        }

        Ok(Expr::Interpolation { parts, span })
    }

    /// Parses a parameter list: `(param1, param2, ...)`
    fn parse_param_list(&mut self) -> CirqResult<Vec<String>> {
        self.expect_kind(&TokenKind::LParen, "expected '(' for parameter list")?;
        let mut params = Vec::new();

        if !self.check_kind(&TokenKind::RParen) {
            params.push(self.expect_ident("expected parameter name")?);
            while self.match_kind(&TokenKind::Comma) {
                params.push(self.expect_ident("expected parameter name")?);
            }
        }

        self.expect_kind(&TokenKind::RParen, "expected ')' after parameters")?;
        Ok(params)
    }

    /// Parses a call argument list: `(arg1, arg2, ...)`
    fn parse_arg_list(&mut self) -> CirqResult<Vec<Expr>> {
        let mut args = Vec::new();

        if !self.check_kind(&TokenKind::RParen) {
            args.push(self.expression()?);
            while self.match_kind(&TokenKind::Comma) {
                args.push(self.expression()?);
            }
        }

        self.expect_kind(&TokenKind::RParen, "expected ')' after arguments")?;
        Ok(args)
    }

    /// Parses the body of a block: `{ stmts... }`
    fn parse_block_body(&mut self) -> CirqResult<Vec<Stmt>> {
        self.expect_kind(&TokenKind::LBrace, "expected '{'")?;
        let mut stmts = Vec::new();
        while !self.check_kind(&TokenKind::RBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.expect_kind(&TokenKind::RBrace, "expected '}'")?;
        Ok(stmts)
    }

    // -------------------------------------------------------------------------
    // TOKEN HELPERS
    // -------------------------------------------------------------------------

    /// Returns a reference to the current token without consuming it.
    #[inline]
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    /// Returns the kind of the current token.
    #[inline]
    fn peek_kind(&self) -> &TokenKind {
        &self.tokens[self.current].kind
    }

    /// Consumes and returns the current token.
    #[inline]
    fn advance(&mut self) -> Token {
        let token = self.tokens[self.current].clone();
        if !self.is_at_end() {
            self.current += 1;
        }
        token
    }

    /// Returns `true` if the current token is `Eof`.
    #[inline]
    fn is_at_end(&self) -> bool {
        matches!(self.tokens[self.current].kind, TokenKind::Eof)
    }

    /// Returns `true` if the current token matches the given kind.
    fn check_kind(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek_kind()) == std::mem::discriminant(kind)
    }

    /// Checks if current token is a `StringEnd` variant.
    fn check_string_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::StringEnd(_))
    }

    /// Consumes the current token if it matches `kind`, returning `true`.
    fn match_kind(&mut self, kind: &TokenKind) -> bool {
        if self.check_kind(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Expects the current token to match `kind`, returning an error otherwise.
    fn expect_kind(&mut self, kind: &TokenKind, msg: &str) -> CirqResult<Token> {
        if self.check_kind(kind) {
            Ok(self.advance())
        } else {
            Err(CirqError::parser(msg, self.peek().span))
        }
    }

    /// Expects an identifier token and returns the name string.
    fn expect_ident(&mut self, msg: &str) -> CirqResult<String> {
        let token = self.peek().clone();
        if let TokenKind::Ident(_) = &token.kind {
            let t = self.advance();
            if let TokenKind::Ident(name) = t.kind {
                Ok(name)
            } else {
                unreachable!()
            }
        } else {
            Err(CirqError::parser(msg, token.span))
        }
    }

    /// Expects a semicolon token.
    fn expect_semicolon(&mut self) -> CirqResult<Token> {
        self.expect_kind(&TokenKind::Semicolon, "expected ';'")
    }
}

// -----------------------------------------------------------------------------
// OPERATOR CONVERSION HELPERS
// -----------------------------------------------------------------------------

/// Converts a binary operator token kind to its AST `BinOp`.
fn token_to_binop(kind: &TokenKind) -> BinOp {
    match kind {
        TokenKind::Plus => BinOp::Add,
        TokenKind::Minus => BinOp::Sub,
        TokenKind::Star => BinOp::Mul,
        TokenKind::Slash => BinOp::Div,
        TokenKind::Percent => BinOp::Mod,
        TokenKind::Power => BinOp::Pow,
        TokenKind::EqEq => BinOp::Eq,
        TokenKind::BangEq => BinOp::Ne,
        TokenKind::Lt => BinOp::Lt,
        TokenKind::Gt => BinOp::Gt,
        TokenKind::LtEq => BinOp::Le,
        TokenKind::GtEq => BinOp::Ge,
        TokenKind::AmpAmp => BinOp::And,
        TokenKind::PipePipe => BinOp::Or,
        TokenKind::Amp => BinOp::BitAnd,
        TokenKind::Pipe => BinOp::BitOr,
        TokenKind::Caret => BinOp::BitXor,
        TokenKind::Shl => BinOp::Shl,
        TokenKind::Shr => BinOp::Shr,
        _ => unreachable!("not a binary operator: {:?}", kind),
    }
}

/// Converts a compound assignment token kind to its underlying `BinOp`.
fn compound_to_binop(kind: &TokenKind) -> BinOp {
    match kind {
        TokenKind::PlusEq => BinOp::Add,
        TokenKind::MinusEq => BinOp::Sub,
        TokenKind::StarEq => BinOp::Mul,
        TokenKind::SlashEq => BinOp::Div,
        TokenKind::PercentEq => BinOp::Mod,
        TokenKind::PowerEq => BinOp::Pow,
        TokenKind::AmpEq => BinOp::BitAnd,
        TokenKind::PipeEq => BinOp::BitOr,
        TokenKind::CaretEq => BinOp::BitXor,
        TokenKind::ShlEq => BinOp::Shl,
        TokenKind::ShrEq => BinOp::Shr,
        _ => unreachable!("not a compound assignment: {:?}", kind),
    }
}

// -----------------------------------------------------------------------------
// PRECEDENCE HELPER
// -----------------------------------------------------------------------------

impl Precedence {
    /// Converts a `u8` to a `Precedence`, clamping to `Call` if out of range.
    fn from_u8(v: u8) -> Self {
        match v {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::BitOr,
            5 => Precedence::BitXor,
            6 => Precedence::BitAnd,
            7 => Precedence::Equality,
            8 => Precedence::Comparison,
            9 => Precedence::Shift,
            10 => Precedence::Term,
            11 => Precedence::Factor,
            12 => Precedence::Power,
            13 => Precedence::Unary,
            _ => Precedence::Call,
        }
    }
}

// -----------------------------------------------------------------------------
// AST SPAN HELPER
// -----------------------------------------------------------------------------

impl Expr {
    /// Returns the source span of this expression node.
    pub fn span(&self) -> Span {
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
            | Expr::PostIncDec { span, .. } => *span,
        }
    }
}
