use crate::error::{CirqError, CirqResult, Span};
use crate::token::{Token, TokenKind, lookup_keyword};
pub struct Lexer<'src> {
    source: &'src [u8],
    pos: usize,
    line: u32,
    col: u32,
    pending: Vec<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source: source.as_bytes(),
            pos: 0,
            line: 1,
            col: 1,
            pending: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> CirqResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.source.len() / 4);

        loop {
            if !self.pending.is_empty() {
                tokens.extend(self.pending.drain(..));
                continue;
            }

            self.skip_whitespace_and_comments();

            if self.is_at_end() {
                tokens.push(Token::new(
                    TokenKind::Eof,
                    Span::new(self.line, self.col, 0),
                ));
                break;
            }

            let token = self.scan_token()?;
            tokens.push(token);
        }

        Ok(tokens)
    }
    fn scan_token(&mut self) -> CirqResult<Token> {
        let start_line = self.line;
        let start_col = self.col;
        let start_pos = self.pos;

        let byte = self.advance();

        let kind = match byte {
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b',' => TokenKind::Comma,
            b';' => TokenKind::Semicolon,
            b'~' => TokenKind::Tilde,

            b'.' => TokenKind::Dot,

            b'+' => {
                if self.match_byte(b'=') {
                    TokenKind::PlusEq
                } else if self.match_byte(b'+') {
                    TokenKind::PlusPlus
                } else {
                    TokenKind::Plus
                }
            }
            b'-' => {
                if self.match_byte(b'=') {
                    TokenKind::MinusEq
                } else if self.match_byte(b'-') {
                    TokenKind::MinusMinus
                } else {
                    TokenKind::Minus
                }
            }
            b'*' => {
                if self.match_byte(b'*') {
                    if self.match_byte(b'=') {
                        TokenKind::PowerEq
                    } else {
                        TokenKind::Power
                    }
                } else if self.match_byte(b'=') {
                    TokenKind::StarEq
                } else {
                    TokenKind::Star
                }
            }
            b'/' => {
                if self.match_byte(b'=') {
                    TokenKind::SlashEq
                } else {
                    TokenKind::Slash
                }
            }
            b'%' => {
                if self.match_byte(b'=') {
                    TokenKind::PercentEq
                } else {
                    TokenKind::Percent
                }
            }
            b'=' => {
                if self.match_byte(b'=') {
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }
            b'!' => {
                if self.match_byte(b'=') {
                    TokenKind::BangEq
                } else {
                    TokenKind::Bang
                }
            }
            b'<' => {
                if self.match_byte(b'<') {
                    if self.match_byte(b'=') {
                        TokenKind::ShlEq
                    } else {
                        TokenKind::Shl
                    }
                } else if self.match_byte(b'=') {
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            b'>' => {
                if self.match_byte(b'>') {
                    if self.match_byte(b'=') {
                        TokenKind::ShrEq
                    } else {
                        TokenKind::Shr
                    }
                } else if self.match_byte(b'=') {
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            b'&' => {
                if self.match_byte(b'&') {
                    TokenKind::AmpAmp
                } else if self.match_byte(b'=') {
                    TokenKind::AmpEq
                } else {
                    TokenKind::Amp
                }
            }
            b'|' => {
                if self.match_byte(b'|') {
                    TokenKind::PipePipe
                } else if self.match_byte(b'=') {
                    TokenKind::PipeEq
                } else {
                    TokenKind::Pipe
                }
            }
            b'^' => {
                if self.match_byte(b'=') {
                    TokenKind::CaretEq
                } else {
                    TokenKind::Caret
                }
            }

            b'"' | b'\'' => return self.scan_string(byte, start_line, start_col, start_pos),

            b'0'..=b'9' => self.scan_number(byte, start_line, start_col)?,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.scan_identifier(start_pos)?,

            _ => {
                return Err(CirqError::lexer(
                    format!("unexpected character: '{}'", byte as char),
                    Span::new(start_line, start_col, 1),
                ));
            }
        };

        let len = (self.pos - start_pos) as u32;
        Ok(Token::new(kind, Span::new(start_line, start_col, len)))
    }
    fn scan_string(
        &mut self,
        quote: u8,
        start_line: u32,
        start_col: u32,
        start_pos: usize,
    ) -> CirqResult<Token> {
        let mut buf = String::new();
        let mut interp_segments: Vec<(String, Vec<Token>)> = Vec::new();

        loop {
            if self.is_at_end() {
                return Err(CirqError::lexer(
                    "unterminated string literal",
                    Span::new(start_line, start_col, 1),
                ));
            }

            let byte = self.peek();

            if byte == quote {
                self.advance();
                break;
            }

            if byte == b'\\' {
                self.advance();
                if self.is_at_end() {
                    return Err(CirqError::lexer(
                        "unterminated escape sequence",
                        Span::new(self.line, self.col, 1),
                    ));
                }

                let esc = self.peek();
                if esc == b'(' {
                    self.advance(); // consume '('
                    let expr_tokens = self.scan_interpolation_expr(start_line, start_col)?;
                    interp_segments.push((std::mem::take(&mut buf), expr_tokens));
                } else {
                    self.advance();
                    match esc {
                        b'n' => buf.push('\n'),
                        b'r' => buf.push('\r'),
                        b't' => buf.push('\t'),
                        b'\\' => buf.push('\\'),
                        b'\'' => buf.push('\''),
                        b'"' => buf.push('"'),
                        b'0' => buf.push('\0'),
                        _ => {
                            buf.push('\\');
                            buf.push(esc as char);
                        }
                    }
                }
                continue;
            }

            if byte == b'\n' {
                self.line += 1;
                self.col = 0;
            }
            self.advance();
            buf.push(byte as char);
        }

        let total_len = (self.pos - start_pos) as u32;
        let span = Span::new(start_line, start_col, total_len);

        if interp_segments.is_empty() {
            return Ok(Token::new(TokenKind::Str(buf), span));
        }
        let first = interp_segments.remove(0);
        let first_token = Token::new(TokenKind::StringStart(first.0), span);

        self.pending.extend(first.1);

        for (part_str, expr_tokens) in interp_segments {
            self.pending
                .push(Token::new(TokenKind::StringPart(part_str), span));
            self.pending.extend(expr_tokens);
        }

        self.pending
            .push(Token::new(TokenKind::StringEnd(buf), span));

        Ok(first_token)
    }

    fn scan_interpolation_expr(
        &mut self,
        str_start_line: u32,
        str_start_col: u32,
    ) -> CirqResult<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut paren_depth: u32 = 1;

        loop {
            self.skip_whitespace_and_comments();

            if self.is_at_end() {
                return Err(CirqError::lexer(
                    "unterminated string interpolation \\(...)",
                    Span::new(str_start_line, str_start_col, 1),
                ));
            }

            if self.peek() == b')' {
                paren_depth -= 1;
                if paren_depth == 0 {
                    self.advance(); // consume the closing ')'
                    break;
                }
            }

            let token = self.scan_token()?;

            match &token.kind {
                TokenKind::LParen => paren_depth += 1,
                _ => {}
            }

            tokens.push(token);
        }

        Ok(tokens)
    }
    fn scan_number(&mut self, first: u8, start_line: u32, start_col: u32) -> CirqResult<TokenKind> {
        if first == b'0' && !self.is_at_end() {
            match self.peek() {
                b'x' | b'X' => {
                    self.advance();
                    return self.scan_radix_number(16, "hex", "0x", start_line, start_col);
                }
                b'b' | b'B' => {
                    self.advance();
                    return self.scan_radix_number(2, "binary", "0b", start_line, start_col);
                }
                b'o' | b'O' => {
                    self.advance();
                    return self.scan_radix_number(8, "octal", "0o", start_line, start_col);
                }
                _ => {}
            }
        }

        let mut num_str = String::new();
        num_str.push(first as char);

        while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == b'_') {
            let b = self.advance();
            if b != b'_' {
                num_str.push(b as char);
            }
        }

        if !self.is_at_end()
            && self.peek() == b'.'
            && self.pos + 1 < self.source.len()
            && self.source[self.pos + 1].is_ascii_digit()
        {
            self.advance(); // consume '.'
            num_str.push('.');
            while !self.is_at_end() && (self.peek().is_ascii_digit() || self.peek() == b'_') {
                let b = self.advance();
                if b != b'_' {
                    num_str.push(b as char);
                }
            }
        }

        if !self.is_at_end() && (self.peek() == b'e' || self.peek() == b'E') {
            num_str.push(self.advance() as char);
            if !self.is_at_end() && (self.peek() == b'+' || self.peek() == b'-') {
                num_str.push(self.advance() as char);
            }
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                num_str.push(self.advance() as char);
            }
        }

        let value: f64 = num_str.parse().map_err(|_| {
            CirqError::lexer(
                format!("invalid number literal: {}", num_str),
                Span::new(start_line, start_col, num_str.len() as u32),
            )
        })?;

        Ok(TokenKind::Number(value))
    }

    fn scan_radix_number(
        &mut self,
        radix: u32,
        name: &str,
        prefix: &str,
        start_line: u32,
        start_col: u32,
    ) -> CirqResult<TokenKind> {
        let mut digits = String::new();

        let is_valid_digit = |b: u8| -> bool {
            match radix {
                16 => b.is_ascii_hexdigit(),
                8 => (b'0'..=b'7').contains(&b),
                2 => b == b'0' || b == b'1',
                _ => false,
            }
        };

        while !self.is_at_end() && (is_valid_digit(self.peek()) || self.peek() == b'_') {
            let b = self.advance();
            if b != b'_' {
                digits.push(b as char);
            }
        }

        if digits.is_empty() {
            return Err(CirqError::lexer(
                format!("expected {} digits after '{}'", name, prefix),
                Span::new(start_line, start_col, prefix.len() as u32),
            ));
        }

        let value = u64::from_str_radix(&digits, radix).map_err(|_| {
            CirqError::lexer(
                format!("invalid {} literal: {}{}", name, prefix, digits),
                Span::new(
                    start_line,
                    start_col,
                    digits.len() as u32 + prefix.len() as u32,
                ),
            )
        })?;

        Ok(TokenKind::Number(value as f64))
    }
    fn scan_identifier(&mut self, start_pos: usize) -> CirqResult<TokenKind> {
        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == b'_') {
            self.advance();
        }

        let text = std::str::from_utf8(&self.source[start_pos..self.pos]).map_err(|_| {
            CirqError::lexer(
                "invalid UTF-8 in identifier",
                Span::new(self.line, self.col, (self.pos - start_pos) as u32),
            )
        })?;

        match lookup_keyword(text) {
            Some(keyword) => Ok(keyword),
            None => Ok(TokenKind::Ident(text.to_string())),
        }
    }
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while !self.is_at_end() {
                match self.peek() {
                    b' ' | b'\t' | b'\r' => {
                        self.advance();
                    }
                    b'\n' => {
                        self.advance();
                        self.line += 1;
                        self.col = 1;
                    }
                    _ => break,
                }
            }

            if self.pos + 1 < self.source.len()
                && self.source[self.pos] == b'/'
                && self.source[self.pos + 1] == b'/'
            {
                let remaining = &self.source[self.pos..];
                match memchr::memchr(b'\n', remaining) {
                    Some(offset) => {
                        self.pos += offset;
                        self.col += offset as u32;
                    }
                    None => {
                        let skip = self.source.len() - self.pos;
                        self.col += skip as u32;
                        self.pos = self.source.len();
                    }
                }
                continue;
            }

            break;
        }
    }
    #[inline(always)]
    fn peek(&self) -> u8 {
        self.source[self.pos]
    }

    #[inline(always)]
    fn advance(&mut self) -> u8 {
        let byte = self.source[self.pos];
        self.pos += 1;
        self.col += 1;
        byte
    }

    #[inline]
    fn match_byte(&mut self, expected: u8) -> bool {
        if !self.is_at_end() && self.source[self.pos] == expected {
            self.pos += 1;
            self.col += 1;
            true
        } else {
            false
        }
    }

    #[inline(always)]
    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }
}
