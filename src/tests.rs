#[cfg(test)]
mod tests {
    use crate::builtin::{BuiltinModule, IoModule, MathModule};
    use crate::compiler::Compiler;
    use crate::error::ErrorKind;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::TokenKind;
    use crate::value::Value;
    use crate::vm::Vm;

    use std::rc::Rc;
    fn run(source: &str) -> Result<Value, String> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().map_err(|e| e.to_string())?;
        let compiler = Compiler::new();
        let program = compiler.compile(&ast).map_err(|e| e.to_string())?;

        let mut vm = Vm::new();
        let io_mod = IoModule;
        vm.register_module(io_mod.name(), io_mod.build());
        let math_mod = MathModule;
        vm.register_module(math_mod.name(), math_mod.build());
        let result = vm.execute(program).map_err(|e| e.to_string())?;

        Ok(vm.get_global("__result").unwrap_or(result))
    }

    fn tokenize(source: &str) -> Result<Vec<TokenKind>, String> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        Ok(tokens
            .into_iter()
            .map(|t| t.kind)
            .filter(|k| !matches!(k, TokenKind::Eof))
            .collect())
    }

    fn expect_error(source: &str, kind: ErrorKind) {
        let mut lexer = Lexer::new(source);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(e) => {
                assert_eq!(e.kind, kind, "Expected {:?} error, got: {}", kind, e);
                return;
            }
        };

        let mut parser = Parser::new(tokens);
        let ast = match parser.parse() {
            Ok(a) => a,
            Err(e) => {
                assert_eq!(e.kind, kind, "Expected {:?} error, got: {}", kind, e);
                return;
            }
        };

        let compiler = Compiler::new();
        let program = match compiler.compile(&ast) {
            Ok(p) => p,
            Err(e) => {
                assert_eq!(e.kind, kind, "Expected {:?} error, got: {}", kind, e);
                return;
            }
        };

        let mut vm = Vm::new();
        let io_mod = IoModule;
        vm.register_module(io_mod.name(), io_mod.build());
        let math_mod = MathModule;
        vm.register_module(math_mod.name(), math_mod.build());

        match vm.execute(program) {
            Ok(_) => panic!("Expected {:?} error but program succeeded", kind),
            Err(e) => {
                assert_eq!(e.kind, kind, "Expected {:?} error, got: {}", kind, e);
            }
        }
    }
    #[test]
    fn lexer_integer_literals() {
        let kinds = tokenize("42").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(42.0)]);
    }

    #[test]
    fn lexer_float_literals() {
        let kinds = tokenize("3.14").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(3.14)]);
    }

    #[test]
    fn lexer_hex_literals() {
        let kinds = tokenize("0xFF").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(255.0)]);
    }

    #[test]
    fn lexer_binary_literals() {
        let kinds = tokenize("0b1010").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(10.0)]);
    }

    #[test]
    fn lexer_octal_literals() {
        let kinds = tokenize("0o17").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(15.0)]);
    }

    #[test]
    fn lexer_underscore_separators() {
        let kinds = tokenize("1_000_000").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(1_000_000.0)]);
    }

    #[test]
    fn lexer_string_double_quotes() {
        let kinds = tokenize(r#""hello""#).unwrap();
        assert_eq!(kinds, vec![TokenKind::Str("hello".to_string())]);
    }

    #[test]
    fn lexer_string_single_quotes() {
        let kinds = tokenize("'world'").unwrap();
        assert_eq!(kinds, vec![TokenKind::Str("world".to_string())]);
    }

    #[test]
    fn lexer_string_escape_sequences() {
        let kinds = tokenize(r#""hello\nworld""#).unwrap();
        assert_eq!(kinds, vec![TokenKind::Str("hello\nworld".to_string())]);
    }

    #[test]
    fn lexer_string_tab_escape() {
        let kinds = tokenize(r#""a\tb""#).unwrap();
        assert_eq!(kinds, vec![TokenKind::Str("a\tb".to_string())]);
    }

    #[test]
    fn lexer_empty_string() {
        let kinds = tokenize(r#""""#).unwrap();
        assert_eq!(kinds, vec![TokenKind::Str(String::new())]);
    }

    #[test]
    fn lexer_keywords() {
        let kinds = tokenize("var const fun mod if else while for return break continue").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Var,
                TokenKind::Const,
                TokenKind::Fun,
                TokenKind::Mod,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::While,
                TokenKind::For,
                TokenKind::Return,
                TokenKind::Break,
                TokenKind::Continue,
            ]
        );
    }

    #[test]
    fn lexer_boolean_and_null() {
        let kinds = tokenize("true false null").unwrap();
        assert_eq!(
            kinds,
            vec![TokenKind::True, TokenKind::False, TokenKind::Null]
        );
    }

    #[test]
    fn lexer_arithmetic_operators() {
        let kinds = tokenize("+ - * / % **").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Power,
            ]
        );
    }

    #[test]
    fn lexer_compound_assignment() {
        let kinds = tokenize("+= -= *= /= %= **=").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::PlusEq,
                TokenKind::MinusEq,
                TokenKind::StarEq,
                TokenKind::SlashEq,
                TokenKind::PercentEq,
                TokenKind::PowerEq,
            ]
        );
    }

    #[test]
    fn lexer_comparison_operators() {
        let kinds = tokenize("== != < > <= >=").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::EqEq,
                TokenKind::BangEq,
                TokenKind::Lt,
                TokenKind::Gt,
                TokenKind::LtEq,
                TokenKind::GtEq,
            ]
        );
    }

    #[test]
    fn lexer_logical_operators() {
        let kinds = tokenize("&& || !").unwrap();
        assert_eq!(
            kinds,
            vec![TokenKind::AmpAmp, TokenKind::PipePipe, TokenKind::Bang]
        );
    }

    #[test]
    fn lexer_bitwise_operators() {
        let kinds = tokenize("& | ^ ~ << >>").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Amp,
                TokenKind::Pipe,
                TokenKind::Caret,
                TokenKind::Tilde,
                TokenKind::Shl,
                TokenKind::Shr,
            ]
        );
    }

    #[test]
    fn lexer_increment_decrement() {
        let kinds = tokenize("++ --").unwrap();
        assert_eq!(kinds, vec![TokenKind::PlusPlus, TokenKind::MinusMinus]);
    }

    #[test]
    fn lexer_delimiters() {
        let kinds = tokenize("( ) { } [ ]").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::LBracket,
                TokenKind::RBracket,
            ]
        );
    }

    #[test]
    fn lexer_punctuation() {
        let kinds = tokenize(", ; .").unwrap();
        assert_eq!(
            kinds,
            vec![TokenKind::Comma, TokenKind::Semicolon, TokenKind::Dot]
        );
    }

    #[test]
    fn lexer_identifiers() {
        let kinds = tokenize("foo bar _baz camelCase").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Ident("foo".to_string()),
                TokenKind::Ident("bar".to_string()),
                TokenKind::Ident("_baz".to_string()),
                TokenKind::Ident("camelCase".to_string()),
            ]
        );
    }

    #[test]
    fn lexer_skips_line_comments() {
        let kinds = tokenize("42 // this is a comment\n99").unwrap();
        assert_eq!(
            kinds,
            vec![TokenKind::Number(42.0), TokenKind::Number(99.0)]
        );
    }

    #[test]
    fn lexer_skips_whitespace() {
        let kinds = tokenize("  42  \n\t  99  ").unwrap();
        assert_eq!(
            kinds,
            vec![TokenKind::Number(42.0), TokenKind::Number(99.0)]
        );
    }

    #[test]
    fn lexer_unterminated_string_error() {
        let result = tokenize(r#""hello"#);
        assert!(result.is_err());
    }

    #[test]
    fn lexer_scientific_notation() {
        let kinds = tokenize("1e3").unwrap();
        assert_eq!(kinds, vec![TokenKind::Number(1000.0)]);
    }

    #[test]
    fn lexer_bitwise_compound_assignment() {
        let kinds = tokenize("&= |= ^= <<= >>=").unwrap();
        assert_eq!(
            kinds,
            vec![
                TokenKind::AmpEq,
                TokenKind::PipeEq,
                TokenKind::CaretEq,
                TokenKind::ShlEq,
                TokenKind::ShrEq,
            ]
        );
    }
    #[test]
    fn value_truthiness_null() {
        assert!(!Value::Null.is_truthy());
    }

    #[test]
    fn value_truthiness_bool() {
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
    }

    #[test]
    fn value_truthiness_num() {
        assert!(Value::Num(1.0).is_truthy());
        assert!(Value::Num(-1.0).is_truthy());
        assert!(!Value::Num(0.0).is_truthy());
    }

    #[test]
    fn value_truthiness_str() {
        assert!(Value::Str(Rc::new("hello".to_string())).is_truthy());
        assert!(!Value::Str(Rc::new(String::new())).is_truthy());
    }

    #[test]
    fn value_truthiness_array() {
        use std::cell::RefCell;
        let arr = Value::Array(Rc::new(RefCell::new(vec![])));
        assert!(arr.is_truthy()); // Empty array is truthy
    }

    #[test]
    fn value_type_names() {
        assert_eq!(Value::Num(0.0).type_name(), "num");
        assert_eq!(Value::Bool(true).type_name(), "bool");
        assert_eq!(Value::Null.type_name(), "null");
        assert_eq!(Value::Str(Rc::new(String::new())).type_name(), "str");
    }

    #[test]
    fn value_display_integer() {
        assert_eq!(Value::Num(42.0).to_display_string(), "42");
        assert_eq!(Value::Num(-7.0).to_display_string(), "-7");
    }

    #[test]
    fn value_display_float() {
        assert_eq!(Value::Num(3.14).to_display_string(), "3.14");
    }

    #[test]
    fn value_display_bool() {
        assert_eq!(Value::Bool(true).to_display_string(), "true");
        assert_eq!(Value::Bool(false).to_display_string(), "false");
    }

    #[test]
    fn value_display_null() {
        assert_eq!(Value::Null.to_display_string(), "null");
    }

    #[test]
    fn value_display_string() {
        assert_eq!(
            Value::Str(Rc::new("hello".to_string())).to_display_string(),
            "hello"
        );
    }

    #[test]
    fn value_display_array() {
        use std::cell::RefCell;
        let arr = Value::Array(Rc::new(RefCell::new(vec![
            Value::Num(1.0),
            Value::Num(2.0),
            Value::Num(3.0),
        ])));
        assert_eq!(arr.to_display_string(), "[1, 2, 3]");
    }

    #[test]
    fn value_equality() {
        assert_eq!(Value::Num(42.0), Value::Num(42.0));
        assert_ne!(Value::Num(42.0), Value::Num(43.0));
        assert_eq!(Value::Bool(true), Value::Bool(true));
        assert_ne!(Value::Bool(true), Value::Bool(false));
        assert_eq!(Value::Null, Value::Null);
        assert_eq!(
            Value::Str(Rc::new("a".to_string())),
            Value::Str(Rc::new("a".to_string()))
        );
        assert_ne!(
            Value::Str(Rc::new("a".to_string())),
            Value::Str(Rc::new("b".to_string()))
        );
    }

    #[test]
    fn value_cross_type_inequality() {
        assert_ne!(Value::Num(1.0), Value::Bool(true));
        assert_ne!(Value::Num(0.0), Value::Null);
        assert_ne!(Value::Bool(false), Value::Null);
    }

    #[test]
    fn integration_var_declaration_with_value() {
        run("var x = 42;").unwrap();
    }

    #[test]
    fn integration_var_declaration_uninitialized() {
        run("var x;").unwrap();
    }

    #[test]
    fn integration_const_declaration() {
        run("const PI = 3.14;").unwrap();
    }
    #[test]
    fn integration_addition() {
        run("var x = 10 + 20;").unwrap();
    }

    #[test]
    fn integration_subtraction() {
        run("var x = 30 - 10;").unwrap();
    }

    #[test]
    fn integration_multiplication() {
        run("var x = 6 * 7;").unwrap();
    }

    #[test]
    fn integration_division() {
        run("var x = 84 / 2;").unwrap();
    }

    #[test]
    fn integration_modulo() {
        run("var x = 10 % 3;").unwrap();
    }

    #[test]
    fn integration_power() {
        run("var x = 2 ** 10;").unwrap();
    }

    #[test]
    fn integration_unary_negation() {
        run("var x = -42;").unwrap();
    }

    #[test]
    fn integration_unary_not() {
        run("var x = !true;").unwrap();
    }

    #[test]
    fn integration_complex_expression() {
        run("var x = (2 + 3) * 4 - 1;").unwrap();
    }

    #[test]
    fn integration_nested_parentheses() {
        run("var x = ((1 + 2) * (3 + 4));").unwrap();
    }
    #[test]
    fn integration_equality() {
        run("var x = 1 == 1;").unwrap();
    }

    #[test]
    fn integration_inequality() {
        run("var x = 1 != 2;").unwrap();
    }

    #[test]
    fn integration_less_than() {
        run("var x = 1 < 2;").unwrap();
    }

    #[test]
    fn integration_greater_than() {
        run("var x = 2 > 1;").unwrap();
    }

    #[test]
    fn integration_less_equal() {
        run("var x = 1 <= 1;").unwrap();
    }

    #[test]
    fn integration_greater_equal() {
        run("var x = 2 >= 2;").unwrap();
    }
    #[test]
    fn integration_logical_and() {
        run("var x = true && true;").unwrap();
    }

    #[test]
    fn integration_logical_or() {
        run("var x = false || true;").unwrap();
    }

    #[test]
    fn integration_logical_short_circuit_and() {
        run("var x = false && (1 / 0 > 0);").unwrap();
    }

    #[test]
    fn integration_logical_short_circuit_or() {
        run("var x = true || (1 / 0 > 0);").unwrap();
    }
    #[test]
    fn integration_bitwise_and() {
        run("var x = 0xFF & 0x0F;").unwrap();
    }

    #[test]
    fn integration_bitwise_or() {
        run("var x = 0xF0 | 0x0F;").unwrap();
    }

    #[test]
    fn integration_bitwise_xor() {
        run("var x = 0xFF ^ 0x0F;").unwrap();
    }

    #[test]
    fn integration_bitwise_not() {
        run("var x = ~0;").unwrap();
    }

    #[test]
    fn integration_shift_left() {
        run("var x = 1 << 3;").unwrap();
    }

    #[test]
    fn integration_shift_right() {
        run("var x = 8 >> 2;").unwrap();
    }
    #[test]
    fn integration_assignment() {
        run("var x = 1; x = 2;").unwrap();
    }

    #[test]
    fn integration_compound_add() {
        run("var x = 10; x += 5;").unwrap();
    }

    #[test]
    fn integration_compound_sub() {
        run("var x = 10; x -= 3;").unwrap();
    }

    #[test]
    fn integration_compound_mul() {
        run("var x = 5; x *= 4;").unwrap();
    }

    #[test]
    fn integration_compound_div() {
        run("var x = 20; x /= 4;").unwrap();
    }

    #[test]
    fn integration_compound_mod() {
        run("var x = 10; x %= 3;").unwrap();
    }

    #[test]
    fn integration_compound_pow() {
        run("var x = 2; x **= 3;").unwrap();
    }

    #[test]
    fn integration_compound_bitwise_and() {
        run("var x = 0xFF; x &= 0x0F;").unwrap();
    }

    #[test]
    fn integration_compound_bitwise_or() {
        run("var x = 0xF0; x |= 0x0F;").unwrap();
    }

    #[test]
    fn integration_compound_shl() {
        run("var x = 1; x <<= 3;").unwrap();
    }

    #[test]
    fn integration_compound_shr() {
        run("var x = 8; x >>= 2;").unwrap();
    }
    #[test]
    fn integration_string_literal() {
        run(r#"var s = "hello";"#).unwrap();
    }

    #[test]
    fn integration_string_single_quotes() {
        run("var s = 'world';").unwrap();
    }

    #[test]
    fn integration_string_concatenation() {
        run(r#"var s = "hello" + " " + "world";"#).unwrap();
    }

    #[test]
    fn integration_empty_string() {
        run(r#"var s = "";"#).unwrap();
    }

    #[test]
    fn integration_string_escape_newline() {
        run(r#"var s = "line1\nline2";"#).unwrap();
    }

    #[test]
    fn integration_string_escape_tab() {
        run(r#"var s = "col1\tcol2";"#).unwrap();
    }
    #[test]
    fn integration_interp_single_variable() {
        run(r#"var name = "world"; var s = "hello \(name)!";"#).unwrap();
    }

    #[test]
    fn integration_interp_number() {
        run(r#"var n = 42; var s = "value: \(n)";"#).unwrap();
    }

    #[test]
    fn integration_interp_bool() {
        run(r#"var b = true; var s = "flag: \(b)";"#).unwrap();
    }

    #[test]
    fn integration_interp_null() {
        run(r#"var s = "nothing: \(null)";"#).unwrap();
    }

    #[test]
    fn integration_interp_expression() {
        run(r#"var s = "sum: \(1 + 2 + 3)";"#).unwrap();
    }

    #[test]
    fn integration_interp_complex_expression() {
        run(r#"
            fun double(x) { return x * 2; }
            var s = "result: \(double(21))";
        "#)
        .unwrap();
    }

    #[test]
    fn integration_interp_multiple() {
        run(r#"
            var a = "hello";
            var b = "world";
            var s = "\(a) \(b)!";
        "#)
        .unwrap();
    }

    #[test]
    fn integration_interp_adjacent() {
        run(r#"
            var x = "foo";
            var y = "bar";
            var s = "\(x)\(y)";
        "#)
        .unwrap();
    }

    #[test]
    fn integration_interp_at_start() {
        run(r#"var x = 42; var s = "\(x) is the answer";"#).unwrap();
    }

    #[test]
    fn integration_interp_at_end() {
        run(r#"var x = 42; var s = "the answer is \(x)";"#).unwrap();
    }

    #[test]
    fn integration_interp_only_expression() {
        run(r#"var x = "test"; var s = "\(x)";"#).unwrap();
    }

    #[test]
    fn integration_interp_empty_string_value() {
        run(r#"var e = ""; var s = "before\(e)after";"#).unwrap();
    }

    #[test]
    fn integration_interp_array() {
        run(r#"var arr = [1, 2, 3]; var s = "arr: \(arr)";"#).unwrap();
    }

    #[test]
    fn integration_interp_three_parts() {
        run(r#"
            var a = 1;
            var b = 2;
            var c = 3;
            var s = "\(a) + \(b) = \(c)";
        "#)
        .unwrap();
    }

    #[test]
    fn integration_interp_nested_call() {
        run(r#"
            fun greet(name) { return "hi " + name; }
            var s = "message: \(greet("user"))";
        "#)
        .unwrap();
    }

    #[test]
    fn integration_interp_with_io_printn() {
        run(r#"
            var name = "cirq";
            var ver = 1;
            io.printn("welcome to \(name) v\(ver)!");
        "#)
        .unwrap();
    }
    #[test]
    fn integration_array_literal() {
        run("var arr = [1, 2, 3];").unwrap();
    }

    #[test]
    fn integration_array_empty() {
        run("var arr = [];").unwrap();
    }

    #[test]
    fn integration_array_mixed_types() {
        run(r#"var arr = [1, "two", true, null];"#).unwrap();
    }

    #[test]
    fn integration_array_index_read() {
        run("var arr = [10, 20, 30]; var x = arr[1];").unwrap();
    }

    #[test]
    fn integration_array_index_write() {
        run("var arr = [1, 2, 3]; arr[0] = 99;").unwrap();
    }

    #[test]
    fn integration_array_nested() {
        run("var arr = [[1, 2], [3, 4]]; var x = arr[0][1];").unwrap();
    }
    #[test]
    fn integration_if_true() {
        run("var x = 0; if (true) { x = 1; }").unwrap();
    }

    #[test]
    fn integration_if_false() {
        run("var x = 0; if (false) { x = 1; }").unwrap();
    }

    #[test]
    fn integration_if_else() {
        run("var x = 0; if (false) { x = 1; } else { x = 2; }").unwrap();
    }

    #[test]
    fn integration_if_else_chain() {
        run(r#"
            var x = 0;
            if (false) {
                x = 1;
            } else if (false) {
                x = 2;
            } else {
                x = 3;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_if_numeric_condition() {
        run("var x = 0; if (1) { x = 1; }").unwrap();
    }

    #[test]
    fn integration_if_null_condition() {
        run("var x = 0; if (null) { x = 1; }").unwrap();
    }
    #[test]
    fn integration_while_loop() {
        run(r#"
            var i = 0;
            while (i < 5) {
                i = i + 1;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_while_false() {
        run("var x = 0; while (false) { x = 1; }").unwrap();
    }

    #[test]
    fn integration_while_break() {
        run(r#"
            var i = 0;
            while (true) {
                if (i >= 3) { break; }
                i = i + 1;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_while_continue() {
        run(r#"
            var i = 0;
            var sum = 0;
            while (i < 10) {
                i = i + 1;
                if (i % 2 == 0) { continue; }
                sum = sum + i;
            }
        "#)
        .unwrap();
    }
    #[test]
    fn integration_for_loop() {
        run(r#"
            var sum = 0;
            for (var i = 0; i < 5; i = i + 1) {
                sum = sum + i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_for_loop_break() {
        run(r#"
            var sum = 0;
            for (var i = 0; i < 100; i = i + 1) {
                if (i >= 5) { break; }
                sum = sum + i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_for_loop_continue() {
        run(r#"
            var sum = 0;
            for (var i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) { continue; }
                sum = sum + i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_for_no_init() {
        run(r#"
            var i = 0;
            for (; i < 3; i = i + 1) {}
        "#)
        .unwrap();
    }

    #[test]
    fn integration_for_no_update() {
        run(r#"
            for (var i = 0; i < 3;) {
                i = i + 1;
            }
        "#)
        .unwrap();
    }
    #[test]
    fn integration_function_declaration() {
        run(r#"
            fun greet() {
                var x = 42;
            }
            greet();
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_with_params() {
        run(r#"
            fun add(a, b) {
                return a + b;
            }
            var result = add(3, 4);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_return_value() {
        run(r#"
            fun double(x) {
                return x * 2;
            }
            var result = double(21);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_implicit_null_return() {
        run(r#"
            fun noop() {}
            var result = noop();
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_recursion() {
        run(r#"
            fun factorial(n) {
                if (n <= 1) { return 1; }
                return n * factorial(n - 1);
            }
            var result = factorial(5);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_closure_via_globals() {
        run(r#"
            var counter = 0;
            fun increment() {
                counter = counter + 1;
            }
            increment();
            increment();
            increment();
        "#)
        .unwrap();
    }

    #[test]
    fn integration_function_multiple_calls() {
        run(r#"
            fun square(x) { return x * x; }
            var a = square(3);
            var b = square(4);
            var c = square(5);
        "#)
        .unwrap();
    }
    #[test]
    fn integration_module_declaration() {
        run(r#"
            mod math {
                fun add(a, b) {
                    return a + b;
                }
            }
            var result = math.add(3, 4);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_module_multiple_functions() {
        run(r#"
            mod calc {
                fun add(a, b) { return a + b; }
                fun mul(a, b) { return a * b; }
            }
            var sum = calc.add(2, 3);
            var prod = calc.mul(4, 5);
        "#)
        .unwrap();
    }
    #[test]
    fn integration_io_printn() {
        run(r#"io.printn("hello from test");"#).unwrap();
    }

    #[test]
    fn integration_io_print() {
        run(r#"io.print("no newline");"#).unwrap();
    }

    #[test]
    fn integration_io_eprintn() {
        run(r#"io.eprintn("stderr test");"#).unwrap();
    }

    #[test]
    fn integration_io_eprint() {
        run(r#"io.eprint("stderr no newline");"#).unwrap();
    }

    #[test]
    fn integration_io_printn_number() {
        run("io.printn(42);").unwrap();
    }

    #[test]
    fn integration_io_printn_bool() {
        run("io.printn(true);").unwrap();
    }

    #[test]
    fn integration_io_printn_null() {
        run("io.printn(null);").unwrap();
    }

    #[test]
    fn integration_io_printn_array() {
        run("io.printn([1, 2, 3]);").unwrap();
    }
    #[test]
    fn integration_prefix_increment() {
        run("var x = 5; ++x;").unwrap();
    }

    #[test]
    fn integration_prefix_decrement() {
        run("var x = 5; --x;").unwrap();
    }

    #[test]
    fn integration_postfix_increment() {
        run("var x = 5; x++;").unwrap();
    }

    #[test]
    fn integration_postfix_decrement() {
        run("var x = 5; x--;").unwrap();
    }
    #[test]
    fn integration_block_scoping() {
        run(r#"
            var x = 1;
            {
                var y = 2;
                x = x + y;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_nested_blocks() {
        run(r#"
            var x = 0;
            {
                var a = 1;
                {
                    var b = 2;
                    x = a + b;
                }
            }
        "#)
        .unwrap();
    }
    #[test]
    fn integration_hex_number() {
        run("var x = 0xFF;").unwrap();
    }

    #[test]
    fn integration_binary_number() {
        run("var x = 0b1010;").unwrap();
    }

    #[test]
    fn integration_octal_number() {
        run("var x = 0o77;").unwrap();
    }

    #[test]
    fn integration_float_number() {
        run("var x = 3.14159;").unwrap();
    }

    #[test]
    fn integration_negative_number() {
        run("var x = -42;").unwrap();
    }

    #[test]
    fn integration_zero() {
        run("var x = 0;").unwrap();
    }
    #[test]
    fn integration_fibonacci() {
        run(r#"
            fun fib(n) {
                if (n <= 1) { return n; }
                return fib(n - 1) + fib(n - 2);
            }
            var result = fib(10);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_bubble_sort() {
        run(r#"
            var arr = [5, 3, 8, 1, 2];
            var n = 5;
            for (var i = 0; i < n; i = i + 1) {
                for (var j = 0; j < n - 1 - i; j = j + 1) {
                    if (arr[j] > arr[j + 1]) {
                        var temp = arr[j];
                        arr[j] = arr[j + 1];
                        arr[j + 1] = temp;
                    }
                }
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_nested_function_calls() {
        run(r#"
            fun square(x) { return x * x; }
            fun sum_of_squares(a, b) {
                return square(a) + square(b);
            }
            var result = sum_of_squares(3, 4);
        "#)
        .unwrap();
    }

    #[test]
    fn integration_counter_pattern() {
        run(r#"
            var count = 0;
            fun tick() {
                count = count + 1;
                return count;
            }
            var a = tick();
            var b = tick();
            var c = tick();
        "#)
        .unwrap();
    }

    #[test]
    fn integration_accumulator_loop() {
        run(r#"
            var total = 0;
            for (var i = 1; i <= 100; i = i + 1) {
                total = total + i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_array_manipulation() {
        run(r#"
            var arr = [0, 0, 0, 0, 0];
            for (var i = 0; i < 5; i = i + 1) {
                arr[i] = i * i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn integration_multiple_returns() {
        run(r#"
            fun abs(x) {
                if (x < 0) { return -x; }
                return x;
            }
            var a = abs(-5);
            var b = abs(3);
        "#)
        .unwrap();
    }
    #[test]
    fn error_undefined_variable() {
        expect_error("io.printn(xyz);", ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_add() {
        expect_error("var x = 1 + true;", ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_subtract() {
        expect_error(r#"var x = "hello" - 1;"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_multiply() {
        expect_error(r#"var x = "hello" * 2;"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_compare() {
        expect_error(r#"var x = "hello" < 2;"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_negate() {
        expect_error(r#"var x = -"hello";"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_bitwise() {
        expect_error(r#"var x = "hello" & 1;"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_bitwise_not() {
        expect_error(r#"var x = ~"hello";"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_type_mismatch_increment() {
        expect_error(r#"var x = "hello"; ++x;"#, ErrorKind::Runtime);
    }

    #[test]
    fn error_call_non_function() {
        expect_error("var x = 42; x();", ErrorKind::Runtime);
    }

    #[test]
    fn error_wrong_argument_count() {
        expect_error(
            r#"
            fun foo(a) { return a; }
            foo(1, 2);
        "#,
            ErrorKind::Runtime,
        );
    }

    #[test]
    fn error_index_out_of_bounds() {
        expect_error("var arr = [1, 2, 3]; var x = arr[10];", ErrorKind::Runtime);
    }

    #[test]
    fn error_index_non_array() {
        expect_error("var x = 42; var y = x[0];", ErrorKind::Runtime);
    }

    #[test]
    fn error_member_access_non_module() {
        expect_error("var x = 42; x.foo;", ErrorKind::Runtime);
    }

    #[test]
    fn error_undefined_module_member() {
        expect_error("io.nonexistent();", ErrorKind::Runtime);
    }

    #[test]
    fn error_missing_semicolon() {
        expect_error("var x = 1", ErrorKind::Parser);
    }

    #[test]
    fn error_unexpected_token() {
        expect_error("var = 1;", ErrorKind::Parser);
    }

    #[test]
    fn error_unterminated_string() {
        expect_error(r#"var s = "hello"#, ErrorKind::Lexer);
    }

    #[test]
    fn error_break_outside_loop() {
        expect_error("break;", ErrorKind::Compiler);
    }

    #[test]
    fn error_continue_outside_loop() {
        expect_error("continue;", ErrorKind::Compiler);
    }

    #[test]
    fn error_const_reassignment() {
        expect_error(
            r#"
            fun test() {
                const x = 1;
                x = 2;
            }
            test();
        "#,
            ErrorKind::Compiler,
        );
    }

    #[test]
    fn error_missing_paren_if() {
        expect_error("if true { }", ErrorKind::Parser);
    }

    #[test]
    fn error_missing_paren_while() {
        expect_error("while true { }", ErrorKind::Parser);
    }

    #[test]
    fn error_missing_paren_for() {
        expect_error("for var i = 0; i < 5; i = i + 1 { }", ErrorKind::Parser);
    }

    #[test]
    fn error_missing_brace_function() {
        expect_error("fun foo() return 1;", ErrorKind::Parser);
    }

    #[test]
    fn error_builtin_wrong_arity() {
        expect_error("io.printn();", ErrorKind::Runtime);
    }
    #[test]
    fn edge_empty_program() {
        run("").unwrap();
    }

    #[test]
    fn edge_comment_only_program() {
        run("// This is just a comment").unwrap();
    }

    #[test]
    fn edge_bare_semicolons_error() {
        expect_error(";;;", ErrorKind::Parser);
    }

    #[test]
    fn edge_deeply_nested_blocks() {
        run(r#"
            var x = 0;
            {{{{{ x = 1; }}}}}
        "#)
        .unwrap();
    }

    #[test]
    fn edge_many_variables() {
        run(r#"
            var a = 1; var b = 2; var c = 3;
            var d = 4; var e = 5; var f = 6;
            var g = 7; var h = 8; var i = 9;
        "#)
        .unwrap();
    }

    #[test]
    fn edge_chained_comparisons_via_logical() {
        run("var x = 1 < 2 && 2 < 3;").unwrap();
    }

    #[test]
    fn edge_operator_precedence() {
        run("var x = 2 + 3 * 4;").unwrap();
    }

    #[test]
    fn edge_right_associative_power() {
        run("var x = 2 ** 3 ** 2;").unwrap();
    }

    #[test]
    fn edge_division_by_zero() {
        run("var x = 1 / 0;").unwrap();
    }

    #[test]
    fn edge_modulo_zero() {
        run("var x = 1 % 0;").unwrap();
    }

    #[test]
    fn edge_function_as_value() {
        run(r#"
            fun foo() { return 42; }
            var f = foo;
            var result = f();
        "#)
        .unwrap();
    }

    #[test]
    fn edge_array_single_element() {
        run("var arr = [42];").unwrap();
    }

    #[test]
    fn edge_nested_loops() {
        run(r#"
            var sum = 0;
            for (var i = 0; i < 3; i = i + 1) {
                for (var j = 0; j < 3; j = j + 1) {
                    sum = sum + 1;
                }
            }
        "#)
        .unwrap();
    }

    #[test]
    fn edge_break_in_nested_loop() {
        run(r#"
            for (var i = 0; i < 10; i = i + 1) {
                for (var j = 0; j < 10; j = j + 1) {
                    if (j >= 3) { break; }
                }
            }
        "#)
        .unwrap();
    }

    #[test]
    fn edge_return_in_if() {
        run(r#"
            fun test(x) {
                if (x > 0) {
                    return x;
                }
                return 0;
            }
            var a = test(5);
            var b = test(-1);
        "#)
        .unwrap();
    }

    #[test]
    fn edge_var_shadowing_in_block() {
        run(r#"
            var x = 1;
            {
                var x = 2;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn edge_recursive_fibonacci_deep() {
        run(r#"
            fun fib(n) {
                if (n <= 1) { return n; }
                return fib(n - 1) + fib(n - 2);
            }
            var result = fib(15);
        "#)
        .unwrap();
    }

    #[test]
    fn edge_string_with_special_chars() {
        run(r#"var s = "hello\nworld\t!";"#).unwrap();
    }

    #[test]
    fn edge_null_operations() {
        run("var x = null; var y = !x;").unwrap();
    }

    #[test]
    fn edge_boolean_equality() {
        run("var x = true == true;").unwrap();
    }

    #[test]
    fn edge_null_equality() {
        run("var x = null == null;").unwrap();
    }

    #[test]
    fn edge_string_equality() {
        run(r#"var x = "abc" == "abc";"#).unwrap();
    }

    #[test]
    fn edge_cross_type_equality() {
        run("var x = 1 == true;").unwrap();
    }

    #[test]
    fn edge_for_infinite_break() {
        run(r#"
            var x = 0;
            for (;;) {
                x = x + 1;
                if (x >= 5) { break; }
            }
        "#)
        .unwrap();
    }

    #[test]
    fn edge_complex_module_usage() {
        run(r#"
            mod utils {
                fun max(a, b) {
                    if (a > b) { return a; }
                    return b;
                }
                fun min(a, b) {
                    if (a < b) { return a; }
                    return b;
                }
            }
            var big = utils.max(10, 20);
            var small = utils.min(10, 20);
        "#)
        .unwrap();
    }

    #[test]
    fn edge_mutual_recursion() {
        run(r#"
            fun is_even(n) {
                if (n == 0) { return true; }
                return is_odd(n - 1);
            }
            fun is_odd(n) {
                if (n == 0) { return false; }
                return is_even(n - 1);
            }
            var result = is_even(10);
        "#)
        .unwrap();
    }

    #[test]
    fn edge_expression_statement() {
        run("1 + 2;").unwrap();
    }

    #[test]
    fn edge_multiple_returns_in_function() {
        run(r#"
            fun classify(n) {
                if (n > 0) { return 1; }
                if (n < 0) { return -1; }
                return 0;
            }
            var a = classify(5);
            var b = classify(-3);
            var c = classify(0);
        "#)
        .unwrap();
    }
    #[test]
    fn builtin_io_module_name() {
        let io = IoModule;
        assert_eq!(io.name(), "io");
    }

    #[test]
    fn builtin_io_module_function_count() {
        let io = IoModule;
        let fns = io.functions();
        assert_eq!(fns.len(), 9);
    }

    #[test]
    fn builtin_io_module_build() {
        let io = IoModule;
        let module = io.build();
        assert_eq!(module.name, "io");
        assert_eq!(module.members.len(), 9);
        assert!(module.get_member("print").is_some());
        assert!(module.get_member("printn").is_some());
        assert!(module.get_member("eprint").is_some());
        assert!(module.get_member("eprintn").is_some());
        assert!(module.get_member("input").is_some());
        assert!(module.get_member("read").is_some());
        assert!(module.get_member("readline").is_some());
        assert!(module.get_member("readlines").is_some());
        assert!(module.get_member("write").is_some());
    }

    #[test]
    fn builtin_io_module_nonexistent_member() {
        let io = IoModule;
        let module = io.build();
        assert!(module.get_member("nonexistent").is_none());
    }
    #[test]
    fn parser_error_const_without_initializer() {
        expect_error("const x;", ErrorKind::Parser);
    }

    #[test]
    fn parser_error_fun_missing_name() {
        expect_error("fun () {}", ErrorKind::Parser);
    }

    #[test]
    fn parser_error_mod_missing_name() {
        expect_error("mod {}", ErrorKind::Parser);
    }

    #[test]
    fn parser_error_unmatched_paren() {
        expect_error("var x = (1 + 2;", ErrorKind::Parser);
    }

    #[test]
    fn parser_error_unmatched_bracket() {
        expect_error("var x = [1, 2;", ErrorKind::Parser);
    }

    #[test]
    fn parser_error_empty_parens_in_expr() {
        expect_error("var x = ();", ErrorKind::Parser);
    }
    #[test]
    fn integration_io_write_and_read() {
        let result = run(r#"
            io.write("/tmp/cirq_test.txt", "hello cirq");
            var content = io.read("/tmp/cirq_test.txt");
            io.printn(content);
        "#);
        assert!(result.is_ok());
        let _ = std::fs::remove_file("/tmp/cirq_test.txt");
    }

    #[test]
    fn integration_io_read_nonexistent_file() {
        expect_error(
            r#"var content = io.read("/tmp/nonexistent_cirq_file.txt");"#,
            ErrorKind::Runtime,
        );
    }
    #[test]
    fn stress_deep_recursion() {
        run(r#"
            fun count(n) {
                if (n <= 0) { return 0; }
                return count(n - 1);
            }
            var result = count(100);
        "#)
        .unwrap();
    }

    #[test]
    fn stress_many_local_variables() {
        run(r#"
            fun test() {
                var a = 1; var b = 2; var c = 3; var d = 4;
                var e = 5; var f = 6; var g = 7; var h = 8;
                var i = 9; var j = 10; var k = 11; var l = 12;
                return a + b + c + d + e + f + g + h + i + j + k + l;
            }
            var result = test();
        "#)
        .unwrap();
    }

    #[test]
    fn stress_many_iterations() {
        run(r#"
            var sum = 0;
            for (var i = 0; i < 10000; i = i + 1) {
                sum = sum + i;
            }
        "#)
        .unwrap();
    }

    #[test]
    fn stress_array_large() {
        run(r#"
            var arr = [];
            var arr = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            for (var i = 0; i < 10; i = i + 1) {
                arr[i] = i * i;
            }
        "#)
        .unwrap();
    }
    #[test]
    fn class_empty_class() {
        run(r#"
            class Empty {}
            var e = Empty();
        "#)
        .unwrap();
    }

    #[test]
    fn class_init_fields() {
        let result = run(r#"
            class Point {
                init(x, y) {
                    self.x = x;
                    self.y = y;
                }
            }
            var p = Point(3, 4);
            var __result = p.x + p.y;
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(7.0));
    }

    #[test]
    fn class_instance_method() {
        let result = run(r#"
            class Rect {
                init(w, h) {
                    self.w = w;
                    self.h = h;
                }
                area() {
                    return self.w * self.h;
                }
            }
            var r = Rect(5, 3);
            var __result = r.area();
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(15.0));
    }

    #[test]
    fn class_method_with_params() {
        let result = run(r#"
            class Calc {
                init(base) {
                    self.base = base;
                }
                add(n) {
                    return self.base + n;
                }
            }
            var c = Calc(10);
            var __result = c.add(25);
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(35.0));
    }

    #[test]
    fn class_multiple_instances() {
        let result = run(r#"
            class Counter {
                init(start) {
                    self.val = start;
                }
                get() {
                    return self.val;
                }
            }
            var a = Counter(10);
            var b = Counter(20);
            var __result = a.get() + b.get();
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(30.0));
    }

    #[test]
    fn class_field_mutation() {
        let result = run(r#"
            class Dog {
                init(name) {
                    self.name = name;
                    self.tricks = 0;
                }
                learn() {
                    self.tricks = self.tricks + 1;
                }
                report() {
                    return self.tricks;
                }
            }
            var d = Dog("Rex");
            d.learn();
            d.learn();
            d.learn();
            var __result = d.report();
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(3.0));
    }

    #[test]
    fn class_no_init() {
        let result = run(r#"
            class Greeter {
                greet() {
                    return 42;
                }
            }
            var g = Greeter();
            var __result = g.greet();
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(42.0));
    }

    #[test]
    fn class_decorative_self_param() {
        let result = run(r#"
            class Foo {
                init(self, x) {
                    self.x = x;
                }
                get(self) {
                    return self.x;
                }
            }
            var f = Foo(99);
            var __result = f.get();
        "#)
        .unwrap();
        assert_eq!(result, Value::Num(99.0));
    }

    #[test]
    fn class_init_wrong_arity() {
        expect_error(
            r#"
            class Pair {
                init(a, b) {
                    self.a = a;
                    self.b = b;
                }
            }
            var p = Pair(1);
        "#,
            ErrorKind::Runtime,
        );
    }

    #[test]
    fn class_no_such_member() {
        expect_error(
            r#"
            class Box {
                init(val) {
                    self.val = val;
                }
            }
            var b = Box(5);
            var x = b.missing;
        "#,
            ErrorKind::Runtime,
        );
    }

    #[test]
    fn math_pi_constant() {
        let result = run("var __result = math.PI;").unwrap();
        assert_eq!(result, Value::Num(std::f64::consts::PI));
    }

    #[test]
    fn math_e_constant() {
        let result = run("var __result = math.E;").unwrap();
        assert_eq!(result, Value::Num(std::f64::consts::E));
    }
    #[test]
    fn math_sin_zero() {
        let result = run("var __result = math.sin(0);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_cos_zero() {
        let result = run("var __result = math.cos(0);").unwrap();
        assert_eq!(result, Value::Num(1.0));
    }

    #[test]
    fn math_tan_zero() {
        let result = run("var __result = math.tan(0);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_asin_zero() {
        let result = run("var __result = math.asin(0);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_acos_one() {
        let result = run("var __result = math.acos(1);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_atan_zero() {
        let result = run("var __result = math.atan(0);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_atan2_basic() {
        let result = run("var __result = math.atan2(1, 1);").unwrap();
        if let Value::Num(n) = result {
            assert!((n - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
        } else {
            panic!("expected num");
        }
    }
    #[test]
    fn math_sqrt_basic() {
        let result = run("var __result = math.sqrt(9);").unwrap();
        assert_eq!(result, Value::Num(3.0));
    }

    #[test]
    fn math_cbrt_basic() {
        let result = run("var __result = math.cbrt(27);").unwrap();
        assert_eq!(result, Value::Num(3.0));
    }

    #[test]
    fn math_pow_basic() {
        let result = run("var __result = math.pow(2, 10);").unwrap();
        assert_eq!(result, Value::Num(1024.0));
    }
    #[test]
    fn math_log_e() {
        let result = run("var __result = math.log(math.E);").unwrap();
        if let Value::Num(n) = result {
            assert!((n - 1.0).abs() < 1e-10);
        } else {
            panic!("expected num");
        }
    }

    #[test]
    fn math_log2_basic() {
        let result = run("var __result = math.log2(8);").unwrap();
        assert_eq!(result, Value::Num(3.0));
    }

    #[test]
    fn math_log10_basic() {
        let result = run("var __result = math.log10(1000);").unwrap();
        if let Value::Num(n) = result {
            assert!((n - 3.0).abs() < 1e-10);
        } else {
            panic!("expected num");
        }
    }

    #[test]
    fn math_exp_zero() {
        let result = run("var __result = math.exp(0);").unwrap();
        assert_eq!(result, Value::Num(1.0));
    }

    #[test]
    fn math_exp_one() {
        let result = run("var __result = math.exp(1);").unwrap();
        if let Value::Num(n) = result {
            assert!((n - std::f64::consts::E).abs() < 1e-10);
        } else {
            panic!("expected num");
        }
    }
    #[test]
    fn math_min_basic() {
        let result = run("var __result = math.min(3, 7);").unwrap();
        assert_eq!(result, Value::Num(3.0));
    }

    #[test]
    fn math_max_basic() {
        let result = run("var __result = math.max(3, 7);").unwrap();
        assert_eq!(result, Value::Num(7.0));
    }

    #[test]
    fn math_clamp_within() {
        let result = run("var __result = math.clamp(5, 0, 10);").unwrap();
        assert_eq!(result, Value::Num(5.0));
    }

    #[test]
    fn math_clamp_below() {
        let result = run("var __result = math.clamp(-5, 0, 10);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }

    #[test]
    fn math_clamp_above() {
        let result = run("var __result = math.clamp(15, 0, 10);").unwrap();
        assert_eq!(result, Value::Num(10.0));
    }
    #[test]
    fn math_sign_positive() {
        let result = run("var __result = math.sign(42);").unwrap();
        assert_eq!(result, Value::Num(1.0));
    }

    #[test]
    fn math_sign_negative() {
        let result = run("var __result = math.sign(-7);").unwrap();
        assert_eq!(result, Value::Num(-1.0));
    }

    #[test]
    fn math_sign_zero() {
        let result = run("var __result = math.sign(0);").unwrap();
        assert_eq!(result, Value::Num(0.0));
    }
    #[test]
    fn math_random_in_range() {
        let result = run("var __result = math.random();").unwrap();
        if let Value::Num(n) = result {
            assert!(
                n >= 0.0 && n < 1.0,
                "random() returned {} (out of [0,1))",
                n
            );
        } else {
            panic!("expected num");
        }
    }
    #[test]
    fn math_sin_type_error() {
        expect_error(
            r#"var x = math.sin("hello");
"#,
            ErrorKind::Runtime,
        );
    }

    #[test]
    fn math_min_type_error() {
        expect_error(
            r#"var x = math.min(1, "two");
"#,
            ErrorKind::Runtime,
        );
    }
    #[test]
    fn math_circle_area() {
        let result = run("var __result = math.PI * 5 ** 2;").unwrap();
        if let Value::Num(n) = result {
            assert!((n - std::f64::consts::PI * 25.0).abs() < 1e-10);
        } else {
            panic!("expected num");
        }
    }
}
