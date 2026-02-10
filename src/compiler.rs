//! # Compiler Module
//!
//! Walks the AST and emits register-based bytecode instructions.
//! Uses a simple linear register allocator: registers are allocated
//! sequentially and reused after expressions complete.
//!
//! ## Key Design
//! - Single-pass compilation from AST to `CompiledFunction`.
//! - Constants are deduplicated in the constant pool.
//! - Scoping via a scope stack tracking local variable slots.
//! - Functions compile to separate `CompiledFunction` instances.

use crate::ast::*;
use crate::error::{CirqError, CirqResult, Span};
use crate::opcode::{CompiledFunction, Instruction};
use crate::value::Value;

use std::rc::Rc;

// -----------------------------------------------------------------------------
// LOCAL VARIABLE
// -----------------------------------------------------------------------------

/// A local variable tracked during compilation.
#[derive(Debug, Clone)]
struct Local {
    /// Variable name.
    name: String,
    /// Register slot assigned to this local.
    slot: u8,
    /// Scope depth where this local was declared.
    depth: u32,
    /// Whether this is a constant binding.
    is_const: bool,
}

// -----------------------------------------------------------------------------
// LOOP CONTEXT
// -----------------------------------------------------------------------------

/// Tracks loop state for break/continue compilation.
#[derive(Debug, Clone)]
struct LoopCtx {
    /// Instruction index where the loop condition starts.
    #[allow(dead_code)]
    start: usize,
    /// Placeholder indices for break jumps to patch after the loop.
    break_jumps: Vec<usize>,
    /// Placeholder indices for continue jumps to patch.
    /// For `while` loops these are patched to `start` (re-check condition).
    /// For `for` loops these are patched to the update expression.
    continue_jumps: Vec<usize>,
}

// -----------------------------------------------------------------------------
// COMPILER STATE
// -----------------------------------------------------------------------------

/// The Cirq bytecode compiler. Transforms an AST into register-based
/// bytecode ready for VM execution.
pub struct Compiler {
    /// Instructions being emitted.
    instructions: Vec<Instruction>,
    /// Constant pool (deduplicated).
    constants: Vec<Value>,
    /// Global name pool for GetGlobal/SetGlobal.
    names: Vec<String>,
    /// Local variables in the current scope.
    locals: Vec<Local>,
    /// Current scope depth (0 = global).
    scope_depth: u32,
    /// Next available register index.
    next_reg: u8,
    /// Maximum register used (for frame sizing).
    max_reg: u8,
    /// Current function parameter count.
    param_count: u8,
    /// Stack of loop contexts for break/continue.
    loops: Vec<LoopCtx>,
}

impl Compiler {
    /// Creates a new compiler instance.
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            names: Vec::new(),
            locals: Vec::new(),
            scope_depth: 0,
            next_reg: 0,
            max_reg: 0,
            param_count: 0,
            loops: Vec::new(),
        }
    }

    /// Compiles a list of statements into a top-level `CompiledFunction`.
    ///
    /// # Errors
    /// Returns a `CirqError` if the AST contains constructs that cannot
    /// be compiled (e.g., break outside loop, too many locals).
    pub fn compile(mut self, stmts: &[Stmt]) -> CirqResult<CompiledFunction> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }

        // Implicit return null at end of script
        let r = self.alloc_reg();
        self.emit(Instruction::LoadNull { dst: r });
        self.emit(Instruction::Return { src: r });

        Ok(CompiledFunction {
            name: "<main>".to_string(),
            instructions: self.instructions,
            constants: self.constants,
            names: self.names,
            local_count: self.max_reg,
            param_count: 0,
        })
    }

    // -------------------------------------------------------------------------
    // STATEMENT COMPILATION
    // -------------------------------------------------------------------------

    /// Compiles a single statement.
    fn compile_stmt(&mut self, stmt: &Stmt) -> CirqResult<()> {
        match stmt {
            Stmt::VarDecl {
                name,
                initializer,
                span,
            } => self.compile_var_decl(name, initializer.as_ref(), *span, false),
            Stmt::ConstDecl { name, value, span } => {
                self.compile_var_decl(name, Some(value), *span, true)
            }
            Stmt::FunDecl {
                name,
                params,
                body,
                span,
            } => self.compile_fun_decl(name, params, body, *span),
            Stmt::ModDecl { name, body, span } => self.compile_mod_decl(name, body, *span),
            Stmt::ExprStmt { expr, .. } => {
                let r = self.compile_expr(expr)?;
                self.free_reg_to(r);
                Ok(())
            }
            Stmt::Block { stmts, .. } => {
                self.begin_scope();
                for s in stmts {
                    self.compile_stmt(s)?;
                }
                self.end_scope();
                Ok(())
            }
            Stmt::Return { value, .. } => {
                if let Some(expr) = value {
                    let r = self.compile_expr(expr)?;
                    self.emit(Instruction::Return { src: r });
                    self.free_reg_to(r);
                } else {
                    let r = self.alloc_reg();
                    self.emit(Instruction::LoadNull { dst: r });
                    self.emit(Instruction::Return { src: r });
                    self.free_reg(r);
                }
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => self.compile_if(condition, then_branch, else_branch.as_deref()),
            Stmt::While {
                condition, body, ..
            } => self.compile_while(condition, body),
            Stmt::For {
                init,
                condition,
                update,
                body,
                ..
            } => self.compile_for(init.as_deref(), condition.as_ref(), update.as_ref(), body),
            Stmt::Break { span } => self.compile_break(*span),
            Stmt::Continue { span } => self.compile_continue(*span),
            Stmt::ClassDecl {
                name,
                methods,
                span,
            } => self.compile_class_decl(name, methods, *span),
        }
    }

    /// Compiles a variable/constant declaration.
    fn compile_var_decl(
        &mut self,
        name: &str,
        init: Option<&Expr>,
        _span: Span,
        is_const: bool,
    ) -> CirqResult<()> {
        let reg = self.alloc_reg();

        if let Some(expr) = init {
            let src = self.compile_expr(expr)?;
            if src != reg {
                self.emit(Instruction::Move { dst: reg, src });
                self.free_reg_to(src);
            }
        } else {
            self.emit(Instruction::LoadNull { dst: reg });
        }

        if self.scope_depth == 0 {
            // Global variable
            let name_idx = self.add_name(name);
            self.emit(Instruction::SetGlobal { name_idx, src: reg });
            self.free_reg(reg);
        } else {
            // Local variable — register is the slot
            self.locals.push(Local {
                name: name.to_string(),
                slot: reg,
                depth: self.scope_depth,
                is_const,
            });
        }

        Ok(())
    }

    /// Compiles a function declaration.
    fn compile_fun_decl(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Stmt],
        _span: Span,
    ) -> CirqResult<()> {
        // Compile the function body in a fresh compiler
        let mut fun_compiler = Compiler::new();
        fun_compiler.param_count = params.len() as u8;
        fun_compiler.scope_depth = 1;

        // Allocate registers for parameters
        for param in params {
            let reg = fun_compiler.alloc_reg();
            fun_compiler.locals.push(Local {
                name: param.clone(),
                slot: reg,
                depth: 1,
                is_const: false,
            });
        }

        // Compile function body
        for stmt in body {
            fun_compiler.compile_stmt(stmt)?;
        }

        // Implicit return null
        let r = fun_compiler.alloc_reg();
        fun_compiler.emit(Instruction::LoadNull { dst: r });
        fun_compiler.emit(Instruction::Return { src: r });

        let compiled = CompiledFunction {
            name: name.to_string(),
            instructions: fun_compiler.instructions,
            constants: fun_compiler.constants,
            names: fun_compiler.names,
            local_count: fun_compiler.max_reg,
            param_count: params.len() as u8,
        };

        // Store the compiled function as a constant and assign to name
        let fun_val = Value::Fun(Rc::new(compiled));
        let const_idx = self.add_constant(fun_val);
        let dst = self.alloc_reg();
        self.emit(Instruction::LoadConst {
            dst,
            idx: const_idx,
        });

        if self.scope_depth == 0 {
            let name_idx = self.add_name(name);
            self.emit(Instruction::SetGlobal { name_idx, src: dst });
            self.free_reg(dst);
        } else {
            self.locals.push(Local {
                name: name.to_string(),
                slot: dst,
                depth: self.scope_depth,
                is_const: true,
            });
        }

        Ok(())
    }

    /// Compiles a module declaration.
    fn compile_mod_decl(&mut self, name: &str, body: &[Stmt], _span: Span) -> CirqResult<()> {
        // Compile all function declarations within the module
        let mut members: Vec<(String, Value)> = Vec::new();

        for stmt in body {
            if let Stmt::FunDecl {
                name: fname,
                params,
                body: fbody,
                ..
            } = stmt
            {
                let mut fun_compiler = Compiler::new();
                fun_compiler.param_count = params.len() as u8;
                fun_compiler.scope_depth = 1;

                for param in params {
                    let reg = fun_compiler.alloc_reg();
                    fun_compiler.locals.push(Local {
                        name: param.clone(),
                        slot: reg,
                        depth: 1,
                        is_const: false,
                    });
                }

                for s in fbody {
                    fun_compiler.compile_stmt(s)?;
                }

                let r = fun_compiler.alloc_reg();
                fun_compiler.emit(Instruction::LoadNull { dst: r });
                fun_compiler.emit(Instruction::Return { src: r });

                let compiled = CompiledFunction {
                    name: format!("{}.{}", name, fname),
                    instructions: fun_compiler.instructions,
                    constants: fun_compiler.constants,
                    names: fun_compiler.names,
                    local_count: fun_compiler.max_reg,
                    param_count: params.len() as u8,
                };

                members.push((fname.clone(), Value::Fun(Rc::new(compiled))));
            }
        }

        let module = crate::value::Module {
            name: name.to_string(),
            members,
        };
        let mod_val = Value::Module(Rc::new(module));
        let const_idx = self.add_constant(mod_val);
        let dst = self.alloc_reg();
        self.emit(Instruction::LoadConst {
            dst,
            idx: const_idx,
        });

        if self.scope_depth == 0 {
            let name_idx = self.add_name(name);
            self.emit(Instruction::SetGlobal { name_idx, src: dst });
            self.free_reg(dst);
        } else {
            self.locals.push(Local {
                name: name.to_string(),
                slot: dst,
                depth: self.scope_depth,
                is_const: true,
            });
        }

        Ok(())
    }

    /// Compiles a class declaration.
    ///
    /// Each method is compiled in a fresh `Compiler` with `self` injected
    /// at register slot 0. The `param_count` reflects user-visible params
    /// only (not counting `self`).
    fn compile_class_decl(
        &mut self,
        name: &str,
        methods: &[MethodDecl],
        _span: Span,
    ) -> CirqResult<()> {
        let mut compiled_methods = rustc_hash::FxHashMap::default();

        for method in methods {
            let mut mc = Compiler::new();
            mc.scope_depth = 1;
            // `self` occupies slot 0 — always present, not counted in param_count.
            let self_reg = mc.alloc_reg();
            mc.locals.push(Local {
                name: "self".to_string(),
                slot: self_reg,
                depth: 1,
                is_const: false,
            });

            // Allocate registers for user-declared parameters.
            for param in &method.params {
                let reg = mc.alloc_reg();
                mc.locals.push(Local {
                    name: param.clone(),
                    slot: reg,
                    depth: 1,
                    is_const: false,
                });
            }

            for stmt in &method.body {
                mc.compile_stmt(stmt)?;
            }

            // Implicit return: `init` returns `self`, others return `null`.
            let r = mc.alloc_reg();
            if method.name == "init" {
                mc.emit(Instruction::GetLocal { dst: r, slot: 0 });
            } else {
                mc.emit(Instruction::LoadNull { dst: r });
            }
            mc.emit(Instruction::Return { src: r });

            let compiled = CompiledFunction {
                name: format!("{}.{}", name, method.name),
                instructions: mc.instructions,
                constants: mc.constants,
                names: mc.names,
                local_count: mc.max_reg,
                // param_count = user params only (self is implicit).
                param_count: method.params.len() as u8,
            };

            compiled_methods.insert(method.name.clone(), Rc::new(compiled));
        }

        let class = crate::value::Class {
            name: name.to_string(),
            methods: compiled_methods,
        };
        let class_val = Value::Class(Rc::new(class));
        let const_idx = self.add_constant(class_val);
        let dst = self.alloc_reg();
        self.emit(Instruction::LoadConst {
            dst,
            idx: const_idx,
        });

        if self.scope_depth == 0 {
            let name_idx = self.add_name(name);
            self.emit(Instruction::SetGlobal { name_idx, src: dst });
            self.free_reg(dst);
        } else {
            self.locals.push(Local {
                name: name.to_string(),
                slot: dst,
                depth: self.scope_depth,
                is_const: true,
            });
        }

        Ok(())
    }

    // -------------------------------------------------------------------------
    // CONTROL FLOW COMPILATION
    // -------------------------------------------------------------------------

    /// Compiles an if/else statement.
    fn compile_if(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> CirqResult<()> {
        let cond_reg = self.compile_expr(condition)?;
        let jump_false = self.emit_placeholder();
        self.free_reg_to(cond_reg);

        self.compile_stmt(then_branch)?;

        if let Some(else_stmt) = else_branch {
            let jump_end = self.emit_placeholder();
            self.patch_jump_false(jump_false, cond_reg);
            self.compile_stmt(else_stmt)?;
            self.patch_jump(jump_end);
        } else {
            self.patch_jump_false(jump_false, cond_reg);
        }

        Ok(())
    }

    /// Compiles a while loop.
    fn compile_while(&mut self, condition: &Expr, body: &Stmt) -> CirqResult<()> {
        let loop_start = self.instructions.len();
        self.loops.push(LoopCtx {
            start: loop_start,
            break_jumps: Vec::new(),
            continue_jumps: Vec::new(),
        });

        let cond_reg = self.compile_expr(condition)?;
        let exit_jump = self.emit_placeholder();
        self.free_reg_to(cond_reg);

        self.compile_stmt(body)?;

        // Patch continue jumps → back to condition
        let loop_ctx_ref = self.loops.last().unwrap();
        let cont_indices: Vec<usize> = loop_ctx_ref.continue_jumps.clone();
        for cont in cont_indices {
            self.patch_jump_to(cont, loop_start);
        }

        // Jump back to condition
        let offset = loop_start as i32 - self.instructions.len() as i32 - 1;
        self.emit(Instruction::Jump { offset });

        self.patch_jump_false(exit_jump, cond_reg);

        // Patch break jumps → after loop
        let loop_ctx = self.loops.pop().unwrap();
        for brk in loop_ctx.break_jumps {
            self.patch_jump(brk);
        }

        Ok(())
    }

    /// Compiles a for loop.
    fn compile_for(
        &mut self,
        init: Option<&Stmt>,
        condition: Option<&Expr>,
        update: Option<&Expr>,
        body: &Stmt,
    ) -> CirqResult<()> {
        self.begin_scope();

        if let Some(init_stmt) = init {
            self.compile_stmt(init_stmt)?;
        }

        let loop_start = self.instructions.len();
        self.loops.push(LoopCtx {
            start: loop_start,
            break_jumps: Vec::new(),
            continue_jumps: Vec::new(),
        });

        let exit_jump = if let Some(cond) = condition {
            let cond_reg = self.compile_expr(cond)?;
            let j = self.emit_placeholder();
            self.free_reg_to(cond_reg);
            Some((j, cond_reg))
        } else {
            None
        };

        self.compile_stmt(body)?;

        // Patch continue jumps → update expression (not condition).
        // This ensures `continue` in a for-loop doesn't skip the update.
        let update_start = self.instructions.len();
        let cont_indices: Vec<usize> = self.loops.last().unwrap().continue_jumps.clone();
        for cont in cont_indices {
            self.patch_jump_to(cont, update_start);
        }

        if let Some(upd) = update {
            let r = self.compile_expr(upd)?;
            self.free_reg_to(r);
        }

        let offset = loop_start as i32 - self.instructions.len() as i32 - 1;
        self.emit(Instruction::Jump { offset });

        if let Some((j, src)) = exit_jump {
            self.patch_jump_false(j, src);
        }

        let loop_ctx = self.loops.pop().unwrap();
        for brk in loop_ctx.break_jumps {
            self.patch_jump(brk);
        }

        self.end_scope();
        Ok(())
    }

    /// Compiles a break statement.
    fn compile_break(&mut self, span: Span) -> CirqResult<()> {
        if self.loops.is_empty() {
            return Err(CirqError::compiler("'break' outside of loop", span));
        }
        let j = self.emit_placeholder();
        self.loops.last_mut().unwrap().break_jumps.push(j);
        Ok(())
    }

    /// Compiles a continue statement.
    /// Emits a placeholder jump that gets patched by the enclosing loop
    /// to jump to the correct target (condition for while, update for for).
    fn compile_continue(&mut self, span: Span) -> CirqResult<()> {
        if self.loops.is_empty() {
            return Err(CirqError::compiler("'continue' outside of loop", span));
        }
        let j = self.emit_placeholder();
        self.loops.last_mut().unwrap().continue_jumps.push(j);
        Ok(())
    }

    // -------------------------------------------------------------------------
    // EXPRESSION COMPILATION
    // -------------------------------------------------------------------------

    /// Compiles an expression, returning the register holding the result.
    fn compile_expr(&mut self, expr: &Expr) -> CirqResult<u8> {
        match expr {
            Expr::Number { value, .. } => {
                let dst = self.alloc_reg();
                let idx = self.add_constant(Value::Num(*value));
                self.emit(Instruction::LoadConst { dst, idx });
                Ok(dst)
            }
            Expr::Str { value, .. } => {
                let dst = self.alloc_reg();
                let idx = self.add_constant(Value::Str(Rc::new(value.clone())));
                self.emit(Instruction::LoadConst { dst, idx });
                Ok(dst)
            }
            Expr::Interpolation { parts, .. } => self.compile_interpolation(parts),
            Expr::Bool { value: true, .. } => {
                let dst = self.alloc_reg();
                self.emit(Instruction::LoadTrue { dst });
                Ok(dst)
            }
            Expr::Bool { value: false, .. } => {
                let dst = self.alloc_reg();
                self.emit(Instruction::LoadFalse { dst });
                Ok(dst)
            }
            Expr::Null { .. } => {
                let dst = self.alloc_reg();
                self.emit(Instruction::LoadNull { dst });
                Ok(dst)
            }
            Expr::Ident { name, span } => self.compile_ident(name, *span),
            Expr::Binary {
                left, op, right, ..
            } => self.compile_binary(left, *op, right),
            Expr::Unary { op, operand, .. } => self.compile_unary(*op, operand),
            Expr::Assign {
                target,
                value,
                span,
                ..
            } => self.compile_assign(target, value, *span),
            Expr::CompoundAssign {
                target,
                op,
                value,
                span,
            } => self.compile_compound_assign(target, *op, value, *span),
            Expr::Call { callee, args, span } => self.compile_call(callee, args, *span),
            Expr::Index { object, index, .. } => self.compile_index(object, index),
            Expr::MemberAccess {
                object,
                member,
                span,
            } => self.compile_member_access(object, member, *span),
            Expr::Array { elements, .. } => self.compile_array(elements),
            Expr::PreIncDec { op, operand, span } => self.compile_pre_inc_dec(*op, operand, *span),
            Expr::PostIncDec { op, operand, span } => {
                self.compile_post_inc_dec(*op, operand, *span)
            }
            Expr::SelfRef { span } => self.compile_ident("self", *span),
        }
    }

    /// Compiles string interpolation into Concat.
    fn compile_interpolation(&mut self, parts: &[InterpolationPart]) -> CirqResult<u8> {
        let start = self.next_reg;

        for part in parts {
            match part {
                InterpolationPart::Literal(s) => {
                    let r = self.alloc_reg();
                    let idx = self.add_constant(Value::Str(Rc::new(s.clone())));
                    self.emit(Instruction::LoadConst { dst: r, idx });
                }
                InterpolationPart::Expr(expr) => {
                    let r = self.compile_expr(expr)?;
                    // Convert to string in-place so only one register is
                    // consumed in the sequential Concat range.
                    self.emit(Instruction::ToString { dst: r, src: r });
                }
            }
        }

        let count = self.next_reg - start;
        let dst = start;
        self.emit(Instruction::Concat { dst, start, count });
        self.free_reg_to(start + 1);
        Ok(dst)
    }

    /// Compiles a variable reference.
    fn compile_ident(&mut self, name: &str, _span: Span) -> CirqResult<u8> {
        // Search locals from innermost scope outward.
        // Extract slot before calling alloc_reg to avoid overlapping borrows.
        let local_slot = self
            .locals
            .iter()
            .rev()
            .find(|l| l.name == name)
            .map(|l| l.slot);

        if let Some(slot) = local_slot {
            let dst = self.alloc_reg();
            self.emit(Instruction::GetLocal { dst, slot });
            return Ok(dst);
        }

        // Global
        let dst = self.alloc_reg();
        let name_idx = self.add_name(name);
        self.emit(Instruction::GetGlobal { dst, name_idx });
        Ok(dst)
    }

    /// Compiles a binary operation.
    fn compile_binary(&mut self, left: &Expr, op: BinOp, right: &Expr) -> CirqResult<u8> {
        // Short-circuit for && and ||
        match op {
            BinOp::And => return self.compile_and(left, right),
            BinOp::Or => return self.compile_or(left, right),
            _ => {}
        }

        let a = self.compile_expr(left)?;
        let b = self.compile_expr(right)?;
        let dst = a; // reuse left register

        let instr = match op {
            BinOp::Add => Instruction::Add { dst, a, b },
            BinOp::Sub => Instruction::Sub { dst, a, b },
            BinOp::Mul => Instruction::Mul { dst, a, b },
            BinOp::Div => Instruction::Div { dst, a, b },
            BinOp::Mod => Instruction::Mod { dst, a, b },
            BinOp::Pow => Instruction::Pow { dst, a, b },
            BinOp::Eq => Instruction::Eq { dst, a, b },
            BinOp::Ne => Instruction::Ne { dst, a, b },
            BinOp::Lt => Instruction::Lt { dst, a, b },
            BinOp::Gt => Instruction::Gt { dst, a, b },
            BinOp::Le => Instruction::Le { dst, a, b },
            BinOp::Ge => Instruction::Ge { dst, a, b },
            BinOp::BitAnd => Instruction::BitAnd { dst, a, b },
            BinOp::BitOr => Instruction::BitOr { dst, a, b },
            BinOp::BitXor => Instruction::BitXor { dst, a, b },
            BinOp::Shl => Instruction::Shl { dst, a, b },
            BinOp::Shr => Instruction::Shr { dst, a, b },
            BinOp::And | BinOp::Or => unreachable!(),
        };

        self.emit(instr);
        self.free_reg(b);
        Ok(dst)
    }

    /// Compiles short-circuit `&&`.
    fn compile_and(&mut self, left: &Expr, right: &Expr) -> CirqResult<u8> {
        let a = self.compile_expr(left)?;
        let jump = self.emit_placeholder();
        self.free_reg_to(a);
        let b = self.compile_expr(right)?;
        self.emit(Instruction::Move { dst: a, src: b });
        self.free_reg(b);
        self.patch_jump_false(jump, a);
        Ok(a)
    }

    /// Compiles short-circuit `||`.
    fn compile_or(&mut self, left: &Expr, right: &Expr) -> CirqResult<u8> {
        let a = self.compile_expr(left)?;
        let jump = self.instructions.len();
        self.emit(Instruction::JumpIfTrue { src: a, offset: 0 });
        self.free_reg_to(a);
        let b = self.compile_expr(right)?;
        self.emit(Instruction::Move { dst: a, src: b });
        self.free_reg(b);
        let offset = self.instructions.len() as i32 - jump as i32 - 1;
        self.instructions[jump] = Instruction::JumpIfTrue { src: a, offset };
        Ok(a)
    }

    /// Compiles a unary operation.
    fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> CirqResult<u8> {
        let src = self.compile_expr(operand)?;
        let dst = src;

        match op {
            UnaryOp::Neg => self.emit(Instruction::Neg { dst, src }),
            UnaryOp::Not => self.emit(Instruction::Not { dst, src }),
            UnaryOp::BitNot => self.emit(Instruction::BitNot { dst, src }),
            UnaryOp::Pos => {} // no-op, identity
        }

        Ok(dst)
    }

    /// Compiles an assignment expression.
    fn compile_assign(&mut self, target: &Expr, value: &Expr, span: Span) -> CirqResult<u8> {
        let val_reg = self.compile_expr(value)?;

        match target {
            Expr::Ident { name, .. } => {
                // Check for const reassignment
                for local in self.locals.iter().rev() {
                    if local.name == *name {
                        if local.is_const {
                            return Err(CirqError::compiler(
                                format!("cannot assign to constant '{}'", name),
                                span,
                            ));
                        }
                        self.emit(Instruction::SetLocal {
                            slot: local.slot,
                            src: val_reg,
                        });
                        return Ok(val_reg);
                    }
                }
                let name_idx = self.add_name(name);
                self.emit(Instruction::SetGlobal {
                    name_idx,
                    src: val_reg,
                });
            }
            Expr::Index { object, index, .. } => {
                let obj = self.compile_expr(object)?;
                let idx = self.compile_expr(index)?;
                self.emit(Instruction::SetIndex {
                    obj,
                    idx,
                    val: val_reg,
                });
                self.free_reg(idx);
                self.free_reg(obj);
            }
            Expr::MemberAccess { object, member, .. } => {
                let obj = self.compile_expr(object)?;
                let name_idx = self.add_name(member);
                self.emit(Instruction::SetMember {
                    obj,
                    name_idx,
                    val: val_reg,
                });
                self.free_reg(obj);
            }
            _ => {
                return Err(CirqError::compiler("invalid assignment target", span));
            }
        }

        Ok(val_reg)
    }

    /// Compiles a compound assignment (`+=`, `-=`, etc.).
    fn compile_compound_assign(
        &mut self,
        target: &Expr,
        op: BinOp,
        value: &Expr,
        span: Span,
    ) -> CirqResult<u8> {
        // Load current value, compute, store back
        let current = self.compile_expr(target)?;
        let rhs = self.compile_expr(value)?;
        let dst = current;

        let instr = match op {
            BinOp::Add => Instruction::Add {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Sub => Instruction::Sub {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Mul => Instruction::Mul {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Div => Instruction::Div {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Mod => Instruction::Mod {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Pow => Instruction::Pow {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::BitAnd => Instruction::BitAnd {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::BitOr => Instruction::BitOr {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::BitXor => Instruction::BitXor {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Shl => Instruction::Shl {
                dst,
                a: current,
                b: rhs,
            },
            BinOp::Shr => Instruction::Shr {
                dst,
                a: current,
                b: rhs,
            },
            _ => {
                return Err(CirqError::compiler(
                    "invalid compound assignment operator",
                    span,
                ));
            }
        };

        self.emit(instr);
        self.free_reg(rhs);

        // Store result back
        self.compile_assign(target, &Expr::Null { span }, span)?;
        // Actually we need to store the register—let's directly store
        match target {
            Expr::Ident { name, .. } => {
                for local in self.locals.iter().rev() {
                    if local.name == *name {
                        self.emit(Instruction::SetLocal {
                            slot: local.slot,
                            src: dst,
                        });
                        return Ok(dst);
                    }
                }
                let name_idx = self.add_name(name);
                self.emit(Instruction::SetGlobal { name_idx, src: dst });
            }
            Expr::Index { object, index, .. } => {
                let obj = self.compile_expr(object)?;
                let idx = self.compile_expr(index)?;
                self.emit(Instruction::SetIndex { obj, idx, val: dst });
                self.free_reg(idx);
                self.free_reg(obj);
            }
            Expr::MemberAccess { object, member, .. } => {
                let obj = self.compile_expr(object)?;
                let name_idx = self.add_name(member);
                self.emit(Instruction::SetMember {
                    obj,
                    name_idx,
                    val: dst,
                });
                self.free_reg(obj);
            }
            _ => {}
        }

        Ok(dst)
    }

    /// Compiles a function call.
    fn compile_call(&mut self, callee: &Expr, args: &[Expr], _span: Span) -> CirqResult<u8> {
        let func_reg = self.compile_expr(callee)?;
        let arg_start = self.next_reg;

        for arg in args {
            let r = self.compile_expr(arg)?;
            // Ensure argument is in the expected sequential register
            if r != self.next_reg - 1 {
                // Already there from compile_expr's alloc_reg
            }
        }

        let arg_count = args.len() as u8;
        let dst = func_reg; // Reuse callee register for return value
        self.emit(Instruction::Call {
            dst,
            func_reg,
            arg_start,
            arg_count,
        });

        // Free argument registers
        self.free_reg_to(arg_start);

        Ok(dst)
    }

    /// Compiles array index access.
    fn compile_index(&mut self, object: &Expr, index: &Expr) -> CirqResult<u8> {
        let obj = self.compile_expr(object)?;
        let idx = self.compile_expr(index)?;
        let dst = obj;
        self.emit(Instruction::GetIndex { dst, obj, idx });
        self.free_reg(idx);
        Ok(dst)
    }

    /// Compiles member access (e.g., `io.print`).
    fn compile_member_access(
        &mut self,
        object: &Expr,
        member: &str,
        _span: Span,
    ) -> CirqResult<u8> {
        let obj = self.compile_expr(object)?;
        let name_idx = self.add_name(member);
        let dst = obj;
        self.emit(Instruction::GetMember { dst, obj, name_idx });
        Ok(dst)
    }

    /// Compiles an array literal.
    fn compile_array(&mut self, elements: &[Expr]) -> CirqResult<u8> {
        let start = self.next_reg;

        for elem in elements {
            self.compile_expr(elem)?;
        }

        let count = elements.len() as u8;
        let dst = if start > 0 { start } else { self.alloc_reg() };

        self.emit(Instruction::NewArray { dst, start, count });
        self.free_reg_to(start + 1);
        Ok(dst)
    }

    /// Compiles prefix `++x` or `--x`.
    fn compile_pre_inc_dec(&mut self, op: IncDecOp, operand: &Expr, span: Span) -> CirqResult<u8> {
        let reg = self.compile_expr(operand)?;
        match op {
            IncDecOp::Inc => self.emit(Instruction::Inc { dst: reg }),
            IncDecOp::Dec => self.emit(Instruction::Dec { dst: reg }),
        }
        // Store back
        self.store_back(operand, reg, span)?;
        Ok(reg)
    }

    /// Compiles postfix `x++` or `x--`.
    fn compile_post_inc_dec(&mut self, op: IncDecOp, operand: &Expr, span: Span) -> CirqResult<u8> {
        let reg = self.compile_expr(operand)?;
        let copy = self.alloc_reg();
        self.emit(Instruction::Move {
            dst: copy,
            src: reg,
        });
        match op {
            IncDecOp::Inc => self.emit(Instruction::Inc { dst: reg }),
            IncDecOp::Dec => self.emit(Instruction::Dec { dst: reg }),
        }
        self.store_back(operand, reg, span)?;
        self.free_reg(reg);
        Ok(copy)
    }

    /// Stores a register value back to the target (used by ++/--).
    fn store_back(&mut self, target: &Expr, reg: u8, _span: Span) -> CirqResult<()> {
        match target {
            Expr::Ident { name, .. } => {
                for local in self.locals.iter().rev() {
                    if local.name == *name {
                        self.emit(Instruction::SetLocal {
                            slot: local.slot,
                            src: reg,
                        });
                        return Ok(());
                    }
                }
                let name_idx = self.add_name(name);
                self.emit(Instruction::SetGlobal { name_idx, src: reg });
            }
            Expr::MemberAccess { object, member, .. } => {
                let obj = self.compile_expr(object)?;
                let name_idx = self.add_name(member);
                self.emit(Instruction::SetMember {
                    obj,
                    name_idx,
                    val: reg,
                });
                self.free_reg(obj);
            }
            _ => {}
        }
        Ok(())
    }

    // -------------------------------------------------------------------------
    // REGISTER ALLOCATION
    // -------------------------------------------------------------------------

    /// Allocates the next available register.
    #[inline]
    fn alloc_reg(&mut self) -> u8 {
        let reg = self.next_reg;
        self.next_reg += 1;
        if self.next_reg > self.max_reg {
            self.max_reg = self.next_reg;
        }
        reg
    }

    /// Frees a single register (must be the most recently allocated).
    #[inline]
    fn free_reg(&mut self, _reg: u8) {
        if self.next_reg > 0 {
            self.next_reg -= 1;
        }
    }

    /// Frees all registers from `target` onwards.
    #[inline]
    fn free_reg_to(&mut self, target: u8) {
        self.next_reg = target;
    }

    // -------------------------------------------------------------------------
    // SCOPE MANAGEMENT
    // -------------------------------------------------------------------------

    /// Enters a new scope level.
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Exits the current scope, removing its locals.
    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth {
                break;
            }
            self.locals.pop();
            if self.next_reg > 0 {
                self.next_reg -= 1;
            }
        }
    }

    // -------------------------------------------------------------------------
    // CONSTANT & NAME POOLS
    // -------------------------------------------------------------------------

    /// Adds a constant to the pool, returning its index.
    fn add_constant(&mut self, value: Value) -> u16 {
        let idx = self.constants.len() as u16;
        self.constants.push(value);
        idx
    }

    /// Adds a name to the name pool, returning its index.
    fn add_name(&mut self, name: &str) -> u16 {
        // Check for existing
        for (i, n) in self.names.iter().enumerate() {
            if n == name {
                return i as u16;
            }
        }
        let idx = self.names.len() as u16;
        self.names.push(name.to_string());
        idx
    }

    // -------------------------------------------------------------------------
    // INSTRUCTION EMISSION
    // -------------------------------------------------------------------------

    /// Emits an instruction, returning its index.
    #[inline]
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    /// Emits a placeholder Jump instruction, returning its index for patching.
    fn emit_placeholder(&mut self) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(Instruction::Jump { offset: 0 });
        idx
    }

    /// Patches a jump placeholder to jump to the current instruction.
    fn patch_jump(&mut self, placeholder: usize) {
        let offset = self.instructions.len() as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::Jump { offset };
    }

    /// Patches a jump placeholder to jump to a specific instruction index.
    /// Unlike `patch_jump`, this allows jumping to any target, not just the
    /// current position. Used for continue-to-update patching in for-loops.
    fn patch_jump_to(&mut self, placeholder: usize, target: usize) {
        let offset = target as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::Jump { offset };
    }

    /// Patches a placeholder to a JumpIfFalse to the current instruction.
    fn patch_jump_false(&mut self, placeholder: usize, src: u8) {
        let offset = self.instructions.len() as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::JumpIfFalse { src, offset };
    }
}
