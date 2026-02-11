use crate::ast::*;
use crate::error::{CirqError, CirqResult, Span};
use crate::opcode::{CompiledFunction, Instruction};
use crate::value::Value;

use std::rc::Rc;
#[derive(Debug, Clone)]
struct Local {
    name: String,
    slot: u8,
    depth: u32,
    is_const: bool,
}
#[derive(Debug, Clone)]
struct LoopCtx {
    #[allow(dead_code)]
    start: usize,
    break_jumps: Vec<usize>,
    continue_jumps: Vec<usize>,
}
pub struct Compiler {
    instructions: Vec<Instruction>,
    spans: Vec<Span>,
    current_span: Span,
    constants: Vec<Value>,
    names: Vec<String>,
    locals: Vec<Local>,
    scope_depth: u32,
    next_reg: u8,
    max_reg: u8,
    param_count: u8,
    loops: Vec<LoopCtx>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            spans: Vec::new(),
            current_span: Span {
                line: 1,
                col: 1,
                len: 0,
            },
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

    pub fn compile(mut self, stmts: &[Stmt]) -> CirqResult<CompiledFunction> {
        for stmt in stmts {
            self.compile_stmt(stmt)?;
        }

        let r = self.alloc_reg();
        self.emit(Instruction::LoadNull { dst: r });
        self.emit(Instruction::Return { src: r });

        Ok(CompiledFunction {
            name: "<main>".to_string(),
            instructions: self.instructions,
            spans: self.spans,
            constants: self.constants,
            names: self.names,
            local_count: self.max_reg,
            param_count: 0,
        })
    }

    pub fn compile_repl(mut self, stmts: &[Stmt]) -> CirqResult<CompiledFunction> {
        if stmts.is_empty() {
            let r = self.alloc_reg();
            self.emit(Instruction::LoadNull { dst: r });
            self.emit(Instruction::Return { src: r });
        } else {
            for stmt in &stmts[..stmts.len() - 1] {
                self.compile_stmt(stmt)?;
            }

            let last = &stmts[stmts.len() - 1];

            if let Stmt::ExprStmt { expr, .. } = last {
                let r = self.compile_expr(expr)?;
                self.emit(Instruction::Return { src: r });
            } else {
                self.compile_stmt(last)?;
                let r = self.alloc_reg();
                self.emit(Instruction::LoadNull { dst: r });
                self.emit(Instruction::Return { src: r });
            }
        }

        Ok(CompiledFunction {
            name: "<repl>".to_string(),
            instructions: self.instructions,
            spans: self.spans,
            constants: self.constants,
            names: self.names,
            local_count: self.max_reg,
            param_count: 0,
        })
    }
    fn compile_stmt(&mut self, stmt: &Stmt) -> CirqResult<()> {
        self.current_span = stmt.get_span();
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
                superclass,
                methods,
                span,
            } => self.compile_class_decl(name, superclass.as_deref(), methods, *span),
            Stmt::Throw { value, span } => self.compile_throw(value, *span),
            Stmt::TryCatch {
                body,
                catch_name,
                catch_body,
                span,
            } => self.compile_try_catch_stmt(
                body,
                catch_name.as_deref(),
                catch_body.as_deref(),
                *span,
            ),
        }
    }

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
            let name_idx = self.add_name(name);
            self.emit(Instruction::SetGlobal { name_idx, src: reg });
            self.free_reg(reg);
        } else {
            self.locals.push(Local {
                name: name.to_string(),
                slot: reg,
                depth: self.scope_depth,
                is_const,
            });
        }

        Ok(())
    }

    fn compile_fun_decl(
        &mut self,
        name: &str,
        params: &[String],
        body: &[Stmt],
        _span: Span,
    ) -> CirqResult<()> {
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

        for stmt in body {
            fun_compiler.compile_stmt(stmt)?;
        }

        let r = fun_compiler.alloc_reg();
        fun_compiler.emit(Instruction::LoadNull { dst: r });
        fun_compiler.emit(Instruction::Return { src: r });

        let compiled = CompiledFunction {
            name: name.to_string(),
            instructions: fun_compiler.instructions,
            spans: fun_compiler.spans,
            constants: fun_compiler.constants,
            names: fun_compiler.names,
            local_count: fun_compiler.max_reg,
            param_count: params.len() as u8,
        };

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

    fn compile_mod_decl(&mut self, name: &str, body: &[Stmt], _span: Span) -> CirqResult<()> {
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
                    spans: fun_compiler.spans,
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

    fn compile_class_decl(
        &mut self,
        name: &str,
        superclass: Option<&str>,
        methods: &[MethodDecl],
        _span: Span,
    ) -> CirqResult<()> {
        let parent_reg = if let Some(sc) = superclass {
            let r = self.alloc_reg();
            let name_idx = self.add_name(sc);
            self.emit(Instruction::GetGlobal { dst: r, name_idx });
            Some(r)
        } else {
            None
        };

        let mut compiled_methods = rustc_hash::FxHashMap::default();

        for method in methods {
            let mut mc = Compiler::new();
            mc.scope_depth = 1;
            let self_reg = mc.alloc_reg();
            mc.locals.push(Local {
                name: "self".to_string(),
                slot: self_reg,
                depth: 1,
                is_const: false,
            });

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
                spans: mc.spans,
                constants: mc.constants,
                names: mc.names,
                local_count: mc.max_reg,
                param_count: method.params.len() as u8,
            };

            compiled_methods.insert(method.name.clone(), Value::Fun(Rc::new(compiled)));
        }

        let class = crate::value::Class {
            name: name.to_string(),
            methods: compiled_methods,
            parent: None,
        };
        let class_val = Value::Class(Rc::new(class));
        let const_idx = self.add_constant(class_val);
        let dst = self.alloc_reg();
        self.emit(Instruction::LoadConst {
            dst,
            idx: const_idx,
        });

        if let Some(pr) = parent_reg {
            self.emit(Instruction::Inherit {
                child: dst,
                parent: pr,
            });
            self.free_reg(pr);
        }

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

        let loop_ctx_ref = self.loops.last().unwrap();
        let cont_indices: Vec<usize> = loop_ctx_ref.continue_jumps.clone();
        for cont in cont_indices {
            self.patch_jump_to(cont, loop_start);
        }

        let offset = loop_start as i32 - self.instructions.len() as i32 - 1;
        self.emit(Instruction::Jump { offset });

        self.patch_jump_false(exit_jump, cond_reg);

        let loop_ctx = self.loops.pop().unwrap();
        for brk in loop_ctx.break_jumps {
            self.patch_jump(brk);
        }

        Ok(())
    }

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

    fn compile_break(&mut self, span: Span) -> CirqResult<()> {
        if self.loops.is_empty() {
            return Err(CirqError::syntax("'break' outside of loop", span));
        }
        let j = self.emit_placeholder();
        self.loops.last_mut().unwrap().break_jumps.push(j);
        Ok(())
    }

    fn compile_continue(&mut self, span: Span) -> CirqResult<()> {
        if self.loops.is_empty() {
            return Err(CirqError::syntax("'continue' outside of loop", span));
        }
        let j = self.emit_placeholder();
        self.loops.last_mut().unwrap().continue_jumps.push(j);
        Ok(())
    }
    fn compile_expr(&mut self, expr: &Expr) -> CirqResult<u8> {
        self.current_span = expr.get_span();
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
            Expr::SuperAccess { member, span, .. } => self.compile_super_access(member, *span),
            Expr::TryExpr {
                body,
                catch_name,
                catch_body,
                span,
            } => self.compile_try_expr(body, catch_name.as_deref(), catch_body.as_deref(), *span),
            Expr::NullCoalesce { left, right, .. } => self.compile_null_coalesce(left, right),
        }
    }

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

    fn compile_ident(&mut self, name: &str, _span: Span) -> CirqResult<u8> {
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

        let dst = self.alloc_reg();
        let name_idx = self.add_name(name);
        self.emit(Instruction::GetGlobal { dst, name_idx });
        Ok(dst)
    }

    fn compile_binary(&mut self, left: &Expr, op: BinOp, right: &Expr) -> CirqResult<u8> {
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

    fn compile_assign(&mut self, target: &Expr, value: &Expr, span: Span) -> CirqResult<u8> {
        let val_reg = self.compile_expr(value)?;

        match target {
            Expr::Ident { name, .. } => {
                for local in self.locals.iter().rev() {
                    if local.name == *name {
                        if local.is_const {
                            return Err(CirqError::type_error(
                                format!("cannot assign to constant '{}'", name),
                                Some(span),
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
                return Err(CirqError::syntax("invalid assignment target", span));
            }
        }

        Ok(val_reg)
    }

    fn compile_compound_assign(
        &mut self,
        target: &Expr,
        op: BinOp,
        value: &Expr,
        span: Span,
    ) -> CirqResult<u8> {
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
                return Err(CirqError::syntax(
                    "invalid compound assignment operator",
                    span,
                ));
            }
        };

        self.emit(instr);
        self.free_reg(rhs);

        self.compile_assign(target, &Expr::Null { span }, span)?;
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

    fn compile_call(&mut self, callee: &Expr, args: &[Expr], _span: Span) -> CirqResult<u8> {
        let func_reg = self.compile_expr(callee)?;
        let arg_start = self.next_reg;

        for arg in args {
            let r = self.compile_expr(arg)?;
            if r != self.next_reg - 1 {}
        }

        let arg_count = args.len() as u8;
        let dst = func_reg; // Reuse callee register for return value
        self.emit(Instruction::Call {
            dst,
            func_reg,
            arg_start,
            arg_count,
        });

        self.free_reg_to(arg_start);

        Ok(dst)
    }

    fn compile_index(&mut self, object: &Expr, index: &Expr) -> CirqResult<u8> {
        let obj = self.compile_expr(object)?;
        let idx = self.compile_expr(index)?;
        let dst = obj;
        self.emit(Instruction::GetIndex { dst, obj, idx });
        self.free_reg(idx);
        Ok(dst)
    }

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

    fn compile_pre_inc_dec(&mut self, op: IncDecOp, operand: &Expr, span: Span) -> CirqResult<u8> {
        let reg = self.compile_expr(operand)?;
        match op {
            IncDecOp::Inc => self.emit(Instruction::Inc { dst: reg }),
            IncDecOp::Dec => self.emit(Instruction::Dec { dst: reg }),
        }
        self.store_back(operand, reg, span)?;
        Ok(reg)
    }

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

    fn compile_throw(&mut self, value: &Expr, _span: Span) -> CirqResult<()> {
        let src = self.compile_expr(value)?;
        self.emit(Instruction::Throw { src });
        self.free_reg_to(src);
        Ok(())
    }

    fn compile_try_catch_stmt(
        &mut self,
        body: &[Stmt],
        catch_name: Option<&str>,
        catch_body: Option<&[Stmt]>,
        _span: Span,
    ) -> CirqResult<()> {
        let result_reg = self.alloc_reg();
        let setup_idx = self.instructions.len();
        self.emit(Instruction::SetupTry {
            catch_offset: 0,
            dst: result_reg,
        });

        self.begin_scope();
        for stmt in body {
            self.compile_stmt(stmt)?;
        }
        self.end_scope();

        self.emit(Instruction::PopTry);
        let jump_over_catch = self.emit_placeholder();

        let catch_ip = self.instructions.len();
        let catch_offset = catch_ip as i32 - setup_idx as i32 - 1;
        self.instructions[setup_idx] = Instruction::SetupTry {
            catch_offset,
            dst: result_reg,
        };

        if let Some(cb) = catch_body {
            self.begin_scope();
            if let Some(name) = catch_name {
                let err_reg = self.alloc_reg();
                self.emit(Instruction::Move {
                    dst: err_reg,
                    src: result_reg,
                });
                self.locals.push(Local {
                    name: name.to_string(),
                    slot: err_reg,
                    depth: self.scope_depth,
                    is_const: true,
                });
            }
            for stmt in cb {
                self.compile_stmt(stmt)?;
            }
            self.end_scope();
        }

        self.patch_jump(jump_over_catch);
        self.free_reg(result_reg);
        Ok(())
    }

    fn compile_super_access(&mut self, member: &str, _span: Span) -> CirqResult<u8> {
        let self_reg = self.compile_ident("self", _span)?;
        let name_idx = self.add_name(member);
        let dst = self_reg;
        self.emit(Instruction::GetSuper {
            dst,
            obj: self_reg,
            name_idx,
        });
        Ok(dst)
    }

    fn compile_try_expr(
        &mut self,
        body: &Expr,
        catch_name: Option<&str>,
        catch_body: Option<&[Stmt]>,
        _span: Span,
    ) -> CirqResult<u8> {
        let result_reg = self.alloc_reg();
        let setup_idx = self.instructions.len();
        self.emit(Instruction::SetupTry {
            catch_offset: 0,
            dst: result_reg,
        });

        let body_reg = self.compile_expr(body)?;
        self.emit(Instruction::Move {
            dst: result_reg,
            src: body_reg,
        });
        self.free_reg_to(result_reg + 1);

        self.emit(Instruction::PopTry);
        let jump_over_catch = self.emit_placeholder();

        let catch_ip = self.instructions.len();
        let catch_offset = catch_ip as i32 - setup_idx as i32 - 1;
        self.instructions[setup_idx] = Instruction::SetupTry {
            catch_offset,
            dst: result_reg,
        };

        if let Some(cb) = catch_body {
            self.begin_scope();
            if let Some(name) = catch_name {
                let err_reg = self.alloc_reg();
                self.emit(Instruction::Move {
                    dst: err_reg,
                    src: result_reg,
                });
                self.locals.push(Local {
                    name: name.to_string(),
                    slot: err_reg,
                    depth: self.scope_depth,
                    is_const: true,
                });
            }
            for stmt in cb {
                self.compile_stmt(stmt)?;
            }
            if let Some(last_stmt) = cb.last() {
                if let Stmt::ExprStmt { expr, .. } = last_stmt {
                    let _ = expr;
                }
            }
            self.end_scope();
        } else {
            self.emit(Instruction::LoadNull { dst: result_reg });
        }

        self.patch_jump(jump_over_catch);
        Ok(result_reg)
    }

    fn compile_null_coalesce(&mut self, left: &Expr, right: &Expr) -> CirqResult<u8> {
        let a = self.compile_expr(left)?;
        let b = self.compile_expr(right)?;
        let dst = a;
        self.emit(Instruction::NullCoalesce { dst, a, b });
        self.free_reg(b);
        Ok(dst)
    }

    #[inline]
    fn alloc_reg(&mut self) -> u8 {
        let reg = self.next_reg;
        self.next_reg += 1;
        if self.next_reg > self.max_reg {
            self.max_reg = self.next_reg;
        }
        reg
    }

    #[inline]
    fn free_reg(&mut self, _reg: u8) {
        if self.next_reg > 0 {
            self.next_reg -= 1;
        }
    }

    #[inline]
    fn free_reg_to(&mut self, target: u8) {
        self.next_reg = target;
    }
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

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
    fn add_constant(&mut self, value: Value) -> u16 {
        let idx = self.constants.len() as u16;
        self.constants.push(value);
        idx
    }

    fn add_name(&mut self, name: &str) -> u16 {
        for (i, n) in self.names.iter().enumerate() {
            if n == name {
                return i as u16;
            }
        }
        let idx = self.names.len() as u16;
        self.names.push(name.to_string());
        idx
    }
    #[inline]
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
        self.spans.push(self.current_span);
    }

    fn emit_placeholder(&mut self) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(Instruction::Jump { offset: 0 });
        self.spans.push(self.current_span);
        idx
    }

    fn patch_jump(&mut self, placeholder: usize) {
        let offset = self.instructions.len() as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::Jump { offset };
    }

    fn patch_jump_to(&mut self, placeholder: usize, target: usize) {
        let offset = target as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::Jump { offset };
    }

    fn patch_jump_false(&mut self, placeholder: usize, src: u8) {
        let offset = self.instructions.len() as i32 - placeholder as i32 - 1;
        self.instructions[placeholder] = Instruction::JumpIfFalse { src, offset };
    }
}
