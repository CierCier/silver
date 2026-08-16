use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType};
use inkwell::values::{BasicValueEnum, FunctionValue};

use crate::codegen::SilverGenerator;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::VarInfo;
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;

impl<'ctx> LlvmIrGenerator<'ctx> {
    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub(crate) fn emit_function_body(
        &mut self,
        function: FunctionValue<'ctx>,
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
        fn_name: &str,
        fn_span: &Span,
        skip_first_param_drop: bool,
    ) -> CodegenResult<()> {
        if function.count_basic_blocks() > 0 {
            return Ok(());
        }

        if let Some(debug) = &mut self.debug {
            let (line, _col, _, _) = debug.source_map.span_to_line_col(fn_span);
            let subroutine_type = debug.create_subroutine_type(
                return_type.and_then(|_| debug.di_types.get("i32").copied()),
                &[],
            );
            let subprogram =
                debug.create_function(fn_name, fn_name, line, subroutine_type, false, true, line);
            function.set_subprogram(subprogram);
            debug.current_subprogram = Some(subprogram);
        }

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.set_debug_location(fn_span);

        let saved_return_type = self.current_return_type.clone();
        self.current_fn = Some(function);
        self.current_return_type = return_type.cloned();
        self.push_scope();

        for (index, param) in parameters.iter().enumerate() {
            let Some(param_value) = function.get_nth_param(index as u32) else {
                continue;
            };
            let alloca =
                self.create_entry_alloca(function, &param.name.name, param_value.get_type())?;
            self.builder.build_store(alloca, param_value).map_err(|e| {
                CodegenError::with_span(
                    format!("failed to store parameter `{}`: {e}", param.name.name),
                    param.span,
                )
            })?;
            if let Some(scope) = self.variables.last_mut() {
                scope.insert(
                    param.name.name.clone(),
                    VarInfo {
                        ptr: alloca,
                        ty: param.param_type.clone(),
                        is_mutable: param.is_mutable,
                        is_volatile: false,
                        drop_flag: None,
                    },
                );
            }
        }

        // Set up drop flags and defers for parameters that implement Drop.
        for (param_index, param) in parameters.iter().enumerate() {
            // Cast receivers are borrowed views, not owned values: the caller
            // retains ownership, so the cast's by-value `self` copy must NOT
            // run the destructor on exit (it would free the caller's buffer
            // and dangle any view the cast returned).
            if skip_first_param_drop && param_index == 0 {
                continue;
            }
            // Skip pointer/reference types: the caller owns the pointee,
            // so we must NOT call the pointee's drop on function exit.
            if matches!(
                param.param_type.kind.as_ref(),
                ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
            ) {
                continue;
            }
            let ty_for_drop = param.param_type.clone();
            let alloca = self
                .variables
                .last()
                .and_then(|scope| scope.get(&param.name.name))
                .map(|vi| vi.ptr)
                .ok_or_else(|| {
                    CodegenError::new(format!(
                        "parameter alloca for `{}` not found",
                        param.name.name
                    ))
                })?;
            self.register_drop_flag(&param.name.name, &ty_for_drop, alloca)?;
        }

        self.generate_block(body)?;
        // Fire any remaining defers (e.g. parameter destructors that were
        // registered in the function scope but not in the body scope).
        // This must happen before the terminator check so that the block
        // created by emit_defers is properly terminated.
        if !self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some()
        {
            self.emit_defers(self.defers.len())?;
        }

        let needs_terminator = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_none();
        if needs_terminator {
            if return_type.is_some_and(|ret| !Self::is_void_primitive(ret)) {
                return Err(CodegenError::with_span(
                    format!("function `{fn_name}` may exit without returning a value"),
                    *fn_span,
                ));
            }
            self.builder
                .build_return(None)
                .map_err(|e| CodegenError::new(format!("failed to emit return: {e}")))?;
        }

        self.pop_scope();
        self.current_fn = None;
        self.current_return_type = saved_return_type;
        self.static_local_counter = 0;
        if let Some(debug) = &mut self.debug {
            debug.current_subprogram = None;
        }
        Ok(())
    }

    pub(crate) fn emit_if_statement(
        &mut self,
        condition: &ast::Expression,
        then_branch: &ast::Block,
        else_branch: Option<&ast::Block>,
        _span: &Span,
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for if statement"))?;

        let then_bb = self.context.append_basic_block(function, "if.then");
        let else_bb = self.context.append_basic_block(function, "if.else");
        let cont_bb = self.context.append_basic_block(function, "if.cont");

        let cond_value = self.emit_expression_value(condition)?;
        let cond_bool = self.emit_as_bool(&cond_value, &condition.span)?;
        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|e| CodegenError::new(format!("failed to branch for if: {e}")))?;

        self.builder.position_at_end(then_bb);
        self.generate_block(then_branch)?;
        let then_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !then_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed to branch from then: {e}")))?;
        }

        self.builder.position_at_end(else_bb);
        if let Some(else_block) = else_branch {
            self.generate_block(else_block)?;
        }
        let else_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !else_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed to branch from else: {e}")))?;
        }
        // When both branches are terminated (e.g., both contain `return`),
        // `cont_bb` is unreachable.  Insert an `unreachable` terminator so
        // the parent's terminator check sees a terminated block.
        self.builder.position_at_end(cont_bb);
        if then_terminated && else_terminated {
            self.builder
                .build_unreachable()
                .map_err(|e| CodegenError::new(format!("failed to emit unreachable: {e}")))?;
        }
        Ok(())
    }

    pub(crate) fn emit_while_statement(
        &mut self,
        condition: &ast::Expression,
        body: &ast::Block,
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for while statement"))?;

        let cond_bb = self.context.append_basic_block(function, "while.cond");
        let body_bb = self.context.append_basic_block(function, "while.body");
        let end_bb = self.context.append_basic_block(function, "while.end");

        self.builder
            .build_unconditional_branch(cond_bb)
            .map_err(|e| CodegenError::new(format!("failed to enter while condition: {e}")))?;

        self.builder.position_at_end(cond_bb);
        let cond_value = self.emit_expression_value(condition)?;
        let cond_bool = self.emit_as_bool(&cond_value, &condition.span)?;
        self.builder
            .build_conditional_branch(cond_bool, body_bb, end_bb)
            .map_err(|e| CodegenError::new(format!("failed while condition branch: {e}")))?;

        self.loop_stack.push((end_bb, cond_bb));
        self.loop_defers_base.push(self.defers.len());
        self.builder.position_at_end(body_bb);
        self.generate_block(body)?;
        self.loop_defers_base.pop();
        let body_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !body_terminated {
            self.builder
                .build_unconditional_branch(cond_bb)
                .map_err(|e| CodegenError::new(format!("failed to loop while body: {e}")))?;
        }
        self.loop_stack.pop();

        self.builder.position_at_end(end_bb);
        Ok(())
    }

    pub(crate) fn emit_for_statement(
        &mut self,
        init: &ast::LetStatement,
        condition: &ast::Expression,
        increment: &ast::Expression,
        body: &ast::Block,
        span: &Span,
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for for statement"))?;

        self.push_scope();

        self.emit_let_statement(init, span)?;

        let cond_bb = self.context.append_basic_block(function, "for.cond");
        let body_bb = self.context.append_basic_block(function, "for.body");
        let incr_bb = self.context.append_basic_block(function, "for.incr");
        let end_bb = self.context.append_basic_block(function, "for.end");

        self.builder
            .build_unconditional_branch(cond_bb)
            .map_err(|e| CodegenError::new(format!("failed to enter for condition: {e}")))?;

        self.builder.position_at_end(cond_bb);
        let cond_value = self.emit_expression_value(condition)?;
        let cond_bool = self.emit_as_bool(&cond_value, &condition.span)?;
        self.builder
            .build_conditional_branch(cond_bool, body_bb, end_bb)
            .map_err(|e| CodegenError::new(format!("failed for condition branch: {e}")))?;

        self.loop_stack.push((end_bb, incr_bb));
        self.loop_defers_base.push(self.defers.len());
        self.builder.position_at_end(body_bb);
        self.generate_block(body)?;
        self.loop_defers_base.pop();
        let body_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !body_terminated {
            self.builder
                .build_unconditional_branch(incr_bb)
                .map_err(|e| CodegenError::new(format!("failed to advance for loop: {e}")))?;
        }

        self.builder.position_at_end(incr_bb);
        let _ = self.emit_expression_value(increment)?;
        let incr_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !incr_terminated {
            self.builder
                .build_unconditional_branch(cond_bb)
                .map_err(|e| CodegenError::new(format!("failed to close for loop: {e}")))?;
        }
        self.loop_stack.pop();

        self.builder.position_at_end(end_bb);
        self.pop_scope();
        Ok(())
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub(crate) fn emit_for_in_statement(
        &mut self,
        binding: &ast::Identifier,
        is_mutable: bool,
        iterable: &ast::Expression,
        body: &ast::Block,
        _item_type: Option<&ast::Type>,
        iterator_type: Option<&ast::Type>,
        mode: ast::IterAccessMode,
        span: &Span,
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for for-in statement"))?;

        match iterable.kind.as_ref() {
            ast::ExpressionKind::Binary {
                left,
                operator: ast::BinaryOperator::Range,
                right,
            } => {
                self.push_scope();
                let start_val = self.emit_expression_value(left)?;
                let end_val = self.emit_expression_value(right)?;

                let llvm_ty = start_val.get_type();
                let i_ptr = self.create_entry_alloca(function, &binding.name, llvm_ty)?;
                self.builder
                    .build_store(i_ptr, start_val)
                    .map_err(|e| CodegenError::new(format!("failed to init loop var: {e}")))?;

                let ast_ty = self.infer_ast_type_from_value(&start_val, span);
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        binding.name.clone(),
                        VarInfo {
                            ptr: i_ptr,
                            ty: ast_ty,
                            is_mutable,
                            is_volatile: false,
                            drop_flag: None,
                        },
                    );
                }

                let cond_bb = self.context.append_basic_block(function, "forin.cond");
                let body_bb = self.context.append_basic_block(function, "forin.body");
                let end_bb = self.context.append_basic_block(function, "forin.end");

                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| CodegenError::new(format!("failed to enter for-in cond: {e}")))?;

                self.builder.position_at_end(cond_bb);
                let i_val_raw = self
                    .builder
                    .build_load(llvm_ty, i_ptr, &binding.name)
                    .map_err(|e| CodegenError::new(format!("failed to load loop var: {e}")))?;
                let (i_val, end_val) = match (i_val_raw, end_val) {
                    (BasicValueEnum::IntValue(iv), BasicValueEnum::IntValue(ev)) => {
                        let iv_ty = iv.get_type();
                        let ev_ty = ev.get_type();
                        let iv_width = iv_ty.get_bit_width();
                        let ev_width = ev_ty.get_bit_width();
                        let ev = if iv_width > ev_width {
                            self.builder
                                .build_int_s_extend(ev, iv_ty, "forin.end.cast")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed to extend end bound: {e}"))
                                })?
                        } else if iv_width < ev_width {
                            self.builder
                                .build_int_truncate(ev, iv_ty, "forin.end.cast")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed to truncate end bound: {e}"))
                                })?
                        } else {
                            ev
                        };
                        (iv, ev)
                    }
                    _ => return Err(CodegenError::new("for-in range loop requires integer type")),
                };
                let is_slt = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::SLT, i_val, end_val, "forin.cmp")
                    .map_err(|e| CodegenError::new(format!("failed for-in cmp: {e}")))?;
                self.builder
                    .build_conditional_branch(is_slt, body_bb, end_bb)
                    .map_err(|e| CodegenError::new(format!("failed for-in branch: {e}")))?;

                self.loop_stack.push((end_bb, cond_bb));
                self.loop_defers_base.push(self.defers.len());
                self.builder.position_at_end(body_bb);
                self.generate_block(body)?;
                self.loop_defers_base.pop();

                let i_val2_raw = self
                    .builder
                    .build_load(llvm_ty, i_ptr, &binding.name)
                    .map_err(|e| CodegenError::new(format!("failed to load loop var: {e}")))?;
                let i_val2 = match i_val2_raw {
                    BasicValueEnum::IntValue(iv) => iv,
                    _ => return Err(CodegenError::new("for-in range loop requires integer type")),
                };
                let one = i_val2.get_type().const_int(1, false);
                let next = self
                    .builder
                    .build_int_add(i_val2, one, "forin.incr")
                    .map_err(|e| CodegenError::new(format!("failed for-in incr: {e}")))?;
                self.builder
                    .build_store(i_ptr, next)
                    .map_err(|e| CodegenError::new(format!("failed to store incr: {e}")))?;

                let body_terminated = self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_terminator())
                    .is_some();
                if !body_terminated {
                    self.builder
                        .build_unconditional_branch(cond_bb)
                        .map_err(|e| CodegenError::new(format!("failed to loop for-in: {e}")))?;
                }
                self.loop_stack.pop();

                self.builder.position_at_end(end_bb);
                self.pop_scope();
                Ok(())
            }
            _ => {
                self.push_scope();

                let dummy_span = Span::default();
                let iterable_name = "__forin_iterable";
                let iter_name = "__forin_iter";
                let next_name = "__forin_next";

                let iterable_val = self.emit_expression_value(iterable)?;
                let iterable_llvm_ty = iterable_val.get_type();
                let iterable_ast_ty = self
                    .resolve_receiver_type(iterable)
                    .unwrap_or_else(|| self.infer_ast_type_from_value(&iterable_val, span));
                let iterable_ptr =
                    self.create_entry_alloca(function, iterable_name, iterable_llvm_ty)?;
                self.builder
                    .build_store(iterable_ptr, iterable_val)
                    .map_err(|e| CodegenError::new(format!("failed to store iterable: {e}")))?;
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        iterable_name.to_string(),
                        VarInfo {
                            ptr: iterable_ptr,
                            ty: iterable_ast_ty,
                            is_mutable: true,
                            is_volatile: false,
                            drop_flag: None,
                        },
                    );
                }

                let iterable_expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: iterable_name.to_string(),
                        span: dummy_span,
                    })),
                    span: dummy_span,
                };
                let method_name = match mode {
                    ast::IterAccessMode::ByValue => "into_iter",
                    ast::IterAccessMode::ByPtr => "into_iter_ptr",
                    ast::IterAccessMode::ByConstPtr => "into_iter_const_ptr",
                };
                let into_iter_ident = ast::Identifier {
                    name: method_name.to_string(),
                    span: dummy_span,
                };
                let iterator_val = self
                    .emit_method_call_expression(
                        &iterable_expr,
                        &into_iter_ident,
                        &[],
                        false,
                        span,
                    )?
                    .ok_or_else(|| CodegenError::new("into_iter() must not return void"))?;

                let iter_llvm_ty = iterator_val.get_type();

                // Use the typeck-resolved iterator type (carries generics), or infer from LLVM value
                let iter_ast_ty = iterator_type
                    .cloned()
                    .unwrap_or_else(|| self.infer_ast_type_from_value(&iterator_val, span));
                let iter_ptr = self.create_entry_alloca(function, iter_name, iter_llvm_ty)?;
                self.builder
                    .build_store(iter_ptr, iterator_val)
                    .map_err(|e| CodegenError::new(format!("failed to store iterator: {e}")))?;
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        iter_name.to_string(),
                        VarInfo {
                            ptr: iter_ptr,
                            ty: iter_ast_ty,
                            is_mutable: true,
                            is_volatile: false,
                            drop_flag: None,
                        },
                    );
                }

                let next_ident = ast::Identifier {
                    name: "next".to_string(),
                    span: dummy_span,
                };
                let iter_expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: iter_name.to_string(),
                        span: dummy_span,
                    })),
                    span: dummy_span,
                };
                let next_val = self
                    .emit_method_call_expression(&iter_expr, &next_ident, &[], false, span)?
                    .ok_or_else(|| CodegenError::new("next() must return Optional<T>"))?;

                let next_llvm_ty = next_val.get_type();
                let next_ptr = self.create_entry_alloca(function, next_name, next_llvm_ty)?;
                self.builder
                    .build_store(next_ptr, next_val)
                    .map_err(|e| CodegenError::new(format!("failed to store next result: {e}")))?;

                let cond_bb = self.context.append_basic_block(function, "forin.cond");
                let body_bb = self.context.append_basic_block(function, "forin.body");
                let end_bb = self.context.append_basic_block(function, "forin.end");

                self.builder
                    .build_unconditional_branch(cond_bb)
                    .map_err(|e| CodegenError::new(format!("failed to enter for-in cond: {e}")))?;

                self.builder.position_at_end(cond_bb);

                let opt_struct = self
                    .builder
                    .build_load(next_llvm_ty, next_ptr, "forin.opt")
                    .map_err(|e| CodegenError::new(format!("failed to load optional: {e}")))?;
                let opt_sv = match opt_struct {
                    BasicValueEnum::StructValue(sv) => sv,
                    _ => return Err(CodegenError::new("next() must return a struct Optional<T>")),
                };
                let is_present = self
                    .builder
                    .build_extract_value(opt_sv, 0, "forin.present")
                    .map_err(|e| CodegenError::new(format!("failed to extract present: {e}")))?;

                self.builder
                    .build_conditional_branch(is_present.into_int_value(), body_bb, end_bb)
                    .map_err(|e| CodegenError::new(format!("failed for-in branch: {e}")))?;

                self.loop_stack.push((end_bb, cond_bb));
                self.loop_defers_base.push(self.defers.len());
                self.builder.position_at_end(body_bb);

                let _item_llvm_ty = {
                    let owners = self.receiver_owner_candidates(&iter_expr);
                    let mut found = None;
                    for owner_name in &owners {
                        let mut resolved = None;
                        for mangled in
                            self.overloaded_method_candidates(owner_name, &next_ident.name)
                        {
                            if let Some(sig) = self.signature_for_name(&mangled) {
                                resolved = Some(sig);
                                break;
                            }
                        }
                        if let Some(sig) = resolved
                            && let Some(return_type) = &sig.return_type
                        {
                            let inner = match return_type.kind.as_ref() {
                                ast::TypeKind::Optional(inner) => Some(inner.as_ref()),
                                ast::TypeKind::Named(named)
                                    if named.path.last().map(|s| s.name.as_str())
                                        == Some("Optional") =>
                                {
                                    named.generics.as_ref().and_then(|g| g.first())
                                }
                                _ => None,
                            };
                            if let Some(inner_ty) = inner {
                                found = Some(self.lower_basic_type(inner_ty)?);
                                break;
                            }
                        }
                    }
                    found.unwrap_or_else(|| self.context.i64_type().as_basic_type_enum())
                };
                let thing_loaded = self
                    .builder
                    .build_extract_value(opt_sv, 1, &binding.name)
                    .map_err(|e| CodegenError::new(format!("failed to extract thing: {e}")))?;

                let ast_ty = self.infer_ast_type_from_value(&thing_loaded, span);
                let var_ptr =
                    self.create_entry_alloca(function, &binding.name, thing_loaded.get_type())?;
                self.builder
                    .build_store(var_ptr, thing_loaded)
                    .map_err(|e| CodegenError::new(format!("failed to store loop var: {e}")))?;
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        binding.name.clone(),
                        VarInfo {
                            ptr: var_ptr,
                            ty: ast_ty,
                            is_mutable,
                            is_volatile: false,
                            drop_flag: None,
                        },
                    );
                }

                self.generate_block(body)?;

                let next_val2 = self
                    .emit_method_call_expression(&iter_expr, &next_ident, &[], false, span)?
                    .ok_or_else(|| CodegenError::new("next() must return Optional<T>"))?;
                self.builder
                    .build_store(next_ptr, next_val2)
                    .map_err(|e| CodegenError::new(format!("failed to store next result: {e}")))?;

                let body_terminated = self
                    .builder
                    .get_insert_block()
                    .and_then(|bb| bb.get_terminator())
                    .is_some();
                if !body_terminated {
                    self.builder
                        .build_unconditional_branch(cond_bb)
                        .map_err(|e| CodegenError::new(format!("failed to loop for-in: {e}")))?;
                }
                self.loop_defers_base.pop();
                self.loop_stack.pop();

                self.builder.position_at_end(end_bb);
                self.pop_scope();
                Ok(())
            }
        }
    }

    pub(crate) fn emit_let_statement(
        &mut self,
        let_stmt: &ast::LetStatement,
        span: &Span,
    ) -> CodegenResult<()> {
        let ast::PatternKind::Identifier(identifier) = &let_stmt.pattern.kind else {
            return Err(CodegenError::with_span(
                "only identifier let-bindings are supported in LLVM IR codegen",
                let_stmt.pattern.span,
            ));
        };

        let (storage_ty, init_value, inferred_ty) = if let Some(init_expr) = &let_stmt.initializer {
            let mut init_value =
                if let ast::ExpressionKind::Initializer { items } = init_expr.kind.as_ref() {
                    let Some(annotation) = &let_stmt.type_annotation else {
                        return Err(CodegenError::with_span(
                            "initializer requires a type annotation in LLVM IR codegen",
                            init_expr.span,
                        ));
                    };
                    self.emit_typed_initializer_value(items, annotation, &init_expr.span)?
                } else {
                    self.emit_expression_value(init_expr)?
                };

            let storage_ty = if let Some(annotation) = &let_stmt.type_annotation {
                self.lower_basic_type(annotation)?
            } else {
                init_value.get_type()
            };
            if let Some(annotation) = &let_stmt.type_annotation {
                init_value =
                    self.cast_value_to_ast_type(init_value, annotation, &init_expr.span)?;
            }
            let inferred_ty = if let Some(annotation) = &let_stmt.type_annotation {
                annotation.clone()
            } else {
                self.infer_ast_type_from_value(&init_value, span)
            };
            (storage_ty, init_value, inferred_ty)
        } else {
            let Some(annotation) = &let_stmt.type_annotation else {
                return Err(CodegenError::with_span(
                    "let binding without initializer requires a type annotation in LLVM IR codegen",
                    *span,
                ));
            };
            let storage_ty = self.lower_basic_type(annotation)?;
            (storage_ty, storage_ty.const_zero(), annotation.clone())
        };
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for let statement"))?;

        if let_stmt.is_static {
            // Static local: function-persistent storage, initialized once, never
            // dropped (C semantics). Backed by an internal-linkage LLVM global.
            // The name is uniqued per function name and declaration ordinal so
            // shadowed `static i32 x;` declarations in nested blocks and each
            // monomorphized generic instantiation get distinct globals. No drop
            // flag, no field drops, no defer entry: the storage lives for the
            // whole program, so it is never destroyed (a static of a Drop type
            // leaks at exit by design).
            let fn_name = function.get_name().to_string_lossy().into_owned();
            let ordinal = {
                let n = self.static_local_counter;
                self.static_local_counter += 1;
                n
            };
            let global_name = format!("{fn_name}.{}.{ordinal}", identifier.name);
            let global = self.module.add_global(storage_ty, None, &global_name);
            global.set_linkage(Linkage::Internal);
            if let Some(init) = &let_stmt.initializer {
                let const_val =
                    self.emit_const_value_for_type(init, &inferred_ty)
                        .map_err(|e| {
                            if e.message
                                == "global initializer must be a compile-time constant expression"
                            {
                                CodegenError::with_span(
                                    "static local initializer must be a compile-time constant",
                                    init.span,
                                )
                            } else {
                                e
                            }
                        })?;
                global.set_initializer(&const_val);
            } // no initializer → zero-initialized (LLVM global default)
            if let Some(scope) = self.variables.last_mut() {
                scope.insert(
                    identifier.name.clone(),
                    VarInfo {
                        ptr: global.as_pointer_value(),
                        ty: inferred_ty,
                        is_mutable: let_stmt.is_mutable,
                        is_volatile: let_stmt.is_volatile,
                        drop_flag: None,
                    },
                );
            }
            return Ok(());
        }

        let alloca = self.create_entry_alloca(function, &identifier.name, storage_ty)?;
        // Large zero-initialized arrays: a store of a huge constant aggregate
        // (e.g. `store [100000 x i8] zeroinitializer`) crashes the LLVM
        // SelectionDAG combiner, so zero-fill with llvm.memset instead.
        let zero_fill_bytes = if let_stmt.initializer.is_none()
            && matches!(inferred_ty.kind.as_ref(), ast::TypeKind::Array(_))
        {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            Some(target_data.get_store_size(&storage_ty.as_any_type_enum()))
        } else {
            None
        };
        if let Some(size) = zero_fill_bytes.filter(|&size| size > 64) {
            self.build_memset(alloca, size, let_stmt.is_volatile)?;
        } else if let_stmt.is_volatile {
            self.emit_volatile_store(alloca, init_value)?;
        } else {
            self.builder.build_store(alloca, init_value).map_err(|e| {
                CodegenError::with_span(
                    format!("failed to store local `{}`: {e}", identifier.name),
                    identifier.span,
                )
            })?;
        }

        let ty = inferred_ty;
        let ty_for_drop = ty.clone();
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(
                identifier.name.clone(),
                VarInfo {
                    ptr: alloca,
                    ty,
                    is_mutable: let_stmt.is_mutable,
                    is_volatile: let_stmt.is_volatile,
                    drop_flag: None,
                },
            );
        }

        // Skip pointer/reference types: the variable is a borrowed view,
        // not an owner.  Only value-type variables get implicit destructors.
        if matches!(
            ty_for_drop.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        ) {
            return Ok(());
        }

        // Check if this variable's type implements Drop; if so, set up a
        // drop flag and register the cascade (field drops, then own drop).
        self.register_drop_flag(&identifier.name, &ty_for_drop, alloca)
    }

    pub(crate) fn infer_ast_type_from_value(
        &self,
        value: &BasicValueEnum<'ctx>,
        span: &Span,
    ) -> ast::Type {
        let kind = match value {
            BasicValueEnum::IntValue(int_value) => match int_value.get_type().get_bit_width() {
                1 => ast::TypeKind::Primitive(ast::PrimitiveType::Bool),
                8 => ast::TypeKind::Primitive(ast::PrimitiveType::I8),
                16 => ast::TypeKind::Primitive(ast::PrimitiveType::I16),
                32 => ast::TypeKind::Primitive(ast::PrimitiveType::I32),
                64 => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
                128 => ast::TypeKind::Primitive(ast::PrimitiveType::I128),
                _ => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
            },
            BasicValueEnum::FloatValue(_) => ast::TypeKind::Primitive(ast::PrimitiveType::F64),
            BasicValueEnum::PointerValue(_) => ast::TypeKind::Primitive(ast::PrimitiveType::Str),
            _ => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
        };

        ast::Type {
            kind: Box::new(kind),
            span: *span,
        }
    }

    pub(crate) fn emit_match_statement(
        &mut self,
        expression: &ast::Expression,
        arms: &[ast::MatchArm],
    ) -> CodegenResult<()> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for match expression"))?;
        let scrutinee = self.emit_expression_value(expression)?;
        let end_bb = self.context.append_basic_block(function, "match.end");
        let mut cond_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;

        for (arm_index, arm) in arms.iter().enumerate() {
            let arm_bb = self
                .context
                .append_basic_block(function, &format!("match.arm.{arm_index}"));
            let next_bb = self
                .context
                .append_basic_block(function, &format!("match.next.{arm_index}"));

            self.builder.position_at_end(cond_bb);

            if arm.guard.is_some() {
                return Err(CodegenError::with_span(
                    "match guards are not supported in LLVM IR codegen yet",
                    arm.span,
                ));
            }

            match &arm.pattern.kind {
                ast::PatternKind::Wildcard | ast::PatternKind::Identifier(_) => {
                    self.builder
                        .build_unconditional_branch(arm_bb)
                        .map_err(|e| CodegenError::new(format!("failed match branch: {e}")))?;
                }
                ast::PatternKind::Literal(literal) => {
                    let cond = match (&scrutinee, literal) {
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Integer(value)) => {
                            let rhs = lhs.get_type().const_int(*value as u64, true);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.int")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match int compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Bool(value)) => {
                            let rhs = lhs.get_type().const_int(u64::from(*value), false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.bool")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match bool compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Char(value)) => {
                            let rhs = lhs.get_type().const_int(*value as u64, false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.char")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match char compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::FloatValue(lhs), ast::Literal::Float(value)) => {
                            let rhs = lhs.get_type().const_float(*value);
                            self.builder
                                .build_float_compare(FloatPredicate::OEQ, *lhs, rhs, "match.f")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match float compare: {e}"))
                                })?
                        }
                        _ => {
                            return Err(CodegenError::with_span(
                                "unsupported match literal for scrutinee type",
                                arm.pattern.span,
                            ));
                        }
                    };

                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .map_err(|e| CodegenError::new(format!("failed match branch: {e}")))?;
                }
                _ => {
                    return Err(CodegenError::with_span(
                        "match pattern kind is not supported in LLVM IR codegen yet",
                        arm.pattern.span,
                    ));
                }
            }

            self.builder.position_at_end(arm_bb);
            self.push_scope();

            if let ast::PatternKind::Identifier(identifier) = &arm.pattern.kind {
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for match identifier binding")
                })?;
                let alloca =
                    self.create_entry_alloca(function, &identifier.name, scrutinee.get_type())?;
                self.builder.build_store(alloca, scrutinee).map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to bind match identifier `{}`: {e}", identifier.name),
                        identifier.span,
                    )
                })?;

                let inferred = self.infer_ast_type_from_value(&scrutinee, &identifier.span);
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        identifier.name.clone(),
                        VarInfo {
                            ptr: alloca,
                            ty: inferred,
                            is_mutable: false,
                            is_volatile: false,
                            drop_flag: None,
                        },
                    );
                }
            }

            self.emit_expression_statement(&arm.body)?;

            let arm_terminated = self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some();
            if !arm_terminated {
                self.builder
                    .build_unconditional_branch(end_bb)
                    .map_err(|e| CodegenError::new(format!("failed match arm end branch: {e}")))?;
            }
            self.pop_scope();

            match arm.pattern.kind {
                ast::PatternKind::Wildcard | ast::PatternKind::Identifier(_) => {
                    cond_bb = next_bb;
                    break;
                }
                _ => {
                    cond_bb = next_bb;
                }
            }
        }

        self.builder.position_at_end(cond_bb);
        let cond_terminated = self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some();
        if !cond_terminated {
            self.builder
                .build_unconditional_branch(end_bb)
                .map_err(|e| CodegenError::new(format!("failed final match branch: {e}")))?;
        }

        self.builder.position_at_end(end_bb);
        Ok(())
    }
}
