use rustc_hash::FxHashMap as HashMap;

use inkwell::AddressSpace;
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

        // Keep a frame pointer in every function so the runtime backtrace
        // walker can follow the rbp chain at any opt level.
        let frame_ptr_attr = self.context.create_string_attribute("frame-pointer", "all");
        function.add_attribute(inkwell::attributes::AttributeLoc::Function, frame_ptr_attr);

        // Record source info for the runtime backtrace table. The lexer's
        // source registry holds every file's path; spans carry the line.
        let src_line = fn_span.start_line.max(1);
        let src_file = crate::lexer::source_file(fn_span.file)
            .map(|sf| {
                std::path::Path::new(&sf.path)
                    .file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("unknown.ag")
                    .to_string()
            })
            .unwrap_or_else(|| "unknown.ag".to_string());
        self.fn_source_info
            .insert(fn_name.to_string(), (src_file, src_line));

        let nested_emission = self.debug_nested;
        let ret_di = if nested_emission {
            None
        } else {
            return_type.and_then(|rt| self.debug_type_for(rt))
        };
        if let Some(debug) = &mut self.debug {
            let (line, _col, _, _) = debug.span_to_line_col(fn_span);
            let file = debug.file_for(fn_span);
            let subroutine_type = debug.create_subroutine_type(file, ret_di, &[]);
            if nested_emission {
                // Lazily-emitted generic instances: no subprogram / debug
                // records (their scopes would dangle under LLVM 22's
                // DbgRecord DIE construction).
                debug.current_subprogram = None;
            } else {
                let subprogram = debug.create_function(
                    fn_name,
                    fn_name,
                    file,
                    line,
                    subroutine_type,
                    false,
                    true,
                    line,
                );
                function.set_subprogram(subprogram);
                debug.current_subprogram = Some(subprogram);
            }
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
                        field_flags: Vec::new(),
                    },
                );
            }
            self.emit_debug_variable(
                &param.name.name,
                &param.param_type,
                &param.name.span,
                alloca,
                Some(index as u32 + 1),
            )?;
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
            // A by-value param is initialized with the caller's value: its
            // fields hold live resources, so mark them live for the cascade.
            if let Some(var) = self.lookup_variable(&param.name.name) {
                for (_, flag) in var.field_flags {
                    self.builder
                        .build_store(flag, self.context.bool_type().const_int(1, false))
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to mark param fields: {e}"),
                                param.name.span,
                            )
                        })?;
                }
            }
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
                            ty: ast_ty.clone(),
                            is_mutable,
                            is_volatile: false,
                            drop_flag: None,
                            field_flags: Vec::new(),
                        },
                    );
                }
                self.emit_debug_variable(&binding.name, &ast_ty, span, i_ptr, None)?;

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
                self.emit_defers(1)?;
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
                if matches!(mode, ast::IterAccessMode::ByValue) {
                    if let ast::ExpressionKind::Identifier(ident) = iterable.kind.as_ref() {
                        if let Some(flag_ptr) =
                            self.lookup_variable(&ident.name).and_then(|v| v.drop_flag)
                        {
                            self.builder
                                .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                                .map_err(|e| {
                                    CodegenError::new(format!("failed to clear drop flag: {e}"))
                                })?;
                        }
                        self.clear_field_flags(&ident.name)?;
                    }
                }
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
                            field_flags: Vec::new(),
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
                            field_flags: Vec::new(),
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

                let mut found_ast_ty = _item_type.cloned();
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
                                if found_ast_ty.is_none() {
                                    found_ast_ty = Some(inner_ty.clone());
                                }
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

                let ast_ty = found_ast_ty.unwrap_or_else(|| self.infer_ast_type_from_value(&thing_loaded, span));
                let debug_ty = ast_ty.clone();
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
                            field_flags: Vec::new(),
                        },
                    );
                }
                self.emit_debug_variable(&binding.name, &debug_ty, &binding.span, var_ptr, None)?;

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
                self.emit_defers(1)?;
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
        match &let_stmt.pattern.kind {
            ast::PatternKind::Identifier(identifier) => {
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
                            self.cast_expr_to_ast_type(init_value, Some(init_expr), annotation, &init_expr.span)?;
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
                    }
                    if let Some(scope) = self.variables.last_mut() {
                        scope.insert(
                            identifier.name.clone(),
                            VarInfo {
                                ptr: global.as_pointer_value(),
                                ty: inferred_ty,
                                is_mutable: let_stmt.is_mutable,
                                is_volatile: let_stmt.is_volatile,
                                drop_flag: None,
                                field_flags: Vec::new(),
                            },
                        );
                    }
                    return Ok(());
                }

                self.bind_local_variable(
                    function,
                    identifier,
                    inferred_ty,
                    storage_ty,
                    init_value,
                    let_stmt.is_mutable,
                    let_stmt.is_volatile,
                    let_stmt.initializer.is_some(),
                )?;
                Ok(())
            }
            ast::PatternKind::Tuple(sub_patterns) => {
                let Some(init_expr) = &let_stmt.initializer else {
                    return Err(CodegenError::with_span(
                        "destructuring let statement requires an initializer",
                        *span,
                    ));
                };
                let function = self
                    .current_fn
                    .ok_or_else(|| CodegenError::new("no active function for let statement"))?;

                let tuple_val = self.emit_expression_value(init_expr)?;
                self.emit_tuple_destructuring(
                    function,
                    sub_patterns,
                    tuple_val,
                    let_stmt.type_annotation.as_ref(),
                    let_stmt.is_mutable,
                    let_stmt.is_volatile,
                    span,
                )?;
                Ok(())
            }
            _ => Err(CodegenError::with_span(
                "unsupported pattern in let statement",
                let_stmt.pattern.span,
            )),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn bind_local_variable(
        &mut self,
        function: inkwell::values::FunctionValue<'ctx>,
        identifier: &ast::Identifier,
        inferred_ty: ast::Type,
        storage_ty: inkwell::types::BasicTypeEnum<'ctx>,
        init_value: BasicValueEnum<'ctx>,
        is_mutable: bool,
        is_volatile: bool,
        has_initializer: bool,
    ) -> CodegenResult<()> {
        let alloca = self.create_entry_alloca(function, &identifier.name, storage_ty)?;
        let zero_fill_bytes = if !has_initializer
            && matches!(inferred_ty.kind.as_ref(), ast::TypeKind::Array(_))
        {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            Some(target_data.get_store_size(&storage_ty.as_any_type_enum()))
        } else {
            None
        };
        if let Some(size) = zero_fill_bytes.filter(|&size| size > 64) {
            self.build_memset(alloca, size, is_volatile)?;
        } else if is_volatile {
            self.emit_volatile_store(alloca, init_value)?;
        } else {
            self.builder.build_store(alloca, init_value).map_err(|e| {
                CodegenError::with_span(
                    format!("failed to store local `{}`: {e}", identifier.name),
                    identifier.span,
                )
            })?;
        }

        let init_identifier = identifier.name.clone();
        let ty = inferred_ty;
        let ty_for_drop = ty.clone();
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(
                identifier.name.clone(),
                VarInfo {
                    ptr: alloca,
                    ty,
                    is_mutable,
                    is_volatile,
                    drop_flag: None,
                    field_flags: Vec::new(),
                },
            );
        }

        self.emit_debug_variable(
            &identifier.name,
            &ty_for_drop,
            &identifier.span,
            alloca,
            None,
        )?;

        if matches!(
            ty_for_drop.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        ) {
            return Ok(());
        }

        self.register_drop_flag(&identifier.name, &ty_for_drop, alloca)?;
        if let Some(var) = self.lookup_variable(&init_identifier) {
            if has_initializer {
                for (_, flag) in var.field_flags {
                    self.builder
                        .build_store(flag, self.context.bool_type().const_int(1, false))
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to mark initialized fields: {e}"),
                                identifier.span,
                            )
                        })?;
                }
            } else if let Some(flag) = var.drop_flag {
                self.builder
                    .build_store(flag, self.context.bool_type().const_int(0, false))
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to clear uninitialized drop flag: {e}"),
                            identifier.span,
                        )
                    })?;
            }
        }
        Ok(())
    }

    fn emit_tuple_destructuring(
        &mut self,
        function: inkwell::values::FunctionValue<'ctx>,
        sub_patterns: &[ast::Pattern],
        tuple_val: BasicValueEnum<'ctx>,
        annotation: Option<&ast::Type>,
        is_mutable: bool,
        is_volatile: bool,
        span: &Span,
    ) -> CodegenResult<()> {
        let elem_annotations = match annotation.and_then(|a| match a.kind.as_ref() {
            ast::TypeKind::Tuple(types) => Some(types),
            _ => None,
        }) {
            Some(types) => types.iter().map(Some).collect::<Vec<_>>(),
            None => vec![None; sub_patterns.len()],
        };

        for (i, sub_pat) in sub_patterns.iter().enumerate() {
            let elem_val = if tuple_val.is_struct_value() {
                self.builder
                    .build_extract_value(
                        tuple_val.into_struct_value(),
                        i as u32,
                        &format!("tuple.ext.{i}"),
                    )
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to extract tuple element {i}: {e}"),
                            *span,
                        )
                    })?
            } else if tuple_val.is_pointer_value() {
                let ptr = tuple_val.into_pointer_value();
                let struct_ty = self.lower_basic_type(annotation.unwrap())?.into_struct_type();
                let elem_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, ptr, i as u32, &format!("tuple.gep.{i}"))
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to GEP tuple element {i}: {e}"),
                            *span,
                        )
                    })?;
                let elem_ty = struct_ty.get_field_type_at_index(i as u32).unwrap();
                self.builder
                    .build_load(elem_ty, elem_ptr, &format!("tuple.load.{i}"))
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load tuple element {i}: {e}"),
                            *span,
                        )
                    })?
            } else {
                return Err(CodegenError::with_span(
                    "cannot destructure non-aggregate value",
                    *span,
                ));
            };

            let elem_annotation = elem_annotations.get(i).copied().flatten();
            let mut elem_val = elem_val;
            if let Some(annot) = elem_annotation {
                elem_val = self.cast_value_to_ast_type(elem_val, annot, &sub_pat.span)?;
            }
            let elem_ast_type = if let Some(annot) = elem_annotation {
                annot.clone()
            } else {
                self.infer_ast_type_from_value(&elem_val, &sub_pat.span)
            };

            match &sub_pat.kind {
                ast::PatternKind::Identifier(ident) => {
                    self.bind_local_variable(
                        function,
                        ident,
                        elem_ast_type,
                        elem_val.get_type(),
                        elem_val,
                        is_mutable,
                        is_volatile,
                        true,
                    )?;
                }
                ast::PatternKind::Wildcard => {}
                ast::PatternKind::Tuple(nested) => {
                    self.emit_tuple_destructuring(
                        function,
                        nested,
                        elem_val,
                        elem_annotation,
                        is_mutable,
                        is_volatile,
                        &sub_pat.span,
                    )?;
                }
                _ => {
                    return Err(CodegenError::with_span(
                        "unsupported destructuring sub-pattern",
                        sub_pat.span,
                    ));
                }
            }
        }
        Ok(())
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
            BasicValueEnum::StructValue(sv) => {
                if let Some(c_name) = sv.get_type().get_name() {
                    let name_str = c_name.to_str().unwrap_or("");
                    let clean = name_str.strip_prefix("struct.").unwrap_or(name_str);
                    if !clean.is_empty() {
                        ast::TypeKind::Named(ast::NamedType {
                            path: vec![ast::Identifier {
                                name: clean.to_string(),
                                span: *span,
                            }],
                            generics: None,
                        })
                    } else {
                        ast::TypeKind::Primitive(ast::PrimitiveType::I64)
                    }
                } else {
                    ast::TypeKind::Primitive(ast::PrimitiveType::I64)
                }
            }
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

            match &arm.pattern.kind {
                ast::PatternKind::Wildcard
                | ast::PatternKind::Identifier(_)
                | ast::PatternKind::Move(_) => {
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
                ast::PatternKind::Enum {
                    path,
                    variant,
                    data,
                } => {
                    // Resolve the enum name: either the explicit `Enum.Variant`
                    // path, or (for bare `Variant` patterns) the scrutinee's
                    // enum type, which typeck already validated.
                    let (enum_name, concrete_name) = if path.len() == 1 {
                        (path[0].name.clone(), path[0].name.clone())
                    } else if let Some(ty) = self.resolve_receiver_type(expression)
                        && let Some(named) = Self::extract_named_type_owned(&ty)
                        && named.path.len() == 1
                    {
                        let base_name = named.path[0].name.clone();
                        let monomorph = Self::monomorph_owner_name_from_named(&named);
                        let conc = if self.enum_payload_layouts.contains_key(&monomorph)
                            || self.enum_variant_payload_types.contains_key(&monomorph)
                        {
                            monomorph
                        } else {
                            base_name.clone()
                        };
                        (base_name, conc)
                    } else {
                        return Err(CodegenError::with_span(
                            "enum type path must be a single name in match".to_string(),
                            arm.pattern.span,
                        ));
                    };
                    if let Some(struct_ty) = self.enum_payload_layouts.get(&concrete_name).cloned()
                    {
                        let function = self.current_fn.ok_or_else(|| {
                            CodegenError::new("no active function for match enum")
                        })?;
                        let scrut_ptr = self.create_entry_alloca(
                            function,
                            "match.scrut.ptr",
                            struct_ty.as_basic_type_enum(),
                        )?;
                        let zero_struct = struct_ty.const_zero();
                        self.builder
                            .build_store(scrut_ptr, zero_struct)
                            .map_err(|e| {
                                CodegenError::new(format!("zero init match scrutinee: {e}"))
                            })?;
                        self.builder
                            .build_store(scrut_ptr, scrutinee)
                            .map_err(|e| {
                                CodegenError::new(format!("store match scrutinee: {e}"))
                            })?;
                        let tag_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, scrut_ptr, 0, "match.tag.ptr")
                            .map_err(|e| CodegenError::new(format!("match tag GEP: {e}")))?;
                        let tag_load = self
                            .builder
                            .build_load(self.context.i16_type(), tag_ptr, "match.tag")
                            .map_err(|e| CodegenError::new(format!("match tag load: {e}")))?;
                        let tag_val = match tag_load {
                            BasicValueEnum::IntValue(v) => v,
                            _ => {
                                return Err(CodegenError::new(
                                    "enum tag is not an i16 value".to_string(),
                                ));
                            }
                        };
                        let expected_tag =
                            if let Some(variants) = self.enum_variants.get(&enum_name) {
                                if let Some(val) = variants.get(&variant.name) {
                                    self.context.i16_type().const_int(*val as u64, false)
                                } else {
                                    return Err(CodegenError::with_span(
                                        format!(
                                            "unknown variant '{}' in enum '{}'",
                                            variant.name, enum_name
                                        ),
                                        variant.span,
                                    ));
                                }
                            } else {
                                return Err(CodegenError::with_span(
                                    format!("unknown enum '{}' in match", enum_name),
                                    arm.pattern.span,
                                ));
                            };
                        let cond = self
                            .builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                tag_val,
                                expected_tag,
                                "match.enum.tag",
                            )
                            .map_err(|e| {
                                CodegenError::new(format!("match enum tag compare: {e}"))
                            })?;
                        self.builder
                            .build_conditional_branch(cond, arm_bb, next_bb)
                            .map_err(|e| CodegenError::new(format!("match enum branch: {e}")))?;
                        self.builder.position_at_end(arm_bb);
                        self.push_scope();
                        if let Some(data_pattern) = data {
                            let payload_types = self
                                .enum_variant_payload_types
                                .get(&concrete_name)
                                .and_then(|m| m.get(&variant.name))
                                .cloned()
                                .unwrap_or_default();
                            let substitutions: HashMap<String, ast::Type> = if let Some(ty) =
                                self.resolve_receiver_type(expression)
                                && let Some(named) = Self::extract_named_type_owned(&ty)
                                && let Some(params) = self.struct_generics.get(&enum_name)
                                && let Some(args) = &named.generics
                                && params.len() == args.len()
                            {
                                params.iter().cloned().zip(args.iter().cloned()).collect()
                            } else {
                                HashMap::default()
                            };
                            let bindings: Vec<Option<(&ast::Identifier, bool)>> =
                                match &data_pattern.kind {
                                    ast::PatternKind::Identifier(binding) => {
                                        vec![Some((binding, false))]
                                    }
                                    ast::PatternKind::Move(binding) => {
                                        vec![Some((binding, true))]
                                    }
                                    ast::PatternKind::Tuple(items) => items
                                        .iter()
                                        .map(|item| match &item.kind {
                                            ast::PatternKind::Identifier(binding) => {
                                                Some((binding, false))
                                            }
                                            ast::PatternKind::Move(binding) => {
                                                Some((binding, true))
                                            }
                                            _ => None,
                                        })
                                        .collect(),
                                    _ => Vec::new(),
                                };
                            if !bindings.is_empty() {
                                let data_ptr = self
                                    .builder
                                    .build_struct_gep(struct_ty, scrut_ptr, 1, "match.data.ptr")
                                    .map_err(|e| {
                                        CodegenError::new(format!("match data GEP: {e}"))
                                    })?;
                                let target_data = TargetData::create(
                                    self.module.get_data_layout().as_str().to_str().unwrap(),
                                );
                                let mut byte_offset: u32 = 0;
                                for (i, pt) in payload_types.iter().enumerate() {
                                    let concrete_pt = if substitutions.is_empty() {
                                        pt.clone()
                                    } else {
                                        Self::substitute_generic_type(pt, &substitutions)
                                    };
                                    let llvm_ty = self.lower_basic_type(&concrete_pt)?;
                                    let binding = bindings.get(i).copied().flatten();
                                    if let Some((binding, is_move)) = binding {
                                        let field_ptr =
                                            if byte_offset == 0 {
                                                data_ptr
                                            } else {
                                                unsafe {
                                                    self.builder.build_gep(
                                                        self.context.i8_type(),
                                                        data_ptr,
                                                        &[self
                                                            .context
                                                            .i32_type()
                                                            .const_int(byte_offset as u64, false)],
                                                        "match.field.gep",
                                                    )
                                                }
                                                .map_err(|e| {
                                                    CodegenError::new(format!(
                                                        "GEP match field: {e}"
                                                    ))
                                                })?
                                            };
                                        let cast_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                field_ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "data.cast",
                                            )
                                            .map_err(|e| {
                                                CodegenError::new(format!("pointer cast: {e}"))
                                            })?;
                                        let loaded = self
                                            .builder
                                            .build_load(llvm_ty, cast_ptr, &binding.name)
                                            .map_err(|e| {
                                                CodegenError::new(format!("load data payload: {e}"))
                                            })?;
                                        let alloca = self.create_entry_alloca(
                                            function,
                                            &binding.name,
                                            llvm_ty,
                                        )?;
                                        self.builder.build_store(alloca, loaded).map_err(|e| {
                                            CodegenError::new(format!("store data binding: {e}"))
                                        })?;
                                        self.emit_debug_variable(
                                            &binding.name,
                                            &concrete_pt,
                                            &binding.span,
                                            alloca,
                                            None,
                                        )?;
                                        if let Some(scope) = self.variables.last_mut() {
                                            scope.insert(
                                                binding.name.clone(),
                                                VarInfo {
                                                    ptr: alloca,
                                                    ty: concrete_pt.clone(),
                                                    is_mutable: false,
                                                    is_volatile: false,
                                                    drop_flag: None,
                                                    field_flags: Vec::new(),
                                                },
                                            );
                                        }
                                        self.register_drop_flag(
                                            &binding.name,
                                            &concrete_pt,
                                            alloca,
                                        )?;
                                        if is_move
                                            && let Ok((orig_ptr, _)) =
                                                self.resolve_lvalue_ptr(expression)
                                        {
                                            if let ast::ExpressionKind::Identifier(ident) =
                                                expression.kind.as_ref()
                                                && let Some(var) = self.lookup_variable(&ident.name)
                                                && let Some(flag) = var.drop_flag
                                            {
                                                self.builder
                                                    .build_store(
                                                        flag,
                                                        self.context
                                                            .bool_type()
                                                            .const_int(0, false),
                                                    )
                                                    .map_err(|e| {
                                                        CodegenError::new(format!(
                                                            "clear moved-out flag: {e}"
                                                        ))
                                                    })?;
                                            }
                                            let orig_data = self
                                                .builder
                                                .build_struct_gep(
                                                    struct_ty,
                                                    orig_ptr,
                                                    1,
                                                    "match.orig.data",
                                                )
                                                .map_err(|e| {
                                                    CodegenError::new(format!("orig data GEP: {e}"))
                                                })?;
                                            let slot_ptr = if byte_offset == 0 {
                                                orig_data
                                            } else {
                                                unsafe {
                                                    self.builder.build_gep(
                                                        self.context.i8_type(),
                                                        orig_data,
                                                        &[self
                                                            .context
                                                            .i32_type()
                                                            .const_int(byte_offset as u64, false)],
                                                        "match.orig.slot",
                                                    )
                                                }
                                                .map_err(|e| {
                                                    CodegenError::new(format!("orig slot GEP: {e}"))
                                                })?
                                            };
                                            self.builder
                                                .build_store(slot_ptr, llvm_ty.const_zero())
                                                .map_err(|e| {
                                                    CodegenError::new(format!(
                                                        "zero orig payload: {e}"
                                                    ))
                                                })?;
                                        }
                                    }
                                    byte_offset += target_data.get_abi_size(&llvm_ty) as u32;
                                }
                            }
                        }
                    } else {
                        // Unit enum — compare integer discriminant
                        if let BasicValueEnum::IntValue(scrutinee_int) = scrutinee {
                            let expected =
                                if let Some(variants) = self.enum_variants.get(&enum_name) {
                                    if let Some(val) = variants.get(&variant.name) {
                                        scrutinee_int.get_type().const_int(*val as u64, true)
                                    } else {
                                        return Err(CodegenError::with_span(
                                            format!(
                                                "unknown variant '{}' in enum '{}'",
                                                variant.name, enum_name
                                            ),
                                            variant.span,
                                        ));
                                    }
                                } else {
                                    return Err(CodegenError::with_span(
                                        format!("unknown enum '{}' in match", enum_name),
                                        arm.pattern.span,
                                    ));
                                };
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    scrutinee_int,
                                    expected,
                                    "match.enum.disc",
                                )
                                .map_err(|e| {
                                    CodegenError::new(format!("match enum disc compare: {e}"))
                                })?;
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .map_err(|e| {
                                    CodegenError::new(format!("match enum branch: {e}"))
                                })?;
                            self.builder.position_at_end(arm_bb);
                            self.push_scope();
                        } else {
                            return Err(CodegenError::with_span(
                                "unit enum match requires integer scrutinee".to_string(),
                                arm.pattern.span,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(CodegenError::with_span(
                        "match pattern kind is not supported in LLVM IR codegen yet",
                        arm.pattern.span,
                    ));
                }
            }

            if !matches!(arm.pattern.kind, ast::PatternKind::Enum { .. }) {
                self.builder.position_at_end(arm_bb);
                self.push_scope();
            }

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
                            field_flags: Vec::new(),
                        },
                    );
                }
            }

            if let Some(guard) = &arm.guard {
                let guard_val = self.emit_expression_value(guard)?;
                let guard_cond = match guard_val {
                    BasicValueEnum::IntValue(v) => v,
                    _ => {
                        return Err(CodegenError::with_span(
                            "match guard must evaluate to a boolean condition",
                            guard.span,
                        ));
                    }
                };
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for match guard")
                })?;
                let body_bb = self
                    .context
                    .append_basic_block(function, &format!("match.body.{arm_index}"));
                self.builder
                    .build_conditional_branch(guard_cond, body_bb, next_bb)
                    .map_err(|e| CodegenError::new(format!("failed match guard branch: {e}")))?;
                self.builder.position_at_end(body_bb);
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
                ast::PatternKind::Wildcard
                | ast::PatternKind::Identifier(_)
                | ast::PatternKind::Move(_) => {
                    cond_bb = next_bb;
                    if arm.guard.is_none() {
                        break;
                    }
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
