use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue};

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::{DeferAction, DeferredEntry};
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::types::Type;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn emit_expression_statement(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<()> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let _ = self.emit_call_expression(function, arguments, true, &expr.span)?;
                Ok(())
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let _ = self
                    .emit_method_call_expression(receiver, method, arguments, true, &expr.span)?;
                Ok(())
            }
            _ => {
                let _ = self.emit_expression_value(expr)?;
                Ok(())
            }
        }
    }

    pub(crate) fn emit_indirect_call_expression(
        &mut self,
        fn_ptr_val: PointerValue<'ctx>,
        func_type: &ast::FunctionType,
        arguments: &[ast::Expression],
        allow_void: bool,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let fn_type = self.lower_function_type(
            &func_type.parameters,
            Some(&func_type.return_type),
            false,
            None,
        )?;

        let declared_param_count = func_type.parameters.len();

        let mut args = Vec::with_capacity(arguments.len());
        for (index, argument) in arguments.iter().enumerate() {
            let mut value = self.emit_expression_value(argument)?;
            if index < declared_param_count {
                if let Some(casted) = self.try_apply_user_cast(
                    value,
                    argument,
                    &func_type.parameters[index],
                    &argument.span,
                )? {
                    value = casted;
                } else {
                    value = self.cast_value_to_ast_type(
                        value,
                        &func_type.parameters[index],
                        &argument.span,
                    )?;
                }
            }
            args.push(BasicMetadataValueEnum::from(value));
        }

        let call = self
            .builder
            .build_indirect_call(fn_type, fn_ptr_val, &args, "calltmp")
            .map_err(|e| CodegenError::new(format!("failed to emit indirect call: {e}")))?;

        if let Some(value) = call.try_as_basic_value().basic() {
            Ok(Some(value))
        } else if allow_void {
            Ok(None)
        } else {
            Err(CodegenError::with_span(
                "void function call cannot be used as a value",
                span.clone(),
            ))
        }
    }

    pub(crate) fn emit_call_expression(
        &mut self,
        function_expr: &ast::Expression,
        arguments: &[ast::Expression],
        allow_void: bool,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let is_indirect =
            if let ast::ExpressionKind::Identifier(identifier) = function_expr.kind.as_ref() {
                self.lookup_storage(&identifier.name)
                    .and_then(|(_, ty)| match ty.kind.as_ref() {
                        ast::TypeKind::Function(f) => Some(f.clone()),
                        ast::TypeKind::Pointer(p) => {
                            if let ast::TypeKind::Function(f) = p.inner.kind.as_ref() {
                                Some(f.clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
            } else {
                self.resolve_receiver_type(function_expr)
                    .and_then(|ty| match ty.kind.as_ref() {
                        ast::TypeKind::Function(f) => Some(f.clone()),
                        ast::TypeKind::Pointer(p) => {
                            if let ast::TypeKind::Function(f) = p.inner.kind.as_ref() {
                                Some(f.clone())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    })
            };

        if let Some(func_type) = is_indirect {
            let fn_ptr = self.emit_expression_value(function_expr)?;
            let fn_ptr_val = fn_ptr.into_pointer_value();
            return self.emit_indirect_call_expression(
                fn_ptr_val, &func_type, arguments, allow_void, span,
            );
        }

        let (fn_name, explicit_generics) = match function_expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => (identifier.name.clone(), None),
            ast::ExpressionKind::TypeName(ty) => {
                if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                    if named.path.len() == 1 {
                        (named.path[0].name.clone(), named.generics.clone())
                    } else {
                        return Err(CodegenError::with_span(
                            "only direct function calls are supported in LLVM IR codegen",
                            function_expr.span.clone(),
                        ));
                    }
                } else {
                    return Err(CodegenError::with_span(
                        "only direct function calls are supported in LLVM IR codegen",
                        function_expr.span.clone(),
                    ));
                }
            }
            _ => {
                return Err(CodegenError::with_span(
                    "only direct function calls are supported in LLVM IR codegen",
                    function_expr.span.clone(),
                ));
            }
        };

        let llvm_name = if let Some(generics) = explicit_generics {
            if !generics.is_empty() {
                let type_args: Vec<Type> = generics.iter().map(Type::from_ast).collect();
                let mangled = crate::semantic::monomorph::mangle_name(&fn_name, &type_args);
                self.imported_function_links
                    .get(&mangled)
                    .cloned()
                    .unwrap_or(mangled)
            } else {
                self.imported_function_links
                    .get(&fn_name)
                    .cloned()
                    .unwrap_or_else(|| fn_name.clone())
            }
        } else {
            self.imported_function_links
                .get(&fn_name)
                .cloned()
                .unwrap_or_else(|| fn_name.clone())
        };
        let function = self.module.get_function(&llvm_name).ok_or_else(|| {
            CodegenError::with_span(
                format!("unknown function `{}`", fn_name),
                function_expr.span.clone(),
            )
        })?;
        let signature = self
            .signature_for_name(&llvm_name)
            .or_else(|| self.signature_for_name(&fn_name));
        let declared_param_count = signature
            .as_ref()
            .map(|sig| sig.params.len())
            .unwrap_or_else(|| function.get_type().get_param_types().len());
        let is_variadic = signature
            .as_ref()
            .map(|sig| sig.is_variadic)
            .unwrap_or_else(|| function.get_type().is_var_arg());

        let mut args = Vec::with_capacity(arguments.len());
        for (index, argument) in arguments.iter().enumerate() {
            let mut value = self.emit_expression_value(argument)?;
            if index < declared_param_count {
                if let Some(signature) = &signature {
                    // Prefer a user-defined cast method (e.g. a struct arg that
                    // `cast i32`s into an i32 parameter) over builtin casts.
                    if let Some(casted) = self.try_apply_user_cast(
                        value,
                        argument,
                        &signature.params[index],
                        &argument.span,
                    )? {
                        value = casted;
                    } else {
                        value = self.cast_value_to_ast_type(
                            value,
                            &signature.params[index],
                            &argument.span,
                        )?;
                    }

                    if let Some(linkage) = &signature.linkage {
                        value =
                            self.coerce_value_to_abi(value, &signature.params[index], linkage)?;
                    }
                }
            } else if is_variadic {
                value = self.apply_variadic_default_promotion(value, &argument.span)?;
            }
            args.push(BasicMetadataValueEnum::from(value));
        }

        let call = self
            .builder
            .build_call(function, &args, "calltmp")
            .map_err(|e| CodegenError::new(format!("failed to emit call: {e}")))?;

        // Add byval attributes to call site for large struct arguments
        if let Some(sig) = &signature
            && let Some(linkage) = &sig.linkage
            && matches!(linkage, ast::ExternLinkage::C)
        {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            for (index, param_ty) in sig.params.iter().enumerate() {
                let lowered = self.lower_basic_type(param_ty)?;
                if let BasicTypeEnum::StructType(struct_ty) = lowered {
                    let size = target_data.get_store_size(&struct_ty);
                    if self.abi_handler.needs_byval(size) {
                        let byval_kind = Attribute::get_named_enum_kind_id("byval");
                        let attr = self
                            .context
                            .create_type_attribute(byval_kind, struct_ty.as_any_type_enum());
                        call.add_attribute(AttributeLoc::Param(index as u32), attr);

                        let align = self.abi_handler.byval_alignment(struct_ty, &target_data);
                        let align_kind = Attribute::get_named_enum_kind_id("align");
                        let align_attr = self.context.create_enum_attribute(align_kind, align);
                        call.add_attribute(AttributeLoc::Param(index as u32), align_attr);
                    }
                }
            }
        }
        if let Some(value) = call.try_as_basic_value().basic() {
            if let Some(signature) = &signature
                && let (Some(ret_ty), Some(linkage)) = (&signature.return_type, &signature.linkage)
            {
                return Ok(Some(self.uncoerce_value_from_abi(value, ret_ty, linkage)?));
            }
            Ok(Some(value))
        } else if allow_void {
            Ok(None)
        } else {
            Err(CodegenError::with_span(
                "void function call cannot be used as a value",
                span.clone(),
            ))
        }
    }

    /// Lowers a method call to a regular function call.
    ///
    /// Resolution strategy:
    /// 1) try `<Owner>__<method>` (mangled impl function)
    /// 2) fall back to `<method>`
    ///
    /// Receiver is passed either by value or pointer depending on the
    /// collected impl metadata / function signature.

    /// True when the callee runs a destructor on this by-value parameter at
    /// function exit: a non-pointer/reference type with a Drop impl.
    pub(crate) fn param_type_drops_on_exit(&mut self, ty: &ast::Type) -> bool {
        if matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        ) {
            return false;
        }
        self.get_drop_function_name(ty)
            .map(|drop_fn| drop_fn.is_some())
            .unwrap_or(false)
    }

    /// Clear the tracked drop flag of `expr` when it names a local or
    /// parameter: ownership of the value is being transferred to a by-value
    /// callee that will run the destructor on its copy.
    pub(crate) fn clear_drop_flag_of(&mut self, expr: &ast::Expression) -> CodegenResult<()> {
        if let ast::ExpressionKind::Identifier(ident) = expr.kind.as_ref()
            && let Some(flag_ptr) = self.drop_flags.get(&ident.name).copied()
        {
            self.builder
                .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                .map_err(|e| CodegenError::new(format!("failed to clear drop flag: {e}")))?;
        }
        Ok(())
    }

    pub(crate) fn emit_method_call_expression(
        &mut self,
        receiver: &ast::Expression,
        method: &ast::Identifier,
        arguments: &[ast::Expression],
        allow_void: bool,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let owners = self.receiver_owner_candidates(receiver);
        let mut candidates = Vec::new();
        for owner_name in &owners {
            candidates.push(Self::mangle_method_name(owner_name, &method.name));
        }
        candidates.push(method.name.clone());

        let mut selected_name = None;
        let mut selected_fn = None;
        for name in candidates {
            if let Some(function) = self.module.get_function(&name) {
                selected_name = Some(name);
                selected_fn = Some(function);
                break;
            }
        }

        let function = if let Some(function) = selected_fn {
            function
        } else {
            let receiver_ty = self.resolve_receiver_type(receiver);

            if let Some(receiver_ty) = receiver_ty {
                if let Some(instantiated_name) =
                    self.try_instantiate_generic_impl_method_for_type(&receiver_ty, &method.name)?
                {
                    self.module
                        .get_function(&instantiated_name)
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                format!("failed to materialize method `{}`", method.name),
                                method.span.clone(),
                            )
                        })?
                } else {
                    return Err(CodegenError::with_span(
                        format!("unknown method `{}`", method.name),
                        method.span.clone(),
                    ));
                }
            } else {
                return Err(CodegenError::with_span(
                    format!("unknown method `{}`", method.name),
                    method.span.clone(),
                ));
            }
        };

        let call_name = selected_name.unwrap_or_else(|| method.name.clone());
        let signature = self.signature_for_name(&call_name);
        let declared_param_count = signature
            .as_ref()
            .map(|sig| sig.params.len())
            .unwrap_or_else(|| function.get_type().get_param_types().len());
        let is_variadic = signature
            .as_ref()
            .map(|sig| sig.is_variadic)
            .unwrap_or_else(|| function.get_type().is_var_arg());

        let inferred_param_count = function.get_type().get_param_types().len();
        let inject_receiver = signature
            .as_ref()
            .map(|sig| sig.params.len() == arguments.len() + 1)
            .unwrap_or(inferred_param_count == arguments.len() + 1);

        let mut args = Vec::with_capacity(arguments.len() + usize::from(inject_receiver));
        if inject_receiver {
            let receiver_ty = self.resolve_receiver_type(receiver);
            let receiver_is_pointer = receiver_ty
                .as_ref()
                .map(|ty| matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(_)))
                .unwrap_or(false);

            let expects_ref = signature
                .as_ref()
                .and_then(|sig| sig.params.first())
                .map(|first| matches!(first.kind.as_ref(), ast::TypeKind::Pointer(_)))
                .unwrap_or_else(|| {
                    function
                        .get_type()
                        .get_param_types()
                        .first()
                        .map(|param| param.is_pointer_type())
                        .unwrap_or(false)
                });
            let expected_receiver_llvm_ty = if expects_ref {
                None
            } else if let Some(signature) = &signature {
                if let Some(first_param) = signature.params.first() {
                    Some(self.lower_basic_type(first_param)?)
                } else {
                    None
                }
            } else {
                None
            };

            let receiver_arg = if expects_ref {
                if receiver_is_pointer {
                    self.emit_expression_value(receiver)?
                } else {
                    if let Ok((ptr, _)) = self.resolve_lvalue_ptr(receiver) {
                        ptr.as_basic_value_enum()
                    } else {
                        let value = self.emit_expression_value(receiver)?;
                        let function_ctx = self.current_fn.ok_or_else(|| {
                            CodegenError::new("no active function for method call")
                        })?;
                        let temp = self.create_entry_alloca(
                            function_ctx,
                            "method.recv.tmp",
                            value.get_type(),
                        )?;
                        self.builder.build_store(temp, value).map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to spill receiver for method call: {e}"),
                                receiver.span.clone(),
                            )
                        })?;

                        // If the spilled temporary owns heap memory (implements Drop),
                        // register a deferred cleanup so it doesn't leak.  Without this,
                        // chained calls like s.trim().replace(...).split(...) leak the
                        // intermediate String buffers.
                        if let Some(ref receiver_ty) = receiver_ty {
                            if !receiver_is_pointer {
                                if let Some(drop_fn_name) =
                                    self.get_drop_function_name(receiver_ty)?
                                {
                                    let flag_name =
                                        format!("method.recv.tmp.{}.drop", self.temp_counter);
                                    self.temp_counter += 1;
                                    let flag_alloca = self.create_entry_alloca(
                                        function_ctx,
                                        &flag_name,
                                        self.context.bool_type().as_basic_type_enum(),
                                    )?;
                                    self.builder
                                        .build_store(
                                            flag_alloca,
                                            self.context.bool_type().const_int(1, false),
                                        )
                                        .map_err(|e| {
                                            CodegenError::new(format!(
                                                "failed to init temp drop flag: {e}"
                                            ))
                                        })?;
                                    if let Some(scope) = self.defers.last_mut() {
                                        scope.push(DeferredEntry {
                                            action: DeferAction::DropCall(drop_fn_name, temp),
                                            flag: Some(flag_alloca),
                                        });
                                    }
                                }
                            }
                        }

                        temp.as_basic_value_enum()
                    }
                }
            } else {
                if receiver_is_pointer {
                    let receiver_value = self.emit_expression_value(receiver)?;
                    let BasicValueEnum::PointerValue(receiver_ptr) = receiver_value else {
                        return Err(CodegenError::with_span(
                            "pointer receiver did not lower to a pointer",
                            receiver.span.clone(),
                        ));
                    };
                    if let Some(expected_receiver_ty) = expected_receiver_llvm_ty {
                        self.builder
                            .build_load(expected_receiver_ty, receiver_ptr, "method.recv.load")
                            .map(|value| value.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load pointer receiver: {e}"),
                                    receiver.span.clone(),
                                )
                            })?
                    } else {
                        receiver_value
                    }
                } else {
                    let value = self.emit_expression_value(receiver)?;
                    // By-value receiver of a Drop type: the callee's parameter
                    // destructor runs on function exit, so ownership transfers
                    // to the callee. Clear the caller's drop flag, otherwise
                    // both the callee's copy and the original free the same
                    // resources (double free). Extern functions never drop
                    // their parameters and are excluded.
                    if let Some(signature) = &signature
                        && signature.linkage.is_none()
                        && signature
                            .params
                            .first()
                            .is_some_and(|ty| self.param_type_drops_on_exit(ty))
                    {
                        self.clear_drop_flag_of(receiver)?;
                    }
                    value
                }
            };

            let receiver_arg = if let Some(signature) = &signature {
                if let Some(first_param) = signature.params.first() {
                    // Prefer a user-defined cast method for the receiver.
                    let cast_arg = if let Some(casted) = self.try_apply_user_cast(
                        receiver_arg,
                        receiver,
                        first_param,
                        &receiver.span,
                    )? {
                        casted
                    } else {
                        self.cast_value_to_ast_type(receiver_arg, first_param, &receiver.span)?
                    };
                    // Apply ABI coercion for extern methods
                    if let Some(linkage) = &signature.linkage {
                        self.coerce_value_to_abi(cast_arg, first_param, linkage)?
                    } else {
                        cast_arg
                    }
                } else {
                    receiver_arg
                }
            } else {
                receiver_arg
            };
            args.push(BasicMetadataValueEnum::from(receiver_arg));
        }

        for (index, argument) in arguments.iter().enumerate() {
            let mut value = self.emit_expression_value(argument)?;
            let param_index = index + usize::from(inject_receiver);
            if param_index < declared_param_count {
                if let Some(signature) = &signature {
                    // By-value argument of a Drop type: the callee's
                    // parameter destructor runs on exit, transferring
                    // ownership. Clear the caller's flag to avoid a
                    // double free; extern functions never drop params.
                    if signature.linkage.is_none()
                        && param_index < signature.params.len()
                        && self.param_type_drops_on_exit(&signature.params[param_index])
                    {
                        self.clear_drop_flag_of(argument)?;
                    }
                    // Prefer a user-defined cast method over builtin casts.
                    if let Some(casted) = self.try_apply_user_cast(
                        value,
                        argument,
                        &signature.params[param_index],
                        &argument.span,
                    )? {
                        value = casted;
                    } else {
                        value = self.cast_value_to_ast_type(
                            value,
                            &signature.params[param_index],
                            &argument.span,
                        )?;
                    }
                    // Apply ABI coercion for extern methods
                    if let Some(linkage) = &signature.linkage {
                        value = self.coerce_value_to_abi(
                            value,
                            &signature.params[param_index],
                            linkage,
                        )?;
                    }
                }
            } else if is_variadic {
                value = self.apply_variadic_default_promotion(value, &argument.span)?;
            }
            args.push(BasicMetadataValueEnum::from(value));
        }

        let call = self
            .builder
            .build_call(function, &args, &format!("call.{call_name}"))
            .map_err(|e| CodegenError::new(format!("failed to emit method call: {e}")))?;
        // If this is an explicit drop() call, clear the drop flag so
        // the implicit destructor at scope exit doesn't double-free.
        if method.name == "drop"
            && let ast::ExpressionKind::Identifier(ident) = &receiver.kind.as_ref()
            && let Some(flag_ptr) = self.drop_flags.get(&ident.name).copied()
        {
            self.builder
                .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                .map_err(|e| CodegenError::new(format!("failed to clear drop flag: {e}")))?;
        }

        // Add byval attributes to call site for large struct arguments
        if let Some(sig) = &signature
            && let Some(linkage) = &sig.linkage
            && matches!(linkage, ast::ExternLinkage::C)
        {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            for (index, param_ty) in sig.params.iter().enumerate() {
                let lowered = self.lower_basic_type(param_ty)?;
                if let BasicTypeEnum::StructType(struct_ty) = lowered {
                    let size = target_data.get_store_size(&struct_ty);
                    if self.abi_handler.needs_byval(size) {
                        let byval_kind = Attribute::get_named_enum_kind_id("byval");
                        let attr = self
                            .context
                            .create_type_attribute(byval_kind, struct_ty.as_any_type_enum());
                        call.add_attribute(AttributeLoc::Param(index as u32), attr);

                        let align = self.abi_handler.byval_alignment(struct_ty, &target_data);
                        let align_kind = Attribute::get_named_enum_kind_id("align");
                        let align_attr = self.context.create_enum_attribute(align_kind, align);
                        call.add_attribute(AttributeLoc::Param(index as u32), align_attr);
                    }
                }
            }
        }

        if let Some(value) = call.try_as_basic_value().basic() {
            if let Some(signature) = &signature
                && let (Some(ret_ty), Some(linkage)) = (&signature.return_type, &signature.linkage)
            {
                return Ok(Some(self.uncoerce_value_from_abi(value, ret_ty, linkage)?));
            }
            Ok(Some(value))
        } else if allow_void {
            Ok(None)
        } else {
            Err(CodegenError::with_span(
                "void method call cannot be used as a value",
                span.clone(),
            ))
        }
    }

    pub(crate) fn apply_variadic_default_promotion(
        &self,
        value: BasicValueEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match value {
            BasicValueEnum::FloatValue(float_value) => {
                if float_value.get_type() == self.context.f32_type() {
                    self.builder
                        .build_float_ext(float_value, self.context.f64_type(), "vararg.fpext")
                        .map(|v| v.as_basic_value_enum())
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed variadic float promotion: {e}"),
                                span.clone(),
                            )
                        })
                } else {
                    Ok(value)
                }
            }
            BasicValueEnum::IntValue(int_value) => {
                if int_value.get_type().get_bit_width() < 32 {
                    self.builder
                        .build_int_s_extend(int_value, self.context.i32_type(), "vararg.sext")
                        .map(|v| v.as_basic_value_enum())
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed variadic integer promotion: {e}"),
                                span.clone(),
                            )
                        })
                } else {
                    Ok(value)
                }
            }
            _ => Ok(value),
        }
    }
}
