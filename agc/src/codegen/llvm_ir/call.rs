use crate::codegen::llvm_ir::HashMap;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::{AtomicOrdering, AtomicRMWBinOp};

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::{DeferAction, DeferredEntry, FunctionSig};
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
                *span,
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
                            function_expr.span,
                        ));
                    }
                } else {
                    return Err(CodegenError::with_span(
                        "only direct function calls are supported in LLVM IR codegen",
                        function_expr.span,
                    ));
                }
            }
            _ => {
                return Err(CodegenError::with_span(
                    "only direct function calls are supported in LLVM IR codegen",
                    function_expr.span,
                ));
            }
        };

        // Atomic intrinsics (`__atomic_*`) lower to LLVM atomic instructions
        // directly; they never exist as LLVM functions.
        if let Some(emitted) =
            self.try_emit_atomic_intrinsic(&fn_name, arguments, allow_void, span)?
        {
            return Ok(emitted);
        }

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
            self.resolve_free_function_symbol(&fn_name, arguments)?
        };
        let function = self.module.get_function(&llvm_name).ok_or_else(|| {
            CodegenError::with_span(
                format!("unknown function `{}`", fn_name),
                function_expr.span,
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
                    // By-value argument of a Drop type: the callee's
                    // parameter destructor runs on exit, transferring
                    // ownership. Clear the caller's flag to avoid a
                    // double free; extern functions never drop params.
                    if signature.linkage.is_none() {
                        let arg_drops = if let Some(arg_ty) = self.resolve_argument_type(argument) {
                            self.param_type_drops_on_exit(&arg_ty).unwrap_or(false)
                        } else if index < signature.params.len() {
                            self.param_type_drops_on_exit(&signature.params[index]).unwrap_or(false)
                        } else {
                            false
                        };
                        if arg_drops {
                            self.clear_drop_flag_of(argument)?;
                        }
                    }

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
                *span,
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
    ///
    /// True when the callee runs a destructor on this by-value parameter at
    /// function exit: a non-pointer/reference type with a Drop impl, cascaded
    /// struct field drops, or an enum payload cascade.
    pub(crate) fn param_type_drops_on_exit(&mut self, ty: &ast::Type) -> CodegenResult<bool> {
        if matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        ) {
            return Ok(false);
        }
        if self
            .get_drop_function_name(ty)
            .map(|drop_fn| drop_fn.is_some())
            .unwrap_or(false)
        {
            return Ok(true);
        }
        // Struct without its own Drop impl but with droppable fields:
        if self.struct_has_field_drops(ty)? {
            return Ok(true);
        }
        // An enum WITHOUT a Drop impl of its own still drops its payload at
        // scope exit via the tag-aware cascade — a by-value transfer must
        // clear the caller's flag for it too.
        self.enum_has_payload_cascade(ty)
    }

    fn struct_has_field_drops(&mut self, ty: &ast::Type) -> CodegenResult<bool> {
        let Some(named) = Self::extract_named_type(ty).cloned() else {
            return Ok(false);
        };
        let base_name = named.path.last().map(|s| &s.name[..]).unwrap_or_default();
        if base_name == "Task"
            || self.enum_backing_type_for_named(&named).is_some()
            || (named.path.len() == 1
                && self.enum_payload_layouts.contains_key(&named.path[0].name))
        {
            return Ok(false);
        }
        let named_key = Self::named_type_key(&named);
        if !self.struct_fields.contains_key(base_name) && !self.struct_fields.contains_key(&named_key) {
            return Ok(false);
        }
        let fields: Vec<(String, ast::Type)> = match self.struct_fields.get(&named_key) {
            Some(f) => f.clone(),
            None => match self.struct_fields.get(base_name) {
                Some(f) => f.clone(),
                None => return Ok(false),
            },
        };
        for (_, fty) in fields {
            if self.param_type_drops_on_exit(&fty)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// True when `ty` is an enum with a payload cascade (a payload layout
    /// whose variants carry Drop-typed values) and no Drop impl of its own.
    fn enum_has_payload_cascade(&mut self, ty: &ast::Type) -> CodegenResult<bool> {
        let Some(named) = Self::extract_named_type(ty).cloned() else {
            return Ok(false);
        };
        if named.path.len() != 1 {
            return Ok(false);
        }
        let enum_name = &named.path[0].name;
        if !self.enum_payload_layouts.contains_key(enum_name) {
            return Ok(false);
        }
        if self
            .get_drop_function_name(ty)
            .map(|drop_fn| drop_fn.is_some())
            .unwrap_or(false)
        {
            // Has its own Drop impl: the body manages the payload.
            return Ok(false);
        }
        let substitutions: HashMap<String, ast::Type> = if let Some(params) = self.struct_generics.get(enum_name)
            && let Some(args) = &named.generics
            && params.len() == args.len()
        {
            params.iter().cloned().zip(args.iter().cloned()).collect()
        } else {
            HashMap::default()
        };
        let Some(payloads) = self.enum_variant_payload_types.get(enum_name).cloned() else {
            return Ok(false);
        };
        let mut has_drop = false;
        for types in payloads.values() {
            for pt in types {
                let concrete_pt = if substitutions.is_empty() {
                    pt.clone()
                } else {
                    Self::substitute_generic_type(pt, &substitutions)
                };
                if self.get_drop_function_name(&concrete_pt)?.is_some() {
                    has_drop = true;
                }
            }
        }
        Ok(has_drop)
    }

    /// Clear the tracked drop flag of `expr` when it names a local or
    /// parameter: ownership of the value is being transferred to a by-value
    /// callee that will run the destructor on its copy.
    pub(crate) fn clear_drop_flag_of(&mut self, expr: &ast::Expression) -> CodegenResult<()> {
        if let ast::ExpressionKind::Identifier(ident) = expr.kind.as_ref() {
            if let Some(flag_ptr) = self.lookup_variable(&ident.name).and_then(|v| v.drop_flag) {
                self.builder
                    .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                    .map_err(|e| CodegenError::new(format!("failed to clear drop flag: {e}")))?;
            }
            self.clear_field_flags(&ident.name)?;
        } else if let Some((root_name, path)) = self.lvalue_root_and_path(expr) {
            self.clear_field_flags_for_path(&root_name, &path)?;
        }
        Ok(())
    }

    /// Whether `arguments` match the callee's parameter types, skipping the
    /// first `skip` params (the receiver for instance methods). Returns true
    /// when the signature is unknown so arity alone decides. Argument types
    /// come from a side-effect-free resolver, so casts (`(i64)x`) participate
    /// in matching as their target type.
    fn argument_types_match(
        &mut self,
        signature: Option<&FunctionSig>,
        arguments: &[ast::Expression],
        skip: usize,
    ) -> bool {
        let Some(sig) = signature else {
            return true;
        };
        if sig.params.len() < skip + arguments.len() {
            return false;
        }
        for (i, argument) in arguments.iter().enumerate() {
            let param_key = Type::from_ast(&sig.params[skip + i]).canonical_key();
            let Some(arg_ty) = self.resolve_argument_type(argument) else {
                return false;
            };
            if Type::from_ast(&arg_ty).canonical_key() != param_key {
                return false;
            }
        }
        true
    }

    /// Resolve the LLVM symbol for a direct call to a (non-generic) free
    /// function. Single-signature names resolve through the plain-name
    /// fallback (extern, #[link_name], imported, and un-overloaded functions
    /// alike); overloaded names pick the candidate whose arity and argument
    /// types match, mirroring method-call resolution.
    fn resolve_free_function_symbol(
        &mut self,
        name: &str,
        arguments: &[ast::Expression],
    ) -> CodegenResult<String> {
        // Overloaded names enumerate their hashed symbols from
        // source_function_symbols; single-symbol names fall back to the
        // #[link_name] mapping (for renamed/imported functions), then the
        // plain name. imported_function_links must NOT short-circuit here:
        // it is a 1:1 map, so a second overload would overwrite the first.
        let candidates = self
            .source_function_symbols
            .get(name)
            .cloned()
            .unwrap_or_else(|| {
                self.imported_function_links
                    .get(name)
                    .cloned()
                    .map_or_else(|| vec![name.to_string()], |linked| vec![linked])
            });
        if candidates.len() <= 1 {
            return Ok(candidates
                .into_iter()
                .next()
                .unwrap_or_else(|| name.to_string()));
        }
        let mut arity_match: Option<String> = None;
        let mut type_match: Option<String> = None;
        for candidate in &candidates {
            let signature = self.signature_for_name(candidate);
            let declared = signature
                .as_ref()
                .map(|sig| sig.params.len())
                .unwrap_or_else(|| {
                    self.module
                        .get_function(candidate)
                        .map(|f| f.get_type().get_param_types().len())
                        .unwrap_or(0)
                });
            let variadic = signature
                .as_ref()
                .map(|sig| sig.is_variadic)
                .unwrap_or(false);
            if !variadic && declared != arguments.len() {
                continue;
            }
            if variadic || self.argument_types_match(signature.as_ref(), arguments, 0) {
                type_match = Some(candidate.clone());
                break;
            }
            if arity_match.is_none() {
                arity_match = Some(candidate.clone());
            }
        }
        Ok(type_match
            .or(arity_match)
            .unwrap_or_else(|| name.to_string()))
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
            candidates.extend(self.overloaded_method_candidates(owner_name, &method.name));
        }
        candidates.push(method.name.clone());

        // Overload resolution: filter candidates by arity (instance methods
        // declare `args + 1` params, static/by-value-receiver methods `args`),
        // then prefer an exact argument-type match against the parameter
        // types. A type match beats the instance/static ordering; when no
        // argument types can be proven (or none match), fall back to the
        // first arity match so untypable expressions keep resolving.
        // A type receiver must select the static form (params == args); a
        // value receiver the instance form (params == args + 1). Arity alone
        // is ambiguous when a static method happens to take args + 1
        // parameters (e.g. the HttpRequest.new overloads) — that would pick
        // the instance form and emit the type name as a receiver value. An
        // identifier that fails type resolution is a type name (mirrors
        // receiver_owner_candidates); imported/unknown types stay static.
        let receiver_is_type = match receiver.kind.as_ref() {
            ast::ExpressionKind::TypeName(_) => true,
            ast::ExpressionKind::Identifier(_) => self.resolve_receiver_type(receiver).is_none(),
            _ => false,
        };
        let mut instance_type: Option<(String, FunctionValue<'ctx>)> = None;
        let mut static_type: Option<(String, FunctionValue<'ctx>)> = None;
        let mut instance_fallback: Option<(String, FunctionValue<'ctx>)> = None;
        let mut static_fallback: Option<(String, FunctionValue<'ctx>)> = None;
        for name in &candidates {
            let Some(function) = self.module.get_function(name) else {
                continue;
            };
            let signature = self.signature_for_name(name);
            let declared = signature
                .as_ref()
                .map(|sig| sig.params.len())
                .unwrap_or_else(|| function.get_type().get_param_types().len());
            let variadic = signature
                .as_ref()
                .map(|sig| sig.is_variadic)
                .unwrap_or_else(|| function.get_type().is_var_arg());
            if variadic
                || (receiver_is_type && declared == arguments.len())
                || (!receiver_is_type && declared == arguments.len() + 1)
            {
                let offset = if receiver_is_type { 0 } else { 1 };
                if variadic || self.argument_types_match(signature.as_ref(), arguments, offset) {
                    if receiver_is_type {
                        if static_type.is_none() {
                            static_type = Some((name.clone(), function));
                        }
                    } else if instance_type.is_none() {
                        instance_type = Some((name.clone(), function));
                    }
                } else if receiver_is_type {
                    if static_fallback.is_none() {
                        static_fallback = Some((name.clone(), function));
                    }
                } else if instance_fallback.is_none() {
                    instance_fallback = Some((name.clone(), function));
                }
            }
        }
        let selected = instance_type
            .or(static_type)
            .or(instance_fallback)
            .or(static_fallback);
        let selected_name = selected.as_ref().map(|(name, _)| name.clone());
        let selected_fn = selected.map(|(_, function)| function);

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
                                method.span,
                            )
                        })?
                } else {
                    return Err(CodegenError::with_span(
                        format!("unknown method `{}`", method.name),
                        method.span,
                    ));
                }
            } else {
                return Err(CodegenError::with_span(
                    format!("unknown method `{}`", method.name),
                    method.span,
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
                .map(|ty| {
                    matches!(
                        ty.kind.as_ref(),
                        ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
                    )
                })
                .unwrap_or(false);

            let expects_ref = signature
                .as_ref()
                .and_then(|sig| sig.params.first())
                .map(|first| {
                    matches!(
                        first.kind.as_ref(),
                        ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
                    )
                })
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
                                receiver.span,
                            )
                        })?;

                        // If the spilled temporary owns heap memory (implements Drop),
                        // register a deferred cleanup so it doesn't leak.  Without this,
                        // chained calls like s.trim().replace(...).split(...) leak the
                        // intermediate String buffers.
                        if let Some(ref receiver_ty) = receiver_ty
                            && !receiver_is_pointer
                            && let Some(drop_fn_name) = self.get_drop_function_name(receiver_ty)?
                        {
                            let flag_name = format!("method.recv.tmp.{}.drop", self.temp_counter);
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
                                    CodegenError::new(format!("failed to init temp drop flag: {e}"))
                                })?;
                            if let Some(scope) = self.defers.last_mut() {
                                scope.push(DeferredEntry {
                                    action: DeferAction::DropCall(drop_fn_name, temp),
                                    flag: Some(flag_alloca),
                                });
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
                            receiver.span,
                        ));
                    };
                    if let Some(expected_receiver_ty) = expected_receiver_llvm_ty {
                        self.builder
                            .build_load(expected_receiver_ty, receiver_ptr, "method.recv.load")
                            .map(|value| value.as_basic_value_enum())
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load pointer receiver: {e}"),
                                    receiver.span,
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
                    let receiver_drops = if let Some(receiver_ty) = &receiver_ty {
                        self.param_type_drops_on_exit(receiver_ty).unwrap_or(false)
                    } else if let Some(signature) = &signature {
                        signature
                            .params
                            .first()
                            .is_some_and(|ty| self.param_type_drops_on_exit(ty).unwrap_or(false))
                    } else {
                        false
                    };
                    if receiver_drops
                        && signature
                            .as_ref()
                            .map(|s| s.linkage.is_none())
                            .unwrap_or(true)
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
                    if signature.linkage.is_none() {
                        let arg_drops = if let Some(arg_ty) = self.resolve_argument_type(argument) {
                            self.param_type_drops_on_exit(&arg_ty).unwrap_or(false)
                        } else if param_index < signature.params.len() {
                            self.param_type_drops_on_exit(&signature.params[param_index]).unwrap_or(false)
                        } else {
                            false
                        };
                        if arg_drops {
                            self.clear_drop_flag_of(argument)?;
                        }
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
        {
            if let Some(flag_ptr) = self.lookup_variable(&ident.name).and_then(|v| v.drop_flag) {
                self.builder
                    .build_store(flag_ptr, self.context.bool_type().const_int(0, false))
                    .map_err(|e| CodegenError::new(format!("failed to clear drop flag: {e}")))?;
            }
            self.clear_field_flags(&ident.name)?;
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
                *span,
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
                                *span,
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
                                *span,
                            )
                        })
                } else {
                    Ok(value)
                }
            }
            _ => Ok(value),
        }
    }

    /// Lowers a call to a `__atomic_*` intrinsic into the matching LLVM atomic
    /// instruction. Returns `Ok(None)` when `name` is not an atomic intrinsic;
    /// `Ok(Some(None))` for a successfully emitted void intrinsic; and
    /// `Ok(Some(Some(v)))` for a value-producing intrinsic.
    ///
    /// The element width is encoded in the name suffix (`_i8`/`_i32`/`_i64`),
    /// since Silver emits opaque LLVM pointers and the pointee type is not
    /// recoverable from the pointer value. The ordering argument must be an
    /// integer literal: LLVM requires a constant ordering in the instruction.
    fn try_emit_atomic_intrinsic(
        &mut self,
        name: &str,
        arguments: &[ast::Expression],
        allow_void: bool,
        span: &Span,
    ) -> CodegenResult<Option<Option<BasicValueEnum<'ctx>>>> {
        let Some(body) = name.strip_prefix("__atomic_") else {
            return Ok(None);
        };
        let (op, width): (&str, u32) = match body {
            "load_i8" => ("load", 8),
            "load_i32" => ("load", 32),
            "load_i64" => ("load", 64),
            "store_i8" => ("store", 8),
            "store_i32" => ("store", 32),
            "store_i64" => ("store", 64),
            "exchange_i8" => ("exchange", 8),
            "exchange_i32" => ("exchange", 32),
            "exchange_i64" => ("exchange", 64),
            "fetch_add_i8" => ("fetch_add", 8),
            "fetch_add_i32" => ("fetch_add", 32),
            "fetch_add_i64" => ("fetch_add", 64),
            "fetch_sub_i8" => ("fetch_sub", 8),
            "fetch_sub_i32" => ("fetch_sub", 32),
            "fetch_sub_i64" => ("fetch_sub", 64),
            "cmpxchg_i8" => ("cmpxchg", 8),
            "cmpxchg_i32" => ("cmpxchg", 32),
            "cmpxchg_i64" => ("cmpxchg", 64),
            "fence" => ("fence", 0),
            _ => return Ok(None),
        };

        let int_ty = match width {
            8 => Some(self.context.i8_type()),
            32 => Some(self.context.i32_type()),
            64 => Some(self.context.i64_type()),
            _ => None,
        };

        // Emit all argument values; arity/type correctness is enforced by typeck.
        let values: Vec<BasicValueEnum<'ctx>> = arguments
            .iter()
            .map(|a| self.emit_expression_value(a))
            .collect::<CodegenResult<_>>()?;

        // `order` (and the two cmpxchg orders) must be compile-time constants.
        // Accept an integer literal or a reference to an immutable global
        // constant that holds a single integer literal (e.g. `seq_cst`).
        let order_of = |consts: &rustc_hash::FxHashMap<String, i128>,
                        idx: usize|
         -> CodegenResult<AtomicOrdering> {
            let int = values[idx].into_int_value();
            let mut value = int.get_zero_extended_constant();
            if value.is_none()
                && let ast::ExpressionKind::Identifier(ident) = &arguments[idx].kind.as_ref()
            {
                value = consts.get(&ident.name).copied().map(|v| v as u64);
            }
            let Some(value) = value else {
                return Err(CodegenError::with_span(
                    format!(
                        "atomic intrinsic '{}': ordering must be a literal constant",
                        name
                    ),
                    *span,
                ));
            };
            Ok(match value {
                0 => AtomicOrdering::Monotonic,
                1 => AtomicOrdering::Acquire,
                2 => AtomicOrdering::Release,
                3 => AtomicOrdering::AcquireRelease,
                4 => AtomicOrdering::SequentiallyConsistent,
                _ => {
                    return Err(CodegenError::with_span(
                        format!(
                            "atomic intrinsic '{}': invalid ordering {} (expected 0..=4)",
                            name, value
                        ),
                        *span,
                    ));
                }
            })
        };

        let void_err = || {
            Err(CodegenError::with_span(
                "void function call cannot be used as a value",
                *span,
            ))
        };

        // Silver integer literals default to i64, so a value argument must be
        // coerced to the width named in the intrinsic suffix (truncate when
        // wider, zero-extend when narrower) before it can drive an atomic op.
        let coerce = |v: BasicValueEnum<'ctx>, ty: inkwell::types::IntType<'ctx>| {
            let int = v.into_int_value();
            let from = int.get_type().get_bit_width();
            let to = ty.get_bit_width();
            let coerced = if from == to {
                int
            } else if from > to {
                self.builder
                    .build_int_truncate(int, ty, "atomic.trunc")
                    .map_err(|e| CodegenError::new(format!("atomic width truncate: {e}")))?
            } else {
                self.builder
                    .build_int_z_extend(int, ty, "atomic.zext")
                    .map_err(|e| CodegenError::new(format!("atomic width extend: {e}")))?
            };
            Ok(coerced)
        };
        let elem_ty = || int_ty.expect("elemental atomic op carries a width");

        match op {
            "load" => {
                let addr = values[0].into_pointer_value();
                let ordering = order_of(&self.global_const_values, 1)?;
                let int_ty = int_ty.expect("load carries an element width");
                let value = self
                    .builder
                    .build_load(int_ty, addr, "atomic.load")
                    .map_err(|e| CodegenError::new(format!("atomic load failed: {e}")))?;
                value
                    .as_instruction_value()
                    .expect("load produces an instruction")
                    .set_atomic_ordering(ordering)
                    .map_err(|e| CodegenError::new(format!("atomic load ordering: {e}")))?;
                Ok(Some(Some(value)))
            }
            "store" => {
                if !allow_void {
                    return void_err();
                }
                let addr = values[0].into_pointer_value();
                let val = coerce(values[1], elem_ty())?;
                let ordering = order_of(&self.global_const_values, 2)?;
                self.builder
                    .build_store(addr, val)
                    .map_err(|e| CodegenError::new(format!("atomic store failed: {e}")))?
                    .set_atomic_ordering(ordering)
                    .map_err(|e| CodegenError::new(format!("atomic store ordering: {e}")))?;
                Ok(Some(None))
            }
            "exchange" | "fetch_add" | "fetch_sub" => {
                let addr = values[0].into_pointer_value();
                let val = coerce(values[1], elem_ty())?;
                let ordering = order_of(&self.global_const_values, 2)?;
                let rmw = match op {
                    "exchange" => AtomicRMWBinOp::Xchg,
                    "fetch_add" => AtomicRMWBinOp::Add,
                    "fetch_sub" => AtomicRMWBinOp::Sub,
                    _ => unreachable!(),
                };
                let result = self
                    .builder
                    .build_atomicrmw(rmw, addr, val, ordering)
                    .map_err(|e| CodegenError::new(format!("atomicrmw failed: {e}")))?;
                Ok(Some(Some(result.as_basic_value_enum())))
            }
            "cmpxchg" => {
                let addr = values[0].into_pointer_value();
                let expected = coerce(values[1], elem_ty())?;
                let desired = coerce(values[2], elem_ty())?;
                let success = order_of(&self.global_const_values, 3)?;
                let failure = order_of(&self.global_const_values, 4)?;
                let pair = self
                    .builder
                    .build_cmpxchg(addr, expected, desired, success, failure)
                    .map_err(|e| CodegenError::new(format!("cmpxchg failed: {e}")))?;
                // cmpxchg yields `{T old, i1 success}` on this LLVM; the flag
                // is a real extractvalue (operand access would return the address).
                let flag = self
                    .builder
                    .build_extract_value(pair, 1, "cmpxchg.ok")
                    .map_err(|e| CodegenError::new(format!("cmpxchg flag: {e}")))?;
                Ok(Some(Some(flag)))
            }
            "fence" => {
                if !allow_void {
                    return void_err();
                }
                let ordering = order_of(&self.global_const_values, 0)?;
                if !matches!(
                    ordering,
                    AtomicOrdering::Acquire
                        | AtomicOrdering::Release
                        | AtomicOrdering::AcquireRelease
                        | AtomicOrdering::SequentiallyConsistent
                ) {
                    return Err(CodegenError::with_span(
                        format!(
                            "atomic intrinsic '{}': fence requires an acquire, release, acq_rel, or seq_cst ordering",
                            name
                        ),
                        *span,
                    ));
                }
                // A fence is a void instruction: naming it produces
                // `%atomic.fence = fence acquire`, which the LLVM verifier
                // rejects ("instructions returning void cannot have a name")
                // and crashes the backend. Emit it unnamed.
                self.builder
                    .build_fence(ordering, false, "")
                    .map_err(|e| CodegenError::new(format!("fence failed: {e}")))?;
                Ok(Some(None))
            }
            _ => unreachable!("atomic op table covers load/store/exchange/fetch/cmpxchg/fence"),
        }
    }
}
