//! Codegen for `launch` / `wait` task expressions (T1 concurrency, v1).
//!
//! Model: a `launch f(args)` sinks a compiler-owned *pack* (a small heap block
//! holding the launch arguments and, for non-void callees, a result slot) onto
//! a fresh OS thread. A per-launch-site static trampoline (`__silver_launch_tramp_N`)
//! marshals the pack back into a direct call of `f` and stores the result. The
//! `Task<T>` value is the registry `TaskRecord*` returned by `__silver_launch`
//! (zero on spawn failure); `wait t` joins via `__silver_wait_done` and frees
//! the pack once the child is finished.
//!
//! Pack layout is compiler-owned and shared implicitly between the launcher,
//! the trampoline, and the waiter. Because the launcher and the waiter may
//! live in different functions and only agree on the result type `T` (from the
//! `Task<T>` annotation), the layout is anchored at index 0:
//!
//! ```text
//!   void result:   { args... }                     (args at 0..n)
//!   non-void T:    { T result; args... }           (result at 0, args at 1..n+1)
//! ```
//!
//! The launcher and trampoline know the full callee signature; the waiter only
//! needs `T` to load the result at field 0. Offsets are computed with LLVM
//! struct types on both sides, so over-aligned results align identically.

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::targets::TargetData;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};

use crate::codegen::llvm_ir::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::types::Type;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn emit_launch_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let ast::ExpressionKind::Launch(inner) = expr.kind.as_ref() else {
            return Err(CodegenError::with_span(
                "internal: Launch arm reached with a non-launch expression",
                expr.span,
            ));
        };
        let ast::ExpressionKind::Call {
            function,
            arguments,
        } = inner.kind.as_ref()
        else {
            return Err(CodegenError::with_span(
                "launch operand must be a call to a named function",
                expr.span,
            ));
        };

        // Resolve the direct callee exactly like `emit_call_expression`:
        // fn pointers are not supported in v1.
        let (fn_name, explicit_generics) = match function.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => (identifier.name.clone(), None),
            ast::ExpressionKind::TypeName(ty) => {
                if let ast::TypeKind::Named(named) = ty.kind.as_ref()
                    && named.path.len() == 1
                {
                    (named.path[0].name.clone(), named.generics.clone())
                } else {
                    return Err(CodegenError::with_span(
                        "launch callee must be a named function",
                        function.span,
                    ));
                }
            }
            _ => {
                return Err(CodegenError::with_span(
                    "launch callee must be a named function",
                    function.span,
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
        let callee = self.module.get_function(&llvm_name).ok_or_else(|| {
            CodegenError::with_span(
                format!("launch: unknown function `{fn_name}`"),
                function.span,
            )
        })?;
        let signature = self
            .signature_for_name(&llvm_name)
            .or_else(|| self.signature_for_name(&fn_name))
            .ok_or_else(|| {
                CodegenError::with_span(
                    format!("launch: no signature for function `{fn_name}`"),
                    function.span,
                )
            })?;
        if arguments.len() != signature.params.len() {
            return Err(CodegenError::with_span(
                format!(
                    "launch: function `{fn_name}` expects {} arguments, got {}",
                    signature.params.len(),
                    arguments.len()
                ),
                inner.span,
            ));
        }

        let ret_is_void = Self::ast_type_is_void(signature.return_type.as_ref());

        // Build the pack struct type; result first (field 0) when non-void,
        // then the arguments in declaration order.
        let mut pack_fields: Vec<BasicTypeEnum<'ctx>> =
            Vec::with_capacity(1 + usize::from(!ret_is_void) + signature.params.len());
        let arg_base: u32 = if ret_is_void { 0 } else { 1 };
        if !ret_is_void && let Some(ret_ty) = &signature.return_type {
            pack_fields.push(self.lower_basic_type(ret_ty)?);
        }
        for param_ty in &signature.params {
            pack_fields.push(self.lower_basic_type(param_ty)?);
        }
        let pack_struct = self.context.struct_type(&pack_fields, false);
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        let pack_size = target_data.get_store_size(&pack_struct);

        // Allocate the pack: mem_alloc_raw_impl(i64 size, i64 align) -> u8*.
        let alloc_fn = self
            .module
            .get_function("mem_alloc_raw_impl")
            .ok_or_else(|| {
                CodegenError::with_span(
                    "launch requires `import std.mem.alloc;` (mem_alloc_raw_impl)",
                    expr.span,
                )
            })?;
        let size_val = self.context.i64_type().const_int(pack_size, false);
        let align_val = self.context.i64_type().const_int(16, false);
        let raw_pack = self
            .builder
            .build_call(alloc_fn, &[size_val.into(), align_val.into()], "pack.alloc")
            .map_err(|e| {
                CodegenError::with_span(format!("launch: pack allocation failed: {e}"), expr.span)
            })?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::with_span("launch: pack allocator returned void", expr.span)
            })?;
        let pack_ptr = self
            .builder
            .build_bit_cast(
                raw_pack.into_pointer_value(),
                self.context.ptr_type(AddressSpace::default()),
                "pack.ptr",
            )
            .map_err(|e| {
                CodegenError::with_span(format!("launch: pack bitcast failed: {e}"), expr.span)
            })?
            .into_pointer_value();

        // Marshal the arguments into the pack, moving them into the task
        // (ownership transfer: the child runs the callee on its own copy).
        for (index, argument) in arguments.iter().enumerate() {
            let value = self.emit_expression_value(argument)?;
            let casted =
                self.cast_value_to_ast_type(value, &signature.params[index], &argument.span)?;
            let slot = self
                .builder
                .build_struct_gep(pack_struct, pack_ptr, arg_base + index as u32, "pack.arg")
                .map_err(|e| {
                    CodegenError::with_span(format!("launch: pack arg GEP failed: {e}"), expr.span)
                })?;
            self.builder.build_store(slot, casted).map_err(|e| {
                CodegenError::with_span(format!("launch: store arg failed: {e}"), expr.span)
            })?;
            self.clear_drop_flag_of(argument)?;
        }

        // Per-launch-site trampoline: void(i64 pack) marshals pack -> call ->
        // store result. `module.add_function` while the parent body is still
        // being built is fine; we restore the insert block afterwards.
        let trampoline = self.emit_launch_trampoline(
            &expr.span,
            pack_struct,
            callee,
            &signature,
            ret_is_void,
            arg_base,
        )?;

        // __silver_launch(void(i64) tramp, u8* pack) -> i64 (TaskRecord*).
        let launch_fn = self.module.get_function("__silver_launch").ok_or_else(|| {
            CodegenError::with_span(
                "launch requires the thread runtime (`import std.sys.entry;` + --static-runtime)",
                expr.span,
            )
        })?;
        let pack_u8 = self
            .builder
            .build_bit_cast(
                pack_ptr,
                self.context.ptr_type(AddressSpace::default()),
                "pack.u8",
            )
            .map_err(|e| {
                CodegenError::with_span(format!("launch: pack u8 bitcast failed: {e}"), expr.span)
            })?
            .into_pointer_value();
        let rec = self
            .builder
            .build_call(launch_fn, &[trampoline, pack_u8.into()], "task.rec")
            .map_err(|e| {
                CodegenError::with_span(format!("launch: __silver_launch failed: {e}"), expr.span)
            })?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::with_span("launch: __silver_launch returned void", expr.span)
            })?;
        Ok(rec)
    }

    pub(crate) fn emit_wait_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let ast::ExpressionKind::Wait(inner) = expr.kind.as_ref() else {
            return Err(CodegenError::with_span(
                "internal: Wait arm reached with a non-wait expression",
                expr.span,
            ));
        };
        let inner_ty = self.resolve_receiver_type(inner).ok_or_else(|| {
            CodegenError::with_span("wait: cannot resolve operand type", expr.span)
        })?;
        let result_ty = Self::task_result_type(&inner_ty)
            .ok_or_else(|| CodegenError::with_span("wait: operand is not a Task", expr.span))?;
        let result_is_void = Self::ast_type_is_void(Some(result_ty));

        let parent = self.current_fn.ok_or_else(|| {
            CodegenError::with_span("wait: no enclosing function for control flow", expr.span)
        })?;

        let handle = self.emit_expression_value(inner)?;
        let handle_int = handle.into_int_value();
        let zero = self.context.i64_type().const_zero();
        let is_null = self
            .builder
            .build_int_compare(IntPredicate::EQ, handle_int, zero, "task.null")
            .map_err(|e| {
                CodegenError::with_span(format!("wait: null check failed: {e}"), expr.span)
            })?;

        let null_bb = self.context.append_basic_block(parent, "wait.null");
        let join_bb = self.context.append_basic_block(parent, "wait.join");
        let after_bb = self.context.append_basic_block(parent, "wait.after");
        self.builder
            .build_conditional_branch(is_null, null_bb, join_bb)
            .map_err(|e| CodegenError::with_span(format!("wait: branch failed: {e}"), expr.span))?;

        // Null task (spawn failure): yield a zeroed result, no join.
        let result_ty_owned = result_ty.clone();
        self.builder.position_at_end(null_bb);
        let null_value = if result_is_void {
            None
        } else {
            let llvm = self.lower_basic_type(&result_ty_owned)?;
            Some(llvm.const_zero())
        };
        self.builder
            .build_unconditional_branch(after_bb)
            .map_err(|e| CodegenError::with_span(format!("wait: branch failed: {e}"), expr.span))?;

        // Live task: __silver_wait_done(task) -> u8* pack, read the result,
        // then free the pack.
        let wait_done = self
            .module
            .get_function("__silver_wait_done")
            .ok_or_else(|| {
                CodegenError::with_span(
                    "wait requires the thread runtime (`import std.sys.entry;` + --static-runtime)",
                    expr.span,
                )
            })?;
        let free_fn = self
            .module
            .get_function("mem_free_raw_impl")
            .ok_or_else(|| {
                CodegenError::with_span("wait requires `import std.mem.alloc;`", expr.span)
            })?;
        self.builder.position_at_end(join_bb);
        let pack_u8 = self
            .builder
            .build_call(wait_done, &[handle_int.into()], "task.pack")
            .map_err(|e| {
                CodegenError::with_span(format!("wait: __silver_wait_done failed: {e}"), expr.span)
            })?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::with_span("wait: __silver_wait_done returned void", expr.span)
            })?
            .into_pointer_value();
        let join_value = if result_is_void {
            None
        } else {
            let result_struct = self
                .context
                .struct_type(&[self.lower_basic_type(&result_ty_owned)?], false);
            let pack_ptr = self
                .builder
                .build_bit_cast(
                    pack_u8,
                    self.context.ptr_type(AddressSpace::default()),
                    "task.result.pack",
                )
                .map_err(|e| {
                    CodegenError::with_span(format!("wait: pack bitcast failed: {e}"), expr.span)
                })?
                .into_pointer_value();
            let slot = self
                .builder
                .build_struct_gep(result_struct, pack_ptr, 0, "task.result")
                .map_err(|e| {
                    CodegenError::with_span(format!("wait: result GEP failed: {e}"), expr.span)
                })?;
            let loaded_ty = self.lower_basic_type(&result_ty_owned)?;
            let loaded = self
                .builder
                .build_load(loaded_ty, slot, "wait.result")
                .map_err(|e| {
                    CodegenError::with_span(format!("wait: result load failed: {e}"), expr.span)
                })?;
            Some(loaded)
        };
        self.builder
            .build_call(free_fn, &[pack_u8.into()], "pack.free")
            .map_err(|e| {
                CodegenError::with_span(format!("wait: pack free failed: {e}"), expr.span)
            })?;
        self.builder
            .build_unconditional_branch(after_bb)
            .map_err(|e| CodegenError::with_span(format!("wait: branch failed: {e}"), expr.span))?;

        self.builder.position_at_end(after_bb);
        match (null_value, join_value) {
            (None, None) => Ok(self.context.i64_type().const_zero().as_basic_value_enum()),
            (Some(nv), Some(jv)) => self
                .builder
                .build_phi(nv.get_type(), "wait.phi")
                .map(|phi| {
                    phi.add_incoming(&[(&nv, null_bb), (&jv, join_bb)]);
                    phi.as_basic_value()
                })
                .map_err(|e| CodegenError::with_span(format!("wait: phi failed: {e}"), expr.span)),
            _ => unreachable!("wait result presence must agree on both sides"),
        }
    }

    /// Emit the per-launch-site trampoline `void __silver_launch_tramp_N(i64 p)`.
    /// Loads the args from the pack, calls the callee directly, and stores the
    /// return value back into pack field 0 (when non-void).
    fn emit_launch_trampoline(
        &mut self,
        span: &Span,
        pack_struct: StructType<'ctx>,
        callee: FunctionValue<'ctx>,
        signature: &FunctionSig,
        ret_is_void: bool,
        arg_base: u32,
    ) -> CodegenResult<BasicMetadataValueEnum<'ctx>> {
        let id = self.task_trampoline_counter;
        self.task_trampoline_counter += 1;
        let name = format!("__silver_launch_tramp_{id}");
        let tramp_ty = self
            .context
            .void_type()
            .fn_type(&[self.context.i64_type().into()], false);
        let tramp = self
            .module
            .add_function(&name, tramp_ty, Some(Linkage::Internal));
        let entry = self.context.append_basic_block(tramp, "entry");
        let saved_block = self.builder.get_insert_block().ok_or_else(|| {
            CodegenError::with_span("launch: no active insert block for trampoline", *span)
        })?;
        self.builder.position_at_end(entry);

        let pack_arg = tramp.get_nth_param(0).ok_or_else(|| {
            CodegenError::with_span("launch: trampoline missing pack parameter", *span)
        })?;
        let pack_ptr = self
            .builder
            .build_int_to_ptr(
                pack_arg.into_int_value(),
                self.context.ptr_type(AddressSpace::default()),
                "pack.ptr",
            )
            .map_err(|e| {
                CodegenError::with_span(format!("launch: trampoline inttoptr failed: {e}"), *span)
            })?;

        let mut args = Vec::with_capacity(signature.params.len());
        for (index, param_ty) in signature.params.iter().enumerate() {
            let slot = self
                .builder
                .build_struct_gep(pack_struct, pack_ptr, arg_base + index as u32, "arg.ptr")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("launch: trampoline arg GEP failed: {e}"),
                        *span,
                    )
                })?;
            let arg_ty = self.lower_basic_type(param_ty)?;
            let value = self.builder.build_load(arg_ty, slot, "arg").map_err(|e| {
                CodegenError::with_span(format!("launch: trampoline arg load failed: {e}"), *span)
            })?;
            args.push(BasicMetadataValueEnum::from(value));
        }

        let call = self
            .builder
            .build_call(callee, &args, "task.call")
            .map_err(|e| {
                CodegenError::with_span(format!("launch: trampoline call failed: {e}"), *span)
            })?;
        if !ret_is_void {
            let result = call.try_as_basic_value().basic().ok_or_else(|| {
                CodegenError::with_span("launch: callee unexpectedly returned void", *span)
            })?;
            let slot = self
                .builder
                .build_struct_gep(pack_struct, pack_ptr, 0, "result.ptr")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("launch: trampoline result GEP failed: {e}"),
                        *span,
                    )
                })?;
            self.builder.build_store(slot, result).map_err(|e| {
                CodegenError::with_span(format!("launch: store result failed: {e}"), *span)
            })?;
        }
        self.builder.build_return(None).map_err(|e| {
            CodegenError::with_span(format!("launch: trampoline return failed: {e}"), *span)
        })?;

        // Restore the parent function's builder position.
        self.builder.position_at_end(saved_block);
        Ok(tramp.as_global_value().as_pointer_value().into())
    }

    /// `true` when `ty` lowers to no value (void primitive or empty tuple).
    fn ast_type_is_void(ty: Option<&ast::Type>) -> bool {
        match ty {
            None => true,
            Some(t) => {
                Self::is_void_primitive(t)
                    || matches!(t.kind.as_ref(), ast::TypeKind::Tuple(items) if items.is_empty())
            }
        }
    }

    /// Extract the payload type `T` from a `Task<T>` type annotation.
    fn task_result_type(ty: &ast::Type) -> Option<&ast::Type> {
        if let ast::TypeKind::Named(named) = ty.kind.as_ref()
            && named.path.len() == 1
            && named.path[0].name == "Task"
            && let Some(generics) = &named.generics
            && generics.len() == 1
        {
            return Some(&generics[0]);
        }
        None
    }
}
