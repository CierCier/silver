use rustc_hash::FxHashMap as HashMap;

use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, PointerValue};

use crate::codegen::SilverGenerator;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::{DeferAction, DeferredEntry, FunctionSig, VarInfo};
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::symbol_table::{CompilerPhase, SymbolKind};
use crate::types::Type;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn set_debug_location(&self, span: &Span) {
        if let Some(debug) = &self.debug {
            let (line, col, _, _) = debug.source_map.span_to_line_col(span);
            let loc = debug.create_debug_location(self.context, line, col);
            self.builder.set_current_debug_location(loc);
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.variables.push(HashMap::default());
        self.defers.push(Vec::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        let _ = self.variables.pop();
        let _ = self.defers.pop();
    }

    pub(crate) fn emit_defers(&mut self, levels: usize) -> CodegenResult<()> {
        let total = self.defers.len();
        if levels == 0 || total == 0 {
            return Ok(());
        }
        let start = total.saturating_sub(levels);
        // Clone scopes to avoid borrow conflicts, emit without draining
        let scopes: Vec<Vec<DeferredEntry<'ctx>>> = self.defers[start..].to_vec();
        for mut scope in scopes.into_iter() {
            for entry in scope.iter_mut().rev() {
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for defer emission".to_string())
                })?;

                // Build conditional guard if drop flag is present
                let after_bb = if let Some(flag_ptr) = entry.flag {
                    let flag_val = self
                        .builder
                        .build_load(self.context.bool_type(), flag_ptr, "defer.flag")
                        .map_err(|e| {
                            CodegenError::new(format!("failed to load defer flag: {e}"))
                        })?;

                    let run_bb = self.context.append_basic_block(function, "defer.run");
                    let after_bb = self.context.append_basic_block(function, "defer.after");

                    self.builder
                        .build_conditional_branch(flag_val.into_int_value(), run_bb, after_bb)
                        .map_err(|e| CodegenError::new(format!("failed to branch defer: {e}")))?;

                    self.builder.position_at_end(run_bb);
                    Some(after_bb)
                } else {
                    None
                };

                // Execute the deferred action
                match &entry.action {
                    DeferAction::Statement(stmt) => {
                        self.generate_statement(stmt)?;
                    }
                    DeferAction::DropCall(drop_fn_name, var_ptr) => {
                        if let Some(func) = self.module.get_function(drop_fn_name) {
                            let args = vec![BasicMetadataValueEnum::from(*var_ptr)];
                            self.builder.build_call(func, &args, "drop").map_err(|e| {
                                CodegenError::new(format!("failed to call drop: {e}"))
                            })?;
                        }
                    }
                }

                // If we had a conditional guard, resume at after_bb
                if let Some(after) = after_bb {
                    self.builder
                        .build_unconditional_branch(after)
                        .map_err(|e| {
                            CodegenError::new(format!("failed to branch after defer: {e}"))
                        })?;
                    self.builder.position_at_end(after);
                }
            }
        }
        Ok(())
    }

    pub(crate) fn lookup_variable(&self, name: &str) -> Option<VarInfo<'ctx>> {
        self.variables
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    pub(crate) fn lookup_extern_global(
        &self,
        name: &str,
    ) -> Option<(inkwell::values::GlobalValue<'ctx>, ast::Type)> {
        let ty = self.extern_globals.get(name)?.clone();
        self.module.get_global(name).map(|global| (global, ty))
    }

    pub(crate) fn lookup_module_global(
        &self,
        name: &str,
    ) -> Option<(inkwell::values::GlobalValue<'ctx>, ast::Type)> {
        if let Some(ty) = self.global_variables.get(name).cloned()
            && let Some(global) = self.module.get_global(name)
        {
            return Some((global, ty));
        }
        self.lookup_extern_global(name)
    }

    pub(crate) fn lookup_storage(&self, name: &str) -> Option<(PointerValue<'ctx>, ast::Type)> {
        if let Some(info) = self.lookup_variable(name) {
            return Some((info.ptr, info.ty));
        }
        self.lookup_module_global(name)
            .map(|(global, ty)| (global.as_pointer_value(), ty))
    }

    pub(crate) fn lookup_value_type(&self, name: &str) -> Option<ast::Type> {
        self.lookup_variable(name)
            .map(|info| info.ty)
            .or_else(|| self.global_variables.get(name).cloned())
            .or_else(|| self.extern_globals.get(name).cloned())
    }

    pub(crate) fn intern_const_string_global(&mut self, value: &str) -> PointerValue<'ctx> {
        if let Some(existing) = self.string_constants.get(value) {
            return *existing;
        }

        let string_value = self.context.const_string(value.as_bytes(), true);
        let global_name = format!(".str.{}", self.string_constants.len());
        let global = self
            .module
            .add_global(string_value.get_type(), None, &global_name);
        global.set_initializer(&string_value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        let ptr = global.as_pointer_value();
        self.string_constants.insert(value.to_string(), ptr);
        ptr
    }

    pub(crate) fn register_function_signature(
        &mut self,
        llvm_name: &str,
        sig: FunctionSig,
        span: Option<Span>,
        kind: SymbolKind,
    ) {
        let symbol_key = format!("codegen::fn::{llvm_name}");
        let symbol_id =
            self.symbol_table
                .intern_symbol(symbol_key, kind, span, CompilerPhase::Codegen);
        self.function_name_to_symbol
            .insert(llvm_name.to_string(), symbol_id);
        self.function_sigs.insert(symbol_id, sig);
    }

    pub(crate) fn signature_for_name(&self, llvm_name: &str) -> Option<FunctionSig> {
        self.function_name_to_symbol
            .get(llvm_name)
            .and_then(|symbol_id| self.function_sigs.get(symbol_id))
            .cloned()
    }

    pub(crate) fn named_type_name(named: &ast::NamedType) -> String {
        named
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub(crate) fn named_type_key(named: &ast::NamedType) -> String {
        let base = Self::named_type_name(named);
        if let Some(args) = &named.generics {
            let rendered = args
                .iter()
                .map(|arg| Type::from_ast(arg).canonical_key())
                .collect::<Vec<_>>()
                .join(",");
            format!("{base}<{rendered}>")
        } else {
            base
        }
    }

    pub(crate) fn get_drop_function_name(
        &mut self,
        ty: &ast::Type,
    ) -> CodegenResult<Option<String>> {
        // 1. Check concrete Drop-impl owners already registered
        let drop_owners = Self::owner_name_candidates_from_type(ty);
        for owner in &drop_owners {
            if self.drop_trait_impl_owners.contains(owner.as_str()) {
                let mangled = Self::mangle_method_name(owner, "drop");
                if self.module.get_function(&mangled).is_some() {
                    return Ok(Some(mangled));
                }
            }
        }
        // 2. Try generic Drop-impl instantiation
        if let Some(mangled) =
            self.try_instantiate_generic_impl_method_for_type_filtered(ty, "drop", Some("Drop"))?
        {
            return Ok(Some(mangled));
        }
        // 3. No Drop trait impl
        Ok(None)
    }

    /// Allocate a 1-bit drop flag for `name`, initialize it to true, and
    /// register `var_ptr`'s destructor (plus cascaded field drops) as a
    /// deferred drop on the current scope. Records the flag so `move` and
    /// by-value transfers can clear it. Shared by parameters and locals.
    pub(crate) fn register_drop_flag(
        &mut self,
        name: &str,
        ty: &ast::Type,
        var_ptr: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let Some(drop_fn_name) = self.get_drop_function_name(ty)? else {
            return Ok(());
        };
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for destructor".to_string()))?;
        let flag_alloca = self.create_entry_alloca(
            function,
            &format!("{name}.drop"),
            self.context.bool_type().as_basic_type_enum(),
        )?;
        self.builder
            .build_store(flag_alloca, self.context.bool_type().const_int(1, false))
            .map_err(|e| CodegenError::new(format!("failed to init drop flag: {e}")))?;
        self.drop_flags.insert(name.to_string(), flag_alloca);

        // Field drops are registered BEFORE the struct's own drop so they
        // fire AFTER it in LIFO order (the struct drop is last-registered,
        // so it runs first).
        self.register_field_drops(ty, var_ptr, flag_alloca)?;

        if let Some(scope) = self.defers.last_mut() {
            scope.push(DeferredEntry {
                action: DeferAction::DropCall(drop_fn_name, var_ptr),
                flag: Some(flag_alloca),
            });
        }
        Ok(())
    }

    pub(crate) fn register_field_drops(
        &mut self,
        ty: &ast::Type,
        struct_ptr: PointerValue<'ctx>,
        flag: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let Some(named) = Self::extract_named_type(ty).cloned() else {
            return Ok(());
        };
        let _ = self.ensure_named_struct_type(&named)?;
        let named_key = Self::named_type_key(&named);
        // Clone fields and the struct type to avoid borrowing self while
        // recursively calling self.register_field_drops / get_drop_function_name.
        let fields: Vec<(String, ast::Type)> = match self.struct_fields.get(&named_key) {
            Some(f) => f.clone(),
            None => return Ok(()),
        };
        let struct_ty = match self.struct_types.get(&named_key) {
            Some(ty) => *ty,
            None => return Ok(()),
        };
        // Iterate in reverse so that declaration-order drops fire at runtime
        // (defers are LIFO, so last-registered fires first).
        for (field_index, (field_name, field_ty)) in fields.iter().enumerate().rev() {
            let field_ptr = self
                .builder
                .build_struct_gep(struct_ty, struct_ptr, field_index as u32, field_name)
                .map_err(|e| CodegenError::new(format!("cascade field GEP: {e}")))?;
            // Only cascade drops into value-type fields — pointers/references are non-owning.
            if !Self::is_pointer_or_reference(field_ty) {
                // Recursively register drops for nested fields first
                // (so the parent field's drop fires after its children).
                self.register_field_drops(field_ty, field_ptr, flag)?;

                // Register this field's own drop if it implements Drop.
                if let Some(drop_fn) = self.get_drop_function_name(field_ty)?
                    && let Some(scope) = self.defers.last_mut()
                {
                    scope.push(DeferredEntry {
                        action: DeferAction::DropCall(drop_fn, field_ptr),
                        flag: Some(flag),
                    });
                }
            }
        }
        Ok(())
    }

    pub(crate) fn extract_named_type(ty: &ast::Type) -> Option<&ast::NamedType> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Some(named),
            ast::TypeKind::Reference(reference) => Self::extract_named_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => Self::extract_named_type(&pointer.inner),
            _ => None,
        }
    }

    pub(crate) fn is_pointer_or_reference(ty: &ast::Type) -> bool {
        matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        )
    }
}
