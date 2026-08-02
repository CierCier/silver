use std::ffi::CString;

use rustc_hash::FxHashMap as HashMap;

use inkwell::OptimizationLevel;
use inkwell::targets::TargetMachine;

use crate::codegen::llvm_ir::{DeferAction, DeferredEntry, FunctionSig};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use llvm_sys::transforms::pass_builder::LLVMRunPasses;

use crate::attributes::function_link_name;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::{CodegenError, CodegenResult, SilverGenerator};
use crate::parser::ast;
use crate::symbol_table::{CompilerPhase, SymbolKind};
pub(crate) fn choose_enum_backing_type(min_value: i128, max_value: i128) -> ast::PrimitiveType {
    if min_value < 0 {
        if min_value >= i8::MIN as i128 && max_value <= i8::MAX as i128 {
            ast::PrimitiveType::I8
        } else if min_value >= i16::MIN as i128 && max_value <= i16::MAX as i128 {
            ast::PrimitiveType::I16
        } else if min_value >= i32::MIN as i128 && max_value <= i32::MAX as i128 {
            ast::PrimitiveType::I32
        } else if min_value >= i64::MIN as i128 && max_value <= i64::MAX as i128 {
            ast::PrimitiveType::I64
        } else {
            ast::PrimitiveType::I128
        }
    } else if max_value <= u8::MAX as i128 {
        ast::PrimitiveType::U8
    } else if max_value <= u16::MAX as i128 {
        ast::PrimitiveType::U16
    } else if max_value <= u32::MAX as i128 {
        ast::PrimitiveType::U32
    } else if max_value <= u64::MAX as i128 {
        ast::PrimitiveType::U64
    } else {
        ast::PrimitiveType::U128
    }
}

pub(crate) fn map_opt_level(opt_level: Option<&str>) -> OptimizationLevel {
    match opt_level.unwrap_or("0") {
        "0" => OptimizationLevel::None,
        "1" => OptimizationLevel::Less,
        "2" | "s" | "z" | "fast" => OptimizationLevel::Default,
        "3" => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    }
}

/// Run a lightweight LLVM optimization pipeline on a module before machine-code
/// emission. Uses the new pass manager (LLVM 17+) via LLVMRunPasses.
/// At opt-level 0 this is a no-op.
pub(crate) fn run_module_optimization_passes(
    module: &Module<'_>,
    machine: &TargetMachine,
    opt_level: Option<&str>,
) -> CodegenResult<()> {
    let level = opt_level.unwrap_or("0");
    if level == "0" {
        return Ok(());
    }
    // Build a lightweight pipeline matching the plan's original intent:
    //   mem2reg, instcombine, reassociate, gvn, simplifycfg
    // At higher opt levels also run an extra instcombine cleanup.
    //
    // instcombine<no-verify-fixpoint> disables instcombine's fixpoint
    // *verification* (a sanity check). LLVM 22 falsely trips it on `nuw` GEPs
    // from struct field access, erroring "did not reach a fixpoint"; the pass
    // itself terminates correctly, so the check is safe to disable.
    let pipeline = if matches!(level, "3" | "s" | "z" | "fast") {
        CString::new(
            "mem2reg,instcombine<no-verify-fixpoint>,reassociate,gvn,simplifycfg,instcombine<no-verify-fixpoint>",
        )
    } else {
        CString::new("mem2reg,instcombine<no-verify-fixpoint>,reassociate,gvn,simplifycfg")
    }
    .expect("pipeline CString should not contain null bytes");

    let options = PassBuilderOptions::create();
    unsafe {
        let err = LLVMRunPasses(
            module.as_mut_ptr(),
            pipeline.as_ptr(),
            machine.as_mut_ptr(),
            options.as_mut_ptr(),
        );
        if !err.is_null() {
            return Err(CodegenError::new(
                "LLVM optimization pipeline returned an error",
            ));
        }
    }
    Ok(())
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn finalize_debug(&mut self) {
        if let Some(debug) = self.debug.take() {
            debug.finalize();
        }
    }

    /// Declare the global __silver_leak_check_enabled i1 flag.
    /// Called at the start of generate_program() before pass 1.
    pub(crate) fn declare_leak_check_flag(&self) {
        let global = self.module.add_global(
            self.context.bool_type(),
            None,
            "__silver_leak_check_enabled",
        );
        global.set_constant(true);
        let initializer = self
            .context
            .bool_type()
            .const_int(self.leak_check as u64, false);
        global.set_initializer(&initializer);
    }
}

impl<'ctx> SilverGenerator for LlvmIrGenerator<'ctx> {
    fn generate_program(&mut self, program: &ast::Program) -> CodegenResult<()> {
        self.declare_leak_check_flag();
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(struct_item) => {
                    self.generate_struct_item(struct_item, &item.visibility, &item.attributes)?;
                }
                ast::ItemKind::Enum(enum_item) => {
                    self.generate_enum_item(enum_item, &item.visibility, &item.attributes)?;
                }
                _ => {}
            }
        }

        // Pass 1a: Declare all functions (declaration only, no bodies) so forward references resolve.
        for item in &program.items {
            if let ast::ItemKind::Function(function_item) = &item.kind {
                if function_item.generics.is_some() {
                    continue;
                }
                if self.has_generic_placeholder_signature(
                    &function_item.parameters,
                    function_item.return_type.as_ref(),
                ) {
                    continue;
                }
                if self.module.get_function(&function_item.name.name).is_some() {
                    continue;
                }
                self.register_function_signature(
                    &function_item.name.name,
                    FunctionSig {
                        params: function_item
                            .parameters
                            .iter()
                            .map(|param| param.param_type.clone())
                            .collect(),
                        return_type: function_item.return_type.clone(),
                        is_variadic: function_item.is_variadic,
                        linkage: None,
                    },
                    Some(function_item.name.span),
                    SymbolKind::Function,
                );
                let fn_ty = self.lower_function_type(
                    &function_item
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect::<Vec<_>>(),
                    function_item.return_type.as_ref(),
                    function_item.is_variadic,
                    None,
                )?;
                self.module
                    .add_function(&function_item.name.name, fn_ty, None);
            }
        }

        // Pass 1b: collect declarations/types so forward references can resolve.
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(struct_item) => {
                    self.generate_struct_item(struct_item, &item.visibility, &item.attributes)?;
                }
                ast::ItemKind::Function(function_item) => {
                    self.generate_function_item(function_item, &item.visibility, &item.attributes)?;
                }
                ast::ItemKind::ExternFunction(extern_function_item) => {
                    self.generate_extern_function_item(
                        extern_function_item,
                        &item.visibility,
                        &item.attributes,
                    )?;
                }
                ast::ItemKind::ExternVariable(extern_variable_item) => {
                    self.generate_extern_variable_item(
                        extern_variable_item,
                        &item.visibility,
                        &item.attributes,
                    )?;
                }
                ast::ItemKind::GlobalVariable(global_variable_item) => {
                    self.generate_global_variable_item(global_variable_item, &item.visibility)?;
                }
                ast::ItemKind::ExternBlock(extern_block_item) => {
                    self.generate_extern_block_item(
                        extern_block_item,
                        &item.visibility,
                        &item.attributes,
                    )?;
                }
                ast::ItemKind::Impl(impl_item) => {
                    if self.has_generic_placeholder_type(&impl_item.self_type) {
                        self.generic_impl_templates.push(impl_item.clone());
                    }
                    self.collect_impl_method_signatures(impl_item, &item.visibility)?;
                    if Self::is_drop_trait_impl(impl_item)
                        && impl_item.generics.is_none()
                        && !self.has_generic_placeholder_type(&impl_item.self_type)
                        && let Some(owner) = Self::owner_name_from_type(&impl_item.self_type)
                    {
                        self.drop_trait_impl_owners.insert(owner);
                    }
                }
                _ => {}
            }
        }

        // Pass 2: emit item bodies/remaining lowering.
        for item in &program.items {
            // Collect doc comments for functions so finish() can emit them as
            // `;` comments in the printed IR (non-generic functions only —
            // monomorphized instances would repeat the comment per instance).
            if let ast::ItemKind::Function(func) = &item.kind
                && func.generics.is_none()
                && let Some(doc) = program.doc_comment_for(item)
            {
                let name = function_link_name(&item.attributes)
                    .map(str::to_string)
                    .unwrap_or_else(|| func.name.name.clone());
                self.doc_comments.push((name, doc));
            }
            self.generate_item(item)?;
        }
        Ok(())
    }

    fn generate_item(&mut self, item: &ast::Item) -> CodegenResult<()> {
        match &item.kind {
            ast::ItemKind::Function(function_item) => {
                self.generate_function_item(function_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::GlobalVariable(global_variable_item) => {
                self.generate_global_variable_item(global_variable_item, &item.visibility)
            }
            ast::ItemKind::Struct(struct_item) => {
                self.generate_struct_item(struct_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::Enum(enum_item) => {
                self.generate_enum_item(enum_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::Impl(impl_item) => {
                self.generate_impl_item(impl_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::Trait(trait_item) => {
                self.generate_trait_item(trait_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::Import(import_item) => {
                self.generate_import_item(import_item, &item.visibility, &item.attributes)
            }
            ast::ItemKind::ExternFunction(extern_function_item) => self
                .generate_extern_function_item(
                    extern_function_item,
                    &item.visibility,
                    &item.attributes,
                ),
            ast::ItemKind::ExternVariable(extern_variable_item) => self
                .generate_extern_variable_item(
                    extern_variable_item,
                    &item.visibility,
                    &item.attributes,
                ),
            ast::ItemKind::ExternBlock(extern_block_item) => self.generate_extern_block_item(
                extern_block_item,
                &item.visibility,
                &item.attributes,
            ),
            ast::ItemKind::Macro(_) => Ok(()),
            ast::ItemKind::TypeAlias(_) => Ok(()),
        }
    }

    fn generate_function_item(
        &mut self,
        func: &ast::FunctionItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        if func.generics.is_some() {
            return Ok(());
        }
        if self.has_generic_placeholder_signature(&func.parameters, func.return_type.as_ref()) {
            return Ok(());
        }

        let link_name = function_link_name(attributes);
        let llvm_name = link_name.unwrap_or(&func.name.name);

        self.register_function_signature(
            llvm_name,
            FunctionSig {
                params: func
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect(),
                return_type: func.return_type.clone(),
                is_variadic: func.is_variadic,
                linkage: None,
            },
            Some(func.name.span),
            SymbolKind::Function,
        );
        // Also register under source name so callers using it can resolve.
        if link_name.is_some() {
            self.register_function_signature(
                &func.name.name,
                FunctionSig {
                    params: func
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: func.return_type.clone(),
                    is_variadic: func.is_variadic,
                    linkage: None,
                },
                Some(func.name.span),
                SymbolKind::Function,
            );
        }

        if self.module.get_function(llvm_name).is_none() {
            let fn_ty = self.lower_function_type(
                &func
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect::<Vec<_>>(),
                func.return_type.as_ref(),
                func.is_variadic,
                None,
            )?;
            let function = self.module.add_function(llvm_name, fn_ty, None);
            Self::apply_function_linkage(function, visibility);
        }

        let Some(function) = self.module.get_function(llvm_name) else {
            return Err(CodegenError::with_span(
                format!("function `{}` declaration is missing", func.name.name),
                func.name.span,
            ));
        };
        Self::apply_function_linkage(function, visibility);
        // Functions named `_start` are entry points for no-libc binaries. Only
        // apply the `naked` attribute when the body is a single asm statement —
        // a user-written `_start` with non-asm body should NOT get naked (the
        // compiler will emit a normal prologue, crashing at runtime, but that
        // is better than silent UB with naked).
        if llvm_name == "_start" {
            let body_is_pure_asm = func.body.statements.len() == 1
                && match &func.body.statements[0].kind {
                    ast::StatementKind::Expression(expr) => {
                        matches!(&*expr.kind, ast::ExpressionKind::Asm { .. })
                    }
                    _ => false,
                };
            if body_is_pure_asm {
                let naked_kind = Attribute::get_named_enum_kind_id("naked");
                let naked_attr = self.context.create_enum_attribute(naked_kind, 0);
                function.add_attribute(AttributeLoc::Function, naked_attr);
            }
        }

        self.emit_function_body(
            function,
            &func.parameters,
            func.return_type.as_ref(),
            &func.body,
            llvm_name,
            &func.name.span,
            false,
        )
    }

    fn generate_struct_item(
        &mut self,
        item: &ast::StructItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        let name = item.name.name.clone();
        self.struct_fields.insert(
            name.clone(),
            item.fields
                .iter()
                .map(|field| (field.name.name.clone(), field.field_type.clone()))
                .collect(),
        );

        if let Some(generics) = &item.generics {
            let params = generics
                .params
                .iter()
                .filter_map(|param| match param {
                    ast::GenericParam::Type(type_param) => Some(type_param.name.name.clone()),
                    ast::GenericParam::Lifetime(_) => None,
                })
                .collect::<Vec<_>>();
            self.struct_generics.insert(name, params);
            self.struct_types
                .entry(item.name.name.clone())
                .or_insert_with(|| self.context.opaque_struct_type(&item.name.name));
            return Ok(());
        }

        self.struct_generics.remove(&name);

        let struct_ty = *self
            .struct_types
            .entry(name.clone())
            .or_insert_with(|| self.context.opaque_struct_type(&name));
        if !struct_ty.is_opaque() {
            return Ok(());
        }

        let mut field_types = Vec::with_capacity(item.fields.len());
        for field in &item.fields {
            field_types.push(self.lower_basic_type(&field.field_type)?);
        }
        struct_ty.set_body(&field_types, false);

        Ok(())
    }

    fn generate_enum_item(
        &mut self,
        item: &ast::EnumItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        let mut variants = HashMap::default();
        let mut next_value = 0i128;
        let mut min_value = 0i128;
        let mut max_value = 0i128;
        let mut saw_any = false;
        let mut has_payload = false;
        let mut max_payload_size: u64 = 0;

        for variant in &item.variants {
            let value = variant.discriminant.unwrap_or(next_value);
            variants.insert(variant.name.name.clone(), value);
            next_value = value.checked_add(1).unwrap_or(value);
            if !saw_any {
                min_value = value;
                max_value = value;
                saw_any = true;
            } else {
                min_value = min_value.min(value);
                max_value = max_value.max(value);
            }
            // Compute payload size for this variant using known type sizes
            let payload_types = match &variant.data {
                ast::EnumVariantData::Unit => vec![],
                ast::EnumVariantData::Tuple(types) => types.clone(),
                ast::EnumVariantData::Struct(fields) => {
                    fields.iter().map(|f| f.field_type.clone()).collect()
                }
            };
            if !payload_types.is_empty() {
                has_payload = true;
                self.enum_variant_payload_types
                    .entry(item.name.name.clone())
                    .or_default()
                    .insert(variant.name.name.clone(), payload_types.clone());
            }
            let mut variant_size: u64 = 0;
            for pt in &payload_types {
                match pt.kind.as_ref() {
                    ast::TypeKind::Primitive(p) => {
                        variant_size += match p {
                            ast::PrimitiveType::I8
                            | ast::PrimitiveType::U8
                            | ast::PrimitiveType::Bool => 1,
                            ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => 2,
                            ast::PrimitiveType::I32
                            | ast::PrimitiveType::U32
                            | ast::PrimitiveType::Char => 4,
                            ast::PrimitiveType::F32 => 4,
                            ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => 8,
                            ast::PrimitiveType::F64 => 8,
                            ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => 16,
                            _ => 8,
                        };
                    }
                    ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_) => {
                        variant_size += 8;
                    }
                    _ => variant_size += 8,
                }
            }
            max_payload_size = max_payload_size.max(variant_size);
        }

        if has_payload {
            let i16_ty = self.context.i16_type();
            let array_ty = self.context.i8_type().array_type(max_payload_size as u32);
            let struct_ty = self
                .context
                .struct_type(&[i16_ty.into(), array_ty.into()], false);
            struct_ty.set_body(&[i16_ty.into(), array_ty.into()], false);
            self.enum_payload_layouts
                .insert(item.name.name.clone(), struct_ty);
            self.struct_types.insert(item.name.name.clone(), struct_ty);
        }

        self.enum_backing_types.insert(
            item.name.name.clone(),
            choose_enum_backing_type(min_value, max_value),
        );
        self.enum_variants.insert(item.name.name.clone(), variants);
        Ok(())
    }

    fn generate_impl_item(
        &mut self,
        item: &ast::ImplItem,
        visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        if item.generics.is_some() {
            return Ok(());
        }
        if self.has_generic_placeholder_type(&item.self_type) {
            return Ok(());
        }
        if self.has_generic_placeholder_type(&item.self_type) {
            return Ok(());
        }

        let Some(owner) = Self::owner_name_from_type(&item.self_type) else {
            return Ok(());
        };

        for impl_item in &item.items {
            match impl_item {
                ast::ImplItemKind::Function(func) => {
                    if func.generics.is_some() {
                        continue;
                    }
                    if self.has_generic_placeholder_signature(
                        &func.parameters,
                        func.return_type.as_ref(),
                    ) {
                        continue;
                    }

                    let mangled_name = Self::mangle_method_name(&owner, &func.name.name);
                    let effective_visibility =
                        Self::method_effective_visibility(visibility, &func.visibility);
                    if self.module.get_function(&mangled_name).is_none() {
                        let fn_ty = self.lower_function_type(
                            &func
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect::<Vec<_>>(),
                            func.return_type.as_ref(),
                            false,
                            None,
                        )?;
                        let function = self.module.add_function(&mangled_name, fn_ty, None);
                        Self::apply_function_linkage(function, &effective_visibility);
                    }

                    let Some(function) = self.module.get_function(&mangled_name) else {
                        return Err(CodegenError::with_span(
                            format!("method `{mangled_name}` declaration is missing"),
                            func.span,
                        ));
                    };
                    Self::apply_function_linkage(function, &effective_visibility);

                    self.emit_function_body(
                        function,
                        &func.parameters,
                        func.return_type.as_ref(),
                        &func.body,
                        &mangled_name,
                        &func.name.span,
                        false,
                    )?;
                }
                ast::ImplItemKind::Cast(cast) => {
                    let cast_method_name = Self::cast_method_name(&cast.target_type);
                    let mangled_name = Self::mangle_method_name(&owner, &cast_method_name);
                    let effective_visibility =
                        Self::method_effective_visibility(visibility, &ast::Visibility::Private);
                    if self.module.get_function(&mangled_name).is_none() {
                        let fn_ty = self.lower_function_type(
                            &cast
                                .parameters
                                .iter()
                                .map(|param| param.param_type.clone())
                                .collect::<Vec<_>>(),
                            Some(&cast.target_type),
                            false,
                            None,
                        )?;
                        let function = self.module.add_function(&mangled_name, fn_ty, None);
                        Self::apply_function_linkage(function, &effective_visibility);
                    }

                    let Some(function) = self.module.get_function(&mangled_name) else {
                        return Err(CodegenError::with_span(
                            format!("cast function `{mangled_name}` declaration is missing"),
                            cast.span,
                        ));
                    };
                    Self::apply_function_linkage(function, &effective_visibility);

                    // Cast receivers are borrowed: skip the by-value self
                    // param's destructor so it does not free the caller's
                    // resources (see emit_function_body).
                    self.emit_function_body(
                        function,
                        &cast.parameters,
                        Some(&cast.target_type),
                        &cast.body,
                        &mangled_name,
                        &cast.span,
                        true,
                    )?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn generate_trait_item(
        &mut self,
        _item: &ast::TraitItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        // Traits are a compile-time abstraction; no direct LLVM IR is emitted.
        // Trait methods are concretized through impl blocks (generate_impl_item).
        Ok(())
    }

    fn generate_import_item(
        &mut self,
        _item: &ast::ImportItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        // Imports are lowered before codegen by parser hooks.
        Ok(())
    }

    fn generate_extern_function_item(
        &mut self,
        item: &ast::ExternFunctionItem,
        _visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        let link_name = function_link_name(attributes);
        let llvm_name = link_name.unwrap_or(&item.name.name);

        self.register_function_signature(
            llvm_name,
            FunctionSig {
                params: item
                    .signature
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect(),
                return_type: item.signature.return_type.clone(),
                is_variadic: item.signature.is_variadic,
                linkage: Some(item.linkage.clone()),
            },
            Some(item.name.span),
            SymbolKind::ExternFunction,
        );
        // Also register under source name so callers using it can resolve.
        if link_name.is_some() {
            self.register_function_signature(
                &item.name.name,
                FunctionSig {
                    params: item
                        .signature
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: item.signature.return_type.clone(),
                    is_variadic: item.signature.is_variadic,
                    linkage: Some(item.linkage.clone()),
                },
                Some(item.name.span),
                SymbolKind::ExternFunction,
            );
        }

        if self.module.get_function(llvm_name).is_none() {
            let sig = FunctionSig {
                params: item
                    .signature
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect(),
                return_type: item.signature.return_type.clone(),
                is_variadic: item.signature.is_variadic,
                linkage: Some(item.linkage.clone()),
            };
            let fn_ty = self.lower_function_type(
                &sig.params,
                sig.return_type.as_ref(),
                sig.is_variadic,
                sig.linkage.clone(),
            )?;
            let function = self.module.add_function(llvm_name, fn_ty, None);
            self.apply_abi_attributes(function, &sig)?;
        }

        Ok(())
    }

    fn generate_extern_variable_item(
        &mut self,
        item: &ast::ExternVariableItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        let llvm_ty = self.lower_basic_type(&item.var_type)?;
        let global = self
            .module
            .get_global(&item.name.name)
            .unwrap_or_else(|| self.module.add_global(llvm_ty, None, &item.name.name));
        global.set_linkage(Linkage::External);
        self.extern_globals
            .insert(item.name.name.clone(), item.var_type.clone());
        self.symbol_table.intern_symbol(
            format!("codegen::extern_var::{}", item.name.name),
            SymbolKind::ExternVariable,
            Some(item.name.span),
            CompilerPhase::Codegen,
        );
        Ok(())
    }

    fn generate_extern_block_item(
        &mut self,
        item: &ast::ExternBlockItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        for function in &item.functions {
            self.generate_extern_function_item(function, visibility, attributes)?;
        }
        for variable in &item.variables {
            self.generate_extern_variable_item(variable, visibility, attributes)?;
        }
        Ok(())
    }
    fn generate_block(&mut self, block: &ast::Block) -> CodegenResult<()> {
        let has_debug_scope = if let Some(debug) = &mut self.debug {
            let (line, col, _, _) = debug.source_map.span_to_line_col(&block.span);
            debug.push_lexical_block(line, col);
            true
        } else {
            false
        };

        self.push_scope();
        for statement in &block.statements {
            self.generate_statement(statement)?;
            let terminated = self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some();
            if terminated {
                break;
            }
        }
        // Only fire defers for the innermost scope if the block hasn't
        // already been terminated (e.g. by a return/break/continue that
        // already handled the defers itself).
        if !self
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_terminator())
            .is_some()
        {
            self.emit_defers(1)?;
        }
        self.pop_scope();

        if has_debug_scope && let Some(debug) = &mut self.debug {
            debug.pop_lexical_block();
        }

        Ok(())
    }

    fn generate_statement(&mut self, statement: &ast::Statement) -> CodegenResult<()> {
        self.set_debug_location(&statement.span);
        match &statement.kind {
            ast::StatementKind::Block(block) => self.generate_block(block),
            ast::StatementKind::Expression(expr) => match expr.kind.as_ref() {
                ast::ExpressionKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => self.emit_if_statement(
                    condition,
                    then_branch,
                    else_branch.as_ref(),
                    &statement.span,
                ),
                ast::ExpressionKind::While { condition, body } => {
                    self.emit_while_statement(condition, body)
                }
                ast::ExpressionKind::For {
                    init,
                    condition,
                    increment,
                    body,
                } => self.emit_for_statement(init, condition, increment, body, &statement.span),
                ast::ExpressionKind::Match { expression, arms } => {
                    self.emit_match_statement(expression, arms)
                }
                ast::ExpressionKind::Block(block) => self.generate_block(block),
                ast::ExpressionKind::ForIn {
                    binding,
                    is_mutable,
                    iterable,
                    body,
                    item_type,
                    mode,
                    iterator_type,
                } => self.emit_for_in_statement(
                    binding,
                    *is_mutable,
                    iterable,
                    body,
                    item_type.as_deref(),
                    iterator_type.as_deref(),
                    *mode,
                    &statement.span,
                ),
                _ => self.emit_expression_statement(expr),
            },
            ast::StatementKind::Let(let_stmt) => self.emit_let_statement(let_stmt, &statement.span),
            ast::StatementKind::Defer(inner) => {
                if let Some(scope) = self.defers.last_mut() {
                    scope.push(DeferredEntry {
                        action: DeferAction::Statement(*inner.clone()),
                        flag: None,
                    });
                }
                Ok(())
            }
            ast::StatementKind::Return(expr) => {
                // Evaluate return value FIRST, before running defers.
                // This prevents use-after-free when the return expression
                // references a variable whose destructor would be fired by
                // emit_defers (Bug A).
                let saved_value = if let Some(expr) = expr {
                    let expr_span = expr.span;
                    // Reject bare identifier return of droppable locals.
                    // `return move x;` already clears the drop flag in expr.rs.
                    if let ast::ExpressionKind::Identifier(ident) = expr.kind.as_ref()
                        && self.drop_flags.contains_key(&ident.name)
                    {
                        return Err(CodegenError::with_span(
                            format!(
                                "return of droppable local '{}' requires 'move {}'",
                                ident.name, ident.name
                            ),
                            expr_span,
                        ));
                    }
                    let mut value = self.emit_expression_value(expr)?;
                    if let Some(return_ty) = self.current_return_type.clone() {
                        value = self.cast_value_to_ast_type(value, &return_ty, &expr_span)?;
                    }
                    Some((value, expr_span))
                } else {
                    None
                };
                // Now run defers (the return value is safely saved)
                self.emit_defers(self.defers.len())?;
                // Return the saved value
                if let Some((value, span)) = saved_value {
                    self.builder.build_return(Some(&value)).map_err(|e| {
                        CodegenError::with_span(format!("failed to emit return value: {e}"), span)
                    })?;
                } else {
                    self.builder.build_return(None).map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit return: {e}"),
                            statement.span,
                        )
                    })?;
                }
                Ok(())
            }
            ast::StatementKind::Break(_) => {
                let loop_base = self.loop_defers_base.last().copied().unwrap_or(0);
                let levels = self.defers.len().saturating_sub(loop_base);
                self.emit_defers(levels)?;
                let Some((break_block, _)) = self.loop_stack.last().copied() else {
                    return Err(CodegenError::with_span(
                        "break used outside of a loop",
                        statement.span,
                    ));
                };
                self.builder
                    .build_unconditional_branch(break_block)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit break branch: {e}"),
                            statement.span,
                        )
                    })?;
                Ok(())
            }
            ast::StatementKind::Continue => {
                let loop_base = self.loop_defers_base.last().copied().unwrap_or(0);
                let levels = self.defers.len().saturating_sub(loop_base);
                self.emit_defers(levels)?;
                let Some((_, continue_block)) = self.loop_stack.last().copied() else {
                    return Err(CodegenError::with_span(
                        "continue used outside of a loop",
                        statement.span,
                    ));
                };
                self.builder
                    .build_unconditional_branch(continue_block)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit continue branch: {e}"),
                            statement.span,
                        )
                    })?;
                Ok(())
            }
        }
    }

    fn generate_expression(&mut self, expr: &ast::Expression) -> CodegenResult<()> {
        let _ = self.emit_expression_value(expr)?;
        Ok(())
    }

    fn generate_pattern(&mut self, pattern: &ast::Pattern) -> CodegenResult<()> {
        Err(CodegenError::with_span(
            "patterns must be generated in context (let binding or match arm)",
            pattern.span,
        ))
    }

    fn generate_type(&mut self, ty: &ast::Type) -> CodegenResult<()> {
        // Ensure the type is lowered and any dependent types (e.g., struct definitions)
        // are registered in the LLVM module.
        let _ = self.lower_basic_type(ty)?;
        Ok(())
    }

    fn generate_initializer_item(&mut self, _item: &ast::InitializerItem) -> CodegenResult<()> {
        Err(CodegenError::new(
            "initializer items must be generated with a target type context (use emit_typed_initializer_value or emit_const_initializer_value)",
        ))
    }

    fn generate_macro_arg(&mut self, _arg: &ast::MacroArg) -> CodegenResult<()> {
        Err(CodegenError::new(
            "macro arguments should be expanded before LLVM codegen",
        ))
    }

    fn generate_literal(&mut self, literal: &ast::Literal) -> CodegenResult<()> {
        // Scalar literals are emitted inline by emit_expression_value;
        // standalone generation has no side effect for non-string types.
        if let ast::Literal::String(value) = literal {
            let _ = self.intern_const_string_global(value);
        }
        Ok(())
    }

    fn finish(self) -> String {
        if let Some(debug) = self.debug {
            debug.finalize();
        }
        let ir = self.module.print_to_string().to_string();
        let mut result = ir;
        for (name, doc) in &self.doc_comments {
            result = splice_doc_comment(&result, name, doc);
        }
        result
    }
}

/// Insert a `; <doc>` comment block into the printed LLVM IR immediately
/// before the `define` line of the function `@name`.
fn splice_doc_comment(ir: &str, name: &str, doc: &str) -> String {
    let needle = format!("@{name}");
    let mut lines: Vec<String> = ir.lines().map(str::to_string).collect();
    let mut inserted = false;
    for (i, line) in lines.iter().enumerate() {
        if line.trim_start().starts_with("define") && line.contains(&needle) {
            let mut comment_lines: Vec<String> =
                doc.lines().map(|line| format!("; {line}")).collect();
            comment_lines.insert(0, String::new()); // blank line for readability
            lines.splice(i..i, comment_lines);
            inserted = true;
            break;
        }
    }
    let mut out = lines.join("\n");
    if inserted && !ir.ends_with('\n') {
        out.push('\n');
    }
    out
}
