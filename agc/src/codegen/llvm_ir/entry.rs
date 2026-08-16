use rustc_hash::FxHashSet as HashSet;
use std::path::Path;

use rustc_hash::FxHashMap as HashMap;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
};
use inkwell::types::BasicType;
use inkwell::values::AsValueRef;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};

use crate::codegen::SilverGenerator;
use crate::codegen::abi;
use crate::codegen::llvm_ir::VarInfo;
use crate::codegen::llvm_ir::generate;
use crate::codegen::llvm_ir::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::debug_info::DebugContext;
use crate::module_artifact::{ModuleArtifact, ast_type_from_canonical_key};
use crate::parser::ast;
use crate::symbol_table::{CompilerSymbolTable, SymbolKind};
use crate::types::Type;
impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn is_private(visibility: &ast::Visibility) -> bool {
        matches!(visibility, ast::Visibility::Private)
    }

    pub(crate) fn method_effective_visibility(
        impl_visibility: &ast::Visibility,
        method_visibility: &ast::Visibility,
    ) -> ast::Visibility {
        if Self::is_private(impl_visibility) || Self::is_private(method_visibility) {
            ast::Visibility::Private
        } else {
            ast::Visibility::Public
        }
    }

    pub(crate) fn apply_function_linkage(
        function: FunctionValue<'ctx>,
        visibility: &ast::Visibility,
    ) {
        if Self::is_private(visibility) {
            function.set_linkage(Linkage::Internal);
        } else {
            function.set_linkage(Linkage::External);
        }
    }

    /// Apply `#[inline(always)]` as the LLVM alwaysinline attribute, so the
    /// always-inline pass inlines the function into every caller.
    pub(crate) fn apply_inline_always_attribute(
        function: FunctionValue<'ctx>,
        attributes: &[ast::Attribute],
        context: &inkwell::context::Context,
    ) {
        if crate::attributes::function_always_inline(attributes) {
            let kind_id = inkwell::attributes::Attribute::get_named_enum_kind_id("alwaysinline");
            let attr = context.create_enum_attribute(kind_id, 0);
            function.add_attribute(inkwell::attributes::AttributeLoc::Function, attr);
        }
    }

    /// Apply `#[target_feature("name")]` attributes as an LLVM
    /// `target-features` function attribute, so the x86 backend may select
    /// instructions from the listed feature sets for this function only.
    /// Calling such a function on a CPU lacking the feature is illegal —
    /// guard with the runtime probes in `std/cpu.ag`.
    pub(crate) fn apply_target_feature_attributes(
        function: FunctionValue<'ctx>,
        attributes: &[ast::Attribute],
    ) {
        let Some(features) = crate::attributes::function_target_features(attributes) else {
            return;
        };
        let kind =
            std::ffi::CString::new("target-features").expect("target-features contains no NUL");
        let value = std::ffi::CString::new(features).expect("feature string contains no NUL");
        unsafe {
            llvm_sys::core::LLVMAddTargetDependentFunctionAttr(
                function.as_value_ref(),
                kind.as_ptr(),
                value.as_ptr(),
            );
        }
    }

    pub fn generate(program: &ast::Program) -> CodegenResult<String> {
        let mut table = CompilerSymbolTable::new();
        Self::generate_with_table(program, &mut table)
    }

    pub fn generate_with_table(
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<String> {
        Self::generate_with_table_and_source(program, table, None, None, false)
    }

    pub fn generate_with_table_and_source(
        program: &ast::Program,
        table: &mut CompilerSymbolTable,
        source_path: Option<&std::path::Path>,
        source_text: Option<&str>,
        debug_info: bool,
    ) -> CodegenResult<String> {
        let context = Context::create();
        let module = context.create_module("silver");
        let builder = context.create_builder();
        let debug = if debug_info {
            match (source_path, source_text) {
                (Some(path), Some(text)) => {
                    let main_file_id = crate::lexer::register_source(&path.to_string_lossy(), text);
                    Some(DebugContext::new(
                        &context,
                        &module,
                        main_file_id,
                        path,
                        text,
                    ))
                }
                _ => None,
            }
        } else {
            None
        };
        let mut generator = LlvmIrGenerator {
            context: &context,
            module,
            builder,
            current_fn: None,
            current_return_type: None,
            variables: vec![HashMap::default()],
            function_sigs: HashMap::default(),
            function_name_to_symbol: HashMap::default(),
            imported_function_links: HashMap::default(),
            extern_globals: HashMap::default(),
            global_variables: HashMap::default(),
            global_const_values: HashMap::default(),
            struct_types: HashMap::default(),
            struct_fields: HashMap::default(),
            enum_backing_types: HashMap::default(),
            enum_variants: HashMap::default(),
            enum_variant_payload_types: HashMap::default(),
            enum_payload_layouts: HashMap::default(),
            defers: vec![vec![]],
            volatile_globals: HashSet::default(),
            type_aliases: HashSet::default(),
            static_local_counter: 0,
            method_receivers: HashMap::default(),
            method_overload_signatures: HashMap::default(),
            string_constants: HashMap::default(),
            struct_generics: HashMap::default(),
            free_function_sigs: HashMap::default(),
            source_function_symbols: HashMap::default(),
            drop_trait_impl_owners: HashSet::default(),
            generic_impl_templates: Vec::new(),
            generic_function_templates: HashMap::default(),
            loop_stack: Vec::new(),
            doc_comments: Vec::new(),
            loop_defers_base: Vec::new(),
            symbol_table: table.clone(),
            temp_counter: 0,
            task_trampoline_counter: 0,
            debug,
            debug_nested: false,
            fn_source_info: rustc_hash::FxHashMap::default(),
            abi_handler: abi::get_abi_handler("x86_64-unknown-linux-gnu"),
            leak_check: false,
        };
        generator.generate_program(program)?;
        table.absorb_from(&generator.symbol_table);
        Ok(generator.finish())
    }

    pub fn generate_with_imports_and_table(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<String> {
        Self::generate_with_imports_and_table_and_source(
            program,
            imported_modules,
            table,
            None,
            None,
            false,
        )
    }

    pub fn generate_with_imports_and_table_and_source(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        table: &mut CompilerSymbolTable,
        source_path: Option<&std::path::Path>,
        source_text: Option<&str>,
        debug_info: bool,
    ) -> CodegenResult<String> {
        Self::generate_with_imports_and_table_and_source_with_leak_check(
            program,
            imported_modules,
            table,
            source_path,
            source_text,
            debug_info,
            false,
        )
    }
    pub fn generate_with_imports_and_table_and_source_with_leak_check(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        table: &mut CompilerSymbolTable,
        source_path: Option<&std::path::Path>,
        source_text: Option<&str>,
        debug_info: bool,
        leak_check: bool,
    ) -> CodegenResult<String> {
        let context = Context::create();
        let module = context.create_module("silver");
        let builder = context.create_builder();
        let debug = if debug_info {
            match (source_path, source_text) {
                (Some(path), Some(text)) => {
                    let main_file_id = crate::lexer::register_source(&path.to_string_lossy(), text);
                    Some(DebugContext::new(
                        &context,
                        &module,
                        main_file_id,
                        path,
                        text,
                    ))
                }
                _ => None,
            }
        } else {
            None
        };
        let mut generator = LlvmIrGenerator {
            context: &context,
            module,
            builder,
            current_fn: None,
            current_return_type: None,
            variables: vec![HashMap::default()],
            function_sigs: HashMap::default(),
            function_name_to_symbol: HashMap::default(),
            imported_function_links: HashMap::default(),
            extern_globals: HashMap::default(),
            global_variables: HashMap::default(),
            global_const_values: HashMap::default(),
            struct_types: HashMap::default(),
            struct_fields: HashMap::default(),
            enum_backing_types: HashMap::default(),
            enum_variant_payload_types: HashMap::default(),
            enum_variants: HashMap::default(),
            enum_payload_layouts: HashMap::default(),
            defers: vec![vec![]],
            volatile_globals: HashSet::default(),
            type_aliases: HashSet::default(),
            static_local_counter: 0,
            method_receivers: HashMap::default(),
            method_overload_signatures: HashMap::default(),
            string_constants: HashMap::default(),
            struct_generics: HashMap::default(),
            free_function_sigs: HashMap::default(),
            source_function_symbols: HashMap::default(),
            generic_impl_templates: Vec::new(),
            generic_function_templates: HashMap::default(),
            drop_trait_impl_owners: HashSet::default(),
            loop_stack: Vec::new(),
            doc_comments: Vec::new(),
            loop_defers_base: Vec::new(),
            symbol_table: table.clone(),
            debug,
            debug_nested: false,
            fn_source_info: rustc_hash::FxHashMap::default(),
            abi_handler: abi::get_abi_handler("x86_64-unknown-linux-gnu"),
            temp_counter: 0,
            task_trampoline_counter: 0,
            leak_check,
        };
        generator.declare_imported_modules(imported_modules)?;
        generator.generate_program(program)?;
        table.absorb_from(&generator.symbol_table);
        Ok(generator.finish())
    }

    pub fn emit_object_file(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
    ) -> CodegenResult<()> {
        let mut table = CompilerSymbolTable::new();
        Self::emit_object_file_with_table(program, path, target_triple, opt_level, &mut table)
    }

    pub fn emit_object_file_with_table(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<()> {
        Self::emit_target_file(
            program,
            path,
            target_triple,
            opt_level,
            FileType::Object,
            table,
            None,
            None,
            false,
        )
    }

    pub fn emit_object_file_with_imports_and_table(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<()> {
        Self::emit_target_file_with_imports(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            FileType::Object,
            table,
            None,
            None,
            false,
            false,
        )
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub fn emit_object_file_with_imports_and_table_and_source(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
    ) -> CodegenResult<()> {
        Self::emit_object_file_with_imports_and_table_and_source_with_leak_check(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            table,
            source_path,
            source_text,
            debug_info,
            false,
        )
    }
    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub fn emit_object_file_with_imports_and_table_and_source_with_leak_check(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
        leak_check: bool,
    ) -> CodegenResult<()> {
        Self::emit_target_file_with_imports(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            FileType::Object,
            table,
            source_path,
            source_text,
            debug_info,
            leak_check,
        )
    }

    pub fn emit_assembly_file(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
    ) -> CodegenResult<()> {
        let mut table = CompilerSymbolTable::new();
        Self::emit_assembly_file_with_table(program, path, target_triple, opt_level, &mut table)
    }

    pub fn emit_assembly_file_with_table(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<()> {
        Self::emit_target_file(
            program,
            path,
            target_triple,
            opt_level,
            FileType::Assembly,
            table,
            None,
            None,
            false,
        )
    }

    pub fn emit_assembly_file_with_imports_and_table(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<()> {
        Self::emit_target_file_with_imports(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            FileType::Assembly,
            table,
            None,
            None,
            false,
            false,
        )
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub fn emit_assembly_file_with_imports_and_table_and_source(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
    ) -> CodegenResult<()> {
        Self::emit_assembly_file_with_imports_and_table_and_source_with_leak_check(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            table,
            source_path,
            source_text,
            debug_info,
            false,
        )
    }
    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub fn emit_assembly_file_with_imports_and_table_and_source_with_leak_check(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
        leak_check: bool,
    ) -> CodegenResult<()> {
        Self::emit_target_file_with_imports(
            program,
            imported_modules,
            path,
            target_triple,
            opt_level,
            FileType::Assembly,
            table,
            source_path,
            source_text,
            debug_info,
            leak_check,
        )
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    fn emit_target_file(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        file_type: FileType,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
    ) -> CodegenResult<()> {
        Self::emit_target_file_with_imports(
            program,
            &[],
            path,
            target_triple,
            opt_level,
            file_type,
            table,
            source_path,
            source_text,
            debug_info,
            false,
        )
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    fn emit_target_file_with_imports(
        program: &ast::Program,
        imported_modules: &[ModuleArtifact],
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        file_type: FileType,
        table: &mut CompilerSymbolTable,
        source_path: Option<&Path>,
        source_text: Option<&str>,
        debug_info: bool,
        leak_check: bool,
    ) -> CodegenResult<()> {
        let context = Context::create();
        let module = context.create_module("silver");
        let builder = context.create_builder();
        let debug = if debug_info {
            match (source_path, source_text) {
                (Some(p), Some(text)) => {
                    let main_file_id = crate::lexer::register_source(&p.to_string_lossy(), text);
                    Some(DebugContext::new(&context, &module, main_file_id, p, text))
                }
                _ => None,
            }
        } else {
            None
        };
        let mut generator = LlvmIrGenerator {
            context: &context,
            module,
            builder,
            current_fn: None,
            current_return_type: None,
            variables: vec![HashMap::default()],
            function_sigs: HashMap::default(),
            function_name_to_symbol: HashMap::default(),
            imported_function_links: HashMap::default(),
            extern_globals: HashMap::default(),
            global_variables: HashMap::default(),
            global_const_values: HashMap::default(),
            struct_types: HashMap::default(),
            struct_fields: HashMap::default(),
            enum_variant_payload_types: HashMap::default(),
            enum_backing_types: HashMap::default(),
            enum_variants: HashMap::default(),
            enum_payload_layouts: HashMap::default(),
            defers: vec![vec![]],
            volatile_globals: HashSet::default(),
            type_aliases: HashSet::default(),
            static_local_counter: 0,
            method_receivers: HashMap::default(),
            method_overload_signatures: HashMap::default(),
            string_constants: HashMap::default(),
            struct_generics: HashMap::default(),
            free_function_sigs: HashMap::default(),
            source_function_symbols: HashMap::default(),
            generic_impl_templates: Vec::new(),
            generic_function_templates: HashMap::default(),
            drop_trait_impl_owners: HashSet::default(),
            loop_stack: Vec::new(),
            doc_comments: Vec::new(),
            loop_defers_base: Vec::new(),
            symbol_table: table.clone(),
            debug,
            debug_nested: false,
            fn_source_info: rustc_hash::FxHashMap::default(),
            abi_handler: abi::get_abi_handler(target_triple.unwrap_or("x86_64-unknown-linux-gnu")),
            temp_counter: 0,
            task_trampoline_counter: 0,
            leak_check,
        };
        generator.declare_imported_modules(imported_modules)?;

        Target::initialize_all(&InitializationConfig::default());
        let triple = target_triple
            .map(inkwell::targets::TargetTriple::create)
            .unwrap_or_else(TargetMachine::get_default_triple);
        generator.module.set_triple(&triple);

        let target = Target::from_triple(&triple).map_err(|e| {
            CodegenError::new(format!("failed to resolve LLVM target `{}`: {e}", triple))
        })?;
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                generate::map_opt_level(opt_level),
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| {
                CodegenError::new(format!(
                    "failed to create LLVM target machine for `{}`",
                    triple
                ))
            })?;
        generator
            .module
            .set_data_layout(&machine.get_target_data().get_data_layout());

        generator.generate_program(program)?;
        table.absorb_from(&generator.symbol_table);

        generate::run_module_optimization_passes(&generator.module, &machine, opt_level)?;

        generator.finalize_debug();

        machine
            .write_to_file(&generator.module, file_type, path)
            .map_err(|e| {
                CodegenError::new(format!(
                    "failed to emit {} via LLVM target machine to {}: {e}",
                    match file_type {
                        FileType::Object => "object file",
                        FileType::Assembly => "assembly file",
                    },
                    path.display()
                ))
            })
    }

    fn declare_imported_modules(
        &mut self,
        imported_modules: &[ModuleArtifact],
    ) -> CodegenResult<()> {
        for module in imported_modules {
            for export in &module.exports {
                if !export.is_struct() {
                    continue;
                }
                self.struct_types
                    .entry(export.name.clone())
                    .or_insert_with(|| self.context.opaque_struct_type(&export.name));
                let fields = export
                    .fields
                    .iter()
                    .map(|field| {
                        ast_type_from_canonical_key(&field.type_key)
                            .map(|ty| (field.name.clone(), ty))
                            .map_err(CodegenError::new)
                    })
                    .collect::<CodegenResult<Vec<_>>>()?;
                self.struct_fields.insert(export.name.clone(), fields);
                // Generic imported structs need their type params recorded so
                // ensure_named_struct_type can substitute concrete args into
                // the field types (e.g. Pair<i64> from a Pair<T> template).
                if !export.type_params.is_empty() {
                    self.struct_generics
                        .insert(export.name.clone(), export.type_params.clone());
                }
            }
        }

        for module in imported_modules {
            for export in &module.exports {
                match export.kind {
                    crate::module_artifact::ExportKind::Function => {
                        // Generic function exports have no concrete signature
                        // to declare here (fn(T) -> T cannot lower); call sites
                        // monomorphize to mangled instances (identity__i64)
                        // which are declared from the monomorphized items.
                        if !export.type_params.is_empty() {
                            continue;
                        }
                        let llvm_name = export
                            .link_name
                            .clone()
                            .unwrap_or_else(|| export.name.clone());
                        let (params, return_type) =
                            crate::types::parse_canonical_function_signature(&export.signature)
                                .map_err(CodegenError::new)?;
                        let param_ast = params
                            .into_iter()
                            .map(|param| param.to_ast())
                            .collect::<Vec<_>>();
                        let return_ast = if matches!(return_type, Type::Unit) {
                            None
                        } else {
                            Some(return_type.to_ast())
                        };
                        self.imported_function_links
                            .insert(export.name.clone(), llvm_name.clone());
                        self.register_source_function_symbol(&export.name, &llvm_name);
                        let abi = export.abi.map(|abi| match abi {
                            crate::module_artifact::ModuleAbi::C => ast::ExternLinkage::C,
                            crate::module_artifact::ModuleAbi::Silver => ast::ExternLinkage::Silver,
                            crate::module_artifact::ModuleAbi::System => ast::ExternLinkage::System,
                            crate::module_artifact::ModuleAbi::Rust => ast::ExternLinkage::Rust,
                            crate::module_artifact::ModuleAbi::Cdecl => ast::ExternLinkage::Cdecl,
                            crate::module_artifact::ModuleAbi::Stdcall => {
                                ast::ExternLinkage::Stdcall
                            }
                            crate::module_artifact::ModuleAbi::Fastcall => {
                                ast::ExternLinkage::Fastcall
                            }
                        });
                        self.register_function_signature(
                            &llvm_name,
                            FunctionSig {
                                params: param_ast.clone(),
                                return_type: return_ast.clone(),
                                is_variadic: export.is_variadic,
                                linkage: abi.clone(),
                            },
                            None,
                            SymbolKind::ExternFunction,
                        );
                        if self.module.get_function(&llvm_name).is_none() {
                            let fn_ty = self.lower_function_type(
                                &param_ast,
                                return_ast.as_ref(),
                                export.is_variadic,
                                abi.clone(),
                            )?;
                            let function = self.module.add_function(&llvm_name, fn_ty, None);
                            self.apply_abi_attributes(
                                function,
                                &FunctionSig {
                                    params: param_ast.clone(),
                                    return_type: return_ast.clone(),
                                    is_variadic: export.is_variadic,
                                    linkage: abi,
                                },
                            )?;
                        }
                    }
                    crate::module_artifact::ExportKind::Struct => {
                        let Some(struct_ty) = self.struct_types.get(&export.name).copied() else {
                            continue;
                        };
                        let field_types = export
                            .fields
                            .iter()
                            .map(|field| {
                                ast_type_from_canonical_key(&field.type_key)
                                    .map_err(CodegenError::new)
                            })
                            .collect::<CodegenResult<Vec<_>>>()?;
                        let llvm_fields = field_types
                            .iter()
                            .map(|field| self.lower_basic_type(field))
                            .collect::<CodegenResult<Vec<_>>>()?;
                        if struct_ty.count_fields() == 0 {
                            struct_ty.set_body(&llvm_fields, false);
                        }
                    }
                    crate::module_artifact::ExportKind::Enum => {
                        if let Some(backing) = &export.enum_backing_type {
                            let ty =
                                ast_type_from_canonical_key(backing).map_err(CodegenError::new)?;
                            if let ast::TypeKind::Primitive(primitive) = *ty.kind {
                                self.enum_backing_types
                                    .insert(export.name.clone(), primitive);
                            }
                        }
                        self.enum_variants.insert(
                            export.name.clone(),
                            export
                                .enum_variants
                                .iter()
                                .map(|variant| (variant.name.clone(), variant.value))
                                .collect(),
                        );
                        // Register payload layouts for imported enums with payload
                        // variants. Generic enums defer layout to the monomorphized
                        // concrete instantiation (fields may reference T).
                        if export.type_params.is_empty() {
                            let mut max_payload_size: u64 = 0;
                            let mut variant_payload_types: HashMap<String, Vec<ast::Type>> =
                                HashMap::default();
                            let target_data = TargetData::create(
                                self.module.get_data_layout().as_str().to_str().unwrap(),
                            );
                            for variant in &export.enum_variants {
                                if variant.payload_types.is_empty() {
                                    continue;
                                }
                                let payload_types: Vec<ast::Type> = variant
                                    .payload_types
                                    .iter()
                                    .map(|key| ast_type_from_canonical_key(key))
                                    .collect::<Result<Vec<_>, _>>()
                                    .unwrap_or_else(|_| vec![]);
                                let mut variant_size: u64 = 0;
                                for pt in &payload_types {
                                    let llvm_ty = self.lower_basic_type(pt)?;
                                    variant_size += target_data.get_abi_size(&llvm_ty);
                                }
                                max_payload_size = max_payload_size.max(variant_size);
                                variant_payload_types.insert(variant.name.clone(), payload_types);
                            }
                            if max_payload_size > 0 {
                                let i16_ty = self.context.i16_type();
                                let array_ty =
                                    self.context.i8_type().array_type(max_payload_size as u32);
                                let struct_ty = self
                                    .context
                                    .struct_type(&[i16_ty.into(), array_ty.into()], false);
                                struct_ty.set_body(&[i16_ty.into(), array_ty.into()], false);
                                self.enum_payload_layouts
                                    .insert(export.name.clone(), struct_ty);
                                self.struct_types.insert(export.name.clone(), struct_ty);
                            }
                            self.enum_variant_payload_types
                                .insert(export.name.clone(), variant_payload_types);
                        }
                    }
                    crate::module_artifact::ExportKind::Trait => {}
                }
            }
        }

        Ok(())
    }

    fn type_name_to_ast_type(&self, name: &str) -> Option<ast::Type> {
        let prim = match name {
            "i8" => ast::PrimitiveType::I8,
            "i16" => ast::PrimitiveType::I16,
            "i32" => ast::PrimitiveType::I32,
            "i64" => ast::PrimitiveType::I64,
            "i128" => ast::PrimitiveType::I128,
            "u8" => ast::PrimitiveType::U8,
            "u16" => ast::PrimitiveType::U16,
            "u32" => ast::PrimitiveType::U32,
            "u64" => ast::PrimitiveType::U64,
            "u128" => ast::PrimitiveType::U128,
            "f32" => ast::PrimitiveType::F32,
            "f64" => ast::PrimitiveType::F64,
            "f80" => ast::PrimitiveType::F80,
            "c32" => ast::PrimitiveType::C32,
            "c64" => ast::PrimitiveType::C64,
            "c80" => ast::PrimitiveType::C80,
            "bool" => ast::PrimitiveType::Bool,
            "char" => ast::PrimitiveType::Char,
            "str" => ast::PrimitiveType::Str,
            "void" => ast::PrimitiveType::Void,
            _ => return None,
        };
        Some(ast::Type {
            kind: Box::new(ast::TypeKind::Primitive(prim)),
            span: crate::lexer::Span::default(),
        })
    }

    pub(crate) fn size_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(inner_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@size requires an expression argument".to_string(),
                expr.span,
            ));
        };
        let llvm_ty = match &inner_expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                let ast_ty = self.type_name_to_ast_type(&ident.name);
                match ast_ty {
                    Some(ty) => self.lower_basic_type(&ty)?.as_basic_type_enum(),
                    None => {
                        let named_ty = ast::Type {
                            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                                path: vec![ident.clone()],
                                generics: None,
                            })),
                            span: expr.span,
                        };
                        let type_result = self.lower_basic_type(&named_ty);
                        match type_result {
                            Ok(ty) => ty.as_basic_type_enum(),
                            Err(_) => {
                                let inner_val = self.emit_expression_value(inner_expr)?;
                                inner_val.get_type()
                            }
                        }
                    }
                }
            }
            ast::ExpressionKind::TypeName(ty) => self.lower_basic_type(ty)?.as_basic_type_enum(),
            _ => {
                let inner_val = self.emit_expression_value(inner_expr)?;
                inner_val.get_type()
            }
        };
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        let size = target_data.get_abi_size(&llvm_ty);
        Ok(self.context.i64_type().const_int(size, false).into())
    }

    pub(crate) fn align_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(inner_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@align requires an expression argument".to_string(),
                expr.span,
            ));
        };
        let llvm_ty = match &inner_expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                let ast_ty = self.type_name_to_ast_type(&ident.name);
                match ast_ty {
                    Some(ty) => self.lower_basic_type(&ty)?.as_basic_type_enum(),
                    None => {
                        let named_ty = ast::Type {
                            kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                                path: vec![ident.clone()],
                                generics: None,
                            })),
                            span: expr.span,
                        };
                        let type_result = self.lower_basic_type(&named_ty);
                        match type_result {
                            Ok(ty) => ty.as_basic_type_enum(),
                            Err(_) => {
                                let inner_val = self.emit_expression_value(inner_expr)?;
                                inner_val.get_type()
                            }
                        }
                    }
                }
            }
            ast::ExpressionKind::TypeName(ty) => self.lower_basic_type(ty)?.as_basic_type_enum(),
            _ => {
                let inner_val = self.emit_expression_value(inner_expr)?;
                inner_val.get_type()
            }
        };
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        let align = u64::from(target_data.get_abi_alignment(&llvm_ty));
        Ok(self.context.i64_type().const_int(align, false).into())
    }

    pub(crate) fn hash_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(inner_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@hash requires an expression argument".to_string(),
                expr.span,
            ));
        };

        let val = self.emit_expression_value(inner_expr)?;
        let llvm_ty = val.get_type();
        let i64_ty = self.context.i64_type();
        let ptr_ty = self.context.ptr_type(AddressSpace::default());

        if llvm_ty.is_pointer_type() {
            // str: hash the string CONTENT via std.hash.hash_str.
            let hash_fn = self.module.get_function("hash_str").ok_or_else(|| {
                CodegenError::with_span(
                    "@hash on a string requires `import std.hash;`".to_string(),
                    expr.span,
                )
            })?;
            let str_ptr = val.into_pointer_value();
            let call = self
                .builder
                .build_call(hash_fn, &[str_ptr.into()], "hash_str_call")
                .map_err(|e| CodegenError::with_span(format!("hash_str call: {e}"), expr.span))?;
            return call
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| CodegenError::new("hash_str returned void"));
        }

        // Non-str: hash the raw byte representation via std.hash.hash_bytes.
        let hash_fn = self.module.get_function("hash_bytes").ok_or_else(|| {
            CodegenError::with_span("@hash requires `import std.hash;`".to_string(), expr.span)
        })?;
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        let size = target_data.get_abi_size(&llvm_ty);
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for hash alloca"))?;
        let alloca = self.create_entry_alloca(function, "hash_val", llvm_ty)?;
        self.builder
            .build_store(alloca, val)
            .map_err(|e| CodegenError::new(format!("hash store: {e}")))?;
        let ptr = self
            .builder
            .build_pointer_cast(alloca, ptr_ty, "hash_ptr")
            .map_err(|e| CodegenError::new(format!("hash ptr cast: {e}")))?;
        let size_val = i64_ty.const_int(size, false);
        let call = self
            .builder
            .build_call(hash_fn, &[ptr.into(), size_val.into()], "hash_call")
            .map_err(|e| CodegenError::with_span(format!("hash_bytes call: {e}"), expr.span))?;
        call.try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::new("hash_bytes returned void"))
    }

    pub(crate) fn memcpy_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(dst_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@memcpy expects dst as first argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(src_expr)) = args.get(1) else {
            return Err(CodegenError::with_span(
                "@memcpy expects src as second argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(len_expr)) = args.get(2) else {
            return Err(CodegenError::with_span(
                "@memcpy expects len as third argument".to_string(),
                expr.span,
            ));
        };
        let dst_val = self.emit_expression_value(dst_expr)?;
        let src_val = self.emit_expression_value(src_expr)?;
        let len_val = self.emit_expression_value(len_expr)?;
        let dst_ptr = dst_val.into_pointer_value();
        let src_ptr = src_val.into_pointer_value();
        let len_i64 = len_val.into_int_value();
        // Get or declare llvm.memcpy.p0.p0.i64 intrinsic
        let memcpy_fn = self
            .module
            .get_function("llvm.memcpy.p0.p0.i64")
            .unwrap_or_else(|| {
                let i64 = self.context.i64_type();
                let i1 = self.context.bool_type();
                let ptr = self.context.ptr_type(AddressSpace::default());
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[ptr.into(), ptr.into(), i64.into(), i1.into()], false);
                self.module
                    .add_function("llvm.memcpy.p0.p0.i64", fn_type, None)
            });
        self.builder
            .build_call(
                memcpy_fn,
                &[
                    dst_ptr.into(),
                    src_ptr.into(),
                    len_i64.into(),
                    self.context.bool_type().const_int(0, false).into(),
                ],
                "memcpy",
            )
            .map_err(|e| CodegenError::with_span(format!("@memcpy call failed: {e}"), expr.span))?;
        Ok(dst_val)
    }

    pub(crate) fn memset_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(dst_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@memset expects dst as first argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(val_expr)) = args.get(1) else {
            return Err(CodegenError::with_span(
                "@memset expects value as second argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(len_expr)) = args.get(2) else {
            return Err(CodegenError::with_span(
                "@memset expects len as third argument".to_string(),
                expr.span,
            ));
        };
        let dst_val = self.emit_expression_value(dst_expr)?;
        let val_val = self.emit_expression_value(val_expr)?;
        let len_val = self.emit_expression_value(len_expr)?;
        let dst_ptr = dst_val.into_pointer_value();
        let val_i8 = val_val.into_int_value();
        let len_i64 = len_val.into_int_value();
        // Get or declare llvm.memset.p0.i64 intrinsic
        let memset_fn = self
            .module
            .get_function("llvm.memset.p0.i64")
            .unwrap_or_else(|| {
                let i64 = self.context.i64_type();
                let i8 = self.context.i8_type();
                let i1 = self.context.bool_type();
                let ptr = self.context.ptr_type(AddressSpace::default());
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[ptr.into(), i8.into(), i64.into(), i1.into()], false);
                self.module
                    .add_function("llvm.memset.p0.i64", fn_type, None)
            });
        self.builder
            .build_call(
                memset_fn,
                &[
                    dst_ptr.into(),
                    val_i8.into(),
                    len_i64.into(),
                    self.context.bool_type().const_int(0, false).into(),
                ],
                "memset",
            )
            .map_err(|e| CodegenError::with_span(format!("@memset call failed: {e}"), expr.span))?;
        Ok(dst_val)
    }

    pub(crate) fn memmove_codegen(
        &mut self,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let Some(ast::MacroArg::Expression(dst_expr)) = args.first() else {
            return Err(CodegenError::with_span(
                "@memmove expects dst as first argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(src_expr)) = args.get(1) else {
            return Err(CodegenError::with_span(
                "@memmove expects src as second argument".to_string(),
                expr.span,
            ));
        };
        let Some(ast::MacroArg::Expression(len_expr)) = args.get(2) else {
            return Err(CodegenError::with_span(
                "@memmove expects len as third argument".to_string(),
                expr.span,
            ));
        };
        let dst_val = self.emit_expression_value(dst_expr)?;
        let src_val = self.emit_expression_value(src_expr)?;
        let len_val = self.emit_expression_value(len_expr)?;
        let dst_ptr = dst_val.into_pointer_value();
        let src_ptr = src_val.into_pointer_value();
        let len_i64 = len_val.into_int_value();
        // Get or declare llvm.memmove.p0.p0.i64 intrinsic
        let memmove_fn = self
            .module
            .get_function("llvm.memmove.p0.p0.i64")
            .unwrap_or_else(|| {
                let i64 = self.context.i64_type();
                let i1 = self.context.bool_type();
                let ptr = self.context.ptr_type(AddressSpace::default());
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[ptr.into(), ptr.into(), i64.into(), i1.into()], false);
                self.module
                    .add_function("llvm.memmove.p0.p0.i64", fn_type, None)
            });
        self.builder
            .build_call(
                memmove_fn,
                &[
                    dst_ptr.into(),
                    src_ptr.into(),
                    len_i64.into(),
                    self.context.bool_type().const_int(0, false).into(),
                ],
                "memmove",
            )
            .map_err(|e| {
                CodegenError::with_span(format!("@memmove call failed: {e}"), expr.span)
            })?;
        Ok(dst_val)
    }

    pub(crate) fn print_codegen(
        &mut self,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        // Determine writer and format-string arg index
        let (fmt_arg_idx, writer_expr) = match name {
            "fprint" => {
                let Some(ast::MacroArg::Expression(w)) = args.first() else {
                    return Err(CodegenError::with_span(
                        "@fprint expects a BufWriter* as first argument".to_string(),
                        expr.span,
                    ));
                };
                // Evaluate writer expression once and store in a temp
                let writer_val = self.emit_expression_value(w)?;
                let fn_ctx = self.current_fn.ok_or_else(|| {
                    CodegenError::with_span("@fprint requires an active function", expr.span)
                })?;
                let writer_tmp =
                    self.create_entry_alloca(fn_ctx, "fprint.writer", writer_val.get_type())?;
                self.builder
                    .build_store(writer_tmp, writer_val)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to spill fprint receiver: {e}"),
                            expr.span,
                        )
                    })?;
                let ast_ty = self
                    .resolve_receiver_type(w)
                    .unwrap_or_else(|| self.infer_ast_type_from_value(&writer_val, &expr.span));
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        "__fprint_writer".to_string(),
                        VarInfo {
                            ptr: writer_tmp,
                            ty: ast_ty,
                            is_mutable: false,
                            is_volatile: false,
                            drop_flag: None,
                            field_flags: Vec::new(),
                        },
                    );
                }
                let w_ident = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: "__fprint_writer".to_string(),
                        span: expr.span,
                    })),
                    span: expr.span,
                };
                (1, w_ident)
            }
            "sprint" => {
                let buf_writer_type = ast::Type {
                    kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                        path: vec![ast::Identifier {
                            name: "BufWriter".to_string(),
                            span: expr.span,
                        }],
                        generics: None,
                    })),
                    span: expr.span,
                };
                let buf_writer_llvm_ty = self.lower_basic_type(&buf_writer_type)?;
                let fn_ctx = self.current_fn.ok_or_else(|| {
                    CodegenError::with_span("@sprint requires an active function", expr.span)
                })?;
                let writer_tmp =
                    self.create_entry_alloca(fn_ctx, "sprint.writer", buf_writer_llvm_ty)?;

                let zero_i64 = self.context.i64_type().const_int(0, false);
                let neg_one_i32 = self.context.i32_type().const_int(u64::MAX, true);

                // Initialize BufWriter fields: data=0, len=0, cap=0, fd=-1.
                // The zero-valued fields (data=0, cap=0) trigger ensure_init's lazy
                // buffer allocation on first write. fd=-1 marks this as a string-only
                // writer (no file descriptor), preventing flush from writing to a real fd.
                let data_ptr = self
                    .builder
                    .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 0, "sprint.data")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("sprint struct gep 0 failed: {e}"),
                            expr.span,
                        )
                    })?;
                self.builder.build_store(data_ptr, zero_i64).map_err(|e| {
                    CodegenError::with_span(format!("sprint store data failed: {e}"), expr.span)
                })?;

                let len_ptr = self
                    .builder
                    .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 1, "sprint.len")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("sprint struct gep 1 failed: {e}"),
                            expr.span,
                        )
                    })?;
                self.builder.build_store(len_ptr, zero_i64).map_err(|e| {
                    CodegenError::with_span(format!("sprint store len failed: {e}"), expr.span)
                })?;

                let cap_ptr = self
                    .builder
                    .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 2, "sprint.cap")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("sprint struct gep 2 failed: {e}"),
                            expr.span,
                        )
                    })?;
                self.builder.build_store(cap_ptr, zero_i64).map_err(|e| {
                    CodegenError::with_span(format!("sprint store cap failed: {e}"), expr.span)
                })?;

                let fd_ptr = self
                    .builder
                    .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 3, "sprint.fd")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("sprint struct gep 3 failed: {e}"),
                            expr.span,
                        )
                    })?;
                self.builder.build_store(fd_ptr, neg_one_i32).map_err(|e| {
                    CodegenError::with_span(format!("sprint store fd failed: {e}"), expr.span)
                })?;

                // mode = IOMODE_BLOCK (2) — ensures the writer allocates a buffer
                // and uses buffered writes. An uninitialized mode risks reading as
                // IOMODE_UNBUFFERED (0), which would skip buffer allocation and
                // silently drop all output since fd is -1.
                let mode_ptr = self
                    .builder
                    .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 6, "sprint.mode")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("sprint struct gep 6 failed: {e}"),
                            expr.span,
                        )
                    })?;
                self.builder
                    .build_store(mode_ptr, self.context.i8_type().const_int(2, false))
                    .map_err(|e| {
                        CodegenError::with_span(format!("sprint store mode failed: {e}"), expr.span)
                    })?;

                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        "__sprint_writer".to_string(),
                        VarInfo {
                            ptr: writer_tmp,
                            ty: buf_writer_type.clone(),
                            is_mutable: false,
                            is_volatile: false,
                            drop_flag: None,
                            field_flags: Vec::new(),
                        },
                    );
                }
                let w_ident = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: "__sprint_writer".to_string(),
                        span: expr.span,
                    })),
                    span: expr.span,
                };
                (0, w_ident)
            }
            _ => {
                // @print, @println, @eprint, @eprintln
                let writer_name = if name.starts_with("e") {
                    "STDERR"
                } else {
                    "STDOUT"
                };
                let w = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: writer_name.to_string(),
                        span: expr.span,
                    })),
                    span: expr.span,
                };
                (0, w)
            }
        };

        // Extract format string (must be a literal — validated by typeck)
        let fmt_str = match &args[fmt_arg_idx] {
            ast::MacroArg::Expression(e) => match &e.kind.as_ref() {
                ast::ExpressionKind::Literal(ast::Literal::String(s)) => s.clone(),
                _ => {
                    return Err(CodegenError::with_span(
                        "format string must be a literal".to_string(),
                        e.span,
                    ));
                }
            },
            _ => {
                return Err(CodegenError::with_span(
                    "format string must be a literal".to_string(),
                    expr.span,
                ));
            }
        };

        let segments = crate::builtin_macros::parse_format(&fmt_str);

        // Collect value arguments for placeholders
        let value_start = fmt_arg_idx + 1;
        let value_args: Vec<&ast::Expression> = args[value_start..]
            .iter()
            .map(|a| match a {
                ast::MacroArg::Expression(e) => e,
                _ => unreachable!("typeck verified all value args are expressions"),
            })
            .collect();

        // Emit method calls for each format segment
        let mut placeholder_idx = 0;
        for segment in &segments {
            match segment {
                crate::builtin_macros::FormatSegment::Literal(text) => {
                    let method = ast::Identifier {
                        name: "write_str".to_string(),
                        span: expr.span,
                    };
                    let lit_expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(
                            text.clone(),
                        ))),
                        span: expr.span,
                    };
                    self.emit_method_call_expression(
                        &writer_expr,
                        &method,
                        &[lit_expr],
                        true, // allow_void
                        &expr.span,
                    )?;
                }
                crate::builtin_macros::FormatSegment::Placeholder => {
                    let val_expr = value_args[placeholder_idx];
                    placeholder_idx += 1;

                    // Determine the write method based on the value's type
                    let method_name = self
                        .value_write_method_name(val_expr)
                        .map_err(|e| CodegenError::with_span(e, val_expr.span))?;

                    let method = ast::Identifier {
                        name: method_name,
                        span: expr.span,
                    };
                    self.emit_method_call_expression(
                        &writer_expr,
                        &method,
                        std::slice::from_ref(val_expr),
                        true,
                        &expr.span,
                    )?;
                }
            }
        }

        // For println/eprintln, append a newline
        if name == "println" || name == "eprintln" {
            let method = ast::Identifier {
                name: "write_str".to_string(),
                span: expr.span,
            };
            let nl_expr = ast::Expression {
                kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(
                    "\n".to_string(),
                ))),
                span: expr.span,
            };
            self.emit_method_call_expression(&writer_expr, &method, &[nl_expr], true, &expr.span)?;
        }

        if name == "sprint" {
            // Write a null terminator at the end of the buffer
            let method = ast::Identifier {
                name: "write_u8".to_string(),
                span: expr.span,
            };
            let zero_expr = ast::Expression {
                kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(0))),
                span: expr.span,
            };
            self.emit_method_call_expression(
                &writer_expr,
                &method,
                &[zero_expr],
                true,
                &expr.span,
            )?;

            // Load `data` from the stack BufWriter
            let buf_writer_type = ast::Type {
                kind: Box::new(ast::TypeKind::Named(ast::NamedType {
                    path: vec![ast::Identifier {
                        name: "BufWriter".to_string(),
                        span: expr.span,
                    }],
                    generics: None,
                })),
                span: expr.span,
            };
            let buf_writer_llvm_ty = self.lower_basic_type(&buf_writer_type)?;
            let writer_tmp = self
                .variables
                .last()
                .and_then(|scope| scope.get("__sprint_writer"))
                .map(|info| info.ptr)
                .ok_or_else(|| {
                    CodegenError::with_span("sprint writer variable missing".to_string(), expr.span)
                })?;
            let data_ptr = self
                .builder
                .build_struct_gep(buf_writer_llvm_ty, writer_tmp, 0, "sprint.data")
                .map_err(|e| {
                    CodegenError::with_span(format!("sprint struct gep 0 failed: {e}"), expr.span)
                })?;
            let data_val = self
                .builder
                .build_load(self.context.i64_type(), data_ptr, "sprint.data.val")
                .map_err(|e| {
                    CodegenError::with_span(format!("sprint load data failed: {e}"), expr.span)
                })?;
            let str_val = self
                .builder
                .build_int_to_ptr(
                    data_val.into_int_value(),
                    self.context.ptr_type(inkwell::AddressSpace::default()),
                    "sprint.str",
                )
                .map_err(|e| {
                    CodegenError::with_span(format!("sprint int to ptr failed: {e}"), expr.span)
                })?;

            Ok(str_val.as_basic_value_enum())
        } else {
            // All non-sprint variants return void
            Ok(self.context.i8_type().const_zero().into())
        }
    }
}
