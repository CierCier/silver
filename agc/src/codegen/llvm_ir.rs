use std::path::Path;

use std::collections::HashMap;

<<<<<<< HEAD
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
=======
>>>>>>> cc823df (shift to LL3)
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
<<<<<<< HEAD
=======
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue,
};
>>>>>>> cc823df (shift to LL3)

use crate::codegen::{CodegenError, CodegenResult, SilverGenerator};
use crate::lexer::Span;
use crate::parser::ast;
use crate::symbol_table::{CompilerPhase, CompilerSymbolTable, SymbolId, SymbolKind};
use crate::types::Type;

#[derive(Clone)]
struct FunctionSig {
    params: Vec<ast::Type>,
    return_type: Option<ast::Type>,
    is_variadic: bool,
}

impl PartialEq for FunctionSig {
    fn eq(&self, other: &Self) -> bool {
        self.is_variadic == other.is_variadic
            && self.params.len() == other.params.len()
            && self
                .params
                .iter()
                .zip(other.params.iter())
                .all(|(lhs, rhs)| {
                    Type::from_ast(lhs).canonical_key() == Type::from_ast(rhs).canonical_key()
                })
            && match (&self.return_type, &other.return_type) {
                (None, None) => true,
                (Some(lhs), Some(rhs)) => {
                    Type::from_ast(lhs).canonical_key() == Type::from_ast(rhs).canonical_key()
                }
                _ => false,
            }
    }
}

#[derive(Clone)]
struct VarInfo<'ctx> {
    ptr: PointerValue<'ctx>,
    ty: ast::Type,
}

pub struct LlvmIrGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_fn: Option<FunctionValue<'ctx>>,
    current_return_type: Option<ast::Type>,
    variables: Vec<HashMap<String, VarInfo<'ctx>>>,
    function_sigs: HashMap<SymbolId, FunctionSig>,
    function_name_to_symbol: HashMap<String, SymbolId>,
<<<<<<< HEAD
=======
    extern_globals: HashMap<String, ast::Type>,
>>>>>>> cc823df (shift to LL3)
    struct_types: HashMap<String, StructType<'ctx>>,
    struct_fields: HashMap<String, Vec<(String, ast::Type)>>,
    method_receivers: HashMap<(String, String), bool>,
    string_constants: HashMap<String, PointerValue<'ctx>>,
    struct_generics: HashMap<String, Vec<String>>,
    generic_impl_templates: Vec<ast::ImplItem>,
    loop_stack: Vec<(
        inkwell::basic_block::BasicBlock<'ctx>,
        inkwell::basic_block::BasicBlock<'ctx>,
    )>,
    symbol_table: CompilerSymbolTable,
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    fn is_private(visibility: &ast::Visibility) -> bool {
        matches!(visibility, ast::Visibility::Private)
    }

    fn method_effective_visibility(
        impl_visibility: &ast::Visibility,
        method_visibility: &ast::Visibility,
    ) -> ast::Visibility {
        if Self::is_private(impl_visibility) || Self::is_private(method_visibility) {
            ast::Visibility::Private
        } else {
            ast::Visibility::Public
        }
    }

    fn apply_function_linkage(function: FunctionValue<'ctx>, visibility: &ast::Visibility) {
        if Self::is_private(visibility) {
            function.set_linkage(Linkage::Internal);
        } else {
            function.set_linkage(Linkage::External);
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
        let context = Context::create();
        let module = context.create_module("silver");
        let builder = context.create_builder();
        let mut generator = LlvmIrGenerator {
            context: &context,
            module,
            builder,
            current_fn: None,
            current_return_type: None,
            variables: vec![HashMap::new()],
            function_sigs: HashMap::new(),
            function_name_to_symbol: HashMap::new(),
<<<<<<< HEAD
=======
            extern_globals: HashMap::new(),
>>>>>>> cc823df (shift to LL3)
            struct_types: HashMap::new(),
            struct_fields: HashMap::new(),
            method_receivers: HashMap::new(),
            string_constants: HashMap::new(),
            struct_generics: HashMap::new(),
            generic_impl_templates: Vec::new(),
            loop_stack: Vec::new(),
            symbol_table: table.clone(),
        };
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
        )
    }

    fn emit_target_file(
        program: &ast::Program,
        path: &Path,
        target_triple: Option<&str>,
        opt_level: Option<&str>,
        file_type: FileType,
        table: &mut CompilerSymbolTable,
    ) -> CodegenResult<()> {
        let context = Context::create();
        let module = context.create_module("silver");
        let builder = context.create_builder();
        let mut generator = LlvmIrGenerator {
            context: &context,
            module,
            builder,
            current_fn: None,
            current_return_type: None,
            variables: vec![HashMap::new()],
            function_sigs: HashMap::new(),
            function_name_to_symbol: HashMap::new(),
<<<<<<< HEAD
=======
            extern_globals: HashMap::new(),
>>>>>>> cc823df (shift to LL3)
            struct_types: HashMap::new(),
            struct_fields: HashMap::new(),
            method_receivers: HashMap::new(),
            string_constants: HashMap::new(),
            struct_generics: HashMap::new(),
            generic_impl_templates: Vec::new(),
            loop_stack: Vec::new(),
            symbol_table: table.clone(),
        };
        generator.generate_program(program)?;
        table.absorb_from(&generator.symbol_table);

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
                map_opt_level(opt_level),
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
<<<<<<< HEAD
        machine.write_to_file(&generator.module, file_type, path).map_err(|e| {
            CodegenError::new(format!(
                "failed to emit {} via LLVM target machine to {}: {e}",
                match file_type {
                    FileType::Object => "object file",
                    FileType::Assembly => "assembly file",
                },
                path.display()
            ))
        })
=======
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
>>>>>>> cc823df (shift to LL3)
    }

    fn lower_basic_type(&mut self, ty: &ast::Type) -> CodegenResult<BasicTypeEnum<'ctx>> {
        match ty.kind.as_ref() {
            ast::TypeKind::Primitive(primitive) => {
                let basic = match primitive {
                    ast::PrimitiveType::I8 | ast::PrimitiveType::U8 => {
                        self.context.i8_type().as_basic_type_enum()
                    }
                    ast::PrimitiveType::I16 | ast::PrimitiveType::U16 => {
                        self.context.i16_type().as_basic_type_enum()
                    }
                    ast::PrimitiveType::I32 | ast::PrimitiveType::U32 => {
                        self.context.i32_type().as_basic_type_enum()
                    }
                    ast::PrimitiveType::I64 | ast::PrimitiveType::U64 => {
                        self.context.i64_type().as_basic_type_enum()
                    }
                    ast::PrimitiveType::I128 | ast::PrimitiveType::U128 => {
                        self.context.i128_type().as_basic_type_enum()
                    }
                    ast::PrimitiveType::F32 => self.context.f32_type().as_basic_type_enum(),
                    ast::PrimitiveType::F64 => self.context.f64_type().as_basic_type_enum(),
                    ast::PrimitiveType::F80 => self.context.x86_f80_type().as_basic_type_enum(),
                    ast::PrimitiveType::C32 => self
                        .context
                        .struct_type(
                            &[
                                self.context.f32_type().as_basic_type_enum(),
                                self.context.f32_type().as_basic_type_enum(),
                            ],
                            false,
                        )
                        .as_basic_type_enum(),
                    ast::PrimitiveType::C64 => self
                        .context
                        .struct_type(
                            &[
                                self.context.f64_type().as_basic_type_enum(),
                                self.context.f64_type().as_basic_type_enum(),
                            ],
                            false,
                        )
                        .as_basic_type_enum(),
                    ast::PrimitiveType::C80 => self
                        .context
                        .struct_type(
                            &[
                                self.context.x86_f80_type().as_basic_type_enum(),
                                self.context.x86_f80_type().as_basic_type_enum(),
                            ],
                            false,
                        )
                        .as_basic_type_enum(),
                    ast::PrimitiveType::Bool => self.context.bool_type().as_basic_type_enum(),
                    ast::PrimitiveType::Char => self.context.i32_type().as_basic_type_enum(),
                    ast::PrimitiveType::Str => self
                        .context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
                };
                Ok(basic)
            }
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 && named.generics.is_none() {
                    let candidate = &named.path[0].name;
                    if self.is_generic_placeholder_name(candidate) {
                        return Ok(self.context.i64_type().as_basic_type_enum());
                    }
                }
                let struct_ty = self.ensure_named_struct_type(named)?;
                Ok(struct_ty.as_basic_type_enum())
            }
            ast::TypeKind::Reference(reference) => {
                let _ = self.lower_basic_type(&reference.inner)?;
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            ast::TypeKind::Pointer(pointer) => {
                let _ = self.lower_basic_type(&pointer.inner)?;
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            ast::TypeKind::Array(array) => {
                let element = self.lower_basic_type(&array.element_type)?;
                let Some(size_expr) = &array.size else {
                    return Err(CodegenError::with_span(
                        "array fields require a constant size in LLVM IR codegen",
                        ty.span.clone(),
                    ));
                };
                let ast::ExpressionKind::Literal(ast::Literal::Integer(size_value)) =
                    size_expr.kind.as_ref()
                else {
                    return Err(CodegenError::with_span(
                        "array size must be an integer literal in LLVM IR codegen",
                        size_expr.span.clone(),
                    ));
                };
                let size = u32::try_from(*size_value).map_err(|_| {
                    CodegenError::with_span("array size is out of range", size_expr.span.clone())
                })?;
                Ok(element.array_type(size).as_basic_type_enum())
            }
            ast::TypeKind::Optional(inner) => {
                let _ = self.lower_basic_type(inner)?;
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            ast::TypeKind::Tuple(items) => {
                let mut fields = Vec::with_capacity(items.len());
                for item in items {
                    fields.push(self.lower_basic_type(item)?);
                }
                Ok(self
                    .context
                    .struct_type(&fields, false)
                    .as_basic_type_enum())
            }
            ast::TypeKind::Function(function) => {
                let _ = self.lower_basic_type(&function.return_type)?;
                for parameter in &function.parameters {
                    let _ = self.lower_basic_type(parameter)?;
                }
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            ast::TypeKind::Generic(_) => Err(CodegenError::with_span(
                "generic type parameters must be monomorphized before LLVM lowering",
                ty.span.clone(),
            )),
        }
    }

    fn lower_function_type(
        &mut self,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
    ) -> CodegenResult<FunctionType<'ctx>> {
        let mut llvm_params = Vec::with_capacity(params.len());
        for param in params {
            let lowered = self.lower_basic_type(param)?;
            llvm_params.push(BasicMetadataTypeEnum::from(lowered));
        }

        if let Some(ret) = return_type {
            let llvm_ret = self.lower_basic_type(ret)?;
            Ok(llvm_ret.fn_type(&llvm_params, is_variadic))
        } else {
            Ok(self.context.void_type().fn_type(&llvm_params, is_variadic))
        }
    }

    fn push_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.variables.pop();
    }

    fn lookup_variable(&self, name: &str) -> Option<VarInfo<'ctx>> {
        self.variables
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

<<<<<<< HEAD
=======
    fn lookup_extern_global(
        &self,
        name: &str,
    ) -> Option<(inkwell::values::GlobalValue<'ctx>, ast::Type)> {
        let ty = self.extern_globals.get(name)?.clone();
        self.module.get_global(name).map(|global| (global, ty))
    }

>>>>>>> cc823df (shift to LL3)
    fn register_function_signature(
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

    fn signature_for_name(&self, llvm_name: &str) -> Option<FunctionSig> {
        self.function_name_to_symbol
            .get(llvm_name)
            .and_then(|symbol_id| self.function_sigs.get(symbol_id))
            .cloned()
    }

    fn named_type_name(named: &ast::NamedType) -> String {
        named
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn named_type_key(named: &ast::NamedType) -> String {
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

    fn substitute_generic_type(
        ty: &ast::Type,
        substitutions: &HashMap<String, ast::Type>,
    ) -> ast::Type {
        let kind = match ty.kind.as_ref() {
            ast::TypeKind::Generic(generic) if generic.args.is_empty() => substitutions
                .get(&generic.name.name)
                .map(|t| t.kind.clone())
                .unwrap_or_else(|| ty.kind.clone()),
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 && named.generics.is_none() {
                    if let Some(mapped) = substitutions.get(&named.path[0].name) {
                        return ast::Type {
                            kind: mapped.kind.clone(),
                            span: ty.span.clone(),
                        };
                    }
                }
                let generics = named.generics.as_ref().map(|args| {
                    args.iter()
                        .map(|arg| Self::substitute_generic_type(arg, substitutions))
                        .collect::<Vec<_>>()
                });
                Box::new(ast::TypeKind::Named(ast::NamedType {
                    path: named.path.clone(),
                    generics,
                }))
            }
            ast::TypeKind::Reference(reference) => {
                Box::new(ast::TypeKind::Reference(ast::ReferenceType {
                    is_mutable: reference.is_mutable,
                    lifetime: reference.lifetime.clone(),
                    inner: Box::new(Self::substitute_generic_type(
                        &reference.inner,
                        substitutions,
                    )),
                }))
            }
            ast::TypeKind::Pointer(pointer) => Box::new(ast::TypeKind::Pointer(ast::PointerType {
                is_mutable: pointer.is_mutable,
                inner: Box::new(Self::substitute_generic_type(&pointer.inner, substitutions)),
            })),
            ast::TypeKind::Array(array) => {
                Box::new(ast::TypeKind::Array(Box::new(ast::ArrayType {
                    element_type: Box::new(Self::substitute_generic_type(
                        &array.element_type,
                        substitutions,
                    )),
                    size: array.size.clone(),
                })))
            }
            ast::TypeKind::Optional(inner) => Box::new(ast::TypeKind::Optional(Box::new(
                Self::substitute_generic_type(inner, substitutions),
            ))),
            ast::TypeKind::Function(function) => {
                Box::new(ast::TypeKind::Function(ast::FunctionType {
                    parameters: function
                        .parameters
                        .iter()
                        .map(|param| Self::substitute_generic_type(param, substitutions))
                        .collect(),
                    return_type: Box::new(Self::substitute_generic_type(
                        &function.return_type,
                        substitutions,
                    )),
                }))
            }
            ast::TypeKind::Tuple(items) => Box::new(ast::TypeKind::Tuple(
                items
                    .iter()
                    .map(|item| Self::substitute_generic_type(item, substitutions))
                    .collect(),
            )),
            _ => ty.kind.clone(),
        };

        ast::Type {
            kind,
            span: ty.span.clone(),
        }
    }

    fn ensure_named_struct_type(
        &mut self,
        named: &ast::NamedType,
    ) -> CodegenResult<StructType<'ctx>> {
        let base_name = Self::named_type_name(named);
        let key = Self::named_type_key(named);

        let struct_ty = *self
            .struct_types
            .entry(key.clone())
            .or_insert_with(|| self.context.opaque_struct_type(&key));

        if !struct_ty.is_opaque() {
            return Ok(struct_ty);
        }

        let template_fields = self.struct_fields.get(&base_name).cloned().ok_or_else(|| {
            CodegenError::new(format!("missing field metadata for struct `{base_name}`"))
        })?;

        let concrete_fields = if let Some(params) = self.struct_generics.get(&base_name) {
            let args = named.generics.as_ref().ok_or_else(|| {
                CodegenError::new(format!(
                    "generic struct `{base_name}` requires concrete type arguments"
                ))
            })?;
            if params.len() != args.len() {
                return Err(CodegenError::new(format!(
                    "generic struct `{base_name}` expected {} type arguments, got {}",
                    params.len(),
                    args.len()
                )));
            }
            let substitutions: HashMap<String, ast::Type> =
                params.iter().cloned().zip(args.iter().cloned()).collect();
            template_fields
                .iter()
                .map(|(name, ty)| {
                    (
                        name.clone(),
                        Self::substitute_generic_type(ty, &substitutions),
                    )
                })
                .collect::<Vec<_>>()
        } else {
            template_fields
        };

        self.struct_fields
            .insert(key.clone(), concrete_fields.clone());

        let mut lowered = Vec::with_capacity(concrete_fields.len());
        for (_, field_ty) in &concrete_fields {
            lowered.push(self.lower_basic_type(field_ty)?);
        }
        struct_ty.set_body(&lowered, false);
        Ok(struct_ty)
    }

    fn path_name(path: &[ast::Identifier]) -> String {
        path.iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn sanitize_monomorph(value: &str) -> String {
        let mut out = String::new();
        let mut last_underscore = false;
        for ch in value.chars() {
            if ch.is_ascii_alphanumeric() {
                out.push(ch);
                last_underscore = false;
            } else if !last_underscore {
                out.push('_');
                last_underscore = true;
            }
        }
<<<<<<< HEAD
        if out.is_empty() {
            "_".to_string()
        } else {
            out
        }
=======
        if out.is_empty() { "_".to_string() } else { out }
>>>>>>> cc823df (shift to LL3)
    }

    fn monomorph_owner_name_from_named(named: &ast::NamedType) -> String {
        let base = Self::named_type_name(named);
        if let Some(args) = &named.generics {
            let parts = args
                .iter()
                .map(|arg| Self::sanitize_monomorph(&Type::from_ast(arg).canonical_key()))
                .collect::<Vec<_>>();
            if parts.is_empty() {
                base
            } else {
                format!("{}__{}", base, parts.join("_"))
            }
        } else {
            base
        }
    }

    fn mangle_method_name(owner: &str, method: &str) -> String {
        format!("{owner}__{method}")
    }

    fn owner_name_from_type(ty: &ast::Type) -> Option<String> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Some(Self::monomorph_owner_name_from_named(named)),
            ast::TypeKind::Reference(reference) => Self::owner_name_from_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => Self::owner_name_from_type(&pointer.inner),
            _ => None,
        }
    }

    fn owner_name_candidates_from_type(ty: &ast::Type) -> Vec<String> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                let mut out = Vec::new();
                let monomorph = Self::monomorph_owner_name_from_named(named);
                out.push(monomorph.clone());
                let key = Self::named_type_key(named);
                if key != monomorph {
                    out.push(key);
                }
                let base = Self::named_type_name(named);
                if base != monomorph {
                    out.push(base);
                }
                out
            }
            ast::TypeKind::Reference(reference) => {
                Self::owner_name_candidates_from_type(&reference.inner)
            }
            ast::TypeKind::Pointer(pointer) => {
                Self::owner_name_candidates_from_type(&pointer.inner)
            }
            _ => Vec::new(),
        }
    }

    fn extract_named_type<'a>(ty: &'a ast::Type) -> Option<&'a ast::NamedType> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Some(named),
            ast::TypeKind::Reference(reference) => Self::extract_named_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => Self::extract_named_type(&pointer.inner),
            _ => None,
        }
    }

    fn substitute_expression_types(
        expr: &mut ast::Expression,
        mapping: &HashMap<String, ast::Type>,
    ) {
        match expr.kind.as_mut() {
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                Self::substitute_expression_types(expression, mapping);
                *target_type = Box::new(Self::substitute_generic_type(target_type, mapping));
            }
            ast::ExpressionKind::TypeName(ty) => {
                *ty = Self::substitute_generic_type(ty, mapping);
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                Self::substitute_expression_types(function, mapping);
                for arg in arguments {
                    Self::substitute_expression_types(arg, mapping);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                Self::substitute_expression_types(receiver, mapping);
                for arg in arguments {
                    Self::substitute_expression_types(arg, mapping);
                }
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                Self::substitute_expression_types(left, mapping);
                Self::substitute_expression_types(right, mapping);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Comptime(operand)
            | ast::ExpressionKind::Reference {
                expression: operand,
                ..
            } => Self::substitute_expression_types(operand, mapping),
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. } => {
                Self::substitute_expression_types(object, mapping)
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_block_types(then_branch, mapping);
                if let Some(else_branch) = else_branch {
                    Self::substitute_block_types(else_branch, mapping);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_block_types(body, mapping);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(annotation) = &mut init.type_annotation {
                    *annotation = Self::substitute_generic_type(annotation, mapping);
                }
                if let Some(init_expr) = &mut init.initializer {
                    Self::substitute_expression_types(init_expr, mapping);
                }
                Self::substitute_expression_types(condition, mapping);
                Self::substitute_expression_types(increment, mapping);
                Self::substitute_block_types(body, mapping);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                Self::substitute_expression_types(expression, mapping);
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        Self::substitute_expression_types(guard, mapping);
                    }
                    Self::substitute_expression_types(&mut arm.body, mapping);
                }
            }
            ast::ExpressionKind::Block(block) => Self::substitute_block_types(block, mapping),
            ast::ExpressionKind::Tuple(items) | ast::ExpressionKind::Array(items) => {
                for item in items {
                    Self::substitute_expression_types(item, mapping);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    Self::substitute_expression_types(&mut field.value, mapping);
                }
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => {
                            Self::substitute_expression_types(expr, mapping)
                        }
                        ast::InitializerItem::Index { index, value } => {
                            Self::substitute_expression_types(index, mapping);
                            Self::substitute_expression_types(value, mapping);
                        }
                        ast::InitializerItem::Field { value, .. } => {
                            Self::substitute_expression_types(value, mapping)
                        }
                    }
                }
            }
            ast::ExpressionKind::Asm(_) => {}
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(expr) = arg {
                        Self::substitute_expression_types(expr, mapping);
                    }
                }
            }
            ast::ExpressionKind::Literal(_) | ast::ExpressionKind::Identifier(_) => {}
        }
    }

    fn substitute_block_types(block: &mut ast::Block, mapping: &HashMap<String, ast::Type>) {
        for statement in &mut block.statements {
            match &mut statement.kind {
                ast::StatementKind::Block(block) => Self::substitute_block_types(block, mapping),
                ast::StatementKind::Let(let_stmt) => {
                    if let Some(annotation) = &mut let_stmt.type_annotation {
                        *annotation = Self::substitute_generic_type(annotation, mapping);
                    }
                    if let Some(init) = &mut let_stmt.initializer {
                        Self::substitute_expression_types(init, mapping);
                    }
                }
                ast::StatementKind::Expression(expr)
                | ast::StatementKind::Return(Some(expr))
                | ast::StatementKind::Break(Some(expr)) => {
                    Self::substitute_expression_types(expr, mapping)
                }
                ast::StatementKind::Return(None)
                | ast::StatementKind::Break(None)
                | ast::StatementKind::Continue => {}
            }
        }
    }

    fn try_instantiate_generic_impl_method_for_type(
        &mut self,
        receiver_type: &ast::Type,
        method_name: &str,
    ) -> CodegenResult<Option<String>> {
        let Some(receiver_named) = Self::extract_named_type(receiver_type) else {
            return Ok(None);
        };
        let Some(receiver_args) = &receiver_named.generics else {
            return Ok(None);
        };
        let base_name = Self::named_type_name(receiver_named);
        let owner = Self::monomorph_owner_name_from_named(receiver_named);

        let templates = self.generic_impl_templates.clone();
        for template in templates {
            let Some(template_named) = Self::extract_named_type(&template.self_type) else {
                continue;
            };
            if Self::named_type_name(template_named) != base_name {
                continue;
            }
            let Some(template_args) = &template_named.generics else {
                continue;
            };
            if template_args.len() != receiver_args.len() {
                continue;
            }

            let mut mapping: HashMap<String, ast::Type> = HashMap::new();
            let mut valid = true;
            for (template_arg, concrete_arg) in template_args.iter().zip(receiver_args.iter()) {
                let ast::TypeKind::Named(named) = template_arg.kind.as_ref() else {
                    valid = false;
                    break;
                };
                if named.path.len() != 1 || named.generics.is_some() {
                    valid = false;
                    break;
                }
                mapping.insert(named.path[0].name.clone(), concrete_arg.clone());
            }
            if !valid {
                continue;
            }

            let Some(template_func) = template.items.iter().find_map(|item| match item {
                ast::ImplItemKind::Function(func)
                    if func.name.name == method_name && func.generics.is_none() =>
                {
                    Some(func.clone())
                }
                _ => None,
            }) else {
                continue;
            };

            let mangled_name = Self::mangle_method_name(&owner, method_name);
            if self.module.get_function(&mangled_name).is_none() {
                let mut func = template_func;
                for param in &mut func.parameters {
                    param.param_type = Self::substitute_generic_type(&param.param_type, &mapping);
                }
                if let Some(return_ty) = &mut func.return_type {
                    *return_ty = Self::substitute_generic_type(return_ty, &mapping);
                }
                Self::substitute_block_types(&mut func.body, &mapping);

                let fn_ty = self.lower_function_type(
                    &func
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect::<Vec<_>>(),
                    func.return_type.as_ref(),
                    false,
                )?;
                let function = self.module.add_function(&mangled_name, fn_ty, None);
                Self::apply_function_linkage(function, &func.visibility);

                let expects_ref = func
                    .parameters
                    .first()
                    .map(|param| {
                        matches!(param.param_type.kind.as_ref(), ast::TypeKind::Pointer(_))
                    })
                    .unwrap_or(false);
                self.method_receivers
                    .insert((owner.clone(), method_name.to_string()), expects_ref);

                let saved_fn = self.current_fn;
                let saved_block = self.builder.get_insert_block();

                self.emit_function_body(
                    function,
                    &func.parameters,
                    func.return_type.as_ref(),
                    &func.body,
                    &mangled_name,
                    &func.body.span,
                )?;

                self.current_fn = saved_fn;
                if let Some(saved_block) = saved_block {
                    self.builder.position_at_end(saved_block);
                }
            }

            return Ok(Some(mangled_name));
        }

        Ok(None)
    }

    fn receiver_owner_name(&mut self, expr: &ast::Expression) -> Option<String> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => self
                .lookup_variable(&identifier.name)
                .and_then(|info| Self::owner_name_from_type(&info.ty))
                .or_else(|| Some(Self::sanitize_monomorph(&identifier.name))),
            ast::ExpressionKind::TypeName(ty) => Self::owner_name_from_type(ty),
            ast::ExpressionKind::StructLiteral { path, .. } => Some(Self::path_name(path)),
            ast::ExpressionKind::FieldAccess { .. } | ast::ExpressionKind::Index { .. } => self
                .resolve_lvalue_ptr(expr)
                .ok()
                .and_then(|(_, ty)| Self::owner_name_from_type(&ty)),
            _ => None,
        }
    }

    fn receiver_owner_candidates(&mut self, expr: &ast::Expression) -> Vec<String> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => self
                .lookup_variable(&identifier.name)
                .map(|info| Self::owner_name_candidates_from_type(&info.ty))
                .unwrap_or_else(|| vec![Self::sanitize_monomorph(&identifier.name)]),
            ast::ExpressionKind::TypeName(ty) => Self::owner_name_candidates_from_type(ty),
            ast::ExpressionKind::StructLiteral { path, .. } => vec![Self::path_name(path)],
            ast::ExpressionKind::FieldAccess { .. } | ast::ExpressionKind::Index { .. } => self
                .resolve_lvalue_ptr(expr)
                .ok()
                .map(|(_, ty)| Self::owner_name_candidates_from_type(&ty))
                .unwrap_or_default(),
            _ => Vec::new(),
        }
    }

    fn has_generic_placeholder_type(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Generic(_) => true,
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1 && named.generics.is_none() {
                    let candidate = &named.path[0].name;
                    if self.is_generic_placeholder_name(candidate) {
                        return true;
                    }
                }
                named
                    .generics
                    .as_ref()
                    .map(|args| {
                        args.iter()
                            .any(|arg| self.has_generic_placeholder_type(arg))
                    })
                    .unwrap_or(false)
            }
            ast::TypeKind::Reference(reference) => {
                self.has_generic_placeholder_type(&reference.inner)
            }
            ast::TypeKind::Pointer(pointer) => self.has_generic_placeholder_type(&pointer.inner),
            ast::TypeKind::Array(array) => self.has_generic_placeholder_type(&array.element_type),
            ast::TypeKind::Optional(inner) => self.has_generic_placeholder_type(inner),
            ast::TypeKind::Function(function) => {
                self.has_generic_placeholder_type(&function.return_type)
                    || function
                        .parameters
                        .iter()
                        .any(|param| self.has_generic_placeholder_type(param))
            }
            ast::TypeKind::Tuple(items) => items
                .iter()
                .any(|item| self.has_generic_placeholder_type(item)),
            _ => false,
        }
    }

    fn is_generic_placeholder_name(&self, name: &str) -> bool {
        self.struct_generics
            .values()
            .any(|params| params.iter().any(|p| p == name))
    }

    fn has_generic_placeholder_signature(
        &self,
        params: &[ast::Parameter],
        return_type: Option<&ast::Type>,
    ) -> bool {
        params
            .iter()
            .any(|param| self.has_generic_placeholder_type(&param.param_type))
            || return_type
                .map(|ret| self.has_generic_placeholder_type(ret))
                .unwrap_or(false)
    }

    /// Pass-1 collection for impl methods.
    ///
    /// Registers receiver mode and declares mangled LLVM function signatures so
    /// method calls can resolve before bodies are emitted.
    fn collect_impl_method_signatures(
        &mut self,
        item: &ast::ImplItem,
        impl_visibility: &ast::Visibility,
    ) -> CodegenResult<()> {
        if item.generics.is_some() {
            return Ok(());
        }
        if self.has_generic_placeholder_type(&item.self_type) {
            return Ok(());
        }

        let Some(owner) = Self::owner_name_from_type(&item.self_type) else {
            return Ok(());
        };

        for impl_item in &item.items {
            let ast::ImplItemKind::Function(func) = impl_item else {
                continue;
            };
            if func.generics.is_some() {
                continue;
            }
            if self.has_generic_placeholder_signature(&func.parameters, func.return_type.as_ref()) {
                continue;
            }

            let expects_ref = func
                .parameters
                .first()
                .map(|param| matches!(param.param_type.kind.as_ref(), ast::TypeKind::Pointer(_)))
                .unwrap_or(false);
            self.method_receivers
                .entry((owner.clone(), func.name.name.clone()))
                .or_insert(expects_ref);

            let mangled_name = Self::mangle_method_name(&owner, &func.name.name);
            let effective_visibility =
                Self::method_effective_visibility(impl_visibility, &func.visibility);
            self.register_function_signature(
                &mangled_name,
                FunctionSig {
                    params: func
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect(),
                    return_type: func.return_type.clone(),
                    is_variadic: false,
                },
                Some(func.name.span.clone()),
                SymbolKind::ImplMethod,
            );

            if self.module.get_function(&mangled_name).is_none() {
                let fn_ty = self.lower_function_type(
                    &func
                        .parameters
                        .iter()
                        .map(|param| param.param_type.clone())
                        .collect::<Vec<_>>(),
                    func.return_type.as_ref(),
                    false,
                )?;
                let function = self.module.add_function(&mangled_name, fn_ty, None);
                Self::apply_function_linkage(function, &effective_visibility);
            } else if let Some(function) = self.module.get_function(&mangled_name) {
                Self::apply_function_linkage(function, &effective_visibility);
            }
        }

        Ok(())
    }

    fn emit_function_body(
        &mut self,
        function: FunctionValue<'ctx>,
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
        fn_name: &str,
        fn_span: &Span,
    ) -> CodegenResult<()> {
        if function.count_basic_blocks() > 0 {
            return Ok(());
        }

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
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
                    param.span.clone(),
                )
            })?;
            if let Some(scope) = self.variables.last_mut() {
                scope.insert(
                    param.name.name.clone(),
                    VarInfo {
                        ptr: alloca,
                        ty: param.param_type.clone(),
                    },
                );
            }
        }

        self.generate_block(body)?;

        let needs_terminator = self
            .builder
            .get_insert_block()
            .and_then(|block| block.get_terminator())
            .is_none();
        if needs_terminator {
            if return_type.is_some() {
                return Err(CodegenError::with_span(
                    format!("function `{fn_name}` may exit without returning a value"),
                    fn_span.clone(),
                ));
            }
            self.builder
                .build_return(None)
                .map_err(|e| CodegenError::new(format!("failed to emit return: {e}")))?;
        }

        self.pop_scope();
        self.current_fn = None;
        self.current_return_type = saved_return_type;
        Ok(())
    }

    fn create_entry_alloca(
        &self,
        function: FunctionValue<'ctx>,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let builder = self.context.create_builder();
        let entry = function
            .get_first_basic_block()
            .ok_or_else(|| CodegenError::new("function has no entry block"))?;
        if let Some(first_instr) = entry.get_first_instruction() {
            builder.position_before(&first_instr);
        } else {
            builder.position_at_end(entry);
        }
        builder
            .build_alloca(ty, name)
            .map_err(|e| CodegenError::new(format!("failed to allocate local `{name}`: {e}")))
    }

    fn intern_string_literal(&mut self, value: &str) -> CodegenResult<PointerValue<'ctx>> {
        if let Some(existing) = self.string_constants.get(value) {
            return Ok(*existing);
        }

        let global_name = format!(".str.{}", self.string_constants.len());
        let global = self
            .builder
            .build_global_string_ptr(value, &global_name)
            .map_err(|e| CodegenError::new(format!("failed to lower string literal: {e}")))?;
        let ptr = global.as_pointer_value();
        self.string_constants.insert(value.to_string(), ptr);
        Ok(ptr)
    }

    fn emit_expression_value(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::Integer(value)) => Ok(self
                .context
                .i64_type()
                .const_int(*value as u64, true)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::Float(value)) => Ok(self
                .context
                .f64_type()
                .const_float(*value)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::Bool(value)) => Ok(self
                .context
                .bool_type()
                .const_int(u64::from(*value), false)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::Char(value)) => Ok(self
                .context
                .i32_type()
                .const_int((*value) as u64, false)
                .as_basic_value_enum()),
            ast::ExpressionKind::Literal(ast::Literal::String(value)) => self
                .intern_string_literal(value)
                .map(|ptr| ptr.as_basic_value_enum()),
            ast::ExpressionKind::Identifier(identifier) => {
<<<<<<< HEAD
                let info = self.lookup_variable(&identifier.name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("unknown variable `{}`", identifier.name),
                        identifier.span.clone(),
                    )
                })?;
                let llvm_ty = self.lower_basic_type(&info.ty)?;
                self.builder
                    .build_load(llvm_ty, info.ptr, &identifier.name)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load variable `{}`: {e}", identifier.name),
                            identifier.span.clone(),
                        )
                    })
=======
                if let Some(info) = self.lookup_variable(&identifier.name) {
                    let llvm_ty = self.lower_basic_type(&info.ty)?;
                    return self
                        .builder
                        .build_load(llvm_ty, info.ptr, &identifier.name)
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to load variable `{}`: {e}", identifier.name),
                                identifier.span.clone(),
                            )
                        });
                }
                if let Some((global, ty)) = self.lookup_extern_global(&identifier.name) {
                    let llvm_ty = self.lower_basic_type(&ty)?;
                    return self
                        .builder
                        .build_load(llvm_ty, global.as_pointer_value(), &identifier.name)
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!(
                                    "failed to load extern variable `{}`: {e}",
                                    identifier.name
                                ),
                                identifier.span.clone(),
                            )
                        });
                }
                Err(CodegenError::with_span(
                    format!("unknown variable `{}`", identifier.name),
                    identifier.span.clone(),
                ))
>>>>>>> cc823df (shift to LL3)
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => self.emit_binary_expression(left, operator, right, expr),
            ast::ExpressionKind::Unary { operator, operand } => {
                self.emit_unary_expression(operator, operand, expr)
            }
            ast::ExpressionKind::Postfix { operator, operand } => {
                self.emit_postfix_expression(operator, operand, expr)
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let value = self.emit_call_expression(function, arguments, false, &expr.span)?;
                value.ok_or_else(|| {
                    CodegenError::with_span(
                        "void function call cannot be used as a value",
                        expr.span.clone(),
                    )
                })
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let value = self
                    .emit_method_call_expression(receiver, method, arguments, false, &expr.span)?;
                value.ok_or_else(|| {
                    CodegenError::with_span(
                        "void method call cannot be used as a value",
                        expr.span.clone(),
                    )
                })
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } => {
                let source = self.emit_expression_value(expression)?;
                self.cast_value_to_ast_type(source, target_type, &expr.span)
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let Some(else_branch) = else_branch.as_ref() else {
                    return Err(CodegenError::with_span(
                        "if expression requires an else branch",
                        expr.span.clone(),
                    ));
                };
                self.emit_if_expression_value(condition, then_branch, else_branch, &expr.span)
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.emit_match_expression_value(expression, arms, &expr.span)
            }
            ast::ExpressionKind::StructLiteral { path, fields } => {
                self.emit_struct_literal_value(path, fields, &expr.span)
            }
            ast::ExpressionKind::Array(items) => self.emit_array_literal_value(items, &expr.span),
            ast::ExpressionKind::Tuple(items) => self.emit_tuple_literal_value(items, &expr.span),
            ast::ExpressionKind::Initializer { .. } => Err(CodegenError::with_span(
                "initializer expression requires a target type context",
                expr.span.clone(),
            )),
            ast::ExpressionKind::FieldAccess { .. } | ast::ExpressionKind::Index { .. } => {
                let (ptr, ty) = self.resolve_lvalue_ptr(expr)?;
                let llvm_ty = self.lower_basic_type(&ty)?;
                self.builder
                    .build_load(llvm_ty, ptr, "lvalue.load")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load lvalue: {e}"),
                            expr.span.clone(),
                        )
                    })
            }
            _ => Err(CodegenError::with_span(
                format!(
                    "expression kind is not supported in LLVM IR codegen yet: {:?}",
                    expr.kind
                ),
                expr.span.clone(),
            )),
        }
    }

    fn emit_struct_literal_value(
        &mut self,
        path: &[ast::Identifier],
        fields: &[ast::FieldInit],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let struct_name = Self::path_name(path);
        let struct_ty = *self.struct_types.get(&struct_name).ok_or_else(|| {
            CodegenError::with_span(format!("unknown struct type `{struct_name}`"), span.clone())
        })?;

        if struct_ty.is_opaque() {
            return Err(CodegenError::with_span(
                format!("struct `{struct_name}` is not fully defined yet"),
                span.clone(),
            ));
        }

        let declared_fields = self
            .struct_fields
            .get(&struct_name)
            .cloned()
            .ok_or_else(|| {
                CodegenError::with_span(
                    format!("missing field metadata for struct `{struct_name}`"),
                    span.clone(),
                )
            })?;

        let mut provided_by_name: HashMap<String, &ast::FieldInit> = HashMap::new();
        for field in fields {
            if provided_by_name
                .insert(field.name.name.clone(), field)
                .is_some()
            {
                return Err(CodegenError::with_span(
                    format!("duplicate field `{}` in struct literal", field.name.name),
                    field.name.span.clone(),
                ));
            }
        }

        for field in fields {
            if !declared_fields
                .iter()
                .any(|(name, _)| name == &field.name.name)
            {
                return Err(CodegenError::with_span(
                    format!("unknown field `{}` on `{struct_name}`", field.name.name),
                    field.name.span.clone(),
                ));
            }
        }

        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for struct literal"))?;
        let temp =
            self.create_entry_alloca(function, "struct.lit.tmp", struct_ty.as_basic_type_enum())?;
        for (index, (field_name, _field_ty)) in declared_fields.iter().enumerate() {
            let Some(field_init) = provided_by_name.get(field_name) else {
                return Err(CodegenError::with_span(
                    format!("missing field `{field_name}` in struct literal"),
                    span.clone(),
                ));
            };
            let field_value = self.emit_expression_value(&field_init.value)?;
            let llvm_field_ty =
                struct_ty
                    .get_field_type_at_index(index as u32)
                    .ok_or_else(|| {
                        CodegenError::with_span(
                            format!("missing LLVM field type for `{field_name}`"),
                            field_init.name.span.clone(),
                        )
                    })?;
            let field_value =
                self.cast_value_to_basic_type(field_value, llvm_field_ty, &field_init.value.span)?;
            let field_ptr = self
                .builder
                .build_struct_gep(struct_ty, temp, index as u32, "struct.lit.ptr")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to access struct literal field `{field_name}`: {e}"),
                        field_init.name.span.clone(),
                    )
                })?;
            self.builder
                .build_store(field_ptr, field_value)
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to store struct literal field `{field_name}`: {e}"),
                        field_init.name.span.clone(),
                    )
                })?;
        }
        self.builder
            .build_load(struct_ty.as_basic_type_enum(), temp, "struct.lit.value")
            .map_err(|e| {
                CodegenError::with_span(format!("failed to load struct literal: {e}"), span.clone())
            })
    }

    fn emit_array_literal_value(
        &mut self,
        items: &[ast::Expression],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if items.is_empty() {
            return Err(CodegenError::with_span(
                "array literal cannot be empty without type context",
                span.clone(),
            ));
        }

        let first = self.emit_expression_value(&items[0])?;
        let element_ty = first.get_type();
        let mut values = vec![first];
        for item in &items[1..] {
            let value = self.emit_expression_value(item)?;
            if value.get_type() != element_ty {
                return Err(CodegenError::with_span(
                    "array literal elements must have the same type",
                    item.span.clone(),
                ));
            }
            values.push(value);
        }

        let array_ty = element_ty.array_type(values.len() as u32);
        let mut aggregate = array_ty.get_undef();
        for (index, value) in values.iter().enumerate() {
            aggregate = self
                .builder
                .build_insert_value(aggregate, *value, index as u32, "arr.lit.ins")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to build array literal element {index}: {e}"),
                        span.clone(),
                    )
                })?
                .into_array_value();
        }

        Ok(aggregate.as_basic_value_enum())
    }

    fn emit_tuple_literal_value(
        &mut self,
        items: &[ast::Expression],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let mut values = Vec::with_capacity(items.len());
        let mut field_types = Vec::with_capacity(items.len());
        for item in items {
            let value = self.emit_expression_value(item)?;
            field_types.push(value.get_type());
            values.push(value);
        }

        let tuple_ty = self.context.struct_type(&field_types, false);
        let mut aggregate = tuple_ty.get_undef();
        for (index, value) in values.iter().enumerate() {
            aggregate = self
                .builder
                .build_insert_value(aggregate, *value, index as u32, "tuple.lit.ins")
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed to build tuple literal element {index}: {e}"),
                        span.clone(),
                    )
                })?
                .into_struct_value();
        }

        Ok(aggregate.as_basic_value_enum())
    }

    fn emit_typed_initializer_value(
        &mut self,
        items: &[ast::InitializerItem],
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match target_type.kind.as_ref() {
            ast::TypeKind::Named(named) => {
                let struct_name = Self::named_type_key(named);
                let struct_ty = self.ensure_named_struct_type(named)?;
                let declared_fields =
                    self.struct_fields
                        .get(&struct_name)
                        .cloned()
                        .ok_or_else(|| {
                            CodegenError::with_span(
                                format!("missing field metadata for struct `{struct_name}`"),
                                span.clone(),
                            )
                        })?;

                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for struct initializer")
                })?;
                let temp = self.create_entry_alloca(
                    function,
                    "init.struct.tmp",
                    struct_ty.as_basic_type_enum(),
                )?;
                let named_mode = items
                    .iter()
                    .any(|item| matches!(item, ast::InitializerItem::Field { .. }));

                if named_mode {
                    let mut by_name: HashMap<String, &ast::Expression> = HashMap::new();
                    for item in items {
                        match item {
                            ast::InitializerItem::Field { name, value } => {
                                if by_name.insert(name.name.clone(), value).is_some() {
                                    return Err(CodegenError::with_span(
                                        format!("duplicate field `{}` in initializer", name.name),
                                        name.span.clone(),
                                    ));
                                }
                            }
                            _ => {
                                return Err(CodegenError::with_span(
                                    "cannot mix positional/indexed items with named struct initializer",
                                    span.clone(),
                                ));
                            }
                        }
                    }

                    for (index, (field_name, field_ty)) in declared_fields.iter().enumerate() {
                        let Some(value_expr) = by_name.get(field_name) else {
                            return Err(CodegenError::with_span(
                                format!("missing field `{field_name}` in initializer"),
                                span.clone(),
                            ));
                        };
                        let value =
                            self.emit_expression_value_for_expected(value_expr, field_ty)?;
                        let llvm_field_ty = struct_ty
                            .get_field_type_at_index(index as u32)
                            .ok_or_else(|| {
                                CodegenError::with_span(
                                    format!("missing LLVM field type for `{field_name}`"),
                                    value_expr.span.clone(),
                                )
                            })?;
                        let value =
                            self.cast_value_to_basic_type(value, llvm_field_ty, &value_expr.span)?;
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, temp, index as u32, "init.struct.ptr")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!(
                                        "failed to access struct initializer field `{field_name}`: {e}"
                                    ),
                                    value_expr.span.clone(),
                                )
                            })?;
                        self.builder.build_store(field_ptr, value).map_err(|e| {
                            CodegenError::with_span(
                                format!(
                                    "failed to store struct initializer field `{field_name}`: {e}"
                                ),
                                value_expr.span.clone(),
                            )
                        })?;
                    }
                } else {
                    if items.len() != declared_fields.len() {
                        return Err(CodegenError::with_span(
                            "positional struct initializer field count mismatch",
                            span.clone(),
                        ));
                    }
                    for (index, item) in items.iter().enumerate() {
                        let ast::InitializerItem::Positional(expr) = item else {
                            return Err(CodegenError::with_span(
                                "struct positional initializer only supports positional items",
                                span.clone(),
                            ));
                        };
                        let (_, field_ty) = declared_fields.get(index).ok_or_else(|| {
                            CodegenError::with_span(
                                format!("missing field metadata at index {index}"),
                                expr.span.clone(),
                            )
                        })?;
                        let value = self.emit_expression_value_for_expected(expr, field_ty)?;
                        let llvm_field_ty = struct_ty
                            .get_field_type_at_index(index as u32)
                            .ok_or_else(|| {
                                CodegenError::with_span(
                                    format!("missing LLVM field type at index {index}"),
                                    expr.span.clone(),
                                )
                            })?;
                        let value =
                            self.cast_value_to_basic_type(value, llvm_field_ty, &expr.span)?;
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, temp, index as u32, "init.struct.ptr")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to access struct initializer field: {e}"),
                                    expr.span.clone(),
                                )
                            })?;
                        self.builder.build_store(field_ptr, value).map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to store struct initializer field: {e}"),
                                expr.span.clone(),
                            )
                        })?;
                    }
                }
                self.builder
                    .build_load(struct_ty.as_basic_type_enum(), temp, "init.struct.value")
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to load struct initializer: {e}"),
                            span.clone(),
                        )
                    })
            }
            ast::TypeKind::Array(array) => {
                let element_ty = self.lower_basic_type(&array.element_type)?;
                let Some(size_expr) = &array.size else {
                    return Err(CodegenError::with_span(
                        "array initializer requires a known array size",
                        span.clone(),
                    ));
                };
                let ast::ExpressionKind::Literal(ast::Literal::Integer(size_value)) =
                    size_expr.kind.as_ref()
                else {
                    return Err(CodegenError::with_span(
                        "array size must be an integer literal",
                        size_expr.span.clone(),
                    ));
                };
                let size = usize::try_from(*size_value).map_err(|_| {
                    CodegenError::with_span("array size is out of range", size_expr.span.clone())
                })?;

                let array_ty = element_ty.array_type(size as u32);
                let mut slots: Vec<Option<BasicValueEnum<'ctx>>> = vec![None; size];
                let mut next_positional = 0usize;

                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => {
                            while next_positional < size && slots[next_positional].is_some() {
                                next_positional += 1;
                            }
                            if next_positional >= size {
                                return Err(CodegenError::with_span(
                                    "too many positional array initializer items",
                                    expr.span.clone(),
                                ));
                            }
                            let value =
                                self.emit_expression_value_for_expected(expr, &array.element_type)?;
                            let value =
                                self.cast_value_to_basic_type(value, element_ty, &expr.span)?;
                            slots[next_positional] = Some(value);
                            next_positional += 1;
                        }
                        ast::InitializerItem::Index { index, value } => {
                            let ast::ExpressionKind::Literal(ast::Literal::Integer(index_value)) =
                                index.kind.as_ref()
                            else {
                                return Err(CodegenError::with_span(
                                    "array designated index must be an integer literal",
                                    index.span.clone(),
                                ));
                            };
                            let idx = usize::try_from(*index_value).map_err(|_| {
                                CodegenError::with_span(
                                    "array designated index is out of range",
                                    index.span.clone(),
                                )
                            })?;
                            if idx >= size {
                                return Err(CodegenError::with_span(
                                    "array designated index exceeds array size",
                                    index.span.clone(),
                                ));
                            }
                            let evaluated = self
                                .emit_expression_value_for_expected(value, &array.element_type)?;
                            let evaluated =
                                self.cast_value_to_basic_type(evaluated, element_ty, &value.span)?;
                            slots[idx] = Some(evaluated);
                        }
                        ast::InitializerItem::Field { name, .. } => {
                            return Err(CodegenError::with_span(
                                format!(
                                    "field designator `{}` is invalid for array initializer",
                                    name.name
                                ),
                                name.span.clone(),
                            ));
                        }
                    }
                }

                let mut aggregate = array_ty.get_undef();
                for (index, slot) in slots.into_iter().enumerate() {
                    let value = slot.unwrap_or_else(|| element_ty.const_zero());
                    aggregate = self
                        .builder
                        .build_insert_value(aggregate, value, index as u32, "init.array.ins")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to build array initializer element {index}: {e}"),
                                span.clone(),
                            )
                        })?
                        .into_array_value();
                }

                Ok(aggregate.as_basic_value_enum())
            }
            ast::TypeKind::Tuple(types) => {
                if items.len() != types.len() {
                    return Err(CodegenError::with_span(
                        "tuple initializer arity mismatch",
                        span.clone(),
                    ));
                }
                let mut field_types = Vec::with_capacity(types.len());
                for ty in types {
                    field_types.push(self.lower_basic_type(ty)?);
                }
                let tuple_ty = self.context.struct_type(&field_types, false);
                let mut aggregate = tuple_ty.get_undef();
                for (index, item) in items.iter().enumerate() {
                    let ast::InitializerItem::Positional(expr) = item else {
                        return Err(CodegenError::with_span(
                            "tuple initializer only supports positional items",
                            span.clone(),
                        ));
                    };
                    let value = self.emit_expression_value_for_expected(expr, &types[index])?;
                    let value =
                        self.cast_value_to_basic_type(value, field_types[index], &expr.span)?;
                    aggregate = self
                        .builder
                        .build_insert_value(aggregate, value, index as u32, "init.tuple.ins")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to build tuple initializer element {index}: {e}"),
                                expr.span.clone(),
                            )
                        })?
                        .into_struct_value();
                }
                Ok(aggregate.as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "initializer is not supported for this target type",
                target_type.span.clone(),
            )),
        }
    }

    fn emit_expression_value_for_expected(
        &mut self,
        expr: &ast::Expression,
        expected_type: &ast::Type,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if let ast::ExpressionKind::Initializer { items } = expr.kind.as_ref() {
            self.emit_typed_initializer_value(items, expected_type, &expr.span)
        } else {
            self.emit_expression_value(expr)
        }
    }

    /// Lowers a block in value position.
    ///
    /// All statements except the last are emitted for side effects. The final
    /// statement must be an expression and becomes the block's result value.
    fn emit_block_value(&mut self, block: &ast::Block) -> CodegenResult<BasicValueEnum<'ctx>> {
        if block.statements.is_empty() {
            return Err(CodegenError::with_span(
                "value-producing block cannot be empty",
                block.span.clone(),
            ));
        }

        self.push_scope();
        for statement in &block.statements[..block.statements.len() - 1] {
            self.generate_statement(statement)?;
            let terminated = self
                .builder
                .get_insert_block()
                .and_then(|bb| bb.get_terminator())
                .is_some();
            if terminated {
                self.pop_scope();
                return Err(CodegenError::with_span(
                    "value-producing block terminated before final expression",
                    statement.span.clone(),
                ));
            }
        }

        let last = block.statements.last().expect("non-empty checked above");
        let value = match &last.kind {
            ast::StatementKind::Expression(expr) => self.emit_expression_value(expr)?,
            _ => {
                self.pop_scope();
                return Err(CodegenError::with_span(
                    "value-producing block must end with an expression",
                    last.span.clone(),
                ));
            }
        };
        self.pop_scope();
        Ok(value)
    }

    /// Lowers `if` as an expression using a PHI merge.
    ///
    /// CFG shape:
    /// - `if.expr.then`
    /// - `if.expr.else`
    /// - `if.expr.cont` (contains the PHI)
    fn emit_if_expression_value(
        &mut self,
        condition: &ast::Expression,
        then_branch: &ast::Block,
        else_branch: &ast::Block,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for if expression"))?;

        let then_bb = self.context.append_basic_block(function, "if.expr.then");
        let else_bb = self.context.append_basic_block(function, "if.expr.else");
        let cont_bb = self.context.append_basic_block(function, "if.expr.cont");

        let cond_value = self.emit_expression_value(condition)?;
        let cond_bool = self.emit_as_bool(&cond_value, &condition.span)?;
        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .map_err(|e| CodegenError::new(format!("failed to branch for if expression: {e}")))?;

        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
            Vec::new();

        self.builder.position_at_end(then_bb);
        let then_value = self.emit_block_value(then_branch)?;
        let then_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing then insertion block"))?;
        let then_terminated = then_end.get_terminator().is_some();
        if !then_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed then->cont branch: {e}")))?;
            incoming.push((then_value, then_end));
        }

        self.builder.position_at_end(else_bb);
        let else_value = self.emit_block_value(else_branch)?;
        let else_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing else insertion block"))?;
        let else_terminated = else_end.get_terminator().is_some();
        if !else_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed else->cont branch: {e}")))?;
            incoming.push((else_value, else_end));
        }

        if incoming.is_empty() {
            return Err(CodegenError::with_span(
                "if expression has no value-producing path",
                span.clone(),
            ));
        }

        let phi_ty = incoming[0].0.get_type();
        for (value, _) in incoming.iter().skip(1) {
            if value.get_type() != phi_ty {
                return Err(CodegenError::with_span(
                    "if expression branches produce different types",
                    span.clone(),
                ));
            }
        }

        self.builder.position_at_end(cont_bb);
        let phi = self
            .builder
            .build_phi(phi_ty, "if.expr")
            .map_err(|e| CodegenError::new(format!("failed to build if phi: {e}")))?;
        let refs: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = incoming
            .iter()
            .map(|(value, bb)| (value as &dyn BasicValue<'ctx>, *bb))
            .collect();
        phi.add_incoming(&refs);
        Ok(phi.as_basic_value())
    }

    /// Lowers `match` as an expression using chained condition blocks and PHI merge.
    ///
    /// Each arm gets an `arm` and `next` block. Value-producing paths branch to
    /// `match.expr.end`, where a PHI joins all incoming arm values.
    fn emit_match_expression_value(
        &mut self,
        expression: &ast::Expression,
        arms: &[ast::MatchArm],
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for match expression"))?;
        let scrutinee = self.emit_expression_value(expression)?;
        let end_bb = self.context.append_basic_block(function, "match.expr.end");
        let mut cond_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;
        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> =
            Vec::new();

        for (arm_index, arm) in arms.iter().enumerate() {
            let arm_bb = self
                .context
                .append_basic_block(function, &format!("match.expr.arm.{arm_index}"));
            let next_bb = self
                .context
                .append_basic_block(function, &format!("match.expr.next.{arm_index}"));

            self.builder.position_at_end(cond_bb);
            if arm.guard.is_some() {
                return Err(CodegenError::with_span(
                    "match guards are not supported in LLVM IR codegen yet",
                    arm.span.clone(),
                ));
            }

            match &arm.pattern.kind {
                ast::PatternKind::Wildcard | ast::PatternKind::Identifier(_) => {
                    self.builder
                        .build_unconditional_branch(arm_bb)
                        .map_err(|e| CodegenError::new(format!("failed match expr branch: {e}")))?;
                }
                ast::PatternKind::Literal(literal) => {
                    let cond = match (&scrutinee, literal) {
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Integer(value)) => {
                            let rhs = lhs.get_type().const_int(*value as u64, true);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.int")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match int compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Bool(value)) => {
                            let rhs = lhs.get_type().const_int(u64::from(*value), false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.bool")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match bool compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::IntValue(lhs), ast::Literal::Char(value)) => {
                            let rhs = lhs.get_type().const_int(*value as u64, false);
                            self.builder
                                .build_int_compare(IntPredicate::EQ, *lhs, rhs, "match.expr.char")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match char compare: {e}"))
                                })?
                        }
                        (BasicValueEnum::FloatValue(lhs), ast::Literal::Float(value)) => {
                            let rhs = lhs.get_type().const_float(*value);
                            self.builder
                                .build_float_compare(FloatPredicate::OEQ, *lhs, rhs, "match.expr.f")
                                .map_err(|e| {
                                    CodegenError::new(format!("failed match float compare: {e}"))
                                })?
                        }
                        _ => {
                            return Err(CodegenError::with_span(
                                "unsupported match literal for scrutinee type",
                                arm.pattern.span.clone(),
                            ));
                        }
                    };

                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .map_err(|e| CodegenError::new(format!("failed match expr branch: {e}")))?;
                }
                _ => {
                    return Err(CodegenError::with_span(
                        "match pattern kind is not supported in LLVM IR codegen yet",
                        arm.pattern.span.clone(),
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
                        identifier.span.clone(),
                    )
                })?;

                let inferred = self.infer_ast_type_from_value(&scrutinee, &identifier.span);
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        identifier.name.clone(),
                        VarInfo {
                            ptr: alloca,
                            ty: inferred,
                        },
                    );
                }
            }

            let arm_value = self.emit_expression_value(&arm.body)?;
            let arm_end = self
                .builder
                .get_insert_block()
                .ok_or_else(|| CodegenError::new("missing match arm insertion block"))?;
            let arm_terminated = arm_end.get_terminator().is_some();
            if !arm_terminated {
                self.builder
                    .build_unconditional_branch(end_bb)
                    .map_err(|e| CodegenError::new(format!("failed match expr end branch: {e}")))?;
                incoming.push((arm_value, arm_end));
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
                .map_err(|e| CodegenError::new(format!("failed final match expr branch: {e}")))?;
        }

        if incoming.is_empty() {
            return Err(CodegenError::with_span(
                "match expression has no value-producing path",
                span.clone(),
            ));
        }

        let phi_ty = incoming[0].0.get_type();
        for (value, _) in incoming.iter().skip(1) {
            if value.get_type() != phi_ty {
                return Err(CodegenError::with_span(
                    "match expression arms produce different types",
                    span.clone(),
                ));
            }
        }

        self.builder.position_at_end(end_bb);
        let phi = self
            .builder
            .build_phi(phi_ty, "match.expr")
            .map_err(|e| CodegenError::new(format!("failed to build match phi: {e}")))?;
        let refs: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = incoming
            .iter()
            .map(|(value, bb)| (value as &dyn BasicValue<'ctx>, *bb))
            .collect();
        phi.add_incoming(&refs);
        Ok(phi.as_basic_value())
    }

    /// Resolves an assignable expression to a pointer + AST type pair.
    ///
    /// This is the shared lvalue path used by assignment, compound-assignment,
    /// and inc/dec so all of them support identifiers, struct fields, and
    /// array indexes consistently.
    fn resolve_lvalue_ptr(
        &mut self,
        expr: &ast::Expression,
    ) -> CodegenResult<(PointerValue<'ctx>, ast::Type)> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(identifier) => {
<<<<<<< HEAD
                let info = self.lookup_variable(&identifier.name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("unknown variable `{}`", identifier.name),
                        identifier.span.clone(),
                    )
                })?;
                Ok((info.ptr, info.ty))
=======
                if let Some(info) = self.lookup_variable(&identifier.name) {
                    return Ok((info.ptr, info.ty));
                }
                if let Some((global, ty)) = self.lookup_extern_global(&identifier.name) {
                    return Ok((global.as_pointer_value(), ty));
                }
                Err(CodegenError::with_span(
                    format!("unknown variable `{}`", identifier.name),
                    identifier.span.clone(),
                ))
>>>>>>> cc823df (shift to LL3)
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let (object_ptr, object_ty) = self.resolve_lvalue_ptr(object)?;
                let (struct_ptr, named) = match object_ty.kind.as_ref() {
                    ast::TypeKind::Named(named) => (object_ptr, named),
                    ast::TypeKind::Reference(reference) => {
                        let ast::TypeKind::Named(named) = reference.inner.kind.as_ref() else {
                            return Err(CodegenError::with_span(
                                "field access on reference requires a struct pointee",
                                object.span.clone(),
                            ));
                        };
                        let ref_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let loaded = self
                            .builder
                            .build_load(ref_llvm_ty, object_ptr, "field.ref.load")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load reference receiver: {e}"),
                                    object.span.clone(),
                                )
                            })?;
                        let BasicValueEnum::PointerValue(struct_ptr) = loaded else {
                            return Err(CodegenError::with_span(
                                "reference receiver did not lower to a pointer",
                                object.span.clone(),
                            ));
                        };
                        (struct_ptr, named)
                    }
                    ast::TypeKind::Pointer(pointer) => {
                        let ast::TypeKind::Named(named) = pointer.inner.kind.as_ref() else {
                            return Err(CodegenError::with_span(
                                "field access on pointer requires a struct pointee",
                                object.span.clone(),
                            ));
                        };
                        let ptr_llvm_ty = self.lower_basic_type(&object_ty)?;
                        let loaded = self
                            .builder
                            .build_load(ptr_llvm_ty, object_ptr, "field.ptr.load")
                            .map_err(|e| {
                                CodegenError::with_span(
                                    format!("failed to load pointer receiver: {e}"),
                                    object.span.clone(),
                                )
                            })?;
                        let BasicValueEnum::PointerValue(struct_ptr) = loaded else {
                            return Err(CodegenError::with_span(
                                "pointer receiver did not lower to a pointer",
                                object.span.clone(),
                            ));
                        };
                        (struct_ptr, named)
                    }
                    _ => {
                        return Err(CodegenError::with_span(
                            "field access currently supports only struct values/references/pointers",
                            object.span.clone(),
                        ));
                    }
                };

                let _ = self.ensure_named_struct_type(named)?;
                let owner_name = Self::named_type_key(named);
                let fields = self.struct_fields.get(&owner_name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("unknown struct type `{owner_name}`"),
                        object.span.clone(),
                    )
                })?;
                let Some((field_index, (_, field_ty))) = fields
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == &field.name)
                else {
                    return Err(CodegenError::with_span(
                        format!("unknown field `{}` on `{owner_name}`", field.name),
                        field.span.clone(),
                    ));
                };

                let struct_ty = *self.struct_types.get(&owner_name).ok_or_else(|| {
                    CodegenError::with_span(
                        format!("missing LLVM struct type for `{owner_name}`"),
                        object.span.clone(),
                    )
                })?;

                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_ty, struct_ptr, field_index as u32, &field.name)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed struct field access: {e}"),
                            field.span.clone(),
                        )
                    })?;

                Ok((field_ptr, field_ty.clone()))
            }
            ast::ExpressionKind::Index { object, index } => {
                let (object_ptr, object_ty) = self.resolve_lvalue_ptr(object)?;
                let ast::TypeKind::Array(array) = object_ty.kind.as_ref() else {
                    return Err(CodegenError::with_span(
                        "index access currently supports only array values",
                        object.span.clone(),
                    ));
                };

                let array_ty = self.lower_basic_type(&object_ty)?;

                let index_value = self.emit_expression_value(index)?;
                let BasicValueEnum::IntValue(index_int) = index_value else {
                    return Err(CodegenError::with_span(
                        "array index must be an integer",
                        index.span.clone(),
                    ));
                };

                let i32_ty = self.context.i32_type();
                let index_i32 = if index_int.get_type().get_bit_width() == 32 {
                    index_int
                } else {
                    self.builder
                        .build_int_cast(index_int, i32_ty, "idx.cast")
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed to cast array index: {e}"),
                                index.span.clone(),
                            )
                        })?
                };

                let zero = i32_ty.const_zero();
                let element_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        array_ty,
                        object_ptr,
                        &[zero, index_i32],
                        "idx.ptr",
                    )
                }
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("failed array indexing: {e}"),
                        index.span.clone(),
                    )
                })?;

                Ok((element_ptr, (*array.element_type).clone()))
            }
            _ => Err(CodegenError::with_span(
                "expression is not assignable",
                expr.span.clone(),
            )),
        }
    }

    fn emit_expression_statement(&mut self, expr: &ast::Expression) -> CodegenResult<()> {
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

    fn emit_call_expression(
        &mut self,
        function_expr: &ast::Expression,
        arguments: &[ast::Expression],
        allow_void: bool,
        span: &Span,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let ast::ExpressionKind::Identifier(identifier) = function_expr.kind.as_ref() else {
            return Err(CodegenError::with_span(
                "only direct function calls are supported in LLVM IR codegen",
                function_expr.span.clone(),
            ));
        };

        let function = self.module.get_function(&identifier.name).ok_or_else(|| {
            CodegenError::with_span(
                format!("unknown function `{}`", identifier.name),
                identifier.span.clone(),
            )
        })?;
        let signature = self.signature_for_name(&identifier.name);
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
                    value = self.cast_value_to_ast_type(
                        value,
                        &signature.params[index],
                        &argument.span,
                    )?;
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

    /// Lowers a method call to a regular function call.
    ///
    /// Resolution strategy:
    /// 1) try `<Owner>__<method>` (mangled impl function)
    /// 2) fall back to `<method>`
    ///
    /// Receiver is passed either by value or pointer depending on the
    /// collected impl metadata / function signature.
    fn emit_method_call_expression(
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
            let receiver_ty = match receiver.kind.as_ref() {
                ast::ExpressionKind::Identifier(identifier) => {
                    self.lookup_variable(&identifier.name).map(|info| info.ty)
                }
                ast::ExpressionKind::FieldAccess { .. } | ast::ExpressionKind::Index { .. } => {
                    self.resolve_lvalue_ptr(receiver).ok().map(|(_, ty)| ty)
                }
                _ => None,
            };

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
            let receiver_ty = match receiver.kind.as_ref() {
                ast::ExpressionKind::Identifier(identifier) => {
                    self.lookup_variable(&identifier.name).map(|info| info.ty)
                }
                ast::ExpressionKind::FieldAccess { .. } | ast::ExpressionKind::Index { .. } => {
                    self.resolve_lvalue_ptr(receiver).ok().map(|(_, ty)| ty)
                }
                _ => None,
            };
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
                        temp.as_basic_value_enum()
                    }
                }
            } else {
                self.emit_expression_value(receiver)?
            };

            let receiver_arg = if let Some(signature) = &signature {
                if let Some(first_param) = signature.params.first() {
                    self.cast_value_to_ast_type(receiver_arg, first_param, &receiver.span)?
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
                    value = self.cast_value_to_ast_type(
                        value,
                        &signature.params[param_index],
                        &argument.span,
                    )?;
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
        if let Some(value) = call.try_as_basic_value().basic() {
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

    fn apply_variadic_default_promotion(
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

    fn emit_unary_expression(
        &mut self,
        operator: &ast::UnaryOperator,
        operand: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::UnaryOperator::Plus => self.emit_expression_value(operand),
            ast::UnaryOperator::Minus => {
                let value = self.emit_expression_value(operand)?;
                match value {
                    BasicValueEnum::IntValue(int_value) => self
                        .builder
                        .build_int_neg(int_value, "ineg")
                        .map(|v| v.as_basic_value_enum())
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed integer negation: {e}"),
                                whole_expr.span.clone(),
                            )
                        }),
                    BasicValueEnum::FloatValue(float_value) => self
                        .builder
                        .build_float_neg(float_value, "fneg")
                        .map(|v| v.as_basic_value_enum())
                        .map_err(|e| {
                            CodegenError::with_span(
                                format!("failed float negation: {e}"),
                                whole_expr.span.clone(),
                            )
                        }),
                    _ => Err(CodegenError::with_span(
                        "unary minus requires numeric operand",
                        whole_expr.span.clone(),
                    )),
                }
            }
            ast::UnaryOperator::Not => {
                let value = self.emit_expression_value(operand)?;
                let bool_value = self.emit_as_bool(&value, &operand.span)?;
                self.builder
                    .build_not(bool_value, "lnot")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed logical not: {e}"),
                            whole_expr.span.clone(),
                        )
                    })
            }
            ast::UnaryOperator::BitwiseNot => {
                let value = self.emit_expression_value(operand)?;
                let BasicValueEnum::IntValue(int_value) = value else {
                    return Err(CodegenError::with_span(
                        "bitwise not requires integer operand",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_not(int_value, "bnot")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed bitwise not: {e}"),
                            whole_expr.span.clone(),
                        )
                    })
            }
            ast::UnaryOperator::Increment => self.emit_inc_dec(operand, true, false, whole_expr),
            ast::UnaryOperator::Decrement => self.emit_inc_dec(operand, false, false, whole_expr),
        }
    }

    fn emit_postfix_expression(
        &mut self,
        operator: &ast::UnaryOperator,
        operand: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::UnaryOperator::Increment => self.emit_inc_dec(operand, true, true, whole_expr),
            ast::UnaryOperator::Decrement => self.emit_inc_dec(operand, false, true, whole_expr),
            _ => Err(CodegenError::with_span(
                "unsupported postfix operator in LLVM IR codegen",
                whole_expr.span.clone(),
            )),
        }
    }

    fn emit_inc_dec(
        &mut self,
        operand: &ast::Expression,
        increment: bool,
        return_old: bool,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let (target_ptr, target_ty) = self.resolve_lvalue_ptr(operand)?;
        let llvm_ty = self.lower_basic_type(&target_ty)?;
        let current = self
            .builder
            .build_load(llvm_ty, target_ptr, "incdec.load")
            .map_err(|e| CodegenError::new(format!("load failed: {e}")))?;

        let updated = match current {
            BasicValueEnum::IntValue(value) => {
                let one = value.get_type().const_int(1, false);
                let next = if increment {
                    self.builder
                        .build_int_add(value, one, "inc")
                        .map_err(|e| CodegenError::new(format!("inc failed: {e}")))?
                } else {
                    self.builder
                        .build_int_sub(value, one, "dec")
                        .map_err(|e| CodegenError::new(format!("dec failed: {e}")))?
                };
                next.as_basic_value_enum()
            }
            BasicValueEnum::FloatValue(value) => {
                let one = value.get_type().const_float(1.0);
                let next = if increment {
                    self.builder
                        .build_float_add(value, one, "finc")
                        .map_err(|e| CodegenError::new(format!("inc failed: {e}")))?
                } else {
                    self.builder
                        .build_float_sub(value, one, "fdec")
                        .map_err(|e| CodegenError::new(format!("dec failed: {e}")))?
                };
                next.as_basic_value_enum()
            }
            _ => {
                return Err(CodegenError::with_span(
                    "increment/decrement requires numeric operand",
                    whole_expr.span.clone(),
                ));
            }
        };

        self.builder.build_store(target_ptr, updated).map_err(|e| {
            CodegenError::with_span(
                format!("failed to update value: {e}"),
                whole_expr.span.clone(),
            )
        })?;

<<<<<<< HEAD
        if return_old {
            Ok(current)
        } else {
            Ok(updated)
        }
=======
        if return_old { Ok(current) } else { Ok(updated) }
>>>>>>> cc823df (shift to LL3)
    }

    fn emit_binary_expression(
        &mut self,
        left: &ast::Expression,
        operator: &ast::BinaryOperator,
        right: &ast::Expression,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::BinaryOperator::LogicalAnd => {
                self.emit_short_circuit_logical(left, right, true, &whole_expr.span)
            }
            ast::BinaryOperator::LogicalOr => {
                self.emit_short_circuit_logical(left, right, false, &whole_expr.span)
            }
            ast::BinaryOperator::Assign => {
                let (target_ptr, target_ty) = self.resolve_lvalue_ptr(left)?;
                let value = if let ast::ExpressionKind::Initializer { items } = right.kind.as_ref()
                {
                    self.emit_typed_initializer_value(items, &target_ty, &right.span)?
                } else {
                    let rhs = self.emit_expression_value(right)?;
                    self.cast_value_to_ast_type(rhs, &target_ty, &right.span)?
                };
                self.builder.build_store(target_ptr, value).map_err(|e| {
                    CodegenError::with_span(
                        format!("failed assignment: {e}"),
                        whole_expr.span.clone(),
                    )
                })?;
                Ok(value)
            }
            ast::BinaryOperator::AddAssign
            | ast::BinaryOperator::SubtractAssign
            | ast::BinaryOperator::MultiplyAssign
            | ast::BinaryOperator::DivideAssign
            | ast::BinaryOperator::ModuloAssign => {
                let (target_ptr, target_ty) = self.resolve_lvalue_ptr(left)?;
                let llvm_ty = self.lower_basic_type(&target_ty)?;
                let lhs = self
                    .builder
                    .build_load(llvm_ty, target_ptr, "assign.load")
                    .map_err(|e| CodegenError::new(format!("load failed: {e}")))?;
                let rhs = self.emit_expression_value(right)?;
                let rhs = self.cast_value_to_basic_type(rhs, llvm_ty, &right.span)?;
                let updated = self.emit_arith_values(&lhs, operator, &rhs, whole_expr)?;
                self.builder.build_store(target_ptr, updated).map_err(|e| {
                    CodegenError::with_span(
                        format!("failed assignment: {e}"),
                        whole_expr.span.clone(),
                    )
                })?;
                Ok(updated)
            }
            _ => {
                let lhs = self.emit_expression_value(left)?;
                let mut rhs = self.emit_expression_value(right)?;
                if rhs.get_type() != lhs.get_type() {
                    rhs = self.cast_value_to_basic_type(rhs, lhs.get_type(), &right.span)?;
                }
                self.emit_binary_values(&lhs, operator, &rhs, whole_expr)
            }
        }
    }

    /// Emits short-circuiting `&&` / `||` semantics with explicit control flow.
    ///
    /// - `&&`: false short-circuits directly to `logic.cont`
    /// - `||`: true short-circuits directly to `logic.cont`
    ///
    /// A PHI in `logic.cont` merges the short-circuit constant and RHS result.
    fn emit_short_circuit_logical(
        &mut self,
        left: &ast::Expression,
        right: &ast::Expression,
        is_and: bool,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for logical expression"))?;

        let lhs_value = self.emit_expression_value(left)?;
        let lhs_bool = self.emit_as_bool(&lhs_value, &left.span)?;
        let lhs_block = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;

        let rhs_bb = self.context.append_basic_block(function, "logic.rhs");
        let cont_bb = self.context.append_basic_block(function, "logic.cont");

        if is_and {
            self.builder
                .build_conditional_branch(lhs_bool, rhs_bb, cont_bb)
                .map_err(|e| CodegenError::new(format!("failed logical-and branch: {e}")))?;
        } else {
            self.builder
                .build_conditional_branch(lhs_bool, cont_bb, rhs_bb)
                .map_err(|e| CodegenError::new(format!("failed logical-or branch: {e}")))?;
        }

        self.builder.position_at_end(rhs_bb);
        let rhs_value = self.emit_expression_value(right)?;
        let rhs_bool = self.emit_as_bool(&rhs_value, &right.span)?;
        let rhs_end = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("missing rhs insertion block"))?;
        let rhs_terminated = rhs_end.get_terminator().is_some();
        if !rhs_terminated {
            self.builder
                .build_unconditional_branch(cont_bb)
                .map_err(|e| CodegenError::new(format!("failed rhs->cont branch: {e}")))?;
        }

        self.builder.position_at_end(cont_bb);
        let phi = self
            .builder
            .build_phi(self.context.bool_type(), "logic.phi")
            .map_err(|e| CodegenError::new(format!("failed logical phi: {e}")))?;

        let short_const = self
            .context
            .bool_type()
            .const_int(u64::from(!is_and), false);
        let mut incoming: Vec<(
            &dyn BasicValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        )> = vec![(&short_const, lhs_block)];

        if !rhs_terminated {
            incoming.push((&rhs_bool, rhs_end));
        }

        if incoming.len() < 2 {
            return Err(CodegenError::with_span(
                "logical expression has no value-producing rhs path",
                span.clone(),
            ));
        }

        phi.add_incoming(&incoming);
        Ok(phi.as_basic_value())
    }

    fn emit_arith_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let value = match operator {
                    ast::BinaryOperator::Add | ast::BinaryOperator::AddAssign => self
                        .builder
                        .build_int_add(*lhs, *rhs, "iadd")
                        .map_err(|e| CodegenError::new(format!("int add failed: {e}")))?,
                    ast::BinaryOperator::Subtract | ast::BinaryOperator::SubtractAssign => self
                        .builder
                        .build_int_sub(*lhs, *rhs, "isub")
                        .map_err(|e| CodegenError::new(format!("int sub failed: {e}")))?,
                    ast::BinaryOperator::Multiply | ast::BinaryOperator::MultiplyAssign => self
                        .builder
                        .build_int_mul(*lhs, *rhs, "imul")
                        .map_err(|e| CodegenError::new(format!("int mul failed: {e}")))?,
                    ast::BinaryOperator::Divide | ast::BinaryOperator::DivideAssign => self
                        .builder
                        .build_int_signed_div(*lhs, *rhs, "idiv")
                        .map_err(|e| CodegenError::new(format!("int div failed: {e}")))?,
                    ast::BinaryOperator::Modulo | ast::BinaryOperator::ModuloAssign => self
                        .builder
                        .build_int_signed_rem(*lhs, *rhs, "irem")
                        .map_err(|e| CodegenError::new(format!("int rem failed: {e}")))?,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported arithmetic operation",
                            whole_expr.span.clone(),
                        ));
                    }
                };
                Ok(value.as_basic_value_enum())
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let value = match operator {
                    ast::BinaryOperator::Add | ast::BinaryOperator::AddAssign => self
                        .builder
                        .build_float_add(*lhs, *rhs, "fadd")
                        .map_err(|e| CodegenError::new(format!("float add failed: {e}")))?,
                    ast::BinaryOperator::Subtract | ast::BinaryOperator::SubtractAssign => self
                        .builder
                        .build_float_sub(*lhs, *rhs, "fsub")
                        .map_err(|e| CodegenError::new(format!("float sub failed: {e}")))?,
                    ast::BinaryOperator::Multiply | ast::BinaryOperator::MultiplyAssign => self
                        .builder
                        .build_float_mul(*lhs, *rhs, "fmul")
                        .map_err(|e| CodegenError::new(format!("float mul failed: {e}")))?,
                    ast::BinaryOperator::Divide | ast::BinaryOperator::DivideAssign => self
                        .builder
                        .build_float_div(*lhs, *rhs, "fdiv")
                        .map_err(|e| CodegenError::new(format!("float div failed: {e}")))?,
                    ast::BinaryOperator::Modulo | ast::BinaryOperator::ModuloAssign => self
                        .builder
                        .build_float_rem(*lhs, *rhs, "frem")
                        .map_err(|e| CodegenError::new(format!("float rem failed: {e}")))?,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported float arithmetic operation",
                            whole_expr.span.clone(),
                        ));
                    }
                };
                Ok(value.as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "binary arithmetic requires matching numeric types",
                whole_expr.span.clone(),
            )),
        }
    }

    fn emit_binary_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match operator {
            ast::BinaryOperator::Add
            | ast::BinaryOperator::Subtract
            | ast::BinaryOperator::Multiply
            | ast::BinaryOperator::Divide
            | ast::BinaryOperator::Modulo => self.emit_arith_values(lhs, operator, rhs, whole_expr),
            ast::BinaryOperator::Equal
            | ast::BinaryOperator::NotEqual
            | ast::BinaryOperator::Less
            | ast::BinaryOperator::Greater
            | ast::BinaryOperator::LessEqual
            | ast::BinaryOperator::GreaterEqual => {
                self.emit_compare_values(lhs, operator, rhs, whole_expr)
            }
            ast::BinaryOperator::LogicalAnd => {
                let lhs_bool = self.emit_as_bool(lhs, &whole_expr.span)?;
                let rhs_bool = self.emit_as_bool(rhs, &whole_expr.span)?;
                let value = self
                    .builder
                    .build_and(lhs_bool, rhs_bool, "land")
                    .map_err(|e| CodegenError::new(format!("logical and failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            ast::BinaryOperator::LogicalOr => {
                let lhs_bool = self.emit_as_bool(lhs, &whole_expr.span)?;
                let rhs_bool = self.emit_as_bool(rhs, &whole_expr.span)?;
                let value = self
                    .builder
                    .build_or(lhs_bool, rhs_bool, "lor")
                    .map_err(|e| CodegenError::new(format!("logical or failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            ast::BinaryOperator::BitwiseAnd => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise and requires integer operands",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_and(*lhs, *rhs, "band")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise and failed: {e}")))
            }
            ast::BinaryOperator::BitwiseOr => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise or requires integer operands",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_or(*lhs, *rhs, "bor")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise or failed: {e}")))
            }
            ast::BinaryOperator::BitwiseXor => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "bitwise xor requires integer operands",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_xor(*lhs, *rhs, "bxor")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("bitwise xor failed: {e}")))
            }
            ast::BinaryOperator::LeftShift => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "left shift requires integer operands",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_left_shift(*lhs, *rhs, "shl")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("left shift failed: {e}")))
            }
            ast::BinaryOperator::RightShift => {
                let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs)
                else {
                    return Err(CodegenError::with_span(
                        "right shift requires integer operands",
                        whole_expr.span.clone(),
                    ));
                };
                self.builder
                    .build_right_shift(*lhs, *rhs, true, "shr")
                    .map(|v| v.as_basic_value_enum())
                    .map_err(|e| CodegenError::new(format!("right shift failed: {e}")))
            }
            _ => Err(CodegenError::with_span(
                "binary operator is not supported in LLVM IR codegen yet",
                whole_expr.span.clone(),
            )),
        }
    }

    fn emit_compare_values(
        &mut self,
        lhs: &BasicValueEnum<'ctx>,
        operator: &ast::BinaryOperator,
        rhs: &BasicValueEnum<'ctx>,
        whole_expr: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let pred = match operator {
                    ast::BinaryOperator::Equal => IntPredicate::EQ,
                    ast::BinaryOperator::NotEqual => IntPredicate::NE,
                    ast::BinaryOperator::Less => IntPredicate::SLT,
                    ast::BinaryOperator::Greater => IntPredicate::SGT,
                    ast::BinaryOperator::LessEqual => IntPredicate::SLE,
                    ast::BinaryOperator::GreaterEqual => IntPredicate::SGE,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported integer comparison",
                            whole_expr.span.clone(),
                        ));
                    }
                };
                let value = self
                    .builder
                    .build_int_compare(pred, *lhs, *rhs, "icmp")
                    .map_err(|e| CodegenError::new(format!("int compare failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let pred = match operator {
                    ast::BinaryOperator::Equal => FloatPredicate::OEQ,
                    ast::BinaryOperator::NotEqual => FloatPredicate::ONE,
                    ast::BinaryOperator::Less => FloatPredicate::OLT,
                    ast::BinaryOperator::Greater => FloatPredicate::OGT,
                    ast::BinaryOperator::LessEqual => FloatPredicate::OLE,
                    ast::BinaryOperator::GreaterEqual => FloatPredicate::OGE,
                    _ => {
                        return Err(CodegenError::with_span(
                            "unsupported float comparison",
                            whole_expr.span.clone(),
                        ));
                    }
                };
                let value = self
                    .builder
                    .build_float_compare(pred, *lhs, *rhs, "fcmp")
                    .map_err(|e| CodegenError::new(format!("float compare failed: {e}")))?;
                Ok(value.as_basic_value_enum())
            }
            _ => Err(CodegenError::with_span(
                "comparison requires matching numeric types",
                whole_expr.span.clone(),
            )),
        }
    }

    fn emit_as_bool(
        &mut self,
        value: &BasicValueEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<inkwell::values::IntValue<'ctx>> {
        match value {
            BasicValueEnum::IntValue(v) => {
                if v.get_type().get_bit_width() == 1 {
                    Ok(*v)
                } else {
                    let zero = v.get_type().const_zero();
                    self.builder
                        .build_int_compare(IntPredicate::NE, *v, zero, "tobool")
                        .map_err(|e| CodegenError::new(format!("int-to-bool failed: {e}")))
                }
            }
            BasicValueEnum::FloatValue(v) => {
                let zero = v.get_type().const_zero();
                self.builder
                    .build_float_compare(FloatPredicate::ONE, *v, zero, "tobool")
                    .map_err(|e| CodegenError::new(format!("float-to-bool failed: {e}")))
            }
            _ => Err(CodegenError::with_span(
                "expression cannot be used as a boolean condition",
                span.clone(),
            )),
        }
    }

    fn cast_value_to_basic_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target: BasicTypeEnum<'ctx>,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if value.get_type() == target {
            return Ok(value);
        }

        match (value, target) {
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_int_cast(int_val, int_ty, "cast.i2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("integer cast failed: {e}"), span.clone())
                }),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::FloatType(float_ty)) => self
                .builder
                .build_signed_int_to_float(int_val, float_ty, "cast.i2f")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("int-to-float cast failed: {e}"), span.clone())
                }),
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_float_to_signed_int(float_val, int_ty, "cast.f2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("float-to-int cast failed: {e}"), span.clone())
                }),
            (BasicValueEnum::FloatValue(float_val), BasicTypeEnum::FloatType(float_ty)) => self
                .builder
                .build_float_cast(float_val, float_ty, "cast.f2f")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("float cast failed: {e}"), span.clone())
                }),
            (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::PointerType(ptr_ty)) => self
                .builder
                .build_pointer_cast(ptr_val, ptr_ty, "cast.p2p")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(format!("pointer cast failed: {e}"), span.clone())
                }),
            (BasicValueEnum::PointerValue(ptr_val), BasicTypeEnum::IntType(int_ty)) => self
                .builder
                .build_ptr_to_int(ptr_val, int_ty, "cast.p2i")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("pointer-to-int cast failed: {e}"),
                        span.clone(),
                    )
                }),
            (BasicValueEnum::IntValue(int_val), BasicTypeEnum::PointerType(ptr_ty)) => self
                .builder
                .build_int_to_ptr(int_val, ptr_ty, "cast.i2p")
                .map(|v| v.as_basic_value_enum())
                .map_err(|e| {
                    CodegenError::with_span(
                        format!("int-to-pointer cast failed: {e}"),
                        span.clone(),
                    )
                }),
            (source, _) => Err(CodegenError::with_span(
                format!(
                    "unsupported cast from `{}` to `{}`",
                    source.get_type().print_to_string(),
                    target.print_to_string()
                ),
                span.clone(),
            )),
        }
    }

    fn cast_value_to_ast_type(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_type: &ast::Type,
        span: &Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let target = self.lower_basic_type(target_type)?;
        self.cast_value_to_basic_type(value, target, span)
    }

    fn emit_if_statement(
        &mut self,
        condition: &ast::Expression,
        then_branch: &ast::Block,
        else_branch: Option<&ast::Block>,
        span: &Span,
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

        self.builder.position_at_end(cont_bb);

        if then_terminated && else_terminated {
            let _ = span;
        }

        Ok(())
    }

    fn emit_while_statement(
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
        self.builder.position_at_end(body_bb);
        self.generate_block(body)?;
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

    fn emit_for_statement(
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
        self.builder.position_at_end(body_bb);
        self.generate_block(body)?;
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

    fn emit_let_statement(
        &mut self,
        let_stmt: &ast::LetStatement,
        span: &Span,
    ) -> CodegenResult<()> {
        let ast::PatternKind::Identifier(identifier) = &let_stmt.pattern.kind else {
            return Err(CodegenError::with_span(
                "only identifier let-bindings are supported in LLVM IR codegen",
                let_stmt.pattern.span.clone(),
            ));
        };

        let (storage_ty, init_value, inferred_ty) = if let Some(init_expr) = &let_stmt.initializer {
            let mut init_value =
                if let ast::ExpressionKind::Initializer { items } = init_expr.kind.as_ref() {
                    let Some(annotation) = &let_stmt.type_annotation else {
                        return Err(CodegenError::with_span(
                            "initializer requires a type annotation in LLVM IR codegen",
                            init_expr.span.clone(),
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
                    span.clone(),
                ));
            };
            let storage_ty = self.lower_basic_type(annotation)?;
            (storage_ty, storage_ty.const_zero(), annotation.clone())
        };
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for let statement"))?;
        let alloca = self.create_entry_alloca(function, &identifier.name, storage_ty)?;
        self.builder.build_store(alloca, init_value).map_err(|e| {
            CodegenError::with_span(
                format!("failed to store local `{}`: {e}", identifier.name),
                identifier.span.clone(),
            )
        })?;

        let ty = inferred_ty;
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(identifier.name.clone(), VarInfo { ptr: alloca, ty });
        }
        Ok(())
    }

    fn infer_ast_type_from_value(&self, value: &BasicValueEnum<'ctx>, span: &Span) -> ast::Type {
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
            _ => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
        };

        ast::Type {
            kind: Box::new(kind),
            span: span.clone(),
        }
    }

    fn emit_match_statement(
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
                    arm.span.clone(),
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
                                arm.pattern.span.clone(),
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
                        arm.pattern.span.clone(),
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
                        identifier.span.clone(),
                    )
                })?;

                let inferred = self.infer_ast_type_from_value(&scrutinee, &identifier.span);
                if let Some(scope) = self.variables.last_mut() {
                    scope.insert(
                        identifier.name.clone(),
                        VarInfo {
                            ptr: alloca,
                            ty: inferred,
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

fn map_opt_level(opt_level: Option<&str>) -> OptimizationLevel {
    match opt_level.unwrap_or("0") {
        "0" => OptimizationLevel::None,
        "1" => OptimizationLevel::Less,
        "2" | "s" | "z" | "fast" => OptimizationLevel::Default,
        "3" => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    }
}

impl<'ctx> SilverGenerator for LlvmIrGenerator<'ctx> {
    fn generate_program(&mut self, program: &ast::Program) -> CodegenResult<()> {
        // Pass 1: collect declarations/types so forward references can resolve.
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
<<<<<<< HEAD
=======
                ast::ItemKind::ExternVariable(extern_variable_item) => {
                    self.generate_extern_variable_item(
                        extern_variable_item,
                        &item.visibility,
                        &item.attributes,
                    )?;
                }
>>>>>>> cc823df (shift to LL3)
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
                }
                _ => {}
            }
        }

        // Pass 2: emit item bodies/remaining lowering.
        for item in &program.items {
            self.generate_item(item)?;
        }
        Ok(())
    }

    fn generate_item(&mut self, item: &ast::Item) -> CodegenResult<()> {
        match &item.kind {
            ast::ItemKind::Function(function_item) => {
                self.generate_function_item(function_item, &item.visibility, &item.attributes)
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
<<<<<<< HEAD
=======
            ast::ItemKind::ExternVariable(extern_variable_item) => self
                .generate_extern_variable_item(
                    extern_variable_item,
                    &item.visibility,
                    &item.attributes,
                ),
>>>>>>> cc823df (shift to LL3)
            ast::ItemKind::ExternBlock(extern_block_item) => self.generate_extern_block_item(
                extern_block_item,
                &item.visibility,
                &item.attributes,
            ),
        }
    }

    fn generate_function_item(
        &mut self,
        func: &ast::FunctionItem,
        visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        if func.generics.is_some() {
            return Ok(());
        }
        if self.has_generic_placeholder_signature(&func.parameters, func.return_type.as_ref()) {
            return Ok(());
        }

        self.register_function_signature(
            &func.name.name,
            FunctionSig {
                params: func
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect(),
                return_type: func.return_type.clone(),
                is_variadic: false,
            },
            Some(func.name.span.clone()),
            SymbolKind::Function,
        );

        if self.module.get_function(&func.name.name).is_none() {
            let fn_ty = self.lower_function_type(
                &func
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect::<Vec<_>>(),
                func.return_type.as_ref(),
                false,
            )?;
            let function = self.module.add_function(&func.name.name, fn_ty, None);
            Self::apply_function_linkage(function, visibility);
        }

        let Some(function) = self.module.get_function(&func.name.name) else {
            return Err(CodegenError::with_span(
                format!("function `{}` declaration is missing", func.name.name),
                func.name.span.clone(),
            ));
        };
        Self::apply_function_linkage(function, visibility);

        self.emit_function_body(
            function,
            &func.parameters,
            func.return_type.as_ref(),
            &func.body,
            &func.name.name,
            &func.body.span,
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
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        todo!()
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
            let ast::ImplItemKind::Function(func) = impl_item else {
                continue;
            };
            if func.generics.is_some() {
                continue;
            }
            if self.has_generic_placeholder_signature(&func.parameters, func.return_type.as_ref()) {
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
                )?;
                let function = self.module.add_function(&mangled_name, fn_ty, None);
                Self::apply_function_linkage(function, &effective_visibility);
            }

            let Some(function) = self.module.get_function(&mangled_name) else {
                return Err(CodegenError::with_span(
                    format!("method `{mangled_name}` declaration is missing"),
                    func.span.clone(),
                ));
            };
            Self::apply_function_linkage(function, &effective_visibility);

            self.emit_function_body(
                function,
                &func.parameters,
                func.return_type.as_ref(),
                &func.body,
                &mangled_name,
                &func.body.span,
            )?;
        }

        Ok(())
    }

    fn generate_trait_item(
        &mut self,
        item: &ast::TraitItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        todo!()
    }

    fn generate_import_item(
        &mut self,
        item: &ast::ImportItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        todo!()
    }

    fn generate_extern_function_item(
        &mut self,
        item: &ast::ExternFunctionItem,
        _visibility: &ast::Visibility,
        _attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
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
            },
            Some(item.name.span.clone()),
            SymbolKind::ExternFunction,
        );

        if self.module.get_function(&item.name.name).is_none() {
            let fn_ty = self.lower_function_type(
                &item
                    .signature
                    .parameters
                    .iter()
                    .map(|param| param.param_type.clone())
                    .collect::<Vec<_>>(),
                item.signature.return_type.as_ref(),
                item.signature.is_variadic,
            )?;
            self.module.add_function(&item.name.name, fn_ty, None);
        }

        Ok(())
    }

<<<<<<< HEAD
=======
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
            Some(item.name.span.clone()),
            CompilerPhase::Codegen,
        );
        Ok(())
    }

>>>>>>> cc823df (shift to LL3)
    fn generate_extern_block_item(
        &mut self,
        item: &ast::ExternBlockItem,
        visibility: &ast::Visibility,
        attributes: &[ast::Attribute],
    ) -> CodegenResult<()> {
        for function in &item.functions {
            self.generate_extern_function_item(function, visibility, attributes)?;
        }
        Ok(())
    }

    fn generate_block(&mut self, block: &ast::Block) -> CodegenResult<()> {
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
        self.pop_scope();
        Ok(())
    }

    fn generate_statement(&mut self, statement: &ast::Statement) -> CodegenResult<()> {
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
                _ => self.emit_expression_statement(expr),
            },
            ast::StatementKind::Let(let_stmt) => self.emit_let_statement(let_stmt, &statement.span),
            ast::StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    let mut value = self.emit_expression_value(expr)?;
                    if let Some(return_ty) = self.current_return_type.clone() {
                        value = self.cast_value_to_ast_type(value, &return_ty, &expr.span)?;
                    }
                    self.builder.build_return(Some(&value)).map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit return value: {e}"),
                            expr.span.clone(),
                        )
                    })?;
                } else {
                    self.builder.build_return(None).map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit return: {e}"),
                            statement.span.clone(),
                        )
                    })?;
                }
                Ok(())
            }
            ast::StatementKind::Break(_) => {
                let Some((break_block, _)) = self.loop_stack.last().copied() else {
                    return Err(CodegenError::with_span(
                        "break used outside of a loop",
                        statement.span.clone(),
                    ));
                };
                self.builder
                    .build_unconditional_branch(break_block)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit break branch: {e}"),
                            statement.span.clone(),
                        )
                    })?;
                Ok(())
            }
            ast::StatementKind::Continue => {
                let Some((_, continue_block)) = self.loop_stack.last().copied() else {
                    return Err(CodegenError::with_span(
                        "continue used outside of a loop",
                        statement.span.clone(),
                    ));
                };
                self.builder
                    .build_unconditional_branch(continue_block)
                    .map_err(|e| {
                        CodegenError::with_span(
                            format!("failed to emit continue branch: {e}"),
                            statement.span.clone(),
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
        todo!()
    }

    fn generate_type(&mut self, ty: &ast::Type) -> CodegenResult<()> {
        todo!()
    }

    fn generate_initializer_item(&mut self, item: &ast::InitializerItem) -> CodegenResult<()> {
        todo!()
    }

    fn generate_macro_arg(&mut self, arg: &ast::MacroArg) -> CodegenResult<()> {
        todo!()
    }

    fn generate_literal(&mut self, literal: &ast::Literal) -> CodegenResult<()> {
        todo!()
    }

    fn finish(self) -> String {
        self.module.print_to_string().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;
    use crate::semantic::monomorph;
    use crate::semantic::typeck::TypeChecker;

    fn parse_and_typecheck(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (mut program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");

        let (type_errors, monomorphs) = TypeChecker::new().check_program(&program);
        assert!(type_errors.is_empty(), "type errors: {type_errors:?}");
        monomorph::append_monomorphs(&mut program, &monomorphs);
        program
    }

    fn lower_to_llvm(source: &str) -> String {
        let program = parse_and_typecheck(source);
        LlvmIrGenerator::generate(&program).expect("llvm generation failed")
    }

    fn find_function_mut<'a>(
        program: &'a mut ast::Program,
        name: &str,
    ) -> &'a mut ast::FunctionItem {
        for item in &mut program.items {
            if let ast::ItemKind::Function(func) = &mut item.kind {
                if func.name.name == name {
                    return func;
                }
            }
        }
        panic!("function `{name}` not found");
    }

    #[test]
    fn lowers_implicit_assignment_cast() {
        let ir = lower_to_llvm("i32 main() { i64 x = 1; i32 y = 2; x = y; return 0; }");
        assert!(
            ir.contains("cast.i2i"),
            "expected integer cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_implicit_return_cast() {
        let ir = lower_to_llvm("i64 main() { i32 x = 1; return x; }");
        assert!(ir.contains("cast.i2i"), "expected return cast in IR:\n{ir}");
    }

    #[test]
    fn lowers_implicit_call_argument_cast() {
        let mut program = parse_and_typecheck(
            "i64 id(i64 x) { return x; } i32 main() { i64 y = 1; f64 z = 1.25; id(y); return 0; }",
        );
        let main_fn = find_function_mut(&mut program, "main");
        let ast::StatementKind::Expression(call_expr) = &mut main_fn.body.statements[2].kind else {
            panic!("expected call expression statement");
        };
        let ast::ExpressionKind::Call { arguments, .. } = call_expr.kind.as_mut() else {
            panic!("expected call expression");
        };
        let span = arguments[0].span.clone();
        arguments[0] = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                name: "z".to_string(),
                span: span.clone(),
            })),
            span,
        };

        let ir = LlvmIrGenerator::generate(&program).expect("llvm generation failed");
        assert!(
            ir.contains("cast.f2i"),
            "expected call-arg cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_explicit_cast_expression() {
        let mut program = parse_and_typecheck("i32 main() { f64 a = 1.5; i32 x = 1; return x; }");
        let main_fn = find_function_mut(&mut program, "main");
        let ast::StatementKind::Let(let_stmt) = &mut main_fn.body.statements[1].kind else {
            panic!("expected let statement");
        };
        let span = let_stmt
            .initializer
            .as_ref()
            .map(|e| e.span.clone())
            .expect("missing initializer span");
        let_stmt.initializer = Some(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Cast {
                expression: Box::new(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                        name: "a".to_string(),
                        span: span.clone(),
                    })),
                    span: span.clone(),
                }),
                target_type: Box::new(ast::Type {
                    kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I32)),
                    span: span.clone(),
                }),
            }),
            span,
        });

        let ir = LlvmIrGenerator::generate(&program).expect("llvm generation failed");
        assert!(
            ir.contains("cast.f2i"),
            "expected explicit cast in IR:\n{ir}"
        );
    }

    #[test]
    fn lowers_let_without_initializer_to_zero() {
        let ir = lower_to_llvm(
            "struct Point { i32 x; i32 y; } i32 main() { Point p; return p.x + p.y; }",
        );
        assert!(
            ir.contains("zeroinitializer"),
            "expected zero initialization for uninitialized let binding:\n{ir}"
        );
    }

    #[test]
    fn lowers_nested_initializer_expressions() {
        let ir = lower_to_llvm(
            "struct Point { i32 x; i32 y; } struct Rect { Point min; Point max; } i32 main() { Rect r = { .min = { .x = 1, .y = 2 }, .max = { .x = 3, .y = 4 } }; return r.max.y; }",
        );
        assert!(
            ir.contains("%Rect = type { %Point, %Point }")
                && ir.contains("getelementptr inbounds nuw %Rect"),
            "expected nested struct initializer lowering:\n{ir}"
        );
    }

    #[test]
    fn lowers_pointer_receiver_method_call_without_double_pointer_cast() {
        let ir = lower_to_llvm(
            "struct Counter { i32 value; } impl Counter { i32 read(Counter* self) { return self.value; } } i32 use_ptr(Counter* p) { return p.read(); } i32 main() { return 0; }",
        );
        assert!(
            ir.contains("call i32 @Counter__read(ptr"),
            "expected pointer receiver method call:\n{ir}"
        );
        assert!(
            ir.contains("%p1 = load ptr, ptr %p"),
            "method receiver should load pointer value before call:\n{ir}"
        );
    }
}
