use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{AnyType, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};

pub(crate) use crate::codegen::abi::AbiSlot;
use crate::codegen::abi::StructPass;
use crate::codegen::llvm_ir::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::diagnostics::messages;
use crate::parser::ast;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn value_write_method_name(
        &mut self,
        expr: &ast::Expression,
    ) -> Result<String, String> {
        let val_type = self.resolve_receiver_type(expr);
        match val_type {
            Some(ty) => Self::type_to_write_method(&ty.kind),
            None => {
                // Fallback: emit the value and infer type from LLVM type
                let val = self
                    .emit_expression_value(expr)
                    .map_err(|e| format!("cannot determine placeholder argument type: {e:?}"))?;
                let inferred = self.infer_ast_type_from_value(&val, &expr.span);
                Self::type_to_write_method(&inferred.kind)
            }
        }
    }

    pub(crate) fn is_string_type_expr(&mut self, expr: &ast::Expression) -> bool {
        if let Some(ty) = self.resolve_receiver_type(expr) {
            match ty.kind.as_ref() {
                ast::TypeKind::Named(named) => {
                    if named.path.last().map(|id| id.name.as_str()) == Some("String") {
                        return true;
                    }
                }
                ast::TypeKind::Reference(r) => {
                    if let ast::TypeKind::Named(named) = r.inner.kind.as_ref() {
                        if named.path.last().map(|id| id.name.as_str()) == Some("String") {
                            return true;
                        }
                    }
                }
                ast::TypeKind::Pointer(p) => {
                    if let ast::TypeKind::Named(named) = p.inner.kind.as_ref() {
                        if named.path.last().map(|id| id.name.as_str()) == Some("String") {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        let owners = self.receiver_owner_candidates(expr);
        owners.iter().any(|o| o == "String")
    }

    /// Map a type kind to the corresponding BufWriter write method name.
    pub(crate) fn type_to_write_method(kind: &ast::TypeKind) -> Result<String, String> {
        use crate::parser::ast::TypeKind;
        match kind {
            TypeKind::Primitive(p) => match p {
                ast::PrimitiveType::Str => Ok("write_str".to_string()),
                ast::PrimitiveType::I8
                | ast::PrimitiveType::I16
                | ast::PrimitiveType::I32
                | ast::PrimitiveType::I64 => Ok("write_i64".to_string()),
                ast::PrimitiveType::U8
                | ast::PrimitiveType::U16
                | ast::PrimitiveType::U32
                | ast::PrimitiveType::U64 => Ok("write_u64".to_string()),
                ast::PrimitiveType::F32 | ast::PrimitiveType::F64 | ast::PrimitiveType::F80 => {
                    Ok("write_f64".to_string())
                }
                ast::PrimitiveType::Bool => Ok("write_bool".to_string()),
                ast::PrimitiveType::Char => Ok("write_char".to_string()),
                ast::PrimitiveType::I128 => Ok("write_i128".to_string()),
                ast::PrimitiveType::U128 => Ok("write_u128".to_string()),
                _ => Err(format!(
                    "no BufWriter write method for primitive type {:?}",
                    p
                )),
            },
            TypeKind::Pointer(_) | TypeKind::Reference(_) => Ok("write_ptr".to_string()),
            TypeKind::Named(named) => Err(format!(
                "type '{}' cannot be formatted with {{}} (it does not implement Display)",
                Self::named_type_name(named)
            )),
            _ => Err(format!("no BufWriter write method for type {:?}", kind)),
        }
    }

    pub(crate) fn lower_basic_type(
        &mut self,
        ty: &ast::Type,
    ) -> CodegenResult<BasicTypeEnum<'ctx>> {
        // x87 80-bit floats have no WebAssembly representation: LLVM's wasm
        // backend only models IEEE types, and x86_f80_type is an x86-only
        // target extension. Reject with a clear error instead of crashing the
        // backend (docs/wasm-target.md, non-goals for v1).
        if crate::codegen::abi::target_is_wasm(Some(
            self.module.get_triple().as_str().to_str().unwrap_or(""),
        )) && matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Primitive(ast::PrimitiveType::F80)
                | ast::TypeKind::Primitive(ast::PrimitiveType::C80)
        ) {
            return Err(CodegenError::with_span(
                "type `f80`/`c80` (x87 80-bit float) is not supported on WebAssembly targets: LLVM has no wasm representation for x87 types — use `f64` instead",
                ty.span,
            ));
        }
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
                    ast::PrimitiveType::Void => {
                        return Err(CodegenError::with_span(
                            "`void` is not a first-class value type",
                            ty.span,
                        ));
                    }
                };
                Ok(basic)
            }
            ast::TypeKind::Named(named) => {
                // Check payload enum layout first: prefer the monomorphized
                // concrete instantiation (e.g. `Optional__i32` for
                // `Optional<i32>`) so the payload sizes come from the concrete
                // type args, then fall back to the bare name for non-generic
                // enums (`Optional`, `OptInt`).
                if named.path.len() == 1 {
                    let monomorph = Self::monomorph_owner_name_from_named(named);
                    if let Some(struct_ty) = self.enum_payload_layouts.get(&monomorph) {
                        return Ok(struct_ty.as_basic_type_enum());
                    }
                    // A generic enum instance (e.g. Result<Nested, JsonError>)
                    // may not be registered yet when a signature is lowered
                    // before its definition (forward-referenced call sites).
                    // Register it now so the payload size comes from the
                    // concrete type args instead of the generic placeholder.
                    if let Some(args) = &named.generics
                        && !args.is_empty()
                        && let Some(params) = self.struct_generics.get(&named.path[0].name).cloned()
                    {
                        let mut mapping = HashMap::default();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            mapping.insert(param.clone(), arg.clone());
                        }
                        let base = named.path[0].name.clone();
                        self.register_monomorphized_enum(&base, &monomorph, &mapping)?;
                        if let Some(struct_ty) = self.enum_payload_layouts.get(&monomorph) {
                            return Ok(struct_ty.as_basic_type_enum());
                        }
                    }
                    if let Some(struct_ty) = self.ensure_enum_payload_layout(&named.path[0].name)? {
                        return Ok(struct_ty.as_basic_type_enum());
                    }
                }
                if let Some(enum_backing) = self.enum_backing_type_for_named(named) {
                    return self.lower_basic_type(&ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(enum_backing)),
                        span: ty.span,
                    });
                }
                if named.path.len() == 1 && named.generics.is_none() {
                    let candidate = &named.path[0].name;
                    if self.is_generic_placeholder_name(candidate) {
                        return Ok(self.context.i64_type().as_basic_type_enum());
                    }
                }
                if named.path.len() == 1
                    && named.path[0].name == "Task"
                    && named.generics.as_ref().is_some_and(|gs| gs.len() == 1)
                {
                    // A Task<T> is an i64 handle to a thread-registry slot.
                    // `wait` consumes it; its drop glue joins a scoped task.
                    return Ok(self.context.i64_type().as_basic_type_enum());
                }
                let struct_ty = self.ensure_named_struct_type(named)?;
                Ok(struct_ty.as_basic_type_enum())
            }
            ast::TypeKind::Reference(_reference) => {
                Ok(self.reference_type().as_basic_type_enum())
            }
            ast::TypeKind::Pointer(_pointer) => {
                // Note: we do NOT recursively lower the inner type here.
                // LLVM pointers work with opaque pointee types, and skipping
                // this prevents infinite recursion for recursive structs
                // (e.g. struct Node<T> { T val; Node<T>* next; }).
                // The inner type will be fully lowered when it's actually
                // needed (field access, GEP, etc.).
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
            }
            ast::TypeKind::Slice(slice) => {
                let _ = self.lower_basic_type(&slice.element_type)?;
                let ptr_ty = self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
                let i64_ty = self.context.i64_type().as_basic_type_enum();
                Ok(self
                    .context
                    .struct_type(&[ptr_ty, i64_ty], false)
                    .as_basic_type_enum())
            }
            ast::TypeKind::Array(array) => {
                let elem_ty = self.lower_basic_type(&array.element_type)?;
                // Compile-time fixed-size arrays — use LLVM's `[N x elem]` type, no pointer+length wrapper.
                Ok(elem_ty.array_type(array.size as u32).as_basic_type_enum())
            }
            ast::TypeKind::Optional(inner) => {
                let named = ast::NamedType {
                    path: vec![ast::Identifier {
                        name: "Optional".to_string(),
                        span: ty.span,
                    }],
                    generics: Some(vec![*inner.clone()]),
                };
                let monomorph = Self::monomorph_owner_name_from_named(&named);
                if let Some(struct_ty) = self
                    .enum_payload_layouts
                    .get(&monomorph)
                    .or_else(|| self.enum_payload_layouts.get("Optional"))
                {
                    return Ok(struct_ty.as_basic_type_enum());
                }
                let inner_ty = self.lower_basic_type(inner)?;
                Ok(self
                    .context
                    .struct_type(&[self.context.i8_type().as_basic_type_enum(), inner_ty], false)
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
                if Self::function_has_reference_abi(&function.parameters, Some(&function.return_type)) {
                    return Ok(self.callable_type().as_basic_type_enum());
                }
                if !Self::is_void_primitive(&function.return_type) {
                    let _ = self.lower_basic_type(&function.return_type)?;
                }
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
                ty.span,
            )),
        }
    }

    /// How the C struct `ty` crosses a call at `slot`, or `None` when `ty`
    /// is not passed as a C struct. Errors on a target without a struct ABI.
    pub(crate) fn c_struct_pass(
        &mut self,
        ty: &ast::Type,
        slot: AbiSlot,
    ) -> CodegenResult<Option<(StructType<'ctx>, StructPass<'ctx>)>> {
        if Self::is_void_primitive(ty)
            || matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_) | ast::TypeKind::Function(_))
        {
            return Ok(None);
        }
        let BasicTypeEnum::StructType(struct_ty) = self.lower_basic_type(ty)? else {
            return Ok(None);
        };
        let target_data = self.target_data();
        let context = self.context;
        let handler = self.abi_handler.as_deref().ok_or_else(|| {
            CodegenError::with_span(
                crate::diagnostics::messages::c_struct_abi_unsupported(
                    &self.module.get_triple().as_str().to_string_lossy(),
                ),
                ty.span,
            )
        })?;
        let pass = match slot {
            AbiSlot::Argument => handler.argument(context, &target_data, struct_ty),
            AbiSlot::Result => handler.result(context, &target_data, struct_ty),
        };
        Ok(Some((struct_ty, pass)))
    }

    /// The `sret`, `byval` and `alignstack` attributes of a C signature, by
    /// LLVM parameter index. A function and every call of it carry the same
    /// set: LLVM reads them from the call site for indirect calls.
    pub(crate) fn c_abi_attributes(
        &mut self,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
    ) -> CodegenResult<Vec<(u32, Attribute)>> {
        let target_data = self.target_data();
        let type_attribute = |context: &'ctx inkwell::context::Context, name: &str, ty: StructType<'ctx>| {
            context.create_type_attribute(Attribute::get_named_enum_kind_id(name), ty.as_any_type_enum())
        };
        let enum_attribute = |context: &'ctx inkwell::context::Context, name: &str, value: u64| {
            context.create_enum_attribute(Attribute::get_named_enum_kind_id(name), value)
        };
        let mut attributes = Vec::new();
        let mut offset = 0;
        if let Some(ret) = return_type
            && let Some((struct_ty, StructPass::Indirect)) = self.c_struct_pass(ret, AbiSlot::Result)?
        {
            attributes.push((0, type_attribute(self.context, "sret", struct_ty)));
            attributes.push((0, enum_attribute(self.context, "align", target_data.get_abi_alignment(&struct_ty).into())));
            offset = 1;
        }
        for (index, param) in params.iter().enumerate() {
            let index = index as u32 + offset;
            match self.c_struct_pass(param, AbiSlot::Argument)? {
                Some((struct_ty, StructPass::ByVal)) => {
                    attributes.push((index, type_attribute(self.context, "byval", struct_ty)));
                    let align = target_data.get_abi_alignment(&struct_ty).into();
                    attributes.push((index, enum_attribute(self.context, "align", align)));
                }
                Some((_, StructPass::Direct { stack_align: Some(align), .. })) => {
                    attributes.push((index, enum_attribute(self.context, "alignstack", align)));
                }
                _ => {}
            }
        }
        Ok(attributes)
    }

    /// Whether a struct of type `ty` crosses a C call at `slot` as a pointer
    /// to a copy (`byval` or indirect) rather than as a register value.
    fn c_struct_passes_by_reference(&mut self, ty: &ast::Type, slot: AbiSlot) -> CodegenResult<bool> {
        Ok(matches!(self.c_struct_pass(ty, slot)?, Some((_, StructPass::ByVal | StructPass::Indirect))))
    }

    /// The function that receives C ABI conversion code.
    fn c_abi_function(&self) -> CodegenResult<FunctionValue<'ctx>> {
        self.current_fn.ok_or_else(|| CodegenError::new(messages::c_abi_conversion_outside_function()))
    }

    /// Reinterprets the bytes of `value` as `to` through a stack slot large
    /// and aligned enough for both types. A C struct and the type that
    /// carries it in registers may differ in size (a 3-byte struct passes as
    /// `i64` on AArch64), so neither side may read past the other.
    fn reinterpret_through_memory(
        &mut self,
        value: BasicValueEnum<'ctx>,
        to: BasicTypeEnum<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let function = self.c_abi_function()?;
        let from = value.get_type();
        let target_data = self.target_data();
        let slot_ty = if target_data.get_abi_size(&to) > target_data.get_abi_size(&from) { to } else { from };
        let slot = self.create_entry_alloca(function, "abi.slot", slot_ty)?;
        let align = target_data.get_abi_alignment(&from).max(target_data.get_abi_alignment(&to));
        slot.as_instruction()
            .expect("alloca instruction")
            .set_alignment(align)
            .map_err(|e| CodegenError::new(messages::c_abi_conversion_failed(e)))?;
        let error = |e: inkwell::builder::BuilderError| CodegenError::new(messages::c_abi_conversion_failed(e));
        self.builder.build_store(slot, value).map_err(error)?;
        self.builder.build_load(to, slot, "abi.value").map_err(error)
    }

    /// The LLVM type that carries a value of `ty` across a call of `linkage`
    /// at `slot`.
    pub(crate) fn lower_abi_type(
        &mut self,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
        slot: AbiSlot,
    ) -> CodegenResult<BasicTypeEnum<'ctx>> {
        // Handle void return types specially - they map to LLVM void type
        // Note: void is not a BasicType, so we return an error here.
        // Callers should check for void before calling this function.
        if Self::is_void_primitive(ty) {
            return Err(CodegenError::with_span(
                "void is not a valid type for ABI lowering",
                ty.span,
            ));
        }

        if matches!(linkage, ast::ExternLinkage::C)
            && matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_) | ast::TypeKind::Function(_))
        {
            return Ok(self.context.ptr_type(AddressSpace::default()).as_basic_type_enum());
        }
        let lowered = self.lower_basic_type(ty)?;
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(lowered);
        }

        // C enums use at least the width of `int` on the supported C ABIs.
        // Keep Silver's compact enum storage and promote only at this boundary.
        if matches!(lowered, BasicTypeEnum::IntType(_))
            && self.c_narrow_unit_enum_is_signed(ty).is_some()
        {
            return Ok(self.context.i32_type().as_basic_type_enum());
        }

        match self.c_struct_pass(ty, slot)? {
            Some((_, pass)) => Ok(pass.llvm_type(self.context)),
            None => Ok(lowered),
        }
    }

    /// Converts `value` of source type `ty`, sent to C as an argument or a
    /// callback result at `slot`, to its C-ABI form. A struct passed by
    /// reference becomes a pointer to a fresh copy.
    pub(crate) fn coerce_value_to_abi(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
        slot: AbiSlot,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(value);
        }

        if matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)) {
            self.require_initialized_c_reference(value, ty.span)?;
            return self.reference_data(value, ty.span).map(|v| v.as_basic_value_enum());
        }
        if let ast::TypeKind::Function(function) = ty.kind.as_ref()
            && Self::function_has_reference_abi(&function.parameters, Some(&function.return_type))
        {
            return self.callable_entry(value, true).map(|v| v.as_basic_value_enum());
        }

        let abi_ty = self.lower_abi_type(ty, linkage, slot)?;
        if abi_ty == value.get_type() {
            return Ok(value);
        }

        if let (Some(is_signed), BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(abi_ty)) =
            (self.c_narrow_unit_enum_is_signed(ty), value, abi_ty)
        {
            let promoted = if is_signed {
                self.builder
                    .build_int_s_extend_or_bit_cast(value, abi_ty, "abi.enum.sext")
            } else {
                self.builder
                    .build_int_z_extend_or_bit_cast(value, abi_ty, "abi.enum.zext")
            }
            .map_err(|e| CodegenError::new(format!("failed to promote C enum argument: {e}")))?;
            return Ok(promoted.as_basic_value_enum());
        }

        if self.c_struct_passes_by_reference(ty, slot)? {
            let copy = self.create_entry_alloca(self.c_abi_function()?, "abi.copy", value.get_type())?;
            self.builder
                .build_store(copy, value)
                .map_err(|e| CodegenError::new(messages::c_abi_conversion_failed(e)))?;
            return Ok(copy.as_basic_value_enum());
        }
        self.reinterpret_through_memory(value, abi_ty)
    }

    /// Converts a value received from C at `slot`, either a call result or a
    /// callback argument, back to source type `ty`. A struct result returned
    /// through `sret` never reaches here; callers load it from the slot.
    pub(crate) fn uncoerce_value_from_abi(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
        slot: AbiSlot,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(value);
        }
        if matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)) {
            let BasicValueEnum::PointerValue(data) = value else {
                return Err(CodegenError::with_span(
                    crate::diagnostics::messages::reference_carrier_expected(), ty.span,
                ));
            };
            return self.build_reference(data, self.initialized_reference_state());
        }
        let native_ty = self.lower_basic_type(ty)?;
        if let ast::TypeKind::Function(function) = ty.kind.as_ref()
            && Self::function_has_reference_abi(&function.parameters, Some(&function.return_type))
        {
            return self.build_callable(self.context.ptr_type(AddressSpace::default()).const_null(), value.into_pointer_value());
        }
        if native_ty == value.get_type() {
            return Ok(value);
        }

        if self.c_narrow_unit_enum_is_signed(ty).is_some()
            && let (BasicValueEnum::IntValue(value), BasicTypeEnum::IntType(native_ty)) =
                (value, native_ty)
        {
            let narrowed = self
                .builder
                .build_int_truncate(value, native_ty, "abi.enum.trunc")
                .map_err(|e| CodegenError::new(format!("failed to narrow C enum return: {e}")))?;
            return Ok(narrowed.as_basic_value_enum());
        }

        if self.c_struct_passes_by_reference(ty, slot)? {
            // The C caller passed a pointer to its own copy.
            return self
                .builder
                .build_load(native_ty, value.into_pointer_value(), "abi.byref")
                .map_err(|e| CodegenError::new(messages::c_abi_conversion_failed(e)));
        }
        self.reinterpret_through_memory(value, native_ty)
    }

    fn c_narrow_unit_enum_is_signed(&self, ty: &ast::Type) -> Option<bool> {
        let ast::TypeKind::Named(named) = ty.kind.as_ref() else {
            return None;
        };
        if named.path.len() != 1 {
            return None;
        }
        let enum_name = &named.path[0].name;
        if self.enum_variant_payload_types.contains_key(enum_name) {
            return None;
        }
        match self.enum_backing_type_for_named(named)? {
            ast::PrimitiveType::I8 | ast::PrimitiveType::I16 => Some(true),
            ast::PrimitiveType::U8 | ast::PrimitiveType::U16 => Some(false),
            _ => None,
        }
    }

    /// Emits a call to the llvm.memcpy intrinsic.
    ///
    /// This is used to copy data between alloca slots of different types
    /// during ABI coercion (e.g., copying a native struct to an ABI-classified struct).
    pub(crate) fn build_memcpy(
        &self,
        dest: PointerValue<'ctx>,
        _dest_align: u32,
        src: PointerValue<'ctx>,
        _src_align: u32,
        size: u64,
    ) -> CodegenResult<()> {
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
                    dest.into(),
                    src.into(),
                    self.context.i64_type().const_int(size, false).into(),
                    self.context.bool_type().const_int(0, false).into(), // isVolatile = false
                ],
                "memcpy",
            )
            .map_err(|e| CodegenError::new(format!("failed to emit memcpy: {e}")))?;

        Ok(())
    }

    /// Emits a call to the llvm.memset intrinsic (zero-fill of `size` bytes).
    ///
    /// Used to zero-initialize large arrays: a store of a huge constant
    /// aggregate (e.g. `store [100000 x i8] zeroinitializer`) crashes the LLVM
    /// SelectionDAG combiner, while memset lowers to a plain libcall/inline
    /// loop. `is_volatile` marks the memset volatile for volatile storage.
    pub(crate) fn build_memset(
        &self,
        dest: PointerValue<'ctx>,
        size: u64,
        is_volatile: bool,
    ) -> CodegenResult<()> {
        let memset_fn = self
            .module
            .get_function("llvm.memset.p0.p0.i64")
            .unwrap_or_else(|| {
                let i8 = self.context.i8_type();
                let i64 = self.context.i64_type();
                let i1 = self.context.bool_type();
                let ptr = self.context.ptr_type(AddressSpace::default());
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[ptr.into(), i8.into(), i64.into(), i1.into()], false);
                self.module
                    .add_function("llvm.memset.p0.p0.i64", fn_type, None)
            });

        self.builder
            .build_call(
                memset_fn,
                &[
                    dest.into(),
                    self.context.i8_type().const_zero().into(),
                    self.context.i64_type().const_int(size, false).into(),
                    self.context
                        .bool_type()
                        .const_int(u64::from(is_volatile), false)
                        .into(),
                ],
                "memset",
            )
            .map_err(|e| CodegenError::new(format!("failed to emit memset: {e}")))?;

        Ok(())
    }

    /// Adds the C-ABI parameter attributes of `sig` to its declaration.
    pub(crate) fn apply_abi_attributes(
        &mut self,
        function: FunctionValue<'ctx>,
        sig: &FunctionSig,
    ) -> CodegenResult<()> {
        if !matches!(sig.linkage, Some(ast::ExternLinkage::C)) {
            return Ok(());
        }
        for (index, attribute) in self.c_abi_attributes(&sig.params, sig.return_type.as_ref())? {
            function.add_attribute(AttributeLoc::Param(index), attribute);
        }
        Ok(())
    }

    /// The struct that a function of `linkage` returns through a hidden
    /// leading `sret` pointer instead of as a value.
    pub(crate) fn c_sret_struct(
        &mut self,
        linkage: Option<&ast::ExternLinkage>,
        return_type: Option<&ast::Type>,
    ) -> CodegenResult<Option<StructType<'ctx>>> {
        let (Some(ast::ExternLinkage::C), Some(ret)) = (linkage, return_type) else {
            return Ok(None);
        };
        Ok(match self.c_struct_pass(ret, AbiSlot::Result)? {
            Some((struct_ty, StructPass::Indirect)) => Some(struct_ty),
            _ => None,
        })
    }

    pub(crate) fn lower_function_type(
        &mut self,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
        linkage: Option<ast::ExternLinkage>,
    ) -> CodegenResult<FunctionType<'ctx>> {
        let Some(linkage) = linkage.filter(|linkage| !Self::internal_abi(Some(linkage))) else {
            return self.lower_internal_function_type(params, return_type, is_variadic);
        };
        let sret = self.c_sret_struct(Some(&linkage), return_type)?.is_some();
        let mut llvm_params = Vec::with_capacity(params.len() + usize::from(sret));
        if sret {
            llvm_params.push(self.context.ptr_type(AddressSpace::default()).into());
        }
        for param in params {
            llvm_params.push(self.lower_abi_type(param, &linkage, AbiSlot::Argument)?.into());
        }
        match return_type {
            Some(ret) if !sret && !Self::is_void_primitive(ret) => {
                Ok(self.lower_abi_type(ret, &linkage, AbiSlot::Result)?.fn_type(&llvm_params, is_variadic))
            }
            _ => Ok(self.context.void_type().fn_type(&llvm_params, is_variadic)),
        }
    }

    pub(crate) fn is_void_primitive(ty: &ast::Type) -> bool {
        matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Primitive(ast::PrimitiveType::Void)
        )
    }
}
