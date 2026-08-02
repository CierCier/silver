use inkwell::AddressSpace;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::targets::TargetData;
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};

use crate::codegen::llvm_ir::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
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
                ast::PrimitiveType::Char => Ok("write_u8".to_string()),
                ast::PrimitiveType::I128 => Ok("write_i128".to_string()),
                ast::PrimitiveType::U128 => Ok("write_u128".to_string()),
                _ => Err(format!(
                    "no BufWriter write method for primitive type {:?}",
                    p
                )),
            },
            _ => Err(format!("no BufWriter write method for type {:?}", kind,)),
        }
    }

    pub(crate) fn lower_basic_type(
        &mut self,
        ty: &ast::Type,
    ) -> CodegenResult<BasicTypeEnum<'ctx>> {
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
                // Check payload enum layout first
                if named.path.len() == 1
                    && let Some(struct_ty) = self.enum_payload_layouts.get(&named.path[0].name)
                {
                    return Ok(struct_ty.as_basic_type_enum());
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
                let struct_ty = self.ensure_named_struct_type(named)?;
                Ok(struct_ty.as_basic_type_enum())
            }
            ast::TypeKind::Reference(_reference) => {
                // Note: we do NOT recursively lower the inner type here,
                // for the same reason as Pointer — it prevents infinite
                // recursion with recursive structs.
                Ok(self
                    .context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum())
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
                let inner = self.lower_basic_type(inner)?;
                Ok(self
                    .context
                    .struct_type(&[self.context.i8_type().as_basic_type_enum(), inner], false)
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

    pub(crate) fn lower_abi_type(
        &mut self,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
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

        let lowered = self.lower_basic_type(ty)?;
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(lowered);
        }

        if let BasicTypeEnum::StructType(struct_ty) = lowered {
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            let size = target_data.get_store_size(&struct_ty);

            match size {
                1 => Ok(self.context.i8_type().as_basic_type_enum()),
                2 => Ok(self.context.i16_type().as_basic_type_enum()),
                4 => Ok(self.context.i32_type().as_basic_type_enum()),
                8 => {
                    // Special case: two floats → <2 x float> for AMD64
                    let fields = struct_ty.get_field_types();
                    if fields.len() == 2 && fields.iter().all(|f| f.is_float_type()) {
                        Ok(self.context.f32_type().vec_type(2).as_basic_type_enum())
                    } else {
                        Ok(self.context.i64_type().as_basic_type_enum())
                    }
                }
                9..=16 => {
                    // Use full ABI classification for 9-16 byte structs
                    Ok(self
                        .abi_handler
                        .classify_argument(self.context, &target_data, struct_ty))
                }
                s if s > 16 => {
                    // Large struct: pass by reference
                    Ok(self
                        .context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum())
                }
                _ => Ok(lowered),
            }
        } else {
            Ok(lowered)
        }
    }

    pub(crate) fn coerce_value_to_abi(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(value);
        }

        let abi_ty = self.lower_abi_type(ty, linkage)?;
        if abi_ty == value.get_type() {
            return Ok(value);
        }

        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for ABI coercion"))?;

        let alloca = self.create_entry_alloca(function, "abi_coercion_tmp", value.get_type())?;
        self.builder
            .build_store(alloca, value)
            .map_err(|e| CodegenError::new(format!("failed to store for ABI coercion: {e}")))?;

        if abi_ty.is_pointer_type() && !value.get_type().is_pointer_type() {
            // Large struct being passed by pointer (byval)
            Ok(alloca.as_basic_value_enum())
        } else if abi_ty.is_struct_type() && value.get_type().is_struct_type() {
            // Struct-to-struct conversion: use memcpy
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            let struct_ty = value.get_type().into_struct_type();
            let size = target_data.get_store_size(&struct_ty);

            let abi_alloca = self.create_entry_alloca(function, "abi_coercion_tmp2", abi_ty)?;

            self.build_memcpy(abi_alloca, 1, alloca, 1, size)?;

            let coerced = self
                .builder
                .build_load(abi_ty, abi_alloca, "abi_coerced")
                .map_err(|e| {
                    CodegenError::new(format!("failed to load coerced struct value: {e}"))
                })?;

            Ok(coerced)
        } else {
            // Scalar conversion: load as new type
            let coerced = self
                .builder
                .build_load(abi_ty, alloca, "abi_coerced")
                .map_err(|e| CodegenError::new(format!("failed to load coerced value: {e}")))?;

            Ok(coerced)
        }
    }

    pub(crate) fn uncoerce_value_from_abi(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &ast::Type,
        linkage: &ast::ExternLinkage,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(value);
        }
        let native_ty = self.lower_basic_type(ty)?;
        if native_ty == value.get_type() {
            return Ok(value);
        }

        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for ABI uncoercion"))?;

        if value.get_type().is_pointer_type() && !native_ty.is_pointer_type() {
            // Large struct returned by pointer (unlikely for many C functions but possible)
            let uncoerced = self
                .builder
                .build_load(native_ty, value.into_pointer_value(), "abi_uncoerced")
                .map_err(|e| CodegenError::new(format!("failed to load uncoerced value: {e}")))?;
            return Ok(uncoerced);
        }

        if native_ty.is_struct_type() && value.get_type().is_struct_type() {
            // Struct-to-struct conversion: use memcpy
            let target_data =
                TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
            let abi_struct_ty = value.get_type().into_struct_type();
            let size = target_data.get_store_size(&abi_struct_ty);

            let native_alloca =
                self.create_entry_alloca(function, "abi_uncoercion_tmp", native_ty)?;

            let abi_alloca =
                self.create_entry_alloca(function, "abi_uncoercion_tmp2", value.get_type())?;

            self.builder.build_store(abi_alloca, value).map_err(|e| {
                CodegenError::new(format!("failed to store for ABI struct uncoercion: {e}"))
            })?;

            self.build_memcpy(native_alloca, 1, abi_alloca, 1, size)?;

            let uncoerced = self
                .builder
                .build_load(native_ty, native_alloca, "abi_uncoerced")
                .map_err(|e| {
                    CodegenError::new(format!("failed to load uncoerced struct value: {e}"))
                })?;

            return Ok(uncoerced);
        }

        let alloca = self.create_entry_alloca(function, "abi_uncoercion_tmp", value.get_type())?;
        self.builder
            .build_store(alloca, value)
            .map_err(|e| CodegenError::new(format!("failed to store for ABI uncoercion: {e}")))?;

        let uncoerced = self
            .builder
            .build_load(native_ty, alloca, "abi_uncoerced")
            .map_err(|e| CodegenError::new(format!("failed to load uncoerced value: {e}")))?;

        Ok(uncoerced)
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

    pub(crate) fn apply_abi_attributes(
        &mut self,
        function: FunctionValue<'ctx>,
        sig: &FunctionSig,
    ) -> CodegenResult<()> {
        let Some(linkage) = &sig.linkage else {
            return Ok(());
        };

        if !matches!(linkage, ast::ExternLinkage::C) {
            return Ok(());
        }

        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());

        // Check if return type needs sret to calculate parameter offset
        let mut sret_offset: u32 = 0;
        if let Some(ret_ty) = &sig.return_type
            && !Self::is_void_primitive(ret_ty)
        {
            let lowered_ret = self.lower_basic_type(ret_ty)?;
            if let BasicTypeEnum::StructType(struct_ty) = lowered_ret {
                let size = target_data.get_store_size(&struct_ty);
                if self.abi_handler.needs_sret(size) {
                    sret_offset = 1;
                    // Add sret attribute to the first parameter (implicit return pointer)
                    let sret_kind = Attribute::get_named_enum_kind_id("sret");
                    let attr = self
                        .context
                        .create_type_attribute(sret_kind, struct_ty.as_any_type_enum());
                    function.add_attribute(AttributeLoc::Param(0), attr);

                    let align = self.abi_handler.byval_alignment(struct_ty, &target_data);
                    let align_kind = Attribute::get_named_enum_kind_id("align");
                    let align_attr = self.context.create_enum_attribute(align_kind, align);
                    function.add_attribute(AttributeLoc::Param(0), align_attr);
                }
            }
        }

        // Add byval attributes to user parameters (shifted by sret_offset)
        for (i, param_ty) in sig.params.iter().enumerate() {
            let lowered = self.lower_basic_type(param_ty)?;
            if let BasicTypeEnum::StructType(struct_ty) = lowered {
                let size = target_data.get_store_size(&struct_ty);
                if self.abi_handler.needs_byval(size) {
                    let param_idx = (i as u32) + sret_offset;
                    let byval_kind = Attribute::get_named_enum_kind_id("byval");
                    let attr = self
                        .context
                        .create_type_attribute(byval_kind, struct_ty.as_any_type_enum());
                    function.add_attribute(AttributeLoc::Param(param_idx), attr);

                    let align = self.abi_handler.byval_alignment(struct_ty, &target_data);
                    let align_kind = Attribute::get_named_enum_kind_id("align");
                    let align_attr = self.context.create_enum_attribute(align_kind, align);
                    function.add_attribute(AttributeLoc::Param(param_idx), align_attr);
                }
            }
        }

        Ok(())
    }

    pub(crate) fn lower_function_type(
        &mut self,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
        linkage: Option<ast::ExternLinkage>,
    ) -> CodegenResult<FunctionType<'ctx>> {
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());

        // Check if return type needs sret (hidden pointer parameter)
        let mut needs_sret = false;
        if let Some(ret) = return_type
            && !Self::is_void_primitive(ret)
            && let Some(_link) = &linkage
        {
            let lowered_ret = self.lower_basic_type(ret)?;
            if let BasicTypeEnum::StructType(struct_ty) = lowered_ret {
                let size = target_data.get_store_size(&struct_ty);
                if self.abi_handler.needs_sret(size) {
                    needs_sret = true;
                }
            }
        }

        let mut llvm_params = Vec::with_capacity(params.len() + if needs_sret { 1 } else { 0 });

        // If sret is needed, add implicit return pointer as first parameter
        if needs_sret {
            let ptr = self.context.ptr_type(AddressSpace::default());
            llvm_params.push(BasicMetadataTypeEnum::from(ptr));
        }

        for param in params {
            let lowered = if let Some(linkage) = &linkage {
                self.lower_abi_type(param, linkage)?
            } else {
                self.lower_basic_type(param)?
            };
            let basic_meta = BasicMetadataTypeEnum::from(lowered);
            llvm_params.push(basic_meta);
        }

        if let Some(ret) = return_type {
            if Self::is_void_primitive(ret) {
                return Ok(self.context.void_type().fn_type(&llvm_params, is_variadic));
            }
            if needs_sret {
                // Return type becomes void when using sret
                return Ok(self.context.void_type().fn_type(&llvm_params, is_variadic));
            }
            let basic_ret = if let Some(linkage) = &linkage {
                self.lower_abi_type(ret, linkage)?
            } else {
                self.lower_basic_type(ret)?
            };
            Ok(basic_ret.fn_type(&llvm_params, is_variadic))
        } else {
            Ok(self.context.void_type().fn_type(&llvm_params, is_variadic))
        }
    }

    pub(crate) fn is_void_primitive(ty: &ast::Type) -> bool {
        matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Primitive(ast::PrimitiveType::Void)
        )
    }
}
