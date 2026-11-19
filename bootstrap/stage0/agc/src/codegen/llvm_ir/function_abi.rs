use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, PointerValue,
};

use super::aggregate::{Operand, scalar_fields};
use super::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;

/// The most scalar fields a struct argument passes as separate arguments.
/// Two such arguments still fit the argument registers of x86-64 (6 integer,
/// 8 float) and AArch64 (8 and 8); a struct in memory costs a store per field
/// in the caller and a load per field in the callee.
const MAX_FIELD_ARGS: usize = 4;

/// How a source-level value crosses an internal (Silver-to-Silver) call.
/// FastISel lowers only scalar arguments and returns, so an aggregate never
/// crosses as a first-class value.
#[derive(Clone, Copy)]
pub(crate) enum PassMode<'ctx> {
    /// As itself: a scalar or a pointer.
    Direct,
    /// As the fields of a struct of up to `MAX_FIELD_ARGS` scalars, one
    /// argument each. A one-field struct is also returned as its field.
    Fields(StructType<'ctx>),
    /// Through a pointer. An argument points to memory that the caller leaves
    /// unchanged until the call returns, and the callee copies it into its own
    /// frame slot on entry (debug info and backtraces read parameters from the
    /// frame). A result points to the caller's slot.
    Indirect,
}

impl<'ctx> PassMode<'ctx> {
    pub(crate) fn of(ty: BasicTypeEnum<'ctx>) -> Self {
        if let Some(struct_ty) = scalar_fields(ty, MAX_FIELD_ARGS) {
            Self::Fields(struct_ty)
        } else if ty.is_struct_type() || ty.is_array_type() {
            Self::Indirect
        } else {
            Self::Direct
        }
    }

    /// Whether a result of this mode is written through a hidden first
    /// pointer argument instead of returned.
    pub(crate) fn returns_in_memory(self) -> bool {
        match self {
            Self::Direct => false,
            Self::Fields(struct_ty) => struct_ty.count_fields() > 1,
            Self::Indirect => true,
        }
    }
}

/// The function an internal call invokes.
#[derive(Clone, Copy)]
pub(crate) enum Callee<'ctx> {
    Direct(FunctionValue<'ctx>),
    Indirect(FunctionType<'ctx>, PointerValue<'ctx>),
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    /// Whether functions of `linkage` use the internal ABI (`PassMode`)
    /// rather than a foreign one.
    pub(crate) fn internal_abi(linkage: Option<&ast::ExternLinkage>) -> bool {
        matches!(linkage, None | Some(ast::ExternLinkage::Silver))
    }

    /// The LLVM type of an internal function; see `PassMode`.
    pub(crate) fn lower_internal_function_type(
        &mut self,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
        is_variadic: bool,
    ) -> CodegenResult<FunctionType<'ctx>> {
        let pointer = self.context.ptr_type(AddressSpace::default());
        let result = self.lower_result_type(return_type)?;
        let mut llvm_params: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::with_capacity(params.len() + 1);
        if result.is_some_and(|ty| PassMode::of(ty).returns_in_memory()) {
            llvm_params.push(pointer.into());
        }
        for param in params {
            let ty = self.lower_basic_type(param)?;
            match PassMode::of(ty) {
                PassMode::Direct => llvm_params.push(ty.into()),
                PassMode::Fields(struct_ty) => {
                    llvm_params.extend(struct_ty.get_field_types().into_iter().map(BasicMetadataTypeEnum::from));
                }
                PassMode::Indirect => llvm_params.push(pointer.into()),
            }
        }
        Ok(match result.map(|ty| (ty, PassMode::of(ty))) {
            Some((ty, PassMode::Direct)) => ty.fn_type(&llvm_params, is_variadic),
            Some((_, PassMode::Fields(struct_ty))) if struct_ty.count_fields() == 1 => struct_ty
                .get_field_type_at_index(0)
                .expect("one-field struct")
                .fn_type(&llvm_params, is_variadic),
            _ => self.context.void_type().fn_type(&llvm_params, is_variadic),
        })
    }

    /// The source-level LLVM result type, or `None` for `void`.
    pub(crate) fn lower_result_type(
        &mut self,
        return_type: Option<&ast::Type>,
    ) -> CodegenResult<Option<BasicTypeEnum<'ctx>>> {
        return_type
            .filter(|ty| !Self::is_void_primitive(ty))
            .map(|ty| self.lower_basic_type(ty))
            .transpose()
    }

    /// The function that owns the builder's insertion block. Calls can be
    /// emitted into a helper (a task trampoline) while `current_fn` still
    /// names the function being lowered.
    pub(crate) fn insertion_function(&self) -> CodegenResult<FunctionValue<'ctx>> {
        self.builder
            .get_insert_block()
            .and_then(|block| block.get_parent())
            .ok_or_else(|| CodegenError::new("call emitted outside a function"))
    }

    /// Emits an internal call of a function with source-level `params` and
    /// `return_type`. Arguments past `params` (a C-style variadic tail) pass
    /// as themselves. Returns the call site and the source-level result.
    pub(crate) fn build_internal_call(
        &mut self,
        callee: Callee<'ctx>,
        params: &[ast::Type],
        return_type: Option<&ast::Type>,
        args: &[Operand<'ctx>],
        name: &str,
    ) -> CodegenResult<(CallSiteValue<'ctx>, Option<BasicValueEnum<'ctx>>)> {
        let error = |e: inkwell::builder::BuilderError| CodegenError::new(format!("failed to emit call: {e}"));
        let function = self.insertion_function()?;
        let result = self.lower_result_type(return_type)?;
        let mut llvm_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(args.len() + 1);
        let result_slot = match result {
            Some(ty) if PassMode::of(ty).returns_in_memory() => {
                let slot = self.create_entry_alloca(function, "call.result", ty)?;
                llvm_args.push(slot.into());
                Some(slot)
            }
            _ => None,
        };
        for (index, &arg) in args.iter().enumerate() {
            let Some(param) = params.get(index) else {
                llvm_args.push(self.operand_value(arg, "call.arg").map_err(error)?.into());
                continue;
            };
            let ty = self.lower_basic_type(param)?;
            match (PassMode::of(ty), arg) {
                (PassMode::Direct, arg) => {
                    llvm_args.push(self.operand_value(arg, "call.arg").map_err(error)?.into());
                }
                (PassMode::Fields(struct_ty), Operand::Value(BasicValueEnum::StructValue(value))) => {
                    for field in 0..struct_ty.count_fields() {
                        llvm_args.push(self.extract_field(value, field, "call.arg").map_err(error)?.into());
                    }
                }
                (PassMode::Fields(struct_ty), Operand::Memory { ptr, .. }) => {
                    for (field, field_ty) in struct_ty.get_field_types().into_iter().enumerate() {
                        let field_ptr = self
                            .builder
                            .build_struct_gep(struct_ty, ptr, field as u32, "call.arg.ptr")
                            .map_err(error)?;
                        llvm_args.push(self.builder.build_load(field_ty, field_ptr, "call.arg").map_err(error)?.into());
                    }
                }
                (PassMode::Fields(_), Operand::Value(value)) => {
                    return Err(CodegenError::new(format!(
                        "internal call argument {index} is `{}`, expected a struct",
                        value.get_type().print_to_string()
                    )));
                }
                (PassMode::Indirect, Operand::Memory { ptr, .. }) => llvm_args.push(ptr.into()),
                (PassMode::Indirect, Operand::Value(value)) => {
                    let copy = self.create_entry_alloca(function, "call.arg", ty)?;
                    self.store_value(copy, value).map_err(error)?;
                    llvm_args.push(copy.into());
                }
            }
        }
        let call = match callee {
            Callee::Direct(function) => self.builder.build_call(function, &llvm_args, name),
            Callee::Indirect(ty, pointer) => self.builder.build_indirect_call(ty, pointer, &llvm_args, name),
        }
        .map_err(error)?;
        let value = match (result, result_slot) {
            (None, _) => None,
            (Some(ty), Some(slot)) => Some(self.load_stable_value(ty, slot, name).map_err(error)?),
            (Some(ty), None) => {
                let returned = call
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::new("internal call returned no value"))?;
                Some(match PassMode::of(ty) {
                    PassMode::Fields(struct_ty) => self
                        .builder
                        .build_insert_value(struct_ty.get_poison(), returned, 0, name)
                        .map_err(error)?
                        .as_basic_value_enum(),
                    _ => returned,
                })
            }
        };
        Ok((call, value))
    }

    /// The operand for internal call argument `value`, evaluated now. An
    /// aggregate passed by pointer is copied into a fresh slot at once, so
    /// arguments evaluated later cannot change it.
    pub(crate) fn internal_argument(&mut self, value: BasicValueEnum<'ctx>) -> CodegenResult<Operand<'ctx>> {
        let ty = value.get_type();
        if !matches!(PassMode::of(ty), PassMode::Indirect) {
            return Ok(Operand::Value(value));
        }
        let slot = self.create_entry_alloca(self.insertion_function()?, "call.arg", ty)?;
        self.store_value(slot, value)
            .map_err(|e| CodegenError::new(format!("failed to copy call argument: {e}")))?;
        Ok(Operand::Memory { ptr: slot, ty })
    }

    /// Calls `function` with source-level `args`, under the internal ABI when
    /// its registered signature uses it. Returns the source-level result.
    pub(crate) fn call_function(
        &mut self,
        function: FunctionValue<'ctx>,
        args: &[BasicValueEnum<'ctx>],
        name: &str,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let signature = function.get_name().to_str().ok().and_then(|llvm_name| self.signature_for_name(llvm_name));
        if let Some(sig) = signature.filter(|sig| Self::internal_abi(sig.linkage.as_ref())) {
            let args: Vec<Operand<'ctx>> = args.iter().copied().map(Operand::Value).collect();
            let (_, value) =
                self.build_internal_call(Callee::Direct(function), &sig.params, sig.return_type.as_ref(), &args, name)?;
            return Ok(value);
        }
        let args: Vec<BasicMetadataValueEnum<'ctx>> = args.iter().copied().map(Into::into).collect();
        let call = self
            .builder
            .build_call(function, &args, name)
            .map_err(|e| CodegenError::new(format!("failed to emit call: {e}")))?;
        Ok(call.try_as_basic_value().basic())
    }

    /// Prepares the return of `value`, of source-level type `ty`, from the
    /// function being built under the internal ABI. Writes an in-memory
    /// result through the result slot and gives `None`, or gives the scalar
    /// to `ret`.
    pub(crate) fn internal_return_value(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        value: Operand<'ctx>,
    ) -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let error = |e: inkwell::builder::BuilderError| CodegenError::new(format!("failed to emit return: {e}"));
        let mode = PassMode::of(ty);
        if mode.returns_in_memory() {
            let slot = self
                .insertion_function()?
                .get_nth_param(0)
                .ok_or_else(|| CodegenError::new("missing result slot parameter"))?
                .into_pointer_value();
            self.store_operand(slot, ty, value).map_err(error)?;
            return Ok(None);
        }
        let value = self.operand_value(value, "ret").map_err(error)?;
        Ok(Some(match (mode, value) {
            (PassMode::Fields(_), BasicValueEnum::StructValue(value)) => self.extract_field(value, 0, "ret").map_err(error)?,
            (PassMode::Fields(_), _) => return Err(CodegenError::new("returned value is not the declared struct")),
            (_, value) => value,
        }))
    }

    /// The source-level values of an internal function's parameters, of LLVM
    /// types `types`. An indirect parameter is the caller's copy in memory.
    pub(crate) fn internal_params(
        &self,
        function: FunctionValue<'ctx>,
        types: &[BasicTypeEnum<'ctx>],
        returns_in_memory: bool,
    ) -> CodegenResult<Vec<Operand<'ctx>>> {
        let error = |e: inkwell::builder::BuilderError| CodegenError::new(format!("failed to read parameter: {e}"));
        let mut llvm_params = function.get_param_iter().skip(usize::from(returns_in_memory));
        let mut next = || llvm_params.next().ok_or_else(|| CodegenError::new("missing LLVM parameter"));
        let mut operands = Vec::with_capacity(types.len());
        for &ty in types {
            operands.push(match PassMode::of(ty) {
                PassMode::Direct => Operand::Value(next()?),
                PassMode::Fields(struct_ty) => {
                    let mut value = struct_ty.get_poison();
                    for field in 0..struct_ty.count_fields() {
                        value = self
                            .builder
                            .build_insert_value(value, next()?, field, "param")
                            .map_err(error)?
                            .into_struct_value();
                    }
                    Operand::Value(value.into())
                }
                PassMode::Indirect => Operand::Memory { ptr: next()?.into_pointer_value(), ty },
            });
        }
        Ok(operands)
    }

    pub(crate) fn emit_carried_indirect_call(&mut self, value: BasicValueEnum<'ctx>,
        signature: &ast::FunctionType, args: &[BasicValueEnum<'ctx>])
        -> CodegenResult<Option<BasicValueEnum<'ctx>>> {
        let value = if let BasicValueEnum::PointerValue(pointer) = value {
            self.build_callable(self.context.ptr_type(AddressSpace::default()).const_null(), pointer)?
        } else { value };
        let internal = self.callable_entry(value, false)?;
        let foreign = self.callable_entry(value, true)?;
        let function = self.current_fn.ok_or_else(|| CodegenError::new("callback outside function"))?;
        let internal_block = self.context.append_basic_block(function, "callable.internal");
        let foreign_block = self.context.append_basic_block(function, "callable.foreign");
        let after = self.context.append_basic_block(function, "callable.result");
        let has_internal = self.builder.build_is_not_null(internal, "callable.has_internal")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_conditional_branch(has_internal, internal_block, foreign_block)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(internal_block);
        let internal_type = self.lower_function_type(&signature.parameters, Some(&signature.return_type), false, None)?;
        let internal_args: Vec<Operand<'ctx>> = args.iter().copied().map(Operand::Value).collect();
        let (_, result) = self.build_internal_call(Callee::Indirect(internal_type, internal), &signature.parameters,
            Some(&signature.return_type), &internal_args, "")?;
        self.builder.build_unconditional_branch(after).map_err(|e| CodegenError::new(e.to_string()))?;
        let internal_end = self.builder.get_insert_block().expect("internal call block");
        self.builder.position_at_end(foreign_block);
        let foreign_type = self.lower_function_type(&signature.parameters, Some(&signature.return_type), false,
            Some(ast::ExternLinkage::C))?;
        let mut foreign_args = Vec::with_capacity(args.len());
        let return_slot = if foreign_type.get_return_type().is_none()
            && !Self::is_void_primitive(&signature.return_type) {
            let ty = self.lower_basic_type(&signature.return_type)?;
            let slot = self.create_entry_alloca(function, "callback.sret", ty)?;
            foreign_args.push(slot.into());
            Some(slot)
        } else { None };
        for (argument, ty) in args.iter().zip(&signature.parameters) {
            foreign_args.push(BasicMetadataValueEnum::from(self.coerce_value_to_abi(
                *argument, ty, &ast::ExternLinkage::C)?));
        }
        let foreign_result = self.builder.build_indirect_call(foreign_type, foreign, &foreign_args, "")
            .map_err(|e| CodegenError::new(e.to_string()))?.try_as_basic_value().basic();
        let foreign_result = if let Some(slot) = return_slot {
            let ty = self.lower_basic_type(&signature.return_type)?;
            Some(self.builder.build_load(ty, slot, "callback.sret.result")
                .map_err(|e| CodegenError::new(e.to_string()))?)
        } else {
            foreign_result.map(|value| self.uncoerce_value_from_abi(
                value, &signature.return_type, &ast::ExternLinkage::C)).transpose()?
        };
        self.builder.build_unconditional_branch(after).map_err(|e| CodegenError::new(e.to_string()))?;
        let foreign_end = self.builder.get_insert_block().expect("foreign call block");
        self.builder.position_at_end(after);
        match (result, foreign_result) {
            (Some(internal), Some(foreign)) => {
                let phi = self.builder.build_phi(internal.get_type(), "callable.result")
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                phi.add_incoming(&[(&internal, internal_end), (&foreign, foreign_end)]);
                Ok(Some(phi.as_basic_value()))
            }
            (None, None) => Ok(None),
            _ => Err(CodegenError::new("callback ABI result mismatch")),
        }
    }

    pub(crate) fn function_has_reference_abi(params: &[ast::Type], result: Option<&ast::Type>) -> bool {
        params.iter().chain(result).any(|ty| match ty.kind.as_ref() {
            ast::TypeKind::Reference(_) => true,
            ast::TypeKind::Function(function) => Self::function_has_reference_abi(
                &function.parameters, Some(&function.return_type)),
            _ => false,
        })
    }

    pub(crate) fn callable_type(&self) -> StructType<'ctx> {
        self.context.get_struct_type("__silver_callable").unwrap_or_else(|| {
            let ty = self.context.opaque_struct_type("__silver_callable");
            let ptr = self.context.ptr_type(AddressSpace::default());
            ty.set_body(&[ptr.into(), ptr.into()], false);
            ty
        })
    }

    pub(crate) fn build_callable(&self, internal: PointerValue<'ctx>, foreign: PointerValue<'ctx>)
        -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = self.builder.build_insert_value(self.callable_type().get_undef(), internal, 0, "callable.internal")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_insert_value(value, foreign, 1, "callable.foreign")
            .map(|v| v.as_basic_value_enum()).map_err(|e| CodegenError::new(e.to_string()))
    }

    pub(crate) fn callable_entry(&self, value: BasicValueEnum<'ctx>, foreign: bool)
        -> CodegenResult<PointerValue<'ctx>> {
        self.extract_field(value.into_struct_value(), u32::from(foreign), "callable.entry")
            .map(|v| v.into_pointer_value()).map_err(|e| CodegenError::new(e.to_string()))
    }

    pub(crate) fn named_callable_value(&mut self, function: FunctionValue<'ctx>, name: &str,
        signature: &FunctionSig) -> CodegenResult<BasicValueEnum<'ctx>> {
        let pointer = function.as_global_value().as_pointer_value();
        if !Self::function_has_reference_abi(&signature.params, signature.return_type.as_ref()) {
            return Ok(pointer.into());
        }
        if matches!(signature.linkage, Some(ast::ExternLinkage::C)) {
            let internal = self.imported_reference_function_value(function, name, signature)?;
            return self.build_callable(internal.as_global_value().as_pointer_value(), pointer);
        }
        let adapter_name = format!("__silver_reference_callback.{}", function.get_name().to_string_lossy());
        let adapter = if let Some(adapter) = self.module.get_function(&adapter_name) { adapter } else {
            let ty = self.lower_function_type(&signature.params, signature.return_type.as_ref(), false,
                Some(ast::ExternLinkage::C))?;
            let adapter = self.module.add_function(&adapter_name, ty, Some(Linkage::Internal));
            let saved_block = self.builder.get_insert_block();
            let saved_function = self.current_fn.replace(adapter);
            self.builder.position_at_end(self.context.append_basic_block(adapter, "entry"));
            let emitted = (|| -> CodegenResult<()> {
                let mut args = Vec::with_capacity(signature.params.len());
                let sret = ty.get_return_type().is_none()
                    && signature.return_type.as_ref().is_some_and(|ret| !Self::is_void_primitive(ret));
                for (index, parameter) in signature.params.iter().enumerate() {
                    let value = adapter.get_nth_param(index as u32 + u32::from(sret))
                        .ok_or_else(|| CodegenError::new("missing callback parameter"))?;
                    args.push(Operand::Value(self.uncoerce_value_from_abi(value, parameter, &ast::ExternLinkage::C)?));
                }
                let (_, result) = self.build_internal_call(Callee::Direct(function), &signature.params,
                    signature.return_type.as_ref(), &args, "")?;
                if let Some(value) = result {
                    if sret {
                        self.builder.build_store(adapter.get_nth_param(0)
                            .expect("C ABI return slot").into_pointer_value(), value)
                            .map_err(|e| CodegenError::new(e.to_string()))?;
                        self.builder.build_return(None).map_err(|e| CodegenError::new(e.to_string()))?;
                        return Ok(());
                    }
                    let value = if let Some(ty) = &signature.return_type {
                        self.coerce_value_to_abi(value, ty, &ast::ExternLinkage::C)?
                    } else { value };
                    self.builder.build_return(Some(&value)).map_err(|e| CodegenError::new(e.to_string()))?;
                } else {
                    self.builder.build_return(None).map_err(|e| CodegenError::new(e.to_string()))?;
                }
                Ok(())
            })();
            self.current_fn = saved_function;
            if let Some(block) = saved_block { self.builder.position_at_end(block); }
            else { self.builder.clear_insertion_position(); }
            emitted?;
            adapter
        };
        self.build_callable(pointer, adapter.as_global_value().as_pointer_value())
    }
}
