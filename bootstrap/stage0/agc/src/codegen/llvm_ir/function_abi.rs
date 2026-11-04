use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::types::StructType;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};

use super::{FunctionSig, LlvmIrGenerator};
use crate::codegen::{CodegenError, CodegenResult};
use crate::parser::ast;

impl<'ctx> LlvmIrGenerator<'ctx> {
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
        let internal_args: Vec<BasicMetadataValueEnum<'ctx>> = args.iter().copied().map(Into::into).collect();
        let result = self.builder.build_indirect_call(internal_type, internal, &internal_args, "")
            .map_err(|e| CodegenError::new(e.to_string()))?.try_as_basic_value().basic();
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
        self.builder.build_extract_value(value.into_struct_value(), u32::from(foreign), "callable.entry")
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
                    args.push(BasicMetadataValueEnum::from(self.uncoerce_value_from_abi(
                        value, parameter, &ast::ExternLinkage::C)?));
                }
                let call = self.builder.build_call(function, &args, "")
                    .map_err(|e| CodegenError::new(e.to_string()))?;
                if let Some(value) = call.try_as_basic_value().basic() {
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
