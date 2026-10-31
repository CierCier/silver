use inkwell::AddressSpace;
use inkwell::types::StructType;
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue};

use super::LlvmIrGenerator;
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;

pub(crate) struct ResolvedPlace<'ctx> {
    pub(crate) ptr: PointerValue<'ctx>,
    pub(crate) ty: ast::Type,
    pub(crate) state: Option<PointerValue<'ctx>>,
    pub(crate) raw: bool,
}

// Real owner nodes are aligned allocations. This capability denotes an already
// initialized value whose storage is managed outside a local owner tree.
const INITIALIZED_CAPABILITY: u64 = 1;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn has_builtin_place_index(ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Pointer(_) | ast::TypeKind::Array(_) | ast::TypeKind::Tuple(_) => true,
            ast::TypeKind::Reference(reference) => Self::has_builtin_place_index(&reference.inner),
            _ => false,
        }
    }

    pub(crate) fn load_reference_value(
        &mut self,
        mut value: BasicValueEnum<'ctx>,
        mut ty: ast::Type,
        span: Span,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        while let ast::TypeKind::Reference(reference) = ty.kind.as_ref() {
            let data = self.reference_data(value, span)?;
            let llvm_type = self.lower_basic_type(&reference.inner)?;
            value = self.builder.build_load(llvm_type, data, "reference.value")
                .map_err(|e| CodegenError::with_span(e.to_string(), span))?;
            ty = (*reference.inner).clone();
        }
        Ok(value)
    }

    pub(crate) fn dereference_reference_place(
        &mut self,
        mut place: ResolvedPlace<'ctx>,
        span: Span,
    ) -> CodegenResult<ResolvedPlace<'ctx>> {
        while let ast::TypeKind::Reference(reference) = place.ty.kind.as_ref() {
            let value = self.builder.build_load(self.reference_type(), place.ptr, "reference.place")
                .map_err(|e| CodegenError::with_span(e.to_string(), span))?;
            place = ResolvedPlace {
                ptr: self.reference_data(value, span)?,
                state: Some(self.reference_state(value, span)?),
                ty: (*reference.inner).clone(),
                raw: false,
            };
        }
        Ok(place)
    }

    pub(crate) fn imported_reference_function_value(
        &mut self,
        function: FunctionValue<'ctx>,
        source_name: &str,
        signature: &super::FunctionSig,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        if !matches!(signature.linkage, Some(ast::ExternLinkage::C))
            || !signature.params.iter().chain(signature.return_type.iter())
                .any(|ty| matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)))
        {
            return Ok(function);
        }
        let name = format!("__silver_c_reference_adapter.{source_name}");
        if let Some(adapter) = self.module.get_function(&name) { return Ok(adapter); }
        let ty = self.lower_function_type(&signature.params, signature.return_type.as_ref(), false, None)?;
        let adapter = self.module.add_function(&name, ty, Some(inkwell::module::Linkage::Internal));
        let saved_block = self.builder.get_insert_block();
        let saved_function = self.current_fn.replace(adapter);
        let block = self.context.append_basic_block(adapter, "entry");
        self.builder.position_at_end(block);
        let result = (|| {
            let mut arguments = Vec::with_capacity(signature.params.len());
            for (index, parameter) in signature.params.iter().enumerate() {
                let value = adapter.get_nth_param(index as u32)
                    .ok_or_else(|| CodegenError::new("missing reference adapter parameter"))?;
                arguments.push(BasicMetadataValueEnum::from(
                    self.coerce_value_to_abi(value, parameter, &ast::ExternLinkage::C)?,
                ));
            }
            let call = self.builder.build_call(function, &arguments, "")
                .map_err(|e| CodegenError::new(e.to_string()))?;
            if let Some(value) = call.try_as_basic_value().basic() {
                let value = if let Some(ty) = &signature.return_type {
                    self.uncoerce_value_from_abi(value, ty, &ast::ExternLinkage::C)?
                } else { value };
                self.builder.build_return(Some(&value))
                    .map_err(|e| CodegenError::new(e.to_string()))?;
            } else {
                self.builder.build_return(None).map_err(|e| CodegenError::new(e.to_string()))?;
            }
            Ok(adapter)
        })();
        self.current_fn = saved_function;
        if let Some(block) = saved_block { self.builder.position_at_end(block); }
        else { self.builder.clear_insertion_position(); }
        result
    }

    pub(crate) fn reference_to_place(
        &mut self,
        place: ResolvedPlace<'ctx>,
        span: Span,
        volatile: bool,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        if matches!(place.ty.kind.as_ref(), ast::TypeKind::Reference(_)) {
            if volatile {
                return self.emit_volatile_load(self.reference_type().into(), place.ptr, "reborrow");
            }
            return self.builder.build_load(self.reference_type(), place.ptr, "reborrow")
                .map_err(|e| CodegenError::with_span(e.to_string(), span));
        }
        let state = match place.state {
            Some(state) => state,
            // A raw-pointee borrow requires an initialized object. Raw writes
            // remain separate and never acquire this capability implicitly.
            None if place.raw => self.initialized_reference_state(),
            None if !self.type_needs_drop_glue(&place.ty, &mut Vec::new())? => {
                self.initialized_reference_state()
            }
            None => return Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_state_unavailable(), span,
            )),
        };
        self.build_reference(place.ptr, state)
    }

    pub(crate) fn emit_reference_argument(
        &mut self,
        argument: &ast::Expression,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let ty = self.resolve_argument_type(argument).ok_or_else(|| CodegenError::with_span(
            crate::diagnostics::messages::reference_state_unavailable(), argument.span,
        ))?;
        if matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)) {
            return self.emit_expression_value(argument);
        }
        if matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(_)) {
            let data = self.emit_expression_value(argument)?.into_pointer_value();
            return self.build_reference(data, self.initialized_reference_state());
        }
        let place = match self.resolve_lvalue_place(argument) {
            Ok(place) => place,
            Err(_) => {
                let value = self.emit_expression_value(argument)?;
                let function = self.current_fn.ok_or_else(|| CodegenError::new("reference argument outside a function"))?;
                let temp = self.create_entry_alloca(function, "argument.owner", value.get_type())?;
                self.builder.build_store(temp, value).map_err(|e| CodegenError::new(e.to_string()))?;
                let name = format!("argument.owner.{}", self.temp_counter);
                self.temp_counter += 1;
                self.register_owner_state(&ast::Identifier { name, span: argument.span }, &ty, temp, true)?;
                ResolvedPlace { ptr: temp, ty, state: self.owner_node_for_storage(temp), raw: false }
            }
        };
        let volatile = self.lvalue_is_volatile(argument);
        self.reference_to_place(place, argument.span, volatile)
    }

    pub(crate) fn reference_state(
        &self,
        value: BasicValueEnum<'ctx>,
        span: Span,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let BasicValueEnum::StructValue(reference) = value else {
            return Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_carrier_expected(), span,
            ));
        };
        let state = self.builder.build_extract_value(reference, 1, "reference.node")
            .map_err(|e| CodegenError::with_span(e.to_string(), span))?;
        match state {
            BasicValueEnum::PointerValue(state) => Ok(state),
            _ => Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_carrier_expected(), span,
            )),
        }
    }

    pub(crate) fn project_reference_state(
        &self,
        state: PointerValue<'ctx>,
        index: IntValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        if state == self.initialized_reference_state() { return Ok(state); }
        let initialized = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ, state, self.initialized_reference_state(), "reference.initialized",
        ).map_err(|e| CodegenError::new(e.to_string()))?;
        let before = self.builder.get_insert_block()
            .ok_or_else(|| CodegenError::new("reference projection outside a block"))?;
        let function = before.get_parent()
            .ok_or_else(|| CodegenError::new("reference projection outside a function"))?;
        let tracked = self.context.append_basic_block(function, "reference.project");
        let done = self.context.append_basic_block(function, "reference.projected");
        self.builder.build_conditional_branch(initialized, done, tracked)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(tracked);
        let child = self.tracked_reference_child(state, index)?;
        self.builder.build_unconditional_branch(done)
            .map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.position_at_end(done);
        let pointer_type = self.context.ptr_type(AddressSpace::default());
        let result = self.builder.build_phi(pointer_type, "reference.projected.node")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        result.add_incoming(&[(&state, before), (&child, tracked)]);
        Ok(result.as_basic_value().into_pointer_value())
    }

    pub(crate) fn tracked_reference_child(
        &self,
        state: PointerValue<'ctx>,
        index: IntValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let children_ptr = self.builder.build_struct_gep(
            self.owner_node_type(), state, 1, "reference.children.ptr",
        ).map_err(|e| CodegenError::new(e.to_string()))?;
        let pointer_type = self.context.ptr_type(AddressSpace::default());
        let children = self.builder.build_load(pointer_type, children_ptr, "reference.children")
            .map_err(|e| CodegenError::new(e.to_string()))?.into_pointer_value();
        unsafe {
            self.builder.build_in_bounds_gep(self.owner_node_type(), children, &[index], "reference.child")
        }.map_err(|e| CodegenError::new(e.to_string()))
    }

    pub(crate) fn reference_range_node(
        &self,
        state: PointerValue<'ctx>,
        index: IntValue<'ctx>,
    ) -> CodegenResult<PointerValue<'ctx>> {
        if state == self.initialized_reference_state() { return Ok(state); }
        let external = self.builder.build_int_compare(inkwell::IntPredicate::EQ,
            state, self.initialized_reference_state(), "reference.range.external")
            .map_err(|e| CodegenError::new(e.to_string()))?;
        // Initialized external ranges have no node array. Do not attach
        // inbounds to the unused arithmetic on their capability token.
        let node = unsafe {
            self.builder.build_gep(self.owner_node_type(), state, &[index], "reference.range.node")
        }.map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_select(external, state, node, "reference.range.state")
            .map(|v| v.into_pointer_value())
            .map_err(|e| CodegenError::new(e.to_string()))
    }

    pub(crate) fn reference_type(&self) -> StructType<'ctx> {
        let pointer = self.context.ptr_type(AddressSpace::default());
        self.context.struct_type(&[pointer.into(), pointer.into()], false)
    }

    pub(crate) fn initialized_reference_state(&self) -> PointerValue<'ctx> {
        self.context.i64_type().const_int(INITIALIZED_CAPABILITY, false)
            .const_to_pointer(self.context.ptr_type(AddressSpace::default()))
    }

    pub(crate) fn build_reference(
        &self,
        data: PointerValue<'ctx>,
        state: PointerValue<'ctx>,
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        let value = self.builder.build_insert_value(
            self.reference_type().get_undef(), data, 0, "reference.data",
        ).map_err(|e| CodegenError::new(e.to_string()))?;
        self.builder.build_insert_value(value, state, 1, "reference.state")
            .map(|v| v.as_basic_value_enum())
            .map_err(|e| CodegenError::new(e.to_string()))
    }

    pub(crate) fn reference_data(
        &self,
        value: BasicValueEnum<'ctx>,
        span: Span,
    ) -> CodegenResult<PointerValue<'ctx>> {
        let BasicValueEnum::StructValue(reference) = value else {
            return Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_carrier_expected(), span,
            ));
        };
        let data = self.builder.build_extract_value(reference, 0, "reference.address")
            .map_err(|e| CodegenError::with_span(e.to_string(), span))?;
        match data {
            BasicValueEnum::PointerValue(data) => Ok(data),
            _ => Err(CodegenError::with_span(
                crate::diagnostics::messages::reference_carrier_expected(), span,
            )),
        }
    }
}
