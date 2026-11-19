//! First-class aggregate values in IR generation.
//!
//! LLVM's fast instruction selector (FastISel, used at -O0) cannot lower a
//! first-class aggregate `load`, `store`, `insertvalue` or `extractvalue`;
//! each one sends the rest of its block to SelectionDAG. Clang never emits
//! them. Codegen keeps small structs (one or two scalar fields: reference
//! carriers, slices, callables) as SSA values, but loads and stores them field
//! by field and takes them apart by reading the `insertvalue` chain that built
//! them. That chain is then dead, and FastISel skips dead instructions.

use inkwell::builder::BuilderError;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    AggregateValue, BasicValue, BasicValueEnum, InstructionValue, PointerValue,
};
use llvm_sys::core::{
    LLVMGetAggregateElement, LLVMGetIndices, LLVMGetNumIndices, LLVMGetOperand, LLVMIsAConstant,
    LLVMIsAInsertValueInst,
};
use llvm_sys::prelude::LLVMValueRef;

use super::LlvmIrGenerator;

/// A value handed to a call or a copy: a first-class value, or an aggregate
/// that already lives in memory and is read before anything writes there.
#[derive(Clone, Copy)]
pub(crate) enum Operand<'ctx> {
    Value(BasicValueEnum<'ctx>),
    Memory {
        ptr: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    },
}

/// A struct that travels as SSA scalars: one or two int, float or pointer
/// fields. Packed structs keep whole-value accesses, whose alignment claim of 1
/// covers every field.
pub(crate) fn scalar_struct(ty: BasicTypeEnum<'_>) -> Option<StructType<'_>> {
    scalar_fields(ty, 2)
}

/// A struct that is not packed and has one to `max_fields` int, float or
/// pointer fields.
pub(crate) fn scalar_fields(ty: BasicTypeEnum<'_>, max_fields: usize) -> Option<StructType<'_>> {
    let BasicTypeEnum::StructType(struct_ty) = ty else {
        return None;
    };
    let fields = struct_ty.get_field_types();
    let scalar = !struct_ty.is_packed()
        && (1..=max_fields).contains(&fields.len())
        && fields.iter().all(|field| {
            matches!(
                field,
                BasicTypeEnum::IntType(_) | BasicTypeEnum::FloatType(_) | BasicTypeEnum::PointerType(_)
            )
        });
    scalar.then_some(struct_ty)
}

/// Field `index` of `aggregate` when the `insertvalue` chain or constant that
/// produced it names the field directly.
fn folded_field(aggregate: LLVMValueRef, index: u32) -> Option<LLVMValueRef> {
    let mut current = aggregate;
    // SAFETY: `current` is a live value of the generator's module; the
    // accessors only read its operands and indices.
    unsafe {
        loop {
            if !LLVMIsAInsertValueInst(current).is_null() {
                let first = *LLVMGetIndices(current);
                if first == index {
                    return (LLVMGetNumIndices(current) == 1).then(|| LLVMGetOperand(current, 1));
                }
                current = LLVMGetOperand(current, 0);
            } else if !LLVMIsAConstant(current).is_null() {
                let element = LLVMGetAggregateElement(current, index);
                return (!element.is_null()).then_some(element);
            } else {
                return None;
            }
        }
    }
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    /// `build_extract_value` that reads the field from the producing
    /// `insertvalue` chain or constant instead of emitting an instruction.
    pub(crate) fn extract_field<AV: AggregateValue<'ctx>>(
        &self,
        aggregate: AV,
        index: u32,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match folded_field(aggregate.as_value_ref(), index) {
            // SAFETY: a struct field operand is a first-class basic value.
            Some(field) => Ok(unsafe { BasicValueEnum::new(field) }),
            None => self.builder.build_extract_value(aggregate, index, name),
        }
    }

    /// `build_load` that loads a scalar struct field by field.
    pub(crate) fn load_value<T: BasicType<'ctx>>(
        &self,
        ty: T,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let ty = ty.as_basic_type_enum();
        let Some(struct_ty) = scalar_struct(ty) else {
            return self.builder.build_load(ty, ptr, name);
        };
        let mut value = struct_ty.get_poison().as_basic_value_enum();
        for (index, field_ty) in struct_ty.get_field_types().into_iter().enumerate() {
            let index = index as u32;
            let field_ptr = self.builder.build_struct_gep(struct_ty, ptr, index, name)?;
            let field = self.builder.build_load(field_ty, field_ptr, name)?;
            value = self
                .builder
                .build_insert_value(value.into_struct_value(), field, index, name)?
                .as_basic_value_enum();
        }
        Ok(value)
    }

    /// `build_store` that stores a scalar struct field by field. Returns the
    /// last store.
    pub(crate) fn store_value<V: BasicValue<'ctx>>(
        &self,
        ptr: PointerValue<'ctx>,
        value: V,
    ) -> Result<InstructionValue<'ctx>, BuilderError> {
        let value = value.as_basic_value_enum();
        let Some(struct_ty) = scalar_struct(value.get_type()) else {
            return self.builder.build_store(ptr, value);
        };
        let mut last = None;
        for index in 0..struct_ty.count_fields() {
            let field = self.extract_field(value.into_struct_value(), index, "field")?;
            let field_ptr = self.builder.build_struct_gep(struct_ty, ptr, index, "field.ptr")?;
            last = Some(self.builder.build_store(field_ptr, field)?);
        }
        Ok(last.expect("scalar structs have at least one field"))
    }

    /// The operand as a first-class value, loading it from memory if needed.
    pub(crate) fn operand_value(
        &self,
        operand: Operand<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match operand {
            Operand::Value(value) => Ok(value),
            Operand::Memory { ptr, ty } => self.load_value(ty, ptr, name),
        }
    }

    /// Writes `operand`, a value of type `ty`, to `dest`. An operand in
    /// memory is copied with `memcpy`: both places hold a `ty`, so both are
    /// aligned for it, and they never partially overlap.
    pub(crate) fn store_operand(
        &self,
        dest: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        operand: Operand<'ctx>,
    ) -> Result<(), BuilderError> {
        match operand {
            Operand::Value(value) => self.store_value(dest, value).map(|_| ()),
            Operand::Memory { ptr, .. } if ptr == dest => Ok(()),
            Operand::Memory { ptr, .. } => {
                let target_data = self.target_data();
                let align = target_data.get_abi_alignment(&ty);
                let size = self.context.i64_type().const_int(target_data.get_abi_size(&ty), false);
                self.builder.build_memcpy(dest, align, ptr, align, size).map(|_| ())
            }
        }
    }
}
