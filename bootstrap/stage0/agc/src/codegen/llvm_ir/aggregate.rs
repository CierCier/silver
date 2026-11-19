//! Struct and array values in IR generation.
//!
//! LLVM's fast instruction selector (FastISel, used at -O0) cannot lower a
//! first-class aggregate `load`, `store`, `insertvalue` or `extractvalue`;
//! each one sends the rest of its block to SelectionDAG. Clang never emits
//! them.
//!
//! Small structs (one or two scalar fields: reference carriers, slices,
//! callables) stay SSA values. Codegen loads and stores them field by field
//! and takes them apart by reading the `insertvalue` chain that built them.
//!
//! Every other aggregate lives in memory. Its value is the `load` of its
//! place. Storing that value copies the place with `memcpy`, and reading a
//! field loads it through a GEP, as long as nothing has written memory since
//! the load. The aggregate load is then dead. FastISel skips dead
//! instructions, and -O2 deletes them.

use inkwell::builder::BuilderError;
use inkwell::module::Linkage;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{AggregateValue, AsValueRef, BasicValue, BasicValueEnum, PointerValue};
use llvm_sys::LLVMAtomicOrdering;
use llvm_sys::LLVMOpcode;
use llvm_sys::core::{
    LLVMGetAggregateElement, LLVMGetAlignment, LLVMGetIndices, LLVMGetInstructionOpcode,
    LLVMGetInstructionParent, LLVMGetNextInstruction, LLVMGetNumIndices, LLVMGetOperand,
    LLVMGetAllocatedType, LLVMGetCalledValue, LLVMGetOrdering, LLVMGetValueName2, LLVMGetVolatile,
    LLVMIsAAllocaInst, LLVMIsAConstant, LLVMIsAFunction, LLVMIsAInsertValueInst, LLVMIsALoadInst,
    LLVMIsNull, LLVMIsPoison, LLVMIsUndef, LLVMTypeOf,
};
use llvm_sys::prelude::LLVMValueRef;
use llvm_sys::target::LLVMABISizeOfType;

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
/// fields. Packed structs stay in memory, where a copy's alignment claim of 1
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

/// # Safety
/// `instruction` must be a live instruction.
unsafe fn may_write_memory(instruction: LLVMValueRef) -> bool {
    matches!(
        unsafe { LLVMGetInstructionOpcode(instruction) },
        LLVMOpcode::LLVMStore
            | LLVMOpcode::LLVMCall
            | LLVMOpcode::LLVMInvoke
            | LLVMOpcode::LLVMCallBr
            | LLVMOpcode::LLVMAtomicRMW
            | LLVMOpcode::LLVMAtomicCmpXchg
            | LLVMOpcode::LLVMFence
            | LLVMOpcode::LLVMVAArg
    )
}

/// Whether `write`, an instruction that may write memory, cannot change a
/// place holding an aggregate of `size` bytes. Owner-state helpers write only
/// owner-state nodes, which no value's storage contains. A store into a local
/// smaller than the aggregate (a drop flag) cannot reach a place inside it.
///
/// # Safety
/// `write` must be a live instruction.
unsafe fn leaves_place_unchanged(write: LLVMValueRef, size: u64, target_data: &TargetData) -> bool {
    unsafe {
        match LLVMGetInstructionOpcode(write) {
            LLVMOpcode::LLVMCall => {
                let callee = LLVMGetCalledValue(write);
                if LLVMIsAFunction(callee).is_null() {
                    return false;
                }
                let mut len = 0;
                let name = LLVMGetValueName2(callee, &mut len);
                std::slice::from_raw_parts(name.cast::<u8>(), len).starts_with(b"__silver_owner_")
            }
            LLVMOpcode::LLVMStore => {
                let target = LLVMGetOperand(write, 1);
                !LLVMIsAAllocaInst(target).is_null()
                    && LLVMABISizeOfType(target_data.as_mut_ptr(), LLVMGetAllocatedType(target)) < size
            }
            _ => false,
        }
    }
}

/// # Safety
/// `value` must be a live value.
unsafe fn is_undefined(value: LLVMValueRef) -> bool {
    unsafe { LLVMIsUndef(value) != 0 || LLVMIsPoison(value) != 0 }
}

impl<'ctx> LlvmIrGenerator<'ctx> {
    /// The place `value` was loaded from and the load's alignment, when the
    /// place still holds `value`: `value` is a plain load of a stable place,
    /// or a plain load in the insertion block after which no instruction may
    /// write memory.
    fn unchanged_place(&self, value: LLVMValueRef) -> Option<(PointerValue<'ctx>, u32)> {
        let block = self.builder.get_insert_block()?;
        // SAFETY: `value` and the instructions after it are live values of
        // the module being built; the accessors only read them.
        unsafe {
            if LLVMIsALoadInst(value).is_null()
                || LLVMGetVolatile(value) != 0
                || LLVMGetOrdering(value) != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
            {
                return None;
            }
            let place = (PointerValue::new(LLVMGetOperand(value, 0)), LLVMGetAlignment(value));
            if self.stable_places.contains(&place.0) {
                return Some(place);
            }
            if LLVMGetInstructionParent(value) != block.as_mut_ptr() {
                return None;
            }
            let target_data = self.target_data();
            let size = LLVMABISizeOfType(target_data.as_mut_ptr(), LLVMTypeOf(value));
            let mut next = LLVMGetNextInstruction(value);
            while !next.is_null() {
                if may_write_memory(next) && !leaves_place_unchanged(next, size, &target_data) {
                    return None;
                }
                next = LLVMGetNextInstruction(next);
            }
            Some(place)
        }
    }

    /// A pointer to element `index` of the aggregate of type `ty` at `ptr`.
    fn element_ptr(
        &self,
        ty: BasicTypeEnum<'ctx>,
        ptr: PointerValue<'ctx>,
        index: u32,
        name: &str,
    ) -> Result<(BasicTypeEnum<'ctx>, PointerValue<'ctx>), BuilderError> {
        match ty {
            BasicTypeEnum::StructType(struct_ty) => {
                let field_ty = struct_ty
                    .get_field_type_at_index(index)
                    .ok_or(BuilderError::ExtractOutOfRange)?;
                Ok((field_ty, self.builder.build_struct_gep(struct_ty, ptr, index, name)?))
            }
            BasicTypeEnum::ArrayType(array_ty) => {
                let i32_ty = self.context.i32_type();
                let indices = [i32_ty.const_zero(), i32_ty.const_int(u64::from(index), false)];
                // SAFETY: callers pass an index below the array length.
                let element = unsafe { self.builder.build_in_bounds_gep(array_ty, ptr, &indices, name)? };
                Ok((array_ty.get_element_type(), element))
            }
            _ => Err(BuilderError::ExtractOutOfRange),
        }
    }

    /// `build_extract_value` that reads the field from the producing
    /// `insertvalue` chain or constant, or loads it from the aggregate's
    /// unchanged place, instead of emitting an instruction.
    pub(crate) fn extract_field<AV: AggregateValue<'ctx>>(
        &self,
        aggregate: AV,
        index: u32,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let raw = aggregate.as_value_ref();
        if let Some(field) = folded_field(raw, index) {
            // SAFETY: a struct field operand is a first-class basic value.
            return Ok(unsafe { BasicValueEnum::new(field) });
        }
        if let Some((ptr, _)) = self.unchanged_place(raw) {
            // SAFETY: `raw` is a live struct or array value.
            let ty = unsafe { BasicValueEnum::new(raw) }.get_type();
            if scalar_struct(ty).is_none() {
                let (field_ty, field_ptr) = self.element_ptr(ty, ptr, index, name)?;
                return self.load_value(field_ty, field_ptr, name);
            }
        }
        self.builder.build_extract_value(aggregate, index, name)
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

    /// `load_value` from `ptr`, a slot that only the instructions just
    /// before this load write. A copy from the slot equals this load's value
    /// at any point it is available: SSA names the latest execution of the
    /// load, and the slot changes only just before the load runs again.
    pub(crate) fn load_stable_value(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        ptr: PointerValue<'ctx>,
        name: &str,
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        if scalar_struct(ty).is_none() {
            self.stable_places.insert(ptr);
        }
        self.load_value(ty, ptr, name)
    }

    /// `build_store` that never stores a first-class aggregate when it can
    /// avoid one: a scalar struct is stored field by field, and any other
    /// aggregate is copied from its place, filled, or stored per element.
    /// An undefined value stores nothing.
    pub(crate) fn store_value<V: BasicValue<'ctx>>(
        &self,
        ptr: PointerValue<'ctx>,
        value: V,
    ) -> Result<(), BuilderError> {
        let value = value.as_basic_value_enum();
        let raw = value.as_value_ref();
        // SAFETY: `raw` is a live value of the module being built.
        if unsafe { is_undefined(raw) } {
            return Ok(());
        }
        let ty = value.get_type();
        if let Some(struct_ty) = scalar_struct(ty) {
            for index in 0..struct_ty.count_fields() {
                let field = self.extract_field(value.into_struct_value(), index, "field")?;
                let field_ptr = self.builder.build_struct_gep(struct_ty, ptr, index, "field.ptr")?;
                self.store_value(field_ptr, field)?;
            }
            return Ok(());
        }
        if !ty.is_struct_type() && !ty.is_array_type() {
            return self.builder.build_store(ptr, value).map(|_| ());
        }
        let target_data = self.target_data();
        let align = target_data.get_abi_alignment(&ty);
        let size = self.context.i64_type().const_int(target_data.get_abi_size(&ty), false);
        // SAFETY: `raw` is a live value of the module being built.
        if unsafe { !LLVMIsAConstant(raw).is_null() } {
            if unsafe { LLVMIsNull(raw) != 0 } {
                return self.builder.build_memset(ptr, align, self.context.i8_type().const_zero(), size).map(|_| ());
            }
            let global = self.module.add_global(ty, None, "aggregate.constant");
            global.set_initializer(&value);
            global.set_constant(true);
            global.set_linkage(Linkage::Private);
            global.set_unnamed_addr(true);
            global.set_alignment(align);
            return self.builder.build_memcpy(ptr, align, global.as_pointer_value(), align, size).map(|_| ());
        }
        if let Some((source, source_align)) = self.unchanged_place(raw) {
            // Places of one type are equal or disjoint, so `memcpy` is
            // exact; a self-assignment copies nothing.
            if source != ptr {
                self.builder.build_memcpy(ptr, align, source, source_align.max(1), size)?;
            }
            return Ok(());
        }
        let count = match ty {
            BasicTypeEnum::StructType(struct_ty) => struct_ty.count_fields(),
            BasicTypeEnum::ArrayType(array_ty) => array_ty.len(),
            _ => 0,
        };
        let fields: Option<Vec<LLVMValueRef>> = (0..count).map(|index| folded_field(raw, index)).collect();
        match fields {
            // SAFETY: `raw` is a live value of the module being built.
            Some(fields) if unsafe { !LLVMIsAInsertValueInst(raw).is_null() } => {
                for (index, field) in fields.into_iter().enumerate() {
                    let (_, field_ptr) = self.element_ptr(ty, ptr, index as u32, "field.ptr")?;
                    // SAFETY: an aggregate element is a first-class basic value.
                    self.store_value(field_ptr, unsafe { BasicValueEnum::new(field) })?;
                }
                Ok(())
            }
            _ => self.builder.build_store(ptr, value).map(|_| ()),
        }
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
            Operand::Value(value) => self.store_value(dest, value),
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
