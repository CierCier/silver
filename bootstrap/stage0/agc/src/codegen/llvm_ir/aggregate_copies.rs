//! Lowers large first-class aggregate stores to memory intrinsics.
//!
//! Codegen moves struct, array, and enum values as SSA aggregates. LLVM
//! scalarizes an aggregate `store` (and a `store` of a `load`) element by
//! element: instcombine unpacks it at -O2 and SelectionDAG expands it at -O0,
//! where it also defeats FastISel. A 40 KB zero-initialized local became about
//! a thousand stores in one block, which made instruction selection and
//! scheduling superlinear. `memset`/`memmove` keep the copy one operation
//! that the backend lowers well at every size.

use llvm_sys::core::*;
use llvm_sys::debuginfo::{LLVMInstructionGetDebugLoc, LLVMInstructionSetDebugLoc};
use llvm_sys::prelude::{LLVMBuilderRef, LLVMValueRef};
use llvm_sys::target::{LLVMABISizeOfType, LLVMTargetDataRef};
use llvm_sys::{LLVMAtomicOrdering, LLVMOpcode, LLVMTypeKind, LLVMUnnamedAddr};

use inkwell::module::Module;
use inkwell::targets::TargetData;

/// Aggregates at least this large are copied with memory intrinsics. Smaller
/// ones stay SSA values, which SROA and the backend handle well.
const MIN_LOWERED_BYTES: u64 = 32;

/// Alignment claimed by the intrinsics. This pass was written while enum
/// payloads sat at offset 2 under their payload type's alignment claim, where
/// aligned vector moves faulted. Payloads are now naturally aligned; the pass
/// keeps align 1 until IR generation stops emitting aggregate stores and the
/// pass is removed. Unaligned moves cost the same on aligned x86-64 data.
const COPY_ALIGN: u32 = 1;

enum Copy {
    /// `store <zero aggregate>, dst`
    Zero { store: LLVMValueRef, size: u64 },
    /// `store <undef/poison aggregate>, dst` stores nothing defined.
    Undefined { store: LLVMValueRef },
    /// `store <other constant aggregate>, dst`
    Constant { store: LLVMValueRef, size: u64 },
    /// `store (load src), dst` with no write in between.
    Move {
        store: LLVMValueRef,
        load: LLVMValueRef,
        size: u64,
    },
}

pub(crate) fn lower_aggregate_copies(module: &Module<'_>, target_data: &TargetData) {
    let copies = find_copies(module, target_data.as_mut_ptr());
    if copies.is_empty() {
        return;
    }
    unsafe {
        let builder = LLVMCreateBuilderInContext(LLVMGetModuleContext(module.as_mut_ptr()));
        let i8_type = LLVMInt8TypeInContext(LLVMGetModuleContext(module.as_mut_ptr()));
        let i64_type = LLVMInt64TypeInContext(LLVMGetModuleContext(module.as_mut_ptr()));
        for (ordinal, copy) in copies.into_iter().enumerate() {
            match copy {
                Copy::Zero { store, size } => {
                    position_at(builder, store);
                    let length = LLVMConstInt(i64_type, size, 0);
                    let call = LLVMBuildMemSet(
                        builder,
                        LLVMGetOperand(store, 1),
                        LLVMConstInt(i8_type, 0, 0),
                        length,
                        COPY_ALIGN,
                    );
                    keep_location(call, store);
                    LLVMInstructionEraseFromParent(store);
                }
                Copy::Undefined { store } => LLVMInstructionEraseFromParent(store),
                Copy::Constant { store, size } => {
                    let value = LLVMGetOperand(store, 0);
                    let name = format!("__silver_aggregate_constant.{ordinal}\0");
                    let global =
                        LLVMAddGlobal(module.as_mut_ptr(), LLVMTypeOf(value), name.as_ptr().cast());
                    LLVMSetInitializer(global, value);
                    LLVMSetGlobalConstant(global, 1);
                    LLVMSetLinkage(global, llvm_sys::LLVMLinkage::LLVMPrivateLinkage);
                    LLVMSetUnnamedAddress(global, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
                    position_at(builder, store);
                    let call = LLVMBuildMemCpy(
                        builder,
                        LLVMGetOperand(store, 1),
                        COPY_ALIGN,
                        global,
                        COPY_ALIGN,
                        LLVMConstInt(i64_type, size, 0),
                    );
                    keep_location(call, store);
                    LLVMInstructionEraseFromParent(store);
                }
                Copy::Move { store, load, size } => {
                    position_at(builder, store);
                    // memmove: a self-assignment copies a place onto itself.
                    let call = LLVMBuildMemMove(
                        builder,
                        LLVMGetOperand(store, 1),
                        COPY_ALIGN,
                        LLVMGetOperand(load, 0),
                        COPY_ALIGN,
                        LLVMConstInt(i64_type, size, 0),
                    );
                    keep_location(call, store);
                    LLVMInstructionEraseFromParent(store);
                    LLVMInstructionEraseFromParent(load);
                }
            }
        }
        LLVMDisposeBuilder(builder);
    }
}

fn find_copies(module: &Module<'_>, target_data: LLVMTargetDataRef) -> Vec<Copy> {
    let mut copies = Vec::new();
    unsafe {
        let mut function = LLVMGetFirstFunction(module.as_mut_ptr());
        while !function.is_null() {
            let mut block = LLVMGetFirstBasicBlock(function);
            while !block.is_null() {
                let mut instruction = LLVMGetFirstInstruction(block);
                while !instruction.is_null() {
                    if let Some(copy) = classify_store(instruction, target_data) {
                        copies.push(copy);
                    }
                    instruction = LLVMGetNextInstruction(instruction);
                }
                block = LLVMGetNextBasicBlock(block);
            }
            function = LLVMGetNextFunction(function);
        }
    }
    copies
}

/// # Safety
/// `store` must be a live instruction of a module the caller owns.
unsafe fn classify_store(store: LLVMValueRef, target_data: LLVMTargetDataRef) -> Option<Copy> {
    unsafe {
        if LLVMGetInstructionOpcode(store) != LLVMOpcode::LLVMStore
            || LLVMGetVolatile(store) != 0
            || LLVMGetOrdering(store) != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        {
            return None;
        }
        let value = LLVMGetOperand(store, 0);
        let ty = LLVMTypeOf(value);
        if !matches!(
            LLVMGetTypeKind(ty),
            LLVMTypeKind::LLVMStructTypeKind | LLVMTypeKind::LLVMArrayTypeKind
        ) {
            return None;
        }
        let size = LLVMABISizeOfType(target_data, ty);
        if size < MIN_LOWERED_BYTES {
            return None;
        }
        if !LLVMIsAConstant(value).is_null() {
            return Some(if LLVMIsUndef(value) != 0 || LLVMIsPoison(value) != 0 {
                Copy::Undefined { store }
            } else if LLVMIsNull(value) != 0 {
                Copy::Zero { store, size }
            } else {
                Copy::Constant { store, size }
            });
        }
        let load = value;
        if LLVMIsALoadInst(load).is_null()
            || LLVMGetVolatile(load) != 0
            || LLVMGetOrdering(load) != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
            || LLVMGetInstructionParent(load) != LLVMGetInstructionParent(store)
            || !has_single_use(load)
        {
            return None;
        }
        // The copy reads the source at the store, so nothing in between may write.
        let mut between = LLVMGetNextInstruction(load);
        while between != store {
            if between.is_null() || may_write_memory(between) {
                return None;
            }
            between = LLVMGetNextInstruction(between);
        }
        Some(Copy::Move { store, load, size })
    }
}

/// # Safety
/// `value` must be a live value.
unsafe fn has_single_use(value: LLVMValueRef) -> bool {
    unsafe {
        let first = LLVMGetFirstUse(value);
        !first.is_null() && LLVMGetNextUse(first).is_null()
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

/// # Safety
/// `builder` and `instruction` must belong to the same live context.
unsafe fn position_at(builder: LLVMBuilderRef, instruction: LLVMValueRef) {
    unsafe { LLVMPositionBuilderBefore(builder, instruction) }
}

/// # Safety
/// `call` and `original` must be live instructions of one module.
unsafe fn keep_location(call: LLVMValueRef, original: LLVMValueRef) {
    unsafe { LLVMInstructionSetDebugLoc(call, LLVMInstructionGetDebugLoc(original)) }
}
