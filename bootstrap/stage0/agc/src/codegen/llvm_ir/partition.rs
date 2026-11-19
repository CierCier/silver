//! Parallel machine-code emission: one module becomes several objects.
//!
//! LLVM's machine-code pipeline works one function at a time, so a module's
//! functions can be emitted into separate objects concurrently. The split
//! happens after the module-wide IR passes, so the optimizer (and its
//! inliner) always sees the whole module. The module is written to bitcode
//! once; each worker parses its own copy into its own context (an LLVM
//! context is single-threaded), turns every function outside its partition
//! into a declaration, and emits one object.
//!
//! Partition 0 keeps every global variable; the others see leaf constants
//! as available_externally so codegen can still fold their contents. A local
//! function or global referenced from another partition is promoted to a hidden global with
//! [`PROMOTED_SUFFIX`], so it resolves at link time without clashing with
//! another object's symbols. The partition count and assignment depend only on
//! the module, never on the thread count, so the objects are reproducible.

use std::ffi::CString;

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::FileType;
use llvm_sys::comdat::{LLVMGetComdat, LLVMSetComdat};
use llvm_sys::core::*;
use llvm_sys::prelude::{LLVMComdatRef, LLVMModuleRef, LLVMValueRef};
use llvm_sys::target::{LLVMGetModuleDataLayout, LLVMPreferredAlignmentOfGlobal};
use llvm_sys::{LLVMAttributeFunctionIndex, LLVMLinkage, LLVMVisibility};
use rayon::prelude::*;
use rustc_hash::FxHashMap as HashMap;

use crate::codegen::{CodegenError, CodegenResult};

/// Partitions per split module. Each partition parses the whole bitcode
/// (12-40 ms for http_server), so beyond four the parse outweighs the emit
/// time saved; four also keeps quad-core machines fully used.
const PARTS: usize = 4;

/// Smallest module, in weighted instructions, worth splitting. Below it the
/// bitcode round trip costs about what the parallel emit saves (hello.ag at
/// -O0: 11K, 40 ms serial vs 33 ms split).
const SPLIT_THRESHOLD: usize = 16_000;

/// Optimized code costs several times as much to emit per instruction
/// (http_server: 6 us at -O0, 18 us at -O2).
const OPTIMIZED_WEIGHT: usize = 4;

/// Suffix of locals promoted to hidden globals.
const PROMOTED_SUFFIX: &str = ".silver_part";

/// The name a symbol had before promotion, which debug info still uses.
pub(crate) fn source_name(symbol: &str) -> &str {
    symbol.strip_suffix(PROMOTED_SUFFIX).unwrap_or(symbol)
}

/// How a module splits: every defined function's owning partition, keyed by
/// symbol name so it survives the bitcode round trip.
pub(crate) struct SplitPlan {
    owners: HashMap<Vec<u8>, usize>,
}

impl SplitPlan {
    /// Plans the split of `module` and promotes the locals partitions share,
    /// or returns `None` when the module is too small or has aliases/ifuncs
    /// (which would have to follow their aliasee into one partition).
    pub(crate) fn prepare(module: &Module<'_>, level: OptimizationLevel) -> Option<Self> {
        let module = module.as_mut_ptr();
        unsafe {
            if !LLVMGetFirstGlobalAlias(module).is_null()
                || !LLVMGetFirstGlobalIFunc(module).is_null()
            {
                return None;
            }
        }
        let defined: Vec<(LLVMValueRef, usize)> = functions(module)
            .into_iter()
            .filter(|&function| is_definition(function))
            .map(|function| (function, instruction_count(function)))
            .collect();
        let weight_factor = if level == OptimizationLevel::None {
            1
        } else {
            OPTIMIZED_WEIGHT
        };
        let total: usize = defined.iter().map(|&(_, count)| count).sum();
        if total * weight_factor < SPLIT_THRESHOLD {
            return None;
        }

        let owner = assign(module, &defined);
        name_unnamed_functions(&defined);
        promote_shared_locals(module, &owner);
        let owners = defined
            .iter()
            .map(|&(function, _)| (name(function).to_vec(), owner[&function]))
            .collect();
        Some(Self { owners })
    }

    /// Emits one object per partition, in partition order.
    pub(crate) fn emit(
        &self,
        module: &Module<'_>,
        triple: &str,
        level: OptimizationLevel,
    ) -> CodegenResult<Vec<Vec<u8>>> {
        preserve_use_list_order();
        let bitcode = module.write_bitcode_to_memory();
        // Includes LLVM's trailing NUL, which create_from_memory_range needs.
        let bitcode = bitcode.as_slice();
        (0..PARTS)
            .into_par_iter()
            .map(|part| self.emit_part(bitcode, part, triple, level))
            .collect::<Result<Vec<_>, String>>()
            .map_err(CodegenError::new)
    }

    fn emit_part(
        &self,
        bitcode: &[u8],
        part: usize,
        triple: &str,
        level: OptimizationLevel,
    ) -> Result<Vec<u8>, String> {
        let context = Context::create();
        let buffer = MemoryBuffer::create_from_memory_range(bitcode, "partition");
        let module = Module::parse_bitcode_from_buffer(&buffer, &context)
            .map_err(|e| format!("failed to reload bitcode for partition {part}: {e}"))?;
        self.keep_partition(module.as_mut_ptr(), part)?;
        let machine = super::entry::create_target_machine(triple, level).map_err(|e| e.message)?;
        let object = machine
            .write_to_memory_buffer(&module, FileType::Object)
            .map_err(|e| format!("failed to emit partition {part}: {e}"))?;
        Ok(super::entry::object_bytes(&object))
    }

    /// Strips `module` (a reloaded copy) down to the definitions `part` owns.
    fn keep_partition(&self, module: LLVMModuleRef, part: usize) -> Result<(), String> {
        for function in functions(module) {
            if !is_definition(function) {
                continue;
            }
            let owner = self.owners.get(name(function)).ok_or_else(|| {
                format!(
                    "function `{}` is missing from the partition plan",
                    String::from_utf8_lossy(name(function))
                )
            })?;
            if *owner != part {
                unsafe { into_declaration(module, function) };
            }
        }
        if part == 0 {
            return Ok(());
        }
        unsafe {
            LLVMSetModuleInlineAsm2(module, c"".as_ptr(), 0);
            let layout = LLVMGetModuleDataLayout(module);
            for global in globals(module) {
                if !is_definition(global) {
                    continue;
                }
                // llvm.used and friends have no declaration form; partition 0
                // carries them.
                if LLVMGetLinkage(global) == LLVMLinkage::LLVMAppendingLinkage {
                    LLVMDeleteGlobal(global);
                    continue;
                }
                // Code generation assumes a strong definition in its own
                // module gets the preferred alignment (and emits it with that
                // alignment), but only ABI alignment for a declaration. Keep
                // the definition's alignment so loads from it stay aligned
                // (movaps rather than movups).
                let linkage = LLVMGetLinkage(global);
                if LLVMGetAlignment(global) == 0 && is_strong_definition(global) {
                    LLVMSetAlignment(global, LLVMPreferredAlignmentOfGlobal(layout, global));
                }
                // A constant whose every copy is identical stays readable:
                // codegen folds loads and memcpys from it into immediates.
                // available_externally keeps the initializer but emits nothing.
                // Only leaf data qualifies: a folded pointer could name a
                // local that was not promoted because no partition used it.
                let foldable = LLVMIsGlobalConstant(global) != 0
                    && (is_strong_definition(global)
                        || matches!(
                            linkage,
                            LLVMLinkage::LLVMLinkOnceODRLinkage | LLVMLinkage::LLVMWeakODRLinkage
                        ))
                    && !references_globals(LLVMGetInitializer(global));
                if foldable {
                    LLVMSetLinkage(global, LLVMLinkage::LLVMAvailableExternallyLinkage);
                } else {
                    LLVMSetInitializer(global, std::ptr::null_mut());
                    LLVMSetLinkage(global, LLVMLinkage::LLVMExternalLinkage);
                }
                LLVMSetComdat(global, std::ptr::null_mut());
                LLVMGlobalClearMetadata(global);
            }
        }
        Ok(())
    }
}

/// Makes the bitcode writer record use-list order, which the reader then
/// restores. Instruction selection and scheduling walk use lists, so without
/// it reloaded functions get different (equally valid) register choices than
/// the serial emit. The LLVM-C writer has no parameter for this, only the
/// process-wide option; agc parses no other LLVM options.
fn preserve_use_list_order() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| unsafe {
        let args = [c"agc".as_ptr(), c"-preserve-bc-uselistorder".as_ptr()];
        llvm_sys::support::LLVMParseCommandLineOptions(2, args.as_ptr(), c"".as_ptr());
    });
}

/// One unit of assignment: a function, or every function of one comdat.
struct Group {
    weight: usize,
    members: Vec<LLVMValueRef>,
    /// Shares a comdat with a global variable, so it stays in partition 0.
    pinned: bool,
}

/// Assigns functions to partitions, heaviest group first to the least-loaded
/// partition. Comdat members must share an object.
fn assign(
    module: LLVMModuleRef,
    defined: &[(LLVMValueRef, usize)],
) -> HashMap<LLVMValueRef, usize> {
    let global_comdats: Vec<LLVMComdatRef> = globals(module)
        .into_iter()
        .map(|global| unsafe { LLVMGetComdat(global) })
        .filter(|comdat| !comdat.is_null())
        .collect();
    let mut groups: Vec<Group> = Vec::new();
    let mut group_of_comdat: HashMap<LLVMComdatRef, usize> = HashMap::default();
    for &(function, count) in defined {
        let comdat = unsafe { LLVMGetComdat(function) };
        let index = if comdat.is_null() {
            None
        } else {
            group_of_comdat.get(&comdat).copied()
        };
        let index = index.unwrap_or_else(|| {
            groups.push(Group {
                weight: 0,
                members: Vec::new(),
                pinned: global_comdats.contains(&comdat),
            });
            if !comdat.is_null() {
                group_of_comdat.insert(comdat, groups.len() - 1);
            }
            groups.len() - 1
        });
        // A function costs something beyond its instructions (prologue,
        // symbol, debug entry), so empty ones still spread out.
        groups[index].weight += count + 8;
        groups[index].members.push(function);
    }

    // Stable sort: equal weights keep module order, so the plan is
    // reproducible.
    groups.sort_by_key(|group| (!group.pinned, std::cmp::Reverse(group.weight)));
    let mut load = [0usize; PARTS];
    let mut owner = HashMap::default();
    for group in groups {
        let part = if group.pinned {
            0
        } else {
            (0..PARTS).min_by_key(|&part| load[part]).unwrap_or(0)
        };
        load[part] += group.weight;
        for function in group.members {
            owner.insert(function, part);
        }
    }
    owner
}

/// Plan keys are names, so every definition needs one. Unnamed definitions
/// are always local, so naming them changes no exported symbol.
fn name_unnamed_functions(defined: &[(LLVMValueRef, usize)]) {
    for (index, &(function, _)) in defined.iter().enumerate() {
        if name(function).is_empty() {
            set_name(function, &format!("__silver_part_fn{index}"));
        }
    }
}

/// Promotes each local referenced from a partition other than its own to a
/// hidden global with a reserved name. Global variables live in partition 0.
fn promote_shared_locals(module: LLVMModuleRef, owner: &HashMap<LLVMValueRef, usize>) {
    let mut constant_users = HashMap::default();
    let candidates: Vec<LLVMValueRef> = functions(module)
        .into_iter()
        .chain(globals(module))
        .filter(|&value| is_definition(value) && is_local(value))
        .collect();
    for (index, value) in candidates.into_iter().enumerate() {
        let home = owner.get(&value).copied().unwrap_or(0);
        if user_parts(value, owner, &mut constant_users) & !(1 << home) == 0 {
            continue;
        }
        let promoted = match std::str::from_utf8(name(value)) {
            Ok(name) if !name.is_empty() => format!("{name}{PROMOTED_SUFFIX}"),
            _ => format!("__silver_part_local{index}{PROMOTED_SUFFIX}"),
        };
        set_name(value, &promoted);
        unsafe {
            LLVMSetLinkage(value, LLVMLinkage::LLVMExternalLinkage);
            LLVMSetVisibility(value, LLVMVisibility::LLVMHiddenVisibility);
        }
    }
}

/// Bit set of the partitions whose code or data refers to `value`.
fn user_parts(
    value: LLVMValueRef,
    owner: &HashMap<LLVMValueRef, usize>,
    constant_users: &mut HashMap<LLVMValueRef, u64>,
) -> u64 {
    let part_of = |function: LLVMValueRef| 1u64 << owner.get(&function).copied().unwrap_or(0);
    let mut parts = 0;
    let mut usage = unsafe { LLVMGetFirstUse(value) };
    while !usage.is_null() {
        let user = unsafe { LLVMGetUser(usage) };
        parts |= unsafe {
            if !LLVMIsAInstruction(user).is_null() {
                part_of(LLVMGetBasicBlockParent(LLVMGetInstructionParent(user)))
            } else if !LLVMIsAFunction(user).is_null() {
                // Personality or prefix data.
                part_of(user)
            } else if !LLVMIsAGlobalValue(user).is_null() {
                // A global variable's initializer: partition 0.
                1
            } else if let Some(&cached) = constant_users.get(&user) {
                cached
            } else {
                // A constant expression or aggregate: whoever uses it.
                let parts = user_parts(user, owner, constant_users);
                constant_users.insert(user, parts);
                parts
            }
        };
        usage = unsafe { LLVMGetNextUse(usage) };
    }
    parts
}

/// Replaces `function`'s definition with a declaration carrying the same
/// name, type, calling convention, visibility, and attributes (call lowering
/// reads ABI attributes from the callee).
///
/// # Safety
///
/// `function` must be a function defined in `module`; it is deleted.
unsafe fn into_declaration(module: LLVMModuleRef, function: LLVMValueRef) {
    unsafe {
        let symbol = CString::new(name(function)).expect("LLVM names contain no NUL");
        LLVMSetValueName2(function, c"".as_ptr(), 0);
        let declaration =
            LLVMAddFunction(module, symbol.as_ptr(), LLVMGlobalGetValueType(function));
        LLVMSetFunctionCallConv(declaration, LLVMGetFunctionCallConv(function));
        LLVMSetVisibility(declaration, LLVMGetVisibility(function));
        // Index 0 is the return value, 1..=params the parameters.
        let params = LLVMCountParams(function);
        for index in std::iter::once(LLVMAttributeFunctionIndex).chain(0..=params) {
            let count = LLVMGetAttributeCountAtIndex(function, index) as usize;
            let mut attributes = vec![std::ptr::null_mut(); count];
            LLVMGetAttributesAtIndex(function, index, attributes.as_mut_ptr());
            for attribute in attributes {
                LLVMAddAttributeAtIndex(declaration, index, attribute);
            }
        }
        LLVMReplaceAllUsesWith(function, declaration);
        // Deleting a function drops its body's references first.
        LLVMDeleteFunction(function);
    }
}

fn functions(module: LLVMModuleRef) -> Vec<LLVMValueRef> {
    let mut out = Vec::new();
    let mut function = unsafe { LLVMGetFirstFunction(module) };
    while !function.is_null() {
        out.push(function);
        function = unsafe { LLVMGetNextFunction(function) };
    }
    out
}

fn globals(module: LLVMModuleRef) -> Vec<LLVMValueRef> {
    let mut out = Vec::new();
    let mut global = unsafe { LLVMGetFirstGlobal(module) };
    while !global.is_null() {
        out.push(global);
        global = unsafe { LLVMGetNextGlobal(global) };
    }
    out
}

fn is_definition(value: LLVMValueRef) -> bool {
    unsafe { LLVMIsDeclaration(value) == 0 }
}

fn is_local(value: LLVMValueRef) -> bool {
    matches!(
        unsafe { LLVMGetLinkage(value) },
        LLVMLinkage::LLVMInternalLinkage | LLVMLinkage::LLVMPrivateLinkage
    )
}

/// Whether `constant` refers to any global value, directly or through
/// nested constants.
fn references_globals(constant: LLVMValueRef) -> bool {
    unsafe {
        if !LLVMIsAGlobalValue(constant).is_null() {
            return true;
        }
        // Constant data (strings, numeric arrays) has no operands.
        (0..LLVMGetNumOperands(constant).max(0) as u32)
            .any(|index| references_globals(LLVMGetOperand(constant, index)))
    }
}

/// A definition the linker never replaces (not weak, linkonce, or common).
fn is_strong_definition(value: LLVMValueRef) -> bool {
    matches!(
        unsafe { LLVMGetLinkage(value) },
        LLVMLinkage::LLVMExternalLinkage
            | LLVMLinkage::LLVMInternalLinkage
            | LLVMLinkage::LLVMPrivateLinkage
    )
}

fn instruction_count(function: LLVMValueRef) -> usize {
    let mut count = 0;
    let mut block = unsafe { LLVMGetFirstBasicBlock(function) };
    while !block.is_null() {
        let mut instruction = unsafe { LLVMGetFirstInstruction(block) };
        while !instruction.is_null() {
            count += 1;
            instruction = unsafe { LLVMGetNextInstruction(instruction) };
        }
        block = unsafe { LLVMGetNextBasicBlock(block) };
    }
    count
}

/// The value's name. The slice is valid until the value is renamed or
/// deleted.
fn name<'a>(value: LLVMValueRef) -> &'a [u8] {
    let mut len = 0;
    let ptr = unsafe { LLVMGetValueName2(value, &mut len) };
    if ptr.is_null() {
        return &[];
    }
    unsafe { std::slice::from_raw_parts(ptr.cast(), len) }
}

fn set_name(value: LLVMValueRef, name: &str) {
    unsafe { LLVMSetValueName2(value, name.as_ptr().cast(), name.len()) };
}
