//! Scope/drop-flag codegen.
//! Runtime flags remain the correctness fallback for conditional and partial moves.
//! The semantic ownership passes provide the future static drop-elision boundary.
//! Why separate this here: codegen should emit drops, not rediscover ownership.
use rustc_hash::FxHashMap as HashMap;

use inkwell::AddressSpace;
use inkwell::debug_info::{AsDIScope, DIFlags, DIFlagsConstants, DIType};
use inkwell::module::Linkage;
use inkwell::types::{AnyType, BasicType};
use inkwell::values::{AsValueRef, BasicMetadataValueEnum, PointerValue};

use crate::codegen::SilverGenerator;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::llvm_ir::{DeferAction, DeferredEntry, FunctionSig, VarInfo};
use crate::codegen::{CodegenError, CodegenResult};
use crate::lexer::Span;
use crate::parser::ast;
use crate::symbol_table::{CompilerPhase, SymbolKind};
use crate::types::Type;
use inkwell::IntPredicate;
use inkwell::targets::TargetData;

impl<'ctx> LlvmIrGenerator<'ctx> {
    pub(crate) fn set_debug_location(&self, span: &Span) {
        // Lazily-emitted generic instances have no subprogram; a DILocation
        // scoped to the compile unit is invalid (LLVM rejects it and crashes
        // DWARF emission). Leave their instructions without locations.
        if self.debug_nested {
            return;
        }
        if let Some(debug) = &self.debug {
            let (line, col, _, _) = debug.span_to_line_col(span);
            let loc = debug.create_debug_location(self.context, line, col);
            self.builder.set_current_debug_location(loc);
        }
    }

    /// Cache key for a Silver type in the DWARF type map.
    fn debug_type_key(ty: &ast::Type) -> String {
        format!("{:?}", ty.kind)
    }

    /// Resolves a Silver type to a DWARF type (basic, pointer, array, struct),
    /// populating the per-module type cache. Returns `None` for types without
    /// a meaningful DWARF mapping (complex, function, tuple, unknown named).
    pub(crate) fn debug_type_for(&mut self, ty: &ast::Type) -> Option<DIType<'ctx>> {
        self.debug.as_ref()?;
        let key = Self::debug_type_key(ty);
        if let Some(cached) = self
            .debug
            .as_ref()
            .and_then(|d| d.di_types.get(&key))
            .copied()
        {
            return Some(cached);
        }
        let di = match ty.kind.as_ref() {
            ast::TypeKind::Primitive(ast::PrimitiveType::Str) => {
                let inner = self.debug.as_mut()?.byte_type();
                self.debug
                    .as_ref()?
                    .dibuilder
                    .create_pointer_type("str", inner, 64, 64, AddressSpace::default())
                    .as_type()
            }
            ast::TypeKind::Primitive(p) => {
                let (name, bits, enc) = match p {
                    ast::PrimitiveType::I8 => ("i8", 8, crate::debug_info::ate::SIGNED_CHAR),
                    ast::PrimitiveType::I16 => ("i16", 16, crate::debug_info::ate::SIGNED),
                    ast::PrimitiveType::I32 => ("i32", 32, crate::debug_info::ate::SIGNED),
                    ast::PrimitiveType::I64 => ("i64", 64, crate::debug_info::ate::SIGNED),
                    ast::PrimitiveType::I128 => ("i128", 128, crate::debug_info::ate::SIGNED),
                    ast::PrimitiveType::U8 => ("u8", 8, crate::debug_info::ate::UNSIGNED_CHAR),
                    ast::PrimitiveType::U16 => ("u16", 16, crate::debug_info::ate::UNSIGNED),
                    ast::PrimitiveType::U32 => ("u32", 32, crate::debug_info::ate::UNSIGNED),
                    ast::PrimitiveType::U64 => ("u64", 64, crate::debug_info::ate::UNSIGNED),
                    ast::PrimitiveType::U128 => ("u128", 128, crate::debug_info::ate::UNSIGNED),
                    ast::PrimitiveType::F32 => ("f32", 32, crate::debug_info::ate::FLOAT),
                    ast::PrimitiveType::F64 => ("f64", 64, crate::debug_info::ate::FLOAT),
                    ast::PrimitiveType::Bool => ("bool", 1, crate::debug_info::ate::BOOLEAN),
                    ast::PrimitiveType::Char => ("char", 32, crate::debug_info::ate::UNSIGNED_CHAR),
                    ast::PrimitiveType::Void
                    | ast::PrimitiveType::Str
                    | ast::PrimitiveType::F80
                    | ast::PrimitiveType::C32
                    | ast::PrimitiveType::C64
                    | ast::PrimitiveType::C80 => return None,
                };
                self.debug
                    .as_mut()?
                    .create_basic_type(name, bits, enc)
                    .ok()?
                    .as_type()
            }
            ast::TypeKind::Pointer(p) => {
                let inner = self.debug_type_for(&p.inner).unwrap_or_else(|| {
                    self.debug
                        .as_mut()
                        .map(|d| d.byte_type())
                        .expect("debug present")
                });
                self.debug
                    .as_ref()?
                    .dibuilder
                    .create_pointer_type("ptr", inner, 64, 64, AddressSpace::default())
                    .as_type()
            }
            ast::TypeKind::Reference(p) => {
                let inner = self.debug_type_for(&p.inner).unwrap_or_else(|| {
                    self.debug
                        .as_mut()
                        .map(|d| d.byte_type())
                        .expect("debug present")
                });
                self.debug
                    .as_ref()?
                    .dibuilder
                    .create_pointer_type("ptr", inner, 64, 64, AddressSpace::default())
                    .as_type()
            }
            ast::TypeKind::Array(a) => {
                let element = self.debug_type_for(&a.element_type)?;
                let llvm_ty = self.lower_basic_type(ty).ok()?;
                let target_data =
                    TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
                let size = target_data.get_abi_size(&llvm_ty.as_any_type_enum()) * 8;
                let align = target_data.get_abi_alignment(&llvm_ty.as_any_type_enum()) * 8;
                self.debug
                    .as_ref()?
                    .dibuilder
                    .create_array_type(element, size, align, std::slice::from_ref(&(0..a.size)))
                    .as_type()
            }
            ast::TypeKind::Named(named) => {
                let named_key = Self::named_type_key(named);
                let Some(fields) = self.struct_fields.get(&named_key).cloned() else {
                    // Enums and unknown named types stay opaque (no DWARF map).
                    return None;
                };
                if self.debug.as_ref()?.building.contains(&named_key) {
                    // Recursive struct (self-referential via pointer): break the
                    // cycle — the pointer member falls back to the byte type.
                    return None;
                }
                self.debug.as_mut()?.building.insert(named_key.clone());
                let scope = self.debug.as_ref()?.compile_unit.as_debug_info_scope();
                let file = self.debug.as_ref()?.main_file();
                let mut member_types = Vec::with_capacity(fields.len());
                let mut offset_bits = 0u64;
                for (field_name, field_ty) in &fields {
                    let Some(fdi) = self.debug_type_for(field_ty) else {
                        continue;
                    };
                    let (size, align) = self.debug_type_layout(field_ty).unwrap_or((0, 8));
                    offset_bits = offset_bits.div_ceil(align) * align;
                    let member = self.debug.as_ref()?.dibuilder.create_member_type(
                        scope,
                        field_name,
                        file,
                        0,
                        size,
                        align as u32,
                        offset_bits,
                        DIFlags::PUBLIC,
                        fdi,
                    );
                    member_types.push(member.as_type());
                    offset_bits += size;
                }
                let (size_bits, align_bits) = self.debug_type_layout(ty).unwrap_or((0, 8));
                let struct_name = Self::named_type_name(named);
                let struct_ty = self.debug.as_ref()?.dibuilder.create_struct_type(
                    scope,
                    &struct_name,
                    file,
                    0,
                    size_bits,
                    align_bits as u32,
                    DIFlags::PUBLIC,
                    None,
                    &member_types,
                    0,
                    None,
                    "struct",
                );
                self.debug
                    .as_mut()?
                    .di_types
                    .insert(key.clone(), struct_ty.as_type());
                self.debug.as_mut()?.building.remove(&named_key);
                struct_ty.as_type()
            }
            _ => return None,
        };
        self.debug.as_mut()?.di_types.insert(key, di);
        Some(di)
    }

    /// (size_bits, align_bits) for a type via the target data layout.
    fn debug_type_layout(&mut self, ty: &ast::Type) -> Option<(u64, u64)> {
        let llvm_ty = self.lower_basic_type(ty).ok()?;
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        Some((
            target_data.get_abi_size(&llvm_ty.as_any_type_enum()) * 8,
            target_data.get_abi_alignment(&llvm_ty.as_any_type_enum()) as u64 * 8,
        ))
    }

    /// Emits a DWARF local/parameter variable for `alloca` so debuggers can
    /// `print` it and `info locals` lists it. No-op when `-g` is off or the
    /// type has no DWARF mapping.
    pub(crate) fn emit_debug_variable(
        &mut self,
        name: &str,
        ty: &ast::Type,
        span: &Span,
        alloca: PointerValue<'ctx>,
        arg_no: Option<u32>,
    ) -> CodegenResult<()> {
        if self.debug.is_none() {
            return Ok(());
        }
        // Skip variables inside lazily-emitted generic instances: they carry
        // no subprogram, and LLVM 22 crashes building their DIE chain.
        if self.debug_nested {
            return Ok(());
        }
        let Some(di_ty) = self.debug_type_for(ty) else {
            return Ok(());
        };
        if di_ty.get_size_in_bits() == 0 {
            return Ok(());
        }
        let (line, col, _, _) = self
            .debug
            .as_mut()
            .expect("debug present")
            .span_to_line_col(span);
        let file = self.debug.as_mut().expect("debug present").file_for(span);
        let debug = self.debug.as_ref().expect("debug present");
        let scope = debug.current_scope();
        // The compile unit is not a DILocalScope; LLVM's createAutoVariable
        // crashes (DILocalScope::getSubprogram) if handed it. Skip instead.
        if scope == debug.compile_unit.as_debug_info_scope() {
            return Ok(());
        }
        if std::env::var("BT_DEBUG").is_ok() {
            let kind = unsafe { llvm_sys::debuginfo::LLVMGetMetadataKind(scope.as_mut_ptr()) };
            let fn_name = self
                .current_fn
                .map(|f| f.get_name().to_string_lossy().into_owned())
                .unwrap_or_default();
            eprintln!(
                "DBGVAR {name} fn={fn_name} kind={kind:?} sub={} blocks={}",
                self.debug
                    .as_ref()
                    .map(|d| d.current_subprogram.is_some())
                    .unwrap_or(false),
                self.debug
                    .as_ref()
                    .map(|d| d.current_lexical_blocks.len())
                    .unwrap_or(0),
            );
        }
        let variable = match arg_no {
            Some(no) => debug.dibuilder.create_parameter_variable(
                scope,
                name,
                no,
                file,
                line,
                di_ty,
                true,
                DIFlags::PUBLIC,
            ),
            None => debug.dibuilder.create_auto_variable(
                scope,
                name,
                file,
                line,
                di_ty,
                true,
                DIFlags::PUBLIC,
                64,
            ),
        };
        let loc = debug.create_debug_location(self.context, line, col);
        let Some(block) = self.builder.get_insert_block() else {
            return Ok(());
        };
        // inkwell's `insert_declare_at_end` is broken under LLVM 19+ (it casts
        // the returned DbgRecord to a Value and asserts), so emit the debug
        // record directly through the C API. The record is attached to the
        // block as a side effect; the returned handle is discarded.
        let expr = debug.dibuilder.create_expression(vec![]);
        unsafe {
            llvm_sys::debuginfo::LLVMDIBuilderInsertDeclareRecordAtEnd(
                debug.dibuilder.as_mut_ptr(),
                alloca.as_value_ref(),
                variable.as_mut_ptr(),
                expr.as_mut_ptr(),
                loc.as_mut_ptr(),
                block.as_mut_ptr(),
            );
        }
        Ok(())
    }

    pub(crate) fn push_scope(&mut self) {
        self.variables.push(HashMap::default());
        self.defers.push(Vec::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        let _ = self.variables.pop();
        let _ = self.defers.pop();
    }

    /// Emit deferred drops/defers for the top `levels` scopes (LIFO).
    ///
    /// # Current runtime flags — correctness mechanism
    ///
    /// Each `DeferredEntry` carries an optional `i1*` drop flag. When present,
    /// this method loads the flag and conditionally branches:
    /// `if flag { drop } else { skip }` (`defer.run` / `defer.after`). This
    /// runtime check is the current correctness mechanism — it handles
    /// unconditional moves, conditional moves (branch/join), and partial moves
    /// (field flags) without any static analysis of `InitState`.
    ///
    /// # Future hybrid — static elimination (optimization only)
    ///
    /// With `MovePathTree` authoritative, drop elaboration (Phase 7) will
    /// classify each place at each scope exit as:
    /// * **Known `Initialized`** → emit a direct `drop(ptr)` with no flag
    ///   load/branch (static drop).
    /// * **Known `Uninitialized` / `Moved`** → emit nothing (drop elided).
    /// * **Dynamic / `PartiallyInitialized` / join-conditional** → keep the
    ///   current runtime flag guard (`dynamic → drop flag`).
    ///
    /// The flag path remains the fallback; static elimination is purely an
    /// optimization and must preserve the current observable behavior. No
    /// deletion of the flag logic until the elaborated path is proven.
    ///
    /// TODO(Phase 7): formalize drop elaboration (`semantic/drop_elaborate.rs`
    /// or `semantic::init` extension) that consumes `MovePathTree` states and
    /// `Place::overlaps` / `Place::is_prefix_of` to build the elaborated drop
    /// list per scope. `emit_defers` will then iterate the elaborated list
    /// (already filtered to `Initialized` leaves, with `Uninitialized` elided
    /// and `dynamic` still flagged) instead of re-deriving ownership. Example:
    /// `struct Foo { a:String, b:String }` + `move foo.a` → elaborated drops
    /// `[drop(foo.b)]` only. See `semantic::move_path::{MovePathTree,
    /// InitState}` and `semantic::init::{is_initialized, move_out}`.
    pub(crate) fn emit_defers(&mut self, levels: usize) -> CodegenResult<()> {
        let total = self.defers.len();
        if levels == 0 || total == 0 {
            return Ok(());
        }
        let start = total.saturating_sub(levels);
        // Clone scopes to avoid borrow conflicts, emit without draining
        let scopes: Vec<Vec<DeferredEntry<'ctx>>> = self.defers[start..].to_vec();
        for mut scope in scopes.into_iter() {
            for entry in scope.iter_mut().rev() {
                let function = self.current_fn.ok_or_else(|| {
                    CodegenError::new("no active function for defer emission".to_string())
                })?;

                // Build conditional guard if drop flag is present
                let after_bb = if let Some(flag_ptr) = entry.flag {
                    let flag_val = self
                        .builder
                        .build_load(self.context.bool_type(), flag_ptr, "defer.flag")
                        .map_err(|e| {
                            CodegenError::new(format!("failed to load defer flag: {e}"))
                        })?;

                    let run_bb = self.context.append_basic_block(function, "defer.run");
                    let after_bb = self.context.append_basic_block(function, "defer.after");

                    self.builder
                        .build_conditional_branch(flag_val.into_int_value(), run_bb, after_bb)
                        .map_err(|e| CodegenError::new(format!("failed to branch defer: {e}")))?;

                    self.builder.position_at_end(run_bb);
                    Some(after_bb)
                } else {
                    None
                };

                // Execute the deferred action
                match &entry.action {
                    DeferAction::Statement(stmt) => {
                        self.generate_statement(stmt)?;
                    }
                    DeferAction::DropCall(drop_fn_name, var_ptr) => {
                        if let Some(func) = self.module.get_function(drop_fn_name) {
                            let args = vec![BasicMetadataValueEnum::from(*var_ptr)];
                            self.builder.build_call(func, &args, "drop").map_err(|e| {
                                CodegenError::new(format!("failed to call drop: {e}"))
                            })?;
                        }
                    }
                    DeferAction::EnumPayloadDrop(enum_name, var_ptr) => {
                        self.emit_enum_payload_drop(enum_name, *var_ptr)?;
                    }
                }

                // If we had a conditional guard, resume at after_bb
                if let Some(after) = after_bb {
                    self.builder
                        .build_unconditional_branch(after)
                        .map_err(|e| {
                            CodegenError::new(format!("failed to branch after defer: {e}"))
                        })?;
                    self.builder.position_at_end(after);
                }
            }
        }
        Ok(())
    }

    pub(crate) fn lookup_variable(&self, name: &str) -> Option<VarInfo<'ctx>> {
        self.variables
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    /// Clear every per-field drop flag of `name` (ownership of the whole
    /// struct is transferring elsewhere): the fields no longer hold live
    /// values owned by this variable.
    ///
    /// Current runtime-flag correctness: field flags are `0 = no live value`,
    /// `1 = live`. Clearing them prevents the field cascade emitted by
    /// `register_field_drops` from double-dropping after a whole-struct move.
    /// Future hybrid (Phase 7): when `MovePathTree` tracks `InitState` per
    /// field `Place`, a whole-struct `move_out(Place::new(name))` will mark
    /// the entire subtree `Uninitialized`; drop elaboration will then elide all
    /// field drops statically, and this runtime clear becomes the `dynamic`
    /// fallback only. `Place::overlaps` will decide which field flags are
    /// invalidated (`move x.a` clears `x.a` and `x.a.b` but not `x.b`).
    ///
    /// TODO(Phase 7): replace string-keyed `field_flags` clearing with
    /// `MovePathTree` subtree invalidation via `Place::is_prefix_of`; see
    /// `semantic::init::move_out` and `semantic/drop_elaborate.rs` (planned).
    pub(crate) fn clear_field_flags(&mut self, name: &str) -> CodegenResult<()> {
        if let Some(var) = self.lookup_variable(name) {
            for (_, flag) in var.field_flags {
                self.builder
                    .build_store(flag, self.context.bool_type().const_int(0, false))
                    .map_err(|e| CodegenError::new(format!("failed to clear field flag: {e}")))?;
            }
        }
        Ok(())
    }

    /// Clear per-field drop flag for a specific field path (or any subfield of it).
    ///
    /// Current: string prefix walk (`path` == `p` || `p.starts_with("path.")`)
    /// mirrors `Place::is_prefix_of` structurally; clears exactly the moved
    /// subtree so overlapping children (e.g. `x.a.b` under `x.a`) are also
    /// cleared while disjoint siblings (`x.b`) remain live.
    /// Future (Phase 7): `Place::is_prefix_of` / `Place::overlaps` over
    /// `MovePathTree` will replace the string walk; drop elaboration will use
    /// the same predicate to decide which elaborated field drops to keep.
    ///
    /// TODO(Phase 7): migrate to `Place`-based invalidation
    /// (`semantic::place::Place::is_prefix_of` + `MovePathTree`); see
    /// `semantic/drop_elaborate.rs` (planned) and `semantic::init::move_out`.
    pub(crate) fn clear_field_flags_for_path(
        &mut self,
        root_name: &str,
        path: &str,
    ) -> CodegenResult<()> {
        if let Some(var) = self.lookup_variable(root_name) {
            let prefix = format!("{path}.");
            for (p, flag) in &var.field_flags {
                if *p == path || p.starts_with(&prefix) {
                    self.builder
                        .build_store(*flag, self.context.bool_type().const_int(0, false))
                        .map_err(|e| {
                            CodegenError::new(format!("failed to clear field flag: {e}"))
                        })?;
                }
            }
        }
        Ok(())
    }

    pub(crate) fn lookup_extern_global(
        &self,
        name: &str,
    ) -> Option<(inkwell::values::GlobalValue<'ctx>, ast::Type)> {
        let ty = self.extern_globals.get(name)?.clone();
        self.module.get_global(name).map(|global| (global, ty))
    }

    pub(crate) fn lookup_module_global(
        &self,
        name: &str,
    ) -> Option<(inkwell::values::GlobalValue<'ctx>, ast::Type)> {
        if let Some(ty) = self.global_variables.get(name).cloned()
            && let Some(global) = self.module.get_global(name)
        {
            return Some((global, ty));
        }
        self.lookup_extern_global(name)
    }

    pub(crate) fn lookup_storage(&self, name: &str) -> Option<(PointerValue<'ctx>, ast::Type)> {
        if let Some(info) = self.lookup_variable(name) {
            return Some((info.ptr, info.ty));
        }
        self.lookup_module_global(name)
            .map(|(global, ty)| (global.as_pointer_value(), ty))
    }

    pub(crate) fn lookup_value_type(&self, name: &str) -> Option<ast::Type> {
        self.lookup_variable(name)
            .map(|info| info.ty)
            .or_else(|| self.global_variables.get(name).cloned())
            .or_else(|| self.extern_globals.get(name).cloned())
    }

    pub(crate) fn intern_const_string_global(&mut self, value: &str) -> PointerValue<'ctx> {
        if let Some(existing) = self.string_constants.get(value) {
            return *existing;
        }

        let string_value = self.context.const_string(value.as_bytes(), true);
        let global_name = format!(".str.{}", self.string_constants.len());
        let global = self
            .module
            .add_global(string_value.get_type(), None, &global_name);
        global.set_initializer(&string_value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        let ptr = global.as_pointer_value();
        self.string_constants.insert(value.to_string(), ptr);
        ptr
    }

    pub(crate) fn register_function_signature(
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

    pub(crate) fn signature_for_name(&self, llvm_name: &str) -> Option<FunctionSig> {
        self.function_name_to_symbol
            .get(llvm_name)
            .and_then(|symbol_id| self.function_sigs.get(symbol_id))
            .cloned()
    }

    pub(crate) fn named_type_name(named: &ast::NamedType) -> String {
        named
            .path
            .iter()
            .map(|segment| segment.name.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub(crate) fn named_type_key(named: &ast::NamedType) -> String {
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

    /// Resolve `ty` to the Drop destructor function name, if `ty` owns resources.
    ///
    /// Current: heuristic `needs_drop` via concrete `Drop` impl owners + generic
    /// instantiation probe. This is the `needs_drop` predicate that decides
    /// whether a flag/defer is emitted (see `register_drop_flag`).
    /// Future hybrid (Phase 7): `semantic::type_properties::needs_drop` /
    /// `semantic::move_path::InitState` will be the source for "does this
    /// place need a drop?" Drop elaboration will query `InitState` per `Place`
    /// and `Place::overlaps` for partial moves; the hybrid at codegen becomes
    /// `Initialized` → direct drop (no flag), `Uninitialized` → elided,
    /// `dynamic` → flag guard. Keep this heuristic authoritative until
    /// `type_properties` cutover; no logic change in this phase.
    ///
    /// TODO(Phase 5): centralize via `semantic::type_properties::{is_copy, needs_drop}`.
    /// `needs_drop` / `is_copy` will be the single source of truth for Copy vs
    /// owning-type classification; this heuristic stays authoritative until the
    /// cutover. Implicit Copy retained (bool/i64/f64/ptr + all-Copy struct = Copy
    /// else non-Copy like String/Vec/HashMap/HashSet/Deque/File).
    /// Future split: `T x = y` => `if is_copy(T) { copy_from(y) } else { move_out(y) }`
    /// — typeck/move_check/codegen must not independently decide Copy/Drop.
    /// No logic change in this phase; keep existing per-subsystem heuristics working in parallel.
    ///
    /// TODO(Phase 7): drop elaboration (`semantic/drop_elaborate.rs` or
    /// `semantic::init` extension) will consume `MovePathTree` states; this
    /// function's role narrows to "which `Drop` symbol to call" after
    /// elaboration has chosen *whether* to drop. See hybrid plan in file header.
    pub(crate) fn get_drop_function_name(
        &mut self,
        ty: &ast::Type,
    ) -> CodegenResult<Option<String>> {
        // 1. Check concrete Drop-impl owners already registered
        let drop_owners = Self::owner_name_candidates_from_type(ty);
        for owner in &drop_owners {
            if self.drop_trait_impl_owners.contains(owner.as_str()) {
                for candidate in self.overloaded_method_candidates(owner, "drop") {
                    if self.module.get_function(&candidate).is_some() {
                        return Ok(Some(candidate));
                    }
                }
            }
        }
        // 2. Try generic Drop-impl instantiation
        if let Some(mangled) =
            self.try_instantiate_generic_impl_method_for_type_filtered(ty, "drop", Some("Drop"))?
        {
            return Ok(Some(mangled));
        }
        // 3. No Drop trait impl
        Ok(None)
    }

    /// Emit a tag-switched drop of an enum's payload: load the i16 tag and
    /// drop the active variant's Drop-typed payload values (enums without a
    /// Drop impl of their own). Zero-initialized enums carry tag 0; their
    /// payload is zeroed, so null-guarded drops are no-ops.
    fn emit_enum_payload_drop(
        &mut self,
        enum_type: &ast::Type,
        var_ptr: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let Some(named) = Self::extract_named_type(enum_type).cloned() else {
            return Ok(());
        };
        let enum_name = &named.path[0].name;
        let Some(struct_ty) = self.enum_payload_layouts.get(enum_name).copied() else {
            return Ok(());
        };
        let Some(variants) = self.enum_variants.get(enum_name).cloned() else {
            return Ok(());
        };
        let payload_types = self
            .enum_variant_payload_types
            .get(enum_name)
            .cloned()
            .unwrap_or_default();
        let substitutions: HashMap<String, ast::Type> = if let Some(params) =
            self.struct_generics.get(enum_name)
            && let Some(args) = &named.generics
            && params.len() == args.len()
        {
            params.iter().cloned().zip(args.iter().cloned()).collect()
        } else {
            HashMap::default()
        };
        let target_data =
            TargetData::create(self.module.get_data_layout().as_str().to_str().unwrap());
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for enum payload drop"))?;
        let tag_ptr = self
            .builder
            .build_struct_gep(struct_ty, var_ptr, 0, "epd.tag.ptr")
            .map_err(|e| CodegenError::new(format!("enum payload tag GEP: {e}")))?;
        let tag_val = self
            .builder
            .build_load(self.context.i16_type(), tag_ptr, "epd.tag")
            .map_err(|e| CodegenError::new(format!("enum payload tag load: {e}")))?;
        let data_ptr = self
            .builder
            .build_struct_gep(struct_ty, var_ptr, 1, "epd.data")
            .map_err(|e| CodegenError::new(format!("enum payload data GEP: {e}")))?;
        let mut cond_bb = self
            .builder
            .get_insert_block()
            .ok_or_else(|| CodegenError::new("builder is not positioned in a basic block"))?;
        let after_bb = self.context.append_basic_block(function, "epd.after");
        let mut any = false;
        // Variants are compared by their stored tag values.
        for (variant_name, tag_value) in &variants {
            let types = payload_types.get(variant_name).cloned().unwrap_or_default();
            // Compute the payload field drops for this variant (by byte offset).
            let mut drops: Vec<(u32, String)> = Vec::new();
            let mut offset: u32 = 0;
            for pt in &types {
                let concrete_pt = if substitutions.is_empty() {
                    pt.clone()
                } else {
                    Self::substitute_generic_type(pt, &substitutions)
                };
                let llvm_ty = self.lower_basic_type(&concrete_pt)?;
                if let Some(drop_fn) = self.get_drop_function_name(&concrete_pt)? {
                    drops.push((offset, drop_fn));
                }
                offset += target_data.get_abi_size(&llvm_ty) as u32;
            }
            if drops.is_empty() {
                continue;
            }
            any = true;
            // tag == tag_value -> run this variant's drops.
            let expected = self.context.i16_type().const_int(*tag_value as u64, false);
            self.builder.position_at_end(cond_bb);
            let cond = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    tag_val.into_int_value(),
                    expected,
                    "epd.cmp",
                )
                .map_err(|e| CodegenError::new(format!("enum payload tag compare: {e}")))?;
            let run_bb = self.context.append_basic_block(function, "epd.run");
            self.builder
                .build_conditional_branch(cond, run_bb, after_bb)
                .map_err(|e| CodegenError::new(format!("enum payload branch: {e}")))?;
            self.builder.position_at_end(run_bb);
            for (byte_offset, drop_fn) in drops {
                let field_ptr = if byte_offset == 0 {
                    data_ptr
                } else {
                    unsafe {
                        self.builder.build_gep(
                            self.context.i8_type(),
                            data_ptr,
                            &[self.context.i32_type().const_int(byte_offset as u64, false)],
                            "epd.field",
                        )
                    }
                    .map_err(|e| CodegenError::new(format!("enum payload field GEP: {e}")))?
                };
                if let Some(func) = self.module.get_function(&drop_fn) {
                    let args = vec![BasicMetadataValueEnum::from(field_ptr)];
                    self.builder
                        .build_call(func, &args, "epd.drop")
                        .map_err(|e| CodegenError::new(format!("enum payload drop call: {e}")))?;
                }
            }
            self.builder
                .build_unconditional_branch(after_bb)
                .map_err(|e| CodegenError::new(format!("enum payload join: {e}")))?;
            cond_bb = after_bb;
        }
        if any {
            self.builder.position_at_end(after_bb);
        }
        Ok(())
    }

    /// Register the tag-aware payload cascade for an enum local WITHOUT a
    /// Drop impl of its own: the active variant's Drop-typed payload values
    /// are dropped at scope exit (guarded by the variable's drop flag so a
    /// moved enum skips it).
    ///
    /// Current: allocates an `i1` flag (`{name}.drop = 1`) and a deferred
    /// `EnumPayloadDrop`; `emit_defers` guards it with a flag check and
    /// `emit_enum_payload_drop` tag-switches to the live variant's payload
    /// drops. Future hybrid (Phase 7): `MovePathTree` per-variant state +
    /// `Place::overlaps` will allow static knowledge (`Initialized` → direct
    /// payload drop, `Uninitialized` → no payload drop, dynamic → flag) — same
    /// hybrid as struct field drops. Keep runtime flag as correctness until
    /// elaboration is proven.
    ///
    /// TODO(Phase 7): formalize drop elaboration for enums using
    /// `MovePathTree`/`InitState` and `Place::overlaps`; see
    /// `semantic/drop_elaborate.rs` (planned).
    fn register_enum_payload_cascade(
        &mut self,
        ty: &ast::Type,
        var_ptr: PointerValue<'ctx>,
        name: &str,
    ) -> CodegenResult<()> {
        let Some(named) = Self::extract_named_type(ty).cloned() else {
            return Ok(());
        };
        if named.path.len() != 1 {
            return Ok(());
        }
        let enum_name = &named.path[0].name;
        if !self.enum_payload_layouts.contains_key(enum_name) {
            return Ok(());
        }
        let substitutions: HashMap<String, ast::Type> = if let Some(params) =
            self.struct_generics.get(enum_name)
            && let Some(args) = &named.generics
            && params.len() == args.len()
        {
            params.iter().cloned().zip(args.iter().cloned()).collect()
        } else {
            HashMap::default()
        };

        // Only enums whose payloads can carry Drop values need the cascade.
        let payload_types = self.enum_variant_payload_types.get(enum_name).cloned();
        let has_drop_payload = payload_types
            .map(|m| {
                m.values().any(|types| {
                    types.iter().any(|pt| {
                        let concrete_pt = if substitutions.is_empty() {
                            pt.clone()
                        } else {
                            Self::substitute_generic_type(pt, &substitutions)
                        };
                        self.get_drop_function_name(&concrete_pt)
                            .unwrap_or(None)
                            .is_some()
                    })
                })
            })
            .unwrap_or(false);
        if !has_drop_payload {
            return Ok(());
        }
        let function = self
            .current_fn
            .ok_or_else(|| CodegenError::new("no active function for enum cascade flag"))?;
        let flag_alloca = self.create_entry_alloca(
            function,
            &format!("{name}.drop"),
            self.context.bool_type().as_basic_type_enum(),
        )?;
        self.builder
            .build_store(flag_alloca, self.context.bool_type().const_int(1, false))
            .map_err(|e| CodegenError::new(format!("failed to init enum flag: {e}")))?;
        if let Some(scope) = self.variables.last_mut()
            && let Some(var) = scope.get_mut(name)
        {
            var.drop_flag = Some(flag_alloca);
        }
        if let Some(scope) = self.defers.last_mut() {
            scope.push(DeferredEntry {
                action: DeferAction::EnumPayloadDrop(ty.clone(), var_ptr),
                flag: Some(flag_alloca),
            });
        }
        Ok(())
    }

    /// Allocate a 1-bit drop flag for `name`, initialize it to true, and
    /// register `var_ptr`'s destructor (plus cascaded field drops) as a
    /// deferred drop on the current scope. Records the flag so `move` and
    /// by-value transfers can clear it. Shared by parameters and locals.
    ///
    /// # Current runtime flags — correctness mechanism
    ///
    /// This is the present authoritative drop setup: if `needs_drop(ty)`
    /// (currently `get_drop_function_name(ty).is_some()`), an `i1` flag
    /// `{name}.drop = 1` is stack-allocated and a `DropCall` deferred entry
    /// is pushed; field drops are cascaded via `register_field_drops`. Flags
    /// are cleared on `move`, by-value transfer, or explicit `drop()`, and
    /// checked at scope exit by `emit_defers`. Field flags start `0` and are
    /// set live on initialization so uninitialized fields are never dropped.
    ///
    /// # Future hybrid — static elimination (optimization only)
    ///
    /// With `MovePathTree`/`InitState` authoritative, drop elaboration (Phase
    /// 7) will classify each `Place` at each exit:
    /// `Initialized` → direct `drop` (no flag), `Uninitialized` → elided,
    /// `PartiallyInitialized`/conditional → keep runtime flag (`dynamic → drop
    /// flag`). This function's flag allocation then becomes the dynamic
    /// fallback; statically known drops bypass it. No logic change in this
    /// phase; existing flags stay correct and the static path is additive.
    ///
    /// TODO(Phase 5): centralize via `semantic::type_properties::{is_copy, needs_drop}`.
    /// `needs_drop(ty)` (equivalently `!is_copy(ty)` for owning types) will gate
    /// whether a flag/defer is emitted. This code stays authoritative until the
    /// cutover. Implicit Copy retained (bool/i64/f64/ptr + all-Copy struct = Copy
    /// else non-Copy like String/Vec/HashMap/HashSet/Deque/File).
    /// Future split: `T x = y` => `if is_copy(T) { copy_from(y) } else { move_out(y) }`
    /// — the `get_drop_function_name` / `is_copy` decision will be shared via
    /// `semantic::type_properties`. No logic change in this phase.
    ///
    /// TODO(Phase 7): formalize drop elaboration (`semantic/drop_elaborate.rs`
    /// or extension) using `MovePathTree` states and `Place::overlaps` /
    /// `Place::is_prefix_of`. For `Foo { a:String, b:String }` + `move foo.a`,
    /// elaboration emits `drop(foo.b)` only; this function will then consult
    /// the elaborated set instead of unconditionally allocating a flag. Keep
    /// runtime flags as correctness until proven. See
    /// `semantic::move_path::{MovePathTree, InitState}` and
    /// `semantic::init::{is_initialized, move_out, initialize}`.
    pub(crate) fn register_drop_flag(
        &mut self,
        name: &str,
        ty: &ast::Type,
        var_ptr: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        let drop_fn_name = self.get_drop_function_name(ty)?;
        if let Some(drop_fn_name) = drop_fn_name {
            let function = self.current_fn.ok_or_else(|| {
                CodegenError::new("no active function for destructor".to_string())
            })?;
            let flag_alloca = self.create_entry_alloca(
                function,
                &format!("{name}.drop"),
                self.context.bool_type().as_basic_type_enum(),
            )?;
            self.builder
                .build_store(flag_alloca, self.context.bool_type().const_int(1, false))
                .map_err(|e| CodegenError::new(format!("failed to init drop flag: {e}")))?;
            if let Some(scope) = self.variables.last_mut()
                && let Some(var) = scope.get_mut(name)
            {
                var.drop_flag = Some(flag_alloca);
            }

            let field_flags = self.register_field_drops(ty, var_ptr, flag_alloca)?;
            if let Some(scope) = self.variables.last_mut()
                && let Some(var) = scope.get_mut(name)
            {
                var.field_flags = field_flags;
            }

            if let Some(scope) = self.defers.last_mut() {
                scope.push(DeferredEntry {
                    action: DeferAction::DropCall(drop_fn_name, var_ptr),
                    flag: Some(flag_alloca),
                });
            }
        } else {
            // Struct without its own Drop impl: register field drop cascade if any fields have Drop.
            let function = self.current_fn.ok_or_else(|| {
                CodegenError::new("no active function for destructor".to_string())
            })?;
            let dummy_flag = self.create_entry_alloca(
                function,
                &format!("{name}.field_guard"),
                self.context.bool_type().as_basic_type_enum(),
            )?;
            let field_flags = self.register_field_drops(ty, var_ptr, dummy_flag)?;
            if !field_flags.is_empty() {
                if let Some(scope) = self.variables.last_mut()
                    && let Some(var) = scope.get_mut(name)
                {
                    var.field_flags = field_flags;
                }
            } else {
                return self.register_enum_payload_cascade(ty, var_ptr, name);
            }
        }
        Ok(())
    }

    /// Register field-drop cascade for `ty`'s `Drop`-typed fields.
    ///
    /// Current: walks struct fields recursively, allocating a per-field `i1`
    /// flag per `Drop` field (init `0`), pushing a `DropCall` per field onto
    /// `defers` (LIFO so declaration-order drops fire). `clear_field_flags*`
    /// and field assignment logic toggle these flags. This string-keyed cascade
    /// is the field-level correctness mechanism.
    /// Future hybrid (Phase 7): drop elaboration will walk `MovePathTree`
    /// children and emit drops only for `Initialized` leaves (`Place::overlaps`
    /// decides disjointness: `x.a` vs `x.b` disjoint, `x` vs `x.a` overlap).
    /// The hybrid becomes `Initialized leaf` → direct field drop, `Uninitialized`
    /// → elided, `PartiallyInitialized`/dynamic → keep per-field flag guard.
    /// Per-field flags remain the dynamic fallback.
    ///
    /// TODO(Phase 7): formalize drop elaboration (`semantic/drop_elaborate.rs`
    /// or extension) that consumes `MovePathTree` + `Place::overlaps` to
    /// produce the elaborated field-drop list; this function's field walk will
    /// then be driven by that list. Keep current cascade until proven. See
    /// file header hybrid plan and `semantic::init` / `semantic::place`.
    pub(crate) fn register_field_drops(
        &mut self,
        ty: &ast::Type,
        struct_ptr: PointerValue<'ctx>,
        _parent_flag: PointerValue<'ctx>,
    ) -> CodegenResult<Vec<(String, PointerValue<'ctx>)>> {
        let Some(named) = Self::extract_named_type(ty).cloned() else {
            return Ok(Vec::new());
        };
        let base_name = named.path.last().map(|s| &s.name[..]).unwrap_or_default();
        if base_name == "Task"
            || self.enum_backing_type_for_named(&named).is_some()
            || (named.path.len() == 1
                && self.enum_payload_layouts.contains_key(&named.path[0].name))
        {
            return Ok(Vec::new());
        }
        let named_key = Self::named_type_key(&named);
        if !self.struct_fields.contains_key(base_name)
            && !self.struct_fields.contains_key(&named_key)
        {
            return Ok(Vec::new());
        }
        let _ = self.ensure_named_struct_type(&named)?;
        let named_key = Self::named_type_key(&named);
        // Clone fields and the struct type to avoid borrowing self while
        // recursively calling self.register_field_drops / get_drop_function_name.
        let fields: Vec<(String, ast::Type)> = match self.struct_fields.get(&named_key) {
            Some(f) => f.clone(),
            None => return Ok(Vec::new()),
        };
        let struct_ty = match self.struct_types.get(&named_key) {
            Some(ty) => *ty,
            None => return Ok(Vec::new()),
        };
        // Iterate in reverse so that declaration-order drops fire at runtime
        // (defers are LIFO, so last-registered fires first). Each Drop-typed
        // field gets its own flag, initialized FALSE: a field only holds a
        // live value once it has been assigned (definite-init tracking —
        // uninitialized fields must not be dropped).
        let mut collected: Vec<(String, PointerValue<'ctx>)> = Vec::new();
        for (field_index, (field_name, field_ty)) in fields.iter().enumerate().rev() {
            let field_ptr = self
                .builder
                .build_struct_gep(struct_ty, struct_ptr, field_index as u32, field_name)
                .map_err(|e| CodegenError::new(format!("cascade field GEP: {e}")))?;
            // Only cascade drops into value-type fields — pointers/references are non-owning.
            if !Self::is_pointer_or_reference(field_ty) {
                // Recursively register drops for nested fields first
                // (so the parent field's drop fires after its children).
                let nested = self.register_field_drops(field_ty, field_ptr, _parent_flag)?;
                for (nested_path, nested_flag) in nested {
                    let full_path = format!("{field_name}.{nested_path}");
                    collected.push((full_path, nested_flag));
                }

                // Register this field's own drop if it implements Drop.
                if let Some(drop_fn) = self.get_drop_function_name(field_ty)? {
                    let function = self.current_fn.ok_or_else(|| {
                        CodegenError::new("no active function for field drop flag")
                    })?;
                    let field_flag = self.create_entry_alloca(
                        function,
                        &format!("field.{field_name}.drop"),
                        self.context.bool_type().as_basic_type_enum(),
                    )?;
                    self.builder
                        .build_store(field_flag, self.context.bool_type().const_int(0, false))
                        .map_err(|e| {
                            CodegenError::new(format!("failed to init field drop flag: {e}"))
                        })?;
                    if let Some(scope) = self.defers.last_mut() {
                        scope.push(DeferredEntry {
                            action: DeferAction::DropCall(drop_fn, field_ptr),
                            flag: Some(field_flag),
                        });
                    }
                    collected.push((field_name.clone(), field_flag));
                }
            }
        }
        Ok(collected)
    }

    pub(crate) fn extract_named_type(ty: &ast::Type) -> Option<&ast::NamedType> {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => Some(named),
            ast::TypeKind::Reference(reference) => Self::extract_named_type(&reference.inner),
            ast::TypeKind::Pointer(pointer) => Self::extract_named_type(&pointer.inner),
            _ => None,
        }
    }

    pub(crate) fn is_pointer_or_reference(ty: &ast::Type) -> bool {
        matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        )
    }
}
