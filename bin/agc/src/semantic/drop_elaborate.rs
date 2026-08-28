//! Drop elaboration — Phase 6-8 skeleton (no behavior change yet).
//!
//! # Pipeline split — AST → Typeck → Place → Borrow/Move → DropElaboration → LLVM
//!
//! ```text
//! AST ──► Typeck ──► Place ──► Borrow / Move ──► DropElaboration ──► LLVM
//!         (typeck)   (place)   (borrow_check,    (this module)      (codegen/llvm_ir/scope)
//!                               move_check,
//!                               init / move_path)
//! ```
//!
//! ## Responsibilities (Phase 6-8 refactor, scaffolding only)
//!
//! * **Typeck** (`semantic::typeck`) resolves `ast::Type` → `Type` and populates
//!   `type_properties::{is_copy, needs_drop}` (single source of truth for
//!   `Copy` vs owning types, implicit `Copy` retained: bool/i64/f64/ptr +
//!   all-`Copy` struct = `Copy` else non-`Copy`).
//!
//! * **Place** (`semantic::place`) builds the structured `Place { local, projections }`
//!   that replaces string dotted paths (`"a.b.c"`) — `Place::new("x").field("a")`,
//!   `Place::is_prefix_of` / `Place::overlaps` / `places_overlap` are pure
//!   structural helpers shared by all later phases.
//!
//! * **Borrow / Move** (`semantic::borrow_check`, `semantic::move_check`,
//!   `semantic::init`, `semantic::move_path`) determines **which `Place`s are
//!   initialized** at each program point. `MovePathTree<Place>` stores
//!   `InitState::{Init, Partial, Uninit}` per `Place` node (`Init` ↔ `Live`,
//!   `Partial` ↔ `PartiallyMoved`, `Uninit` ↔ `FullyMoved`). Operations
//!   `init::move_out`, `init::initialize`, `init::copy_from`, `init::read` are
//!   the primitives; the checker reports use-after-move via
//!   `has_uninit_prefix` / `Place::is_prefix_of` (no string `rfind`).
//!
//! * **Drop elaboration** (this module) determines **which initialized values
//!   to destroy** at each scope exit / unwind / early-return point. It is a
//!   *read-only* consumer of the `MovePathTree` produced by the previous phase:
//!   it never mutates init state, it only queries `is_initialized` / `InitState`
//!   via `Place::overlaps` to collect the live `Drop` places that need a
//!   destructor call. Codegen then emits conditional `DropCall`s guarded by
//!   per-`Place` flags (hybrid drop-flag elimination target — not yet switched).
//!
//! * **LLVM** (`codegen::llvm_ir::scope`) currently **figures ownership itself**
//!   via `register_drop_flag` / `field_flags` / `DeferredEntry::DropCall` and
//!   `clear_field_flags_for_path` (string prefix walks like `p == path ||
//!   p.starts_with("path.")`). That stays **authoritative** until this
//!   elaborator is proven — no deletion, no flag removal in this phase. Future
//!   cutover: `drop_elaborate(&tree) -> Vec<PlaceToDrop>` will drive
//!   `LlvmIrGenerator::emit_defers` / `register_drop_flag` instead of the
//!   ad-hoc string logic, using `Place::overlaps` for field vs whole invalidation.
//!
//! ## Sketch interface (not yet called — scaffolding)
//!
//! ```ignore
//! // Producer: move checker builds the tree per function / per path
//! let tree: MovePathTree = build_move_path_tree(&func, &type_info)?;
//!
//! // Consumer: drop elaborator queries the tree read-only
//! let drops: Vec<PlaceToDrop> = drop_elaborate(&tree);
//! // or per-scope:
//! let drops: Vec<PlaceToDrop> = DropElaborate::elaborate(&tree, scope_id);
//!
//! // Codegen: emit conditional drops for each PlaceToDrop
//! for PlaceToDrop { place, span } in drops {
//!     // if is_initialized(tree, &place) && needs_drop(place_ty) { emit_drop(place) }
//! }
//! ```
//!
//! TODO(Phase 6-8): wire `MovePathTree` through `check_program` / CFG, implement
//! `drop_elaborate` via `tree.iter_initialized_places()` + `needs_drop` +
//! `Place::overlaps`-based liveness, then migrate `scope.rs` field-flag logic to
//! consume `Vec<PlaceToDrop>` instead of string `field_flags`. Keep old `VarState` /
//! `DeferredEntry` path hot until the new path is proven; mark TODOs, no deletion.
//!
//! No logic is deleted or switched in this phase; this file is scaffolding so
//! `cargo check` passes and downstream patches have a stable import path.

use crate::semantic::move_path::MovePathTree;
use crate::semantic::place::Place;

/// A single destructor to emit at a scope exit / unwind point.
///
/// `place` is the structured `Place` to destroy (e.g. `x`, `x.a`, `s.field`).
/// Codegen will lower it to a `DropCall` guarded by the `Place`'s drop flag;
/// `needs_drop(place_ty)` (via `type_properties`) decides whether a call is
/// needed at all. Additional fields (span, insertion block, kind) can be added
/// without breaking the `drop_elaborate(tree) -> Vec<PlaceToDrop>` shape.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PlaceToDrop {
    /// The `Place` whose value is live and needs dropping.
    pub place: Place,
    // Future: span / scope / kind for diagnostics & insertion point.
    // pub span: Option<crate::lexer::Span>,
    // pub scope_id: usize,
}

/// Drop elaboration driver — empty scaffolding struct.
///
/// Exists so call-sites can write `DropElaborate::elaborate(&tree)` or hold
/// elaboration state in future (e.g. per-function cache, `needs_drop` table).
/// No fields yet; the free function `drop_elaborate` is the primary sketch.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DropElaborate;

/// Elaborate drops for `tree` — which initialized `Place`s need destroying.
///
/// Read-only over `MovePathTree`: inspects `InitState` via `is_initialized` /
/// `Place::overlaps` and returns every live `Place` whose type `needs_drop`.
/// Not yet implemented — returns empty vec so `cargo check` passes and callers
/// can depend on the signature.
///
/// Signature sketch required by Phase 6-8 split:
/// `fn drop_elaborate(tree: &MovePathTree) -> Vec<PlaceToDrop>`
///
/// Future variants (all compatible with this shape):
/// * `fn drop_elaborate_for_scope(tree: &MovePathTree, scope: ScopeId) -> Vec<PlaceToDrop>`
/// * `impl DropElaborate { fn elaborate(&self, tree: &MovePathTree) -> Vec<PlaceToDrop> }`
/// * `fn drop_elaborate(tree: &MovePathTree, needs_drop: impl Fn(&Place) -> bool) -> Vec<PlaceToDrop>`
#[allow(dead_code)]
pub fn drop_elaborate(_tree: &MovePathTree) -> Vec<PlaceToDrop> {
    // TODO(Phase 6-8): iterate `tree` places where `is_initialized(tree, place)`
    // && `needs_drop(place_ty)`, collect via `Place::overlaps` liveness at scope exits.
    Vec::new()
}

impl DropElaborate {
    /// Associated-function alias for `drop_elaborate` — `DropElaborate::elaborate(&tree)`.
    #[allow(dead_code)]
    pub fn elaborate(_tree: &MovePathTree) -> Vec<PlaceToDrop> {
        drop_elaborate(_tree)
    }

    /// Instance-method alias — `DropElaborate::default().elaborate_for(&tree)`.
    #[allow(dead_code)]
    pub fn elaborate_for(&self, tree: &MovePathTree) -> Vec<PlaceToDrop> {
        drop_elaborate(tree)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::move_path::MovePathTree;

    #[test]
    fn skeleton_compiles_and_returns_empty() {
        let tree = MovePathTree::default();
        let drops = drop_elaborate(&tree);
        assert!(drops.is_empty());
        let drops2 = DropElaborate::elaborate(&tree);
        assert!(drops2.is_empty());
        let elab = DropElaborate::default();
        assert!(elab.elaborate_for(&tree).is_empty());
    }

    #[test]
    fn place_to_drop_holds_place() {
        let p = Place::new("x").field("a");
        let ptd = PlaceToDrop { place: p.clone() };
        assert_eq!(ptd.place, p);
    }
}
