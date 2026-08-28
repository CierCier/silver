//! Drop elaboration — hybrid flag plan (read-only over `MovePathTree`).
//! Why: collect live `Place`s needing `Drop` via `is_initialized` + `needs_drop` + `Place::overlaps`; LLVM flag emission stays authoritative until proven.
//! Sketch: `drop_elaborate(&tree) -> Vec<PlaceToDrop>` drives `scope.rs` flag emission.

use crate::semantic::move_path::MovePathTree;
use crate::semantic::place::Place;

/// One destructor to emit at scope exit (why: `Place` + `needs_drop` decides flag-guarded `DropCall`).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PlaceToDrop {
    /// The `Place` whose value is live and needs dropping.
    pub place: Place,
    // Future: span / scope / kind for diagnostics & insertion point.
    // pub span: Option<crate::lexer::Span>,
    // pub scope_id: usize,
}

/// Scaffolding driver — enables `DropElaborate::elaborate(&tree)`; free fn is primary.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DropElaborate;

/// Read-only over `MovePathTree`: collect live `needs_drop` places via `Place::overlaps` (stub returns empty).
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
