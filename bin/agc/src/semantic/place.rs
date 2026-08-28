//! Structural `Place` representation — Phase 1 foundation, Phase 2 projection semantics.
//!
//! A `Place` names a storage location in Silver's ownership model, mirroring
//! Silver syntax directly (not a Rust clone). It replaces the ad-hoc
//! `expr_root_and_path()` string paths with a structurally comparable type
//! shared by borrow checking, move checking, initialization and drop
//! elaboration.
//!
//! # Phase 1 examples
//!
//! * `x`          — local `x` with no projections: `Place::new("x")`
//! * `x.foo.bar`  — field projections: `Place { local: "x", projections: [Field("foo"), Field("bar")] }`
//! * `x.0`        — tuple field projection: `Place { local: "x", projections: [TupleField(0)] }`
//! * `x.a.b.c`    — chained fields: `Place { local: "x", projections: [Field("a"), Field("b"), Field("c")] }`
//!
//! Additional projections exist for later phases but are already part of the
//! enum so code can name them uniformly:
//!
//! * `*p` / deref — `Projection::Deref`
//! * `v[i]`       — `Projection::Index`
//!
//! `Place` is intentionally pure data: helpers like `is_prefix_of`,
//! `overlaps`, `parent`, etc. live as associated functions/methods added in
//! Phase 2 and are not required for Phase 1's "no semantic change" gate.
//! Existing checkers continue to use string-based `expr_root_and_path` in
//! parallel; `Place` is parallel infrastructure.
//!
//! # Phase 2 — Projection semantics (Phase 2 deliverable)
//!
//! Phase 2 delivers the **shared overlap logic** for `Place`. The helpers
//! `Place::is_prefix_of`, `Place::overlaps`, and `places_overlap` are pure,
//! structural, and Silver-natural: overlap is defined by **local equality +
//! prefix of projections** (no string splitting, no `rfind('.')`).
//!
//! > **Phase 2 deliverable — no semantic change yet.** The string-based
//! > checkers in `move_check.rs` (`VarState::is_field_moved`,
//! > `mark_field_moved`, `expr_root_and_path`) and `borrow_check.rs`
//! > (`ActiveBorrow`, `paths_overlap`, `extract_root_and_path`) remain the
//! > authoritative path. `Place` helpers are parallel infrastructure verified
//! > by unit tests; the cutover to `Place` keys happens in follow-up phases.
//!
//! ## Sharing plan — one overlap impl for four consumers
//!
//! `places_overlap` / `Place::overlaps` is the single source of truth called
//! by:
//!
//! * **Borrow checker** (`semantic/borrow_check.rs`) — `ActiveBorrow { place: Place }`
//!   conflict detection replaces `paths_overlap` (`b.root == root &&
//!   paths_overlap(b.path, path)`). A loan of `x` conflicts with `x.a`, `x.a.b`,
//!   but not `x.b` or `y.a`.
//! * **Move checker** (`semantic/move_check.rs`) — `VarState` field-move tracking
//!   replaces `moved_fields: FxHashMap<String,_>` string prefix walks;
//!   `is_field_moved` becomes `Place::is_prefix_of`, whole-vs-partial moves use
//!   `overlaps`.
//! * **Initialization / definite-assignment** (Phase 4) — `read(place)`,
//!   `move_out(place)`, `initialize(place)` operate on `Place` keys; `overlaps`
//!   decides whether `move x.a` poisons `x.a.b` but not `x.b`.
//! * **Drop elaboration** (`codegen/llvm_ir/scope.rs`) — per-field drop flags and
//!   cascade emission query `overlaps` to decide which deferred `DropCall`s are
//!   invalidated by `move x.a` vs `move x`.
//!
//! All four subsystems share the same `Projection` equality (structural, not
//! stringly) so `x.a` vs `x.a.b`, `x.0` vs `x.0`, `*p` vs `*p`, and `v[i]`
//! are judged uniformly.
//!
//! ## Overlap semantics — Silver-natural, not Rust-cloned
//!
//! ```text
//! is_prefix_of(self, other) :=
//!     self.local == other.local
//!     && self.projections.len() <= other.projections.len()
//!     && self.projections == other.projections[0..self.projections.len()]
//!
//! overlaps(self, other) :=
//!     is_prefix_of(self, other) || is_prefix_of(other, self)
//!
//! places_overlap(a, b) := a.overlaps(b)   // free-function alias; also used
//!                                         // as `places_overlap(&[Place], &[Place])`
//!                                         // → true if any pair overlaps
//! ```
//!
//! Consequences:
//!
//! * Reflexive: `p.overlaps(p)` is always `true`.
//! * Symmetric: `a.overlaps(b) == b.overlaps(a)`.
//! * Prefix-closed: `x` is a prefix of `x.a`, `x.a.b`, etc., so `x` overlaps every
//!   place rooted at `x`.
//! * Local-sensitive: different locals never overlap, even with identical
//!   projections (`x.a` vs `y.a` → `false`).
//! * Structural: `Field("a") != Field("b")`, `Field("a") != TupleField(0)`,
//!   `Deref != Field`, `Index != Field`; projections compare by `==` on the
//!   `Projection` enum (see `Projection` docs).
//!
//! ## Truth table — `is_prefix_of` / `overlaps` / `places_overlap`
//!
//! `places_overlap` below is the pair form (`a.overlaps(b)`). The
//! slice form returns `true` if *any* pair overlaps.
//!
//! | Place A | Place B | `A.is_prefix_of(B)` | `B.is_prefix_of(A)` | `A.overlaps(B)` / `places_overlap(A,B)` | Notes |
//! |---------|---------|---------------------|---------------------|---------------------------------------------|---------------------|
//! | `x` | `x` | `true` | `true` | `true` | same place — reflexive |
//! | `x` | `x.a` | `true` | `false` | `true` | whole overlaps field (parent prefix) |
//! | `x.a` | `x` | `false` | `true` | `true` | symmetric |
//! | `x.a` | `x.a` | `true` | `true` | `true` | identical field |
//! | `x.a` | `x.a.b` | `true` | `false` | `true` | field overlaps sub-field |
//! | `x.a.b` | `x.a` | `false` | `true` | `true` | reverse |
//! | `x.a` | `x.b` | `false` | `false` | `false` | sibling fields — disjoint |
//! | `x.a.b` | `x.a.c` | `false` | `false` | `false` | sibling sub-fields — disjoint |
//! | `x.0` | `x.0` | `true` | `true` | `true` | tuple field equality |
//! | `x.0` | `x.1` | `false` | `false` | `false` | different tuple indices — disjoint |
//! | `*p` | `*p` | `true` | `true` | `true` | `Place { local:"p", [Deref] }` overlaps itself |
//! | `*p` | `p` | `false` | `true` | `true` | `p` is prefix of `*p` (`[]` prefix of `[Deref]`) — whole overlaps deref; direction matters for `is_prefix_of` |
//! | `v[i]` | `v[i]` | `true` | `true` | `true` | `Index` projection — structural equality (dynamic index is opaque) |
//! | `x.a` | `y.a` | `false` | `false` | `false` | different locals — never overlap |
//! | `x` | `y` | `false` | `false` | `false` | different locals, no projections |
//! | `x.a.b` | `y.a.b` | `false` | `false` | `false` | locals differ — projections ignored |
//!
//! ## Examples
//!
//! ```ignore
//! use crate::semantic::place::{Place, Projection, places_overlap};
//!
//! let x = Place::new("x");
//! let xa = Place::new("x").field("a");
//! let xb = Place::new("x").field("b");
//! let xab = Place::from_slice("x", &["a", "b"]);
//! let ya = Place::new("y").field("a");
//!
//! // is_prefix_of — prefix direction matters
//! assert!(x.is_prefix_of(&xa));          // x is prefix of x.a
//! assert!(!xa.is_prefix_of(&x));         // x.a is not prefix of x
//! assert!(xa.is_prefix_of(&xab));        // x.a is prefix of x.a.b
//! assert!(!xab.is_prefix_of(&xa));
//! assert!(xa.is_prefix_of(&xa));         // reflexive — equal places are prefixes
//!
//! // overlaps — symmetric, either direction prefix
//! assert!(xa.overlaps(&xab));            // x.a overlaps x.a.b
//! assert!(xab.overlaps(&xa));            // symmetric
//! assert!(x.overlaps(&xa));              // whole overlaps part
//! assert!(!xa.overlaps(&xb));            // siblings disjoint
//! assert!(!xa.overlaps(&ya));            // different locals disjoint
//!
//! // places_overlap — free-function alias (pair form)
//! assert!(places_overlap(&xa, &xab));
//! assert!(!places_overlap(&xa, &xb));
//!
//! // Sibling sub-fields are disjoint even though they share a common prefix `x.a`
//! let xac = Place::from_slice("x", &["a", "c"]);
//! assert!(xab.overlaps(&xa));            // x.a.b overlaps x.a
//! assert!(xac.overlaps(&xa));            // x.a.c overlaps x.a
//! assert!(!xab.overlaps(&xac));          // but x.a.b does NOT overlap x.a.c
//!
//! // Projection-type sensitivity
//! let x0 = Place { local: "x".into(), projections: vec![Projection::TupleField(0)] };
//! let x1 = Place { local: "x".into(), projections: vec![Projection::TupleField(1)] };
//! assert!(!x0.overlaps(&x1));
//!
//! let deref_p = Place::new("p").deref();
//! assert!(Place::new("p").is_prefix_of(&deref_p));
//! assert!(deref_p.overlaps(&Place::new("p")));
//! ```
//!
/// Local variable identifier — the root of a place.
///
/// Phase 1 uses `String` (Silver source name). A future revision may switch
/// to `SymbolId` (`usize`/`u64` interned id); `Place::new` accepts
/// `impl Into<LocalId>` so either spelling stays ergonomic.
pub type LocalId = String;

/// Field identifier — Silver `struct` field name.
///
/// Alias kept for readability in `Projection::Field(FieldId)`. Like `LocalId`,
/// this is `String` today and may become a `SymbolId`/`FieldId` intern id
/// later without changing the `Place` shape.
pub type FieldId = String;

/// A structural place: a local plus a sequence of projections.
///
/// Mirrors Silver syntax directly:
/// * `x`         → `Place { local: "x", projections: [] }`
/// * `x.foo.bar` → `Place { local: "x", projections: [Field("foo"), Field("bar")] }`
/// * `x.0`       → `Place { local: "x", projections: [TupleField(0)] }`
/// * `x.a.b.c`   → `Place { local: "x", projections: [Field("a"), Field("b"), Field("c")] }`
/// * `*p`        → `Place { local: "p", projections: [Deref] }` (future)
/// * `v[i]`      → `Place { local: "v", projections: [Index] }` (future, dynamic index)
///
/// Derives structural equality/hashing so `x.a == x.a`, `x.a != x.b`, and
/// `HashSet<Place>` work out of the box.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Place {
    /// Root local variable, e.g. `x` in `x.foo.bar`.
    pub local: LocalId,
    /// Projections applied left-to-right, e.g. `[Field("foo"), Field("bar")]`.
    pub projections: Vec<Projection>,
}

/// Place projection — Silver-natural, not Rust-cloned.
///
/// Variants are ordered to match Silver surface syntax:
/// * `Field("foo")`   — `x.foo` / `x.foo.bar` (struct field)
/// * `TupleField(0)`  — `x.0` (tuple index, statically known)
/// * `Index`          — `v[i]` (dynamic container index, Phase 11B)
/// * `Deref`          — `*p` (dereference)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Projection {
    /// Struct field access: `x.foo` or `x.foo.bar` (second `Field`).
    Field(FieldId),
    /// Tuple field access: `x.0`, `x.1`, etc.
    TupleField(usize),
    /// Dynamic index: `v[i]` — value of `i` is not part of the place identity
    /// in Phase 1 (future may carry `Option<usize>` or symbolic index).
    Index,
    /// Dereference: `*p`.
    Deref,
}

impl Place {
    /// Empty place — no local, no projections.
    ///
    /// Useful as a sentinel / const default. Real program places are built
    /// with [`Place::new`] and projections.
    pub const EMPTY: Place = Place {
        local: String::new(),
        projections: Vec::new(),
    };

    /// Create a place for a bare local `x` (no projections).
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let x = Place::new("x");
    /// let y = Place::new(String::from("y"));
    /// assert_eq!(x.local, "x");
    /// assert!(x.projections.is_empty());
    /// ```
    pub fn new(local: impl Into<LocalId>) -> Self {
        Self {
            local: local.into(),
            projections: Vec::new(),
        }
    }
}
impl Place {
    /// Root local of this place.
    ///
    /// # Examples
    ///
    /// ```
    /// // pseudo: Place::new("x").field("a").root() == "x"
    /// ```
    pub fn root(&self) -> &LocalId {
        &self.local
    }

    /// Return a new `Place` extended with a field projection.
    ///
    /// Pure / non-mutating — clones `self` and pushes `Field(field)`.
    ///
    /// # Examples
    ///
    /// ```
    /// // Place::new("x").field("a").field("b") == Place { local: "x", projections: [Field("a"), Field("b")] }
    /// ```
    pub fn field(&self, field: &str) -> Place {
        let mut p = self.clone();
        p.projections.push(Projection::Field(field.to_string()));
        p
    }

    /// Return a new `Place` extended with a deref projection (`*p`).
    pub fn deref(&self) -> Place {
        let mut p = self.clone();
        p.projections.push(Projection::Deref);
        p
    }

    /// Parent place (one projection removed), or `None` if this is a bare local.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// assert_eq!(Place::new("x").field("a").field("b").parent(), Some(Place::new("x").field("a")));
    /// assert_eq!(Place::new("x").parent(), None);
    /// ```
    pub fn parent(&self) -> Option<Place> {
        if self.projections.is_empty() {
            None
        } else {
            let mut p = self.clone();
            p.projections.pop();
            Some(p)
        }
    }

    /// Push a projection onto this place in place.
    pub fn push_projection(&mut self, p: Projection) {
        self.projections.push(p);
    }

    /// Helper for tests: build a place from a local and a slice of field names.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// let p = Place::from_slice("x", &["a", "b"]);
    /// assert_eq!(p, Place::new("x").field("a").field("b"));
    /// ```
    pub fn from_slice(local: impl Into<LocalId>, fields: &[&str]) -> Self {
        let mut place = Self::new(local);
        for f in fields {
            place.projections.push(Projection::Field((*f).to_string()));
        }
        place
    }
}

impl Place {
    /// Returns `true` if `self` is a prefix of `other`.
    ///
    /// Silver-natural definition: same `local` and `self.projections` is a
    /// prefix of `other.projections` (element-wise `==`). The empty prefix
    /// (bare local) is a prefix of any place with the same local.
    ///
    /// Equality of each [`Projection`] is exact:
    /// * `Field` — string equality
    /// * `TupleField` — `usize` equality
    /// * `Index` / `Deref` — unit equality (distinct projections do not match)
    ///
    /// # Examples
    ///
    /// ```
    /// // pseudo: Place::new("x").is_prefix_of(Place::new("x").field("a")) == true
    /// // Place::new("x").field("a").is_prefix_of(Place::new("x").field("a").field("b")) == true
    /// // Place::new("x").field("a").is_prefix_of(Place::new("x").field("b")) == false
    /// // Place::new("x").field("a").is_prefix_of(Place::new("y").field("a")) == false
    /// ```
    pub fn is_prefix_of(&self, other: &Place) -> bool {
        if self.local != other.local {
            return false;
        }
        if self.projections.len() > other.projections.len() {
            return false;
        }
        self.projections[..] == other.projections[..self.projections.len()]
    }

    /// Returns `true` if `self` and `other` overlap.
    ///
    /// Overlap is defined as either place being a prefix of the other
    /// (including equality). This requires the same `local`; different locals
    /// never overlap regardless of projections. Projection equality is exact
    /// per variant.
    ///
    /// # Examples
    ///
    /// ```
    /// // x.a overlaps x.a          (equal)
    /// // x.a overlaps x.a.b        (prefix)
    /// // x.a does NOT overlap x.b  (sibling field)
    /// // x overlaps x.a            (bare local prefix)
    /// // x.a does NOT overlap y.a  (different local)
    /// ```
    pub fn overlaps(&self, other: &Place) -> bool {
        self.is_prefix_of(other) || other.is_prefix_of(self)
    }
}

/// Free-function alias for [`Place::overlaps`].
///
/// Exists for call-sites that prefer `places_overlap(&a, &b)` spelling;
/// borrow/move/init/drop helpers can use either form.
pub fn places_overlap(a: &Place, b: &Place) -> bool {
    a.overlaps(b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_chaining() {
        let p = Place::new("x").field("a").field("b");
        assert_eq!(p.local, "x");
        assert_eq!(
            p.projections,
            vec![
                Projection::Field("a".to_string()),
                Projection::Field("b".to_string())
            ]
        );
        // from_slice equivalence
        assert_eq!(p, Place::from_slice("x", &["a", "b"]));
        // empty slice
        assert_eq!(Place::from_slice("x", &[]), Place::new("x"));
        // single field via from_slice vs field
        assert_eq!(Place::from_slice("x", &["a"]), Place::new("x").field("a"));
    }

    #[test]
    fn test_parent_round_trip() {
        let p = Place::new("x").field("a").field("b");
        assert_eq!(p.parent(), Some(Place::new("x").field("a")));
        assert_eq!(
            Place::new("x").field("a").field("b").parent(),
            Some(Place::new("x").field("a"))
        );
        assert_eq!(Place::new("x").field("a").parent(), Some(Place::new("x")));
        assert_eq!(Place::new("x").parent(), None);
        // parent is inverse of field
        let base = Place::new("x").field("a");
        let extended = base.field("b");
        assert_eq!(extended.parent().as_ref(), Some(&base));
        // parent after deref
        let d = Place::new("p").deref();
        assert_eq!(d.parent(), Some(Place::new("p")));
        // from_slice parent
        let q = Place::from_slice("x", &["a", "b", "c"]);
        assert_eq!(q.parent(), Some(Place::from_slice("x", &["a", "b"])));
    }

    #[test]
    fn test_root_equality() {
        let p = Place::new("x").field("a");
        let q = Place::new("x").field("b");
        assert_eq!(p.root(), q.root());
        assert_eq!(p.root(), &"x".to_string());
        assert_eq!(Place::new("x").root(), &LocalId::from("x"));
        assert_ne!(Place::new("x").root(), Place::new("y").root());
        // root stable through projections
        let deep = Place::new("x").field("a").field("b").deref();
        assert_eq!(deep.root(), &"x".to_string());
        // from_slice root
        assert_eq!(Place::from_slice("x", &["a", "b"]).root(), &"x".to_string());
    }

    #[test]
    fn test_xa_vs_ya_distinction() {
        let xa = Place::new("x").field("a");
        let ya = Place::new("y").field("a");
        assert_ne!(xa, ya);
        assert_ne!(xa.local, ya.local);
        assert_ne!(xa.root(), ya.root());
        // same field name, different local → not equal
        assert_ne!(Place::from_slice("x", &["a"]), Place::from_slice("y", &["a"]));
        // same local, different field → not equal
        assert_ne!(Place::new("x").field("a"), Place::new("x").field("b"));
        // hash distinction via HashSet
        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(xa.clone());
        assert!(!set.contains(&ya));
        assert!(set.contains(&xa));
    }

    #[test]
    fn test_push_projection_and_deref() {
        let mut p = Place::new("x");
        p.push_projection(Projection::Field("a".to_string()));
        assert_eq!(p, Place::new("x").field("a"));
        p.push_projection(Projection::Deref);
        assert_eq!(p, Place::new("x").field("a").deref());
        // deref helper is pure (does not mutate original)
        let base = Place::new("p");
        let derefed = base.deref();
        assert_eq!(derefed.projections, vec![Projection::Deref]);
        assert!(base.projections.is_empty());
    }

    #[test]
    fn test_acceptance_example() {
        // Helpers correct for examples: Place::new("x").field("a").field("b").parent() == Some(Place::new("x").field("a"))
        assert_eq!(
            Place::new("x").field("a").field("b").parent(),
            Some(Place::new("x").field("a"))
        );
    }
    // -----------------------------------------------------------------
    // Phase 2 — overlap / prefix semantics
    // -----------------------------------------------------------------

    #[test]
    fn test_overlap_xa_vs_xa() {
        // spec: x.a vs x.a → overlap (equal)
        let xa1 = Place::new("x").field("a");
        let xa2 = Place::new("x").field("a");
        assert!(xa1.overlaps(&xa2));
        assert!(xa2.overlaps(&xa1));
        assert!(xa1.is_prefix_of(&xa2));
        assert!(xa2.is_prefix_of(&xa1));
        assert!(places_overlap(&xa1, &xa2));
        assert!(places_overlap(&xa2, &xa1));
    }

    #[test]
    fn test_overlap_xa_vs_xab() {
        // spec: x.a vs x.a.b → overlap (prefix)
        let xa = Place::new("x").field("a");
        let xab = Place::new("x").field("a").field("b");
        assert!(xa.overlaps(&xab));
        assert!(xab.overlaps(&xa));
        assert!(xa.is_prefix_of(&xab));
        assert!(!xab.is_prefix_of(&xa));
        assert!(places_overlap(&xa, &xab));
        // free function symmetry
        assert!(places_overlap(&xab, &xa));
    }

    #[test]
    fn test_no_overlap_xa_vs_xb() {
        // spec: x.a vs x.b → no overlap (sibling fields)
        let xa = Place::new("x").field("a");
        let xb = Place::new("x").field("b");
        assert!(!xa.overlaps(&xb));
        assert!(!xb.overlaps(&xa));
        assert!(!xa.is_prefix_of(&xb));
        assert!(!xb.is_prefix_of(&xa));
        assert!(!places_overlap(&xa, &xb));
    }

    #[test]
    fn test_overlap_x_vs_xa() {
        // spec: x vs x.a → overlap (bare local is prefix of projection)
        let x = Place::new("x");
        let xa = Place::new("x").field("a");
        assert!(x.overlaps(&xa));
        assert!(xa.overlaps(&x));
        assert!(x.is_prefix_of(&xa));
        assert!(!xa.is_prefix_of(&x));
        assert!(places_overlap(&x, &xa));
    }

    #[test]
    fn test_no_overlap_xa_vs_ya() {
        // spec: x.a vs y.a → no overlap (different local)
        let xa = Place::new("x").field("a");
        let ya = Place::new("y").field("a");
        assert!(!xa.overlaps(&ya));
        assert!(!ya.overlaps(&xa));
        assert!(!xa.is_prefix_of(&ya));
        assert!(!ya.is_prefix_of(&xa));
        assert!(!places_overlap(&xa, &ya));
        // bare locals different
        assert!(!Place::new("x").overlaps(&Place::new("y")));
        assert!(!Place::new("x").is_prefix_of(&Place::new("y")));
    }

    #[test]
    fn test_is_prefix_of_edge_cases() {
        // self is prefix of self
        let xab = Place::new("x").field("a").field("b");
        assert!(xab.is_prefix_of(&xab));
        assert!(xab.overlaps(&xab));
        // bare local vs bare local same
        let x = Place::new("x");
        assert!(x.is_prefix_of(&x));
        assert!(x.overlaps(&x));
        // longer not prefix of shorter
        let xa = Place::new("x").field("a");
        assert!(!xab.is_prefix_of(&xa));
        // empty prefix? x vs x.a.b
        assert!(x.is_prefix_of(&xab));
        // different projection kind at same depth → not prefix
        let mut p_field = Place::new("x");
        p_field.push_projection(Projection::Field("a".to_string()));
        let mut p_tuple = Place::new("x");
        p_tuple.push_projection(Projection::TupleField(0));
        assert!(!p_field.is_prefix_of(&p_tuple));
        assert!(!p_field.overlaps(&p_tuple));
    }

    #[test]
    fn test_deref_overlap_cases() {
        // *p vs *p → overlap (equal)
        let p_deref1 = Place::new("p").deref();
        let p_deref2 = Place::new("p").deref();
        assert!(p_deref1.overlaps(&p_deref2));
        assert!(p_deref1.is_prefix_of(&p_deref2));
        assert!(places_overlap(&p_deref1, &p_deref2));

        // *p vs *p.a → overlap (prefix with Deref)
        let p_deref_a = Place::new("p").deref().field("a");
        assert!(p_deref1.overlaps(&p_deref_a));
        assert!(p_deref1.is_prefix_of(&p_deref_a));
        assert!(!p_deref_a.is_prefix_of(&p_deref1));

        // p vs *p → overlap (bare local prefix)
        let p = Place::new("p");
        assert!(p.overlaps(&p_deref1));
        assert!(p.is_prefix_of(&p_deref1));
        assert!(!p_deref1.is_prefix_of(&p));

        // *p vs p.a → no overlap (Deref != Field at first projection)
        let pa = Place::new("p").field("a");
        assert!(!p_deref1.overlaps(&pa));
        assert!(!p_deref1.is_prefix_of(&pa));
        assert!(!pa.is_prefix_of(&p_deref1));

        // *p vs q deref (different local) → no overlap
        let q_deref = Place::new("q").deref();
        assert!(!p_deref1.overlaps(&q_deref));
        assert!(!p_deref1.is_prefix_of(&q_deref));

        // Deref sibling vs Deref divergent path: *p.a vs *p.b → no overlap
        let p_deref_a2 = Place::new("p").deref().field("a");
        let p_deref_b = Place::new("p").deref().field("b");
        assert!(!p_deref_a2.overlaps(&p_deref_b));

        // Chained deref: *p vs **p (p.deref().deref()) → prefix
        let pp = Place::new("p").deref().deref();
        assert!(p_deref1.overlaps(&pp));
        assert!(p_deref1.is_prefix_of(&pp));
        assert!(!pp.is_prefix_of(&p_deref1));
    }

    #[test]
    fn test_index_overlap_cases() {
        // v[i] vs v[i] → overlap (Index exact equality)
        let mut v_idx1 = Place::new("v");
        v_idx1.push_projection(Projection::Index);
        let mut v_idx2 = Place::new("v");
        v_idx2.push_projection(Projection::Index);
        assert!(v_idx1.overlaps(&v_idx2));
        assert!(v_idx1.is_prefix_of(&v_idx2));
        assert!(places_overlap(&v_idx1, &v_idx2));

        // v vs v[i] → overlap (prefix)
        let v = Place::new("v");
        assert!(v.overlaps(&v_idx1));
        assert!(v.is_prefix_of(&v_idx1));
        assert!(!v_idx1.is_prefix_of(&v));

        // v[i] vs v[i].a → overlap (prefix)
        let mut v_idx_a = v_idx1.clone();
        v_idx_a.push_projection(Projection::Field("a".to_string()));
        assert!(v_idx1.overlaps(&v_idx_a));
        assert!(v_idx1.is_prefix_of(&v_idx_a));
        assert!(!v_idx_a.is_prefix_of(&v_idx1));

        // v[i] vs v.a → no overlap (Index != Field)
        let va = Place::new("v").field("a");
        assert!(!v_idx1.overlaps(&va));
        assert!(!v_idx1.is_prefix_of(&va));

        // v[i] vs v[i] on different local w.a → no overlap
        let mut w_idx = Place::new("w");
        w_idx.push_projection(Projection::Index);
        assert!(!v_idx1.overlaps(&w_idx));

        // v[i] sibling divergence: v[i].a vs v[i].b → no overlap
        let mut v_idx_b = v_idx1.clone();
        v_idx_b.push_projection(Projection::Field("b".to_string()));
        assert!(!v_idx_a.overlaps(&v_idx_b));

        // Index after field: x.a[i] vs x.a[i] → overlap, vs x.a → prefix
        let mut xa_idx = Place::new("x").field("a");
        xa_idx.push_projection(Projection::Index);
        let xa_idx2 = xa_idx.clone();
        assert!(xa_idx.overlaps(&xa_idx2));
        let xa = Place::new("x").field("a");
        assert!(xa.overlaps(&xa_idx));
        assert!(xa.is_prefix_of(&xa_idx));
    }

    #[test]
    fn test_tuple_field_overlap_cases() {
        // x.0 vs x.0 → overlap
        let mut x0_1 = Place::new("x");
        x0_1.push_projection(Projection::TupleField(0));
        let mut x0_2 = Place::new("x");
        x0_2.push_projection(Projection::TupleField(0));
        assert!(x0_1.overlaps(&x0_2));
        assert!(x0_1.is_prefix_of(&x0_2));

        // x.0 vs x.1 → no overlap (different tuple index)
        let mut x1 = Place::new("x");
        x1.push_projection(Projection::TupleField(1));
        assert!(!x0_1.overlaps(&x1));
        assert!(!x0_1.is_prefix_of(&x1));

        // x.0 vs x.0.a → overlap (prefix)
        let mut x0_a = x0_1.clone();
        x0_a.push_projection(Projection::Field("a".to_string()));
        assert!(x0_1.overlaps(&x0_a));
        assert!(x0_1.is_prefix_of(&x0_a));

        // x vs x.0 → overlap (bare prefix)
        let x = Place::new("x");
        assert!(x.overlaps(&x0_1));
        assert!(x.is_prefix_of(&x0_1));

        // x.0 vs x.a → no overlap (TupleField != Field)
        let xa = Place::new("x").field("a");
        assert!(!x0_1.overlaps(&xa));

        // x.0 vs y.0 → no overlap (different local)
        let mut y0 = Place::new("y");
        y0.push_projection(Projection::TupleField(0));
        assert!(!x0_1.overlaps(&y0));
    }

    #[test]
    fn test_mixed_projection_prefix_no_overlap() {
        // Field vs TupleField sibling already covered, also Deref vs Index
        let mut with_deref = Place::new("x");
        with_deref.push_projection(Projection::Deref);
        let mut with_index = Place::new("x");
        with_index.push_projection(Projection::Index);
        assert!(!with_deref.overlaps(&with_index));

        // Deep prefix vs sibling divergence
        let xab = Place::new("x").field("a").field("b");
        let xac = Place::new("x").field("a").field("c");
        assert!(!xab.overlaps(&xac));
        assert!(!xab.is_prefix_of(&xac));

        // x.a.b vs x.a.b.c → overlap (extension)
        let xabc = xab.clone().field("c");
        assert!(xab.overlaps(&xabc));
        assert!(xab.is_prefix_of(&xabc));

        // x.a.b.c vs x.a.b.c → overlap (equal deep)
        let xabc2 = Place::new("x").field("a").field("b").field("c");
        assert!(xabc.overlaps(&xabc2));
    }

    #[test]
    fn test_places_overlap_alias_symmetry() {
        let a = Place::new("x").field("a");
        let b = Place::new("x").field("a").field("b");
        // alias equals method both directions
        assert_eq!(places_overlap(&a, &b), a.overlaps(&b));
        assert_eq!(places_overlap(&b, &a), b.overlaps(&a));
        assert_eq!(places_overlap(&a, &b), places_overlap(&b, &a));
        // non-overlap symmetry
        let c = Place::new("x").field("b");
        assert_eq!(places_overlap(&a, &c), a.overlaps(&c));
        assert!(!places_overlap(&a, &c));
    }

}
