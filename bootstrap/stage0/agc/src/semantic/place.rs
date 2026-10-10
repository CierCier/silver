//! Structural places shared by borrow, move, init, and drop checks.
//! Why: projections provide field-aware overlap without string paths.
//!
//! Invariants (one-line):
//! - Overlap = `a.is_prefix_of(b) || b.is_prefix_of(a)` (same local + prefix projections)
//! - Different locals never overlap; sibling fields/indices disjoint; reflexive + symmetric
//! - `Index`/`Deref` compare by variant equality; `Index == Index` so all `v[i]` overlap
//!
/// Local variable identifier — root of a place (String today, may become interned SymbolId).
pub type LocalId = String;

/// Field identifier — `Projection::Field` payload (String today, may become interned).
pub type FieldId = String;

/// Structural place `local + projections` (e.g. `x`, `x.a.b`, `x.0`, `*p`, `v[i]`).
/// Structural equality/hashing: `x.a == x.a`, `x.a != x.b`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Place {
    /// Root local variable, e.g. `x` in `x.foo.bar`.
    pub local: LocalId,
    /// Projections applied left-to-right, e.g. `[Field("foo"), Field("bar")]`.
    pub projections: Vec<Projection>,
}
/// Place projection — structural, not string-based.
/// `Field`/`TupleField` exact equality; `Index` opaque (all `v[i]` equal, conservative).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Projection {
    Field(FieldId),
    TupleField(usize),
    Index, // `v[i]` — value opaque, `v[i]` overlaps `v[j]`
    Deref,
}

impl Place {
    /// Sentinel empty place.
    pub const EMPTY: Place = Place {
        local: String::new(),
        projections: Vec::new(),
    };

    /// Bare local `x` (no projections).
    pub fn new(local: impl Into<LocalId>) -> Self {
        Self {
            local: local.into(),
            projections: Vec::new(),
        }
    }
}
impl Place {
    /// Root local.
    pub fn root(&self) -> &LocalId {
        &self.local
    }

    /// New place with `Field(field)` appended.
    pub fn field(&self, field: &str) -> Place {
        let mut p = self.clone();
        p.projections.push(Projection::Field(field.to_string()));
        p
    }

    /// New place with `Deref` appended.
    pub fn deref(&self) -> Place {
        let mut p = self.clone();
        p.projections.push(Projection::Deref);
        p
    }

    /// New place with `Index` appended — conservative: all `v[i]` map to same `Index`.
    pub fn index(&self) -> Place {
        let mut p = self.clone();
        p.projections.push(Projection::Index);
        p
    }

    /// Parent by popping one projection, or `None` if bare local.
    pub fn parent(&self) -> Option<Place> {
        if self.projections.is_empty() {
            None
        } else {
            let mut p = self.clone();
            p.projections.pop();
            Some(p)
        }
    }
    pub fn push_projection(&mut self, p: Projection) {
        self.projections.push(p);
    }

    /// Build from local + field names (test helper).
    pub fn from_slice(local: impl Into<LocalId>, fields: &[&str]) -> Self {
        let mut place = Self::new(local);
        for f in fields {
            place.projections.push(Projection::Field((*f).to_string()));
        }
        place
    }
}

impl Place {
    /// Shared prefix check; Index remains conservative.
    pub fn is_prefix_of(&self, other: &Place) -> bool {
        if self.local != other.local {
            return false;
        }
        if self.projections.len() > other.projections.len() {
            return false;
        }
        self.projections[..] == other.projections[..self.projections.len()]
    }

    /// Symmetric overlap used by ownership checks.
    pub fn overlaps(&self, other: &Place) -> bool {
        self.is_prefix_of(other) || other.is_prefix_of(self)
    }
}

/// Alias for `Place::overlaps`.
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
        assert_ne!(
            Place::from_slice("x", &["a"]),
            Place::from_slice("y", &["a"])
        );
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

    // -----------------------------------------------------------------
    // Phase 11 — Dynamic Index place (v[i]) — acceptance tests
    // -----------------------------------------------------------------

    #[test]
    fn test_vi_place_creation() {
        // v[i] Place creation: Place { local: v, projections: [Index] } for v[i].
        // Via both helper and direct push.
        let vi_via_helper = Place::new("v").index();
        let mut vi_via_push = Place::new("v");
        vi_via_push.push_projection(Projection::Index);
        assert_eq!(vi_via_helper, vi_via_push);
        assert_eq!(vi_via_helper.local, "v");
        assert_eq!(vi_via_helper.projections, vec![Projection::Index]);
        // Distinct index expressions map to same Place (index-insensitive)
        let vj = Place::new("v").index();
        assert_eq!(vi_via_helper, vj);
        // Index after field: x.a[i]
        let mut xa_idx = Place::new("x").field("a");
        xa_idx.push_projection(Projection::Index);
        assert_eq!(xa_idx, Place::new("x").field("a").index());
        // Parent of v[i] is v
        assert_eq!(vi_via_helper.parent(), Some(Place::new("v")));
        assert_eq!(vi_via_helper.root(), &"v".to_string());
    }

    #[test]
    fn test_v_vs_vi_overlap() {
        // v vs v[i] overlap — Phase 11: parent overlaps dynamic index (prefix)
        let v = Place::new("v");
        let vi = Place::new("v").index();
        // v is prefix of v[i]
        assert!(v.is_prefix_of(&vi));
        assert!(!vi.is_prefix_of(&v));
        assert!(v.overlaps(&vi));
        assert!(vi.overlaps(&v));
        assert!(places_overlap(&v, &vi));
        assert!(places_overlap(&vi, &v));
        // v[i].field also overlaps v
        let vi_a = vi.clone().field("a");
        assert!(v.overlaps(&vi_a));
        assert!(vi.overlaps(&vi_a));
        assert!(vi.is_prefix_of(&vi_a));
    }

    #[test]
    fn test_vi_vs_vj_overlap_conservative() {
        // v[i] vs v[j] overlap (conservative, index-insensitive) — Phase 11 limitation.
        // Both v[i] and v[j] are Place { local:"v", [Index] } for any i,j, so they
        // are equal and overlap. Documented as conservative: distinct runtime indices
        // *may* be disjoint, but we treat them as overlapping for soundness.
        let vi = Place::new("v").index();
        let vj = Place::new("v").index();
        // Simulate two different index expressions (i vs j) — same Place
        let mut vi2 = Place::new("v");
        vi2.push_projection(Projection::Index);
        let mut vj2 = Place::new("v");
        vj2.push_projection(Projection::Index);
        assert_eq!(vi, vj);
        assert_eq!(vi2, vj2);
        assert!(vi.overlaps(&vj));
        assert!(vj.overlaps(&vi));
        assert!(vi.is_prefix_of(&vj));
        assert!(vj.is_prefix_of(&vi));
        assert!(places_overlap(&vi, &vj));
        assert!(places_overlap(&vi2, &vj2));
        // v[i] vs v[j] with trailing field: v[i].a vs v[j].a also overlap (same Index prefix)
        let vi_a = vi.clone().field("a");
        let vj_a = vj.clone().field("a");
        assert!(vi_a.overlaps(&vj_a));
        assert!(places_overlap(&vi_a, &vj_a));
        // v[i] vs v[j] still considered overlapping even though runtime values differ — conservative
        // Contrast: v[i] vs v.a is disjoint (Index != Field)
        let va = Place::new("v").field("a");
        assert!(!vi.overlaps(&va));
    }
}
