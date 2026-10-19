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
    fn projection_builders_preserve_order_and_leave_the_base_unchanged() {
        let base = Place::new("x");
        let projected = base.field("a").index().deref();
        assert_eq!(projected.local, "x");
        assert_eq!(
            projected.projections,
            vec![
                Projection::Field("a".into()),
                Projection::Index,
                Projection::Deref
            ]
        );
        assert!(base.projections.is_empty());
        assert_eq!(Place::from_slice("x", &[]), base);
        assert_eq!(
            Place::from_slice("x", &["a", "b"]),
            base.field("a").field("b")
        );
        let mut pushed = base.field("a");
        pushed.push_projection(Projection::Index);
        assert_eq!(pushed, base.field("a").index());
    }

    #[test]
    fn parent_removes_one_projection_and_root_survives() {
        let base = Place::new("x").field("a");
        for projection in [
            Projection::Field("b".into()),
            Projection::TupleField(0),
            Projection::Index,
            Projection::Deref,
        ] {
            let mut child = base.clone();
            child.push_projection(projection);
            assert_eq!(child.parent(), Some(base.clone()));
            assert_eq!(child.root(), "x");
        }
        assert_eq!(base.parent(), Some(Place::new("x")));
        assert_eq!(Place::new("x").parent(), None);
    }

    #[test]
    fn overlap_distinguishes_roots_siblings_and_prefix_direction() {
        let x = Place::new("x");
        let a = x.field("a");
        let ab = a.field("b");
        let mut tuple = x.clone();
        tuple.push_projection(Projection::TupleField(0));
        let cases = [
            (x.clone(), x.clone(), true, true),
            (a.clone(), a.clone(), true, true),
            (x.clone(), ab.clone(), true, false),
            (a.clone(), ab.clone(), true, false),
            (ab.clone(), ab.field("c"), true, false),
            (a.clone(), x.field("b"), false, false),
            (ab.clone(), a.field("c"), false, false),
            (x.clone(), Place::new("y"), false, false),
            (a.clone(), Place::new("y").field("a"), false, false),
            (x.clone(), x.deref(), true, false),
            (x.deref(), x.deref().field("a"), true, false),
            (x.deref(), x.deref().deref(), true, false),
            (x.deref().field("a"), x.deref().field("b"), false, false),
            (x.deref(), Place::new("y").deref(), false, false),
            (x.clone(), x.index(), true, false),
            (x.index(), x.index(), true, true),
            (x.index(), x.index().field("a"), true, false),
            (x.index().field("a"), x.index().field("b"), false, false),
            (x.index(), Place::new("y").index(), false, false),
            (a.clone(), a.index(), true, false),
            (x.clone(), tuple.clone(), true, false),
            (tuple.clone(), tuple.field("a"), true, false),
        ];
        for (left, right, forward, reverse) in cases {
            assert_eq!(left.is_prefix_of(&right), forward, "{left:?}, {right:?}");
            assert_eq!(right.is_prefix_of(&left), reverse, "{right:?}, {left:?}");
            let overlap = forward || reverse;
            assert_eq!(left.overlaps(&right), overlap, "{left:?}, {right:?}");
            assert_eq!(right.overlaps(&left), overlap, "{right:?}, {left:?}");
            assert_eq!(places_overlap(&left, &right), overlap);
        }
    }

    #[test]
    fn distinct_projection_kinds_and_tuple_fields_do_not_alias() {
        let projections = [
            Projection::Field("a".into()),
            Projection::Field("b".into()),
            Projection::TupleField(0),
            Projection::TupleField(1),
            Projection::Index,
            Projection::Deref,
        ];
        for (i, left) in projections.iter().enumerate() {
            for (j, right) in projections.iter().enumerate() {
                let mut a = Place::new("x");
                let mut b = Place::new("x");
                a.push_projection(left.clone());
                b.push_projection(right.clone());
                assert_eq!(a.is_prefix_of(&b), i == j, "{a:?}, {b:?}");
                assert_eq!(a.overlaps(&b), i == j, "{a:?}, {b:?}");
            }
        }
    }
}
