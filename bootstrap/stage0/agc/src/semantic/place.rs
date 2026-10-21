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
