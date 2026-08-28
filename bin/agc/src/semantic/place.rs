//! Structural `Place` representation — Phase 1 foundation.
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
}
