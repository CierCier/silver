//! MovePathTree — field-granular init states (`InitState::{Init,Partial,Uninit}`).
//! Why: one `MovePath` per `Place`; `x` root with children `x.a`, `x.a.b`; siblings disjoint.
//! Example: `move x.a` → `x=Partial`, `x.a=Uninit`, `x.b` stays `Init`.

use rustc_hash::FxHashMap;

use crate::semantic::place::{LocalId, Place};

/// Init state for a `MovePath` node.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum InitState {
    Initialized,          // live
    Uninitialized,        // moved / never init
    PartiallyInitialized, // descendant uninit, sibling live (e.g. `x` after `move x.a`)
}

impl Default for InitState {
    fn default() -> Self {
        Self::Initialized
    }
}

/// One `MovePath` node — exactly one `Place` + children for direct field projections.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MovePath {
    /// The `Place` this node represents (e.g. `x`, `x.a`, `x.a.b`).
    pub place: Place,
    /// Initialization state of this place.
    pub state: InitState,
    /// Direct children — one per immediate field projection.
    pub children: Vec<MovePath>,
}

impl MovePath {
    /// New node `Initialized`, no children.
    pub fn new(place: Place) -> Self {
        Self {
            place,
            state: InitState::Initialized,
            children: Vec::new(),
        }
    }

    /// New root for bare local `x`.
    pub fn new_local(local: impl Into<LocalId>) -> Self {
        Self::new(Place::new(local))
    }

    /// New node with explicit state.
    pub fn with_state(place: Place, state: InitState) -> Self {
        Self {
            place,
            state,
            children: Vec::new(),
        }
    }

    pub fn state(&self) -> InitState {
        self.state
    }

    /// Set state of this node only (no cascade).
    pub fn set_state(&mut self, state: InitState) {
        self.state = state;
    }

    pub fn is_initialized(&self) -> bool {
        self.state == InitState::Initialized
    }

    pub fn is_uninitialized(&self) -> bool {
        self.state == InitState::Uninitialized
    }

    /// Find by exact `Place` in subtree (depth-first).
    pub fn find(&self, place: &Place) -> Option<&MovePath> {
        if &self.place == place {
            return Some(self);
        }
        for child in &self.children {
            if let Some(found) = child.find(place) {
                return Some(found);
            }
        }
        None
    }

    /// Mutable variant of `find`.
    pub fn find_mut(&mut self, place: &Place) -> Option<&mut MovePath> {
        if &self.place == place {
            return Some(self);
        }
        for child in &mut self.children {
            if let Some(found) = child.find_mut(place) {
                return Some(found);
            }
        }
        None
    }

    /// Find direct child by exact `Place`.
    pub fn find_child(&self, place: &Place) -> Option<&MovePath> {
        self.children.iter().find(|c| &c.place == place)
    }

    /// Find direct child mutably.
    pub fn find_child_mut(&mut self, place: &Place) -> Option<&mut MovePath> {
        self.children.iter_mut().find(|c| &c.place == place)
    }

    /// Add child; replace if same `Place` exists, return old.
    pub fn add_child(&mut self, child: MovePath) -> Option<MovePath> {
        if let Some(existing) = self.find_child_mut(&child.place) {
            let old = std::mem::replace(existing, child);
            return Some(old);
        }
        self.children.push(child);
        None
    }

    /// Ensure direct child for `place` exists (`Initialized` if missing).
    pub fn get_or_create_child(&mut self, place: Place) -> &mut MovePath {
        let idx = self.children.iter().position(|c| c.place == place);
        if let Some(i) = idx {
            return &mut self.children[i];
        }
        self.children.push(MovePath::new(place));
        self.children.last_mut().unwrap()
    }

    /// Insert `place` creating missing intermediates (`Initialized` if new, preserve existing).
    /// Returns node for `place`, or `None` if not descendant of `self.place`.
    pub fn insert(&mut self, place: Place) -> Option<&mut MovePath> {
        if self.place == place {
            return Some(self);
        }
        if !self.place.is_prefix_of(&place) {
            return None;
        }
        // Build the chain of prefixes from self+1 projection up to `place`.
        // Walk/create child by child.
        let depth_self = self.place.projections.len();
        let depth_target = place.projections.len();
        let mut current = self;
        for len in (depth_self + 1)..=depth_target {
            let prefix = Place {
                local: place.local.clone(),
                projections: place.projections[..len].to_vec(),
            };
            current = current.get_or_create_child(prefix);
        }
        Some(current)
    }
}

/// Forest keyed by root local (`x`, `y` separate roots).
#[derive(Clone, Debug, Default)]
pub struct MovePathTree {
    /// Roots indexed by `LocalId` (the `Place::local` of each root `MovePath`).
    roots: FxHashMap<LocalId, MovePath>,
}

impl MovePathTree {
    /// Create an empty tree.
    pub fn new() -> Self {
        Self {
            roots: FxHashMap::default(),
        }
    }

    /// Create a tree with a single root local `x` (`Place::new(local)`).
    pub fn new_with_local(local: impl Into<LocalId>) -> Self {
        let mut t = Self::new();
        t.get_or_create_root(local);
        t
    }

    /// Number of roots (distinct locals).
    pub fn len(&self) -> usize {
        self.roots.len()
    }

    /// `true` if no roots.
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    /// Get immutable root for `local` if present.
    pub fn root(&self, local: &str) -> Option<&MovePath> {
        self.roots.get(local)
    }

    /// Get mutable root for `local` if present.
    pub fn root_mut(&mut self, local: &str) -> Option<&mut MovePath> {
        self.roots.get_mut(local)
    }

    /// Ensure root for `local` exists (`Initialized` if missing).
    pub fn get_or_create_root(&mut self, local: impl Into<LocalId>) -> &mut MovePath {
        let local: LocalId = local.into();
        self.roots
            .entry(local.clone())
            .or_insert_with(|| MovePath::new(Place::new(local)))
    }

    /// Insert `Place`, creating missing root/intermediates (preserve existing states).
    pub fn insert(&mut self, place: Place) -> &mut MovePath {
        let local = place.local.clone();
        let root = self
            .roots
            .entry(local.clone())
            .or_insert_with(|| MovePath::new(Place::new(local)));
        root.insert(place)
            .expect("tree root and place have the same local")
    }

    /// Find by `Place` in forest (exact equality).
    pub fn find(&self, place: &Place) -> Option<&MovePath> {
        let root = self.roots.get(&place.local)?;
        root.find(place)
    }

    /// Mutable find.
    pub fn find_mut(&mut self, place: &Place) -> Option<&mut MovePath> {
        let root = self.roots.get_mut(&place.local)?;
        root.find_mut(place)
    }

    /// Set the `InitState` of `place` if it exists. Returns `true` iff the node was found.
    pub fn set_state(&mut self, place: &Place, state: InitState) -> bool {
        if let Some(node) = self.find_mut(place) {
            node.set_state(state);
            true
        } else {
            false
        }
    }

    /// Returns `true` iff `place` exists and is `Initialized`.
    /// If the place is not in the tree, returns `false` (conservative: not tracked).
    pub fn is_initialized(&self, place: &Place) -> bool {
        self.find(place)
            .map(|n| n.is_initialized())
            .unwrap_or(false)
    }

    /// Iterate over roots.
    pub fn roots(&self) -> impl Iterator<Item = &MovePath> {
        self.roots.values()
    }

    /// Iterate mutably over roots.
    pub fn roots_mut(&mut self) -> impl Iterator<Item = &mut MovePath> {
        self.roots.values_mut()
    }
}
