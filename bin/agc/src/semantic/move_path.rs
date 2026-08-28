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
        let mut cur: *mut MovePath = self as *mut _;
        for len in (depth_self + 1)..=depth_target {
            let prefix = Place {
                local: place.local.clone(),
                projections: place.projections[..len].to_vec(),
            };
            // SAFETY: cur points inside `self` tree; we only hold one `&mut` at a time.
            let cur_ref = unsafe { &mut *cur };
            let child_idx = cur_ref.children.iter().position(|c| c.place == prefix);
            let next: *mut MovePath = if let Some(idx) = child_idx {
                &mut cur_ref.children[idx] as *mut _
            } else {
                cur_ref.children.push(MovePath::new(prefix));
                let idx = cur_ref.children.len() - 1;
                &mut cur_ref.children[idx] as *mut _
            };
            cur = next;
        }
        unsafe { Some(&mut *cur) }
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
        if root.place == place {
            return root;
        }
        // SAFETY: we split the insert walk to avoid double-borrow.
        // `root.insert(place)` would need ownership; we reuse MovePath::insert logic inline.
        let depth_target = place.projections.len();
        let mut cur: *mut MovePath = root as *mut _;
        for len in 1..=depth_target {
            let prefix = Place {
                local: place.local.clone(),
                projections: place.projections[..len].to_vec(),
            };
            let cur_ref = unsafe { &mut *cur };
            // Skip len == 0 (root) — already handled.
            let child_idx = cur_ref.children.iter().position(|c| c.place == prefix);
            let next: *mut MovePath = if let Some(idx) = child_idx {
                &mut cur_ref.children[idx] as *mut _
            } else {
                cur_ref.children.push(MovePath::new(prefix));
                let idx = cur_ref.children.len() - 1;
                &mut cur_ref.children[idx] as *mut _
            };
            cur = next;
        }
        unsafe { &mut *cur }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::place::Place;

    #[test]
    fn test_init_state_default() {
        assert_eq!(InitState::default(), InitState::Initialized);
        let p = MovePath::new(Place::new("x"));
        assert!(p.is_initialized());
        assert!(!p.is_uninitialized());
        assert_eq!(p.state(), InitState::Initialized);
    }

    #[test]
    fn test_move_path_new_local() {
        let p = MovePath::new_local("x");
        assert_eq!(p.place, Place::new("x"));
        assert!(p.children.is_empty());
        assert_eq!(p.state, InitState::Initialized);

        let q = MovePath::new(Place::new("y").field("a"));
        assert_eq!(q.place, Place::new("y").field("a"));
    }

    #[test]
    fn test_move_path_set_state_and_is_initialized() {
        let mut p = MovePath::new_local("x");
        assert!(p.is_initialized());
        p.set_state(InitState::Uninitialized);
        assert!(!p.is_initialized());
        assert!(p.is_uninitialized());
        assert_eq!(p.state(), InitState::Uninitialized);
        p.set_state(InitState::PartiallyInitialized);
        assert!(!p.is_initialized());
        assert_eq!(p.state(), InitState::PartiallyInitialized);
    }

    #[test]
    fn test_move_path_find_self() {
        let p = MovePath::new_local("x");
        assert!(p.find(&Place::new("x")).is_some());
        assert!(p.find(&Place::new("y")).is_none());
        assert!(p.find(&Place::new("x").field("a")).is_none());
    }

    #[test]
    fn test_tree_shape_x_a_b_c_d() {
        // Build x { a { b, c }, d } as described in acceptance.
        // Using MovePathTree::insert which creates intermediates.
        let mut tree = MovePathTree::new();

        // Insert in arbitrary order to ensure intermediates are auto-created.
        let x = Place::new("x");
        let xa = Place::from_slice("x", &["a"]);
        let xab = Place::from_slice("x", &["a", "b"]);
        let xac = Place::from_slice("x", &["a", "c"]);
        let xd = Place::new("x").field("d");

        tree.insert(x.clone());
        tree.insert(xa.clone());
        tree.insert(xab.clone());
        tree.insert(xac.clone());
        tree.insert(xd.clone());

        // Roots
        assert_eq!(tree.len(), 1);
        let root = tree.root("x").expect("x root");
        assert_eq!(root.place, x);
        assert_eq!(root.children.len(), 2, "x should have children a and d");

        // Check a and d exist as direct children of x
        let node_a = root.find_child(&xa).expect("x.a child");
        let node_d = root.find_child(&xd).expect("x.d child");
        assert_eq!(node_a.place, xa);
        assert_eq!(node_d.place, xd);
        assert!(node_d.children.is_empty(), "d is leaf");

        // a should have b and c
        assert_eq!(node_a.children.len(), 2, "a should have b and c");
        let node_b = node_a.find_child(&xab).expect("x.a.b");
        let node_c = node_a.find_child(&xac).expect("x.a.c");
        assert_eq!(node_b.place, xab);
        assert_eq!(node_c.place, xac);
        assert!(node_b.children.is_empty());
        assert!(node_c.children.is_empty());

        // Tree-level find for every place
        assert!(tree.find(&x).is_some());
        assert!(tree.find(&xa).is_some());
        assert!(tree.find(&xab).is_some());
        assert!(tree.find(&xac).is_some());
        assert!(tree.find(&xd).is_some());

        // Non-existent sibling / deep child
        let xae = Place::from_slice("x", &["a", "e"]);
        assert!(tree.find(&xae).is_none());
        assert!(tree.find(&Place::new("x").field("z")).is_none());
        assert!(tree.find(&Place::new("y")).is_none());

        // is_initialized helpers
        assert!(tree.is_initialized(&x));
        assert!(tree.is_initialized(&xa));
        assert!(tree.is_initialized(&xab));
        assert!(tree.is_initialized(&xd));

        // set_state on a leaf
        assert!(tree.set_state(&xab, InitState::Uninitialized));
        assert!(!tree.is_initialized(&xab));
        assert!(tree.find(&xab).unwrap().is_uninitialized());
        // siblings unaffected
        assert!(tree.is_initialized(&xac));
        assert!(tree.is_initialized(&xd));
        // parent still initialized unless explicitly set to PartiallyInitialized
        assert!(tree.is_initialized(&xa));

        // Set parent to PartiallyInitialized to model partial move of x.a
        assert!(tree.set_state(&xa, InitState::PartiallyInitialized));
        assert_eq!(
            tree.find(&xa).unwrap().state(),
            InitState::PartiallyInitialized
        );
        assert!(!tree.find(&xa).unwrap().is_initialized());

        // find_mut mutation: reinitialize b
        {
            let b_mut = tree.find_mut(&xab).expect("x.a.b mut");
            b_mut.set_state(InitState::Initialized);
        }
        assert!(tree.is_initialized(&xab));

        // MovePath-level find (subtree of a)
        let subtree_a = tree.find(&xa).unwrap();
        assert!(subtree_a.find(&xab).is_some());
        assert!(subtree_a.find(&xd).is_none(), "x.d not under x.a");
    }

    #[test]
    fn test_tree_insert_out_of_order_and_dedup() {
        // Insert leaf first — intermediates should be synthesized.
        let mut tree = MovePathTree::new();
        let xab = Place::from_slice("x", &["a", "b"]);
        tree.insert(xab.clone());

        // x and x.a should now exist even though never directly inserted.
        assert!(tree.find(&Place::new("x")).is_some());
        assert!(tree.find(&Place::from_slice("x", &["a"])).is_some());
        assert!(tree.find(&xab).is_some());

        let len_before = tree.find(&Place::new("x")).unwrap().children.len();
        // Re-inserting same place must not duplicate nodes.
        tree.insert(xab.clone());
        let len_after = tree.find(&Place::new("x")).unwrap().children.len();
        assert_eq!(len_before, len_after);

        let xa = Place::from_slice("x", &["a"]);
        let a_children_before = tree.find(&xa).unwrap().children.len();
        tree.insert(xa.clone());
        let a_children_after = tree.find(&xa).unwrap().children.len();
        assert_eq!(a_children_before, a_children_after);
    }

    #[test]
    fn test_move_path_insert_helper() {
        // Direct MovePath::insert (subtree) helper.
        let mut root = MovePath::new_local("x");
        let xab = Place::from_slice("x", &["a", "b"]);
        let inserted = root.insert(xab.clone()).expect("insert x.a.b");
        assert_eq!(inserted.place, xab);

        assert!(root.find(&Place::from_slice("x", &["a"])).is_some());
        assert!(root.find(&xab).is_some());
        // Non-descendant (different local) returns None
        assert!(root
            .insert(Place::new("y").field("a"))
            .is_none());
        // Sibling under same root
        let xd = Place::new("x").field("d");
        assert!(root.insert(xd.clone()).is_some());
        assert!(root.find(&xd).is_some());
        assert_eq!(root.children.len(), 2);
    }

    #[test]
    fn test_forest_multiple_roots_and_set_state_missing() {
        let mut tree = MovePathTree::new();
        tree.insert(Place::new("x").field("a"));
        tree.insert(Place::new("y").field("b"));

        assert_eq!(tree.len(), 2);
        assert!(tree.find(&Place::new("x").field("a")).is_some());
        assert!(tree.find(&Place::new("y").field("b")).is_some());
        assert!(tree.find(&Place::new("z")).is_none());

        // set_state on missing place returns false
        assert!(!tree.set_state(&Place::new("z"), InitState::Uninitialized));
        assert!(!tree.set_state(
            &Place::from_slice("x", &["a", "c"]),
            InitState::Uninitialized
        ));
    }

    #[test]
    fn test_children_add_and_get_or_create() {
        let mut root = MovePath::new_local("x");
        let xa = Place::from_slice("x", &["a"]);
        let xb = Place::new("x").field("b");

        // add_child
        root.add_child(MovePath::new(xa.clone()));
        assert_eq!(root.children.len(), 1);
        // get_or_create on existing returns existing
        let existing = root.get_or_create_child(xa.clone());
        assert_eq!(existing.place, xa);
        assert_eq!(root.children.len(), 1);
        // get_or_create on missing creates
        root.get_or_create_child(xb.clone());
        assert_eq!(root.children.len(), 2);
        assert!(root.find(&xb).is_some());
    }
}
