//! Definite-initialization and move checking.
//! `Place` + `MovePathTree` preserve field-level ownership; `TypeProperties`
//! distinguishes implicit Copy from ownership-transferring Move.
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::module_artifact::{ExportKind, ModuleArtifact};
use crate::parser::ast;
use crate::semantic::init;
use crate::semantic::move_path::{InitState, MovePathTree};
use crate::semantic::place::{Place, Projection};
use crate::semantic::typeck::{CallSiteKey, MethodCallKey};
use rustc_hash::{FxHashMap, FxHashSet};

// Phase 4 scaffolding (parallel, not yet hot): `Place`/`MovePathTree`/`InitOp`
// mirror `semantic::init`. String logic stays authoritative; see `semantic::init`
// and `semantic::move_path` for the tree.

/// Definite-init op on a Place (Phase 4 scaffolding, mirrors `semantic::init::InitOp`).
#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum InitOp {
    MoveOut,    // `move x.a` — destructive
    CopyFrom,   // `i64 b = x.a` — non-destructive Copy
    Initialize, // `x.a = v`
    Read,       // pure read
}

/// Source transition for `let b = <src>` (plus `initialize(b)` on dest).
/// `is_copy` → `CopyFrom`, else `MoveOut`. TODO(Phase 5): `is_copy` from `type_properties::is_copy`.
#[allow(dead_code)]
fn transition_for_assign(place: &Place, is_copy: bool) -> InitOp {
    let _ = place;
    if is_copy {
        InitOp::CopyFrom
    } else {
        InitOp::MoveOut
    }
}

/// `move <place>` → `MoveOut`.
#[allow(dead_code)]
fn transition_for_move(place: &Place) -> InitOp {
    let _ = place;
    InitOp::MoveOut
}

/// `initialize(<place>)` (`x.a = v`).
#[allow(dead_code)]
fn transition_for_initialize(place: &Place) -> InitOp {
    let _ = place;
    InitOp::Initialize
}

/// One move-check diagnostic; same shape as `typeck::TypeError` with optional
/// secondary note pointing to the earlier move origin.
#[derive(Debug, Clone)]
pub struct MoveError {
    pub message: String,
    pub span: Span,
    pub note_span: Option<Span>,
    pub note_message: Option<String>,
}

/// Per-variable moved-ness: `level` for whole value + `MovePathTree<Place>` for fields.
#[derive(Debug, Clone, Default)]
pub struct VarState {
    /// `0=Live/Init`, `1=PartiallyMoved/Partial`, `2=FullyMoved/Uninit`.
    pub level: u8,
    pub move_span: Option<Span>,
    pub move_reason: Option<&'static str>,
    pub moved_fields: FxHashMap<String, (Span, &'static str)>,
    pub tree: MovePathTree,
    pub place_spans: FxHashMap<Place, (Span, &'static str)>,
}

fn place_from_root_and_path(root: &str, path: &str) -> Place {
    if path.is_empty() {
        Place::new(root)
    } else {
        let fields: Vec<&str> = path.split('.').collect();
        Place::from_slice(root, &fields)
    }
}

/// Dotted path without local (drops Index/Deref) — for legacy display.
fn place_to_path_str(place: &Place) -> String {
    place
        .projections
        .iter()
        .filter_map(|p| match p {
            Projection::Field(f) => Some(f.clone()),
            Projection::TupleField(i) => Some(i.to_string()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join(".")
}

/// All prefixes of `place` from root to itself (inclusive).
fn place_prefixes(place: &Place) -> Vec<Place> {
    let mut out = Vec::with_capacity(place.projections.len() + 1);
    for len in 0..=place.projections.len() {
        out.push(Place {
            local: place.local.clone(),
            projections: place.projections[..len].to_vec(),
        });
    }
    out
}

impl VarState {
    pub fn new_live() -> Self {
        Self {
            level: 0,
            move_span: None,
            move_reason: None,
            moved_fields: FxHashMap::default(),
            tree: MovePathTree::new(),
            place_spans: FxHashMap::default(),
        }
    }

    pub fn mark_moved(&mut self, span: Span, reason: &'static str) {
        self.level = 2;
        self.move_span = Some(span);
        self.move_reason = Some(reason);
        self.moved_fields.clear();
        self.place_spans.clear();
        // Whole move invalidates field tree — clear to reflect Uninit root.
        // Keep tree empty; `is_fully_moved` is authoritative for whole-var case.
        self.tree = MovePathTree::new();
    }

    /// Legacy string API — delegates to Place-based implementation using stored spans.
    /// Constructs a `Place` with dummy local `"__tmp"` and checks via `Place::is_prefix_of` logic,
    /// but callers should migrate to `mark_place_moved`.
    pub fn mark_field_moved(&mut self, path: &str, span: Span, reason: &'static str) {
        // For backward compatibility when local is unknown, treat path as relative field.
        // We cannot build full Place without local, so use a synthetic Place with empty local
        // and rely on projection prefix check only among place_spans entries.
        // However primary path is via `mark_place_moved`; this fallback keeps hybrid behavior.
        if self.level < 2 {
            self.level = 1;
            if self.move_span.is_none() {
                self.move_span = Some(span);
                self.move_reason = Some(reason);
            }
            self.moved_fields.insert(path.to_string(), (span, reason));
            // Also try to insert synthetic place for overlap checks where possible (local unknown).
            let synth = Place {
                local: String::new(),
                projections: path
                    .split('.')
                    .filter(|s| !s.is_empty())
                    .map(|s| Projection::Field(s.to_string()))
                    .collect(),
            };
            let _ = init::move_out(&mut self.tree, &synth);
            self.place_spans.insert(synth, (span, reason));
        }
    }

    /// Place-based field move: marks `place` Uninitialized via `MovePathTree` + `Place::is_prefix_of`.
    pub fn mark_place_moved(&mut self, place: &Place, span: Span, reason: &'static str) {
        if self.level >= 2 {
            return;
        }
        // Only mark if currently initialized (avoid double-move error via tree).
        if !init::is_initialized(&self.tree, place) {
            return;
        }
        if self.level < 2 {
            self.level = 1;
            if self.move_span.is_none() {
                self.move_span = Some(span);
                self.move_reason = Some(reason);
            }
            let path_str = place_to_path_str(place);
            if !path_str.is_empty() {
                self.moved_fields.insert(path_str, (span, reason));
            }
            let _ = init::move_out(&mut self.tree, place);
            self.place_spans.insert(place.clone(), (span, reason));
        }
    }

    pub fn mark_field_reinitialized(&mut self, path: &str) {
        if self.level == 1 {
            self.moved_fields.remove(path);
            let prefix = format!("{path}.");
            self.moved_fields.retain(|k, _| !k.starts_with(&prefix));
            // Also reinitialize synthetic place in tree when possible.
            let synth = Place {
                local: String::new(),
                projections: path
                    .split('.')
                    .filter(|s| !s.is_empty())
                    .map(|s| Projection::Field(s.to_string()))
                    .collect(),
            };
            init::initialize(&mut self.tree, &synth);
            self.place_spans.remove(&synth);
            self.place_spans.retain(|p, _| !synth.is_prefix_of(p));
            if self.moved_fields.is_empty() && self.place_spans.is_empty() {
                self.level = 0;
                self.move_span = None;
                self.move_reason = None;
            }
        }
    }

    /// Place-based reinitialize: marks `place` Initialized and clears descendant tracking.
    pub fn mark_place_reinitialized(&mut self, place: &Place) {
        if self.level == 1 || !self.place_spans.is_empty() {
            init::initialize(&mut self.tree, place);
            self.place_spans.remove(place);
            self.place_spans.retain(|p, _| !place.is_prefix_of(p));
            let path_str = place_to_path_str(place);
            if !path_str.is_empty() {
                self.moved_fields.remove(&path_str);
                let prefix = format!("{path_str}.");
                self.moved_fields.retain(|k, _| !k.starts_with(&prefix));
            } else {
                self.moved_fields.clear();
            }
            // Recompute level from tree root state if known.
            let root = Place::new(place.local.clone());
            if let Some(node) = self.tree.find(&root) {
                match node.state {
                    InitState::Initialized => {
                        if self.place_spans.is_empty() {
                            self.level = 0;
                            self.move_span = None;
                            self.move_reason = None;
                        } else {
                            self.level = 1;
                        }
                    }
                    InitState::PartiallyInitialized => {
                        self.level = 1;
                    }
                    InitState::Uninitialized => {
                        self.level = 2;
                    }
                }
            } else if self.place_spans.is_empty() && self.moved_fields.is_empty() {
                self.level = 0;
                self.move_span = None;
                self.move_reason = None;
            }
        }
    }

    pub fn is_moved(&self) -> bool {
        self.level > 0
    }

    pub fn is_fully_moved(&self) -> bool {
        self.level >= 2
    }

    pub fn is_field_moved(&self, path: &str) -> Option<(Span, &'static str)> {
        if self.is_fully_moved() {
            return self.move_span.zip(self.move_reason);
        }
        // Prefer tree-based check when place_spans populated; fallback to string map.
        if !self.place_spans.is_empty() || !self.tree.is_empty() {
            // Build synthetic place for this path (local unknown) and check via overlap.
            let synth = Place {
                local: String::new(),
                projections: path
                    .split('.')
                    .filter(|s| !s.is_empty())
                    .map(|s| Projection::Field(s.to_string()))
                    .collect(),
            };
            // Check via tree using synthetic local — need to find any stored Place that is prefix.
            for (stored_place, (span, reason)) in &self.place_spans {
                // Compare only projections prefix, ignoring local mismatch for synthetic check.
                if stored_place.projections.len() <= synth.projections.len()
                    && stored_place.projections
                        == synth.projections[..stored_place.projections.len()]
                {
                    return Some((*span, *reason));
                }
            }
        }
        if let Some(&(span, reason)) = self.moved_fields.get(path) {
            return Some((span, reason));
        }
        let mut curr = path;
        while let Some(idx) = curr.rfind('.') {
            curr = &curr[..idx];
            if let Some(&(span, reason)) = self.moved_fields.get(curr) {
                return Some((span, reason));
            }
        }
        None
    }

    /// Place-based query using `Place::is_prefix_of` / `Place::overlaps` and `MovePathTree`.
    pub fn is_place_moved(&self, place: &Place) -> Option<(Span, &'static str)> {
        if self.is_fully_moved() {
            return self.move_span.zip(self.move_reason);
        }
        if init::is_initialized(&self.tree, place) {
            return None;
        }
        // Find the uninitialized prefix that poisons this place.
        for prefix in place_prefixes(place) {
            if let Some(node) = self.tree.find(&prefix) {
                if node.state == InitState::Uninitialized {
                    if let Some((span, reason)) = self.place_spans.get(&prefix) {
                        return Some((*span, *reason));
                    }
                }
            }
        }
        // Fallback: search place_spans via overlaps (sibling disjointness handled).
        for (stored, (span, reason)) in &self.place_spans {
            if stored.overlaps(place) && stored.is_prefix_of(place) {
                return Some((*span, *reason));
            }
        }
        self.move_span.zip(self.move_reason)
    }

    pub fn merge_with(&mut self, other: &VarState) {
        if other.level > self.level {
            self.level = other.level;
            self.move_span = other.move_span;
            self.move_reason = other.move_reason;
        } else if self.move_span.is_none() && other.move_span.is_some() {
            self.move_span = other.move_span;
            self.move_reason = other.move_reason;
        }
        for (k, &(s, r)) in &other.moved_fields {
            self.moved_fields.entry(k.clone()).or_insert((s, r));
        }
        if !self.moved_fields.is_empty() && self.level == 0 {
            self.level = 1;
        }
        // Merge MovePathTree field granularity via structural Place prefixes.
        for (place, (span, reason)) in &other.place_spans {
            // A partial place still takes a whole-place move from `other`.
            if !init::is_initialized(&other.tree, place) && !init::is_moved_out(&self.tree, place)
            {
                // Use move_out to propagate Partial to ancestors via is_prefix_of.
                let _ = init::move_out(&mut self.tree, place);
                self.place_spans
                    .entry(place.clone())
                    .or_insert((*span, *reason));
                let path_str = place_to_path_str(place);
                if !path_str.is_empty() {
                    self.moved_fields
                        .entry(path_str)
                        .or_insert((*span, *reason));
                }
                if self.level == 0 {
                    self.level = 1;
                }
            }
        }
        // If other has no field moves but self does, keep self. No downgrade.
    }
}

/// String root+path splitter for legacy `moved_fields` keys. TODO(Place Phase 1): replace with `Place::from_expr`.
fn expr_root_and_path(expr: &ast::Expression) -> Option<(String, String)> {
    let mut path = Vec::new();
    let mut curr = expr;
    loop {
        match curr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                path.reverse();
                return Some((ident.name.clone(), path.join(".")));
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                path.push(field.name.clone());
                curr = object;
            }
            _ => return None,
        }
    }
}

/// Builds `Place` from expr (`Identifier`/`FieldAccess`/`Index`/`*p`). Index is opaque (`v[i]` overlaps `v[j]`).
fn place_from_expr(expr: &ast::Expression) -> Option<Place> {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Identifier(ident) => Some(Place::new(ident.name.clone())),
        ast::ExpressionKind::FieldAccess { object, field } => {
            let mut base = place_from_expr(object)?;
            base.push_projection(Projection::Field(field.name.clone()));
            Some(base)
        }
        ast::ExpressionKind::Index { object, .. } => {
            let mut base = place_from_expr(object)?;
            base.push_projection(Projection::Index);
            Some(base)
        }
        ast::ExpressionKind::Unary {
            operator: ast::UnaryOperator::Dereference,
            operand,
        } => {
            let mut base = place_from_expr(operand)?;
            base.push_projection(Projection::Deref);
            Some(base)
        }
        _ => None,
    }
}

/// Moved-ness lattice per live variable: tracks status, move site and reason.
type State = FxHashMap<String, VarState>;

/// The states that leave an enclosing loop through `break`, and that return
/// to its head through `continue`. `depth` is the scope depth outside the
/// loop body, so an exit runs the defers of the deeper scopes.
#[derive(Default)]
struct LoopFrame {
    depth: usize,
    breaks: Option<State>,
    continues: Option<State>,
}

/// Joins `other` into `into`: a variable moved on either path is moved.
fn join_state(into: &mut Option<State>, other: &State) {
    match into {
        None => *into = Some(other.clone()),
        Some(into) => {
            join_changed(into, other);
        }
    }
}

/// Joins `other` into `into`'s variables; returns whether any of them changed.
fn join_changed(into: &mut State, other: &State) -> bool {
    let mut changed = false;
    for (name, var) in into.iter_mut() {
        if let Some(theirs) = other.get(name) {
            let before = (var.level, var.place_spans.len(), var.moved_fields.len());
            var.merge_with(theirs);
            changed |= before != (var.level, var.place_spans.len(), var.moved_fields.len());
        }
    }
    changed
}

/// Upper bound on loop passes; the moved-ness lattice is short, so a loop's
/// head state stops changing well before this.
const MAX_LOOP_PASSES: usize = 8;

/// Scope entry: (name, previous state, previous type, previous owner,
/// previous locked type, previously a borrowed payload) so shadowing restores.
type ScopeEntry = (
    String,
    Option<VarState>,
    Option<ast::Type>,
    Option<super::init_plan::OwnerLocalId>,
    Option<ast::Type>,
    bool,
);
/// Facts for move classification. TODO(Phase 5): centralize via `type_properties::{is_copy,needs_drop}`.
#[derive(Clone, Debug, Default)]
struct Facts {
    opaque_owners: FxHashSet<String>,
    /// Base type names that implement `Drop`.
    drop_owners: FxHashSet<String>,
    /// By-value (`InstanceValue`) overloads of each (owner, method): the
    /// fewest explicit arguments it takes and whether it takes more, through a
    /// bare `...` or a typed rest parameter. A borrowing overload of another
    /// arity must not make its calls consume.
    value_receivers: FxHashMap<(String, String), Vec<(usize, bool)>>,
    /// Whether the overload typeck selected at a method call takes its
    /// receiver by value; it overrides `value_receivers` for that call.
    consuming_calls: FxHashMap<MethodCallKey, bool>,
    /// The container and element type of each `container[i]` that typeck
    /// resolved through `__index_get`, keyed by the index's span.
    indexed_elements: FxHashMap<CallSiteKey, (ast::Type, ast::Type)>,
    /// (function/method name, param index) pairs with a value (non-view) param.
    value_args: FxHashSet<(String, usize)>,
    /// (owner, method, explicit argument index) pairs with value parameters.
    value_method_args: FxHashSet<(String, String, usize)>,
    /// Struct field names and types for known struct types.
    struct_fields: FxHashMap<String, StructInfo>,
    /// Payload types of each tuple variant, by (enum, variant).
    variant_payloads: FxHashMap<(String, String), Vec<ast::Type>>,
    /// Variant names of each enum, for match exhaustiveness.
    enum_variants: FxHashMap<String, Vec<String>>,
}

#[derive(Clone, Debug)]
struct StructInfo {
    generic_params: Vec<String>,
    fields: Vec<(String, ast::Type)>,
}

impl Facts {
    fn build(program: &ast::Program, imported_modules: &[ModuleArtifact]) -> Facts {
        let mut facts = Facts::default();
        for module in imported_modules {
            facts.add_module(module);
        }
        for item in &program.items {
            facts.add_item(item);
        }
        facts
    }

    fn add_item(&mut self, item: &ast::Item) {
        match &item.kind {
            ast::ItemKind::Struct(strct) => {
                let fields = strct
                    .fields
                    .iter()
                    .map(|f| (f.name.name.clone(), f.field_type.clone()))
                    .collect();
                let generic_params = strct
                    .generics
                    .as_ref()
                    .map(|generics| {
                        generics
                            .params
                            .iter()
                            .filter_map(|param| match param {
                                ast::GenericParam::Type(param) => Some(param.name.name.clone()),
                                ast::GenericParam::Lifetime(_) => None,
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                self.struct_fields.insert(
                    strct.name.name.clone(),
                    StructInfo {
                        generic_params,
                        fields,
                    },
                );
            }
            ast::ItemKind::Enum(enumeration) => {
                self.opaque_owners.insert(enumeration.name.name.clone());
                self.enum_variants.insert(
                    enumeration.name.name.clone(),
                    enumeration.variants.iter().map(|v| v.name.name.clone()).collect(),
                );
                for variant in &enumeration.variants {
                    if let ast::EnumVariantData::Tuple(types) = &variant.data {
                        self.variant_payloads.insert(
                            (enumeration.name.name.clone(), variant.name.name.clone()),
                            types.clone(),
                        );
                    }
                }
                let fields = enumeration
                    .variants
                    .iter()
                    .flat_map(|variant| match &variant.data {
                        ast::EnumVariantData::Unit => Vec::new(),
                        ast::EnumVariantData::Tuple(types) => types
                            .iter()
                            .enumerate()
                            .map(|(i, ty)| (i.to_string(), ty.clone()))
                            .collect(),
                        ast::EnumVariantData::Struct(fields) => fields
                            .iter()
                            .map(|field| (field.name.name.clone(), field.field_type.clone()))
                            .collect(),
                    })
                    .collect();
                let generic_params = enumeration
                    .generics
                    .as_ref()
                    .map(|generics| {
                        generics
                            .params
                            .iter()
                            .filter_map(|param| match param {
                                ast::GenericParam::Type(param) => Some(param.name.name.clone()),
                                ast::GenericParam::Lifetime(_) => None,
                            })
                            .collect()
                    })
                    .unwrap_or_default();
                self.struct_fields.insert(
                    enumeration.name.name.clone(),
                    StructInfo {
                        generic_params,
                        fields,
                    },
                );
            }
            ast::ItemKind::Impl(imp) => {
                let owner = Self::owner_key(&imp.self_type);
                if imp
                    .trait_ref
                    .as_ref()
                    .is_some_and(|t| t.path.last().is_some_and(|seg| seg.name == "Drop"))
                {
                    self.drop_owners.insert(owner.clone());
                }
                for member in &imp.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        if func.method_kind == ast::MethodKind::InstanceValue {
                            // A typed rest parameter takes zero or more
                            // arguments; a bare `...` adds no parameter.
                            let rest = func.parameters.last().is_some_and(|param| param.is_variadic);
                            let fewest = func.parameters.len().saturating_sub(1 + usize::from(rest));
                            self.value_receivers
                                .entry((owner.clone(), func.name.name.clone()))
                                .or_default()
                                .push((fewest, rest || func.is_variadic));
                        }
                        let first_arg = usize::from(func.method_kind != ast::MethodKind::Static);
                        for (i, param) in func.parameters.iter().enumerate().skip(first_arg) {
                            if !Self::is_view_type(&param.param_type) {
                                let argument_index = i - first_arg;
                                self.value_args
                                    .insert((func.name.name.clone(), argument_index));
                                self.value_method_args.insert((
                                    owner.clone(),
                                    func.name.name.clone(),
                                    argument_index,
                                ));
                            }
                        }
                    }
                }
            }
            ast::ItemKind::Function(func) => {
                for (i, param) in func.parameters.iter().enumerate() {
                    if !Self::is_view_type(&param.param_type) {
                        self.value_args.insert((func.name.name.clone(), i));
                    }
                }
            }
            _ => {}
        }
    }

    fn add_module(&mut self, module: &ModuleArtifact) {
        for export in &module.exports {
            match export.kind {
                ExportKind::Struct => {
                    let fields = export
                        .fields
                        .iter()
                        .filter_map(|field| {
                            crate::module_artifact::ast_type_from_canonical_key(&field.type_key)
                                .ok()
                                .map(|ty| (field.name.clone(), ty))
                        })
                        .collect();
                    self.struct_fields.insert(
                        export.name.clone(),
                        StructInfo {
                            generic_params: export.type_params.clone(),
                            fields,
                        },
                    );
                }
                ExportKind::Enum => {
                    self.opaque_owners.insert(export.name.clone());
                    self.enum_variants.insert(
                        export.name.clone(),
                        export.enum_variants.iter().map(|v| v.name.clone()).collect(),
                    );
                    for variant in &export.enum_variants {
                        let payloads: Option<Vec<ast::Type>> = variant
                            .payload_types
                            .iter()
                            .map(|key| {
                                crate::module_artifact::ast_type_from_canonical_key(key).ok()
                            })
                            .collect();
                        if let Some(payloads) = payloads
                            && !payloads.is_empty()
                        {
                            self.variant_payloads
                                .insert((export.name.clone(), variant.name.clone()), payloads);
                        }
                    }
                    let fields = export
                        .enum_variants
                        .iter()
                        .flat_map(|variant| {
                            variant
                                .payload_types
                                .iter()
                                .enumerate()
                                .filter_map(|(i, key)| {
                                    crate::module_artifact::ast_type_from_canonical_key(key)
                                        .ok()
                                        .map(|ty| (i.to_string(), ty))
                                })
                                .chain(variant.payload_fields.iter().filter_map(|field| {
                                    crate::module_artifact::ast_type_from_canonical_key(
                                        &field.type_key,
                                    )
                                    .ok()
                                    .map(|ty| (field.name.clone(), ty))
                                }))
                                .collect::<Vec<_>>()
                        })
                        .collect();
                    self.struct_fields.insert(
                        export.name.clone(),
                        StructInfo {
                            generic_params: export.type_params.clone(),
                            fields,
                        },
                    );
                }
                ExportKind::Function => {
                    if export.impl_trait.as_deref() == Some("Drop") {
                        if let Some(owner) = export.name.split_once("::").map(|(owner, _)| owner) {
                            self.drop_owners
                                .insert(owner.split('<').next().unwrap_or(owner).to_string());
                        }
                    }
                    let Ok(ty) = crate::types::Type::from_canonical_key(&export.signature) else {
                        continue;
                    };
                    let ast::TypeKind::Function(function) = ty.to_ast().kind.as_ref().clone()
                    else {
                        continue;
                    };
                    if let Some((owner, method)) = export.name.split_once("::") {
                        if function
                            .parameters
                            .first()
                            .is_some_and(|receiver| !Self::is_view_type(receiver))
                        {
                            self.value_receivers
                                .entry((
                                    owner.split('<').next().unwrap_or(owner).to_string(),
                                    method.to_string(),
                                ))
                                .or_default()
                                .push((function.parameters.len() - 1, export.is_variadic));
                        }
                        for (i, param) in function.parameters.iter().enumerate().skip(1) {
                            if !Self::is_view_type(param) {
                                self.value_args.insert((method.to_string(), i - 1));
                                self.value_method_args.insert((
                                    owner.split('<').next().unwrap_or(owner).to_string(),
                                    method.to_string(),
                                    i - 1,
                                ));
                            }
                        }
                    } else {
                        for (i, param) in function.parameters.iter().enumerate() {
                            if !Self::is_view_type(param) {
                                self.value_args.insert((export.name.clone(), i));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Generic impl exports are represented by source templates because a
        // method signature alone cannot identify their generic owner.
        for template in &module.generic_templates {
            let file_id = crate::lexer::register_source(&module.source_path, template);
            let Ok(tokens) = crate::lexer::lex_with_source(template, file_id) else {
                continue;
            };
            let mut parser =
                crate::parser::Parser::new_with_source(tokens, module.source_path.clone());
            let (program, errors) = parser.parse_program();
            if errors.is_empty() {
                for item in &program.items {
                    self.add_item(item);
                }
            }
        }
    }

    /// Pointer/reference types are views and are never consumed by value.
    /// A parameter of a view type borrows its argument. A `Slice` parameter
    /// views a `Vec` or array argument, which keeps its owner.
    fn is_view_type(ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_) => true,
            ast::TypeKind::Named(named) => named.path.last().is_some_and(|seg| seg.name == "Slice"),
            _ => false,
        }
    }

    /// Base type name for matching impl owners (generics stripped).
    fn owner_key(ty: &ast::Type) -> String {
        match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => named
                .path
                .last()
                .map(|seg| seg.name.clone())
                .unwrap_or_default(),
            _ => String::new(),
        }
    }

    /// Tracked if Drop-owned (or `Task` handle). TODO(Phase 5): `type_properties::{is_copy,needs_drop}`.
    fn is_tracked(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => self.is_tracked(&arr.element_type),
            ast::TypeKind::Tuple(types) => types.iter().any(|t| self.is_tracked(t)),
            ast::TypeKind::Named(named) => {
                let owner = Facts::owner_key(ty);
                (named.path.len() == 1 && named.path[0].name == "Task")
                    || self.drop_owners.contains(&owner)
                    || self.struct_fields.get(&owner).is_some_and(|info| {
                        let substitutions = MoveChecker::generic_substitutions(ty, info);
                        info.fields.iter().any(|(_, fty)| {
                            self.is_tracked(&MoveChecker::substitute_type(fty, &substitutions))
                        })
                    })
            }
            _ => self.drop_owners.contains(&Facts::owner_key(ty)),
        }
    }
}

fn is_noreturn_call(expr: &ast::Expression) -> bool {
    if let ast::ExpressionKind::Call { function, .. } = expr.kind.as_ref() {
        if let ast::ExpressionKind::Identifier(id) = function.kind.as_ref() {
            return matches!(
                id.name.as_str(),
                "abort"
                    | "silver_rt_abort"
                    | "exit"
                    | "sys_exit"
                    | "sys_exit_group"
                    | "__optional_abort"
                    | "__result_abort"
                    | "__silver_assert_failed"
            );
        }
    }
    false
}

/// True if control never falls through `stmt` (return / break / continue).
fn statement_terminates(stmt: &ast::Statement) -> bool {
    match &stmt.kind {
        ast::StatementKind::Return(_)
        | ast::StatementKind::Break(_)
        | ast::StatementKind::Continue => true,
        ast::StatementKind::Block(block) => block_terminates(block),
        ast::StatementKind::Expression(expr) => {
            is_noreturn_call(expr) || expression_terminates(expr)
        }
        _ => false,
    }
}

fn block_terminates(block: &ast::Block) -> bool {
    block.statements.iter().any(statement_terminates)
}

fn expression_terminates(expr: &ast::Expression) -> bool {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Block(block) => block_terminates(block),
        ast::ExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => block_terminates(then_branch) && else_branch.as_ref().is_some_and(block_terminates),
        ast::ExpressionKind::Match { arms, .. } => {
            arms.iter().all(|arm| expression_terminates(&arm.body))
        }
        // Loops may exit through their condition, so post-loop code is
        // reachable even when the body returns.
        ast::ExpressionKind::While { .. }
        | ast::ExpressionKind::ForIn { .. }
        | ast::ExpressionKind::For { .. } => false,
        _ => false,
    }
}

pub struct MoveReport {
    pub errors: Vec<MoveError>,
    pub init_plan: super::init_plan::ProgramInitPlan,
}

/// `consuming_calls` is typeck's receiver mode per method call; a call typeck
/// did not record falls back to the declared methods' arities.
/// `indexed_elements` is typeck's element type for each user-defined index.
pub fn analyze_program_with_imports(
    program: &ast::Program,
    imported_modules: &[ModuleArtifact],
    consuming_calls: &FxHashMap<MethodCallKey, bool>,
    indexed_elements: &FxHashMap<CallSiteKey, (ast::Type, ast::Type)>,
) -> MoveReport {
    let mut facts = Facts::build(program, imported_modules);
    facts.consuming_calls = consuming_calls.clone();
    facts.indexed_elements = indexed_elements.clone();
    let mut checker = MoveChecker {
        layouts: OwnerLayouts(facts.clone()),
        facts,
        errors: Vec::new(),
        functions: FxHashMap::default(),
        current_plan: super::init_plan::FunctionInitPlan::default(),
        owner_bindings: FxHashMap::default(),
        locked_bindings: FxHashMap::default(),
        borrowed_payloads: FxHashSet::default(),
        loops: Vec::new(),
        defers: Vec::new(),
    };
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                checker.check_function(&func.parameters, func.return_type.as_ref(), &func.body);
            }
            ast::ItemKind::Impl(imp) => {
                for member in &imp.items {
                    match member {
                        ast::ImplItemKind::Function(func) => {
                            checker.check_function(
                                &func.parameters,
                                func.return_type.as_ref(),
                                &func.body,
                            );
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            checker.check_function(
                                &cast.parameters,
                                Some(&cast.target_type),
                                &cast.body,
                            );
                        }
                        ast::ImplItemKind::AssociatedType(_) => {}
                    }
                }
            }
            _ => {}
        }
    }
    MoveReport {
        errors: checker.errors,
        init_plan: super::init_plan::ProgramInitPlan {
            functions: checker.functions,
            layouts: checker.layouts,
        },
    }
}

struct MoveChecker {
    facts: Facts,
    errors: Vec<MoveError>,
    layouts: OwnerLayouts,
    functions: FxHashMap<super::init_plan::FunctionId, super::init_plan::FunctionInitPlan>,
    current_plan: super::init_plan::FunctionInitPlan,
    owner_bindings: FxHashMap<String, super::init_plan::OwnerLocalId>,
    /// Owning payload bindings of a match on a lock block's value, with their
    /// types: the Mutex still owns what they name.
    locked_bindings: FxHashMap<String, ast::Type>,
    /// Payload bindings of the enclosing match arms that borrow their
    /// payload: the matched enum still owns it.
    borrowed_payloads: FxHashSet<String>,
    /// Enclosing loops of the statement being checked, innermost last.
    loops: Vec<LoopFrame>,
    /// Registered `defer` statements with the scope depth that owns them.
    defers: Vec<(usize, ast::Statement)>,
}

/// Layout completion belongs to the semantic ownership boundary. All required
/// fields participate, including Copy scalars read by a parent's destructor.
#[derive(Clone, Debug, Default)]
pub(crate) struct OwnerLayouts(Facts);

impl OwnerLayouts {
    /// Whether an array's elements get their own owner nodes. An array whose
    /// elements own nothing is one initialization unit: its elements can never
    /// be moved out, and per-element nodes would make every declaration and
    /// transition linear in the array length. Its elements carry no owner
    /// state, so only whole-array writes and moves change the array's state.
    pub(crate) fn tracks_elements(&self, array: &ast::ArrayType) -> bool {
        self.0.is_tracked(&array.element_type)
    }

    pub(crate) fn analyze_body(
        &self,
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
    ) -> Result<super::init_plan::FunctionInitPlan, String> {
        let mut checker = MoveChecker {
            facts: self.0.clone(),
            errors: Vec::new(),
            layouts: self.clone(),
            functions: FxHashMap::default(),
            current_plan: Default::default(),
            owner_bindings: FxHashMap::default(),
            locked_bindings: FxHashMap::default(),
            borrowed_payloads: FxHashSet::default(),
            loops: Vec::new(),
            defers: Vec::new(),
        };
        checker.check_function(parameters, return_type, body);
        if let Some(error) = checker.errors.first() {
            return Err(error.message.clone());
        }
        Ok(checker
            .functions
            .remove(&super::init_plan::FunctionId::for_body(
                parameters,
                return_type,
                body,
            ))
            .expect("analyzed function plan"))
    }
    pub(crate) fn owner(
        &self,
        id: super::init_plan::OwnerLocalId,
        ty: &ast::Type,
        initialized: bool,
    ) -> Result<super::init_plan::OwnerLocalPlan, super::init_plan::InitPlanError> {
        use super::init_plan::MAX_OWNER_NODES as MAX_NODES;
        use super::init_plan::{InitNodePlan, InitPlanError, LayoutProjection, OwnerLocalPlan};
        let state = if initialized {
            InitState::Initialized
        } else {
            InitState::Uninitialized
        };
        let mut nodes = vec![InitNodePlan {
            place: Place::new(&id.name),
            ty: ty.clone(),
            parent: None,
            first_child: 0,
            child_count: 0,
            initial_state: state,
            projection: None,
        }];
        let mut index = 0;
        while index < nodes.len() {
            let node_ty = nodes[index].ty.clone();
            let mut parent = nodes[index].parent;
            while let Some(p) = parent {
                if crate::types::Type::from_ast(&nodes[p].ty).canonical_key()
                    == crate::types::Type::from_ast(&node_ty).canonical_key()
                {
                    return Err(InitPlanError::RecursiveLayout);
                }
                parent = nodes[p].parent;
            }
            let children: Vec<(Projection, LayoutProjection, ast::Type)> =
                match node_ty.kind.as_ref() {
                    ast::TypeKind::Tuple(types) => types
                        .iter()
                        .enumerate()
                        .map(|(i, t)| {
                            (
                                Projection::TupleField(i),
                                LayoutProjection::Field(i as u32),
                                t.clone(),
                            )
                        })
                        .collect(),
                    ast::TypeKind::Array(array) if self.tracks_elements(array) => {
                        if array.size as usize > MAX_NODES {
                            return Err(InitPlanError::TooManyNodes);
                        }
                        (0..array.size)
                            .map(|i| {
                                (
                                    Projection::ArrayElement(i as usize),
                                    LayoutProjection::Element(i as u32),
                                    (*array.element_type).clone(),
                                )
                            })
                            .collect()
                    }
                    ast::TypeKind::Named(_)
                        if !self.0.opaque_owners.contains(&Facts::owner_key(&node_ty)) =>
                    {
                        self.0
                            .struct_fields
                            .get(&Facts::owner_key(&node_ty))
                            .map(|info| {
                                let substitutions =
                                    MoveChecker::generic_substitutions(&node_ty, info);
                                info.fields
                                    .iter()
                                    .enumerate()
                                    .map(|(i, (name, t))| {
                                        (
                                            Projection::Field(name.clone()),
                                            LayoutProjection::Field(i as u32),
                                            MoveChecker::substitute_type(t, &substitutions),
                                        )
                                    })
                                    .collect()
                            })
                            .unwrap_or_default()
                    }
                    _ => Vec::new(),
                };
            if nodes
                .len()
                .checked_add(children.len())
                .is_none_or(|n| n > MAX_NODES)
            {
                return Err(InitPlanError::TooManyNodes);
            }
            nodes[index].first_child = nodes.len();
            nodes[index].child_count = children.len() as u32;
            for (projection, layout, child_ty) in children {
                let mut place = nodes[index].place.clone();
                place.push_projection(projection);
                nodes.push(InitNodePlan {
                    place,
                    ty: child_ty,
                    parent: Some(index),
                    first_child: 0,
                    child_count: 0,
                    initial_state: state,
                    projection: Some(layout),
                });
            }
            index += 1;
        }
        let plan = OwnerLocalPlan {
            id,
            ty: ty.clone(),
            nodes,
            initialized_at_declaration: initialized,
        };
        plan.validate()?;
        Ok(plan)
    }
}

impl MoveChecker {
    fn reject_partial_move_from_drop_ancestor(
        &mut self,
        root_name: &str,
        path: &str,
        span: Span,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        let Some((place, owner)) = self.partial_move_from_drop_ancestor(root_name, path, var_types)
        else {
            return false;
        };
        self.errors.push(MoveError {
            message: msg::cannot_move_field_out_of_drop_type(&place, &owner),
            span,
            note_span: None,
            note_message: None,
        });
        true
    }

    /// True when the method call at `call` selects a by-value overload, so it
    /// consumes `receiver`. Without typeck's choice, any by-value overload
    /// that accepts `argument_count` arguments counts.
    fn consuming_receiver(
        &self,
        call: Span,
        receiver: &ast::Expression,
        method: &str,
        argument_count: usize,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        let key = ((call.file, call.start, call.end, call.expansion), method.to_string());
        if let Some(&consumes) = self.facts.consuming_calls.get(&key) {
            return consumes;
        }
        let Some(ty) = self.receiver_type(receiver, var_types) else {
            return false;
        };
        self.facts
            .value_receivers
            .get(&(Facts::owner_key(&ty), method.to_string()))
            .is_some_and(|overloads| {
                overloads.iter().any(|&(fewest, more)| {
                    argument_count == fewest || (more && argument_count > fewest)
                })
            })
    }

    fn is_value_call_argument(
        &self,
        function: &ast::Expression,
        function_name: Option<&str>,
        index: usize,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref()
            && let Some(ty) = var_types.get(&ident.name)
        {
            let function_type = match ty.kind.as_ref() {
                ast::TypeKind::Function(function) => Some(function),
                ast::TypeKind::Pointer(pointer) => match pointer.inner.kind.as_ref() {
                    ast::TypeKind::Function(function) => Some(function),
                    _ => None,
                },
                _ => None,
            };
            return function_type
                .and_then(|function| function.parameters.get(index))
                .is_some_and(|parameter| !Facts::is_view_type(parameter));
        }
        function_name.is_some_and(|name| self.facts.value_args.contains(&(name.to_string(), index)))
    }

    fn method_has_value_argument(
        &self,
        receiver: &ast::Expression,
        method: &str,
        index: usize,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        self.receiver_type(receiver, var_types).is_some_and(|ty| {
            self.facts.value_method_args.contains(&(
                Facts::owner_key(&ty),
                method.to_string(),
                index,
            ))
        })
    }

    fn receiver_type(
        &self,
        receiver: &ast::Expression,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<ast::Type> {
        // A lock block's value is `(*__lock_N__name)`, which has no root
        // local of its own, and a payload bound from it has a type only in
        // `locked_bindings`.
        self.locked_place(receiver, var_types)
            .map(|(_, ty)| ty)
            .or_else(|| match receiver.kind.as_ref() {
                ast::ExpressionKind::Index { object, index } => {
                    let container = self.receiver_type(object, var_types);
                    self.indexed_element(receiver, container.as_ref())
                        .or_else(|| index_element_type(&container?, index))
                }
                // A field reached through an index (`boxes[0].inner.items`)
                // has no root local path.
                ast::ExpressionKind::FieldAccess { object, field }
                    if expr_root_and_path(receiver).is_none() =>
                {
                    self.field_type(self.receiver_type(object, var_types)?, &field.name)
                }
                _ => {
                    let (root, path) = expr_root_and_path(receiver)?;
                    self.get_field_type(&root, &path, var_types)
                }
            })
    }

    /// The element type typeck resolved for `container[i]` through
    /// `__index_get`. A monomorph shares its generic body's span, so the type
    /// arguments of the recorded container are mapped onto `container`'s:
    /// `Wrap<T>` recorded for `Crate<T>` reads as `Wrap<Text>` in `Crate<Text>`.
    fn indexed_element(
        &self,
        index: &ast::Expression,
        container: Option<&ast::Type>,
    ) -> Option<ast::Type> {
        let span = index.span;
        let (recorded, element) = self
            .facts
            .indexed_elements
            .get(&(span.file, span.start, span.end, span.expansion))?;
        let mut substitutions = FxHashMap::default();
        if let Some(container) = container {
            bind_type_arguments(recorded, container, &mut substitutions);
        }
        Some(Self::substitute_type(element, &substitutions))
    }

    /// Checks a value stored into a new place (`T u = t`, `u = t`, a struct
    /// literal field). A tracked source place is moved, and the transfer is
    /// recorded so codegen clears the source's drop state.
    fn transfer_value(
        &mut self,
        value: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        self.transfer_place(value, state, scopes, var_types, true);
    }

    /// Moves the tracked place `value` reads. A conditional moves the arm it
    /// takes; codegen moves that arm's value itself, so no owner transfer is
    /// recorded for it.
    fn transfer_place(
        &mut self,
        value: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
        record: bool,
    ) {
        if let ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } = value.kind.as_ref()
        {
            self.check_expr(condition, state, scopes, var_types);
            let mut other = state.clone();
            self.transfer_place(then_expr, state, scopes, var_types, false);
            self.transfer_place(else_expr, &mut other, scopes, var_types, false);
            join_changed(state, &other);
            return;
        }
        self.reject_locked_move(value, var_types);
        if self.reject_container_element_move(value, var_types) {
            self.check_expr(value, state, scopes, var_types);
            return;
        }
        let Some((root, path)) = expr_root_and_path(value).filter(|(root, path)| {
            state.contains_key(root) && self.is_path_tracked(root, path, var_types)
        }) else {
            self.check_expr(value, state, scopes, var_types);
            return;
        };
        // The ordinary read check runs first so an earlier move is reported.
        self.check_expr(value, state, scopes, var_types);
        if !path.is_empty()
            && self.reject_partial_move_from_drop_ancestor(&root, &path, value.span, var_types)
        {
            return;
        }
        if record {
            self.record_transfer(value);
        }
        if let Some(var) = state.get_mut(&root) {
            if path.is_empty() {
                var.mark_moved(value.span, msg::note_value_moved_here());
            } else {
                var.mark_place_moved(
                    &place_from_root_and_path(&root, &path),
                    value.span,
                    msg::note_value_moved_here(),
                );
            }
        }
    }

    /// Rejects taking an owned element, or an owned field of one, out of
    /// `container[i]` when the container is a heap collection: it still owns
    /// and later drops that element. Arrays, slices and tuples are places the
    /// checker tracks.
    fn reject_container_element_move(
        &mut self,
        value: &ast::Expression,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        let mut fields = Vec::new();
        let mut element = value;
        while let ast::ExpressionKind::FieldAccess { object, field } = element.kind.as_ref() {
            fields.push(field.name.as_str());
            element = object;
        }
        let ast::ExpressionKind::Index { object, index } = element.kind.as_ref() else {
            return false;
        };
        let Some(container) = self.receiver_type(object, var_types) else {
            return false;
        };
        if !matches!(container.kind.as_ref(), ast::TypeKind::Named(_)) {
            return false;
        }
        fields.reverse();
        let path = fields.join(".");
        // The type typeck resolved for the index (an `__index_get` result may
        // differ from the type argument); the type argument only as a fallback.
        let owned = self
            .indexed_element(element, Some(&container))
            .or_else(|| index_element_type(&container, index))
            .and_then(|element| self.field_type(element, &path))
            .is_some_and(|moved| self.facts.is_tracked(&moved));
        if !owned {
            return false;
        }
        let name = crate::types::Type::from_ast(&container).to_string();
        self.error(msg::cannot_move_out_of_indexed_element(&name), value.span);
        true
    }

    /// A borrow passed to a call lives until the call returns, so it must
    /// still be valid after every argument has been evaluated:
    /// `f(&t, move t)` and `f(&v[next()], move v)` are rejected. A computed
    /// index is not evaluated again; its plain owner is checked instead.
    fn recheck_borrowed_arguments(
        &mut self,
        arguments: &[ast::Expression],
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        for argument in arguments {
            let ast::ExpressionKind::Reference { expression, .. } = argument.kind.as_ref() else {
                continue;
            };
            if is_plain_place(expression) {
                self.check_expr(argument, state, scopes, var_types);
            } else if let Some(owner) = plain_prefix(expression) {
                self.check_expr(owner, state, scopes, var_types);
            }
        }
    }

    fn move_value_argument(
        &mut self,
        argument: &ast::Expression,
        reason: &'static str,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        // A conditional or a match passes the value of the path that ran.
        if matches!(
            argument.kind.as_ref(),
            ast::ExpressionKind::Ternary { .. } | ast::ExpressionKind::Match { .. }
        ) {
            self.transfer_place(argument, state, scopes, var_types, false);
            return;
        }
        self.reject_locked_move(argument, var_types);
        if self.reject_container_element_move(argument, var_types) {
            self.check_expr(argument, state, scopes, var_types);
            return;
        }
        if let Some((root, path)) = expr_root_and_path(argument)
            && state.contains_key(&root)
            && self.is_path_tracked(&root, &path, var_types)
        {
            self.check_expr(argument, state, scopes, var_types);
            if !path.is_empty()
                && self.reject_partial_move_from_drop_ancestor(
                    &root,
                    &path,
                    argument.span,
                    var_types,
                )
            {
                return;
            }
            if let Some(var) = state.get_mut(&root) {
                if path.is_empty() {
                    var.mark_moved(argument.span, reason);
                } else {
                    var.mark_place_moved(
                        &place_from_root_and_path(&root, &path),
                        argument.span,
                        reason,
                    );
                }
            }
        } else {
            self.check_expr(argument, state, scopes, var_types);
        }
    }

    /// Reports a use of indexed `place` when its local, or the part of it
    /// that `place` names, has moved. Index operands are not evaluated.
    fn report_moved_index_place(&mut self, place: &Place, span: Span, state: &State) {
        let Some(var) = state.get(&place.local) else {
            return;
        };
        if var.is_fully_moved() {
            let reason = var.move_reason.unwrap_or(msg::note_value_explicitly_moved());
            let move_span = var.move_span;
            self.error_with_note(msg::use_of_moved_value(&place.local), span, move_span, reason);
        } else if let Some((move_span, reason)) = var.is_place_moved(place) {
            self.error_with_note(
                format!("use of moved field '{}[index]'", place.local),
                span,
                Some(move_span),
                reason,
            );
        }
    }

    /// Check expressions evaluated to compute indexed/dereferenced places.
    fn check_place_operands(
        &mut self,
        expr: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        self.record_place(expr);
        match expr.kind.as_ref() {
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.check_place_operands(object, state, scopes, var_types);
            }
            ast::ExpressionKind::Index { object, index } => {
                self.check_place_operands(object, state, scopes, var_types);
                self.check_expr(index, state, scopes, var_types);
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                self.check_place_operands(object, state, scopes, var_types);
                if let Some(s) = start {
                    self.check_expr(s, state, scopes, var_types);
                }
                if let Some(e) = end {
                    self.check_expr(e, state, scopes, var_types);
                }
                if let Some(st) = step {
                    self.check_expr(st, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => self.check_place_operands(operand, state, scopes, var_types),
            _ => {}
        }
    }

    fn check_function(
        &mut self,
        parameters: &[ast::Parameter],
        return_type: Option<&ast::Type>,
        body: &ast::Block,
    ) {
        let id = super::init_plan::FunctionId::for_body(parameters, return_type, body);
        self.current_plan = super::init_plan::FunctionInitPlan::default();
        let mut state = State::default();
        let mut scopes: Vec<Vec<ScopeEntry>> = Vec::new();
        let mut var_types: FxHashMap<String, ast::Type> = FxHashMap::default();
        scopes.push(Vec::new());
        for param in parameters {
            self.record_owner(&param.name, &param.param_type, true);
            self.declare(
                &param.name,
                Some(&param.param_type),
                &mut state,
                &mut scopes,
                &mut var_types,
            );
        }
        self.check_block(body, &mut state, &mut scopes, &mut var_types);
        self.functions
            .insert(id, std::mem::take(&mut self.current_plan));
    }

    fn record_owner(&mut self, name: &ast::Identifier, ty: &ast::Type, initialized: bool) {
        if !self.facts.is_tracked(ty) {
            return;
        }
        let id = super::init_plan::OwnerLocalId {
            declaration: name.span.into(),
            name: name.name.clone(),
        };
        match self.layouts.owner(id, ty, initialized) {
            Ok(owner) => self.current_plan.owners.push(owner),
            Err(error) => self.error(msg::owner_init_plan_error(&format!("{error:?}")), name.span),
        }
    }

    fn record_place(&mut self, expr: &ast::Expression) {
        let Some(place) = place_from_expr(expr) else {
            return;
        };
        let Some(id) = self.owner_bindings.get(&place.local) else {
            return;
        };
        let Some(owner) = self.current_plan.owner(id) else {
            return;
        };
        if let Some(node) = owner.resolve_expression(expr) {
            self.current_plan.places.insert(
                expr.span.into(),
                super::init_plan::ResolvedOwnerPlace {
                    owner: id.clone(),
                    node,
                },
            );
        }
    }

    fn record_transfer(&mut self, expr: &ast::Expression) {
        self.record_place(expr);
        if let Some(resolved) = self.current_plan.places.get(&expr.span.into())
            && let Some(owner) = self.current_plan.owner(&resolved.owner)
        {
            let place = owner.nodes[resolved.node].place.clone();
            self.current_plan
                .transitions
                .push(super::init_plan::OwnerTransition {
                    site: expr.span.into(),
                    place,
                    state: InitState::Uninitialized,
                });
        }
    }

    fn get_field_type(
        &self,
        root_name: &str,
        path: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<ast::Type> {
        self.field_type(var_types.get(root_name)?.clone(), path)
    }

    /// The type of dotted field `path` inside a value of type `curr_ty`.
    fn field_type(&self, mut curr_ty: ast::Type, path: &str) -> Option<ast::Type> {
        if path.is_empty() {
            return Some(curr_ty);
        }
        for segment in path.split('.') {
            if let ast::TypeKind::Slice(elem) = curr_ty.kind.as_ref() {
                if segment == "len" {
                    curr_ty = ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I64)),
                        span: curr_ty.span,
                    };
                    continue;
                } else if segment == "data" {
                    curr_ty = ast::Type {
                        kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                            is_mutable: true,
                            is_volatile: false,
                            inner: elem.element_type.clone(),
                        })),
                        span: curr_ty.span,
                    };
                    continue;
                }
            } else if let ast::TypeKind::Primitive(ast::PrimitiveType::Str) = curr_ty.kind.as_ref()
            {
                if segment == "len" {
                    curr_ty = ast::Type {
                        kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I64)),
                        span: curr_ty.span,
                    };
                    continue;
                } else if segment == "data" {
                    curr_ty = ast::Type {
                        kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                            is_mutable: false,
                            is_volatile: false,
                            inner: Box::new(ast::Type {
                                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::U8)),
                                span: curr_ty.span,
                            }),
                        })),
                        span: curr_ty.span,
                    };
                    continue;
                }
            }
            let owner = Facts::owner_key(&curr_ty);
            let info = self.facts.struct_fields.get(&owner)?;
            let (_, field_ty) = info.fields.iter().find(|(name, _)| name == segment)?;
            let substitutions = Self::generic_substitutions(&curr_ty, info);
            curr_ty = Self::substitute_type(field_ty, &substitutions);
        }
        Some(curr_ty)
    }

    fn generic_substitutions(ty: &ast::Type, info: &StructInfo) -> FxHashMap<String, ast::Type> {
        let args = match ty.kind.as_ref() {
            ast::TypeKind::Named(named) => named.generics.as_deref().unwrap_or_default(),
            _ => &[],
        };
        info.generic_params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect()
    }

    fn substitute_type(ty: &ast::Type, substitutions: &FxHashMap<String, ast::Type>) -> ast::Type {
        let mut ty = ty.clone();
        ty.kind = Box::new(match ty.kind.as_ref() {
            ast::TypeKind::Generic(generic) if generic.args.is_empty() => {
                return substitutions.get(&generic.name.name).cloned().unwrap_or(ty);
            }
            ast::TypeKind::Named(named) => {
                if named.path.len() == 1
                    && named.generics.as_ref().is_none_or(Vec::is_empty)
                    && let Some(substituted) = substitutions.get(&named.path[0].name)
                {
                    return substituted.clone();
                }
                let mut named = named.clone();
                named.generics = named.generics.map(|args| {
                    args.iter()
                        .map(|arg| Self::substitute_type(arg, substitutions))
                        .collect()
                });
                ast::TypeKind::Named(named)
            }
            ast::TypeKind::Pointer(pointer) => {
                let mut pointer = pointer.clone();
                pointer.inner = Box::new(Self::substitute_type(&pointer.inner, substitutions));
                ast::TypeKind::Pointer(pointer)
            }
            ast::TypeKind::Reference(reference) => {
                let mut reference = reference.clone();
                reference.inner = Box::new(Self::substitute_type(&reference.inner, substitutions));
                ast::TypeKind::Reference(reference)
            }
            ast::TypeKind::Array(array) => {
                let mut array = array.as_ref().clone();
                array.element_type =
                    Box::new(Self::substitute_type(&array.element_type, substitutions));
                ast::TypeKind::Array(Box::new(array))
            }
            ast::TypeKind::Optional(inner) => {
                ast::TypeKind::Optional(Box::new(Self::substitute_type(inner, substitutions)))
            }
            ast::TypeKind::Tuple(types) => ast::TypeKind::Tuple(
                types
                    .iter()
                    .map(|inner| Self::substitute_type(inner, substitutions))
                    .collect(),
            ),
            other => other.clone(),
        });
        ty
    }

    /// A place inside a `lock` block, `(*__lock_N__name)` or an owning payload
    /// binding of a match on it, with fields and indexes: the user's spelling
    /// (`name.field[...]`) and its type.
    fn locked_place(
        &self,
        expr: &ast::Expression,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<(String, ast::Type)> {
        let mut steps = Vec::new();
        let mut place = expr;
        loop {
            match place.kind.as_ref() {
                ast::ExpressionKind::FieldAccess { object, field } => {
                    steps.push(Ok(field.name.as_str()));
                    place = object;
                }
                ast::ExpressionKind::Index { object, index } => {
                    steps.push(Err(index.as_ref()));
                    place = object;
                }
                _ => break,
            }
        }
        let (mut spelling, mut ty) = match place.kind.as_ref() {
            ast::ExpressionKind::Identifier(binding) => (
                binding.name.clone(),
                self.locked_bindings.get(&binding.name)?.clone(),
            ),
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => {
                let ast::ExpressionKind::Identifier(pointer) = operand.kind.as_ref() else {
                    return None;
                };
                let name = crate::sync_sugar::locked_name(&pointer.name)?;
                let Some(ast::TypeKind::Pointer(pointer_type)) =
                    var_types.get(&pointer.name).map(|ty| ty.kind.as_ref())
                else {
                    return None;
                };
                (name.to_string(), pointer_type.inner.as_ref().clone())
            }
            _ => return None,
        };
        for step in steps.into_iter().rev() {
            ty = match step {
                Ok(field) => {
                    spelling = format!("{spelling}.{field}");
                    self.field_type(ty, field)?
                }
                Err(index) => {
                    spelling.push_str("[...]");
                    index_element_type(&ty, index)?
                }
            };
        }
        Some((spelling, ty))
    }

    /// Rejects moving a `lock` block's value, or an owned field or element of
    /// it, out of the block. The value is `(*__lock_N__name)`, a place inside
    /// the Mutex (see sync_sugar), and the Mutex would still own and drop it.
    /// An aggregate or a match consumes each value it is built from.
    fn reject_locked_move(
        &mut self,
        expr: &ast::Expression,
        var_types: &FxHashMap<String, ast::Type>,
    ) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Array(items)
            | ast::ExpressionKind::Tuple(items)
            | ast::ExpressionKind::EnumVariant { fields: items, .. } => {
                for item in items {
                    self.reject_locked_move(item, var_types);
                }
                return;
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.reject_locked_move(&field.value, var_types);
                }
                return;
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    let (ast::InitializerItem::Positional(value)
                    | ast::InitializerItem::Field { value, .. }
                    | ast::InitializerItem::Index { value, .. }) = item;
                    self.reject_locked_move(value, var_types);
                }
                return;
            }
            // Either result of a conditional is the value taken.
            ast::ExpressionKind::Ternary {
                then_expr,
                else_expr,
                ..
            } => {
                self.reject_locked_move(then_expr, var_types);
                self.reject_locked_move(else_expr, var_types);
                return;
            }
            // Each arm's own locked payloads were checked with the arm, so
            // its bindings only shadow outer locked names here.
            ast::ExpressionKind::Match { arms, .. } => {
                for arm in arms {
                    let saved = self.bind_locked_payloads(&arm.pattern, None);
                    self.reject_locked_move(&arm.body, var_types);
                    self.locked_bindings = saved;
                }
                return;
            }
            _ => {}
        }
        if let Some((place, ty)) = self.locked_place(expr, var_types)
            && self.facts.is_tracked(&ty)
        {
            self.error(msg::move_out_of_lock(&place), expr.span);
        }
    }

    /// A non-Copy field cannot be detached from any enclosing value whose own
    /// destructor must observe the complete value.
    fn partial_move_from_drop_ancestor(
        &self,
        root_name: &str,
        path: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<(String, String)> {
        let target = self.get_field_type(root_name, path, var_types)?;
        if !self.facts.is_tracked(&target) {
            return None;
        }
        let mut owner_ty = var_types.get(root_name)?.clone();
        let mut owner_path = root_name.to_string();
        for segment in path.split('.').filter(|segment| !segment.is_empty()) {
            let owner = Facts::owner_key(&owner_ty);
            if self.facts.drop_owners.contains(&owner) {
                return Some((format!("{root_name}.{path}"), owner_path));
            }
            let info = self.facts.struct_fields.get(&owner)?;
            let (_, field_ty) = info.fields.iter().find(|(name, _)| name == segment)?;
            owner_ty =
                Self::substitute_type(field_ty, &Self::generic_substitutions(&owner_ty, info));
            owner_path.push('.');
            owner_path.push_str(segment);
        }
        None
    }

    fn is_path_tracked(
        &self,
        root_name: &str,
        path: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        if path.is_empty() {
            return true;
        }
        if let Some(ty) = self.get_field_type(root_name, path, var_types) {
            self.facts.is_tracked(&ty)
        } else {
            false
        }
    }

    fn declare(
        &mut self,
        identifier: &ast::Identifier,
        ty: Option<&ast::Type>,
        state: &mut State,
        scopes: &mut [Vec<ScopeEntry>],
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        let name = &identifier.name;
        let old_borrowed = self.borrowed_payloads.remove(name);
        let old_locked = self.locked_bindings.remove(name);
        if let Some(ty) = ty {
            let old_owner = self.owner_bindings.remove(name);
            let id = super::init_plan::OwnerLocalId {
                declaration: identifier.span.into(),
                name: name.clone(),
            };
            if self.current_plan.owner(&id).is_some() {
                self.owner_bindings.insert(name.clone(), id);
            }
            let old_state = state.get(name).cloned();
            let old_type = var_types.get(name).cloned();
            if self.facts.is_tracked(ty) {
                let mut var = VarState::new_live();
                if let Some(owner) = self
                    .owner_bindings
                    .get(name)
                    .and_then(|id| self.current_plan.owner(id))
                {
                    for node in &owner.nodes {
                        var.tree.insert(node.place.clone());
                    }
                }
                state.insert(name.to_string(), var);
            } else {
                // An untracked binding shadows any outer state of the same
                // name, such as a moved-from scrutinee; the scope restores it.
                state.remove(name);
            }
            var_types.insert(name.to_string(), ty.clone());
            scopes.last_mut().expect("scope stack is non-empty").push((
                name.to_string(),
                old_state,
                old_type,
                old_owner,
                old_locked,
                old_borrowed,
            ));
        } else if old_locked.is_some() || old_borrowed {
            // An untyped local leaves the state alone but still shadows the
            // locked name until its scope ends.
            scopes.last_mut().expect("scope stack is non-empty").push((
                name.to_string(),
                state.get(name).cloned(),
                var_types.get(name).cloned(),
                self.owner_bindings.get(name).cloned(),
                old_locked,
                old_borrowed,
            ));
        }
    }

    fn declare_pattern(
        &mut self,
        pattern: &ast::Pattern,
        ty: Option<&ast::Type>,
        state: &mut State,
        scopes: &mut [Vec<ScopeEntry>],
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident) => {
                self.declare(ident, ty, state, scopes, var_types);
            }
            ast::PatternKind::Tuple(items) => {
                let elem_annotations = match ty.and_then(|a| match a.kind.as_ref() {
                    ast::TypeKind::Tuple(types) => Some(types),
                    _ => None,
                }) {
                    Some(types) => types.iter().map(Some).collect::<Vec<_>>(),
                    None => vec![None; items.len()],
                };
                for (item, elem_annot) in items.iter().zip(elem_annotations.iter()) {
                    self.declare_pattern(item, *elem_annot, state, scopes, var_types);
                }
            }
            _ => {}
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn pop_scope(
        &mut self,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        if let Some(scope) = scopes.pop() {
            for (name, old_state, old_type, old_owner, old_locked, old_borrowed) in scope {
                self.owner_bindings.remove(&name);
                self.locked_bindings.remove(&name);
                self.borrowed_payloads.remove(&name);
                if old_borrowed {
                    self.borrowed_payloads.insert(name.clone());
                }
                if let Some(locked) = old_locked {
                    self.locked_bindings.insert(name.clone(), locked);
                }
                if let Some(owner) = old_owner {
                    self.owner_bindings.insert(name.clone(), owner);
                }
                match old_state {
                    Some(v) => {
                        state.insert(name.clone(), v);
                    }
                    None => {
                        state.remove(&name);
                        var_types.remove(&name);
                    }
                }
                if let Some(t) = old_type {
                    var_types.insert(name, t);
                }
            }
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.push_error(MoveError {
            message: message.into(),
            span,
            note_span: None,
            note_message: None,
        });
    }

    /// Records `error` once: loop passes and defers walked at several exits
    /// revisit the same code.
    fn push_error(&mut self, error: MoveError) {
        if !self
            .errors
            .iter()
            .any(|seen| seen.span == error.span && seen.message == error.message)
        {
            self.errors.push(error);
        }
    }

    fn error_with_note(
        &mut self,
        message: impl Into<String>,
        span: Span,
        note_span: Option<Span>,
        note_message: impl Into<String>,
    ) {
        self.push_error(MoveError {
            message: message.into(),
            span,
            note_span,
            note_message: Some(note_message.into()),
        });
    }

    // ------------------------------------------------------------------
    // Statements
    // ------------------------------------------------------------------

    fn check_block(
        &mut self,
        block: &ast::Block,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        self.check_block_yielding(block, false, state, scopes, var_types);
    }

    /// Checks `block`; when it `yields`, its trailing expression is the
    /// block's value and moves out before the block's defers run, so a defer
    /// cannot consume what the block hands out.
    fn check_block_yielding(
        &mut self,
        block: &ast::Block,
        yields: bool,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        scopes.push(Vec::new());
        let depth = scopes.len();
        // Statements after one that never falls through cannot run.
        for (position, stmt) in block.statements.iter().enumerate() {
            match &stmt.kind {
                ast::StatementKind::Expression(value)
                    if yields && position + 1 == block.statements.len() =>
                {
                    self.transfer_value(value, state, scopes, var_types);
                }
                _ => self.check_statement(stmt, state, scopes, var_types),
            }
            if statement_terminates(stmt) {
                break;
            }
        }
        let first = self
            .defers
            .iter()
            .position(|(owner, _)| *owner >= depth)
            .unwrap_or(self.defers.len());
        let pending = self.defers.split_off(first);
        // A terminating block already ran its defers at the exit.
        if !block_terminates(block) {
            for (_, deferred) in pending.iter().rev() {
                self.check_statement(deferred, state, scopes, var_types);
            }
        }
        self.pop_scope(state, scopes, var_types);
    }

    /// Checks the defers that an exit to scope depth `depth` runs, innermost
    /// first, against the exiting path's state.
    fn check_exit_defers(
        &mut self,
        depth: usize,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        let pending: Vec<(usize, ast::Statement)> = self
            .defers
            .iter()
            .rev()
            .filter(|(owner, _)| *owner > depth)
            .cloned()
            .collect();
        for (owner, deferred) in &pending {
            self.check_defer_where_written(*owner, deferred, state, scopes, var_types);
        }
    }

    /// Checks `deferred`, registered when `owner` scopes were open, against
    /// the bindings visible there: a binding that a later scope shadows is
    /// read from, and moved in, that scope's saved entry.
    fn check_defer_where_written(
        &mut self,
        owner: usize,
        deferred: &ast::Statement,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        let mut view = state.clone();
        let mut view_types = var_types.clone();
        let mut shadowed = FxHashSet::default();
        let saved_owners = self.owner_bindings.clone();
        let saved_locked = self.locked_bindings.clone();
        let saved_borrowed = self.borrowed_payloads.clone();
        for scope in scopes.iter().skip(owner).rev() {
            for (name, old_state, old_type, old_owner, old_locked, old_borrowed) in
                scope.iter().rev()
            {
                shadowed.insert(name.clone());
                self.owner_bindings.remove(name);
                if let Some(old) = old_owner {
                    self.owner_bindings.insert(name.clone(), old.clone());
                }
                self.locked_bindings.remove(name);
                if let Some(old) = old_locked {
                    self.locked_bindings.insert(name.clone(), old.clone());
                }
                self.borrowed_payloads.remove(name);
                if *old_borrowed {
                    self.borrowed_payloads.insert(name.clone());
                }
                match old_state {
                    Some(old) => {
                        view.insert(name.clone(), old.clone());
                    }
                    None => {
                        view.remove(name);
                    }
                }
                match old_type {
                    Some(old) => {
                        view_types.insert(name.clone(), old.clone());
                    }
                    None => {
                        view_types.remove(name);
                    }
                }
            }
        }
        self.check_statement(deferred, &mut view, scopes, &mut view_types);
        self.owner_bindings = saved_owners;
        self.locked_bindings = saved_locked;
        self.borrowed_payloads = saved_borrowed;
        for (name, var) in view {
            if !shadowed.contains(&name) {
                state.insert(name, var);
                continue;
            }
            // The outermost later scope saved the binding the defer sees.
            if let Some(entry) = scopes
                .iter_mut()
                .skip(owner)
                .flat_map(|scope| scope.iter_mut())
                .find(|entry| entry.0 == name)
                && entry.1.is_some()
            {
                entry.1 = Some(var);
            }
        }
    }

    fn check_statement(
        &mut self,
        stmt: &ast::Statement,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        match &stmt.kind {
            ast::StatementKind::Block(block) => {
                self.check_block(block, state, scopes, var_types);
            }
            ast::StatementKind::Let(let_stmt) => {
                if let ast::PatternKind::Identifier(name) = &let_stmt.pattern.kind
                    && let Some(ty) = &let_stmt.type_annotation
                {
                    self.record_owner(name, ty, let_stmt.initializer.is_some());
                }
                if let Some(init) = &let_stmt.initializer {
                    self.transfer_value(init, state, scopes, var_types);
                }
                self.declare_pattern(
                    &let_stmt.pattern,
                    let_stmt.type_annotation.as_ref(),
                    state,
                    scopes,
                    var_types,
                );
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, state, scopes, var_types);
            }
            ast::StatementKind::Return(value) => {
                if let Some(expr) = value {
                    self.check_return_value(expr, state, scopes, var_types);
                }
                self.check_exit_defers(0, state, scopes, var_types);
            }
            ast::StatementKind::Break(value) => {
                if let Some(value) = value {
                    self.check_expr(value, state, scopes, var_types);
                }
                self.leave_loop(true, state, scopes, var_types);
            }
            ast::StatementKind::Continue => {
                self.leave_loop(false, state, scopes, var_types);
            }
            ast::StatementKind::Defer(inner) => {
                // A deferred statement runs at each exit of its scope and is
                // checked there, against the state that exit carries.
                self.defers.push((scopes.len(), (**inner).clone()));
            }
        }
    }

    /// Checks a returned value: returning a bare local or an owned field
    /// moves it.
    fn check_return_value(
        &mut self,
        expr: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        // Returning a tracked local or field moves it out; the source is
        // dead afterwards. A heap container keeps its elements.
        self.reject_locked_move(expr, var_types);
        if self.reject_container_element_move(expr, var_types) {
            return;
        }
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                self.check_expr(expr, state, scopes, var_types);
                if let Some(var) = state.get_mut(&ident.name) {
                    var.mark_moved(expr.span, msg::note_value_moved_by_return());
                }
            }
            _ => {
                if let Some((root_name, path)) = expr_root_and_path(expr)
                    && !path.is_empty()
                    && state.contains_key(&root_name)
                    && self.is_path_tracked(&root_name, &path, var_types)
                {
                    self.check_expr(expr, state, scopes, var_types);
                    if self.reject_partial_move_from_drop_ancestor(
                        &root_name, &path, expr.span, var_types,
                    ) {
                        return;
                    }
                    if let Some(var) = state.get_mut(&root_name) {
                        self.record_transfer(expr);
                        var.mark_place_moved(
                            &place_from_root_and_path(&root_name, &path),
                            expr.span,
                            msg::note_value_moved_by_return(),
                        );
                    }
                } else {
                    self.check_expr(expr, state, scopes, var_types);
                }
            }
        }
    }

    /// Checks a loop until its head state stops changing, then leaves `state`
    /// as the state after the loop. `condition` runs at the head of every
    /// pass; `step` runs on the paths that reach the next pass: the end of
    /// the body and each `continue`. The loop exits when the condition fails
    /// (never for a literal `true`) and at each `break`.
    fn check_loop(
        &mut self,
        condition: Option<&ast::Expression>,
        body: &ast::Block,
        step: Option<&ast::Expression>,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        let endless = condition.is_some_and(|condition| {
            matches!(
                condition.kind.as_ref(),
                ast::ExpressionKind::Literal(ast::Literal::Bool(true))
            )
        });
        if let Some(condition) = condition
            && matches!(
                condition.kind.as_ref(),
                ast::ExpressionKind::Literal(ast::Literal::Bool(false))
            )
        {
            self.check_expr(condition, state, scopes, var_types);
            return;
        }
        let mut head = state.clone();
        let mut exit: Option<State> = None;
        for _ in 0..MAX_LOOP_PASSES {
            let mut pass = head.clone();
            let mut pass_scopes = scopes.clone();
            if let Some(condition) = condition {
                self.check_expr(condition, &mut pass, &mut pass_scopes, var_types);
            }
            if !endless {
                join_state(&mut exit, &pass);
            }
            self.loops.push(LoopFrame {
                depth: pass_scopes.len(),
                ..LoopFrame::default()
            });
            self.check_block(body, &mut pass, &mut pass_scopes, var_types);
            let frame = self.loops.pop().expect("loop frame");
            if let Some(breaks) = &frame.breaks {
                join_state(&mut exit, breaks);
            }
            let mut next = frame.continues;
            if !block_terminates(body) {
                join_state(&mut next, &pass);
            }
            let Some(mut next) = next else {
                break;
            };
            if let Some(step) = step {
                self.check_expr(step, &mut next, &mut pass_scopes, var_types);
            }
            if !join_changed(&mut head, &next) {
                break;
            }
        }
        // With no exit path the code after the loop is unreachable; keep the
        // head state for it.
        let after = exit.unwrap_or(head);
        for (name, var) in state.iter_mut() {
            if let Some(exit_var) = after.get(name) {
                *var = exit_var.clone();
            }
        }
    }

    /// Records a `break` (`is_break`) or `continue` path in the innermost
    /// loop after running the defers of the scopes it leaves.
    fn leave_loop(
        &mut self,
        is_break: bool,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        let Some(depth) = self.loops.last().map(|frame| frame.depth) else {
            return;
        };
        self.check_exit_defers(depth, state, scopes, var_types);
        // The exit sees the bindings that scopes inside the loop shadowed.
        let mut exit = state.clone();
        for scope in scopes.iter().skip(depth).rev() {
            for (name, old_state, ..) in scope.iter().rev() {
                match old_state {
                    Some(old) => {
                        exit.insert(name.clone(), old.clone());
                    }
                    None => {
                        exit.remove(name);
                    }
                }
            }
        }
        let frame = self.loops.last_mut().expect("loop frame");
        join_state(
            if is_break {
                &mut frame.breaks
            } else {
                &mut frame.continues
            },
            &exit,
        );
    }

    // ------------------------------------------------------------------
    // Expressions (per-path control flow)
    // ------------------------------------------------------------------

    #[allow(clippy::too_many_arguments)]
    fn check_expr(
        &mut self,
        expr: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        self.record_place(expr);
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if let Some(var) = state.get(&ident.name) {
                    if var.is_fully_moved() {
                        let reason = var
                            .move_reason
                            .unwrap_or(msg::note_value_explicitly_moved());
                        self.error_with_note(
                            msg::use_of_moved_value(&ident.name),
                            ident.span,
                            var.move_span,
                            reason,
                        );
                    } else if var.level == 1 {
                        let reason = var
                            .move_reason
                            .unwrap_or(msg::note_value_explicitly_moved());
                        self.error_with_note(
                            format!("use of partially moved value '{}'", &ident.name),
                            ident.span,
                            var.move_span,
                            reason,
                        );
                    }
                }
            }
            ast::ExpressionKind::Move(inner) => {
                self.reject_locked_move(inner, var_types);
                if self.reject_container_element_move(inner, var_types) {
                    self.check_expr(inner, state, scopes, var_types);
                    return;
                }
                self.check_place_operands(inner, state, scopes, var_types);
                // Phase 11 — Index place: try structured Place first for v[i] / x.a[i] etc.
                // Index projections are built via Place::new(local).index() /
                // push_projection(Projection::Index) and are index-insensitive (conservative).
                if let Some(place) = place_from_expr(inner) {
                    // If place contains Index, handle via Place-based move tracking
                    if place.projections.contains(&Projection::Index) {
                        let root_name = place.local.clone();
                        if state.contains_key(&root_name) {
                            // Check if already moved (whole or Index prefix)
                            if let Some(var) = state.get(&root_name) {
                                if var.is_fully_moved() {
                                    let reason = var
                                        .move_reason
                                        .unwrap_or(msg::note_value_explicitly_moved());
                                    self.error_with_note(
                                        msg::use_of_moved_value(&root_name),
                                        inner.span,
                                        var.move_span,
                                        reason,
                                    );
                                } else if let Some((move_span, reason)) = var.is_place_moved(&place)
                                {
                                    self.error_with_note(
                                        format!("use of moved field '{root_name}[index]'"),
                                        inner.span,
                                        Some(move_span),
                                        reason,
                                    );
                                } else {
                                    // Index element move — treat as non-Copy (owning) for soundness
                                    // unless proven Copy; for now move via Place.
                                    if let Some(v) = state.get_mut(&root_name) {
                                        v.mark_place_moved(
                                            &place,
                                            inner.span,
                                            msg::note_value_explicitly_moved(),
                                        );
                                    }
                                    return;
                                }
                            }
                            // If conflict already reported, still mark moved if possible
                            if let Some(v) = state.get_mut(&root_name) {
                                // Only mark if not already moved to avoid double-move error pollution
                                if v.is_place_moved(&place).is_none() && !v.is_fully_moved() {
                                    v.mark_place_moved(
                                        &place,
                                        inner.span,
                                        msg::note_value_explicitly_moved(),
                                    );
                                }
                            }
                        } else {
                            self.check_expr(inner, state, scopes, var_types);
                        }
                        return;
                    }
                }
                if let Some((root_name, path)) = expr_root_and_path(inner) {
                    if !self.is_path_tracked(&root_name, &path, var_types) {
                        self.check_expr(inner, state, scopes, var_types);
                    } else if path.is_empty() {
                        // `move x` (whole variable)
                        if let Some(var) = state.get_mut(&root_name) {
                            if var.is_fully_moved() {
                                let reason = var
                                    .move_reason
                                    .unwrap_or(msg::note_value_explicitly_moved());
                                self.error_with_note(
                                    msg::use_of_moved_value(&root_name),
                                    inner.span,
                                    var.move_span,
                                    reason,
                                );
                            } else if var.level == 1 {
                                let reason = var
                                    .move_reason
                                    .unwrap_or(msg::note_value_explicitly_moved());
                                self.error_with_note(
                                    format!(
                                        "cannot move already partially moved value '{root_name}'"
                                    ),
                                    inner.span,
                                    var.move_span,
                                    reason,
                                );
                            }
                            var.mark_moved(inner.span, msg::note_value_explicitly_moved());
                        }
                    } else {
                        // `move x.field` (partial field move) — now via `MovePathTree` + `Place::is_prefix_of`
                        if self.reject_partial_move_from_drop_ancestor(
                            &root_name, &path, inner.span, var_types,
                        ) {
                            return;
                        }
                        if let Some(var) = state.get_mut(&root_name) {
                            let place = place_from_root_and_path(&root_name, &path);
                            // Distinguish Copy vs Move: `Copy` types use `copy_from` (read-only).
                            let is_copy = !self.is_path_tracked(&root_name, &path, var_types);
                            if var.is_fully_moved() {
                                let reason = var
                                    .move_reason
                                    .unwrap_or(msg::note_value_explicitly_moved());
                                self.error_with_note(
                                    msg::use_of_moved_value(&root_name),
                                    inner.span,
                                    var.move_span,
                                    reason,
                                );
                            } else if let Some((move_span, reason)) = var.is_place_moved(&place) {
                                self.error_with_note(
                                    format!("use of moved field '{root_name}.{path}'"),
                                    inner.span,
                                    Some(move_span),
                                    reason,
                                );
                            } else if is_copy {
                                // Hybrid verbosity: Copy types are `copy_from` (no state change).
                                let _ = init::copy_from(&var.tree, &place);
                            } else {
                                var.mark_place_moved(
                                    &place,
                                    inner.span,
                                    msg::note_value_explicitly_moved(),
                                );
                            }
                        }
                    }
                } else {
                    self.check_expr(inner, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let is_consuming = method.name == "drop"
                    || self.consuming_receiver(expr.span, receiver, &method.name, arguments.len(), var_types);
                if is_consuming {
                    self.move_value_argument(
                        receiver,
                        msg::note_value_consumed_by_method(),
                        state,
                        scopes,
                        var_types,
                    );
                } else {
                    self.check_expr(receiver, state, scopes, var_types);
                }
                for (i, arg) in arguments.iter().enumerate() {
                    if self.method_has_value_argument(receiver, &method.name, i, var_types) {
                        self.move_value_argument(
                            arg,
                            msg::note_value_moved_into_param(),
                            state,
                            scopes,
                            var_types,
                        );
                    } else {
                        self.check_expr(arg, state, scopes, var_types);
                    }
                }
                // A borrowed receiver place must outlive the arguments too.
                // A computed index such as `slot(t)` in `items[slot(t)]`
                // already ran once, so only that place's state is read again.
                if !is_consuming && let Some(place) = place_from_expr(receiver) {
                    if is_plain_place(receiver) {
                        self.check_expr(receiver, state, scopes, var_types);
                    } else {
                        self.report_moved_index_place(&place, receiver.span, state);
                    }
                }
                self.recheck_borrowed_arguments(arguments, state, scopes, var_types);
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let fn_name = match function.kind.as_ref() {
                    ast::ExpressionKind::Identifier(ident) => Some(ident.name.clone()),
                    _ => None,
                };
                for (i, arg) in arguments.iter().enumerate() {
                    if self.is_value_call_argument(function, fn_name.as_deref(), i, var_types) {
                        self.move_value_argument(
                            arg,
                            msg::note_value_moved_into_param(),
                            state,
                            scopes,
                            var_types,
                        );
                    } else {
                        self.check_expr(arg, state, scopes, var_types);
                    }
                }
                self.recheck_borrowed_arguments(arguments, state, scopes, var_types);
                self.check_expr(function, state, scopes, var_types);
            }
            ast::ExpressionKind::Launch(inner) => {
                // Every launch argument is moved into the child thread.
                match inner.kind.as_ref() {
                    ast::ExpressionKind::Call {
                        function,
                        arguments,
                    } => {
                        for arg in arguments {
                            self.reject_locked_move(arg, var_types);
                            if let Some((root_name, path)) = expr_root_and_path(arg)
                                && state.contains_key(&root_name)
                            {
                                if path.is_empty() {
                                    if let Some(var) = state.get(&root_name)
                                        && var.is_moved()
                                    {
                                        let reason = var
                                            .move_reason
                                            .unwrap_or(msg::note_value_explicitly_moved());
                                        self.error_with_note(
                                            msg::use_of_moved_value(&root_name),
                                            arg.span,
                                            var.move_span,
                                            reason,
                                        );
                                    }
                                    if let Some(var) = state.get_mut(&root_name) {
                                        var.mark_moved(
                                            arg.span,
                                            msg::note_value_moved_into_launch(),
                                        );
                                    }
                                } else {
                                    if !path.is_empty()
                                        && self.reject_partial_move_from_drop_ancestor(
                                            &root_name, &path, arg.span, var_types,
                                        )
                                    {
                                        continue;
                                    }
                                    let place = place_from_root_and_path(&root_name, &path);
                                    if let Some(var) = state.get(&root_name)
                                        && let Some((move_span, reason)) =
                                            var.is_place_moved(&place)
                                    {
                                        self.error_with_note(
                                            format!("use of moved field '{root_name}.{path}'"),
                                            arg.span,
                                            Some(move_span),
                                            reason,
                                        );
                                    }
                                    if let Some(var) = state.get_mut(&root_name) {
                                        var.mark_place_moved(
                                            &place,
                                            arg.span,
                                            msg::note_value_moved_into_launch(),
                                        );
                                    }
                                }
                            } else {
                                self.check_expr(arg, state, scopes, var_types);
                            }
                        }
                        self.check_expr(function, state, scopes, var_types);
                    }
                    _ => self.check_expr(inner, state, scopes, var_types),
                }
            }
            ast::ExpressionKind::Wait(inner) => {
                // `wait t` consumes the Task handle: a second `wait t` is a
                // use of a moved value. Non-identifier tasks (e.g. `wait
                // tasks[0]`) cannot be tracked per-element in v1.
                match inner.kind.as_ref() {
                    ast::ExpressionKind::Identifier(ident) => {
                        if let Some(var) = state.get(&ident.name)
                            && var.is_moved()
                        {
                            let reason = var
                                .move_reason
                                .unwrap_or(msg::note_value_explicitly_moved());
                            self.error_with_note(
                                msg::use_of_moved_value(&ident.name),
                                ident.span,
                                var.move_span,
                                reason,
                            );
                        }
                        if let Some(var) = state.get_mut(&ident.name) {
                            var.mark_moved(inner.span, msg::note_task_handle_consumed());
                        }
                    }
                    _ => self.check_expr(inner, state, scopes, var_types),
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.check_place_operands(expr, state, scopes, var_types);
                if let Some(place) = place_from_expr(expr) {
                    if let Some(var) = state.get(&place.local.clone()) {
                        if var.is_fully_moved() {
                            let reason = var
                                .move_reason
                                .unwrap_or(msg::note_value_explicitly_moved());
                            self.error_with_note(
                                msg::use_of_moved_value(&place.local),
                                expr.span,
                                var.move_span,
                                reason,
                            );
                        } else if let Some((move_span, reason)) = var.is_place_moved(&place) {
                            let display = if place.projections.contains(&Projection::Index) {
                                format!("{}[index]", place.local)
                            } else {
                                format!("{}.{}", place.local, place_to_path_str(&place))
                            };
                            self.error_with_note(
                                format!("use of moved field '{display}'"),
                                expr.span,
                                Some(move_span),
                                reason,
                            );
                        }
                    }
                } else if let Some((root_name, path)) = expr_root_and_path(expr) {
                    if let Some(var) = state.get(&root_name) {
                        if var.is_fully_moved() {
                            let reason = var
                                .move_reason
                                .unwrap_or(msg::note_value_explicitly_moved());
                            self.error_with_note(
                                msg::use_of_moved_value(&root_name),
                                expr.span,
                                var.move_span,
                                reason,
                            );
                        } else {
                            let place = place_from_root_and_path(&root_name, &path);
                            if let Some((move_span, reason)) = var.is_place_moved(&place) {
                                self.error_with_note(
                                    format!("use of moved field '{root_name}.{path}'"),
                                    expr.span,
                                    Some(move_span),
                                    reason,
                                );
                            }
                        }
                    }
                } else {
                    self.check_expr(object, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Index { object, index } => {
                // Phase 11 — dynamic Index place: `v[i]` overlaps `v` and `v[j]` conservatively.
                // Use Place::new(local).index() / push_projection(Index) via place_from_expr.
                if let Some(place) = place_from_expr(expr) {
                    self.report_moved_index_place(&place, expr.span, state);
                    // Also check index expression itself for moves
                    self.check_expr(index, state, scopes, var_types);
                } else {
                    self.check_expr(object, state, scopes, var_types);
                    self.check_expr(index, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                self.check_expr(object, state, scopes, var_types);
                if let Some(s) = start {
                    self.check_expr(s, state, scopes, var_types);
                }
                if let Some(e) = end {
                    self.check_expr(e, state, scopes, var_types);
                }
                if let Some(st) = step {
                    self.check_expr(st, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Reference { expression, .. } => {
                self.check_expr(expression, state, scopes, var_types);
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == ast::BinaryOperator::Assign {
                    // Evaluate RHS first (in case it uses or moves resources).
                    self.transfer_value(right, state, scopes, var_types);
                    self.check_place_operands(left, state, scopes, var_types);

                    // Re-initialization handling — now via `MovePathTree` + `Place::is_prefix_of`
                    // Phase 11: Index places `v[i]` use Place::new(...).index() / Projection::Index
                    if let Some(place) = place_from_expr(left) {
                        if place.projections.contains(&Projection::Index) {
                            if let Some(var) = state.get_mut(&place.local.clone()) {
                                if var.is_fully_moved() {
                                    let reason = var
                                        .move_reason
                                        .unwrap_or(msg::note_value_explicitly_moved());
                                    self.error_with_note(
                                        msg::use_of_moved_value(&place.local),
                                        left.span,
                                        var.move_span,
                                        reason,
                                    );
                                } else {
                                    var.mark_place_reinitialized(&place);
                                }
                            }
                        } else if let Some((root_name, path)) = expr_root_and_path(left) {
                            if path.is_empty() {
                                if state.contains_key(&root_name) {
                                    state.insert(root_name.clone(), VarState::new_live());
                                }
                            } else if let Some(var) = state.get_mut(&root_name) {
                                if var.is_fully_moved() {
                                    let reason = var
                                        .move_reason
                                        .unwrap_or(msg::note_value_explicitly_moved());
                                    self.error_with_note(
                                        msg::use_of_moved_value(&root_name),
                                        left.span,
                                        var.move_span,
                                        reason,
                                    );
                                } else {
                                    let place2 = place_from_root_and_path(&root_name, &path);
                                    var.mark_place_reinitialized(&place2);
                                }
                            }
                        } else {
                            self.check_expr(left, state, scopes, var_types);
                        }
                    } else if let Some((root_name, path)) = expr_root_and_path(left) {
                        if path.is_empty() {
                            if state.contains_key(&root_name) {
                                state.insert(root_name.clone(), VarState::new_live());
                            }
                        } else if let Some(var) = state.get_mut(&root_name) {
                            if var.is_fully_moved() {
                                let reason = var
                                    .move_reason
                                    .unwrap_or(msg::note_value_explicitly_moved());
                                self.error_with_note(
                                    msg::use_of_moved_value(&root_name),
                                    left.span,
                                    var.move_span,
                                    reason,
                                );
                            } else {
                                let place = place_from_root_and_path(&root_name, &path);
                                var.mark_place_reinitialized(&place);
                            }
                        }
                    } else {
                        self.check_expr(left, state, scopes, var_types);
                    }
                } else {
                    self.check_expr(left, state, scopes, var_types);
                    self.check_expr(right, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Cast {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Comptime(operand) => {
                self.check_expr(operand, state, scopes, var_types);
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.check_expr(condition, state, scopes, var_types);
                self.check_expr(then_expr, state, scopes, var_types);
                self.check_expr(else_expr, state, scopes, var_types);
            }
            // `value ? fallback` consumes an owned value; codegen releases it.
            // The fallback runs, and moves into the result, only when the
            // value is absent.
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.transfer_place(value, state, scopes, var_types, false);
                let mut absent = state.clone();
                self.transfer_place(fallback, &mut absent, scopes, var_types, false);
                join_changed(state, &absent);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(condition, state, scopes, var_types);
                // Branches that never fall through (return / break / continue)
                // do not contribute their end-state to the merge.
                let then_terminates = block_terminates(then_branch);
                let else_terminates = else_branch.as_ref().is_some_and(block_terminates);
                let mut then_state = state.clone();
                let mut then_scopes = scopes.clone();
                self.check_block(then_branch, &mut then_state, &mut then_scopes, var_types);
                let mut else_state = state.clone();
                let mut else_scopes = scopes.clone();
                if let Some(else_branch) = else_branch {
                    self.check_block(else_branch, &mut else_state, &mut else_scopes, var_types);
                }
                // Merge: a variable moved on any *fall-through* path is
                // unusable afterwards.
                for (name, var) in state.iter_mut() {
                    let then_var = if then_terminates {
                        VarState::default()
                    } else {
                        then_state.get(name).cloned().unwrap_or_default()
                    };
                    let else_var = if else_terminates {
                        VarState::default()
                    } else {
                        else_state.get(name).cloned().unwrap_or_default()
                    };
                    var.merge_with(&then_var);
                    var.merge_with(&else_var);
                }
            }
            ast::ExpressionKind::While { condition, body } => {
                self.check_loop(Some(condition), body, None, state, scopes, var_types);
            }
            ast::ExpressionKind::ForIn { iterable, body, .. } => {
                self.check_expr(iterable, state, scopes, var_types);
                // The loop binding is a fresh per-iteration variable; the
                // original iterable is only borrowed (a copy feeds iteration).
                self.check_loop(None, body, None, state, scopes, var_types);
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                // The header declares into the loop's own scope, which ends
                // with the loop and restores what the header shadowed.
                scopes.push(Vec::new());
                // The header is a `let`, and codegen emits it as one.
                if let ast::PatternKind::Identifier(name) = &init.pattern.kind
                    && let Some(ty) = &init.type_annotation
                {
                    self.record_owner(name, ty, init.initializer.is_some());
                }
                if let Some(init_expr) = &init.initializer {
                    self.transfer_value(init_expr, state, scopes, var_types);
                }
                self.declare_pattern(
                    &init.pattern,
                    init.type_annotation.as_ref(),
                    state,
                    scopes,
                    var_types,
                );
                self.check_loop(
                    Some(condition),
                    body,
                    Some(increment),
                    state,
                    scopes,
                    var_types,
                );
                self.pop_scope(state, scopes, var_types);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expr(expression, state, scopes, var_types);
                let locked = self.locked_place(expression, var_types).map(|(_, ty)| ty);
                let scrutinee = self.receiver_type(expression, var_types);
                // Each arm is an independent path from the pre-match state;
                // arms that never fall through do not contribute. The
                // pre-match state joins only when no arm may match.
                let mut merged: Option<State> = None;
                // A heap container keeps owning its elements, so an arm cannot
                // move payloads out of one, as `move v[i]` cannot.
                // Arms after an unguarded catch-all never run.
                let reachable = arms
                    .iter()
                    .position(|arm| arm.guard.is_none() && !Self::pattern_is_refutable(&arm.pattern))
                    .map_or(arms.len(), |end| end + 1);
                let moves = arms[..reachable]
                    .iter()
                    .any(|arm| Self::pattern_has_move(&arm.pattern));
                let container_move = moves && self.reject_container_element_move(expression, var_types);
                if moves && !container_move {
                    self.reject_borrowed_scrutinee_move(expression);
                    // A field whose holder has its own destructor stays whole.
                    if let Some((root, path)) = expr_root_and_path(expression)
                        && !path.is_empty()
                    {
                        self.reject_partial_move_from_drop_ancestor(
                            &root,
                            &path,
                            expression.span,
                            var_types,
                        );
                    }
                }
                // An arm after an unguarded catch-all never runs; codegen
                // emits none, so it neither checks nor joins.
                let mut caught = false;
                let mut guarded = false;
                for arm in arms {
                    if caught {
                        break;
                    }
                    caught = arm.guard.is_none() && !Self::pattern_is_refutable(&arm.pattern);
                    if arm.guard.is_some() && Self::pattern_has_move(&arm.pattern) {
                        self.error(
                            "cannot move out of payload in a match arm with a guard".to_string(),
                            arm.pattern.span,
                        );
                    }
                    // The guard sees the arm's locked payloads too.
                    let saved = self.bind_locked_payloads(&arm.pattern, locked.as_ref());
                    // A guard runs only after its pattern matched, so a
                    // refutable arm misses with or without the guard's effects.
                    let missed = (arm.guard.is_some() && Self::pattern_is_refutable(&arm.pattern))
                        .then(|| state.clone());
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard, state, scopes, var_types);
                    }
                    let mut arm_state = state.clone();
                    // An earlier guard may have consumed the scrutinee, and
                    // this arm reads its payloads again.
                    // Only a place can be consumed by a guard. The scrutinee was
                    // evaluated once, above, so its call or index operands are
                    // not walked again.
                    if guarded
                        && Self::pattern_has_move(&arm.pattern)
                        && let Some(owner) = plain_prefix(expression)
                    {
                        self.check_expr(owner, &mut arm_state, &mut scopes.clone(), var_types);
                    }
                    guarded |= arm.guard.is_some();
                    let mut arm_scopes = scopes.clone();
                    if let Some(missed) = &missed {
                        join_changed(state, missed);
                    }
                    // An arm that moves a payload takes the scrutinee's
                    // payloads, so the scrutinee is moved-from after it until
                    // reinitialized. Codegen releases it, so no transfer is
                    // recorded.
                    let indexed = place_from_expr(expression)
                        .filter(|place| place.projections.contains(&Projection::Index));
                    if Self::pattern_has_move(&arm.pattern)
                        && !container_move
                        && let Some(place) = indexed
                    {
                        // An element place is index-insensitive, like `move v[i]`.
                        if let Some(var) = arm_state.get_mut(&place.local) {
                            var.mark_place_moved(
                                &place,
                                expression.span,
                                msg::note_value_moved_here(),
                            );
                        }
                    } else if Self::pattern_has_move(&arm.pattern)
                        && !container_move
                        && let Some((root, path)) = expr_root_and_path(expression)
                        && let Some(var) = arm_state.get_mut(&root)
                    {
                        if path.is_empty() {
                            var.mark_moved(expression.span, msg::note_value_moved_here());
                        } else {
                            var.mark_place_moved(
                                &place_from_root_and_path(&root, &path),
                                expression.span,
                                msg::note_value_moved_here(),
                            );
                        }
                    }
                    // A `move` binding owns its payload within the arm.
                    arm_scopes.push(Vec::new());
                    let owned = self.move_payload_bindings(&arm.pattern, scrutinee.as_ref());
                    for (binding, ty) in &owned {
                        self.record_owner(binding, ty, true);
                        self.declare(binding, Some(ty), &mut arm_state, &mut arm_scopes, var_types);
                    }
                    // A borrowed payload binding owns nothing, but it still
                    // hides an outer local of its name until the arm ends,
                    // and keeps its payload type for nested matches.
                    let typed = self.payload_bindings(&arm.pattern, scrutinee.as_ref(), true);
                    let saved_borrowed = self.borrowed_payloads.clone();
                    for binding in Self::payload_binding_names(&arm.pattern) {
                        if owned.iter().any(|(o, _)| o.name == binding.name) {
                            continue;
                        }
                        let name = binding.name.clone();
                        let old_state = arm_state.remove(&name);
                        let old_type = var_types.remove(&name);
                        let old_owner = self.owner_bindings.remove(&name);
                        if let Some((_, ty)) = typed.iter().find(|(t, _)| t.name == name) {
                            var_types.insert(name.clone(), ty.clone());
                        }
                        let old_borrowed = !self.borrowed_payloads.insert(name.clone());
                        arm_scopes.last_mut().expect("arm scope is pushed").push((
                            name,
                            old_state,
                            old_type,
                            old_owner,
                            None,
                            old_borrowed,
                        ));
                    }
                    // The arm's result is consumed even when the match is a
                    // statement: codegen moves it out, as it does a block's
                    // result, and it cannot hold the arm's locked payloads.
                    self.transfer_place(&arm.body, &mut arm_state, &mut arm_scopes, var_types, false);
                    self.pop_scope(&mut arm_state, &mut arm_scopes, var_types);
                    self.reject_locked_move(&arm.body, var_types);
                    self.locked_bindings = saved;
                    self.borrowed_payloads = saved_borrowed;
                    if !expression_terminates(&arm.body) {
                        // Join every fall-through arm, as an if/else does:
                        // two arms at the same level can still move
                        // different fields.
                        Self::join_exit(&mut merged, state, &arm_state);
                    }
                }
                if !self.match_is_exhaustive(arms, scrutinee.as_ref()) {
                    let fallthrough = state.clone();
                    Self::join_exit(&mut merged, state, &fallthrough);
                }
                // With no reachable exit the code after the match is dead.
                if let Some(merged) = merged {
                    *state = merged;
                }
            }
            ast::ExpressionKind::Block(block) => {
                self.check_block_yielding(block, true, state, scopes, var_types);
            }
            ast::ExpressionKind::Lambda { body, .. } => {
                // A lambda body has its own exits: its `return` does not run
                // the enclosing defers, and it cannot leave enclosing loops.
                let defers = std::mem::take(&mut self.defers);
                let loops = std::mem::take(&mut self.loops);
                self.check_block(body, state, scopes, var_types);
                self.defers = defers;
                self.loops = loops;
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => {
                            self.transfer_value(expr, state, scopes, var_types);
                        }
                        ast::InitializerItem::Field { value, .. } => {
                            self.transfer_value(value, state, scopes, var_types);
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.check_expr(index, state, scopes, var_types);
                            self.transfer_value(value, state, scopes, var_types);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.transfer_value(item, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.transfer_value(&field.value, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::EnumVariant { fields, .. } => {
                for field in fields {
                    self.check_expr(field, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.check_expr(input, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(expr) = arg {
                        self.check_expr(expr, state, scopes, var_types);
                    }
                }
            }
            ast::ExpressionKind::Literal(_) | ast::ExpressionKind::TypeName(_) => {}
        }
    }

    /// Updates `locked_bindings` for one arm of a match on `locked`, the type
    /// of a lock block's value or None: its owning payload bindings name
    /// storage the Mutex owns, and any binding shadows an outer one. Returns
    /// the previous bindings to restore after the arm.
    fn bind_locked_payloads(
        &mut self,
        pattern: &ast::Pattern,
        locked: Option<&ast::Type>,
    ) -> FxHashMap<String, ast::Type> {
        let saved = self.locked_bindings.clone();
        let bindings: Vec<Option<&ast::Identifier>> = match &pattern.kind {
            ast::PatternKind::Identifier(binding) | ast::PatternKind::Move(binding) => {
                self.locked_bindings.remove(&binding.name);
                return saved;
            }
            ast::PatternKind::Enum {
                data: Some(data), ..
            } => match &data.kind {
                ast::PatternKind::Tuple(items) => items
                    .iter()
                    .map(|item| match &item.kind {
                        ast::PatternKind::Identifier(binding) | ast::PatternKind::Move(binding) => {
                            Some(binding)
                        }
                        _ => None,
                    })
                    .collect(),
                ast::PatternKind::Identifier(binding) | ast::PatternKind::Move(binding) => {
                    vec![Some(binding)]
                }
                _ => return saved,
            },
            _ => return saved,
        };
        let ast::PatternKind::Enum { variant, .. } = &pattern.kind else {
            return saved;
        };
        let owner = locked.map(Facts::owner_key);
        let payloads = owner
            .as_ref()
            .and_then(|owner| {
                self.facts
                    .variant_payloads
                    .get(&(owner.clone(), variant.name.clone()))
            })
            .cloned()
            .unwrap_or_default();
        let substitutions = match (
            locked,
            owner
                .as_ref()
                .and_then(|owner| self.facts.struct_fields.get(owner)),
        ) {
            (Some(ty), Some(info)) => Self::generic_substitutions(ty, info),
            _ => FxHashMap::default(),
        };
        for (index, binding) in bindings.into_iter().enumerate() {
            let Some(binding) = binding else { continue };
            self.locked_bindings.remove(&binding.name);
            if let Some(payload) = payloads.get(index) {
                let ty = Self::substitute_type(payload, &substitutions);
                if self.facts.is_tracked(&ty) {
                    self.locked_bindings.insert(binding.name.clone(), ty);
                }
            }
        }
        saved
    }

    /// A pattern that can fail to match: anything but a binding or `_`.
    fn pattern_is_refutable(pattern: &ast::Pattern) -> bool {
        !matches!(
            pattern.kind,
            ast::PatternKind::Identifier(_) | ast::PatternKind::Wildcard
        )
    }

    /// The `move` payload bindings of `pattern` on a `scrutinee` value, with
    /// the payload type each takes.
    fn move_payload_bindings(
        &self,
        pattern: &ast::Pattern,
        scrutinee: Option<&ast::Type>,
    ) -> Vec<(ast::Identifier, ast::Type)> {
        self.payload_bindings(pattern, scrutinee, false)
    }

    /// The payload bindings of `pattern` with their payload types: the owned
    /// ones, or with `all` every named one, borrowed bindings included.
    fn payload_bindings(
        &self,
        pattern: &ast::Pattern,
        scrutinee: Option<&ast::Type>,
        all: bool,
    ) -> Vec<(ast::Identifier, ast::Type)> {
        let ast::PatternKind::Enum {
            variant,
            data: Some(data),
            ..
        } = &pattern.kind
        else {
            return Vec::new();
        };
        let bindings: Vec<Option<&ast::Identifier>> = match &data.kind {
            // One `move` element gives the arm every payload, so a named
            // sibling is owned too and its transfers must be tracked.
            ast::PatternKind::Tuple(items) if all || items.iter().any(Self::pattern_has_move) => items
                .iter()
                .map(|item| match &item.kind {
                    ast::PatternKind::Identifier(binding) | ast::PatternKind::Move(binding) => {
                        Some(binding)
                    }
                    _ => None,
                })
                .collect(),
            ast::PatternKind::Move(binding) => vec![Some(binding)],
            ast::PatternKind::Identifier(binding) if all => vec![Some(binding)],
            _ => return Vec::new(),
        };
        let Some(scrutinee) = scrutinee else {
            return Vec::new();
        };
        let owner = Facts::owner_key(scrutinee);
        let Some(payloads) = self
            .facts
            .variant_payloads
            .get(&(owner.clone(), variant.name.clone()))
        else {
            return Vec::new();
        };
        let substitutions = self
            .facts
            .struct_fields
            .get(&owner)
            .map(|info| Self::generic_substitutions(scrutinee, info))
            .unwrap_or_default();
        bindings
            .into_iter()
            .zip(payloads)
            .filter_map(|(binding, payload)| {
                Some((binding?.clone(), Self::substitute_type(payload, &substitutions)))
            })
            .collect()
    }

    /// Rejects an arm that moves payloads out of an enclosing arm's borrowed
    /// payload binding: the outer enum still owns them and drops them.
    fn reject_borrowed_scrutinee_move(&mut self, expression: &ast::Expression) {
        if let Some(place) = place_from_expr(expression)
            && self.borrowed_payloads.contains(&place.local)
        {
            self.error(msg::cannot_move_out_of_borrowed_payload(&place.local), expression.span);
        }
    }

    /// The identifiers an enum arm binds to its payloads.
    fn payload_binding_names(pattern: &ast::Pattern) -> Vec<&ast::Identifier> {
        let ast::PatternKind::Enum {
            data: Some(data), ..
        } = &pattern.kind
        else {
            return Vec::new();
        };
        let items: Vec<&ast::Pattern> = match &data.kind {
            ast::PatternKind::Tuple(items) => items.iter().collect(),
            _ => vec![data.as_ref()],
        };
        items
            .into_iter()
            .filter_map(|item| match &item.kind {
                ast::PatternKind::Identifier(binding) | ast::PatternKind::Move(binding) => {
                    Some(binding)
                }
                _ => None,
            })
            .collect()
    }

    /// Whether some arm always matches: an unguarded catch-all, or unguarded
    /// arms that take every variant of an enum scrutinee whatever its payloads.
    fn match_is_exhaustive(&self, arms: &[ast::MatchArm], scrutinee: Option<&ast::Type>) -> bool {
        let unguarded = || arms.iter().filter(|arm| arm.guard.is_none());
        if unguarded().any(|arm| !Self::pattern_is_refutable(&arm.pattern)) {
            return true;
        }
        let Some(variants) = scrutinee.and_then(|ty| self.facts.enum_variants.get(&Facts::owner_key(ty)))
        else {
            return false;
        };
        let binds_any = |p: &ast::Pattern| {
            matches!(
                p.kind,
                ast::PatternKind::Identifier(_) | ast::PatternKind::Move(_) | ast::PatternKind::Wildcard
            )
        };
        variants.iter().all(|name| {
            unguarded().any(|arm| match &arm.pattern.kind {
                ast::PatternKind::Enum { variant, data, .. } if &variant.name == name => {
                    match data.as_deref().map(|d| &d.kind) {
                        None => true,
                        Some(ast::PatternKind::Tuple(items)) => items.iter().all(binds_any),
                        Some(_) => binds_any(data.as_deref().expect("data is present")),
                    }
                }
                _ => false,
            })
        })
    }

    /// Joins one reachable exit of a match into `merged`, the first exit
    /// seeding it, over the names of the pre-match `state`.
    fn join_exit(merged: &mut Option<State>, state: &State, exit: &State) {
        match merged {
            None => {
                let mut seeded = state.clone();
                for (name, var) in seeded.iter_mut() {
                    *var = exit.get(name).cloned().unwrap_or_default();
                }
                *merged = Some(seeded);
            }
            Some(merged) => {
                for (name, var) in merged.iter_mut() {
                    let exit_var = exit.get(name).cloned().unwrap_or_default();
                    var.merge_with(&exit_var);
                }
            }
        }
    }

    fn pattern_has_move(pattern: &ast::Pattern) -> bool {
        match &pattern.kind {
            ast::PatternKind::Move(_) => true,
            ast::PatternKind::Enum { data, .. } => {
                data.as_ref().is_some_and(|p| Self::pattern_has_move(p))
            }
            ast::PatternKind::Tuple(items) => items.iter().any(Self::pattern_has_move),
            _ => false,
        }
    }
}

/// The longest plain place that `expr` projects from: `v` in `v[next()]`.
fn plain_prefix(expr: &ast::Expression) -> Option<&ast::Expression> {
    if is_plain_place(expr) {
        return Some(expr);
    }
    match expr.kind.as_ref() {
        ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } => plain_prefix(object),
        _ => None,
    }
}

/// A place whose re-evaluation has no effects: a local, its fields, and
/// elements at a literal or local index. A borrow of such a place can be
/// checked again without repeating a move its operands made.
fn is_plain_place(expr: &ast::Expression) -> bool {
    match expr.kind.as_ref() {
        ast::ExpressionKind::Identifier(_) => true,
        ast::ExpressionKind::FieldAccess { object, .. } => is_plain_place(object),
        ast::ExpressionKind::Index { object, index } => {
            is_plain_place(object)
                && matches!(
                    index.kind.as_ref(),
                    ast::ExpressionKind::Literal(_) | ast::ExpressionKind::Identifier(_)
                )
        }
        _ => false,
    }
}

/// The element type of indexing `ty`: an array's or slice's element, or the
/// one type argument of a container such as `Vec<T>`.
/// The type `ty[index]` yields; a tuple field needs a constant index.
/// Binds each bare type name in `pattern` (`Crate<T*>`, `Crate<[T, i64]>`) to
/// the matching part of `actual` (`Crate<Text*>`, so `T` is `Text`). A
/// reference or pointer on
/// one side only is looked through, since typeck may record a borrowed
/// container.
fn bind_type_arguments(
    pattern: &ast::Type,
    actual: &ast::Type,
    bindings: &mut FxHashMap<String, ast::Type>,
) {
    use ast::TypeKind::{Array, Generic, Named, Pointer, Reference, Slice, Tuple};
    match (pattern.kind.as_ref(), actual.kind.as_ref()) {
        (Reference(pattern), Reference(actual)) => {
            bind_type_arguments(&pattern.inner, &actual.inner, bindings);
        }
        (Pointer(pattern), Pointer(actual)) => {
            bind_type_arguments(&pattern.inner, &actual.inner, bindings);
        }
        (Named(named), _)
            if named.path.len() == 1 && named.generics.as_ref().is_none_or(Vec::is_empty) =>
        {
            bindings.insert(named.path[0].name.clone(), actual.clone());
        }
        (Generic(generic), _) if generic.args.is_empty() => {
            bindings.insert(generic.name.name.clone(), actual.clone());
        }
        (Named(pattern), Named(actual)) => {
            let parameters = pattern.generics.as_deref().unwrap_or_default();
            let arguments = actual.generics.as_deref().unwrap_or_default();
            for (parameter, argument) in parameters.iter().zip(arguments) {
                bind_type_arguments(parameter, argument, bindings);
            }
        }
        (Tuple(parameters), Tuple(arguments)) => {
            for (parameter, argument) in parameters.iter().zip(arguments) {
                bind_type_arguments(parameter, argument, bindings);
            }
        }
        (Array(pattern), Array(actual)) => {
            bind_type_arguments(&pattern.element_type, &actual.element_type, bindings);
        }
        (Slice(pattern), Slice(actual)) => {
            bind_type_arguments(&pattern.element_type, &actual.element_type, bindings);
        }
        (Reference(pattern), _) => bind_type_arguments(&pattern.inner, actual, bindings),
        (Pointer(pattern), _) => bind_type_arguments(&pattern.inner, actual, bindings),
        (_, Reference(actual)) => bind_type_arguments(pattern, &actual.inner, bindings),
        (_, Pointer(actual)) => bind_type_arguments(pattern, &actual.inner, bindings),
        _ => {}
    }
}

fn index_element_type(ty: &ast::Type, index: &ast::Expression) -> Option<ast::Type> {
    match ty.kind.as_ref() {
        ast::TypeKind::Tuple(fields) => match index.kind.as_ref() {
            ast::ExpressionKind::Literal(ast::Literal::Integer(position)) => {
                fields.get(usize::try_from(*position).ok()?).cloned()
            }
            _ => None,
        },
        ast::TypeKind::Array(array) => Some(array.element_type.as_ref().clone()),
        ast::TypeKind::Slice(slice) => Some(slice.element_type.as_ref().clone()),
        ast::TypeKind::Named(named) => match named.generics.as_deref() {
            Some([element]) => Some(element.clone()),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer, parser::Parser, semantic::typeck::TypeChecker, symbol_table::CompilerSymbolTable,
    };

    /// Stand-ins for std.sync's Mutex and Guard, which `shared` and `lock`
    /// lower to, and an owned payload type with borrowing `inspect` and
    /// `peek` overloads beside consuming ones.
    const PRELUDE: &str = "trait Drop { void drop(*self); }\n\
        struct Text { i64 len; }\n\
        impl Drop for Text { void drop(*self) {} }\n\
        impl Text {\n\
          void consume(self) {}\n\
          i64 inspect(&self) { return self.len; }\n\
          i64 inspect(self, i64 tag) { return tag; }\n\
          i64 inspect(&self, Sink sink) { return sink.n; }\n\
          i64 peek(&self) { return self.len; }\n\
          void peek(self, i64 tag, ...) {}\n\
          void absorb(self, i64... rest) {}\n\
        }\n\
        void eat(Text text) {}\n\
        void mixed(&Text seen, Text text) {}\n\
        Text fresh() { Text text; return text; }\n\
        struct Mutex<T> { T value; }\n\
        impl<T> Mutex<T> {\n\
          Mutex<T> new(T value) { Mutex<T> m; m.value = move value; return m; }\n\
          Guard<T> lock(&self) { Mutex<T>* owner = self; Guard<T> g; g.owner = owner; return g; }\n\
        }\n\
        struct Guard<T> { Mutex<T>* owner; }\n\
        impl<T> Guard<T> { T* get_ptr(&self) { return &self.owner.value; } }\n\
        enum Bin { Full(Text); Empty; }\n\
        struct Sink { i64 n; }\n\
        impl Sink { void take(&self, Text text) {} }\n\
        struct Slot<T> { T value; }\n\
        impl<T: Copy> Slot<T> { T get(&self) { return self.value; } }\n\
        struct Shelf { Text items[2]; }\n\
        i64 pick() { return 0; }\n\
        void keep(&Text seen, Shelf shelf) {}\n\
        trait IndexedAccess { type Item; Item __index_get(*self, i64 index); \
          void __index_set(*self, i64 index, Item value); }\n\
        struct Bag<T> { T* data; }\n\
        struct Card { Text text; i64 n; }\n\
        struct Other { Text n; }\n\
        struct Inner { Bag<Text> items; }\n\
        struct Outer { Inner inner; }\n\
        struct Count<T> { i64* data; }\n\
        impl<T> IndexedAccess for Count<T> {\n\
          type Item = i64;\n\
          i64 __index_get(*self, i64 index) { return self.data[index]; }\n\
          void __index_set(*self, i64 index, i64 value) { self.data[index] = value; }\n\
        }\n\
        struct Wrap<T> { Text keep; T inner; }\n\
        struct Crate<T> { Wrap<T>* data; }\n\
        impl<T> IndexedAccess for Crate<T> {\n\
          type Item = Wrap<T>;\n\
          Wrap<T> __index_get(*self, i64 index) { return self.data[index]; }\n\
          void __index_set(*self, i64 index, Wrap<T> value) { self.data[index] = move value; }\n\
        }\n\
        void sink<U>(U value) {}\n\
        struct Deck<T> { Card* data; }\n\
        impl<T> IndexedAccess for Deck<T> {\n\
          type Item = Card;\n\
          Card __index_get(*self, i64 index) { return self.data[index]; }\n\
          void __index_set(*self, i64 index, Card value) { self.data[index] = move value; }\n\
        }\n\
        impl<T> IndexedAccess for Bag<T> {\n\
          type Item = T;\n\
          T __index_get(*self, i64 index) { return self.data[index]; }\n\
          void __index_set(*self, i64 index, T value) { self.data[index] = move value; }\n\
        }\n";

    /// Messages from the type checker and from the move checker, kept apart
    /// because each reports some moves out of a lock.
    #[derive(Debug)]
    struct Errors {
        types: Vec<String>,
        moves: Vec<String>,
    }

    fn errors_in(body: &str) -> Errors {
        errors_with_items("", body)
    }

    /// Like `errors_in`, with top-level `items` declared before `main`. As in
    /// the driver, generic instances are appended before move checking.
    fn errors_with_items(items: &str, body: &str) -> Errors {
        let source = format!("{PRELUDE}{items}i64 main() {{\n{body}\nreturn 0;\n}}\n");
        let tokens = lexer::lex(&source).expect("probe should lex");
        let (mut program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        TypeChecker::resolve_type_aliases_in_program(&mut program);
        let mut checker = TypeChecker::new();
        let mut symbols = CompilerSymbolTable::new();
        let (types, mut monomorphs) = checker.check_program_with_table(&program, &mut symbols);
        let inferred_lets = checker.take_inferred_lets();
        crate::semantic::typeck::populate_inferred_let_types(&mut program, &inferred_lets);
        crate::semantic::monomorph::refresh_monomorph_bodies(&mut monomorphs, &program);
        let items = crate::semantic::monomorph::append_monomorphs(&mut program, &monomorphs, &[]);
        let instances = crate::parser::ast::Program {
            attributes: Vec::new(),
            items,
            comments: Vec::new(),
            span: program.span,
        };
        let _ = checker.check_program_with_table(&instances, &mut symbols);
        let moves =
            super::analyze_program_with_imports(
                &program,
                &[],
                checker.consuming_method_calls(),
                checker.indexed_elements(),
            )
                .errors;
        Errors {
            types: types.into_iter().map(|error| error.message).collect(),
            moves: moves.into_iter().map(|error| error.message).collect(),
        }
    }

    /// Errors for `arms`, the arms of a match on a locked `Bin`.
    fn locked_match_errors(arms: &str) -> Errors {
        errors_in(&format!(
            "Text text; Bin initial = Bin.Full(move text);\n\
             shared Bin state = move initial;\n\
             lock state as value {{ match value {{ {arms} }}; }}"
        ))
    }

    /// A heap container keeps owning an element read by index; only Copy
    /// elements leave by indexing.
    #[test]
    fn an_owned_element_cannot_leave_a_container_by_index() {
        for (container, misuse) in [
            ("Bag<Text>", "Bag<Text> bag; Text t = move bag[0];"),
            ("Bag<Text>", "Bag<Text> bag; eat(bag[0]);"),
            ("Bag<Text>", "Bag<Text> bag; Text t = bag[pick()];"),
            ("Bag<Card>", "Bag<Card> bag; Text t = move bag[0].text;"),
            ("Bag<Card>", "Bag<Card> bag; eat(bag[0].text);"),
            ("Bag<Card>", "Bag<Bag<Card>> bags; eat(bags[0][0].text);"),
            ("Deck<i64>", "Deck<i64> deck; eat(deck[0].text);"),
            ("Bag<Text>", "Bag<Outer> boxes; eat(boxes[0].inner.items[0]);"),
        ] {
            let rejected = super::msg::cannot_move_out_of_indexed_element(container);
            let errors = errors_in(misuse);
            assert!(errors.moves.contains(&rejected), "{misuse}: {errors:?}");
        }
        for control in [
            "Bag<i64> bag; i64 n = bag[0];",
            "Bag<Text> bag; i64 n = bag[0].inspect();",
            "Bag<Card> bag; i64 n = bag[0].n;",
            "Bag<Card> bag; i64 n = bag[0].text.inspect();",
            "Bag<Bag<Card>> bags; i64 n = bags[0][0].n;",
            "Deck<i64> deck; i64 n = deck[0].n;",
            // `Item` is Card, whose `n` is i64, though Other's `n` is owned.
            "Deck<Other> deck; i64 n = deck[0].n;",
            "Count<Text> counts; i64 n = counts[0];",
            "Bag<Outer> boxes; i64 n = boxes[0].inner.items[0].inspect();",
        ] {
            let errors = errors_in(control);
            assert!(errors.types.is_empty() && errors.moves.is_empty(), "{control}: {errors:?}");
        }
    }

    /// An instance of a generic body shares the body's index spans; its
    /// element type follows its own container, so `Crate<Text>`'s owned
    /// `inner` cannot leave by index although `Crate<T>`'s may be Copy.
    #[test]
    fn an_element_move_is_checked_against_every_instantiation() {
        let pull = "void pull<T>(Crate<T> crate) { sink(crate[0].inner); }\n\
                    struct Rack<T> { T* data; }\n\
                    impl<T> IndexedAccess for Rack<T*> {\n\
                      type Item = T;\n\
                      T __index_get(*self, i64 index) { return *(self.data[index]); }\n\
                      void __index_set(*self, i64 index, T value) {}\n\
                    }\n\
                    void unrack<T>(Rack<T*> rack) { sink(rack[0]); }\n\
                    void pair<T>(Crate<[T, i64]> crate) { sink(crate[0].inner); }\n";
        for (body, container, owned) in [
            ("Crate<i64> copies; pull(copies);", "Crate<Text>", false),
            ("Crate<i64> copies; Crate<Text> texts; pull(copies); pull(texts);", "Crate<Text>", true),
            // The parameter sits under a pointer in the impl's container.
            ("Rack<i64*> copies; unrack(copies);", "Rack<Text*>", false),
            ("Rack<Text*> texts; unrack(texts);", "Rack<Text*>", true),
            // The parameter sits inside a tuple argument.
            ("Crate<[i64, i64]> copies; pair(copies);", "Crate<[Text, i64]>", false),
            ("Crate<[Text, i64]> texts; pair(texts);", "Crate<[Text, i64]>", true),
        ] {
            let rejected = super::msg::cannot_move_out_of_indexed_element(container);
            let errors = errors_with_items(pull, body);
            assert!(errors.types.is_empty(), "{body}: {errors:?}");
            assert_eq!(errors.moves.contains(&rejected), owned, "{body}: {errors:?}");
        }
    }

    #[test]
    fn a_locked_payload_cannot_leave_its_lock() {
        let moved = crate::diagnostics::messages::move_out_of_lock("payload");
        for arms in [
            "Full(payload): eat(payload), Empty: {}",
            "Full(payload): payload.consume(), Empty: {}",
            "Full(payload): payload.inspect(1), Empty: 0",
            "Full(payload): { { i64 payload = 0; } eat(payload); }, Empty: {}",
            // A statement-position match's arm result is consumed.
            "Full(payload): payload, Empty: fresh()",
            "Full(payload): [payload, 0], Empty: [fresh(), 0]",
            "Full(payload): { Text out = true ? payload : fresh(); }, Empty: {}",
            "Full(payload): { for (Text item = payload; false; ) {} }, Empty: {}",
            // The loop header's binding ends with the loop.
            "Full(payload): { for (i64 payload = 0; payload < 1; payload += 1) {} eat(payload); }, \
             Empty: {}",
        ] {
            let errors = locked_match_errors(arms);
            assert!(errors.moves.contains(&moved), "{arms}: {errors:?}");
        }
    }

    #[test]
    fn a_locked_field_or_element_cannot_leave_its_lock() {
        let cases = [
            ("pair[...]", "Text out = pair[0];"),
            ("payload", "match pair[0] { Full(move payload): {}, Empty: {} };"),
        ];
        for (place, body) in cases {
            let errors = errors_in(&format!(
                "[Bin, i64] init = [Bin.Full(fresh()), 1];\n\
                 Mutex<[Bin, i64]> state = Mutex<[Bin, i64]>.new(move init);\n\
                 lock state as pair {{ {body} }}"
            ));
            let moved = crate::diagnostics::messages::move_out_of_lock(place);
            assert!(
                errors.types.contains(&moved) || errors.moves.contains(&moved),
                "{body}: {errors:?}"
            );
        }
        let errors = errors_in(
            "Sink k; shared Sink sinks = move k; shared Text state = fresh();\n\
             lock sinks as sink { lock state as text { sink.take(text); } }",
        );
        let moved = crate::diagnostics::messages::move_out_of_lock("text");
        assert!(errors.moves.contains(&moved), "{errors:?}");
    }

    #[test]
    fn a_locked_payload_can_be_borrowed_and_shadowed() {
        for arms in [
            "Full(payload): payload.inspect(), Empty: 0",
            "Full(payload): { i64 seen = match 0 { payload: payload }; }, Empty: {}",
            "Full(payload): { i64 payload = 1; i64 seen = payload; }, Empty: {}",
        ] {
            let errors = locked_match_errors(arms);
            assert!(errors.types.is_empty() && errors.moves.is_empty(), "{arms}: {errors:?}");
        }
    }

    #[test]
    fn a_borrowing_overload_keeps_its_receiver() {
        let errors = errors_in("Text text; i64 a = text.inspect(); i64 b = text.inspect();");
        assert!(errors.types.is_empty() && errors.moves.is_empty(), "{errors:?}");
        // A bare `...` adds no parameter, so `peek()` fits only the borrow.
        let errors = errors_in("Text text; i64 a = text.peek(); i64 b = text.peek();");
        assert!(errors.types.is_empty() && errors.moves.is_empty(), "{errors:?}");
        // Typeck's choice decides between overloads of one arity.
        let errors =
            errors_in("Text text; Sink k; i64 a = text.inspect(k); i64 b = text.inspect(k);");
        assert!(errors.types.is_empty() && errors.moves.is_empty(), "{errors:?}");
        for consumed in ["text.inspect(1);", "text.peek(1);", "text.absorb();", "text.absorb(1, 2);"]
        {
            let errors = errors_in(&format!("Text text; {consumed} i64 b = text.inspect();"));
            assert!(errors.types.is_empty(), "{consumed}: {errors:?}");
            assert!(
                errors.moves.iter().any(|message| message.contains("moved value 'text'")),
                "{consumed}: {errors:?}"
            );
        }
    }

    #[test]
    fn moves_follow_transfers_and_every_loop_exit() {
        for misuse in [
            "Text t; Text u = t; i64 n = t.inspect();",
            "Text t; Text u; u = t; i64 n = t.inspect();",
            "Text t; Mutex<Text> m = { .value = t }; i64 n = t.inspect();",
            "Text t; while (true) { eat(t); }",
            "Text t; while (true) { eat(t); break; } i64 n = t.inspect();",
            "Text t; i64 i = 0; while (i < 2) { i += 1; if (i == 1) { eat(t); continue; } } \
             i64 n = t.inspect();",
            "Text t; { defer eat(t); } i64 n = t.inspect();",
            "Text t; defer t.inspect(); eat(t);",
            "Text t; mixed(&t, move t);",
        ] {
            let errors = errors_in(misuse);
            assert!(
                errors.moves.iter().any(|message| message.contains("moved value 't'")),
                "{misuse}: {errors:?}"
            );
        }
        for control in [
            "Text t; Text u = move t; t = fresh(); i64 n = t.inspect();",
            "Text t; i64 i = 0; while (i < 2) { eat(t); t = fresh(); i += 1; } \
             i64 n = t.inspect();",
            "Text t; while (true) { eat(t); break; }",
            "Text t; { defer t.inspect(); } eat(t);",
            "Text t; Text u; mixed(&t, move u); eat(t);",
        ] {
            let errors = errors_in(control);
            assert!(
                errors.types.is_empty() && errors.moves.is_empty(),
                "{control}: {errors:?}"
            );
        }
    }

    #[test]
    fn move_bindings_of_a_multi_payload_variant_are_owned() {
        let items = "enum Twin { Both(Text, Text); Neither; }\n";
        let control = "Twin w = Twin.Both(fresh(), fresh()); \
                       match w { Both(move a, b): { eat(a); i64 n = b.inspect(); }, Neither: {} };";
        let errors = errors_with_items(items, control);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{control}: {errors:?}"
        );
        let misuse = "Twin w = Twin.Both(fresh(), fresh()); \
                      match w { Both(a, move b): { eat(b); i64 n = b.inspect(); }, Neither: {} };";
        let errors = errors_with_items(items, misuse);
        assert!(errors.types.is_empty(), "{misuse}: {errors:?}");
        assert!(
            errors.moves.iter().any(|error| error.contains("'b'")),
            "{misuse}: {errors:?}"
        );
        // The arm takes the scrutinee's payloads, so the scrutinee is
        // moved-from afterwards, while a Copy payload that reuses its name
        // stays readable inside the arm.
        let reuse = "Twin w = Twin.Both(fresh(), fresh()); \
                     match w { Both(move a, b): { eat(a); eat(b); }, Neither: {} }; \
                     match w { Both(a, b): {}, Neither: {} };";
        let errors = errors_with_items(items, reuse);
        assert!(
            errors.moves.iter().any(|error| error.contains("'w'")),
            "{reuse}: {errors:?}"
        );
        // A later arm's field move still reaches the join when an earlier
        // arm already moved another field of the same root.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     struct Holder { Twin twin; Text other; }\n";
        let joined = "Holder h; h.twin = Twin.Both(fresh(), fresh()); h.other = fresh(); \
                      match h.twin { Both(move a, b): { eat(a); eat(b); }, \
                      Neither: { eat(h.other); } }; eat(h.other);";
        let errors = errors_with_items(items, joined);
        assert!(!errors.moves.is_empty(), "{joined}: {errors:?}");
        let items = "enum Mixed { Both(i64, Text); Neither; }\n";
        let shadow = "Mixed w = Mixed.Both(4, fresh()); \
                      match w { Both(w, move leaf): { i64 n = w; eat(leaf); }, Neither: {} };";
        let errors = errors_with_items(items, shadow);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{shadow}: {errors:?}"
        );
        // A borrowed payload binding hides a moved-from local of its name,
        // and an exhaustive match that reinitializes it in every arm leaves
        // it live.
        for control in [
            "Mixed w = Mixed.Both(4, fresh()); \
             match w { Both(n, move leaf): { eat(leaf); }, Neither: {} }; \
             Mixed m = Mixed.Both(5, fresh()); \
             match m { Both(w, leaf): { i64 n = w; }, Neither: {} };",
            "Mixed w = Mixed.Both(4, fresh()); \
             match w { Both(n, move leaf): { eat(leaf); }, Neither: {} }; \
             Mixed m = Mixed.Neither; \
             match m { Both(n, leaf): { w = Mixed.Neither; }, Neither: { w = Mixed.Neither; } }; \
             match w { Both(n, leaf): {}, Neither: {} };",
        ] {
            let errors = errors_with_items(items, control);
            assert!(
                errors.types.is_empty() && errors.moves.is_empty(),
                "{control}: {errors:?}"
            );
        }
        for misuse in [
            // A match without a catch-all may match no arm.
            "Mixed w = Mixed.Both(4, fresh()); \
             match w { Both(n, move leaf): { eat(leaf); }, Neither: {} }; \
             Mixed m = Mixed.Neither; \
             match m { Both(n, leaf): { w = Mixed.Neither; } }; \
             match w { Both(n, leaf): {}, Neither: {} };",
            // A payload move from an array element moves the element.
            "Mixed items[2]; items[0] = Mixed.Both(4, fresh()); \
             match items[0] { Both(n, move leaf): { eat(leaf); }, Neither: {} }; \
             match items[0] { Both(n, leaf): {}, Neither: {} };",
        ] {
            let errors = errors_with_items(items, misuse);
            assert!(!errors.moves.is_empty(), "{misuse}: {errors:?}");
        }
        // One arm moving a whole field outweighs another moving part of it.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     struct Inner { i64 id; Twin twin; }\n\
                     struct Holder { Inner inner; }\n";
        let partial = "Holder h; h.inner.twin = Twin.Both(fresh(), fresh()); bool f = true; \
                       if (f) { match h.inner.twin { Both(move a, b): { eat(a); eat(b); }, \
                       Neither: {} }; } else { Inner gone = h.inner; } i64 n = h.inner.id;";
        let errors = errors_with_items(items, partial);
        assert!(!errors.moves.is_empty(), "{partial}: {errors:?}");
        // A borrowed payload stays owned by the matched enum, so a nested arm
        // cannot move its payloads; one bound with `move` can.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     enum Parcel { Held(Twin); Empty; }\n";
        let nested = "Twin w = Twin.Neither; Parcel c = Parcel.Held(Twin.Both(fresh(), fresh())); \
                      match c { Held(w): { match w { Both(move a, b): { Text kept = a; eat(a); }, \
                      Neither: {} }; }, Empty: {} };";
        let errors = errors_with_items(items, nested);
        assert!(
            errors.moves.contains(&crate::diagnostics::messages::cannot_move_out_of_borrowed_payload("w")),
            "{nested}: {errors:?}"
        );
        let owned = "Parcel c = Parcel.Held(Twin.Both(fresh(), fresh())); \
                     match c { Held(move w): { match w { Both(move a, b): { eat(a); eat(b); }, \
                     Neither: {} }; }, Empty: {} };";
        let errors = errors_with_items(items, owned);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{owned}: {errors:?}"
        );
        // A heap container keeps its elements.
        let items = "enum Twin { Both(Text, Text); Neither; }\n";
        let container = "Bag<Twin> bag; \
                         match bag[0] { Both(move a, b): { eat(a); }, Neither: {} };";
        let errors = errors_with_items(items, container);
        assert!(errors.types.is_empty(), "{container}: {errors:?}");
        assert!(
            errors.moves.contains(&crate::diagnostics::messages::cannot_move_out_of_indexed_element("Bag<Twin>")),
            "{container}: {errors:?}"
        );
        // A guard that consumes the scrutinee leaves nothing for a later arm
        // to take, and a field of a holder with a destructor stays whole.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     struct Kept { Twin twin; }\n\
                     impl Drop for Kept { void drop(*self) {} }\n\
                     bool used(Twin t) { return false; }\n";
        for (misuse, expected) in [
            (
                "Twin w = Twin.Both(fresh(), fresh()); \
                 match w { Both(a, b), used(w): {}, Both(move a, b): { eat(a); eat(b); }, \
                 Neither: {} };",
                "'w'",
            ),
            (
                "Kept k; k.twin = Twin.Both(fresh(), fresh()); \
                 match k.twin { Both(move a, b): { eat(a); eat(b); }, Neither: {} };",
                "implements Drop",
            ),
        ] {
            let errors = errors_with_items(items, misuse);
            assert!(errors.types.is_empty(), "{misuse}: {errors:?}");
            assert!(
                errors.moves.iter().any(|error| error.contains(expected)),
                "{misuse}: {errors:?}"
            );
        }
        // An arm after an unguarded catch-all never runs.
        let unreachable = "Twin w = Twin.Both(fresh(), fresh()); \
                           match w { _: {}, Both(move a, b): { eat(a); eat(b); } }; \
                           match w { Both(a, b): {}, Neither: {} };";
        let errors = errors_with_items(items, unreachable);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{unreachable}: {errors:?}"
        );
        let unreachable = "Kept k; k.twin = Twin.Both(fresh(), fresh()); \
                           match k.twin { _: {}, Both(move a, b): { eat(a); eat(b); } };";
        let errors = errors_with_items(items, unreachable);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{unreachable}: {errors:?}"
        );
        // A value scrutinee runs once, so a later move arm after a guard
        // does not consume its operands again.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     Twin pass(Twin t) { return t; }\n\
                     bool ok() { return true; }\n";
        let once = "Twin w = Twin.Both(fresh(), fresh()); \
                    match pass(w) { Both(a, b), ok(): {}, Both(move a, b): { eat(a); eat(b); }, \
                    Neither: {} };";
        let errors = errors_with_items(items, once);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{once}: {errors:?}"
        );
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     i64 index(Text t) { return 0; }\n";
        let once = "Twin items[2]; items[0] = Twin.Both(fresh(), fresh()); Text probe = fresh(); \
                    match items[index(probe)] { Neither, false: {}, \
                    Both(move a, b): { eat(a); eat(b); }, Neither: {} };";
        let errors = errors_with_items(items, once);
        assert!(
            errors.types.is_empty() && errors.moves.is_empty(),
            "{once}: {errors:?}"
        );
        // A nested block's shadow ends with the block, so the borrowed
        // payload binding is borrowed again after it.
        let items = "enum Twin { Both(Text, Text); Neither; }\n\
                     enum Parcel { Held(Twin); Empty; }\n";
        let shadowed = "Parcel c = Parcel.Held(Twin.Both(fresh(), fresh())); \
                        match c { Held(w): { if (true) { Twin w = Twin.Neither; } \
                        match w { Both(move a, b): { eat(a); eat(b); }, Neither: {} }; }, \
                        Empty: {} };";
        let errors = errors_with_items(items, shadowed);
        assert!(
            errors.moves.contains(&crate::diagnostics::messages::cannot_move_out_of_borrowed_payload("w")),
            "{shadowed}: {errors:?}"
        );
    }

    #[test]
    fn moves_follow_conditionals_headers_defers_and_payload_bindings() {
        for (misuse, name) in [
            ("Text t; bool f = true; Text u = f ? t : fresh(); i64 n = t.inspect();", "t"),
            ("Text t; for (Text u = t; false; ) {} i64 n = t.inspect();", "t"),
            ("Text t; while (true) { eat(t); { Text t = fresh(); break; } } i64 n = t.inspect();", "t"),
            ("Text t; defer t.inspect(); eat(t); { Text t = fresh(); return 0; }", "t"),
            (
                "Bin b = Bin.Full(fresh()); \
                 match b { Full(move p): { eat(p); i64 n = p.inspect(); }, Empty: {} };",
                "p",
            ),
            // A value block hands out its result before its defers run.
            (
                "Bin b = Bin.Empty; \
                 Text out = match b { Full(x): { Text leaf = fresh(); defer eat(leaf); leaf; }, \
                 Empty: fresh() };",
                "leaf",
            ),
            (
                "Text t; Bin b = Bin.Empty; \
                 Text out = match b { Full(x): { t; }, Empty: fresh() }; i64 n = t.inspect();",
                "t",
            ),
            // A match arm and a conditional argument pass the taken value on.
            (
                "Text t; Bin b = Bin.Empty; \
                 Text out = match b { Full(x): t, Empty: fresh() }; i64 n = t.inspect();",
                "t",
            ),
            ("Text t; bool f = true; eat(f ? t : fresh()); i64 n = t.inspect();", "t"),
            (
                "Text t; Optional<Text> none = Optional<Text>.None; Text u = none ? t; \
                 i64 n = t.inspect();",
                "t",
            ),
            (
                "Text t; Bin b = Bin.Empty; eat(match b { Full(x): t, Empty: fresh() }); \
                 i64 n = t.inspect();",
                "t",
            ),
        ] {
            let errors = errors_in(misuse);
            assert!(
                errors.moves.iter().any(|message| message.contains(&format!("moved value '{name}'"))),
                "{misuse}: {errors:?}"
            );
        }
        // The computed index is not evaluated twice, but its owner is checked.
        let errors = errors_in("Shelf s; keep(&s.items[pick()], move s);");
        assert!(
            errors.moves.iter().any(|message| message.contains("moved value 's")),
            "{errors:?}"
        );
        for control in [
            "Shelf s; Shelf other; keep(&s.items[pick()], move other);",
            "Text t; while (false) { eat(t); } i64 n = t.inspect();",
            "Text t; { eat(t); return 0; } i64 n = t.inspect();",
            "Text t; bool f = true; Text u = f ? fresh() : fresh(); i64 n = t.inspect();",
            "Text t; bool f = true; eat(f ? t : fresh());",
            "Text t; Optional<Text> none = Optional<Text>.None; Text u = none ? t;",
            "Text t; Bin b = Bin.Empty; eat(match b { Full(x): t, Empty: fresh() });",
            "Bin b = Bin.Empty; \
             Text out = match b { Full(x): { Text leaf = fresh(); defer leaf.inspect(); fresh(); }, \
             Empty: fresh() };",
        ] {
            let errors = errors_in(control);
            assert!(errors.moves.is_empty(), "{control}: {errors:?}");
        }
    }

    /// A copying accessor behind `T: Copy` serves Copy payloads only, since a
    /// copy of an owned payload would be dropped twice.
    #[test]
    fn a_copy_bound_rejects_owned_payloads() {
        let errors = errors_in("Slot<i64> n; n.value = 1; i64 v = n.get();");
        assert!(errors.types.is_empty() && errors.moves.is_empty(), "{errors:?}");
        let errors = errors_in("Slot<Text> t; t.value = fresh(); Text v = t.get();");
        let rejected = crate::diagnostics::messages::bound_requires_copy(&"Text");
        assert!(errors.types.iter().any(|message| message.contains(&rejected)), "{errors:?}");
    }
}
