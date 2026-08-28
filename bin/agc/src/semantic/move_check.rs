//! Definite-initialization and move checking.
//! `Place` + `MovePathTree` preserve field-level ownership; `TypeProperties`
//! distinguishes implicit Copy from ownership-transferring Move.
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use crate::semantic::init;
use crate::semantic::move_path::{InitState, MovePathTree};
use crate::semantic::place::{Place, Projection};
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

/// Lossless Place display (`x.a`, `x[index]`, `*p`) for diagnostics.
fn place_display(place: &Place) -> String {
    let mut s = place.local.clone();
    for p in &place.projections {
        match p {
            Projection::Field(f) => {
                s.push('.');
                s.push_str(f);
            }
            Projection::TupleField(i) => {
                s.push('.');
                s.push_str(&i.to_string());
            }
            Projection::Index => s.push_str("[index]"),
            Projection::Deref => s = format!("*{s}"),
        }
    }
    s
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
            if !init::is_initialized(&other.tree, place) && init::is_initialized(&self.tree, place)
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

/// Scope entry: (name, previous state, previous type) so shadowing restores.
type ScopeEntry = (String, Option<VarState>, Option<ast::Type>);
/// Facts for move classification. TODO(Phase 5): centralize via `type_properties::{is_copy,needs_drop}`.
#[derive(Default)]
struct Facts {
    /// Base type names that implement `Drop`.
    drop_owners: FxHashSet<String>,
    /// (owner, method) pairs whose receiver is by-value (`InstanceValue`).
    value_receivers: FxHashSet<(String, String)>,
    /// (function/method name, param index) pairs with a value (non-view) param.
    value_args: FxHashSet<(String, usize)>,
    /// Struct field names and types for known struct types.
    struct_fields: FxHashMap<String, Vec<(String, ast::Type)>>,
}

impl Facts {
    fn build(program: &ast::Program) -> Facts {
        let mut facts = Facts::default();
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(strct) => {
                    let fields = strct
                        .fields
                        .iter()
                        .map(|f| (f.name.name.clone(), f.field_type.clone()))
                        .collect();
                    facts.struct_fields.insert(strct.name.name.clone(), fields);
                }
                ast::ItemKind::Impl(imp) => {
                    let owner = Self::owner_key(&imp.self_type);
                    if imp
                        .trait_ref
                        .as_ref()
                        .is_some_and(|t| t.path.last().is_some_and(|seg| seg.name == "Drop"))
                    {
                        facts.drop_owners.insert(owner.clone());
                    }
                    for member in &imp.items {
                        if let ast::ImplItemKind::Function(func) = member {
                            if func.method_kind == ast::MethodKind::InstanceValue {
                                facts
                                    .value_receivers
                                    .insert((owner.clone(), func.name.name.clone()));
                            }
                            for (i, param) in func.parameters.iter().enumerate() {
                                if !Self::is_view_type(&param.param_type) {
                                    facts.value_args.insert((func.name.name.clone(), i));
                                }
                            }
                        }
                    }
                }
                ast::ItemKind::Function(func) => {
                    for (i, param) in func.parameters.iter().enumerate() {
                        if !Self::is_view_type(&param.param_type) {
                            facts.value_args.insert((func.name.name.clone(), i));
                        }
                    }
                }
                _ => {}
            }
        }
        facts
    }

    /// Pointer/reference types are views and are never consumed by value.
    fn is_view_type(ty: &ast::Type) -> bool {
        matches!(
            ty.kind.as_ref(),
            ast::TypeKind::Pointer(_) | ast::TypeKind::Reference(_)
        )
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
}

/// True if control never falls through `stmt` (return / break / continue).
fn statement_terminates(stmt: &ast::Statement) -> bool {
    match &stmt.kind {
        ast::StatementKind::Return(_)
        | ast::StatementKind::Break(_)
        | ast::StatementKind::Continue => true,
        ast::StatementKind::Block(block) => block_terminates(block),
        ast::StatementKind::Expression(expr) => expression_terminates(expr),
        _ => false,
    }
}

fn block_terminates(block: &ast::Block) -> bool {
    block.statements.last().is_some_and(statement_terminates)
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

pub fn check_program(program: &ast::Program) -> Vec<MoveError> {
    let facts = Facts::build(program);
    let mut checker = MoveChecker {
        facts,
        errors: Vec::new(),
    };
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                checker.check_function(&func.parameters, &func.body);
            }
            ast::ItemKind::Impl(imp) => {
                for member in &imp.items {
                    match member {
                        ast::ImplItemKind::Function(func) => {
                            checker.check_function(&func.parameters, &func.body);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            checker.check_function(&cast.parameters, &cast.body);
                        }
                        ast::ImplItemKind::AssociatedType(_) => {}
                    }
                }
            }
            _ => {}
        }
    }
    checker.errors
}

struct MoveChecker {
    facts: Facts,
    errors: Vec<MoveError>,
}

impl MoveChecker {
    fn consuming_receiver(
        &self,
        receiver: &ast::Expression,
        method: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> bool {
        let Some((root, path)) = expr_root_and_path(receiver) else {
            return false;
        };
        let Some(ty) = self.get_field_type(&root, &path, var_types) else {
            return false;
        };
        self.facts
            .value_receivers
            .contains(&(Facts::owner_key(&ty), method.to_string()))
    }

    /// Check expressions evaluated to compute indexed/dereferenced places.
    fn check_place_operands(
        &mut self,
        expr: &ast::Expression,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.check_place_operands(object, state, scopes, var_types);
            }
            ast::ExpressionKind::Index { object, index } => {
                self.check_place_operands(object, state, scopes, var_types);
                self.check_expr(index, state, scopes, var_types);
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => self.check_place_operands(operand, state, scopes, var_types),
            _ => {}
        }
    }

    fn check_function(&mut self, parameters: &[ast::Parameter], body: &ast::Block) {
        let mut state = State::default();
        let mut scopes: Vec<Vec<ScopeEntry>> = Vec::new();
        let mut var_types: FxHashMap<String, ast::Type> = FxHashMap::default();
        scopes.push(Vec::new());
        for param in parameters {
            self.declare(
                &param.name.name,
                Some(&param.param_type),
                &mut state,
                &mut scopes,
                &mut var_types,
            );
        }
        self.check_block(body, &mut state, &mut scopes, &mut var_types);
    }

    /// Tracked if Drop-owned (or `Task` handle). TODO(Phase 5): `type_properties::{is_copy,needs_drop}`.
    fn is_tracked(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => self.is_tracked(&arr.element_type),
            ast::TypeKind::Tuple(types) => types.iter().any(|t| self.is_tracked(t)),
            ast::TypeKind::Named(named) => {
                let owner = Facts::owner_key(ty);
                (named.path.len() == 1 && named.path[0].name == "Task")
                    || self.facts.drop_owners.contains(&owner)
                    || self
                        .facts
                        .struct_fields
                        .get(&owner)
                        .is_some_and(|fields| fields.iter().any(|(_, fty)| self.is_tracked(fty)))
            }
            _ => self.facts.drop_owners.contains(&Facts::owner_key(ty)),
        }
    }

    fn get_field_type(
        &self,
        root_name: &str,
        path: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<ast::Type> {
        let mut curr_ty = var_types.get(root_name)?.clone();
        if path.is_empty() {
            return Some(curr_ty);
        }
        for segment in path.split('.') {
            let owner = Facts::owner_key(&curr_ty);
            let fields = self.facts.struct_fields.get(&owner)?;
            let (_, next_ty) = fields.iter().find(|(name, _)| name == segment)?;
            curr_ty = next_ty.clone();
        }
        Some(curr_ty)
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
            self.is_tracked(&ty)
        } else {
            false
        }
    }

    fn declare(
        &mut self,
        name: &str,
        ty: Option<&ast::Type>,
        state: &mut State,
        scopes: &mut [Vec<ScopeEntry>],
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
        if ty.is_some_and(|t| self.is_tracked(t)) {
            let old_state = state.get(name).cloned();
            let old_type = var_types.get(name).cloned();
            state.insert(name.to_string(), VarState::new_live());
            if let Some(t) = ty {
                var_types.insert(name.to_string(), t.clone());
            }
            scopes.last_mut().expect("scope stack is non-empty").push((
                name.to_string(),
                old_state,
                old_type,
            ));
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
            for (name, old_state, old_type) in scope {
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
        self.errors.push(MoveError {
            message: message.into(),
            span,
            note_span: None,
            note_message: None,
        });
    }

    fn error_with_note(
        &mut self,
        message: impl Into<String>,
        span: Span,
        note_span: Option<Span>,
        note_message: impl Into<String>,
    ) {
        self.errors.push(MoveError {
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
        scopes.push(Vec::new());
        for stmt in &block.statements {
            self.check_statement(stmt, state, scopes, var_types);
        }
        self.pop_scope(state, scopes, var_types);
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
                if let Some(init) = &let_stmt.initializer {
                    self.check_expr(init, state, scopes, var_types);
                }
                if let ast::PatternKind::Identifier(ident) = &let_stmt.pattern.kind {
                    self.declare(
                        &ident.name,
                        let_stmt.type_annotation.as_ref(),
                        state,
                        scopes,
                        var_types,
                    );
                }
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, state, scopes, var_types);
            }
            ast::StatementKind::Return(Some(expr)) => {
                // Bare identifier returns are implicit moves (the drop flag is
                // cleared on the return path); the variable is dead afterwards.
                // Field returns (`return x.field;`) are views and do not move.
                match expr.kind.as_ref() {
                    ast::ExpressionKind::Identifier(ident) => {
                        if let Some(var) = state.get_mut(&ident.name) {
                            var.mark_moved(expr.span, msg::note_value_moved_by_return());
                        }
                    }
                    _ => self.check_expr(expr, state, scopes, var_types),
                }
            }
            ast::StatementKind::Return(None)
            | ast::StatementKind::Break(_)
            | ast::StatementKind::Continue => {}
            ast::StatementKind::Defer(inner) => {
                // Deferred bodies run at scope exit; walk them with the state
                // at registration so a defer that uses a later-moved variable
                // is caught conservatively.
                self.check_statement(inner, state, scopes, var_types);
            }
        }
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
                    || self.consuming_receiver(receiver, &method.name, var_types);
                self.check_place_operands(receiver, state, scopes, var_types);
                if is_consuming {
                    if let Some((root_name, path)) = expr_root_and_path(receiver)
                        && state.contains_key(&root_name)
                        && self.is_path_tracked(&root_name, &path, var_types)
                    {
                        if path.is_empty() {
                            if let Some(var) = state.get_mut(&root_name) {
                                var.mark_moved(receiver.span, msg::note_value_consumed_by_method());
                            }
                        } else if let Some(var) = state.get_mut(&root_name) {
                            let place = place_from_root_and_path(&root_name, &path);
                            var.mark_place_moved(
                                &place,
                                receiver.span,
                                msg::note_value_consumed_by_method(),
                            );
                        }
                    }
                } else {
                    self.check_expr(receiver, state, scopes, var_types);
                }
                for arg in arguments {
                    self.check_expr(arg, state, scopes, var_types);
                }
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
                    let is_val_arg = fn_name
                        .as_ref()
                        .is_some_and(|name| self.facts.value_args.contains(&(name.clone(), i)));
                    if is_val_arg
                        && let Some((root_name, path)) = expr_root_and_path(arg)
                        && state.contains_key(&root_name)
                        && self.is_path_tracked(&root_name, &path, var_types)
                    {
                        if path.is_empty() {
                            if let Some(var) = state.get_mut(&root_name) {
                                var.mark_moved(arg.span, msg::note_value_moved_into_param());
                            }
                        } else if let Some(var) = state.get_mut(&root_name) {
                            let place = place_from_root_and_path(&root_name, &path);
                            var.mark_place_moved(
                                &place,
                                arg.span,
                                msg::note_value_moved_into_param(),
                            );
                        }
                    } else {
                        self.check_expr(arg, state, scopes, var_types);
                    }
                }
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
                            self.error_with_note(
                                format!("use of moved field '{}[index]'", place.local),
                                expr.span,
                                Some(move_span),
                                reason,
                            );
                        }
                    }
                    // Also check index expression itself for moves
                    self.check_expr(index, state, scopes, var_types);
                } else {
                    self.check_expr(object, state, scopes, var_types);
                    self.check_expr(index, state, scopes, var_types);
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
                    self.check_expr(right, state, scopes, var_types);
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
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.check_expr(value, state, scopes, var_types);
                self.check_expr(fallback, state, scopes, var_types);
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
                self.check_expr(condition, state, scopes, var_types);
                // The body may run zero or more times; merge its effect into
                // the pre-loop state to a fixpoint (moves only accumulate).
                // A body that never falls through (always returns/breaks)
                // cannot move anything onto the loop-exit path.
                let body_terminates = block_terminates(body);
                for _ in 0..8 {
                    let mut body_state = state.clone();
                    let mut body_scopes = scopes.clone();
                    self.check_block(body, &mut body_state, &mut body_scopes, var_types);
                    let mut changed = false;
                    for (name, var) in state.iter_mut() {
                        let body_var = if body_terminates {
                            VarState::default()
                        } else {
                            body_state.get(name).cloned().unwrap_or_default()
                        };
                        if body_var.level > var.level {
                            var.merge_with(&body_var);
                            changed = true;
                        }
                    }
                    if !changed {
                        break;
                    }
                }
            }
            ast::ExpressionKind::ForIn { iterable, body, .. } => {
                self.check_expr(iterable, state, scopes, var_types);
                // The loop binding is a fresh per-iteration variable; the
                // original iterable is only borrowed (a copy feeds iteration).
                let body_terminates = block_terminates(body);
                for _ in 0..8 {
                    let mut body_state = state.clone();
                    let mut body_scopes = scopes.clone();
                    self.check_block(body, &mut body_state, &mut body_scopes, var_types);
                    let mut changed = false;
                    for (name, var) in state.iter_mut() {
                        let body_var = if body_terminates {
                            VarState::default()
                        } else {
                            body_state.get(name).cloned().unwrap_or_default()
                        };
                        if body_var.level > var.level {
                            var.merge_with(&body_var);
                            changed = true;
                        }
                    }
                    if !changed {
                        break;
                    }
                }
            }
            ast::ExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init_expr) = &init.initializer {
                    self.check_expr(init_expr, state, scopes, var_types);
                }
                if let ast::PatternKind::Identifier(ident) = &init.pattern.kind {
                    self.declare(
                        &ident.name,
                        init.type_annotation.as_ref(),
                        state,
                        scopes,
                        var_types,
                    );
                }
                self.check_expr(condition, state, scopes, var_types);
                let body_terminates = block_terminates(body);
                for _ in 0..8 {
                    let mut body_state = state.clone();
                    let mut body_scopes = scopes.clone();
                    self.check_block(body, &mut body_state, &mut body_scopes, var_types);
                    self.check_expr(increment, &mut body_state, &mut body_scopes, var_types);
                    let mut changed = false;
                    for (name, var) in state.iter_mut() {
                        let body_var = if body_terminates {
                            VarState::default()
                        } else {
                            body_state.get(name).cloned().unwrap_or_default()
                        };
                        if body_var.level > var.level {
                            var.merge_with(&body_var);
                            changed = true;
                        }
                    }
                    if !changed {
                        break;
                    }
                }
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expr(expression, state, scopes, var_types);
                // Each arm is an independent path from the pre-match state;
                // arms that never fall through do not contribute.
                let mut merged = state.clone();
                for arm in arms {
                    if arm.guard.is_some() && Self::pattern_has_move(&arm.pattern) {
                        self.error(
                            "cannot move out of payload in a match arm with a guard".to_string(),
                            arm.pattern.span,
                        );
                    }
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard, state, scopes, var_types);
                    }
                    let mut arm_state = state.clone();
                    let mut arm_scopes = scopes.clone();
                    self.check_expr(&arm.body, &mut arm_state, &mut arm_scopes, var_types);
                    if !expression_terminates(&arm.body) {
                        for (name, var) in merged.iter_mut() {
                            let arm_var = arm_state.get(name).cloned().unwrap_or_default();
                            if arm_var.level > var.level {
                                var.merge_with(&arm_var);
                            }
                        }
                    }
                }
                *state = merged;
            }
            ast::ExpressionKind::Block(block) => {
                self.check_block(block, state, scopes, var_types);
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr) => {
                            self.check_expr(expr, state, scopes, var_types);
                        }
                        ast::InitializerItem::Field { value, .. } => {
                            self.check_expr(value, state, scopes, var_types);
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.check_expr(index, state, scopes, var_types);
                            self.check_expr(value, state, scopes, var_types);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.check_expr(item, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.check_expr(&field.value, state, scopes, var_types);
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

#[cfg(test)]
mod tests {
    use super::*;

    const DROP: &str = "struct T { i64 p; }\n\
                        impl Drop<T> for T {\n\
                            void drop(T* self) { }\n\
                        }\n";

    fn errors(source: &str) -> Vec<String> {
        let program = parse(&format!("{DROP}{source}"));
        check_program(&program)
            .into_iter()
            .map(|e| e.message)
            .collect()
    }

    fn parse(source: &str) -> ast::Program {
        let tokens = crate::lexer::lex(source).expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens.clone());
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn use_after_explicit_move_errors() {
        let errs = errors("i32 f() { T t; move t; return t.p; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-move error, got {errs:?}"
        );
    }

    #[test]
    fn use_after_by_value_receiver_errors() {
        let errs = errors(
            "impl T { i64 consume(T self) { return self.p; } }\n\
             i32 g() { T t; t.consume(); return (i32)t.p; }",
        );
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-receiver-move error, got {errs:?}"
        );
    }
    #[test]
    fn use_after_consuming_field_receiver_errors() {
        let errs = errors(
            "impl T { i64 consume(T self) { return self.p; } }\n\
             struct Holder { T item; }\n\
             i32 g() { Holder h; h.item.consume(); return (i32)h.item.p; }",
        );
        assert!(
            errs.iter()
                .any(|m| m.contains("use of moved field 'h.item.p'")),
            "expected use-after-field-receiver error, got {errs:?}"
        );
    }

    #[test]
    fn dynamic_index_operand_is_checked() {
        let errs = errors(
            "struct Box { T item; }\n\
                    i32 g() { T items[1]; Box b; move b.item; return (i32)items[b.item.p].p; }",
        );
        assert!(
            errs.iter()
                .any(|m| m.contains("use of moved field 'b.item.p'")),
            "expected moved dynamic-index operand error, got {errs:?}"
        );
    }

    #[test]
    fn use_after_by_value_argument_errors() {
        let errs = errors(
            "void take(T t) { }\n\
             i32 g() { T t; take(t); return (i32)t.p; }",
        );
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-argument-move error, got {errs:?}"
        );
    }

    #[test]
    fn pointer_receiver_does_not_move() {
        let errs = errors(
            "impl T { void peek(T* self) { } }\n\
             i32 g() { T t; t.peek(); return (i32)t.p; }",
        );
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn explicit_drop_moves() {
        let errs = errors("i32 g() { T t; t.drop(); return (i32)t.p; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-drop error, got {errs:?}"
        );
    }

    #[test]
    fn conditional_move_in_terminated_branch_is_allowed() {
        // The drop + return path never falls through, so `t` is alive after.
        let errs = errors("i32 g(bool c) { T t; if (c) { t.drop(); return 0; } return (i32)t.p; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn conditional_move_any_path_errors() {
        let errs = errors("i32 g(bool c) { T t; if (c) { move t; } return (i32)t.p; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-conditional-move error, got {errs:?}"
        );
    }

    #[test]
    fn move_in_loop_errors_but_terminated_loop_ok() {
        let errs = errors("i32 g(bool c) { T t; while (c) { move t; } return (i32)t.p; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-loop-move error, got {errs:?}"
        );
        let ok = errors("i32 g(bool c) { T t; while (c) { move t; return 0; } return (i32)t.p; }");
        assert!(ok.is_empty(), "unexpected errors: {ok:?}");
    }

    #[test]
    fn moved_parameter_use_errors() {
        let errs = errors("void g(T t) { move t; t.p = 1; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-move error, got {errs:?}"
        );
    }

    #[test]
    fn copyable_values_are_not_tracked() {
        let errs = errors("i32 g() { i32 x = 1; i32 y = x; move x; i32 z = x; return z; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn reassignment_to_moved_value_reinitializes() {
        let errs = errors("void g() { T t; move t; t = T.new(); (i32)t.p; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn field_assignment_on_moved_value_errors() {
        let errs = errors("void g() { T t; move t; t.p = (i32*)0; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-of-moved error for field write, got {errs:?}"
        );
    }

    #[test]
    fn use_after_second_move_errors() {
        let errs = errors("void g() { T t; move t; t = T.new(); move t; (i32)t.p; }");
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-of-moved error, got {errs:?}"
        );
    }

    #[test]
    fn field_extraction_and_null_out_is_allowed() {
        // The std idiom: extract a field with move, then null it so the
        // container's cascade is a no-op. The container is not moved.
        let errs = errors(
            "struct U { u8* data; }\n\
             impl Drop<U> for U { void drop(U* self) { } }\n\
             u8* take(U self) { u8* d = move self.data; self.data = (u8*)0; return d; }",
        );
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn bare_return_moves_and_makes_later_use_error() {
        // `return t;` is an implicit move; a subsequent use in the same
        // function is a use-after-move.
        let errs = errors(
            "T make() { T t; return t; }\n\
             i32 g() { T t; t = make(); move t; return (i32)t.p; }",
        );
        assert!(
            errs.iter().any(|m| m.contains("use of moved value 't'")),
            "expected use-after-move error, got {errs:?}"
        );
    }

    #[test]
    fn move_error_carries_origin_note() {
        let program = parse(&format!(
            "{DROP}i32 f() {{ T t; move t; return (i32)t.p; }}"
        ));
        let errs = check_program(&program);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("use of moved value 't'"));
        assert!(errs[0].note_span.is_some());
        assert_eq!(
            errs[0].note_message.as_deref(),
            Some(msg::note_value_explicitly_moved())
        );
    }

    #[test]
    fn partial_field_move_allows_other_fields() {
        let errs = errors(
            "struct Pair { T left; T right; }\n\
             void consume(T _t) { }\n\
             void f() {\n\
                 Pair p;\n\
                 consume(move p.left);\n\
                 consume(move p.right);\n\
             }",
        );
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn partial_field_move_prevents_whole_use() {
        let errs = errors(
            "struct Pair { T left; T right; }\n\
             void consume(T _t) { }\n\
             void consume_pair(Pair _p) { }\n\
             void f() {\n\
                 Pair p;\n\
                 consume(move p.left);\n\
                 consume_pair(move p);\n\
             }",
        );
        assert!(
            errs.iter().any(|m| m.contains("partially moved value 'p'")),
            "expected partially moved error, got {errs:?}"
        );
    }

    #[test]
    fn partial_field_move_reinitialization_restores_whole_use() {
        let errs = errors(
            "struct Pair { T left; T right; }\n\
             void consume(T _t) { }\n\
             void consume_pair(Pair _p) { }\n\
             void f() {\n\
                 Pair p;\n\
                 consume(move p.left);\n\
                 p.left = T.new();\n\
                 consume_pair(move p);\n\
             }",
        );
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn foo_partial_move_via_place_overlaps() {
        // Acceptance: `String b = move x.a` where x.b remains readable, x.a errors.
        // Uses T as non-Copy Stand-in for String (has Drop). Verifies Place::overlaps
        // distinguishes siblings (x.a vs x.b) not string prefix hack.
        let ok = errors(
            "struct Foo { T a; T b; }\n\
             void g() { Foo x; T y = move x.a; T z = move x.b; }",
        );
        assert!(
            ok.is_empty(),
            "x.b should stay readable after move x.a via Place::overlaps, got {ok:?}"
        );
        let errs = errors(
            "struct Foo { T a; T b; }\n\
             void g() { Foo x; T y = move x.a; T z = move x.a; }",
        );
        assert!(
            errs.iter().any(|m| m.contains("moved field")),
            "expected error for x.a after move x.a, got {errs:?}"
        );
    }

    #[test]
    fn foo_partial_move_reinit_via_initialize() {
        // Acceptance: x.a = make() reinitializes via `initialize` clearing children.
        let errs = errors(
            "struct Foo { T a; T b; }\n\
             void g() { Foo x; T y = move x.a; x.a = T.new(); T z = move x.a; T w = move x.b; }",
        );
        assert!(
            errs.is_empty(),
            "after x.a = T.new() via initialize, both fields should be readable, got {errs:?}"
        );
    }

    #[test]
    fn foo_partial_move_whole_and_reinit_via_place() {
        let errs = errors(
            "struct Foo { T a; T b; }\n\
             void consume_foo(Foo f) {}\n\
             void g() { Foo x; T y = move x.a; consume_foo(move x); }",
        );
        assert!(
            errs.iter().any(|m| m.contains("partially moved")),
            "expected partially moved for whole x after partial move, got {errs:?}"
        );
        let ok = errors(
            "struct Foo { T a; T b; }\n\
             void consume_foo(Foo f) {}\n\
             void g() { Foo x; T y = move x.a; x.a = T.new(); consume_foo(move x); }",
        );
        assert!(
            ok.is_empty(),
            "after reinit whole move should succeed via Place reinit, got {ok:?}"
        );
    }

    #[test]
    fn string_foo_partial_move_string_fields_via_place() {
        // Exact spec: struct Foo { String a; String b; } Foo x; String y = move x.a;
        // Use x.b ok, use x.a error, x.a = make(); use x.a ok.
        // String is made trackable via explicit Drop impl in test source.
        let ok = errors(
            "impl Drop<String> for String { void drop(String* self) {} }\n\
             struct Foo { String a; String b; }\n\
             String make_string() { String s; return s; }\n\
             void g() { Foo x; String y = move x.a; String z = x.b; }",
        );
        assert!(
            ok.is_empty(),
            "x.b should be readable after move x.a (String) via Place::overlaps, got {ok:?}"
        );
        let errs = errors(
            "impl Drop<String> for String { void drop(String* self) {} }\n\
             struct Foo { String a; String b; }\n\
             void g() { Foo x; String y = move x.a; String z = x.a; }",
        );
        // Note: `String z = x.a` without move may be copy check; test explicit move
        let errs2 = errors(
            "impl Drop<String> for String { void drop(String* self) {} }\n\
             struct Foo { String a; String b; }\n\
             void g() { Foo x; String y = move x.a; String z = move x.a; }",
        );
        assert!(
            errs2.iter().any(|m| m.contains("moved field")),
            "expected moved field error for String x.a, got {errs:?} / {errs2:?}"
        );
        let ok2 = errors(
            "impl Drop<String> for String { void drop(String* self) {} }\n\
             struct Foo { String a; String b; }\n\
             String make_string() { String s; return s; }\n\
             void g() { Foo x; String y = move x.a; x.a = make_string(); String z = move x.a; String w = move x.b; }",
        );
        assert!(
            ok2.is_empty(),
            "after x.a = make_string() reinit, both String fields should be readable via initialize, got {ok2:?}"
        );
    }
}
