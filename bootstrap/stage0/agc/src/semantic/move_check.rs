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
type ScopeEntry = (
    String,
    Option<VarState>,
    Option<ast::Type>,
    Option<super::init_plan::OwnerLocalId>,
);
/// Facts for move classification. TODO(Phase 5): centralize via `type_properties::{is_copy,needs_drop}`.
#[derive(Clone, Debug, Default)]
struct Facts {
    opaque_owners: FxHashSet<String>,
    /// Base type names that implement `Drop`.
    drop_owners: FxHashSet<String>,
    /// (owner, method) pairs whose receiver is by-value (`InstanceValue`).
    value_receivers: FxHashSet<(String, String)>,
    /// (function/method name, param index) pairs with a value (non-view) param.
    value_args: FxHashSet<(String, usize)>,
    /// (owner, method, explicit argument index) pairs with value parameters.
    value_method_args: FxHashSet<(String, String, usize)>,
    /// Struct field names and types for known struct types.
    struct_fields: FxHashMap<String, StructInfo>,
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
                            self.value_receivers
                                .insert((owner.clone(), func.name.name.clone()));
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
                            self.value_receivers.insert((
                                owner.split('<').next().unwrap_or(owner).to_string(),
                                method.to_string(),
                            ));
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

pub fn check_program(program: &ast::Program) -> Vec<MoveError> {
    check_program_with_imports(program, &[])
}

pub fn check_program_with_imports(
    program: &ast::Program,
    imported_modules: &[ModuleArtifact],
) -> Vec<MoveError> {
    analyze_program_with_imports(program, imported_modules).errors
}

pub struct MoveReport {
    pub errors: Vec<MoveError>,
    pub init_plan: super::init_plan::ProgramInitPlan,
}

pub fn analyze_program_with_imports(
    program: &ast::Program,
    imported_modules: &[ModuleArtifact],
) -> MoveReport {
    let facts = Facts::build(program, imported_modules);
    let mut checker = MoveChecker {
        layouts: OwnerLayouts(facts.clone()),
        facts,
        errors: Vec::new(),
        functions: FxHashMap::default(),
        current_plan: super::init_plan::FunctionInitPlan::default(),
        owner_bindings: FxHashMap::default(),
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
}

/// Layout completion belongs to the semantic ownership boundary. All required
/// fields participate, including Copy scalars read by a parent's destructor.
#[derive(Clone, Debug, Default)]
pub(crate) struct OwnerLayouts(Facts);

impl OwnerLayouts {
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
                    ast::TypeKind::Array(array) => {
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
        let Some((root, path)) = expr_root_and_path(receiver) else {
            return false;
        };
        self.get_field_type(&root, &path, var_types)
            .is_some_and(|ty| {
                self.facts.value_method_args.contains(&(
                    Facts::owner_key(&ty),
                    method.to_string(),
                    index,
                ))
            })
    }

    fn move_value_argument(
        &mut self,
        argument: &ast::Expression,
        reason: &'static str,
        state: &mut State,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        var_types: &mut FxHashMap<String, ast::Type>,
    ) {
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
        if !self.is_tracked(ty) {
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

    /// Tracked if Drop-owned (or `Task` handle). TODO(Phase 5): `type_properties::{is_copy,needs_drop}`.
    fn is_tracked(&self, ty: &ast::Type) -> bool {
        match ty.kind.as_ref() {
            ast::TypeKind::Array(arr) => self.is_tracked(&arr.element_type),
            ast::TypeKind::Tuple(types) => types.iter().any(|t| self.is_tracked(t)),
            ast::TypeKind::Named(named) => {
                let owner = Facts::owner_key(ty);
                (named.path.len() == 1 && named.path[0].name == "Task")
                    || self.facts.drop_owners.contains(&owner)
                    || self.facts.struct_fields.get(&owner).is_some_and(|info| {
                        let substitutions = Self::generic_substitutions(ty, info);
                        info.fields.iter().any(|(_, fty)| {
                            self.is_tracked(&Self::substitute_type(fty, &substitutions))
                        })
                    })
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

    /// A non-Copy field cannot be detached from any enclosing value whose own
    /// destructor must observe the complete value.
    fn partial_move_from_drop_ancestor(
        &self,
        root_name: &str,
        path: &str,
        var_types: &FxHashMap<String, ast::Type>,
    ) -> Option<(String, String)> {
        let target = self.get_field_type(root_name, path, var_types)?;
        if !self.is_tracked(&target) {
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
            self.is_tracked(&ty)
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
            if self.is_tracked(ty) {
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
            }
            var_types.insert(name.to_string(), ty.clone());
            scopes.last_mut().expect("scope stack is non-empty").push((
                name.to_string(),
                old_state,
                old_type,
                old_owner,
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
            for (name, old_state, old_type, old_owner) in scope {
                self.owner_bindings.remove(&name);
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
                if let ast::PatternKind::Identifier(name) = &let_stmt.pattern.kind
                    && let Some(ty) = &let_stmt.type_annotation
                {
                    self.record_owner(name, ty, let_stmt.initializer.is_some());
                }
                if let Some(init) = &let_stmt.initializer {
                    if let Some((root_name, path)) = expr_root_and_path(init)
                        && !path.is_empty()
                        && state.contains_key(&root_name)
                        && self.is_path_tracked(&root_name, &path, var_types)
                    {
                        // Preserve the ordinary read check before recording the
                        // implicit transfer. This catches a field moved earlier.
                        self.check_expr(init, state, scopes, var_types);
                        if !self.reject_partial_move_from_drop_ancestor(
                            &root_name, &path, init.span, var_types,
                        ) && let Some(var) = state.get_mut(&root_name)
                        {
                            self.record_transfer(init);
                            var.mark_place_moved(
                                &place_from_root_and_path(&root_name, &path),
                                init.span,
                                msg::note_value_moved_here(),
                            );
                        }
                    } else {
                        self.check_expr(init, state, scopes, var_types);
                    }
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
                    || self.consuming_receiver(receiver, &method.name, var_types);
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
                    if let Some((root_name, path)) = expr_root_and_path(right)
                        && !path.is_empty()
                        && state.contains_key(&root_name)
                        && self.is_path_tracked(&root_name, &path, var_types)
                    {
                        self.check_expr(right, state, scopes, var_types);
                        if !self.reject_partial_move_from_drop_ancestor(
                            &root_name, &path, right.span, var_types,
                        ) && let Some(var) = state.get_mut(&root_name)
                        {
                            self.record_transfer(right);
                            var.mark_place_moved(
                                &place_from_root_and_path(&root_name, &path),
                                right.span,
                                msg::note_value_moved_here(),
                            );
                        }
                    } else {
                        self.check_expr(right, state, scopes, var_types);
                    }
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
                self.declare_pattern(
                    &init.pattern,
                    init.type_annotation.as_ref(),
                    state,
                    scopes,
                    var_types,
                );
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
            ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => {
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
