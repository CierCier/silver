//! Borrow checker: enforces `&`/`&mut` exclusivity; raw pointers are unchecked.
//! Places make field conflicts structural instead of string-prefix based.
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use crate::semantic::typeck::{CallSiteKey, ResolvedCallable};
use crate::semantic::place::{Place, Projection, places_overlap};
use rustc_hash::{FxHashMap, FxHashSet};

/// One borrow conflict diagnostic with optional multi-span note.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BorrowError {
    pub message: String,
    pub span: Span,
    pub note_span: Option<Span>,
    pub note_message: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorrowKind {
    Shared,
    Exclusive,
}

impl BorrowKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            BorrowKind::Shared => "shared",
            BorrowKind::Exclusive => "mutable",
        }
    }
}

/// Metadata about a reference variable. TODO(Place Phase 1): replace `(root,path)` with `Place`.
#[derive(Debug, Clone)]
pub struct RefVarInfo {
    pub root: String,
    pub path: String,
    pub kind: BorrowKind,
    pub span: Span,
}

/// Active loan on `root.path`. TODO(Place Phase 1): replace with `Place` + `Place::overlaps`.
#[derive(Debug, Clone)]
pub struct ActiveBorrow {
    pub root: String,
    pub path: String,
    pub kind: BorrowKind,
    pub span: Span,
    pub borrower: Option<String>,
    /// Aggregate field path holding this reference, when it is field-stored.
    pub borrower_path: Option<String>,
    /// True only for references stored in aggregate fields.
    pub aggregate_field: bool,
    /// Lexical binding scopes captured when the loan was created.
    pub borrower_scope: usize,
    pub referent_scope: Option<usize>,
    /// Statement index after which this loan expires (NLL); `None` = scope exit.
    pub last_use: Option<usize>,
    /// Reference-parameter loan: live for whole function, never NLL-expired.
    pub param: bool,
}

/// Place-based borrow (Phase 8 scaffolding, parallel to `ActiveBorrow`). Overlap via `places_overlap`.
/// `&x.a` vs `move x.b` allowed (disjoint); `&x` vs `move x.a` overlaps (prefix).
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Borrow {
    pub place: Place,
    pub kind: BorrowKind,
}

/// `true` iff places overlap (one is prefix of the other on same local).
#[allow(dead_code)]
pub fn borrow_conflicts(a: &Place, b: &Place) -> bool {
    places_overlap(a, b)
}

/// `true` iff `place` overlaps any active borrow (any `&x.a` vs `move x.b` disjoint else conflict).
#[allow(dead_code)]
pub fn does_move_conflict_with_borrow(place: &Place, borrows: &[Borrow]) -> bool {
    borrows.iter().any(|b| places_overlap(&b.place, place))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallAccessKind {
    Exclusive,
    Shared,
    Read,
}

impl CallAccessKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Exclusive => "mutable",
            Self::Shared => "shared",
            Self::Read => "direct read",
        }
    }
}

struct CallArgAccess {
    // String-based place for call-arg overlap checks — mirrors ActiveBorrow's
    // `(root, path)` model. Overlap in `check_call` uses `paths_overlap`.
    // TODO(Place Phase 1): `Place` key — `semantic::place::Place` with pure
    // `overlaps` helper will replace `(String,String)` here as well.
    root: String,
    path: String,
    kind: CallAccessKind,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ReturnCallKey {
    name: String,
    arity: usize,
    method: bool,
}

#[derive(Debug, Clone)]
struct ReturnBorrowSummary {
    parameter_indices: FxHashSet<usize>,
    kind: BorrowKind,
}

fn named_type_name(ty: &ast::Type) -> Option<&str> {
    match ty.kind.as_ref() {
        ast::TypeKind::Named(named) => named.path.last().map(|name| name.name.as_str()),
        ast::TypeKind::Reference(reference) => named_type_name(&reference.inner),
        ast::TypeKind::Pointer(pointer) => named_type_name(&pointer.inner),
        _ => None,
    }
}

pub fn check_program(program: &ast::Program) -> Vec<BorrowError> {
    let report = crate::semantic::escape_check::analyze_program(program);
    check_program_with_return_origins(program, &report.return_origins)
}

pub fn check_program_with_return_origins(
    program: &ast::Program,
    return_origins: &[crate::semantic::escape_check::ReturnOrigin],
) -> Vec<BorrowError> {
    check_program_with_resolved_calls(program, return_origins, &FxHashMap::default())
}

pub fn check_program_with_resolved_calls(
    program: &ast::Program,
    return_origins: &[crate::semantic::escape_check::ReturnOrigin],
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
) -> Vec<BorrowError> {
    let mut checker = BorrowChecker::new(return_origins, resolved_callables);
    checker.check_program(program);
    checker.errors
}

struct BorrowChecker<'a> {
    errors: Vec<BorrowError>,
    /// Stack of lexical scopes containing active loans registered in each block.
    scopes: Vec<Vec<ActiveBorrow>>,
    /// Stack of last-use maps per lexical block.
    block_last_uses: Vec<FxHashMap<String, usize>>,
    /// Aggregate field reads and replacements per lexical block.
    block_field_reads: Vec<FxHashMap<(String, String), Vec<usize>>>,
    block_field_writes: Vec<FxHashMap<(String, String), Vec<usize>>>,
    /// Local declaration scopes, used to tell a local referent from a
    /// reference parameter or an outer binding with the same spelling.
    local_scopes: Vec<FxHashMap<String, usize>>,
    /// Statement indices of the nested blocks currently being visited.
    statement_indices: Vec<usize>,
    /// Maps struct name -> set of field names that are reference types (`&T` / `&'a T`).
    struct_ref_fields: FxHashMap<String, FxHashSet<String>>,
    struct_field_types: FxHashMap<String, FxHashMap<String, String>>,
    global_var_types: FxHashMap<String, String>,
    method_receiver_access: FxHashMap<String, FxHashMap<String, CallAccessKind>>,
    method_argument_access: FxHashMap<String, FxHashMap<String, Vec<Option<CallAccessKind>>>>,
    /// Maps local variable name -> struct type name.
    var_types: FxHashMap<String, String>,
    /// Active reference variables currently in scope (`r -> RefVarInfo`).
    ref_bindings: FxHashMap<String, RefVarInfo>,
    /// Variables declared as raw pointers (`T*`), whose dereferences bypass borrow check.
    raw_ptr_vars: FxHashSet<String>,
    /// Function return origins map parameters to call results for loan tracking.
    return_borrow_summaries: FxHashMap<ReturnCallKey, ReturnBorrowSummary>,
    resolved_return_borrow_summaries: FxHashMap<crate::semantic::typeck::CallableIdentity, ReturnBorrowSummary>,
    resolved_callables: &'a FxHashMap<CallSiteKey, ResolvedCallable>,
}

impl<'a> BorrowChecker<'a> {
    fn new(
        return_origins: &[crate::semantic::escape_check::ReturnOrigin],
        resolved_callables: &'a FxHashMap<CallSiteKey, ResolvedCallable>,
    ) -> Self {
        let mut return_borrow_summaries: FxHashMap<ReturnCallKey, ReturnBorrowSummary> =
            FxHashMap::default();
        let mut resolved_return_borrow_summaries = FxHashMap::default();
        for origin in return_origins {
            let key = ReturnCallKey {
                name: origin.function.clone(),
                arity: origin.arity,
                method: origin.is_method,
            };
            let entry = return_borrow_summaries
                .entry(key)
                .or_insert_with(|| ReturnBorrowSummary {
                    parameter_indices: FxHashSet::default(),
                    kind: if origin.is_mutable {
                        BorrowKind::Exclusive
                    } else {
                        BorrowKind::Shared
                    },
                });
            entry
                .parameter_indices
                .extend(origin.borrow_param_indices.iter().copied());
            if origin.is_mutable {
                entry.kind = BorrowKind::Exclusive;
            }
            let resolved_entry = resolved_return_borrow_summaries
                .entry(origin.callable)
                .or_insert_with(|| ReturnBorrowSummary {
                    parameter_indices: FxHashSet::default(),
                    kind: if origin.is_mutable {
                        BorrowKind::Exclusive
                    } else {
                        BorrowKind::Shared
                    },
                });
            resolved_entry
                .parameter_indices
                .extend(origin.borrow_param_indices.iter().copied());
            if origin.is_mutable {
                resolved_entry.kind = BorrowKind::Exclusive;
            }
        }
        Self {
            errors: Vec::new(),
            scopes: vec![Vec::new()],
            block_last_uses: Vec::new(),
            block_field_reads: Vec::new(),
            block_field_writes: Vec::new(),
            local_scopes: vec![FxHashMap::default()],
            statement_indices: Vec::new(),
            struct_ref_fields: FxHashMap::default(),
            struct_field_types: FxHashMap::default(),
            global_var_types: FxHashMap::default(),
            method_receiver_access: FxHashMap::default(),
            method_argument_access: FxHashMap::default(),
            var_types: FxHashMap::default(),
            ref_bindings: FxHashMap::default(),
            raw_ptr_vars: FxHashSet::default(),
            return_borrow_summaries,
            resolved_return_borrow_summaries,
            resolved_callables,
        }
    }

    fn get_last_use_location(&self, name: &str) -> Option<(usize, usize)> {
        self.block_last_uses
            .iter()
            .enumerate()
            .find_map(|(scope, uses)| uses.get(name).map(|statement| (scope + 1, *statement)))
    }

    fn local_scope_for(&self, name: &str) -> Option<usize> {
        self.local_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn borrower_last_use(&self, name: &str) -> Option<usize> {
        match self.get_last_use_location(name) {
            Some((scope, statement)) if scope == self.block_last_uses.len() => Some(statement),
            Some(_) => None,
            None => None,
        }
    }

    fn error_with_note(
        &mut self,
        message: String,
        span: Span,
        note_span: Option<Span>,
        note_message: Option<String>,
    ) {
        self.errors.push(BorrowError {
            message,
            span,
            note_span,
            note_message,
        });
    }

    fn push_scope(&mut self) {
        self.scopes.push(Vec::new());
    }

    fn pop_scope(&mut self) {
        if let Some(borrows) = self.scopes.pop() {
            for b in borrows {
                if let Some(ref name) = b.borrower {
                    self.ref_bindings.remove(name);
                }
            }
        }
    }

    fn check_scope_escaping_borrows(&mut self) {
        let scope_depth = self.block_last_uses.len();
        let parent_statement = self.statement_indices.last().copied();
        let escaping: Vec<_> = self
            .scopes
            .last()
            .into_iter()
            .flatten()
            .filter_map(|borrow| {
                if borrow.param
                    || !borrow.aggregate_field
                    || borrow.referent_scope != Some(scope_depth)
                {
                    return None;
                }
                let borrower = borrow.borrower.as_deref()?;
                if borrow.borrower_scope >= scope_depth {
                    return None;
                }
                let used_after_scope = if let Some(field_path) = borrow.borrower_path.as_deref() {
                    self.aggregate_field_used_after_scope(borrower, field_path, scope_depth)
                } else {
                    let (last_use_scope, last_use_statement) =
                        self.get_last_use_location(borrower)?;
                    last_use_scope < scope_depth
                        && !(last_use_scope + 1 == scope_depth
                            && Some(last_use_statement) == parent_statement)
                };
                used_after_scope.then_some(borrow.span)
            })
            .collect();
        for span in escaping {
            self.errors.push(BorrowError {
                message: msg::reference_escapes_scope().to_string(),
                span,
                note_span: None,
                note_message: None,
            });
        }
    }

    fn aggregate_field_used_after_scope(
        &self,
        owner: &str,
        field_path: &str,
        scope_depth: usize,
    ) -> bool {
        let key = (owner.to_string(), field_path.to_string());
        for block_depth in (1..scope_depth).rev() {
            let Some(start_statement) = self.statement_indices.get(block_depth - 1) else {
                continue;
            };
            let Some(reads) = self.block_field_reads[block_depth - 1].get(&key) else {
                if self.block_field_writes[block_depth - 1]
                    .get(&key)
                    .is_some_and(|writes| writes.iter().any(|index| index > start_statement))
                {
                    return false;
                }
                continue;
            };
            let next_read = reads
                .iter()
                .copied()
                .filter(|index| index > start_statement)
                .min();
            let next_write = self.block_field_writes[block_depth - 1]
                .get(&key)
                .and_then(|writes| {
                    writes
                        .iter()
                        .copied()
                        .filter(|index| index > start_statement)
                        .min()
                });
            match (next_read, next_write) {
                (Some(read), Some(write)) if write < read => return false,
                (Some(_), _) => return true,
                (None, Some(_)) => return false,
                (None, None) => {}
            }
        }
        false
    }

    /// Check if two field paths on the same root variable overlap.
    /// String-based overlap: `p1` overlaps `p2` when one is a dot-prefix of the
    /// other (or either is empty = whole place). This is the string analogue of
    /// `Place` projection prefix testing.
    /// TODO(Place Phase 1): replace with `semantic::place::Place::overlaps` /
    /// `is_prefix_of` on structured `Projection::Field` vectors. Pure helper on
    /// `Place` will handle `Field`/`Deref`/`Index`/`TupleField` structurally.
    fn paths_overlap(p1: &str, p2: &str) -> bool {
        if p1 == p2 || p1.is_empty() || p2.is_empty() {
            return true;
        }
        let prefix1 = format!("{p1}.");
        let prefix2 = format!("{p2}.");
        p2.starts_with(&prefix1) || p1.starts_with(&prefix2)
    }
    /// Find an active loan that conflicts with `(root, path, requested_kind)`.
    fn find_conflict(
        &self,
        root: &str,
        path: &str,
        requested_kind: BorrowKind,
        ignore_borrower: Option<&str>,
    ) -> Option<&ActiveBorrow> {
        for scope in self.scopes.iter().rev() {
            for b in scope.iter().rev() {
                if let Some(ign) = ignore_borrower
                    && b.borrower.as_deref() == Some(ign)
                {
                    continue;
                }
                if b.root == root && Self::paths_overlap(&b.path, path) {
                    match requested_kind {
                        BorrowKind::Shared => {
                            if b.kind == BorrowKind::Exclusive {
                                return Some(b);
                            }
                        }
                        BorrowKind::Exclusive => {
                            return Some(b);
                        }
                    }
                }
            }
        }
        None
    }

    /// Find any active loan overlapping `(root, path)` regardless of kind.
    fn find_any_borrow(
        &self,
        root: &str,
        path: &str,
        ignore_borrower: Option<&str>,
    ) -> Option<&ActiveBorrow> {
        for scope in self.scopes.iter().rev() {
            for b in scope.iter().rev() {
                if let Some(ign) = ignore_borrower
                    && b.borrower.as_deref() == Some(ign)
                {
                    continue;
                }
                if b.root == root && Self::paths_overlap(&b.path, path) {
                    return Some(b);
                }
            }
        }
        None
    }

    /// Find an active mutable loan overlapping `(root, path)`.
    fn find_mutable_borrow(
        &self,
        root: &str,
        path: &str,
        ignore_borrower: Option<&str>,
    ) -> Option<&ActiveBorrow> {
        for scope in self.scopes.iter().rev() {
            for b in scope.iter().rev() {
                if let Some(ign) = ignore_borrower
                    && b.borrower.as_deref() == Some(ign)
                {
                    continue;
                }
                if b.root == root
                    && b.kind == BorrowKind::Exclusive
                    && Self::paths_overlap(&b.path, path)
                {
                    return Some(b);
                }
            }
        }
        None
    }

    /// Phase 11 — Place-based conflict finder for `Index` (`v[i]`) overlap.
    ///
    /// Uses `places_overlap` / `Place::overlaps` on structured `Place`s.
    /// `v` overlaps `v[i]` (prefix `[]` vs `[Index]`), and `v[i]` overlaps
    /// `v[j]` (both `[Index]` → `Index == Index`, conservative, index-insensitive).
    /// This is the structured counterpart to `paths_overlap` for container indices,
    /// ensuring `move v[i]` and `&v[i]` conflicts are judged via `Place::Index`.
    fn find_conflict_for_place(
        &self,
        place: &Place,
        requested_kind: BorrowKind,
        ignore_borrower: Option<&str>,
    ) -> Option<&ActiveBorrow> {
        // Convert active string loans to Places for structural comparison.
        // For loans that were originally `v[i]`, the string model stored them as `v`
        // (transparent), so the converted Place will be `Place::new("v")` without Index.
        // To remain conservative, we also consider that any active loan on `v` overlaps `v[i]`
        // via Place prefix check: `Place::new("v")` is prefix of `Place::new("v").index()`.
        // Thus building Places from stored (root,path) and checking `places_overlap`
        // yields the same conservative result as the string check for existing loans,
        // but correctly handles new `v[i]` targets via their `[Index]` projection.
        for scope in self.scopes.iter().rev() {
            for b in scope.iter().rev() {
                if let Some(ign) = ignore_borrower
                    && b.borrower.as_deref() == Some(ign)
                {
                    continue;
                }
                // Build Place for active borrow: root + dotted fields (Index-transparent stored form)
                let mut active_place = Place::new(&b.root);
                if !b.path.is_empty() {
                    for seg in b.path.split('.') {
                        active_place.push_projection(Projection::Field(seg.to_string()));
                    }
                }
                if self.places_overlap_for_borrow(&active_place, place) {
                    match requested_kind {
                        BorrowKind::Shared => {
                            if b.kind == BorrowKind::Exclusive {
                                return Some(b);
                            }
                        }
                        BorrowKind::Exclusive => {
                            return Some(b);
                        }
                    }
                }
                // Also consider the stored loan's Place overlapping via string-style prefix
                // for Index-transparent loans: `v` (active) overlaps `v[i]` (requested) was already
                // covered by `active_place` being prefix of `place` when `place` has Index.
            }
        }
        None
    }

    fn check_program(&mut self, program: &ast::Program) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Struct(st) => {
                    let mut ref_fields = FxHashSet::default();
                    let mut field_types = FxHashMap::default();
                    for field in &st.fields {
                        if matches!(field.field_type.kind.as_ref(), ast::TypeKind::Reference(_)) {
                            ref_fields.insert(field.name.name.clone());
                        }
                        if let Some(ty_name) = named_type_name(&field.field_type) {
                            field_types.insert(field.name.name.clone(), ty_name.to_string());
                        }
                    }
                    self.struct_ref_fields
                        .insert(st.name.name.clone(), ref_fields);
                    self.struct_field_types
                        .insert(st.name.name.clone(), field_types);
                }
                ast::ItemKind::GlobalVariable(global) => {
                    if let Some(ty_name) = named_type_name(&global.var_type) {
                        self.global_var_types
                            .insert(global.name.name.clone(), ty_name.to_string());
                    }
                }
                _ => {}
            }
        }

        for item in &program.items {
            let ast::ItemKind::Impl(imp) = &item.kind else {
                continue;
            };
            let Some(owner) = named_type_name(&imp.self_type) else {
                continue;
            };
            for member in &imp.items {
                let ast::ImplItemKind::Function(func) = member else {
                    continue;
                };
                let ast::MethodKind::InstancePointer { is_mutable } = &func.method_kind else {
                    continue;
                };
                let Some(receiver) = func.parameters.first() else {
                    continue;
                };
                if !matches!(
                    receiver.param_type.kind.as_ref(),
                    ast::TypeKind::Reference(_)
                ) {
                    continue;
                }

                let access = if *is_mutable {
                    CallAccessKind::Exclusive
                } else {
                    CallAccessKind::Shared
                };
                self.method_receiver_access
                    .entry(owner.to_string())
                    .or_default()
                    .entry(func.name.name.clone())
                    .and_modify(|existing| {
                        if *existing != access {
                            *existing = CallAccessKind::Exclusive;
                        }
                    })
                    .or_insert(access);
                let argument_access = func
                    .parameters
                    .iter()
                    .skip(1)
                    .map(|param| match param.param_type.kind.as_ref() {
                        ast::TypeKind::Reference(reference) => Some(if reference.is_mutable {
                            CallAccessKind::Exclusive
                        } else {
                            CallAccessKind::Shared
                        }),
                        ast::TypeKind::Pointer(_) => None,
                        _ => Some(CallAccessKind::Read),
                    })
                    .collect::<Vec<_>>();
                self.method_argument_access
                    .entry(owner.to_string())
                    .or_default()
                    .entry(func.name.name.clone())
                    .or_insert(argument_access);
            }
        }

        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Function(func) => {
                    self.check_function(&func.parameters, &func.body);
                }
                ast::ItemKind::Impl(imp) => {
                    for member in &imp.items {
                        match member {
                            ast::ImplItemKind::Function(func) => {
                                self.check_function(&func.parameters, &func.body);
                            }
                            ast::ImplItemKind::Cast(cast) => {
                                self.check_function(&cast.parameters, &cast.body);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn check_function(&mut self, parameters: &[ast::Parameter], body: &ast::Block) {
        self.scopes.clear();
        self.scopes.push(Vec::new());
        self.block_last_uses.clear();
        self.block_field_reads.clear();
        self.block_field_writes.clear();
        self.local_scopes.clear();
        self.local_scopes.push(FxHashMap::default());
        self.statement_indices.clear();
        self.var_types.clear();
        self.ref_bindings.clear();
        self.raw_ptr_vars.clear();

        for param in parameters {
            if let Some(scope) = self.local_scopes.last_mut() {
                scope.insert(param.name.name.clone(), 0);
            }
            if let Some(type_name) = named_type_name(&param.param_type) {
                self.var_types
                    .insert(param.name.name.clone(), type_name.to_string());
            }
            match param.param_type.kind.as_ref() {
                ast::TypeKind::Reference(r) => {
                    let kind = if r.is_mutable {
                        BorrowKind::Exclusive
                    } else {
                        BorrowKind::Shared
                    };
                    self.ref_bindings.insert(
                        param.name.name.clone(),
                        RefVarInfo {
                            root: param.name.name.clone(),
                            path: String::new(),
                            kind,
                            span: param.name.span,
                        },
                    );
                    if let Some(scope) = self.scopes.last_mut() {
                        scope.push(ActiveBorrow {
                            root: param.name.name.clone(),
                            path: String::new(),
                            kind,
                            span: param.name.span,
                            borrower: Some(param.name.name.clone()),
                            borrower_path: None,
                            aggregate_field: false,
                            borrower_scope: 0,
                            referent_scope: Some(0),
                            last_use: None,
                            param: true,
                        });
                    }
                }
                ast::TypeKind::Pointer(_) => {
                    self.raw_ptr_vars.insert(param.name.name.clone());
                }
                _ => {}
            }
        }

        self.check_block(body);
    }

    fn check_block(&mut self, block: &ast::Block) {
        let prev_var_types = self.var_types.clone();
        let prev_raw_ptr_vars = self.raw_ptr_vars.clone();
        self.push_scope();

        // Precompute the final use statement index for every variable in this block (NLL)
        let mut block_last_use = FxHashMap::default();
        for (i, stmt) in block.statements.iter().enumerate() {
            let uses = self.collect_stmt_uses(stmt);
            for name in uses {
                block_last_use.insert(name, i);
            }
        }
        let mut block_field_reads = FxHashMap::default();
        let mut block_field_writes = FxHashMap::default();
        for (i, stmt) in block.statements.iter().enumerate() {
            let mut one_statement = ast::Block {
                statements: vec![stmt.clone()],
                span: stmt.span,
            };
            let mut nested_block_spans = Vec::new();
            if let ast::StatementKind::Block(nested) = &stmt.kind {
                nested_block_spans.push(nested.span);
            }
            crate::parser::visit::expressions_in_block(&mut one_statement, &mut |expression| {
                match expression.kind.as_ref() {
                    ast::ExpressionKind::Lambda { body, .. }
                    | ast::ExpressionKind::Block(body)
                    | ast::ExpressionKind::While { body, .. }
                    | ast::ExpressionKind::ForIn { body, .. }
                    | ast::ExpressionKind::For { body, .. } => {
                        nested_block_spans.push(body.span);
                    }
                    ast::ExpressionKind::If {
                        then_branch,
                        else_branch,
                        ..
                    } => {
                        nested_block_spans.push(then_branch.span);
                        if let Some(body) = else_branch {
                            nested_block_spans.push(body.span);
                        }
                    }
                    _ => {}
                }
            });
            let mut write_spans = FxHashSet::default();
            let mut field_accesses = Vec::new();
            crate::parser::visit::expressions_in_block(&mut one_statement, &mut |expression| {
                if nested_block_spans.iter().any(|span| {
                    span.file == expression.span.file
                        && span.start <= expression.span.start
                        && expression.span.end <= span.end
                }) {
                    return;
                }
                match expression.kind.as_ref() {
                    ast::ExpressionKind::Binary {
                        left,
                        operator: ast::BinaryOperator::Assign,
                        ..
                    } if matches!(left.kind.as_ref(), ast::ExpressionKind::FieldAccess { .. }) => {
                        if let Some((place, path, _)) = self.extract_root_and_path(left)
                            && !path.is_empty()
                        {
                            block_field_writes.entry((place, path)).or_insert_with(Vec::new).push(i);
                            write_spans.insert((left.span.file, left.span.start, left.span.end));
                        }
                    }
                    ast::ExpressionKind::FieldAccess { .. } => {
                        if let Some((place, path, _)) = self.extract_root_and_path(expression)
                            && !path.is_empty()
                        {
                            field_accesses.push((
                                place,
                                path,
                                (expression.span.file, expression.span.start, expression.span.end),
                            ));
                        }
                    }
                    _ => {}
                }
            });
            for (place, path, span) in field_accesses {
                if !write_spans.contains(&span) {
                    block_field_reads
                        .entry((place, path))
                        .or_insert_with(Vec::new)
                        .push(i);
                }
            }
        }
        self.block_last_uses.push(block_last_use);
        self.block_field_reads.push(block_field_reads);
        self.block_field_writes.push(block_field_writes);
        self.local_scopes.push(FxHashMap::default());

        for (i, stmt) in block.statements.iter().enumerate() {
            self.expire_loans_before(i);
            self.statement_indices.push(i);
            self.check_statement(stmt);
            self.statement_indices.pop();
        }
        // Final statement boundary: expire loans whose last use was within this block.
        self.expire_loans_before(block.statements.len());
        self.check_scope_escaping_borrows();
        self.pop_scope();
        self.local_scopes.pop();
        self.block_field_reads.pop();
        self.block_field_writes.pop();
        self.block_last_uses.pop();
        self.var_types = prev_var_types;
        self.raw_ptr_vars = prev_raw_ptr_vars;
    }

    /// Remove loans whose last use was strictly before statement `i`:
    /// they no longer constrain statement `i` or later code.
    fn expire_loans_before(&mut self, i: usize) {
        let mut expired: FxHashSet<String> = FxHashSet::default();
        // Last-use indices belong to the block currently being checked.
        if let Some(scope) = self.scopes.last_mut() {
            let mut kept: Vec<ActiveBorrow> = Vec::with_capacity(scope.len());
            for b in scope.drain(..) {
                if !b.param && b.last_use.is_some_and(|last| last < i) {
                    if let Some(ref name) = b.borrower {
                        expired.insert(name.clone());
                    }
                } else {
                    kept.push(b);
                }
            }
            *scope = kept;
        }
        if !expired.is_empty() {
            self.ref_bindings.retain(|name, _| !expired.contains(name));
        }
    }

    /// Names of reference bindings used anywhere in `stmt` (NLL last-use scan).
    fn collect_stmt_uses(&self, stmt: &ast::Statement) -> FxHashSet<String> {
        let mut uses = FxHashSet::default();
        match &stmt.kind {
            ast::StatementKind::Let(let_stmt) => {
                if let Some(init) = &let_stmt.initializer {
                    self.collect_expr_uses(init, &mut uses);
                }
            }
            ast::StatementKind::Expression(expr) | ast::StatementKind::Return(Some(expr)) => {
                self.collect_expr_uses(expr, &mut uses);
            }
            ast::StatementKind::Defer(inner) => {
                uses.extend(self.collect_stmt_uses(inner));
            }
            ast::StatementKind::Block(block) => {
                for statement in &block.statements {
                    uses.extend(self.collect_stmt_uses(statement));
                }
            }
            _ => {}
        }
        uses
    }

    /// Collect identifiers that appear in expression (for NLL last-use scan).
    fn collect_expr_uses(&self, expr: &ast::Expression, uses: &mut FxHashSet<String>) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                uses.insert(ident.name.clone());
            }
            ast::ExpressionKind::Binary { left, right, .. } => {
                self.collect_expr_uses(left, uses);
                self.collect_expr_uses(right, uses);
            }
            ast::ExpressionKind::Postfix { operand, .. } => {
                self.collect_expr_uses(operand, uses);
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Cast {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Reference {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Launch(operand)
            | ast::ExpressionKind::Wait(operand)
            | ast::ExpressionKind::Comptime(operand) => {
                self.collect_expr_uses(operand, uses);
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            }
            | ast::ExpressionKind::MethodCall {
                receiver: function,
                arguments,
                ..
            } => {
                self.collect_expr_uses(function, uses);
                for arg in arguments {
                    self.collect_expr_uses(arg, uses);
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. } => {
                self.collect_expr_uses(object, uses);
                if let ast::ExpressionKind::Index { index, .. } = expr.kind.as_ref() {
                    self.collect_expr_uses(index, uses);
                }
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                self.collect_expr_uses(object, uses);
                if let Some(s) = start {
                    self.collect_expr_uses(s, uses);
                }
                if let Some(e) = end {
                    self.collect_expr_uses(e, uses);
                }
                if let Some(st) = step {
                    self.collect_expr_uses(st, uses);
                }
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_expr_uses(condition, uses);
                for stmt in &then_branch.statements {
                    uses.extend(self.collect_stmt_uses(stmt));
                }
                if let Some(else_b) = else_branch {
                    for stmt in &else_b.statements {
                        uses.extend(self.collect_stmt_uses(stmt));
                    }
                }
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_expr_uses(condition, uses);
                self.collect_expr_uses(then_expr, uses);
                self.collect_expr_uses(else_expr, uses);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.collect_expr_uses(value, uses);
                self.collect_expr_uses(fallback, uses);
            }
            ast::ExpressionKind::While { condition, body } => {
                self.collect_expr_uses(condition, uses);
                for stmt in &body.statements {
                    uses.extend(self.collect_stmt_uses(stmt));
                }
            }
            ast::ExpressionKind::ForIn { iterable, body, .. } => {
                self.collect_expr_uses(iterable, uses);
                for stmt in &body.statements {
                    uses.extend(self.collect_stmt_uses(stmt));
                }
            }
            ast::ExpressionKind::For {
                condition, body, ..
            } => {
                self.collect_expr_uses(condition, uses);
                for stmt in &body.statements {
                    uses.extend(self.collect_stmt_uses(stmt));
                }
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.collect_expr_uses(expression, uses);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_expr_uses(guard, uses);
                    }
                    self.collect_expr_uses(&arm.body, uses);
                }
            }
            ast::ExpressionKind::Block(block) => {
                for stmt in &block.statements {
                    uses.extend(self.collect_stmt_uses(stmt));
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.collect_expr_uses(item, uses);
                }
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(e)
                        | ast::InitializerItem::Field { value: e, .. }
                        | ast::InitializerItem::Index { value: e, .. } => {
                            self.collect_expr_uses(e, uses);
                        }
                    }
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for f in fields {
                    self.collect_expr_uses(&f.value, uses);
                }
            }
            ast::ExpressionKind::EnumVariant { fields, .. } => {
                for f in fields {
                    self.collect_expr_uses(f, uses);
                }
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.collect_expr_uses(input, uses);
                }
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    match arg {
                        ast::MacroArg::Expression(e) => self.collect_expr_uses(e, uses),
                        ast::MacroArg::Statement(s) => {
                            uses.extend(self.collect_stmt_uses(s));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    fn check_statement(&mut self, stmt: &ast::Statement) {
        match &stmt.kind {
            ast::StatementKind::Let(let_stmt) => {
                let name = match &let_stmt.pattern.kind {
                    ast::PatternKind::Identifier(ident) => Some(ident.name.clone()),
                    _ => None,
                };

                if let Some(ref name) = name {
                    if let Some(scope) = self.local_scopes.last_mut() {
                        scope.insert(name.clone(), self.block_last_uses.len());
                    }
                    if let Some(ref ty) = let_stmt.type_annotation {
                        if matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(_)) {
                            self.raw_ptr_vars.insert(name.clone());
                        } else {
                            self.raw_ptr_vars.remove(name);
                        }
                        if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                            if let Some(last) = named.path.last() {
                                self.var_types.insert(name.clone(), last.name.clone());
                            }
                        }
                    }
                }

                if let Some(ref init) = let_stmt.initializer {
                    if let Some(ref name) = name {
                        // If initializer is TypeName.new(...) or TypeName { ... }, record struct type
                        if let ast::ExpressionKind::MethodCall { receiver, .. } = init.kind.as_ref()
                        {
                            if let ast::ExpressionKind::TypeName(ty) = receiver.kind.as_ref() {
                                if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                                    if let Some(last) = named.path.last() {
                                        self.var_types.insert(name.clone(), last.name.clone());
                                    }
                                }
                            }
                        } else if let ast::ExpressionKind::StructLiteral { path, .. } =
                            init.kind.as_ref()
                        {
                            if let Some(last) = path.last() {
                                self.var_types.insert(name.clone(), last.name.clone());
                            }
                        }

                        // Check if initializer is a borrow expression:
                        if let ast::ExpressionKind::Reference {
                            is_mutable,
                            expression,
                        } = init.kind.as_ref()
                        {
                            let kind = if let Some(ast::Type { kind, .. }) =
                                &let_stmt.type_annotation
                                && let ast::TypeKind::Reference(r) = kind.as_ref()
                            {
                                if r.is_mutable {
                                    BorrowKind::Exclusive
                                } else {
                                    BorrowKind::Shared
                                }
                            } else if *is_mutable {
                                BorrowKind::Exclusive
                            } else {
                                BorrowKind::Shared
                            };
                            self.register_named_borrow(name, expression, kind, init.span);
                        } else if let ast::ExpressionKind::Identifier(ident) = init.kind.as_ref() {
                            // Reborrow from an existing reference binding:
                            if let Some(existing) = self.ref_bindings.get(&ident.name).cloned() {
                                self.ref_bindings.insert(
                                    name.clone(),
                                    RefVarInfo {
                                        root: existing.root,
                                        path: existing.path,
                                        kind: existing.kind,
                                        span: init.span,
                                    },
                                );
                            } else {
                                self.check_expr(init);
                            }
                            let inherited: Vec<_> = self
                                .scopes
                                .iter()
                                .flatten()
                                .filter(|loan| {
                                    loan.borrower.as_deref() == Some(ident.name.as_str())
                                })
                                .cloned()
                                .collect();
                            for mut loan in inherited {
                                loan.borrower = Some(name.clone());
                                loan.last_use = self.borrower_last_use(name);
                                loan.span = init.span;
                                loan.param = false;
                                if let Some(scope) = self.scopes.last_mut() {
                                    scope.push(loan);
                                }
                            }
                        } else if let ast::ExpressionKind::StructLiteral { fields, .. } =
                            init.kind.as_ref()
                        {
                            for f in fields {
                                self.check_aggregate_field_borrow(
                                    name,
                                    Some(f.name.name.as_str()),
                                    &f.value,
                                );
                            }
                        } else if let ast::ExpressionKind::Initializer { items } =
                            init.kind.as_ref()
                        {
                            for item in items {
                                let (val, field_name) = match item {
                                    ast::InitializerItem::Positional(e) => (e, None),
                                    ast::InitializerItem::Field { name, value } => {
                                        (value, Some(name.name.as_str()))
                                    }
                                    ast::InitializerItem::Index { value, .. } => (value, None),
                                };
                                self.check_aggregate_field_borrow(name, field_name, val);
                            }
                        } else {
                            if !self.register_call_result_borrow(name, None, false, init) {
                                self.check_expr(init);
                            }
                        }
                    } else {
                        self.check_expr(init);
                    }
                }
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr);
            }
            ast::StatementKind::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.check_expr(expr);
                }
            }
            ast::StatementKind::Block(block) => {
                self.check_block(block);
            }
            _ => {}
        }
    }

    /// Extract root variable name, field path, and whether accessed through a reference variable.
    /// String-based `Place` builder: walks `Identifier`/`FieldAccess`/`Index`/`Deref` chains
    /// and produces `(root, "a.b", ref_var)` where the path is `"."`-joined.
    /// `Index` and `Deref` are transparent passthroughs (path unchanged) in this
    /// string model — deeper `Place` tracking will need distinct projections.
    /// Reborrows via `ref_bindings` re-root through an existing `(root,path)`.
    /// TODO(Place Phase 1): replace with `semantic::place::Place::from_expr` —
    /// `Place { local, projections: [Field|Index|Deref|TupleField] }` is the
    /// structured counterpart. Structured projections avoid the lossy
    /// `Index→transparent` / `TupleField→Field` conflation and let
    /// `Place::overlaps` handle `Field` vs `Index`/`Deref` soundly. No deletion
    /// in this phase; comments only.
    ///
    /// Phase 11 — Index is conservative: `v[i]` extracts as `(v, "", ref_var)` in
    /// the string model (transparent), which already yields conservative overlap
    /// (`v` overlaps `v[i]` because empty path overlaps anything, and `v[i]` vs
    /// `v[j]` both collapse to `v` and thus overlap). Structured `extract_place`
    /// below preserves `Projection::Index` (`Place::new("v").index()` via
    /// `push_projection(Projection::Index)`) and `Place::overlaps` gives the same
    /// conservative result (`v` prefix of `v[i]`, `v[i]` == `v[j]`), documented as
    /// Phase 11 limitation (index-insensitive, index values opaque).
    fn extract_root_and_path(
        &self,
        expr: &ast::Expression,
    ) -> Option<(String, String, Option<String>)> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if self.raw_ptr_vars.contains(&ident.name) {
                    return None;
                }
                if let Some(existing) = self.ref_bindings.get(&ident.name) {
                    return Some((
                        existing.root.clone(),
                        existing.path.clone(),
                        Some(ident.name.clone()),
                    ));
                }
                Some((ident.name.clone(), String::new(), None))
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let (root, parent_path, ref_var) = self.extract_root_and_path(object)?;
                let path = if parent_path.is_empty() {
                    field.name.clone()
                } else {
                    format!("{parent_path}.{}", field.name)
                };
                Some((root, path, ref_var))
            }
            ast::ExpressionKind::Index { object, .. }
            | ast::ExpressionKind::Slice { object, .. } => self.extract_root_and_path(object),
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => self.extract_root_and_path(operand),
            _ => None,
        }
    }

    /// A direct borrow of a raw-pointer variable owns the pointer slot. The
    /// raw-pointer escape hatch applies only after projecting to its pointee.
    fn extract_borrow_root_and_path(
        &self,
        expr: &ast::Expression,
    ) -> Option<(String, String, Option<String>)> {
        self.extract_root_and_path(expr).or_else(|| {
            let ast::ExpressionKind::Identifier(ident) = expr.kind.as_ref() else {
                return None;
            };
            self.raw_ptr_vars
                .contains(&ident.name)
                .then(|| (ident.name.clone(), String::new(), None))
        })
    }

    /// Phase 11 — Structured `Place` extraction for `Index` (`v[i]`) handling.
    ///
    /// Builds `Place { local, projections: [Field|Index|Deref] }` from
    /// `Identifier` / `FieldAccess` / `Index` / `*p` chains. `v[i]` → `Place::new("v").index()`
    /// (`push_projection(Projection::Index)`), `x.a[i]` → `Place::new("x").field("a").index()`.
    /// Index values are opaque — any `v[i]`/`v[j]` both become `[Index]` and
    /// `Place::overlaps` treats them as overlapping (conservative, index-insensitive,
    /// Phase 11 limitation). Reborrows re-root like `extract_root_and_path`.
    /// Returns `None` for non-place expressions or raw-pointer bases.
    fn extract_place(&self, expr: &ast::Expression) -> Option<(Place, Option<String>)> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if self.raw_ptr_vars.contains(&ident.name) {
                    return None;
                }
                if let Some(existing) = self.ref_bindings.get(&ident.name) {
                    let mut place = Place::new(&existing.root);
                    if !existing.path.is_empty() {
                        for seg in existing.path.split('.') {
                            place.push_projection(Projection::Field(seg.to_string()));
                        }
                    }
                    return Some((place, Some(ident.name.clone())));
                }
                Some((Place::new(ident.name.clone()), None))
            }
            ast::ExpressionKind::FieldAccess { object, field } => {
                let (mut base, ref_var) = self.extract_place(object)?;
                base.push_projection(Projection::Field(field.name.clone()));
                Some((base, ref_var))
            }
            ast::ExpressionKind::Index { object, .. } => {
                let (mut base, ref_var) = self.extract_place(object)?;
                base.push_projection(Projection::Index);
                Some((base, ref_var))
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => {
                let (mut base, ref_var) = self.extract_place(operand)?;
                base.push_projection(Projection::Deref);
                Some((base, ref_var))
            }
            _ => None,
        }
    }

    /// Place-based overlap check for `extract_place` results.
    /// Delegates to `places_overlap` / `Place::overlaps` — conservative for `Index`.
    fn places_overlap_for_borrow(&self, a: &Place, b: &Place) -> bool {
        places_overlap(a, b)
    }

    /// Register a borrow bound to a named variable (`let r = &...`).
    fn register_named_borrow(
        &mut self,
        binding_name: &str,
        target: &ast::Expression,
        kind: BorrowKind,
        span: Span,
    ) {
        // Phase 11 — Try structured Place for Index first (conservative Index overlap).
        // If target is `v[i]` (or `x.a[i]`), `extract_place` yields `Place::new("v").index()`
        // and we check via `places_overlap` / `Place::overlaps` (v overlaps v[i], v[i] overlaps v[j]).
        // This complements the string `extract_root_and_path` path which collapses Index transparently.
        if let Some((place, ref_var_place)) = self.extract_place(target) {
            if place.projections.contains(&Projection::Index) {
                if let Some(conflict) =
                    self.find_conflict_for_place(&place, kind, ref_var_place.as_deref())
                {
                    let full_target = format!("{}[index]", place.local);
                    let msg = match (kind, conflict.kind) {
                        (BorrowKind::Shared, BorrowKind::Exclusive) => {
                            msg::cannot_borrow_as_shared_while_mutable(&full_target)
                        }
                        (BorrowKind::Exclusive, BorrowKind::Shared) => {
                            msg::cannot_borrow_as_mutable_while_shared(&full_target)
                        }
                        (BorrowKind::Exclusive, BorrowKind::Exclusive) => {
                            msg::cannot_borrow_as_mutable_more_than_once(&full_target)
                        }
                        (BorrowKind::Shared, BorrowKind::Shared) => unreachable!(),
                    };
                    self.error_with_note(
                        msg,
                        span,
                        Some(conflict.span),
                        Some(msg::note_previous_borrow_here(conflict.kind.as_str())),
                    );
                }
                let (root, path, _ref_var) = self.extract_root_and_path(target).unwrap_or((
                    place.local.clone(),
                    String::new(),
                    ref_var_place,
                ));
                let loan = ActiveBorrow {
                    root: root.clone(),
                    path: path.clone(),
                    kind,
                    span,
                    borrower: Some(binding_name.to_string()),
                    borrower_path: None,
                    aggregate_field: false,
                    borrower_scope: self
                        .local_scope_for(binding_name)
                        .unwrap_or(self.block_last_uses.len()),
                    referent_scope: self.local_scope_for(&root),
                    last_use: self.borrower_last_use(binding_name),
                    param: false,
                };
                self.ref_bindings.insert(
                    binding_name.to_string(),
                    RefVarInfo {
                        root,
                        path,
                        kind,
                        span,
                    },
                );
                if let Some(scope) = self.scopes.last_mut() {
                    scope.push(loan);
                }
                return;
            }
        }
        if let Some((root, path, ref_var)) = self.extract_borrow_root_and_path(target) {
            let full_target = if path.is_empty() {
                root.clone()
            } else {
                format!("{root}.{path}")
            };

            if let Some(conflict) = self.find_conflict(&root, &path, kind, ref_var.as_deref()) {
                let msg = match (kind, conflict.kind) {
                    (BorrowKind::Shared, BorrowKind::Exclusive) => {
                        msg::cannot_borrow_as_shared_while_mutable(&full_target)
                    }
                    (BorrowKind::Exclusive, BorrowKind::Shared) => {
                        msg::cannot_borrow_as_mutable_while_shared(&full_target)
                    }
                    (BorrowKind::Exclusive, BorrowKind::Exclusive) => {
                        msg::cannot_borrow_as_mutable_more_than_once(&full_target)
                    }
                    (BorrowKind::Shared, BorrowKind::Shared) => unreachable!(),
                };
                self.error_with_note(
                    msg,
                    span,
                    Some(conflict.span),
                    Some(msg::note_previous_borrow_here(conflict.kind.as_str())),
                );
            }

            let loan = ActiveBorrow {
                root: root.clone(),
                path: path.clone(),
                kind,
                span,
                borrower: Some(binding_name.to_string()),
                borrower_path: None,
                aggregate_field: false,
                borrower_scope: self
                    .local_scope_for(binding_name)
                    .unwrap_or(self.block_last_uses.len()),
                referent_scope: self.local_scope_for(&root),
                last_use: self.borrower_last_use(binding_name),
                param: false,
            };
            self.ref_bindings.insert(
                binding_name.to_string(),
                RefVarInfo {
                    root,
                    path,
                    kind,
                    span,
                },
            );
            if let Some(scope) = self.scopes.last_mut() {
                scope.push(loan);
            }
        } else {
            self.check_expr(target);
        }
    }

    fn register_call_result_borrow(
        &mut self,
        binding_name: &str,
        borrower_path: Option<&str>,
        aggregate_field: bool,
        expression: &ast::Expression,
    ) -> bool {
        let (key, arguments) = match expression.kind.as_ref() {
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let ast::ExpressionKind::Identifier(name) = function.kind.as_ref() else {
                    return false;
                };
                (
                    ReturnCallKey {
                        name: name.name.clone(),
                        arity: arguments.len(),
                        method: false,
                    },
                    arguments.iter().collect::<Vec<_>>(),
                )
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let mut args = Vec::with_capacity(arguments.len() + 1);
                args.push(receiver.as_ref());
                args.extend(arguments.iter());
                (
                    ReturnCallKey {
                        name: method.name.clone(),
                        arity: args.len(),
                        method: true,
                    },
                    args,
                )
            }
            _ => return false,
        };
        let call_site = (
            expression.span.file,
            expression.span.start,
            expression.span.end,
            expression.span.expansion,
        );
        let summary = match self.resolved_callables.get(&call_site) {
            Some(resolved) if resolved.returns_reference => self
                .resolved_return_borrow_summaries
                .get(&resolved.identity)
                .cloned()
                .or_else(|| {
                    Some(ReturnBorrowSummary {
                        parameter_indices: resolved
                            .reference_parameters
                            .iter()
                            .copied()
                            .collect(),
                        kind: if resolved.returns_mutable {
                            BorrowKind::Exclusive
                        } else {
                            BorrowKind::Shared
                        },
                    })
                }),
            Some(_) => None,
            None => self.return_borrow_summaries.get(&key).cloned(),
        };
        let Some(summary) = summary else {
            return false;
        };
        self.check_expr(expression);
        for index in summary.parameter_indices {
            let Some(argument) = arguments.get(index).copied() else {
                continue;
            };
            match argument.kind.as_ref() {
                ast::ExpressionKind::Reference { expression, .. } => {
                    if aggregate_field {
                        self.register_aggregate_borrow(
                            binding_name,
                            borrower_path,
                            expression,
                            summary.kind,
                            argument.span,
                        );
                    } else {
                        self.register_named_borrow(
                            binding_name,
                            expression,
                            summary.kind,
                            argument.span,
                        );
                    }
                }
                ast::ExpressionKind::Identifier(ident)
                    if self.ref_bindings.contains_key(&ident.name) =>
                {
                    let inherited: Vec<_> = self
                        .scopes
                        .iter()
                        .flatten()
                        .filter(|loan| loan.borrower.as_deref() == Some(ident.name.as_str()))
                        .cloned()
                        .collect();
                    if inherited.is_empty() {
                        if aggregate_field {
                            self.register_aggregate_borrow(
                                binding_name,
                                borrower_path,
                                argument,
                                summary.kind,
                                argument.span,
                            );
                        } else {
                            self.register_named_borrow(
                                binding_name,
                                argument,
                                summary.kind,
                                argument.span,
                            );
                        }
                    } else {
                        for mut loan in inherited {
                            loan.borrower = Some(binding_name.to_string());
                            loan.borrower_scope = self
                                .local_scope_for(binding_name)
                                .unwrap_or(self.block_last_uses.len());
                            loan.last_use = self.borrower_last_use(binding_name);
                            loan.kind = summary.kind;
                            loan.span = argument.span;
                            loan.param = false;
                            if let Some(scope) = self.scopes.last_mut() {
                                loan.borrower_path = borrower_path.map(str::to_string);
                                loan.aggregate_field = aggregate_field;
                                scope.push(loan);
                            }
                        }
                        if !aggregate_field
                            && let Some(existing) = self.ref_bindings.get(&ident.name).cloned()
                        {
                            self.ref_bindings.insert(
                                binding_name.to_string(),
                                RefVarInfo {
                                    kind: summary.kind,
                                    span: argument.span,
                                    ..existing
                                },
                            );
                        }
                    }
                }
                ast::ExpressionKind::Call { .. } | ast::ExpressionKind::MethodCall { .. } => {
                    self.register_call_result_borrow(
                        binding_name,
                        borrower_path,
                        aggregate_field,
                        argument,
                    );
                }
                _ => {
                    if aggregate_field {
                        self.register_aggregate_borrow(
                            binding_name,
                            borrower_path,
                            argument,
                            summary.kind,
                            argument.span,
                        );
                    } else {
                        self.register_named_borrow(
                            binding_name,
                            argument,
                            summary.kind,
                            argument.span,
                        );
                    }
                }
            }
        }
        true
    }

    fn check_aggregate_field_borrow(
        &mut self,
        owner_name: &str,
        field_name_opt: Option<&str>,
        field_expr: &ast::Expression,
    ) {
        let is_aggregate = matches!(
            field_expr.kind.as_ref(),
            ast::ExpressionKind::StructLiteral { .. }
                | ast::ExpressionKind::Initializer { .. }
                | ast::ExpressionKind::Tuple(_)
                | ast::ExpressionKind::Array(_)
        );
        let has_field_metadata = self.var_types.get(owner_name).is_some_and(|struct_name| {
            self.struct_ref_fields.contains_key(struct_name)
                || self.struct_field_types.contains_key(struct_name)
        });
        if is_aggregate && !has_field_metadata {
            self.check_expr(field_expr);
            return;
        }
        match field_expr.kind.as_ref() {
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    let path = field_name_opt
                        .map(|parent| format!("{parent}.{}", field.name.name))
                        .unwrap_or_else(|| field.name.name.clone());
                    self.check_aggregate_field_borrow(owner_name, Some(&path), &field.value);
                }
                return;
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    let (value, field_name) = match item {
                        ast::InitializerItem::Positional(value)
                        | ast::InitializerItem::Index { value, .. } => (value, None),
                        ast::InitializerItem::Field { name, value } => {
                            (value, Some(name.name.as_str()))
                        }
                    };
                    let path = match (field_name_opt, field_name) {
                        (Some(parent), Some(child)) => Some(format!("{parent}.{child}")),
                        (None, Some(child)) => Some(child.to_string()),
                        _ => None,
                    };
                    self.check_aggregate_field_borrow(owner_name, path.as_deref(), value);
                }
                return;
            }
            ast::ExpressionKind::Tuple(values) | ast::ExpressionKind::Array(values) => {
                for value in values {
                    self.check_aggregate_field_borrow(owner_name, None, value);
                }
                return;
            }
            _ => {}
        }
        if let Some(field_name) = field_name_opt {
            if let Some(struct_name) = self.var_types.get(owner_name) {
                if let Some(is_reference) = self.field_path_is_reference(struct_name, field_name)
                    && !is_reference
                {
                    return;
                }
            }
        }
        if let ast::ExpressionKind::Reference {
            is_mutable,
            expression,
        } = field_expr.kind.as_ref()
        {
            let kind = if *is_mutable {
                BorrowKind::Exclusive
            } else {
                BorrowKind::Shared
            };
            self.register_aggregate_borrow(
                owner_name,
                field_name_opt,
                expression,
                kind,
                field_expr.span,
            );
        } else if let ast::ExpressionKind::Identifier(ident) = field_expr.kind.as_ref() {
            if let Some(existing) = self.ref_bindings.get(&ident.name).cloned() {
                let loan = ActiveBorrow {
                    root: existing.root.clone(),
                    path: existing.path.clone(),
                    kind: existing.kind,
                    span: field_expr.span,
                    borrower: Some(owner_name.to_string()),
                    borrower_path: field_name_opt.map(str::to_string),
                    aggregate_field: true,
                    borrower_scope: self
                        .local_scope_for(owner_name)
                        .unwrap_or(self.block_last_uses.len()),
                    referent_scope: self.local_scope_for(&existing.root),
                    last_use: self.borrower_last_use(owner_name),
                    param: false,
                };
                if let Some(scope) = self.scopes.last_mut() {
                    scope.push(loan);
                }
            } else {
                self.check_expr(field_expr);
            }
        } else if !self.register_call_result_borrow(owner_name, field_name_opt, true, field_expr) {
            self.check_expr(field_expr);
        }
    }

    fn mark_last_aggregate_borrow(
        &mut self,
        owner_name: &str,
        field_path: Option<&str>,
        aggregate_field: bool,
        scope_len: usize,
    ) {
        if let Some(borrow) = self
            .scopes
            .last_mut()
            .filter(|scope| scope.len() > scope_len)
            .and_then(|scope| scope.last_mut())
            && borrow.borrower.as_deref() == Some(owner_name)
        {
            borrow.borrower_path = field_path.map(str::to_string);
            borrow.aggregate_field = aggregate_field;
        }
    }

    fn clear_overwritten_aggregate_borrows(
        &mut self,
        owner: &str,
        owner_scope: usize,
        path: &str,
    ) {
        for scope in &mut self.scopes {
            scope.retain(|borrow| {
                if !borrow.aggregate_field
                    || borrow.borrower.as_deref() != Some(owner)
                    || borrow.borrower_scope != owner_scope
                {
                    return true;
                }
                borrow
                    .borrower_path
                    .as_deref()
                    .is_some_and(|existing_path| !Self::paths_overlap(existing_path, path))
            });
        }
    }

    fn register_aggregate_borrow(
        &mut self,
        owner_name: &str,
        field_path: Option<&str>,
        target: &ast::Expression,
        kind: BorrowKind,
        span: Span,
    ) {
        let previous_binding = self.ref_bindings.get(owner_name).cloned();
        let scope_len = self.scopes.last().map_or(0, Vec::len);
        self.register_named_borrow(owner_name, target, kind, span);
        if let Some(previous) = previous_binding {
            self.ref_bindings.insert(owner_name.to_string(), previous);
        } else {
            self.ref_bindings.remove(owner_name);
        }
        self.mark_last_aggregate_borrow(owner_name, field_path, true, scope_len);
    }

    fn field_path_is_reference(&self, struct_name: &str, path: &str) -> Option<bool> {
        let mut current_type = struct_name;
        let mut fields = path.split('.').peekable();
        while let Some(field) = fields.next() {
            if fields.peek().is_none() {
                return self
                    .struct_ref_fields
                    .get(current_type)
                    .map(|reference_fields| reference_fields.contains(field));
            }
            current_type = self.struct_field_types.get(current_type)?.get(field)?;
        }
        None
    }

    /// Check an assignment target (`x = val` or `p.left = val`).
    fn check_assignment_target(&mut self, target: &ast::Expression) {
        if let Some((root, path, ref_var)) = self.extract_borrow_root_and_path(target) {
            let full_target = if path.is_empty() {
                root.clone()
            } else {
                format!("{root}.{path}")
            };
            if let Some(borrow) = self.find_any_borrow(&root, &path, ref_var.as_deref()) {
                self.error_with_note(
                    msg::cannot_assign_to_borrowed(&full_target),
                    target.span,
                    Some(borrow.span),
                    Some(msg::note_previous_borrow_here(borrow.kind.as_str())),
                );
            }
        }
    }

    fn extract_call_access(&self, expr: &ast::Expression) -> Option<CallArgAccess> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => {
                let (root, path, _) = self.extract_borrow_root_and_path(expression)?;
                let kind = if *is_mutable {
                    CallAccessKind::Exclusive
                } else {
                    CallAccessKind::Shared
                };
                Some(CallArgAccess {
                    root,
                    path,
                    kind,
                    span: expr.span,
                })
            }
            _ => {
                let (root, path, _) = self.extract_root_and_path(expr)?;
                if self.raw_ptr_vars.contains(&root) {
                    return None;
                }
                Some(CallArgAccess {
                    root,
                    path,
                    kind: CallAccessKind::Read,
                    span: expr.span,
                })
            }
        }
    }

    fn method_owner_type(&self, receiver: &ast::Expression) -> Option<&str> {
        let (root, path, _) = self.extract_root_and_path(receiver)?;
        let mut type_name = self
            .var_types
            .get(&root)
            .or_else(|| self.global_var_types.get(&root))?
            .as_str();
        for field in path.split('.').filter(|field| !field.is_empty()) {
            type_name = self.struct_field_types.get(type_name)?.get(field)?.as_str();
        }
        Some(type_name)
    }

    fn method_receiver_kind(
        &self,
        receiver: &ast::Expression,
        method: &str,
    ) -> Option<CallAccessKind> {
        let type_name = self.method_owner_type(receiver)?;
        self.method_receiver_access
            .get(type_name)?
            .get(method)
            .copied()
    }

    fn method_argument_kind(
        &self,
        receiver: &ast::Expression,
        method: &str,
        index: usize,
    ) -> Option<Option<CallAccessKind>> {
        let type_name = self.method_owner_type(receiver)?;
        self.method_argument_access
            .get(type_name)?
            .get(method)?
            .get(index)
            .copied()
    }

    fn check_method_receiver_conflict(
        &mut self,
        receiver: &ast::Expression,
        access: CallAccessKind,
    ) {
        let Some((root, path, ref_var)) = self.extract_root_and_path(receiver) else {
            return;
        };
        let kind = match access {
            CallAccessKind::Exclusive => BorrowKind::Exclusive,
            CallAccessKind::Shared | CallAccessKind::Read => BorrowKind::Shared,
        };
        let full_target = if path.is_empty() {
            root.clone()
        } else {
            format!("{root}.{path}")
        };
        if let Some(conflict) = self.find_conflict(&root, &path, kind, ref_var.as_deref()) {
            let message = match (kind, conflict.kind) {
                (BorrowKind::Shared, BorrowKind::Exclusive) => {
                    msg::cannot_borrow_as_shared_while_mutable(&full_target)
                }
                (BorrowKind::Exclusive, BorrowKind::Shared) => {
                    msg::cannot_borrow_as_mutable_while_shared(&full_target)
                }
                (BorrowKind::Exclusive, BorrowKind::Exclusive) => {
                    msg::cannot_borrow_as_mutable_more_than_once(&full_target)
                }
                (BorrowKind::Shared, BorrowKind::Shared) => unreachable!(),
            };
            self.error_with_note(
                message,
                receiver.span,
                Some(conflict.span),
                Some(msg::note_previous_borrow_here(conflict.kind.as_str())),
            );
        }
    }

    /// Check for intra-call argument and receiver borrow conflicts.
    fn check_call_arguments(
        &mut self,
        receiver: Option<&ast::Expression>,
        receiver_kind: Option<CallAccessKind>,
        method: Option<&str>,
        arguments: &[ast::Expression],
    ) {
        let mut accesses = Vec::new();
        if let Some(recv) = receiver {
            if let Some(mut access) = self.extract_call_access(recv) {
                if let Some(kind) = receiver_kind {
                    access.kind = kind;
                }
                accesses.push(access);
            }
        }
        for (index, arg) in arguments.iter().enumerate() {
            let parameter_access = receiver
                .zip(method)
                .and_then(|(receiver, method)| self.method_argument_kind(receiver, method, index));
            match parameter_access {
                Some(None) => continue,
                Some(Some(kind)) => {
                    if let Some(mut access) = self.extract_call_access(arg) {
                        access.kind = kind;
                        accesses.push(access);
                    }
                }
                None => {
                    if let Some(access) = self.extract_call_access(arg) {
                        accesses.push(access);
                    }
                }
            }
        }

        for i in 0..accesses.len() {
            for j in (i + 1)..accesses.len() {
                let a = &accesses[i];
                let b = &accesses[j];
                if a.root == b.root && Self::paths_overlap(&a.path, &b.path) {
                    let full_target = if a.path.is_empty() {
                        a.root.clone()
                    } else {
                        format!("{}.{}", a.root, a.path)
                    };
                    match (a.kind, b.kind) {
                        (CallAccessKind::Exclusive, CallAccessKind::Exclusive) => {
                            self.error_with_note(
                                msg::cannot_borrow_as_mutable_more_than_once_in_same_call(
                                    &full_target,
                                ),
                                b.span,
                                Some(a.span),
                                Some(msg::note_argument_borrow_here(a.kind.as_str())),
                            );
                        }
                        (CallAccessKind::Exclusive, CallAccessKind::Shared)
                        | (CallAccessKind::Shared, CallAccessKind::Exclusive) => {
                            self.error_with_note(
                                msg::cannot_borrow_as_mutable_and_shared_in_same_call(&full_target),
                                b.span,
                                Some(a.span),
                                Some(msg::note_argument_borrow_here(a.kind.as_str())),
                            );
                        }
                        (CallAccessKind::Exclusive, CallAccessKind::Read)
                        | (CallAccessKind::Read, CallAccessKind::Exclusive) => {
                            self.error_with_note(
                                msg::cannot_access_while_mutably_borrowed_in_same_call(
                                    &full_target,
                                ),
                                b.span,
                                Some(a.span),
                                Some(msg::note_argument_borrow_here(a.kind.as_str())),
                            );
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &ast::Expression) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => {
                let kind = if *is_mutable {
                    BorrowKind::Exclusive
                } else {
                    BorrowKind::Shared
                };
                if let Some((root, path, ref_var)) =
                    self.extract_borrow_root_and_path(expression)
                {
                    let full_target = if path.is_empty() {
                        root.clone()
                    } else {
                        format!("{root}.{path}")
                    };
                    if let Some(conflict) =
                        self.find_conflict(&root, &path, kind, ref_var.as_deref())
                    {
                        let msg = match (kind, conflict.kind) {
                            (BorrowKind::Shared, BorrowKind::Exclusive) => {
                                msg::cannot_borrow_as_shared_while_mutable(&full_target)
                            }
                            (BorrowKind::Exclusive, BorrowKind::Shared) => {
                                msg::cannot_borrow_as_mutable_while_shared(&full_target)
                            }
                            (BorrowKind::Exclusive, BorrowKind::Exclusive) => {
                                msg::cannot_borrow_as_mutable_more_than_once(&full_target)
                            }
                            (BorrowKind::Shared, BorrowKind::Shared) => unreachable!(),
                        };
                        self.error_with_note(
                            msg,
                            expr.span,
                            Some(conflict.span),
                            Some(msg::note_previous_borrow_here(conflict.kind.as_str())),
                        );
                    }
                } else {
                    self.check_expr(expression);
                }
            }
            ast::ExpressionKind::Move(operand) => {
                if let Some((root, path, ref_var)) = self.extract_root_and_path(operand) {
                    let full_target = if path.is_empty() {
                        root.clone()
                    } else {
                        format!("{root}.{path}")
                    };
                    if let Some(borrow) = self.find_any_borrow(&root, &path, ref_var.as_deref()) {
                        self.error_with_note(
                            msg::cannot_move_out_of_borrowed(&full_target),
                            expr.span,
                            Some(borrow.span),
                            Some(msg::note_previous_borrow_here(borrow.kind.as_str())),
                        );
                    }
                } else {
                    self.check_expr(operand);
                }
            }
            ast::ExpressionKind::Identifier(ident) => {
                if !self.ref_bindings.contains_key(&ident.name)
                    && let Some(borrow) = self.find_mutable_borrow(&ident.name, "", None)
                {
                    self.error_with_note(
                        msg::cannot_use_mutably_borrowed(&ident.name),
                        ident.span,
                        Some(borrow.span),
                        Some(msg::note_previous_borrow_here(borrow.kind.as_str())),
                    );
                }
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == ast::BinaryOperator::Assign {
                    self.check_expr(right);
                    self.check_assignment_target(left);
                    if let Some((root, path, _)) = self.extract_root_and_path(left) {
                        if !path.is_empty() {
                            let owner_scope = self
                                .local_scope_for(&root)
                                .unwrap_or(self.block_last_uses.len());
                            self.clear_overwritten_aggregate_borrows(
                                &root,
                                owner_scope,
                                &path,
                            );
                        }
                        let field = if path.is_empty() {
                            None
                        } else {
                            Some(path.as_str())
                        };
                        self.check_aggregate_field_borrow(&root, field, right);
                    }
                } else {
                    self.check_expr(left);
                    self.check_expr(right);
                }
            }
            ast::ExpressionKind::Unary { operand, .. } => {
                self.check_expr(operand);
            }
            ast::ExpressionKind::FieldAccess { object, .. } => {
                self.check_expr(object);
            }
            ast::ExpressionKind::Index { object, index } => {
                self.check_expr(object);
                self.check_expr(index);
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                self.check_expr(object);
                if let Some(s) = start {
                    self.check_expr(s);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
                if let Some(st) = step {
                    self.check_expr(st);
                }
            }
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.check_expr(function);
                self.check_call_arguments(None, None, None, arguments);
                for arg in arguments {
                    self.check_expr(arg);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                self.check_expr(receiver);
                let receiver_kind = self.method_receiver_kind(receiver, &method.name);
                if let Some(kind) = receiver_kind {
                    self.check_method_receiver_conflict(receiver, kind);
                }
                self.check_call_arguments(
                    Some(receiver),
                    receiver_kind,
                    Some(&method.name),
                    arguments,
                );
                for arg in arguments {
                    self.check_expr(arg);
                }
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(condition);
                self.check_block(then_branch);
                if let Some(else_b) = else_branch {
                    self.check_block(else_b);
                }
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.check_expr(condition);
                self.check_expr(then_expr);
                self.check_expr(else_expr);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.check_expr(value);
                self.check_expr(fallback);
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expr(expression);
                for arm in arms {
                    self.push_scope();
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    self.pop_scope();
                }
            }
            ast::ExpressionKind::Block(block) => {
                self.check_block(block);
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    match arg {
                        ast::MacroArg::Expression(expression) => self.check_expr(expression),
                        ast::MacroArg::Statement(statement) => self.check_statement(statement),
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{check_program, check_program_with_resolved_calls};
    use crate::{
        lexer,
        parser::Parser,
        semantic::{escape_check, typeck::TypeChecker},
        symbol_table::CompilerSymbolTable,
    };

    fn borrow_errors(source: &str) -> Vec<super::BorrowError> {
        let tokens = lexer::lex(source).expect("probe should lex");
        let (mut program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        TypeChecker::resolve_type_aliases_in_program(&mut program);
        check_program(&program)
    }

    fn integrated_safety_errors(
        source: &str,
        file: u32,
    ) -> (Vec<escape_check::EscapeError>, Vec<super::BorrowError>) {
        let tokens = lexer::lex_with_source(source, file).expect("probe should lex");
        let (mut program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        TypeChecker::resolve_type_aliases_in_program(&mut program);
        let mut checker = TypeChecker::new();
        let (type_errors, _) = checker
            .check_program_with_table(&program, &mut CompilerSymbolTable::new());
        assert!(type_errors.is_empty(), "probe should type check: {type_errors:?}");
        let resolved_callables = checker.resolved_callables().clone();
        let inferred_lets = checker.take_inferred_lets();
        crate::semantic::typeck::populate_inferred_let_types(&mut program, &inferred_lets);
        let escape = escape_check::analyze_program_with_callables(&program, &resolved_callables);
        let borrow = check_program_with_resolved_calls(
            &program,
            &escape.return_origins,
            &resolved_callables,
        );
        (escape.errors, borrow)
    }

    #[test]
    fn receiver_specific_method_origin_does_not_include_other_receiver_overload() {
        let (escape, borrow) = integrated_safety_errors(
            "struct A { i32 value; } struct B { i32 value; }\n\
             impl A { &i32 select(&self, i32 candidate) { return &self.value; } }\n\
             impl B { &i32 select(&self, &i32 candidate) { return candidate; } }\n\
             &i32 pass(&A owner) { i32 count = 1; return owner.select(count); }\n\
             i32 main() { A owner; let result = pass(&owner); return *result; }",
            17,
        );
        assert!(escape.is_empty(), "valid receiver-specific return rejected: {escape:?}");
        assert!(borrow.is_empty(), "valid receiver-specific call borrowed its scalar arg: {borrow:?}");
    }

    #[test]
    fn selected_receiver_method_still_rejects_expired_argument_origin() {
        let (escape, _) = integrated_safety_errors(
            "struct A { i32 value; } struct B { i32 value; }\n\
             impl A { &i32 select(&self, i32 candidate) { return &self.value; } }\n\
             impl B { &i32 select(&self, &i32 candidate) { return candidate; } }\n\
             &i32 bad(&B owner) { i32 count = 1; return owner.select(&count); }\n\
             i32 main() { B owner; let result = bad(&owner); return *result; }",
            18,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "expired B argument was accepted: {escape:?}");
    }

    #[test]
    fn raw_pointer_slot_reference_cannot_escape_function() {
        let (escape, _) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             &i32* bad() { i32* slot = &global_value; return &slot; }",
            31,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "returned pointer-slot reference was accepted: {escape:?}");
    }

    #[test]
    fn forwarded_pointer_slot_reference_cannot_escape() {
        let (escape, _) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             &i32* identity(&i32* value) { return value; }\n\
             &i32* forward(&i32* value) { return identity(value); }\n\
             &i32* bad() { let slot = &global_value; return forward(&slot); }\n\
             &i32* bad_parameter(i32* slot) { return &slot; }",
            37,
        );
        assert_eq!(escape.len(), 2, "local and parameter slots must not escape: {escape:?}");
        assert!(escape.iter().all(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }));
    }

    #[test]
    fn aggregate_held_pointer_slot_reference_cannot_outlive_block() {
        let (escape, borrow) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             struct Holder { &i32* slot; }\n\
             &i32* identity(&i32* value) { return value; }\n\
             void main() { Holder holder; if (true) { i32* slot = &global_value; holder.slot = identity(&slot); } i32* loaded = *holder.slot; }",
            38,
        );
        assert!(escape.is_empty(), "unexpected escape error: {escape:?}");
        assert!(borrow.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }), "aggregate-held pointer-slot reference escaped: {borrow:?}");
    }

    #[test]
    fn aggregate_held_mutable_pointer_slot_borrow_blocks_reassignment() {
        let (_, borrow) = integrated_safety_errors(
            "i32 first = 7; i32 second = 8;\n\
             struct Holder { &mut i32* slot; }\n\
             void main() { i32* slot = &first; Holder holder = { .slot = &mut slot }; slot = &second; i32* loaded = *holder.slot; }",
            39,
        );
        assert!(borrow.iter().any(|error| {
            error.message == crate::diagnostics::messages::cannot_assign_to_borrowed("slot")
        }), "aggregate-held mutable loan did not block slot reassignment: {borrow:?}");
    }

    #[test]
    fn raw_pointee_borrows_do_not_borrow_the_pointer_slot() {
        let (escape, borrow) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             &i32 index() { i32* raw = &global_value; return &raw[0]; }\n\
             &i32 dereference() { i32* raw = &global_value; return &*raw; }\n\
             &i32 unchecked_local_pointee() { i32 local = 9; i32* raw = &local; return &*raw; }\n\
             void main() { i32 local = 9; i32* raw = &local; let view = &raw[0]; raw = &global_value; i32 value = *view; }",
            40,
        );
        assert!(escape.is_empty(), "raw pointee was constrained to pointer slot: {escape:?}");
        assert!(borrow.is_empty(), "raw pointee loan blocked pointer reassignment: {borrow:?}");
    }

    #[test]
    fn initialized_raw_destination_remains_unchecked() {
        let (escape, borrow) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             void main() { i32* raw = &global_value; if (true) { i32 local = 9; raw = &local; } }",
            42,
        );
        assert!(escape.is_empty(), "raw destination acquired a lifetime constraint: {escape:?}");
        assert!(borrow.is_empty(), "raw destination acquired a loan: {borrow:?}");
    }

    #[test]
    fn pointer_slot_in_match_arm_has_local_origin() {
        let (escape, _) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             &i32* bad() { match 0 { _: { i32* slot = &global_value; return &slot; } }; }",
            43,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "match arm erased the pointer-slot lifetime: {escape:?}");
    }

    #[test]
    fn raw_pointer_classification_respects_shadowed_bindings() {
        let (escape, _) = integrated_safety_errors(
            "struct Value { i32 value; }\n\
             &i32 bad(i32* slot) { if (true) { Value slot; return &slot.value; } return slot; }\n\
             &i32 bad_after_shadow() { Value slot; if (true) { i32* slot; } return &slot.value; }",
            41,
        );
        assert_eq!(escape.len(), 2, "shadowed raw-pointer classification lost local origins: {escape:?}");
    }

    #[test]
    fn raw_pointer_slot_reference_cannot_outlive_block() {
        let (_, borrow) = integrated_safety_errors(
            "i32 global_value = 7;\n\
             void main() { &i32* escaped; if (true) { i32* slot = &global_value; escaped = &slot; } i32* loaded = *escaped; }",
            32,
        );
        assert!(borrow.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }), "pointer-slot reference escaped its block: {borrow:?}");
    }

    #[test]
    fn live_mutable_raw_pointer_slot_borrow_blocks_slot_reassignment() {
        let (_, borrow) = integrated_safety_errors(
            "i32 first = 7; i32 second = 8;\n\
             void main() { i32* slot = &first; let slot_reference = &mut slot; slot = &second; i32* loaded = *slot_reference; }",
            33,
        );
        assert!(borrow.iter().any(|error| {
            error.message == crate::diagnostics::messages::cannot_assign_to_borrowed("slot")
        }), "mutable pointer-slot borrow did not block reassignment: {borrow:?}");
    }

    #[test]
    fn pointer_slot_borrows_preserve_live_and_raw_pointer_controls() {
        let (escape, borrow) = integrated_safety_errors(
            "i32 first = 7; i32 second = 8; i32* global_pointer = &first;\n\
             void read_slot(&i32* slot_reference) { i32* loaded = *slot_reference; i32 value = *loaded; }\n\
             void main() { i32* slot = &first; read_slot(&slot); &mut i32* mutable_slot = &mut slot; *mutable_slot = &second; let pointee = &global_pointer[0]; i32 local = 9; i32* raw_destination; raw_destination = &local; }",
            34,
        );
        assert!(escape.is_empty(), "valid pointer-slot control rejected: {escape:?}");
        assert!(borrow.is_empty(), "valid pointer-slot control rejected: {borrow:?}");
    }

    #[test]
    fn opaque_external_return_cannot_escape_a_local_reference_argument() {
        let (escape, _) = integrated_safety_errors(
            "extern \"C\" { &i32 foreign_reference(&i32 value); }\n\
             &i32 bad() { i32 owner = 149; return foreign_reference(&owner); }",
            35,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "opaque external return escaped a local argument: {escape:?}");
    }

    #[test]
    fn known_reference_origin_survives_an_opaque_external_branch() {
        let (escape, _) = integrated_safety_errors(
            "extern \"C\" { &i32 foreign_reference(&i32 value); }\n\
             &i32 choose(&i32 value, bool use_foreign) { if (use_foreign) { return foreign_reference(value); } return value; }\n\
             &i32 bad() { i32 owner = 151; return choose(&owner, true); }",
            36,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "known local origin was erased by opaque branch: {escape:?}");
    }

    #[test]
    fn overloaded_function_origins_follow_the_selected_declaration() {
        let (escape, borrow) = integrated_safety_errors(
            "&i32 select(i32 candidate, &i32 owner) { return owner; }\n\
             &i32 select(&i32 candidate, i32 owner) { return candidate; }\n\
             &i32 pass(&i32 owner) { i32 local = 3; return select(local, owner); }\n\
             i32 main() { i32 owner = 7; let result = pass(&owner); return *result; }",
            19,
        );
        assert!(escape.is_empty(), "selected overload return rejected: {escape:?}");
        assert!(borrow.is_empty(), "selected overload borrowed the local scalar: {borrow:?}");
    }

    #[test]
    fn recursive_forwarding_keeps_the_resolved_parameter_origin() {
        let (escape, borrow) = integrated_safety_errors(
            "&i32 forward(&i32 value, bool done) {\n\
             if (done) { return value; } return forward(value, true); }\n\
             &i32 pass(&i32 owner) { return forward(owner, false); }\n\
             i32 main() { i32 owner = 7; let result = pass(&owner); return *result; }",
            20,
        );
        assert!(escape.is_empty(), "recursive forwarding rejected: {escape:?}");
        assert!(borrow.is_empty(), "recursive forwarding borrowed incorrectly: {borrow:?}");
    }

    #[test]
    fn receiver_type_alias_preserves_the_selected_method_identity() {
        let (escape, borrow) = integrated_safety_errors(
            "struct A { i32 value; } struct B { i32 value; } type AliasA = A;\n\
             impl A { &i32 select(&self, i32 candidate) { return &self.value; } }\n\
             impl B { &i32 select(&self, &i32 candidate) { return candidate; } }\n\
             &i32 pass(&AliasA owner) { i32 count = 1; return owner.select(count); }",
            23,
        );
        assert!(escape.is_empty(), "aliased receiver return rejected: {escape:?}");
        assert!(borrow.is_empty(), "aliased receiver borrowed its scalar arg: {borrow:?}");
    }

    #[test]
    fn generic_receiver_method_uses_its_resolved_declaration() {
        let (escape, borrow) = integrated_safety_errors(
            "struct Holder<T> { T value; }\n\
             impl<T> Holder<T> { &T select(&self, i32 candidate) { return &self.value; } }\n\
             &i32 pass(&Holder<i32> owner) { i32 count = 1; return owner.select(count); }",
            24,
        );
        assert!(escape.is_empty(), "generic receiver return rejected: {escape:?}");
        assert!(borrow.is_empty(), "generic receiver borrowed its scalar arg: {borrow:?}");
    }

    #[test]
    fn resolved_external_return_keeps_all_reference_argument_origins() {
        let (escape, borrow) = integrated_safety_errors(
            "struct RefBox { &i32 value; }\n\
             extern \"C\" { &i32 foreign_ref(&i32 value); }\n\
             i32 main() { RefBox box; { i32 owner = 8;\n\
             box.value = foreign_ref(&owner); } return *box.value; }",
            25,
        );
        assert!(escape.is_empty(), "opaque external return misclassified: {escape:?}");
        assert!(borrow.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }), "external reference parameter loan was lost: {borrow:?}");
    }

    #[test]
    fn resolved_call_sites_include_source_file_identity() {
        let source_a = "struct A { i32 value; }\n\
                        impl A { &i32 get(&self) { return &self.value; } }\n\
                        &i32 pass_a(&A owner) { return owner.get(); }";
        let source_b = "struct B { i32 value; }\n\
                        impl B { &i32 get(&self) { return &self.value; } }\n\
                        &i32 pass_b(&B owner) { return owner.get(); }";
        let tokens = lexer::lex_with_source(source_a, 21).expect("probe should lex");
        let (mut program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        let tokens = lexer::lex_with_source(source_b, 22).expect("probe should lex");
        let (second_program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        program.items.extend(second_program.items);
        let mut checker = TypeChecker::new();
        let (type_errors, _) = checker
            .check_program_with_table(&program, &mut CompilerSymbolTable::new());
        assert!(type_errors.is_empty(), "probe should type check: {type_errors:?}");
        let sites = checker.resolved_callables().keys().copied().collect::<Vec<_>>();
        assert_eq!(sites.len(), 2, "both files' call sites should resolve: {sites:?}");
        assert!(sites.iter().any(|site| site.0 == 21));
        assert!(sites.iter().any(|site| site.0 == 22));
        assert_ne!(sites[0], sites[1]);
        let identities = checker
            .resolved_callables()
            .values()
            .map(|resolved| resolved.identity)
            .collect::<Vec<_>>();
        assert_ne!(identities[0], identities[1]);
    }

    #[test]
    fn fixed_point_waits_for_resolved_method_summary_when_name_saturated() {
        let (escape, borrow) = integrated_safety_errors(
            "struct A { i32 value; } struct B { i32 value; }\n\
             impl A { &i32 select(&self, &i32 a, &i32 b) { return a; } }\n\
             impl B {\n\
                 &i32 step(&self, &i32 a, &i32 b) { return b; }\n\
                 &i32 select(&self, &i32 a, &i32 b) { return self.step(a, b); }\n\
             }\n\
             &i32 pass(&B owner, &i32 owner_val) { i32 local = 4; return owner.select(&local, owner_val); }\n\
             i32 main() { B owner; i32 val = 10; let result = pass(&owner, &val); return *result; }",
            26,
        );
        assert!(escape.is_empty(), "forwarded parameter origin in B rejected: {escape:?}");
        assert!(borrow.is_empty(), "local parameter was falsely marked as escaped: {borrow:?}");
    }

    #[test]
    fn fixed_point_rejects_expired_forwarded_parameter_when_name_saturated() {
        let (escape, _) = integrated_safety_errors(
            "struct A { i32 value; } struct B { i32 value; }\n\
             impl A { &i32 select(&self, &i32 a, &i32 b) { return b; } }\n\
             impl B {\n\
                 &i32 step(&self, &i32 a, &i32 b) { return a; }\n\
                 &i32 select(&self, &i32 a, &i32 b) { return self.step(a, b); }\n\
             }\n\
             &i32 bad(&B owner, &i32 owner_val) { i32 local = 4; return owner.select(&local, owner_val); }\n\
             i32 main() { B owner; i32 val = 10; let result = bad(&owner, &val); return *result; }",
            27,
        );
        assert!(escape.iter().any(|error| {
            error.message == crate::diagnostics::messages::returned_reference_escapes()
        }), "expired forwarded parameter was accepted: {escape:?}");
    }

    #[test]
    fn aggregate_field_borrow_cannot_outlive_direct_local() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; { i32 owner = 8; box.value = &owner; }\n\
             return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn aggregate_field_borrow_cannot_outlive_call_origin() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             &i32 identity(&i32 value) { return value; }\n\
             i32 main() { RefBox box; { i32 owner = 8;\n\
             box.value = identity(&owner); } return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn aggregate_field_borrow_remains_valid_with_live_referent() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { i32 owner = 8; RefBox box;\n\
             box.value = &owner; return *box.value; }",
        );
        assert!(errors.is_empty(), "live aggregate borrow rejected: {errors:?}");
    }

    #[test]
    fn aggregate_field_borrow_used_before_referent_scope_ends_is_valid() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; { i32 owner = 8;\n\
             box.value = &owner; return *box.value; } }",
        );
        assert!(errors.is_empty(), "inner-scope borrow rejected: {errors:?}");
    }

    #[test]
    fn chained_call_keeps_each_possible_origin_until_escape_check() {
        let errors = borrow_errors(
            "&i32 select(&i32 first, &i32 second) {\n\
             if (true) { return first; } return second; }\n\
             &i32 identity(&i32 value) { return value; }\n\
             struct RefBox { &i32 value; }\n\
             i32 main() { i32 outer = 1; RefBox box; { i32 inner = 2;\n\
             &i32 selected = select(&outer, &inner);\n\
             box.value = identity(selected); } return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn opaque_external_return_preserves_reference_argument_loan() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             extern \"C\" { &i32 foreign_ref(&i32 value); }\n\
             i32 main() { RefBox box; { i32 owner = 8;\n\
             box.value = foreign_ref(&owner); } return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn nested_aggregate_field_assignment_tracks_referent_scope() {
        let errors = borrow_errors(
            "struct Inner { &i32 value; } struct Outer { Inner inner; }\n\
             i32 main() { Outer box; { i32 owner = 8;\n\
             box.inner.value = &owner; } return *box.inner.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn shadowed_referent_name_does_not_inherit_outer_scope() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; i32 owner = 1; { i32 owner = 2;\n\
             box.value = &owner; } return 0; }",
        );
        assert!(errors.is_empty(), "shadowed local borrow rejected: {errors:?}");
    }

    #[test]
    fn shadowed_borrower_name_does_not_hide_outer_aggregate_escape() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; { i32 owner = 8; box.value = &owner;\n\
             RefBox box; box.value = &owner; } return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn write_through_shadowed_aggregate_does_not_replace_outer_field() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; { i32 owner = 8; box.value = &owner; }\n\
             { RefBox box; i32 other = 4; box.value = &other; }\n\
             return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn write_inside_expression_block_does_not_replace_outer_field() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { RefBox box; { i32 owner = 8; box.value = &owner; }\n\
             if (true) { RefBox box; i32 other = 4; box.value = &other; }\n\
             return *box.value; }",
        );
        assert!(errors.iter().any(|error| {
            error.message == crate::diagnostics::messages::reference_escapes_scope()
        }));
    }

    #[test]
    fn aggregate_field_reassignment_replaces_the_previous_origin() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { i32 outer = 3; RefBox box; { i32 inner = 8;\n\
             box.value = &inner; box.value = &outer; } return *box.value; }",
        );
        assert!(errors.is_empty(), "overwritten field origin retained: {errors:?}");
    }

    #[test]
    fn aggregate_initializer_tracks_reference_field() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { i32 owner = 8; RefBox box = { .value = &owner };\n\
             return *box.value; }",
        );
        assert!(errors.is_empty(), "live initialized aggregate borrow rejected: {errors:?}");
    }

    #[test]
    fn use_of_another_aggregate_field_does_not_extend_reference_field() {
        let errors = borrow_errors(
            "struct RefPair { &i32 value; i32 tag; }\n\
             i32 main() { RefPair pair; pair.tag = 9; { i32 owner = 8;\n\
             pair.value = &owner; } return pair.tag; }",
        );
        assert!(errors.is_empty(), "other field use extended the borrow: {errors:?}");
    }

    #[test]
    fn field_replacement_before_read_ends_the_expired_origin() {
        let errors = borrow_errors(
            "struct RefBox { &i32 value; }\n\
             i32 main() { i32 outer = 3; RefBox box; { i32 inner = 8;\n\
             box.value = &inner; } box.value = &outer; return *box.value; }",
        );
        assert!(errors.is_empty(), "replacement did not end the old field loan: {errors:?}");
    }

}
