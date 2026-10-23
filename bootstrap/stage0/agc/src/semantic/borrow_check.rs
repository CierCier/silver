//! Borrow checker: enforces `&`/`&mut` exclusivity; raw pointers are unchecked.
//! Places make field conflicts structural instead of string-prefix based.
use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
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

fn named_type_name(ty: &ast::Type) -> Option<&str> {
    match ty.kind.as_ref() {
        ast::TypeKind::Named(named) => named.path.last().map(|name| name.name.as_str()),
        ast::TypeKind::Reference(reference) => named_type_name(&reference.inner),
        ast::TypeKind::Pointer(pointer) => named_type_name(&pointer.inner),
        _ => None,
    }
}

pub fn check_program(program: &ast::Program) -> Vec<BorrowError> {
    let mut checker = BorrowChecker::new();
    checker.check_program(program);
    checker.errors
}

struct BorrowChecker {
    errors: Vec<BorrowError>,
    /// Stack of lexical scopes containing active loans registered in each block.
    scopes: Vec<Vec<ActiveBorrow>>,
    /// Stack of last-use maps per lexical block.
    block_last_uses: Vec<FxHashMap<String, usize>>,
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
}

impl BorrowChecker {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            scopes: vec![Vec::new()],
            block_last_uses: Vec::new(),
            struct_ref_fields: FxHashMap::default(),
            struct_field_types: FxHashMap::default(),
            global_var_types: FxHashMap::default(),
            method_receiver_access: FxHashMap::default(),
            method_argument_access: FxHashMap::default(),
            var_types: FxHashMap::default(),
            ref_bindings: FxHashMap::default(),
            raw_ptr_vars: FxHashSet::default(),
        }
    }

    fn get_last_use_for(&self, name: &str) -> Option<usize> {
        for map in self.block_last_uses.iter().rev() {
            if let Some(pos) = map.get(name) {
                return Some(*pos);
            }
        }
        None
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
        self.var_types.clear();
        self.ref_bindings.clear();
        self.raw_ptr_vars.clear();

        for param in parameters {
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
        self.block_last_uses.push(block_last_use);

        for (i, stmt) in block.statements.iter().enumerate() {
            self.expire_loans_before(i);
            self.check_statement(stmt);
        }
        // Final statement boundary: expire loans whose last use was within this block.
        self.expire_loans_before(block.statements.len());
        self.pop_scope();
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
                    if let Some(ref ty) = let_stmt.type_annotation {
                        if matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(_)) {
                            self.raw_ptr_vars.insert(name.clone());
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
                                loan.last_use = self.get_last_use_for(name);
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
                            self.check_expr(init);
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
                    last_use: self.get_last_use_for(binding_name),
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
        if let Some((root, path, ref_var)) = self.extract_root_and_path(target) {
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
                last_use: self.get_last_use_for(binding_name),
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

    fn check_aggregate_field_borrow(
        &mut self,
        owner_name: &str,
        field_name_opt: Option<&str>,
        field_expr: &ast::Expression,
    ) {
        if let Some(field_name) = field_name_opt {
            if let Some(struct_name) = self.var_types.get(owner_name) {
                if let Some(ref_fields) = self.struct_ref_fields.get(struct_name) {
                    if !ref_fields.contains(field_name) {
                        return;
                    }
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
            self.register_named_borrow(owner_name, expression, kind, field_expr.span);
        } else if let ast::ExpressionKind::Identifier(ident) = field_expr.kind.as_ref() {
            if let Some(existing) = self.ref_bindings.get(&ident.name).cloned() {
                let loan = ActiveBorrow {
                    root: existing.root.clone(),
                    path: existing.path.clone(),
                    kind: existing.kind,
                    span: field_expr.span,
                    borrower: Some(owner_name.to_string()),
                    last_use: self.get_last_use_for(owner_name),
                    param: false,
                };
                if let Some(scope) = self.scopes.last_mut() {
                    scope.push(loan);
                }
            } else {
                self.check_expr(field_expr);
            }
        } else {
            self.check_expr(field_expr);
        }
    }

    /// Check an assignment target (`x = val` or `p.left = val`).
    fn check_assignment_target(&mut self, target: &ast::Expression) {
        if let Some((root, path, ref_var)) = self.extract_root_and_path(target) {
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
                let (root, path, _) = self.extract_root_and_path(expression)?;
                if self.raw_ptr_vars.contains(&root) {
                    return None;
                }
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
                if let Some((root, path, ref_var)) = self.extract_root_and_path(expression) {
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
