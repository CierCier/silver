//! Active Borrow Conflict Checker (`semantic/borrow_check.rs`).
//!
//! Enforces the core memory-safety invariant:
//! For any memory location `P` at any program point:
//!   `(Any number of &P) ⊕ (Exactly one &mut P)`
//!
//! Invariants:
//! 1. Cannot take `&mut x` while any active `&x` or `&mut x` exists.
//! 2. Cannot take `&x` while any active `&mut x` exists.
//! 3. Cannot assign to `x` or mutate `x` while any borrow of `x` is active.
//! 4. Cannot move `x` (`move x`) while any borrow of `x` is active.
//! 5. Cannot read/use `x` while an exclusive borrow `&mut x` is active.
//! 6. Disjoint field borrows (`&mut p.left` and `&mut p.right`) are permitted simultaneously.
//! 7. Raw pointers (`T*`) bypass borrow checking.
//! 8. Reborrowing from `&mut T` creates a temporary reborrow that suspends the original.

use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
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

/// Metadata about a reference variable in scope.
#[derive(Debug, Clone)]
pub struct RefVarInfo {
    pub root: String,
    pub path: String,
    pub kind: BorrowKind,
    pub span: Span,
}

/// An active loan taken on `root.path`.
#[derive(Debug, Clone)]
pub struct ActiveBorrow {
    pub root: String,
    pub path: String,
    pub kind: BorrowKind,
    pub span: Span,
    pub borrower: Option<String>,
    /// Statement index (within the enclosing block) after which this loan
    /// expires, if it was last used there (NLL); `None` = live until scope exit
    /// (reference parameters, or last use not yet observed).
    pub last_use: Option<usize>,
    /// Reference-parameter loan: stays live for the whole function (the caller
    /// still holds the borrow), never NLL-expired.
    pub param: bool,
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

    fn check_program(&mut self, program: &ast::Program) {
        self.struct_ref_fields.clear();
        for item in &program.items {
            if let ast::ItemKind::Struct(st) = &item.kind {
                let mut ref_fields = FxHashSet::default();
                for f in &st.fields {
                    if matches!(f.field_type.kind.as_ref(), ast::TypeKind::Reference(_)) {
                        ref_fields.insert(f.name.name.clone());
                    }
                }
                self.struct_ref_fields.insert(st.name.name.clone(), ref_fields);
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
            if let ast::TypeKind::Named(named) = param.param_type.kind.as_ref() {
                if let Some(last) = named.path.last() {
                    self.var_types.insert(param.name.name.clone(), last.name.clone());
                }
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
    }

    /// Remove loans whose last use was strictly before statement `i`:
    /// they no longer constrain statement `i` or later code.
    fn expire_loans_before(&mut self, i: usize) {
        let mut expired: FxHashSet<String> = FxHashSet::default();
        for scope in &mut self.scopes {
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
                self.collect_stmt_uses(inner);
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
                        if let ast::ExpressionKind::MethodCall { receiver, .. } = init.kind.as_ref() {
                            if let ast::ExpressionKind::TypeName(ty) = receiver.kind.as_ref() {
                                if let ast::TypeKind::Named(named) = ty.kind.as_ref() {
                                    if let Some(last) = named.path.last() {
                                        self.var_types.insert(name.clone(), last.name.clone());
                                    }
                                }
                            }
                        } else if let ast::ExpressionKind::StructLiteral { path, .. } = init.kind.as_ref() {
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
                                let loan = ActiveBorrow {
                                    root: existing.root.clone(),
                                    path: existing.path.clone(),
                                    kind: existing.kind,
                                    span: init.span,
                                    borrower: Some(name.clone()),
                                    last_use: self.get_last_use_for(name),
                                    param: false,
                                };
                                self.ref_bindings.insert(
                                    name.clone(),
                                    RefVarInfo {
                                        root: existing.root,
                                        path: existing.path,
                                        kind: existing.kind,
                                        span: init.span,
                                    },
                                );
                                if let Some(scope) = self.scopes.last_mut() {
                                    scope.push(loan);
                                }
                            } else {
                                self.check_expr(init);
                            }
                        } else if let ast::ExpressionKind::StructLiteral { fields, .. } =
                            init.kind.as_ref()
                        {
                            for f in fields {
                                self.check_aggregate_field_borrow(name, Some(f.name.name.as_str()), &f.value);
                            }
                        } else if let ast::ExpressionKind::Initializer { items } =
                            init.kind.as_ref()
                        {
                            for item in items {
                                let (val, field_name) = match item {
                                    ast::InitializerItem::Positional(e) => (e, None),
                                    ast::InitializerItem::Field { name, value } => (value, Some(name.name.as_str())),
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
            ast::ExpressionKind::Index { object, .. } => self.extract_root_and_path(object),
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => self.extract_root_and_path(operand),
            _ => None,
        }
    }

    /// Register a borrow bound to a named variable (`let r = &...`).
    fn register_named_borrow(
        &mut self,
        binding_name: &str,
        target: &ast::Expression,
        kind: BorrowKind,
        span: Span,
    ) {
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
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                self.check_expr(function);
                for arg in arguments {
                    self.check_expr(arg);
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            } => {
                self.check_expr(receiver);
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
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expr(expression);
                for arm in arms {
                    self.push_scope();
                    self.check_expr(&arm.body);
                    self.pop_scope();
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> ast::Program {
        let tokens = crate::lexer::lex(source).expect("lex failed");
        let mut parser = crate::parser::prt_parser::PRT_Parser::new(None);
        parser.parse_program(&tokens).expect("parse failed")
    }

    fn check_source(source: &str) -> Vec<BorrowError> {
        let program = parse(source);
        check_program(&program)
    }

    #[test]
    fn allows_multiple_shared_borrows() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &Point r1 = &pt;
                &Point r2 = &pt;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn rejects_mutable_borrow_when_shared_active() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &Point r1 = &pt;
                &mut Point r2 = &mut pt;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot borrow 'pt' as mutable because it is already borrowed as shared")
        );
    }

    #[test]
    fn rejects_shared_borrow_when_mutable_active() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &mut Point r1 = &mut pt;
                &Point r2 = &pt;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot borrow 'pt' as shared because it is already borrowed as mutable")
        );
    }

    #[test]
    fn rejects_multiple_mutable_borrows() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &mut Point r1 = &mut pt;
                &mut Point r2 = &mut pt;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot borrow 'pt' as mutable more than once at a time")
        );
    }

    #[test]
    fn allows_disjoint_field_borrows() {
        let src = r#"
            struct Pair { i64 left; i64 right; }
            void test() {
                Pair p;
                p.left = 1;
                p.right = 2;
                &mut i64 l = &mut p.left;
                &mut i64 r = &mut p.right;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn rejects_conflicting_field_borrows() {
        let src = r#"
            struct Pair { i64 left; i64 right; }
            void test() {
                Pair p;
                p.left = 1;
                p.right = 2;
                &mut i64 l1 = &mut p.left;
                &mut i64 l2 = &mut p.left;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot borrow 'p.left' as mutable more than once at a time")
        );
    }

    #[test]
    fn rejects_field_borrow_when_whole_container_borrowed() {
        let src = r#"
            struct Pair { i64 left; i64 right; }
            void test() {
                Pair p;
                p.left = 1;
                p.right = 2;
                &Pair p_ref = &p;
                &mut i64 l = &mut p.left;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains(
            "cannot borrow 'p.left' as mutable because it is already borrowed as shared"
        ));
    }

    #[test]
    fn rejects_mutation_while_borrowed() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &Point r = &pt;
                pt.x = 3;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot assign to 'pt.x' because it is borrowed")
        );
    }

    #[test]
    fn rejects_move_while_borrowed() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void sink(Point p) {}
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                &Point r = &pt;
                sink(move pt);
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot move out of 'pt' because it is borrowed")
        );
    }

    #[test]
    fn allows_borrow_after_previous_borrow_scope_ended() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                {
                    &mut Point m = &mut pt;
                }
                &Point r = &pt;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn allows_nested_subfield_disjoint_borrows() {
        let src = r#"
            struct Pair { i64 left; i64 right; }
            struct Node { Pair pair; i64 val; }
            void test() {
                Node n;
                &mut i64 l = &mut n.pair.left;
                &mut i64 r = &mut n.pair.right;
                &mut i64 v = &mut n.val;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn rejects_nested_subfield_conflict() {
        let src = r#"
            struct Pair { i64 left; i64 right; }
            struct Node { Pair pair; i64 val; }
            void test() {
                Node n;
                &Pair p = &n.pair;
                &mut i64 l = &mut n.pair.left;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains(
            "cannot borrow 'n.pair.left' as mutable because it is already borrowed as shared"
        ));
    }

    #[test]
    fn allows_sequential_temporary_call_borrows() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void modify(&mut Point p) {}
            void test() {
                Point pt;
                pt.x = 1;
                pt.y = 2;
                modify(&mut pt);
                modify(&mut pt);
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn raw_pointers_bypass_borrow_checker() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test(Point* p) {
                p.x = 10;
                p.x = 20;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn allows_mutating_through_mut_ref_param() {
        let src = r#"
            struct HttpRequest { str body; }
            void set_body(&mut HttpRequest self, str body) {
                self.body = body;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn allows_mutation_after_ref_last_use() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            i32 read(i64 v) { return 0; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                read(r.x);
                pt.x = 100;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn allows_mut_borrow_after_shared_last_use() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            i32 read(i64 v) { return 0; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                read(r.x);
                &mut Point m = &mut pt;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn rejects_mutation_when_ref_never_used() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                pt.x = 100;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot assign to 'pt.x' because it is borrowed")
        );
    }

    #[test]
    fn rejects_conflicting_mut_borrow_before_shared_use() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            i32 read(i64 v) { return 0; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                &mut Point m = &mut pt;
                read(r.x);
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot borrow 'pt' as mutable because it is already borrowed as shared")
        );
    }

    #[test]
    fn ref_param_borrow_survives_unused_body() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void touch(&Point p) {}
            void test() {
                Point pt;
                pt.x = 1;
                touch(&pt);
                pt.x = 2;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn reborrow_after_last_use_keeps_loan_chain() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            i32 read(i64 v) { return 0; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                read(r.x);
                &Point m = &*r;
                read(m.x);
                pt.x = 100;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn rejects_mutation_while_reborrow_still_live() {
        let src = r#"
            struct Point { i64 x; i64 y; }
            void test() {
                Point pt;
                pt.x = 1;
                &Point r = &pt;
                &Point m = &*r;
                pt.x = 100;
                i64 v = m.x;
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot assign to 'pt.x' because it is borrowed")
        );
    }

    #[test]
    fn struct_literal_with_borrow_locks_referent() {
        let src = r#"
            struct StringView<'a> { &'a i64 data; i64 len; }
            i64 read_view(StringView v) { return *v.data; }
            void test() {
                i64 val = 42;
                StringView view;
                view.data = &val;
                view.len = 1;
                val = 100;
                read_view(view);
            }
        "#;
        let errors = check_source(src);
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .message
                .contains("cannot assign to 'val' because it is borrowed")
        );
    }

    #[test]
    fn struct_literal_with_borrow_unlocked_after_nll_last_use() {
        let src = r#"
            struct StringView<'a> { &'a i64 data; i64 len; }
            i64 read_view(StringView v) { return *v.data; }
            void test() {
                i64 val = 42;
                StringView view;
                view.data = &val;
                view.len = 1;
                read_view(view);
                val = 100;
            }
        "#;
        let errors = check_source(src);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }
}
