//! Move-out checker: per-path dataflow over each function body that reports
//! use-after-move of non-copyable (Drop) values.
//!
//! A variable is *moved* (its runtime drop flag is cleared) when it is:
//! - the operand of `move x` (or the root of `move x.field`),
//! - a by-value method receiver (`x.consume()` where the method has an
//!   `InstanceValue` receiver),
//! - a by-value argument to a function/method whose parameter is a value type,
//! - the root of a bare `return x;` (implicit move), or
//! - the receiver of an explicit `v.drop()` call.
//!
//! Once moved, any use of the variable — reading it, borrowing it, writing
//! through it — is a use-after-free and is reported as an error. The analysis
//! is per-path (Rust-like): a variable moved on *any* fall-through path is
//! unusable afterwards, but moves inside a branch that never falls through
//! (return / break / continue) do not propagate past it.
//!
//! v1 limitations (safe, no false positives, but incomplete):
//! - generic-typed bindings (`T x` inside a generic function) are not tracked;
//! - `break`/`continue` are handled conservatively through the loop merge.

use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use rustc_hash::{FxHashMap, FxHashSet};

/// One move-check diagnostic; same shape as `typeck::TypeError` with optional
/// secondary note pointing to the earlier move origin.
#[derive(Debug, Clone)]
pub struct MoveError {
    pub message: String,
    pub span: Span,
    pub note_span: Option<Span>,
    pub note_message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VarState {
    pub level: u8, // 0 = live, 1 = partially moved, 2 = fully moved
    pub move_span: Option<Span>,
    pub move_reason: Option<&'static str>,
    pub moved_fields: FxHashMap<String, (Span, &'static str)>,
}

impl VarState {
    pub fn new_live() -> Self {
        Self {
            level: 0,
            move_span: None,
            move_reason: None,
            moved_fields: FxHashMap::default(),
        }
    }

    pub fn mark_moved(&mut self, span: Span, reason: &'static str) {
        self.level = 2;
        self.move_span = Some(span);
        self.move_reason = Some(reason);
        self.moved_fields.clear();
    }

    pub fn mark_field_moved(&mut self, path: &str, span: Span, reason: &'static str) {
        if self.level < 2 {
            self.level = 1;
            if self.move_span.is_none() {
                self.move_span = Some(span);
                self.move_reason = Some(reason);
            }
            self.moved_fields.insert(path.to_string(), (span, reason));
        }
    }

    pub fn mark_field_reinitialized(&mut self, path: &str) {
        if self.level == 1 {
            self.moved_fields.remove(path);
            let prefix = format!("{path}.");
            self.moved_fields.retain(|k, _| !k.starts_with(&prefix));
            if self.moved_fields.is_empty() {
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
    }
}

/// Helper to split an expression into its root variable name and dot-separated field path.
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

/// Moved-ness lattice per live variable: tracks status, move site and reason.
type State = FxHashMap<String, VarState>;

/// Scope entry: (name, previous state, previous type) so shadowing restores.
type ScopeEntry = (String, Option<VarState>, Option<ast::Type>);
/// Program-wide facts used to classify moves (computed once per program).
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

    /// True if `ty` can own resources (has a Drop impl, possibly nested).
    /// `Task<T>` handles are tracked too: `wait` consumes the handle, so a
    /// second `wait` on the same identifier is a use-after-move.
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
                        // `move x.field` (partial field move)
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
                            } else if let Some((move_span, reason)) = var.is_field_moved(&path) {
                                self.error_with_note(
                                    format!("use of moved field '{root_name}.{path}'"),
                                    inner.span,
                                    Some(move_span),
                                    reason,
                                );
                            } else {
                                var.mark_field_moved(
                                    &path,
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
                    || match receiver.kind.as_ref() {
                        ast::ExpressionKind::Identifier(ident) => {
                            var_types.get(&ident.name).is_some_and(|ty| {
                                self.facts
                                    .value_receivers
                                    .contains(&(Facts::owner_key(ty), method.name.clone()))
                            })
                        }
                        _ => false,
                    };
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
                            var.mark_field_moved(
                                &path,
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
                            var.mark_field_moved(
                                &path,
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
                                    if let Some(var) = state.get(&root_name)
                                        && let Some((move_span, reason)) = var.is_field_moved(&path)
                                    {
                                        self.error_with_note(
                                            format!("use of moved field '{root_name}.{path}'"),
                                            arg.span,
                                            Some(move_span),
                                            reason,
                                        );
                                    }
                                    if let Some(var) = state.get_mut(&root_name) {
                                        var.mark_field_moved(
                                            &path,
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
                if let Some((root_name, path)) = expr_root_and_path(expr) {
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
                        } else if let Some((move_span, reason)) = var.is_field_moved(&path) {
                            self.error_with_note(
                                format!("use of moved field '{root_name}.{path}'"),
                                expr.span,
                                Some(move_span),
                                reason,
                            );
                        }
                    }
                } else {
                    self.check_expr(object, state, scopes, var_types);
                }
            }
            ast::ExpressionKind::Index { object, .. } => {
                self.check_expr(object, state, scopes, var_types);
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

                    // Re-initialization handling
                    if let Some((root_name, path)) = expr_root_and_path(left) {
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
                                var.mark_field_reinitialized(&path);
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
}
