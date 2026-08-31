//! Escape checker: references (`&T`) must not outlive their referent.
//!
//! A borrow of a function-local value (`&local`, `&param` of value type) dies
//! with the frame. Such a reference must not escape: it cannot be returned
//! from the function or stored into a global. A reference parameter (`&T` or
//! `&mut T`) is a caller-owned borrow origin; the returned reference carries
//! that parameter's name. Globals, heap values, and raw-pointer pointees are
//! independent origins and may escape without a caller-lifetime constraint.
//!
//! The checker remains intentionally conservative at opaque expressions:
//! references stored into structs/containers are not tracked, cross-function
//! global stores through reference parameters are not detected, and unknown
//! calls/casts are treated as independent outliving values.

use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use rustc_hash::{FxHashMap, FxHashSet};

/// One escape diagnostic; same shape as `typeck::TypeError`.
#[derive(Debug, Clone)]
pub struct EscapeError {
    pub message: String,
    pub span: Span,
}

/// The borrow origins for one valid returned reference.
///
/// `borrow_params` contains the `&T`/`&mut T` parameters whose referents may
/// keep the returned reference alive. `borrow_param_indices` is the stable
/// declaration-order form for caller-side mapping. An empty vector means the
/// reference is independent of caller-owned borrows (for example, a known
/// global or raw-pointer pointee). Entries are per return statement; callers
/// should group by `function_span` and union the indices. Opaque or
/// unclassified returns produce no entry rather than a false independent claim.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnOrigin {
    pub function: String,
    pub function_span: Span,
    pub span: Span,
    pub borrow_params: Vec<String>,
    pub borrow_param_indices: Vec<usize>,
}

/// Escape-check output, including the origin metadata needed by caller-side
/// lifetime checking.
#[derive(Debug, Clone)]
pub struct EscapeReport {
    pub errors: Vec<EscapeError>,
    pub return_origins: Vec<ReturnOrigin>,
}

/// Where a borrow points: a function-local value (dies with the frame) or
/// something that outlives the function. `Escapable` carries the set of
/// borrow-origin parameter names the referent derives from; an empty set
/// means the referent is truly independent (a global, or the pointee of a
/// raw `T*` — heap/owned). A non-empty set means the returned reference is
/// tied to those borrow params and the caller must keep them alive.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Source {
    Local,
    Escapable {
        origins: FxHashSet<String>,
    },
    /// The old checker permits opaque expressions conservatively, but they
    /// cannot be advertised as independent to caller-side checking.
    Opaque,
}

/// Scope entry: (name, previous source) for shadow restore.
type ScopeEntry = (String, Option<Source>);

struct Checker {
    errors: Vec<EscapeError>,
    return_origins: Vec<ReturnOrigin>,
    current_function: String,
    current_function_span: Span,
    current_param_indices: FxHashMap<String, usize>,
    recorded_return_spans: FxHashSet<(u32, usize, usize)>,
    globals: FxHashSet<String>,
}

pub fn check_program(program: &ast::Program) -> Vec<EscapeError> {
    analyze_program(program).errors
}

pub fn analyze_program(program: &ast::Program) -> EscapeReport {
    let globals = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ast::ItemKind::GlobalVariable(global) => Some(global.name.name.clone()),
            _ => None,
        })
        .collect();
    let mut checker = Checker {
        errors: Vec::new(),
        return_origins: Vec::new(),
        current_function: String::new(),
        current_function_span: Span::default(),
        current_param_indices: FxHashMap::default(),
        recorded_return_spans: FxHashSet::default(),
        globals,
    };
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Function(func) => {
                checker.check_function(
                    &func.name.name,
                    func.name.span,
                    &func.parameters,
                    &func.return_type,
                    &func.body,
                );
            }
            ast::ItemKind::Impl(imp) => {
                for member in &imp.items {
                    match member {
                        ast::ImplItemKind::Function(func) => {
                            checker.check_function(
                                &func.name.name,
                                func.name.span,
                                &func.parameters,
                                &func.return_type,
                                &func.body,
                            );
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            checker.check_function(
                                "<cast>",
                                cast.span,
                                &cast.parameters,
                                &Some(cast.target_type.clone()),
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
    EscapeReport {
        errors: checker.errors,
        return_origins: checker.return_origins,
    }
}

impl Checker {
    fn check_function(
        &mut self,
        function_name: &str,
        function_span: Span,
        parameters: &[ast::Parameter],
        return_type: &Option<ast::Type>,
        body: &ast::Block,
    ) {
        self.current_function.clear();
        self.current_function.push_str(function_name);
        self.current_function_span = function_span;
        self.current_param_indices.clear();
        for (index, param) in parameters.iter().enumerate() {
            self.current_param_indices
                .insert(param.name.name.clone(), index);
        }
        let _ = return_type;
        let mut scopes: Vec<Vec<ScopeEntry>> = vec![Vec::new()];
        let mut ref_sources: FxHashMap<String, Source> = FxHashMap::default();
        let mut ptr_locals: FxHashSet<String> = FxHashSet::default();
        let mut ref_params: FxHashSet<String> = FxHashSet::default();
        for param in parameters {
            match param.param_type.kind.as_ref() {
                ast::TypeKind::Reference(_) => {
                    ref_params.insert(param.name.name.clone());
                    ref_sources.insert(
                        param.name.name.clone(),
                        Source::Escapable {
                            origins: FxHashSet::from_iter([param.name.name.clone()]),
                        },
                    );
                }
                ast::TypeKind::Pointer(_) => {
                    ptr_locals.insert(param.name.name.clone());
                }
                _ => {}
            }
            scopes
                .last_mut()
                .expect("scope stack non-empty")
                .push((param.name.name.clone(), None));
        }
        self.check_block(
            body,
            &mut scopes,
            &mut ref_sources,
            &mut ptr_locals,
            &ref_params,
        );
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(EscapeError {
            message: message.into(),
            span,
        });
    }

    fn pop_scope(
        &mut self,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        ref_sources: &mut FxHashMap<String, Source>,
        ptr_locals: &mut FxHashSet<String>,
    ) {
        if let Some(scope) = scopes.pop() {
            for (name, old) in scope {
                match old {
                    Some(source) => {
                        ref_sources.insert(name.clone(), source.clone());
                        if !matches!(source, Source::Local) {
                            ptr_locals.insert(name);
                        }
                    }
                    None => {
                        ref_sources.remove(&name);
                        ptr_locals.remove(&name);
                    }
                }
            }
        }
    }

    /// Classify the referent of `&expr`.
    fn classify(
        &self,
        expr: &ast::Expression,
        scopes: &[Vec<ScopeEntry>],
        ref_sources: &FxHashMap<String, Source>,
        ptr_locals: &FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) -> Source {
        self.classify_depth(expr, false, scopes, ref_sources, ptr_locals, ref_params)
    }

    fn classify_depth(
        &self,
        expr: &ast::Expression,
        through_fields: bool,
        scopes: &[Vec<ScopeEntry>],
        ref_sources: &FxHashMap<String, Source>,
        ptr_locals: &FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) -> Source {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if ref_params.contains(&ident.name) {
                    Source::Escapable {
                        origins: FxHashSet::from_iter([ident.name.clone()]),
                    }
                } else if let Some(source) = ref_sources.get(&ident.name) {
                    source.clone()
                } else if self.is_local(ident, scopes) {
                    // Access through a pointer-typed local (`&p.x`, `&self.data[i]`)
                    // reaches the pointee (heap/owned, independent), which outlives
                    // the frame; a bare or value-local root borrows the local's own
                    // storage.
                    if through_fields && ptr_locals.contains(&ident.name) {
                        Source::Escapable {
                            origins: FxHashSet::default(),
                        }
                    } else {
                        Source::Local
                    }
                } else if self.globals.contains(&ident.name) {
                    // A known global is independent of caller-owned borrows.
                    Source::Escapable {
                        origins: FxHashSet::default(),
                    }
                } else {
                    // Unknown identifiers are conservatively allowed for
                    // compatibility, but cannot be advertised as independent.
                    Source::Opaque
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. }
            | ast::ExpressionKind::Slice { object, .. } => {
                self.classify_depth(object, true, scopes, ref_sources, ptr_locals, ref_params)
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand,
            } => self.classify_deref(operand, scopes, ref_sources, ptr_locals, ref_params),
            // Heap allocations, casts, temporaries, and call results are
            // unknown here; preserve the old permissive escape behavior, but
            // do not claim they are independent caller-side origins.
            _ => Source::Opaque,
        }
    }

    /// Classify the pointee of `*operand`: a raw-pointer deref outlives the
    /// frame; a reference deref inherits the reference's source.
    fn classify_deref(
        &self,
        operand: &ast::Expression,
        scopes: &[Vec<ScopeEntry>],
        ref_sources: &FxHashMap<String, Source>,
        ptr_locals: &FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) -> Source {
        match operand.kind.as_ref() {
            ast::ExpressionKind::Identifier(ident) => {
                if ref_params.contains(&ident.name) {
                    Source::Escapable {
                        origins: FxHashSet::from_iter([ident.name.clone()]),
                    }
                } else if let Some(source) = ref_sources.get(&ident.name) {
                    source.clone()
                } else {
                    // Raw pointer (or unknown) — pointee outlives the frame,
                    // independent of any borrow param.
                    Source::Escapable {
                        origins: FxHashSet::default(),
                    }
                }
            }
            ast::ExpressionKind::FieldAccess { .. }
            | ast::ExpressionKind::Index { .. }
            | ast::ExpressionKind::Slice { .. } => {
                self.classify_depth(operand, true, scopes, ref_sources, ptr_locals, ref_params)
            }
            ast::ExpressionKind::Unary {
                operator: ast::UnaryOperator::Dereference,
                operand: inner,
            } => self.classify_deref(inner, scopes, ref_sources, ptr_locals, ref_params),
            // An opaque dereference expression cannot be safely assigned a
            // caller-origin identity.
            _ => Source::Opaque,
        }
    }

    fn is_local(&self, ident: &ast::Identifier, scopes: &[Vec<ScopeEntry>]) -> bool {
        scopes
            .iter()
            .rev()
            .any(|scope| scope.iter().any(|(name, _)| name == &ident.name))
    }

    /// The source of a returned/stored reference expression (or None if the
    /// expression is not itself a reference).
    fn reference_source(
        &self,
        expr: &ast::Expression,
        scopes: &[Vec<ScopeEntry>],
        ref_sources: &FxHashMap<String, Source>,
        ptr_locals: &FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) -> Option<Source> {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Reference { expression, .. } => {
                Some(self.classify(expression, scopes, ref_sources, ptr_locals, ref_params))
            }
            ast::ExpressionKind::Identifier(ident) => ref_sources.get(&ident.name).cloned(),
            _ => None,
        }
    }

    // ------------------------------------------------------------------
    // Statements
    // ------------------------------------------------------------------

    fn check_block(
        &mut self,
        block: &ast::Block,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        ref_sources: &mut FxHashMap<String, Source>,
        ptr_locals: &mut FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) {
        scopes.push(Vec::new());
        for stmt in &block.statements {
            self.check_statement(stmt, scopes, ref_sources, ptr_locals, ref_params);
        }
        self.pop_scope(scopes, ref_sources, ptr_locals);
    }

    fn check_statement(
        &mut self,
        stmt: &ast::Statement,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        ref_sources: &mut FxHashMap<String, Source>,
        ptr_locals: &mut FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) {
        match &stmt.kind {
            ast::StatementKind::Block(block) => {
                self.check_block(block, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::StatementKind::Let(let_stmt) => {
                let is_ref_var = let_stmt
                    .type_annotation
                    .as_ref()
                    .is_some_and(|ty| matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)));
                let mut declared_source = None;
                if let Some(init) = &let_stmt.initializer {
                    // Any variable initialized from a borrow (`&x`, or another
                    // reference variable) inherits the referent's source —
                    // even if its declared type is a raw pointer.
                    declared_source =
                        self.reference_source(init, scopes, ref_sources, ptr_locals, ref_params);
                    if declared_source.is_none() && !is_ref_var {
                        self.check_expr(init, scopes, ref_sources, ptr_locals, ref_params);
                    }
                } else if is_ref_var {
                    // Uninitialized reference — conservatively Local so a
                    // later assignment to &x is caught on escape.
                    declared_source = Some(Source::Local);
                }
                if let ast::PatternKind::Identifier(ident) = &let_stmt.pattern.kind {
                    let old = ref_sources.get(&ident.name).cloned();
                    if let Some(source) = declared_source {
                        ref_sources.insert(ident.name.clone(), source);
                        scopes
                            .last_mut()
                            .expect("scope stack non-empty")
                            .push((ident.name.clone(), old));
                    } else {
                        scopes
                            .last_mut()
                            .expect("scope stack non-empty")
                            .push((ident.name.clone(), old));
                    }
                }
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::StatementKind::Return(Some(expr)) => {
                if let Some(source) =
                    self.reference_source(expr, scopes, ref_sources, ptr_locals, ref_params)
                {
                    match source {
                        Source::Local => {
                            self.error(msg::returned_reference_escapes(), expr.span);
                        }
                        Source::Escapable { origins } => {
                            let mut borrow_params: Vec<String> = origins.into_iter().collect();
                            borrow_params.sort();
                            let mut borrow_param_indices: Vec<usize> = borrow_params
                                .iter()
                                .filter_map(|name| self.current_param_indices.get(name).copied())
                                .collect();
                            borrow_param_indices.sort_unstable();
                            let return_key = (expr.span.file, expr.span.start, expr.span.end);
                            if self.recorded_return_spans.insert(return_key) {
                                self.return_origins.push(ReturnOrigin {
                                    function: self.current_function.clone(),
                                    function_span: self.current_function_span,
                                    span: expr.span,
                                    borrow_params,
                                    borrow_param_indices,
                                });
                            }
                        }
                        Source::Opaque => {}
                    }
                }
                if !matches!(expr.kind.as_ref(), ast::ExpressionKind::Reference { .. }) {
                    self.check_expr(expr, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::StatementKind::Return(None)
            | ast::StatementKind::Break(_)
            | ast::StatementKind::Continue => {}
            ast::StatementKind::Defer(inner) => {
                self.check_statement(inner, scopes, ref_sources, ptr_locals, ref_params);
            }
        }
    }

    fn check_expr(
        &mut self,
        expr: &ast::Expression,
        scopes: &mut Vec<Vec<ScopeEntry>>,
        ref_sources: &mut FxHashMap<String, Source>,
        ptr_locals: &mut FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) {
        match expr.kind.as_ref() {
            ast::ExpressionKind::Reference { expression, .. } => {
                // `&x` itself never escapes here — only returns/stores of the
                // resulting reference are checked at their own sites. Recurse
                // into the referent for nested borrows.
                self.check_expr(expression, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == ast::BinaryOperator::Assign {
                    // Global store of a local borrow: `g = &x;` / `g = r;`.
                    if let ast::ExpressionKind::Identifier(target) = left.kind.as_ref()
                        && !self.is_local(target, scopes)
                        && let Some(source) = self.reference_source(
                            right,
                            scopes,
                            ref_sources,
                            ptr_locals,
                            ref_params,
                        )
                        && source == Source::Local
                    {
                        self.error(msg::reference_stored_into_global(&target.name), right.span);
                    }
                    // Propagate reference sources through variable assignment.
                    if let ast::ExpressionKind::Identifier(target) = left.kind.as_ref()
                        && ref_sources.contains_key(&target.name)
                        && let Some(source) = self.reference_source(
                            right,
                            scopes,
                            ref_sources,
                            ptr_locals,
                            ref_params,
                        )
                    {
                        ref_sources.insert(target.name.clone(), source);
                    }
                    self.check_expr(right, scopes, ref_sources, ptr_locals, ref_params);
                } else {
                    self.check_expr(left, scopes, ref_sources, ptr_locals, ref_params);
                    self.check_expr(right, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::Unary { operand, .. }
            | ast::ExpressionKind::Postfix { operand, .. }
            | ast::ExpressionKind::Cast {
                expression: operand,
                ..
            }
            | ast::ExpressionKind::Comptime(operand)
            | ast::ExpressionKind::Move(operand)
            | ast::ExpressionKind::Launch(operand)
            | ast::ExpressionKind::Wait(operand) => {
                self.check_expr(operand, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                arguments,
                ..
            }
            | ast::ExpressionKind::Call {
                function: receiver,
                arguments,
            } => {
                self.check_expr(receiver, scopes, ref_sources, ptr_locals, ref_params);
                for arg in arguments {
                    self.check_expr(arg, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::FieldAccess { object, .. }
            | ast::ExpressionKind::Index { object, .. } => {
                self.check_expr(object, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::Slice {
                object,
                start,
                end,
                step,
            } => {
                self.check_expr(object, scopes, ref_sources, ptr_locals, ref_params);
                if let Some(s) = start {
                    self.check_expr(s, scopes, ref_sources, ptr_locals, ref_params);
                }
                if let Some(e) = end {
                    self.check_expr(e, scopes, ref_sources, ptr_locals, ref_params);
                }
                if let Some(st) = step {
                    self.check_expr(st, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.check_expr(condition, scopes, ref_sources, ptr_locals, ref_params);
                self.check_expr(then_expr, scopes, ref_sources, ptr_locals, ref_params);
                self.check_expr(else_expr, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::UnwrapOr { value, fallback } => {
                self.check_expr(value, scopes, ref_sources, ptr_locals, ref_params);
                self.check_expr(fallback, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(condition, scopes, ref_sources, ptr_locals, ref_params);
                let mut then_scopes = scopes.clone();
                let mut then_sources = ref_sources.clone();
                let mut then_ptrs = ptr_locals.clone();
                self.check_block(
                    then_branch,
                    &mut then_scopes,
                    &mut then_sources,
                    &mut then_ptrs,
                    ref_params,
                );
                let mut else_scopes = scopes.clone();
                let mut else_sources = ref_sources.clone();
                let mut else_ptrs = ptr_locals.clone();
                if let Some(else_branch) = else_branch {
                    self.check_block(
                        else_branch,
                        &mut else_scopes,
                        &mut else_sources,
                        &mut else_ptrs,
                        ref_params,
                    );
                }
            }
            ast::ExpressionKind::While { condition, body }
            | ast::ExpressionKind::ForIn {
                iterable: condition,
                body,
                ..
            }
            | ast::ExpressionKind::For {
                init: _,
                condition,
                increment: _,
                body,
            } => {
                // `condition` holds the iterable for ForIn; check it.
                if matches!(expr.kind.as_ref(), ast::ExpressionKind::ForIn { .. }) {
                    self.check_expr(condition, scopes, ref_sources, ptr_locals, ref_params);
                }
                for _ in 0..4 {
                    let mut body_scopes = scopes.clone();
                    let mut body_sources = ref_sources.clone();
                    let mut body_ptrs = ptr_locals.clone();
                    self.check_block(
                        body,
                        &mut body_scopes,
                        &mut body_sources,
                        &mut body_ptrs,
                        ref_params,
                    );
                    if let ast::ExpressionKind::For { increment, .. } = expr.kind.as_ref() {
                        self.check_expr(
                            increment,
                            &mut body_scopes,
                            &mut body_sources,
                            &mut body_ptrs,
                            ref_params,
                        );
                    }
                }
            }
            ast::ExpressionKind::Match { expression, arms } => {
                self.check_expr(expression, scopes, ref_sources, ptr_locals, ref_params);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard, scopes, ref_sources, ptr_locals, ref_params);
                    }
                    let mut arm_scopes = scopes.clone();
                    let mut arm_sources = ref_sources.clone();
                    let mut arm_ptrs = ptr_locals.clone();
                    self.check_expr(
                        &arm.body,
                        &mut arm_scopes,
                        &mut arm_sources,
                        &mut arm_ptrs,
                        ref_params,
                    );
                }
            }
            ast::ExpressionKind::Block(block) => {
                self.check_block(block, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::ExpressionKind::Initializer { items } => {
                for item in items {
                    match item {
                        ast::InitializerItem::Positional(expr)
                        | ast::InitializerItem::Field { value: expr, .. } => {
                            self.check_expr(expr, scopes, ref_sources, ptr_locals, ref_params);
                        }
                        ast::InitializerItem::Index { index, value } => {
                            self.check_expr(index, scopes, ref_sources, ptr_locals, ref_params);
                            self.check_expr(value, scopes, ref_sources, ptr_locals, ref_params);
                        }
                    }
                }
            }
            ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
                for item in items {
                    self.check_expr(item, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                for field in fields {
                    self.check_expr(&field.value, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::EnumVariant { fields, .. } => {
                for field in fields {
                    self.check_expr(field, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::Asm { inputs, .. } => {
                for input in inputs {
                    self.check_expr(input, scopes, ref_sources, ptr_locals, ref_params);
                }
            }
            ast::ExpressionKind::MacroCall { args, .. } => {
                for arg in args {
                    if let ast::MacroArg::Expression(expr) = arg {
                        self.check_expr(expr, scopes, ref_sources, ptr_locals, ref_params);
                    }
                }
            }
            ast::ExpressionKind::Identifier(_)
            | ast::ExpressionKind::Literal(_)
            | ast::ExpressionKind::TypeName(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn errors(source: &str) -> Vec<String> {
        let program = parse(source);
        check_program(&program)
            .into_iter()
            .map(|e| e.message)
            .collect()
    }
    fn report(source: &str) -> EscapeReport {
        let program = parse(source);
        analyze_program(&program)
    }

    fn parse(source: &str) -> ast::Program {
        let tokens = crate::lexer::lex(source).expect("lex failed");
        let mut parser = crate::parser::Parser::new(tokens.clone());
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn returning_borrow_of_local_errors() {
        let errs = errors("i64* f() { i64 x = 1; return &x; }");
        assert!(
            errs.iter()
                .any(|m| m.contains("does not outlive the function")),
            "expected escape error, got {errs:?}"
        );
    }

    #[test]
    fn returning_borrow_via_pointer_var_errors() {
        let errs = errors("i64* f() { i64 x = 1; i64* r = &x; return r; }");
        assert!(
            errs.iter()
                .any(|m| m.contains("does not outlive the function")),
            "expected escape error, got {errs:?}"
        );
    }

    #[test]
    fn returning_reference_param_is_allowed() {
        let errs = errors("i64* f(&i64 r) { return r; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn returning_global_borrow_is_allowed() {
        let errs = errors("i64 g = 42; i64* f() { return &g; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn returning_through_pointer_param_is_allowed() {
        // &self.data[i] into a heap buffer through a pointer param escapes.
        let errs = errors(
            "struct V { i64* data; }\n\
             i64* get(V* self, i64 i) { return &(self.data[i]); }",
        );
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn local_borrow_used_only_inside_is_allowed() {
        let errs = errors("i64 f() { i64 x = 1; i64* p = &x; return *p; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn global_store_of_local_borrow_errors() {
        let errs = errors("i64* g = (i64*)0; void f() { i64 x = 1; g = &x; }");
        assert!(
            errs.iter().any(|m| m.contains("stored into global")),
            "expected global-store escape error, got {errs:?}"
        );
    }

    #[test]
    fn global_store_of_param_borrow_is_allowed() {
        let errs = errors("i64* g = (i64*)0; void f(&i64 r) { g = r; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }

    #[test]
    fn mut_reference_param_is_allowed() {
        let errs = errors("i64* f(&mut i64 r) { *r = *r + 1; return r; }");
        assert!(errs.is_empty(), "unexpected errors: {errs:?}");
    }
    #[test]
    fn return_origin_tracks_reference_parameter() {
        let report = report("i64* f(&i64 r) { return r; }");
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert_eq!(report.return_origins[0].function, "f");
        assert_eq!(report.return_origins[0].borrow_params, ["r"]);
        assert_eq!(report.return_origins[0].borrow_param_indices, [0]);
    }

    #[test]
    fn return_origin_tracks_the_actual_reference_parameter() {
        let report = report("i64* f(&i64 first, &i64 second) { return second; }");
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert_eq!(report.return_origins[0].borrow_params, ["second"]);
        assert_eq!(report.return_origins[0].borrow_param_indices, [1]);
    }

    #[test]
    fn independent_return_origin_is_empty() {
        let report = report("i64 g = 42; i64* f() { return &g; }");
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert_eq!(report.return_origins[0].borrow_params, Vec::<String>::new());
    }
    #[test]
    fn return_origin_tracks_reference_method_receiver() {
        let report =
            report("struct T { i64 value; } impl T { i64* get(&T self) { return &self.value; } }");
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert_eq!(report.return_origins[0].function, "get");
        assert_eq!(report.return_origins[0].borrow_params, ["self"]);
        assert_eq!(report.return_origins[0].borrow_param_indices, [0]);
    }

    #[test]
    fn return_origin_tracks_mut_reference_method_receiver() {
        let report = report(
            "struct T { i64 value; } impl T { i64* get_mut(&mut T self) { return &self.value; } }",
        );
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert_eq!(report.return_origins[0].function, "get_mut");
        assert_eq!(report.return_origins[0].borrow_params, ["self"]);
        assert_eq!(report.return_origins[0].borrow_param_indices, [0]);
    }

    #[test]
    fn opaque_return_is_not_advertised_as_independent() {
        let report = report("i64* f() { return &make_value(); }");
        assert!(
            report.errors.is_empty(),
            "unexpected errors: {:?}",
            report.errors
        );
        assert!(report.return_origins.is_empty());
    }
}
