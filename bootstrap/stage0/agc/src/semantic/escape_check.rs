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
//! aggregate construction propagates contained borrows, but field projections,
//! cross-function global stores, and opaque calls remain incomplete.

use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use crate::semantic::typeck::{CallSiteKey, CallableIdentity, ResolvedCallable};
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
/// should group by `function_span` and union the indices. Opaque returns carry
/// every reference parameter as a possible origin, so uncertainty cannot erase
/// a known caller loan. Raw-pointer pointees remain outside this proof.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnOrigin {
    pub callable: CallableIdentity,
    pub function: String,
    pub function_span: Span,
    pub arity: usize,
    pub is_method: bool,
    pub is_mutable: bool,
    pub is_opaque: bool,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CallKey {
    name: String,
    arity: usize,
    method: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ReturnSummary {
    Local,
    Escapable { parameter_indices: FxHashSet<usize> },
    Opaque,
}

fn merge_return_summaries(left: ReturnSummary, right: ReturnSummary) -> ReturnSummary {
    match (left, right) {
        (ReturnSummary::Local, _) | (_, ReturnSummary::Local) => ReturnSummary::Local,
        (ReturnSummary::Opaque, _) | (_, ReturnSummary::Opaque) => ReturnSummary::Opaque,
        (
            ReturnSummary::Escapable {
                mut parameter_indices,
            },
            ReturnSummary::Escapable {
                parameter_indices: other,
            },
        ) => {
            parameter_indices.extend(other);
            ReturnSummary::Escapable { parameter_indices }
        }
    }
}

/// Where a borrow points: a function-local value (dies with the frame) or
/// something that outlives the function. `Escapable` carries the set of
/// borrow-origin parameter names the referent derives from; an empty set
/// means the referent is truly independent (a global, or the pointee of a
/// raw `T*` — heap/owned). A non-empty set means the returned reference is
/// tied to those borrow params and the caller must keep them alive.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Source {
    Local {
        depth: usize,
    },
    Escapable {
        origins: FxHashSet<String>,
    },
    /// The old checker permits opaque expressions conservatively, but they
    /// cannot be advertised as independent to caller-side checking.
    Opaque,
}

fn merge_sources(left: Option<Source>, right: Option<Source>) -> Option<Source> {
    match (left, right) {
        (None, source) | (source, None) => source,
        (Some(Source::Local { depth: left }), Some(Source::Local { depth: right })) => {
            Some(Source::Local {
                depth: left.max(right),
            })
        }
        (Some(local @ Source::Local { .. }), _) | (_, Some(local @ Source::Local { .. })) => {
            Some(local)
        }
        (Some(Source::Opaque), _) | (_, Some(Source::Opaque)) => Some(Source::Opaque),
        (Some(Source::Escapable { mut origins }), Some(Source::Escapable { origins: other })) => {
            origins.extend(other);
            Some(Source::Escapable { origins })
        }
    }
}

/// Scope entry: (name, previous source) for shadow restore.
type ScopeEntry = (String, Option<Source>);

struct Checker<'a> {
    errors: Vec<EscapeError>,
    return_origins: Vec<ReturnOrigin>,
    return_summaries: FxHashMap<CallKey, ReturnSummary>,
    resolved_return_summaries: FxHashMap<CallableIdentity, ReturnSummary>,
    resolved_callables: &'a FxHashMap<CallSiteKey, ResolvedCallable>,
    defined_callables: FxHashSet<CallableIdentity>,
    current_return_summary: Option<Source>,
    collect_return_origins: bool,
    current_function: String,
    current_function_span: Span,
    current_function_arity: usize,
    current_function_is_method: bool,
    current_return_is_mutable: bool,
    current_param_indices: FxHashMap<String, usize>,
    current_ref_param_indices: FxHashSet<usize>,
    recorded_return_spans: FxHashSet<(u32, usize, usize)>,
    globals: FxHashSet<String>,
}

pub fn check_program(program: &ast::Program) -> Vec<EscapeError> {
    analyze_program(program).errors
}

pub fn analyze_program(program: &ast::Program) -> EscapeReport {
    analyze_program_with_callables(program, &FxHashMap::default())
}

pub fn analyze_program_with_callables(
    program: &ast::Program,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
) -> EscapeReport {
    let globals = program
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ast::ItemKind::GlobalVariable(global) => Some(global.name.name.clone()),
            _ => None,
        })
        .collect();
    let defined_callables = program
        .items
        .iter()
        .flat_map(|item| match &item.kind {
            ast::ItemKind::Function(function) => vec![CallableIdentity::from_decl_span(
                function.name.span,
                false,
            )],
            ast::ItemKind::Impl(implementation) => implementation
                .items
                .iter()
                .filter_map(|member| match member {
                    ast::ImplItemKind::Function(function) => Some(
                        CallableIdentity::from_decl_span(function.name.span, true),
                    ),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        })
        .collect();
    let mut checker = Checker {
        errors: Vec::new(),
        return_origins: Vec::new(),
        return_summaries: FxHashMap::default(),
        resolved_return_summaries: FxHashMap::default(),
        resolved_callables,
        defined_callables,
        current_return_summary: None,
        collect_return_origins: false,
        current_function: String::new(),
        current_function_span: Span::default(),
        current_function_arity: 0,
        current_function_is_method: false,
        current_return_is_mutable: false,
        current_param_indices: FxHashMap::default(),
        current_ref_param_indices: FxHashSet::default(),
        recorded_return_spans: FxHashSet::default(),
        globals,
    };
    checker.seed_extern_return_summaries(program);

    // Call summaries are monotone: each pass can add an origin or make a
    // result more conservative. Iterate to a fixed point for forward calls
    // and recursive forwarding before emitting diagnostics and metadata.
    loop {
        checker.errors.clear();
        checker.return_origins.clear();
        checker.recorded_return_spans.clear();
        let old_summaries = checker.return_summaries.clone();
        let old_resolved_summaries = checker.resolved_return_summaries.clone();
        checker.check_program_functions(program);
        if checker.return_summaries == old_summaries
            && checker.resolved_return_summaries == old_resolved_summaries
        {
            break;
        }
    }

    checker.errors.clear();
    checker.return_origins.clear();
    checker.recorded_return_spans.clear();
    checker.collect_return_origins = true;
    checker.check_program_functions(program);
    checker.collect_extern_return_origins(program);
    EscapeReport {
        errors: checker.errors,
        return_origins: checker.return_origins,
    }
}

impl Checker<'_> {
    fn has_reference_return(return_type: Option<&ast::Type>) -> bool {
        return_type.is_some_and(|ty| matches!(ty.kind.as_ref(), ast::TypeKind::Reference(_)))
    }

    fn seed_extern_return_summaries(&mut self, program: &ast::Program) {
        for item in &program.items {
            let mut add = |name: &str,
                           function_span: Span,
                           arity: usize,
                           return_type: Option<&ast::Type>| {
                if Self::has_reference_return(return_type) {
                    let summary = ReturnSummary::Opaque;
                    self.return_summaries.insert(CallKey {
                            name: name.to_string(),
                            arity,
                            method: false,
                        }, summary.clone());
                    self.resolved_return_summaries.insert(
                        CallableIdentity::from_decl_span(function_span, false),
                        // External implementations have no body from which to
                        // derive origins, so callers must retain uncertainty.
                        summary,
                    );
                }
            };
            match &item.kind {
                ast::ItemKind::ExternFunction(function) => add(
                    &function.name.name,
                    function.name.span,
                    function.signature.parameters.len(),
                    function.signature.return_type.as_ref(),
                ),
                ast::ItemKind::ExternBlock(block) => {
                    for function in &block.functions {
                        add(
                            &function.name.name,
                            function.name.span,
                            function.signature.parameters.len(),
                            function.signature.return_type.as_ref(),
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_extern_return_origins(&mut self, program: &ast::Program) {
        for item in &program.items {
            let mut add = |function: &ast::ExternFunctionItem| {
                let Some(return_type) = function.signature.return_type.as_ref() else {
                    return;
                };
                let ast::TypeKind::Reference(reference) = return_type.kind.as_ref() else {
                    return;
                };
                let borrow_params: Vec<_> = function
                    .signature
                    .parameters
                    .iter()
                    .filter(|parameter| {
                        matches!(parameter.param_type.kind.as_ref(), ast::TypeKind::Reference(_))
                    })
                    .map(|parameter| parameter.name.name.clone())
                    .collect();
                let borrow_param_indices = function
                    .signature
                    .parameters
                    .iter()
                    .enumerate()
                    .filter_map(|(index, parameter)| {
                        matches!(parameter.param_type.kind.as_ref(), ast::TypeKind::Reference(_))
                            .then_some(index)
                    })
                    .collect();
                self.return_origins.push(ReturnOrigin {
                    callable: CallableIdentity::from_decl_span(function.name.span, false),
                    function: function.name.name.clone(),
                    function_span: function.name.span,
                    arity: function.signature.parameters.len(),
                    is_method: false,
                    is_mutable: reference.is_mutable,
                    is_opaque: true,
                    span: function.name.span,
                    borrow_params,
                    borrow_param_indices,
                });
            };
            match &item.kind {
                ast::ItemKind::ExternFunction(function) => add(function),
                ast::ItemKind::ExternBlock(block) => {
                    for function in &block.functions {
                        add(function);
                    }
                }
                _ => {}
            }
        }
    }

    fn check_program_functions(&mut self, program: &ast::Program) {
        for item in &program.items {
            match &item.kind {
                ast::ItemKind::Function(func) => self.check_function(
                    &func.name.name,
                    func.name.span,
                    &func.parameters,
                    &func.return_type,
                    &func.body,
                    false,
                ),
                ast::ItemKind::Impl(imp) => {
                    for member in &imp.items {
                        match member {
                            ast::ImplItemKind::Function(func) => self.check_function(
                                &func.name.name,
                                func.name.span,
                                &func.parameters,
                                &func.return_type,
                                &func.body,
                                true,
                            ),
                            ast::ImplItemKind::Cast(cast) => self.check_function(
                                "<cast>",
                                cast.span,
                                &cast.parameters,
                                &Some(cast.target_type.clone()),
                                &cast.body,
                                false,
                            ),
                            ast::ImplItemKind::AssociatedType(_) => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn check_function(
        &mut self,
        function_name: &str,
        function_span: Span,
        parameters: &[ast::Parameter],
        return_type: &Option<ast::Type>,
        body: &ast::Block,
        is_method: bool,
    ) {
        self.current_function.clear();
        self.current_function.push_str(function_name);
        self.current_function_span = function_span;
        self.current_function_arity = parameters.len();
        self.current_function_is_method = is_method;
        self.current_return_is_mutable = return_type.as_ref().is_some_and(|ty| {
            matches!(ty.kind.as_ref(), ast::TypeKind::Reference(reference) if reference.is_mutable)
        });
        self.current_return_summary = None;
        self.current_param_indices.clear();
        self.current_ref_param_indices.clear();
        for (index, param) in parameters.iter().enumerate() {
            self.current_param_indices
                .insert(param.name.name.clone(), index);
            if matches!(param.param_type.kind.as_ref(), ast::TypeKind::Reference(_)) {
                self.current_ref_param_indices.insert(index);
            }
        }
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
        if let Some(source) = self.current_return_summary.take() {
            let summary = match source {
                Source::Local { .. } => ReturnSummary::Local,
                Source::Opaque => ReturnSummary::Opaque,
                Source::Escapable { origins } => {
                    let mut parameter_indices = FxHashSet::default();
                    for origin in origins {
                        let Some(index) = self.current_param_indices.get(&origin) else {
                            parameter_indices.clear();
                            parameter_indices.insert(usize::MAX);
                            break;
                        };
                        parameter_indices.insert(*index);
                    }
                    if parameter_indices.contains(&usize::MAX) {
                        ReturnSummary::Opaque
                    } else {
                        ReturnSummary::Escapable { parameter_indices }
                    }
                }
            };
            let key = CallKey {
                name: function_name.to_string(),
                arity: parameters.len(),
                method: is_method,
            };
            self.return_summaries
                .entry(key)
                .and_modify(|existing| {
                    *existing = merge_return_summaries(existing.clone(), summary.clone());
                })
                .or_insert(summary.clone());
            let identity = CallableIdentity::from_decl_span(function_span, is_method);
            self.resolved_return_summaries
                .entry(identity)
                .and_modify(|existing| {
                    *existing = merge_return_summaries(existing.clone(), summary.clone());
                })
                .or_insert(summary);
        }
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
    ) {
        if let Some(scope) = scopes.pop() {
            for (name, old) in scope {
                match old {
                    Some(source) => {
                        ref_sources.insert(name, source);
                    }
                    None => {
                        ref_sources.remove(&name);
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
                if ref_params.contains(&ident.name) && self.local_scope(ident, scopes) == Some(0) {
                    Source::Escapable {
                        origins: FxHashSet::from_iter([ident.name.clone()]),
                    }
                } else if ptr_locals.contains(&ident.name) {
                    if through_fields {
                        Source::Escapable {
                            origins: FxHashSet::default(),
                        }
                    } else if let Some(depth) = self.local_scope(ident, scopes) {
                        Source::Local { depth }
                    } else {
                        Source::Opaque
                    }
                } else if let Some(source) = ref_sources.get(&ident.name) {
                    source.clone()
                } else if let Some(depth) = self.local_scope(ident, scopes) {
                    Source::Local { depth }
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
                if ptr_locals.contains(&ident.name) {
                    Source::Escapable {
                        origins: FxHashSet::default(),
                    }
                } else if ref_params.contains(&ident.name)
                    && self.local_scope(ident, scopes) == Some(0)
                {
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

    fn local_scope(&self, ident: &ast::Identifier, scopes: &[Vec<ScopeEntry>]) -> Option<usize> {
        scopes.iter().enumerate().rev().find_map(|(depth, scope)| {
            scope
                .iter()
                .any(|(name, _)| name == &ident.name)
                .then_some(depth)
        })
    }

    fn call_return_source(
        &self,
        key: &CallKey,
        call_span: Span,
        arguments: &[&ast::Expression],
        method_receiver: bool,
        scopes: &[Vec<ScopeEntry>],
        ref_sources: &FxHashMap<String, Source>,
        ptr_locals: &FxHashSet<String>,
        ref_params: &FxHashSet<String>,
    ) -> Option<Source> {
        let call_site = (call_span.file, call_span.start, call_span.end, call_span.expansion);
        let summary = match self.resolved_callables.get(&call_site) {
            Some(resolved) => self
                .resolved_return_summaries
                .get(&resolved.identity)
                .cloned()
                .or_else(|| {
                    (!self.defined_callables.contains(&resolved.identity)
                        && resolved.returns_reference)
                    .then_some(ReturnSummary::Opaque)
                })?,
            None => self.return_summaries.get(key)?.clone(),
        };
        match &summary {
            ReturnSummary::Local => Some(Source::Local { depth: usize::MAX }),
            ReturnSummary::Opaque => {
                let Some(resolved) = self.resolved_callables.get(&call_site) else {
                    return Some(Source::Opaque);
                };
                let mut result = None;
                for index in &resolved.reference_parameters {
                    let Some(argument) = arguments.get(*index) else {
                        return Some(Source::Opaque);
                    };
                    let source = self.reference_source(
                        argument,
                        scopes,
                        ref_sources,
                        ptr_locals,
                        ref_params,
                    );
                    result = merge_sources(result, source);
                }
                Some(result.unwrap_or(Source::Opaque))
            }
            ReturnSummary::Escapable { parameter_indices } => {
                let mut result = None;
                for index in parameter_indices {
                    let Some(argument) = arguments.get(*index) else {
                        return Some(Source::Opaque);
                    };
                    let source = self.reference_source(
                        argument,
                        scopes,
                        ref_sources,
                        ptr_locals,
                        ref_params,
                    );
                    let source = source.or_else(|| {
                        if !method_receiver {
                            return Some(Source::Opaque);
                        }
                        if let ast::ExpressionKind::Identifier(ident) = argument.kind.as_ref()
                            && ptr_locals.contains(&ident.name)
                        {
                            return Some(Source::Opaque);
                        }
                        Some(self.classify(argument, scopes, ref_sources, ptr_locals, ref_params))
                    });
                    result = merge_sources(result, source);
                }
                Some(result.unwrap_or(Source::Escapable {
                    origins: FxHashSet::default(),
                }))
            }
        }
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
            ast::ExpressionKind::Move(value) => {
                self.reference_source(value, scopes, ref_sources, ptr_locals, ref_params)
            }
            ast::ExpressionKind::StructLiteral { fields, .. } => {
                fields.iter().fold(None, |source, field| {
                    merge_sources(
                        source,
                        self.reference_source(
                            &field.value,
                            scopes,
                            ref_sources,
                            ptr_locals,
                            ref_params,
                        ),
                    )
                })
            }
            ast::ExpressionKind::Tuple(values) | ast::ExpressionKind::Array(values) => {
                values.iter().fold(None, |source, value| {
                    merge_sources(
                        source,
                        self.reference_source(value, scopes, ref_sources, ptr_locals, ref_params),
                    )
                })
            }
            ast::ExpressionKind::Initializer { items } => {
                items.iter().fold(None, |source, item| {
                    let value = match item {
                        ast::InitializerItem::Positional(value)
                        | ast::InitializerItem::Field { value, .. }
                        | ast::InitializerItem::Index { value, .. } => value,
                    };
                    merge_sources(
                        source,
                        self.reference_source(value, scopes, ref_sources, ptr_locals, ref_params),
                    )
                })
            }
            ast::ExpressionKind::Reference { expression, .. } => {
                Some(self.classify(expression, scopes, ref_sources, ptr_locals, ref_params))
            }
            ast::ExpressionKind::Cast {
                expression,
                target_type,
            } if matches!(
                target_type.kind.as_ref(),
                ast::TypeKind::Pointer(_)
                    | ast::TypeKind::Reference(_)
                    | ast::TypeKind::Primitive(ast::PrimitiveType::Str)
            ) =>
            {
                self.reference_source(expression, scopes, ref_sources, ptr_locals, ref_params)
            }
            ast::ExpressionKind::Identifier(ident) => ref_sources.get(&ident.name).cloned(),
            ast::ExpressionKind::Call {
                function,
                arguments,
            } => {
                let ast::ExpressionKind::Identifier(name) = function.kind.as_ref() else {
                    return None;
                };
                let arguments = arguments.iter().collect::<Vec<_>>();
                self.call_return_source(
                    &CallKey {
                        name: name.name.clone(),
                        arity: arguments.len(),
                        method: false,
                    },
                    expr.span,
                    &arguments,
                    false,
                    scopes,
                    ref_sources,
                    ptr_locals,
                    ref_params,
                )
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } => {
                let mut call_arguments = Vec::with_capacity(arguments.len() + 1);
                call_arguments.push(receiver.as_ref());
                call_arguments.extend(arguments.iter());
                self.call_return_source(
                    &CallKey {
                        name: method.name.clone(),
                        arity: call_arguments.len(),
                        method: true,
                    },
                    expr.span,
                    &call_arguments,
                    true,
                    scopes,
                    ref_sources,
                    ptr_locals,
                    ref_params,
                )
            }
            ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => {
                let mut block_scopes = scopes.to_vec();
                let mut block_sources = ref_sources.clone();
                let mut block_ptrs = ptr_locals.clone();
                block_scopes.push(Vec::new());
                for (index, statement) in block.statements.iter().enumerate() {
                    match &statement.kind {
                        ast::StatementKind::Let(let_stmt) => {
                            let source = let_stmt.initializer.as_ref().and_then(|init| {
                                self.reference_source(
                                    init,
                                    &block_scopes,
                                    &block_sources,
                                    &block_ptrs,
                                    ref_params,
                                )
                            });
                            Self::record_pattern_refs(
                                &let_stmt.pattern,
                                source,
                                &mut block_scopes,
                                &mut block_sources,
                            );
                            Self::record_pointer_binding(let_stmt, &mut block_ptrs);
                        }
                        ast::StatementKind::Expression(value) => {
                            if index + 1 == block.statements.len() {
                                return self.reference_source(
                                    value,
                                    &block_scopes,
                                    &block_sources,
                                    &block_ptrs,
                                    ref_params,
                                );
                            }
                            if let ast::ExpressionKind::Binary {
                                left,
                                operator,
                                right,
                            } = value.kind.as_ref()
                                && *operator == ast::BinaryOperator::Assign
                                && let ast::ExpressionKind::Identifier(target) = left.kind.as_ref()
                                && block_sources.contains_key(&target.name)
                            {
                                let source = self
                                    .reference_source(
                                        right,
                                        &block_scopes,
                                        &block_sources,
                                        &block_ptrs,
                                        ref_params,
                                    )
                                    .unwrap_or(Source::Opaque);
                                block_sources.insert(target.name.clone(), source);
                            }
                        }
                        _ => {}
                    }
                }
                None
            }
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
        let previous_ptrs = ptr_locals.clone();
        scopes.push(Vec::new());
        for stmt in &block.statements {
            self.check_statement(stmt, scopes, ref_sources, ptr_locals, ref_params);
        }
        self.pop_scope(scopes, ref_sources);
        *ptr_locals = previous_ptrs;
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
                    if (declared_source.is_none() && !is_ref_var)
                        || matches!(init.kind.as_ref(), ast::ExpressionKind::Block(_))
                    {
                        self.check_expr(init, scopes, ref_sources, ptr_locals, ref_params);
                    }
                } else if is_ref_var {
                    // Uninitialized reference — conservatively Local so a
                    // later assignment to &x is caught on escape.
                    declared_source = Some(Source::Local {
                        depth: scopes.len() - 1,
                    });
                }
                if let (Some(Source::Local { depth }), Some(init)) =
                    (&declared_source, &let_stmt.initializer)
                    && *depth > scopes.len() - 1
                {
                    self.error(msg::reference_escapes_scope(), init.span);
                }
                Self::record_pattern_refs(&let_stmt.pattern, declared_source, scopes, ref_sources);
                Self::record_pointer_binding(let_stmt, ptr_locals);
            }
            ast::StatementKind::Expression(expr) => {
                self.check_expr(expr, scopes, ref_sources, ptr_locals, ref_params);
            }
            ast::StatementKind::Return(Some(expr)) => {
                if let Some(source) =
                    self.reference_source(expr, scopes, ref_sources, ptr_locals, ref_params)
                {
                    self.current_return_summary =
                        merge_sources(self.current_return_summary.take(), Some(source.clone()));
                    match source {
                        Source::Local { .. } => {
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
                            if self.collect_return_origins {
                                let return_key = (expr.span.file, expr.span.start, expr.span.end);
                                if self.recorded_return_spans.insert(return_key) {
                                    self.return_origins.push(ReturnOrigin {
                                        callable: CallableIdentity::from_decl_span(
                                            self.current_function_span,
                                            self.current_function_is_method,
                                        ),
                                        function: self.current_function.clone(),
                                        function_span: self.current_function_span,
                                        arity: self.current_function_arity,
                                        is_method: self.current_function_is_method,
                                        is_mutable: self.current_return_is_mutable,
                                        is_opaque: false,
                                        span: expr.span,
                                        borrow_params,
                                        borrow_param_indices,
                                    });
                                }
                            }
                        }
                        Source::Opaque => {
                            if self.collect_return_origins {
                                let mut borrow_params: Vec<_> = self
                                    .current_param_indices
                                    .iter()
                                    .filter_map(|(name, index)| {
                                        self.current_ref_param_indices
                                            .contains(index)
                                            .then_some(name.clone())
                                    })
                                    .collect();
                                borrow_params.sort();
                                let mut borrow_param_indices: Vec<_> =
                                    self.current_ref_param_indices.iter().copied().collect();
                                borrow_param_indices.sort_unstable();
                                let return_key = (expr.span.file, expr.span.start, expr.span.end);
                                if self.recorded_return_spans.insert(return_key) {
                                    self.return_origins.push(ReturnOrigin {
                                        callable: CallableIdentity::from_decl_span(
                                            self.current_function_span,
                                            self.current_function_is_method,
                                        ),
                                        function: self.current_function.clone(),
                                        function_span: self.current_function_span,
                                        arity: self.current_function_arity,
                                        is_method: self.current_function_is_method,
                                        is_mutable: self.current_return_is_mutable,
                                        is_opaque: true,
                                        span: expr.span,
                                        borrow_params,
                                        borrow_param_indices,
                                    });
                                }
                            }
                        }
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

    fn record_pointer_binding(let_stmt: &ast::LetStatement, ptr_locals: &mut FxHashSet<String>) {
        if let ast::PatternKind::Identifier(ident) = &let_stmt.pattern.kind {
            if let_stmt.type_annotation.as_ref().is_some_and(|ty| {
                matches!(ty.kind.as_ref(), ast::TypeKind::Pointer(_))
            }) {
                ptr_locals.insert(ident.name.clone());
            } else {
                ptr_locals.remove(&ident.name);
            }
        }
    }

    fn record_pattern_refs(
        pattern: &ast::Pattern,
        declared_source: Option<Source>,
        scopes: &mut [Vec<ScopeEntry>],
        ref_sources: &mut FxHashMap<String, Source>,
    ) {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident) => {
                let old = ref_sources.get(&ident.name).cloned();
                if let Some(source) = declared_source {
                    ref_sources.insert(ident.name.clone(), source);
                } else {
                    ref_sources.remove(&ident.name);
                }
                scopes
                    .last_mut()
                    .expect("scope stack non-empty")
                    .push((ident.name.clone(), old));
            }
            ast::PatternKind::Tuple(items) => {
                for item in items {
                    Self::record_pattern_refs(item, declared_source.clone(), scopes, ref_sources);
                }
            }
            _ => {}
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
                    if let ast::ExpressionKind::Identifier(target) = left.kind.as_ref()
                        && let Some(source) = self.reference_source(
                            right,
                            scopes,
                            ref_sources,
                            ptr_locals,
                            ref_params,
                        )
                    {
                        if self.local_scope(target, scopes).is_none()
                            && matches!(source, Source::Local { .. })
                        {
                            self.error(msg::reference_stored_into_global(&target.name), right.span);
                        }
                        if ref_sources.contains_key(&target.name)
                            && !ptr_locals.contains(&target.name)
                        {
                            if let Some(target_depth) = self.local_scope(target, scopes)
                                && matches!(source, Source::Local { depth } if depth > target_depth)
                            {
                                self.error(msg::reference_escapes_scope(), right.span);
                            }
                            ref_sources.insert(target.name.clone(), source);
                        }
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
            ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => {
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
