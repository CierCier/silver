//! Placement and pinning rules for scoped launches.
//!
//! A launch whose callee takes a `&T`/`&mut T` parameter borrows from the
//! launching scope, so the task must be joined before that scope ends. The
//! `Task` drop glue performs the join; this pass keeps the handle where that
//! glue is guaranteed to run in time:
//!
//! - the launch must initialize a block local (`Task<T> t = launch f(&x);`),
//!   not a `for` initializer, or be the direct operand of `wait`. A local declared by such an initializer is
//!   always dropped before the locals it borrows, which were declared earlier.
//! - that local (the holder) may only be consumed by `wait`, or read by
//!   `started()`. Any other use could move the handle to a place that
//!   outlives the borrow.
//! - the launch may instead be pushed into a local collection
//!   (`tasks.push(launch f(&x));`) declared after everything it borrows, so
//!   the collection's drop joins its tasks first. Its tasks may run at once,
//!   so they borrow only with `&`, and the collection is used only through
//!   `push`, `len`, `is_empty`, `clear`, `drop`, or `wait` on `pop`, `take`,
//!   `swap_remove` or `get`.
//!
//! Holders are matched by name within one function, so a second local with a
//! holder's name is pinned too. Loans on the borrowed places are tracked by
//! `borrow_check`.

use crate::diagnostics::messages as msg;
use crate::lexer::Span;
use crate::parser::ast;
use crate::parser::visit;
use crate::semantic::typeck::{CallSiteKey, ResolvedCallable};
use rustc_hash::{FxHashMap, FxHashSet};

/// One placement or pinning diagnostic.
#[derive(Debug, Clone)]
pub struct ScopedLaunchError {
    pub message: String,
    pub span: Span,
    /// The launch a pinned task came from.
    pub launched_at: Option<Span>,
}

type SpanKey = (u32, usize, usize, u32);

fn span_key(span: Span) -> SpanKey {
    (span.file, span.start, span.end, span.expansion)
}

/// The variable a scoped launch borrows first, for diagnostics: the root of
/// its first `&place` argument, or a forwarded reference variable.
fn borrowed_name(launch: &ast::Expression) -> String {
    let ast::ExpressionKind::Launch(call) = launch.kind.as_ref() else {
        return String::new();
    };
    let ast::ExpressionKind::Call { arguments, .. } = call.kind.as_ref() else {
        return String::new();
    };
    for argument in arguments {
        let mut place = match argument.kind.as_ref() {
            ast::ExpressionKind::Reference { expression, .. } => expression,
            _ => argument,
        };
        while let ast::ExpressionKind::FieldAccess { object, .. } = place.kind.as_ref() {
            place = object;
        }
        if let ast::ExpressionKind::Identifier(identifier) = place.kind.as_ref() {
            return identifier.name.clone();
        }
    }
    "its argument".to_string()
}

/// `true` when `expr` is a launch of a callee with a reference parameter.
pub(crate) fn is_scoped_launch(
    expr: &ast::Expression,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
) -> bool {
    let ast::ExpressionKind::Launch(call) = expr.kind.as_ref() else {
        return false;
    };
    resolved_callables
        .get(&span_key(call.span))
        .is_some_and(|callable| !callable.reference_parameters.is_empty())
}

struct FunctionFacts<'a> {
    resolved_callables: &'a FxHashMap<CallSiteKey, ResolvedCallable>,
    /// Locals initialized by a scoped launch, with that launch's span and
    /// the variable it borrows.
    holders: FxHashMap<String, (Span, String)>,
    /// Scoped launches in an allowed position.
    placed_launches: FxHashSet<SpanKey>,
    /// Identifiers a holder may appear as: the operand of `wait`, or the
    /// receiver of `started()`.
    allowed_uses: FxHashSet<SpanKey>,
    /// Locals a scoped launch was pushed into, with that launch's span and
    /// the variable it borrows.
    collection_holders: FxHashMap<String, (Span, String)>,
    /// Identifiers a collection holder may appear as (see the module docs).
    collection_uses: FxHashSet<SpanKey>,
    /// Where each local is declared, by span start.
    declared_at: FxHashMap<String, usize>,
    /// Parameters and locals declared as references, with whether each is
    /// `&mut T`.
    references: FxHashMap<String, bool>,
}

/// Methods a collection holder may be called with directly.
const COLLECTION_METHODS: &[&str] = &["push", "len", "is_empty", "clear", "drop"];
/// Methods that take a task out of a collection holder; only under `wait`.
const COLLECTION_WAIT_METHODS: &[&str] = &["pop", "take", "swap_remove", "get"];

/// The single launch argument of `holder.push(launch ...)`, with the holder.
fn pushed_launch(expression: &ast::Expression) -> Option<(&ast::Identifier, &ast::Expression)> {
    let ast::ExpressionKind::MethodCall {
        receiver,
        method,
        arguments,
    } = expression.kind.as_ref()
    else {
        return None;
    };
    let ast::ExpressionKind::Identifier(holder) = receiver.kind.as_ref() else {
        return None;
    };
    match arguments.as_slice() {
        [launch] if method.name == "push"
            && matches!(launch.kind.as_ref(), ast::ExpressionKind::Launch(_)) =>
        {
            Some((holder, launch))
        }
        _ => None,
    }
}

/// Root variables a launch borrows, each with whether the borrow is mutable:
/// the root of each `&place`/`&mut place` argument (through fields and
/// indexes), and each reference variable forwarded to a reference
/// parameter. A forwarded reference was declared after what it borrows, so
/// ordering against it is enough.
fn borrowed_roots(
    launch: &ast::Expression,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
    references: &FxHashMap<String, bool>,
) -> Vec<(String, bool)> {
    let mut roots = Vec::new();
    let ast::ExpressionKind::Launch(call) = launch.kind.as_ref() else {
        return roots;
    };
    let ast::ExpressionKind::Call { arguments, .. } = call.kind.as_ref() else {
        return roots;
    };
    let reference_parameters = resolved_callables
        .get(&span_key(call.span))
        .map(|callable| callable.reference_parameters.as_slice())
        .unwrap_or_default();
    for (index, argument) in arguments.iter().enumerate() {
        let (mut place, mutable) = match argument.kind.as_ref() {
            ast::ExpressionKind::Reference {
                is_mutable,
                expression,
            } => (expression.as_ref(), *is_mutable),
            ast::ExpressionKind::Identifier(forwarded)
                if reference_parameters.contains(&index) =>
            {
                (argument, references.get(&forwarded.name).copied().unwrap_or(false))
            }
            _ => continue,
        };
        while let ast::ExpressionKind::FieldAccess { object, .. }
        | ast::ExpressionKind::Index { object, .. } = place.kind.as_ref()
        {
            place = object;
        }
        if let ast::ExpressionKind::Identifier(root) = place.kind.as_ref() {
            roots.push((root.name.clone(), mutable));
        }
    }
    roots
}

impl FunctionFacts<'_> {
    fn record_block(&mut self, block: &ast::Block) {
        for statement in &block.statements {
            self.record_statement(statement);
        }
    }

    fn record_statement(&mut self, statement: &ast::Statement) {
        match &statement.kind {
            ast::StatementKind::Let(binding) => self.record_binding(binding),
            ast::StatementKind::Block(block) => self.record_block(block),
            ast::StatementKind::Defer(inner) => self.record_statement(inner),
            _ => {}
        }
    }

    fn record_binding(&mut self, binding: &ast::LetStatement) {
        self.record_declaration(binding);
        if let Some(initializer) = &binding.initializer
            && let ast::PatternKind::Identifier(name) = &binding.pattern.kind
            && is_scoped_launch(initializer, self.resolved_callables)
        {
            self.holders.insert(
                name.name.clone(),
                (initializer.span, borrowed_name(initializer)),
            );
            self.placed_launches.insert(span_key(initializer.span));
        }
    }

    /// Records where `binding` declares its name and whether it is a reference.
    fn record_declaration(&mut self, binding: &ast::LetStatement) {
        if let ast::PatternKind::Identifier(name) = &binding.pattern.kind {
            self.declared_at.insert(name.name.clone(), name.span.start);
            match binding.type_annotation.as_ref().map(|ty| ty.kind.as_ref()) {
                Some(ast::TypeKind::Reference(reference)) => {
                    self.references.insert(name.name.clone(), reference.is_mutable);
                }
                _ => {
                    self.references.remove(&name.name);
                }
            }
        }
    }

    /// Records the statements of blocks owned by `expression`, and `wait`
    /// operands. The visitor reaches nested expressions on its own.
    fn record_expression(&mut self, expression: &ast::Expression) {
        match expression.kind.as_ref() {
            ast::ExpressionKind::Lambda { body, .. }
            | ast::ExpressionKind::Block(body)
            | ast::ExpressionKind::While { body, .. }
            | ast::ExpressionKind::ForIn { body, .. } => self.record_block(body),
            ast::ExpressionKind::For { init, body, .. } => {
                // A `for` initializer cannot hold a borrowing launch: the
                // borrow checker tracks task loans only for block locals.
                self.record_declaration(init);
                self.record_block(body);
            }
            ast::ExpressionKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.record_block(then_branch);
                if let Some(body) = else_branch {
                    self.record_block(body);
                }
            }
            ast::ExpressionKind::Wait(operand) => {
                if matches!(operand.kind.as_ref(), ast::ExpressionKind::Identifier(_)) {
                    self.allowed_uses.insert(span_key(operand.span));
                } else if is_scoped_launch(operand, self.resolved_callables) {
                    self.placed_launches.insert(span_key(operand.span));
                } else if let ast::ExpressionKind::MethodCall {
                    receiver, method, ..
                } = operand.kind.as_ref()
                    && COLLECTION_WAIT_METHODS.contains(&method.name.as_str())
                    && matches!(receiver.kind.as_ref(), ast::ExpressionKind::Identifier(_))
                {
                    self.collection_uses.insert(span_key(receiver.span));
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver, method, ..
            } if COLLECTION_METHODS.contains(&method.name.as_str())
                && matches!(receiver.kind.as_ref(), ast::ExpressionKind::Identifier(_)) =>
            {
                self.collection_uses.insert(span_key(receiver.span));
                if let Some((holder, launch)) = pushed_launch(expression)
                    && is_scoped_launch(launch, self.resolved_callables)
                {
                    self.placed_launches.insert(span_key(launch.span));
                    self.collection_holders
                        .entry(holder.name.clone())
                        .or_insert((launch.span, borrowed_name(launch)));
                }
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
            } if method.name == "started"
                && arguments.is_empty()
                && matches!(receiver.kind.as_ref(), ast::ExpressionKind::Identifier(_)) =>
            {
                self.allowed_uses.insert(span_key(receiver.span));
            }
            _ => {}
        }
    }
}

/// Rejects an argument for a reference parameter of `launch` that is not
/// `&place`, `&mut place` or a reference variable: a bare local would be
/// borrowed implicitly, and a call result's borrow is not tracked.
fn check_borrowed_arguments(
    launch: &ast::Expression,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
    references: &FxHashMap<String, bool>,
    errors: &mut Vec<ScopedLaunchError>,
) {
    let ast::ExpressionKind::Launch(call) = launch.kind.as_ref() else {
        return;
    };
    let ast::ExpressionKind::Call { arguments, .. } = call.kind.as_ref() else {
        return;
    };
    let Some(callable) = resolved_callables.get(&span_key(call.span)) else {
        return;
    };
    for &index in &callable.reference_parameters {
        let Some(argument) = arguments.get(index) else {
            continue;
        };
        let message = match argument.kind.as_ref() {
            ast::ExpressionKind::Reference { .. } => continue,
            ast::ExpressionKind::Identifier(name) if references.contains_key(&name.name) => {
                continue;
            }
            ast::ExpressionKind::Identifier(name) => {
                msg::launch_borrows_without_reference(&name.name)
            }
            _ => msg::scoped_launch_argument_not_a_place().to_string(),
        };
        errors.push(ScopedLaunchError {
            message,
            span: argument.span,
            launched_at: None,
        });
    }
}

fn check_function(
    parameters: &[ast::Parameter],
    body: &ast::Block,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
    errors: &mut Vec<ScopedLaunchError>,
) {
    // The shared expression visitor takes a mutable tree.
    let mut body = body.clone();
    let mut facts = FunctionFacts {
        resolved_callables,
        holders: FxHashMap::default(),
        placed_launches: FxHashSet::default(),
        allowed_uses: FxHashSet::default(),
        collection_holders: FxHashMap::default(),
        collection_uses: FxHashSet::default(),
        declared_at: FxHashMap::default(),
        references: parameters
            .iter()
            .filter_map(|parameter| match parameter.param_type.kind.as_ref() {
                ast::TypeKind::Reference(reference) => {
                    Some((parameter.name.name.clone(), reference.is_mutable))
                }
                _ => None,
            })
            .collect(),
    };
    facts.record_block(&body);
    visit::expressions_in_block(&mut body, &mut |expression| {
        facts.record_expression(expression);
    });

    visit::expressions_in_block(&mut body, &mut |expression| match expression.kind.as_ref() {
        ast::ExpressionKind::Launch(_) => {
            check_borrowed_arguments(expression, resolved_callables, &facts.references, errors);
            if is_scoped_launch(expression, resolved_callables)
                && !facts.placed_launches.contains(&span_key(expression.span))
            {
                errors.push(ScopedLaunchError {
                    message: msg::scoped_launch_must_be_bound(&borrowed_name(expression)),
                    span: expression.span,
                    launched_at: None,
                });
            }
        }
        ast::ExpressionKind::Identifier(identifier) => {
            let key = span_key(expression.span);
            if let Some((launched_at, borrowed)) = facts.holders.get(&identifier.name)
                && !facts.allowed_uses.contains(&key)
            {
                errors.push(ScopedLaunchError {
                    message: msg::task_cannot_leave_scope(borrowed),
                    span: expression.span,
                    launched_at: Some(*launched_at),
                });
            }
            if let Some((launched_at, borrowed)) = facts.collection_holders.get(&identifier.name)
                && !facts.collection_uses.contains(&key)
            {
                errors.push(ScopedLaunchError {
                    message: msg::task_collection_misused(&identifier.name, borrowed),
                    span: expression.span,
                    launched_at: Some(*launched_at),
                });
            }
        }
        ast::ExpressionKind::MethodCall { .. } => {
            let Some((holder, launch)) = pushed_launch(expression) else {
                return;
            };
            if !is_scoped_launch(launch, resolved_callables) {
                return;
            }
            let holder_declared = facts.declared_at.get(&holder.name).copied();
            for (root, mutable) in
                borrowed_roots(launch, resolved_callables, &facts.references)
            {
                if mutable {
                    errors.push(ScopedLaunchError {
                        message: msg::task_collection_mutable_borrow(&root),
                        span: launch.span,
                        launched_at: None,
                    });
                }
                let root_declared = facts.declared_at.get(&root).copied();
                let joined_first = match (holder_declared, root_declared) {
                    (Some(holder_at), Some(root_at)) => holder_at > root_at,
                    // A parameter outlives every local.
                    (Some(_), None) => true,
                    (None, _) => false,
                };
                if !joined_first {
                    errors.push(ScopedLaunchError {
                        message: msg::task_collection_declared_too_early(&root, &holder.name),
                        span: launch.span,
                        launched_at: None,
                    });
                }
            }
        }
        _ => {}
    });
}

/// Checks every function body in `program`.
pub fn check_program(
    program: &ast::Program,
    resolved_callables: &FxHashMap<CallSiteKey, ResolvedCallable>,
) -> Vec<ScopedLaunchError> {
    let mut errors = Vec::new();
    for item in &program.items {
        match &item.kind {
            ast::ItemKind::Function(function) => {
                check_function(&function.parameters, &function.body, resolved_callables, &mut errors);
            }
            ast::ItemKind::Impl(implementation) => {
                for member in &implementation.items {
                    match member {
                        ast::ImplItemKind::Function(function) => {
                            check_function(&function.parameters, &function.body, resolved_callables, &mut errors);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            check_function(&cast.parameters, &cast.body, resolved_callables, &mut errors);
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    errors
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer, parser::Parser, semantic::typeck::TypeChecker, symbol_table::CompilerSymbolTable,
    };

    /// A stand-in for std's Vec: the checks key on the `push` method.
    const POOL: &str = "struct Vec<T> { T* data; }\n\
                        impl Vec<T> { void push(&mut self, T value) {} }\n\
                        void read(&i64 value) {}\n\
                        &i64 pick(&i64 value) { return value; }\n";

    fn launch_errors(body: &str) -> Vec<String> {
        let source = format!("{POOL}i64 main() {{\n{body}\nreturn 0;\n}}\n");
        let tokens = lexer::lex(&source).expect("probe should lex");
        let (mut program, errors) = Parser::new(tokens).parse_program();
        assert!(errors.is_empty(), "probe should parse: {errors:?}");
        TypeChecker::resolve_type_aliases_in_program(&mut program);
        let mut checker = TypeChecker::new();
        let (type_errors, _) =
            checker.check_program_with_table(&program, &mut CompilerSymbolTable::new());
        assert!(type_errors.is_empty(), "probe should type check: {type_errors:?}");
        let inferred_lets = checker.take_inferred_lets();
        crate::semantic::typeck::populate_inferred_let_types(&mut program, &inferred_lets);
        super::check_program(&program, checker.resolved_callables())
            .into_iter()
            .map(|error| error.message)
            .collect()
    }

    #[test]
    fn an_indexed_borrow_orders_the_collection_after_its_root() {
        let early = launch_errors(
            "Vec<Task<void>> pool;\ni64 values[2] = { 1, 2 };\npool.push(launch read(&values[0]));",
        );
        assert!(
            early.iter().any(|message| message.contains("declared after 'values'")),
            "{early:?}"
        );
        let late = launch_errors(
            "i64 values[2] = { 1, 2 };\nVec<Task<void>> pool;\npool.push(launch read(&values[0]));",
        );
        assert!(late.is_empty(), "{late:?}");
    }

    #[test]
    fn a_reference_parameter_takes_only_an_explicit_borrow() {
        let bare = launch_errors("i64 x = 1;\nTask<void> t = launch read(x);\nwait t;");
        assert_eq!(
            bare,
            ["launch passes `x` to a reference parameter without `&`: declare it `shared`, \
              or move it into the task through a by-value parameter"]
        );
        let call = launch_errors("i64 x = 1;\nTask<void> t = launch read(pick(&x));\nwait t;");
        assert_eq!(
            call,
            ["a borrowed launch argument must be `&place`, `&mut place`, or a reference variable"]
        );
        let forwarded =
            launch_errors("i64 x = 1;\n&i64 r = &x;\nTask<void> t = launch read(r);\nwait t;");
        assert!(forwarded.is_empty(), "{forwarded:?}");
    }

    #[test]
    fn a_for_initializer_cannot_hold_a_borrowing_launch() {
        let errors = launch_errors(
            "i64 x = 1;\nfor (Task<void> t = launch read(&x); x < 3; x += 1) { }",
        );
        assert!(
            errors.iter().any(|message| message.starts_with("a launch that borrows 'x' must be bound")),
            "{errors:?}"
        );
    }
}
