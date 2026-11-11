//! Placement and pinning rules for scoped launches.
//!
//! A launch whose callee takes a `&T`/`&mut T` parameter borrows from the
//! launching scope, so the task must be joined before that scope ends. The
//! `Task` drop glue performs the join; this pass keeps the handle where that
//! glue is guaranteed to run in time:
//!
//! - the launch must initialize a local (`Task<T> t = launch f(&x);`) or be
//!   the direct operand of `wait`. A local declared by such an initializer is
//!   always dropped before the locals it borrows, which were declared earlier.
//! - that local (the holder) may only be consumed by `wait`. Any other use
//!   could move the handle to a place that outlives the borrow.
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
    /// Identifiers that are the direct operand of `wait`.
    waited_identifiers: FxHashSet<SpanKey>,
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

    /// Records the statements of blocks owned by `expression`, and `wait`
    /// operands. The visitor reaches nested expressions on its own.
    fn record_expression(&mut self, expression: &ast::Expression) {
        match expression.kind.as_ref() {
            ast::ExpressionKind::Lambda { body, .. }
            | ast::ExpressionKind::Block(body)
            | ast::ExpressionKind::While { body, .. }
            | ast::ExpressionKind::ForIn { body, .. } => self.record_block(body),
            ast::ExpressionKind::For { init, body, .. } => {
                self.record_binding(init);
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
                    self.waited_identifiers.insert(span_key(operand.span));
                } else if is_scoped_launch(operand, self.resolved_callables) {
                    self.placed_launches.insert(span_key(operand.span));
                }
            }
            _ => {}
        }
    }
}

fn check_function(
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
        waited_identifiers: FxHashSet::default(),
    };
    facts.record_block(&body);
    visit::expressions_in_block(&mut body, &mut |expression| {
        facts.record_expression(expression);
    });

    visit::expressions_in_block(&mut body, &mut |expression| match expression.kind.as_ref() {
        ast::ExpressionKind::Launch(_)
            if is_scoped_launch(expression, resolved_callables)
                && !facts.placed_launches.contains(&span_key(expression.span)) =>
        {
            errors.push(ScopedLaunchError {
                message: msg::scoped_launch_must_be_bound(&borrowed_name(expression)),
                span: expression.span,
                launched_at: None,
            });
        }
        ast::ExpressionKind::Identifier(identifier)
            if !facts
                .waited_identifiers
                .contains(&span_key(expression.span)) =>
        {
            if let Some((launched_at, borrowed)) = facts.holders.get(&identifier.name) {
                errors.push(ScopedLaunchError {
                    message: msg::task_cannot_leave_scope(borrowed),
                    span: expression.span,
                    launched_at: Some(*launched_at),
                });
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
                check_function(&function.body, resolved_callables, &mut errors);
            }
            ast::ItemKind::Impl(implementation) => {
                for member in &implementation.items {
                    match member {
                        ast::ImplItemKind::Function(function) => {
                            check_function(&function.body, resolved_callables, &mut errors);
                        }
                        ast::ImplItemKind::Cast(cast) => {
                            check_function(&cast.body, resolved_callables, &mut errors);
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
