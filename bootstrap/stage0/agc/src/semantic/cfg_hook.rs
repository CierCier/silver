//! Semantic hook that folds `@cfg(key)` expressions and prunes dead branches.
//!
//! Runs as an analyzer hook after symbol analysis but before type checking:
//!   - `@cfg(key)` with `key` absent from the cfg set → literal `false`.
//!   - `@cfg(key)` with a custom key present → literal `true`.
//!   - `@cfg(cpu.feature)` with the key present → identifier `g_has_feature`,
//!     the runtime probe global set by `__silver_cpu_init` before `main`.
//!   - `if (true/false)` with a literal condition collapses to the live
//!     branch. This is required, not an optimization: the dead branch may
//!     reference items the cfg gate removed, so type checking must never see
//!     it. Malformed `@cfg` calls are left untouched and fail in typeck with
//!     the unknown-builtin-macro diagnostic.

use crate::cfg::CfgSet;
use crate::lexer::Span;
use crate::parser::ast;

use super::analyzer::{SemanticAnalyzerHook, SemanticError};

pub struct CfgFoldHook<'a> {
    cfg: &'a CfgSet,
}

impl<'a> CfgFoldHook<'a> {
    pub fn new(cfg: &'a CfgSet) -> Self {
        Self { cfg }
    }
}

impl SemanticAnalyzerHook for CfgFoldHook<'_> {
    fn after_analysis(&mut self, program: &mut ast::Program, _errors: &[SemanticError]) {
        fold_and_prune(program, self.cfg);
    }
}

/// Fold every `@cfg(...)` expression in the program and collapse
/// constant-condition ifs. Runs after the item gate and before semantic
/// analysis, because the analyzer validates identifiers: dead branches
/// referencing gated-out items must already be gone.
pub fn fold_and_prune(program: &mut ast::Program, cfg: &CfgSet) {
    for item in &mut program.items {
        rewrite_item(item, cfg);
    }
}

fn rewrite_item(item: &mut ast::Item, cfg: &CfgSet) {
    match &mut item.kind {
        ast::ItemKind::Function(function) => rewrite_block(&mut function.body, cfg),
        ast::ItemKind::GlobalVariable(global) => {
            if let Some(init) = &mut global.initializer {
                rewrite_expression(init, cfg);
            }
        }
        ast::ItemKind::Impl(impl_item) => {
            for impl_member in &mut impl_item.items {
                match impl_member {
                    ast::ImplItemKind::Function(function) => rewrite_block(&mut function.body, cfg),
                    ast::ImplItemKind::Cast(cast) => rewrite_block(&mut cast.body, cfg),
                    ast::ImplItemKind::AssociatedType(_) => {}
                }
            }
        }
        ast::ItemKind::Trait(trait_item) => {
            for member in &mut trait_item.items {
                if let ast::TraitItemKind::Function(f) = member
                    && let Some(body) = &mut f.default_body
                {
                    rewrite_block(body, cfg);
                }
            }
        }
        ast::ItemKind::Macro(m) => rewrite_block(&mut m.body, cfg),
        // Defensive: expansion runs before folding, so no CfgIf survives.
        // Recurse anyway so direct fold_and_prune callers still fold inner.
        ast::ItemKind::CfgIf(cfg_if) => {
            for inner in cfg_if
                .then_items
                .iter_mut()
                .chain(cfg_if.else_items.iter_mut())
            {
                rewrite_item(inner, cfg);
            }
        }
        _ => {}
    }
}

fn rewrite_block(block: &mut ast::Block, cfg: &CfgSet) {
    for statement in &mut block.statements {
        rewrite_statement(statement, cfg);
    }
}

fn rewrite_statement(statement: &mut ast::Statement, cfg: &CfgSet) {
    match &mut statement.kind {
        ast::StatementKind::Block(block) => rewrite_block(block, cfg),
        ast::StatementKind::Let(let_stmt) => {
            if let Some(initializer) = &mut let_stmt.initializer {
                rewrite_expression(initializer, cfg);
            }
        }
        ast::StatementKind::Expression(expression) => rewrite_expression(expression, cfg),
        ast::StatementKind::Return(value) | ast::StatementKind::Break(value) => {
            if let Some(expression) = value {
                rewrite_expression(expression, cfg);
            }
        }
        ast::StatementKind::Continue => {}
        ast::StatementKind::Defer(inner) => rewrite_statement(inner, cfg),
    }
}

fn rewrite_expression(expression: &mut ast::Expression, cfg: &CfgSet) {
    match expression.kind.as_mut() {
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::EnumVariant { .. } => {}
        ast::ExpressionKind::MacroCall { name, args } => {
            if name.name == "cfg"
                && let Some(folded) = eval_cfg_call(cfg, args, expression.span)
            {
                *expression = folded;
            } else if name.name == "assert"
                && let Some(expanded) = expand_assert(args, expression.span)
            {
                // Rewrite the expansion so its own @cfg(debug) folds and the
                // branch prunes: debug builds keep the check, release builds
                // compile it away entirely.
                *expression = expanded;
                rewrite_expression(expression, cfg);
            } else if name.name == "file" && args.is_empty() {
                let file = crate::lexer::source_file(expression.span.file)
                    .map(|f| f.path)
                    .unwrap_or_else(|| "<unknown>".to_string());
                *expression = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(file))),
                    span: expression.span,
                };
            } else if name.name == "line" && args.is_empty() {
                let line = expression.span.start_line as i128;
                *expression = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(line))),
                    span: expression.span,
                };
            } else if name.name == "column" && args.is_empty() {
                let col = expression.span.start_col as i128;
                *expression = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(col))),
                    span: expression.span,
                };
            } else if name.name == "dbg"
                && let Some(expanded) = expand_dbg(args, expression.span)
            {
                *expression = expanded;
                rewrite_expression(expression, cfg);
            }
        }
        ast::ExpressionKind::Binary {
            left,
            right,
            operator,
        } => {
            rewrite_expression(left, cfg);
            rewrite_expression(right, cfg);
            // Short-circuit-safe constant folding: `false && X` → false,
            // `true && X` → X, `true || X` → true, `false || X` → X. These
            // never change evaluation (the skipped side was already
            // short-circuited), so they are valid for any X, side effects
            // included.
            let simplified = match operator {
                ast::BinaryOperator::LogicalAnd => match &*left.kind {
                    ast::ExpressionKind::Literal(ast::Literal::Bool(false)) => {
                        Some(lit_bool(false, expression.span))
                    }
                    ast::ExpressionKind::Literal(ast::Literal::Bool(true)) => {
                        Some((**right).clone())
                    }
                    _ => None,
                },
                ast::BinaryOperator::LogicalOr => match &*left.kind {
                    ast::ExpressionKind::Literal(ast::Literal::Bool(true)) => {
                        Some(lit_bool(true, expression.span))
                    }
                    ast::ExpressionKind::Literal(ast::Literal::Bool(false)) => {
                        Some((**right).clone())
                    }
                    _ => None,
                },
                _ => None,
            };
            if let Some(simplified) = simplified {
                *expression = simplified;
            }
        }
        ast::ExpressionKind::Unary { operator, operand } => {
            rewrite_expression(operand, cfg);
            if *operator == ast::UnaryOperator::Not
                && let ast::ExpressionKind::Literal(ast::Literal::Bool(value)) = &*operand.kind
            {
                *expression = lit_bool(!value, expression.span);
            }
        }
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            rewrite_expression(function, cfg);
            for argument in arguments {
                rewrite_expression(argument, cfg);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            rewrite_expression(receiver, cfg);
            for argument in arguments {
                rewrite_expression(argument, cfg);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. } => rewrite_expression(object, cfg),
        ast::ExpressionKind::Index { object, index } => {
            rewrite_expression(object, cfg);
            rewrite_expression(index, cfg);
        }
        ast::ExpressionKind::Slice {
            object,
            start,
            end,
            step,
        } => {
            rewrite_expression(object, cfg);
            if let Some(s) = start {
                rewrite_expression(s, cfg);
            }
            if let Some(e) = end {
                rewrite_expression(e, cfg);
            }
            if let Some(st) = step {
                rewrite_expression(st, cfg);
            }
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression(condition, cfg);
            if let ast::ExpressionKind::Literal(ast::Literal::Bool(value)) = &*condition.kind {
                // Constant condition: keep only the live branch, rewritten.
                let mut chosen = if *value {
                    then_branch.clone()
                } else {
                    else_branch.clone().unwrap_or_else(|| ast::Block {
                        statements: Vec::new(),
                        span: expression.span,
                    })
                };
                rewrite_block(&mut chosen, cfg);
                *expression = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Block(chosen)),
                    span: expression.span,
                };
            } else {
                rewrite_block(then_branch, cfg);
                if let Some(else_branch) = else_branch {
                    rewrite_block(else_branch, cfg);
                }
            }
        }
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            rewrite_expression(condition, cfg);
            if let ast::ExpressionKind::Literal(ast::Literal::Bool(value)) = &*condition.kind {
                // Constant condition: keep only the live branch, rewritten.
                let mut chosen = if *value {
                    (**then_expr).clone()
                } else {
                    (**else_expr).clone()
                };
                rewrite_expression(&mut chosen, cfg);
                *expression = chosen;
            } else {
                rewrite_expression(then_expr, cfg);
                rewrite_expression(else_expr, cfg);
            }
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            rewrite_expression(value, cfg);
            rewrite_expression(fallback, cfg);
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression(condition, cfg);
            rewrite_block(body, cfg);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(initializer) = &mut init.initializer {
                rewrite_expression(initializer, cfg);
            }
            rewrite_expression(condition, cfg);
            rewrite_expression(increment, cfg);
            rewrite_block(body, cfg);
        }
        ast::ExpressionKind::Match {
            expression: inner,
            arms,
        } => {
            rewrite_expression(inner, cfg);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_expression(guard, cfg);
                }
                rewrite_expression(&mut arm.body, cfg);
            }
        }
        ast::ExpressionKind::Block(block) | ast::ExpressionKind::Lambda { body: block, .. } => rewrite_block(block, cfg),
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(value)
                    | ast::InitializerItem::Field { value, .. }
                    | ast::InitializerItem::Index { value, .. } => rewrite_expression(value, cfg),
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                rewrite_expression(item, cfg);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression(&mut field.value, cfg);
            }
        }
        ast::ExpressionKind::Cast {
            expression: inner, ..
        } => rewrite_expression(inner, cfg),
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            rewrite_expression(iterable, cfg);
            rewrite_block(body, cfg);
        }
        ast::ExpressionKind::Comptime(inner) => rewrite_expression(inner, cfg),
        ast::ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                rewrite_expression(input, cfg);
            }
        }
        ast::ExpressionKind::Launch(inner) => rewrite_expression(inner, cfg),
        ast::ExpressionKind::Wait(inner) => rewrite_expression(inner, cfg),
        ast::ExpressionKind::Postfix { operand, .. } => rewrite_expression(operand, cfg),
        ast::ExpressionKind::Move(inner) => rewrite_expression(inner, cfg),
        ast::ExpressionKind::Reference { expression, .. } => rewrite_expression(expression, cfg),
    }
}

/// Evaluate `@cfg(...)` against the cfg set. Returns `None` (leave the call
/// for typeck to reject) when the arguments are malformed.
fn lit_bool(value: bool, span: Span) -> ast::Expression {
    ast::Expression {
        kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(value))),
        span,
    }
}

/// Expand `@assert(cond)` / `@assert(cond, "message")` into
/// `if (@cfg(debug)) { if (!(cond)) { __silver_assert_failed(file, line, msg); } }`.
/// The cfg fold then keeps the check in debug builds and prunes it in
/// release builds (the report call is never referenced there). Returns None
/// for malformed calls, which then fail in typeck as an unknown macro.
fn expand_assert(args: &[ast::MacroArg], span: Span) -> Option<ast::Expression> {
    if args.is_empty() || args.len() > 2 {
        return None;
    }
    let ast::MacroArg::Expression(cond) = &args[0] else {
        return None;
    };
    let message_expr = match args.get(1) {
        None => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(
                "assertion failed".to_string(),
            ))),
            span,
        },
        Some(ast::MacroArg::Expression(expr)) => (*expr).clone(),
        Some(_) => return None,
    };
    let file = crate::lexer::source_file(span.file)
        .map(|f| f.path)
        .unwrap_or_else(|| "<unknown>".to_string());
    let line = span.start_line as i128;

    let report_call = ast::Expression {
        kind: Box::new(ast::ExpressionKind::Call {
            function: Box::new(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                    name: "__silver_assert_failed".to_string(),
                    span,
                })),
                span,
            }),
            arguments: vec![
                ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(file))),
                    span,
                },
                ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(line))),
                    span,
                },
                message_expr,
            ],
        }),
        span,
    };
    let inner_if = ast::Expression {
        kind: Box::new(ast::ExpressionKind::If {
            condition: Box::new(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Unary {
                    operator: ast::UnaryOperator::Not,
                    operand: Box::new((*cond).clone()),
                }),
                span,
            }),
            then_branch: ast::Block {
                statements: vec![ast::Statement {
                    kind: ast::StatementKind::Expression(report_call),
                    span,
                }],
                span,
            },
            else_branch: None,
        }),
        span,
    };
    let debug_cond = ast::Expression {
        kind: Box::new(ast::ExpressionKind::MacroCall {
            name: ast::Identifier {
                name: "cfg".to_string(),
                span,
            },
            args: vec![ast::MacroArg::Identifier(ast::Identifier {
                name: "debug".to_string(),
                span,
            })],
        }),
        span,
    };
    Some(ast::Expression {
        kind: Box::new(ast::ExpressionKind::If {
            condition: Box::new(debug_cond),
            then_branch: ast::Block {
                statements: vec![ast::Statement {
                    kind: ast::StatementKind::Expression(inner_if),
                    span,
                }],
                span,
            },
            else_branch: None,
        }),
        span,
    })
}

fn expand_dbg(args: &[ast::MacroArg], span: Span) -> Option<ast::Expression> {
    if args.len() != 1 {
        return None;
    }
    let ast::MacroArg::Expression(inner_expr) = &args[0] else {
        return None;
    };
    let file = crate::lexer::source_file(span.file)
        .map(|f| f.path)
        .unwrap_or_else(|| "<unknown>".to_string());
    let line = span.start_line;
    let col = span.start_col;

    let expr_text = crate::lexer::source_file(inner_expr.span.file)
        .and_then(|f| {
            if inner_expr.span.start < f.text.len() && inner_expr.span.end <= f.text.len() {
                let txt = f.text[inner_expr.span.start..inner_expr.span.end].trim();
                if !txt.is_empty() {
                    Some(txt.to_string())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .unwrap_or_else(|| "<expr>".to_string());

    static DBG_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);
    let var_name = format!(
        "__silver_dbg_{}",
        DBG_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    );

    let fmt_str = format!("[{}:{}:{}] {} = {{}}", file, line, col, expr_text);

    Some(ast::Expression {
        kind: Box::new(ast::ExpressionKind::Block(ast::Block {
            statements: vec![
                ast::Statement {
                    kind: ast::StatementKind::Let(ast::LetStatement {
                        pattern: ast::Pattern {
                            kind: ast::PatternKind::Identifier(ast::Identifier {
                                name: var_name.clone(),
                                span,
                            }),
                            span,
                        },
                        type_annotation: None,
                        initializer: Some((*inner_expr).clone()),
                        is_mutable: false,
                        is_static: false,
                        is_volatile: false,
                    }),
                    span,
                },
                ast::Statement {
                    kind: ast::StatementKind::Expression(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::MacroCall {
                            name: ast::Identifier {
                                name: "eprintln".to_string(),
                                span,
                            },
                            args: vec![
                                ast::MacroArg::Expression(ast::Expression {
                                    kind: Box::new(ast::ExpressionKind::Literal(
                                        ast::Literal::String(fmt_str),
                                    )),
                                    span,
                                }),
                                ast::MacroArg::Expression(ast::Expression {
                                    kind: Box::new(ast::ExpressionKind::Identifier(
                                        ast::Identifier {
                                            name: var_name.clone(),
                                            span,
                                        },
                                    )),
                                    span,
                                }),
                            ],
                        }),
                        span,
                    }),
                    span,
                },
                ast::Statement {
                    kind: ast::StatementKind::Expression(ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Move(Box::new(ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                                name: var_name,
                                span,
                            })),
                            span,
                        }))),
                        span,
                    }),
                    span,
                },
            ],
            span,
        })),
        span,
    })
}

fn eval_cfg_call(cfg: &CfgSet, args: &[ast::MacroArg], span: Span) -> Option<ast::Expression> {
    if args.len() != 1 {
        return None;
    }
    let key = match &args[0] {
        ast::MacroArg::Expression(expr) => expression_key(expr)?,
        ast::MacroArg::Identifier(id) => id.name.clone(),
        _ => return None,
    };
    let literal = |value: bool| ast::Expression {
        kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(value))),
        span,
    };
    if !cfg.contains(&key) {
        return Some(literal(false));
    }
    if let Some(suffix) = key.strip_prefix("cpu.") {
        // Present CPU feature: fold to a read of the runtime probe global
        // (`g_has_<feature>`) initialized by `__silver_cpu_init` in
        // std/cpu.ag. Unknown features surface as unknown-variable errors in
        // typeck when std/cpu.ag does not define the global.
        let global = format!("g_has_{}", sanitize_feature(suffix));
        return Some(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                name: global,
                span,
            })),
            span,
        });
    }
    Some(literal(true))
}

/// Extract a dotted key like `cpu.sse41` from an argument expression
/// (`cpu.sse41` parses as a field-access chain; strings are accepted too).
fn expression_key(expr: &ast::Expression) -> Option<String> {
    match &*expr.kind {
        ast::ExpressionKind::Identifier(id) => Some(id.name.clone()),
        ast::ExpressionKind::Literal(ast::Literal::String(value)) => Some(value.clone()),
        ast::ExpressionKind::FieldAccess { object, field } => {
            Some(format!("{}.{}", expression_key(object)?, field.name))
        }
        _ => None,
    }
}

/// Map a feature suffix to a global name: `sse4.1` → `sse4_1`.
fn sanitize_feature(suffix: &str) -> String {
    suffix
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}
