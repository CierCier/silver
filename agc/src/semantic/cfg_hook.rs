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
        ast::ItemKind::Impl(impl_item) => {
            for impl_member in &mut impl_item.items {
                match impl_member {
                    ast::ImplItemKind::Function(function) => rewrite_block(&mut function.body, cfg),
                    ast::ImplItemKind::Cast(cast) => rewrite_block(&mut cast.body, cfg),
                    ast::ImplItemKind::AssociatedType(_) => {}
                }
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
            }
        }
        ast::ExpressionKind::Unary { operand, .. } => rewrite_expression(operand, cfg),
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
        ast::ExpressionKind::Block(block) => rewrite_block(block, cfg),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cfg::CfgSet;
    use crate::lexer::lex;
    use crate::parser::Parser;
    use crate::semantic::analyzer::Analyzer;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lexer should succeed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parser errors: {errors:?}");
        program
    }

    fn run_hook(program: &mut ast::Program, cfg: &CfgSet) {
        let mut analyzer = Analyzer::new();
        let mut hook = CfgFoldHook::new(cfg);
        let mut hooks: [&mut dyn SemanticAnalyzerHook; 1] = [&mut hook];
        let errors = analyzer.analyze_program_with_hooks(program, &mut hooks);
        assert!(errors.is_empty(), "semantic errors: {errors:?}");
    }

    #[test]
    fn folds_absent_key_to_false() {
        let mut program = parse("i32 main() { if (@cfg(flag)) { return 1; } return 0; }");
        let cfg = CfgSet::default();
        run_hook(&mut program, &cfg);
        // The if collapsed to a block; the dead `return 1;` must be gone.
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        assert!(!contains_return_one(body), "dead branch pruned");
        assert!(contains_return_zero(body), "live statement retained");
    }

    fn contains_return_zero(block: &ast::Block) -> bool {
        for statement in &block.statements {
            match &statement.kind {
                ast::StatementKind::Return(Some(expr)) => {
                    if let ast::ExpressionKind::Literal(ast::Literal::Integer(0)) = &*expr.kind {
                        return true;
                    }
                }
                ast::StatementKind::Block(inner) => {
                    if contains_return_zero(inner) {
                        return true;
                    }
                }
                ast::StatementKind::Expression(expr) => {
                    if let ast::ExpressionKind::Block(inner) = &*expr.kind {
                        if contains_return_zero(inner) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    #[test]
    fn folds_present_key_to_true() {
        let mut program = parse("i32 main() { if (@cfg(flag)) { return 1; } return 0; }");
        let cfg = CfgSet::parse(&["flag".to_string()]);
        run_hook(&mut program, &cfg);
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        // Live branch kept: the block now contains `return 1;`.
        let block = &body.statements[0].kind;
        assert!(matches!(
            block,
            ast::StatementKind::Block(_) | ast::StatementKind::Expression(_)
        ));
        // Walk: the collapsed block must contain the return 1.
        let contains_return_one = body_contains_return_one(&program.items[0]);
        assert!(contains_return_one, "live branch retained");
    }

    fn body_contains_return_one(item: &ast::Item) -> bool {
        match &item.kind {
            ast::ItemKind::Function(f) => contains_return_one(&f.body),
            _ => false,
        }
    }

    fn contains_return_one(block: &ast::Block) -> bool {
        for statement in &block.statements {
            match &statement.kind {
                ast::StatementKind::Return(Some(expr)) => {
                    if let ast::ExpressionKind::Literal(ast::Literal::Integer(1)) = &*expr.kind {
                        return true;
                    }
                }
                ast::StatementKind::Block(inner) => {
                    if contains_return_one(inner) {
                        return true;
                    }
                }
                ast::StatementKind::Expression(expr) => {
                    if let ast::ExpressionKind::Block(inner) = &*expr.kind {
                        if contains_return_one(inner) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    #[test]
    fn folds_string_form_key() {
        let mut program = parse("i32 main() { if (@cfg(\"cpu.sse41\")) { return 1; } return 0; }");
        let cfg = CfgSet::parse(&["cpu.sse41=1".to_string()]);
        run_hook(&mut program, &cfg);
        // String form folds to the same probe global as the dotted form.
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        let cond = match &body.statements[0].kind {
            ast::StatementKind::Expression(expr) => match &*expr.kind {
                ast::ExpressionKind::If { condition, .. } => condition,
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        };
        match &*cond.kind {
            ast::ExpressionKind::Identifier(id) => assert_eq!(id.name, "g_has_sse41"),
            _ => panic!("expected probe global identifier"),
        }
    }

    #[test]
    fn folds_cpu_key_to_probe_global() {
        let mut program = parse("i32 main() { if (@cfg(cpu.sse41)) { return 1; } return 0; }");
        let cfg = CfgSet::parse(&["cpu.sse41=1".to_string()]);
        run_hook(&mut program, &cfg);
        // The condition became `g_has_sse41`; the if must remain (runtime probe).
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        let cond = match &body.statements[0].kind {
            ast::StatementKind::Expression(expr) => match &*expr.kind {
                ast::ExpressionKind::If { condition, .. } => condition,
                _ => panic!("expected if expression"),
            },
            _ => panic!("expected expression statement"),
        };
        match &*cond.kind {
            ast::ExpressionKind::Identifier(id) => assert_eq!(id.name, "g_has_sse41"),
            _ => panic!("expected probe global identifier"),
        }
    }

    #[test]
    fn prunes_else_if_chains() {
        let mut program = parse(
            "i32 main() { if (@cfg(a)) { return 1; } else if (@cfg(b)) { return 2; } else { return 3; } }",
        );
        let cfg = CfgSet::parse(&["b".to_string()]);
        run_hook(&mut program, &cfg);
        // First branch folded false; the else-if's condition folded true → the
        // whole chain collapses to `return 2;`.
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        assert!(contains_return_two(body), "else-if live branch retained");
    }

    fn contains_return_two(block: &ast::Block) -> bool {
        for statement in &block.statements {
            match &statement.kind {
                ast::StatementKind::Return(Some(expr)) => {
                    if let ast::ExpressionKind::Literal(ast::Literal::Integer(2)) = &*expr.kind {
                        return true;
                    }
                }
                ast::StatementKind::Block(inner) => {
                    if contains_return_two(inner) {
                        return true;
                    }
                }
                ast::StatementKind::Expression(expr) => {
                    if let ast::ExpressionKind::Block(inner) = &*expr.kind {
                        if contains_return_two(inner) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    #[test]
    fn leaves_malformed_cfg_alone() {
        let mut program = parse("i32 main() { return @cfg(); }");
        let cfg = CfgSet::default();
        run_hook(&mut program, &cfg);
        let body = match &program.items[0].kind {
            ast::ItemKind::Function(f) => &f.body,
            _ => panic!("expected function"),
        };
        match &body.statements[0].kind {
            ast::StatementKind::Return(Some(expr)) => {
                assert!(
                    matches!(&*expr.kind, ast::ExpressionKind::MacroCall { .. }),
                    "malformed @cfg left for typeck"
                );
            }
            _ => panic!("expected return"),
        }
    }
}
