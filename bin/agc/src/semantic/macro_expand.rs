//! Native Silver Macro System.
//!
//! Provides compile-time macro expansion and comptime evaluation.
//! Macros are defined with native Silver function syntax:
//!   `macro T id(args...) { ... }` or `macro id(args...) { ... }`
//! and invoked with `@id(args...)`.
//!
//! Supports:
//! - Comptime evaluation / constant folding for literals and pure computations.
//! - AST expansion and inlining for expressions and statements.
//! - Typed variadics `T... args` with `.len` introspection, indexing, and `for (x in args)` loop unrolling.
//! - Macro hygiene for local variables declared with `let`.
//! - Nested / recursive macro expansion to fixpoint.

use std::sync::atomic::{AtomicUsize, Ordering};
use rustc_hash::FxHashMap as HashMap;

use crate::lexer::Span;
use crate::parser::ast::{
    self, BinaryOperator, Block, Expression, ExpressionKind, Identifier, Item, ItemKind,
    Literal, MacroArg, MacroDef, PatternKind, Statement, StatementKind,
    UnaryOperator,
};

static MACRO_COUNTER: AtomicUsize = AtomicUsize::new(1);

const MAX_EXPANSION_DEPTH: usize = 128;

/// Expands all user-defined macros throughout the program.
pub fn expand_macros_in_program(program: &mut ast::Program) {
    let mut macro_table: HashMap<String, MacroDef> = HashMap::default();
    for item in &program.items {
        if let ItemKind::Macro(def) = &item.kind {
            macro_table.insert(def.name.name.clone(), def.clone());
        }
    }

    if macro_table.is_empty() {
        return;
    }

    for item in &mut program.items {
        expand_macros_in_item(item, &macro_table);
    }
}

fn expand_macros_in_item(item: &mut Item, macro_table: &HashMap<String, MacroDef>) {
    match &mut item.kind {
        ItemKind::Function(func) => {
            expand_macros_in_block(&mut func.body, macro_table, 0);
        }
        ItemKind::Impl(impl_item) => {
            for member in &mut impl_item.items {
                if let ast::ImplItemKind::Function(func) = member {
                    expand_macros_in_block(&mut func.body, macro_table, 0);
                }
            }
        }
        ItemKind::GlobalVariable(global) => {
            if let Some(init) = &mut global.initializer {
                expand_macros_in_expr(init, macro_table, 0);
            }
        }
        ItemKind::Macro(def) => {
            expand_macros_in_block(&mut def.body, macro_table, 0);
        }
        _ => {}
    }
}

fn expand_macros_in_block(block: &mut Block, macro_table: &HashMap<String, MacroDef>, depth: usize) {
    if depth >= MAX_EXPANSION_DEPTH {
        return;
    }

    let mut new_statements = Vec::with_capacity(block.statements.len());

    for mut stmt in block.statements.drain(..) {
        expand_macros_in_statement(&mut stmt, macro_table, depth);

        // If this statement was a macro call that expanded into a block of statements,
        // splice the inner statements directly into the enclosing block.
        if let StatementKind::Expression(ref expr) = stmt.kind {
            if let ExpressionKind::Block(inner_block) = expr.kind.as_ref() {
                new_statements.extend(inner_block.statements.clone());
                continue;
            }
        }

        new_statements.push(stmt);
    }

    block.statements = new_statements;
}

fn expand_macros_in_statement(
    stmt: &mut Statement,
    macro_table: &HashMap<String, MacroDef>,
    depth: usize,
) {
    if depth >= MAX_EXPANSION_DEPTH {
        return;
    }

    match &mut stmt.kind {
        StatementKind::Block(inner) => {
            expand_macros_in_block(inner, macro_table, depth);
        }
        StatementKind::Expression(expr) => {
            expand_macros_in_expr(expr, macro_table, depth);
        }
        StatementKind::Let(let_stmt) => {
            if let Some(init) = &mut let_stmt.initializer {
                expand_macros_in_expr(init, macro_table, depth);
            }
        }
        StatementKind::Return(Some(expr)) => {
            expand_macros_in_expr(expr, macro_table, depth);
        }
        StatementKind::Return(None) => {}
        StatementKind::Break(Some(expr)) => {
            expand_macros_in_expr(expr, macro_table, depth);
        }
        StatementKind::Break(None) | StatementKind::Continue => {}
        StatementKind::Defer(inner) => {
            expand_macros_in_statement(inner, macro_table, depth);
        }
    }
}

fn expand_macros_in_expr(
    expr: &mut Expression,
    macro_table: &HashMap<String, MacroDef>,
    depth: usize,
) {
    if depth >= MAX_EXPANSION_DEPTH {
        return;
    }

    // First, recursively expand subexpressions:
    match expr.kind.as_mut() {
        ExpressionKind::Binary { left, right, .. } => {
            expand_macros_in_expr(left, macro_table, depth);
            expand_macros_in_expr(right, macro_table, depth);
        }
        ExpressionKind::Unary { operand, .. }
        | ExpressionKind::Postfix { operand, .. }
        | ExpressionKind::Move(operand)
        | ExpressionKind::Launch(operand)
        | ExpressionKind::Wait(operand)
        | ExpressionKind::Comptime(operand)
        | ExpressionKind::Reference { expression: operand, .. }
        | ExpressionKind::Cast { expression: operand, .. } => {
            expand_macros_in_expr(operand, macro_table, depth);
        }
        ExpressionKind::Call { function, arguments } => {
            expand_macros_in_expr(function, macro_table, depth);
            for arg in arguments {
                expand_macros_in_expr(arg, macro_table, depth);
            }
        }
        ExpressionKind::MethodCall { receiver, arguments, .. } => {
            expand_macros_in_expr(receiver, macro_table, depth);
            for arg in arguments {
                expand_macros_in_expr(arg, macro_table, depth);
            }
        }
        ExpressionKind::FieldAccess { object, .. } => {
            expand_macros_in_expr(object, macro_table, depth);
        }
        ExpressionKind::Index { object, index } => {
            expand_macros_in_expr(object, macro_table, depth);
            expand_macros_in_expr(index, macro_table, depth);
        }
        ExpressionKind::Slice { object, start, end, step } => {
            expand_macros_in_expr(object, macro_table, depth);
            if let Some(s) = start {
                expand_macros_in_expr(s, macro_table, depth);
            }
            if let Some(e) = end {
                expand_macros_in_expr(e, macro_table, depth);
            }
            if let Some(st) = step {
                expand_macros_in_expr(st, macro_table, depth);
            }
        }
        ExpressionKind::If { condition, then_branch, else_branch } => {
            expand_macros_in_expr(condition, macro_table, depth);
            expand_macros_in_block(then_branch, macro_table, depth);
            if let Some(else_b) = else_branch {
                expand_macros_in_block(else_b, macro_table, depth);
            }
        }
        ExpressionKind::Ternary { condition, then_expr, else_expr } => {
            expand_macros_in_expr(condition, macro_table, depth);
            expand_macros_in_expr(then_expr, macro_table, depth);
            expand_macros_in_expr(else_expr, macro_table, depth);
        }
        ExpressionKind::UnwrapOr { value, fallback } => {
            expand_macros_in_expr(value, macro_table, depth);
            expand_macros_in_expr(fallback, macro_table, depth);
        }
        ExpressionKind::While { condition, body } => {
            expand_macros_in_expr(condition, macro_table, depth);
            expand_macros_in_block(body, macro_table, depth);
        }
        ExpressionKind::ForIn { iterable, body, .. } => {
            expand_macros_in_expr(iterable, macro_table, depth);
            expand_macros_in_block(body, macro_table, depth);
        }
        ExpressionKind::For { init, condition, increment, body } => {
            if let Some(init_expr) = &mut init.initializer {
                expand_macros_in_expr(init_expr, macro_table, depth);
            }
            expand_macros_in_expr(condition, macro_table, depth);
            expand_macros_in_expr(increment, macro_table, depth);
            expand_macros_in_block(body, macro_table, depth);
        }
        ExpressionKind::Block(b) => {
            expand_macros_in_block(b, macro_table, depth);
        }
        ExpressionKind::Array(items) | ExpressionKind::Tuple(items) => {
            for item in items {
                expand_macros_in_expr(item, macro_table, depth);
            }
        }
        ExpressionKind::MacroCall { name, args } => {
            // Expand arguments first
            for arg in args.iter_mut() {
                if let MacroArg::Expression(arg_expr) = arg {
                    expand_macros_in_expr(arg_expr, macro_table, depth);
                }
            }

            // Check if name is a user macro
            if let Some(def) = macro_table.get(&name.name) {
                let mut type_args: Vec<ast::Type> = Vec::new();
                let mut call_args: Vec<Expression> = Vec::new();
                for a in args.iter() {
                    match a {
                        MacroArg::Type(ty) => type_args.push(ty.clone()),
                        MacroArg::Expression(e) => call_args.push(e.clone()),
                        _ => {}
                    }
                }

                let expanded = expand_macro_invocation(def, &type_args, &call_args, expr.span, macro_table, depth + 1);
                *expr = expanded;
                // Re-expand in case the macro returned another macro call
                expand_macros_in_expr(expr, macro_table, depth + 1);
                return;
            }
        }
        _ => {}
    }
}

/// Expands a single user macro invocation.
fn expand_macro_invocation(
    def: &MacroDef,
    type_args: &[ast::Type],
    args: &[Expression],
    call_span: Span,
    _macro_table: &HashMap<String, MacroDef>,
    _depth: usize,
) -> Expression {
    // Generic type substitution:
    let mut type_subst: HashMap<String, ast::Type> = HashMap::default();
    if let Some(generics) = &def.generics {
        for (i, param) in generics.params.iter().enumerate() {
            let param_name = match param {
                ast::GenericParam::Type(tp) => &tp.name.name,
                ast::GenericParam::Lifetime(lp) => &lp.name.name,
            };
            if i < type_args.len() {
                type_subst.insert(param_name.clone(), type_args[i].clone());
            } else if let Some(inferred) = infer_generic_param_type(param_name, def, args) {
                type_subst.insert(param_name.clone(), inferred);
            }
        }
    }

    // Parameter matching:
    let mut param_subst: HashMap<String, Expression> = HashMap::default();
    let mut vararg_subst: Option<(String, Vec<Expression>)> = None;

    for (i, param) in def.parameters.iter().enumerate() {
        if param.is_variadic {
            let varargs = if i < args.len() {
                args[i..].to_vec()
            } else {
                Vec::new()
            };
            vararg_subst = Some((param.name.name.clone(), varargs));
            break;
        } else if i < args.len() {
            param_subst.insert(param.name.name.clone(), args[i].clone());
        }
    }

    // Macro hygiene: rename local `let` variables in the body to prevent collisions
    let macro_id = MACRO_COUNTER.fetch_add(1, Ordering::Relaxed);
    let mut hygiene_map: HashMap<String, String> = HashMap::default();
    collect_local_bindings_in_block(&def.body, &mut hygiene_map, macro_id);

    let mut body = def.body.clone();
    apply_hygiene_and_subst(&mut body, &hygiene_map, &param_subst, &vararg_subst);
    if !type_subst.is_empty() {
        substitute_types_in_block(&mut body, &type_subst);
    }

    // Try compile-time evaluation if all arguments are constant
    let mut const_env: HashMap<String, Literal> = HashMap::default();
    for (param_name, expr) in &param_subst {
        if let ExpressionKind::Literal(lit) = expr.kind.as_ref() {
            const_env.insert(param_name.clone(), lit.clone());
        }
    }
    if let Some((_vararg_name, varargs)) = &vararg_subst {
        let mut vararg_lits = Vec::new();
        for arg in varargs {
            if let ExpressionKind::Literal(lit) = arg.kind.as_ref() {
                vararg_lits.push(lit.clone());
            }
        }
    }

    if let Some(folded_lit) = try_eval_const_block(&body, &const_env) {
        return Expression {
            kind: Box::new(ExpressionKind::Literal(folded_lit)),
            span: call_span,
        };
    }

    // Convert if-else ending with returns to ternary if applicable, or return expr to expression stmt:
    if let Some(last) = body.statements.last_mut() {
        if let Some(ternary) = try_convert_if_to_ternary(last) {
            *last = Statement {
                kind: StatementKind::Expression(ternary),
                span: last.span,
            };
        } else if let StatementKind::Return(Some(ret_expr)) = &mut last.kind {
            *last = Statement {
                kind: StatementKind::Expression(ret_expr.clone()),
                span: ret_expr.span,
            };
        }
    }

    // Check if the body is a single expression:
    if body.statements.len() == 1 {
        if let StatementKind::Expression(expr) = &body.statements[0].kind {
            return expr.clone();
        }
    }

    Expression {
        kind: Box::new(ExpressionKind::Block(body)),
        span: call_span,
    }
}

fn try_convert_if_to_ternary(stmt: &Statement) -> Option<Expression> {
    if let StatementKind::Expression(expr) = &stmt.kind {
        if let ExpressionKind::If {
            condition,
            then_branch,
            else_branch: Some(else_branch),
        } = expr.kind.as_ref()
        {
            let then_ret = match &then_branch.statements.last()?.kind {
                StatementKind::Return(Some(r)) => r.clone(),
                StatementKind::Expression(e) => e.clone(),
                _ => return None,
            };
            let else_ret = match &else_branch.statements.last()?.kind {
                StatementKind::Return(Some(r)) => r.clone(),
                StatementKind::Expression(e) => e.clone(),
                _ => return None,
            };
            return Some(Expression {
                kind: Box::new(ExpressionKind::Ternary {
                    condition: condition.clone(),
                    then_expr: Box::new(then_ret),
                    else_expr: Box::new(else_ret),
                }),
                span: expr.span,
            });
        }
    }
    None
}

/// Collects local variable names from `let` statements in a block and maps them to unique hygienic names.
fn collect_local_bindings_in_block(
    block: &Block,
    hygiene_map: &mut HashMap<String, String>,
    macro_id: usize,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            StatementKind::Let(let_stmt) => {
                if let PatternKind::Identifier(id) = &let_stmt.pattern.kind {
                    hygiene_map.insert(
                        id.name.clone(),
                        format!("{}_m{}", id.name, macro_id),
                    );
                }
            }
            StatementKind::Block(inner) => {
                collect_local_bindings_in_block(inner, hygiene_map, macro_id);
            }
            StatementKind::Expression(expr) => {
                if let ExpressionKind::ForIn { binding, body, .. } = expr.kind.as_ref() {
                    hygiene_map.insert(
                        binding.name.clone(),
                        format!("{}_m{}", binding.name, macro_id),
                    );
                    collect_local_bindings_in_block(body, hygiene_map, macro_id);
                }
            }
            _ => {}
        }
    }
}

/// Applies hygiene renaming, parameter substitution, variadic unrolling, and `.len` replacement to a block.
fn apply_hygiene_and_subst(
    block: &mut Block,
    hygiene_map: &HashMap<String, String>,
    param_subst: &HashMap<String, Expression>,
    vararg_subst: &Option<(String, Vec<Expression>)>,
) {
    let mut new_statements = Vec::with_capacity(block.statements.len());

    for mut stmt in block.statements.drain(..) {
        // Check for variadic `for x in args` loop unrolling:
        if let StatementKind::Expression(ref expr) = stmt.kind {
            if let ExpressionKind::ForIn { binding, iterable, body, .. } = expr.kind.as_ref() {
                if let ExpressionKind::Identifier(iter_id) = iterable.kind.as_ref() {
                    if let Some((vararg_name, varargs)) = vararg_subst {
                        if &iter_id.name == vararg_name {
                            // Unroll loop for each variadic argument!
                            for arg in varargs {
                                let mut unrolled_body = body.clone();
                                let mut loop_subst = param_subst.clone();
                                loop_subst.insert(binding.name.clone(), arg.clone());
                                if let Some(hyg_name) = hygiene_map.get(&binding.name) {
                                    loop_subst.insert(hyg_name.clone(), arg.clone());
                                }
                                apply_hygiene_and_subst(
                                    &mut unrolled_body,
                                    hygiene_map,
                                    &loop_subst,
                                    vararg_subst,
                                );
                                new_statements.extend(unrolled_body.statements);
                            }
                            continue;
                        }
                    }
                }
            }
        }

        apply_hygiene_and_subst_in_statement(&mut stmt, hygiene_map, param_subst, vararg_subst);
        new_statements.push(stmt);
    }

    block.statements = new_statements;
}

fn apply_hygiene_and_subst_in_statement(
    stmt: &mut Statement,
    hygiene_map: &HashMap<String, String>,
    param_subst: &HashMap<String, Expression>,
    vararg_subst: &Option<(String, Vec<Expression>)>,
) {
    match &mut stmt.kind {
        StatementKind::Block(inner) => {
            apply_hygiene_and_subst(inner, hygiene_map, param_subst, vararg_subst);
        }
        StatementKind::Expression(expr) => {
            apply_hygiene_and_subst_in_expr(expr, hygiene_map, param_subst, vararg_subst);
        }
        StatementKind::Let(let_stmt) => {
            if let PatternKind::Identifier(id) = &mut let_stmt.pattern.kind {
                if let Some(new_name) = hygiene_map.get(&id.name) {
                    id.name = new_name.clone();
                }
            }
            if let Some(init) = &mut let_stmt.initializer {
                apply_hygiene_and_subst_in_expr(init, hygiene_map, param_subst, vararg_subst);
            }
        }
        StatementKind::Return(Some(expr)) => {
            apply_hygiene_and_subst_in_expr(expr, hygiene_map, param_subst, vararg_subst);
        }
        StatementKind::Return(None) => {}
        StatementKind::Break(Some(expr)) => {
            apply_hygiene_and_subst_in_expr(expr, hygiene_map, param_subst, vararg_subst);
        }
        StatementKind::Break(None) | StatementKind::Continue => {}
        StatementKind::Defer(inner) => {
            apply_hygiene_and_subst_in_statement(inner, hygiene_map, param_subst, vararg_subst);
        }
    }
}

fn apply_hygiene_and_subst_in_expr(
    expr: &mut Expression,
    hygiene_map: &HashMap<String, String>,
    param_subst: &HashMap<String, Expression>,
    vararg_subst: &Option<(String, Vec<Expression>)>,
) {
    // Check if expr is an identifier matching a parameter
    if let ExpressionKind::Identifier(id) = expr.kind.as_ref() {
        if let Some(subst) = param_subst.get(&id.name) {
            *expr = subst.clone();
            return;
        }
        if let Some(hyg_name) = hygiene_map.get(&id.name) {
            *expr = Expression {
                kind: Box::new(ExpressionKind::Identifier(Identifier {
                    name: hyg_name.clone(),
                    span: id.span,
                })),
                span: expr.span,
            };
            return;
        }
    }

    // Check for `varargs.len`:
    if let ExpressionKind::FieldAccess { object, field } = expr.kind.as_ref() {
        if field.name == "len" {
            if let ExpressionKind::Identifier(id) = object.kind.as_ref() {
                if let Some((vararg_name, varargs)) = vararg_subst {
                    if &id.name == vararg_name {
                        *expr = Expression {
                            kind: Box::new(ExpressionKind::Literal(Literal::Integer(varargs.len() as i128))),
                            span: expr.span,
                        };
                        return;
                    }
                }
            }
        }
    }

    // Check for `varargs.len()`:
    if let ExpressionKind::MethodCall { receiver, method, arguments } = expr.kind.as_ref() {
        if method.name == "len" && arguments.is_empty() {
            if let ExpressionKind::Identifier(id) = receiver.kind.as_ref() {
                if let Some((vararg_name, varargs)) = vararg_subst {
                    if &id.name == vararg_name {
                        *expr = Expression {
                            kind: Box::new(ExpressionKind::Literal(Literal::Integer(varargs.len() as i128))),
                            span: expr.span,
                        };
                        return;
                    }
                }
            }
        }
    }

    // Check for `varargs[index]`:
    if let ExpressionKind::Index { object, index } = expr.kind.as_ref() {
        if let ExpressionKind::Identifier(id) = object.kind.as_ref() {
            if let Some((vararg_name, varargs)) = vararg_subst {
                if &id.name == vararg_name {
                    if let ExpressionKind::Literal(Literal::Integer(idx)) = index.kind.as_ref() {
                        let i = *idx as usize;
                        if i < varargs.len() {
                            *expr = varargs[i].clone();
                            return;
                        }
                    }
                }
            }
        }
    }

    match expr.kind.as_mut() {
        ExpressionKind::Binary { left, right, .. } => {
            apply_hygiene_and_subst_in_expr(left, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(right, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Unary { operand, .. }
        | ExpressionKind::Postfix { operand, .. }
        | ExpressionKind::Move(operand)
        | ExpressionKind::Launch(operand)
        | ExpressionKind::Wait(operand)
        | ExpressionKind::Comptime(operand)
        | ExpressionKind::Reference { expression: operand, .. }
        | ExpressionKind::Cast { expression: operand, .. } => {
            apply_hygiene_and_subst_in_expr(operand, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Call { function, arguments } => {
            apply_hygiene_and_subst_in_expr(function, hygiene_map, param_subst, vararg_subst);
            for arg in arguments {
                apply_hygiene_and_subst_in_expr(arg, hygiene_map, param_subst, vararg_subst);
            }
        }
        ExpressionKind::MethodCall { receiver, arguments, .. } => {
            apply_hygiene_and_subst_in_expr(receiver, hygiene_map, param_subst, vararg_subst);
            for arg in arguments {
                apply_hygiene_and_subst_in_expr(arg, hygiene_map, param_subst, vararg_subst);
            }
        }
        ExpressionKind::FieldAccess { object, .. } => {
            apply_hygiene_and_subst_in_expr(object, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Index { object, index } => {
            apply_hygiene_and_subst_in_expr(object, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(index, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Slice { object, start, end, step } => {
            apply_hygiene_and_subst_in_expr(object, hygiene_map, param_subst, vararg_subst);
            if let Some(s) = start {
                apply_hygiene_and_subst_in_expr(s, hygiene_map, param_subst, vararg_subst);
            }
            if let Some(e) = end {
                apply_hygiene_and_subst_in_expr(e, hygiene_map, param_subst, vararg_subst);
            }
            if let Some(st) = step {
                apply_hygiene_and_subst_in_expr(st, hygiene_map, param_subst, vararg_subst);
            }
        }
        ExpressionKind::If { condition, then_branch, else_branch } => {
            apply_hygiene_and_subst_in_expr(condition, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst(then_branch, hygiene_map, param_subst, vararg_subst);
            if let Some(else_b) = else_branch {
                apply_hygiene_and_subst(else_b, hygiene_map, param_subst, vararg_subst);
            }
        }
        ExpressionKind::Ternary { condition, then_expr, else_expr } => {
            apply_hygiene_and_subst_in_expr(condition, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(then_expr, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(else_expr, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::UnwrapOr { value, fallback } => {
            apply_hygiene_and_subst_in_expr(value, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(fallback, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::While { condition, body } => {
            apply_hygiene_and_subst_in_expr(condition, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst(body, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::ForIn { iterable, body, .. } => {
            apply_hygiene_and_subst_in_expr(iterable, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst(body, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::For { init, condition, increment, body } => {
            if let Some(init_expr) = &mut init.initializer {
                apply_hygiene_and_subst_in_expr(init_expr, hygiene_map, param_subst, vararg_subst);
            }
            apply_hygiene_and_subst_in_expr(condition, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst_in_expr(increment, hygiene_map, param_subst, vararg_subst);
            apply_hygiene_and_subst(body, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Block(b) => {
            apply_hygiene_and_subst(b, hygiene_map, param_subst, vararg_subst);
        }
        ExpressionKind::Array(items) | ExpressionKind::Tuple(items) => {
            for item in items {
                apply_hygiene_and_subst_in_expr(item, hygiene_map, param_subst, vararg_subst);
            }
        }
        ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let MacroArg::Expression(arg_expr) = arg {
                    apply_hygiene_and_subst_in_expr(arg_expr, hygiene_map, param_subst, vararg_subst);
                }
            }
        }
        _ => {}
    }
}

// ----------------------------------------------------------------------------
// Compile-Time Constant Evaluator
// ----------------------------------------------------------------------------

fn try_eval_const_block(block: &Block, env: &HashMap<String, Literal>) -> Option<Literal> {
    let mut local_env = env.clone();
    for stmt in &block.statements {
        match &stmt.kind {
            StatementKind::Let(let_stmt) => {
                if let PatternKind::Identifier(id) = &let_stmt.pattern.kind {
                    if let Some(init) = &let_stmt.initializer {
                        let val = try_eval_const_expr(init, &local_env)?;
                        local_env.insert(id.name.clone(), val);
                    }
                } else {
                    return None;
                }
            }
            StatementKind::Expression(expr) => {
                if let ExpressionKind::Binary { left, operator: BinaryOperator::Assign, right } = expr.kind.as_ref() {
                    if let ExpressionKind::Identifier(id) = left.kind.as_ref() {
                        let val = try_eval_const_expr(right, &local_env)?;
                        local_env.insert(id.name.clone(), val);
                    } else {
                        return None;
                    }
                } else if let ExpressionKind::Binary { left, operator, right } = expr.kind.as_ref() {
                    let bin_op = match operator {
                        BinaryOperator::AddAssign => BinaryOperator::Add,
                        BinaryOperator::SubtractAssign => BinaryOperator::Subtract,
                        BinaryOperator::MultiplyAssign => BinaryOperator::Multiply,
                        BinaryOperator::DivideAssign => BinaryOperator::Divide,
                        BinaryOperator::ModuloAssign => BinaryOperator::Modulo,
                        _ => return None,
                    };
                    if let ExpressionKind::Identifier(id) = left.kind.as_ref() {
                        let cur = local_env.get(&id.name)?.clone();
                        let rhs = try_eval_const_expr(right, &local_env)?;
                        let new_val = eval_binary_op(&cur, &bin_op, &rhs)?;
                        local_env.insert(id.name.clone(), new_val);
                    } else {
                        return None;
                    }
                } else if let ExpressionKind::If { condition, then_branch, else_branch } = expr.kind.as_ref() {
                    let cond_val = try_eval_const_expr(condition, &local_env)?;
                    if let Literal::Bool(b) = cond_val {
                        if b {
                            if let Some(ret) = try_eval_const_block(then_branch, &local_env) {
                                return Some(ret);
                            }
                        } else if let Some(else_b) = else_branch {
                            if let Some(ret) = try_eval_const_block(else_b, &local_env) {
                                return Some(ret);
                            }
                        }
                    } else {
                        return None;
                    }
                }
            }
            StatementKind::Return(Some(ret_expr)) => {
                return try_eval_const_expr(ret_expr, &local_env);
            }
            _ => return None,
        }
    }

    if let Some(last) = block.statements.last() {
        if let StatementKind::Expression(expr) = &last.kind {
            return try_eval_const_expr(expr, &local_env);
        }
    }

    None
}

fn try_eval_const_expr(expr: &Expression, env: &HashMap<String, Literal>) -> Option<Literal> {
    match expr.kind.as_ref() {
        ExpressionKind::Literal(lit) => Some(lit.clone()),
        ExpressionKind::Identifier(id) => env.get(&id.name).cloned(),
        ExpressionKind::Binary { left, operator, right } => {
            let l = try_eval_const_expr(left, env)?;
            let r = try_eval_const_expr(right, env)?;
            eval_binary_op(&l, operator, &r)
        }
        ExpressionKind::Unary { operator, operand } => {
            let val = try_eval_const_expr(operand, env)?;
            eval_unary_op(operator, &val)
        }
        ExpressionKind::If { condition, then_branch, else_branch } => {
            let cond_val = try_eval_const_expr(condition, env)?;
            if let Literal::Bool(b) = cond_val {
                if b {
                    try_eval_const_block(then_branch, env)
                } else if let Some(else_b) = else_branch {
                    try_eval_const_block(else_b, env)
                } else {
                    None
                }
            } else {
                None
            }
        }
        ExpressionKind::Block(block) => try_eval_const_block(block, env),
        _ => None,
    }
}

fn eval_binary_op(left: &Literal, op: &BinaryOperator, right: &Literal) -> Option<Literal> {
    match (left, right) {
        (Literal::Integer(l), Literal::Integer(r)) => match op {
            BinaryOperator::Add => Some(Literal::Integer(l.wrapping_add(*r))),
            BinaryOperator::Subtract => Some(Literal::Integer(l.wrapping_sub(*r))),
            BinaryOperator::Multiply => Some(Literal::Integer(l.wrapping_mul(*r))),
            BinaryOperator::Divide => {
                if *r == 0 {
                    None
                } else {
                    Some(Literal::Integer(l.wrapping_div(*r)))
                }
            }
            BinaryOperator::Modulo => {
                if *r == 0 {
                    None
                } else {
                    Some(Literal::Integer(l.wrapping_rem(*r)))
                }
            }
            BinaryOperator::BitwiseAnd => Some(Literal::Integer(l & r)),
            BinaryOperator::BitwiseOr => Some(Literal::Integer(l | r)),
            BinaryOperator::BitwiseXor => Some(Literal::Integer(l ^ r)),
            BinaryOperator::LeftShift => Some(Literal::Integer(l.wrapping_shl(*r as u32))),
            BinaryOperator::RightShift => Some(Literal::Integer(l.wrapping_shr(*r as u32))),
            BinaryOperator::Equal => Some(Literal::Bool(l == r)),
            BinaryOperator::NotEqual => Some(Literal::Bool(l != r)),
            BinaryOperator::Less => Some(Literal::Bool(l < r)),
            BinaryOperator::LessEqual => Some(Literal::Bool(l <= r)),
            BinaryOperator::Greater => Some(Literal::Bool(l > r)),
            BinaryOperator::GreaterEqual => Some(Literal::Bool(l >= r)),
            _ => None,
        },
        (Literal::Float(l), Literal::Float(r)) => match op {
            BinaryOperator::Add => Some(Literal::Float(l + r)),
            BinaryOperator::Subtract => Some(Literal::Float(l - r)),
            BinaryOperator::Multiply => Some(Literal::Float(l * r)),
            BinaryOperator::Divide => Some(Literal::Float(l / r)),
            BinaryOperator::Equal => Some(Literal::Bool((l - r).abs() < f64::EPSILON)),
            BinaryOperator::NotEqual => Some(Literal::Bool((l - r).abs() >= f64::EPSILON)),
            BinaryOperator::Less => Some(Literal::Bool(l < r)),
            BinaryOperator::LessEqual => Some(Literal::Bool(l <= r)),
            BinaryOperator::Greater => Some(Literal::Bool(l > r)),
            BinaryOperator::GreaterEqual => Some(Literal::Bool(l >= r)),
            _ => None,
        },
        (Literal::Bool(l), Literal::Bool(r)) => match op {
            BinaryOperator::LogicalAnd => Some(Literal::Bool(*l && *r)),
            BinaryOperator::LogicalOr => Some(Literal::Bool(*l || *r)),
            BinaryOperator::Equal => Some(Literal::Bool(l == r)),
            BinaryOperator::NotEqual => Some(Literal::Bool(l != r)),
            _ => None,
        },
        (Literal::String(l), Literal::String(r)) => match op {
            BinaryOperator::Add => Some(Literal::String(format!("{l}{r}"))),
            BinaryOperator::Equal => Some(Literal::Bool(l == r)),
            BinaryOperator::NotEqual => Some(Literal::Bool(l != r)),
            _ => None,
        },
        (Literal::Char(l), Literal::Char(r)) => match op {
            BinaryOperator::Equal => Some(Literal::Bool(l == r)),
            BinaryOperator::NotEqual => Some(Literal::Bool(l != r)),
            _ => None,
        },
        _ => None,
    }
}

fn eval_unary_op(op: &UnaryOperator, val: &Literal) -> Option<Literal> {
    match (op, val) {
        (UnaryOperator::Minus, Literal::Integer(i)) => Some(Literal::Integer(-i)),
        (UnaryOperator::Minus, Literal::Float(f)) => Some(Literal::Float(-f)),
        (UnaryOperator::Not, Literal::Bool(b)) => Some(Literal::Bool(!b)),
        (UnaryOperator::BitwiseNot, Literal::Integer(i)) => Some(Literal::Integer(!i)),
        _ => None,
    }
}

fn substitute_types_in_block(block: &mut Block, subst: &HashMap<String, ast::Type>) {
    if subst.is_empty() {
        return;
    }
    for stmt in &mut block.statements {
        substitute_types_in_statement(stmt, subst);
    }
}

fn substitute_types_in_statement(stmt: &mut Statement, subst: &HashMap<String, ast::Type>) {
    match &mut stmt.kind {
        StatementKind::Let(let_stmt) => {
            if let Some(ty) = &mut let_stmt.type_annotation {
                substitute_type(ty, subst);
            }
            if let Some(init) = &mut let_stmt.initializer {
                substitute_types_in_expr(init, subst);
            }
        }
        StatementKind::Expression(expr)
        | StatementKind::Return(Some(expr))
        | StatementKind::Break(Some(expr)) => {
            substitute_types_in_expr(expr, subst);
        }
        StatementKind::Block(b) => {
            substitute_types_in_block(b, subst);
        }
        StatementKind::Defer(s) => {
            substitute_types_in_statement(s, subst);
        }
        _ => {}
    }
}

fn substitute_types_in_expr(expr: &mut Expression, subst: &HashMap<String, ast::Type>) {
    match expr.kind.as_mut() {
        ExpressionKind::TypeName(ty) => {
            substitute_type(ty, subst);
        }
        ExpressionKind::Cast { target_type, expression } => {
            substitute_type(target_type, subst);
            substitute_types_in_expr(expression, subst);
        }
        ExpressionKind::Binary { left, right, .. } => {
            substitute_types_in_expr(left, subst);
            substitute_types_in_expr(right, subst);
        }
        ExpressionKind::Unary { operand, .. }
        | ExpressionKind::Postfix { operand, .. }
        | ExpressionKind::Move(operand)
        | ExpressionKind::Launch(operand)
        | ExpressionKind::Wait(operand)
        | ExpressionKind::Comptime(operand)
        | ExpressionKind::Reference { expression: operand, .. } => {
            substitute_types_in_expr(operand, subst);
        }
        ExpressionKind::Call { function, arguments } => {
            substitute_types_in_expr(function, subst);
            for a in arguments {
                substitute_types_in_expr(a, subst);
            }
        }
        ExpressionKind::MethodCall { receiver, arguments, .. } => {
            substitute_types_in_expr(receiver, subst);
            for a in arguments {
                substitute_types_in_expr(a, subst);
            }
        }
        ExpressionKind::FieldAccess { object, .. } => {
            substitute_types_in_expr(object, subst);
        }
        ExpressionKind::Index { object, index } => {
            substitute_types_in_expr(object, subst);
            substitute_types_in_expr(index, subst);
        }
        ExpressionKind::Slice { object, start, end, step } => {
            substitute_types_in_expr(object, subst);
            if let Some(s) = start { substitute_types_in_expr(s, subst); }
            if let Some(e) = end { substitute_types_in_expr(e, subst); }
            if let Some(st) = step { substitute_types_in_expr(st, subst); }
        }
        ExpressionKind::If { condition, then_branch, else_branch } => {
            substitute_types_in_expr(condition, subst);
            substitute_types_in_block(then_branch, subst);
            if let Some(eb) = else_branch { substitute_types_in_block(eb, subst); }
        }
        ExpressionKind::Ternary { condition, then_expr, else_expr } => {
            substitute_types_in_expr(condition, subst);
            substitute_types_in_expr(then_expr, subst);
            substitute_types_in_expr(else_expr, subst);
        }
        ExpressionKind::Block(b) => {
            substitute_types_in_block(b, subst);
        }
        ExpressionKind::Array(items) | ExpressionKind::Tuple(items) => {
            for item in items {
                substitute_types_in_expr(item, subst);
            }
        }
        ExpressionKind::ForIn { iterable, body, .. } => {
            substitute_types_in_expr(iterable, subst);
            substitute_types_in_block(body, subst);
        }
        ExpressionKind::While { condition, body } => {
            substitute_types_in_expr(condition, subst);
            substitute_types_in_block(body, subst);
        }
        _ => {}
    }
}

fn substitute_type(ty: &mut ast::Type, subst: &HashMap<String, ast::Type>) {
    match ty.kind.as_mut() {
        ast::TypeKind::Named(named) => {
            if named.path.len() == 1 && named.generics.is_none() {
                if let Some(replacement) = subst.get(&named.path[0].name) {
                    *ty = replacement.clone();
                    return;
                }
            }
            if let Some(generics) = &mut named.generics {
                for g in generics {
                    substitute_type(g, subst);
                }
            }
        }
        ast::TypeKind::Pointer(ptr) => {
            substitute_type(&mut ptr.inner, subst);
        }
        ast::TypeKind::Slice(slice) => {
            substitute_type(&mut slice.element_type, subst);
        }
        ast::TypeKind::Array(arr) => {
            substitute_type(&mut arr.element_type, subst);
        }
        ast::TypeKind::Optional(inner) => {
            substitute_type(inner, subst);
        }
        ast::TypeKind::Reference(ref_type) => {
            substitute_type(&mut ref_type.inner, subst);
        }
        ast::TypeKind::Tuple(types) => {
            for t in types {
                substitute_type(t, subst);
            }
        }
        ast::TypeKind::Function(fn_type) => {
            for p in &mut fn_type.parameters {
                substitute_type(p, subst);
            }
            substitute_type(&mut fn_type.return_type, subst);
        }
        _ => {}
    }
}

fn infer_generic_param_type(
    param_name: &str,
    def: &MacroDef,
    args: &[Expression],
) -> Option<ast::Type> {
    for (i, p) in def.parameters.iter().enumerate() {
        if type_uses_param(&p.param_type, param_name) {
            if p.is_variadic {
                if i < args.len() {
                    return infer_type_from_expr(&args[i]);
                }
            } else if i < args.len() {
                return infer_type_from_expr(&args[i]);
            }
        }
    }
    if let Some(first) = args.first() {
        infer_type_from_expr(first)
    } else {
        None
    }
}

fn type_uses_param(ty: &ast::Type, name: &str) -> bool {
    match ty.kind.as_ref() {
        ast::TypeKind::Named(named) => {
            if named.path.len() == 1 && named.path[0].name == name {
                return true;
            }
            if let Some(generics) = &named.generics {
                return generics.iter().any(|g| type_uses_param(g, name));
            }
            false
        }
        ast::TypeKind::Slice(slice) => type_uses_param(&slice.element_type, name),
        ast::TypeKind::Array(arr) => type_uses_param(&arr.element_type, name),
        ast::TypeKind::Pointer(ptr) => type_uses_param(&ptr.inner, name),
        ast::TypeKind::Optional(inner) => type_uses_param(inner, name),
        _ => false,
    }
}

fn infer_type_from_expr(expr: &Expression) -> Option<ast::Type> {
    match expr.kind.as_ref() {
        ExpressionKind::Literal(lit) => match lit {
            Literal::Integer(_) => Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::I32)),
                span: expr.span,
            }),
            Literal::Float(_) => Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::F64)),
                span: expr.span,
            }),
            Literal::String(_) => Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Str)),
                span: expr.span,
            }),
            Literal::Char(_) => Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Char)),
                span: expr.span,
            }),
            Literal::Bool(_) => Some(ast::Type {
                kind: Box::new(ast::TypeKind::Primitive(ast::PrimitiveType::Bool)),
                span: expr.span,
            }),
            _ => None,
        },
        ExpressionKind::TypeName(ty) => Some(ty.clone()),
        ExpressionKind::Cast { target_type, .. } => Some((**target_type).clone()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::Parser;

    fn parse_and_expand(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex ok");
        let mut parser = Parser::new(tokens);
        let (mut program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        expand_macros_in_program(&mut program);
        program
    }

    #[test]
    fn test_macro_comptime_eval() {
        let source = r#"
macro i32 add(i32 a, i32 b) {
    return a + b;
}
i32 test_fn() {
    return @add(15, 27);
}
"#;
        let program = parse_and_expand(source);
        for item in &program.items {
            if let ItemKind::Function(func) = &item.kind {
                if func.name.name == "test_fn" {
                    let ret = &func.body.statements[0];
                    if let StatementKind::Return(Some(expr)) = &ret.kind {
                        if let ExpressionKind::Literal(Literal::Integer(val)) = expr.kind.as_ref() {
                            assert_eq!(*val, 42);
                            return;
                        }
                    }
                }
            }
        }
        panic!("test_fn return value was not folded to 42");
    }

    #[test]
    fn test_variadic_macro_comptime_eval() {
        let source = r#"
macro i32 sum(i32... nums) {
    let s = 0;
    for n in nums {
        s = s + n;
    }
    return s;
}
i32 test_fn() {
    return @sum(1, 2, 3, 4);
}
"#;
        let program = parse_and_expand(source);
        for item in &program.items {
            if let ItemKind::Function(func) = &item.kind {
                if func.name.name == "test_fn" {
                    let ret = &func.body.statements[0];
                    if let StatementKind::Return(Some(expr)) = &ret.kind {
                        if let ExpressionKind::Literal(Literal::Integer(val)) = expr.kind.as_ref() {
                            assert_eq!(*val, 10);
                            return;
                        }
                    }
                }
            }
        }
        panic!("test_fn return value was not folded to 10");
    }

    #[test]
    fn test_macro_runtime_substitution() {
        let source = r#"
macro i32 add(i32 a, i32 b) {
    return a + b;
}
i32 test_fn(i32 x) {
    return @add(x, 5);
}
"#;
        let program = parse_and_expand(source);
        for item in &program.items {
            if let ItemKind::Function(func) = &item.kind {
                if func.name.name == "test_fn" {
                    let ret = &func.body.statements[0];
                    if let StatementKind::Return(Some(expr)) = &ret.kind {
                        if let ExpressionKind::Binary { left, operator, right: _ } = expr.kind.as_ref() {
                            assert_eq!(*operator, BinaryOperator::Add);
                            assert!(matches!(left.kind.as_ref(), ExpressionKind::Identifier(_)));
                            return;
                        }
                    }
                }
            }
        }
        panic!("test_fn return was not substituted to x + 5");
    }
}
