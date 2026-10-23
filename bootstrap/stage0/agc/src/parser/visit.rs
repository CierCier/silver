use super::ast::{
    Block, Expression, ExpressionKind, ImplItemKind, InitializerItem, ItemKind, MacroArg, Program,
    Statement, StatementKind,
};

/// Visit expressions from the leaves outward, including nested statement bodies.
pub fn expressions_in_program(program: &mut Program, visit: &mut impl FnMut(&mut Expression)) {
    for item in &mut program.items {
        match &mut item.kind {
            ItemKind::Function(function) => expressions_in_block(&mut function.body, visit),
            ItemKind::GlobalVariable(variable) => {
                if let Some(value) = &mut variable.initializer {
                    expression(value, visit);
                }
            }
            ItemKind::Impl(implementation) => {
                for member in &mut implementation.items {
                    match member {
                        ImplItemKind::Function(function) => {
                            expressions_in_block(&mut function.body, visit)
                        }
                        ImplItemKind::Cast(cast) => expressions_in_block(&mut cast.body, visit),
                        ImplItemKind::AssociatedType(_) => {}
                    }
                }
            }
            ItemKind::Macro(definition) => expressions_in_block(&mut definition.body, visit),
            _ => {}
        }
    }
}

pub fn expressions_in_block(block: &mut Block, visit: &mut impl FnMut(&mut Expression)) {
    for statement in &mut block.statements {
        statement_expressions(statement, visit);
    }
}

fn statement_expressions(statement: &mut Statement, visit: &mut impl FnMut(&mut Expression)) {
    match &mut statement.kind {
        StatementKind::Block(block) => expressions_in_block(block, visit),
        StatementKind::Expression(value)
        | StatementKind::Return(Some(value))
        | StatementKind::Break(Some(value)) => expression(value, visit),
        StatementKind::Let(binding) => {
            if let Some(value) = &mut binding.initializer {
                expression(value, visit);
            }
        }
        StatementKind::Defer(statement) => statement_expressions(statement, visit),
        StatementKind::Return(None) | StatementKind::Break(None) | StatementKind::Continue => {}
    }
}

pub fn expression(value: &mut Expression, visit: &mut impl FnMut(&mut Expression)) {
    match value.kind.as_mut() {
        ExpressionKind::Lambda { body, .. } | ExpressionKind::Block(body) => {
            expressions_in_block(body, visit)
        }
        ExpressionKind::Binary { left, right, .. } => {
            expression(left, visit);
            expression(right, visit);
        }
        ExpressionKind::Unary { operand, .. }
        | ExpressionKind::Postfix { operand, .. }
        | ExpressionKind::Move(operand)
        | ExpressionKind::Comptime(operand)
        | ExpressionKind::Launch(operand)
        | ExpressionKind::Wait(operand)
        | ExpressionKind::Cast {
            expression: operand,
            ..
        }
        | ExpressionKind::Reference {
            expression: operand,
            ..
        } => expression(operand, visit),
        ExpressionKind::Call {
            function,
            arguments,
        } => {
            expression(function, visit);
            for argument in arguments {
                expression(argument, visit);
            }
        }
        ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            expression(receiver, visit);
            for argument in arguments {
                expression(argument, visit);
            }
        }
        ExpressionKind::FieldAccess { object, .. } => expression(object, visit),
        ExpressionKind::Index { object, index } => {
            expression(object, visit);
            expression(index, visit);
        }
        ExpressionKind::Slice {
            object,
            start,
            end,
            step,
        } => {
            expression(object, visit);
            for bound in [start, end, step].into_iter().flatten() {
                expression(bound, visit);
            }
        }
        ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            expression(condition, visit);
            expressions_in_block(then_branch, visit);
            if let Some(body) = else_branch {
                expressions_in_block(body, visit);
            }
        }
        ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            expression(condition, visit);
            expression(then_expr, visit);
            expression(else_expr, visit);
        }
        ExpressionKind::UnwrapOr { value, fallback } => {
            expression(value, visit);
            expression(fallback, visit);
        }
        ExpressionKind::While { condition, body } => {
            expression(condition, visit);
            expressions_in_block(body, visit);
        }
        ExpressionKind::ForIn { iterable, body, .. } => {
            expression(iterable, visit);
            expressions_in_block(body, visit);
        }
        ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(value) = &mut init.initializer {
                expression(value, visit);
            }
            expression(condition, visit);
            expression(increment, visit);
            expressions_in_block(body, visit);
        }
        ExpressionKind::Match {
            expression: scrutinee,
            arms,
        } => {
            expression(scrutinee, visit);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    expression(guard, visit);
                }
                expression(&mut arm.body, visit);
            }
        }
        ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    InitializerItem::Positional(value) | InitializerItem::Field { value, .. } => {
                        expression(value, visit)
                    }
                    InitializerItem::Index { index, value } => {
                        expression(index, visit);
                        expression(value, visit);
                    }
                }
            }
        }
        ExpressionKind::Array(values) | ExpressionKind::Tuple(values) => {
            for value in values {
                expression(value, visit);
            }
        }
        ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                expression(&mut field.value, visit);
            }
        }
        ExpressionKind::EnumVariant { fields, .. } => {
            for field in fields {
                expression(field, visit);
            }
        }
        ExpressionKind::Asm { inputs, .. } => {
            for input in inputs {
                expression(input, visit);
            }
        }
        ExpressionKind::MacroCall { args, .. } => {
            for argument in args {
                if let MacroArg::Expression(value) = argument {
                    expression(value, visit);
                }
            }
        }
        ExpressionKind::Literal(_)
        | ExpressionKind::Identifier(_)
        | ExpressionKind::TypeName(_) => {}
    }
    visit(value);
}
