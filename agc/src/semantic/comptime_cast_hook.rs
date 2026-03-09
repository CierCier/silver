use crate::parser::ast;

use super::analyzer::{SemanticAnalyzerHook, SemanticError};

#[derive(Default)]
pub struct ComptimeCastHook;

impl ComptimeCastHook {
    pub fn new() -> Self {
        Self
    }
}

impl SemanticAnalyzerHook for ComptimeCastHook {
    fn after_analysis(&mut self, program: &mut ast::Program, _errors: &[SemanticError]) {
        for item in &mut program.items {
            rewrite_item(item);
        }
    }
}

fn rewrite_item(item: &mut ast::Item) {
    match &mut item.kind {
        ast::ItemKind::Function(function) => rewrite_block(&mut function.body),
        ast::ItemKind::Impl(impl_item) => {
            for impl_member in &mut impl_item.items {
                match impl_member {
                    ast::ImplItemKind::Function(function) => rewrite_block(&mut function.body),
                    ast::ImplItemKind::Cast(cast) => rewrite_block(&mut cast.body),
                    ast::ImplItemKind::AssociatedType(_) => {}
                }
            }
        }
        _ => {}
    }
}

fn rewrite_block(block: &mut ast::Block) {
    for statement in &mut block.statements {
        rewrite_statement(statement);
    }
}

fn rewrite_statement(statement: &mut ast::Statement) {
    match &mut statement.kind {
        ast::StatementKind::Block(block) => rewrite_block(block),
        ast::StatementKind::Let(let_stmt) => {
            if let Some(initializer) = &mut let_stmt.initializer {
                rewrite_expression(initializer);
            }
        }
        ast::StatementKind::Expression(expression) => rewrite_expression(expression),
        ast::StatementKind::Return(value) | ast::StatementKind::Break(value) => {
            if let Some(expression) = value {
                rewrite_expression(expression);
            }
        }
        ast::StatementKind::Continue => {}
    }
}

fn rewrite_expression(expression: &mut ast::Expression) {
    match expression.kind.as_mut() {
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::Identifier(_)
        | ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Asm(_)
        | ast::ExpressionKind::MacroCall { .. } => {}
        ast::ExpressionKind::Binary { left, right, .. } => {
            rewrite_expression(left);
            rewrite_expression(right);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Reference {
            expression: operand,
            ..
        } => rewrite_expression(operand),
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            rewrite_expression(function);
            for argument in arguments {
                rewrite_expression(argument);
            }
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            rewrite_expression(receiver);
            for argument in arguments {
                rewrite_expression(argument);
            }
        }
        ast::ExpressionKind::FieldAccess { object, .. } => rewrite_expression(object),
        ast::ExpressionKind::Index { object, index } => {
            rewrite_expression(object);
            rewrite_expression(index);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rewrite_expression(condition);
            rewrite_block(then_branch);
            if let Some(else_branch) = else_branch {
                rewrite_block(else_branch);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            rewrite_expression(condition);
            rewrite_block(body);
        }
        ast::ExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            if let Some(initializer) = &mut init.initializer {
                rewrite_expression(initializer);
            }
            rewrite_expression(condition);
            rewrite_expression(increment);
            rewrite_block(body);
        }
        ast::ExpressionKind::Match { expression, arms } => {
            rewrite_expression(expression);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_expression(guard);
                }
                rewrite_expression(&mut arm.body);
            }
        }
        ast::ExpressionKind::Block(block) => rewrite_block(block),
        ast::ExpressionKind::Initializer { items } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(value)
                    | ast::InitializerItem::Field { value, .. } => rewrite_expression(value),
                    ast::InitializerItem::Index { index, value } => {
                        rewrite_expression(index);
                        rewrite_expression(value);
                    }
                }
            }
        }
        ast::ExpressionKind::Array(items) | ast::ExpressionKind::Tuple(items) => {
            for item in items {
                rewrite_expression(item);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_expression(&mut field.value);
            }
        }
        ast::ExpressionKind::Cast { expression, .. } => rewrite_expression(expression),
        ast::ExpressionKind::Comptime(inner) => {
            rewrite_expression(inner);
            if let Some(folded) = fold_comptime_cast(inner) {
                *expression = folded;
            }
        }
    }
}

fn fold_comptime_cast(inner: &ast::Expression) -> Option<ast::Expression> {
    let ast::ExpressionKind::Cast {
        expression,
        target_type,
    } = inner.kind.as_ref()
    else {
        return None;
    };
    let ast::ExpressionKind::Literal(source_literal) = expression.kind.as_ref() else {
        return None;
    };
    let ast::TypeKind::Primitive(target_primitive) = target_type.kind.as_ref() else {
        return None;
    };

    let folded_literal = cast_literal(source_literal, target_primitive)?;
    Some(ast::Expression {
        kind: Box::new(ast::ExpressionKind::Literal(folded_literal)),
        span: inner.span.clone(),
    })
}

fn cast_literal(source: &ast::Literal, target: &ast::PrimitiveType) -> Option<ast::Literal> {
    match target {
        ast::PrimitiveType::Bool => Some(ast::Literal::Bool(match source {
            ast::Literal::Bool(value) => *value,
            ast::Literal::Integer(value) => *value != 0,
            ast::Literal::Float(value) => *value != 0.0,
            ast::Literal::Char(value) => *value != '\0',
            _ => return None,
        })),
        ast::PrimitiveType::Char => match source {
            ast::Literal::Char(value) => Some(ast::Literal::Char(*value)),
            ast::Literal::Integer(value) => {
                let code = u32::try_from(*value).ok()?;
                let value = char::from_u32(code)?;
                Some(ast::Literal::Char(value))
            }
            _ => None,
        },
        ast::PrimitiveType::F32 | ast::PrimitiveType::F64 | ast::PrimitiveType::F80 => {
            let mut value = match source {
                ast::Literal::Float(value) => *value,
                ast::Literal::Integer(value) => *value as f64,
                ast::Literal::Bool(value) => {
                    if *value {
                        1.0
                    } else {
                        0.0
                    }
                }
                ast::Literal::Char(value) => u32::from(*value) as f64,
                _ => return None,
            };
            if matches!(target, ast::PrimitiveType::F32) {
                value = (value as f32) as f64;
            }
            Some(ast::Literal::Float(value))
        }
        _ => {
            let (is_signed, bits) = int_target_spec(target)?;
            let int_value = source_to_i128(source)?;
            if is_signed {
                Some(ast::Literal::Integer(cast_i128_to_signed_bits(
                    int_value, bits,
                )))
            } else {
                let unsigned = cast_i128_to_unsigned_bits(int_value, bits);
                if unsigned > i128::MAX as u128 {
                    return None;
                }
                Some(ast::Literal::Integer(unsigned as i128))
            }
        }
    }
}

fn int_target_spec(target: &ast::PrimitiveType) -> Option<(bool, u16)> {
    match target {
        ast::PrimitiveType::I8 => Some((true, 8)),
        ast::PrimitiveType::I16 => Some((true, 16)),
        ast::PrimitiveType::I32 => Some((true, 32)),
        ast::PrimitiveType::I64 => Some((true, 64)),
        ast::PrimitiveType::I128 => Some((true, 128)),
        ast::PrimitiveType::U8 => Some((false, 8)),
        ast::PrimitiveType::U16 => Some((false, 16)),
        ast::PrimitiveType::U32 => Some((false, 32)),
        ast::PrimitiveType::U64 => Some((false, 64)),
        ast::PrimitiveType::U128 => Some((false, 128)),
        _ => None,
    }
}

fn source_to_i128(source: &ast::Literal) -> Option<i128> {
    match source {
        ast::Literal::Integer(value) => Some(*value),
        ast::Literal::Float(value) => {
            if !value.is_finite() {
                return None;
            }
            Some(*value as i128)
        }
        ast::Literal::Bool(value) => {
            if *value {
                Some(1)
            } else {
                Some(0)
            }
        }
        ast::Literal::Char(value) => Some(u32::from(*value) as i128),
        _ => None,
    }
}

fn cast_i128_to_signed_bits(value: i128, bits: u16) -> i128 {
    match bits {
        8 => (value as i8) as i128,
        16 => (value as i16) as i128,
        32 => (value as i32) as i128,
        64 => (value as i64) as i128,
        128 => value,
        _ => value,
    }
}

fn cast_i128_to_unsigned_bits(value: i128, bits: u16) -> u128 {
    match bits {
        8 => (value as u8) as u128,
        16 => (value as u16) as u128,
        32 => (value as u32) as u128,
        64 => (value as u64) as u128,
        128 => value as u128,
        _ => value as u128,
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lex;
    use crate::parser::Parser;
    use crate::semantic::analyzer::Analyzer;

    use super::*;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lexer should succeed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parser errors: {errors:?}");
        program
    }

    fn run_hook(program: &mut ast::Program) {
        let mut analyzer = Analyzer::new();
        let mut hook = ComptimeCastHook::new();
        let mut hooks: [&mut dyn SemanticAnalyzerHook; 1] = [&mut hook];
        let errors = analyzer.analyze_program_with_hooks(program, &mut hooks);
        assert!(errors.is_empty(), "semantic errors: {errors:?}");
    }

    #[test]
    fn folds_comptime_cast_literal_to_integer_literal() {
        let mut program = parse("i32 main() { return comptime (i32) 3.9; }");
        run_hook(&mut program);

        let ast::ItemKind::Function(function) = &program.items[0].kind else {
            panic!("expected function item");
        };
        let ast::StatementKind::Return(Some(expression)) = &function.body.statements[0].kind else {
            panic!("expected return expression");
        };
        let ast::ExpressionKind::Literal(ast::Literal::Integer(value)) = expression.kind.as_ref()
        else {
            panic!("expected folded integer literal");
        };
        assert_eq!(*value, 3);
    }

    #[test]
    fn folds_comptime_cast_literal_to_bool_literal() {
        let mut program = parse("bool main() { return comptime (bool) 0; }");
        run_hook(&mut program);

        let ast::ItemKind::Function(function) = &program.items[0].kind else {
            panic!("expected function item");
        };
        let ast::StatementKind::Return(Some(expression)) = &function.body.statements[0].kind else {
            panic!("expected return expression");
        };
        let ast::ExpressionKind::Literal(ast::Literal::Bool(value)) = expression.kind.as_ref()
        else {
            panic!("expected folded bool literal");
        };
        assert!(!*value);
    }

    #[test]
    fn leaves_non_literal_comptime_cast_untouched() {
        let mut program = parse("i32 main(i32 x) { return comptime (i32) x; }");
        run_hook(&mut program);

        let ast::ItemKind::Function(function) = &program.items[0].kind else {
            panic!("expected function item");
        };
        let ast::StatementKind::Return(Some(expression)) = &function.body.statements[0].kind else {
            panic!("expected return expression");
        };
        assert!(
            matches!(expression.kind.as_ref(), ast::ExpressionKind::Comptime(_)),
            "non-literal comptime cast should stay as comptime"
        );
    }
}
