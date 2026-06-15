use std::collections::HashMap;

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::CodegenResult;
use crate::parser::ast;
use crate::semantic::typeck::TypeChecker;
use crate::types::Type;
use inkwell::values::BasicValueEnum;

pub trait MacroHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type;

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>>;
}

pub struct MacroRegistry {
    handlers: HashMap<String, Box<dyn MacroHandler>>,
}

impl MacroRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            handlers: HashMap::new(),
        };
        registry.register("size", Box::new(SizeHandler));
        registry
    }

    pub fn register(&mut self, name: &str, handler: Box<dyn MacroHandler>) {
        self.handlers.insert(name.to_string(), handler);
    }

    pub fn get(&self, name: &str) -> Option<&dyn MacroHandler> {
        self.handlers.get(name).map(|h| h.as_ref())
    }

    pub fn is_registered(&self, name: &str) -> bool {
        self.handlers.contains_key(name)
    }

    pub fn type_check(
        &self,
        name: &str,
        checker: &mut TypeChecker,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Option<Type> {
        self.get(name)
            .map(|h| h.type_check(checker, expr, args))
    }

    pub fn codegen<'ctx>(
        &self,
        name: &str,
        generator: &mut LlvmIrGenerator<'ctx>,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Option<CodegenResult<BasicValueEnum<'ctx>>> {
        self.get(name).map(|h| h.codegen(generator, expr, args))
    }
}

pub struct SizeHandler;

impl MacroHandler for SizeHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.size_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.size_codegen(expr, args)
    }
}

pub fn handle_typeck(
    name: &str,
    checker: &mut TypeChecker,
    expr: &ast::Expression,
    args: &[ast::MacroArg],
) -> Option<Type> {
    let registry = MacroRegistry::new();
    registry.type_check(name, checker, expr, args)
}

pub fn handle_codegen<'ctx>(
    name: &str,
    generator: &mut LlvmIrGenerator<'ctx>,
    expr: &ast::Expression,
    args: &[ast::MacroArg],
) -> Option<CodegenResult<BasicValueEnum<'ctx>>> {
    let registry = MacroRegistry::new();
    registry.codegen(name, generator, expr, args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{lex, Span};
    use crate::parser::Parser;
    use crate::parser::ast::{Expression, ExpressionKind, Identifier};

    struct MockHandler;

    impl MacroHandler for MockHandler {
        fn type_check(
            &self,
            _checker: &mut TypeChecker,
            _expr: &ast::Expression,
            _args: &[ast::MacroArg],
        ) -> Type {
            Type::Primitive(ast::PrimitiveType::U64)
        }

        fn codegen<'ctx>(
            &self,
            _generator: &mut LlvmIrGenerator<'ctx>,
            _expr: &ast::Expression,
            _args: &[ast::MacroArg],
        ) -> CodegenResult<BasicValueEnum<'ctx>> {
            unreachable!("codegen not expected in these tests")
        }
    }

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    fn dummy_expr() -> Expression {
        Expression {
            kind: Box::new(ExpressionKind::MacroCall {
                name: Identifier { name: String::new(), span: Span { start: 0, end: 0 } },
                args: vec![],
            }),
            span: Span { start: 0, end: 0 },
        }
    }

    #[test]
    fn registry_new_registers_size() {
        let registry = MacroRegistry::new();
        assert!(registry.is_registered("size"));
        assert!(!registry.is_registered("unknown_name"));
    }

    #[test]
    fn registry_register_and_get() {
        let mut registry = MacroRegistry::new();
        registry.register("mock", Box::new(MockHandler));
        assert!(registry.get("mock").is_some());
    }

    #[test]
    fn registry_get_returns_none_for_unknown() {
        let registry = MacroRegistry::new();
        assert!(registry.get("nonexistent").is_none());
    }

    #[test]
    fn registry_is_registered_checks_membership() {
        let mut registry = MacroRegistry::new();
        assert!(!registry.is_registered("tmp"));
        registry.register("tmp", Box::new(MockHandler));
        assert!(registry.is_registered("tmp"));
    }

    #[test]
    fn registry_type_check_dispatches_to_handler() {
        let mut registry = MacroRegistry::new();
        registry.register("mock", Box::new(MockHandler));

        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = registry.type_check("mock", &mut checker, &expr, &[]);

        assert_eq!(result, Some(Type::Primitive(ast::PrimitiveType::U64)));
    }

    #[test]
    fn registry_type_check_returns_none_for_unknown() {
        let registry = MacroRegistry::new();
        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = registry.type_check("unknown", &mut checker, &expr, &[]);
        assert!(result.is_none());
    }

    #[test]
    fn size_handler_typeck_with_type_name() {
        let program = parse("i32 main() { return @size(i32); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn size_handler_typeck_with_expression() {
        let program = parse("i32 main() { i32 x = 0; return @size(x); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn size_handler_typeck_with_struct_type_name() {
        let program = parse("struct Point { i32 x; i32 y; } i32 main() { return @size(Point); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn size_handler_typeck_with_generic_type_param() {
        let program = parse("u64 foo<T>() { return @size(T); } i32 main() { foo<i32>(); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn handle_typeck_free_function() {
        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = handle_typeck("size", &mut checker, &expr, &[]);
        assert!(result.is_some(), "handle_typeck should return Some for registered macro");
    }

    #[test]
    fn handle_typeck_returns_none_for_unknown() {
        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = handle_typeck("nonexistent", &mut checker, &expr, &[]);
        assert!(result.is_none());
    }
}
