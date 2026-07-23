use rustc_hash::FxHashMap as HashMap;

use crate::codegen::CodegenResult;
use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::parser::ast;
use crate::semantic::typeck::TypeChecker;
use crate::types::Type;
use inkwell::values::BasicValueEnum;

pub trait MacroHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type;

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>>;
}

pub struct MacroRegistry {
    handlers: HashMap<String, Box<dyn MacroHandler>>,
}

impl Default for MacroRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            handlers: HashMap::default(),
        };
        registry.register("size", Box::new(SizeHandler));
        registry.register("align", Box::new(AlignHandler));
        registry.register("hash", Box::new(HashHandler));
        registry.register("print", Box::new(PrintHandler));
        registry.register("println", Box::new(PrintHandler));
        registry.register("eprint", Box::new(PrintHandler));
        registry.register("eprintln", Box::new(PrintHandler));
        registry.register("fprint", Box::new(PrintHandler));
        registry.register("sprint", Box::new(PrintHandler));
        registry.register("memcpy", Box::new(MemcpyHandler));
        registry.register("memset", Box::new(MemsetHandler));
        registry.register("memmove", Box::new(MemmoveHandler));
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
            .map(|h| h.type_check(checker, name, expr, args))
    }

    pub fn codegen<'ctx>(
        &self,
        name: &str,
        generator: &mut LlvmIrGenerator<'ctx>,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Option<CodegenResult<BasicValueEnum<'ctx>>> {
        self.get(name)
            .map(|h| h.codegen(generator, name, expr, args))
    }
}

pub struct SizeHandler;

impl MacroHandler for SizeHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.size_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.size_codegen(expr, args)
    }
}
pub struct AlignHandler;

impl MacroHandler for AlignHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.align_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.align_codegen(expr, args)
    }
}
pub struct HashHandler;

impl MacroHandler for HashHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.hash_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.hash_codegen(expr, args)
    }
}
pub struct MemcpyHandler;

impl MacroHandler for MemcpyHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.memcpy_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.memcpy_codegen(expr, args)
    }
}

pub struct MemsetHandler;

impl MacroHandler for MemsetHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.memset_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.memset_codegen(expr, args)
    }
}

pub struct MemmoveHandler;

impl MacroHandler for MemmoveHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.memmove_typeck(expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        _name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.memmove_codegen(expr, args)
    }
}

// ---------------------------------------------------------------------------
// Format string parsing & @print macro family
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FormatSegment {
    Literal(String),
    Placeholder,
}

/// Parse a format string into segments.
/// `{{` and `}}` produce literal braces; `{}` produces Placeholder.
pub(crate) fn parse_format(fmt: &str) -> Vec<FormatSegment> {
    let mut segments = Vec::new();
    let mut chars = fmt.chars().peekable();
    let mut literal = String::new();

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped brace: {{
                chars.next();
                literal.push('{');
            } else if chars.peek() == Some(&'}') {
                // Placeholder: {}
                chars.next();
                if !literal.is_empty() {
                    segments.push(FormatSegment::Literal(std::mem::take(&mut literal)));
                }
                segments.push(FormatSegment::Placeholder);
            } else {
                literal.push(ch);
            }
        } else if ch == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped brace: }}
                chars.next();
                literal.push('}');
            } else {
                literal.push(ch);
            }
        } else {
            literal.push(ch);
        }
    }

    if !literal.is_empty() {
        segments.push(FormatSegment::Literal(literal));
    }

    segments
}

/// Handler for @print, @println, @eprint, @eprintln, @fprint, @sprint
pub(crate) struct PrintHandler;

impl MacroHandler for PrintHandler {
    fn type_check(
        &self,
        checker: &mut TypeChecker,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> Type {
        checker.print_typeck(name, expr, args)
    }

    fn codegen<'ctx>(
        &self,
        generator: &mut LlvmIrGenerator<'ctx>,
        name: &str,
        expr: &ast::Expression,
        args: &[ast::MacroArg],
    ) -> CodegenResult<BasicValueEnum<'ctx>> {
        generator.print_codegen(name, expr, args)
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
    use crate::lexer::{Span, lex};
    use crate::parser::Parser;
    use crate::parser::ast::{Expression, ExpressionKind, Identifier};

    struct MockHandler;

    impl MacroHandler for MockHandler {
        fn type_check(
            &self,
            _checker: &mut TypeChecker,
            _name: &str,
            _expr: &ast::Expression,
            _args: &[ast::MacroArg],
        ) -> Type {
            Type::Primitive(ast::PrimitiveType::U64)
        }

        fn codegen<'ctx>(
            &self,
            _generator: &mut LlvmIrGenerator<'ctx>,
            _name: &str,
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
                name: Identifier {
                    name: String::new(),
                    span: Span { start: 0, end: 0 },
                },
                args: vec![],
            }),
            span: Span { start: 0, end: 0 },
        }
    }

    #[test]
    fn registry_new_registers_builtin_macros() {
        let registry = MacroRegistry::new();
        assert!(registry.is_registered("size"));
        assert!(registry.is_registered("align"));
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
        let program =
            parse("u64 foo<T>() { return @size(T); } i32 main() { foo<i32>(); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn align_handler_typeck_with_type_name() {
        let program = parse("i32 main() { return @align(i32); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn align_handler_typeck_with_expression() {
        let program = parse("i32 main() { i32 x = 0; return @align(x); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn align_handler_typeck_with_struct_type_name() {
        let program =
            parse("struct Point { i8 tag; i64 value; } i32 main() { return @align(Point); }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn align_handler_typeck_with_generic_type_param() {
        let program =
            parse("u64 foo<T>() { return @align(T); } i32 main() { foo<i32>(); return 0; }");
        let (errors, _) = TypeChecker::new().check_program(&program);
        assert!(errors.is_empty(), "type errors: {errors:?}");
    }

    #[test]
    fn handle_typeck_free_function() {
        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = handle_typeck("size", &mut checker, &expr, &[]);
        assert!(
            result.is_some(),
            "handle_typeck should return Some for registered macro"
        );
    }

    #[test]
    fn handle_typeck_returns_none_for_unknown() {
        let mut checker = TypeChecker::new();
        let expr = dummy_expr();
        let result = handle_typeck("nonexistent", &mut checker, &expr, &[]);
        assert!(result.is_none());
    }
}
