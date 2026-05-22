use std::collections::HashMap;

use crate::codegen::llvm_ir::LlvmIrGenerator;
use crate::codegen::{CodegenError, CodegenResult};
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
