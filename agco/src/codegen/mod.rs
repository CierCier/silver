use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use std::any::Any;
use std::collections::HashMap;

pub mod decl;
pub mod expr;
pub mod stmt;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub variables: HashMap<String, inkwell::values::PointerValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
        }
    }

    pub fn get_llvm_type(&self, ty: &crate::ast::TypeName) -> BasicTypeEnum<'ctx> {
        // TODO: Handle pointers, arrays, generics
        match ty.name.as_str() {
            "i8" => self.context.i8_type().into(),
            "i16" => self.context.i16_type().into(),
            "i32" => self.context.i32_type().into(),
            "i64" => self.context.i64_type().into(),
            "u8" => self.context.i8_type().into(),
            "u16" => self.context.i16_type().into(),
            "u32" => self.context.i32_type().into(),
            "u64" => self.context.i64_type().into(),
            "f32" => self.context.f32_type().into(),
            "f64" => self.context.f64_type().into(),
            "bool" => self.context.bool_type().into(),
            "void" => self.context.void_type().into(), // Void is often i8 or void depending on usage (void for return, i8 for ptr?)
            _ => panic!("Unknown type: {}", ty.name),
        }
    }
}
