use super::CodeGen;

// Stub for Expression Codegen
pub fn compile_expr<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    _expr: &crate::ast::Expr,
) -> inkwell::values::BasicValueEnum<'ctx> {
    unimplemented!()
}
