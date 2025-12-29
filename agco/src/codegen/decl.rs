use super::CodeGen;
use inkwell::types::BasicMetadataTypeEnum;

pub fn compile_decl<'ctx>(codegen: &mut CodeGen<'ctx>, decl: &crate::ast::Decl) {
    match decl {
        crate::ast::Decl::Func {
            name,
            params,
            ret,
            body,
            ..
        } => {
            // Prepared argument types
            let mut arg_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            for param in params {
                let ty = codegen.get_llvm_type(&param.ty);
                arg_types.push(ty.into());
            }

            // Return type
            // TODO: Void return type handling?
            let fn_type = if ret.name == "void" {
                codegen.context.void_type().fn_type(&arg_types, false)
            } else {
                let ret_ty = codegen.get_llvm_type(ret);
                ret_ty.fn_type(&arg_types, false)
            };

            let function = codegen.module.add_function(name, fn_type, None);

            // If body exists, compile it
            if let Some(stmts) = body {
                let entry = codegen.context.append_basic_block(function, "entry");
                codegen.builder.position_at_end(entry);

                // Param mapping to variables would happen here

                for stmt in stmts {
                    super::stmt::compile_stmt(codegen, stmt);
                }
            }
        }
        _ => {
            // TODO: Structs, Imports, etc.
        }
    }
}
