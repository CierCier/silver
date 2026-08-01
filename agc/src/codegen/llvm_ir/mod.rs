use std::collections::HashSet;

use rustc_hash::FxHashMap as HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, PointerValue};

use crate::codegen::abi::AbiHandler;
use crate::debug_info::DebugContext;
use crate::parser::ast;
use crate::symbol_table::{CompilerSymbolTable, SymbolId};
use crate::types::Type;

#[derive(Clone)]
pub(crate) struct FunctionSig {
    pub(crate) params: Vec<ast::Type>,
    pub(crate) return_type: Option<ast::Type>,
    pub(crate) is_variadic: bool,
    pub(crate) linkage: Option<ast::ExternLinkage>,
}

impl PartialEq for FunctionSig {
    fn eq(&self, other: &Self) -> bool {
        self.is_variadic == other.is_variadic
            && self.linkage == other.linkage
            && self.params.len() == other.params.len()
            && self
                .params
                .iter()
                .zip(other.params.iter())
                .all(|(lhs, rhs)| {
                    Type::from_ast(lhs).canonical_key() == Type::from_ast(rhs).canonical_key()
                })
            && match (&self.return_type, &other.return_type) {
                (None, None) => true,
                (Some(lhs), Some(rhs)) => {
                    Type::from_ast(lhs).canonical_key() == Type::from_ast(rhs).canonical_key()
                }
                _ => false,
            }
    }
}

#[derive(Clone)]
pub(crate) struct VarInfo<'ctx> {
    pub(crate) ptr: PointerValue<'ctx>,
    pub(crate) ty: ast::Type,
    pub(crate) is_mutable: bool,
    pub(crate) is_volatile: bool,
}

#[derive(Clone)]
pub(crate) enum DeferAction<'ctx> {
    /// Execute a parsed AST statement (from `defer { }` or `defer stmt;`)
    Statement(ast::Statement),
    /// Call the drop function for a variable: (mangled_fn_name, ptr_to_var)
    DropCall(String, PointerValue<'ctx>),
}

#[derive(Clone)]
pub(crate) struct DeferredEntry<'ctx> {
    pub(crate) action: DeferAction<'ctx>,
    /// Optional i1* drop flag; if set, action only executes when the flag is true
    pub(crate) flag: Option<PointerValue<'ctx>>,
}

pub struct LlvmIrGenerator<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) current_fn: Option<FunctionValue<'ctx>>,
    pub(crate) current_return_type: Option<ast::Type>,
    pub(crate) variables: Vec<HashMap<String, VarInfo<'ctx>>>,
    pub(crate) function_sigs: HashMap<SymbolId, FunctionSig>,
    pub(crate) function_name_to_symbol: HashMap<String, SymbolId>,
    pub(crate) imported_function_links: HashMap<String, String>,
    pub(crate) extern_globals: HashMap<String, ast::Type>,
    pub(crate) global_variables: HashMap<String, ast::Type>,
    pub(crate) struct_types: HashMap<String, StructType<'ctx>>,
    pub(crate) struct_fields: HashMap<String, Vec<(String, ast::Type)>>,
    pub(crate) enum_backing_types: HashMap<String, ast::PrimitiveType>,
    pub(crate) enum_variants: HashMap<String, HashMap<String, i128>>,
    pub(crate) enum_payload_layouts: HashMap<String, StructType<'ctx>>,
    pub(crate) enum_variant_payload_types: HashMap<String, HashMap<String, Vec<ast::Type>>>,
    pub(crate) defers: Vec<Vec<DeferredEntry<'ctx>>>,
    pub(crate) drop_flags: HashMap<String, PointerValue<'ctx>>,
    pub(crate) volatile_globals: HashSet<String>,
    pub(crate) static_local_counter: usize,
    pub(crate) method_receivers: HashMap<(String, String), bool>,
    pub(crate) string_constants: HashMap<String, PointerValue<'ctx>>,
    pub(crate) struct_generics: HashMap<String, Vec<String>>,
    pub(crate) generic_impl_templates: Vec<ast::ImplItem>,
    pub(crate) drop_trait_impl_owners: HashSet<String>,
    pub(crate) loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    pub(crate) loop_defers_base: Vec<usize>,
    pub(crate) symbol_table: CompilerSymbolTable,
    pub(crate) debug: Option<DebugContext<'ctx>>,
    pub(crate) abi_handler: Box<dyn AbiHandler>,
    pub(crate) temp_counter: u64,
    pub(crate) leak_check: bool,
    /// (LLVM function name, doc comment text) pairs collected while
    /// generating; spliced into the printed IR in `finish()` as `;` lines.
    pub(crate) doc_comments: Vec<(String, String)>,
}

pub(crate) mod call;
pub(crate) mod entry;
pub(crate) mod expr;
pub(crate) mod generate;
pub(crate) mod operators;
pub(crate) mod scope;
pub(crate) mod stmt;
pub(crate) mod symbols;
pub(crate) mod types;

#[cfg(test)]
mod tests;
