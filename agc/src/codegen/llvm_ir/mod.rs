use rustc_hash::FxHashSet as HashSet;

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

/// Canonical signature of a free function, used to decide whether a name is
/// overloaded and to compute its collision-safe symbol hash. Keys are the
/// PRE-sanitization canonical type keys of concrete types.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct FreeFunctionSig {
    pub(crate) params: Vec<String>,
    pub(crate) return_type: Option<String>,
    pub(crate) is_variadic: bool,
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
    /// Drop flag (i1*): cleared on move/by-value transfer, checked at scope
    /// exit. Lives on the variable (not a name-keyed map) so shadowed
    /// bindings never clear each other's flags.
    pub(crate) drop_flag: Option<PointerValue<'ctx>>,
    /// Per-field Drop flags (i1* each, initialized false = "field holds no
    /// live value yet"), for struct-typed variables. Keyed by the dotted
    /// field path ("f", "f.g", …) as registered by register_field_drops;
    /// set true when the field is assigned, checked by the scope-exit field
    /// cascade and the assignment pre-drop. This is the definite-init
    /// tracking that keeps uninitialized fields from being spuriously
    /// dropped.
    pub(crate) field_flags: Vec<(String, PointerValue<'ctx>)>,
}

#[derive(Clone)]
#[expect(
    clippy::large_enum_variant,
    reason = "defer payloads are emitted once; boxing adds indirection for no benefit"
)]
pub(crate) enum DeferAction<'ctx> {
    /// Execute a parsed AST statement (from `defer { }` or `defer stmt;`)
    Statement(ast::Statement),
    /// Call the drop function for a variable: (mangled_fn_name, ptr_to_var)
    DropCall(String, PointerValue<'ctx>),
    /// Tag-aware drop of an enum's payload: (enum_type, ptr_to_enum).
    /// Switches on the i16 tag and drops the active variant's Drop-typed
    /// payload values (for enums WITHOUT a Drop impl of their own — those
    /// manage payloads in the drop body).
    EnumPayloadDrop(ast::Type, PointerValue<'ctx>),
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
    /// Distinct full signatures per source function name (params + return +
    /// variadic, as canonical keys). Names with more than one signature get
    /// hash-suffixed symbols; single-signature names keep their plain name.
    pub(crate) free_function_sigs: HashMap<String, Vec<FreeFunctionSig>>,
    /// Source function name -> every LLVM symbol registered for it. Used at
    /// call sites to enumerate overload candidates.
    pub(crate) source_function_symbols: HashMap<String, Vec<String>>,
    pub(crate) imported_function_links: HashMap<String, String>,
    pub(crate) extern_globals: HashMap<String, ast::Type>,
    pub(crate) global_variables: HashMap<String, ast::Type>,
    /// Immutable global constants that hold a single integer literal, keyed by
    /// name; used by `__atomic_*` to fold an ordering argument that names a
    /// constant (e.g. `seq_cst`) into a literal, since LLVM atomic instructions
    /// require a compile-time ordering.
    pub(crate) global_const_values: HashMap<String, i128>,
    pub(crate) struct_types: HashMap<String, StructType<'ctx>>,
    pub(crate) struct_fields: HashMap<String, Vec<(String, ast::Type)>>,
    pub(crate) union_types: HashSet<String>,
    pub(crate) enum_backing_types: HashMap<String, ast::PrimitiveType>,
    pub(crate) enum_variants: HashMap<String, HashMap<String, i128>>,
    pub(crate) enum_payload_layouts: HashMap<String, StructType<'ctx>>,
    pub(crate) enum_variant_payload_types: HashMap<String, HashMap<String, Vec<ast::Type>>>,
    pub(crate) defers: Vec<Vec<DeferredEntry<'ctx>>>,
    pub(crate) volatile_globals: HashSet<String>,
    /// Names declared as `type X = ...` aliases; consulted by the generic-
    /// placeholder classifier so an alias may share a generic-param name.
    pub(crate) type_aliases: HashSet<String>,
    pub(crate) static_local_counter: usize,
    pub(crate) method_receivers: HashMap<(String, String), bool>,
    /// Distinct full signatures per `(owner, method)` (params + return +
    /// variadic, as canonical keys). Every method symbol carries an
    /// FNV-1a-64 hash of its full signature, so distinct methods always get
    /// distinct symbols (see crate::mangling).
    pub(crate) method_overload_signatures: HashMap<(String, String), Vec<FreeFunctionSig>>,
    pub(crate) string_constants: HashMap<String, PointerValue<'ctx>>,
    pub(crate) struct_generics: HashMap<String, Vec<String>>,
    pub(crate) generic_impl_templates: Vec<ast::ImplItem>,
    /// Generic free-function templates (functions with `generics`), keyed by
    /// name. The lazy generic-impl instantiation path emits nested instances
    /// (e.g. `realloc<i64>` inside `Vec<i64>.push`) that the semantic
    /// monomorph pass never requested.
    pub(crate) generic_function_templates: HashMap<String, ast::FunctionItem>,
    pub(crate) drop_trait_impl_owners: HashSet<String>,
    pub(crate) loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    pub(crate) loop_defers_base: Vec<usize>,
    pub(crate) symbol_table: CompilerSymbolTable,
    pub(crate) debug: Option<DebugContext<'ctx>>,
    /// True while a generic instance is being emitted mid-codegen (inside
    /// another function's body). Nested emissions get no debug info: their
    /// scopes would dangle under LLVM 22's DbgRecord DIE construction.
    pub(crate) debug_nested: bool,
    /// llvm symbol name -> (source file basename, declaration line) for the
    /// runtime backtrace table (works without -g: the lexer's source
    /// registry holds the file path, and spans carry precomputed lines).
    pub(crate) fn_source_info: rustc_hash::FxHashMap<String, (String, u32)>,
    pub(crate) abi_handler: Box<dyn AbiHandler>,
    pub(crate) temp_counter: u64,
    /// Monotonic counter for per-launch-site trampoline function names.
    pub(crate) task_trampoline_counter: u64,
    pub(crate) leak_check: bool,
    /// (LLVM function name, doc comment text) pairs collected while
    /// generating; spliced into the printed IR in `finish()` as `;` lines.
    pub(crate) doc_comments: Vec<(String, String)>,
}

pub(crate) mod call;
pub(crate) mod entry;
pub(crate) mod expr;
pub(crate) mod generate;
pub(crate) mod json;
pub(crate) mod operators;
pub(crate) mod scope;
pub(crate) mod stmt;
pub(crate) mod symbols;
pub(crate) mod tasks;
pub(crate) mod types;

#[cfg(test)]
mod tests;
