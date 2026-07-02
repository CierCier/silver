// Silver compiler frontend library — reusable by LSP and other tools.
pub mod ast_tree;
pub mod attributes;
pub mod builtin_macros;
pub mod codegen;
pub mod debug_info;
pub mod diagnostics;
pub mod lexer;
pub mod module_artifact;
pub mod module_loader;
pub mod parser;
pub mod profiler;
pub mod semantic;
pub mod symbol_table;
pub mod traits;
pub mod types;
