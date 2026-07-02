Silver is a systems programming language with an LLVM‑backed compiler (Rust crate `agc`). HashMap with indexing, string hashing fixes, Arena/Rc/Vec/Box, defer/move/RAII, operator overloading, Slice<T>, mutability enforcement, and module export generics are implemented.

**Compiler frontend**: now a reusable library (`libagc`) exposing lexer, parser, typeck, types, traits, etc. Both the CLI (`agc`) and LSP server (`agc-lsp`) use the same crate.

**LSP server** (`agc-lsp`): minimal implementation using `tower-lsp`. Currently does diagnostics (lexer → parser → type checker → publishDiagnostics) on file open/change. Hover returns nothing. Requires `--features lsp` to build.

**Test count**: 435 tests pass (5 suites — lib + bin + silver_runtime × 2 targets + integration).

**Stdlib**: Optional<T>, Result<T,E> (generic), HashMap<K,V>, Slice<T>, String, Vec<T>, Arena, Rc<T>, I/O, math, operator traits, iterator traits, C FFI helpers.
