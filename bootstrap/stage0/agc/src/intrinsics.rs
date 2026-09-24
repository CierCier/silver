//! Compiler-recognized intrinsic function names.
//!
//! These are not real functions: they never enter the symbol table, never get
//! LLVM function declarations, and never reach the linker. Each stage that
//! inspects call sites (semantic analyzer, type checker, codegen) recognizes
//! them by name and handles them specially.
//!
//! Atomic intrinsics lower to LLVM atomic instructions (`load atomic`,
//! `store atomic`, `atomicrmw`, `cmpxchg`, `fence`). The element width is
//! encoded in the name suffix because Silver emits opaque LLVM pointers, so
//! the pointee type is not recoverable from a pointer value at codegen time.

/// Returns true when `name` is a recognized `__atomic_*` intrinsic.
pub fn is_atomic_intrinsic_name(name: &str) -> bool {
    matches!(
        name,
        "__atomic_load_i8"
            | "__atomic_load_i32"
            | "__atomic_load_i64"
            | "__atomic_store_i8"
            | "__atomic_store_i32"
            | "__atomic_store_i64"
            | "__atomic_exchange_i8"
            | "__atomic_exchange_i32"
            | "__atomic_exchange_i64"
            | "__atomic_fetch_add_i8"
            | "__atomic_fetch_add_i32"
            | "__atomic_fetch_add_i64"
            | "__atomic_fetch_sub_i8"
            | "__atomic_fetch_sub_i32"
            | "__atomic_fetch_sub_i64"
            | "__atomic_cmpxchg_i8"
            | "__atomic_cmpxchg_i32"
            | "__atomic_cmpxchg_i64"
            | "__atomic_fence"
    )
}
