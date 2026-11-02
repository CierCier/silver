# PR #52 review comment — @greptile-apps
Review thread id: PRRT_kwDOQeTfZc6ybe5m
File: bootstrap/stage0/agc/src/codegen/llvm_ir/expr.rs:1382
URL: https://github.com/CierCier/silver/pull/52#discussion_r4436117722

<a href="#"><img alt="P1" src="https://greptile-static-assets.s3.amazonaws.com/badges/p1.svg?v=9" align="top"></a> **Slices read the wrong bytes**

The new branch matches every `TypeKind::Slice`, but always calculates byte offsets and loads an `i8`. A parameter declared as `[i32]` keeps this slice type, so `values[1]` now reads the second byte rather than the second integer. Other non-byte elements also get the wrong address and return type.

Lower `slice.element_type` for the address calculation and load, or keep non-byte slices on the existing `__index_get` path.
