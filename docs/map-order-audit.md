# Map-order determinism audit (STD-001 / plan S2-2a)

Date: 2026-09-28. Stage0 uses `rustc_hash::FxHashMap` in ~40 files; stage1
uses FNV-1a `std/map.ag` + `std/hash.ag`. Iteration order differs BY DESIGN,
so every place whose OUTPUT depends on map iteration must get a deterministic
secondary sort (span, then name) in the bootstrap FIRST (plan S2-2a).

## Confirmed parity breakers (fix required)

| # | Site | Effect | Fix sketch |
| --- | --- | --- | --- |
| 1 | `traits/mod.rs:412` `for method in trait_def.methods.values()` | missing-method errors pushed in hash order → diagnostic emission order nondeterministic | DONE 2026-09-28: `sort_trait_errors` at both public entries (`validate_traits`, `validate_traits_with_imports`) sorts by (file, start, end, message). NOTE: missing-method errors all share `trait_ref.span`, so the message tiebreak is load-bearing — stage1 must replicate span-then-message order. Verified live: zebra/yak/xerus fixture now emits xerus,yak,zebra; 565 lib tests green. |
| 2 | `traits/mod.rs:469` `for assoc in trait_def.assoc_types.values()` | same as #1 for assoc types | same fix (covered by `sort_trait_errors`) |
| 3 | `traits/mod.rs:484` `for fv in trait_def.assoc_fn_values.values()` | same as #1 for assoc fn values | same fix (covered by `sort_trait_errors`) |
| 4 | `semantic/monomorph.rs:1414` `for inst in instantiations.values().filter(...)` | monomorphized impl items emitted in hash order → codegen/symbol order differs stage0-vs-stage1; also decides WHICH non-convergent error surfaces under the 256 cap | sort instantiations by `(base, mangled)` before the fixpoint loop |
| 5 | `build_graph.rs:355` `for node_name in self.nodes.keys()` (Tarjan SCC) | SCC discovery order → parallel scheduling/join order (S2-6 reproducibility) | sort keys (or `BTreeMap` for `nodes`) |

## Verified benign (order-independent reductions / already sorted)

- `build_graph.rs:161` `nodes.values()` → `CodegenElements::add` is integer
  sums (`build_graph.rs:59-67`), commutative.
- `codegen/llvm_ir/entry.rs:1368` payload loop computes MAX variant size.
- `codegen/llvm_ir/call.rs:510` payload loop computes ANY-drop boolean.
- `module_artifact.rs:1402` `write_tags` already sorts entries by key —
  the precedent for S2-2b "make .agm sorted". (Other artifact sections are
  built from `program.items` order, which is deterministic.)
- `symbol_table.rs` `.iter()` hits are `Vec`/scope-stack iterations,
  deterministic by construction.

## Method

`grep -rn "\.values()\|\.keys()" bootstrap/stage0/agc/src --include=*.rs`
filtered to `FxHashMap` receivers; each site classified by whether its result
flows into diagnostics, emitted items, scheduling, or a commutative fold.
Re-run the grep after every semantic/ change; any NEW map-ordered output path
fails this audit until sorted.

## Out of scope for this slice

Actually applying the sorts (bootstrap behavior change + test updates),
order-insensitive diff sections, and the stage1-side secondary sorts.
