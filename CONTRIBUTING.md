# Contributing to Silver

Keep changes focused, executable, and easy to review. Read [AGENTS.md](AGENTS.md)
for the repository workflow and [SYNTAX.md](SYNTAX.md) before writing Silver.
The [compiler reference](docs/compiler-guide.md) describes pipeline and ownership
contracts. The [maintainer profile](docs/maintainer-preferences.md) explains the
preferences behind these conventions.

## Set up and build

Run commands from the repo root. Enter the development environment with
`nix develop`, or use the existing `.envrc` with direnv. Without Nix, install
a Rust toolchain supporting edition 2024, LLVM 22 development libraries, Python 3,
and a system compiler/linker. Follow [the README](README.md) for installation.

```bash
cargo build -p agc
cargo run -p agc -- check examples/control_flow.ag
cargo run -p agc -- run examples/control_flow.ag
```

Cargo builds Rust stage0 from `bootstrap/stage0/agc`, normally into `target/`.
The Silver workspace is the root `silver.toml`; its driver and reusable library
live in `bin/agc` and `libs/agc`. Build its driver through the package command:

```bash
cargo run -p agc -- build silver.toml --bin agc -o /tmp/agc-stage1
```

An executable produced by stage0 is not evidence that stage1 can compile itself.
Verify which commands run locally and which delegate to stage0 before making a
self-hosting claim. See [the self-host gates](tests/selfhost/README.md).

## Make one coherent change

1. Inspect the branch, working tree, relevant implementation, and nearby tests.
   Preserve unrelated work, including staged changes.
2. Define the behavior and affected callers. Discuss new syntax or public API
   choices with examples before committing to a design.
3. Implement the simplest complete change. Reuse existing stdlib APIs and traits,
   keep files cohesive, and avoid compatibility code without a supported caller.
4. Test the behavior, update its docs and examples, and inspect the final diff.
5. Commit that change before starting an unrelated one when committing is part
   of the task. Publish a branch and PR when requested.

Compiler phases have distinct responsibilities. Keep parsing/lowering separate
from type checking, ownership, monomorphization, and codegen. Put shared compiler
logic in the library and driver orchestration in the binary. Keep diagnostic
messages in the relevant stage's catalog.

## Silver conventions

| Element | Convention | Examples |
| --- | --- | --- |
| Types and traits | `PascalCase` | `Vec`, `HashMap`, `Drop`, `Display` |
| Functions, methods, variables | `snake_case` | `mem_align_up`, `tracked_drop_count` |
| Constants | `SCREAMING_SNAKE_CASE` | `MEM_PAGE_SIZE` |
| Existing internal helpers | `__` prefix and `snake_case` | `__fmt_write_fd` |

Use current Silver syntax. Items are public by default; `private` restricts
visibility. Constructors return values, and `move` expresses explicit ownership
transfer. Prefer `&T` or `&mut T` receivers for caller-owned state; use `T*` when
the raw memory or FFI contract requires it. Import the modules you use rather
than depending accidentally on a broad transitive import.

Prefer compiler formatting macros such as `@println("value {}", value)`.
Use typed allocation APIs such as `alloc<T>()`. For recoverable failures, follow
nearby `Optional<T>` and `Result<T, Error>` APIs and return useful error data.
Unrecoverable allocator or bounds failures should fail explicitly.

The compiler drops owned struct fields after the outer destructor. Do not call
`field.drop()` again from the destructor. Clean up raw-pointer pointees, file
descriptors, and other resources according to their ownership contract. Custom
enum destructors manage their own payloads. See `tests/cascade_drop_test.ag`,
`tests/enum_cascade_test.ag`, and `tests/memory_pentest.ag`.

For Rust changes, follow the crate's edition and nearby conventions. Respect
the workspace lints and `clippy.toml`, including the existing compiler map/set
choices. Keep target layouts and ABI behavior grounded in the target interfaces.

## Verify behavior

```bash
# Compiler tests
cargo test -p agc

# Focused integration tests while developing
python3 tests/run_tests.py --no-tui memory_pentest cascade_drop_test

# Complete integration suite before committing behavior changes
python3 tests/run_tests.py --no-tui

# Stage1 frontend and native command gates when affected
bash tests/selfhost/run_stage.sh --include-std
bash tests/selfhost/run_native.sh --no-tui --jobs 4
```

Rebuild stage0 after Rust changes before invoking the self-host scripts, which
may reuse an existing binary. Run the ownership suite for ownership/pass changes
and the complete integration suite for runtime changes. Report which compiler,
target, flags, and execution path were used. Record failures, skips, and checks
that were not run.

Tests must observe behavior and failure modes, not merely find source text or
confirm that a function exists. Check output and cleanup where relevant; exit
zero alone can miss a broken program. For standalone `tests/*.ag` fixtures, use
`std.test` helpers and return `done()` from `main()`. Fixtures are discovered
automatically. Declare intentional nonzero exits with a `// expected_exit: N`
comment, as supported by `get_expected_exit` in `tests/run_tests.py`. Expected
compile failures and platform skips belong in the corresponding harness sets
with a reason. See [the test guide](tests/README.md).

Use existing `#[test]`/`agc test` conventions for package tests where appropriate;
they supplement the compiler regression harness. Leak checking is opt-in per
fixture through the integration runner. Do not enable it for stage builds.

For performance changes, measure before and after on the same workload and
configuration. Comparative reports need at least 10 runs and the median, with
latency percentiles when relevant. State differences in concurrency and workload
instead of presenting unlike execution models as equivalent.

Documentation-only changes need checked links, accurate commands, and consistent
contracts. They do not require unrelated compiler or runtime test runs.

## Documentation and review

Use concise `///` comments for public contracts: behavior, ownership, errors, and
constraints that callers need. Use `//` for a non-obvious reason or edge case.
Do not narrate code or add long progress reports inside source files. Put durable
module explanations near their implementation and update examples with behavior.

Keep temporary plans out of Git. Root `todo.md` and `handoff.md` are ignored local
trackers; do not include their contents or private plan references in commit or
PR descriptions. Keep permanent documentation limited to useful contracts and
working instructions.

Commit subjects name the behavior changed. Follow nearby Git history, without
phase numbering or model coauthor trailers. A PR should explain the problem,
resulting behavior, verification, and remaining limits. Include a concrete
trigger/example for compiler or language changes, and measured before/after data
for performance claims.

Verify requested review findings before fixing them. Recheck the updated head
after changes. Allow time for external reviewers to finish; report unavailable
review tools. The maintainer decides when to merge unless they delegate it.
