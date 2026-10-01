# Working on Silver

These instructions apply to the whole repository and to agents working through
OMP, OpenCode, Codex, or another host. Current user instructions take precedence
over the historical preferences summarized here.

## Start here

1. Read this file and [SYNTAX.md](SYNTAX.md) before reading or editing Silver code.
2. Inspect `git status`, the current branch, and the relevant diff. Preserve
   existing staged, unstaged, and untracked work.
3. Read nearby implementations and tests. Use `rg` and `rg --files` for searches.
4. For compiler work, consult [the compiler reference](docs/compiler-guide.md).
   For collaboration details, consult [the maintainer preference profile](docs/maintainer-preferences.md).
5. If present, read root `todo.md` and `handoff.md` for active work. Verify their
   claims against the current tree; they are local notes, not implementation truth.

## How to work

- Carry the authorized task through implementation, verification, and handoff.
  "Continue" means resume the current objective. Fix blockers before expanding
  the feature list. Honor explicit instructions to plan, report, or stop.
- Work one coherent change at a time. Think through nontrivial changes before
  editing, then implement. Keep designs simple, with no speculative framework,
  compatibility layer, or placeholder implementation.
- Default to one agent. The latest explicit preference favors a single process.
  Follow the current task's delegation instructions if they differ.
- Ask about consequential syntax and public API choices with concrete examples
  and tradeoffs. Resolve routine implementation details from source and tests.
- Use relevant available skills. An unavailable tool or reviewer is a limitation
  to report, not a reason to invent a result.
- Use the host's patch/edit tools. Inspect every diff. Avoid blind regex rewrites
  or Python/shell scripts that rewrite source by line number.
- Give concise progress reports with the result, remaining gap, and next action.
  Number multi-step instructions, use tables for comparisons, and provide deeper
  detail when requested. Do not repeatedly ask whether to continue authorized work.

## Design and code

- Keep Silver idiomatic. Check actual syntax before introducing declarations,
  receivers, casts, trait implementations, or examples. Public items do not need
  `pub`; the current visibility keyword is `private`. Do not copy Rust syntax.
- Prefer existing stdlib functions, traits, shims, and generic implementations.
  Avoid redundant casts, magic numbers, duplicated dispatch, and wrapper chains.
- Keep files cohesive and imports narrow. Put reusable compiler functionality
  in `libs/agc`; keep `bin/agc` focused on driver responsibilities.
- Keep parsing/lowering, symbol registration, type checking, ownership analysis,
  monomorphization, and code generation separate. Do not resolve types in parsing
  or imports in codegen. Use target layout/ABI interfaces rather than guesses.
- Prefer borrow references for caller-owned state. Use raw pointers when the
  memory or FFI contract needs them. Account for moves, temporaries, reinitialization,
  overwrite cleanup, returns, and scope exits.
- Struct fields cascade after the struct's own destructor. Do not manually drop
  owned fields again. Free raw-pointer pointees and other external resources as
  required. Enums with their own `Drop` implementation manage their payloads.
- Return structured errors for recoverable failures; avoid silent success,
  untyped error integers, and sentinel objects in new high-level APIs.
- Keep compiler diagnostic text in the stage's message catalog. Preserve source
  locations and useful errors across imports, generics, and LSP consumers.
- Keep comments short. Explain why a choice or constraint exists and which edge
  case matters. Public doc comments describe the contract, ownership, and errors.
- If a compiler limitation forces awkward stdlib code, identify the missing
  capability and propose the proper fix. Implement it when within the task's scope.

## Builds and verification

All commands below run from the repo root. Cargo builds Rust stage0 under
`bootstrap/`; `silver.toml` defines the Silver workspace under `bin/` and `libs/`.
The default Cargo output is `target/`. Use `nix develop` or the existing direnv
shell for the LLVM 22 toolchain. Test tools belong in the dev shell, not runtime
package dependencies.

| Task | Command |
| --- | --- |
| Build stage0 | `cargo build -p agc` |
| Compiler tests | `cargo test -p agc` |
| Check a file | `cargo run -p agc -- check path/to/file.ag` |
| Run a file | `cargo run -p agc -- run path/to/file.ag` |
| Build Silver driver | `cargo run -p agc -- build silver.toml --bin agc -o /tmp/agc-stage1` |
| Integration suite | `python3 tests/run_tests.py --no-tui` |
| Focused ownership tests | `python3 tests/run_tests.py --no-tui memory_pentest cascade_drop_test` |
| Frontend parity | `bash tests/selfhost/run_stage.sh --include-std` |
| Stage1 native command coverage | `bash tests/selfhost/run_native.sh --no-tui --jobs 4` |

- Test observable behavior, failure paths, output, and resource cleanup.
  Source-presence checks and a successful compile alone do not prove behavior.
- Run focused checks while developing. Before committing compiler or stdlib
  behavior changes, run compiler tests and the complete integration suite.
  Runtime changes require the integration suite. Ownership/pass changes also
  require `memory_pentest`. Changes to stage1 require the relevant self-host gates.
- Rebuild stage0 after Rust edits before invoking self-host scripts; the scripts
  may reuse an existing executable. Identify the actual compiler used in results.
- Exercise package/workspace commands through `agc build`, not only isolated files.
  Distinguish frontend parity, a stage0 bridge, a native smoke path, and genuine
  stage1/stage2 execution. State which one a passing gate covers.
- Check the current gate scripts and backend sources for stage1 status. Old plans,
  cached executables, and test counts are not proof of current completion.
- Do not enable `--leak-check` for stage builds. The integration harness opts
  selected fixtures into leak checking separately.
- Performance work needs a representative before/after benchmark. Keep workload,
  build flags, concurrency, and execution model comparable. For comparative
  performance reports use at least 10 runs and the median, with latency percentiles
  when relevant. Include measured results in the commit or PR when applicable.
- Report failed, skipped, and unrun checks explicitly. Documentation-only changes
  need link, command, and consistency checks; they do not require a compiler rebuild.

## Git and documentation

- Work on a branch. Do not mix unrelated changes or publish directly to `main`.
  Commit and publish when authorized by the active task; historical requests are
  evidence of preferred workflow, not authorization to push or merge now.
- Keep commits atomic. Finish and verify a change before beginning another.
  Name the behavior changed, without "phase N", numbered plan steps, or model
  coauthor trailers. Follow nearby commit conventions.
- Use PRs for review when publishing. If Greptile or another reviewer is requested,
  allow time for it to finish, verify each finding, fix valid findings, and check
  the updated head. The maintainer chooses when to merge unless explicitly delegated.
- Keep public behavior docs and examples synchronized with the implementation.
  Remove obsolete instructions within the edited scope. Verify language examples.
- Keep `todo.md` and `handoff.md` at the repo root, ignored and uncommitted. Use
  stable issue IDs, status, concrete evidence, and next actions in local trackers.
  Do not commit temporary plans, raw transcripts, or references to private plans
  in commit/PR text. Avoid multiplying progress documents.
- Hand off what changed, why, verification results, and remaining limits. Never
  call a migration complete merely because a smaller gate passed.
