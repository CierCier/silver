# Contributing

Thank you for taking an interest in Silver.

This project is still evolving quickly, so the most helpful contributions are the ones that are clear, focused, and easy to review.

## Before You Start

- Read the current code and nearby tests before making changes.
- Prefer small, well-scoped pull requests over large mixed changes.
- If you plan to change language behavior or compiler architecture, open an issue or discussion first when possible.

## Development Expectations

- Follow existing naming, structure, and code style in the surrounding files.
- Keep new code readable; avoid clever shortcuts that make maintenance harder.
- Update bootstrap outputs when your changes affect the compiler or standard library.
- Add or update tests when behavior changes.


## Silver Style Guide

### Naming

| Element | Convention | Examples |
|---|---|---|
| Types (structs, enums) | `PascalCase` | `Vec`, `HashMap`, `BufWriter` |
| Traits | `PascalCase` | `Drop`, `Display`, `Iterator` |
| Functions / methods | `snake_case` | `silver_rt_alloc`, `mem_align_up` |
| Variables | `snake_case` | `tracked_drop_count`, `idx` |
| Constants | `SCREAMING_SNAKE_CASE` | `MEM_PAGE_SIZE`, `IO_SEEK` |
| Internal helpers | `__` prefix + `snake_case` | `__fmt_write_fd`, `__optional_abort` |

### Documentation

- Use `///` for public-item doc comments (doc comments), `//` for implementation notes.
- Every public type, function, and method should carry at least a one-sentence `///` summary.
- Module-level documentation goes at the top of the file, before `import` statements.
- See `std/mem/alloc.ag` and `std/rt/types.ag` for well-documented examples.

### Error Handling

- Use `Optional<T>` / `Result<T,E>` from `std.optional` for recoverable errors.
- Use `SysResult` from `std.sys.result` for decoded syscall outcomes.
- Unrecoverable errors (OOM, bounds violations) call `abort()` — do not return null and hope.
- New allocation code should use the typed generic interface (`alloc<T>()`) which aborts on OOM.

### Testing

- Import `std.test` for shared assertion helpers (`assert_true`, `assert_eq_i64`, `done()`).
- Return the result of `done()` from `main()` so the test harness sees the failure count.
- Do not write ad-hoc `printf`-based assertion helpers; use the standard module.
- Tests that intentionally exit nonzero must be registered in `tests/run_tests.sh` `expected_exit`.

### Standard Library

- Prefer `@println("fmt {}", val)` (compiler builtin) over `println(str)` (plain function).
- Prefer pointer receivers (`T* self`) for methods that inspect or mutate state.
- Structs that own resources must explicitly call `field.drop()` in their own `Drop` implementation — field destruction is NOT automatic.
- Follow the import structure: import only what you use; do not rely on transitive imports.
## Recommended Workflow

Build the compiler:

```bash
cargo build -p agc
```

Run tests:

```bash
cargo test -p agc
```

Refresh bootstrap artifacts when needed:

```bash
bash ./update-bootstrap.sh
```

## Pull Requests

When opening a pull request, please:
- explain the problem being solved
- describe the approach you took
- mention any known limitations or follow-up work
- include the verification steps you ran

If your change affects the language, parser, type checker, code generation, module system, or standard library behavior, include at least one concrete example or test case.

## Communication

Please keep discussions respectful, direct, and constructive.

We may not merge every contribution, but thoughtful work and clear reasoning are always appreciated.
