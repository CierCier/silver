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
