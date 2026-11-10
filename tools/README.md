# Editor tools

Editor support for Silver. Every editor uses the same language server,
`aglsp` (built from [`bin/aglsp`](../bin/aglsp)); these packages add syntax
highlighting and start the server.

| Directory | What it is |
| --- | --- |
| [`tree-sitter-silver/`](tree-sitter-silver) | Tree-sitter grammar for `.ag` files, used by Zed (and any tree-sitter editor). |
| [`zed/`](zed) | Zed extension: grammar queries, language config, and the `aglsp` launcher. |
| [`vscode/`](vscode) | VS Code extension: TextMate grammar, language config, and an `aglsp` client. |
| [`check.sh`](check.sh) | Runs every check below. CI runs it in the `editor-tools` job. |

Install the toolchain first, so `aglsp` is on `PATH`:

```sh
./scripts/install.sh
```

## Checks

```sh
tools/check.sh              # grammar, Zed, and VS Code checks
tools/check.sh --no-vscode  # skip the VS Code npm install
```

`check.sh` fails when:

- `tree-sitter-silver/src/` is not regenerated from `grammar.js`;
- a corpus or highlight test fails;
- any `.ag` file in the repository does not parse cleanly;
- `zed/extension.toml` points at a grammar revision older than the grammar;
- the Zed crate has a clippy warning;
- the VS Code type check, grammar test, or `vsce package` fails.

The VS Code extension also has an integration test that runs a real editor;
see [`vscode/README.md`](vscode/README.md#integration-test).

## Changing the grammar

1. Edit `tree-sitter-silver/grammar.js`.
2. Run `npm run generate` in `tree-sitter-silver/` and add tests under `test/`.
3. Commit the grammar and its generated `src/`.
4. Set `rev` in `zed/extension.toml` to that commit, and commit again.

Zed fetches the grammar by `rev`, so step 4 needs a separate commit.
