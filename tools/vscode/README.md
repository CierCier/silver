# Silver for Visual Studio Code

Language support for [Silver](https://github.com/CierCier/silver) (`.ag` files):

- Syntax highlighting, bracket matching, comment toggling, and doc-comment
  continuation for `///` and `/** */`.
- The `aglsp` language server: diagnostics, completion (including `@` macros
  and auto-imports), hover with doc comments, go to definition, references,
  rename, signature help, inlay hints, semantic tokens, formatting, code
  actions, call and type hierarchies, and workspace symbols.

## Requirements

The extension starts the `aglsp` server from your Silver installation. It
does not download or bundle a server. Install the toolchain from the Silver
repository:

```sh
./scripts/install.sh
```

This puts `aglsp` in `~/.local/share/silver/bin` and links it into
`~/.local/bin`. VS Code finds it on `PATH`; otherwise set `silver.server.path`.

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `silver.server.path` | `""` | Path to `aglsp`. Empty means `aglsp` from `PATH`. |
| `silver.sysroot` | `""` | Installation prefix, passed to the server as `SILVER_SYSROOT`. Empty lets `aglsp` find the standard library next to its executable. |
| `silver.trace.server` | `"off"` | Log protocol traffic (`messages` or `verbose`) in the output channel. |

Changing `silver.server.path` or `silver.sysroot` restarts the server.

## Commands

- **Silver: Restart Language Server**
- **Silver: Show Language Server Output**

## Workspace trust

The server runs only in trusted workspaces, because it executes the
configured binary. In restricted mode you still get syntax highlighting.

## Building from source

```sh
cd tools/vscode
npm ci
npm test          # type-check and grammar tests
npm run vsix      # writes silver.vsix
code --install-extension silver.vsix
```

## Integration test

`npm run test:integration` opens `test/integration/workspace/main.ag` in a
real VS Code and checks that the extension starts `aglsp` and returns document
symbols and a doc-comment hover. It needs `aglsp` on `PATH` and a display (use
`xvfb-run` on a headless machine). Set `VSCODE_EXECUTABLE` to an installed
editor binary to skip the VS Code download; on NixOS, point it at VSCodium's
Electron binary (`lib/vscode/codium`), not its `bin/codium` launcher.
