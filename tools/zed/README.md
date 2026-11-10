# Silver for Zed

Silver support for [Zed](https://zed.dev): tree-sitter highlighting, outline,
brackets, indentation, Vim text objects, and the `aglsp` language server.

The extension starts the `aglsp` from your Silver installation. It does not
download or bundle a server. Install the toolchain from the Silver repository:

```sh
./scripts/install.sh
```

## Install

1. Install Rust with [rustup](https://rustup.rs). Zed adds the
   `wasm32-wasip2` target itself.
2. In Zed, run **zed: install dev extension** and select this `tools/zed`
   directory.

Zed builds the grammar from the commit named by `rev` in
[`extension.toml`](extension.toml). To try grammar changes that are not pushed
yet, set `repository` to `file:///absolute/path/to/silver` and `rev` to a
local commit, then reinstall the dev extension. Do not commit that change.

## Settings

Zed finds `aglsp` on `PATH`. To use another binary or pass `SILVER_SYSROOT`:

```json
{
  "lsp": {
    "aglsp": {
      "binary": {
        "path": "/home/me/.local/share/silver/bin/aglsp",
        "env": { "SILVER_SYSROOT": "/home/me/.local/share/silver" }
      }
    }
  },
  "languages": {
    "Silver": {
      "semantic_tokens": "combined"
    }
  }
}
```

When `binary.env` is set, it replaces the shell environment for the server.

`"semantic_tokens": "combined"` layers `aglsp`'s semantic tokens over the
tree-sitter highlighting. That colors locals, parameters, fields, and calls
inside function bodies, which the grammar only parses as token groups.
