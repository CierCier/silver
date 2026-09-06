# Silver Package Specification v0.1

This document defines the package-management interface implemented by the
`agc` compiler in Silver 0.2.5. Package management is part of `agc`; v0.1 does
not add a second package-manager executable.

## 1. Package roots

A package root is a directory containing `silver.toml`. The compiler accepts a
package root directory, an explicit `silver.toml` path, or (when no source
input is given) an implicit `silver.toml` in the current directory. A package
may still be compiled directly from one or more `.ag` files; direct source
compilation does not require a manifest.

All paths in a manifest are relative to the manifest that contains them.
Manifest paths and source entries are canonicalized while resolving the graph.
This makes equivalent local paths resolve to one package node and prevents
duplicate compilation of the same package.

## 2. Manifest format

The top-level `name` and `version` fields are required non-empty strings.
`url` is an optional non-empty string used as package metadata.

Targets are declared in `[bin.<name>]` and `[lib.<name>]` tables. Each target
must contain exactly one of the following:

```toml
[bin.app]
entry = "src/main.ag"

[lib.widgets]
entry = "src/widgets.ag"
```

`entry` identifies a source file in the containing package. A target can
instead forward to another package manifest:

```toml
[lib.widgets]
manifest = "../widgets"
```

Manifest-backed targets may use the Git fields described below. An `entry`
target may not specify Git selectors. A target entry must exist and absolute
target paths are rejected.

Target names are local to their kind. If no target name is supplied on the
command line, `agc` selects the target whose name equals the package name, or
the sole target of the requested kind. Multiple candidates require an explicit
selection.

## 3. Dependencies

Dependencies are package-level tables keyed by the import/dependency alias:

```toml
[dependencies.widgets]
manifest = "../widgets"

[dependencies.graphics]
manifest = "git+https://example.com/graphics.git"
branch = "stable"
```

The `manifest` value is either a local package directory or `silver.toml`, or
a Git URL prefixed with `git+`. Local dependencies are resolved relative to
the containing manifest. Every dependency is parsed and recursively resolved,
including dependencies of Git packages. Repeated canonical local manifests
and repeated Git revisions are deduplicated. A dependency cycle is an error
and reports the package chain.

The dependency alias maps to the dependency package's library target. A
binary-only package is valid as a graph dependency but does not provide an
importable module. The library target is selected by alias, then package name,
then sole-library fallback; ambiguous or missing library targets are reported
with the available target names.

Source imports use the existing Silver import syntax, for example:

```silver
import widgets;
```

Aliases are scoped to the package containing the importing source. Nested
packages may therefore reuse an alias safely. A sibling `.agm` is preferred
when present, followed by `<package>/lib/silver/<target>.agm`, and finally the
library source entry. Existing relative imports, `-I`, `--root`, standard
library lookup, and module closure resolution retain their established
precedence.

## 4. Git sources and cache

Git sources support exactly one of:

```toml
branch = "name"
tag = "name"
rev = "0123456789abcdef0123456789abcdef01234567"
```

With no selector, the remote default branch is used. `rev` must be a full
40-character hexadecimal commit ID; symbolic and abbreviated revisions are
rejected rather than silently falling back to another selector. Invalid
selectors, unreachable revisions, repositories without `silver.toml`, and
Git command failures are diagnostics that include the dependency and source
manifest context.

Repositories are cached by canonical URL under:

```text
$XDG_CACHE_HOME/silver/git/<url-hash>/repo.git
```

When `XDG_CACHE_HOME` is unset, the fallback is
`$HOME/.local/cache/silver/git/<url-hash>/repo.git`. Selected commits are materialized
under the same cache entry, so different selectors can share a repository
clone while retaining immutable commit trees. Credentials and URL userinfo
are not printed in diagnostics.

## 5. Compiler commands

Package-aware forms are integrated into the existing driver:

```text
agc                         # default binary target in ./silver.toml
agc init [TARGET_DIR]       # initialize a package in CWD or TARGET_DIR
agc build [--bin NAME]     # build a binary target
agc check [--bin NAME]     # check a binary target
agc run [--bin NAME]       # build and execute a binary target
agc --lib [NAME]           # emit/check a library target as an .agm module
agc build --lib [NAME]     # explicit library build
```

`--bin` and `--lib` are mutually exclusive and may omit the target name when
the default-selection rules are sufficient. `run --lib` is rejected because a
library module is not executable. A library target changes the default emit
kind to `module`; executable/object/assembly emits are rejected unless the
command explicitly requests checking or module output. `-o` may be used to
choose the resulting artifact path.

`agc init` creates `silver.toml` and a minimal `src/main.ag`. Without a target
directory it uses the current working directory; otherwise it creates or uses
the supplied directory. The default package name is the target directory's
final component. `--name NAME` overrides that detected name. Initialization
`src/main.ag`. If `git` is available in `PATH`, initialization also initializes
a new Git repository in the package directory by default, preserving an existing
`.git` directory if one is already present.

The existing direct `.ag` forms and module artifact behavior remain supported:
`-I`, `--root`, `build`, `check`, `run`, `.agm` loading, and source-relative
imports do not require a package manifest.

## 6. Diagnostics and determinism

Malformed TOML, missing required fields, wrong field types, invalid target
definitions, missing paths, dependency cycles, target-selection errors, and
Git failures return ordinary compiler diagnostics. User-controlled manifest
content is never used as an unchecked index or panic path. Resolution order is
stable: dependency tables are traversed in sorted alias order, targets follow
TOML table order, and package roots/import aliases are registered in graph
order with duplicate paths removed.

## 7. Deliberate v0.1 boundaries

Silver v0.1 does not define a lockfile, registry protocol, published package
archive format, checksums, or version-range solving. The `version` field is
validated package metadata only; dependency selection is determined by its
local path or Git selector. These are reserved for a compatible future package
specification rather than being inferred from the v0.1 manifest.
