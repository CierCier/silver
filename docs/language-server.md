# Native Silver language server

`bin/aglsp` is the Silver implementation of AGLSP. It communicates through
JSON-RPC over standard input and output. Configure an editor to launch the
resulting executable with no arguments.

The [protocol support ledger](lsp-protocol.md) records every stable LSP request
and notification, its direction, support status, limits and verification.

## Build and check

Run these commands from the repository root in `nix develop`:

```sh
cargo build --workspace
target/debug/agc build silver.toml --bin aglsp --no-cache -o /tmp/aglsp-native
python3 scripts/check_lsp.py --server /tmp/aglsp-native -v
```

To build the server with the native compiler:

```sh
target/debug/agc build silver.toml --bin agc --no-cache -o /tmp/agc-stage1
/tmp/agc-stage1 build --manifest silver.toml --bin aglsp --no-cache -o /tmp/aglsp-native-stage2
python3 scripts/check_lsp.py --server /tmp/aglsp-native-stage2 -v
```

The protocol checks start real server processes and create temporary workspaces.
They cover requests, notifications, edits, diagnostics, imports, and shutdown.
They do not add runnable programs to the stdlib suite.

The installer, Linux bundle, and Nix package provide the native server as
`aglsp`. The Rust language server has been removed. Cargo builds the bootstrap
compiler and tools; `agc build` builds the language server.

To install the compiler, native server, and includes on x86_64 Linux:

```sh
./scripts/install.sh
```

The default server path is `~/.local/share/silver/bin/aglsp`. The installer also
creates a link in `~/.local/bin` when that directory exists. Use `--prefix DIR`
to select an installation directory, `--no-symlinks` to skip links, and `--debug`
for a debug bootstrap with an unoptimized native server. Release installs compile
the native server with `-O2`. If `--bin-dir` is outside the installation prefix,
set `SILVER_SYSROOT` to the prefix when launching the server.

Check an installation, including automatic stdlib discovery from an unrelated
working directory, with:

```sh
python3 scripts/check_lsp.py --server ~/.local/share/silver/bin/aglsp --stdlib-root ~/.local/share/silver/include/std -v
```

## Protocol behavior

The native server provides these features:

- Hover signatures and documentation, definition/type/declaration/implementation navigation, references,
  document highlights, rename preparation, and rename edits.
- Document links from active imports to source modules, following compiler
  search-path precedence and unsaved buffers. Link tooltips use the client's
  advertised support. Missing modules and binary artifacts have no source link.
- Completion of visible local bindings, declarations, imported declarations,
  fields, methods, keywords, and primitive types. Completion edits replace the
  whole identifier around the cursor. Matching accepts case-insensitive
  abbreviations across camel/snake names and ranks exact/prefix matches first.
  Private imported declarations and shadowed bindings are excluded.
  Clients supporting snippets receive call arguments as editable placeholders;
  existing call parentheses prevent duplicate insertion. Clients advertising
  deferred detail/documentation receive them through `completionItem/resolve`.
  Resolve preserves insertion edits and rejects stale symbol revisions.
- Signature help for nested calls and generic function calls. Instance methods
  omit the implicit receiver from the parameter list.
- Document symbols with name selections and declaration ranges, workspace symbol
  search, semantic tokens with full/delta/range requests, folding ranges, and selection ranges.
  Folding honors client range limits and line-only output, keeping closing lines
  visible. Selection requests validate the full UTF-16 position batch before
  computing nested ranges.
- Call hierarchies with grouped call sites and type hierarchies over explicit
  trait implementations. Hierarchy follow-up requests reject stale document revisions.
- Inferred local type hints and parameter name hints, restricted to the requested
  range. Direct lambda initializers currently have no inferred-type hints.
- Versioned diagnostics from the native frontend's name, declared-type, borrow,
  Send, ownership, expression, and lint checks. Lambda diagnostics use the
  compiler's closure conversion on an owned syntax-tree copy. Source-import
  signatures come from current document snapshots, including unsaved changes;
  imported executable bodies are checked in their own document. Related
  diagnostic locations are limited to the queried document.
- Pull diagnostic reports and workspace reports with unchanged-result caching,
  selected when the client supports pull diagnostics. Other clients receive push diagnostics.
- Import quick fixes for unresolved names and types, including unsaved workspace
  modules. The server checks the compiler's search-path precedence, excludes
  private declarations, and rejects imports that would introduce name collisions.
  Multiple valid modules remain separate choices. Versioned workspace edits
  are used when supported. Code actions require client support for action literals.
- Structural document, range and multi-range formatting, with block indentation, statement
  layout, tab/space preferences, and preserved line endings. Range formatting
  changes whitespace only inside the selections. Multi-range requests validate
  every range before formatting once and merging overlapping selections.
  On-type formatting adjusts the
  current line's indentation after newline, `}`, or `;`.

The formatter preserves literal and comment bytes and checks that every lexer
token remains unchanged. Invalid syntax produces no document/range edits.
On-type formatting works in unfinished blocks but skips lexical errors and
literal/comment contents. `tabSize` and `insertSpaces` are required;
`insertFinalNewline: false` preserves the absence of a final newline.

Positions use UTF-16 code units. Incremental changes apply sequentially to a
temporary buffer. Invalid change batches leave the document unchanged. Older or
repeated document versions are ignored. Opening or changing a file republishes
diagnostics for that file and its open dependents. Closing a file clears its diagnostics and restores
its disk contents for navigation from other files.

The server indexes `.ag` files under `rootUri` and `workspaceFolders`, including
unopened files. It skips hidden entries, `target`, `node_modules`, symlinks, and
directories deeper than 32 levels. Import resolution uses the compiler's module
resolver. Source imports use unsaved open buffers when available.
An explicit `workspaceFolders` list takes precedence over `rootUri`.
`workspace/didChangeWorkspaceFolders` adds/removes normalized folders and refreshes
imports and indexing. Malformed batches leave the roots unchanged. Removing a
folder excludes cached closed files from search, references, rename, implementations,
hierarchies and import fixes. Open documents and reachable imports remain available;
an explicitly queried file can participate without becoming a workspace root.
Relative source paths become absolute before URI conversion, preventing duplicate
document identities for imports found through the working directory.
`workspace/didChangeWatchedFiles` refreshes closed files after disk changes.
The client must send those notifications; the server has no filesystem watcher.

Import search includes workspace roots, `SILVER_SYSROOT`, and include directories
beside the executable's installation prefix. A sysroot can contain `std` directly,
under `include`, or under `include/silver`.

Rename identifies declarations by document and symbol ID rather than spelling.
Import aliases have independent rename identities. Ambiguous imported names and
free-function overloads do not resolve to an arbitrary declaration. Rename rejects
non-ASCII identifiers, keywords, primitive types, and detected binding conflicts.
Conflict checking is conservative and can reject otherwise valid renames.
Match payload and bare bindings are limited to their own arm, including guards
and nested matches. Constructor arguments in arm values remain references.
Typed and untyped lambda parameters shadow captures only inside their own body.
Local initializer inference uses those lexical scopes, so a parameter's type
does not replace an outer binding's type in a later declaration.
Renaming a trait method updates its contract, implementations, and resolved call
sites together. Ambiguous calls and conflicting implementation names reject the
edit. Clients supporting `documentChanges` receive edits with open document
versions and `null` versions for closed files.

Initialization negotiates documentation formats, hierarchical document symbols,
diagnostic versions/related locations, and versioned workspace edits. Clients
without hierarchical symbol support receive flat symbol information. Markdown
is sent only when it is preferred in the client's supported formats.

Semantic tokens require `textDocument.semanticTokens`, the `relative` format,
and a supported full or range request mode. The server advertises only those
request modes and legend entries supported by the client. Delta requests require
`requests.full.delta`. An absent or unusable capability disables the provider.
Unsupported semantic methods return `MethodNotFound`.

The legend follows Silver's token order. Unsupported struct, enum, and type
parameter kinds fall back to `type`; methods fall back to `function`; parameters,
properties, and enum members fall back to `variable`. Kinds without a supported
fallback are omitted. Modifier bits refer to the negotiated modifier legend.
Filtering happens before relative position encoding and full-token caching.
Range queries return complete overlapping tokens and preserve the full-token
delta baseline. Multiline tokens are split into single-line tokens.

Workspace queries and pull diagnostics accept a client-supplied `workDoneToken`.
Progress ends before the response, including cancellation and invalid-parameter
errors. `window/workDoneProgress/cancel` cancels the associated operation.
Refresh requests for diagnostics, semantic tokens, and inlay hints use the
client's advertised refresh support. Semantic refresh also requires an enabled
token provider. Outstanding requests coalesce; a change arriving before an
acknowledgement schedules another refresh. A rejected refresh disables further
requests of that kind for the session.

Inlay refresh follows analysis changes, including imported signature changes,
and asks the editor to request hints again. It refreshes all visible editors;
the server coalesces changes while a refresh is outstanding. Save notifications
retain their diagnostic-only behavior.

Messages are limited to 16 MiB, with header lines limited to 8192 bytes. The server
returns JSON-RPC errors for malformed JSON, invalid requests, invalid parameters,
unsupported methods, and requests outside its initialization lifecycle.
Present JSON-RPC parameters must be an object or array. Primitive parameters,
including `null`, produce `InvalidRequest` before lifecycle state can change.

The stdin reader feeds a bounded queue of 32 messages. One dispatcher owns all
document snapshots and stdout. Background workspace indexing yields after each
file or directory, so local requests can run while indexing remains unfinished.
Workspace search, references, and rename finish the pending index before
returning. Cancellation checks stop that traversal between files and return
`RequestCancelled`; individual frontend passes currently run to completion.
Cancelled and completed request identities are removed, allowing ID reuse.

## Performance checks

`scripts/benchmark_lsp.py` measures real stdio requests, using at least ten fresh
server processes per build. A comparison on 2026-10-25 used unoptimized builds,
one server at a time, and the same frozen copy of 327 repository `.ag` files,
106,860 lines. Filesystem caches were warm. These are local measurements, not
latency guarantees for arbitrary workspaces.

| Operation | Previous median | Indexed server median | Indexed server p95 |
| --- | ---: | ---: | ---: |
| Initialize workspace through first local hover | 4282.981 ms | 0.562 ms | 0.720 ms |
| Hover after an edit, with workspace already indexed | 15.894 ms | 0.723 ms | 0.863 ms |
| Workspace search immediately after first hover | 4.564 ms | 2239.776 ms | 2322.735 ms |

The previous server completed indexing before the first hover. Adding first
hover and subsequent workspace-search medians gives approximately 4.29 s before
and 2.24 s after for the complete sequence. The new server moves most indexing
after the first hover and also reduces total analysis work through indexed
symbol/token lookup and cached receiver names.

A separate ten-run comparison on the same frozen sources measured full semantic
tokens for `libs/agc/src/frontend/typecheck_expr.ag`. Replacing repeated whole-buffer
position scans with a line index reduced the median from 5345.096 ms to 15.281 ms,
and p95 from 5406.561 ms to 16.422 ms. This measured a fresh token computation;
subsequent full requests use cached data, and deltas retain one previous result.
An unknown or older result ID falls back to a complete token response.

```sh
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10 --semantic-document libs/agc/src/frontend/typecheck_expr.ag
```

## Remaining parity gaps

This implementation does not establish parity with clangd or rust-analyzer.

Analysis of an individual file runs synchronously. There is no worker query pool,
persistent index, or incremental parser. Each changed document gets a new index;
diagnostic caches are invalidated through import dependencies. Recovery balances missing closing delimiters for
editor queries, but does not recover every kind of malformed syntax.

Type inference and diagnostics inherit the native frontend's documented limits.
Member resolution handles named receivers and explicit local types, but does not
provide general expression-based receiver inference, overload selection, or
generic substitution. More complex patterns, contextual lambda parameter types,
and capture ownership/move precision need further semantic work. Binary AGM
imports are not indexed. Workspace configuration does not yet
load build targets, cfg values, or package graphs from `silver.toml`.

Diagnostic fixes beyond import insertion, refactoring assists, code lenses and
macro expansion remain unimplemented. Completion still needs
expected-type ranking, automatic imports and postfix assists. Implicit ownership hints remain
unimplemented.

## Implementation references

### Parity acceptance inventory

The target is applicable stable LSP behavior plus the editor capabilities of
clangd and rust-analyzer. An empty response or a capability flag alone does not
establish parity. Each implementation needs protocol tests for successful,
ambiguous, incomplete, stale, and invalid input where those cases apply.
Language-specific features need a Silver equivalent or an explicit explanation
of why the language has no corresponding construct.

The [method-by-method protocol ledger](lsp-protocol.md) tracks every stable LSP
request and notification, including direction, support status, known limits,
implementation references and runtime checks. Update it alongside protocol changes.

The family overview below uses the [LSP 3.18 metamodel](https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.18/metaModel/metaModel.json),
excluding entries marked proposed, and the upstream
[clangd features](https://clangd.llvm.org/features) and
[rust-analyzer features](https://rust-analyzer.github.io/book/features.html).
It is a work list, not a statement of full support.

| Family and protocol methods | Current state | Acceptance gap |
| --- | --- | --- |
| Lifecycle: `initialize`, `initialized`, `shutdown`, `exit` | Implemented with client format/edit/diagnostic negotiation | Initialization options and remaining capability-specific limits |
| Transport: `$/cancelRequest`, `$/progress`, `$/setTrace`, `$/logTrace` | Bounded intake, cancellation and request progress implemented | Server-created background progress, trace and cancellation inside individual analysis passes |
| Sync: `textDocument/didOpen`, `didChange`, `didClose`, `didSave`, `willSave`, `willSaveWaitUntil` | Open/change/close/save implemented | Save participation and version-aware analysis scheduling |
| Workspace: `workspace/didChangeWorkspaceFolders`, `workspace/workspaceFolders`, `didChangeConfiguration`, `configuration`, `didChangeWatchedFiles` | Initial/dynamic folders, import/query invalidation and watched files implemented | Settings and target/package/cfg graph; outbound folder/configuration requests when needed |
| File operations: `workspace/willCreateFiles`, `didCreateFiles`, `willRenameFiles`, `didRenameFiles`, `willDeleteFiles`, `didDeleteFiles` | Open | Import-preserving edits and disk/overlay invalidation |
| Navigation: `textDocument/definition`, `declaration`, `typeDefinition`, `implementation` | Implemented for resolved declarations and explicit types | General inferred receivers, generic substitution and overloads |
| References: `textDocument/references`, `documentHighlight`, `rename`, `prepareRename`, `linkedEditingRange` | First four implemented, with versioned edits and coordinated trait rename | Pattern/capture precision and linked ranges |
| Completion: `textDocument/completion`, `completionItem/resolve`, `textDocument/signatureHelp`, `textDocument/inlineCompletion` | Scoped fuzzy completion, call snippets, deferred detail/docs, signatures | Type ranking, auto-import, postfix assists, inline completion and generic substitution |
| Inspection: `textDocument/hover`, `inlayHint`, `inlayHint/resolve`, `workspace/inlayHint/refresh` | Signature/docs hover, basic hints and negotiated hint refresh | Expression types, ownership hints and lazy hints |
| Outline: `textDocument/documentSymbol`, `workspace/symbol`, `workspaceSymbol/resolve` | Eager symbols and ranked fuzzy workspace search | Deferred workspace locations |
| Structure: `textDocument/foldingRange`, `selectionRange`, `workspace/foldingRange/refresh` | Client folding limits/line-only output and validated selection ranges implemented | Region/import folds and folding refresh |
| Tokens: `textDocument/semanticTokens/full`, `full/delta`, `range`, `workspace/semanticTokens/refresh` | Negotiated request modes, compact legends, type fallbacks, modifier bits, full/delta/range caches and refresh implemented | Classification inherits the semantic index's inference limits |
| Diagnostics: `textDocument/publishDiagnostics`, `textDocument/diagnostic`, `workspace/diagnostic`, `workspace/diagnostic/refresh` | Push/pull reports, dependency caches, result IDs and refresh implemented | Actionable diagnostic data and build/package configuration |
| Actions: `textDocument/codeAction`, `codeAction/resolve`, `workspace/executeCommand`, `workspace/applyEdit` | Import quick fixes for clients supporting action literals | Other diagnostic fixes, extraction, inline and structural assists, lazy actions and command contracts |
| Lenses: `textDocument/codeLens`, `codeLens/resolve`, `workspace/codeLens/refresh` | Open | Reference/implementation annotations and runnable targets |
| Links: `textDocument/documentLink`, `documentLink/resolve` | Eager links for active source imports, with negotiated tooltips | Documentation links; lazy resolution if needed |
| Formatting: `textDocument/formatting`, `rangeFormatting`, `rangesFormatting`, `onTypeFormatting` | Token-preserving block/statement layout, options, selected whitespace edits and on-type indentation | Richer wrapping/alignment and style configuration |
| Calls: `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `outgoingCalls` | Resolved direct calls, grouped call sites and stale items implemented | Indirect calls, overload/generic resolution, cached call graph |
| Types: `textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`, `subtypes` | Explicit trait implementation edges implemented | Generic bounds and substituted implementations |
| Debug: `textDocument/inlineValue`, `workspace/inlineValue/refresh` | Open | Debugger-visible expression/binding scopes |
| Identity: `textDocument/moniker` | Open | Stable package and symbol identity |
| Colors: `textDocument/documentColor`, `colorPresentation` | No general Silver color literal | Decide supported library-specific color APIs before advertising |
| Notebooks: `notebookDocument/didOpen`, `didChange`, `didSave`, `didClose` | No Silver notebook execution model | Requires a notebook document/cell contract |
| Virtual documents: `workspace/textDocumentContent`, `workspace/textDocumentContent/refresh` | Open | Content provider and refresh contract |
| Client integration: `client/registerCapability`, `unregisterCapability`, `window/showMessage`, `showMessageRequest`, `logMessage`, `showDocument`, `window/workDoneProgress/create`, `cancel`, `telemetry/event` | Work-done progress cancellation implemented; other listed client services open | Use only negotiated client services; do not emit unsolicited telemetry |

Upstream extensions also set targets for Silver: module/macro navigation,
expansion views, related control-flow highlights, source/header-like module
switching, syntactic and type-directed assists, postfix completion, run/debug
targets, and analysis status. Rust lifetime/crate machinery and C++ template/header
machinery require language-specific mapping rather than copied syntax. No upstream
code is incorporated in this implementation.

Analysis acceptance includes overlays winning over disk, changes invalidating
dependent results, bounded queues, cancellation, shutdown during indexing, and
repeatable latency measurements on real projects. Persistent indices must reject
incompatible compiler/configuration/source versions. Incremental parsing must
preserve diagnostics and symbol identity across malformed edits.

The reusable [semantic index](../libs/agc/src/tooling/index.ag) owns declaration
and occurrence information. The [document store](../bin/aglsp/src/document.ag)
owns source snapshots and import dependencies. Navigation, completion and token
queries reuse those snapshots. Formatting separately validates syntax and token
preservation before proposing edits.

Reference code inspected for this implementation includes
[clangd's LSP server](https://github.com/llvm/llvm-project/blob/5f576704c2b701878c22c4841e68133cbfb5593e/clang-tools-extra/clangd/ClangdLSPServer.cpp),
[rust-analyzer's request handlers](https://github.com/rust-lang/rust-analyzer/blob/0c4d74686b21f2b3bd8872b6d3cf36ad2e740d49/crates/rust-analyzer/src/handlers/request.rs),
and [rust-analyzer's signature analysis](https://github.com/rust-lang/rust-analyzer/blob/0c4d74686b21f2b3bd8872b6d3cf36ad2e740d49/crates/ide/src/signature_help.rs).
