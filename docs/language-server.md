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

The installed standard-library root is discovered relative to the server executable
or configured through `SILVER_SYSROOT`. In source, `DocumentStore.new` registers both
`<prefix>/include/silver` and `<prefix>/include` (as well as the root prefix when
`SILVER_SYSROOT` is set):

- The local installation script (`scripts/install.sh`) installs to `<prefix>/include/std`
  (for example, `~/.local/share/silver/include/std`).
- Linux bundle packaging (`scripts/create-linux-bundle.sh`) and Nix packaging (`nix/package.nix`)
  install to `<prefix>/include/silver/std`.

Both layouts are discovered automatically. Check an installation, including automatic stdlib
discovery from an unrelated working directory, with:

```sh
python3 scripts/check_lsp.py --server ~/.local/share/silver/bin/aglsp --stdlib-root ~/.local/share/silver/include/std -v
```

Validating automatic sysroot discovery requires either an actual installation or an
isolated prefix matching one of these directory layouts.

## Neovim completion triggers

The server advertises completion triggers `.` and `:` along with contextual
import triggers for a space after `import`, an opening selection brace, and
commas between selections (`triggerCharacters: [".", ":", " ", "{", ","]`).
Blink blocks space triggers by default. To allow them for Silver without affecting
other filetypes, add this to Blink's `completion` options:

```lua
trigger = {
  show_on_blocked_trigger_characters = function()
    return vim.bo.filetype == "silver" and { "\n", "\t" } or { " ", "\n", "\t" }
  end,
},
```

Other languages retain Blink's default blocked characters. Silver returns no
candidates for these import-only triggers in ordinary code, keeping import-context
triggers language-specific. Typing module names and dots also requests completions;
manual completion remains available.

The server advertises these triggers in `capabilities.ag` and verifies them in
`test_import_triggers_are_contextual_and_replace_existing_segments`. In editors
configured for automatic import workflows (such as Neovim with Blink.cmp), typing
`import ` triggers module path suggestions such as `std.string`; accepting the module
candidate inserts `import std.string`. Typing `{` then requests selection suggestions
such as `String`.
Autopair plugins must not suppress the follow-up trigger, and space triggers must be
unblocked for Silver. Trigger parity across general editor configurations remains open.

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
  Import statements receive module-path and public selection completions without
  call snippets. Path lookup follows source overlays and compiler search order.
  Selection completion excludes names already selected or bound as aliases on
  either side of the cursor and offers `as` where an alias can begin.
  Nonempty unqualified prefixes offer reachable indexed source declarations
  from the workspace and standard library with import edits.
  Each provider has a separate item identifying its module. Existing imports,
  private declarations, caller-local top-level collisions, and imported non-callable
  clashes are excluded. Valid imported free-function overloads can be offered;
  a compiler-backed check suppresses providers introducing new ambiguity at
  existing local call sites. Generic functions or unsupported constructs in
  relevant bodies cause conservative suppression (inconclusive). The check does
  not claim to prove absence of all silent callee changes or macro-generated calls.
  Completion ranks candidates across providers by fuzzy score and returns the top
  100 results, marking lists incomplete (`isIncomplete: true`) when truncated or
  while background workspace indexing is pending. Auto-import resolution checks
  both source and caller revisions and preserves the import edit. When the identifier
  starts at offset zero, the server combines the import with the main edit to avoid
  overlapping edits. Import-only triggers include spaces, opening selection braces,
  and commas. These triggers return no candidates outside import statements.
- Signature help for nested calls and generic function calls, including comments
  between the callee and its argument list. Commas in nested generic type arguments
  do not advance the active function parameter. Instance methods
  omit the implicit receiver from the parameter list. Clients advertising
  `parameterInformation.labelOffsetSupport` receive UTF-16 parameter ranges
  within the displayed signature for precise active-parameter highlighting.
  Other clients receive parameter strings taken from the displayed signature.
- Document symbols with name selections and declaration ranges, workspace symbol
  search, semantic tokens with full/delta/range requests, folding ranges, and selection ranges.
  Hierarchical outlines nest trait declarations' methods once under their trait;
  implementation methods remain at the top level. Flat outlines retain both.
  Folding honors client range limits and line-only output, keeping closing lines
  visible. Selection requests validate the full UTF-16 position batch before
  computing nested ranges.
- Call hierarchies with grouped call sites and type hierarchies over explicit
  trait implementations. Hierarchy follow-up requests reject stale document revisions.
- Inferred local type hints and parameter name hints, restricted to the requested
  range. Parameter hints omit arguments that already name the parameter, including
  parenthesized, borrowed, and explicitly moved names. Expressions such as
  `value + 1` retain their hints. Generic type arguments do not advance parameter
  hints. Mismatched argument delimiters stop hint scanning
  so labels do not spill into later code. Direct lambda initializers currently have no
  inferred-type hints.
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
  private declarations, and enforces the shared import safety contract: caller-local
  top-level collisions and imported non-callable clashes are rejected, valid
  function overloads are permitted, and compiler-backed checks suppress providers
  introducing new ambiguity at existing local calls (conservatively rejecting generic
  or unsupported relevant bodies). Multiple valid modules remain separate choices.
  Versioned workspace edits are used when supported. Code actions require client
  support for action literals.
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
unopened files. It follows symlinked source files and skips hidden entries,
`target`, `node_modules`, symlinked directories, and directories deeper than 32
levels. Import resolution uses the compiler's module resolver. Source imports use
unsaved open buffers when available.
Startup indexing also queues standard-library sources and follows `silver.toml`
target manifests and local dependencies, including packages outside the workspace.
Standalone parent manifest discovery runs only when neither `rootUri` nor a
`workspaceFolders` array is declared during initialization; declaring an explicit
empty folders list (`[]`) or removing the last declared workspace root keeps
discovery disabled. Manifest change notifications rebuild the package scope.

Closed files retain source text and symbol metadata. Their parse trees, tokens,
and occurrence tables are released after use. Cross-file navigation restores
syntax from the cached text one file at a time; opening a file keeps its full
snapshot available for editing. Cached text remains available until a watched-file
notification updates or removes it.

Opening or closing an overlay rebuilds import selections for every cached document,
including imports that previously resolved to a lower-priority source file.
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
beside the executable's installation prefix. Indexing also includes an existing
`std` directory beneath each include directory unless a workspace root already
covers it. A sysroot can contain `std` directly, under `include`, or under
`include/silver`.

Rename identifies declarations by document and symbol ID rather than spelling.
Import aliases have independent rename identities. Ambiguous imported names and
free-function overloads do not resolve to an arbitrary declaration. Rename rejects
non-ASCII identifiers, keywords, primitive types, and detected binding conflicts.
Local renames also reject names that would capture existing imported references
within the local's visible scope.
Module paths are excluded from symbol references and rename edits. Dot-qualified
members resolve through visible type declarations and aliases in their defining
documents, using the same receiver lookup as completion. Chained receivers follow
the preceding member's declaration, so an unrelated local with the same name
cannot redirect navigation or rename. Original names in import selections resolve
to imported declarations; aliases keep their independent rename identities.
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
document snapshots and stdout. Closing the input drains queued messages before
the dispatcher exits. Background workspace indexing yields after each
file or directory, so local requests can run while indexing remains unfinished.
Workspace search, references, and rename finish the pending index before
returning. Cancellation checks stop that traversal between files and return
`RequestCancelled`; individual frontend passes currently run to completion.
If the reader is waiting for queue space, interruptible analysis stops at its next
checkpoint and returns `RequestFailed` with a retry message. Pending document
changes remain ordered. Queue pressure does not count as client cancellation;
a cancellation behind the backlog may arrive after the overload response.
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

A 2026-10-27 comparison used native-compiled `-O2` servers against a frozen
`772367e` workspace containing 338 `.ag` files and 109,565 lines. Each prefix
used ten fresh processes per build, alternating build order, with warm filesystem
caches and one server running at a time. Completion was timed after workspace
indexing and document analysis. Reusing module resolution and conflict checks
for each provider within a request reduced repeated import planning.

| Completion prefix | Items | Previous median / p95 | Optimized median / p95 |
| --- | ---: | ---: | ---: |
| `sys_` | 480 | 22.922 / 24.664 ms | 8.583 / 11.025 ms |
| `read` | 192 | 20.295 / 20.947 ms | 10.766 / 11.469 ms |
| `Str` | 490 | 46.305 / 52.108 ms | 18.653 / 20.942 ms |

Canonical completion-item fingerprints matched across all 60 responses. The
benchmark records item counts, provider counts, and fingerprints with the latency
samples. Temporary instrumentation found 482 module resolutions for the
480 `sys_` choices from 22 providers before this change. Reuse lasts only for
the current request; later edits and resolver changes are checked again.

Other operations showed mixed small timing changes. Across the 30 runs per
build, cached workspace-search medians were 1.650 / 1.832 ms and edited-hover
medians were 0.378 / 0.417 ms before / after. These results establish a completion
improvement, not an overall server speedup.

A separate memory comparison on 2026-10-27 used the installed `-O2` baseline
and a stage0-built `-O2` server with compact closed snapshots. Both indexed the
same 339-file Silver workspace plus the installed source stdlib, with
`SILVER_SYSROOT` set to the installation prefix. Each build ran ten fresh
processes serially, with a 50% CPU quota and swap disabled. RSS was read from
the server's `/proc/PID/status` after a cached workspace search.

| Measurement | Baseline median | Compact snapshot median |
| --- | ---: | ---: |
| Indexed idle RSS | 460.33 MiB | 67.06 MiB |
| Initial indexing and search | 3730.60 ms | 4142.03 ms |
| Cached workspace search | 2.43 ms | 2.50 ms |
| Hover after an edit | 0.47 ms | 0.42 ms |

Idle RSS fell 85.4%; initial indexing took 11.0% longer. The corresponding
indexing p95 values were 3924.98 and 4378.58 ms. Closed-file syntax is rebuilt
for cross-file scans, so this comparison does not establish their latency.
The protocol suite also checks that repeated workspace analysis, edits, and
semantic-token refreshes release superseded snapshots. Its edit regression
reproduces 53.6 MiB of growth over 20 identical edits in the baseline.

### Benchmark tool invocation and options

`scripts/benchmark_lsp.py` supports measuring cold and warm analysis, semantic tokens,
memory, and first-use completion:

```sh
# Basic latency across initial hover, workspace search, cached search, and edited hover
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10

# Semantic token computation latency for a specific source document
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10 --semantic-document libs/agc/src/frontend/typecheck_expr.ag

# Warmed completion latency for a given prefix after workspace indexing finishes
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10 --completion-prefix sys_

# First-use completion before indexing finishes, with expected label verification
python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10 --completion-prefix Str --completion-first-use --completion-expect String

# Server resident memory (VmRSS) after indexing completes (requires Linux /proc)
SILVER_SYSROOT="$HOME/.local/share/silver" python3 scripts/benchmark_lsp.py --server /tmp/aglsp-native --workspace . --runs 10 --memory
```

The benchmark options operate as follows:

- `--completion-first-use`: requires `--completion-prefix`. Runs completion before draining the background workspace index. The first reply (`first_reply`) is measured immediately after opening the completion overlay document. The benchmark then executes a workspace symbol query to drain the index, followed by a second completion request to measure the index-ready (`ready`) response. Output JSON separates `first_reply` and `ready` latencies, item counts, provider counts, auto-imports, `reported_incomplete` and `item_limit_reached` flags, fingerprints, and elapsed sequence times.
- `--completion-expect LABEL`: repeatable flag (requires `--completion-prefix`). Records whether expected completion labels (such as `String`) appear in `first_reply` and `ready` responses, outputting `expected_present` and `expected_missing`.
- `--memory`: records resident memory (`VmRSS` in KiB from `/proc/PID/status`) after initial indexing and cached workspace search.

**Measurement scope and limits**:
- The `--completion-first-use` and `--completion-expect` flags in `scripts/benchmark_lsp.py` evaluate cold partial responses versus index-drained completion; do not claim speedup, latency reductions, or parity without verified runs.
- Do not compare partial or truncated candidate sets against complete responses, nor equate untruncated candidate lists (such as earlier ~490-item responses) with top-100 capped responses, or compare differing provider payloads (such as single-character `S` returning 38 versus 42 providers). Differing payloads and candidate counts do not represent equal work; state no current verified equal-work speedup. While top-100 truncation and single-character prefix behavior are supported in source, overall clangd parity remains open.
- All numbers in the performance tables above remain historical baseline evidence from earlier completed and verified runs.

## Clangd 22 default parity comparison

The reference implementation is pinned to upstream
[LLVM/clangd 22.1.0](https://github.com/llvm/llvm-project/tree/llvmorg-22.1.0/clang-tools-extra/clangd)
(binary `/nix/store/w3ifgwjmsvy8dwlfir4i4jz17kzxr1xb-clang-22.1.8/bin/clangd`).
Default configuration options derive from `ClangdMain.cpp` and `ClangdLSPServer.cpp`.

| Area / Feature | Official clangd 22 default | Silver aglsp behavior | Status & evidence | Language mapping & notes |
| --- | --- | --- | --- | --- |
| **1. 100 limit cap** | `--limit-results=100`. Returns top 100 scored candidates; sets `isIncomplete: true` when results exceed 100. | In `bin/aglsp/src/completion.ag`, caps candidates to top 100 and sets `isIncomplete: true` when truncated or indexing. | Supported in source; exercised by `test_completion_returns_ranked_top_hundred_and_requeries_truncated_lists` and `test_completion_responds_with_available_data_while_indexing_continues`. | Directly applicable to Silver; prevents UI lockup on broad prefixes. |
| **2. All-scopes completion** | `--all-scopes-completion=true`. Searches across global and all namespace scopes; inserts qualified names or includes. | Searches workspace and stdlib modules for reachable unimported declarations (`append_auto_imports`), offering auto-import items. | Supported for source files (`test_single_character_prefix_includes_auto_imports_after_indexing`, `test_auto_import_completion_applies_primary_and_import_edits_and_resolves`); binary `.agm` exports open. | Silver uses module paths (`import std.io;`) rather than C++ nested lexical namespaces (`a::b::c`). No lexical namespace qualifiers. |
| **3. Argument placeholders** | `--function-arg-placeholders=true`. Emits `${1:arg}` placeholders when snippets supported. | Inserts call argument placeholders `${1:param}` when client advertises snippets; omits implicit receiver `self`; suppresses if cursor precedes `(`. | Conditional. Requires client `snippetSupport`. Verified in protocol checks (`test_completion_snippets_and_deferred_details_preserve_edits_and_reject_stale_items`); generic substitution in method signatures open. | Maps function and method signatures to snippet syntax. |
| **4. Header insertion / Auto-import** | `--header-insertion=iwyu`. Inserts missing `#include <header>` via `additionalTextEdits`. | Inserts `import path;` via `additionalTextEdits`; merges with primary edit if cursor is at offset 0; offers import quick-fixes. | Supported for source `.ag` files (`test_auto_import_at_file_start_combines_edits_without_overlap`, `test_import_quickfix_applies_to_current_source_and_resolves_diagnostics`). Binary `.agm` exports open. | Silver uses module imports instead of C textual preprocessor `#include`. No angle vs quote syntax or IWYU pragmas. |
| **5. Background index** | `--background-index=true`. Multithreaded background crawler indexes workspace files in worker threads. | Single-threaded cooperative indexing yields between files during event loop. Responds with available data and `isIncomplete: true`. | Partial. Analysis passes themselves are synchronous on the main dispatch thread; no worker thread pool (`test_completion_responds_with_available_data_while_indexing_continues`). | Prevents complete server stall on startup, but long single-file frontend passes remain non-preemptible. |
| **6. Persistence** | Writes disk index files to `.cache/clangd/index/*.idx` for fast cross-session restarts. | Zero disk persistence. In-memory only. Re-indexes on every LSP startup. Releases ASTs of closed files to conserve memory (~67 MiB idle RSS). | Open / Missing. No on-disk index format or cache directory. Closed-file compaction reduces memory but requires re-parsing on demand. | Clangd stores standalone `.idx` files. Silver currently rebuilds index in RAM every run. |
| **7. Diagnostics** | Clang compiler frontend diagnostics (parse, template, type, lint, fix-its, notes/related information). | Push diagnostics and LSP 3.17 pull diagnostics (`textDocument/diagnostic`, `workspace/diagnostic`). Emits frontend errors/warnings. | Supported with limits. Lambda analysis uses owned CST. Related locations are strictly document-local; cross-document related locations remain open. | Silver has no C++ templates or SFINAE errors; diagnoses borrow/ownership and nominal generics instead. |
| **8. Semantic features** | Semantic tokens (modifiers, types), hover, definition, declaration, typeDefinition, implementation, references, rename, hierarchies, inlay hints. | Implemented: tokens (full/delta/range), hover, definition/typeDefinition/implementation, references, rename (trait methods, scope-aware), hierarchies, parameter/type hints. | Partial. General expression-based receiver inference, free-function overloads, macro expansion hover, and code lenses remain open. | Silver method syntax and trait impls map to hierarchies; lack of general receiver inference limits dot-completion on complex expressions. |
| **9. Editor triggers** | Advertises single characters `.`, `<`, `>`, `:`, `"`, `/`, `*` (in `ClangdLSPServer.cpp`). | Advertises `.`, `:`, ` `, `{`, `,` (`triggerCharacters: [".", ":", " ", "{", ","]`). | Conditional. Advertised in server capabilities and tested in protocol suite (`test_import_triggers_are_contextual_and_replace_existing_segments`). Exercised with Neovim and Blink.cmp, but general client configuration parity remains open. | Maps Silver syntax: `.` (member), `:` (type/enum), ` ` (after `import`), `{` (import selection), `,` (import selection item). Clangd uses `>` as part of `->` and `:` as part of `::`. |

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
| Completion: `textDocument/completion`, `completionItem/resolve`, `textDocument/signatureHelp`, `textDocument/inlineCompletion` | Scoped fuzzy completion, module paths, import selections, source auto-import edits, call snippets, deferred detail/docs, signatures | Type ranking, binary artifact exports, postfix assists, inline completion and generic substitution |
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
