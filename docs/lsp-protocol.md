# Native LSP protocol support

This is the method-level support ledger for Silver's native `aglsp`. See
[language-server setup and behavior](language-server.md) for installation,
analysis limits, and performance checks. Update this ledger in the same change
as a handler, capability, client request, or protocol test.

The inventory covers all **69 requests and 26 notifications** in the
[LSP 3.18 metamodel][metamodel], excluding entries marked `proposed`.
Each method has its own row. Directions describe the protocol contract:
`C → S` means client to server, `S → C` means server to client, and `Both`
means the specification permits either direction. Silver's implemented direction
can be narrower, as recorded below.

| Status | Meaning |
| --- | --- |
| Supported | Implemented for the scope stated in the row. This does not establish clangd/rust-analyzer semantic parity. |
| Conditional | Implemented when the stated client capability or session mode applies. |
| Partial | Implemented, with a known protocol or capability-negotiation gap stated in the row. |
| Open | No handler or outgoing request/notification. Ignoring a notification does not count as support. |
| Unmapped | Not provided; a Silver language, library, or document model must be defined first. |

Capabilities are statically advertised by [capabilities.ag][capabilities].
Requests without handlers return `MethodNotFound`. The server does not dynamically
register capabilities. Resolve requests marked open are not advertised; their
providers return eager results instead.

## Client-to-server requests

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `initialize` | C → S | Supported | Lifecycle, UTF-16 positions, client formats, snippets, edit and diagnostic negotiation. No build configuration in initialization options. [Source][dispatch], [checks][checks]. |
| `shutdown` | C → S | Supported | Stops request handling; subsequent `exit` completes shutdown. [Source][dispatch], [checks][checks]. |
| `textDocument/hover` | C → S | Supported | Resolved declaration signatures and docs; negotiated Markdown/plaintext. General expression-type hover remains open. [Source][symbols], [checks][checks]. |
| `textDocument/definition` | C → S | Supported | Scoped declaration identities and source imports, including overlays. Ambiguous declarations do not select an arbitrary target. [Source][symbols], [checks][checks]. |
| `textDocument/declaration` | C → S | Supported | Resolved declarations and explicit trait method contracts. [Source][navigation], [checks][checks]. |
| `textDocument/typeDefinition` | C → S | Supported | Explicit named types and named alias targets; primitive types have no source target. General receiver inference and generic substitution remain open. [Source][navigation], [checks][checks]. |
| `textDocument/implementation` | C → S | Supported | Explicit trait implementations and resolved method bodies in the active document/import graph. [Source][navigation], [checks][checks]. |
| `textDocument/references` | C → S | Supported | Resolved identities across workspace/open files and reachable imports; honors declaration inclusion. Indirect and unresolved calls remain outside analysis. [Source][symbols], [checks][checks]. |
| `textDocument/documentHighlight` | C → S | Supported | Occurrences of the resolved symbol in the selected document. [Source][symbols], [checks][checks]. |
| `textDocument/prepareRename` | C → S | Supported | Validates the resolved identifier and returns its selection. [Source][symbols], [checks][checks]. |
| `textDocument/rename` | C → S | Supported | Scoped identities, independent import aliases, coordinated trait methods, conservative conflict checks, negotiated versioned edits. Complex pattern/capture analysis remains limited. [Source][symbols], [checks][checks]. |
| `textDocument/linkedEditingRange` | C → S | Open | No linked-range provider. |
| `textDocument/completion` | C → S | Supported | Visible declarations, locals, fields/methods, keywords and primitives; fuzzy ranking and whole-identifier edits. Snippets require client support. Expected-type ranking, auto-import and postfix assists remain open. [Source][completion], [checks][checks]. |
| `completionItem/resolve` | C → S | Supported | Deferred detail/docs when requested by client capabilities; preserves insertion edits and rejects stale revisions. [Source][completion], [checks][checks]. |
| `textDocument/signatureHelp` | C → S | Supported | Nested/generic calls and instance receiver omission. Generic expressions in arguments and overload/generic substitution remain limited. [Source][completion], [checks][checks]. |
| `textDocument/inlineCompletion` | C → S | Open | No inline-completion provider. Added to the stable inventory in 3.18. |
| `textDocument/documentSymbol` | C → S | Supported | Declaration ranges and name selections; nested symbols or flat fallback according to client capability. [Source][symbols], [checks][checks]. |
| `workspace/symbol` | C → S | Supported | Eager locations, ranked fuzzy search of active workspace/open files and reachable imports. [Source][symbols], [checks][checks]. |
| `workspaceSymbol/resolve` | C → S | Open | Workspace symbol locations are eager; no resolve provider. |
| `textDocument/foldingRange` | C → S | Supported | Blocks, parentheses, brackets and multiline comments. Honors `rangeLimit` and `lineFoldingOnly`; line-only folds keep closing lines visible, and character folds stop at the closing delimiter. Import/region folds remain open. [Source][features], [checks][checks]. |
| `textDocument/selectionRange` | C → S | Supported | Nested CST ranges; validates the complete UTF-16 position batch before computing results and rejects invalid input with `InvalidParams`. [Source][features], [checks][checks]. |
| `textDocument/inlayHint` | C → S | Supported | Local inferred types and call parameter labels within the requested range. Direct lambda initializers have no inferred-type hints; ownership/move hints remain open. [Source][features], [checks][checks]. |
| `inlayHint/resolve` | C → S | Open | Hints are eager; no resolve provider. |
| `textDocument/semanticTokens/full` | C → S | Partial | Cached UTF-16 token stream with result IDs. The fixed legend does not yet adapt to the client's supported token types. [Source][features], [checks][checks]. |
| `textDocument/semanticTokens/full/delta` | C → S | Partial | One previous generation; unknown/older IDs return full data. Same legend negotiation gap as full tokens. [Source][features], [checks][checks]. |
| `textDocument/semanticTokens/range` | C → S | Partial | UTF-16 clipping and multiline token splitting. Same legend negotiation gap as full tokens. [Source][features], [checks][checks]. |
| `textDocument/diagnostic` | C → S | Conditional | Advertised for pull-diagnostic clients; full/unchanged result IDs and dependency invalidation. Lambda analysis uses owned syntax copies and current import signatures. Diagnostics inherit native frontend limits. [Source][diagnostics], [analysis][analysis], [checks][checks]. |
| `workspace/diagnostic` | C → S | Conditional | Pull-diagnostic sessions; workspace/open-file reports, previous result IDs and removal reports. No partial-result streaming. [Source][diagnostics], [checks][checks]. |
| `textDocument/codeAction` | C → S | Conditional | Import quick fixes require action-literal support. Honors `context.only`, compiler path precedence, ambiguity/privacy/conflict checks and negotiated versioned edits. Other fixes and refactorings remain open. [Source][actions], [checks][checks]. |
| `codeAction/resolve` | C → S | Open | Import edits are eager; no lazy action resolution. |
| `workspace/executeCommand` | C → S | Open | No command provider or command-based action fallback. |
| `textDocument/codeLens` | C → S | Open | No reference/implementation or runnable-target lenses. |
| `codeLens/resolve` | C → S | Open | No lens provider. |
| `textDocument/documentLink` | C → S | Supported | Eager source-import links, compiler search precedence and overlays; tooltips require client support. Missing/inactive imports and AGM artifacts have no link. [Source][navigation], [checks][checks]. |
| `documentLink/resolve` | C → S | Open | Import link targets are eager; no resolve provider. |
| `textDocument/formatting` | C → S | Supported | Token-preserving structural whitespace layout; tabs/spaces, final newline preference and line endings. Invalid syntax produces no edits. Rich wrapping/alignment remains open. [Source][format], [checks][checks]. |
| `textDocument/rangeFormatting` | C → S | Supported | Changes only whitespace gaps fully contained in the UTF-16 selection. [Source][format], [checks][checks]. |
| `textDocument/rangesFormatting` | C → S | Supported | LSP 3.18 `rangesSupport`; validates all ranges, merges overlapping/adjacent selections and formats once. No duplicate edits. [Source][format], [checks][checks]. |
| `textDocument/onTypeFormatting` | C → S | Supported | Current-line indentation after newline, `}` or `;`; unfinished blocks allowed, literals/comments skipped. [Source][format], [checks][checks]. |
| `textDocument/prepareCallHierarchy` | C → S | Supported | Resolved direct function/method identities. [Source][hierarchy], [checks][checks]. |
| `callHierarchy/incomingCalls` | C → S | Supported | Grouped direct call sites in the active document/import graph; stale items rejected. Indirect calls and a cached call graph remain open. [Source][hierarchy], [checks][checks]. |
| `callHierarchy/outgoingCalls` | C → S | Supported | Direct calls enclosed by the selected function/method; grouped ranges and stale-item checks. [Source][hierarchy], [checks][checks]. |
| `textDocument/prepareTypeHierarchy` | C → S | Supported | Named types/traits with explicit implementation edges. [Source][hierarchy], [checks][checks]. |
| `typeHierarchy/supertypes` | C → S | Supported | Explicit implemented traits; stale items rejected. Generic bounds/substitution remain open. [Source][hierarchy], [checks][checks]. |
| `typeHierarchy/subtypes` | C → S | Supported | Explicit implementing types; stale items rejected. Generic bounds/substitution remain open. [Source][hierarchy], [checks][checks]. |
| `textDocument/inlineValue` | C → S | Open | Debugger-visible binding/expression scopes are not provided. |
| `textDocument/moniker` | C → S | Open | No persistent package/symbol identity contract. |
| `textDocument/documentColor` | C → S | Unmapped | No general Silver color literal; library-specific color APIs need an explicit mapping. |
| `textDocument/colorPresentation` | C → S | Unmapped | Requires the color model above. |
| `textDocument/willSaveWaitUntil` | C → S | Open | No save-time edit participation. |
| `workspace/willCreateFiles` | C → S | Open | No file-operation edits. |
| `workspace/willRenameFiles` | C → S | Open | No import-preserving file-rename edits. |
| `workspace/willDeleteFiles` | C → S | Open | No file-operation edits. |
| `workspace/textDocumentContent` | C → S | Open | No virtual document content provider. Added to the stable inventory in 3.18. |

## Server-to-client requests

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `workspace/diagnostic/refresh` | S → C | Conditional | Pull-diagnostic sessions with `workspace.diagnostics.refreshSupport`. Coalesces requests; changes during an outstanding request schedule another. Client errors disable further requests of this kind. [Source][client-requests], [checks][checks]. |
| `workspace/semanticTokens/refresh` | S → C | Conditional | Requires `workspace.semanticTokens.refreshSupport`; dependency invalidation, coalescing and acknowledgement/error handling. [Source][client-requests], [checks][checks]. |
| `workspace/foldingRange/refresh` | S → C | Open | No folding refresh request. Added to the stable inventory in 3.18. |
| `workspace/inlayHint/refresh` | S → C | Open | No hint refresh request. |
| `workspace/codeLens/refresh` | S → C | Open | No lens provider or refresh request. |
| `workspace/inlineValue/refresh` | S → C | Open | No inline-value provider or refresh request. |
| `workspace/textDocumentContent/refresh` | S → C | Open | No virtual content provider or refresh request. Added to the stable inventory in 3.18. |
| `workspace/workspaceFolders` | S → C | Open | Initial folders and folder-change notifications are supported; the server does not request the folder list. |
| `workspace/configuration` | S → C | Open | No client settings request or package/target/cfg configuration. |
| `workspace/applyEdit` | S → C | Open | Rename and import actions return edits directly; the server does not ask the client to apply edits. |
| `client/registerCapability` | S → C | Open | Static capability negotiation only. Watched-file notifications must be supplied by the client without server registration. |
| `client/unregisterCapability` | S → C | Open | No dynamic registrations. |
| `window/workDoneProgress/create` | S → C | Open | Only client-supplied request progress tokens are used; no server-created background progress. |
| `window/showMessageRequest` | S → C | Open | No interactive client message requests. |
| `window/showDocument` | S → C | Open | No client document-opening requests. |

## Notifications

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `initialized` | C → S | Supported | Starts background workspace indexing after initialization. [Source][dispatch], [checks][checks]. |
| `exit` | C → S | Supported | Stops the reader and process, including an incomplete following frame. [Source][dispatch], [checks][checks]. |
| `$/cancelRequest` | Both | Partial | Incoming cancellation of queued/running requests; IDs are reusable after completion. Unknown IDs are discarded. No outgoing cancellation of client requests. Cancellation checks occur between files, not inside frontend passes. [Source][transport], [checks][checks]. |
| `$/progress` | Both | Partial | Outgoing begin/end for client-supplied work-done tokens on selected queries. No incoming progress consumer, partial-result streaming, percentages or server-created tokens. [Source][progress], [checks][checks]. |
| `$/setTrace` | C → S | Open | No trace-level handling. |
| `$/logTrace` | S → C | Open | No trace notifications. |
| `textDocument/didOpen` | C → S | Supported | Unsaved overlay snapshots, analysis and dependency invalidation. [Source][document], [checks][checks]. |
| `textDocument/didChange` | C → S | Supported | Sequential UTF-16 incremental edits, atomic batches and monotonic document versions. Invalid batches preserve the snapshot. [Source][document], [checks][checks]. |
| `textDocument/didClose` | C → S | Supported | Clears push diagnostics and restores disk contents for dependent navigation. [Source][dispatch], [checks][checks]. |
| `textDocument/didSave` | C → S | Supported | Refreshes analysis/diagnostics; optional save text is not requested. [Source][dispatch], [checks][checks]. |
| `textDocument/willSave` | C → S | Open | No save participation. |
| `textDocument/publishDiagnostics` | S → C | Conditional | Push mode for clients without pull diagnostics; negotiated versions and related locations. Native frontend analysis limits apply. [Source][diagnostics], [checks][checks]. |
| `workspace/didChangeWorkspaceFolders` | C → S | Supported | Atomic normalized add/remove batches; refreshes imports and indexing. Removed closed files leave workspace queries; open files and reachable imports remain available. [Source][workspace], [checks][checks]. |
| `workspace/didChangeWatchedFiles` | C → S | Supported | Client-sent disk refreshes for closed files; preserves open overlays. No native filesystem watcher or dynamic watcher registration. [Source][dispatch], [checks][checks]. |
| `workspace/didChangeConfiguration` | C → S | Open | No settings/configuration invalidation handler. |
| `workspace/didCreateFiles` | C → S | Open | No file-operation notification handler; watched-file handling is separate. |
| `workspace/didRenameFiles` | C → S | Open | No file-operation notification handler. |
| `workspace/didDeleteFiles` | C → S | Open | No file-operation notification handler. |
| `window/workDoneProgress/cancel` | C → S | Supported | Cancels the operation associated with a client-supplied request progress token. [Source][transport], [checks][checks]. |
| `window/showMessage` | S → C | Open | No client message notifications. |
| `window/logMessage` | S → C | Open | No client log notifications. |
| `telemetry/event` | S → C | Open | No telemetry notifications. |
| `notebookDocument/didOpen` | C → S | Unmapped | No Silver notebook/cell execution or synchronization model. |
| `notebookDocument/didChange` | C → S | Unmapped | Requires a notebook document model. |
| `notebookDocument/didSave` | C → S | Unmapped | Requires a notebook document model. |
| `notebookDocument/didClose` | C → S | Unmapped | Requires a notebook document model. |

## Verification and maintenance

[Protocol checks][checks] exercise real framed stdio sessions against the built
server, not source-text checks or capability flags alone. Relevant scenarios
include:

- Lifecycle/transport: `test_protocol_errors_and_string_request_ids`,
  `test_transport_rejects_invalid_utf8_nul_suffixes_and_noninteger_request_ids`,
  `test_cancelled_index_request_and_bounded_pipeline`,
  `test_exit_stops_reader_with_incomplete_following_frame`.
- Synchronization/workspace: `test_utf16_incremental_changes_are_atomic_and_versioned`,
  `test_save_republishes_overlay_diagnostics_or_requests_pull_refresh`,
  `test_unopened_files_imports_and_unsaved_overlay`,
  `test_workspace_folder_removal_excludes_closed_files_from_all_workspace_queries`,
  `test_workspace_folder_batches_are_atomic_and_preserve_sysroot_imports`,
  `test_watched_files_refresh_closed_imports`.
- Navigation/rename: `test_scope_aware_navigation_and_rename`,
  `test_type_declaration_and_implementation_navigation`,
  `test_ambiguous_methods_do_not_choose_an_arbitrary_implementation`,
  `test_trait_method_rename_updates_contract_implementations_and_calls`,
  and the match/lambda scope scenarios.
- Completion/structure: `test_completion_signature_and_structural_features`,
  `test_fuzzy_completion_ranking_replaces_the_whole_identifier_and_honors_scope`,
  `test_completion_snippets_and_deferred_details_preserve_edits_and_reject_stale_items`,
  `test_folding_honors_line_only_clients_and_range_limits`,
  `test_selection_ranges_validate_the_entire_utf16_position_batch`,
  `test_client_formats_flat_symbols_and_versioned_workspace_edits`.
- Hierarchies: `test_call_hierarchy_groups_call_sites_and_rejects_stale_items`,
  `test_type_hierarchy_follows_trait_implementations`.
- Diagnostics/tokens/progress: `test_diagnostics_stay_owned_by_document_and_close_clears`,
  `test_lambda_example_diagnostics_and_hints_use_lambda_semantics`,
  `test_push_lambda_diagnostics_report_real_errors_and_clear_after_edits`,
  `test_lambda_diagnostics_use_unsaved_imported_methods_and_invalidate_dependencies`,
  `test_pull_diagnostics_cache_dependency_changes_and_refresh_acknowledgements`,
  `test_semantic_token_deltas_round_trip_and_ranges_use_utf16`,
  `test_semantic_tokens_invalidate_when_an_import_changes`,
  `test_request_progress_cancellation_and_completion_are_balanced`,
  `test_client_refresh_requests_coalesce_without_losing_later_changes`.
- Actions/links: the `test_import_quickfix_*` scenarios and
  `test_document_links_follow_active_imports_unsaved_modules_and_disk_precedence`.
- Formatting: `test_formatting_options_ranges_comments_and_incomplete_code`,
  `test_multi_range_formatting_merges_selections_and_validates_every_utf16_range`,
  `test_on_type_formatting_indents_and_dedents_without_touching_literals`.

The lifecycle and open/change/close methods also run through session setup,
edits and cleanup.
Individual rows do not imply exhaustive testing of every optional
field, client capability or malformed input. Known gaps stay recorded until the
implementation and its runtime checks cover them.

The [snapshot diagnostic checker][analysis] lowers lambdas on a separate owned
tree. Imported signatures provide lookup context without imported executable
bodies. Foreign primary diagnostics remain on their source document. Related
locations are emitted only for the queried document; cross-document related
locations remain open.

For a protocol change, update the row's scope/limits, its advertised capability,
and the corresponding runtime scenario together. Check incoming and outgoing
methods separately. When refreshing the specification version, compare exact
method names, message kinds and directions against every non-proposed metamodel
entry; resolve requests and client services must remain visible even when open.

[metamodel]: https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/3.18/metaModel/metaModel.json
[checks]: ../scripts/check_lsp.py
[dispatch]: ../bin/aglsp/src/main.ag
[capabilities]: ../bin/aglsp/src/capabilities.ag
[document]: ../bin/aglsp/src/document.ag
[workspace]: ../bin/aglsp/src/workspace.ag
[transport]: ../bin/aglsp/src/transport.ag
[progress]: ../bin/aglsp/src/progress.ag
[client-requests]: ../bin/aglsp/src/client_requests.ag
[symbols]: ../bin/aglsp/src/symbols.ag
[navigation]: ../bin/aglsp/src/navigation.ag
[completion]: ../bin/aglsp/src/completion.ag
[features]: ../bin/aglsp/src/features.ag
[diagnostics]: ../bin/aglsp/src/diagnostics.ag
[analysis]: ../libs/agc/src/tooling/diagnostics.ag
[actions]: ../bin/aglsp/src/actions.ag
[format]: ../bin/aglsp/src/format.ag
