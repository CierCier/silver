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

Capabilities are advertised by [capabilities.ag][capabilities], and the server
accepts dynamic `client/registerCapability`/`client/unregisterCapability`
requests. Requests without handlers return `MethodNotFound`. Resolve requests
marked open are not advertised; their providers return eager results instead.

## Client-to-server requests

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `initialize` | C → S | Supported | Lifecycle, UTF-16 positions, client formats, snippets, edit and diagnostic negotiation. No build configuration in initialization options. [Source][dispatch], [checks][checks]. |
| `shutdown` | C → S | Supported | Stops request handling; subsequent `exit` completes shutdown. [Source][dispatch], [checks][checks]. |
| `textDocument/hover` | C → S | Supported | Resolved declaration signatures and docs; negotiated Markdown/plaintext. Doc blocks attach only when directly above the item (an empty line detaches), and `$n : ...` lines render as a Parameters section mapped onto parameter names; `@macro` invocations hover over the builtin macro table or the documented `macro` declared in the file. General expression-type hover remains open. [Source][symbols], [checks][checks]. |
| `textDocument/definition` | C → S | Supported | Scoped declaration identities and source imports, including overlays. `@name` resolves to the `macro` declared in the file and never to a same-named function. Ambiguous declarations do not select an arbitrary target. [Source][symbols], [checks][checks]. |
| `textDocument/declaration` | C → S | Supported | Resolved declarations and explicit trait method contracts. [Source][navigation], [checks][checks]. |
| `textDocument/typeDefinition` | C → S | Supported | Explicit named types and named alias targets; primitive types have no source target. General receiver inference and generic substitution remain open. [Source][navigation], [checks][checks]. |
| `textDocument/implementation` | C → S | Supported | Explicit trait implementations and resolved method bodies in the active document/import graph. [Source][navigation], [checks][checks]. |
| `textDocument/references` | C → S | Supported | Resolved identities across workspace/open files and reachable imports; honors declaration inclusion. Indirect and unresolved calls remain outside analysis. [Source][symbols], [checks][checks]. |
| `textDocument/documentHighlight` | C → S | Supported | Occurrences of the resolved symbol in the selected document. [Source][symbols], [checks][checks]. |
| `textDocument/prepareRename` | C → S | Supported | Validates the resolved identifier and returns its selection. [Source][symbols], [checks][checks]. |
| `textDocument/rename` | C → S | Supported | Scoped identities, independent import aliases, coordinated trait methods, conservative conflict checks, negotiated versioned edits. Complex pattern/capture analysis remains limited. [Source][symbols], [checks][checks]. |
| `textDocument/linkedEditingRange` | C → S | Open | No linked-range provider. |
| `textDocument/completion` | C → S | Supported | Visible declarations, locals, fields/methods, keywords and types; module paths, import selections and source auto-import edits. Contextual import triggers, fuzzy ranking, callable overload ambiguity checks, whole-identifier edits, and `@`-prefixed macro completion (builtin plus user macros); snippets require client support. Best 100 results; incomplete while truncated or background workspace indexing is pending. Expected-type ranking, artifact exports and postfix assists remain open. [Source][completion], [import planning][imports], [checks][checks]. |
| `completionItem/resolve` | C → S | Supported | Deferred detail/docs when requested; preserves primary/import edits and rejects stale source or caller revisions and invalidated module mappings. [Source][completion], [checks][checks]. |
| `textDocument/signatureHelp` | C → S | Supported | Nested/generic calls, `@name(...)` calls of macros declared in the file, generic-aware argument boundaries and instance receiver omission. Negotiated UTF-16 parameter label offsets support precise highlighting; other clients receive strings from the displayed signature. Overload resolution and generic substitution remain limited. [Source][completion], [checks][checks]. |
| `textDocument/inlineCompletion` | C → S | Supported | Statically advertised (`inlineCompletionProvider`). Offers the strongest completion item as a single inline edit (its own `textEdit` when present, otherwise the current word replaced). [Source][completion], [checks][checks]. |
| `textDocument/documentSymbol` | C → S | Supported | Declaration ranges and name selections; nested symbols or flat fallback according to client capability. [Source][symbols], [checks][checks]. |
| `workspace/symbol` | C → S | Supported | Eager locations, ranked fuzzy search of active workspace/open files and reachable imports. [Source][symbols], [checks][checks]. |
| `workspaceSymbol/resolve` | C → S | Open | Workspace symbol locations are eager; no resolve provider. |
| `textDocument/foldingRange` | C → S | Supported | Blocks, parentheses, brackets and multiline comments. Honors `rangeLimit` and `lineFoldingOnly`; line-only folds keep closing lines visible, and character folds stop at the closing delimiter. Import/region folds remain open. [Source][features], [checks][checks]. |
| `textDocument/selectionRange` | C → S | Supported | Nested CST ranges; validates the complete UTF-16 position batch before computing results and rejects invalid input with `InvalidParams`. [Source][features], [checks][checks]. |
| `textDocument/inlayHint` | C → S | Supported | Local inferred types and call parameter labels within the requested range. Redundant matching-name labels are suppressed through parentheses, borrows, and moves; larger expressions retain labels. Direct lambda initializers have no inferred-type hints; ownership/move hints remain open. [Source][features], [checks][checks]. |
| `inlayHint/resolve` | C → S | Open | Hints are eager; no resolve provider. |
| `textDocument/semanticTokens/full` | C → S | Conditional | Requires a negotiated full provider and relative format. Cached UTF-16 tokens use a compact supported legend, type fallbacks, and repacked modifier bits. Unsupported kinds are omitted before relative encoding. [Source][capabilities], [tokens][features], [checks][checks]. |
| `textDocument/semanticTokens/full/delta` | C → S | Conditional | Requires negotiated delta support. One previous generation; unknown/older IDs return full data. Deltas compare arrays encoded with the session's immutable legend. Range queries preserve the full-token cache. [Source][features], [checks][checks]. |
| `textDocument/semanticTokens/range` | C → S | Conditional | Requires negotiated range support. Returns complete overlapping tokens in UTF-16, with multiline tokens split by line. Uses the same negotiated legend as full tokens. [Source][features], [checks][checks]. |
| `textDocument/diagnostic` | C → S | Conditional | Advertised for pull-diagnostic clients; full/unchanged result IDs and dependency invalidation. Lambda analysis uses owned syntax copies and current import signatures. Diagnostics inherit native frontend limits. [Source][diagnostics], [analysis][analysis], [checks][checks]. |
| `workspace/diagnostic` | C → S | Conditional | Pull-diagnostic sessions; workspace/open-file reports, previous result IDs and removal reports. No partial-result streaming. [Source][diagnostics], [checks][checks]. |
| `textDocument/codeAction` | C → S | Conditional | Import quick fixes require action-literal support. Honors `context.only`, compiler path precedence, callable overload safety, ambiguity/privacy/conflict checks and negotiated versioned edits. `refactor.extract` offers extract-to-variable for a single-line expression selection; refactors are opt-in through `context.only`. Other fixes and refactorings remain open. [Source][actions], [checks][checks]. |
| `codeAction/resolve` | C → S | Open | Import edits are eager; no lazy action resolution. |
| `workspace/executeCommand` | C → S | Supported | Commands `silver.extractVariable` (arguments `[uri, range, name?]`, answered via `workspace/applyEdit` when the client supports it, else the `WorkspaceEdit` is the result) and `silver.openDocument` (answers through `window/showDocument` when supported). Unknown commands return `-32602`. [Source][dispatch], [checks][checks]. |
| `textDocument/codeLens` | C → S | Supported | One lens per non-local declaration, carrying the document URI, symbol index and revision. Counts are deferred to resolve. Runnable-target and implementation lenses remain open. [Source][lenses], [checks][checks]. |
| `codeLens/resolve` | C → S | Supported | Counts references across the workspace and returns an `N references` title; stale revisions and unknown symbols resolve to `null`. [Source][lenses], [checks][checks]. |
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
| `textDocument/moniker` | C → S | Supported | Statically advertised (`monikerProvider`). Resolves the symbol at the position and returns a `silver`-scheme, project-unique moniker keyed by document URI and qualified name; `null` off-symbol. [Source][navigation], [checks][checks]. |
| `textDocument/documentColor` | C → S | Unmapped | No general Silver color literal; library-specific color APIs need an explicit mapping. |
| `textDocument/colorPresentation` | C → S | Unmapped | Requires the color model above. |
| `textDocument/willSaveWaitUntil` | C → S | Conditional | Advertised with `willSaveWaitUntil` client support; always answers `null` (no save-time edits). [Source][dispatch], [checks][checks]. |
| `workspace/willCreateFiles` | C → S | Conditional | Requires `workspace.fileOperations.willCreate`; validates the `files` array and answers `null` (no edits needed on create). [Source][file-operations], [checks][checks]. |
| `workspace/willRenameFiles` | C → S | Conditional | Requires `workspace.fileOperations.willRename`. Rewrites import module paths for renamed files and directories, preserving the base that produced the original name and any selection block or alias. Versioned `documentChanges` when negotiated. [Source][file-operations], [checks][checks]. |
| `workspace/willDeleteFiles` | C → S | Conditional | Requires `workspace.fileOperations.willDelete`; validates the `files` array and answers `null`. [Source][file-operations], [checks][checks]. |
| `workspace/textDocumentContent` | C → S | Open | No virtual document content provider. Added to the stable inventory in 3.18. |

## Server-to-client requests

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `workspace/diagnostic/refresh` | S → C | Conditional | Pull-diagnostic sessions with `workspace.diagnostics.refreshSupport`. Coalesces requests; changes during an outstanding request schedule another. Client errors disable further requests of this kind. [Source][client-requests], [checks][checks]. |
| `workspace/semanticTokens/refresh` | S → C | Conditional | Requires an enabled semantic-token provider and `workspace.semanticTokens.refreshSupport`; dependency invalidation, coalescing and acknowledgement/error handling. [Source][client-requests], [checks][checks]. |
| `workspace/foldingRange/refresh` | S → C | Conditional | Requires `workspace.foldingRange.refreshSupport`; coalesced like other refresh kinds and disabled alone on client error. [Source][client-requests], [checks][checks]. |
| `workspace/inlayHint/refresh` | S → C | Conditional | Requires `workspace.inlayHint.refreshSupport`. Analysis changes ask editors to recompute hints from current snapshots, including imported signatures. Coalesces requests, repeats after acknowledgement when needed, and disables only this kind on client error. [Source][client-requests], [checks][checks]. |
| `workspace/codeLens/refresh` | S → C | Conditional | Requires `workspace.codeLens.refreshSupport`; sent on analysis changes alongside the other refresh kinds. [Source][client-requests], [checks][checks]. |
| `workspace/inlineValue/refresh` | S → C | Open | No inline-value provider or refresh request. |
| `workspace/textDocumentContent/refresh` | S → C | Open | No virtual content provider or refresh request. Added to the stable inventory in 3.18. |
| `workspace/workspaceFolders` | S → C | Conditional | Sent once after `initialized` when the client supports `workspace.workspaceFolders` and `initialize` carried neither `rootUri` nor `workspaceFolders`; the response adds roots and rebuilds the index. [Source][client-requests], [checks][checks]. |
| `workspace/configuration` | S → C | Conditional | Requires `workspace.configuration`; requested for section `silver` on `workspace/didChangeConfiguration`. The `trace` key sets the trace level. [Source][client-requests], [checks][checks]. |
| `workspace/applyEdit` | S → C | Conditional | Requires `workspace.applyEdit`; used by `workspace/executeCommand` (`silver.extractVariable`) to hand the edit to the client. The originating command is answered from the applyEdit response; a decline returns `-32802`. [Source][client-requests], [checks][checks]. |
| `client/registerCapability` | S → C | Open | The server never sends dynamic registrations (watched-file handling runs unconditionally). Nonstandard inbound requests are tolerated: `registrations` are validated (string `id` and `method` per entry) and answered `null`. [Source][dispatch], [checks][checks]. |
| `client/unregisterCapability` | S → C | Open | The server never sends dynamic unregistrations. Nonstandard inbound requests are tolerated: `unregisterations` (the spec's own field spelling) are validated and answered `null`. [Source][dispatch], [checks][checks]. |
| `window/workDoneProgress/create` | S → C | Conditional | Requires `window.workDoneProgress`; sent once after `initialized` to create the `silver/index` token. Background indexing then reports begin/report/end `$/progress` on that token, and `window/workDoneProgress/cancel` for it stops the scan. [Source][client-requests], [checks][checks]. |
| `window/showMessageRequest` | S → C | Conditional | Sent when a watched package manifest changes, offering `Reload workspace`/`Ignore`; `Reload workspace` rebuilds the index. Responses to other server requests are consumed without action. [Source][client-requests], [checks][checks]. |
| `window/showDocument` | S → C | Conditional | Requires `window.showDocumentSupport`; sent by the `silver.openDocument` command with an optional selection. [Source][client-requests], [checks][checks]. |

## Notifications

| Method | Direction | Status | Scope, limits, and evidence |
| --- | --- | --- | --- |
| `initialized` | C → S | Supported | Starts background workspace indexing after initialization. [Source][dispatch], [checks][checks]. |
| `exit` | C → S | Supported | Stops the reader and process, including an incomplete following frame. [Source][dispatch], [checks][checks]. |
| `$/cancelRequest` | Both | Partial | Incoming cancellation of queued/running requests; IDs are reusable after completion. Unknown IDs are discarded. A full input queue interrupts eligible analysis with `RequestFailed` so intake can resume; this is distinct from observed client cancellation. No outgoing cancellation of client requests. Checks occur between files/items, not inside frontend passes. [Source][transport], [checks][checks]. |
| `$/progress` | Both | Partial | Outgoing begin/report/end for client-supplied tokens on selected queries and for the server-created `silver/index` indexing token. No incoming progress consumer, partial-result streaming or percentages. [Source][progress], [checks][checks]. |
| `$/setTrace` | C → S | Supported | Stores `off`/`messages`/`verbose`; unknown values are ignored. The level can also be set through the `silver` configuration section. [Source][dispatch], [checks][checks]. |
| `$/logTrace` | S → C | Conditional | Sent for each incoming message while the trace level is `messages` or above; `verbose` includes the params payload. [Source][json-rpc], [checks][checks]. |
| `textDocument/didOpen` | C → S | Supported | Unsaved overlay snapshots, analysis and dependency invalidation. [Source][document], [checks][checks]. |
| `textDocument/didChange` | C → S | Supported | Sequential UTF-16 incremental edits, atomic batches and monotonic document versions. Invalid batches preserve the snapshot. [Source][document], [checks][checks]. |
| `textDocument/didClose` | C → S | Supported | Clears push diagnostics and restores disk contents for dependent navigation. [Source][dispatch], [checks][checks]. |
| `textDocument/didSave` | C → S | Supported | Refreshes analysis/diagnostics; when the client offered `save.includeText`, the save text replaces the open buffer (as a new revision) before republishing. [Source][dispatch], [checks][checks]. |
| `textDocument/willSave` | C → S | Conditional | Advertised with `willSave` client support; accepted without a response (`didSave` republishes analysis, including `includeText` save text). [Source][dispatch], [checks][checks]. |
| `textDocument/publishDiagnostics` | S → C | Conditional | Push mode for clients without pull diagnostics; negotiated versions and related locations. Native frontend analysis limits apply. [Source][diagnostics], [checks][checks]. |
| `workspace/didChangeWorkspaceFolders` | C → S | Supported | Atomic normalized add/remove batches; refreshes imports and indexing. Removed closed files leave workspace queries; open files and reachable imports remain available. [Source][workspace], [checks][checks]. |
| `workspace/didChangeWatchedFiles` | C → S | Supported | Client-sent disk refreshes for closed files; preserves open overlays. No native filesystem watcher or dynamic watcher registration. [Source][dispatch], [checks][checks]. |
| `workspace/didChangeConfiguration` | C → S | Supported | Requests the `silver` configuration section when the client supports `workspace/configuration`, then invalidates analysis like other change notifications. [Source][dispatch], [checks][checks]. |
| `workspace/didCreateFiles` | C → S | Supported | New `.ag` files join the background index; new `silver.toml` manifests rebuild the workspace scope. [Source][file-operations], [checks][checks]. |
| `workspace/didRenameFiles` | C → S | Supported | Drops closed documents for the old URI and indexes the new path; manifests rebuild the scope. Import rewrites stay with `workspace/willRenameFiles`. [Source][file-operations], [checks][checks]. |
| `workspace/didDeleteFiles` | C → S | Supported | Removes closed documents for deleted URIs and invalidates imports. [Source][file-operations], [checks][checks]. |
| `window/workDoneProgress/cancel` | C → S | Supported | Cancels the operation for a client-supplied request progress token, and also stops background indexing when the token is the server-created `silver/index`. [Source][transport], [checks][checks]. |
| `window/showMessage` | S → C | Supported | Sent as a warning when a watched package manifest changes. [Source][json-rpc], [checks][checks]. |
| `window/logMessage` | S → C | Supported | Sent for package-manifest preload errors. [Source][json-rpc], [checks][checks]. |
| `telemetry/event` | S → C | Conditional | Sent on `initialized` and `shutdown` only when `initializationOptions.telemetry` is true. [Source][json-rpc], [checks][checks]. |
| `notebookDocument/didOpen` | C → S | Unmapped | No Silver notebook/cell execution or synchronization model. |
| `notebookDocument/didChange` | C → S | Unmapped | Requires a notebook document model. |
| `notebookDocument/didSave` | C → S | Unmapped | Requires a notebook document model. |
| `notebookDocument/didClose` | C → S | Unmapped | Requires a notebook document model. |

## Verification and maintenance

[Protocol checks][checks] exercise real framed stdio sessions against the built
server, not source-text checks or capability flags alone. The test suite contains
140 distinct test scenarios in `scripts/check_lsp.py` (generated by `python3 scripts/check_lsp.py --list`; keep this list in sync):

- `test_active_progress_cancellation_behind_saturated_edit_pipeline`, `test_active_progress_cancellation_behind_saturated_request_pipeline`, `test_active_progress_interruption_behind_saturated_edits_without_cancel`, `test_ambiguous_methods_do_not_choose_an_arbitrary_implementation`,
- `test_auto_import_at_file_start_combines_edits_without_overlap`, `test_auto_import_candidates_preserve_distinct_sources_and_reject_bad_bindings`, `test_auto_import_completion_applies_primary_and_import_edits_and_resolves`, `test_auto_import_completion_preserves_overloads_from_one_provider`,
- `test_auto_import_completion_tracks_open_source_modules_and_close_restores_disk_state`, `test_auto_import_discovers_source_sysroot_std_modules_outside_the_workspace`, `test_auto_import_real_stdlib_string_allows_member_overloads`, `test_auto_import_resolve_preserves_source_edits_and_rejects_stale_revisions`,
- `test_call_hierarchy_groups_call_sites_and_rejects_stale_items`, `test_call_hints_ignore_comments_between_callee_and_arguments`, `test_cancelled_index_request_and_bounded_pipeline`, `test_chained_members_resolve_receiver_identity_despite_shadowing`,
- `test_client_formats_flat_symbols_and_versioned_workspace_edits`, `test_client_refresh_requests_coalesce_without_losing_later_changes`, `test_closed_source_rehydrates_without_invalidating_completion_or_hierarchy`, `test_code_lens_and_folding_refresh_requests`,
- `test_code_lens_reports_reference_counts`, `test_code_lens_resolve_rejects_stale_revision`, `test_completion_responds_with_available_data_while_indexing_continues`, `test_completion_returns_ranked_top_hundred_and_requeries_truncated_lists`,
- `test_completion_signature_and_structural_features`, `test_completion_snippets_and_deferred_details_preserve_edits_and_reject_stale_items`, `test_configuration_request_and_trace_levels`, `test_counter_scope_does_not_escape_loop`,
- `test_diagnostics_invalidate_dependents_without_republishing_unrelated_files`, `test_diagnostics_stay_owned_by_document_and_close_clears`, `test_directory_move_rewrites_imports_from_the_new_home`, `test_directory_rename_rewrites_nested_imports`,
- `test_disk_sources_reject_invalid_encoding_without_exposing_truncated_symbols`, `test_document_links_follow_active_imports_unsaved_modules_and_disk_precedence`, `test_dot_receivers_follow_alias_context_without_guessing_value_types`, `test_dynamic_capability_registration_roundtrip`,
- `test_execute_command_apply_edit_and_show_document`, `test_execute_command_extract_variable_returns_workspace_edit`, `test_exit_stops_reader_with_incomplete_following_frame`, `test_extract_variable_refactor`,
- `test_extract_variable_skips_quickfix_only_requests`, `test_file_move_across_directories_rewrites_import`, `test_file_move_skips_names_shadowed_by_a_nearer_file`, `test_file_operation_will_requests_and_did_notifications`,
- `test_file_rename_requires_client_capability`, `test_file_rename_rewrites_imports`, `test_folding_honors_line_only_clients_and_range_limits`, `test_formatting_options_ranges_comments_and_incomplete_code`,
- `test_formatting_preserves_multiline_tokens_and_utf16_range`, `test_function_rename_and_references_exclude_import_module_paths`, `test_fuzzy_completion_ranking_replaces_the_whole_identifier_and_honors_scope`, `test_generic_commas_comments_and_operators_do_not_shift_call_arguments`,
- `test_generics_and_private_import_visibility`, `test_grouped_import_alias_rename_preserves_dotted_module_path`, `test_import_completion_context_survives_repeated_snapshot_changes`, `test_import_completion_uses_absolute_compiler_search_roots`,
- `test_import_path_completion_uses_module_paths_and_replaces_the_full_segment`, `test_import_quickfix_applies_to_current_source_and_resolves_diagnostics`, `test_import_quickfix_preserves_multiple_choices_and_finds_types_in_unsaved_modules`, `test_import_quickfix_rejects_collisions_private_symbols_and_wrong_module_paths`,
- `test_import_quickfix_uses_versions_and_excludes_legacy_clients`, `test_import_selection_originals_do_not_bind_to_local_names`, `test_import_suggestions_reject_callable_overloads_that_make_existing_calls_ambiguous`, `test_import_suggestions_reject_conflicting_types_in_new_dependencies`,
- `test_import_triggers_are_contextual_and_replace_existing_segments`, `test_imported_primitive_members_and_types_shadowed_by_value_names`, `test_incomplete_code_keeps_local_completion_and_signature_help`, `test_inlay_refresh_coalesces_changes_and_ignores_invalid_or_duplicate_acknowledgements`,
- `test_inlay_refresh_is_not_requested_on_save`, `test_inlay_refresh_recomputes_unchanged_callers_before_acknowledgement`, `test_inlay_refresh_requires_literal_true_and_hints_remain_available`, `test_inlay_refresh_shares_ordered_ids_and_errors_disable_only_the_failed_kind`,
- `test_inlay_refresh_tracks_workspace_folder_changes`, `test_installed_sysroot_source_import`, `test_jsonrpc_params_must_be_structured_and_invalid_shutdown_does_not_stop_server`, `test_keyword_and_type_completions_have_plain_text_kinds_and_respect_context`,
- `test_lambda_diagnostics_use_unsaved_imported_methods_and_invalidate_dependencies`, `test_lambda_example_diagnostics_and_hints_use_lambda_semantics`, `test_lambda_parameters_shadow_captures_only_inside_their_own_body`, `test_lint_does_not_treat_generic_type_arguments_as_parameters`,
- `test_local_inference_uses_the_lambda_scope_without_leaking_parameter_types`, `test_manifest_change_prompts_reload_via_show_message_request`, `test_manifest_changes_rebuild_package_scope_and_recover_from_invalid_input`, `test_match_bindings_are_scoped_to_their_arm_and_payload_calls_are_not_declarations`,
- `test_moniker_and_inline_completion`, `test_multi_range_formatting_merges_selections_and_validates_every_utf16_range`, `test_nearer_unsaved_import_rebinds_cached_direct_transitive_and_unused_origins`, `test_nested_and_incomplete_lambda_parameters_remain_local`,
- `test_nested_manifest_discovers_external_packages_during_preload`, `test_nested_match_and_guard_occurrences_keep_distinct_binding_identities`, `test_new_unsaved_module_participates_in_import_resolution`, `test_on_type_formatting_indents_and_dedents_without_touching_literals`,
- `test_open_file_discovers_parent_manifest_without_workspace_folders`, `test_parameter_hints_distinguish_names_from_expressions`, `test_parameter_hints_stop_at_mismatched_delimiters`, `test_protocol_errors_and_string_request_ids`,
- `test_pull_diagnostics_cache_dependency_changes_and_refresh_acknowledgements`, `test_push_lambda_diagnostics_report_real_errors_and_clear_after_edits`, `test_rename_rejects_capture`, `test_rename_rejects_capture_of_imported_function_and_preserves_safe_uses`,
- `test_rename_to_imported_allows_predeclaration_and_outside_block_uses`, `test_repeated_closed_workspace_analysis_releases_discarded_snapshots`, `test_request_progress_cancellation_and_completion_are_balanced`, `test_save_republishes_overlay_diagnostics_or_requests_pull_refresh`,
- `test_save_will_save_and_include_text`, `test_scope_aware_navigation_and_rename`, `test_selection_ranges_validate_the_entire_utf16_position_batch`, `test_selective_import_alias_has_independent_rename_identity`,
- `test_selective_import_completion_offers_only_unselected_public_exports`, `test_semantic_compact_deltas_preserve_baselines_across_ranges_and_dependencies`, `test_semantic_compact_legends_fallbacks_and_dropped_tokens_keep_positions`, `test_semantic_modifier_bits_are_repacked`,
- `test_semantic_range_returns_whole_multiline_tokens_with_utf16_lengths`, `test_semantic_request_modes_negotiate_and_gate_each_method`, `test_semantic_token_deltas_round_trip_and_ranges_use_utf16`, `test_semantic_tokens_invalidate_when_an_import_changes`,
- `test_semantic_tokens_require_complete_usable_client_capabilities`, `test_sequential_completions_are_not_abandoned`, `test_server_help_and_version_flags`, `test_signature_parameter_labels_negotiate_exact_utf16_offsets`,
- `test_single_character_prefix_includes_auto_imports_after_indexing`, `test_startup_preloads_std_and_transitive_manifest_packages`, `test_stdio_eof_drains_pipelined_lifecycle_responses`, `test_superseded_completions_are_abandoned`,
- `test_symlinked_source_modules_complete_paths_and_auto_imports`, `test_telemetry_events_when_opted_in`, `test_trait_document_symbols_nest_trait_method_without_duplicating_impl`, `test_trait_method_rename_updates_contract_implementations_and_calls`,
- `test_transport_rejects_invalid_utf8_nul_suffixes_and_noninteger_request_ids`, `test_type_declaration_and_implementation_navigation`, `test_type_hierarchy_follows_trait_implementations`, `test_type_qualified_enum_dot_calls_navigate_and_rename`,
- `test_type_qualified_static_dot_calls_preserve_value_receiver_targets`, `test_unopened_files_imports_and_unsaved_overlay`, `test_utf16_incremental_changes_are_atomic_and_versioned`, `test_watched_files_refresh_closed_imports`,
- `test_work_done_progress_create_and_indexing_reports`, `test_workspace_folder_batches_are_atomic_and_preserve_sysroot_imports`, `test_workspace_folder_removal_excludes_closed_files_from_all_workspace_queries`, `test_workspace_folders_request_when_no_root_supplied`.st_ids`,
  `test_jsonrpc_params_must_be_structured_and_invalid_shutdown_does_not_stop_server`,
  `test_transport_rejects_invalid_utf8_nul_suffixes_and_noninteger_request_ids`,
  `test_cancelled_index_request_and_bounded_pipeline`,
  `test_active_progress_cancellation_behind_saturated_edit_pipeline`,
  `test_active_progress_interruption_behind_saturated_edits_without_cancel`,
  `test_active_progress_cancellation_behind_saturated_request_pipeline`,
  `test_stdio_eof_drains_pipelined_lifecycle_responses`,
  `test_exit_stops_reader_with_incomplete_following_frame`.
- Synchronization/workspace: `test_utf16_incremental_changes_are_atomic_and_versioned`,
  `test_save_republishes_overlay_diagnostics_or_requests_pull_refresh`,
  `test_unopened_files_imports_and_unsaved_overlay`,
  `test_new_unsaved_module_participates_in_import_resolution`,
  `test_nearer_unsaved_import_rebinds_cached_direct_transitive_and_unused_origins`,
  `test_workspace_folder_removal_excludes_closed_files_from_all_workspace_queries`,
  `test_workspace_folder_batches_are_atomic_and_preserve_sysroot_imports`,
  `test_watched_files_refresh_closed_imports`,
  `test_installed_sysroot_source_import`,
  `test_disk_sources_reject_invalid_encoding_without_exposing_truncated_symbols`,
  `test_startup_preloads_std_and_transitive_manifest_packages`,
  `test_manifest_changes_rebuild_package_scope_and_recover_from_invalid_input`,
  `test_nested_manifest_discovers_external_packages_during_preload`,
  `test_open_file_discovers_parent_manifest_without_workspace_folders`,
  `test_closed_source_rehydrates_without_invalidating_completion_or_hierarchy`,
  `test_repeated_closed_workspace_analysis_releases_discarded_snapshots`.
- Navigation/rename: `test_scope_aware_navigation_and_rename`,
  `test_match_bindings_are_scoped_to_their_arm_and_payload_calls_are_not_declarations`,
  `test_nested_match_and_guard_occurrences_keep_distinct_binding_identities`,
  `test_counter_scope_does_not_escape_loop`,
  `test_lambda_parameters_shadow_captures_only_inside_their_own_body`,
  `test_nested_and_incomplete_lambda_parameters_remain_local`,
  `test_local_inference_uses_the_lambda_scope_without_leaking_parameter_types`,
  `test_rename_rejects_capture`,
  `test_rename_rejects_capture_of_imported_function_and_preserves_safe_uses`,
  `test_rename_to_imported_allows_predeclaration_and_outside_block_uses`,
  `test_function_rename_and_references_exclude_import_module_paths`,
  `test_grouped_import_alias_rename_preserves_dotted_module_path`,
  `test_selective_import_alias_has_independent_rename_identity`,
  `test_import_selection_originals_do_not_bind_to_local_names`,
  `test_type_qualified_enum_dot_calls_navigate_and_rename`,
  `test_type_qualified_static_dot_calls_preserve_value_receiver_targets`,
  `test_dot_receivers_follow_alias_context_without_guessing_value_types`,
  `test_chained_members_resolve_receiver_identity_despite_shadowing`,
  `test_imported_primitive_members_and_types_shadowed_by_value_names`,
  `test_generics_and_private_import_visibility`,
  `test_type_declaration_and_implementation_navigation`,
  `test_ambiguous_methods_do_not_choose_an_arbitrary_implementation`,
  `test_trait_method_rename_updates_contract_implementations_and_calls`.
- Completion/structure: `test_completion_signature_and_structural_features`,
  `test_incomplete_code_keeps_local_completion_and_signature_help`,
  `test_import_path_completion_uses_module_paths_and_replaces_the_full_segment`,
  `test_import_completion_uses_absolute_compiler_search_roots`,
  `test_selective_import_completion_offers_only_unselected_public_exports`,
  `test_import_completion_context_survives_repeated_snapshot_changes`,
  `test_keyword_and_type_completions_have_plain_text_kinds_and_respect_context`,
  `test_auto_import_completion_applies_primary_and_import_edits_and_resolves`,
  `test_auto_import_completion_preserves_overloads_from_one_provider`,
  `test_auto_import_at_file_start_combines_edits_without_overlap`,
  `test_single_character_prefix_includes_auto_imports_after_indexing`,
  `test_completion_returns_ranked_top_hundred_and_requeries_truncated_lists`,
  `test_completion_responds_with_available_data_while_indexing_continues`,
  `test_import_triggers_are_contextual_and_replace_existing_segments`,
  `test_auto_import_candidates_preserve_distinct_sources_and_reject_bad_bindings`,
  `test_auto_import_completion_tracks_open_source_modules_and_close_restores_disk_state`,
  `test_auto_import_discovers_source_sysroot_std_modules_outside_the_workspace`,
  `test_auto_import_real_stdlib_string_allows_member_overloads`,
  `test_symlinked_source_modules_complete_paths_and_auto_imports`,
  `test_auto_import_resolve_preserves_source_edits_and_rejects_stale_revisions`,
  `test_fuzzy_completion_ranking_replaces_the_whole_identifier_and_honors_scope`,
  `test_completion_snippets_and_deferred_details_preserve_edits_and_reject_stale_items`,
  `test_signature_parameter_labels_negotiate_exact_utf16_offsets`,
  `test_generic_commas_comments_and_operators_do_not_shift_call_arguments`,
  `test_call_hints_ignore_comments_between_callee_and_arguments`,
  `test_trait_document_symbols_nest_trait_method_without_duplicating_impl`,
  `test_parameter_hints_distinguish_names_from_expressions`,
  `test_parameter_hints_stop_at_mismatched_delimiters`,
  `test_folding_honors_line_only_clients_and_range_limits`,
  `test_selection_ranges_validate_the_entire_utf16_position_batch`,
  `test_client_formats_flat_symbols_and_versioned_workspace_edits`.
- Hierarchies: `test_call_hierarchy_groups_call_sites_and_rejects_stale_items`,
  `test_type_hierarchy_follows_trait_implementations`.
- Diagnostics/tokens/progress: `test_diagnostics_stay_owned_by_document_and_close_clears`,
  `test_diagnostics_invalidate_dependents_without_republishing_unrelated_files`,
  `test_lint_does_not_treat_generic_type_arguments_as_parameters`,
  `test_lambda_example_diagnostics_and_hints_use_lambda_semantics`,
  `test_push_lambda_diagnostics_report_real_errors_and_clear_after_edits`,
  `test_lambda_diagnostics_use_unsaved_imported_methods_and_invalidate_dependencies`,
  `test_pull_diagnostics_cache_dependency_changes_and_refresh_acknowledgements`,
  `test_semantic_token_deltas_round_trip_and_ranges_use_utf16`,
  `test_semantic_tokens_require_complete_usable_client_capabilities`,
  `test_semantic_request_modes_negotiate_and_gate_each_method`,
  `test_semantic_compact_legends_fallbacks_and_dropped_tokens_keep_positions`,
  `test_semantic_modifier_bits_are_repacked`,
  `test_semantic_compact_deltas_preserve_baselines_across_ranges_and_dependencies`,
  `test_semantic_range_returns_whole_multiline_tokens_with_utf16_lengths`,
  `test_semantic_tokens_invalidate_when_an_import_changes`,
  `test_request_progress_cancellation_and_completion_are_balanced`,
  `test_client_refresh_requests_coalesce_without_losing_later_changes`.
- Hint refresh: `test_inlay_refresh_requires_literal_true_and_hints_remain_available`,
  `test_inlay_refresh_coalesces_changes_and_ignores_invalid_or_duplicate_acknowledgements`,
  `test_inlay_refresh_shares_ordered_ids_and_errors_disable_only_the_failed_kind`,
  `test_inlay_refresh_recomputes_unchanged_callers_before_acknowledgement`,
  `test_inlay_refresh_tracks_workspace_folder_changes`,
  `test_inlay_refresh_is_not_requested_on_save`.
- Actions/links: `test_import_quickfix_applies_to_current_source_and_resolves_diagnostics`,
  `test_import_quickfix_rejects_collisions_private_symbols_and_wrong_module_paths`,
  `test_import_suggestions_reject_conflicting_types_in_new_dependencies`,
  `test_import_suggestions_reject_callable_overloads_that_make_existing_calls_ambiguous`,
  `test_import_quickfix_preserves_multiple_choices_and_finds_types_in_unsaved_modules`,
  `test_import_quickfix_uses_versions_and_excludes_legacy_clients`,
  `test_document_links_follow_active_imports_unsaved_modules_and_disk_precedence`.
- Formatting: `test_formatting_preserves_multiline_tokens_and_utf16_range`,
  `test_formatting_options_ranges_comments_and_incomplete_code`,
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
[imports]: ../bin/aglsp/src/imports.ag
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
[hierarchy]: ../bin/aglsp/src/hierarchy.ag
[file-operations]: ../bin/aglsp/src/file_operations.ag
[lenses]: ../bin/aglsp/src/lenses.ag
