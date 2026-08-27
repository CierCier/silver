// Silver Language Server — diagnostics, hover (type + definition + docs),
// go-to-definition, find-references, rename, completion, signature help,
// semantic highlighting, and import resolution.

mod completion;
mod diagnostics;
mod doc;
mod document_symbols;
mod format;
mod inlay_hints;
mod references;
mod semantic_tokens;
mod util;

use agc::module_loader::ModuleLoader;
use agc::parser::ast;
use parking_lot::Mutex;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::path::PathBuf;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};

use agc::symbol_index::SymbolIndex;
use util::*;

pub(crate) struct Backend {
    pub(crate) client: Client,
    pub(crate) documents: Mutex<HashMap<Uri, String>>,
    /// Per‑URI analysis: source text, symbols, occurrences, expression types.
    pub(crate) cache: Mutex<HashMap<Uri, SymbolIndex>>,
    pub(crate) loader: ModuleLoader,
    /// Path → (mtime_nanos, fully-parsed program) for imported files.
    pub(crate) file_cache: parking_lot::Mutex<HashMap<PathBuf, (u128, ast::Program)>>,
    /// Diagnostic URIs published by the last diagnostics check.
    pub(crate) diagnostic_uris: Mutex<HashSet<Uri>>,
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), ":".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), ",".into()]),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                // The current formatter is whitespace-only, so it is not advertised as document formatting.
                semantic_tokens_provider: Some(semantic_tokens::server_capability()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Silver LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.documents.lock().insert(uri.clone(), text.clone());
        self.check_diagnostics(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = {
            let mut docs = self.documents.lock();
            let doc = docs.entry(uri.clone()).or_insert_with(String::new);
            apply_document_changes(doc, params.content_changes);
            doc.clone()
        };
        self.check_diagnostics(&uri, &text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.lock().remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);

        let mut parts: Vec<String> = Vec::new();

        // Type of the expression under the cursor.
        if let Some(ty) = find_expr_type(offset, &analysis.expr_types) {
            parts.push(format!("**type:** `{ty}`"));
        }

        // Symbol under the cursor: signature + documentation.
        if let Some(sym) = references::symbol_under_cursor(analysis, offset) {
            parts.push(format!("```silver\n{}\n```", sym.signature));
            if let Some(doc) = &sym.doc {
                parts.push(doc::doc_to_markdown(doc));
            }
        } else if let Some((_, _, name)) = extract_identifier(&analysis.text, offset) {
            if is_builtin_type(&name) {
                parts.push(format!("**type:** `{name}`"));
            } else if is_keyword(&name) {
                parts.push(format!("`{name}` — keyword"));
            }
        }

        if parts.is_empty() {
            return Ok(None);
        }
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: parts.join("\n\n"),
            }),
            range: None,
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);
        let Some(sym) = references::symbol_under_cursor(analysis, offset) else {
            return Ok(None);
        };
        let Some(location) = references::location_for_span(analysis, uri, &sym.span) else {
            return Ok(None);
        };
        Ok(Some(GotoDefinitionResponse::Scalar(location)))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);
        let analyses: HashMap<Uri, SymbolIndex> = cache
            .iter()
            .map(|(cached_uri, analysis)| (cached_uri.clone(), analysis.clone()))
            .collect();
        drop(cache);
        let locations = references::symbol_occurrences_across_buffers(
            &analyses,
            uri,
            offset,
            params.context.include_declaration,
        )
        .into_iter()
        .filter_map(|(cached_uri, analysis, occ)| {
            references::location_for_span(analysis, cached_uri, &occ.span)
        })
        .collect();
        Ok(Some(locations))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        if !references::is_valid_identifier(&params.new_name) {
            return Err(tower_lsp_server::jsonrpc::Error::invalid_params(format!(
                "`{}` is not a valid identifier",
                params.new_name
            )));
        }

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);
        let analyses: HashMap<Uri, SymbolIndex> = cache
            .iter()
            .map(|(cached_uri, analysis)| (cached_uri.clone(), analysis.clone()))
            .collect();
        drop(cache);
        Ok(references::rename_edit(
            &analyses,
            uri,
            offset,
            &params.new_name,
        ))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);
        let items = completion::completion(analysis, offset);
        if items.is_empty() {
            return Ok(None);
        }
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let offset = position_to_byte(&analysis.text, pos);
        Ok(completion::signature_help(analysis, offset))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        Ok(Some(SemanticTokensResult::Tokens(
            semantic_tokens::semantic_tokens(analysis),
        )))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let symbols = document_symbols::document_symbols(analysis);
        if symbols.is_empty() {
            return Ok(None);
        }
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let hints = inlay_hints::inlay_hints(analysis);
        if hints.is_empty() {
            return Ok(None);
        }
        Ok(Some(hints))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let cache = self.cache.lock();
        let Some(analysis) = cache.get(uri) else {
            return Ok(None);
        };
        let formatted = format::format_silver(&analysis.text);
        if formatted == analysis.text {
            return Ok(None);
        }
        let end = byte_to_position(&analysis.text, analysis.text.len());
        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end,
            },
            new_text: formatted,
        }]))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Mutex::new(HashMap::default()),
        cache: Mutex::new(HashMap::default()),
        loader: build_lsp_loader(),
        file_cache: Mutex::new(HashMap::default()),
        diagnostic_uris: Mutex::new(HashSet::default()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
