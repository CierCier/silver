// Silver Language Server — diagnostics, hover (type + definition), go-to-definition, import resolution.

mod diagnostics;
mod format;
mod util;

use agc::parser::ast;
use agc::module_loader::ModuleLoader;
use std::collections::HashMap;
use std::path::PathBuf;
use parking_lot::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use util::*;

pub(crate) struct Backend {
    pub(crate) client: Client,
    /// Per‑URI: (source_text, expr_types, definitions, hover_texts)
    pub(crate) cache: Mutex<HashMap<Url, (String, ExprTypeMap, DefMap, HoverTextMap)>>,
    pub(crate) loader: ModuleLoader,
    /// Path → (mtime_nanos, fully-parsed program) for imported files.
    pub(crate) file_cache: parking_lot::Mutex<HashMap<PathBuf, (u128, ast::Program)>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
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
        self.check_diagnostics(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.check_diagnostics(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}


    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some((text, expr_types, _defs, hover_texts)) = cache.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_byte(text, pos);

        // Build hover parts: type info + formatted definition info.
        let mut parts: Vec<MarkedString> = Vec::new();

        // Type from expression.
        if let Some(ty) = find_expr_type(offset, expr_types) {
            parts.push(MarkedString::String(format!("type: {ty}")));
        }

        // Pre-formatted hover text from definition (top-level items, imported items).
        if let Some((_, _, name)) = extract_identifier(text, offset) {
            if let Some(hover_text) = hover_texts.get(&name) {
                parts.push(MarkedString::String(hover_text.clone()));
            } else if is_builtin_type(&name) {
                parts.push(MarkedString::String("builtin".to_string()));
            } else if is_keyword(&name) {
                parts.push(MarkedString::String("keyword".to_string()));
            }
        }
        if parts.is_empty() {
            return Ok(None);
        }

        let contents = if parts.len() == 1 {
            HoverContents::Scalar(parts.into_iter().next().unwrap())
        } else {
            HoverContents::Array(parts)
        };

        Ok(Some(Hover { contents, range: None }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some((text, _expr_types, defs, _hover_texts)) = cache.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_byte(text, pos);
        let Some((_, _, name)) = extract_identifier(text, offset) else {
            return Ok(None);
        };
        let Some(def_span) = defs.get(&name) else {
            return Ok(None);
        };

        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range: span_to_range(text, def_span),
        })))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache: Mutex::new(HashMap::new()),
        loader: build_lsp_loader(),
        file_cache: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
