// Silver Language Server — diagnostics + hover via compiler frontend.

use agc::lexer;
use agc::lexer::Span;
use agc::parser::Parser;
use agc::semantic::typeck::TypeChecker;
use agc::symbol_table::CompilerSymbolTable;

use std::collections::HashMap;
use parking_lot::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// Convert byte offset to 0-based line/col from source text.
fn byte_to_position(text: &str, offset: usize) -> Position {
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position { line, character: col }
}

fn span_to_range(text: &str, span: &Span) -> Range {
    Range {
        start: byte_to_position(text, span.start),
        end: byte_to_position(text, span.end),
    }
}

/// Convert an LSP position to a byte offset in `text`.
fn position_to_byte(text: &str, pos: Position) -> usize {
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    for (i, ch) in text.char_indices() {
        if line == pos.line && col == pos.character {
            return i;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    text.len()
}

/// Find the tightest expression span containing `offset`.
fn find_expr_type(
    offset: usize,
    map: &HashMap<(usize, usize), String>,
) -> Option<String> {
    map.iter()
        .filter(|((start, end), _)| *start <= offset && offset <= *end)
        .min_by_key(|((start, end), _)| end - start)
        .map(|(_, ty)| ty.clone())
}

struct Backend {
    client: Client,
    /// Per-URI: (source text, expr_type map)
    cache: Mutex<HashMap<Url, (String, HashMap<(usize, usize), String>)>>,
}

impl Backend {
    fn check_diagnostics(&self, uri: &Url, text: &str) {
        let tokens = match lexer::lex(text) {
            Ok(t) => t,
            Err(errors) => {
                let diags: Vec<Diagnostic> = errors
                    .iter()
                    .map(|e| Diagnostic {
                        range: span_to_range(text, &e.span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("{:?}", e.kind),
                        ..Default::default()
                    })
                    .collect();
                self.cache
                    .lock()
                    .insert(uri.clone(), (text.to_string(), HashMap::new()));
                let _ = self.client.publish_diagnostics(uri.clone(), diags, None);
                return;
            }
        };

        let mut parser = Parser::new(tokens);
        let (program, parse_errors) = parser.parse_program();

        let mut diagnostics: Vec<Diagnostic> = parse_errors
            .iter()
            .map(|e| Diagnostic {
                range: span_to_range(text, e.span()),
                severity: Some(DiagnosticSeverity::ERROR),
                message: match e {
                    agc::parser::ParseError::InvalidSyntax { message, .. } => message.clone(),
                    _ => format!("{:?}", e),
                },
                ..Default::default()
            })
            .collect();

        // Type-check and capture expression types for hover.
        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (type_errors, _monomorphs) = tc.check_program_with_table(&program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);

        for err in &type_errors {
            diagnostics.push(Diagnostic {
                range: span_to_range(text, &err.span),
                severity: Some(DiagnosticSeverity::ERROR),
                message: err.message.clone(),
                ..Default::default()
            });
        }

        self.cache
            .lock()
            .insert(uri.clone(), (text.to_string(), expr_types));
        let _ = self.client.publish_diagnostics(uri.clone(), diagnostics, None);
    }
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
        self.check_diagnostics(&uri, &text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.check_diagnostics(&uri, &change.text);
        }
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let cache = self.cache.lock();
        let Some((text, expr_types)) = cache.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_byte(text, pos);
        let Some(ty) = find_expr_type(offset, expr_types) else {
            return Ok(None);
        };

        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!("type: {ty}"))),
            range: None,
        }))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
