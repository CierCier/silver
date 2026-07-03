// Silver Language Server — diagnostics, hover (type + definition), go-to-definition.

use agc::lexer;
use agc::lexer::Span;
use agc::parser::ast::{self, ItemKind};
use agc::parser::Parser;
use agc::semantic::typeck::TypeChecker;
use agc::symbol_table::CompilerSymbolTable;

use std::collections::HashMap;
use parking_lot::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// Convert byte offset to 0‑based line/col from source text.
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

/// Convert an LSP position (UTF‑16 code units) to a byte offset.
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
    let mut best: Option<((usize, usize), &String)> = None;
    for ((start, end), ty) in map {
        if *start <= offset && offset <= *end {
            match &best {
                Some(((bs, be), _)) if (end - start) < (be - bs) => {
                    best = Some(((*start, *end), ty))
                }
                None => best = Some(((*start, *end), ty)),
                _ => {}
            }
        }
    }
    best.map(|(_, ty)| ty.clone())
}

/// Extract the identifier under `offset` from source text.
/// Returns (start_byte, end_byte, name).
fn extract_identifier(text: &str, offset: usize) -> Option<(usize, usize, String)> {
    if offset >= text.len() {
        return None;
    }
    let c = text[offset..].chars().next()?;
    if !c.is_ascii_alphanumeric() && c != '_' {
        return None;
    }

    // Walk forward to end of identifier.
    let end = text[offset..]
        .char_indices()
        .find(|(_, c)| !c.is_alphanumeric() && *c != '_')
        .map_or(text.len(), |(i, _)| offset + i);

    // Walk backward to start of identifier.
    let prefix = &text[..offset];
    let start = prefix
        .char_indices()
        .rfind(|(_, c)| !c.is_alphanumeric() && *c != '_')
        .map_or(0, |(i, c)| i + c.len_utf8());

    if start < end {
        Some((start, end, text[start..end].to_string()))
    } else {
        None
    }
}

/// Collect top‑level definition names and their spans.
fn collect_definitions(program: &ast::Program) -> HashMap<String, Span> {
    let mut defs = HashMap::new();
    for item in &program.items {
        let (name, span) = match &item.kind {
            ItemKind::Function(f) => (&f.name.name, f.name.span.clone()),
            ItemKind::Struct(s) => (&s.name.name, s.name.span.clone()),
            ItemKind::Enum(e) => (&e.name.name, e.name.span.clone()),
            ItemKind::GlobalVariable(v) => (&v.name.name, v.name.span.clone()),
            ItemKind::Trait(t) => (&t.name.name, t.name.span.clone()),
            ItemKind::TypeAlias(a) => (&a.name.name, a.name.span.clone()),
            ItemKind::ExternFunction(f) => (&f.name.name, f.name.span.clone()),
            ItemKind::ExternVariable(v) => (&v.name.name, v.name.span.clone()),
            ItemKind::Macro(m) => (&m.name.name, m.name.span.clone()),
            _ => continue,
        };
        defs.entry(name.clone()).or_insert(span);
    }
    defs
}

type ExprTypeMap = HashMap<(usize, usize), String>;
type DefMap = HashMap<String, Span>;

struct Backend {
    client: Client,
    /// Per‑URI: (source_text, expr_types, definitions)
    cache: Mutex<HashMap<Url, (String, ExprTypeMap, DefMap)>>,
}

/// Extract the source line containing `offset`, trimmed.
fn extract_line(text: &str, offset: usize) -> String {
    // Walk backward to start of line.
    let line_start = text[..offset]
        .char_indices()
        .rfind(|(_, c)| *c == '\n')
        .map_or(0, |(i, _)| i + 1);
    // Walk forward to end of line.
    let line_end = text[offset..]
        .char_indices()
        .find(|(_, c)| *c == '\n')
        .map_or(text.len(), |(i, _)| offset + i);
    let line = &text[line_start..line_end];
    line.trim().to_string()
}

impl Backend {
    async fn check_diagnostics(&self, uri: &Url, text: &str) {
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
                self.cache.lock().insert(
                    uri.clone(),
                    (text.to_string(), HashMap::new(), HashMap::new()),
                );
                self.client
                    .publish_diagnostics(uri.clone(), diags, None)
                    .await;
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
                    agc::parser::ParseError::InvalidSyntax { message, .. } => {
                        message.clone()
                    }
                    _ => format!("{:?}", e),
                },
                ..Default::default()
            })
            .collect();

        // Collect definitions for go‑to‑definition.
        let defs = collect_definitions(&program);

        // Type‑check and capture expression types for hover.
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
            .insert(uri.clone(), (text.to_string(), expr_types, defs));
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
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
        let Some((text, expr_types, defs)) = cache.get(uri) else {
            return Ok(None);
        };

        let offset = position_to_byte(text, pos);

        // Build hover parts: type info + definition info.
        let mut parts: Vec<MarkedString> = Vec::new();

        // Type from expression.
        if let Some(ty) = find_expr_type(offset, expr_types) {
            parts.push(MarkedString::String(format!("type: {ty}")));
        }

        // Definition source text if cursor is on a known identifier.
        if let Some((_, _, name)) = extract_identifier(text, offset) {
            if let Some(def_span) = defs.get(&name) {
                // Extract the definition line(s) from source.
                let def_text = extract_line(text, def_span.start);
                parts.push(MarkedString::String(def_text));
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
        let Some((text, _expr_types, defs)) = cache.get(uri) else {
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
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
