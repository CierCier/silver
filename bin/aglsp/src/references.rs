//! Find-references and rename support.

use agc::lexer::Span;
use rustc_hash::FxHashMap as HashMap;
use tower_lsp_server::ls_types::*;

use crate::util::*;
use agc::symbol_index::{Occurrence, SymbolIndex, SymbolKind};

#[derive(Clone, Copy, PartialEq, Eq)]
struct SymbolIdentity {
    file: u32,
    start: usize,
    end: usize,
    kind: SymbolKind,
}

impl SymbolIdentity {
    fn from_symbol(symbol: &agc::symbol_index::Symbol) -> Self {
        Self {
            file: symbol.span.file,
            start: symbol.span.start,
            end: symbol.span.end,
            kind: symbol.kind,
        }
    }
}

/// All occurrences of the symbol under `offset`: every identifier use that
/// resolved to the same symbol, plus (optionally) the definition.
pub(crate) fn symbol_occurrences(
    analysis: &SymbolIndex,
    offset: usize,
    include_declaration: bool,
) -> Vec<&Occurrence> {
    let Some(cursor) = find_occurrence(offset, &analysis.occurrences) else {
        return Vec::new();
    };
    let Some(symbol_id) = cursor.symbol else {
        return Vec::new();
    };

    analysis
        .occurrences
        .iter()
        .filter(|occ| occ.symbol == Some(symbol_id) && (include_declaration || !occ.is_definition))
        .collect()
}
/// Find the selected symbol's occurrences in every cached open buffer.
///
/// Symbol IDs are local to each index, so buffers are matched by the
/// source-backed identity of their resolved symbols instead.
pub(crate) fn symbol_occurrences_across_buffers<'a>(
    analyses: &'a HashMap<Uri, SymbolIndex>,
    buffer_uri: &Uri,
    offset: usize,
    include_declaration: bool,
) -> Vec<(&'a Uri, &'a SymbolIndex, &'a Occurrence)> {
    let Some(active) = analyses.get(buffer_uri) else {
        return Vec::new();
    };
    let Some(target) = symbol_under_cursor(active, offset) else {
        return Vec::new();
    };
    let target_identity = SymbolIdentity::from_symbol(target);

    analyses
        .iter()
        .flat_map(|(uri, analysis)| {
            analysis.occurrences.iter().filter_map(move |occ| {
                let symbol_id = occ.symbol?;
                let symbol = analysis.symbols.get(symbol_id)?;
                let identity = SymbolIdentity::from_symbol(symbol);
                let same_identity =
                    identity == target_identity && (target_identity.file != 0 || uri == buffer_uri);
                (same_identity && (include_declaration || !occ.is_definition))
                    .then_some((uri, analysis, occ))
            })
        })
        .collect()
}

/// The symbol definition under `offset`.
pub(crate) fn symbol_under_cursor(
    analysis: &SymbolIndex,
    offset: usize,
) -> Option<&agc::symbol_index::Symbol> {
    let occ = find_occurrence(offset, &analysis.occurrences)?;
    let id = occ.symbol?;
    analysis.symbols.get(id)
}

/// Convert a span to an LSP location, resolving foreign files (inlined
/// imports) through the analysis snapshot.
pub(crate) fn location_for_span(
    analysis: &SymbolIndex,
    buffer_uri: &Uri,
    span: &Span,
) -> Option<Location> {
    if let Some((path, text)) = analysis.foreign_files.get(&span.file) {
        let uri = Uri::from_file_path(path.as_str())?;
        Some(Location {
            uri,
            range: span_to_range(text, span),
        })
    } else {
        Some(Location {
            uri: buffer_uri.clone(),
            range: span_to_range(&analysis.text, span),
        })
    }
}

/// True when `name` is a valid identifier for a rename target: a non-empty
/// word that is not a keyword or a builtin type name.
pub(crate) fn is_valid_identifier(name: &str) -> bool {
    if name.is_empty() || is_keyword(name) || is_builtin_type(name) {
        return false;
    }
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Build a workspace edit renaming the symbol under `offset` to `new_name`
/// across all cached open buffers and any inlined imported files.
pub(crate) fn rename_edit(
    analyses: &HashMap<Uri, SymbolIndex>,
    buffer_uri: &Uri,
    offset: usize,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let occurrences = symbol_occurrences_across_buffers(analyses, buffer_uri, offset, true);
    if occurrences.is_empty() {
        return None;
    }
    // Group edits by file.
    let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::default();
    for (uri, analysis, occ) in occurrences {
        let (target_uri, text) = match analysis.foreign_files.get(&occ.span.file) {
            Some((path, text)) => (Uri::from_file_path(path.as_str())?, text),
            None => (uri.clone(), &analysis.text),
        };
        changes.entry(target_uri).or_default().push(TextEdit {
            range: span_to_range(text, &occ.span),
            new_text: new_name.to_string(),
        });
    }
    // WorkspaceEdit.changes is a std HashMap (RandomState) — required by lsp-types.
    #[expect(
        clippy::disallowed_types,
        reason = "WorkspaceEdit.changes requires std HashMap"
    )]
    let std_changes: std::collections::HashMap<Uri, Vec<TextEdit>> = changes.into_iter().collect();
    Some(WorkspaceEdit {
        changes: Some(std_changes),
        ..Default::default()
    })
}
