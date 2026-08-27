//! Completion and signature help.
//!
//! Three completion modes:
//! 1. `import std.io.` — module path completion from the std search dirs.
//! 2. `expr.` / `Type::` — member completion (fields, methods, variants)
//!    resolved through the type checker's expression types.
//! 3. Plain identifiers — keywords, locals in scope, and all known symbols.

use agc::lexer::Token;
use tower_lsp_server::ls_types::*;

use crate::doc;
use crate::util::*;
use agc::symbol_index::{Symbol, SymbolIndex, SymbolKind, type_root_name_of_str};

const KEYWORDS: &[&str] = &[
    "struct", "enum", "impl", "trait", "mut", "const", "static", "if", "else", "while", "for",
    "match", "break", "continue", "return", "defer", "import", "comptime", "cast", "move",
    "extern", "asm", "in", "macro", "true", "false",
];

pub(crate) fn completion(analysis: &SymbolIndex, offset: usize) -> Vec<CompletionItem> {
    if let Some(items) = import_completion(analysis, offset) {
        return items;
    }
    if let Some(items) = member_completion(analysis, offset) {
        return items;
    }
    identifier_completion(analysis, offset)
}

// ----- import path completion -----

fn import_completion(analysis: &SymbolIndex, offset: usize) -> Option<Vec<CompletionItem>> {
    let tokens = &analysis.tokens;
    let mut import_idx: Option<usize> = None;
    for (i, t) in tokens.iter().enumerate() {
        if t.span.end > offset {
            break;
        }
        match t.kind {
            Token::Import => import_idx = Some(i),
            Token::Semicolon => import_idx = None,
            _ => {}
        }
    }
    let import_idx = import_idx?;

    // The text between `import` and the cursor must be a dotted path: only
    // identifiers and dots are allowed.
    let mut segments: Vec<String> = Vec::new();
    let mut prefix = String::new();
    let mut after_dot = false;
    for t in &tokens[import_idx + 1..] {
        if t.span.start >= offset {
            break;
        }
        match &t.kind {
            Token::Identifier(name) => {
                if after_dot {
                    segments.push(prefix.clone());
                    prefix.clear();
                    after_dot = false;
                } else if !prefix.is_empty() {
                    return None; // two identifiers without a dot: not a path
                }
                prefix = name.clone();
            }
            Token::Dot => after_dot = true,
            _ => return None,
        }
    }
    if !after_dot && !prefix.is_empty() {
        // `import std.` (trailing dot) keeps prefix empty; `import std` has a
        // single partial/full segment with no separator yet.
        if segments.is_empty() {
            // First segment: module root.
            return Some(root_module_completion(analysis, &prefix));
        }
        // Completed first segment(s) but no trailing dot: user may still be
        // typing the last segment.
        return Some(module_dir_completion(analysis, &segments, &prefix));
    }
    if after_dot {
        // Trailing dot: complete within the directory of the segments.
        return Some(module_dir_completion(analysis, &segments, ""));
    }
    None
}

fn root_module_completion(analysis: &SymbolIndex, prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for dir in find_std_search_dirs() {
        collect_module_entries(&dir, prefix, &mut items);
    }
    // Also offer the module roots of imports already used in this file.
    for path in &analysis.import_paths {
        if let Some(root) = path.first()
            && root.starts_with(prefix)
        {
            items.push(module_item(root, ""));
        }
    }
    sort_dedupe(items)
}

fn module_dir_completion(
    analysis: &SymbolIndex,
    segments: &[String],
    prefix: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for dir in find_std_search_dirs() {
        let mut path = dir;
        for seg in segments {
            path = path.join(seg);
        }
        collect_module_entries(&path, prefix, &mut items);
    }
    let _ = analysis;
    sort_dedupe(items)
}

fn collect_module_entries(dir: &std::path::Path, prefix: &str, items: &mut Vec<CompletionItem>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);
        if !is_dir && !name.ends_with(".ag") {
            continue;
        }
        let stem = if is_dir {
            name.clone()
        } else {
            name.strip_suffix(".ag").unwrap_or(&name).to_string()
        };
        if stem.is_empty() || !stem.starts_with(prefix) {
            continue;
        }
        items.push(module_item(&stem, ""));
    }
}

fn module_item(stem: &str, detail: &str) -> CompletionItem {
    CompletionItem {
        label: stem.to_string(),
        kind: Some(CompletionItemKind::MODULE),
        detail: if detail.is_empty() {
            Some("module".to_string())
        } else {
            Some(detail.to_string())
        },
        insert_text: Some(stem.to_string()),
        ..Default::default()
    }
}

// ----- member completion (`expr.` / `Type::`) -----

fn member_completion(analysis: &SymbolIndex, offset: usize) -> Option<Vec<CompletionItem>> {
    let tokens = &analysis.tokens;
    let trigger_idx = tokens
        .iter()
        .rposition(|t| t.span.end <= offset && matches!(t.kind, Token::Dot | Token::DoubleColon))?;
    // Nothing between the trigger and the cursor.
    for t in &tokens[trigger_idx + 1..] {
        if t.span.start < offset {
            return None;
        }
    }
    let prev = tokens.get(trigger_idx.wrapping_sub(1))?;
    if !matches!(&prev.kind, Token::Identifier(_)) {
        return None;
    }
    let recv_span = (prev.span.start, prev.span.end);
    let container = analysis
        .expr_types
        .get(&recv_span)
        .and_then(|ty| type_root_name_of_str(ty));
    let qualifier = container.map(|c| format!("{c}::"));
    let qualifier = qualifier.as_deref().unwrap_or("");
    let mut items: Vec<CompletionItem> = Vec::new();
    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() != Some(qualifier) {
            continue;
        }
        let (kind, item_kind) = match sym.kind {
            SymbolKind::Field => (true, CompletionItemKind::FIELD),
            SymbolKind::Variant => (true, CompletionItemKind::ENUM_MEMBER),
            SymbolKind::Method => (true, CompletionItemKind::METHOD),
            _ => (false, CompletionItemKind::TEXT),
        };
        if !kind {
            continue;
        }
        items.push(CompletionItem {
            label: sym.name.clone(),
            kind: Some(item_kind),
            detail: Some(sym.signature.clone()),
            sort_text: Some(if sym.kind == SymbolKind::Field {
                "0".to_string()
            } else {
                "1".to_string()
            }),
            ..Default::default()
        });
    }
    Some(sort_dedupe(items))
}

// ----- identifier completion -----

fn identifier_completion(analysis: &SymbolIndex, offset: usize) -> Vec<CompletionItem> {
    let text = &analysis.text;
    let mut start = offset;
    let bytes = text.as_bytes();
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            start -= 1;
        } else {
            break;
        }
    }
    let prefix = &text[start..offset];

    let mut items: Vec<CompletionItem> = Vec::new();
    let mut seen: Vec<(String, Option<String>, Option<CompletionItemKind>)> = Vec::new();

    for sym in &analysis.symbols {
        if !sym.name.starts_with(prefix) {
            continue;
        }
        // Locals/parameters only before their declaration point.
        match sym.kind {
            SymbolKind::Local | SymbolKind::Parameter | SymbolKind::TypeParam
                if sym.span.end > offset =>
            {
                continue;
            }
            _ => {}
        }
        let item = symbol_item(sym);
        if seen.iter().any(|(name, detail, kind)| {
            name == &item.label && detail == &item.detail && kind == &item.kind
        }) {
            continue;
        }
        seen.push((item.label.clone(), item.detail.clone(), item.kind));
        items.push(item);
    }

    for keyword in KEYWORDS {
        if keyword.starts_with(prefix) {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                sort_text: Some("z".to_string()),
                ..Default::default()
            });
        }
    }

    sort_dedupe(items)
}

fn symbol_item(sym: &Symbol) -> CompletionItem {
    let kind = match sym.kind {
        SymbolKind::Function | SymbolKind::ExternFunction | SymbolKind::Macro => {
            CompletionItemKind::FUNCTION
        }
        SymbolKind::Method => CompletionItemKind::METHOD,
        SymbolKind::Struct => CompletionItemKind::STRUCT,
        SymbolKind::Enum => CompletionItemKind::ENUM,
        SymbolKind::Trait => CompletionItemKind::INTERFACE,
        SymbolKind::TypeAlias => CompletionItemKind::CLASS,
        SymbolKind::Global | SymbolKind::Local | SymbolKind::Parameter => {
            CompletionItemKind::VARIABLE
        }
        SymbolKind::Const | SymbolKind::ExternVariable => CompletionItemKind::CONSTANT,
        SymbolKind::Field => CompletionItemKind::FIELD,
        SymbolKind::Variant => CompletionItemKind::ENUM_MEMBER,
        SymbolKind::TypeParam => CompletionItemKind::TYPE_PARAMETER,
    };
    let sort = match sym.kind {
        SymbolKind::Local | SymbolKind::Parameter | SymbolKind::TypeParam => "0",
        SymbolKind::Function | SymbolKind::Method => "1",
        _ => "2",
    };
    CompletionItem {
        label: sym.name.clone(),
        kind: Some(kind),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc::doc_to_markdown(d),
            })
        }),
        sort_text: Some(sort.to_string()),
        ..Default::default()
    }
}

fn sort_dedupe(mut items: Vec<CompletionItem>) -> Vec<CompletionItem> {
    items.sort_by(|a, b| {
        let ka = a.sort_text.as_deref().unwrap_or("2");
        let kb = b.sort_text.as_deref().unwrap_or("2");
        ka.cmp(kb).then_with(|| a.label.cmp(&b.label))
    });
    let mut out: Vec<CompletionItem> = Vec::new();
    let mut seen: Vec<(String, Option<String>, Option<CompletionItemKind>)> = Vec::new();
    for item in items {
        if seen.iter().any(|(label, detail, kind)| {
            label == &item.label && detail == &item.detail && kind == &item.kind
        }) {
            continue;
        }
        seen.push((item.label.clone(), item.detail.clone(), item.kind));
        out.push(item);
    }
    out
}

// ----- signature help -----

pub(crate) fn signature_help(analysis: &SymbolIndex, offset: usize) -> Option<SignatureHelp> {
    let tokens = &analysis.tokens;
    // Last `(` before the cursor.
    let mut paren_idx: Option<usize> = None;
    for (i, t) in tokens.iter().enumerate() {
        if t.span.start >= offset {
            break;
        }
        if matches!(t.kind, Token::LeftParen) {
            paren_idx = Some(i);
        }
    }
    let mut idx = paren_idx?;

    // Walk back from the `(` to the call target identifier, skipping nested
    // argument lists like `f(g(x), ...`.
    let target = loop {
        let prev = tokens.get(idx.wrapping_sub(1))?;
        match &prev.kind {
            Token::DoubleColon => {
                idx -= 1;
            }
            Token::Identifier(_) => break prev,
            Token::RightParen => {
                let mut depth = 0;
                let mut j = idx;
                loop {
                    if j == 0 {
                        return None;
                    }
                    j -= 1;
                    match &tokens[j].kind {
                        Token::RightParen => depth += 1,
                        Token::LeftParen => {
                            depth -= 1;
                            if depth == 0 {
                                idx = j;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => return None,
        }
    };

    let occ = find_occurrence(target.span.start, &analysis.occurrences)?;
    let sym = analysis.symbols.get(occ.symbol?)?;
    if sym.parameters.is_empty() {
        return None;
    }
    let signature = SignatureInformation {
        label: sym.signature.clone(),
        documentation: sym.doc.as_deref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc::doc_to_markdown(d),
            })
        }),
        parameters: Some(
            sym.parameters
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(format!("{}: {}", p.name, p.type_str)),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    };

    // Active parameter: number of top-level commas between `(` and cursor.
    let open_end = tokens[idx].span.end;
    let mut commas: u32 = 0;
    let mut depth = 0;
    for t in tokens {
        if t.span.end <= open_end {
            continue;
        }
        if t.span.start >= offset {
            break;
        }
        match &t.kind {
            Token::LeftParen => depth += 1,
            Token::RightParen => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            Token::Comma if depth == 0 => commas += 1,
            _ => {}
        }
    }
    let receiver_offset = if sym.kind == SymbolKind::Method && !sym.is_static {
        1
    } else {
        0
    };
    let active = commas
        .saturating_add(receiver_offset)
        .min((sym.parameters.len() as u32).saturating_sub(1))
        .saturating_sub(receiver_offset);
    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: Some(active),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use agc::lexer;
    use agc::module_loader::ModuleLoader;
    use agc::parser::{FileImportResolverHook, Parser};
    use agc::symbol_index::analyze;

    /// A selectively imported alias (`println as pln`) must appear in
    /// identifier completions: import lowering materializes the alias as a
    /// real function symbol before the index is built.
    #[test]
    fn aliased_selective_import_appears_in_completions() {
        let source = "import std.io.file { println as pln };\nfn main() {\n    pl;\n}";
        let path = std::path::Path::new("/tmp/lsp_sel_import_test.ag");
        let file_id = lexer::register_source(&path.display().to_string(), source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let mut parser = Parser::new(tokens.clone());
        let (mut program, parse_errors) = parser.parse_program();
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");

        // Mirror the LSP diagnostics pipeline: run import lowering so the
        // alias clone is materialized into the program.
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(repo_root);
        let hook = FileImportResolverHook::new(&loader);
        let lowered = hook.lower_program_imports(&mut program, path.parent(), Some(path));
        assert!(lowered.is_ok(), "lowering failed: {:?}", lowered.err());

        let expr_types = Default::default();
        let analysis = analyze(&program, source, &tokens, expr_types, file_id);

        // Complete at the end of "pl" (before the semicolon).
        let offset = source.rfind("pl").unwrap() + 2;
        let items = completion(&analysis, offset);
        assert!(
            items.iter().any(|item| item.label == "pln"),
            "alias 'pln' missing from completions: {:?}",
            items.iter().map(|i| i.label.clone()).collect::<Vec<_>>()
        );
    }
}
