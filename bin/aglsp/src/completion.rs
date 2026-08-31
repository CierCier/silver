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

const PRIMITIVE_TYPES: &[&str] = &[
    "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "f80",
    "c32", "c64", "c80", "bool", "str", "char", "void",
];

const MACRO_BUILTINS: &[(&str, &str, &str, &str)] = &[
    (
        "@println",
        "@println(\"{}\", val);",
        "println(\"${1:{}\", ${2:val});$0",
        "Prints formatted text followed by a newline to stdout.",
    ),
    (
        "@print",
        "@print(\"{}\", val);",
        "print(\"${1:{}\", ${2:val});$0",
        "Prints formatted text to stdout without a trailing newline.",
    ),
    (
        "@format",
        "@format(\"{}\", val)",
        "format(\"${1:{}\", ${2:val})",
        "Formats arguments into an owned String on the heap.",
    ),
    (
        "@assert",
        "@assert(condition, \"message\");",
        "assert(${1:condition});$0",
        "Asserts a condition in debug builds; aborted with backtrace on failure.",
    ),
    (
        "@size",
        "@size(Type)",
        "size(${1:Type})",
        "Returns the byte size of a type or struct layout.",
    ),
    (
        "@align",
        "@align(Type)",
        "align(${1:Type})",
        "Returns the memory alignment in bytes of a type or struct.",
    ),
    (
        "@json",
        "@json(value)",
        "json(${1:value})",
        "Serializes a struct to a JSON String using synthesized or explicit ToJson.",
    ),
    (
        "@from_json",
        "@from_json<Type>(json_str)",
        "from_json<${1:Type}>(${2:json_str})",
        "Deserializes a JSON string into Result<Type, JsonError>.",
    ),
    (
        "@cfg",
        "@cfg(key)",
        "cfg(${1:debug})",
        "Checks compile-time cfg flag condition.",
    ),
    (
        "@hash",
        "@hash(value)",
        "hash(${1:value})",
        "Computes the 64-bit hash of a value.",
    ),
];

pub(crate) fn completion(analysis: &SymbolIndex, offset: usize) -> Vec<CompletionItem> {
    if let Some(items) = import_completion(analysis, offset) {
        return items;
    }
    if let Some(items) = macro_builtin_completion(&analysis.text, offset) {
        return items;
    }
    if let Some(items) = member_completion(analysis, offset) {
        return items;
    }
    identifier_completion(analysis, offset)
}

// ----- compiler macro / builtin completion (`@`) -----

fn macro_builtin_completion(text: &str, offset: usize) -> Option<Vec<CompletionItem>> {
    let mut start = offset;
    let bytes = text.as_bytes();
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_alphanumeric() || c == '_' || c == '@' {
            start -= 1;
            if c == '@' {
                break;
            }
        } else {
            break;
        }
    }
    if start >= offset {
        return None;
    }
    let prefix = &text[start..offset];
    if !prefix.starts_with('@') {
        return None;
    }

    let mut items = Vec::new();
    for (name, detail, snippet, doc_str) in MACRO_BUILTINS {
        if name.starts_with(prefix) {
            let insert_snippet = if prefix.starts_with('@') {
                snippet.to_string()
            } else {
                format!("@{}", snippet)
            };
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail.to_string()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc_str.to_string(),
                })),
                insert_text: Some(insert_snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("0_{}", name)),
                ..Default::default()
            });
        }
    }
    Some(items)
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

// ----- member completion (`expr.` / `Type::` / `Type.`) -----

fn member_completion(analysis: &SymbolIndex, offset: usize) -> Option<Vec<CompletionItem>> {
    let text = &analysis.text;
    if offset == 0 || offset > text.len() {
        return None;
    }

    // 1. Find the member prefix currently being typed before cursor (alphanumeric + '_')
    let mut member_start = offset;
    let bytes = text.as_bytes();
    while member_start > 0 {
        let c = bytes[member_start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            member_start -= 1;
        } else {
            break;
        }
    }
    let member_prefix = &text[member_start..offset];

    // 2. Check if immediately preceded by `.` or `::`
    let (_is_dot, _is_double_colon, trigger_len) = if member_start >= 2 && &text[member_start - 2..member_start] == "::" {
        (false, true, 2)
    } else if member_start >= 1 && &text[member_start - 1..member_start] == "." {
        (true, false, 1)
    } else {
        return None;
    };

    let before_trigger = member_start - trigger_len;
    // Extract receiver text by walking backwards (skipping whitespace)
    let mut recv_end = before_trigger;
    while recv_end > 0 && (bytes[recv_end - 1] as char).is_whitespace() {
        recv_end -= 1;
    }
    if recv_end == 0 {
        return struct_initializer_completion(analysis, offset, member_prefix);
    }

    // Find receiver start (identifier or closing bracket/paren)
    let mut recv_start = recv_end;
    if bytes[recv_end - 1] == b')' || bytes[recv_end - 1] == b']' {
        let close = bytes[recv_end - 1];
        let open = if close == b')' { b'(' } else { b'[' };
        let mut depth = 1;
        recv_start -= 1;
        while recv_start > 0 && depth > 0 {
            recv_start -= 1;
            if bytes[recv_start] == close {
                depth += 1;
            } else if bytes[recv_start] == open {
                depth -= 1;
            }
        }
        while recv_start > 0 {
            let c = bytes[recv_start - 1] as char;
            if c.is_alphanumeric() || c == '_' {
                recv_start -= 1;
            } else {
                break;
            }
        }
    } else {
        while recv_start > 0 {
            let c = bytes[recv_start - 1] as char;
            if c.is_alphanumeric() || c == '_' {
                recv_start -= 1;
            } else {
                break;
            }
        }
    }

    let recv_text = text[recv_start..recv_end].trim();
    if recv_text.is_empty() {
        return struct_initializer_completion(analysis, offset, member_prefix);
    }

    // 3. Resolve container name:
    // First try expr_types at (recv_start, recv_end) or surrounding spans
    let mut container: Option<String> = analysis
        .expr_types
        .get(&(recv_start, recv_end))
        .and_then(|ty| type_root_name_of_str(ty));

    // If not found in expr_types, check if recv_text is a known struct/enum name directly
    if container.is_none() {
        if analysis.symbols.iter().any(|s| {
            s.name == recv_text
                && matches!(
                    s.kind,
                    SymbolKind::Struct | SymbolKind::Enum | SymbolKind::TypeAlias
                )
        }) {
            container = Some(recv_text.to_string());
        }
    }

    // If still not found, check if recv_text matches a local variable or parameter in scope
    if container.is_none() {
        for s in &analysis.symbols {
            if s.name == recv_text && matches!(s.kind, SymbolKind::Local | SymbolKind::Parameter) {
                if let Some(ty) = s.inferred_type.as_deref().or_else(|| {
                    let parts: Vec<&str> = s.signature.split_whitespace().collect();
                    if parts.len() >= 2 {
                        Some(parts[0])
                    } else {
                        None
                    }
                }) {
                    if let Some(root) = type_root_name_of_str(ty) {
                        container = Some(root);
                        break;
                    }
                }
            }
        }
    }

    let Some(container_name) = container else {
        return None;
    };

    let qualifier = format!("{container_name}::");
    let mut items: Vec<CompletionItem> = Vec::new();
    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() != Some(qualifier.as_str()) {
            continue;
        }
        if !member_prefix.is_empty() && !sym.name.starts_with(member_prefix) {
            continue;
        }
        let (item_kind, sort_prefix) = match sym.kind {
            SymbolKind::Field => (CompletionItemKind::FIELD, "0"),
            SymbolKind::Variant => (CompletionItemKind::ENUM_MEMBER, "0"),
            SymbolKind::Method if !sym.is_static => (CompletionItemKind::METHOD, "1"),
            SymbolKind::Method => (CompletionItemKind::METHOD, "2"),
            _ => continue,
        };

        let is_callable = sym.kind == SymbolKind::Method;
        let insert_text = if is_callable {
            let has_params = if sym.is_static {
                !sym.parameters.is_empty()
            } else {
                sym.parameters.len() > 1
            };
            if has_params {
                Some(format!("{}($1)$0", sym.name))
            } else {
                Some(format!("{}()$0", sym.name))
            }
        } else {
            Some(sym.name.clone())
        };

        items.push(CompletionItem {
            label: sym.name.clone(),
            kind: Some(item_kind),
            detail: Some(sym.signature.clone()),
            documentation: sym.doc.as_deref().map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc::doc_to_markdown(d),
                })
            }),
            insert_text,
            insert_text_format: if is_callable {
                Some(InsertTextFormat::SNIPPET)
            } else {
                Some(InsertTextFormat::PLAIN_TEXT)
            },
            sort_text: Some(format!("{sort_prefix}_{}", sym.name)),
            ..Default::default()
        });
    }

    Some(sort_dedupe(items))
}

// ----- struct literal field completion (`{ .` / `.field`) -----

fn struct_initializer_completion(
    analysis: &SymbolIndex,
    offset: usize,
    field_prefix: &str,
) -> Option<Vec<CompletionItem>> {
    let text = &analysis.text;
    let bytes = text.as_bytes();
    let mut i = offset;
    let mut found_brace = false;
    let mut depth = 0;
    while i > 0 {
        i -= 1;
        if bytes[i] == b'}' {
            depth += 1;
        } else if bytes[i] == b'{' {
            if depth == 0 {
                found_brace = true;
                break;
            }
            depth -= 1;
        }
    }
    if !found_brace {
        return None;
    }

    let mut before_brace = i;
    while before_brace > 0 && (bytes[before_brace - 1] as char).is_whitespace() {
        before_brace -= 1;
    }
    if before_brace > 0 && bytes[before_brace - 1] == b'=' {
        before_brace -= 1;
        while before_brace > 0 && (bytes[before_brace - 1] as char).is_whitespace() {
            before_brace -= 1;
        }
    }

    let name_end = before_brace;
    let mut name_start = name_end;
    while name_start > 0 {
        let c = bytes[name_start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            name_start -= 1;
        } else {
            break;
        }
    }
    let target_name = text[name_start..name_end].trim();
    if target_name.is_empty() {
        return None;
    }

    let mut struct_name = if analysis
        .symbols
        .iter()
        .any(|s| s.name == target_name && s.kind == SymbolKind::Struct)
    {
        Some(target_name.to_string())
    } else {
        None
    };

    if struct_name.is_none() {
        for s in &analysis.symbols {
            if s.name == target_name && matches!(s.kind, SymbolKind::Local | SymbolKind::Global) {
                if let Some(ty) = s.inferred_type.as_deref().or_else(|| {
                    let parts: Vec<&str> = s.signature.split_whitespace().collect();
                    if parts.len() >= 2 {
                        Some(parts[0])
                    } else {
                        None
                    }
                }) {
                    if let Some(root) = type_root_name_of_str(ty) {
                        struct_name = Some(root);
                        break;
                    }
                }
            }
        }
    }

    if struct_name.is_none() {
        let mut type_end = name_start;
        while type_end > 0 && (bytes[type_end - 1] as char).is_whitespace() {
            type_end -= 1;
        }
        let mut type_start = type_end;
        while type_start > 0 {
            let c = bytes[type_start - 1] as char;
            if c.is_alphanumeric() || c == '_' {
                type_start -= 1;
            } else {
                break;
            }
        }
        let declared_type = text[type_start..type_end].trim();
        if !declared_type.is_empty()
            && analysis
                .symbols
                .iter()
                .any(|s| s.name == declared_type && s.kind == SymbolKind::Struct)
        {
            struct_name = Some(declared_type.to_string());
        }
    }

    let struct_name = struct_name?;
    let qualifier = format!("{struct_name}::");
    let mut items = Vec::new();
    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() == Some(qualifier.as_str()) && sym.kind == SymbolKind::Field {
            if !field_prefix.is_empty() && !sym.name.starts_with(field_prefix) {
                continue;
            }
            items.push(CompletionItem {
                label: format!(".{}", sym.name),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(sym.signature.clone()),
                insert_text: Some(format!("{} = $1", sym.name)),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("0_{}", sym.name)),
                ..Default::default()
            });
        }
    }
    if items.is_empty() {
        None
    } else {
        Some(sort_dedupe(items))
    }
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

    for pt in PRIMITIVE_TYPES {
        if pt.starts_with(prefix) {
            items.push(CompletionItem {
                label: pt.to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("primitive type".to_string()),
                sort_text: Some(format!("3_{pt}")),
                ..Default::default()
            });
        }
    }

    for keyword in KEYWORDS {
        if keyword.starts_with(prefix) {
            items.push(CompletionItem {
                label: keyword.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                sort_text: Some(format!("4_{keyword}")),
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

    let is_callable = matches!(
        sym.kind,
        SymbolKind::Function | SymbolKind::ExternFunction | SymbolKind::Method
    );
    let insert_text = if is_callable {
        let has_params = if sym.kind == SymbolKind::Method && !sym.is_static {
            sym.parameters.len() > 1
        } else {
            !sym.parameters.is_empty()
        };
        if has_params {
            Some(format!("{}($1)$0", sym.name))
        } else {
            Some(format!("{}()$0", sym.name))
        }
    } else {
        Some(sym.name.clone())
    };

    let sort = match sym.kind {
        SymbolKind::Local | SymbolKind::Parameter | SymbolKind::TypeParam => "0",
        SymbolKind::Function | SymbolKind::Method => "1",
        SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait => "2",
        _ => "3",
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
        insert_text,
        insert_text_format: if is_callable {
            Some(InsertTextFormat::SNIPPET)
        } else {
            Some(InsertTextFormat::PLAIN_TEXT)
        },
        sort_text: Some(format!("{sort}_{}", sym.name)),
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
    use agc::parser::FileImportResolverHook;
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
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        assert!(!graph.has_errors(), "parse errors: {:?}", graph.errors());

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

    #[test]
    fn macro_builtins_completion_at_at_sign() {
        let source = "i32 main() {\n    @pr;\n}";
        let path = std::path::Path::new("/tmp/lsp_macro_test.ag");
        let file_id = lexer::register_source(&path.display().to_string(), source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind("@pr").unwrap() + 3;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"@println".to_string()), "got {labels:?}");
        assert!(labels.contains(&"@print".to_string()), "got {labels:?}");
    }

    #[test]
    fn member_completion_partial_and_static() {
        let source = r#"
        struct User {
            i32 id;
            str name;
        }
        impl User {
            User make(str name) {
                User u = { .id = 1, .name = name };
                return u;
            }
            i32 get_id(User* self) {
                return self.id;
            }
        }
        i32 main() {
            User u = User.make("Alice");
            u.na;
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_member_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let mut tc = agc::semantic::typeck::TypeChecker::new();
        let mut table = agc::symbol_table::CompilerSymbolTable::new();
        let (_, _) = tc.check_program_with_table(&mut program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);
        let analysis = analyze(&program, source, &tokens, expr_types, file_id);

        // 1. Complete on `u.na`
        let offset = source.rfind("u.na").unwrap() + 4;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"name".to_string()), "expected 'name' in {labels:?}");

        // 2. Complete on `User.`
        let static_offset = source.rfind("User.").unwrap() + 5;
        let static_items = completion(&analysis, static_offset);
        let static_labels: Vec<String> = static_items.iter().map(|i| i.label.clone()).collect();
        assert!(
            static_labels.contains(&"make".to_string()),
            "expected 'make' in {static_labels:?}"
        );
    }

    #[test]
    fn struct_literal_designated_initializer_completion() {
        let source = r#"
        struct User {
            i32 id;
            str name;
        }
        i32 main() {
            User u = { .id = 1, .name = "Alice" };
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_struct_init_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind(".name").unwrap() + 1;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(
            labels.contains(&".id".to_string()) && labels.contains(&".name".to_string()),
            "expected struct fields in {labels:?}"
        );
    }

    #[test]
    fn member_completion_on_self_and_explicit_local() {
        let source = r#"
        struct Point {
            i32 x;
            i32 y;
        }
        impl Point {
            i32 magnitude(Point* self) {
                self.x;
                return 0;
            }
        }
        i32 main() {
            Point pt;
            pt.y;
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_self_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        // 1. Complete on `self.x`
        let self_offset = source.rfind("self.x").unwrap() + 5;
        let self_items = completion(&analysis, self_offset);
        let self_labels: Vec<String> = self_items.iter().map(|i| i.label.clone()).collect();
        assert!(self_labels.contains(&"x".to_string()), "got {self_labels:?}");
        assert!(self_labels.contains(&"y".to_string()), "got {self_labels:?}");

        // 2. Complete on `pt.y`
        let pt_offset = source.rfind("pt.y").unwrap() + 3;
        let pt_items = completion(&analysis, pt_offset);
        let pt_labels: Vec<String> = pt_items.iter().map(|i| i.label.clone()).collect();
        assert!(pt_labels.contains(&"x".to_string()), "got {pt_labels:?}");
        assert!(pt_labels.contains(&"y".to_string()), "got {pt_labels:?}");
    }
}
