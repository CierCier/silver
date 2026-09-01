use agc::lexer::Span;
use agc::module_loader::{ModuleLoader, module_loader_default_dirs};
use rustc_hash::FxHashMap as HashMap;
use std::path::PathBuf;
use tower_lsp_server::ls_types::*;

/// Convert a byte offset to a 0-based LSP position.
///
/// LSP columns are UTF-16 code units, not Unicode scalar values.
pub(crate) fn byte_to_position(text: &str, offset: usize) -> Position {
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
            col += ch.len_utf16() as u32;
        }
    }
    Position {
        line,
        character: col,
    }
}

pub(crate) fn span_to_range(text: &str, span: &Span) -> Range {
    Range {
        start: byte_to_position(text, span.start),
        end: byte_to_position(text, span.end),
    }
}

/// Convert an LSP position (UTF-16 code units) to a byte offset.
pub(crate) fn position_to_byte(text: &str, pos: Position) -> usize {
    let mut line: u32 = 0;
    let mut col: u32 = 0;
    for (i, ch) in text.char_indices() {
        if line == pos.line && col >= pos.character {
            return i;
        }
        if ch == '\n' {
            if line == pos.line {
                return i;
            }
            line += 1;
            col = 0;
        } else {
            col += ch.len_utf16() as u32;
        }
    }
    text.len()
}

/// Find the tightest expression span containing `offset`.
pub(crate) fn find_expr_type(
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

/// Find the tightest occurrence span containing `offset`.
pub(crate) fn find_occurrence(
    offset: usize,
    occurrences: &[agc::symbol_index::Occurrence],
) -> Option<&agc::symbol_index::Occurrence> {
    let mut best: Option<&agc::symbol_index::Occurrence> = None;
    for occ in occurrences {
        if occ.span.start <= offset && offset <= occ.span.end {
            match best {
                Some(prev) if occ.span.end - occ.span.start < prev.span.end - prev.span.start => {
                    best = Some(occ)
                }
                None => best = Some(occ),
                _ => {}
            }
        }
    }
    best
}

/// Extract the identifier under `offset` from source text.
/// Returns (start_byte, end_byte, name).
pub(crate) fn extract_identifier(text: &str, offset: usize) -> Option<(usize, usize, String)> {
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

pub(crate) fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "f32"
            | "f64"
            | "f80"
            | "c32"
            | "c64"
            | "c80"
            | "bool"
            | "str"
            | "char"
            | "void"
    )
}

pub(crate) fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "struct"
            | "enum"
            | "impl"
            | "trait"
            | "fn"
            | "mut"
            | "const"
            | "if"
            | "else"
            | "while"
            | "for"
            | "break"
            | "continue"
            | "return"
            | "defer"
            | "import"
            | "comptime"
            | "cast"
            | "move"
            | "extern"
            | "pub"
            | "private"
            | "asm"
            | "in"
            | "macro"
            | "true"
            | "false"
    )
}

/// Find the Silver std library search dirs (bootstrap/include/silver/ etc.).
pub(crate) fn find_std_search_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    let default_dirs = if let Ok(home) = std::env::var("SILVER_SYSROOT")
        && !home.is_empty()
    {
        module_loader_default_dirs(Some(std::path::Path::new(&home)))
    } else {
        module_loader_default_dirs(None)
    };
    for dir in default_dirs {
        if !dirs.contains(&dir) {
            dirs.push(dir);
        }
    }

    if let Ok(exe) = std::env::current_exe() {
        if let Some(parent) = exe.parent() {
            let candidate_root = parent.join("..").join("..");
            if candidate_root.join("std").is_dir() {
                if let Ok(p) = candidate_root.canonicalize() {
                    if !dirs.contains(&p) {
                        dirs.push(p);
                    }
                } else if !dirs.contains(&candidate_root) {
                    dirs.push(candidate_root);
                }
            }
            let candidate = parent
                .join("..")
                .join("bootstrap")
                .join("include")
                .join("silver");
            if candidate.is_dir() && !dirs.contains(&candidate) {
                dirs.push(candidate);
            }
        }
    }

    if let Ok(cwd) = std::env::current_dir() {
        let mut curr: &std::path::Path = cwd.as_path();
        loop {
            if curr.join("std").is_dir() {
                let p = curr.to_path_buf();
                if !dirs.contains(&p) {
                    dirs.push(p);
                }
                break;
            }
            match curr.parent() {
                Some(parent) => curr = parent,
                None => break,
            }
        }
    }

    dirs
}

pub(crate) fn build_lsp_loader() -> ModuleLoader {
    let mut loader = ModuleLoader::new();
    if let Ok(root) = std::env::current_dir() {
        loader.add_search_dir(&root);
        let local_lib = root.join("lib").join("silver");
        if local_lib.is_dir() {
            loader.add_search_dir(local_lib);
        }
    }

    for dir in find_std_search_dirs() {
        loader.add_search_dir(dir);
    }
    loader
}
/// Apply a sequence of LSP content changes to a document buffer.
pub(crate) fn apply_document_changes(
    doc: &mut String,
    changes: impl IntoIterator<Item = TextDocumentContentChangeEvent>,
) {
    for change in changes {
        if let Some(range) = change.range {
            let start = position_to_byte(doc, range.start).min(doc.len());
            let end = position_to_byte(doc, range.end).min(doc.len());
            doc.replace_range(start..end, &change.text);
        } else {
            *doc = change.text;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_count_utf16_units() {
        let text = "a🚀b";
        assert_eq!(
            byte_to_position(text, 1),
            Position {
                line: 0,
                character: 1
            }
        );
        assert_eq!(
            byte_to_position(text, 5),
            Position {
                line: 0,
                character: 3
            }
        );
        assert_eq!(
            byte_to_position(text, text.len()),
            Position {
                line: 0,
                character: 4
            }
        );
    }

    #[test]
    fn positions_round_trip_utf16_offsets() {
        let text = "a🚀b\nc";
        for offset in [0, 1, 5, 7, 8] {
            let position = byte_to_position(text, offset);
            assert_eq!(position_to_byte(text, position), offset);
        }
    }

    #[test]
    fn positions_clamp_to_line_end() {
        let text = "abc\ndef";
        assert_eq!(
            position_to_byte(
                text,
                Position {
                    line: 0,
                    character: 99
                }
            ),
            3
        );
        assert_eq!(
            position_to_byte(
                text,
                Position {
                    line: 9,
                    character: 0
                }
            ),
            text.len()
        );
    }

    #[test]
    fn incremental_change_insertion_and_replacement() {
        let mut doc = "fn main() {\n    return 0;\n}\n".to_string();

        // Insert `i32 ` before `main`
        apply_document_changes(
            &mut doc,
            vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position { line: 0, character: 3 },
                    end: Position { line: 0, character: 3 },
                }),
                range_length: None,
                text: "i32 ".to_string(),
            }],
        );
        assert_eq!(doc, "fn i32 main() {\n    return 0;\n}\n");

        // Replace `return 0;` with `return 42;`
        apply_document_changes(
            &mut doc,
            vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position { line: 1, character: 11 },
                    end: Position { line: 1, character: 12 },
                }),
                range_length: None,
                text: "42".to_string(),
            }],
        );
        assert_eq!(doc, "fn i32 main() {\n    return 42;\n}\n");
    }
}
