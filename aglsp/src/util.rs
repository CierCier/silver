use agc::lexer::Span;
use agc::module_loader::{ModuleLoader, module_loader_default_dirs};
use std::collections::HashMap;
use std::path::PathBuf;
use tower_lsp::lsp_types::*;

pub(crate) type ExprTypeMap = HashMap<(usize, usize), String>;
pub(crate) type DefMap = HashMap<String, Span>;
pub(crate) type HoverTextMap = HashMap<String, String>;

/// Convert byte offset to 0‑based line/col from source text.
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
            col += 1;
        }
    }
    Position { line, character: col }
}

pub(crate) fn span_to_range(text: &str, span: &Span) -> Range {
    Range {
        start: byte_to_position(text, span.start),
        end: byte_to_position(text, span.end),
    }
}

/// Convert an LSP position (UTF‑16 code units) to a byte offset.
pub(crate) fn position_to_byte(text: &str, pos: Position) -> usize {
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
        "i8" | "i16" | "i32" | "i64" | "i128"
            | "u8" | "u16" | "u32" | "u64" | "u128"
            | "f32" | "f64" | "f80"
            | "c32" | "c64" | "c80"
            | "bool" | "str" | "char" | "void"
    )
}

pub(crate) fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "struct" | "enum" | "impl" | "trait" | "fn" | "let" | "mut" | "const"
            | "if" | "else" | "while" | "for" | "break" | "continue"
            | "return" | "defer" | "import" | "comptime" | "cast"
            | "move" | "ref" | "extern" | "pub" | "private" | "asm"
            | "in" | "macro" | "true" | "false"
    )
}

/// Find the Silver std library search dirs (bootstrap/include/silver/ etc.).
pub(crate) fn find_std_search_dirs() -> Vec<PathBuf> {
    if let Ok(home) = std::env::var("SILVER_SYSROOT") {
        if !home.is_empty() {
            let root = PathBuf::from(home);
            let dirs = module_loader_default_dirs(Some(&root));
            if !dirs.is_empty() {
                return dirs;
            }
        }
    }
    if let Ok(exe) = std::env::current_exe() {
        if let Some(parent) = exe.parent() {
            let candidate = parent.join("..").join("bootstrap").join("include").join("silver");
            if candidate.is_dir() {
                return vec![candidate];
            }
        }
    }
    Vec::new()
}

pub(crate) fn build_lsp_loader() -> ModuleLoader {
    let mut loader = ModuleLoader::new();
    for dir in find_std_search_dirs() {
        loader.add_search_dir(dir);
    }
    loader
}
