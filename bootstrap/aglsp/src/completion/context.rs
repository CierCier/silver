//! Syntactic context discriminator for cursor position.

use agc::lexer::Token;
use agc::symbol_index::SymbolIndex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SyntaxContext {
    ImportPath {
        segments: Vec<String>,
        prefix: String,
        after_dot: bool,
    },
    SelectiveImport {
        module_path: Vec<String>,
        prefix: String,
    },
    MemberAccess {
        receiver_text: String,
        member_prefix: String,
        is_double_colon: bool,
        recv_span: (usize, usize),
    },
    StructInit {
        struct_name: String,
        field_prefix: String,
        existing_fields: Vec<String>,
    },
    MatchArms {
        match_expr: String,
        arm_prefix: String,
        existing_arms: Vec<String>,
    },
    TypePosition {
        prefix: String,
    },
    MacroBuiltin {
        prefix: String,
    },
    TopLevel {
        prefix: String,
    },
    StatementOrExpr {
        prefix: String,
        in_loop: bool,
    },
}

pub(crate) fn determine_context(analysis: &SymbolIndex, offset: usize) -> SyntaxContext {
    let text = &analysis.text;
    if offset == 0 || offset > text.len() {
        return SyntaxContext::TopLevel {
            prefix: String::new(),
        };
    }

    let prefix = extract_word_prefix(text, offset);

    // 1. Check for macro `@` trigger
    if let Some(macro_pfx) = extract_macro_prefix(text, offset) {
        return SyntaxContext::MacroBuiltin { prefix: macro_pfx };
    }

    // 2. Check for import statements (dotted path or selective `{}`)
    if let Some(ctx) = check_import_context(analysis, offset) {
        return ctx;
    }

    // 3. Check for member access (`.` or `::`)
    if let Some(ctx) = check_member_access_context(text, offset) {
        return ctx;
    }

    // 4. Check for struct initializer (`{ .field = ... }`)
    if let Some(ctx) = check_struct_init_context(analysis, offset, &prefix) {
        return ctx;
    }

    // 5. Check for match arms (`match expr { ... }`)
    if let Some(ctx) = check_match_arm_context(text, offset, &prefix) {
        return ctx;
    }

    // 6. Check for type positions (`let <Type>`, `fn (x: <Type>)`, `cast<<Type>>`)
    if check_type_position(text, offset, &prefix) {
        return SyntaxContext::TypePosition { prefix };
    }

    // 7. Check scope depth (TopLevel vs Function body vs Loop)
    let (brace_depth, in_loop) = check_scope_depth(analysis, offset);
    if brace_depth == 0 {
        SyntaxContext::TopLevel { prefix }
    } else {
        SyntaxContext::StatementOrExpr { prefix, in_loop }
    }
}

fn extract_word_prefix(text: &str, offset: usize) -> String {
    let bytes = text.as_bytes();
    let mut start = offset;
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            start -= 1;
        } else {
            break;
        }
    }
    text[start..offset].to_string()
}

fn extract_macro_prefix(text: &str, offset: usize) -> Option<String> {
    let bytes = text.as_bytes();
    let mut start = offset;
    while start > 0 {
        let c = bytes[start - 1] as char;
        if c.is_alphanumeric() || c == '_' || c == '@' {
            start -= 1;
            if c == '@' {
                return Some(text[start..offset].to_string());
            }
        } else {
            break;
        }
    }
    None
}

fn check_import_context(analysis: &SymbolIndex, offset: usize) -> Option<SyntaxContext> {
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

    // Scan tokens after `import`
    let mut segments = Vec::new();
    let mut current_ident = String::new();
    let mut after_dot = false;
    let mut in_brace = false;
    let mut selective_prefix = String::new();

    for t in &tokens[import_idx + 1..] {
        if t.span.start >= offset {
            break;
        }
        match &t.kind {
            Token::LeftBrace => {
                if !current_ident.is_empty() {
                    segments.push(current_ident.clone());
                    current_ident.clear();
                }
                in_brace = true;
            }
            Token::RightBrace => {
                in_brace = false;
            }
            Token::Identifier(name) if in_brace => {
                selective_prefix = name.clone();
            }
            Token::Identifier(name) => {
                if after_dot {
                    segments.push(current_ident.clone());
                    current_ident.clear();
                    after_dot = false;
                }
                current_ident = name.clone();
            }
            Token::Dot if !in_brace => {
                after_dot = true;
            }
            _ => {}
        }
    }

    if in_brace {
        return Some(SyntaxContext::SelectiveImport {
            module_path: segments,
            prefix: selective_prefix,
        });
    }

    Some(SyntaxContext::ImportPath {
        segments,
        prefix: current_ident,
        after_dot,
    })
}

fn check_member_access_context(text: &str, offset: usize) -> Option<SyntaxContext> {
    let bytes = text.as_bytes();
    let mut member_start = offset;
    while member_start > 0 {
        let c = bytes[member_start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            member_start -= 1;
        } else {
            break;
        }
    }
    let member_prefix = text[member_start..offset].to_string();

    let (is_double_colon, trigger_len) = if member_start >= 2 && &text[member_start - 2..member_start] == "::" {
        (true, 2)
    } else if member_start >= 1 && &text[member_start - 1..member_start] == "." {
        (false, 1)
    } else {
        return None;
    };

    let before_trigger = member_start - trigger_len;
    let mut recv_end = before_trigger;
    while recv_end > 0 && (bytes[recv_end - 1] as char).is_whitespace() {
        recv_end -= 1;
    }
    if recv_end == 0 {
        return None;
    }

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
        while recv_start > 0 && (bytes[recv_start - 1] as char).is_alphanumeric() {
            recv_start -= 1;
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

    let receiver_text = text[recv_start..recv_end].trim().to_string();
    if receiver_text.is_empty() {
        return None;
    }

    Some(SyntaxContext::MemberAccess {
        receiver_text,
        member_prefix,
        is_double_colon,
        recv_span: (recv_start, recv_end),
    })
}

fn check_struct_init_context(
    analysis: &SymbolIndex,
    offset: usize,
    field_prefix: &str,
) -> Option<SyntaxContext> {
    let text = &analysis.text;
    let bytes = text.as_bytes();
    let mut i = offset;
    let mut depth = 0;
    let mut found_brace = false;
    let mut brace_offset = 0;

    while i > 0 {
        i -= 1;
        if bytes[i] == b'}' {
            depth += 1;
        } else if bytes[i] == b'{' {
            if depth == 0 {
                found_brace = true;
                brace_offset = i;
                break;
            }
            depth -= 1;
        } else if bytes[i] == b';' && depth == 0 {
            break;
        }
    }

    if !found_brace {
        return None;
    }

    // Collect existing fields in the brace `{ .x = 1, .y = 2 }`
    let body = &text[brace_offset + 1..offset];
    let mut existing_fields = Vec::new();
    for part in body.split(',') {
        let trimmed = part.trim();
        if trimmed.starts_with('.') {
            let name = trimmed[1..]
                .split(|c: char| !c.is_alphanumeric() && c != '_')
                .next()
                .unwrap_or("");
            if !name.is_empty() {
                existing_fields.push(name.to_string());
            }
        }
    }

    // Resolve struct name before `{`
    let mut before_brace = brace_offset;
    while before_brace > 0 && (bytes[before_brace - 1] as char).is_whitespace() {
        before_brace -= 1;
    }
    if before_brace > 0 && bytes[before_brace - 1] == b'=' {
        before_brace -= 1;
        while before_brace > 0 && (bytes[before_brace - 1] as char).is_whitespace() {
            before_brace -= 1;
        }
    }

    let mut name_start = before_brace;
    while name_start > 0 {
        let c = bytes[name_start - 1] as char;
        if c.is_alphanumeric() || c == '_' {
            name_start -= 1;
        } else {
            break;
        }
    }
    let target = text[name_start..before_brace].trim();
    if target.is_empty() {
        return None;
    }

    let struct_name = resolve_struct_name(analysis, target, name_start)?;
    Some(SyntaxContext::StructInit {
        struct_name,
        field_prefix: field_prefix.to_string(),
        existing_fields,
    })
}

fn resolve_struct_name(analysis: &SymbolIndex, target: &str, start: usize) -> Option<String> {
    use agc::symbol_index::{SymbolKind, type_root_name_of_str};

    for s in &analysis.symbols {
        if s.name == target && s.kind == SymbolKind::Struct {
            return Some(target.to_string());
        }
    }

    for s in &analysis.symbols {
        if s.name == target && matches!(s.kind, SymbolKind::Local | SymbolKind::Global | SymbolKind::Parameter) {
            if let Some(ty) = s.inferred_type.as_deref() {
                if let Some(root) = type_root_name_of_str(ty) {
                    return Some(root);
                }
            }
        }
    }

    // Check if preceded by a type name e.g. `User u = {`
    let text = &analysis.text;
    let bytes = text.as_bytes();
    let mut type_end = start;
    while type_end > 0 && (bytes[type_end - 1] as char).is_whitespace() {
        type_end -= 1;
    }
    let mut type_start = type_end;
    while type_start > 0 && ((bytes[type_start - 1] as char).is_alphanumeric() || bytes[type_start - 1] == b'_') {
        type_start -= 1;
    }
    let declared = text[type_start..type_end].trim();
    if !declared.is_empty() && analysis.symbols.iter().any(|s| s.name == declared && s.kind == SymbolKind::Struct) {
        return Some(declared.to_string());
    }

    None
}

fn check_match_arm_context(text: &str, offset: usize, arm_prefix: &str) -> Option<SyntaxContext> {
    let bytes = text.as_bytes();
    let mut i = offset;
    let mut depth = 0;
    let mut found_brace = false;
    let mut brace_offset = 0;

    while i > 0 {
        i -= 1;
        if bytes[i] == b'}' {
            depth += 1;
        } else if bytes[i] == b'{' {
            if depth == 0 {
                found_brace = true;
                brace_offset = i;
                break;
            }
            depth -= 1;
        }
    }

    if !found_brace {
        return None;
    }

    let before = text[..brace_offset].trim_end();
    if !before.ends_with(|c: char| c.is_alphanumeric() || c == ')' || c == '_') {
        return None;
    }

    let match_pos = before.rfind("match ")?;
    let expr = before[match_pos + 6..].trim().to_string();

    let body = &text[brace_offset + 1..offset];
    let mut existing_arms = Vec::new();
    for line in body.lines() {
        let trimmed = line.trim();
        if let Some(arrow) = trimmed.find("=>") {
            let pattern = trimmed[..arrow].trim();
            let name = pattern
                .split(|c: char| !c.is_alphanumeric() && c != '_')
                .next()
                .unwrap_or("");
            if !name.is_empty() {
                existing_arms.push(name.to_string());
            }
        }
    }

    Some(SyntaxContext::MatchArms {
        match_expr: expr,
        arm_prefix: arm_prefix.to_string(),
        existing_arms,
    })
}

fn check_type_position(text: &str, offset: usize, prefix: &str) -> bool {
    let before = text[..offset].trim_end();
    if before.is_empty() {
        return false;
    }

    let check_str = if prefix.is_empty() {
        before
    } else {
        let trimmed_len = offset.saturating_sub(prefix.len());
        text[..trimmed_len].trim_end()
    };

    check_str.ends_with(':')
        || check_str.ends_with("->")
        || check_str.ends_with("let")
        || check_str.ends_with("const")
        || check_str.ends_with("mut")
        || check_str.ends_with("cast<")
        || check_str.ends_with('<')
        || check_str.ends_with("as")
}

fn check_scope_depth(analysis: &SymbolIndex, offset: usize) -> (usize, bool) {
    let mut depth: usize = 0;
    let mut in_loop = false;
    let mut loop_depths = Vec::new();

    for t in &analysis.tokens {
        if t.span.start >= offset {
            break;
        }
        match t.kind {
            Token::LeftBrace => {
                depth += 1;
            }
            Token::RightBrace => {
                depth = depth.saturating_sub(1);
                loop_depths.retain(|&d| d <= depth);
            }
            Token::While | Token::For => {
                loop_depths.push(depth + 1);
            }
            _ => {}
        }
    }

    if !loop_depths.is_empty() && loop_depths.iter().any(|&d| depth >= d) {
        in_loop = true;
    }

    (depth, in_loop)
}
