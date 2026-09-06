//! Doc comment parsing for hover rendering.
//!
//! Doc comments arrive as raw text (consecutive `///` lines joined with
//! newlines, or `/** ... */` content). This module cleans the text and
//! renders a small set of tags (`@param`, `@return`, `@example`, ...) as
//! markdown suitable for `MarkupContent`.

pub(crate) fn doc_to_markdown(doc: &str) -> String {
    let cleaned = clean_lines(doc);
    let mut description: Vec<&str> = Vec::new();
    let mut params: Vec<(String, String)> = Vec::new();
    let mut returns: Vec<String> = Vec::new();
    let mut examples: Vec<String> = Vec::new();
    let mut notes: Vec<String> = Vec::new();

    let mut example_lines: Vec<String> = Vec::new();
    let mut in_example = false;

    for raw in cleaned.lines() {
        let line = raw.trim();
        if line.is_empty() {
            if in_example {
                example_lines.push(String::new());
            } else {
                description.push("");
            }
            continue;
        }
        if let Some(rest) = line.strip_prefix("@param") {
            // normalize: "@param name description" / "@param(name) desc"
            let rest = rest.trim();
            let (name, desc) = split_param(rest);
            params.push((name, desc));
            continue;
        }
        if let Some(rest) = line.strip_prefix("@return") {
            returns.push(rest.trim().to_string());
            continue;
        }
        if let Some(rest) = line.strip_prefix("@returns") {
            returns.push(rest.trim().to_string());
            continue;
        }
        if let Some(rest) = line.strip_prefix("@note") {
            notes.push(rest.trim().to_string());
            continue;
        }
        if line.starts_with("@example") {
            in_example = true;
            example_lines.clear();
            continue;
        }
        if line.starts_with("@") {
            // Unknown tag: keep as description text.
            description.push(line);
            continue;
        }
        if in_example {
            example_lines.push(line.to_string());
            continue;
        }
        description.push(line);
    }
    if in_example {
        examples.push(example_lines.join("\n"));
    }

    let mut out = String::new();
    let mut prev_blank = true;
    for line in &description {
        if line.is_empty() {
            if !prev_blank {
                out.push('\n');
            }
            prev_blank = true;
        } else {
            out.push_str(line);
            out.push('\n');
            prev_blank = false;
        }
    }
    trim_trailing_blank(&mut out);

    if !params.is_empty() {
        push_section(&mut out, "Parameters");
        for (name, desc) in &params {
            out.push_str(&format!("- `{name}` — {desc}\n"));
        }
    }
    if !returns.is_empty() {
        push_section(&mut out, "Returns");
        for r in &returns {
            out.push_str(&format!("- {r}\n"));
        }
    }
    if !notes.is_empty() {
        push_section(&mut out, "Notes");
        for n in &notes {
            out.push_str(&format!("- {n}\n"));
        }
    }
    for example in &examples {
        push_section(&mut out, "Example");
        out.push_str("```silver\n");
        out.push_str(example.trim());
        out.push('\n');
        out.push_str("```\n");
    }
    trim_trailing_blank(&mut out);
    out
}

/// Clean block-comment text: strip leading whitespace + `*` decoration.
fn clean_lines(doc: &str) -> String {
    doc.lines()
        .map(|line| {
            let trimmed = line.trim_start();
            if let Some(rest) = trimmed.strip_prefix('*') {
                rest.trim_start().to_string()
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Split `name description` (or `(name) description`) into (name, desc).
fn split_param(rest: &str) -> (String, String) {
    let rest = rest.trim();
    if let Some(inner) = rest.strip_prefix('(').and_then(|r| r.split_once(')')) {
        return (inner.0.trim().to_string(), inner.1.trim().to_string());
    }
    match rest.split_once(char::is_whitespace) {
        Some((name, desc)) => (name.to_string(), desc.trim().to_string()),
        None => (rest.to_string(), String::new()),
    }
}

fn push_section(out: &mut String, title: &str) {
    if !out.is_empty() {
        out.push('\n');
    }
    out.push_str(&format!("**{title}:**\n"));
}

fn trim_trailing_blank(out: &mut String) {
    while out.ends_with('\n') {
        out.pop();
    }
}
