pub mod messages;

use owo_colors::OwoColorize;

use crate::lexer::{Span, source_file};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

/// Render a diagnostic for `span`. The span carries its own file id and
/// 1-based line/column, so the correct source file is looked up through the
/// source registry — errors in imported modules render against their own
/// file rather than the top-level input.
pub fn render(span: Span, message: &str, severity: Severity) -> String {
    let (path, line_text, line, col) = match source_file(span.file) {
        Some(file) if span.start_line > 0 => {
            let line_text = file
                .text
                .lines()
                .nth(span.start_line as usize - 1)
                .unwrap_or("");
            (
                file.path,
                line_text.to_string(),
                span.start_line,
                span.start_col,
            )
        }
        _ => (
            String::from("memory"),
            String::new(),
            span.start_line,
            span.start_col,
        ),
    };

    let header = match severity {
        Severity::Error => {
            if line > 0 {
                format!(
                    "{}: {}: {}",
                    "error".red().bold(),
                    format!("{path}:{line}:{col}").bold(),
                    message.bold()
                )
            } else {
                format!("{}: {}", "error".red().bold(), message.bold())
            }
        }
        Severity::Warning => {
            if line > 0 {
                format!(
                    "{}: {}: {}",
                    "warn".yellow().bold(),
                    format!("{path}:{line}:{col}").bold(),
                    message.bold()
                )
            } else {
                format!("{}: {}", "warn".yellow().bold(), message.bold())
            }
        }
        Severity::Note => {
            if line > 0 {
                format!(
                    "{}: {}: {}",
                    "note".cyan().bold(),
                    format!("{path}:{line}:{col}").bold(),
                    message
                )
            } else {
                format!("{}: {}", "note".cyan().bold(), message)
            }
        }
    };

    // No source text (synthetic span or unregistered file): header only.
    if line_text.is_empty() {
        return header;
    }

    let col_idx = col.saturating_sub(1) as usize;
    let span_len = span.end.saturating_sub(span.start);
    let span_chars = span_len.max(1);

    let mut rendered_line = String::new();
    let mut underline = String::new();

    let mut char_count = 0usize;
    for (char_idx, ch) in line_text.chars().enumerate() {
        char_count += 1;
        let is_in_span = char_idx >= col_idx && char_idx < col_idx + span_chars;
        let is_before_span = char_idx < col_idx;

        if ch == '\t' {
            rendered_line.push_str("    ");
            if is_before_span {
                underline.push_str("    ");
            } else if is_in_span {
                underline.push_str("^^^^");
            }
        } else {
            rendered_line.push(ch);
            if is_before_span {
                underline.push(' ');
            } else if is_in_span {
                underline.push('^');
            }
        }
    }

    // If span started past the line end, point at the end of the line.
    if col_idx >= char_count && !rendered_line.is_empty() {
        underline.clear();
        underline.push_str(&" ".repeat(rendered_line.chars().count().saturating_sub(1)));
        underline.push('^');
    }

    let line_num_width = line.to_string().len();
    let line_prefix = format!("{:>width$} | ", line, width = line_num_width);
    let underline = match severity {
        Severity::Error => underline.red().bold().to_string(),
        Severity::Warning => underline.yellow().bold().to_string(),
        Severity::Note => underline.cyan().bold().to_string(),
    };

    format!(
        "{header}\n{line_prefix}{rendered_line}\n{:>width$} | {underline}",
        "",
        width = line_num_width
    )
}

/// Render a diagnostic with an optional secondary note pointing to another span.
pub fn render_with_note(
    span: Span,
    message: &str,
    severity: Severity,
    note_span: Option<Span>,
    note_message: Option<&str>,
) -> String {
    let main_diag = render(span, message, severity);
    if let (Some(n_span), Some(n_msg)) = (note_span, note_message)
        && n_span.start_line > 0
    {
        return format!("{}\n{}", main_diag, render(n_span, n_msg, Severity::Note));
    }
    main_diag
}

/// Compute the Levenshtein edit distance between two strings.
pub fn levenshtein(a: &str, b: &str) -> usize {
    if a == b {
        return 0;
    }
    let a_len = a.chars().count();
    let b_len = b.chars().count();
    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let b_chars: Vec<char> = b.chars().collect();
    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row: Vec<usize> = vec![0; b_len + 1];

    for (i, ca) in a.chars().enumerate() {
        curr_row[0] = i + 1;
        for (j, &cb) in b_chars.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr_row[j + 1] = (prev_row[j + 1] + 1)
                .min(curr_row[j] + 1)
                .min(prev_row[j] + cost);
        }
        prev_row.copy_from_slice(&curr_row);
    }
    prev_row[b_len]
}

/// Find the best matching candidate for `name` among `candidates`.
/// Returns `Some(best_match)` if a candidate is sufficiently close.
pub fn find_best_match<I, S>(name: &str, candidates: I) -> Option<String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let name_len = name.chars().count();
    if name_len == 0 {
        return None;
    }
    // Allow edit distance scaled to name length.
    let max_dist = match name_len {
        0..=2 => 1,
        3..=5 => 2,
        6..=10 => 3,
        _ => (name_len / 3).max(3),
    };

    let name_lower = name.to_lowercase();
    let mut best_candidate: Option<String> = None;
    let mut best_dist = usize::MAX;

    for cand in candidates {
        let cand_str = cand.as_ref();
        if cand_str == name {
            continue;
        }
        let cand_lower = cand_str.to_lowercase();
        let dist = if name_lower == cand_lower {
            0
        } else {
            levenshtein(&name_lower, &cand_lower)
        };

        if dist <= max_dist && dist < best_dist {
            best_dist = dist;
            best_candidate = Some(cand_str.to_string());
        }
    }

    best_candidate
}

/// Helper that formats a typo suggestion suffix, e.g. `", did you mean 'len'?"`
pub fn suggestion_suffix<I, S>(name: &str, candidates: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    if let Some(best) = find_best_match(name, candidates) {
        format!(", did you mean '{}'?", best)
    } else {
        String::new()
    }
}
