use owo_colors::OwoColorize;

use crate::lexer::Span;

#[derive(Debug, Clone, Copy)]
pub enum Severity {
    Error,
    Warning,
}

pub fn render(source: &str, path: &str, span: Span, message: &str, severity: Severity) -> String {
    let (line, col, line_start, line_end) = line_info(source, span.start);
    let line_text = &source[line_start..line_end];

    let line_num_width = line.to_string().len();
    let header = match severity {
        Severity::Error => format!(
            "{}: {}: {}",
            format!("{path}:{line}:{col}").bold(),
            "error".red().bold(),
            message.bold()
        ),
        Severity::Warning => format!(
            "{}: {}: {}",
            format!("{path}:{line}:{col}").bold(),
            "warning".yellow().bold(),
            message.bold()
        ),
    };

    let mut caret_start = col.saturating_sub(1);
    let mut caret_len = span.end.saturating_sub(span.start);
    if caret_len == 0 {
        caret_len = 1;
    }

    let line_len = line_text.chars().count();
    if caret_start >= line_len {
        caret_start = line_len.saturating_sub(1);
    }
    if caret_start + caret_len > line_len {
        caret_len = line_len.saturating_sub(caret_start).max(1);
    }

    let line_prefix = format!("{:>width$} | ", line, width = line_num_width);
    let mut underline = String::new();
    underline.push_str(&" ".repeat(caret_start));
    underline.push_str(&"^".repeat(caret_len));

    let underline = match severity {
        Severity::Error => underline.red().bold().to_string(),
        Severity::Warning => underline.yellow().bold().to_string(),
    };

    format!(
        "{header}\n{line_prefix}{line_text}\n{:>width$} | {underline}",
        "",
        width = line_num_width
    )
}

fn line_info(source: &str, offset: usize) -> (usize, usize, usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut idx = 0usize;
    let mut line_start = 0usize;

    for ch in source.chars() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            line_start = idx + ch.len_utf8();
        } else {
            col += 1;
        }
        idx += ch.len_utf8();
    }

    let mut line_end = source.len();
    let mut scan = line_start;
    for ch in source[line_start..].chars() {
        if ch == '\n' {
            line_end = scan;
            break;
        }
        scan += ch.len_utf8();
    }

    (line, col, line_start, line_end)
}
