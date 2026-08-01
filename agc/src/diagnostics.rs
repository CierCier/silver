use owo_colors::OwoColorize;

use crate::lexer::{Span, source_file};

#[derive(Debug, Clone, Copy)]
pub enum Severity {
    Error,
    Warning,
}

/// Render a diagnostic for `span`. The span carries its own file id and
/// 1-based line/column, so the correct source file is looked up through the
/// source registry — errors in imported modules render against their own
/// file rather than the top-level input.
pub fn render(span: Span, message: &str, severity: Severity) -> String {
    let (path, line_text, line, col) = match source_file(span.file) {
        Some(file) if span.start_line > 0 => {
            let line_text = file.text.lines().nth(span.start_line as usize - 1).unwrap_or("");
            (file.path, line_text.to_string(), span.start_line, span.start_col)
        }
        _ => (String::from("memory"), String::new(), span.start_line, span.start_col),
    };

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

    // No source text (synthetic span or unregistered file): header only.
    if line_text.is_empty() {
        return header;
    }

    let mut caret_start = span.start_col.saturating_sub(1);
    let mut caret_len = (span.end.saturating_sub(span.start)) as u32;
    if caret_len == 0 {
        caret_len = 1;
    }

    let line_len = line_text.chars().count() as u32;
    if caret_start >= line_len {
        caret_start = line_len.saturating_sub(1);
    }
    if caret_start + caret_len > line_len {
        caret_len = line_len.saturating_sub(caret_start).max(1);
    }

    let line_num_width = line.to_string().len();
    let line_prefix = format!("{:>width$} | ", line, width = line_num_width);
    let mut underline = String::new();
    underline.push_str(&" ".repeat(caret_start as usize));
    underline.push_str(&"^".repeat(caret_len as usize));

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Span, line_col_at, register_source};

    #[test]
    fn renders_span_in_registered_file_with_caret() {
        let path = "/tmp/diag_test.ag";
        let text = "i32 main() {\n    u8 bad = 300;\n}\n";
        let file = register_source(path, text);
        assert!(file != 0, "registered files get nonzero ids");

        let (line, col) = line_col_at(text, 26); // '3' of `300` on line 2
        let span = Span {
            start: 26,
            end: 29,
            file,
            start_line: line,
            start_col: col,
            end_line: line,
            end_col: col + 3,
        };
        let out = render(span, "integer literal 300 does not fit in type U8", Severity::Error);
        assert!(out.contains("/tmp/diag_test.ag:2:14"), "header location: {out}");
        assert!(
            out.contains("300 does not fit in type U8"),
            "header message: {out}"
        );
        assert!(out.contains("    u8 bad = 300;"), "line text: {out}");
        assert!(out.contains("^^^"), "caret: {out}");
    }

    #[test]
    fn synthetic_span_renders_header_only() {
        let out = render(Span::default(), "synthetic error", Severity::Error);
        assert!(out.contains("synthetic error"), "{out}");
        assert!(!out.contains("^"));
    }

    #[test]
    fn line_col_at_counts_utf8_chars() {
        let text = "first line\ncafé second\n";
        let (l1, c1) = line_col_at(text, 0);
        assert_eq!((l1, c1), (1, 1));
        // 'café' = 4 chars (é is one char, two bytes): é starts at byte 14, col 4.
        let (l2, c2) = line_col_at(text, 14);
        assert_eq!((l2, c2), (2, 4));
    }
}
