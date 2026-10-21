//! Conservative whole-document formatting.
//!
//! v1 is whitespace-only and cannot corrupt code:
//! - strips trailing whitespace from every line,
//! - collapses runs of two or more blank lines into one,
//! - guarantees a single trailing newline at EOF.

pub(crate) fn format_silver(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut pending_blank = false;
    let mut any_content = false;

    for raw_line in text.split('\n') {
        let line = raw_line.trim_end();
        if line.is_empty() {
            if any_content {
                pending_blank = true;
            }
            continue;
        }
        if pending_blank {
            out.push('\n'); // one blank separator line
        }
        out.push_str(line);
        out.push('\n');
        any_content = true;
        pending_blank = false;
    }
    out
}
