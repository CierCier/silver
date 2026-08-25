//! The scan driver: whitespace → comments → identifier/keyword → operator
//! trie → spec hook. One pass over the byte buffer, no per-token allocation.

use crate::spec::{CommentConfig, CommonKinds, LexSpec};
use crate::token::TokenBuf;
use crate::LexError;

/// Scan `src` with `spec`, producing a flat [`TokenBuf`].
///
/// Stops at the first error (matching the legacy lexer's fail-fast
/// contract); the trailing EOF row is only appended on success.
pub fn scan<S: LexSpec>(spec: &mut S, src: &str) -> Result<TokenBuf, LexError> {
    let bytes = src.as_bytes();
    let len = bytes.len();
    let mut buf = TokenBuf::new();
    let common: CommonKinds = *spec.common_kinds();
    let comment_cfg: Option<CommentConfig> = spec.comment_config();

    let mut pos = 0usize;
    while pos < len {
        let b = bytes[pos];

        // Layout runs.
        if spec.is_layout(b) {
            let start = pos;
            while pos < len && spec.is_layout(bytes[pos]) {
                pos += 1;
            }
            buf.push_trivia(common.layout, start as u32, (pos - start) as u32);
            continue;
        }

        // Comments — generic scanner driven by the config.
        if let Some(cfg) = comment_cfg {
            if b == b'/' && pos + 1 < len {
                match bytes[pos + 1] {
                    b'/' => {
                        let doc = pos + 2 < len && bytes[pos + 2] == b'/';
                        let content_start = if doc { pos + 3 } else { pos + 2 };
                        let mut end = content_start;
                        while end < len && bytes[end] != b'\n' {
                            end += 1;
                        }
                        let kind = if doc {
                            common.doc_line_comment
                        } else {
                            common.line_comment
                        };
                        buf.push_trivia(kind, pos as u32, (end - pos) as u32);
                        pos = end;
                        continue;
                    }
                    b'*' => {
                        pos = scan_block_comment(
                            &mut buf,
                            bytes,
                            pos,
                            &common,
                            cfg.nested_blocks,
                        )?;
                        continue;
                    }
                    _ => {}
                }
            }
        }

        // Identifier / keyword words.
        if spec.ident_start(b) {
            let start = pos;
            pos += 1;
            while pos < len && spec.ident_continue(bytes[pos]) {
                pos += 1;
            }
            let word = &bytes[start..pos];
            let kind = spec.keywords().get(word).unwrap_or_else(|| spec.ident_kind());
            buf.push_token(kind, start as u32, (pos - start) as u32);
            continue;
        }

        // Operator longest-match.
        if let Some((kind, matched)) = spec.op_trie().longest_match(&bytes[pos..]) {
            buf.push_token(kind, pos as u32, matched as u32);
            pos += matched;
            continue;
        }

        // Everything else belongs to the spec.
        pos = spec.scan_other(bytes, pos, &mut buf)?;
    }

    buf.push_eof(common.eof, pos as u32);
    Ok(buf)
}

/// Nestable block-comment scan. `pos` points at `/`; returns position after
/// the closing marker. Emits one trivia row covering the whole comment.
fn scan_block_comment(
    buf: &mut TokenBuf,
    bytes: &[u8],
    start: usize,
    common: &CommonKinds,
    nested: bool,
) -> Result<usize, LexError> {
    let len = bytes.len();
    let doc = start + 2 < len && bytes[start + 2] == b'*';
    // Consume the opener (`/*` or `/**`). A doc-marker star is part of the
    // delimiter and can never act as a closer — so `/**/` is an unterminated
    // doc comment, exactly like the legacy scanner.
    let mut pos = start + if doc { 3 } else { 2 };
    let mut depth = 1usize;

    while pos < len {
        let b = bytes[pos];
        if nested && b == b'/' && pos + 1 < len && bytes[pos + 1] == b'*' {
            depth += 1;
            pos += 2;
        } else if b == b'*' && pos + 1 < len && bytes[pos + 1] == b'/' {
            depth -= 1;
            pos += 2;
            if depth == 0 {
                let kind = if doc {
                    common.doc_block_comment
                } else {
                    common.block_comment
                };
                buf.push_trivia(kind, start as u32, (pos - start) as u32);
                return Ok(pos);
            }
        } else {
            pos += 1;
        }
    }
    Err(LexError::UnexpectedEof { pos: start as u32 })
}
