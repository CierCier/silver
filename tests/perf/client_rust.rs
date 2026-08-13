// tests/perf/client_rust.rs — Rust HTTP/1.1 client benchmark, std-only.
//
// This client mirrors the *structural work* the Silver HttpClient does per
// request, so the comparison is apples-to-apples (a raw TcpStream ping-pong
// would measure the loopback ceiling, not an HTTP client):
//
//   - parse the URL (host/port/path) every request,
//   - build the request head as a fresh String every request,
//   - parse the response head (status line + headers) into a per-response
//     Vec<(String, String)>,
//   - allocate and fill the response body Vec per request,
//   - drop all of it per request.
//
// The connection-pool lookup (a single-map get in Silver/Go) is omitted; the
// dominant per-request costs above are mirrored.
//
// Build: rustc -O tests/perf/client_rust.rs -o /tmp/http_perf_rust

use std::io::{Read, Write};
use std::net::TcpStream;
use std::time::Instant;

/// Parse "http://host:port/path" into (host, port, path), fresh Strings.
fn parse_url(url: &str) -> (String, u16, String) {
    let rest = url.strip_prefix("http://").unwrap_or(url);
    let (authority, path) = match rest.find('/') {
        Some(i) => (&rest[..i], &rest[i..]),
        None => (rest, "/"),
    };
    let (host, port) = match authority.find(':') {
        Some(i) => (&authority[..i], authority[i + 1..].parse().unwrap_or(80)),
        None => (authority, 80),
    };
    (host.to_string(), port, path.to_string())
}

/// Parse the response head: status line + headers into a Vec, plus the
/// Content-Length.
fn parse_head(head: &[u8]) -> (u16, Vec<(String, String)>, Option<usize>) {
    let text = String::from_utf8_lossy(head);
    let mut lines = text.lines();
    let status: u16 = lines
        .next()
        .and_then(|l| l.split_whitespace().nth(1))
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    let mut headers: Vec<(String, String)> = Vec::new();
    let mut len = None;
    for line in lines {
        if let Some((k, v)) = line.split_once(':') {
            let key = k.trim().to_string();
            let value = v.trim().to_string();
            if key.eq_ignore_ascii_case("content-length") {
                len = value.parse().ok();
            }
            headers.push((key, value));
        }
    }
    (status, headers, len)
}

fn find_head_end(data: &[u8]) -> Option<usize> {
    data.windows(4).position(|w| w == b"\r\n\r\n").map(|p| p + 4)
}

/// One full request: parse URL, build request, send, parse head, read body.
fn round_trip(stream: &mut TcpStream, url: &str, buf: &mut [u8]) -> bool {
    let (host, _port, path) = parse_url(url);
    let req = format!(
        "GET {} HTTP/1.1\r\nHost: {}\r\nConnection: keep-alive\r\n\r\n",
        path, host
    );
    if stream.write_all(req.as_bytes()).is_err() {
        return false;
    }
    let mut total = 0usize;
    let head_end = loop {
        match stream.read(&mut buf[total..]) {
            Ok(0) => return false,
            Ok(n) => {
                total += n;
                if let Some(pos) = find_head_end(&buf[..total]) {
                    break pos;
                }
                if total == buf.len() {
                    return false;
                }
            }
            Err(_) => return false,
        }
    };
    let (_status, _headers, content_len) = parse_head(&buf[..head_end]);
    let len = match content_len {
        Some(l) => l,
        None => return false,
    };
    // Own the body: allocate a fresh Vec per response (like Silver's String).
    let mut body = vec![0u8; len];
    let mut have = total - head_end;
    let mut offset = 0usize;
    while have < len {
        match stream.read(&mut buf[..]) {
            Ok(0) => return false,
            Ok(n) => {
                let take = n.min(len - have);
                body[offset..offset + take].copy_from_slice(&buf[..take]);
                offset += take;
                have += take;
            }
            Err(_) => return false,
        }
    }
    // `req`, `host`, `path`, `_headers`, `body` all drop here.
    let _ = &body;
    true
}

fn main() {
    let url = "http://127.0.0.1:18099/";
    let Ok(mut stream) = TcpStream::connect("127.0.0.1:18099") else {
        eprintln!("connect failed");
        std::process::exit(1);
    };
    let _ = stream.set_nodelay(true);
    let mut buf = vec![0u8; 65536];

    if !round_trip(&mut stream, url, &mut buf) {
        eprintln!("warmup failed");
        std::process::exit(1);
    }

    let start = Instant::now();
    let mut count = 0u64;
    while start.elapsed().as_secs() < 15 {
        if !round_trip(&mut stream, url, &mut buf) {
            eprintln!("request failed");
            break;
        }
        count += 1;
    }
    let elapsed = start.elapsed().as_secs_f64();
    println!("rust: {} requests in {:.2}s ({:.0} req/s)", count, elapsed, count as f64 / elapsed);
}
