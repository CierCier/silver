# HTTP/1.0 & HTTP/1.1 Standard Compliance

- **Primary Specifications**: [RFC 9110 (HTTP Semantics)](https://datatracker.ietf.org/doc/html/rfc9110), [RFC 9112 (HTTP/1.1 Framing)](https://datatracker.ietf.org/doc/html/rfc9112).
- **Implementation**: [`std/net/http.ag`](file:///home/cier/Projects/silver/std/net/http.ag) (`HttpClient`, `HttpServer`, `ResponseWriter`, `ServeMux`, `HttpRequest`, `HttpResponse`, `HttpStream`, `HttpConnection`).
- **Test Suite**: [`tests/http_test.ag`](file:///home/cier/Projects/silver/tests/http_test.ag), [`tests/http_server_test.ag`](file:///home/cier/Projects/silver/tests/http_server_test.ag), [`tests/http_bench.ag`](file:///home/cier/Projects/silver/tests/http_bench.ag), [`tests/stream_test.ag`](file:///home/cier/Projects/silver/tests/stream_test.ag), [`tests/pool_test.ag`](file:///home/cier/Projects/silver/tests/pool_test.ag).

## Client Compliance Table

| Feature / Capability | RFC Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **HTTP Methods** | RFC 9110 §9 | Supported | `client.get`, `post`, `head`, `do` | Supports GET, HEAD, POST, PUT, DELETE, PATCH, OPTIONS. |
| **Status Codes & Canonical Reasons** | RFC 9110 §15 | Supported | `HttpStatus`, `http_status_reason()` | Complete mapping for 1xx, 2xx, 3xx, 4xx, and 5xx series. |
| **Chunked Transfer Encoding** | RFC 9112 §7.1 | Supported | `HttpResponse.read_chunked`, `HttpStream.read_chunked` | Decodes hex chunk sizes, variable body data, and final `0\r\n` chunk. |
| **Chunk Extensions** | RFC 9112 §7.1.1 | Supported | `http_parse_hex` | Unrecognized chunk extensions after `;` are ignored per specification. |
| **Trailers Parsing & Storage** | RFC 9112 §7.1.2 | Supported | `response.get_trailer(name)`, `stream.get_trailer(name)` | Preserves trailer headers following the terminating zero chunk. |
| **Interim 1xx Response Handling** | RFC 9110 §15.2 | Supported | `HttpResponse.read_final_head` | Automatically advances past interim 100/102/103 headers with full state resets. |
| **Transfer-Encoding Smuggling Safeguard** | RFC 9112 §6.1 | Supported | `HttpResponse.read_head` | Forces `content_length = -1` when `chunked` is active to prevent smuggling attacks. |
| **Conflicting Content-Length Safeguard** | RFC 9112 §6.1 | Supported | `HttpResponse.read_head` | Rejects responses carrying multiple conflicting `Content-Length` headers with `HTTP_ERR_MALFORMED`. |
| **Keep-Alive Connection Reuse** | RFC 9112 §9.3 | Supported | `HttpClient.set_pool_max_idle` | Thread-safe connection pool reusing connections, with auto-retry on stale peer disconnects. |
| **Redirect Handling** | RFC 9110 §15.4 | Supported | `HttpClient.do(req)` | Follows 301, 302, 303, 307, and 308 redirects up to configurable `max_redirects`. |
| **Cross-Origin Header Scrubbing** | RFC 9110 §15.4 | Supported | `http_strip_sensitive_headers` | Drops `Authorization` and `Cookie` headers when redirected to a different origin. |
| **Streaming Response Bodies** | RFC 9110 §8.6 | Supported | `HttpClient.do_stream(req)` | Streams payloads on-demand through `HttpStream` without buffering entire bodies into memory. |
| **Socket Read/Write Timeouts** | Silver Standard | Supported | `client.timeout_secs`, `conn.set_timeout` | Bounded socket operations including handshake and chunk processing. |
| **Top-Level Client Convenience** | Go Convention | Supported | `http_get(url)`, `http_post(url, type, body)` | Ergonomic one-line HTTP client methods matching Go's `http.Get`/`Post`. |

## Server Compliance Table

| Feature / Capability | RFC Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Go-Like ResponseWriter** | RFC 9110 / Go | Supported | `ResponseWriter.set_header`, `write_header`, `write`, `write_str` | Buffered and streaming response output with automatic default 200 OK and Content-Length calculation. |
| **Request Multiplexer & Routing** | Go Convention | Supported | `ServeMux`, `http_handle_func` | Prefix and exact route matching with fallback default `404 page not found`. |
| **Mandatory Host Header Validation** | RFC 9112 §3.2 | Supported | `HttpServer.serve_conn` | Automatically rejects HTTP/1.1 requests missing `Host:` header with `400 Bad Request`. |
| **Expect: 100-continue Handling** | RFC 9110 §10.1.1 | Supported | `HttpServer.serve_conn` | Emits interim `HTTP/1.1 100 Continue` prior to consuming request bodies. |
| **Inbound Chunked Body Decoding** | RFC 9112 §7.1 | Supported | `HttpRequest.read_chunked` | Decodes inbound chunked request bodies up to configured `max_body_bytes`. |
| **Chunked Server Responses** | RFC 9112 §7.1 | Supported | `ResponseWriter.write_chunk`, `finish` | Automatically uses chunked framing when body size is not known in advance, terminated by `0\r\n\r\n`. |
| **Keep-Alive Connection Persistence** | RFC 9112 §9.3 | Supported | `HttpServer.serve_conn` | Serves sequential requests over persistent connections, exiting on `Connection: close` or idle timeout. |
| **One-Line Server Startup** | Go Convention | Supported | `http_listen_and_serve(addr)` | Binds TCP listener and serves requests via `g_default_mux` matching Go's `http.ListenAndServe`. |
| **Inbound Request Smuggling Guard** | RFC 9112 §6.1 | Supported | `HttpRequest.read_head` | Rejects requests with conflicting `Content-Length` headers or ambiguous `Transfer-Encoding`. |

