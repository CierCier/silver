# HTTP/2 Standard Compliance

- **Primary Specifications**: [RFC 9113 (HTTP/2)](https://datatracker.ietf.org/doc/html/rfc9113).
- **Implementation**: [`std/net/http2.ag`](file:///home/cier/Projects/silver/std/net/http2.ag) (`H2Connection`, `H2Stream`, `H2Response`), [`std/net/http.ag`](file:///home/cier/Projects/silver/std/net/http.ag) (`HttpServer`, `ResponseWriter`, `h2_serve_conn`).
- **Test Suite**: [`tests/http2_test.ag`](file:///home/cier/Projects/silver/tests/http2_test.ag), [`tests/http2_server_test.ag`](file:///home/cier/Projects/silver/tests/http2_server_test.ag), [`tests/http2_tls_test.ag`](file:///home/cier/Projects/silver/tests/http2_tls_test.ag).

## Client Compliance Table

| Feature / Capability | RFC 9113 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Client Connection Preface** | §3.4 | Supported | `h2_send_preface` | Transmits `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` followed immediately by initial SETTINGS frame. |
| **9-Byte Frame Header** | §4.1 | Supported | `h2_send_frame`, `h2_read_frame` | Encodes/decodes length (24-bit), type (8-bit), flags (8-bit), and stream identifier (31-bit). |
| **SETTINGS Exchange & Negotiation** | §6.5 | Supported | `h2_apply_settings` | Parses peer settings (`MAX_FRAME_SIZE`, `INITIAL_WINDOW_SIZE`, `HEADER_TABLE_SIZE`). |
| **SETTINGS Acknowledgment** | §6.5.3 | Supported | `h2_send_settings_ack` | Emits empty SETTINGS frame with ACK flag set upon receiving peer settings. |
| **HEADERS Frame Emission** | §6.2 | Supported | `h2_send_request_frames` | Encodes `:method`, `:scheme`, `:authority`, `:path`, and custom headers using HPACK. |
| **Outbound CONTINUATION Frames** | §6.10 | Supported | `h2_send_request_frames` | Dynamically fragments header blocks exceeding `peer_max_frame_size` across continuous CONTINUATION frames with final `END_HEADERS`. |
| **DATA Frame Emission & Slicing** | §6.1 | Supported | `h2_send_request_frames` | Slices request payload according to `peer_max_frame_size` and available flow control credit. |
| **Outbound Flow Control** | §5.2 | Supported | `c.peer_conn_window` | Tracks peer connection credit via `WINDOW_UPDATE` frames; decrements credit as DATA frames are sent. |
| **Inbound Flow Control** | §5.2 | Supported | `h2_send_window_update` | Automatically replenishes connection and stream flow windows when unconsumed credit drops below 32KB. |
| **Per-Stream RST_STREAM Handling** | §6.4 | Supported | `resp.rst_error`, `c.streams[i]` | Cancels only the designated stream (`sid`) without terminating the multiplexed connection. |
| **Response Trailers** | §8.1 | Supported | `h2_response_get_trailer` | Decodes subsequent trailing HEADERS carrying `END_STREAM` into `resp.trailers`. |
| **Multiplexed Streams** | §5.1 | Supported | `h2_begin`, `h2_wait_all` | Supports parallel concurrent stream dispatch and unified response waiting over a single connection. |
| **PING & Heartbeat Frames** | §6.7 | Supported | `h2_read_response`, `h2_wait_all` | Automatically echoes back received PING frames with the ACK flag set. |
| **GOAWAY Teardown** | §6.8 | Supported | `h2_read_response`, `h2_wait_all` | Gracefully closes connections on peer GOAWAY notifications. |
| **HttpClient Transparent H2** | §3.3 | Supported | `HttpClient.get(...)`, `do(...)` | Uses ALPN negotiation to transparently invoke HTTP/2 and convert `H2Response` into `HttpResponse`. |

## Server Compliance Table

| Feature / Capability | RFC 9113 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Client Preface Verification** | §3.4 | Supported | `h2_check_client_preface` | Validates client magic `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` upon connection accept. |
| **Transparent h2c Sniffing** | §3.4 / Go | Supported | `HttpServer.serve_conn` | Automatically peeks connection preface to dispatch between HTTP/1.1 and HTTP/2 over the same socket. |
| **Server SETTINGS Negotiation** | §6.5 | Supported | `h2_send_server_settings` | Emits server SETTINGS with concurrency limits, window size, and frame size. |
| **Client Stream Parity Check** | §5.1.1 | Supported | `h2_serve_conn` | Enforces that client-initiated streams must have odd stream IDs, rejecting even IDs with `PROTOCOL_ERROR`. |
| **Inbound Flow Control** | §5.2 | Supported | `h2_serve_conn` | Emits `WINDOW_UPDATE` frames on stream and connection levels when credit falls below threshold. |
| **Server Response HEADERS** | §6.2 | Supported | `h2_send_response_headers` | Encodes `:status` and response headers via HPACK, filtering connection-specific headers (§8.3.1). |
| **Server Flow-Controlled DATA** | §6.1 | Supported | `h2_send_response_data` | Emits payload chunks respecting client frame size and flow control credits. |
| **Protocol-Agnostic Handlers** | Go Convention | Supported | `ResponseWriter`, `HttpRequest` | Handlers receive standard `(ResponseWriter* w, HttpRequest* r)` regardless of HTTP/1.1 or HTTP/2 protocol. |
| **Server RST_STREAM & GOAWAY** | §6.4, §6.8 | Supported | `h2_send_rst_stream`, `h2_send_goaway` | Handles stream resets and emits graceful GOAWAY frames on connection termination. |

