# WebSocket Protocol Standard Compliance

- **Primary Specifications**: [RFC 6455 (The WebSocket Protocol)](https://datatracker.ietf.org/doc/html/rfc6455).
- **Implementation**: [`std/net/websocket.ag`](file:///home/cier/Projects/silver/std/net/websocket.ag) (`WebSocket`, `WebSocketFrame`).
- **Test Suite**: [`tests/websocket_test.ag`](file:///home/cier/Projects/silver/tests/websocket_test.ag).

## Compliance Table

| Feature / Capability | RFC 6455 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Opening Handshake** | §4 | Supported | `WebSocket.connect(url)` | Emits HTTP/1.1 Upgrade request with 16-byte base64 nonce in `Sec-WebSocket-Key`. |
| **Accept Key Verification** | §4.2.2 | Supported | `ws_verify_accept` | Verifies peer `Sec-WebSocket-Accept` using SHA-1 hash concatenated with `258EAFA5-E914-47DA-95CA-C5AB0DC85B11` and base64 encoded. |
| **Client Frame Masking** | §5.3 | Supported | `ws_apply_mask` | All client-to-server frames are masked with a 4-byte random masking key. |
| **Text Frame Handling (0x1)** | §5.6 | Supported | `ws.send_text(str)`, `ws.recv()` | Encodes and decodes UTF-8 text messages. |
| **Binary Frame Handling (0x2)** | §5.6 | Supported | `ws.send_binary(buf, len)`, `ws.recv()` | Transmits raw binary octet payloads. |
| **Ping Heartbeat (0x9)** | §5.5.2 | Supported | `ws.ping()` | Transmits Ping frame; reads and handles incoming Ping frames. |
| **Pong Response (0xA)** | §5.5.3 | Supported | `ws.pong()` | Automatically returns Pong frame carrying identical payload upon receiving a Ping. |
| **Closing Handshake (0x8)** | §5.5.1 | Supported | `ws.close()` | Sends close frame with status 1000 (normal closure) and awaits peer close response. |
| **TLS / WSS Transport** | §3 | Supported | `wss://` URLs | Runs over TLS encrypted streams seamlessly when connecting to secure endpoints. |
