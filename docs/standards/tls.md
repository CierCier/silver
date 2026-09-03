# TLS & ALPN Standard Compliance

- **Primary Specifications**: [RFC 8446 (The Transport Layer Security (TLS) Protocol Version 1.3)](https://datatracker.ietf.org/doc/html/rfc8446), [RFC 5246 (TLS 1.2)](https://datatracker.ietf.org/doc/html/rfc5246), [RFC 7301 (Transport Layer Security (TLS) Application-Layer Protocol Negotiation Extension)](https://datatracker.ietf.org/doc/html/rfc7301), [RFC 6066 (Server Name Indication)](https://datatracker.ietf.org/doc/html/rfc6066).
- **Implementation**: [`std/net/tls.ag`](file:///home/cier/Projects/silver/std/net/tls.ag) (`TlsStream`, `tls_init`), [`std/net/transport.ag`](file:///home/cier/Projects/silver/std/net/transport.ag) (`StreamIo`, `TlsProvider`).
- **Test Suite**: [`tests/tls_test.ag`](file:///home/cier/Projects/silver/tests/tls_test.ag), [`tests/http2_tls_test.ag`](file:///home/cier/Projects/silver/tests/http2_tls_test.ag).

## Compliance Table

| Feature / Capability | Standard Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **TLS Handshake (1.2 / 1.3)** | RFC 8446 / RFC 5246 | Supported | `TlsStream.connect` | Establishes encrypted channel over an existing TCP socket using OpenSSL native C-ABI. |
| **ALPN Negotiation** | RFC 7301 | Supported | `TlsStream.is_h2()`, `transport_tls_is_h2()` | Advertises `"h2"` and `"http/1.1"` in ClientHello; queries negotiated protocol via `SSL_get0_alpn_selected`. |
| **Server Name Indication (SNI)** | RFC 6066 §3 | Supported | `SSL_CTRL_SET_TLSEXT_HOSTNAME` | Passes virtual hostname during ClientHello for virtual-hosted domains. |
| **Peer Certificate Verification** | RFC 5280 | Supported | `client.set_ca_file(path)` | Verifies remote peer certificates against custom CA bundles or default system trust stores. |
| **Insecure Verification Bypass** | Dev / Testing | Supported | `client.set_insecure_skip_verify()` | Disables peer verification for testing against local self-signed endpoints. |
| **Encrypted Read / Write** | RFC 8446 | Supported | `tls.read(buf, len)`, `tls.write(buf, len)` | Translates to `SSL_read` and `SSL_write` with return error decoding. |
| **Pluggable Freestanding Transport** | Silver Architecture | Supported | `tls_provider_register()` | `std/net/http` uses registered function pointers so plain HTTP binaries never link OpenSSL. |
| **Deterministic Resource Destruction** | Silver Invariants | Supported | `impl Drop for TlsStream` | Automatically calls `SSL_free` and `SSL_CTX_free` on scope exit. |
