# Networking Standards Specifications

This directory contains dedicated compliance tables for every networking standard implemented in the Silver standard library (`std/net`).

## Supported Standards Index

| Standard | Description | RFC / Specification | Dedicated Documentation | Status |
|:---|:---|:---|:---|:---:|
| **UDP** | User Datagram Protocol & POSIX Datagram Sockets | RFC 768, POSIX.1-2017, RFC 8200 | [udp.md](udp.md) | **Full** |
| **TCP** | Transmission Control Protocol & Stream Sockets | RFC 9293, POSIX.1-2017 | [tcp.md](tcp.md) | **Full** |
| **epoll & Server I/O** | Event Multiplexing, EventFd, Vectorized I/O & Sendfile | epoll(7), eventfd(2), writev(2), sendfile(2) | [epoll.md](epoll.md) | **Full** |
| **io_uring** | Asynchronous I/O Framework, SQ/CQ Ring Buffer Architecture | io_uring(7), io_uring_setup(2), io_uring_enter(2) | [io_uring.md](io_uring.md) | **Full** |
| **HTTP/1.1** | HTTP Semantics, Framing & Go-like Server Engine | RFC 9110, RFC 9112 | [http.md](http.md) | **Full (Client & Server)** |
| **HTTP/2** | HTTP/2 Framing, Flow Control & Native Server Engine | RFC 9113 | [http2.md](http2.md) | **Full (Client & Server)** |
| **HPACK** | Header Compression for HTTP/2 | RFC 7541 | [hpack.md](hpack.md) | **Full** |
| **TLS & ALPN** | Transport Layer Security & Protocol Negotiation | RFC 8446, RFC 7301, RFC 6066 | [tls.md](tls.md) | **Full** |
| **DNS** | Domain Name System Resolution | RFC 1034, RFC 1035 | [dns.md](dns.md) | **Full (A records)** |
| **Cookies** | HTTP State Management Mechanism | RFC 6265 | [cookie.md](cookie.md) | **Full** |
| **WebSocket** | Full-Duplex Bidirectional Framing | RFC 6455 | [websocket.md](websocket.md) | **Full** |
| **Server-Sent Events** | Unidirectional Server Streaming | W3C / WHATWG HTML Living Standard | [sse.md](sse.md) | **Full** |
| **UUID** | Universally Unique Identifier (v4, v7, Nil, Max) | RFC 9562, RFC 4122 | [uuid.md](uuid.md) | **Full** |

## Editor protocols

| Protocol | Implementation | Support reference |
| --- | --- | --- |
| Language Server Protocol 3.18 | Native Silver `aglsp` | [Method-by-method support ledger](../lsp-protocol.md), including supported, conditional, partial and open methods, limits and verification. See [setup](../language-server.md) for installation. |
