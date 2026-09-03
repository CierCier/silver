# Networking Standards Specifications

This directory contains dedicated compliance tables for every networking standard implemented in the Silver standard library (`std/net`).

## Supported Standards Index

| Standard | Description | RFC / Specification | Dedicated Documentation | Status |
|:---|:---|:---|:---|:---:|
| **UDP** | User Datagram Protocol & POSIX Datagram Sockets | RFC 768, POSIX.1-2017, RFC 8200 | [udp.md](file:///home/cier/Projects/silver/docs/standards/udp.md) | **Full** |
| **TCP** | Transmission Control Protocol & Stream Sockets | RFC 9293, POSIX.1-2017 | [tcp.md](file:///home/cier/Projects/silver/docs/standards/tcp.md) | **Full** |
| **epoll & Server I/O** | Event Multiplexing, EventFd, Vectorized I/O & Sendfile | epoll(7), eventfd(2), writev(2), sendfile(2) | [epoll.md](file:///home/cier/Projects/silver/docs/standards/epoll.md) | **Full** |
| **HTTP/1.1** | HTTP Semantics & Framing | RFC 9110, RFC 9112 | [http.md](file:///home/cier/Projects/silver/docs/standards/http.md) | **Full** |
| **HTTP/2** | HTTP/2 Framing, Multiplexing & Flow Control | RFC 9113 | [http2.md](file:///home/cier/Projects/silver/docs/standards/http2.md) | **Full** |
| **HPACK** | Header Compression for HTTP/2 | RFC 7541 | [hpack.md](file:///home/cier/Projects/silver/docs/standards/hpack.md) | **Full** |
| **TLS & ALPN** | Transport Layer Security & Protocol Negotiation | RFC 8446, RFC 7301, RFC 6066 | [tls.md](file:///home/cier/Projects/silver/docs/standards/tls.md) | **Full** |
| **DNS** | Domain Name System Resolution | RFC 1034, RFC 1035 | [dns.md](file:///home/cier/Projects/silver/docs/standards/dns.md) | **Full (A records)** |
| **Cookies** | HTTP State Management Mechanism | RFC 6265 | [cookie.md](file:///home/cier/Projects/silver/docs/standards/cookie.md) | **Full** |
| **WebSocket** | Full-Duplex Bidirectional Framing | RFC 6455 | [websocket.md](file:///home/cier/Projects/silver/docs/standards/websocket.md) | **Full** |
| **Server-Sent Events** | Unidirectional Server Streaming | W3C / WHATWG HTML Living Standard | [sse.md](file:///home/cier/Projects/silver/docs/standards/sse.md) | **Full** |
