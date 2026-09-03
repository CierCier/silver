# TCP Standard Compliance

- **Primary Specifications**: [RFC 9293 (Transmission Control Protocol)](https://datatracker.ietf.org/doc/html/rfc9293), POSIX.1-2017 Socket API.
- **Implementation**: [`std/net/tcp.ag`](file:///home/cier/Projects/silver/std/net/tcp.ag) (`TcpStream`, `TcpListener`), [`std/sys/socket.ag`](file:///home/cier/Projects/silver/std/sys/socket.ag).
- **Test Suite**: [`tests/net_test.ag`](file:///home/cier/Projects/silver/tests/net_test.ag), [`tests/dial_timeout_test.ag`](file:///home/cier/Projects/silver/tests/dial_timeout_test.ag).

## Compliance Table

| Feature / Capability | Standard Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Active Stream Connection** | RFC 9293 §3.4 | Supported | `TcpStream.connect(addr)` | Establishes three-way TCP handshake to destination `SocketAddr`. |
| **Connection Timeouts** | POSIX.1-2017 | Supported | `TcpStream.connect_timeout(addr, sec)` | Uses non-blocking connect paired with `poll(POLLOUT)` to enforce timeouts. |
| **Passive Stream Listening** | POSIX `listen` | Supported | `TcpListener.bind(addr)` | Binds and initiates socket listen state with configurable backlog. |
| **Connection Acceptance** | POSIX `accept4` | Supported | `listener.accept()` | Accepts incoming connections with `SOCK_CLOEXEC` enabled automatically. |
| **Stream Data Transmission** | RFC 9293 §3.5 | Supported | `stream.write(buf, len)` | Emits stream bytes via `sys_write` / `try_sendto`. |
| **Stream Data Reception** | RFC 9293 §3.5 | Supported | `stream.read(buf, len)` | Reads stream bytes via `sys_read` / `try_recvfrom`. |
| **Half-Close / Shutdown** | RFC 9293 §3.5 | Supported | `stream.shutdown(how)` | Supports `ShutdownHow.RD`, `WR`, and `RDWR` via `sys_shutdown`. |
| **Socket Options (Timeouts)** | POSIX `SO_RCVTIMEO` / `SO_SNDTIMEO` | Supported | `stream.sock.set_recv_timeout()`, `set_send_timeout()` | Native kernel socket timeouts. |
| **Address Reuse** | POSIX `SO_REUSEADDR` | Supported | `SocketOption.REUSEADDR` | Configured on listeners to allow rapid port rebinding. |
| **Non-blocking Mode** | POSIX `O_NONBLOCK` | Supported | `tcp_clear_nonblocking()`, `F_SETFL` | Full control over blocking and non-blocking stream execution. |
| **Deterministic Resource Cleanup** | Silver Invariants | Supported | `impl Drop for TcpStream`, `TcpListener` | Automatically closes file descriptors on block exit. |
