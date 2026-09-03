# UDP Standard Compliance

- **Primary Specifications**: [RFC 768 (User Datagram Protocol)](https://datatracker.ietf.org/doc/html/rfc768), POSIX.1-2017 Socket API, [RFC 8200 (IPv6 Specification)](https://datatracker.ietf.org/doc/html/rfc8200).
- **Implementation**: [`std/net/socket.ag`](file:///home/cier/Projects/silver/std/net/socket.ag) (`UdpSocket`) and [`std/sys/socket.ag`](file:///home/cier/Projects/silver/std/sys/socket.ag).
- **Test Suite**: [`tests/net_udp_test.ag`](file:///home/cier/Projects/silver/tests/net_udp_test.ag), [`tests/udp_test.ag`](file:///home/cier/Projects/silver/tests/udp_test.ag).

## Compliance Table

| Feature / Capability | Standard Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **IPv4 Datagram Sockets** | RFC 768 | Supported | `UdpSocket.new()` | Creates unbound `AF_INET` `SOCK_DGRAM` with `SOCK_CLOEXEC` enabled by default. |
| **IPv6 Datagram Sockets** | RFC 8200 | Supported | `UdpSocket.new_v6()` | Creates unbound `AF_INET6` `SOCK_DGRAM` with `SOCK_CLOEXEC`. |
| **Socket Address Binding** | POSIX `bind` | Supported | `sock.bind(&addr)` | Supports `SocketAddr.V4`, `SocketAddr.V6`, and `SocketAddr.Unix`. Supports ephemeral port `0` for dynamic port allocation. |
| **Bound Address Discovery** | POSIX `getsockname` | Supported | `sock.local_addr()`, `sock.local_port()` | Extracts the bound port and local address into `SocketAddr` across IPv4 and IPv6. |
| **Connected Peer Discovery** | POSIX `getpeername` | Supported | `sock.peer_addr()` | Returns the remote connected peer address as a `Result<SocketAddr, Error>`. |
| **Connected UDP Sockets** | POSIX `connect` | Supported | `sock.connect(&addr)` | Associates the socket with a specific remote peer. Filters inbound packets from other origins. |
| **Connected Datagram Send** | POSIX `send` | Supported | `sock.send(buf, len)` | Emits a datagram to the connected peer without providing a destination address on each call. |
| **Connected Datagram Recv** | POSIX `recv` | Supported | `sock.recv(buf, len)` | Reads a datagram payload from the connected peer without capturing source address metadata. |
| **Connectionless Datagram Send** | POSIX `sendto` | Supported | `sock.send_to(buf, len, &dst)` | Sends a datagram to target destination `SocketAddr` (`V4`, `V6`, or `Unix`). |
| **Connectionless Datagram Recv** | POSIX `recvfrom` | Supported | `sock.recv_from(buf, len, &src)` | Uses 128-byte `sockaddr_storage` buffer to capture sender `SocketAddr` (`V4`, `V6`, or `Unix`). |
| **Receive Timeout** | POSIX `SO_RCVTIMEO` | Supported | `sock.set_recv_timeout(sec)` | Configures blocking receive duration using kernel socket options via `setsockopt`. |
| **Send Timeout** | POSIX `SO_SNDTIMEO` | Supported | `sock.set_send_timeout(sec)` | Configures blocking send duration via `setsockopt`. |
| **UDP Broadcast** | POSIX `SO_BROADCAST` | Supported | `sock.set_broadcast(on)` | Enables or disables permission to transmit datagrams to broadcast addresses. |
| **Non-blocking Mode** | POSIX `O_NONBLOCK` | Supported | `sock.set_nonblocking(on)` | Updates file descriptor flags via `fcntl(F_GETFL/F_SETFL)` for event loops and polling. |
| **Deterministic RAII Destruction** | Silver Invariants | Supported | `impl Drop for UdpSocket` | Closes the underlying file descriptor deterministically upon scope exit. |
