# Linux epoll & Server I/O Primitives Compliance

- **Primary Specifications**: [epoll(7) Linux Programmer's Manual](https://man7.org/linux/man-pages/man7/epoll.7.html), [eventfd(2)](https://man7.org/linux/man-pages/man2/eventfd.2.html), [writev(2) / readv(2)](https://man7.org/linux/man-pages/man2/writev.2.html), [sendfile(2)](https://man7.org/linux/man-pages/man2/sendfile.2.html), POSIX.1-2017 Socket API.
- **Implementations**:
  - [`std/sys/epoll.ag`](file:///home/cier/Projects/silver/std/sys/epoll.ag) (`Epoll`, `EpollEvent`, `try_epoll_create1`, `try_epoll_ctl`, `try_epoll_wait`)
  - [`std/sys/eventfd.ag`](file:///home/cier/Projects/silver/std/sys/eventfd.ag) (`EventFd`, `try_eventfd`)
  - [`std/sys/io_vec.ag`](file:///home/cier/Projects/silver/std/sys/io_vec.ag) (`IoVec`, `try_writev`, `try_readv`, `try_sendfile`)
  - [`std/net/tcp.ag`](file:///home/cier/Projects/silver/std/net/tcp.ag) (`bind_reuse_port`, `set_nodelay`, `set_quickack`, `set_cork`, `set_keepalive`, `set_nonblocking`, `writev`, `readv`, `sendfile`)
- **Test Suite**: [`tests/server_raw_test.ag`](file:///home/cier/Projects/silver/tests/server_raw_test.ag).

## Compliance & Capability Table

| Feature / Primitive | Specification / Syscall | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Packed epoll_event Layout** | Linux ABI (12 bytes) | Supported | `#[packed] struct EpollEvent` | Emits 12-byte packed struct (`u32 events`, `u64 data`) with 1-byte alignment, matching Linux kernel ABI. |
| **epoll Instance Creation** | epoll_create1(2) | Supported | `Epoll.new()`, `try_epoll_create1` | Sets `EPOLL_CLOEXEC` automatically. |
| **epoll Registration** | epoll_ctl(EPOLL_CTL_ADD) | Supported | `ep.add(fd, events, data)` | Registers interest in file descriptors with arbitrary 64-bit user data / tokens. |
| **epoll Modification** | epoll_ctl(EPOLL_CTL_MOD) | Supported | `ep.modify(fd, events, data)` | Updates active interest masks dynamically without closing fds. |
| **epoll Deregistration** | epoll_ctl(EPOLL_CTL_DEL) | Supported | `ep.delete(fd)` | Removes interest from epoll monitor loop. |
| **epoll Event Wait / Poll** | epoll_wait(2) | Supported | `ep.poll(events, max, timeout_ms)` | Blocks waiting for ready I/O events, supporting 0ms non-blocking polling and millisecond timeouts. |
| **Edge-Triggered Mode** | EPOLLET (1 << 31) | Supported | `EPOLLET` constant | Enables edge-triggered notifications for high-throughput zero-syscall repeat loops. |
| **One-Shot Notifications** | EPOLLONESHOT (1 << 30) | Supported | `EPOLLONESHOT` constant | Disables event delivery after firing until rearmed with `modify()`. |
| **Inter-Thread EventFd** | eventfd2(2) | Supported | `EventFd.new(val, nonblock)` | 8-byte event counter file descriptor with `EFD_CLOEXEC` and `EFD_NONBLOCK`. |
| **EventFd Notify / Wakeup** | write(2) on eventfd | Supported | `efd.notify(counter)` | Writes 64-bit counter increments to wake up epoll worker/reactor loops. |
| **EventFd Counter Read** | read(2) on eventfd | Supported | `efd.read_val()` | Reads and resets the 64-bit notification counter. |
| **Scatter/Gather Vectorized Write** | writev(2) | Supported | `stream.writev(iov, count)` | Sends multiple distinct buffers (e.g. HTTP headers + body) in a single syscall. |
| **Scatter/Gather Vectorized Read** | readv(2) | Supported | `stream.readv(iov, count)` | Scatters incoming stream bytes into pre-allocated memory buffers. |
| **Zero-Copy File Transfer** | sendfile(2) | Supported | `stream.sendfile(in_fd, &offset, n)` | Transfers file data directly from kernel page cache to TCP socket without userspace copying. |
| **Nagle's Algorithm Disable** | TCP_NODELAY (RFC 896) | Supported | `stream.set_nodelay(bool)` | Disables delayed ACK buffering for microsecond HTTP/RPC request-response cycles. |
| **Quick ACK Delivery** | TCP_QUICKACK | Supported | `stream.set_quickack(bool)` | Instructs Linux TCP stack to immediately send ACK packets without delayed ACK wait. |
| **TCP Corking** | TCP_CORK | Supported | `stream.set_cork(bool)` | Coalesces partial writes into full MTU packets before emitting to the wire. |
| **TCP Keep-Alive Probing** | SO_KEEPALIVE | Supported | `stream.set_keepalive(bool)` | Emits periodic probes to detect dead peers on long-lived connections. |
| **Port Reuse Connection Balancing** | SO_REUSEPORT | Supported | `TcpListener.bind_reuse_port()` | Allows multiple worker processes/threads to bind to the identical IP:Port for kernel-level connection balancing. |
| **Non-blocking Server Sockets** | O_NONBLOCK via fcntl(2) | Supported | `listener.set_nonblocking(bool)` | Configures listeners and streams for non-blocking edge-triggered epoll event loops. |
