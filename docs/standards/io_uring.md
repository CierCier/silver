# Linux io_uring Asynchronous I/O Compliance

- **Primary Specifications**: [io_uring(7) Linux Programmer's Manual](https://man7.org/linux/man-pages/man7/io_uring.7.html), [io_uring_setup(2)](https://man7.org/linux/man-pages/man2/io_uring_setup.2.html), [io_uring_enter(2)](https://man7.org/linux/man-pages/man2/io_uring_enter.2.html), [io_uring_register(2)](https://man7.org/linux/man-pages/man2/io_uring_register.2.html).
- **Implementation**: [`std/sys/io_uring.ag`](file:///home/cier/Projects/silver/std/sys/io_uring.ag) (`IoUring`, `IoUringSqe`, `IoUringCqe`, `IoUringParams`, `try_io_uring_setup`, `try_io_uring_enter`, `try_io_uring_register`).
- **Test Suite**: [`tests/io_uring_test.ag`](file:///home/cier/Projects/silver/tests/io_uring_test.ag).

## Compliance & Capability Table

| Feature / Primitive | Kernel ABI / Specification | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Submission Queue Entry (`IoUringSqe`)** | `struct io_uring_sqe` (64 bytes) | Supported | `struct IoUringSqe` | Exact 64-byte layout matching Linux kernel ABI. |
| **Completion Queue Entry (`IoUringCqe`)** | `struct io_uring_cqe` (16 bytes) | Supported | `struct IoUringCqe` | Exact 16-byte layout (`user_data: u64`, `res: i32`, `flags: u32`). |
| **Setup Parameters (`IoUringParams`)** | `struct io_uring_params` (120 bytes) | Supported | `struct IoUringParams` | Exact 120-byte layout including nested `IoSqringOffsets` and `IoCqringOffsets`. |
| **Ring Setup & Initialization** | io_uring_setup(2) (syscall 425) | Supported | `IoUring.new(entries)`, `try_io_uring_setup` | Allocates kernel ring and retrieves submission/completion queue offsets. |
| **Shared Ring Memory Mapping** | IORING_FEAT_SINGLE_MMAP (1) | Supported | Automatic in `IoUring.new()` | Detects single mmap feature support to map SQ and CQ rings in a single memory region. |
| **SQE Ring Memory Mapping** | IORING_OFF_SQES (0x10000000) | Supported | Automatic in `IoUring.new()` | Maps submission queue entries array using `MAP_SHARED \| MAP_POPULATE`. |
| **Ring Submission & Reaping** | io_uring_enter(2) (syscall 426) | Supported | `ring.submit()`, `ring.submit_and_wait()` | Flushes queued SQEs to the kernel and reaps CQEs with `IORING_ENTER_GETEVENTS`. |
| **Non-blocking Completion Peek** | Userspace CQ head/tail index | Supported | `ring.peek_cqe(&cqe)` | Inspects completion queue without issuing a syscall. |
| **Completion Acknowledgment** | Userspace CQ head increment | Supported | `ring.cqe_seen(&cqe)` | Advances CQ head pointer to mark event as consumed. |
| **NOP Submission** | IORING_OP_NOP (0) | Supported | `ring.prep_nop(sqe, user_data)` | Submits a no-op operation to verify ring pipeline health. |
| **Asynchronous File Read** | IORING_OP_READ (22) | Supported | `ring.prep_read(sqe, fd, buf, len, off, user_data)` | Submits non-blocking asynchronous file read. |
| **Asynchronous File Write** | IORING_OP_WRITE (23) | Supported | `ring.prep_write(sqe, fd, buf, len, off, user_data)` | Submits non-blocking asynchronous file write. |
| **Asynchronous Socket Accept** | IORING_OP_ACCEPT (13) | Supported | `ring.prep_accept(sqe, fd, addr, addrlen, flags, user_data)` | Non-blocking asynchronous TCP accept. |
| **Asynchronous Socket Connect** | IORING_OP_CONNECT (16) | Supported | `ring.prep_connect(sqe, fd, addr, addrlen, user_data)` | Non-blocking asynchronous TCP connect. |
| **Asynchronous Socket Send** | IORING_OP_SEND (26) | Supported | `ring.prep_send(sqe, fd, buf, len, flags, user_data)` | Zero-syscall TCP socket send. |
| **Asynchronous Socket Receive** | IORING_OP_RECV (27) | Supported | `ring.prep_recv(sqe, fd, buf, len, flags, user_data)` | Zero-syscall TCP socket recv. |
| **Asynchronous Close** | IORING_OP_CLOSE (19) | Supported | `ring.prep_close(sqe, fd, user_data)` | Asynchronously closes file or socket descriptors. |
| **Resource Cleanup (RAII)** | `impl Drop for IoUring` | Supported | `ring.close()` | Automatically munmaps SQ/CQ rings and closes `ring_fd` on scope exit. |
