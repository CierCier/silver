# Silver Rust FFI bridge

`silver-ffi` is the first stable Rust-backed C ABI for Silver tooling. It is
deliberately a C ABI boundary: the public interface does **not** promise Rust
ABI compatibility and does not expose Rust generics, `Vec`, `String`,
`Result`, trait objects, or Rust-owned layout.

## Build and link

From the repository root:

```sh
cargo build -p silver-ffi
cargo run -p agc -- examples/rust_ffi_tool.ag \
  -L target/debug -o /tmp/rust_ffi_tool
LD_LIBRARY_PATH="$PWD/target/debug" /tmp/rust_ffi_tool
```

The crate produces `libsilver_ffi.a`, `libsilver_ffi.so`, and an `rlib` under
Cargo's target directory. `std/ffi/rust_abi.ag` opts into the bridge with
`#[link(silver_ffi)]`; the `-L` directory is therefore required when linking a
Silver executable. The compiler's existing native-library propagation and
`#[link_name]` support are used unchanged.

The C header is at [`include/silver_ffi.h`](include/silver_ffi.h). Programs
written in C may include it directly. The Silver declarations, small RAII
wrappers, and the native-ergonomic layer are in
[`../../std/ffi.ag`](../../std/ffi.ag) (re-exporting
[`../../std/ffi/rust_abi.ag`](../../std/ffi/rust_abi.ag) and
[`../../std/ffi/rust.ag`](../../std/ffi/rust.ag)), and the development-tool
example is [`../../examples/rust_ffi_tool.ag`](../../examples/rust_ffi_tool.ag).

## Native Silver layer

`std/ffi/rust.ag` provides a second layer above the raw ABI that matches the
shapes of the rest of the stdlib: `Result<T, Error>` returns, owned `String`
values, `str` arguments, and `Drop`-managed process handles:

```silver
import std.ffi;

Result<String, Error> config = rust_ffi_read_to_string("app.config");

Vec<str> args = Vec<str>.new();
args.push("-c");
args.push("exit 7");
Result<RustFfiProcess, Error> spawned =
    RustFfiProcess.spawn_with_args("/bin/sh", &args);
RustFfiProcess child = spawned.unwrap();
Result<i32, Error> exited = child.join();
i32 code = exited.unwrap();
```

The wrappers pass a null error slot to the ABI (the status code carries the
failure) and build native `Error` values with
`silver_ffi_status_message`, whose static, process-lifetime text satisfies the
borrowed-`str` lifetime rule of `std/error.ag` without copying. The raw ABI
remains available through `std.ffi.rust_abi` for streaming callbacks and for
callers that want the exact Rust-side message buffer. The join method is named `join`
because `wait` is a Silver keyword (Task joining).

## ABI contract

The ABI version is returned by `silver_ffi_abi_version()` and is currently
`1`. `SilverSlice` is a borrowed pointer plus signed 64-bit byte length;
non-empty slices must have a non-null pointer and all text inputs must be
valid UTF-8. The producer retains borrowed input only for the duration of the
call. Empty slices may use a null pointer.

`SilverBuffer` is Rust-owned output. Initialize a new slot with
`silver_ffi_buffer_init` (or zero initialization), and release every returned
buffer with `silver_ffi_buffer_free`. Do not call `free`, copy a live buffer
struct, or retain its pointer after freeing it. Output calls release a prior
buffer in the output slot before replacing it, so the slot must contain either
zero initialization or a buffer previously returned by this ABI.

`SilverError` contains a status code and an owned `SilverBuffer` message.
Initialize it before first use, pass it to fallible calls, and clear it with
`silver_ffi_error_clear` when it is no longer needed. A call clears/replaces a
previous message. Passing a null error pointer is allowed when only the status
code is needed.

The fixed status values are:

| Code | Meaning |
| --- | --- |
| `0` | success |
| `1` | invalid argument |
| `2` | required pointer was null |
| `3` | invalid UTF-8 |
| `4` | filesystem I/O failure |
| `5` | process failure |
| `6` | path or environment variable not found |
| `7` | callback requested that a read stop |
| `8` | a process handle was already waited or otherwise invalid |
| `9` | a Rust panic was contained at the boundary |

`silver_ffi_status_message` maps every status code to NUL-terminated,
process-lifetime static text. Bindings use it to build native error values
without copying; the returned pointer must not be freed or mutated.

## Threading

Every export is thread-safe and re-entrant: they share no mutable global
state, so calls from multiple threads may run concurrently. Owned values
(`SilverBuffer`, `SilverError`, process handles) are single-ownership — one
owner at a time may use, replace, or free them. The exports never spawn
threads internally.

The callback API is synchronous. Each chunk is borrowed until the callback
returns and must not be retained. `userdata` is passed through unchanged and
is the caller's responsibility to keep valid for the complete call. A
non-zero callback result maps to `SILVER_FFI_ERR_CALLBACK_STOPPED`.

The callback signature intentionally contains only scalar and pointer values.
Silver function-pointer types do not carry an independent ABI annotation, so
aggregate callback arguments are outside this bridge's contract.

Process handles are opaque `void *` values. A successful spawn transfers
ownership of the handle to the caller. `silver_process_wait` consumes the
child wait operation once; call `silver_process_free` with the address of the
handle afterward. Freeing an un-waited handle terminates and reaps its child,
then nulls the caller's handle. The Silver `RustFfiProcess` wrapper applies
that cleanup through `Drop`.

## Panic and error containment

Rust implementation entry points catch unwinding panics and return
`SILVER_FFI_ERR_PANIC` rather than allowing a Rust unwind to cross into
Silver/C. This does not make arbitrary foreign callbacks unwind-safe: a Rust
callback supplied by a C caller must not panic across an `extern "C"` function
pointer. Builds that override Cargo's panic strategy must retain unwinding for
the status-code containment path; aborting panics cannot be converted to an
error.

## Implemented operations

- path joining;
- file existence, whole-file read/write/remove, directory creation
  (with parents, idempotent) and empty-directory removal, and synchronous
  chunk reads;
- environment lookup and current-directory lookup;
- process spawn, one-shot wait, and owned cleanup;
- static status-code messages for building native error values;
- explicit buffer/error initialization and release.

The implementation uses `std::fs`, `std::env`, and `std::process`, so it is a
Rust `std` component rather than part of Silver's freestanding runtime.

## Platform assumptions and limitations

The bridge and integration tests are developed and verified on Linux x86-64
with the system linker and Rust stable. The Rust implementation uses
cross-platform `std` APIs, but Windows and non-Unix targets are not covered by
the current integration test; non-Unix path/environment output is converted
through lossy UTF-8. The example and Silver integration test use `/bin/true`
and should be adapted for other operating systems. The callback interop is
likewise only verified on this target; do not infer a portable Silver
function-pointer ABI for aggregate values from the scalar callback test.

This is intentionally a small foundation, not a complete operating-system
API. It does not yet expose directory iteration, process stdio pipes,
environment mutation, rich OS error details, asynchronous callbacks, or a
directory/file handle object model. The C layout and ownership rules are the
stable surface; implementation internals and Rust crate type choices may
change behind that surface.
