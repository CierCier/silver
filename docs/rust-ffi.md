# Rust-backed C ABI

Silver can opt into a small Rust `std` component without changing the
compiler's existing `extern "C"`, `#[link]`, or `#[link_name]` architecture.
The bridge is documented and built in [`ffi/rust/README.md`](../ffi/rust/README.md).

Import the Silver declarations only in programs that need the bridge:

```silver
import std.ffi;

i32 main() {
    // Native layer: Result<T, Error> shapes, str arguments, Drop ownership.
    Result<String, Error> text = rust_ffi_read_to_string("app.config");
    if (text.is_err()) {
        Error err = text.unwrap_err();
        @eprintln("read failed: {}", err.message);
        return 1;
    }
    String config = text.unwrap();
    i64 size = config.len();
    config.drop();

    // Raw ABI is still available: status codes, owned RustFfiBuffer values,
    // borrowed-chunk callbacks, and explicit error slots.
    RustFfiError error = rust_ffi_error_new();
    RustFfiBuffer value = rust_ffi_buffer_new();
    RustFfiSlice key = rust_ffi_text("PATH");
    i32 status = silver_env_get((const RustFfiSlice*)&key, &value, &error);
    return status;
}
```

The native wrappers (`rust_ffi_read_to_string`, `rust_ffi_write_string`,
`rust_ffi_exists`, `rust_ffi_remove_file`, `rust_ffi_create_dir`,
`rust_ffi_remove_dir`, `rust_ffi_env_get`, `rust_ffi_current_dir`,
`rust_ffi_path_join`, and `RustFfiProcess.spawn`/`spawn_with_args`/`join`)
turn ABI statuses into native `Error` values whose messages are static text
owned by the Rust library for the whole process. `RustFfiBuffer`,
`RustFfiError`, and `RustFfiProcess` release their owned state through Silver
`Drop` when the scope ends.

Build `silver-ffi` first and pass its Cargo target directory with `-L` when
linking. The module is an explicit opt-in so programs that do not import it
remain pure Silver binaries. This bridge uses a versioned C layout and
explicit ownership; it is not a Rust ABI or Rust-language mode.
