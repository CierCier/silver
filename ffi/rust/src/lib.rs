//! Stable C ABI entry points used by Silver development tooling.
//!
//! The ABI deliberately uses fixed-width integers, pointer/length pairs, and
//! opaque handles. Rust implementation types never cross this boundary.
//!
//! Threading: every export is thread-safe and re-entrant because they share
//! no mutable global state. Owned values (buffers, error slots, process
//! handles) are single-ownership — one owner at a time may use and free
//! them. `silver_ffi_status_message` returns pointers to process-lifetime
//! static text, so language bindings can keep the messages borrowed without
//! copying or freeing.

use std::ffi::{CStr, c_void};
use std::io::{self, Read};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::{env, mem, ptr, slice, str};

#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

pub const SILVER_FFI_ABI_VERSION: i32 = 1;
pub const SILVER_FFI_OK: i32 = 0;
pub const SILVER_FFI_ERR_INVALID_ARGUMENT: i32 = 1;
pub const SILVER_FFI_ERR_NULL_POINTER: i32 = 2;
pub const SILVER_FFI_ERR_INVALID_UTF8: i32 = 3;
pub const SILVER_FFI_ERR_IO: i32 = 4;
pub const SILVER_FFI_ERR_PROCESS: i32 = 5;
pub const SILVER_FFI_ERR_NOT_FOUND: i32 = 6;
pub const SILVER_FFI_ERR_CALLBACK_STOPPED: i32 = 7;
pub const SILVER_FFI_ERR_INVALID_HANDLE: i32 = 8;
pub const SILVER_FFI_ERR_PANIC: i32 = 9;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct SilverSlice {
    pub data: *const u8,
    pub len: i64,
}

#[repr(C)]
#[derive(Debug)]
pub struct SilverBuffer {
    pub data: *mut u8,
    pub len: i64,
    pub cap: i64,
}

#[repr(C)]
#[derive(Debug)]
pub struct SilverError {
    pub code: i32,
    pub message: SilverBuffer,
}

pub type SilverChunkCallback =
    Option<unsafe extern "C" fn(userdata: *mut c_void, chunk: *const SilverSlice) -> i32>;

#[derive(Debug)]
struct Failure {
    code: i32,
    message: &'static str,
}

impl Failure {
    const fn new(code: i32, message: &'static str) -> Self {
        Self { code, message }
    }
}

struct ProcessHandle {
    child: Child,
    waited: bool,
}

impl SilverBuffer {
    const fn empty() -> Self {
        Self {
            data: ptr::null_mut(),
            len: 0,
            cap: 0,
        }
    }

    fn from_vec(mut bytes: Vec<u8>) -> Self {
        if bytes.is_empty() {
            return Self::empty();
        }

        let len = bytes.len() as i64;
        let cap = bytes.capacity() as i64;
        let data = bytes.as_mut_ptr();
        mem::forget(bytes);
        Self { data, len, cap }
    }
}

impl SilverError {
    const fn empty() -> Self {
        Self {
            code: SILVER_FFI_OK,
            message: SilverBuffer::empty(),
        }
    }
}

fn failure_for_io(error: io::Error) -> Failure {
    let message = match error.kind() {
        io::ErrorKind::NotFound => "path was not found",
        io::ErrorKind::PermissionDenied => "permission denied",
        io::ErrorKind::InvalidInput => "invalid filesystem input",
        io::ErrorKind::AlreadyExists => "path already exists",
        _ => "filesystem operation failed",
    };
    let code = if error.kind() == io::ErrorKind::NotFound {
        SILVER_FFI_ERR_NOT_FOUND
    } else {
        SILVER_FFI_ERR_IO
    };
    Failure::new(code, message)
}

fn failure_for_process(error: io::Error) -> Failure {
    let message = match error.kind() {
        io::ErrorKind::NotFound => "program was not found",
        io::ErrorKind::PermissionDenied => "program permission denied",
        io::ErrorKind::InvalidInput => "invalid process input",
        _ => "process operation failed",
    };
    let code = if error.kind() == io::ErrorKind::NotFound {
        SILVER_FFI_ERR_NOT_FOUND
    } else {
        SILVER_FFI_ERR_PROCESS
    };
    Failure::new(code, message)
}

fn borrowed_bytes<'a>(value: SilverSlice) -> Result<&'a [u8], Failure> {
    if value.len < 0 {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "slice length must not be negative",
        ));
    }
    if value.len == 0 {
        return Ok(&[]);
    }
    if value.data.is_null() {
        return Err(Failure::new(
            SILVER_FFI_ERR_NULL_POINTER,
            "non-empty slice has a null data pointer",
        ));
    }
    let len = usize::try_from(value.len).map_err(|_| {
        Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "slice length does not fit this target",
        )
    })?;
    if len > isize::MAX as usize {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "slice is too large for this target",
        ));
    }
    // The caller owns this memory for the duration of the ABI call.
    Ok(unsafe { slice::from_raw_parts(value.data, len) })
}

fn text_from_slice<'a>(value: SilverSlice) -> Result<&'a str, Failure> {
    str::from_utf8(borrowed_bytes(value)?).map_err(|_| {
        Failure::new(
            SILVER_FFI_ERR_INVALID_UTF8,
            "text arguments must be valid UTF-8",
        )
    })
}

fn path_from_ptr(path: *const SilverSlice) -> Result<PathBuf, Failure> {
    if path.is_null() {
        return Err(Failure::new(
            SILVER_FFI_ERR_NULL_POINTER,
            "path pointer is null",
        ));
    }
    let text = text_from_slice(unsafe { *path })?;
    if text.is_empty() {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "path must not be empty",
        ));
    }
    Ok(PathBuf::from(text))
}

fn env_key_from_ptr(key: *const SilverSlice) -> Result<String, Failure> {
    if key.is_null() {
        return Err(Failure::new(
            SILVER_FFI_ERR_NULL_POINTER,
            "environment key pointer is null",
        ));
    }
    let text = text_from_slice(unsafe { *key })?;
    if text.is_empty() || text.contains(['=', '\0']) {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "environment key is empty or contains a forbidden character",
        ));
    }
    Ok(text.to_owned())
}

fn args_from_ptr(args: *const SilverSlice, count: i64) -> Result<Vec<String>, Failure> {
    if count < 0 {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "argument count must not be negative",
        ));
    }
    if count == 0 {
        return Ok(Vec::new());
    }
    if args.is_null() {
        return Err(Failure::new(
            SILVER_FFI_ERR_NULL_POINTER,
            "non-empty argument list has a null pointer",
        ));
    }
    let count = usize::try_from(count).map_err(|_| {
        Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "argument count does not fit this target",
        )
    })?;
    let max_count = (isize::MAX as usize) / mem::size_of::<SilverSlice>();
    if count > max_count {
        return Err(Failure::new(
            SILVER_FFI_ERR_INVALID_ARGUMENT,
            "argument list is too large for this target",
        ));
    }
    let values = unsafe { slice::from_raw_parts(args, count) };
    values
        .iter()
        .copied()
        .map(|value| text_from_slice(value).map(str::to_owned))
        .collect()
}

fn path_bytes(path: &Path) -> Vec<u8> {
    #[cfg(unix)]
    {
        path.as_os_str().as_bytes().to_vec()
    }
    #[cfg(not(unix))]
    {
        path.to_string_lossy().as_bytes().to_vec()
    }
}

fn environment_bytes(value: std::ffi::OsString) -> Vec<u8> {
    #[cfg(unix)]
    {
        value.as_os_str().as_bytes().to_vec()
    }
    #[cfg(not(unix))]
    {
        value.to_string_lossy().as_bytes().to_vec()
    }
}

fn reset_error(error: *mut SilverError) {
    if !error.is_null() {
        unsafe {
            free_buffer_inner(&mut (*error).message);
            (*error).code = SILVER_FFI_OK;
        }
    }
}

fn set_error_safely(error: *mut SilverError, failure: Failure) {
    if error.is_null() {
        return;
    }
    let _ = catch_unwind(AssertUnwindSafe(|| unsafe {
        (*error).code = failure.code;
        (*error).message = SilverBuffer::from_vec(failure.message.as_bytes().to_vec());
    }));
}

fn ffi_call<F>(error: *mut SilverError, function: F) -> i32
where
    F: FnOnce() -> Result<(), Failure>,
{
    reset_error(error);
    match catch_unwind(AssertUnwindSafe(function)) {
        Ok(Ok(())) => SILVER_FFI_OK,
        Ok(Err(failure)) => {
            let code = failure.code;
            set_error_safely(error, failure);
            code
        }
        Err(_) => {
            set_error_safely(
                error,
                Failure::new(
                    SILVER_FFI_ERR_PANIC,
                    "Rust panic was contained at the FFI boundary",
                ),
            );
            SILVER_FFI_ERR_PANIC
        }
    }
}

fn free_buffer_inner(buffer: *mut SilverBuffer) {
    if buffer.is_null() {
        return;
    }
    let value = unsafe { ptr::read(buffer) };
    unsafe { *buffer = SilverBuffer::empty() };
    if value.data.is_null() || value.len < 0 || value.cap < value.len {
        return;
    }
    let Ok(len) = usize::try_from(value.len) else {
        return;
    };
    let Ok(cap) = usize::try_from(value.cap) else {
        return;
    };
    if cap == 0 || cap > isize::MAX as usize || len > cap {
        return;
    }
    // This pointer may only have come from SilverBuffer returned by this ABI.
    unsafe { drop(Vec::from_raw_parts(value.data, len, cap)) };
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_ffi_abi_version() -> i32 {
    SILVER_FFI_ABI_VERSION
}

#[unsafe(no_mangle)]
/// Initializes a new caller-owned buffer slot.
///
/// # Safety
/// `buffer` must be null or point to writable storage for a new or zeroed
/// `SilverBuffer`. This function does not release an existing buffer.
pub unsafe extern "C" fn silver_ffi_buffer_init(buffer: *mut SilverBuffer) {
    if !buffer.is_null() {
        unsafe { *buffer = SilverBuffer::empty() };
    }
}

#[unsafe(no_mangle)]
/// Releases a buffer returned by this ABI and resets its slot.
///
/// # Safety
/// `buffer` must be null or point to an initialized `SilverBuffer` whose
/// allocation came from this ABI, and it must not be accessed concurrently.
pub unsafe extern "C" fn silver_ffi_buffer_free(buffer: *mut SilverBuffer) {
    let _ = catch_unwind(AssertUnwindSafe(|| free_buffer_inner(buffer)));
}

#[unsafe(no_mangle)]
/// Initializes a new caller-owned error slot.
///
/// # Safety
/// `error` must be null or point to writable storage for a new or zeroed
/// `SilverError`. This function does not release an existing message.
pub unsafe extern "C" fn silver_ffi_error_init(error: *mut SilverError) {
    if !error.is_null() {
        unsafe { *error = SilverError::empty() };
    }
}

#[unsafe(no_mangle)]
/// Clears an error and releases its owned message.
///
/// # Safety
/// `error` must be null or point to an initialized `SilverError` whose message
/// allocation came from this ABI, and it must not be accessed concurrently.
pub unsafe extern "C" fn silver_ffi_error_clear(error: *mut SilverError) {
    let _ = catch_unwind(AssertUnwindSafe(|| {
        if error.is_null() {
            return;
        }
        free_buffer_inner(unsafe { &mut (*error).message });
        unsafe { (*error).code = SILVER_FFI_OK };
    }));
}

/// Process-lifetime message text for every status code. Bindings build
/// native error values from these instead of copying owned buffers.
fn status_text(code: i32) -> &'static CStr {
    match code {
        SILVER_FFI_OK => c"operation succeeded",
        SILVER_FFI_ERR_INVALID_ARGUMENT => c"invalid argument",
        SILVER_FFI_ERR_NULL_POINTER => c"null pointer where data was required",
        SILVER_FFI_ERR_INVALID_UTF8 => c"text is not valid UTF-8",
        SILVER_FFI_ERR_IO => c"filesystem operation failed",
        SILVER_FFI_ERR_PROCESS => c"process operation failed",
        SILVER_FFI_ERR_NOT_FOUND => c"path or variable was not found",
        SILVER_FFI_ERR_CALLBACK_STOPPED => c"callback stopped the operation early",
        SILVER_FFI_ERR_INVALID_HANDLE => c"handle is spent or invalid",
        SILVER_FFI_ERR_PANIC => c"Rust panic was contained at the FFI boundary",
        _ => c"unknown silver-ffi status code",
    }
}

#[unsafe(no_mangle)]
/// Returns a process-lifetime, NUL-terminated message for a status code.
///
/// The returned pointer addresses static text owned by this library: it
/// stays valid for the whole process and must not be freed or mutated.
///
/// # Safety
/// The result is a read-only static pointer; callers must not write through
/// it or free it.
pub unsafe extern "C" fn silver_ffi_status_message(code: i32) -> *const u8 {
    status_text(code).as_ptr().cast()
}

#[unsafe(no_mangle)]
/// Joins two UTF-8 paths into an owned output buffer.
///
/// # Safety
/// Pointer arguments must be null or point to readable/writable storage of
/// the types described by the ABI. Slice data and output/error slots must
/// remain valid for the duration of the call.
pub unsafe extern "C" fn silver_path_join(
    base: *const SilverSlice,
    child: *const SilverSlice,
    out: *mut SilverBuffer,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "path output pointer is null",
            ));
        }
        free_buffer_inner(out);
        let base = path_from_ptr(base)?;
        let child = path_from_ptr(child)?;
        let joined = base.join(child);
        unsafe { *out = SilverBuffer::from_vec(path_bytes(&joined)) };
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Reports whether a path exists.
///
/// # Safety
/// Pointer arguments must be null or point to readable/writable storage of
/// the types described by the ABI, valid for the duration of the call.
pub unsafe extern "C" fn silver_fs_exists(
    path: *const SilverSlice,
    out_exists: *mut i32,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out_exists.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "exists output pointer is null",
            ));
        }
        unsafe { *out_exists = 0 };
        let path = path_from_ptr(path)?;
        match std::fs::metadata(path) {
            Ok(_) => unsafe { *out_exists = 1 },
            Err(error) if error.kind() == io::ErrorKind::NotFound => {}
            Err(error) => return Err(failure_for_io(error)),
        }
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Reads a whole file into an owned output buffer.
///
/// # Safety
/// Pointer arguments must be null or point to readable/writable storage of
/// the types described by the ABI. The output slot must be initialized by the
/// ABI or zeroed and remain valid for the duration of the call.
pub unsafe extern "C" fn silver_fs_read_file(
    path: *const SilverSlice,
    out: *mut SilverBuffer,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "file output pointer is null",
            ));
        }
        free_buffer_inner(out);
        let path = path_from_ptr(path)?;
        let bytes = std::fs::read(path).map_err(failure_for_io)?;
        unsafe { *out = SilverBuffer::from_vec(bytes) };
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Writes a borrowed byte slice to a file.
///
/// # Safety
/// Pointer arguments and borrowed slice data must remain valid and readable
/// for the duration of the call; the error slot must be writable when present.
pub unsafe extern "C" fn silver_fs_write_file(
    path: *const SilverSlice,
    contents: *const SilverSlice,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        let path = path_from_ptr(path)?;
        if contents.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "file contents pointer is null",
            ));
        }
        let contents = borrowed_bytes(unsafe { *contents })?;
        std::fs::write(path, contents).map_err(failure_for_io)?;
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Removes one filesystem path.
///
/// # Safety
/// Pointer arguments and borrowed slice data must remain valid and readable
/// for the duration of the call; the error slot must be writable when present.
pub unsafe extern "C" fn silver_fs_remove_file(
    path: *const SilverSlice,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        let path = path_from_ptr(path)?;
        std::fs::remove_file(path).map_err(failure_for_io)?;
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Creates a directory and every missing parent (idempotent).
///
/// # Safety
/// Pointer arguments and borrowed slice data must remain valid and readable
/// for the duration of the call; the error slot must be writable when present.
pub unsafe extern "C" fn silver_fs_create_dir(
    path: *const SilverSlice,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        let path = path_from_ptr(path)?;
        std::fs::create_dir_all(path).map_err(failure_for_io)?;
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Removes one empty directory.
///
/// # Safety
/// Pointer arguments and borrowed slice data must remain valid and readable
/// for the duration of the call; the error slot must be writable when present.
pub unsafe extern "C" fn silver_fs_remove_dir(
    path: *const SilverSlice,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        let path = path_from_ptr(path)?;
        std::fs::remove_dir(path).map_err(failure_for_io)?;
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Reads a file synchronously and passes borrowed chunks to a callback.
///
/// # Safety
/// Pointer arguments and borrowed slice data must remain valid for the call.
/// The callback must be valid, synchronous, and must not unwind across the C
/// ABI; callback chunks are only valid until the callback returns.
pub unsafe extern "C" fn silver_fs_read_file_callback(
    path: *const SilverSlice,
    callback: SilverChunkCallback,
    userdata: *mut c_void,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        let callback = callback.ok_or(Failure::new(
            SILVER_FFI_ERR_NULL_POINTER,
            "file callback is null",
        ))?;
        let path = path_from_ptr(path)?;
        let mut file = std::fs::File::open(path).map_err(failure_for_io)?;
        let mut chunk = [0_u8; 8192];
        loop {
            let count = file.read(&mut chunk).map_err(failure_for_io)?;
            if count == 0 {
                break;
            }
            let borrowed = SilverSlice {
                data: chunk.as_ptr(),
                len: count as i64,
            };
            let callback_code = unsafe { callback(userdata, &borrowed) };
            if callback_code != 0 {
                return Err(Failure::new(
                    SILVER_FFI_ERR_CALLBACK_STOPPED,
                    "file callback stopped the read",
                ));
            }
        }
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Looks up an environment variable into an owned output buffer.
///
/// # Safety
/// Pointer arguments and borrowed key data must remain valid for the duration
/// of the call. The output slot must be initialized by the ABI or zeroed.
pub unsafe extern "C" fn silver_env_get(
    key: *const SilverSlice,
    out: *mut SilverBuffer,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "environment output pointer is null",
            ));
        }
        free_buffer_inner(out);
        let key = env_key_from_ptr(key)?;
        let Some(value) = env::var_os(key) else {
            return Err(Failure::new(
                SILVER_FFI_ERR_NOT_FOUND,
                "environment variable was not found",
            ));
        };
        unsafe { *out = SilverBuffer::from_vec(environment_bytes(value)) };
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Returns the current directory in an owned output buffer.
///
/// # Safety
/// `out` and the optional error slot must be null or point to valid writable
/// storage for the duration of the call; `out` must be initialized by the ABI
/// or zeroed.
pub unsafe extern "C" fn silver_env_current_dir(
    out: *mut SilverBuffer,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "current-directory output pointer is null",
            ));
        }
        free_buffer_inner(out);
        unsafe { *out = SilverBuffer::empty() };
        let current = env::current_dir().map_err(failure_for_io)?;
        unsafe { *out = SilverBuffer::from_vec(path_bytes(&current)) };
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Spawns a process and returns an opaque owned handle.
///
/// # Safety
/// Pointer arguments and borrowed argument data must remain valid for the
/// duration of the call. `out_handle` must point to a writable null slot.
pub unsafe extern "C" fn silver_process_spawn(
    program: *const SilverSlice,
    args: *const SilverSlice,
    arg_count: i64,
    out_handle: *mut *mut c_void,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if out_handle.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "process handle output pointer is null",
            ));
        }
        if unsafe { !(*out_handle).is_null() } {
            return Err(Failure::new(
                SILVER_FFI_ERR_INVALID_ARGUMENT,
                "process handle output must be null",
            ));
        }
        unsafe { *out_handle = ptr::null_mut() };
        let program = text_from_slice(unsafe {
            if program.is_null() {
                return Err(Failure::new(
                    SILVER_FFI_ERR_NULL_POINTER,
                    "program pointer is null",
                ));
            }
            *program
        })?;
        if program.is_empty() {
            return Err(Failure::new(
                SILVER_FFI_ERR_INVALID_ARGUMENT,
                "program must not be empty",
            ));
        }
        let args = args_from_ptr(args, arg_count)?;
        let child = Command::new(program)
            .args(args)
            .spawn()
            .map_err(failure_for_process)?;
        let handle = Box::new(ProcessHandle {
            child,
            waited: false,
        });
        unsafe { *out_handle = Box::into_raw(handle).cast() };
        Ok(())
    })
}

#[unsafe(no_mangle)]
/// Waits once for a process handle and writes its exit code.
///
/// # Safety
/// `handle` must be a live handle returned by this ABI, and output/error
/// pointers must be valid writable storage when present.
pub unsafe extern "C" fn silver_process_wait(
    handle: *mut c_void,
    out_exit_code: *mut i32,
    error: *mut SilverError,
) -> i32 {
    ffi_call(error, || {
        if handle.is_null() || out_exit_code.is_null() {
            return Err(Failure::new(
                SILVER_FFI_ERR_NULL_POINTER,
                "process handle or exit-code output is null",
            ));
        }
        unsafe { *out_exit_code = -1 };
        let process = unsafe { &mut *handle.cast::<ProcessHandle>() };
        if process.waited {
            return Err(Failure::new(
                SILVER_FFI_ERR_INVALID_HANDLE,
                "process has already been waited",
            ));
        }
        let status = process.child.wait().map_err(failure_for_process)?;
        process.waited = true;
        let code = status.code().unwrap_or_else(|| {
            #[cfg(unix)]
            {
                status.signal().map(|signal| 128 + signal).unwrap_or(-1)
            }
            #[cfg(not(unix))]
            {
                -1
            }
        });
        unsafe { *out_exit_code = code };
        Ok(())
    })
}

fn free_process_inner(handle: *mut *mut c_void) {
    if handle.is_null() {
        return;
    }
    let raw = unsafe { *handle };
    unsafe { *handle = ptr::null_mut() };
    if raw.is_null() {
        return;
    }
    let mut process = unsafe { Box::from_raw(raw.cast::<ProcessHandle>()) };
    if !process.waited {
        let _ = process.child.kill();
        let _ = process.child.wait();
        process.waited = true;
    }
}

#[unsafe(no_mangle)]
/// Frees an opaque process handle and resets its slot.
///
/// # Safety
/// `handle` must be null or point to a slot containing null or a live handle
/// returned by this ABI, and the slot must not be accessed concurrently.
pub unsafe extern "C" fn silver_process_free(handle: *mut *mut c_void) {
    let _ = catch_unwind(AssertUnwindSafe(|| free_process_inner(handle)));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    fn slice(text: &str) -> SilverSlice {
        SilverSlice {
            data: text.as_ptr(),
            len: text.len() as i64,
        }
    }

    fn buffer_text(buffer: &SilverBuffer) -> &[u8] {
        if buffer.len == 0 {
            return &[];
        }
        unsafe { std::slice::from_raw_parts(buffer.data, buffer.len as usize) }
    }

    static CALLBACK_RESULT: AtomicI32 = AtomicI32::new(0);

    /// # Safety
    /// The test passes a valid borrowed chunk pointer for the callback's
    /// duration.
    unsafe extern "C" fn collect_chunk(_userdata: *mut c_void, chunk: *const SilverSlice) -> i32 {
        let chunk = unsafe { &*chunk };
        CALLBACK_RESULT.fetch_add(chunk.len as i32, Ordering::Relaxed);
        0
    }

    /// # Safety
    /// The test invokes this callback only with ABI-valid arguments.
    unsafe extern "C" fn stop_after_first(
        _userdata: *mut c_void,
        _chunk: *const SilverSlice,
    ) -> i32 {
        1
    }

    #[test]
    fn panic_is_converted_to_status() {
        unsafe {
            let mut error = SilverError::empty();
            let status = ffi_call(&mut error, || -> Result<(), Failure> {
                panic!("test panic")
            });
            assert_eq!(status, SILVER_FFI_ERR_PANIC);
            assert_eq!(error.code, SILVER_FFI_ERR_PANIC);
            silver_ffi_error_clear(&mut error);
        }
    }

    #[test]
    fn c_layout_is_explicit() {
        assert_eq!(std::mem::size_of::<SilverSlice>(), 16);
        assert_eq!(std::mem::size_of::<SilverBuffer>(), 24);
        assert_eq!(std::mem::size_of::<SilverError>(), 32);
        assert_eq!(std::mem::align_of::<SilverSlice>(), 8);
    }

    #[test]
    fn borrowed_slice_validation_is_explicit() {
        let null = SilverSlice {
            data: ptr::null(),
            len: 1,
        };
        let negative = SilverSlice {
            data: b"x".as_ptr(),
            len: -1,
        };
        assert_eq!(
            borrowed_bytes(null).unwrap_err().code,
            SILVER_FFI_ERR_NULL_POINTER
        );
        assert_eq!(
            borrowed_bytes(negative).unwrap_err().code,
            SILVER_FFI_ERR_INVALID_ARGUMENT
        );
        assert_eq!(
            borrowed_bytes(SilverSlice {
                data: ptr::null(),
                len: 0
            })
            .unwrap(),
            &[]
        );
    }

    #[test]
    fn buffers_round_trip_through_the_explicit_free_function() {
        unsafe {
            let mut buffer = SilverBuffer::from_vec(b"owned bytes".to_vec());
            assert_eq!(buffer_text(&buffer), b"owned bytes");
            silver_ffi_buffer_free(&mut buffer);
            assert!(buffer.data.is_null());
            assert_eq!(buffer.len, 0);
            assert_eq!(buffer.cap, 0);
            silver_ffi_buffer_free(&mut buffer);
        }
    }

    #[test]
    fn filesystem_and_callback_exports_use_borrowed_inputs() {
        unsafe {
            let path = std::env::temp_dir().join(format!(
                "silver-ffi-test-{}-{}",
                std::process::id(),
                std::thread::current().name().unwrap_or("thread")
            ));
            let path_text = path.to_string_lossy().into_owned();
            let path_slice = slice(&path_text);
            let contents = slice("callback content");
            let mut error = SilverError::empty();

            assert_eq!(
                silver_fs_write_file(&path_slice, &contents, &mut error),
                SILVER_FFI_OK
            );

            let mut output = SilverBuffer::empty();
            assert_eq!(
                silver_fs_read_file(&path_slice, &mut output, &mut error),
                SILVER_FFI_OK
            );
            assert_eq!(buffer_text(&output), b"callback content");
            silver_ffi_buffer_free(&mut output);

            CALLBACK_RESULT.store(0, Ordering::Relaxed);
            assert_eq!(
                silver_fs_read_file_callback(
                    &path_slice,
                    Some(collect_chunk),
                    ptr::null_mut(),
                    &mut error,
                ),
                SILVER_FFI_OK
            );
            assert_eq!(CALLBACK_RESULT.load(Ordering::Relaxed), 16);

            assert_eq!(
                silver_fs_read_file_callback(
                    &path_slice,
                    Some(stop_after_first),
                    ptr::null_mut(),
                    &mut error,
                ),
                SILVER_FFI_ERR_CALLBACK_STOPPED
            );
            assert_eq!(
                buffer_text(&error.message),
                b"file callback stopped the read"
            );

            assert_eq!(
                silver_fs_remove_file(&path_slice, &mut error),
                SILVER_FFI_OK
            );
            silver_ffi_error_clear(&mut error);
        }
    }

    #[test]
    fn path_and_environment_exports_return_owned_buffers() {
        unsafe {
            let base = slice("/tmp");
            let child = slice("silver-ffi");
            let mut joined = SilverBuffer::empty();
            let mut error = SilverError::empty();
            assert_eq!(
                silver_path_join(&base, &child, &mut joined, &mut error),
                SILVER_FFI_OK
            );
            assert_eq!(buffer_text(&joined), b"/tmp/silver-ffi");
            silver_ffi_buffer_free(&mut joined);

            let key = slice("PATH");
            assert_eq!(silver_env_get(&key, &mut joined, &mut error), SILVER_FFI_OK);
            assert!(joined.len > 0);
            silver_ffi_buffer_free(&mut joined);

            assert_eq!(
                silver_env_current_dir(&mut joined, &mut error),
                SILVER_FFI_OK
            );
            assert!(joined.len > 0);
            assert_eq!(
                silver_env_current_dir(&mut joined, &mut error),
                SILVER_FFI_OK
            );
            assert!(joined.len > 0);
            silver_ffi_buffer_free(&mut joined);
            silver_ffi_error_clear(&mut error);
        }
    }

    #[cfg(unix)]
    #[test]
    fn process_handles_are_wait_once_and_explicitly_freed() {
        unsafe {
            let program = slice("/bin/sh");
            let arg0 = slice("-c");
            let arg1 = slice("exit 7");
            let args = [arg0, arg1];
            let mut handle = ptr::null_mut();
            let mut exit_code = -1;
            let mut error = SilverError::empty();

            assert_eq!(
                silver_process_spawn(
                    &program,
                    args.as_ptr(),
                    args.len() as i64,
                    &mut handle,
                    &mut error,
                ),
                SILVER_FFI_OK
            );
            assert!(!handle.is_null());
            assert_eq!(
                silver_process_wait(handle, &mut exit_code, &mut error),
                SILVER_FFI_OK
            );
            assert_eq!(exit_code, 7);
            assert_eq!(
                silver_process_wait(handle, &mut exit_code, &mut error),
                SILVER_FFI_ERR_INVALID_HANDLE
            );
            silver_process_free(&mut handle);
            assert!(handle.is_null());
            silver_ffi_error_clear(&mut error);
        }
    }

    #[cfg(unix)]
    #[test]
    fn process_spawn_rejects_a_live_output_handle() {
        unsafe {
            let program = slice("/bin/true");
            let mut handle = ptr::null_mut();
            let mut exit_code = -1;
            let mut error = SilverError::empty();

            assert_eq!(
                silver_process_spawn(&program, ptr::null(), 0, &mut handle, &mut error),
                SILVER_FFI_OK
            );
            let original = handle;
            assert_eq!(
                silver_process_spawn(&program, ptr::null(), 0, &mut handle, &mut error),
                SILVER_FFI_ERR_INVALID_ARGUMENT
            );
            assert_eq!(handle, original);
            assert_eq!(
                silver_process_wait(handle, &mut exit_code, &mut error),
                SILVER_FFI_OK
            );
            silver_process_free(&mut handle);
            silver_ffi_error_clear(&mut error);
        }
    }

    #[test]
    fn status_messages_are_static_and_nul_terminated() {
        unsafe {
            let codes = [
                SILVER_FFI_OK,
                SILVER_FFI_ERR_INVALID_ARGUMENT,
                SILVER_FFI_ERR_NULL_POINTER,
                SILVER_FFI_ERR_INVALID_UTF8,
                SILVER_FFI_ERR_IO,
                SILVER_FFI_ERR_PROCESS,
                SILVER_FFI_ERR_NOT_FOUND,
                SILVER_FFI_ERR_CALLBACK_STOPPED,
                SILVER_FFI_ERR_INVALID_HANDLE,
                SILVER_FFI_ERR_PANIC,
                i32::MAX,
            ];
            for code in codes {
                let text = silver_ffi_status_message(code);
                assert!(!text.is_null());
                let message = CStr::from_ptr(text.cast::<std::ffi::c_char>());
                assert!(!message.to_bytes().is_empty());
            }
        }
    }

    #[test]
    fn directories_are_created_idempotently_and_removed() {
        unsafe {
            let root = std::env::temp_dir().join(format!("silver-ffi-dir-{}", std::process::id()));
            let nested = root.join("a/b");
            let path_text = nested.to_string_lossy().into_owned();
            let path = slice(&path_text);
            let mut error = SilverError::empty();

            assert_eq!(silver_fs_create_dir(&path, &mut error), SILVER_FFI_OK);
            assert_eq!(silver_fs_create_dir(&path, &mut error), SILVER_FFI_OK);
            let mut exists = 0;
            assert_eq!(
                silver_fs_exists(&path, &mut exists, &mut error),
                SILVER_FFI_OK
            );
            assert_eq!(exists, 1);
            assert_eq!(silver_fs_remove_dir(&path, &mut error), SILVER_FFI_OK);
            assert_eq!(
                silver_fs_exists(&path, &mut exists, &mut error),
                SILVER_FFI_OK
            );
            assert_eq!(exists, 0);
            let _ = std::fs::remove_dir_all(&root);
            silver_ffi_error_clear(&mut error);
        }
    }

    #[test]
    fn null_error_slots_are_accepted_by_fallible_exports() {
        unsafe {
            let path = slice("silver-ffi-null-slot.txt");
            let contents = slice("data");
            assert_eq!(
                silver_fs_write_file(&path, &contents, ptr::null_mut()),
                SILVER_FFI_OK
            );
            let mut buffer = SilverBuffer::empty();
            assert_eq!(
                silver_fs_read_file(&path, &mut buffer, ptr::null_mut()),
                SILVER_FFI_OK
            );
            silver_ffi_buffer_free(&mut buffer);
            assert_eq!(silver_fs_remove_file(&path, ptr::null_mut()), SILVER_FFI_OK);
            assert!(!silver_ffi_status_message(SILVER_FFI_ERR_IO).is_null());
        }
    }
}
