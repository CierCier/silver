use std::alloc::Layout;
use std::ffi::CStr;
use std::ptr;

/// Minimal C-ABI surface for LLVM-generated code to call into.
///
/// Keep this module small and stable: it becomes part of the language/runtime ABI.

/// C-friendly borrowed slice.
///
/// Used to pass byte/string data without requiring NUL-termination.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct SilverSlice {
    pub ptr: *const u8,
    pub len: usize,
}

impl SilverSlice {
    fn is_null(self) -> bool {
        self.ptr.is_null()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_i64(v: i64) {
    println!("{v}");
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_u64(v: u64) {
    println!("{v}");
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_bool(v: u8) {
    println!("{}", v != 0);
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_f64(v: f64) {
    println!("{v}");
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_cstr(ptr: *const i8) {
    if ptr.is_null() {
        println!("<null>");
        return;
    }

    // Safety: caller promises ptr is NUL-terminated.
    let s = unsafe { CStr::from_ptr(ptr) };
    match s.to_str() {
        Ok(text) => println!("{text}"),
        Err(_) => println!("<non-utf8>"),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_print_bytes(s: SilverSlice) {
    if s.is_null() {
        println!("<null>");
        return;
    }

    // Safety: caller promises the pointer is valid for `len` bytes.
    let bytes = unsafe { std::slice::from_raw_parts(s.ptr, s.len) };
    match std::str::from_utf8(bytes) {
        Ok(text) => print!("{text}"),
        Err(_) => print!("<non-utf8>"),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_abort() {
    std::process::abort();
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_abort_cstr(ptr: *const i8) {
    if !ptr.is_null() {
        // Safety: caller promises ptr is NUL-terminated.
        let s = unsafe { CStr::from_ptr(ptr) };
        if let Ok(text) = s.to_str() {
            eprintln!("{text}");
        }
    }
    std::process::abort();
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_strlen(ptr: *const i8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // Safety: caller promises ptr is NUL-terminated.
    unsafe { CStr::from_ptr(ptr) }.to_bytes().len()
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_alloc(size: usize, align: usize) -> *mut u8 {
    if size == 0 {
        // Match typical C-like behavior: a non-null unique-ish pointer.
        return ptr::NonNull::<u8>::dangling().as_ptr();
    }

    let Ok(layout) = Layout::from_size_align(size, align.max(1)) else {
        return ptr::null_mut();
    };

    // Safety: layout is valid.
    unsafe { std::alloc::alloc(layout) }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_alloc_zeroed(size: usize, align: usize) -> *mut u8 {
    if size == 0 {
        return ptr::NonNull::<u8>::dangling().as_ptr();
    }

    let Ok(layout) = Layout::from_size_align(size, align.max(1)) else {
        return ptr::null_mut();
    };

    // Safety: layout is valid.
    unsafe { std::alloc::alloc_zeroed(layout) }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_realloc(
    ptr0: *mut u8,
    old_size: usize,
    new_size: usize,
    align: usize,
) -> *mut u8 {
    if new_size == 0 {
        silver_rt_dealloc(ptr0, old_size, align);
        return ptr::NonNull::<u8>::dangling().as_ptr();
    }

    let Ok(layout) = Layout::from_size_align(old_size.max(1), align.max(1)) else {
        return ptr::null_mut();
    };

    if ptr0.is_null() {
        return silver_rt_alloc(new_size, align);
    }

    // Safety: caller promises ptr0 was allocated with (old_size, align).
    unsafe { std::alloc::realloc(ptr0, layout, new_size) }
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_dealloc(ptr0: *mut u8, size: usize, align: usize) {
    if ptr0.is_null() || size == 0 {
        return;
    }
    let Ok(layout) = Layout::from_size_align(size, align.max(1)) else {
        return;
    };

    // Safety: caller promises ptr0 was allocated with this layout.
    unsafe { std::alloc::dealloc(ptr0, layout) };
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_memset(dst: *mut u8, value: u8, len: usize) -> *mut u8 {
    if dst.is_null() {
        return dst;
    }
    // Safety: caller promises `dst` is valid for `len` bytes.
    unsafe { ptr::write_bytes(dst, value, len) };
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_memcpy(dst: *mut u8, src: *const u8, len: usize) -> *mut u8 {
    if dst.is_null() || src.is_null() {
        return dst;
    }
    // Safety: caller promises non-overlapping regions and validity for `len` bytes.
    unsafe { ptr::copy_nonoverlapping(src, dst, len) };
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn silver_rt_memmove(dst: *mut u8, src: *const u8, len: usize) -> *mut u8 {
    if dst.is_null() || src.is_null() {
        return dst;
    }
    // Safety: caller promises validity for `len` bytes; overlap is allowed.
    unsafe { ptr::copy(src, dst, len) };
    dst
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_realloc_dealloc_roundtrip() {
        unsafe {
            let p = silver_rt_alloc(16, 8);
            assert!(!p.is_null());
            *p.add(0) = 1;
            *p.add(15) = 2;

            let p2 = silver_rt_realloc(p, 16, 32, 8);
            assert!(!p2.is_null());
            assert_eq!(*p2.add(0), 1);
            assert_eq!(*p2.add(15), 2);

            silver_rt_dealloc(p2, 32, 8);
        }
    }

    #[test]
    fn memset_memcpy_memmove() {
        unsafe {
            let a = silver_rt_alloc_zeroed(16, 1);
            let b = silver_rt_alloc(16, 1);
            assert!(!a.is_null() && !b.is_null());

            silver_rt_memset(b, 0xAB, 16);
            silver_rt_memcpy(a, b, 16);
            assert_eq!(*a.add(3), 0xAB);

            // Overlapping move: shift right by 1.
            silver_rt_memmove(a.add(1), a, 15);
            assert_eq!(*a.add(1), 0xAB);

            silver_rt_dealloc(a, 16, 1);
            silver_rt_dealloc(b, 16, 1);
        }
    }
}
