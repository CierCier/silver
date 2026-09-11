//! llvm-config shim for Windows.
//!
//! The official LLVM Windows installer ships no `llvm-config.exe` and no static
//! LLVM libraries — only `LLVM-C.lib` (import lib for `LLVM-C.dll`). llvm-sys's
//! build script refuses dynamic linking on MSVC (it bails on `--libnames
//! --link-shared`) and falls back to the static query, so this shim serves
//! `LLVM-C.lib` under `--libnames --link-static`. Linking an import lib with the
//! `static` hint is fine: the linker resolves it identically and the runtime
//! dependency is `LLVM-C.dll` (resolved via `PATH`).
//!
//! Query contract: `llvm-sys` 221 build.rs probes `--version`, `--prefix`,
//! `--libdir`, `--libnames [--link-static|--link-shared]`,
//! `--system-libs [--link-static]`, `--cflags`, `--build-mode`.
//!
//! Headers: the official install ships only stub C headers
//! (`include/llvm-c/{Remarks.h,lto.h}` — no `Target.h`), which `llvm-sys`'s
//! `wrappers/target.c` needs. When a provisioned headers tree exists at
//! `SILVER_LLVM_HEADERS` (default `%LOCALAPPDATA%\silver\llvm-headers`,
//! containing `llvm-c/*.h` + generated `llvm/Config/{llvm-config.h,targets.h,
//! targets.def}`), `--cflags` points there; otherwise it falls back to the
//! install include dir.
//!
//! Environment overrides: `SILVER_LLVM_ROOT` (default `C:\Program Files\LLVM`),
//! `SILVER_LLVM_VERSION` (default `22.1.8`), `SILVER_LLVM_HEADERS`.

use std::env;
use std::path::Path;

const DEFAULT_ROOT: &str = r"C:\Program Files\LLVM";
const DEFAULT_VERSION: &str = "22.1.8";

const TARGETS: &[&str] = &[
    "aarch64", "amdgpu", "arm", "avr", "bpf", "hexagon", "lanai", "loongarch", "mips",
    "msp430", "nvptx", "powerpc", "riscv", "sparc", "systemz", "webassembly", "x86", "xcore",
];

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetShortPathNameW(
        lpsz_long_path: *const u16,
        lpsz_short_path: *mut u16,
        cch_buffer: u32,
    ) -> u32;
}

/// 8.3 short path so the flag survives whitespace splitting in build systems
/// (`cc` crate splits `CFLAGS` blindly and "Program Files" contains a space).
#[cfg(windows)]
fn short_path(path: &Path) -> String {
    use std::os::windows::ffi::OsStrExt;

    let wide: Vec<u16> = path
        .as_os_str()
        .encode_wide()
        .chain(std::iter::once(0))
        .collect();
    let mut buf = vec![0u16; wide.len() * 2];
    let len = unsafe { GetShortPathNameW(wide.as_ptr(), buf.as_mut_ptr(), buf.len() as u32) };
    if len == 0 {
        // 8.3 name generation disabled — fall back to the long path.
        return path.to_string_lossy().into_owned();
    }
    buf.truncate(len as usize);
    String::from_utf16_lossy(&buf)
}

#[cfg(not(windows))]
fn short_path(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

/// Provisioned headers tree wins over the install's stub include dir.
fn include_dir(root: &Path) -> std::path::PathBuf {
    let default_headers = format!(
        r"{}\silver\llvm-headers",
        env::var("LOCALAPPDATA").unwrap_or_default()
    );
    let headers = env::var("SILVER_LLVM_HEADERS").unwrap_or(default_headers);
    let headers = Path::new(&headers);
    if headers.join("llvm-c").join("Target.h").is_file() {
        headers.to_path_buf()
    } else {
        root.join("include")
    }
}

fn main() {
    let root = env::var("SILVER_LLVM_ROOT").unwrap_or_else(|_| DEFAULT_ROOT.to_string());
    let version = env::var("SILVER_LLVM_VERSION").unwrap_or_else(|_| DEFAULT_VERSION.to_string());
    let root_path = Path::new(&root);

    let mut args: Vec<String> = env::args().skip(1).collect();
    // Normalize the flavor modifiers llvm-sys appends after an option.
    args.retain(|a| a != "--link-static" && a != "--link-shared");

    let out = match args.first().map(String::as_str) {
        Some("--version") => version,
        Some("--prefix") => root.clone(),
        Some("--bindir") => root_path.join("bin").to_string_lossy().into_owned(),
        Some("--includedir") => root_path.join("include").to_string_lossy().into_owned(),
        Some("--libdir") => root_path.join("lib").to_string_lossy().into_owned(),
        Some("--cmakedir") => root_path.join("lib/cmake/llvm").to_string_lossy().into_owned(),
        Some("--host-target") => "x86_64-pc-windows-msvc".to_string(),
        // Must not contain "Debug" or llvm-sys links the debug CRT.
        Some("--build-mode") => "Release".to_string(),
        Some("--link-mode") => "static".to_string(),
        Some("--cflags") | Some("--cppflags") => {
            format!("-I{}", short_path(&include_dir(root_path)))
        }
        // Import lib; llvm-sys strips the `.lib` suffix and links `static=LLVM-C`.
        Some("--libnames") => "LLVM-C.lib".to_string(),
        Some("--libs") => "LLVM-C".to_string(),
        // LLVM-C.lib carries its own /DEFAULTLIB directives (advapi32, shell32,
        // ole32, uuid, psapi); nothing extra needed here.
        Some("--system-libs") => String::new(),
        Some("--components") | Some("--targets-built") => TARGETS.join(" "),
        Some("--help") => {
            print_usage();
            return;
        }
        _ => {
            if args.is_empty() {
                print_usage();
                std::process::exit(1);
            }
            // Unknown/unsupported query: fail loudly rather than silently
            // returning wrong data — callers here only use the contract above.
            eprintln!(
                "llvm-config-shim: unsupported query {:?} (report it to the Silver windows port)",
                args
            );
            std::process::exit(1);
        }
    };

    println!("{out}");
}

fn print_usage() {
    eprintln!("llvm-config shim for the official Windows LLVM install (LLVM-C.dll model)");
    eprintln!("usage: llvm-config --version|--prefix|--bindir|--includedir|--libdir|--cmakedir");
    eprintln!("                   --host-target|--build-mode|--link-mode|--cflags|--cppflags");
    eprintln!("                   --libnames|--libs|--system-libs|--components|--targets-built");
    eprintln!("environment: SILVER_LLVM_ROOT, SILVER_LLVM_VERSION");
}
