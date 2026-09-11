//! Linker orchestration: object assembly, cc/ld.lld invocation, CRT
//! discovery and library search.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::LazyLock;

use crate::driver::CompilePlan;

/// How to invoke the linker for a given target.
///
/// - `GnuLd`: GNU ld / ld.lld / mold via GNU flags (ELF, Mach-O).
/// - `LldLink`: COFF linking with MSVC-style flags — `lld-link`, or the MSVC
///   toolchain `link.exe` located via vswhere as a drop-in replacement.
/// - `UnsupportedMinGW`: MinGW triples use the Win64 ABI for codegen but need
///   a dedicated GNU-ld-on-PE link flavor (mingw CRT, `-Wl` flags) that does
///   not exist yet; linking fails with a clear error instead of silently
///   producing MSVC-runtime binaries.
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum LinkFlavor {
    GnuLd,
    LldLink,
    UnsupportedMinGW,
}

impl LinkFlavor {
    /// Resolves the flavor from an explicit target triple. When no target is
    /// given the host is assumed: windows hosts link via the MSVC model,
    /// everything else via the GNU model.
    fn for_target(target: Option<&str>) -> Self {
        let is_windows = crate::codegen::abi::target_is_windows(target);
        let is_mingw = target
            .map(|t| t.to_ascii_lowercase().contains("mingw"))
            .unwrap_or(false);
        if is_windows {
            if is_mingw {
                Self::UnsupportedMinGW
            } else {
                Self::LldLink
            }
        } else {
            Self::GnuLd
        }
    }
}

const MINGW_UNSUPPORTED_ERR: &str = "MinGW targets are not yet supported: the Win64 ABI codegen \
     is correct, but no GNU-ld-on-PE link flavor exists yet (see docs/windows-port.md §3.3). \
     Use an MSVC triple instead, e.g. --target x86_64-pc-windows-msvc";

/// Locates the COFF linker (lld-link, or the MSVC link.exe as a drop-in).
/// Honors `SILVER_LINKER` as an explicit override.
fn find_lld_link() -> Result<Command, String> {
    if let Ok(override_path) = std::env::var("SILVER_LINKER") {
        if !override_path.trim().is_empty() {
            return Ok(Command::new(override_path.trim()));
        }
    }
    if command_exists("lld-link") {
        return Ok(Command::new("lld-link"));
    }
    // Fall back to the MSVC link.exe next to a VS installation.
    if let Some(link_exe) = locate_msvc_link_exe() {
        return Ok(Command::new(link_exe));
    }
    Err(
        "no COFF linker found: install LLVM (lld-link) or VS Build Tools (link.exe), \
         or set SILVER_LINKER"
            .to_string(),
    )
}

/// Best-effort location of VS `link.exe` via vswhere.exe.
fn locate_msvc_link_exe() -> Option<PathBuf> {
    let program_files_x86 =
        std::env::var("ProgramFiles(x86)").unwrap_or_else(|_| "C:\\Program Files (x86)".to_string());
    let vswhere = PathBuf::from(&program_files_x86)
        .join("Microsoft Visual Studio")
        .join("Installer")
        .join("vswhere.exe");
    if !vswhere.is_file() {
        return None;
    }
    let output = Command::new(&vswhere)
        .args([
            "-latest",
            "-products",
            "*",
            "-requires",
            "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
            "-property",
            "installationPath",
        ])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let install_root = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if install_root.is_empty() {
        return None;
    }
    // Pick the highest-versioned MSVC toolset directory.
    let vc_tools = PathBuf::from(&install_root).join("VC").join("Tools").join("MSVC");
    let mut versions: Vec<_> = std::fs::read_dir(&vc_tools)
        .ok()?
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().join("bin").join("Hostx64").join("x64").is_dir())
        .map(|entry| entry.file_name())
        .collect();
    versions.sort();
    let link = versions
        .last()?
        .to_str()
        .map(|ver| vc_tools.join(ver).join("bin").join("Hostx64").join("x64").join("link.exe"))?;
    link.is_file().then_some(link)
}

/// Library search dirs for windows-flavored links: `%LIB%` (populated by VS
/// dev shells) plus the MSVC CRT and Windows SDK dirs discovered via vswhere.
fn msvc_library_dirs() -> Vec<PathBuf> {
    let mut dirs: Vec<PathBuf> =
        std::env::split_paths(&std::env::var("LIB").unwrap_or_default())
            .filter(|p| !p.as_os_str().is_empty())
            .collect();

    let program_files_x86 = std::env::var("ProgramFiles(x86)")
        .unwrap_or_else(|_| "C:\\Program Files (x86)".to_string());
    let vswhere = PathBuf::from(&program_files_x86)
        .join("Microsoft Visual Studio")
        .join("Installer")
        .join("vswhere.exe");
    let Ok(output) = Command::new(&vswhere)
        .args([
            "-latest",
            "-products",
            "*",
            "-requires",
            "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
            "-property",
            "installationPath",
        ])
        .output()
    else {
        return dirs;
    };
    if !output.status.success() {
        return dirs;
    }
    let install_root = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if install_root.is_empty() {
        return dirs;
    }
    let vc_tools = PathBuf::from(&install_root).join("VC").join("Tools").join("MSVC");
    if let Ok(entries) = std::fs::read_dir(&vc_tools) {
        let mut versions: Vec<_> = entries
            .filter_map(|entry| entry.ok())
            .map(|entry| entry.file_name())
            .collect();
        versions.sort();
        if let Some(ver) = versions.last().and_then(|v| v.to_str()) {
            let crt_dir = vc_tools.join(ver).join("lib").join("x64");
            if crt_dir.is_dir() {
                dirs.push(crt_dir);
            }
        }
    }
    // Windows SDK: Windows Kits\10\Lib\<version>\{um,ucrt}\x64
    let kits = PathBuf::from(&program_files_x86)
        .join("Windows Kits")
        .join("10")
        .join("Lib");
    if let Ok(entries) = std::fs::read_dir(&kits) {
        let mut versions: Vec<_> = entries
            .filter_map(|entry| entry.ok())
            .map(|entry| entry.file_name())
            .collect();
        versions.sort();
        if let Some(ver) = versions.last().and_then(|v| v.to_str()) {
            for sub in ["um", "ucrt"] {
                let dir = kits.join(ver).join(sub).join("x64");
                if dir.is_dir() {
                    dirs.push(dir);
                }
            }
        }
    }
    dirs
}

/// The CRT + OS import libraries every console executable needs. The static
/// set (/MT) makes the exe self-contained; the dynamic set (/MD) is the
/// default and defers to the installed UCRT.
fn msvc_default_libs(static_crt: bool) -> &'static [&'static str] {
    if static_crt {
        &[
            "libcmt.lib",
            "libvcruntime.lib",
            "libucrt.lib",
            "kernel32.lib",
            "bcrypt.lib",
        ]
    } else {
        &["msvcrt.lib", "vcruntime.lib", "ucrt.lib", "kernel32.lib", "bcrypt.lib"]
    }
}

/// Appends a `#[link(name)]` dependency in the flavor's syntax: bare names
/// resolve as `<name>.lib` on the MSVC side.
fn add_native_library_win(command: &mut Command, library: &str) {
    let path = Path::new(library);
    if path.is_absolute() {
        command.arg(library);
    } else if path.extension().is_some_and(|e| e.eq_ignore_ascii_case("lib")) {
        command.arg(library);
    } else {
        command.arg(format!("{library}.lib"));
    }
}

pub(crate) fn run_tool(mut command: Command, label: &str) -> Result<(), String> {
    let output = command
        .output()
        .map_err(|e| format!("failed to run {label}: {e}"))?;
    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let details = if !stderr.trim().is_empty() {
        stderr.trim().to_string()
    } else {
        stdout.trim().to_string()
    };
    Err(format!(
        "{label} failed: {}",
        if details.is_empty() {
            "<no tool output>".to_string()
        } else {
            details
        }
    ))
}

fn add_native_library(command: &mut Command, library: &str) {
    if Path::new(library).is_absolute() {
        command.arg(library);
    } else {
        command.arg(format!("-l{library}"));
    }
}

// ---- Cached cc queries — each arg is a LazyLock, spawned once ----

static CC_LIB_DIRS: LazyLock<Vec<PathBuf>> = LazyLock::new(|| {
    let mut dirs = cc_query_raw("-print-search-dirs")
        .ok()
        .map(|output| {
            let mut dirs = Vec::new();
            for line in output.lines() {
                if let Some(rest) = line.strip_prefix("libraries: =") {
                    for raw in rest.split(':') {
                        if !raw.is_empty() {
                            dirs.push(PathBuf::from(raw));
                        }
                    }
                }
            }
            dirs
        })
        .unwrap_or_default();
    // Nix-style dev shells export their library search dirs via NIX_LDFLAGS
    // (`-L<dir>` tokens) and LIBRARY_PATH, which `cc -print-search-dirs` does
    // not report. Honor them so external shared libraries (e.g. raylib) are
    // found by the primary lld path without an explicit `-L`.
    if let Ok(nix_ldflags) = std::env::var("NIX_LDFLAGS") {
        let mut tokens = nix_ldflags.split_whitespace().peekable();
        while let Some(token) = tokens.next() {
            if let Some(dir) = token.strip_prefix("-L") {
                let dir = if dir.is_empty() {
                    tokens.next().unwrap_or_default()
                } else {
                    dir
                };
                if !dir.is_empty() && !dirs.iter().any(|d| d.as_os_str() == dir) {
                    dirs.push(PathBuf::from(dir));
                }
            }
        }
    }
    if let Ok(library_path) = std::env::var("LIBRARY_PATH") {
        for dir in std::env::split_paths(&library_path) {
            if !dirs.contains(&dir) {
                dirs.push(dir);
            }
        }
    }
    dirs
});

pub(crate) fn cc_query_raw(arg: &str) -> Result<String, String> {
    let output = Command::new("cc")
        .arg(arg)
        .output()
        .map_err(|e| format!("failed to query cc {arg}: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "cc {arg} failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// The ELF dynamic loader (PT_INTERP) for dynamically-linked executables.
/// ld.lld only adds the default interpreter when libc is a direct NEEDED
/// dependency, so links against e.g. raylib alone would run without a loader
/// and jump to null on the first PLT call. Prefer the loader the C compiler
/// knows about; fall back to the standard /lib64 path.
static DYNAMIC_LINKER: LazyLock<String> = LazyLock::new(|| {
    let queried = cc_query_raw("-print-file-name=ld-linux-x86-64.so.2").unwrap_or_default();
    let trimmed = queried.trim();
    if !trimmed.is_empty() && !trimmed.starts_with("ld-linux") && Path::new(trimmed).is_absolute() {
        trimmed.to_string()
    } else {
        "/lib64/ld-linux-x86-64.so.2".to_string()
    }
});

pub(crate) fn cc_library_dirs() -> Vec<PathBuf> {
    CC_LIB_DIRS.clone()
}

pub(crate) fn command_exists(name: &str) -> bool {
    // Windows PATH lookups via CreateProcess resolve `name.exe` even when only
    // the bare name is invoked; a plain directory check for `name` would miss
    // installations that ship only the .exe (e.g. lld-link.exe).
    let candidates: [String; 2] = if cfg!(windows) {
        [name.to_string(), format!("{name}.exe")]
    } else {
        [name.to_string(), name.to_string()]
    };
    std::env::var_os("PATH")
        .map(|paths| {
            std::env::split_paths(&paths).any(|p| candidates.iter().any(|c| p.join(c).is_file()))
        })
        .unwrap_or(false)
}

pub(crate) fn link_exe(
    plan: &CompilePlan,
    object_paths: &[PathBuf],
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    if object_paths.is_empty() {
        return Err("no object files to link".to_string());
    }
    match LinkFlavor::for_target(plan.target.as_deref()) {
        LinkFlavor::LldLink => {
            link_exe_with_lld_link(plan, object_paths, dependency_paths, native_libs)
        }
        LinkFlavor::UnsupportedMinGW => Err(MINGW_UNSUPPORTED_ERR.to_string()),
        LinkFlavor::GnuLd => link_exe_with_ld_lld(plan, object_paths, dependency_paths, native_libs)
            .or_else(|ld_err| {
                link_exe_with_cc(plan, object_paths, dependency_paths, native_libs).map_err(
                    |cc_err| {
                        format!("ld.lld path failed: {ld_err}; fallback linker failed: {cc_err}")
                    },
                )
            }),
    }
}

/// Windows-flavored executable link via `lld-link` (or MSVC link.exe).
///
/// Uses the standard `mainCRTStartup` entry from the CRT: Silver programs
/// define `main` and the CRT initializes the heap/stdio/environ around it.
/// No dynamic loader, no rpath — DLLs are found next to the exe or on PATH.
pub(crate) fn link_exe_with_lld_link(
    plan: &CompilePlan,
    object_paths: &[PathBuf],
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let mut link = find_lld_link()?;
    let tool_name = "COFF linker";

    link.arg(format!("/OUT:{}", plan.output.display()));
    link.arg("/MACHINE:X64");
    link.arg("/SUBSYSTEM:CONSOLE");
    // std.sys.entry defines _start (argv setup, thread join, flush, exit);
    // the CRT's mainCRTStartup is bypassed.
    link.arg("/ENTRY:_start");

    for obj in object_paths {
        link.arg(obj);
    }

    // Static .agm module objects pass through as inputs; dynamic (.dll)
    // module dependencies need import-lib support and are rejected for now.
    for dep in dependency_paths {
        if dep.extension().is_some_and(|e| e.eq_ignore_ascii_case("dll")) {
            return Err(format!(
                "dynamic module dependencies ({}) are not yet supported for Windows targets; build modules without --shared",
                dep.display()
            ));
        }
        link.arg(dep);
    }

    for dir in msvc_library_dirs() {
        link.arg(format!("/LIBPATH:{}", dir.display()));
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg(format!("/LIBPATH:{}", dir.display()));
    }
    for dir in &plan.lib_dirs {
        link.arg(format!("/LIBPATH:{}", dir.display()));
    }

    for lib in msvc_default_libs(plan.static_link) {
        link.arg(lib);
    }
    // `#[link(name = "foo")]` native deps resolve against the SDK/libpath as foo.lib.
    for lib in native_libs {
        add_native_library_win(&mut link, lib);
    }

    run_tool(link, tool_name)
}

pub(crate) fn should_force_non_pie(target: Option<&str>) -> bool {
    match target {
        Some(triple) => triple.contains("linux"),
        None => cfg!(target_os = "linux"),
    }
}

/// Whether the link pulls in any shared object: a `.so` module dependency or
/// a `-l` library that resolves to `lib<name>.so` in the search dirs. Only
/// then does the executable need a dynamic loader (PT_INTERP); adding one
/// without a dynamic section makes ld-linux crash in `dl_main`.
fn link_has_shared_libraries(
    native_libs: &[String],
    search_dirs: &[PathBuf],
    dependency_paths: &[PathBuf],
) -> bool {
    if dependency_paths
        .iter()
        .any(|p| p.extension().is_some_and(|e| e == "so"))
    {
        return true;
    }
    native_libs.iter().any(|lib| {
        let path = Path::new(lib);
        if path.is_absolute() {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.contains(".so") || name.ends_with(".dylib"))
        } else {
            search_dirs
                .iter()
                .any(|dir| dir.join(format!("lib{lib}.so")).exists())
        }
    })
}

pub(crate) fn dependency_library_dirs(dependency_paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    for path in dependency_paths {
        let Some(parent) = path.parent() else {
            continue;
        };
        let parent = parent.to_path_buf();
        if !dirs.contains(&parent) {
            dirs.push(parent);
        }
    }
    dirs
}

pub(crate) fn link_exe_with_ld_lld(
    plan: &CompilePlan,
    object_paths: &[PathBuf],
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    // Try ld.lld first; mold is broken in current Nix (produces segfaulting
    // binaries even for trivial objects — see strace SIGSEGV at 0x8). Keep
    // mold opt-in via SILVER_USE_MOLD if needed.
    let use_mold = std::env::var("SILVER_USE_MOLD").is_ok() && command_exists("mold");
    let lld_name = if use_mold {
        "mold"
    } else if command_exists("ld.lld") {
        "ld.lld"
    } else if command_exists("lld") {
        "lld"
    } else {
        return Err("no linker found (mold/ld.lld/lld)".to_string());
    };

    let mut link = Command::new(lld_name);
    if lld_name == "lld" {
        link.arg("-flavor").arg("gnu");
    }
    // mold supports ld.lld-compatible flags.
    link.arg("-o").arg(&plan.output);

    // Silver code and std are always statically linked into the objects; the
    // executable stays non-PIE (codegen is not PIC) but is dynamically linked
    // by default, so the linker adds PT_INTERP and DT_NEEDED entries when
    // external shared libraries (e.g. raylib) are linked. `--static` restores
    // the fully static executable.
    let mut search_dirs = cc_library_dirs();
    search_dirs.extend(plan.lib_dirs.iter().cloned());
    if plan.static_link {
        link.arg("-static");
    } else if link_has_shared_libraries(native_libs, &search_dirs, dependency_paths) {
        link.arg("--dynamic-linker").arg(&*DYNAMIC_LINKER);
    }

    if let Some(target) = &plan.target {
        link.arg("-mtriple").arg(target);
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }

    for obj in object_paths {
        link.arg(obj);
    }
    for dep in dependency_paths {
        link.arg(dep);
    }

    for dir in cc_library_dirs() {
        link.arg("-L").arg(dir);
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
        link.arg("-rpath").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg("-rpath").arg(&dir);
    }

    for lib in native_libs {
        add_native_library(&mut link, lib);
    }

    run_tool(link, lld_name)
}

pub(crate) fn link_exe_with_cc(
    plan: &CompilePlan,
    object_paths: &[PathBuf],
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let mut link = Command::new("cc");
    link.arg("-o").arg(&plan.output);
    for obj in object_paths {
        link.arg(obj);
    }
    for dep in dependency_paths {
        link.arg(dep);
    }
    // Non-PIC objects require an ET_EXEC executable; when shared libraries
    // are linked the driver adds PT_INTERP/DYNAMIC itself.
    if should_force_non_pie(plan.target.as_deref()) {
        link.arg("-no-pie");
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    if plan.debug_info {
        link.arg("-g");
    }
    // Silver never links libc: no CRT startup, no libc/libgcc.
    link.arg("-nostdlib");
    let mut search_dirs = cc_library_dirs();
    search_dirs.extend(plan.lib_dirs.iter().cloned());
    if plan.static_link {
        link.arg("-static");
    } else {
        // External shared libraries carry their own glibc/libstdc++ deps; let
        // ld resolve them at runtime instead of demanding them at link time
        // (the `-nostdlib` above leaves them out of the link).
        link.arg("-Wl,--allow-shlib-undefined");
        if link_has_shared_libraries(native_libs, &search_dirs, dependency_paths) {
            link.arg(format!("-Wl,-dynamic-linker,{}", *DYNAMIC_LINKER));
        }
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for lib in native_libs {
        add_native_library(&mut link, lib);
    }
    run_tool(link, "cc linker")
}

pub(crate) fn link_shared_module(
    plan: &CompilePlan,
    object_path: &Path,
    output_path: &Path,
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let flavor = LinkFlavor::for_target(plan.target.as_deref());
    if flavor == LinkFlavor::UnsupportedMinGW {
        return Err(MINGW_UNSUPPORTED_ERR.to_string());
    }
    if flavor == LinkFlavor::LldLink {
        let mut link = find_lld_link()?;
        link.arg("/DLL");
        link.arg(format!("/OUT:{}", output_path.display()));
        link.arg("/MACHINE:X64");
        link.arg(object_path);
        // Static module objects pass through as inputs; dynamic (.dll) module
        // dependencies need import-lib support and are rejected for now.
        for dep in dependency_paths {
            if dep.extension().is_some_and(|e| e.eq_ignore_ascii_case("dll")) {
                return Err(format!(
                    "dynamic module dependencies ({}) are not yet supported for Windows targets; build modules without --shared",
                    dep.display()
                ));
            }
            link.arg(dep);
        }
        for dir in msvc_library_dirs() {
            link.arg(format!("/LIBPATH:{}", dir.display()));
        }
        for dir in dependency_library_dirs(dependency_paths) {
            link.arg(format!("/LIBPATH:{}", dir.display()));
        }
        for dir in &plan.lib_dirs {
            link.arg(format!("/LIBPATH:{}", dir.display()));
        }
        for lib in msvc_default_libs(plan.static_link) {
            link.arg(lib);
        }
        for lib in native_libs {
            add_native_library_win(&mut link, lib);
        }
        return run_tool(link, "COFF shared linker");
    }
    let mut link = Command::new("cc");
    link.arg("-shared")
        .arg("-o")
        .arg(output_path)
        .arg(object_path);
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    for dep in dependency_paths {
        link.arg(dep);
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for lib in native_libs {
        add_native_library(&mut link, lib);
    }
    run_tool(link, "cc shared linker")
}
