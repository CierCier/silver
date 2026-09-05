//! Linker orchestration: object assembly, cc/ld.lld invocation, CRT
//! discovery and library search.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::LazyLock;

use crate::driver::CompilePlan;

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
    std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).any(|p| p.join(name).is_file()))
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
    link_exe_with_ld_lld(plan, object_paths, dependency_paths, native_libs).or_else(|ld_err| {
        link_exe_with_cc(plan, object_paths, dependency_paths, native_libs).map_err(|cc_err| {
            format!("ld.lld path failed: {ld_err}; fallback linker failed: {cc_err}")
        })
    })
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
