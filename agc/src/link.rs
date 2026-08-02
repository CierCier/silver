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
// ---- Cached cc queries — each arg is a LazyLock, spawned once ----

static CRT1_O: LazyLock<String> =
    LazyLock::new(|| cc_query_raw("-print-file-name=crt1.o").unwrap_or_default());
static CRTI_O: LazyLock<String> =
    LazyLock::new(|| cc_query_raw("-print-file-name=crti.o").unwrap_or_default());
static CRTBEGIN_O: LazyLock<String> =
    LazyLock::new(|| cc_query_raw("-print-file-name=crtbegin.o").unwrap_or_default());
static CRTEND_O: LazyLock<String> =
    LazyLock::new(|| cc_query_raw("-print-file-name=crtend.o").unwrap_or_default());
static CRTN_O: LazyLock<String> =
    LazyLock::new(|| cc_query_raw("-print-file-name=crtn.o").unwrap_or_default());
static CC_LIB_DIRS: LazyLock<Vec<PathBuf>> = LazyLock::new(|| {
    cc_query_raw("-print-search-dirs")
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
        .unwrap_or_default()
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

pub(crate) fn cc_query_crt(arg: &str) -> Result<String, String> {
    match arg {
        "crt1.o" => Ok(CRT1_O.to_string()),
        "crti.o" => Ok(CRTI_O.to_string()),
        "crtbegin.o" => Ok(CRTBEGIN_O.to_string()),
        "crtend.o" => Ok(CRTEND_O.to_string()),
        "crtn.o" => Ok(CRTN_O.to_string()),
        _ => cc_query_raw(&format!("-print-file-name={arg}")),
    }
}

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

pub(crate) fn default_dynamic_linker(target: Option<&str>) -> Option<&'static str> {
    match target.unwrap_or("") {
        t if t.contains("aarch64") => Some("/lib/ld-linux-aarch64.so.1"),
        t if t.contains("x86_64") || t.is_empty() => Some("/lib64/ld-linux-x86-64.so.2"),
        _ => None,
    }
}

pub(crate) fn should_force_non_pie(target: Option<&str>) -> bool {
    match target {
        Some(triple) => triple.contains("linux"),
        None => cfg!(target_os = "linux"),
    }
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
    // Try mold first (fastest), then ld.lld, then lld.
    let lld_name = if command_exists("mold") {
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

    if let Some(target) = &plan.target {
        link.arg("-mtriple").arg(target);
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    if !plan.no_std
        && let Some(loader) = default_dynamic_linker(plan.target.as_deref())
    {
        link.arg("-dynamic-linker").arg(loader);
    }

    if !plan.no_std {
        for crt in ["crt1.o", "crti.o", "crtbegin.o"] {
            let path = cc_query_crt(crt)?;
            if path != crt {
                link.arg(path);
            }
        }
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
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg("-rpath").arg(&dir);
    }

    if !plan.no_std {
        link.arg("-lc").arg("-lgcc_s").arg("-lgcc");
        for crt in ["crtend.o", "crtn.o"] {
            let path = cc_query_crt(crt)?;
            if path != crt {
                link.arg(path);
            }
        }
    }
    for lib in native_libs {
        link.arg(format!("-l{lib}"));
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
    if should_force_non_pie(plan.target.as_deref()) {
        link.arg("-no-pie");
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    if plan.debug_info {
        link.arg("-g");
    }
    if plan.no_std {
        link.arg("-nostdlib");
    }
    if plan.static_runtime {
        link.arg("-static");
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for lib in native_libs {
        link.arg(format!("-l{lib}"));
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
        link.arg(format!("-l{lib}"));
    }
    run_tool(link, "cc shared linker")
}
