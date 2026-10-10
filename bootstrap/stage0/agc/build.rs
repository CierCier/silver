fn main() {
    // Windows: the official LLVM-C.dll only builds a subset of LLVM targets,
    // but inkwell declares init externs for all of them — link no-op stubs
    // for the missing ones (see win_llvm_stubs.c).
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("windows") {
        println!("cargo:rerun-if-changed=win_llvm_stubs.c");
        cc::Build::new()
            .file("win_llvm_stubs.c")
            .compile("silver_llvm_target_stubs");
    }

    let git_dir = git_cmd(&["rev-parse", "--git-dir"]).map(std::path::PathBuf::from);

    let describe = git_cmd(&["describe", "--tags", "--dirty", "--always"])
        .unwrap_or_else(|| "unknown".to_string());
    let sha = git_cmd(&["rev-parse", "--short", "HEAD"]).unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=GIT_DESCRIBE={}", describe);
    println!("cargo:rustc-env=GIT_SHA={}", sha);
    println!(
        "cargo:rustc-env=GIT_DIRTY={}",
        if describe.ends_with("-dirty") {
            "1"
        } else {
            "0"
        }
    );

    if let Some(git) = &git_dir
        && git.exists()
    {
        println!("cargo:rerun-if-changed={}", git.join("HEAD").display());
        let refs = git.join("refs");
        if refs.exists() {
            println!("cargo:rerun-if-changed={}", refs.display());
        }
    }

    // Build configuration for include/root/sysroot search paths
    if let Ok(val) = std::env::var("SILVER_EXTRA_INCLUDE_DIRS") {
        println!("cargo:rustc-env=SILVER_BUILD_INCLUDE_DIRS={}", val);
    }
    println!("cargo:rerun-if-env-changed=SILVER_EXTRA_INCLUDE_DIRS");

    if let Ok(val) = std::env::var("SILVER_DEFAULT_INCLUDE_DIRS") {
        println!("cargo:rustc-env=SILVER_BUILD_INCLUDE_DIRS={}", val);
    }
    println!("cargo:rerun-if-env-changed=SILVER_DEFAULT_INCLUDE_DIRS");

    if let Ok(val) = std::env::var("SILVER_EXTRA_ROOT_DIRS") {
        println!("cargo:rustc-env=SILVER_BUILD_ROOT_DIRS={}", val);
    }
    println!("cargo:rerun-if-env-changed=SILVER_EXTRA_ROOT_DIRS");

    if let Ok(val) = std::env::var("SILVER_DEFAULT_SYSROOT") {
        println!("cargo:rustc-env=SILVER_BUILD_SYSROOT={}", val);
    }
    println!("cargo:rerun-if-env-changed=SILVER_DEFAULT_SYSROOT");
}

fn git_cmd(args: &[&str]) -> Option<String> {
    std::process::Command::new("git")
        .args(args)
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                String::from_utf8(o.stdout).ok()
            } else {
                None
            }
        })
        .map(|s| s.trim().to_string())
}
