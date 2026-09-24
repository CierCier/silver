use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .find(|dir| dir.join("std").is_dir())
        .expect("repository root containing std/")
        .to_path_buf()
}

fn unique_temp_dir() -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system clock after Unix epoch")
        .as_nanos();
    std::env::temp_dir().join(format!(
        "agc-root-cache-test-{}-{nonce}",
        std::process::id()
    ))
}

#[test]
fn root_object_cache_stays_disabled_for_implicit_entry_imports() {
    let root = repo_root();
    let temp = unique_temp_dir();
    let cache_parent = temp.join("xdg");
    let source = temp.join("main.ag");
    let output = temp.join(if cfg!(windows) { "root.exe" } else { "root" });
    fs::create_dir_all(&temp).unwrap();
    fs::write(&source, "i32 main() { return 0; }\n").unwrap();

    let result = Command::new(env!("CARGO_BIN_EXE_agc"))
        .current_dir(&root)
        .env("XDG_CACHE_HOME", &cache_parent)
        .env_remove("SILVER_CACHE_DIR")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .arg("--no-progress")
        .arg("--cfg")
        .arg("cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1")
        .output()
        .expect("run agc");

    assert!(
        result.status.success(),
        "cached compilation failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );

    let object_dir = cache_parent.join("silver").join("obj");
    let cached_objects = fs::read_dir(object_dir)
        .map(|entries| entries.count())
        .unwrap_or(0);
    assert_eq!(
        cached_objects, 0,
        "root objects must stay uncached until implicit imports are in the graph"
    );

    let _ = fs::remove_dir_all(temp);
}

#[test]
fn cached_dependency_keeps_runtime_arg_bridge() {
    let root = repo_root();
    let temp = unique_temp_dir();
    let cache_parent = temp.join("xdg");
    let source = temp.join("main.ag");
    let dependency = temp.join("testdep.ag");
    fs::create_dir_all(&temp).unwrap();
    fs::write(
        &dependency,
        "import std.args;\n\ni32 use_args() { return (i32)count(); }\n",
    )
    .unwrap();
    fs::write(
        &source,
        "import testdep;\n\ni32 main() { return use_args(); }\n",
    )
    .unwrap();

    let compile = |output: &Path| {
        Command::new(env!("CARGO_BIN_EXE_agc"))
            .current_dir(&root)
            .env("XDG_CACHE_HOME", &cache_parent)
            .env_remove("SILVER_CACHE_DIR")
            .arg(&source)
            .arg("-o")
            .arg(output)
            .arg("--no-progress")
            .output()
            .expect("run agc")
    };

    let first_output = temp.join(if cfg!(windows) { "first.exe" } else { "first" });
    let first = compile(&first_output);
    assert!(
        first.status.success(),
        "initial cached compilation failed: {}",
        String::from_utf8_lossy(&first.stderr)
    );

    let second_output = temp.join(if cfg!(windows) {
        "second.exe"
    } else {
        "second"
    });
    let second = compile(&second_output);
    assert!(
        second.status.success(),
        "reused cached compilation failed: {}",
        String::from_utf8_lossy(&second.stderr)
    );

    let _ = fs::remove_dir_all(temp);
}

#[test]
fn cached_module_uses_root_runtime_entry_state() {
    let root = repo_root();
    let temp = unique_temp_dir();
    let cache_parent = temp.join("xdg");
    let source = temp.join("main.ag");
    let dependency = temp.join("testdep.ag");
    let output = temp.join(if cfg!(windows) {
        "runtime.exe"
    } else {
        "runtime"
    });
    fs::create_dir_all(&temp).unwrap();
    fs::write(
        &dependency,
        "import std.sys.entry;\n\ni32 envp_is_null() { return silver_envp() == (i8**)0; }\n",
    )
    .unwrap();
    fs::write(
        &source,
        "import testdep;\n\ni32 main() { return envp_is_null(); }\n",
    )
    .unwrap();

    let compile = || {
        Command::new(env!("CARGO_BIN_EXE_agc"))
            .current_dir(&root)
            .env("XDG_CACHE_HOME", &cache_parent)
            .env_remove("SILVER_CACHE_DIR")
            .arg(&source)
            .arg("-o")
            .arg(&output)
            .arg("--no-progress")
            .output()
            .expect("run agc")
    };

    let first = compile();
    assert!(
        first.status.success(),
        "initial cached compilation failed: {}",
        String::from_utf8_lossy(&first.stderr)
    );
    let second = compile();
    assert!(
        second.status.success(),
        "reused cached compilation failed: {}",
        String::from_utf8_lossy(&second.stderr)
    );

    let run = Command::new(&output)
        .current_dir(&root)
        .env("XDG_CACHE_HOME", &cache_parent)
        .output()
        .expect("run compiled program");
    assert!(
        run.status.success(),
        "cached module did not use the root entry state: {}",
        String::from_utf8_lossy(&run.stderr)
    );

    let _ = fs::remove_dir_all(temp);
}
