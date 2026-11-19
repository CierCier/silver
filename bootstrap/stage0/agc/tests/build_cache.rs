//! Cache invalidation observed at the `agc` command boundary.

use std::path::{Path, PathBuf};
use std::process::Command;

fn temp_project(name: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!("agc-cache-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(
        root.join("helper.ag"),
        "i32 helper_value() {\n    return 4;\n}\n",
    )
    .unwrap();
    std::fs::write(
        root.join("main.ag"),
        "import helper;\n\ni32 main() {\n    return helper_value();\n}\n",
    )
    .unwrap();
    root
}

fn build(compiler: &Path, root: &Path) -> Vec<u8> {
    let output = Command::new(compiler)
        .current_dir(root)
        .args(["build", "main.ag", "-c", "-o", "out.o", "--no-progress"])
        .args(["--cache-dir", "cache"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    std::fs::read(root.join("out.o")).unwrap()
}

fn cached_objects(root: &Path) -> usize {
    std::fs::read_dir(root.join("cache/obj")).unwrap().count()
}

#[test]
fn new_compiler_build_reuses_no_object() {
    let root = temp_project("compiler");
    let compiler = root.join("bin/agc");
    std::fs::create_dir_all(compiler.parent().unwrap()).unwrap();
    std::fs::copy(env!("CARGO_BIN_EXE_agc"), &compiler).unwrap();
    let first = build(&compiler, &root);
    let objects = cached_objects(&root);
    assert_eq!(build(&compiler, &root), first);
    assert_eq!(
        cached_objects(&root),
        objects,
        "an unchanged build added objects"
    );

    // A rebuilt binary at the same path gets a new modification time.
    let later = std::time::SystemTime::now() + std::time::Duration::from_secs(5);
    std::fs::File::options()
        .write(true)
        .open(&compiler)
        .unwrap()
        .set_modified(later)
        .unwrap();
    assert_eq!(build(&compiler, &root), first);
    assert_eq!(
        cached_objects(&root),
        2 * objects,
        "objects of another compiler build reused"
    );
    std::fs::remove_dir_all(&root).unwrap();
}
