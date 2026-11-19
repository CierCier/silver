//! A cached build must equal a fresh build. These tests change one build
//! input at a time and check that the next cached build is not reused from
//! the whole-build manifest and produces the same object and diagnostics as a
//! build with an empty cache. (`--no-cache` is no oracle: it inlines modules
//! that cached builds compile as separate units.)

use std::path::{Path, PathBuf};
use std::process::Command;

struct Project {
    root: PathBuf,
    compiler: PathBuf,
}

struct Build {
    success: bool,
    stderr: String,
    object: Option<Vec<u8>>,
}

impl Build {
    /// A reused build runs no frontend phase, so `--profile` reports none.
    fn reused(&self) -> bool {
        self.success && !self.stderr.contains("type check")
    }

    /// Diagnostics without the `--profile` report.
    fn diagnostics(&self) -> &str {
        // The report starts with an empty line, then the colored title.
        match self.stderr.find("=== Compiler Profile Report ===") {
            Some(title) => &self.stderr[..self.stderr[..title].rfind('\n').unwrap_or(0)],
            None => &self.stderr,
        }
    }
}

impl Project {
    fn new(name: &str) -> Self {
        let root = std::env::temp_dir().join(format!("agc-manifest-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        let project = Self {
            root,
            compiler: PathBuf::from(env!("CARGO_BIN_EXE_agc")),
        };
        project.write("helper.ag", "i32 helper_value() {\n    return 4;\n}\n");
        project.write(
            "main.ag",
            "import helper;\n\ni32 main() {\n    i32 unused = 3;\n    return helper_value();\n}\n",
        );
        project
    }

    fn path(&self, relative: &str) -> PathBuf {
        self.root.join(relative)
    }

    fn write(&self, relative: &str, text: &str) {
        let path = self.path(relative);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }

    fn compile(&self, compiler: &Path, cache: &[&str], args: &[&str]) -> Build {
        let output_path = self.path("out.o");
        let _ = std::fs::remove_file(&output_path);
        let output = Command::new(compiler)
            .current_dir(&self.root)
            .args([
                "build",
                "main.ag",
                "-c",
                "-o",
                "out.o",
                "--no-progress",
                "--profile",
            ])
            .args(cache)
            .args(args)
            .output()
            .unwrap();
        Build {
            success: output.status.success(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            object: std::fs::read(&output_path).ok(),
        }
    }

    fn cached_with(&self, compiler: &Path, args: &[&str]) -> Build {
        self.compile(compiler, &["--cache-dir", "cache"], args)
    }

    fn cached(&self, args: &[&str]) -> Build {
        self.cached_with(&self.compiler, args)
    }

    fn fresh(&self, args: &[&str]) -> Build {
        let _ = std::fs::remove_dir_all(self.path("fresh-cache"));
        self.compile(&self.compiler, &["--cache-dir", "fresh-cache"], args)
    }

    /// Builds with `args` after a change: the manifest must not be reused and
    /// the result must equal a fresh build.
    fn assert_rebuilt_like_fresh(&self, args: &[&str]) {
        let cached = self.cached(args);
        let fresh = self.fresh(args);
        assert!(
            !cached.reused(),
            "stale manifest reused:\n{}",
            cached.stderr
        );
        assert_eq!(
            cached.success, fresh.success,
            "{}\n---\n{}",
            cached.stderr, fresh.stderr
        );
        assert!(
            cached.object == fresh.object,
            "cached object differs from a fresh build"
        );
        assert_eq!(cached.diagnostics(), fresh.diagnostics());
    }

    /// Populates the cache and checks that an unchanged rebuild is reused.
    fn warm(&self, args: &[&str]) {
        let first = self.cached(args);
        assert!(first.success, "{}", first.stderr);
        let second = self.cached(args);
        assert!(
            second.reused(),
            "unchanged build was not reused:\n{}",
            second.stderr
        );
        assert!(second.object == first.object, "reused object differs");
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

#[test]
fn reused_build_replays_warnings_and_matches_fresh_output() {
    let project = Project::new("replay");
    let first = project.cached(&[]);
    let reused = project.cached(&[]);
    assert!(reused.reused(), "{}", reused.stderr);
    assert!(first.diagnostics().contains("unused variable 'unused'"));
    assert_eq!(reused.diagnostics(), first.diagnostics());
    assert!(
        reused.object == project.fresh(&[]).object,
        "reused object differs from a fresh build"
    );
}

#[test]
fn imported_file_edit_invalidates() {
    let project = Project::new("import-edit");
    project.warm(&[]);
    project.write("helper.ag", "i32 helper_value() {\n    return 5;\n}\n");
    project.assert_rebuilt_like_fresh(&[]);
}

#[test]
fn stdlib_edit_invalidates() {
    let project = Project::new("std-edit");
    // A std tree next to the input takes precedence over the installed one.
    let std_source = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../../std");
    copy_dir(&std_source, &project.path("std"));
    project.warm(&[]);
    let cpu = project.path("std/cpu.ag");
    let mut text = std::fs::read_to_string(&cpu).unwrap();
    text.push_str("\ni32 manifest_probe() {\n    return 1;\n}\n");
    std::fs::write(&cpu, text).unwrap();
    project.assert_rebuilt_like_fresh(&[]);
}

#[test]
fn added_and_removed_import_files_invalidate() {
    let project = Project::new("import-set");
    // `lib.helper` resolves through the include dir until a file next to the
    // input shadows it.
    std::fs::remove_file(project.path("helper.ag")).unwrap();
    project.write(
        "inc/lib/helper.ag",
        "i32 helper_value() {\n    return 4;\n}\n",
    );
    project.write(
        "main.ag",
        "import lib.helper;\n\ni32 main() {\n    return helper_value();\n}\n",
    );
    let args = ["-I", "inc"];
    project.warm(&args);

    project.write("lib/helper.ag", "i32 helper_value() {\n    return 6;\n}\n");
    project.assert_rebuilt_like_fresh(&args);

    project.warm(&args);
    std::fs::remove_file(project.path("lib/helper.ag")).unwrap();
    project.assert_rebuilt_like_fresh(&args);

    project.warm(&args);
    std::fs::remove_file(project.path("inc/lib/helper.ag")).unwrap();
    let removed = project.cached(&args);
    assert!(!removed.success, "build with a missing import succeeded");
}

#[test]
fn changed_flags_and_target_invalidate() {
    let project = Project::new("flags");
    project.warm(&[]);
    project.assert_rebuilt_like_fresh(&["-O2"]);
    project.assert_rebuilt_like_fresh(&["--cfg", "manifest_probe"]);
    project.assert_rebuilt_like_fresh(&["--target", "aarch64-unknown-linux-gnu"]);
    // The first configuration is still cached under its own key.
    assert!(project.cached(&[]).reused());
}

#[test]
fn new_compiler_build_invalidates() {
    let project = Project::new("compiler");
    let compiler = project.path("bin/agc");
    std::fs::create_dir_all(compiler.parent().unwrap()).unwrap();
    std::fs::copy(&project.compiler, &compiler).unwrap();
    let first = project.cached_with(&compiler, &[]);
    assert!(first.success, "{}", first.stderr);
    assert!(project.cached_with(&compiler, &[]).reused());
    let objects_before = std::fs::read_dir(project.path("cache/obj"))
        .unwrap()
        .count();

    // A rebuilt binary at the same path gets a new modification time.
    let later = std::time::SystemTime::now() + std::time::Duration::from_secs(5);
    std::fs::File::options()
        .write(true)
        .open(&compiler)
        .unwrap()
        .set_modified(later)
        .unwrap();
    let rebuilt = project.cached_with(&compiler, &[]);
    assert!(rebuilt.success, "{}", rebuilt.stderr);
    assert!(
        !rebuilt.reused(),
        "manifest of another compiler build reused"
    );
    // Module and root objects are keyed by the binary too, so none is reused.
    let objects_after = std::fs::read_dir(project.path("cache/obj"))
        .unwrap()
        .count();
    assert_eq!(objects_after, 2 * objects_before);
    assert!(rebuilt.object == first.object, "rebuilt object differs");
}

fn copy_dir(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).unwrap();
    for entry in std::fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), target).unwrap();
        }
    }
}
