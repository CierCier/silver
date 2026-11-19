//! Records the filesystem facts and diagnostics of one build, so a later build
//! can prove that nothing it depends on changed and reuse its result whole
//! (see `build_manifest`).
//!
//! Contract: every frontend step that reads a source or module file, or makes
//! a decision from whether a path exists, goes through the functions below.
//! A raw `std::fs` call in import resolution, module loading or the build
//! graph is a dependency the manifest cannot see.
//!
//! Outside a recording the functions are plain filesystem calls.

use std::io;
use std::path::{Path, PathBuf};

use parking_lot::Mutex;
use rustc_hash::FxHashMap;

use crate::cache_store::Sha256;

pub type Digest = [u8; 32];

/// What a build observed at one path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputFact {
    Missing,
    Directory,
    /// A path that exists but is neither a file nor a directory.
    Other,
    /// A file whose existence mattered but whose bytes were not read.
    File,
    /// A file read with these contents.
    Content(Digest),
}

impl InputFact {
    fn observe_kind(path: &Path) -> Self {
        match std::fs::metadata(path) {
            Ok(meta) if meta.is_file() => InputFact::File,
            Ok(meta) if meta.is_dir() => InputFact::Directory,
            Ok(_) => InputFact::Other,
            Err(_) => InputFact::Missing,
        }
    }

    /// Whether `path` still matches this fact. Read files are read again.
    pub fn still_holds(&self, path: &Path) -> bool {
        match self {
            InputFact::Content(expected) => {
                std::fs::read(path).is_ok_and(|bytes| Sha256::digest(&bytes) == *expected)
            }
            fact => InputFact::observe_kind(path) == *fact,
        }
    }

    /// Combines two observations of one path. `None` means they contradict,
    /// so the path changed while the build ran.
    fn merge(self, other: Self) -> Option<Self> {
        match (self, other) {
            (InputFact::Content(_), InputFact::File) => Some(self),
            (InputFact::File, InputFact::Content(_)) => Some(other),
            (a, b) => (a == b).then_some(a),
        }
    }
}

/// Everything a finished build recorded.
pub struct Recording {
    pub inputs: Vec<(PathBuf, InputFact)>,
    pub diagnostics: Vec<String>,
}

#[derive(Default)]
struct Recorder {
    inputs: FxHashMap<PathBuf, InputFact>,
    diagnostics: Vec<String>,
    /// Set when the build did something a manifest cannot replay or verify.
    unreplayable: bool,
}

static RECORDER: Mutex<Option<Recorder>> = Mutex::new(None);

pub fn begin() {
    *RECORDER.lock() = Some(Recorder::default());
}

/// Stops recording. Returns `None` when nothing was recording or the build
/// cannot be replayed (a contradicting observation or an unreplayable step).
pub fn finish() -> Option<Recording> {
    let recorder = RECORDER.lock().take()?;
    if recorder.unreplayable {
        return None;
    }
    let mut inputs: Vec<_> = recorder.inputs.into_iter().collect();
    inputs.sort_by(|a, b| a.0.cmp(&b.0));
    Some(Recording {
        inputs,
        diagnostics: recorder.diagnostics,
    })
}

fn record(path: &Path, fact: InputFact) {
    let mut guard = RECORDER.lock();
    let Some(recorder) = guard.as_mut() else {
        return;
    };
    let merged = match recorder.inputs.get(path) {
        Some(existing) => existing.merge(fact),
        None => Some(fact),
    };
    match merged {
        Some(fact) => {
            recorder.inputs.insert(path.to_path_buf(), fact);
        }
        None => recorder.unreplayable = true,
    }
}

fn recording() -> bool {
    RECORDER.lock().is_some()
}

/// Marks the current build as impossible to replay from a manifest, e.g.
/// because it ran an external tool or printed output outside `emit_diagnostic`.
pub fn mark_unreplayable() {
    if let Some(recorder) = RECORDER.lock().as_mut() {
        recorder.unreplayable = true;
    }
}

/// Prints a non-fatal diagnostic of a successful build. A reused build prints
/// the same text again, so warnings never disappear on a manifest hit.
pub fn emit_diagnostic(text: String) {
    eprintln!("{text}");
    if let Some(recorder) = RECORDER.lock().as_mut() {
        recorder.diagnostics.push(text);
    }
}

pub fn is_file(path: &Path) -> bool {
    if !recording() {
        return path.is_file();
    }
    let fact = InputFact::observe_kind(path);
    record(path, fact);
    fact == InputFact::File
}

pub fn exists(path: &Path) -> bool {
    if !recording() {
        return path.exists();
    }
    let fact = InputFact::observe_kind(path);
    record(path, fact);
    fact != InputFact::Missing
}

pub fn read(path: &Path) -> io::Result<Vec<u8>> {
    let result = std::fs::read(path);
    if recording() {
        match &result {
            Ok(bytes) => record(path, InputFact::Content(Sha256::digest(bytes))),
            Err(_) => record(path, InputFact::observe_kind(path)),
        }
    }
    result
}

pub fn read_to_string(path: &Path) -> io::Result<String> {
    let bytes = read(path)?;
    String::from_utf8(bytes).map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
}

/// Records the digest of a file the caller already hashed.
pub fn record_file_digest(path: &Path, digest: Digest) {
    record(path, InputFact::Content(digest));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_detects_changed_content() {
        let a = InputFact::Content([1; 32]);
        assert_eq!(a.merge(InputFact::Content([2; 32])), None);
        assert_eq!(a.merge(InputFact::File), Some(a));
        assert_eq!(InputFact::File.merge(a), Some(a));
        assert_eq!(InputFact::Missing.merge(a), None);
        assert_eq!(
            InputFact::Missing.merge(InputFact::Missing),
            Some(InputFact::Missing)
        );
    }
}
