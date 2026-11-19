//! Whole-build manifests: the result of one cached `exe`/`obj` build, stored
//! under a key of the compiler identity, the build options and the import
//! search configuration.
//!
//! A manifest lists every filesystem fact the frontend observed (file
//! contents, and the existence of every path import resolution probed), the
//! cache keys of the root objects, the link inputs and the warnings the build
//! printed. When every fact still holds, a later build with the same key
//! reaches the same frontend result, so it replays the warnings and links the
//! cached objects without parsing anything.

use std::path::PathBuf;

use crate::build_record::{Digest, InputFact};
use crate::cache_store::{CacheKey, CacheStore};

const MAGIC: &[u8] = b"agc-build-manifest-1\n";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildManifest {
    pub inputs: Vec<(PathBuf, InputFact)>,
    /// Object cache keys of the build inputs, in input order.
    pub root_objects: Vec<String>,
    pub dependency_artifacts: Vec<PathBuf>,
    pub native_libs: Vec<String>,
    pub lib_dirs: Vec<PathBuf>,
    /// Non-fatal diagnostics in print order.
    pub diagnostics: Vec<String>,
}

/// A manifest whose facts were verified against the filesystem.
pub struct ReusedBuild {
    pub manifest: BuildManifest,
    /// Cached root objects, in input order.
    pub root_object_paths: Vec<PathBuf>,
}

impl BuildManifest {
    /// Loads the manifest stored under `key` if the build it describes is
    /// still current: every input fact holds and every root object is cached.
    pub fn load_current(store: &CacheStore, key: &CacheKey) -> Option<ReusedBuild> {
        let manifest = Self::decode(&store.get_manifest(key)?)?;
        if !manifest
            .inputs
            .iter()
            .all(|(path, fact)| fact.still_holds(path))
        {
            return None;
        }
        let root_object_paths = manifest
            .root_objects
            .iter()
            .map(|hash| store.get_obj(&CacheKey::new("root", hash.clone())))
            .collect::<Option<Vec<_>>>()?;
        Some(ReusedBuild {
            manifest,
            root_object_paths,
        })
    }

    /// `None` when a path is not UTF-8; such builds are not recorded.
    pub fn encode(&self) -> Option<Vec<u8>> {
        let mut out = MAGIC.to_vec();
        put_len(&mut out, self.inputs.len());
        for (path, fact) in &self.inputs {
            put_str(&mut out, path.to_str()?);
            match fact {
                InputFact::Missing => out.push(0),
                InputFact::Directory => out.push(1),
                InputFact::Other => out.push(2),
                InputFact::File => out.push(3),
                InputFact::Content(digest) => {
                    out.push(4);
                    out.extend_from_slice(digest);
                }
            }
        }
        put_strs(&mut out, self.root_objects.iter().map(String::as_str));
        put_paths(&mut out, &self.dependency_artifacts)?;
        put_strs(&mut out, self.native_libs.iter().map(String::as_str));
        put_paths(&mut out, &self.lib_dirs)?;
        put_strs(&mut out, self.diagnostics.iter().map(String::as_str));
        Some(out)
    }

    pub fn decode(bytes: &[u8]) -> Option<Self> {
        let mut reader = Reader {
            bytes: bytes.strip_prefix(MAGIC)?,
        };
        let input_count = reader.len()?;
        let mut inputs = Vec::new();
        for _ in 0..input_count {
            let path = PathBuf::from(reader.str()?);
            let fact = match reader.take(1)?[0] {
                0 => InputFact::Missing,
                1 => InputFact::Directory,
                2 => InputFact::Other,
                3 => InputFact::File,
                4 => InputFact::Content(Digest::try_from(reader.take(32)?).ok()?),
                _ => return None,
            };
            inputs.push((path, fact));
        }
        let manifest = Self {
            inputs,
            root_objects: reader.strs()?,
            dependency_artifacts: reader.strs()?.into_iter().map(PathBuf::from).collect(),
            native_libs: reader.strs()?,
            lib_dirs: reader.strs()?.into_iter().map(PathBuf::from).collect(),
            diagnostics: reader.strs()?,
        };
        reader.bytes.is_empty().then_some(manifest)
    }
}

fn put_len(out: &mut Vec<u8>, len: usize) {
    out.extend_from_slice(&(len as u64).to_le_bytes());
}

fn put_str(out: &mut Vec<u8>, text: &str) {
    put_len(out, text.len());
    out.extend_from_slice(text.as_bytes());
}

fn put_strs<'a>(out: &mut Vec<u8>, items: impl ExactSizeIterator<Item = &'a str>) {
    put_len(out, items.len());
    for item in items {
        put_str(out, item);
    }
}

fn put_paths(out: &mut Vec<u8>, paths: &[PathBuf]) -> Option<()> {
    let texts = paths
        .iter()
        .map(|path| path.to_str())
        .collect::<Option<Vec<_>>>()?;
    put_strs(out, texts.into_iter());
    Some(())
}

struct Reader<'a> {
    bytes: &'a [u8],
}

impl<'a> Reader<'a> {
    fn take(&mut self, len: usize) -> Option<&'a [u8]> {
        if len > self.bytes.len() {
            return None;
        }
        let (head, rest) = self.bytes.split_at(len);
        self.bytes = rest;
        Some(head)
    }

    fn len(&mut self) -> Option<usize> {
        usize::try_from(u64::from_le_bytes(self.take(8)?.try_into().ok()?)).ok()
    }

    fn str(&mut self) -> Option<String> {
        let len = self.len()?;
        String::from_utf8(self.take(len)?.to_vec()).ok()
    }

    fn strs(&mut self) -> Option<Vec<String>> {
        let count = self.len()?;
        (0..count).map(|_| self.str()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn sample(dir: &Path) -> BuildManifest {
        BuildManifest {
            inputs: vec![
                (dir.join("main.ag"), InputFact::File),
                (dir.join("absent.ag"), InputFact::Missing),
                (dir.to_path_buf(), InputFact::Directory),
            ],
            root_objects: vec!["ab".repeat(32)],
            dependency_artifacts: vec![dir.join("dep.o")],
            native_libs: vec!["m".to_string()],
            lib_dirs: vec![dir.join("lib")],
            diagnostics: vec!["warn: x".to_string()],
        }
    }

    #[test]
    fn manifest_round_trips_and_rejects_truncation() {
        let mut manifest = sample(Path::new("/src"));
        manifest.inputs[0].1 = InputFact::Content([7; 32]);
        let bytes = manifest.encode().unwrap();
        assert_eq!(BuildManifest::decode(&bytes), Some(manifest));
        assert_eq!(BuildManifest::decode(&bytes[..bytes.len() - 1]), None);
        let mut trailing = bytes.clone();
        trailing.push(0);
        assert_eq!(BuildManifest::decode(&trailing), None);
    }

    #[test]
    fn verification_tracks_content_and_existence() {
        let dir = std::env::temp_dir().join(format!("agc-manifest-test-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let source = dir.join("main.ag");
        std::fs::write(&source, "i32 main() { return 0; }").unwrap();

        let content = InputFact::Content(crate::cache_store::Sha256::digest(
            b"i32 main() { return 0; }",
        ));
        assert!(content.still_holds(&source));
        std::fs::write(&source, "i32 main() { return 1; }").unwrap();
        assert!(!content.still_holds(&source));

        let shadow = dir.join("shadow.ag");
        assert!(InputFact::Missing.still_holds(&shadow));
        std::fs::write(&shadow, "").unwrap();
        assert!(!InputFact::Missing.still_holds(&shadow));
        assert!(InputFact::Directory.still_holds(&dir));

        std::fs::remove_dir_all(&dir).unwrap();
    }
}
