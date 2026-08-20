//! XDG-compliant, content-addressed on-disk cache store for Silver compiler artifacts.
//!
//! Handles cryptographic SHA-256 content key generation, XDG directory discovery,
//! and atomic cache reads/writes for compiled module metadata (`.agm`) and
//! native object code (`.o`).

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

// ===========================================================================
// Pure-Rust Zero-Dependency SHA-256 Implementation (FIPS 180-4)
// ===========================================================================

#[derive(Clone, Debug)]
pub struct Sha256 {
    state: [u32; 8],
    count: u64,
    buffer: [u8; 64],
}

impl Default for Sha256 {
    fn default() -> Self {
        Self::new()
    }
}

impl Sha256 {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];

    pub fn new() -> Self {
        Self {
            state: [
                0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c,
                0x1f83d9ab, 0x5be0cd19,
            ],
            count: 0,
            buffer: [0u8; 64],
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        let mut input = data;
        let mut buf_idx = (self.count & 0x3f) as usize;
        self.count += input.len() as u64;

        if buf_idx > 0 {
            let space = 64 - buf_idx;
            if input.len() < space {
                self.buffer[buf_idx..buf_idx + input.len()].copy_from_slice(input);
                return;
            }
            self.buffer[buf_idx..64].copy_from_slice(&input[..space]);
            let block = self.buffer;
            self.transform(&block);
            input = &input[space..];
            buf_idx = 0;
        }

        while input.len() >= 64 {
            let block: [u8; 64] = input[..64].try_into().unwrap();
            self.transform(&block);
            input = &input[64..];
        }

        if !input.is_empty() {
            self.buffer[buf_idx..buf_idx + input.len()].copy_from_slice(input);
        }
    }

    pub fn finalize(mut self) -> [u8; 32] {
        let bit_count = self.count * 8;
        let buf_idx = (self.count & 0x3f) as usize;

        let pad_len = if buf_idx < 56 {
            56 - buf_idx
        } else {
            120 - buf_idx
        };

        let mut padding = [0u8; 64];
        padding[0] = 0x80;
        self.update(&padding[..pad_len]);

        let length_bytes = bit_count.to_be_bytes();
        self.update(&length_bytes);

        let mut result = [0u8; 32];
        for (i, word) in self.state.iter().enumerate() {
            result[i * 4..(i + 1) * 4].copy_from_slice(&word.to_be_bytes());
        }
        result
    }

    pub fn digest(data: &[u8]) -> [u8; 32] {
        let mut hasher = Self::new();
        hasher.update(data);
        hasher.finalize()
    }

    pub fn digest_hex(data: &[u8]) -> String {
        bytes_to_hex(&Self::digest(data))
    }

    fn transform(&mut self, block: &[u8; 64]) {
        let mut w = [0u32; 64];
        for i in 0..16 {
            w[i] = u32::from_be_bytes(block[i * 4..(i + 1) * 4].try_into().unwrap());
        }
        for i in 16..64 {
            let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
            let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }

        let mut a = self.state[0];
        let mut b = self.state[1];
        let mut c = self.state[2];
        let mut d = self.state[3];
        let mut e = self.state[4];
        let mut f = self.state[5];
        let mut g = self.state[6];
        let mut h = self.state[7];

        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ (!e & g);
            let temp1 = h
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(Self::K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);

            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }

        self.state[0] = self.state[0].wrapping_add(a);
        self.state[1] = self.state[1].wrapping_add(b);
        self.state[2] = self.state[2].wrapping_add(c);
        self.state[3] = self.state[3].wrapping_add(d);
        self.state[4] = self.state[4].wrapping_add(e);
        self.state[5] = self.state[5].wrapping_add(f);
        self.state[6] = self.state[6].wrapping_add(g);
        self.state[7] = self.state[7].wrapping_add(h);
    }
}

fn bytes_to_hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

// ===========================================================================
// Content-Addressed Cache Key
// ===========================================================================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CacheKey {
    pub hash_hex: String,
    pub module_name: String,
}

impl CacheKey {
    pub fn new(module_name: impl Into<String>, hash_hex: impl Into<String>) -> Self {
        Self {
            module_name: module_name.into(),
            hash_hex: hash_hex.into(),
        }
    }
}

pub struct CacheKeyBuilder {
    module_name: String,
    hasher: Sha256,
}

impl CacheKeyBuilder {
    pub fn new(module_name: impl Into<String>) -> Self {
        let mut builder = Self {
            module_name: module_name.into(),
            hasher: Sha256::new(),
        };
        builder.add_str("module_name");
        builder.add_str(&builder.module_name.clone());
        builder
    }

    pub fn add_str(&mut self, text: &str) -> &mut Self {
        self.hasher.update(&(text.len() as u64).to_be_bytes());
        self.hasher.update(text.as_bytes());
        self
    }

    pub fn add_bytes(&mut self, bytes: &[u8]) -> &mut Self {
        self.hasher.update(&(bytes.len() as u64).to_be_bytes());
        self.hasher.update(bytes);
        self
    }

    pub fn add_file(&mut self, path: &Path) -> io::Result<&mut Self> {
        let mut file = fs::File::open(path)?;
        let mut buffer = [0u8; 8192];
        let mut total: u64 = 0;
        let mut content_hasher = Sha256::new();
        loop {
            let n = file.read(&mut buffer)?;
            if n == 0 {
                break;
            }
            content_hasher.update(&buffer[..n]);
            total += n as u64;
        }
        let digest = content_hasher.finalize();
        self.add_str(path.to_str().unwrap_or_default());
        self.hasher.update(&total.to_be_bytes());
        self.hasher.update(&digest);
        Ok(self)
    }

    pub fn add_compiler_version(&mut self, version: &str) -> &mut Self {
        self.add_str("compiler_version");
        self.add_str(version);
        self
    }

    pub fn add_target(&mut self, target: &str) -> &mut Self {
        self.add_str("target");
        self.add_str(target);
        self
    }

    pub fn add_opt_level(&mut self, opt: Option<&str>) -> &mut Self {
        self.add_str("opt_level");
        self.add_str(opt.unwrap_or("0"));
        self
    }

    pub fn add_flags(&mut self, flags: &[String]) -> &mut Self {
        self.add_str("flags");
        for flag in flags {
            self.add_str(flag);
        }
        self
    }

    pub fn finish(self) -> CacheKey {
        let digest = self.hasher.finalize();
        let hash_hex = bytes_to_hex(&digest);
        CacheKey::new(self.module_name, hash_hex)
    }
}

// ===========================================================================
// XDG Directory Resolution & Cache Store
// ===========================================================================

#[derive(Debug, Clone)]
pub struct CachedModule {
    pub key: CacheKey,
    pub agm_path: PathBuf,
    pub obj_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CacheStore {
    root_dir: PathBuf,
}

impl CacheStore {
    /// Resolves the default XDG-compliant cache root directory:
    /// 1. `$SILVER_CACHE_DIR` if set.
    /// 2. `$XDG_CACHE_HOME/silver` if `$XDG_CACHE_HOME` is set.
    /// 3. Platform fallback:
    ///    - Windows: `%LOCALAPPDATA%/silver/cache` or `~/.cache/silver`
    ///    - macOS: `~/Library/Caches/silver`
    ///    - Linux/Other: `~/.cache/silver`
    pub fn default_cache_dir() -> PathBuf {
        if let Some(override_dir) = std::env::var_os("SILVER_CACHE_DIR") {
            return PathBuf::from(override_dir);
        }

        if let Some(xdg_cache) = std::env::var_os("XDG_CACHE_HOME") {
            return PathBuf::from(xdg_cache).join("silver");
        }

        #[cfg(target_os = "windows")]
        {
            if let Some(local_app_data) = std::env::var_os("LOCALAPPDATA") {
                return PathBuf::from(local_app_data).join("silver").join("cache");
            }
        }

        #[cfg(target_os = "macos")]
        {
            if let Some(home) = std::env::var_os("HOME") {
                return PathBuf::from(home).join("Library").join("Caches").join("silver");
            }
        }

        if let Some(home) = std::env::var_os("HOME") {
            PathBuf::from(home).join(".cache").join("silver")
        } else {
            PathBuf::from(".silver_cache")
        }
    }

    pub fn new() -> io::Result<Self> {
        Self::with_dir(Self::default_cache_dir())
    }

    pub fn with_dir(root_dir: PathBuf) -> io::Result<Self> {
        let store = Self { root_dir };
        store.ensure_dirs()?;
        Ok(store)
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn agm_dir(&self) -> PathBuf {
        self.root_dir.join("agm")
    }

    pub fn obj_dir(&self) -> PathBuf {
        self.root_dir.join("obj")
    }

    pub fn tmp_dir(&self) -> PathBuf {
        self.root_dir.join("tmp")
    }

    pub fn ensure_dirs(&self) -> io::Result<()> {
        fs::create_dir_all(self.agm_dir())?;
        fs::create_dir_all(self.obj_dir())?;
        fs::create_dir_all(self.tmp_dir())?;
        Ok(())
    }

    pub fn agm_path(&self, key: &CacheKey) -> PathBuf {
        self.agm_dir().join(format!("{}.agm", key.hash_hex))
    }

    pub fn obj_path(&self, key: &CacheKey) -> PathBuf {
        self.obj_dir().join(format!("{}.o", key.hash_hex))
    }

    /// Query the cache for a compiled module and its corresponding object file.
    pub fn get(&self, key: &CacheKey) -> Option<CachedModule> {
        let agm_path = self.agm_path(key);
        let obj_path = self.obj_path(key);

        if agm_path.is_file() && obj_path.is_file() {
            Some(CachedModule {
                key: key.clone(),
                agm_path,
                obj_path,
            })
        } else {
            None
        }
    }

    /// Atomically writes `.agm` metadata and `.o` object artifacts to the cache.
    pub fn put(&self, key: &CacheKey, agm_bytes: &[u8], obj_bytes: &[u8]) -> io::Result<CachedModule> {
        self.ensure_dirs()?;
        let pid = std::process::id();
        let tmp_agm = self.tmp_dir().join(format!("{}.{pid}.tmp.agm", key.hash_hex));
        let tmp_obj = self.tmp_dir().join(format!("{}.{pid}.tmp.o", key.hash_hex));

        fs::write(&tmp_agm, agm_bytes)?;
        fs::write(&tmp_obj, obj_bytes)?;

        let final_agm = self.agm_path(key);
        let final_obj = self.obj_path(key);

        // Atomic rename into place
        fs::rename(&tmp_agm, &final_agm)?;
        fs::rename(&tmp_obj, &final_obj)?;

        Ok(CachedModule {
            key: key.clone(),
            agm_path: final_agm,
            obj_path: final_obj,
        })
    }

    /// Cleans temporary staging files left behind by interrupted runs.
    pub fn clean_tmp(&self) -> io::Result<usize> {
        let mut count = 0;
        if let Ok(entries) = fs::read_dir(self.tmp_dir()) {
            for entry in entries.flatten() {
                if let Ok(file_type) = entry.file_type() {
                    if file_type.is_file() && fs::remove_file(entry.path()).is_ok() {
                        count += 1;
                    }
                }
            }
        }
        Ok(count)
    }
}

// ===========================================================================
// Unit Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_standard_vectors() {
        // NIST Test Vectors
        assert_eq!(
            Sha256::digest_hex(b""),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            Sha256::digest_hex(b"abc"),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(
            Sha256::digest_hex(b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        );
    }

    #[test]
    fn test_cache_key_determinism_and_sensitivity() {
        let mut builder1 = CacheKeyBuilder::new("std.net.tcp");
        builder1
            .add_compiler_version("0.2.1")
            .add_target("x86_64-unknown-linux-gnu")
            .add_opt_level(Some("2"))
            .add_bytes(b"struct TcpStream { i32 fd; }");
        let key1 = builder1.finish();

        let mut builder2 = CacheKeyBuilder::new("std.net.tcp");
        builder2
            .add_compiler_version("0.2.1")
            .add_target("x86_64-unknown-linux-gnu")
            .add_opt_level(Some("2"))
            .add_bytes(b"struct TcpStream { i32 fd; }");
        let key2 = builder2.finish();

        assert_eq!(key1, key2);

        // Different opt level produces different key
        let mut builder3 = CacheKeyBuilder::new("std.net.tcp");
        builder3
            .add_compiler_version("0.2.1")
            .add_target("x86_64-unknown-linux-gnu")
            .add_opt_level(Some("3"))
            .add_bytes(b"struct TcpStream { i32 fd; }");
        let key3 = builder3.finish();

        assert_ne!(key1, key3);
    }

    #[test]
    fn test_cache_store_put_get_and_atomic() {
        let tmp_root = std::env::temp_dir().join("silver_cache_test_store");
        let _ = fs::remove_dir_all(&tmp_root);

        let store = CacheStore::with_dir(tmp_root.clone()).expect("init cache store");
        let key = CacheKey::new("test.module", "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789");

        assert!(store.get(&key).is_none());

        let agm_data = b"AGM\x00\x00\x02mock_metadata";
        let obj_data = b"\x7fELFmock_object_code";

        let cached = store.put(&key, agm_data, obj_data).expect("put cache entry");
        assert!(cached.agm_path.is_file());
        assert!(cached.obj_path.is_file());

        let fetched = store.get(&key).expect("cache hit");
        assert_eq!(fs::read(&fetched.agm_path).unwrap(), agm_data);
        assert_eq!(fs::read(&fetched.obj_path).unwrap(), obj_data);

        let _ = fs::remove_dir_all(&tmp_root);
    }
}
