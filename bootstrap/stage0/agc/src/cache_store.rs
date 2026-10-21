//! XDG-compliant, content-addressed on-disk cache store for Silver compiler artifacts.
//!
//! Handles cryptographic SHA-256 content key generation, XDG directory discovery,
//! and atomic cache reads/writes for compiled module metadata (`.agm`) and
//! native object code (`.o`).

use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

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

/// Compiler identity for cache keys and artifact compatibility: the crate
/// version plus the git SHA, so codegen fixes invalidate cached artifacts
/// even when the version number has not changed.
pub fn compiler_cache_version() -> String {
    format!(
        "{}+{}",
        env!("CARGO_PKG_VERSION"),
        option_env!("GIT_SHA").unwrap_or("unknown")
    )
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

    pub fn add_bool(&mut self, name: &str, value: bool) -> &mut Self {
        self.add_str(name);
        self.add_str(if value { "1" } else { "0" });
        self
    }

    pub fn add_flags(&mut self, flags: &[String]) -> &mut Self {
        self.add_str("flags");
        for flag in flags {
            self.add_str(flag);
        }
        self
    }

    pub fn add_dependency_hash(&mut self, dep_name: &str, dep_hash: &str) -> &mut Self {
        self.add_str("dep");
        self.add_str(dep_name);
        self.add_str(dep_hash);
        self
    }

    pub fn add_dependencies(&mut self, deps: &[(String, String)]) -> &mut Self {
        self.add_str("dependencies");
        self.hasher.update(&(deps.len() as u64).to_be_bytes());
        let mut sorted = deps.to_vec();
        sorted.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        for (name, hash) in sorted {
            self.add_dependency_hash(&name, &hash);
        }
        self
    }

    pub fn finish(self) -> CacheKey {
        let digest = self.hasher.finalize();
        let hash_hex = bytes_to_hex(&digest);
        CacheKey::new(self.module_name, hash_hex)
    }
}

static STAGING_SEQUENCE: AtomicU64 = AtomicU64::new(0);

fn read_u16_at(bytes: &[u8], offset: usize, little_endian: bool) -> Option<u16> {
    let raw = bytes.get(offset..offset.checked_add(2)?)?;
    Some(if little_endian {
        u16::from_le_bytes(raw.try_into().ok()?)
    } else {
        u16::from_be_bytes(raw.try_into().ok()?)
    })
}

fn read_u32_at(bytes: &[u8], offset: usize, little_endian: bool) -> Option<u32> {
    let raw = bytes.get(offset..offset.checked_add(4)?)?;
    Some(if little_endian {
        u32::from_le_bytes(raw.try_into().ok()?)
    } else {
        u32::from_be_bytes(raw.try_into().ok()?)
    })
}

fn read_u64_at(bytes: &[u8], offset: usize, little_endian: bool) -> Option<u64> {
    let raw = bytes.get(offset..offset.checked_add(8)?)?;
    Some(if little_endian {
        u64::from_le_bytes(raw.try_into().ok()?)
    } else {
        u64::from_be_bytes(raw.try_into().ok()?)
    })
}

fn range_fits(bytes: &[u8], offset: usize, len: usize) -> bool {
    offset
        .checked_add(len)
        .is_some_and(|end| end <= bytes.len())
}

fn elf_object_is_well_formed(bytes: &[u8]) -> bool {
    if !bytes.starts_with(b"\x7fELF") {
        return false;
    }

    let (
        header_len,
        section_len,
        little_endian,
        address_len,
        section_entry_offset,
        section_count_offset,
    ) = match (bytes.get(4), bytes.get(5)) {
        (Some(1), Some(1)) => (52, 40, true, 4, 46, 48),
        (Some(1), Some(2)) => (52, 40, false, 4, 46, 48),
        (Some(2), Some(1)) => (64, 64, true, 8, 58, 60),
        (Some(2), Some(2)) => (64, 64, false, 8, 58, 60),
        _ => return false,
    };
    if bytes.get(6) != Some(&1) || bytes.len() < header_len {
        return false;
    }

    let section_offset = match address_len {
        4 => read_u32_at(bytes, 32, little_endian).map(|value| value as usize),
        8 => read_u64_at(bytes, 40, little_endian).and_then(|value| usize::try_from(value).ok()),
        _ => return false,
    }
    .unwrap_or(usize::MAX);
    let section_entry_len =
        read_u16_at(bytes, section_entry_offset, little_endian).unwrap_or(0) as usize;
    let section_count =
        read_u16_at(bytes, section_count_offset, little_endian).unwrap_or(0) as usize;
    if section_count == 0
        || section_entry_len != section_len
        || !range_fits(
            bytes,
            section_offset,
            section_entry_len.saturating_mul(section_count),
        )
    {
        return false;
    }

    for index in 0..section_count {
        let Some(section) = section_offset
            .checked_add(section_entry_len.saturating_mul(index))
            .and_then(|offset| bytes.get(offset..))
        else {
            return false;
        };
        let section_type = read_u32_at(section, 4, little_endian).unwrap_or(u32::MAX);
        let (data_offset, data_len) = if address_len == 4 {
            (
                read_u32_at(section, 16, little_endian).unwrap_or(u32::MAX) as usize,
                read_u32_at(section, 20, little_endian).unwrap_or(u32::MAX) as usize,
            )
        } else {
            (
                read_u64_at(section, 24, little_endian)
                    .and_then(|value| usize::try_from(value).ok())
                    .unwrap_or(usize::MAX),
                read_u64_at(section, 32, little_endian)
                    .and_then(|value| usize::try_from(value).ok())
                    .unwrap_or(usize::MAX),
            )
        };
        if section_type != 8 && !range_fits(bytes, data_offset, data_len) {
            return false;
        }
    }
    true
}

fn coff_object_is_well_formed(bytes: &[u8]) -> bool {
    if bytes.len() < 20 {
        return false;
    }
    let machine = read_u16_at(bytes, 0, true).unwrap_or(0);
    let section_count = read_u16_at(bytes, 2, true).unwrap_or(0) as usize;
    let symbol_offset = read_u32_at(bytes, 8, true).unwrap_or(u32::MAX) as usize;
    let symbol_count = read_u32_at(bytes, 12, true).unwrap_or(u32::MAX) as usize;
    let optional_len = read_u16_at(bytes, 16, true).unwrap_or(u16::MAX) as usize;
    let section_table = 20usize.saturating_add(optional_len);
    if machine == 0
        || section_count == 0
        || section_count > 96
        || !range_fits(bytes, section_table, section_count.saturating_mul(40))
        || (symbol_count > 0 && !range_fits(bytes, symbol_offset, symbol_count.saturating_mul(18)))
    {
        return false;
    }

    (0..section_count).all(|index| {
        let Some(section) = section_table
            .checked_add(index.saturating_mul(40))
            .and_then(|offset| bytes.get(offset..))
        else {
            return false;
        };
        let size = read_u32_at(section, 16, true).unwrap_or(u32::MAX) as usize;
        let offset = read_u32_at(section, 20, true).unwrap_or(u32::MAX) as usize;
        size == 0 || range_fits(bytes, offset, size)
    })
}

fn macho_object_is_well_formed(bytes: &[u8]) -> bool {
    let (header_len, little_endian) = match bytes.get(..4) {
        Some([0xce, 0xfa, 0xed, 0xfe]) => (28, true),
        Some([0xcf, 0xfa, 0xed, 0xfe]) => (32, true),
        Some([0xfe, 0xed, 0xfa, 0xce]) => (28, false),
        Some([0xfe, 0xed, 0xfa, 0xcf]) => (32, false),
        _ => return false,
    };
    let command_count = read_u32_at(bytes, 16, little_endian).unwrap_or(u32::MAX) as usize;
    let command_len = read_u32_at(bytes, 20, little_endian).unwrap_or(u32::MAX) as usize;
    bytes.len() >= header_len && range_fits(bytes, header_len, command_len) && command_count > 0
}

fn wasm_object_is_well_formed(bytes: &[u8]) -> bool {
    if !bytes.starts_with(b"\0asm\x01\0\0\0") {
        return false;
    }

    let mut cursor = 8;
    while cursor < bytes.len() {
        cursor += 1;
        let mut size = 0usize;
        let mut shift = 0;
        loop {
            let Some(&byte) = bytes.get(cursor) else {
                return false;
            };
            cursor += 1;
            let value = usize::from(byte & 0x7f);
            if shift >= usize::BITS || value > (usize::MAX >> shift) {
                return false;
            }
            size |= value << shift;
            if byte & 0x80 == 0 {
                break;
            }
            shift += 7;
            if shift >= usize::BITS {
                return false;
            }
        }
        let Some(end) = cursor.checked_add(size) else {
            return false;
        };
        if end > bytes.len() {
            return false;
        }
        cursor = end;
    }
    true
}

fn object_bytes_are_well_formed(bytes: &[u8]) -> bool {
    elf_object_is_well_formed(bytes)
        || coff_object_is_well_formed(bytes)
        || macho_object_is_well_formed(bytes)
        || wasm_object_is_well_formed(bytes)
}

fn agm_bytes_are_well_formed(bytes: &[u8]) -> bool {
    crate::module_artifact::ModuleArtifact::from_bytes(bytes).is_ok()
}

fn invalid_artifact_error(kind: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("invalid cached {kind} artifact"),
    )
}

struct StagedFile {
    path: PathBuf,
    published: bool,
}

impl StagedFile {
    fn create(path: PathBuf, bytes: &[u8]) -> io::Result<Self> {
        let staged = Self {
            path,
            published: false,
        };
        let write_result = (|| {
            let mut file = fs::File::create(&staged.path)?;
            file.write_all(bytes)?;
            file.sync_all()
        })();
        if let Err(error) = write_result {
            drop(staged);
            return Err(error);
        }
        Ok(staged)
    }

    fn publish(&mut self, destination: &Path) -> io::Result<()> {
        fs::rename(&self.path, destination)?;
        self.published = true;
        Ok(())
    }
}

impl Drop for StagedFile {
    fn drop(&mut self) {
        if !self.published {
            let _ = fs::remove_file(&self.path);
        }
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

    fn lock_dir(&self) -> PathBuf {
        self.root_dir.join("locks")
    }

    pub fn ensure_dirs(&self) -> io::Result<()> {
        fs::create_dir_all(self.agm_dir())?;
        fs::create_dir_all(self.obj_dir())?;
        fs::create_dir_all(self.tmp_dir())?;
        fs::create_dir_all(self.lock_dir())?;
        Ok(())
    }

    fn open_lock(&self, name: &str) -> io::Result<fs::File> {
        fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(self.lock_dir().join(name))
    }

    // Acquire the store lock before any key lock. Shared store locks allow
    // unrelated keys to proceed while exclusive maintenance waits for all I/O.
    fn lock_store(&self, exclusive: bool) -> io::Result<fs::File> {
        fs::create_dir_all(self.lock_dir())?;
        let lock = self.open_lock("store.lock")?;
        if exclusive {
            lock.lock()?;
        } else {
            lock.lock_shared()?;
        }
        Ok(lock)
    }

    fn lock_key(&self, key: &CacheKey) -> io::Result<fs::File> {
        let lock = self.open_lock(&format!("{}.lock", key.hash_hex))?;
        lock.lock()?;
        Ok(lock)
    }

    fn remove_file_if_exists(path: &Path) -> io::Result<()> {
        match fs::remove_file(path) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
            Err(error) => Err(error),
        }
    }

    fn discard_artifact(path: &Path) {
        let _ = Self::remove_file_if_exists(path);
    }

    fn discard_pair(agm_path: &Path, obj_path: &Path) {
        Self::discard_artifact(agm_path);
        Self::discard_artifact(obj_path);
    }

    fn artifact_is_valid(path: &Path, validate: fn(&[u8]) -> bool) -> bool {
        fs::read(path).is_ok_and(|bytes| validate(&bytes))
    }

    fn staging_path(&self, key: &CacheKey, kind: &str) -> PathBuf {
        let sequence = STAGING_SEQUENCE.fetch_add(1, Ordering::Relaxed);
        self.tmp_dir().join(format!(
            "{}.{}.{}.tmp.{kind}",
            key.hash_hex,
            std::process::id(),
            sequence
        ))
    }

    pub fn agm_path(&self, key: &CacheKey) -> PathBuf {
        self.agm_dir().join(format!("{}.agm", key.hash_hex))
    }

    pub fn obj_path(&self, key: &CacheKey) -> PathBuf {
        self.obj_dir().join(format!("{}.o", key.hash_hex))
    }

    /// Query the cache for a compiled module and its corresponding object file.
    ///
    /// Missing, incomplete, unreadable, or malformed entries are discarded so
    /// the caller recompiles the module from source.
    pub fn get(&self, key: &CacheKey) -> Option<CachedModule> {
        let _store_lock = self.lock_store(false).ok()?;
        let _key_lock = self.lock_key(key).ok()?;
        let agm_path = self.agm_path(key);
        let obj_path = self.obj_path(key);
        let valid = Self::artifact_is_valid(&agm_path, agm_bytes_are_well_formed)
            && Self::artifact_is_valid(&obj_path, object_bytes_are_well_formed);

        if valid {
            Some(CachedModule {
                key: key.clone(),
                agm_path,
                obj_path,
            })
        } else {
            Self::discard_pair(&agm_path, &obj_path);
            None
        }
    }

    /// Publishes `.agm` metadata and its `.o` object to the cache.
    ///
    /// Both files are fully staged before either final path is changed. The
    /// metadata file is published last and is the pair's completion marker.
    pub fn put(
        &self,
        key: &CacheKey,
        agm_bytes: &[u8],
        obj_bytes: &[u8],
    ) -> io::Result<CachedModule> {
        if !agm_bytes_are_well_formed(agm_bytes) {
            return Err(invalid_artifact_error("module metadata"));
        }
        if !object_bytes_are_well_formed(obj_bytes) {
            return Err(invalid_artifact_error("object"));
        }

        let _store_lock = self.lock_store(false)?;
        self.ensure_dirs()?;
        let _key_lock = self.lock_key(key)?;
        let final_agm = self.agm_path(key);
        let final_obj = self.obj_path(key);

        if Self::artifact_is_valid(&final_agm, agm_bytes_are_well_formed)
            && Self::artifact_is_valid(&final_obj, object_bytes_are_well_formed)
        {
            return Ok(CachedModule {
                key: key.clone(),
                agm_path: final_agm,
                obj_path: final_obj,
            });
        }

        let mut staged_agm = StagedFile::create(self.staging_path(key, "agm"), agm_bytes)?;
        let mut staged_obj = StagedFile::create(self.staging_path(key, "o"), obj_bytes)?;

        Self::remove_file_if_exists(&final_agm)?;
        Self::remove_file_if_exists(&final_obj)?;

        if let Err(error) = staged_obj.publish(&final_obj) {
            Self::discard_pair(&final_agm, &final_obj);
            return Err(error);
        }
        if let Err(error) = staged_agm.publish(&final_agm) {
            Self::discard_pair(&final_agm, &final_obj);
            return Err(error);
        }

        Ok(CachedModule {
            key: key.clone(),
            agm_path: final_agm,
            obj_path: final_obj,
        })
    }

    /// Cleans temporary staging files left behind by interrupted runs.
    pub fn clean_tmp(&self) -> io::Result<usize> {
        let _store_lock = self.lock_store(true)?;
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

    /// Checks for a cached standalone object artifact.
    pub fn get_obj(&self, key: &CacheKey) -> Option<PathBuf> {
        let _store_lock = self.lock_store(false).ok()?;
        let _key_lock = self.lock_key(key).ok()?;
        let path = self.obj_path(key);
        if Self::artifact_is_valid(&path, object_bytes_are_well_formed) {
            Some(path)
        } else {
            Self::discard_artifact(&path);
            None
        }
    }

    /// Atomically stores a standalone object artifact.
    pub fn put_obj(&self, key: &CacheKey, obj_bytes: &[u8]) -> io::Result<PathBuf> {
        if !object_bytes_are_well_formed(obj_bytes) {
            return Err(invalid_artifact_error("object"));
        }

        let _store_lock = self.lock_store(false)?;
        self.ensure_dirs()?;
        let _key_lock = self.lock_key(key)?;
        let final_obj = self.obj_path(key);

        if Self::artifact_is_valid(&final_obj, object_bytes_are_well_formed) {
            return Ok(final_obj);
        }

        let mut staged_obj = StagedFile::create(self.staging_path(key, "o"), obj_bytes)?;
        Self::remove_file_if_exists(&final_obj)?;
        staged_obj.publish(&final_obj)?;
        Ok(final_obj)
    }

    /// Compute summary statistics of the cache store.
    pub fn stats(&self) -> io::Result<CacheStats> {
        let _store_lock = self.lock_store(false)?;
        let mut stats = CacheStats {
            root_dir: self.root_dir.clone(),
            ..Default::default()
        };

        let now = std::time::SystemTime::now();
        let mut min_mtime: Option<std::time::SystemTime> = None;
        let mut max_mtime: Option<std::time::SystemTime> = None;

        if let Ok(entries) = fs::read_dir(self.agm_dir()) {
            for entry in entries.flatten() {
                if let Ok(meta) = entry.metadata() {
                    if meta.is_file() {
                        stats.agm_count += 1;
                        stats.agm_bytes += meta.len();
                        if let Ok(modified) = meta.modified() {
                            min_mtime = Some(min_mtime.map_or(modified, |m| m.min(modified)));
                            max_mtime = Some(max_mtime.map_or(modified, |m| m.max(modified)));
                        }
                    }
                }
            }
        }

        if let Ok(entries) = fs::read_dir(self.obj_dir()) {
            for entry in entries.flatten() {
                if let Ok(meta) = entry.metadata() {
                    if meta.is_file() {
                        stats.obj_count += 1;
                        stats.obj_bytes += meta.len();
                        if let Ok(modified) = meta.modified() {
                            min_mtime = Some(min_mtime.map_or(modified, |m| m.min(modified)));
                            max_mtime = Some(max_mtime.map_or(modified, |m| m.max(modified)));
                        }
                    }
                }
            }
        }

        stats.total_bytes = stats.agm_bytes + stats.obj_bytes;
        stats.total_files = stats.agm_count + stats.obj_count;

        stats.oldest_entry_age_secs = min_mtime.and_then(|m| now.duration_since(m).ok()).map(|d| d.as_secs());
        stats.newest_entry_age_secs = max_mtime.and_then(|m| now.duration_since(m).ok()).map(|d| d.as_secs());

        Ok(stats)
    }

    /// Prune cache entries until total disk usage is under `max_bytes`.
    /// Evicts the oldest files first based on modification time (mtime).
    /// Returns the number of files deleted and total bytes freed.
    pub fn prune_to_max_size(&self, max_bytes: u64) -> io::Result<(usize, u64)> {
        let _store_lock = self.lock_store(true)?;
        let mut files: Vec<(PathBuf, u64, std::time::SystemTime)> = Vec::new();
        let mut total_bytes: u64 = 0;

        for dir in &[self.agm_dir(), self.obj_dir()] {
            if let Ok(entries) = fs::read_dir(dir) {
                for entry in entries.flatten() {
                    if let Ok(meta) = entry.metadata() {
                        if meta.is_file() {
                            let len = meta.len();
                            total_bytes += len;
                            let mtime = meta.modified().unwrap_or(std::time::SystemTime::UNIX_EPOCH);
                            files.push((entry.path(), len, mtime));
                        }
                    }
                }
            }
        }

        if total_bytes <= max_bytes {
            return Ok((0, 0));
        }

        // Sort oldest first
        files.sort_by_key(|f| f.2);

        let mut deleted_count = 0;
        let mut freed_bytes = 0;

        for (path, size, _) in files {
            if total_bytes.saturating_sub(freed_bytes) <= max_bytes {
                break;
            }
            if fs::remove_file(&path).is_ok() {
                deleted_count += 1;
                freed_bytes += size;
            }
        }

        Ok((deleted_count, freed_bytes))
    }

    /// Prune cache entries older than `max_age`.
    pub fn prune_older_than(&self, max_age: std::time::Duration) -> io::Result<(usize, u64)> {
        let _store_lock = self.lock_store(true)?;
        let now = std::time::SystemTime::now();
        let mut deleted_count = 0;
        let mut freed_bytes = 0;

        for dir in &[self.agm_dir(), self.obj_dir()] {
            if let Ok(entries) = fs::read_dir(dir) {
                for entry in entries.flatten() {
                    if let Ok(meta) = entry.metadata() {
                        if meta.is_file() {
                            if let Ok(mtime) = meta.modified() {
                                if let Ok(age) = now.duration_since(mtime) {
                                    if age > max_age {
                                        let len = meta.len();
                                        if fs::remove_file(entry.path()).is_ok() {
                                            deleted_count += 1;
                                            freed_bytes += len;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok((deleted_count, freed_bytes))
    }

    /// Delete all cache content and recreate empty directory structure.
    pub fn clean_all(&self) -> io::Result<()> {
        let _store_lock = self.lock_store(true)?;
        // Keep lock inodes stable for processes that open them after this call.
        for directory in [self.agm_dir(), self.obj_dir(), self.tmp_dir()] {
            match fs::remove_dir_all(&directory) {
                Ok(()) => {}
                Err(error) if error.kind() == io::ErrorKind::NotFound => {}
                Err(error) => return Err(error),
            }
        }
        self.ensure_dirs()
    }
}

#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub root_dir: PathBuf,
    pub agm_count: usize,
    pub agm_bytes: u64,
    pub obj_count: usize,
    pub obj_bytes: u64,
    pub total_bytes: u64,
    pub total_files: usize,
    pub oldest_entry_age_secs: Option<u64>,
    pub newest_entry_age_secs: Option<u64>,
}

/// Parse human-readable size strings like "500M", "1G", "100KB", "1048576" into bytes.
pub fn parse_size_to_bytes(s: &str) -> Option<u64> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    let (num_str, unit) = if let Some(stripped) = s.strip_suffix(['G', 'g']) {
        (stripped, 1024 * 1024 * 1024)
    } else if let Some(stripped) = s.strip_suffix("GB").or_else(|| s.strip_suffix("gb")) {
        (stripped, 1024 * 1024 * 1024)
    } else if let Some(stripped) = s.strip_suffix(['M', 'm']) {
        (stripped, 1024 * 1024)
    } else if let Some(stripped) = s.strip_suffix("MB").or_else(|| s.strip_suffix("mb")) {
        (stripped, 1024 * 1024)
    } else if let Some(stripped) = s.strip_suffix(['K', 'k']) {
        (stripped, 1024)
    } else if let Some(stripped) = s.strip_suffix("KB").or_else(|| s.strip_suffix("kb")) {
        (stripped, 1024)
    } else if let Some(stripped) = s.strip_suffix(['B', 'b']) {
        (stripped, 1)
    } else {
        (s, 1024 * 1024) // Default unit is MB
    };
    let num: f64 = num_str.trim().parse().ok()?;
    Some((num * unit as f64) as u64)
}

// ===========================================================================
// Unit Tests
// ===========================================================================
