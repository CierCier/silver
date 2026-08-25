//! Keyword recognition via a minimal-hash table.
//!
//! Built at spec-construction time from `(word, kind)` pairs. Lookup hashes
//! the word bytes (FNV-1a — fast, decent distribution on identifiers),
//! probes an open-addressed table, and confirms with a byte comparison.
//! No allocations during lookup; the hot path is two loads and a compare.

/// Word → kind map optimized for identifier-shaped lookups.
#[derive(Debug, Clone, Default)]
pub struct KeywordMap {
    slots: Vec<Slot>,
    /// Backing storage for keyword bytes; slots hold offsets into this.
    arena: Vec<u8>,
    /// `slots.len() - 1` for index masking.
    mask: u32,
}

#[derive(Debug, Clone, Copy, Default)]
struct Slot {
    hash: u32,
    word_off: u32,
    word_len: u32,
    kind: u16,
    occupied: bool,
}

impl KeywordMap {
    pub fn new(keywords: &[(&str, u16)]) -> Self {
        let capacity = (keywords.len() * 2).next_power_of_two().max(8);
        let mut map = KeywordMap {
            slots: vec![Slot::default(); capacity],
            arena: Vec::with_capacity(64 * keywords.len()),
            mask: (capacity - 1) as u32,
        };
        for (word, kind) in keywords {
            let word_bytes = word.as_bytes();
            let off = map.arena.len() as u32;
            map.arena.extend_from_slice(word_bytes);
            let hash = fnv1a(word_bytes);
            let mut idx = (hash & map.mask) as usize;
            while map.slots[idx].occupied {
                idx = (idx + 1) & map.mask as usize;
            }
            map.slots[idx] = Slot {
                hash,
                word_off: off,
                word_len: word_bytes.len() as u32,
                kind: *kind,
                occupied: true,
            };
        }
        map
    }

    /// Exact-match lookup of `word`. Returns the spec's kind when the word
    /// is a keyword; `None` means plain identifier.
    #[inline]
    pub fn get(&self, word: &[u8]) -> Option<u16> {
        if self.slots.is_empty() {
            return None;
        }
        let hash = fnv1a(word);
        let mut idx = (hash & self.mask) as usize;
        loop {
            let slot = &self.slots[idx];
            if !slot.occupied {
                return None;
            }
            if slot.hash == hash {
                let stored = &self.arena[slot.word_off as usize..][..slot.word_len as usize];
                if stored == word {
                    return Some(slot.kind);
                }
            }
            idx = (idx + 1) & self.mask as usize;
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }
}

#[inline]
fn fnv1a(bytes: &[u8]) -> u32 {
    let mut hash: u32 = 0x811C9DC5;
    for &b in bytes {
        hash ^= b as u32;
        hash = hash.wrapping_mul(0x01000193);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_and_miss() {
        let map = KeywordMap::new(&[("fn", 1), ("let", 2), ("Self", 3)]);
        assert_eq!(map.get(b"fn"), Some(1));
        assert_eq!(map.get(b"let"), Some(2));
        assert_eq!(map.get(b"Self"), Some(3));
        assert_eq!(map.get(b"le"), None);
        assert_eq!(map.get(b"lett"), None);
        assert_eq!(map.get(b"ident"), None);
    }

    #[test]
    fn empty_map() {
        let map = KeywordMap::default();
        assert_eq!(map.get(b"anything"), None);
    }
}
