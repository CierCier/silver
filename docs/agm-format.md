# AGM module artifact format

Stage1 reads every format version listed below and publishes **v11 only**. A format change must add a new six-byte magic/version, update the stage0 encoder and decoder plus the stage1 supported-version predicates and field gates, and add a concrete cross-stage roundtrip fixture before the new version is published. Never reinterpret an existing version's record layout or silently write an older version for compatibility.

All integers are little-endian. Strings are `u32 byte_length` followed by exactly that many UTF-8 bytes (no terminator). Optional strings are a one-byte tag (`0` absent, `1` present) followed by a string for tag `1`. Counts are `u32`. Boolean fields occupy one byte.

The current v11 file is laid out as:

1. Six bytes `41 47 4d 00 00 0b` (`AGM\0\0\x0b`).
2. Module name, module path, source path strings.
3. Source FNV-1a-64 hash as a little-endian `u64`.
4. Compiler-version and target-triple strings.
5. Static-library and shared-library booleans.
6. Direct dependency count and strings; transitive dependency count and strings.
7. Export count, then each export in deterministic `(kind byte, name, signature)` ascending order: kind byte (`1` function, `2` struct, `3` enum, `4` trait, `5` constant, `6` global, `7` type alias), name and signature strings, type-parameter count and strings, optional link-name, optional ABI byte (`1` C, `2` Silver, `3` system, `4` Rust, `5` cdecl, `6` stdcall, `7` fastcall), variadic boolean, optional canonical type key, fields (count then name/type-key strings and tag maps), optional layout (size and alignment optional `u64`s, then packed boolean), optional enum backing type, variants (name, signed 128-bit little-endian value, payload type strings, payload fields), trait items (name/signature pairs), optional constant value, mutable boolean, and optional implementation-trait string.
8. Native-library count and strings; native-library-path count and strings; generic-template count and strings.

For older supported versions the decoder uses explicit per-version gates, not numeric ranges: v2 has no field tags, packed bit, constants/globals, library paths, or generic templates; v6 adds field tags; v7 adds library paths; v8 adds constant/global data; v9 adds no new record fields; v10 adds the packed-layout bit; v11 adds implementation-trait provenance and generic templates. Unsupported versions are rejected.

Stage0's v9 reader accepts a legacy four-byte all-zero trailer present in the
vendored v9 artifacts. Stage1 accepts exactly that v9-only trailer; other
trailing bytes remain invalid.

The current stage1 writer publishes free, non-generic function exports with
primitive scalar or unit parameter and result types. It emits the
reader-compatible v11 envelope, sorts exports by kind/name/signature, and writes
empty/default values only for function fields that stage1 does not yet model.
Unsupported signatures, export kinds, and generic parameters fail publication
rather than producing misleading artifacts. This is not yet a lossless writer
for stage0's rich struct/enum/ABI/native-library metadata; consumers needing
those fields must continue to use stage0-produced artifacts.

`artifact-source` records the FNV-1a-64 hash of the source bytes. Both stage1
publication commands write `compiler_version="foreign"` and
`target_triple="unknown"`. Stage0 treats these as portable markers, skipping
compiler/source freshness and target-triple rejection for stage1 projections.

