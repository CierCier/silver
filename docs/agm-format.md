# AGM module artifact format

Both compilers read and publish **v12 only**. This version requires the internal borrow-state reference ABI. Rebuild older modules and their compiled objects before importing them. The metadata record layout is unchanged from v11; the new header prevents linking objects whose references use the old thin internal ABI. Extern-C references retain their thin-pointer ABI.

All integers are little-endian. Strings are `u32 byte_length` followed by exactly that many UTF-8 bytes (no terminator). Optional strings are a one-byte tag (`0` absent, `1` present) followed by a string for tag `1`. Counts are `u32`. Boolean fields occupy one byte.

The current v12 file is laid out as:

1. Six bytes `41 47 4d 00 00 0c` (`AGM\0\0\x0c`).
2. Module name, module path, source path strings.
3. Source FNV-1a-64 hash as a little-endian `u64`.
4. Compiler-version and target-triple strings.
5. Static-library and shared-library booleans.
6. Direct dependency count and strings; transitive dependency count and strings.
7. Export count, then each export in deterministic `(kind byte, name, signature)` ascending order: kind byte (`1` function, `2` struct, `3` enum, `4` trait, `5` constant, `6` global, `7` type alias), name and signature strings, type-parameter count and strings, optional link-name, optional ABI byte (`1` C, `2` Silver, `3` system, `4` Rust, `5` cdecl, `6` stdcall, `7` fastcall), variadic boolean, optional canonical type key, fields (count then name/type-key strings and tag maps), optional layout (size and alignment optional `u64`s, then packed boolean), optional enum backing type, variants (name, signed 128-bit little-endian value, payload type strings, payload fields), trait items (name/signature pairs), optional constant value, mutable boolean, and optional implementation-trait string.
8. Native-library count and strings; native-library-path count and strings; generic-template count and strings.

Stage0 stores source declarations in the template section for generic items and
all trait implementations. Complete implementation declarations preserve trait
arguments, owner parameters, and bounds, including implementations with no methods.
Bound checking uses these templates rather than inferring an implementation from
the optional trait name on a method export.

Versions through v11 are rejected, including their historical trailer variants.
Stage0 cache keys include `borrow-state-abi1`; native object cache keys use
`silver-stage1-native-object-v4-borrow-state-abi1`. Neither compiler may reuse an
object compiled with the earlier internal reference ABI.

The current stage1 writer publishes free, non-generic function exports with
primitive scalar or unit parameter and result types. It emits the
reader-compatible v12 envelope, sorts exports by kind/name/signature, and writes
empty/default values only for function fields that stage1 does not yet model.
`artifact-source` rejects unsupported signatures, export kinds, and generic
parameters. `build --emit=module` omits unsupported exports from this projection
while still emitting the compiled object. This is not yet a lossless writer
for stage0's rich struct/enum/ABI/native-library metadata; consumers needing
those fields must continue to use stage0-produced artifacts.

`artifact-source` records the FNV-1a-64 hash of the source bytes. Both stage1
publication commands write `compiler_version="foreign"` and
`target_triple="unknown"`. Stage0 treats these as portable markers, skipping
compiler/source freshness and target-triple rejection for stage1 projections.
