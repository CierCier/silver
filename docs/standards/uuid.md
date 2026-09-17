# Universally Unique Identifier (UUID) Standard Compliance

- **Primary Specification**: [RFC 9562 (Universally Unique IDentifiers (UUIDs))](https://www.rfc-editor.org/rfc/rfc9562.html) (obsoletes RFC 4122).
- **Implementation**: [`std/uuid.ag`](file:///home/cier/Projects/silver/std/uuid.ag) (`Uuid`).
- **Test Suite**: [`tests/uuid_test.ag`](file:///home/cier/Projects/silver/tests/uuid_test.ag).

## Overview

The `std.uuid` module provides generation, parsing, serialization, and comparison for Universally Unique Identifiers as defined by **RFC 9562**. It models a 128-bit identifier stored as a fixed 16-byte array in big-endian network byte order.

Following modern systems language standards (such as Go 1.27+ `uuid` and Rust `uuid`), the module focuses on modern, secure, and index-friendly specifications:
- **UUIDv4**: Cryptographically secure random UUIDs generated via OS CSPRNG.
- **UUIDv7**: Epoch-timestamp-ordered UUIDs (Unix millisecond timestamp + CSPRNG entropy) for high-performance B-tree and LSM-tree database primary keys.
- **Nil & Max UUIDs**: Special sentinel values (all-zeros and all-ones).

---

## Compliance Table

| Feature / Capability | RFC 9562 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Binary Representation** | §4 | Supported | `struct Uuid { u8 bytes[16]; }` | 128-bit unsigned octet array in big-endian network byte order; zero-overhead Copy type. |
| **UUID Version 4 (Random)** | §5.4 | Supported | `Uuid.v4()`, `Uuid.new()` | 122 bits of cryptographic entropy from `std.random: random_bytes`; 4-bit version set to `0100b`, 2-bit variant set to `10b`. |
| **UUID Version 7 (Time-Ordered)** | §5.7 | Supported | `Uuid.v7()`, `Uuid.v7_at(ms)` | 48-bit millisecond Unix timestamp prefix (big-endian) + 74 bits of random entropy; chronological monotonicity across milliseconds. |
| **Nil UUID** | §5.9 | Supported | `Uuid.nil()`, `Uuid.zero()`, `uuid.is_nil()` | All 128 bits set to zero (`00000000-0000-0000-0000-000000000000`). |
| **Max UUID** | §5.10 | Supported | `Uuid.max()`, `uuid.is_max()` | All 128 bits set to one (`ffffffff-ffff-ffff-ffff-ffffffffffff`). |
| **Canonical String Formatting** | §4 | Supported | `uuid.to_string()`, `uuid.write_buf(out)` | Outputs standard 36-character hyphenated lowercase hex (`8-4-4-4-12`). `write_buf` operates with zero heap allocations. |
| **Simple Hex Formatting** | §4 | Supported | `uuid.to_simple_string()`, `uuid.write_simple_buf(out)` | Outputs 32-character unhyphenated lowercase hex. `write_simple_buf` operates with zero heap allocations. |
| **Permissive Parsing** | §4 | Supported | `Uuid.parse(str)`, `Uuid.must_parse(str)` | Parses canonical 36-char hyphenated, 32-char simple hex, 38-char braced (`{...}`), and 45-char URN (`urn:uuid:...`). Case-insensitive. |
| **Timestamp Extraction** | §5.7 | Supported | `uuid.timestamp_millis()` | Extracts 48-bit Unix millisecond timestamp from Version 7 UUIDs. |
| **Version & Variant Inspection** | §4.1, §4.2 | Supported | `uuid.version()`, `uuid.variant()` | Extracts 4-bit version (`4`, `7`) and variant (`1` for RFC 9562/4122). |
| **Ordering & Equality** | §4 | Supported | `uuid == other`, `uuid < other` | Lexicographical byte-by-byte unsigned comparison (`memcmp`); aligns with chronological ordering for UUIDv7. |
| **Hashing & Containers** | — | Supported | `@hash(uuid)` | Native compatibility with `HashMap<Uuid, V>` and `HashSet<Uuid>` via compiler `@hash` macro. |
| **Integer & Raw Conversions** | — | Supported | `uuid.to_u128()`, `Uuid.from_u128()`, `uuid.as_ptr()`, `uuid.as_slice()`, `Uuid.from_bytes()` | Bidirectional conversion with `u128` integer and byte buffers. |

---

## Usage Examples

### 1. Generating UUIDs
```silver
import std.uuid;

// Generate a random UUIDv4
Uuid id_v4 = Uuid.v4();
println(id_v4.to_string()); // e.g. "9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"

// Generate a time-ordered UUIDv7 (ideal for database primary keys)
Uuid id_v7 = Uuid.v7();
println(id_v7.to_string()); // e.g. "018e5b42-7a2c-7b9c-843e-c6f376f9d2bb"
```

### 2. Parsing UUIDs
```silver
Result<Uuid, Error> parsed = Uuid.parse("550e8400-e29b-41d4-a716-446655440000");
if (parsed.is_ok()) {
    Uuid u = parsed.unwrap();
    assert(u.version() == 4);
}

// Accepts 32-char hex, braced, and URN formats
Uuid from_simple = Uuid.must_parse("550e8400e29b41d4a716446655440000");
Uuid from_braced = Uuid.must_parse("{550e8400-e29b-41d4-a716-446655440000}");
Uuid from_urn    = Uuid.must_parse("urn:uuid:550e8400-e29b-41d4-a716-446655440000");
```

### 3. Zero-Allocation Stack Formatting
```silver
u8 buf[37];
id_v7.write_buf(&buf[0]);
buf[36] = (u8)0;
str text = (str)&buf[0];
// Use text directly without heap allocations
```

### 4. Database Key & Collection Usage
```silver
import std.map;

HashMap<Uuid, String> users = HashMap<Uuid, String>.new();
Uuid user_id = Uuid.v7();
users.insert(user_id, String.from_str("Alice"));
```
