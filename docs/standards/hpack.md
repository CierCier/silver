# HPACK Header Compression Standard Compliance

- **Primary Specifications**: [RFC 7541 (HPACK: Header Compression for HTTP/2)](https://datatracker.ietf.org/doc/html/rfc7541).
- **Implementation**: [`std/net/hpack.ag`](file:///home/cier/Projects/silver/std/net/hpack.ag) (`HpackContext`, `HpackField`, `hpack_decode`, `hpack_encode`).
- **Test Suite**: [`tests/hpack_test.ag`](file:///home/cier/Projects/silver/tests/hpack_test.ag).

## Compliance Table

| Feature / Capability | RFC 7541 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Static Table** | §2.3.1, Appendix A | Supported | `HPACK_STATIC_TABLE` | Complete static table of 61 pre-defined header name/value pairs. |
| **Dynamic Table Management** | §2.3.2, §4 | Supported | `HpackContext.dynamic_table` | Ring-buffer storage with entry size accounting (`len(name) + len(val) + 32`) and FIFO eviction. |
| **Indexed Header Representation** | §6.1 | Supported | `hpack_decode`, `hpack_encode` | 1-prefix pattern (`1xxxxxxx`) pointing into static or dynamic table entries. |
| **Literal with Incremental Indexing** | §6.2.1 | Supported | `hpack_decode`, `hpack_encode` | 01-prefix pattern (`01xxxxxx`); automatically inserts new entry into dynamic table. |
| **Literal without Indexing** | §6.2.2 | Supported | `hpack_decode`, `hpack_encode` | 0000-prefix pattern (`0000xxxx`); decodes fields without dynamic table insertion. |
| **Variable-Length Integer Encoding** | §5.1 | Supported | `hpack_decode_int`, `hpack_encode_int` | Encodes/decodes arbitrary integers across N-bit prefix byte boundaries. |
| **Raw String Literal Representation** | §5.2 | Supported | `hpack_decode_str`, `hpack_encode_str` | Extracts length-prefixed raw octet sequences. |
| **Huffman Code Decoding** | §5.2, Appendix B | Supported | `hpack_huffman_decode` | Decodes Huffman bitstreams using the canonical RFC 7541 8-bit Huffman code table. |
