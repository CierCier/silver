# DNS Standard Compliance

- **Primary Specifications**: [RFC 1034 (Domain Names - Concepts and Facilities)](https://datatracker.ietf.org/doc/html/rfc1034), [RFC 1035 (Domain Names - Implementation and Specification)](https://datatracker.ietf.org/doc/html/rfc1035).
- **Implementation**: [`std/net/dns.ag`](file:///home/cier/Projects/silver/std/net/dns.ag) (`dns_resolve`, `dns_build_query`, `dns_parse_a`).
- **Test Suite**: [`tests/net_dns_test.ag`](file:///home/cier/Projects/silver/tests/net_dns_test.ag).

## Compliance Table

| Feature / Capability | RFC 1035 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **DNS A-Record Queries** | §4.1.1, §4.1.2 | Supported | `dns_build_query` | Constructs wire-format header with random `txid`, `RD` (recursion desired), and QNAME dot-separated labels. |
| **A-Record Response Parsing** | §4.1.3 | Supported | `dns_parse_a` | Verifies `QR` response flag, confirms `RCODE == 0`, parses answers, handles label compression pointers (`0xC0`), and extracts 4-byte IPv4 address. |
| **System Nameserver Discovery** | POSIX / Unix | Supported | `dns_parse_resolv_conf` | Parses `nameserver <ip>` lines from `/etc/resolv.conf`; falls back to `8.8.8.8` if missing or unreadable. |
| **TTL Cache Management** | §7.3 | Supported | `dns_cache_get`, `dns_cache_put` | Caches successful resolutions with expiration timestamps honoring upstream TTLs (minimum 60s). |
| **UDP Transport Roundtrip** | RFC 768 / §4.2.1 | Supported | `dns_try_ns` | Uses `UdpSocket.new()` with socket receive timeouts, cycling through configured nameservers until success. |
