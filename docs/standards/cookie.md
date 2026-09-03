# HTTP State Management (Cookies) Standard Compliance

- **Primary Specifications**: [RFC 6265 (HTTP State Management Mechanism)](https://datatracker.ietf.org/doc/html/rfc6265).
- **Implementation**: [`std/net/cookie.ag`](file:///home/cier/Projects/silver/std/net/cookie.ag) (`CookieJar`, `Cookie`).
- **Test Suite**: [`tests/cookie_test.ag`](file:///home/cier/Projects/silver/tests/cookie_test.ag).

## Compliance Table

| Feature / Capability | RFC 6265 Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Set-Cookie Header Parsing** | §5.2 | Supported | `Cookie.parse(header)` | Extracts cookie name, value, `Path`, `Domain`, `Max-Age`, `Secure`, and `HttpOnly` attributes. |
| **Cookie Header Serialization** | §5.4 | Supported | `jar.cookies_for(host, path, https)` | Concatenates matching cookies into a single `Cookie: name=val; name2=val2` request header string. |
| **Domain Scoping & Matching** | §5.1.3 | Supported | `cookie.domain_matches(host)` | Implements canonical exact match and domain-suffix matching (ignoring leading dots per §5.2.3). |
| **Path Scoping & Matching** | §5.1.4 | Supported | `cookie.path_matches(req_path)` | Verifies that request URI path starts with the cookie's path attribute. |
| **Secure Attribute Scoping** | §4.1.2.5 | Supported | `cookie.secure` | Excludes secure cookies from requests sent over non-HTTPS connections. |
| **Max-Age & Cookie Expiration** | §5.2.2 | Supported | `jar.set_cookies(...)` | Computes expiration epoch time using `Max-Age` seconds; purges expired entries automatically. |
| **Thread-Safe Cookie Jar Storage** | §5.3 | Supported | `CookieJar` | Stores cookies scoped per host and path, protected by an internal `Mutex`. |
