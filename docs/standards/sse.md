# Server-Sent Events (SSE) Standard Compliance

- **Primary Specifications**: [WHATWG HTML Living Standard §9.2 (Server-sent events)](https://html.spec.whatwg.org/multipage/server-sent-events.html), W3C Server-Sent Events Recommendation.
- **Implementation**: [`std/net/sse.ag`](file:///home/cier/Projects/silver/std/net/sse.ag) (`SseReader`, `SseEvent`).
- **Test Suite**: [`tests/sse_test.ag`](file:///home/cier/Projects/silver/tests/sse_test.ag).

## Compliance Table

| Feature / Capability | Specification Section | Status | API in Silver | Implementation Notes |
|:---|:---|:---:|:---|:---|
| **Event Stream MIME Negotiation** | WHATWG §9.2.4 | Supported | `Accept: text/event-stream` | Connects via HTTP GET, enforcing `text/event-stream` Content-Type validation. |
| **Event Name Field (`event: ...`)** | WHATWG §9.2.6 | Supported | `SseEvent.event` | Captures named event types; defaults to `"message"` if omitted. |
| **Data Field (`data: ...`)** | WHATWG §9.2.6 | Supported | `SseEvent.data` | Multi-line data buffers are joined with LF characters per specification. |
| **Event ID Tracking (`id: ...`)** | WHATWG §9.2.6 | Supported | `SseEvent.id` | Records stream event ID for client reconnection and replay tracking. |
| **Reconnection Interval (`retry: ...`)** | WHATWG §9.2.6 | Supported | `SseEvent.retry` | Decodes integer millisecond reconnection retry parameter. |
| **Comment Lines (`:...`)** | WHATWG §9.2.6 | Supported | `SseReader.next()` | Heartbeat and keepalive comment lines beginning with `:` are cleanly discarded. |
| **Dispatch on Empty Line** | WHATWG §9.2.6 | Supported | `SseReader.next()` | Dispatches accumulated event state upon reading consecutive CRLF / LF line endings. |
