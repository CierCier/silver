# Formatting, errors, conversions, and DMA

## Formatting borrows its inputs

`Display.fmt(*self, BufWriter* out)` is the shared contract for `{}` formatting
and `String.push_display(&value)`. The source retains ownership. A formatter
may emit arbitrary bytes. `String` formatting uses its stored length and keeps
embedded NULs, including when nested inside `Optional` or `Result`.

`str` is a NUL-terminated view, so formatting a `str` stops at its first NUL.
Converting a `String` to `str` does not transfer ownership or extend its lifetime.
Use `AsSlice<u8>` when a consumer needs all bytes.

`char` is a 32-bit codepoint. Formatting encodes Unicode scalars as UTF-8 and
replaces surrogates or values above U+10FFFF with U+FFFD. U+0000 emits one NUL
byte. `BufWriter.write_char` uses the same encoder as compiler formatting.
`write_u8` writes one raw byte; formatting a `u8` prints its decimal value.
`String.push_codepoint` uses the same encoder but rejects invalid scalars
without appending, preserving its fallible append contract.

Buffered output still has a limitation: `BufWriter.write` and `flush` return
`void` and cannot report fd write failures. Use typed syscall results when
the caller must observe I/O failure. These formatting changes do not add a
fallible output trait.

## Composing fallible operations

`Result<T, E>` provides:

| Method | Callback | Result |
| --- | --- | --- |
| `map` | `U(T)` | `Result<U, E>` |
| `map_err` | `F(E)` | `Result<T, F>` |
| `and_then` | `Result<U, E>(T)` | `Result<U, E>` |

`Optional<T>` provides `map` with `U(T)` and `and_then` with `Optional<U>(T)`.
Methods infer their type arguments from the callback. All consume the original
container and move the selected payload into the callback. The other variant
passes through without calling it. Callbacks are ordinary function pointers;
these APIs do not introduce closures or implicit error propagation.

```silver
import std.result;
import std.string;

i64 length(String text) { return text.len(); }

i32 main() {
    Result<String, i32> value = Ok(String.from_str("silver"));
    Result<i64, i32> size = value.map(length);
    return size.unwrap() == 6 ? 0 : 1;
}
```

`Error.message` remains borrowed. Use a string literal or storage that outlives
the error. Module-specific error codes need their module's interpretation;
syscall errors use positive errno values.

## Explicit checked conversion

`std.convert` defines `TryFrom<T>` with a static
`Result<Self, Error> try_from(T value)` method. Its integer implementations
accept `i128`, which represents every value of the integer types through `u64`.
Targets are `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, and `char`.
Range failure returns code 34, ERANGE. Character conversion also rejects
surrogates. It does not replace invalid characters.

```silver
import std.convert;

Result<T, Error> narrow<T: TryFrom<i128>>(i128 value) {
    return T.try_from(value);
}

i32 main() {
    Result<u8, Error> byte = narrow<u8>((i128)255);
    return byte.unwrap() == (u8)255 ? 0 : 1;
}
```

This is an integer conversion API. It does not promise checked floating-point
conversion or accept the whole `u128` range. Convert an integer source to `i128`
before calling it. Floating-point arguments can undergo the language's ordinary
implicit conversion before the method runs, losing information already.

The current language permits implicit conversions among numeric and boolean
primitives, including narrowing. Assignment, arguments, and returns can trigger
them. User-declared `cast` implementations also participate in implicit
conversion. `(T)value` requests an explicit cast, without an overflow result.
`TryFrom` is a separate opt-in contract and never runs implicitly.
Calls and trait bounds inside ordinary casts are still validated.

## Typed allocation

Typed allocation uses `@align(T)`. Counted allocations reject negative counts
and multiplication overflow. Mapping overhead is checked before allocation.
`try_alloc` and `try_realloc` return null on failure, including under leak
checking; their aborting counterparts abort. Invalid `try_realloc` counts leave
the original allocation owned by the caller and preserve its bytes.

`alloc<T>()` is uninitialized. Counted `alloc_n<T>(count)` and `alloc<T>(count)`
zero the allocation; `alloc_uninit_n<T>` does not. Zero count retains the
allocator's non-null dangling-pointer convention. Allocation does not construct
elements or run their destructors. Raw allocation requires a positive power-of-two
alignment, and realloc honors a stronger alignment even when capacity suffices.

## Linux hardware DMA

`std.dma` exposes `DmaBuffer` on Linux. It owns a DMA-BUF fd and closes it on
drop. `allocate(heap_path, size)` uses the Linux DMA-heap ioctl with close-on-exec;
the explicit heap determines the allocation policy. `from_fd(fd)` duplicates
an existing fd with close-on-exec and queries its size. It leaves the caller's
fd open. `fd()` lends the owned descriptor for a device-specific import.

`with_cpu_access(callback)` maps a borrowed `Slice<u8>`, starts read/write CPU
cache coherency, calls `Result<T, Error>(Slice<u8>)`, ends coherency, and unmaps.
The method infers `T` from the callback. It retries interrupted coherency ioctls.
The callback result survives cleanup, including owned payloads. An end-sync or
unmap error takes precedence over the callback result. A start-sync failure
unmaps without calling the callback. No CPU view may escape the callback.

Callers must wait for device work before CPU access and submit subsequent work
only after successful completion. Serialize CPU access through duplicated
descriptors and other processes. Cache coherency is separate from waiting for
device fences. A virtual pointer, aligned heap allocation, `mlock`, atomic fence,
or volatile access does not supply a DMA address or establish device coherency.
Device addresses, command queues, and fence submission remain driver-specific.
An ordinary file can pass the size query but fails the DMA sync ioctl; `from_fd`
requires an actual DMA-BUF descriptor for successful CPU access.

The API follows the Linux [DMA-BUF synchronization contract](https://docs.kernel.org/driver-api/dma-buf.html)
and [DMA heap UAPI](https://raw.githubusercontent.com/torvalds/linux/master/include/uapi/linux/dma-heap.h).
Current syscall and ioctl encoding support is Linux x86_64. This is not a
bare-metal DMA controller API or a portable device driver framework.

`tests/dma_test.ag` exercises failure paths on every Linux run and uses
`/dev/udmabuf` for a real DMA-BUF round trip when that exporter is accessible.
It reports an explicit skip when unavailable. That test proves descriptor,
mapping, and coherency behavior, not a peripheral transfer. Successful DMA-heap
allocation requires an accessible `/dev/dma_heap/<name>` and is a separate gate.
