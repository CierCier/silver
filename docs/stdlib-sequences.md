# Borrowing and comparing sequences

Import `std.slice` for `AsSlice<T>`, a shared contract for borrowing contiguous
elements. `Vec<T>` and `Slice<T>` implement `AsSlice<T>`. `String` and `Bytes`
implement `AsSlice<u8>`.

```silver
import std.slice;
import std.string;

i64 byte_count<C: AsSlice<u8>>(&C source, u8 byte) {
    Slice<u8> view = source.as_slice();
    i64 count = 0;
    for (i64 i = 0; i < view.len; i += 1) {
        if (view.data[i] == byte) { count += 1; }
    }
    return count;
}

i32 main() {
    String text = String.from_str("banana");
    return byte_count<String>(&text, (u8)'a') == 3 ? 0 : 1;
}
```

`as_slice()` returns a view of the original allocation. It does not copy elements
or transfer ownership. A String view includes its content bytes and excludes the
trailing NUL. The source must remain alive, and its storage must remain valid
while the view is used. An implementation must return a nonnegative length and
storage valid for that many elements. Empty views may have null data.

`AsSlice` grants shared access. Mutation requires a separate mutable borrow,
such as `Vec.as_mut_slice()`. The current Slice representation holds a raw
pointer, so callers must respect the borrowing contract when using that pointer.

`Slice<T>`, `Vec<T>`, and `Bytes` implement `Eq`, `Ne`, `Compare`, `Lt`, `Gt`,
`Le`, and `Ge` for their own type. String retains its existing comparison traits.
Compare different container types through their slice views.

Equality compares lengths and then elements with `==`. It works with builtin
element equality and custom `Eq` implementations, including owned String
elements. Comparisons borrow both operands and preserve their elements.
String equality includes bytes after embedded NULs in both compiler stages.

Ordering requires `T: Compare` and is lexicographic. The first nonzero element
comparison determines the result. When common elements compare equal, the
shorter sequence sorts first. Empty sequences compare equal. `Compare` promises
a negative, zero, or positive result, not specifically -1 or 1. Sequence ordering
inherits the element implementation's behavior, including floating-point NaNs.

These traits let existing algorithms sort nested vectors. For example, a
`Vec<Vec<i64>>` can use `sort(&values)` from `std.algo`.

String concatenation implements both `Add<String>` and `Add<str>`. Both forms
consume the left String. Adding another String also consumes the right String.

## Review limits

Hashing still needs a separate contract. `HashMap` and `HashSet` use `@hash`,
which hashes String and `str` content but hashes other types by representation.
Content equality alone does not make Vec, Bytes, Slice, or a custom type with
content equality a valid hash key. Equal keys must produce equal hashes.

Stage0's source trait checker matches owner types, trait arguments, and
implementation bounds. Imported AGM exports currently retain the trait name
and owner without its trait arguments, so that metadata path does not provide
the same argument validation. A generic implementation in an imported module
is matched by its owner name alone, so a bound on such an owner needs the
implementation's source to be visible.

The runtime regression is [sequence_traits_test.ag](../tests/sequence_traits_test.ag).
It exercises all four AsSlice implementations, comparison traits, custom
elements, embedded NUL bytes, temporary slices, nested sorting, and ownership.
