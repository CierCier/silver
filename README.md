# Silver
A High level C-Like programming language that nobody asked for

## Features

- Static type system even more ambiguous than C
- No memory safety whatsoever
- standard library is non-existent

## Example

```
// Basic types and variables
let i32 x = 47;
let *i32 ptr = &x;
let i32 value = *ptr;

// Structs
struct Point {
    f64 x;
    f64 y;
}

let Point p = {x: 1.0, y: 2.0};

// Functions
func add(i32 a, i32 b) i32 {
    return a + b;
}
```

## License

MIT