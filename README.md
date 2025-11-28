# Silver Programming Language

Silver is a compiled programming language designed for performance and simplicity. It is implemented in C++ and uses LLVM for code generation.

## Features

- **Static Typing**: Strong static typing with type inference.
- **Native Compilation**: Compiles directly to machine code via LLVM.
- **C Interoperability**: Easy integration with C libraries.
- **Modern Syntax**: Clean and expressive syntax inspired by C, Rust, and Go.

## Building the Compiler

To build the Silver compiler (`agc`), you need CMake and a C++20 compliant compiler.

```bash
mkdir build
cd build
cmake ..
make
```

## Usage

To compile a Silver program:

```bash
./agc/agc path/to/source.ag
```

## Examples

Check the `examples/` directory for sample Silver programs.

```cpp
// examples/hello.ag
i32 main() {
    return 0;
}
```
