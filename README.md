# Silver Programming Language

Silver is a compiled programming language designed for general purpose programming.

## Features

- **Static Typing**: Strong static typing with type inference.
- **Native Compilation**: Compiles directly to machine code via LLVM.

## Repository Information

This repository currently contains the compiler and standard library for the Silver programming language.

- The Standard Library is planned to be moved to its own repository in the near future.
- The language specification is also planned to be documented separately.
- The current compiler is a prototype bootstrapped using C++. A future self-hosted version is planned when the language is more mature.


## Building the Compiler

To build the Silver compiler (`agc`), you need CMake and a C++20 compliant compiler.

```bash
cmake -B build
make -C build
```

## Usage

To compile a Silver program:

```bash
.build/agc/agc -I libag/include path/to/source.ag
```
currently the compiler expects the standard library to be located at `/usr/include/silver/` but i dont expect you to install it right now so you can just use the `-I` flag to point to the `libag/include` directory in this repository.
## Examples

Check the `examples/` directory for sample Silver programs.

```cpp
// examples/hello.ag
i32 main() {
    return 0;
}
```
