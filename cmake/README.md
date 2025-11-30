# Silver CMake Tools

This directory contains CMake modules and tools for the Silver language.

## SilverUtils.cmake

Provides the `add_silver_executable` function to compile Silver programs.

### Usage

```cmake
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(SilverUtils)

add_silver_executable(my_program main.ag)
```
