# Silver Standard Library Tests

This directory contains integration tests for the Silver standard library modules.

## Test Files

| File | Description |
|------|-------------|
| `io_test.ag` | Tests for `std.io` (printf, puts, putchar, etc.) |
| `mem_test.ag` | Tests for `std.mem` (alloc, free, memset, memcpy, memmove, realloc) |
| `string_test.ag` | Tests for `std.string` (String object with dynamic memory) |
| `vec_test.ag` | Tests for `std.vec` (Vec_i32, Vec_i64, Vec_f64, Vec_ptr) |
| `file_test.ag` | Tests for `std.file` (File handle, read/write, seek, utilities) |
| `optional_test.ag` | Tests for `std.optional` (Optional and Result types) |

## Building Tests

From the build directory:

```bash
# Build all stdlib tests
make stdlib_tests

# Build a specific test
make stdlib_string_test
```

## Running Tests

```bash
# Run all stdlib tests via CTest
ctest -R "^stdlib_" --output-on-failure

# Or use the convenience target
make run_stdlib_tests

# Run a specific test directly
./tests/stdlib_string_test
```

## Test Structure

Each test file follows a consistent pattern:

```silver
import std.io;
// Import module being tested

i32 tests_passed = 0;
i32 tests_failed = 0;

void assert_true(bool cond, str msg) {
    if (cond) {
        tests_passed = tests_passed + 1;
        printf("  [PASS] %s\n", msg);
    } else {
        tests_failed = tests_failed + 1;
        printf("  [FAIL] %s\n", msg);
    }
}

i32 main() {
    // Test cases...
    
    printf("\n=== Results ===\n");
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_failed);
    
    return tests_failed > 0 ? 1 : 0;
}
```

Tests return exit code 0 on success, 1 on failure, making them compatible with CTest.

## Adding New Tests

1. Create a new `.ag` file in this directory
2. Follow the test structure pattern above
3. The CMake configuration will automatically discover and add the test
4. Rebuild to include the new test: `make stdlib_tests`
