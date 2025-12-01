#include "libag/libag.hpp"
#include <cstdlib>
#include <cstring>

// Note: alloc<T>() and free() are built-in keywords that compile directly
// to malloc/free calls. This file provides additional memory utilities.

// These are already provided by libc, but we re-export them for Silver:
// - realloc(ptr, size) - reallocate memory
// - memset(ptr, value, size) - set memory to value
// - memcpy(dest, src, size) - copy memory
// - memmove(dest, src, size) - move memory (overlapping safe)
