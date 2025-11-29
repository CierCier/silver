#include "libag/libag.hpp"
#include <cstdint>
#include <cstdio>
#include <stdint.h>

extern "C" AG_API int32_t println(const char *s) { return printf("%s\n", s); }
