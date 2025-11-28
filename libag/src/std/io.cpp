#include <cstdint>
#include <cstdio>
#include <stdint.h>

extern "C" int32_t println(const char *s) { return printf("%s\n", s); }
