#include <cstdio>

extern "C" void println(const char* s) {
    printf("%s\n", s);
}
