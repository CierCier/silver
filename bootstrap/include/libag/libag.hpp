#pragma once
#include "libag/version.hpp"
#include <string_view>


// Export/import macro for shared builds
#if defined(_WIN32) || defined(__CYGWIN__)
#if defined(AG_SHARED)
#if defined(AG_BUILD)
#define AG_API __declspec(dllexport)
#else
#define AG_API __declspec(dllimport)
#endif
#else
#define AG_API
#endif
#else
#if defined(AG_SHARED)
#define AG_API __attribute__((visibility("default")))
#else
#define AG_API
#endif
#endif

namespace ag {

// Returns the library version as a C string (stable for program lifetime).
AG_API const char *version();

// Returns the full version string (includes git hash for dev builds).
AG_API const char *version_full();

// Returns the git commit hash.
AG_API const char *git_hash();

// Returns version components.
AG_API int version_major();
AG_API int version_minor();
AG_API int version_patch();

} // namespace ag
