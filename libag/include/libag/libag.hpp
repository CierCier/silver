#pragma once
#include "libag/version.hpp"

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
inline const char *version() { return ver::STRING; }

// Returns the full version string (includes git hash for dev builds).
inline const char *version_full() { return ver::STRING_FULL; }

// Returns the git commit hash.
inline const char *git_hash() { return ver::GIT_HASH; }

// Returns version components.
inline int version_major() { return ver::MAJOR; }
inline int version_minor() { return ver::MINOR; }
inline int version_patch() { return ver::PATCH; }

} // namespace ag
