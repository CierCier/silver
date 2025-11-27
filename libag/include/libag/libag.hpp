#pragma once
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
inline constexpr std::string_view version_string{"0.1.0"};

// Returns the library version as a C string (stable for program lifetime).
AG_API const char* version();

} // namespace ag
