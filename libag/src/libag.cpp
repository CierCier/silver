#include "libag/libag.hpp"

namespace ag {

const char *version() { return ver::STRING; }

const char *version_full() { return ver::STRING_FULL; }

const char *git_hash() { return ver::GIT_HASH; }

int version_major() { return ver::MAJOR; }

int version_minor() { return ver::MINOR; }

int version_patch() { return ver::PATCH; }

} // namespace ag
