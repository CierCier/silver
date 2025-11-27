#pragma once
#include <iosfwd>
#include "agc/ast.hpp"

namespace agc {

void dump(const Program& prog, std::ostream& os);

} // namespace agc
