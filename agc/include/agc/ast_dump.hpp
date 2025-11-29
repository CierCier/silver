#pragma once
#include "agc/ast.hpp"
#include <iosfwd>


namespace agc {

void dump(const Program &prog, std::ostream &os);

} // namespace agc
