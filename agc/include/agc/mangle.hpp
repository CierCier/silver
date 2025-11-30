#pragma once
#include "agc/ast.hpp"
#include <string>

namespace agc {

// Mangle a global function name
std::string mangle_function(const DeclFunc &f);

// Mangle a method name (inside impl block)
std::string mangle_method(const std::string &structName, const DeclFunc &f);

// Mangle a cast function name (inside impl block)
std::string mangle_cast(const std::string &structName, const DeclCast &c);

} // namespace agc
