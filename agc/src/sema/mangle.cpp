#include "agc/mangle.hpp"
#include <sstream>

namespace agc {

static void mangle_type(std::stringstream &ss, const TypeName &t) {
  // Handle pointers
  for (unsigned i = 0; i < t.pointerDepth; ++i) {
    ss << "P";
  }
  // Handle arrays
  for (const auto &dim : t.arrayDims) {
    ss << "A";
    if (dim)
      ss << *dim;
    else
      ss << "0";
    ss << "_";
  }

  // Base type
  if (t.name == "void")
    ss << "v";
  else if (t.name == "bool" || t.name == "i1")
    ss << "b";
  else if (t.name == "i8")
    ss << "c";
  else if (t.name == "char")
    ss << "c"; // C-compatible char (same as i8)
  else if (t.name == "u8")
    ss << "h";
  else if (t.name == "i16")
    ss << "s";
  else if (t.name == "u16")
    ss << "t";
  else if (t.name == "i32" || t.name == "int")
    ss << "i";
  else if (t.name == "u32")
    ss << "j";
  else if (t.name == "i64" || t.name == "long")
    ss << "x";
  else if (t.name == "u64")
    ss << "y";
  else if (t.name == "f32" || t.name == "float")
    ss << "f";
  else if (t.name == "f64" || t.name == "double")
    ss << "d";
  else if (t.name == "str" || t.name == "string")
    ss << "S";
  else {
    // Struct or other named type
    ss << "N" << t.name.length() << t.name;
  }

  // Generic args
  if (!t.genericArgs.empty()) {
    ss << "I";
    for (const auto &arg : t.genericArgs) {
      mangle_type(ss, arg);
    }
    ss << "E";
  }
}

std::string mangle_function(const DeclFunc &f) {
  if (f.isExtern)
    return f.name; // Don't mangle externs
  if (f.name == "main")
    return "main"; // Don't mangle main

  std::stringstream ss;
  ss << "_S" << f.name.length() << f.name;

  ss << "R";
  mangle_type(ss, f.ret);

  if (!f.params.empty()) {
    ss << "P";
    for (const auto &p : f.params) {
      mangle_type(ss, p.type);
    }
  }

  return ss.str();
}

std::string mangle_method(const std::string &structName, const DeclFunc &f) {
  std::stringstream ss;
  ss << "_S" << structName.length() << structName << f.name.length() << f.name;

  ss << "R";
  mangle_type(ss, f.ret);

  if (!f.params.empty()) {
    ss << "P";
    for (const auto &p : f.params) {
      mangle_type(ss, p.type);
    }
  }

  return ss.str();
}

std::string mangle_cast(const std::string &structName, const DeclCast &c) {
  std::stringstream ss;
  std::string castName = "cast";
  ss << "_S" << structName.length() << structName << castName.length()
     << castName;

  // Target type is part of the name/signature
  ss << "T";
  mangle_type(ss, c.target);

  if (!c.params.empty()) {
    ss << "P";
    for (const auto &p : c.params) {
      mangle_type(ss, p.type);
    }
  }

  return ss.str();
}

} // namespace agc
