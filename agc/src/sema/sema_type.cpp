#include "agc/sema.hpp"

namespace agc {

Type *SemanticAnalyzer::resolveType(const TypeName &typeName) {
  Type *base = nullptr;

  // Check type scopes first (for generic params)
  for (auto it = typeScopes_.rbegin(); it != typeScopes_.rend(); ++it) {
    if (it->count(typeName.name)) {
      base = it->at(typeName.name);
      break;
    }
  }

  if (base) {
    // Found in scope, proceed to apply pointers/arrays
  } else if (typeName.name == "void")
    base = typeCtx_.getVoid();
  else if (typeName.name == "bool")
    base = typeCtx_.getBool();
  else if (typeName.name == "i32")
    base = typeCtx_.getInt();
  else if (typeName.name == "f64")
    base = typeCtx_.getFloat();
  else if (typeName.name == "str")
    base = typeCtx_.getString();
  else if (structTypes_.count(typeName.name))
    base = structTypes_[typeName.name];
  else if (genericStructs_.count(typeName.name)) {
    // Instantiate generic struct
    std::vector<Type *> args;
    for (const auto &arg : typeName.genericArgs) {
      args.push_back(resolveType(arg));
    }
    instantiateStruct(genericStructs_[typeName.name], args);

    std::string mangledName = typeName.name;
    for (auto *t : args) {
      mangledName += "_" + t->toString();
    }
    if (structTypes_.count(mangledName)) {
      base = structTypes_[mangledName];
    } else {
      // Error? Should have been created.
      diags_.report(DiagLevel::Error, {},
                    "failed to instantiate generic struct '" + typeName.name +
                        "'");
      return typeCtx_.getVoid();
    }
  } else if (enumTypes_.count(typeName.name)) {
    base = enumTypes_[typeName.name];
  } else {
    // Check if it's a generic type or something else?
    // For now, error.
    diags_.report(DiagLevel::Error, {}, "unknown type '" + typeName.name + "'");
    return typeCtx_.getVoid();
  }

  // Apply pointers
  for (unsigned i = 0; i < typeName.pointerDepth; ++i) {
    base = typeCtx_.getPointer(base);
  }

  // Apply arrays (reverse order?)
  // TypeName: int x[10][20] -> arrayDims = {10, 20}
  // Type: Array(Array(int, 20), 10)
  for (auto it = typeName.arrayDims.rbegin(); it != typeName.arrayDims.rend();
       ++it) {
    uint64_t size = 0;
    if (*it)
      size = **it;
    base = typeCtx_.getArray(base, size);
  }

  return base;
}

bool SemanticAnalyzer::checkType(Type *expected, Type *actual,
                                 const DiagLoc &loc) {
  if (!expected || !actual)
    return false;
  if (expected->equals(actual))
    return true;

  // Implicit conversions?
  // void* can accept any pointer?
  // For now, strict.

  diags_.report(DiagLevel::Error, loc,
                "type mismatch: expected '" + expected->toString() +
                    "', got '" + actual->toString() + "'");
  return false;
}

} // namespace agc
