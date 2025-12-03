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
  else if (typeName.name == "i32" || typeName.name == "int")
    base = typeCtx_.getInt();
  else if (typeName.name == "i8" || typeName.name == "u8" ||
           typeName.name == "char")
    base = typeCtx_.getInt8();
  else if (typeName.name == "i64" || typeName.name == "u64" ||
           typeName.name == "long")
    base = typeCtx_.getInt64();
  else if (typeName.name == "f64" || typeName.name == "double")
    base = typeCtx_.getFloat();
  else if (typeName.name == "f32" || typeName.name == "float")
    base = typeCtx_.getFloat32();
  else if (typeName.name == "str" || typeName.name == "string")
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

  // Implicit conversions
  // Allow widening integer conversions: i8 -> i32 -> i64
  // Allow widening float conversions: f32 -> f64

  // Helper to get integer size in bits
  auto intSize = [](TypeKind k) -> int {
    switch (k) {
    case TypeKind::Bool:
      return 1;
    case TypeKind::Int8:
      return 8;
    case TypeKind::Int16:
      return 16;
    case TypeKind::Int32:
      return 32;
    case TypeKind::Int64:
      return 64;
    default:
      return 0;
    }
  };

  int expSize = intSize(expected->kind());
  int actSize = intSize(actual->kind());

  // Allow integer conversions between integral types (both widening and
  // narrowing) This matches C-like behavior where integers are implicitly
  // convertible
  if (expSize > 0 && actSize > 0) {
    return true;
  }

  // Allow int <-> float conversions (implicit)
  if ((expected->isFloat() || expected->isFloat32()) &&
      (actual->isIntegral() || actual->isFloat() || actual->isFloat32())) {
    return true;
  }
  if ((expected->isIntegral()) && (actual->isFloat() || actual->isFloat32())) {
    return true;
  }

  // void* can accept any pointer, and any pointer can cast to void*
  if (expected->isPointer() && actual->isPointer()) {
    auto *expPtr = static_cast<PointerType *>(expected);
    auto *actPtr = static_cast<PointerType *>(actual);
    if (expPtr->pointee()->isVoid() || actPtr->pointee()->isVoid()) {
      return true;
    }
    // Allow i8* <-> str (which is i8*)
    if (expPtr->pointee()->isIntegral() &&
        expPtr->pointee()->kind() == TypeKind::Int8 &&
        actPtr->pointee()->isIntegral() &&
        actPtr->pointee()->kind() == TypeKind::Int8) {
      return true;
    }
  }

  diags_.report(DiagLevel::Error, loc,
                "type mismatch: expected '" + expected->toString() +
                    "', got '" + actual->toString() + "'");
  return false;
}

} // namespace agc
