#include "agc/type.hpp"
#include <sstream>

namespace agc {

bool Type::equals(const Type *other) const {
  if (this == other)
    return true;
  if (kind() != other->kind())
    return false;
  // Deep comparison for structural types if not interned
  // For now, let's assume if they are not the same pointer, they might still be
  // equal if they are structural types with same content. But to simplify, we
  // rely on TypeContext to intern or we implement deep equals. Let's implement
  // deep equals for now.
  return toString() == other->toString(); // Hacky but works for simple cases
}

std::string PrimitiveType::toString() const {
  switch (k_) {
  case TypeKind::Void:
    return "void";
  case TypeKind::Bool:
    return "bool";
  case TypeKind::Int8:
    return "i8";
  case TypeKind::Int16:
    return "i16";
  case TypeKind::Int32:
    return "i32";
  case TypeKind::Int64:
    return "i64";
  case TypeKind::Float32:
    return "f32";
  case TypeKind::Float64:
    return "f64";
  case TypeKind::String:
    return "str";
  case TypeKind::Char:
    return "char";
  default:
    return "?";
  }
}

std::string PointerType::toString() const { return "*" + pointee_->toString(); }

std::string ArrayType::toString() const {
  return element_->toString() + "[" + std::to_string(size_) + "]";
}

std::string FunctionType::toString() const {
  std::stringstream ss;
  ss << "fn(";
  for (size_t i = 0; i < params_.size(); ++i) {
    if (i > 0)
      ss << ", ";
    ss << params_[i]->toString();
  }
  ss << ") -> " << ret_->toString();
  return ss.str();
}

const CastInfo *StructType::findCast(Type *targetType) const {
  for (const auto &cast : casts_) {
    if (cast.targetType->equals(targetType)) {
      return &cast;
    }
  }
  return nullptr;
}

const MethodInfo *StructType::findMethod(const std::string &name) const {
  for (const auto &method : methods_) {
    if (method.name == name) {
      return &method;
    }
  }
  return nullptr;
}

bool StructType::hasTrait(const std::string &trait) const {
  for (const auto &t : traits_) {
    if (t == trait) {
      return true;
    }
  }
  return false;
}

TypeContext::TypeContext() {
  voidType_ = new PrimitiveType(TypeKind::Void);
  boolType_ = new PrimitiveType(TypeKind::Bool);
  int8Type_ = new PrimitiveType(TypeKind::Int8);
  int16Type_ = new PrimitiveType(TypeKind::Int16);
  int32Type_ = new PrimitiveType(TypeKind::Int32);
  int64Type_ = new PrimitiveType(TypeKind::Int64);
  float32Type_ = new PrimitiveType(TypeKind::Float32);
  float64Type_ = new PrimitiveType(TypeKind::Float64);
  stringType_ = new PrimitiveType(TypeKind::String);
  charType_ = new PrimitiveType(TypeKind::Char);

  types_.emplace_back(voidType_);
  types_.emplace_back(boolType_);
  types_.emplace_back(int8Type_);
  types_.emplace_back(int16Type_);
  types_.emplace_back(int32Type_);
  ;
  types_.emplace_back(float32Type_);
  types_.emplace_back(float64Type_);
  types_.emplace_back(stringType_);
  types_.emplace_back(charType_);
}

TypeContext::~TypeContext() {
  // unique_ptrs handle deletion
}

Type *TypeContext::getVoid() { return voidType_; }
Type *TypeContext::getBool() { return boolType_; }
Type *TypeContext::getInt8() { return int8Type_; }
Type *TypeContext::getInt16() { return int16Type_; }
Type *TypeContext::getInt32() { return int32Type_; }
Type *TypeContext::getInt() { return int32Type_; } // Alias for i32
Type *TypeContext::getInt64() { return int64Type_; }
Type *TypeContext::getFloat32() { return float32Type_; }
Type *TypeContext::getFloat64() { return float64Type_; }
Type *TypeContext::getFloat() { return float64Type_; } // Alias for f64
Type *TypeContext::getString() { return stringType_; }
Type *TypeContext::getChar() { return charType_; }

Type *TypeContext::getPointer(Type *pointee) {
  // Check if we already have this pointer type?
  // For now, just create new.
  auto t = new PointerType(pointee);
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getArray(Type *element, uint64_t size) {
  auto t = new ArrayType(element, size);
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getStruct(std::string name, std::vector<Field> fields) {
  auto t = new StructType(std::move(name), std::move(fields));
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getOpaqueStruct(std::string name) {
  auto t = new StructType(std::move(name));
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getEnum(std::string name, std::vector<TypeEnumItem> items) {
  auto t = new EnumType(std::move(name), std::move(items));
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getFunction(Type *ret, std::vector<Type *> params) {
  auto t = new FunctionType(ret, std::move(params));
  types_.emplace_back(t);
  return t;
}

Type *TypeContext::getMeta(Type *representedType) {
  auto t = new MetaType(representedType);
  types_.emplace_back(t);
  return t;
}

} // namespace agc
