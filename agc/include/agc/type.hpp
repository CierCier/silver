#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace agc {

enum class TypeKind {
  Void,
  Bool,
  Int8,    // i8
  Int16,   // i16
  Int32,   // i32
  Int64,   // i64
  Float32, // f32
  Float64, // f64
  String,
  Char, // C-compatible char (stored as i8)
  Pointer,
  Array,
  Struct,
  Enum,
  Function,
  Meta // Represents a type itself (e.g. "Color" in "Color.Red")
};

class Type {
public:
  virtual ~Type() = default;
  virtual TypeKind kind() const = 0;
  virtual std::string toString() const = 0;
  virtual bool equals(const Type *other) const;

  bool isVoid() const { return kind() == TypeKind::Void; }
  bool isBool() const { return kind() == TypeKind::Bool; }
  bool isInt8() const { return kind() == TypeKind::Int8; }
  bool isInt16() const { return kind() == TypeKind::Int16; }
  bool isInt32() const { return kind() == TypeKind::Int32; }
  bool isInt64() const { return kind() == TypeKind::Int64; }
  bool isInt() const { return isInt8() || isInt16() || isInt32() || isInt64(); }
  bool isFloat32() const { return kind() == TypeKind::Float32; }
  bool isFloat64() const { return kind() == TypeKind::Float64; }
  bool isFloat() const { return isFloat32() || isFloat64(); }
  bool isString() const { return kind() == TypeKind::String; }
  bool isChar() const { return kind() == TypeKind::Char; }
  bool isPointer() const { return kind() == TypeKind::Pointer; }
  bool isArray() const { return kind() == TypeKind::Array; }
  bool isStruct() const { return kind() == TypeKind::Struct; }
  bool isEnum() const { return kind() == TypeKind::Enum; }
  bool isFunction() const { return kind() == TypeKind::Function; }
  bool isMeta() const { return kind() == TypeKind::Meta; }
  bool isIntegral() const { return isInt() || isBool() || isChar(); }
  bool isFloating() const { return isFloat(); }
  bool isNumeric() const { return isIntegral() || isFloating(); }
};

class PrimitiveType : public Type {
public:
  explicit PrimitiveType(TypeKind k) : k_(k) {}
  TypeKind kind() const override { return k_; }
  std::string toString() const override;

private:
  TypeKind k_;
};

class PointerType : public Type {
public:
  explicit PointerType(Type *pointee) : pointee_(pointee) {}
  TypeKind kind() const override { return TypeKind::Pointer; }
  std::string toString() const override;
  Type *pointee() const { return pointee_; }

private:
  Type *pointee_;
};

class ArrayType : public Type {
public:
  ArrayType(Type *element, uint64_t size) : element_(element), size_(size) {}
  TypeKind kind() const override { return TypeKind::Array; }
  std::string toString() const override;
  Type *element() const { return element_; }
  uint64_t size() const { return size_; }

private:
  Type *element_;
  uint64_t size_;
};

struct Field {
  std::string name;
  Type *type;
};

struct CastInfo {
  Type *targetType;         // The type we're casting to
  std::string functionName; // The mangled function name to call
  bool isImplicit{false};   // Whether this cast can be applied implicitly
};

struct MethodInfo {
  std::string name;
  std::string mangledName; // The actual function name in the generated code
  Type *returnType;
  std::vector<Type *> paramTypes;
};

class StructType : public Type {
public:
  StructType(std::string name, std::vector<Field> fields)
      : name_(std::move(name)), fields_(std::move(fields)) {}
  StructType(std::string name) : name_(std::move(name)) {} // Opaque constructor

  void setFields(std::vector<Field> fields) { fields_ = std::move(fields); }

  // Method/cast registration
  void addCast(CastInfo cast) { casts_.push_back(std::move(cast)); }
  void addMethod(MethodInfo method) { methods_.push_back(std::move(method)); }

  // Lookup
  const CastInfo *findCast(Type *targetType) const;
  const MethodInfo *findMethod(const std::string &name) const;

  const std::vector<CastInfo> &casts() const { return casts_; }
  const std::vector<MethodInfo> &methods() const { return methods_; }

  // Trait support
  void addTrait(const std::string &trait) { traits_.push_back(trait); }
  bool hasTrait(const std::string &trait) const;
  const std::vector<std::string> &traits() const { return traits_; }

  TypeKind kind() const override { return TypeKind::Struct; }
  std::string toString() const override { return name_; }
  const std::string &name() const { return name_; }
  const std::vector<Field> &fields() const { return fields_; }

private:
  std::string name_;
  std::vector<Field> fields_;
  std::vector<CastInfo> casts_;
  std::vector<MethodInfo> methods_;
  std::vector<std::string> traits_;
};

struct TypeEnumItem {
  std::string name;
  uint64_t value;
};

class EnumType : public Type {
public:
  EnumType(std::string name, std::vector<TypeEnumItem> items)
      : name_(std::move(name)), items_(std::move(items)) {}
  TypeKind kind() const override { return TypeKind::Enum; }
  std::string toString() const override { return name_; }
  const std::string &name() const { return name_; }
  const std::vector<TypeEnumItem> &items() const { return items_; }

private:
  std::string name_;
  std::vector<TypeEnumItem> items_;
};

class FunctionType : public Type {
public:
  FunctionType(Type *ret, std::vector<Type *> params)
      : ret_(ret), params_(std::move(params)) {}
  TypeKind kind() const override { return TypeKind::Function; }
  std::string toString() const override;
  Type *returnType() const { return ret_; }
  const std::vector<Type *> &paramTypes() const { return params_; }

private:
  Type *ret_;
  std::vector<Type *> params_;
};

class MetaType : public Type {
public:
  explicit MetaType(Type *representedType)
      : representedType_(representedType) {}
  TypeKind kind() const override { return TypeKind::Meta; }
  std::string toString() const override {
    return "type(" + representedType_->toString() + ")";
  }
  Type *representedType() const { return representedType_; }

private:
  Type *representedType_;
};

class TypeContext {
public:
  TypeContext();
  ~TypeContext(); // Clean up types

  Type *getVoid();
  Type *getBool();
  Type *getInt8();    // i8
  Type *getInt16();   // i16
  Type *getInt32();   // i32
  Type *getInt();     // alias for i32
  Type *getInt64();   // i64
  Type *getFloat32(); // f32
  Type *getFloat64(); // f64
  Type *getFloat();   // alias for f64
  Type *getString();
  Type *getChar(); // UTF-8 codepoint

  Type *getPointer(Type *pointee);
  Type *getArray(Type *element, uint64_t size);
  Type *getStruct(std::string name, std::vector<Field> fields);
  Type *getOpaqueStruct(std::string name);
  Type *getEnum(std::string name, std::vector<TypeEnumItem> items);
  Type *getFunction(Type *ret, std::vector<Type *> params);
  Type *getMeta(Type *representedType);

private:
  std::vector<std::unique_ptr<Type>> types_;

  // Cache for primitives
  Type *voidType_;
  Type *boolType_;
  Type *int8Type_;
  Type *int16Type_;
  Type *int32Type_;
  Type *int64Type_;
  Type *float32Type_;
  Type *float64Type_;
  Type *stringType_;
  Type *charType_;

  // Simple caching for pointers/arrays could be added, but for now just create
  // new ones or maybe intern them? Let's just store them all in types_ for
  // ownership.
};

} // namespace agc
