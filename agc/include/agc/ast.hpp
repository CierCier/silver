#pragma once

#include "agc/token.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace agc {

class Type; // Forward declaration

struct TypeName {
  // simple representation: keyword or identifier plus pointer depth and
  // optional array dims (prefix and suffix merged)
  std::string name; // e.g., "i32", "str", or user-defined
  unsigned pointerDepth{0};
  std::vector<std::optional<uint64_t>> arrayDims; // each [] optional size
  std::vector<TypeName> genericArgs;              // e.g. Box<i32> -> {i32}
};

struct Expr;
using ExprPtr = std::unique_ptr<Expr>;

struct ExprIdent {
  std::string name;
};
struct ExprInt {
  uint64_t value{};
};
struct ExprFloat {
  double value{};
};
struct ExprBool {
  bool value{};
};
struct ExprStr {
  std::string value;
};
struct ExprUnary {
  TokenKind op;
  ExprPtr rhs;
};
struct ExprBinary {
  TokenKind op;
  ExprPtr lhs, rhs;
};
struct ExprAssign {
  ExprPtr lhs, rhs;
  TokenKind op;
};
struct ExprCond {
  ExprPtr cond, thenE, elseE;
};
struct ExprCall {
  std::string callee;
  std::string mangledCallee; // Set by Sema
  std::vector<ExprPtr> args;
  std::vector<TypeName> genericArgs;
};
struct ExprIndex {
  ExprPtr base;
  ExprPtr index;
};
struct ExprMember {
  ExprPtr base;
  std::string member;
  bool ptr{false};
};
struct ExprMethodCall {
  ExprPtr base;
  std::string method;
  std::string mangledMethod; // Set by Sema
  std::vector<ExprPtr> args;
  bool ptr{false}; // true for -> operator
};
struct ExprComptime {
  ExprPtr expr; // expression to evaluate at compile time
};
struct ExprAddressOf {
  ExprPtr operand; // &operand
};
struct ExprDeref {
  ExprPtr operand; // *operand
};
struct ExprCast {
  ExprPtr expr;
  TypeName target;
  // Filled in by semantic analysis if a custom cast function exists
  std::optional<std::string> customCastFunc;
};

// Built-in new<T>() - zero-initializes and returns a value of type T
struct ExprNew {
  TypeName targetType;
  // Resolved by sema - the drop method name if type has @trait(drop)
  std::optional<std::string> dropMethod;
};

// Built-in drop(val) - calls drop method if type has @trait(drop), otherwise
// no-op
struct ExprDrop {
  ExprPtr operand;
  // Resolved by sema - the drop method name if type has @trait(drop)
  std::optional<std::string> dropMethod;
};

// Built-in alloc<T>() - heap allocates, returns T*
// alloc<T>(count) - allocates array of count elements, returns T*
struct ExprAlloc {
  TypeName targetType;
  std::optional<ExprPtr> count; // nullptr for single allocation
};

// Built-in free(ptr) - deallocates heap memory
struct ExprFree {
  ExprPtr operand;
};

struct InitItem {
  std::optional<ExprPtr> designator; // [index] or .field (future)
  ExprPtr value;
};

struct ExprInitList {
  std::vector<InitItem> values;
};

struct Expr {
  std::variant<ExprIdent, ExprInt, ExprFloat, ExprBool, ExprStr, ExprUnary,
               ExprBinary, ExprAssign, ExprCond, ExprCall, ExprIndex,
               ExprMember, ExprMethodCall, ExprComptime, ExprAddressOf,
               ExprDeref, ExprCast, ExprInitList, ExprNew, ExprDrop, ExprAlloc,
               ExprFree>
      v;
  DiagLoc loc;
  Type *type{nullptr}; // Resolved type

  std::unique_ptr<Expr> clone() const;
};

struct Stmt;
using StmtPtr = std::unique_ptr<Stmt>;

struct StmtExpr {
  ExprPtr expr;
};
struct StmtReturn {
  std::optional<ExprPtr> expr;
};
struct Declarator {
  std::string name;
  DiagLoc loc;
  std::optional<ExprPtr> init;
};

struct StmtDecl {
  TypeName type;
  std::vector<Declarator> declarators;
  bool isConst{false};
  Type *resolvedType{nullptr}; // Set by Sema
};
struct StmtBlock {
  std::vector<StmtPtr> stmts;
  StmtBlock clone() const; // Helper for deep copy
};
struct StmtFor {
  std::optional<StmtPtr> init; // either decl or expr stmt
  std::optional<ExprPtr> cond;
  std::optional<ExprPtr> iter;
  std::unique_ptr<Stmt> body;
};
struct StmtIf {
  ExprPtr cond;
  StmtPtr thenBranch;
  std::optional<StmtPtr> elseBranch;
};
struct StmtWhile {
  ExprPtr cond;
  StmtPtr body;
};
struct StmtBreak {};
struct StmtContinue {};
struct StmtAsm {
  std::string code;
  bool isVolatile{true};
};

struct Case {
  std::vector<ExprPtr> values;
  StmtPtr body;
};

struct StmtSwitch {
  ExprPtr cond;
  std::vector<Case> cases;
  std::optional<StmtPtr> defaultCase;
};

struct Stmt {
  std::variant<StmtExpr, StmtReturn, StmtDecl, StmtBlock, StmtFor, StmtIf,
               StmtWhile, StmtBreak, StmtContinue, StmtAsm, StmtSwitch>
      v;
  DiagLoc loc;

  std::unique_ptr<Stmt> clone() const;
};

struct Param {
  TypeName type;
  std::string name;
  Type *resolvedType{nullptr}; // Set by Sema
};

struct Decl;
using DeclPtr = std::unique_ptr<Decl>;

struct StructField {
  TypeName type;
  std::vector<std::string> names;
};

// Attribute for decorator syntax: @trait(copy, clone, drop)
struct Attribute {
  std::string name;              // e.g. "trait"
  std::vector<std::string> args; // e.g. ["copy", "clone", "drop"]
};

struct DeclStruct {
  std::string name;
  std::vector<StructField> fields;
  std::vector<std::string> genericParams; // e.g. struct Box<T> -> {"T"}
  std::vector<Attribute> attributes;      // e.g. @trait(copy, drop)
};

struct EnumItem {
  std::string name;
  std::optional<uint64_t> value;
};
struct DeclEnum {
  std::string name;
  std::vector<EnumItem> items;
};

struct DeclVar {
  TypeName type;
  std::vector<Declarator> declarators;
  bool isExtern{false};
  bool isStatic{false};
  bool isConst{false};
};
struct DeclFunc {
  TypeName ret;
  std::string name;
  std::string mangledName; // Set by Sema
  std::vector<Param> params;
  std::optional<StmtBlock> body;
  bool isExtern{false};
  bool isVariadic{false};
  std::vector<std::string> genericParams; // e.g. T foo<T>(...) -> {"T"}
};

struct DeclCast {
  TypeName target;
  std::string mangledName; // Set by Sema
  std::vector<Param> params;
  std::optional<StmtBlock> body;
  bool isImplicit{false};
};

struct DeclImpl {
  TypeName type;
  std::vector<DeclPtr> methods; // Funcs or Casts
};

struct DeclImport {
  std::vector<std::string> path;
};

struct DeclLink {
  std::string lib;
};

// Trait method signature (no body)
struct TraitMethod {
  TypeName returnType;
  std::string name;
  std::vector<Param> params;
};

// Trait definition: trait Clone<T> { T clone(); }
struct DeclTrait {
  std::string name;
  std::vector<std::string> genericParams; // e.g. trait Foo<T> -> {"T"}
  std::vector<TraitMethod> methods;       // Method signatures
};

struct Decl {
  std::variant<DeclStruct, DeclEnum, DeclVar, DeclFunc, DeclImport, DeclLink,
               DeclImpl, DeclCast, DeclTrait>
      v;
  DiagLoc loc;
};

struct Program {
  std::vector<DeclPtr> decls;
};

} // namespace agc
