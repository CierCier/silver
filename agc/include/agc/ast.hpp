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

struct InitItem {
  std::optional<ExprPtr> designator; // [index] or .field (future)
  ExprPtr value;
};

struct ExprInitList {
  std::vector<InitItem> values;
};

struct Expr {
  std::variant<ExprIdent, ExprInt, ExprFloat, ExprStr, ExprUnary, ExprBinary,
               ExprAssign, ExprCond, ExprCall, ExprIndex, ExprMember,
               ExprComptime, ExprAddressOf, ExprDeref, ExprCast, ExprInitList>
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
struct DeclStruct {
  std::string name;
  std::vector<StructField> fields;
  std::vector<std::string> genericParams; // e.g. struct Box<T> -> {"T"}
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

struct Decl {
  std::variant<DeclStruct, DeclEnum, DeclVar, DeclFunc, DeclImport, DeclLink,
               DeclImpl, DeclCast>
      v;
  DiagLoc loc;
};

struct Program {
  std::vector<DeclPtr> decls;
};

} // namespace agc
