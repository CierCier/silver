#pragma once
#include "agc/ast.hpp"
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "agc/diagnostics.hpp"
#include "agc/type.hpp"

namespace agc {

class SemanticAnalyzer {
public:
  explicit SemanticAnalyzer(DiagnosticEngine &diags, TypeContext &typeCtx)
      : diags_(diags), typeCtx_(typeCtx) {}
  void analyze(Program &prog);

  TypeContext &getTypeContext() { return typeCtx_; }
  const std::unordered_map<std::string, Type *> &getStructTypes() const {
    return structTypes_;
  }
  const std::vector<DeclPtr> &getInstantiatedDecls() const {
    return instantiatedDecls_;
  }

private:
  DiagnosticEngine &diags_;
  TypeContext &typeCtx_;

  struct VarInfo {
    bool isMutated{false};
    // Pointer to the bool flag in the AST node that we will update
    bool *isConstFlag{nullptr};
    Type *type{nullptr};
  };

  struct FuncInfo {
    Type *returnType{nullptr};
    std::vector<Type *> paramTypes;
    std::string mangledName;
  };

  // Map from variable name to info.
  // Since we have scopes, we need a stack of maps.
  std::vector<std::unordered_map<std::string, VarInfo>> scopes_;
  std::unordered_map<std::string, Type *> structTypes_;
  std::unordered_map<std::string, Type *> enumTypes_;
  std::unordered_map<std::string, FuncInfo> functions_;

  // Generic templates
  std::unordered_map<std::string, DeclStruct *> genericStructs_;
  std::unordered_map<std::string, DeclFunc *> genericFunctions_;
  std::multimap<std::string, DeclImpl *> genericImpls_; // struct name -> impl
  std::vector<DeclPtr> instantiatedDecls_; // Owns instantiated generic
                                           // functions/structs if needed
  // Type parameter scopes (for T -> i32 substitution)
  std::vector<std::unordered_map<std::string, Type *>> typeScopes_;

  void pushTypeScope();
  void popTypeScope();
  void declareTypeAlias(const std::string &name, Type *type);

  void pushScope();
  void popScope();
  void declareVar(const std::string &name, const DiagLoc &loc,
                  bool *isConstFlag, Type *type);
  void markMutated(const std::string &name, const DiagLoc &loc);
  Type *checkVar(const std::string &name, const DiagLoc &loc);

  Type *resolveType(const TypeName &typeName);
  void instantiateStruct(DeclStruct *ds, const std::vector<Type *> &args);
  void instantiateImpl(DeclImpl *impl, DeclStruct *ds,
                       const std::vector<Type *> &args,
                       StructType *targetStruct);
  void instantiateFunction(DeclFunc *df, const std::vector<Type *> &args,
                           const DiagLoc &loc);
  bool checkType(Type *expected, Type *actual, const DiagLoc &loc);

  void visit(Stmt &stmt);
  void visit(Expr &expr);
  void visit(Decl &decl);
};

} // namespace agc
