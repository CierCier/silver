#pragma once
#include "agc/ast.hpp"
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
    Type *type{nullptr};
  };

  // Map from variable name to info.
  // Since we have scopes, we need a stack of maps.
  std::vector<std::unordered_map<std::string, VarInfo>> scopes_;
  std::unordered_map<std::string, Type *> structTypes_;
  std::unordered_map<std::string, Type *> enumTypes_;

  void pushScope();
  void popScope();
  void declareVar(const std::string &name, const DiagLoc &loc,
                  bool *isConstFlag, Type *type);
  void markMutated(const std::string &name, const DiagLoc &loc);
  Type *checkVar(const std::string &name, const DiagLoc &loc);

  Type *resolveType(const TypeName &typeName);
  bool checkType(Type *expected, Type *actual, const DiagLoc &loc);

  void visit(Stmt &stmt);
  void visit(Expr &expr);
  void visit(Decl &decl);
};

} // namespace agc
