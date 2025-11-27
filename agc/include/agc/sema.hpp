#pragma once
#include "agc/ast.hpp"
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "agc/diagnostics.hpp"

namespace agc {

class SemanticAnalyzer {
public:
  explicit SemanticAnalyzer(DiagnosticEngine &diags) : diags_(diags) {}
  void analyze(Program &prog);

private:
  DiagnosticEngine &diags_;

  struct VarInfo {
    bool isMutated{false};
    // Pointer to the bool flag in the AST node that we will update
    bool *isConstFlag{nullptr}; 
  };

  // Map from variable name to info. 
  // Since we have scopes, we need a stack of maps.
  std::vector<std::unordered_map<std::string, VarInfo>> scopes_;

  void pushScope();
  void popScope();
  void declareVar(const std::string &name, bool *isConstFlag);
  void markMutated(const std::string &name, const DiagLoc &loc);
  void checkVar(const std::string &name, const DiagLoc &loc);

  void visit(Stmt &stmt);
  void visit(Expr &expr);
  void visit(Decl &decl);
};

} // namespace agc
