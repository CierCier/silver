#pragma once
#include "agc/ast.hpp"
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace agc {

class SemanticAnalyzer {
public:
  void analyze(Program &prog);

private:
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
  void markMutated(const std::string &name);

  void visit(Stmt &stmt);
  void visit(Expr &expr);
  void visit(Decl &decl);
};

} // namespace agc
