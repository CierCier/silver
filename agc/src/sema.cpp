#include "agc/sema.hpp"
#include <iostream>

namespace agc {

void SemanticAnalyzer::analyze(Program &prog) {
  pushScope(); // Global scope

  // First pass: register globals
  for (auto &d : prog.decls) {
    if (auto *dv = std::get_if<DeclVar>(&d->v)) {
      for (auto &pair : dv->declarators) {
        declareVar(pair.first, &dv->isConst);
      }
    }
  }

  // Second pass: visit functions and inits
  for (auto &d : prog.decls) {
    visit(*d);
  }

  popScope();
}

void SemanticAnalyzer::pushScope() {
  scopes_.emplace_back();
}

void SemanticAnalyzer::popScope() {
  if (scopes_.empty()) return;
  
  // Before popping, check for non-mutated variables and mark them const
  for (auto &pair : scopes_.back()) {
    if (!pair.second.isMutated && pair.second.isConstFlag) {
      *pair.second.isConstFlag = true;
    }
  }
  scopes_.pop_back();
}

void SemanticAnalyzer::declareVar(const std::string &name, bool *isConstFlag) {
  if (scopes_.empty()) return;
  scopes_.back()[name] = VarInfo{false, isConstFlag};
  // Initialize as false, will be set to true at end of scope if not mutated
  if (isConstFlag) *isConstFlag = false; 
}

void SemanticAnalyzer::markMutated(const std::string &name) {
  // Search from innermost scope
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto vit = it->find(name);
    if (vit != it->end()) {
      vit->second.isMutated = true;
      return;
    }
  }
}

void SemanticAnalyzer::visit(Decl &decl) {
  if (auto *df = std::get_if<DeclFunc>(&decl.v)) {
    if (df->body) {
      pushScope();
      // Register params (we don't mark params as const usually)
      // For now, let's not touch params.
      for (auto &p : df->params) {
          if (!scopes_.empty()) scopes_.back()[p.name] = VarInfo{true, nullptr}; // Assume mutated/non-const
      }
      
      // Visit body
      for (auto &s : df->body->stmts) {
        visit(*s);
      }
      popScope();
    }
  } else if (auto *dv = std::get_if<DeclVar>(&decl.v)) {
      // Visit initializers
      for (auto &pair : dv->declarators) {
          if (pair.second && *pair.second) {
              visit(**pair.second);
          }
      }
  }
}

void SemanticAnalyzer::visit(Stmt &stmt) {
    std::visit([this](auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
            visit(*s.expr);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
            if (s.expr) visit(**s.expr);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
            for (auto &pair : s.declarators) {
                if (pair.second && *pair.second) {
                    visit(**pair.second);
                }
                declareVar(pair.first, &s.isConst);
            }
        } else if constexpr (std::is_same_v<T, StmtIf>) {
            visit(*s.cond);
            visit(*s.thenBranch);
            if (s.elseBranch) visit(**s.elseBranch);
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
            visit(*s.cond);
            visit(*s.body);
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
            pushScope();
            for (auto &st : s.stmts) visit(*st);
            popScope();
        } else if constexpr (std::is_same_v<T, StmtFor>) {
             pushScope();
             if (s.init) visit(**s.init);
             if (s.cond) visit(**s.cond);
             if (s.iter) visit(**s.iter);
             visit(*s.body);
             popScope();
        }
        // Others: Break, Continue, Asm - nothing to do
    }, stmt.v);
}

void SemanticAnalyzer::visit(Expr &expr) {
    std::visit([this](auto &n) {
        using T = std::decay_t<decltype(n)>;
        if constexpr (std::is_same_v<T, ExprUnary>) {
            visit(*n.rhs);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
            visit(*n.lhs);
            visit(*n.rhs);
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
            visit(*n.rhs);
            // Check LHS for mutation
            if (auto *id = std::get_if<ExprIdent>(&n.lhs->v)) {
                markMutated(id->name);
            } else if (auto *idx = std::get_if<ExprIndex>(&n.lhs->v)) {
                // Array assignment: a[i] = x. Mutates a.
                if (auto *baseId = std::get_if<ExprIdent>(&idx->base->v)) {
                    markMutated(baseId->name);
                }
                visit(*idx->index); // index is read
            } else if (auto *dr = std::get_if<ExprDeref>(&n.lhs->v)) {
                visit(*dr->operand); // *ptr = x. ptr is read.
            }
            // If LHS is complex, we might miss some mutations, but this covers basics.
        } else if constexpr (std::is_same_v<T, ExprCond>) {
            visit(*n.cond);
            visit(*n.thenE);
            visit(*n.elseE);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
             for(auto &arg : n.args) visit(*arg);
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
             visit(*n.base);
             visit(*n.index);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
             // &x -> x escapes, assume mutated
             if (auto *id = std::get_if<ExprIdent>(&n.operand->v)) {
                 markMutated(id->name);
             }
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
             visit(*n.operand);
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
             visit(*n.expr);
        }
        // Ident, Int, Str: nothing to do
    }, expr.v);
}

} // namespace agc
