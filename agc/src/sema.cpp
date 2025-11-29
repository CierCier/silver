#include "agc/sema.hpp"
#include <iostream>

namespace agc {

void SemanticAnalyzer::analyze(Program &prog) {
  pushScope(); // Global scope

  // First pass: register globals
  for (auto &d : prog.decls) {
    if (auto *dv = std::get_if<DeclVar>(&d->v)) {
      for (auto &decl : dv->declarators) {
        declareVar(decl.name, decl.loc, &dv->isConst);
      }
    }
  }

  // Second pass: visit functions and inits
  for (auto &d : prog.decls) {
    visit(*d);
  }

  popScope();
}

void SemanticAnalyzer::pushScope() { scopes_.emplace_back(); }

void SemanticAnalyzer::popScope() {
  if (scopes_.empty())
    return;

  // Before popping, check for non-mutated variables and mark them const
  for (auto &pair : scopes_.back()) {
    if (!pair.second.isMutated && pair.second.isConstFlag) {
      *pair.second.isConstFlag = true;
    }
  }
  scopes_.pop_back();
}

void SemanticAnalyzer::checkVar(const std::string &name, const DiagLoc &loc) {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    if (it->find(name) != it->end())
      return;
  }
  diags_.report(DiagLevel::Error, loc, "undefined variable '" + name + "'");
}

void SemanticAnalyzer::declareVar(const std::string &name, const DiagLoc &loc,
                                  bool *isConstFlag) {
  if (scopes_.empty())
    return;
  if (scopes_.back().count(name)) {
    diags_.report(DiagLevel::Error, loc, "redefinition of '" + name + "'");
    // Note: we could report previous definition location if we stored it in
    // VarInfo
    return;
  }
  scopes_.back()[name] = VarInfo{false, isConstFlag};
  // Initialize as false, will be set to true at end of scope if not mutated
  if (isConstFlag)
    *isConstFlag = false;
}

void SemanticAnalyzer::markMutated(const std::string &name,
                                   const DiagLoc &loc) {
  // Search from innermost scope
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto vit = it->find(name);
    if (vit != it->end()) {
      vit->second.isMutated = true;
      return;
    }
  }
  diags_.report(DiagLevel::Error, loc, "undefined variable '" + name + "'");
}

void SemanticAnalyzer::visit(Decl &decl) {
  if (auto *df = std::get_if<DeclFunc>(&decl.v)) {
    if (df->body) {
      pushScope();
      // Register params (we don't mark params as const usually)
      // For now, let's not touch params.
      for (auto &p : df->params) {
        if (!scopes_.empty())
          scopes_.back()[p.name] =
              VarInfo{true, nullptr}; // Assume mutated/non-const
      }

      // Visit body
      for (auto &s : df->body->stmts) {
        visit(*s);
      }
      popScope();
    }
  } else if (auto *dv = std::get_if<DeclVar>(&decl.v)) {
    // Visit initializers
    for (auto &decl : dv->declarators) {
      if (decl.init && *decl.init) {
        visit(**decl.init);
      }
    }
  }
}

void SemanticAnalyzer::visit(Stmt &stmt) {
  std::visit(
      [this, &stmt](auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          visit(*s.expr);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          if (s.expr)
            visit(**s.expr);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          for (auto &decl : s.declarators) {
            if (decl.init && *decl.init) {
              visit(**decl.init);
            }
            declareVar(decl.name, decl.loc, &s.isConst);
          }
        } else if constexpr (std::is_same_v<T, StmtIf>) {
          visit(*s.cond);
          visit(*s.thenBranch);
          if (s.elseBranch)
            visit(**s.elseBranch);
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          visit(*s.cond);
          visit(*s.body);
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          pushScope();
          for (auto &st : s.stmts)
            visit(*st);
          popScope();
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          pushScope();
          if (s.init)
            visit(**s.init);
          if (s.cond)
            visit(**s.cond);
          if (s.iter)
            visit(**s.iter);
          visit(*s.body);
          popScope();
        }
        // Others: Break, Continue, Asm - nothing to do
      },
      stmt.v);
}

void SemanticAnalyzer::visit(Expr &expr) {
  std::visit(
      [this, &expr](auto &n) {
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
            markMutated(id->name, n.lhs->loc);
          } else if (auto *idx = std::get_if<ExprIndex>(&n.lhs->v)) {
            // Array assignment: a[i] = x. Mutates a.
            if (auto *baseId = std::get_if<ExprIdent>(&idx->base->v)) {
              markMutated(baseId->name, idx->base->loc);
            }
            visit(*idx->index); // index is read
          } else if (auto *dr = std::get_if<ExprDeref>(&n.lhs->v)) {
            visit(*dr->operand); // *ptr = x. ptr is read.
          }
          // If LHS is complex, we might miss some mutations, but this covers
          // basics.
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          visit(*n.cond);
          visit(*n.thenE);
          visit(*n.elseE);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          // Check callee if it's a function?
          // For now, we only track variables.
          // But if callee is a variable (function pointer), we should check it.
          // ExprCall has `callee` as string.
          // We should check if `callee` is defined?
          // But `callee` could be a global function which we don't track in
          // `scopes_` fully (we only track vars). Wait, `analyze` first pass
          // registers globals. But `declareVar` is for `DeclVar`. `DeclFunc`
          // are not registered in `scopes_`. So we can't check function
          // existence yet with this `SemanticAnalyzer`. It seems
          // `SemanticAnalyzer` is mainly for const correctness of variables.
          for (auto &arg : n.args)
            visit(*arg);
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          visit(*n.base);
          visit(*n.index);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          // &x -> x escapes, assume mutated
          if (auto *id = std::get_if<ExprIdent>(&n.operand->v)) {
            markMutated(id->name, n.operand->loc);
          }
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          visit(*n.operand);
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          visit(*n.expr);
        } else if constexpr (std::is_same_v<T, ExprIdent>) {
          checkVar(n.name, expr.loc);
        }
        // Int, Float, Str: nothing to do
      },
      expr.v);
}

} // namespace agc
