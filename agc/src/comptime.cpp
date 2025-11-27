#include "agc/comptime.hpp"
#include <cmath>

namespace agc {

void ComptimeEvaluator::registerUserFunc(const std::string &name,
                                         const DeclFunc *func) {
  userFunctions_[name] = func;
}

void ComptimeEvaluator::registerFunc(const std::string &name, ComptimeFunc func) {
  functions_[name] = std::move(func);
}

void ComptimeEvaluator::setConst(const std::string &name, ComptimeValue value) {
  constants_[name] = std::move(value);
}

void ComptimeEvaluator::pushScope() {
  scopes_.emplace_back();
}

void ComptimeEvaluator::popScope() {
  if (!scopes_.empty())
    scopes_.pop_back();
}

void ComptimeEvaluator::setVar(const std::string &name, ComptimeValue value) {
  if (!scopes_.empty()) {
    scopes_.back()[name] = std::move(value);
  }
}

std::optional<ComptimeValue> ComptimeEvaluator::getVar(const std::string &name) {
  // Check local scopes from innermost to outermost
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto vit = it->find(name);
    if (vit != it->end()) {
      return vit->second;
    }
  }
  // Check globals
  auto it = constants_.find(name);
  if (it != constants_.end()) {
    return it->second;
  }
  return std::nullopt;
}

bool ComptimeEvaluator::isComptime(const Expr &expr) const {
  return std::visit(
      [this](auto const &node) -> bool {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, ExprInt> || std::is_same_v<T, ExprStr>) {
          return true;
        } else if constexpr (std::is_same_v<T, ExprIdent>) {
          // Const cast to call non-const getVar, or duplicate getVar logic
          // Since getVar is private and non-const (though it could be const),
          // let's duplicate the lookup logic for const correctness
          for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
            if (it->count(node.name)) return true;
          }
          if (constants_.count(node.name)) return true;
          return false;
        } else if constexpr (std::is_same_v<T, ExprUnary>) {
          return isComptime(*node.rhs);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          return isComptime(*node.lhs) && isComptime(*node.rhs);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          return isComptime(*node.cond) && isComptime(*node.thenE) &&
                 isComptime(*node.elseE);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          if (!functions_.count(node.callee) && !userFunctions_.count(node.callee))
            return false;
          for (auto &arg : node.args) {
            if (!isComptime(*arg)) return false;
          }
          return true;
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          return isComptime(*node.expr);
        }
        return false;
      },
      expr.v);
}

ComptimeResult ComptimeEvaluator::evaluate(const Expr &expr) {
  return std::visit(
      [this](auto const &node) -> ComptimeResult {
        using T = std::decay_t<decltype(node)>;
        // ... (existing evaluate logic) ...
        if constexpr (std::is_same_v<T, ExprInt>) {
          return ComptimeResult::success(
              ComptimeInt{static_cast<int64_t>(node.value)});
        } else if constexpr (std::is_same_v<T, ExprStr>) {
          return ComptimeResult::success(ComptimeStr{node.value});
        } else if constexpr (std::is_same_v<T, ExprIdent>) {
          auto val = getVar(node.name);
          if (val) return ComptimeResult::success(*val);
          return ComptimeResult::fail("unknown identifier: " + node.name);
        } else if constexpr (std::is_same_v<T, ExprUnary>) {
          auto operandResult = evaluate(*node.rhs);
          if (!operandResult.ok())
            return operandResult;
          return evalUnary(node.op, *operandResult.value);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          auto lhsResult = evaluate(*node.lhs);
          if (!lhsResult.ok())
            return lhsResult;
          auto rhsResult = evaluate(*node.rhs);
          if (!rhsResult.ok())
            return rhsResult;
          return evalBinary(node.op, *lhsResult.value, *rhsResult.value);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          auto condResult = evaluate(*node.cond);
          if (!condResult.ok())
            return condResult;
          auto condBool = getBool(*condResult.value);
          if (!condBool)
            return ComptimeResult::fail("condition must be boolean");
          return *condBool ? evaluate(*node.thenE) : evaluate(*node.elseE);
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          return evaluate(*node.expr);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
            // Check builtins
          if (auto it = functions_.find(node.callee); it != functions_.end()) {
              std::vector<ComptimeValue> args;
              for (const auto &arg : node.args) {
                auto r = evaluate(*arg);
                if (!r.ok()) return r;
                args.push_back(std::move(*r.value));
              }
              return it->second(args);
          }
          // Check user functions
          if (auto it = userFunctions_.find(node.callee); it != userFunctions_.end()) {
              const DeclFunc* func = it->second;
              if (func->params.size() != node.args.size()) {
                  return ComptimeResult::fail("argument count mismatch");
              }
              
              // Evaluate args in current scope
              std::vector<ComptimeValue> argVals;
              for(const auto& arg : node.args) {
                  auto r = evaluate(*arg);
                  if (!r.ok()) return r;
                  argVals.push_back(std::move(*r.value));
              }

              // New scope for function call
              pushScope();
              // Bind params
              for(size_t i=0; i<func->params.size(); ++i) {
                  setVar(func->params[i].name, std::move(argVals[i]));
              }
              
              ComptimeResult res = ComptimeResult::success(ComptimeNull{});
              if (func->body) {
                  res = evalBlock(*func->body);
              }
              
              popScope();
              return res;
          }
          return ComptimeResult::fail("unknown function: " + node.callee);
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
            // Evaluate RHS
            auto rhs = evaluate(*node.rhs);
            if (!rhs.ok()) return rhs;
            
            // LHS must be identifier for now
            if (auto* id = std::get_if<ExprIdent>(&node.lhs->v)) {
                // Find variable location and update
                // Simple update: check scopes
                for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
                    if (it->count(id->name)) {
                        (*it)[id->name] = *rhs.value;
                        return rhs;
                    }
                }
                return ComptimeResult::fail("assignment to unknown variable: " + id->name);
            }
            return ComptimeResult::fail("assignment to non-identifier not supported in comptime");
        } else {
          return ComptimeResult::fail(
              "expression cannot be evaluated at compile time");
        }
      },
      expr.v);
}

static void replace_comptime(ExprPtr &expr, ComptimeEvaluator &evaluator) {
    if (!expr) return;

    // Check if this node is ExprComptime
    if (auto *ct = std::get_if<ExprComptime>(&expr->v)) {
        auto res = evaluator.evaluate(*ct->expr);
        if (res.ok()) {
            // Replace expr with constant
            if (auto *i = std::get_if<ComptimeInt>(&*res.value)) {
                expr->v = ExprInt{static_cast<uint64_t>(i->value)};
            } else if (auto *s = std::get_if<ComptimeStr>(&*res.value)) {
                expr->v = ExprStr{s->value};
            } else if (auto *b = std::get_if<ComptimeBool>(&*res.value)) {
                expr->v = ExprInt{static_cast<uint64_t>(b->value ? 1 : 0)}; // Bool as int for now
            } else {
                // Warning or error?
            }
        } else {
            // Error handling? For now just print to stderr as before, or throw?
            // Since we moved this from main, we might want a better error reporting mechanism.
            // For now, let's keep it simple and maybe print to stderr if we can't propagate.
             fprintf(stderr, "error: comptime evaluation failed: %s\n", res.error.c_str());
             exit(1);
        }
        return;
    }

    // Recurse
    std::visit([&](auto &n) {
        using T = std::decay_t<decltype(n)>;
        if constexpr (std::is_same_v<T, ExprUnary>) {
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
            replace_comptime(n.cond, evaluator);
            replace_comptime(n.thenE, evaluator);
            replace_comptime(n.elseE, evaluator);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
            for (auto &arg : n.args) replace_comptime(arg, evaluator);
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
            replace_comptime(n.base, evaluator);
            replace_comptime(n.index, evaluator);
        } else if constexpr (std::is_same_v<T, ExprMember>) {
            replace_comptime(n.base, evaluator);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
            replace_comptime(n.operand, evaluator);
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
            replace_comptime(n.operand, evaluator);
        }
    }, expr->v);
}

static void visit_stmt_comptime(StmtPtr &stmt, ComptimeEvaluator &evaluator) {
    if (!stmt) return;
    std::visit([&](auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
            replace_comptime(s.expr, evaluator);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
            if (s.expr) replace_comptime(*s.expr, evaluator);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
            for (auto &pair : s.declarators) {
                if (pair.second && *pair.second) replace_comptime(*pair.second, evaluator);
            }
        } else if constexpr (std::is_same_v<T, StmtIf>) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.thenBranch, evaluator);
            if (s.elseBranch) visit_stmt_comptime(*s.elseBranch, evaluator);
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.body, evaluator);
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
            for (auto &st : s.stmts) visit_stmt_comptime(st, evaluator);
        } else if constexpr (std::is_same_v<T, StmtFor>) {
            if (s.init) visit_stmt_comptime(*s.init, evaluator);
            if (s.cond) replace_comptime(*s.cond, evaluator);
            if (s.iter) replace_comptime(*s.iter, evaluator);
            visit_stmt_comptime(s.body, evaluator);
        }
    }, stmt->v);
}

void ComptimeEvaluator::evaluateProgram(Program &prog) {
  for (auto &d : prog.decls) {
      if (auto *f = std::get_if<DeclFunc>(&d->v)) {
          if (f->body) {
              for (auto &s : f->body->stmts) {
                  visit_stmt_comptime(s, *this);
              }
          }
      } else if (auto *v = std::get_if<DeclVar>(&d->v)) {
          for (auto &pair : v->declarators) {
              if (pair.second && *pair.second) {
                  replace_comptime(*pair.second, *this);
              }
          }
      }
  }
}

ComptimeResult ComptimeEvaluator::evalStmt(const Stmt &stmt) {
    return std::visit([this](auto const& s) -> ComptimeResult {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
            return evaluate(*s.expr);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
            if (s.expr) {
                auto res = evaluate(**s.expr);
                if (!res.ok()) return res;
                return ComptimeResult::success(*res.value, ComptimeFlow::Return);
            }
            return ComptimeResult::success(ComptimeNull{}, ComptimeFlow::Return);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
            for(const auto& [name, init] : s.declarators) {
                if (init && *init) {
                    auto val = evaluate(**init);
                    if (!val.ok()) return val;
                    setVar(name, *val.value);
                } else {
                    // Default init to 0/null
                    setVar(name, ComptimeInt{0});
                }
            }
            return ComptimeResult::success(ComptimeNull{});
        } else if constexpr (std::is_same_v<T, StmtIf>) {
            auto cond = evaluate(*s.cond);
            if (!cond.ok()) return cond;
            auto b = getBool(*cond.value);
            if (!b) return ComptimeResult::fail("if condition must be boolean");
            
            if (*b) {
                return evalStmt(*s.thenBranch);
            } else if (s.elseBranch) {
                return evalStmt(**s.elseBranch);
            }
            return ComptimeResult::success(ComptimeNull{});
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
            while(true) {
                auto cond = evaluate(*s.cond);
                if (!cond.ok()) return cond;
                auto b = getBool(*cond.value);
                if (!b) return ComptimeResult::fail("while condition must be boolean");
                if (!*b) break;
                
                auto res = evalStmt(*s.body);
                if (!res.ok()) return res;
                if (res.flow == ComptimeFlow::Return) return res;
                if (res.flow == ComptimeFlow::Break) break;
                if (res.flow == ComptimeFlow::Continue) continue;
            }
            return ComptimeResult::success(ComptimeNull{});
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
            return evalBlock(s);
        }
        return ComptimeResult::success(ComptimeNull{});
    }, stmt.v);
}

ComptimeResult ComptimeEvaluator::evalBlock(const StmtBlock &block) {
    ComptimeValue lastVal = ComptimeNull{};
    for(const auto& s : block.stmts) {
        auto res = evalStmt(*s);
        if (!res.ok()) return res;
        if (res.flow != ComptimeFlow::Normal) return res;
        if (res.value) lastVal = *res.value;
    }
    return ComptimeResult::success(lastVal);
}

ComptimeResult ComptimeEvaluator::evalUnary(TokenKind op,
                                            const ComptimeValue &operand) {
  auto i = getInt(operand);
  auto b = getBool(operand);

  switch (op) {
  case TokenKind::Minus:
    if (i)
      return ComptimeResult::success(ComptimeInt{-*i});
    break;
  case TokenKind::Plus:
    if (i)
      return ComptimeResult::success(ComptimeInt{*i});
    break;
  case TokenKind::Bang:
    if (b)
      return ComptimeResult::success(ComptimeBool{!*b});
    break;
  case TokenKind::Tilde:
    if (i)
      return ComptimeResult::success(ComptimeInt{~*i});
    break;
  default:
    break;
  }
  return ComptimeResult::fail("unsupported unary operation");
}

ComptimeResult ComptimeEvaluator::evalBinary(TokenKind op,
                                             const ComptimeValue &lhs,
                                             const ComptimeValue &rhs) {
  auto li = getInt(lhs);
  auto ri = getInt(rhs);

  // Integer operations
  if (li && ri) {
    switch (op) {
    case TokenKind::Plus:
      return ComptimeResult::success(ComptimeInt{*li + *ri});
    case TokenKind::Minus:
      return ComptimeResult::success(ComptimeInt{*li - *ri});
    case TokenKind::Star:
      return ComptimeResult::success(ComptimeInt{*li * *ri});
    case TokenKind::Slash:
      if (*ri == 0)
        return ComptimeResult::fail("division by zero");
      return ComptimeResult::success(ComptimeInt{*li / *ri});
    case TokenKind::Percent:
      if (*ri == 0)
        return ComptimeResult::fail("modulo by zero");
      return ComptimeResult::success(ComptimeInt{*li % *ri});
    case TokenKind::Amp:
      return ComptimeResult::success(ComptimeInt{*li & *ri});
    case TokenKind::Pipe:
      return ComptimeResult::success(ComptimeInt{*li | *ri});
    case TokenKind::Caret:
      return ComptimeResult::success(ComptimeInt{*li ^ *ri});
    case TokenKind::Shl:
      return ComptimeResult::success(ComptimeInt{*li << *ri});
    case TokenKind::Shr:
      return ComptimeResult::success(ComptimeInt{*li >> *ri});
    case TokenKind::Eq:
      return ComptimeResult::success(ComptimeBool{*li == *ri});
    case TokenKind::Ne:
      return ComptimeResult::success(ComptimeBool{*li != *ri});
    case TokenKind::Lt:
      return ComptimeResult::success(ComptimeBool{*li < *ri});
    case TokenKind::Le:
      return ComptimeResult::success(ComptimeBool{*li <= *ri});
    case TokenKind::Gt:
      return ComptimeResult::success(ComptimeBool{*li > *ri});
    case TokenKind::Ge:
      return ComptimeResult::success(ComptimeBool{*li >= *ri});
    default:
      break;
    }
  }

  // Boolean operations
  auto lb = getBool(lhs);
  auto rb = getBool(rhs);
  if (lb && rb) {
    switch (op) {
    case TokenKind::AndAnd:
      return ComptimeResult::success(ComptimeBool{*lb && *rb});
    case TokenKind::OrOr:
      return ComptimeResult::success(ComptimeBool{*lb || *rb});
    default:
      break;
    }
  }

  // String concatenation
  auto ls = getStr(lhs);
  auto rs = getStr(rhs);
  if (ls && rs && op == TokenKind::Plus) {
    return ComptimeResult::success(ComptimeStr{*ls + *rs});
  }

  return ComptimeResult::fail("unsupported binary operation");
}

} // namespace agc
