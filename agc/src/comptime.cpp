#include "agc/comptime.hpp"
#include "agc/overloaded.hpp"
#include <cmath>

namespace agc {

void ComptimeEvaluator::registerUserFunc(const std::string &name,
                                         const DeclFunc *func) {
  userFunctions_[name] = func;
}

void ComptimeEvaluator::registerFunc(const std::string &name,
                                     ComptimeFunc func) {
  functions_[name] = std::move(func);
}

void ComptimeEvaluator::setConst(const std::string &name, ComptimeValue value) {
  constants_[name] = std::move(value);
}

void ComptimeEvaluator::pushScope() { scopes_.emplace_back(); }

void ComptimeEvaluator::popScope() {
  if (!scopes_.empty())
    scopes_.pop_back();
}

void ComptimeEvaluator::setVar(const std::string &name, ComptimeValue value) {
  if (!scopes_.empty()) {
    scopes_.back()[name] = std::move(value);
  }
}

std::optional<ComptimeValue>
ComptimeEvaluator::getVar(const std::string &name) {
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
      overloaded{
          [](const ExprInt &) { return true; },
          [](const ExprFloat &) { return true; },
          [](const ExprStr &) { return true; },
          [this](const ExprIdent &node) {
            for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
              if (it->count(node.name))
                return true;
            }
            return constants_.count(node.name) > 0;
          },
          [this](const ExprUnary &node) { return isComptime(*node.rhs); },
          [this](const ExprBinary &node) {
            return isComptime(*node.lhs) && isComptime(*node.rhs);
          },
          [this](const ExprCond &node) {
            return isComptime(*node.cond) && isComptime(*node.thenE) &&
                   isComptime(*node.elseE);
          },
          [this](const ExprCall &node) {
            if (!functions_.count(node.callee) &&
                !userFunctions_.count(node.callee))
              return false;
            for (auto &arg : node.args) {
              if (!isComptime(*arg))
                return false;
            }
            return true;
          },
          [this](const ExprComptime &node) { return isComptime(*node.expr); },
          [this](const ExprInitList &node) {
            for (auto &v : node.values) {
              if (v.designator && !isComptime(**v.designator))
                return false;
              if (!isComptime(*v.value))
                return false;
            }
            return true;
          },
          [](const auto &) { return false; },
      },
      expr.v);
}

ComptimeResult ComptimeEvaluator::evaluate(const Expr &expr) {
  return std::visit(
      overloaded{
          [](const ExprInt &node) -> ComptimeResult {
            return ComptimeResult::success(
                ComptimeInt{static_cast<int64_t>(node.value)});
          },
          [](const ExprFloat &node) -> ComptimeResult {
            return ComptimeResult::success(ComptimeFloat{node.value});
          },
          [](const ExprStr &node) -> ComptimeResult {
            return ComptimeResult::success(ComptimeStr{node.value});
          },
          [this](const ExprIdent &node) -> ComptimeResult {
            auto val = getVar(node.name);
            if (val)
              return ComptimeResult::success(*val);
            return ComptimeResult::fail("unknown identifier: " + node.name);
          },
          [this](const ExprUnary &node) -> ComptimeResult {
            auto operandResult = evaluate(*node.rhs);
            if (!operandResult.ok())
              return operandResult;
            return evalUnary(node.op, *operandResult.value);
          },
          [this](const ExprBinary &node) -> ComptimeResult {
            auto lhsResult = evaluate(*node.lhs);
            if (!lhsResult.ok())
              return lhsResult;
            auto rhsResult = evaluate(*node.rhs);
            if (!rhsResult.ok())
              return rhsResult;
            return evalBinary(node.op, *lhsResult.value, *rhsResult.value);
          },
          [this](const ExprCond &node) -> ComptimeResult {
            auto condResult = evaluate(*node.cond);
            if (!condResult.ok())
              return condResult;
            auto condBool = getBool(*condResult.value);
            if (!condBool)
              return ComptimeResult::fail("condition must be boolean");
            return *condBool ? evaluate(*node.thenE) : evaluate(*node.elseE);
          },
          [this](const ExprComptime &node) -> ComptimeResult {
            return evaluate(*node.expr);
          },
          [this](const ExprCall &node) -> ComptimeResult {
            // Check builtins
            if (auto it = functions_.find(node.callee);
                it != functions_.end()) {
              std::vector<ComptimeValue> args;
              for (const auto &arg : node.args) {
                auto r = evaluate(*arg);
                if (!r.ok())
                  return r;
                args.push_back(std::move(*r.value));
              }
              return it->second(args);
            }
            // Check user functions
            if (auto it = userFunctions_.find(node.callee);
                it != userFunctions_.end()) {
              const DeclFunc *func = it->second;
              if (func->params.size() != 0 &&
                  func->params.size() != node.args.size()) {
                return ComptimeResult::fail("argument count mismatch");
              }

              // Evaluate args in current scope
              std::vector<ComptimeValue> argVals;
              for (const auto &arg : node.args) {
                auto r = evaluate(*arg);
                if (!r.ok())
                  return r;
                argVals.push_back(std::move(*r.value));
              }

              // New scope for function call
              pushScope();
              // Bind params
              for (size_t i = 0; i < func->params.size(); ++i) {
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
          },
          [this](const ExprAssign &node) -> ComptimeResult {
            // Evaluate RHS
            auto rhs = evaluate(*node.rhs);
            if (!rhs.ok())
              return rhs;

            // LHS must be identifier for now
            if (auto *id = std::get_if<ExprIdent>(&node.lhs->v)) {
              // Find variable location and update
              for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
                if (it->count(id->name)) {
                  (*it)[id->name] = *rhs.value;
                  return rhs;
                }
              }
              return ComptimeResult::fail("assignment to unknown variable: " +
                                          id->name);
            }
            return ComptimeResult::fail(
                "assignment to non-identifier not supported in comptime");
          },
          [](const ExprInitList &) -> ComptimeResult {
            return ComptimeResult::fail(
                "initializer list cannot be evaluated at "
                "compile time without type context");
          },
          [](const auto &) -> ComptimeResult {
            return ComptimeResult::fail(
                "expression cannot be evaluated at compile time");
          },
      },
      expr.v);
}

static void replace_comptime(ExprPtr &expr, ComptimeEvaluator &evaluator) {
  if (!expr)
    return;

  // Check if this node is ExprComptime
  if (auto *ct = std::get_if<ExprComptime>(&expr->v)) {
    auto res = evaluator.evaluate(*ct->expr);
    if (res.ok()) {
      // Replace expr with constant
      std::visit(
          overloaded{
              [&](const ComptimeInt &i) {
                expr->v = ExprInt{static_cast<uint64_t>(i.value)};
              },
              [&](const ComptimeFloat &f) { expr->v = ExprFloat{f.value}; },
              [&](const ComptimeStr &s) { expr->v = ExprStr{s.value}; },
              [&](const ComptimeBool &b) {
                expr->v = ExprInt{static_cast<uint64_t>(b.value ? 1 : 0)};
              },
              [](const auto &) {
                // Warning or error for unhandled types
              },
          },
          *res.value);
    } else {
      fprintf(stderr, "error: comptime evaluation failed: %s\n",
              res.error.c_str());
      exit(1);
    }
    return;
  }

  // Recurse into child expressions
  std::visit(
      overloaded{
          [&](ExprUnary &n) { replace_comptime(n.rhs, evaluator); },
          [&](ExprBinary &n) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
          },
          [&](ExprAssign &n) {
            replace_comptime(n.lhs, evaluator);
            replace_comptime(n.rhs, evaluator);
          },
          [&](ExprCond &n) {
            replace_comptime(n.cond, evaluator);
            replace_comptime(n.thenE, evaluator);
            replace_comptime(n.elseE, evaluator);
          },
          [&](ExprCall &n) {
            for (auto &arg : n.args)
              replace_comptime(arg, evaluator);
          },
          [&](ExprIndex &n) {
            replace_comptime(n.base, evaluator);
            replace_comptime(n.index, evaluator);
          },
          [&](ExprMember &n) { replace_comptime(n.base, evaluator); },
          [&](ExprMethodCall &n) {
            replace_comptime(n.base, evaluator);
            for (auto &arg : n.args) {
              replace_comptime(arg, evaluator);
            }
          },
          [&](ExprAddressOf &n) { replace_comptime(n.operand, evaluator); },
          [&](ExprDeref &n) { replace_comptime(n.operand, evaluator); },
          [&](ExprCast &n) { replace_comptime(n.expr, evaluator); },
          [&](ExprInitList &n) {
            for (auto &item : n.values) {
              if (item.designator)
                replace_comptime(*item.designator, evaluator);
              replace_comptime(item.value, evaluator);
            }
          },
          [](auto &) { /* leaf nodes - nothing to recurse into */ },
      },
      expr->v);
}

static void visit_stmt_comptime(StmtPtr &stmt, ComptimeEvaluator &evaluator) {
  if (!stmt)
    return;
  std::visit(
      overloaded{
          [&](StmtExpr &s) { replace_comptime(s.expr, evaluator); },
          [&](StmtReturn &s) {
            if (s.expr)
              replace_comptime(*s.expr, evaluator);
          },
          [&](StmtDecl &s) {
            for (auto &decl : s.declarators) {
              if (decl.init && *decl.init)
                replace_comptime(*decl.init, evaluator);
            }
          },
          [&](StmtIf &s) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.thenBranch, evaluator);
            if (s.elseBranch)
              visit_stmt_comptime(*s.elseBranch, evaluator);
          },
          [&](StmtWhile &s) {
            replace_comptime(s.cond, evaluator);
            visit_stmt_comptime(s.body, evaluator);
          },
          [&](StmtBlock &s) {
            for (auto &st : s.stmts)
              visit_stmt_comptime(st, evaluator);
          },
          [&](StmtFor &s) {
            if (s.init)
              visit_stmt_comptime(*s.init, evaluator);
            if (s.cond)
              replace_comptime(*s.cond, evaluator);
            if (s.iter)
              replace_comptime(*s.iter, evaluator);
            visit_stmt_comptime(s.body, evaluator);
          },
          [&](StmtSwitch &s) {
            replace_comptime(s.cond, evaluator);
            for (auto &c : s.cases) {
              for (auto &v : c.values)
                replace_comptime(v, evaluator);
              visit_stmt_comptime(c.body, evaluator);
            }
            if (s.defaultCase)
              visit_stmt_comptime(*s.defaultCase, evaluator);
          },
          [](auto &) { /* StmtBreak, StmtContinue, StmtAsm - nothing to do */ },
      },
      stmt->v);
}

void ComptimeEvaluator::evaluateProgram(Program &prog) {
  for (auto &d : prog.decls) {
    std::visit(overloaded{
                   [this](DeclFunc &f) {
                     if (f.body) {
                       for (auto &s : f.body->stmts) {
                         visit_stmt_comptime(s, *this);
                       }
                     }
                   },
                   [this](DeclVar &v) {
                     for (auto &decl : v.declarators) {
                       if (decl.init && *decl.init) {
                         replace_comptime(*decl.init, *this);
                       }
                     }
                   },
                   [this](DeclImpl &impl) {
                     for (auto &m : impl.methods) {
                       if (auto *f = std::get_if<DeclFunc>(&m->v)) {
                         if (f->body) {
                           for (auto &s : f->body->stmts) {
                             visit_stmt_comptime(s, *this);
                           }
                         }
                       }
                     }
                   },
                   [](auto &) { /* other declaration types */ },
               },
               d->v);
  }
}

ComptimeResult ComptimeEvaluator::evalStmt(const Stmt &stmt) {
  return std::visit(
      overloaded{
          [this](const StmtExpr &s) -> ComptimeResult {
            return evaluate(*s.expr);
          },
          [this](const StmtReturn &s) -> ComptimeResult {
            if (s.expr) {
              auto res = evaluate(**s.expr);
              if (!res.ok())
                return res;
              return ComptimeResult::success(*res.value, ComptimeFlow::Return);
            }
            return ComptimeResult::success(ComptimeNull{},
                                           ComptimeFlow::Return);
          },
          [this](const StmtDecl &s) -> ComptimeResult {
            for (const auto &decl : s.declarators) {
              if (decl.init && *decl.init) {
                auto val = evaluate(**decl.init);
                if (!val.ok())
                  return val;
                setVar(decl.name, *val.value);
              } else {
                // Default init to 0/null
                setVar(decl.name, ComptimeInt{0});
              }
            }
            return ComptimeResult::success(ComptimeNull{});
          },
          [this](const StmtIf &s) -> ComptimeResult {
            auto cond = evaluate(*s.cond);
            if (!cond.ok())
              return cond;
            auto b = getBool(*cond.value);
            if (!b)
              return ComptimeResult::fail("if condition must be boolean");

            if (*b) {
              return evalStmt(*s.thenBranch);
            } else if (s.elseBranch) {
              return evalStmt(**s.elseBranch);
            }
            return ComptimeResult::success(ComptimeNull{});
          },
          [this](const StmtWhile &s) -> ComptimeResult {
            while (true) {
              auto cond = evaluate(*s.cond);
              if (!cond.ok())
                return cond;
              auto b = getBool(*cond.value);
              if (!b)
                return ComptimeResult::fail("while condition must be boolean");
              if (!*b)
                break;

              auto res = evalStmt(*s.body);
              if (!res.ok())
                return res;
              if (res.flow == ComptimeFlow::Return)
                return res;
              if (res.flow == ComptimeFlow::Break)
                break;
              if (res.flow == ComptimeFlow::Continue)
                continue;
            }
            return ComptimeResult::success(ComptimeNull{});
          },
          [this](const StmtBlock &s) -> ComptimeResult { return evalBlock(s); },
          [](const StmtBreak &) -> ComptimeResult {
            return ComptimeResult::success(ComptimeNull{}, ComptimeFlow::Break);
          },
          [](const StmtContinue &) -> ComptimeResult {
            return ComptimeResult::success(ComptimeNull{},
                                           ComptimeFlow::Continue);
          },
          [](const auto &) -> ComptimeResult {
            return ComptimeResult::success(ComptimeNull{});
          },
      },
      stmt.v);
}

ComptimeResult ComptimeEvaluator::evalBlock(const StmtBlock &block) {
  ComptimeValue lastVal = ComptimeNull{};
  for (const auto &s : block.stmts) {
    auto res = evalStmt(*s);
    if (!res.ok())
      return res;
    if (res.flow != ComptimeFlow::Normal)
      return res;
    if (res.value)
      lastVal = *res.value;
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