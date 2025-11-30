#include "agc/ast.hpp"

namespace agc {

// Helper to clone vector of unique_ptrs
template <typename T>
std::vector<std::unique_ptr<T>>
cloneVec(const std::vector<std::unique_ptr<T>> &v) {
  std::vector<std::unique_ptr<T>> res;
  res.reserve(v.size());
  for (const auto &e : v) {
    if (e)
      res.push_back(e->clone());
    else
      res.push_back(nullptr);
  }
  return res;
}

// Helper to clone optional unique_ptr
template <typename T>
std::optional<std::unique_ptr<T>>
cloneOpt(const std::optional<std::unique_ptr<T>> &o) {
  if (o && *o)
    return (*o)->clone();
  return std::nullopt;
}

std::unique_ptr<Expr> Expr::clone() const {
  auto res = std::make_unique<Expr>();
  res->loc = loc;
  res->type = type; // Type might need re-resolution, but copying pointer is
                    // fine for now

  std::visit(
      [&](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, ExprIdent> ||
                      std::is_same_v<T, ExprInt> ||
                      std::is_same_v<T, ExprFloat> ||
                      std::is_same_v<T, ExprStr>) {
          res->v = arg;
        } else if constexpr (std::is_same_v<T, ExprUnary>) {
          res->v = ExprUnary{arg.op, arg.rhs->clone()};
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          res->v = ExprBinary{arg.op, arg.lhs->clone(), arg.rhs->clone()};
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
          res->v = ExprAssign{arg.lhs->clone(), arg.rhs->clone(), arg.op};
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          res->v = ExprCond{arg.cond->clone(), arg.thenE->clone(),
                            arg.elseE->clone()};
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          res->v = ExprCall{arg.callee, arg.mangledCallee, cloneVec(arg.args),
                            arg.genericArgs};
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          res->v = ExprIndex{arg.base->clone(), arg.index->clone()};
        } else if constexpr (std::is_same_v<T, ExprMember>) {
          res->v = ExprMember{arg.base->clone(), arg.member, arg.ptr};
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          res->v = ExprComptime{arg.expr->clone()};
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          res->v = ExprAddressOf{arg.operand->clone()};
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          res->v = ExprDeref{arg.operand->clone()};
        } else if constexpr (std::is_same_v<T, ExprCast>) {
          res->v = ExprCast{arg.expr->clone(), arg.target, arg.customCastFunc};
        } else if constexpr (std::is_same_v<T, ExprInitList>) {
          std::vector<InitItem> clonedValues;
          clonedValues.reserve(arg.values.size());
          for (const auto &item : arg.values) {
            clonedValues.push_back(
                {cloneOpt(item.designator), item.value->clone()});
          }
          res->v = ExprInitList{std::move(clonedValues)};
        }
      },
      v);

  return res;
}

StmtBlock StmtBlock::clone() const {
  StmtBlock res;
  res.stmts = cloneVec(stmts);
  return res;
}

std::unique_ptr<Stmt> Stmt::clone() const {
  auto res = std::make_unique<Stmt>();
  res->loc = loc;

  std::visit(
      [&](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          res->v = StmtExpr{arg.expr->clone()};
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          res->v = StmtReturn{cloneOpt(arg.expr)};
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          StmtDecl sd;
          sd.type = arg.type;
          sd.isConst = arg.isConst;
          sd.resolvedType = arg.resolvedType;
          for (const auto &d : arg.declarators) {
            sd.declarators.push_back({d.name, d.loc, cloneOpt(d.init)});
          }
          res->v = std::move(sd);
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          res->v = arg.clone();
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          res->v = StmtFor{arg.init ? (*arg.init)->clone() : nullptr,
                           cloneOpt(arg.cond), cloneOpt(arg.iter),
                           arg.body->clone()};
        } else if constexpr (std::is_same_v<T, StmtIf>) {
          res->v =
              StmtIf{arg.cond->clone(), arg.thenBranch->clone(),
                     arg.elseBranch ? (*arg.elseBranch)->clone() : nullptr};
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          res->v = StmtWhile{arg.cond->clone(), arg.body->clone()};
        } else if constexpr (std::is_same_v<T, StmtBreak> ||
                             std::is_same_v<T, StmtContinue>) {
          res->v = arg;
        } else if constexpr (std::is_same_v<T, StmtAsm>) {
          res->v = arg;
        } else if constexpr (std::is_same_v<T, StmtSwitch>) {
          StmtSwitch sw;
          sw.cond = arg.cond->clone();
          for (const auto &c : arg.cases) {
            sw.cases.push_back({cloneVec(c.values), c.body->clone()});
          }
          if (arg.defaultCase)
            sw.defaultCase = (*arg.defaultCase)->clone();
          res->v = std::move(sw);
        }
      },
      v);

  return res;
}

} // namespace agc
