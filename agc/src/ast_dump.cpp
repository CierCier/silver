#include "agc/ast_dump.hpp"
#include <functional>
#include <iostream>

namespace agc {

static void indent(std::ostream &os, int n) {
  for (int i = 0; i < n; ++i)
    os << ' ';
}

static void dumpType(const TypeName &t, std::ostream &os) {
  os << t.name;
  for (unsigned i = 0; i < t.pointerDepth; ++i)
    os << '*';
  for (const auto &dim : t.arrayDims) {
    os << '[';
    if (dim)
      os << *dim;
    os << ']';
  }
}

static const char *opName(TokenKind k) {
  switch (k) {
  case TokenKind::Plus:
    return "+";
  case TokenKind::Minus:
    return "-";
  case TokenKind::Star:
    return "*";
  case TokenKind::Slash:
    return "/";
  case TokenKind::Percent:
    return "%";
  case TokenKind::AndAnd:
    return "&&";
  case TokenKind::OrOr:
    return "||";
  case TokenKind::Eq:
    return "==";
  case TokenKind::Ne:
    return "!=";
  case TokenKind::Lt:
    return "<";
  case TokenKind::Le:
    return "<=";
  case TokenKind::Gt:
    return ">";
  case TokenKind::Ge:
    return ">=";
  case TokenKind::Assign:
    return "=";
  case TokenKind::PlusAssign:
    return "+=";
  case TokenKind::MinusAssign:
    return "-=";
  case TokenKind::StarAssign:
    return "*=";
  case TokenKind::SlashAssign:
    return "/=";
  case TokenKind::PercentAssign:
    return "%=";
  case TokenKind::PlusPlus:
    return "++";
  case TokenKind::MinusMinus:
    return "--";
  default:
    return "?";
  }
}

static void dumpExpr(const Expr &e, std::ostream &os, int ind);
static void dumpStmt(const Stmt &s, std::ostream &os, int ind);
static void dumpStmtBlock(const StmtBlock &b, std::ostream &os, int ind);

static void dumpExpr(const Expr &e, std::ostream &os, int ind) {
  std::visit(
      [&](auto const &node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, ExprIdent>) {
          indent(os, ind);
          os << "Ident(" << node.name << ")\n";
        } else if constexpr (std::is_same_v<T, ExprInt>) {
          indent(os, ind);
          os << "Int(" << node.value << ")\n";
        } else if constexpr (std::is_same_v<T, ExprStr>) {
          indent(os, ind);
          os << "Str(\"" << node.value << "\")\n";
        } else if constexpr (std::is_same_v<T, ExprUnary>) {
          indent(os, ind);
          os << "Unary(" << opName(node.op) << ")\n";
          dumpExpr(*node.rhs, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          indent(os, ind);
          os << "Binary(" << opName(node.op) << ")\n";
          dumpExpr(*node.lhs, os, ind + 2);
          dumpExpr(*node.rhs, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
          indent(os, ind);
          os << "Assign(" << opName(node.op) << ")\n";
          dumpExpr(*node.lhs, os, ind + 2);
          dumpExpr(*node.rhs, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          indent(os, ind);
          os << "Cond\n";
          dumpExpr(*node.cond, os, ind + 2);
          dumpExpr(*node.thenE, os, ind + 2);
          dumpExpr(*node.elseE, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          indent(os, ind);
          os << "Call(" << node.callee << ")\n";
          for (auto &a : node.args)
            dumpExpr(*a, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          indent(os, ind);
          os << "Index\n";
          dumpExpr(*node.base, os, ind + 2);
          dumpExpr(*node.index, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprMember>) {
          indent(os, ind);
          os << (node.ptr ? "MemberPtr(" : "Member(") << node.member << ")\n";
          dumpExpr(*node.base, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          indent(os, ind);
          os << "Comptime\n";
          dumpExpr(*node.expr, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          indent(os, ind);
          os << "AddressOf(&)\n";
          dumpExpr(*node.operand, os, ind + 2);
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          indent(os, ind);
          os << "Deref(*)\n";
          dumpExpr(*node.operand, os, ind + 2);
        }
      },
      e.v);
}

static void dumpStmt(const Stmt &s, std::ostream &os, int ind) {
  std::visit(
      [&](auto const &node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          indent(os, ind);
          os << "ExprStmt\n";
          dumpExpr(*node.expr, os, ind + 2);
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          indent(os, ind);
          os << "Return\n";
          if (node.expr)
            dumpExpr(**node.expr, os, ind + 2);
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          indent(os, ind);
          os << "DeclStmt ";
          if (node.isConst)
            os << "(const) ";
          dumpType(node.type, os);
          os << "\n";
          for (auto &de : node.declarators) {
            indent(os, ind + 2);
            os << de.name << "\n";
          }
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          dumpStmtBlock(node, os, ind);
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          indent(os, ind);
          os << "For\n";
          if (node.init)
            dumpStmt(*node.init->get(), os, ind + 2);
          if (node.cond)
            dumpExpr(**node.cond, os, ind + 2);
          if (node.iter)
            dumpExpr(**node.iter, os, ind + 2);
          dumpStmt(*node.body, os, ind + 2);
        } else if constexpr (std::is_same_v<T, StmtIf>) {
          indent(os, ind);
          os << "If\n";
          dumpExpr(*node.cond, os, ind + 2);
          indent(os, ind + 2);
          os << "Then:\n";
          dumpStmt(*node.thenBranch, os, ind + 4);
          if (node.elseBranch) {
            indent(os, ind + 2);
            os << "Else:\n";
            dumpStmt(**node.elseBranch, os, ind + 4);
          }
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          indent(os, ind);
          os << "While\n";
          dumpExpr(*node.cond, os, ind + 2);
          dumpStmt(*node.body, os, ind + 2);
        } else if constexpr (std::is_same_v<T, StmtBreak>) {
          indent(os, ind);
          os << "Break\n";
        } else if constexpr (std::is_same_v<T, StmtContinue>) {
          indent(os, ind);
          os << "Continue\n";
        }
      },
      s.v);
}

static void dumpStmtBlock(const StmtBlock &b, std::ostream &os, int ind) {
  indent(os, ind);
  os << "Block\n";
  for (auto &st : b.stmts)
    dumpStmt(*st, os, ind + 2);
}

void dump(const Program &prog, std::ostream &os) {
  os << "Program\n";
  for (auto const &dptr : prog.decls) {
    const Decl &d = *dptr;
    std::visit(
        [&](auto const &node) {
          using T = std::decay_t<decltype(node)>;
          if constexpr (std::is_same_v<T, DeclImport>) {
            os << "  Import ";
            for (size_t i = 0; i < node.path.size(); ++i) {
              if (i)
                os << '.';
              os << node.path[i];
            }
            os << "\n";
          } else if constexpr (std::is_same_v<T, DeclStruct>) {
            os << "  Struct " << node.name << "\n";
            for (auto &f : node.fields) {
              os << "    Field ";
              dumpType(f.type, os);
              os << ":";
              for (size_t i = 0; i < f.names.size(); ++i) {
                os << (i ? "," : " ") << f.names[i];
              }
              os << "\n";
            }
          } else if constexpr (std::is_same_v<T, DeclEnum>) {
            os << "  Enum " << node.name << "\n";
            for (auto &it : node.items) {
              os << "    " << it.name;
              if (it.value)
                os << " = " << *it.value;
              os << "\n";
            }
          } else if constexpr (std::is_same_v<T, DeclVar>) {
            os << "  Var ";
            if (node.isConst)
              os << "(const) ";
            dumpType(node.type, os);
            os << ":";
            for (size_t i = 0; i < node.declarators.size(); ++i) {
              os << (i ? "," : " ") << node.declarators[i].name;
            }
            os << "\n";
          } else if constexpr (std::is_same_v<T, DeclFunc>) {
            os << "  Func " << node.name << " : ";
            dumpType(node.ret, os);
            os << "\n";
            os << "    Params";
            if (node.params.empty())
              os << " (none)\n";
            else
              os << "\n";
            for (auto &p : node.params) {
              os << "      ";
              dumpType(p.type, os);
              os << " " << p.name << "\n";
            }
            if (node.body) {
              dumpStmtBlock(*node.body, os, 4);
            }
          }
        },
        d.v);
  }
}

} // namespace agc
