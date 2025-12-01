#include "agc/ast_dump.hpp"
#include "agc/overloaded.hpp"
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
      overloaded{
          [&](const ExprIdent &node) {
            indent(os, ind);
            os << "Ident(" << node.name << ")\n";
          },
          [&](const ExprInt &node) {
            indent(os, ind);
            os << "Int(" << node.value << ")\n";
          },
          [&](const ExprFloat &node) {
            indent(os, ind);
            os << "Float(" << node.value << ")\n";
          },
          [&](const ExprStr &node) {
            indent(os, ind);
            os << "Str(\"" << node.value << "\")\n";
          },
          [&](const ExprUnary &node) {
            indent(os, ind);
            os << "Unary(" << opName(node.op) << ")\n";
            dumpExpr(*node.rhs, os, ind + 2);
          },
          [&](const ExprBinary &node) {
            indent(os, ind);
            os << "Binary(" << opName(node.op) << ")\n";
            dumpExpr(*node.lhs, os, ind + 2);
            dumpExpr(*node.rhs, os, ind + 2);
          },
          [&](const ExprAssign &node) {
            indent(os, ind);
            os << "Assign(" << opName(node.op) << ")\n";
            dumpExpr(*node.lhs, os, ind + 2);
            dumpExpr(*node.rhs, os, ind + 2);
          },
          [&](const ExprCond &node) {
            indent(os, ind);
            os << "Cond\n";
            dumpExpr(*node.cond, os, ind + 2);
            dumpExpr(*node.thenE, os, ind + 2);
            dumpExpr(*node.elseE, os, ind + 2);
          },
          [&](const ExprCall &node) {
            indent(os, ind);
            os << "Call(" << node.callee << ")\n";
            for (auto &a : node.args)
              dumpExpr(*a, os, ind + 2);
          },
          [&](const ExprIndex &node) {
            indent(os, ind);
            os << "Index\n";
            dumpExpr(*node.base, os, ind + 2);
            dumpExpr(*node.index, os, ind + 2);
          },
          [&](const ExprMember &node) {
            indent(os, ind);
            os << (node.ptr ? "MemberPtr(" : "Member(") << node.member << ")\n";
            dumpExpr(*node.base, os, ind + 2);
          },
          [&](const ExprComptime &node) {
            indent(os, ind);
            os << "Comptime\n";
            dumpExpr(*node.expr, os, ind + 2);
          },
          [&](const ExprAddressOf &node) {
            indent(os, ind);
            os << "AddressOf(&)\n";
            dumpExpr(*node.operand, os, ind + 2);
          },
          [&](const ExprDeref &node) {
            indent(os, ind);
            os << "Deref(*)\n";
            dumpExpr(*node.operand, os, ind + 2);
          },
          [&](const ExprCast &node) {
            indent(os, ind);
            os << "Cast(";
            dumpType(node.target, os);
            os << ")\n";
            dumpExpr(*node.expr, os, ind + 2);
          },
          [&](const ExprInitList &node) {
            indent(os, ind);
            os << "InitList\n";
            for (auto &v : node.values) {
              if (v.designator) {
                indent(os, ind + 2);
                os << "Designator:\n";
                dumpExpr(**v.designator, os, ind + 4);
              }
              dumpExpr(*v.value, os, ind + 2);
            }
          },
      },
      e.v);
}

static void dumpStmt(const Stmt &s, std::ostream &os, int ind) {
  std::visit(
      overloaded{
          [&](const StmtExpr &node) {
            indent(os, ind);
            os << "ExprStmt\n";
            dumpExpr(*node.expr, os, ind + 2);
          },
          [&](const StmtReturn &node) {
            indent(os, ind);
            os << "Return\n";
            if (node.expr)
              dumpExpr(**node.expr, os, ind + 2);
          },
          [&](const StmtDecl &node) {
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
          },
          [&](const StmtBlock &node) { dumpStmtBlock(node, os, ind); },
          [&](const StmtFor &node) {
            indent(os, ind);
            os << "For\n";
            if (node.init)
              dumpStmt(*node.init->get(), os, ind + 2);
            if (node.cond)
              dumpExpr(**node.cond, os, ind + 2);
            if (node.iter)
              dumpExpr(**node.iter, os, ind + 2);
            dumpStmt(*node.body, os, ind + 2);
          },
          [&](const StmtIf &node) {
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
          },
          [&](const StmtWhile &node) {
            indent(os, ind);
            os << "While\n";
            dumpExpr(*node.cond, os, ind + 2);
            dumpStmt(*node.body, os, ind + 2);
          },
          [&](const StmtBreak &) {
            indent(os, ind);
            os << "Break\n";
          },
          [&](const StmtContinue &) {
            indent(os, ind);
            os << "Continue\n";
          },
          [&](const StmtAsm &node) {
            indent(os, ind);
            os << "Asm";
            if (node.isVolatile)
              os << " (volatile)";
            os << "\n";
          },
          [&](const StmtSwitch &node) {
            indent(os, ind);
            os << "Switch\n";
            dumpExpr(*node.cond, os, ind + 2);
            for (auto &c : node.cases) {
              indent(os, ind + 2);
              os << "Case:\n";
              for (auto &v : c.values)
                dumpExpr(*v, os, ind + 4);
              dumpStmt(*c.body, os, ind + 4);
            }
            if (node.defaultCase) {
              indent(os, ind + 2);
              os << "Default:\n";
              dumpStmt(**node.defaultCase, os, ind + 4);
            }
          },
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
        overloaded{
            [&](const DeclImport &node) {
              os << "  Import ";
              for (size_t i = 0; i < node.path.size(); ++i) {
                if (i)
                  os << '.';
                os << node.path[i];
              }
              os << "\n";
            },
            [&](const DeclLink &node) {
              os << "  Link " << node.lib << "\n";
            },
            [&](const DeclStruct &node) {
              os << "  Struct " << node.name;
              if (!node.genericParams.empty()) {
                os << "<";
                for (size_t i = 0; i < node.genericParams.size(); ++i) {
                  if (i)
                    os << ", ";
                  os << node.genericParams[i];
                }
                os << ">";
              }
              os << "\n";
              for (auto &f : node.fields) {
                os << "    Field ";
                dumpType(f.type, os);
                os << ":";
                for (size_t i = 0; i < f.names.size(); ++i) {
                  os << (i ? "," : " ") << f.names[i];
                }
                os << "\n";
              }
            },
            [&](const DeclEnum &node) {
              os << "  Enum " << node.name << "\n";
              for (auto &it : node.items) {
                os << "    " << it.name;
                if (it.value)
                  os << " = " << *it.value;
                os << "\n";
              }
            },
            [&](const DeclVar &node) {
              os << "  Var ";
              if (node.isExtern)
                os << "(extern) ";
              if (node.isStatic)
                os << "(static) ";
              if (node.isConst)
                os << "(const) ";
              dumpType(node.type, os);
              os << ":";
              for (size_t i = 0; i < node.declarators.size(); ++i) {
                os << (i ? "," : " ") << node.declarators[i].name;
              }
              os << "\n";
            },
            [&](const DeclFunc &node) {
              os << "  Func " << node.name;
              if (!node.genericParams.empty()) {
                os << "<";
                for (size_t i = 0; i < node.genericParams.size(); ++i) {
                  if (i)
                    os << ", ";
                  os << node.genericParams[i];
                }
                os << ">";
              }
              os << " : ";
              dumpType(node.ret, os);
              if (node.isExtern)
                os << " (extern)";
              if (node.isVariadic)
                os << " (variadic)";
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
            },
            [&](const DeclImpl &node) {
              os << "  Impl ";
              dumpType(node.type, os);
              os << "\n";
              for (auto &m : node.methods) {
                std::visit(
                    overloaded{
                        [&](const DeclFunc &fn) {
                          os << "    Method " << fn.name << " : ";
                          dumpType(fn.ret, os);
                          os << "\n";
                        },
                        [&](const DeclCast &c) {
                          os << "    Cast -> ";
                          dumpType(c.target, os);
                          if (c.isImplicit)
                            os << " (implicit)";
                          os << "\n";
                        },
                        [&](const auto &) { os << "    <unknown method>\n"; },
                    },
                    m->v);
              }
            },
            [&](const DeclCast &node) {
              os << "  Cast -> ";
              dumpType(node.target, os);
              if (node.isImplicit)
                os << " (implicit)";
              os << "\n";
            },
        },
        d.v);
  }
}

} // namespace agc