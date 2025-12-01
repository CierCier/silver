#include "agc/codegen.hpp"
#include "agc/overloaded.hpp"
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

namespace agc {

static void indent(std::ostream &os, int n) {
  for (int i = 0; i < n; ++i)
    os << ' ';
}

static std::string emitType(const TypeName &t) {
  std::string base = t.name;
  if (base == "i8")
    base = "sbyte";
  else if (base == "u8")
    base = "byte";
  else if (base == "i16")
    base = "short";
  else if (base == "u16")
    base = "ushort";
  else if (base == "i32")
    base = "int";
  else if (base == "u32")
    base = "uint";
  else if (base == "i64")
    base = "long";
  else if (base == "u64")
    base = "ulong";
  else if (base == "f32")
    base = "float";
  else if (base == "f64")
    base = "double";
  else if (base == "str")
    base = "string";
  else if (base == "bool")
    base = "bool";
  else if (base == "void")
    base = "void";

  std::ostringstream os;
  os << base;
  for (unsigned i = 0; i < t.pointerDepth; ++i)
    os << "*";
  for (auto &d : t.arrayDims) {
    os << "[";
    if (d)
      os << *d;
    os << "]";
  }
  return os.str();
}

static void emitExpr(const Expr &e, std::ostream &os, int prec = 0);
static void emitBlock(const StmtBlock &blk, std::ostream &os, int ind);

static int binPrec(TokenKind k) {
  switch (k) {
  case TokenKind::OrOr:
    return 1;
  case TokenKind::AndAnd:
    return 2;
  case TokenKind::Pipe:
    return 3;
  case TokenKind::Caret:
    return 4;
  case TokenKind::Amp:
    return 5;
  case TokenKind::Eq:
  case TokenKind::Ne:
    return 6;
  case TokenKind::Lt:
  case TokenKind::Le:
  case TokenKind::Gt:
  case TokenKind::Ge:
    return 7;
  case TokenKind::Shl:
  case TokenKind::Shr:
    return 8;
  case TokenKind::Plus:
  case TokenKind::Minus:
    return 9;
  case TokenKind::Star:
  case TokenKind::Slash:
  case TokenKind::Percent:
    return 10;
  default:
    return 0;
  }
}

static void emitCallArgs(const std::vector<ExprPtr> &args, std::ostream &os) {
  for (size_t i = 0; i < args.size(); ++i) {
    if (i)
      os << ", ";
    emitExpr(*args[i], os);
  }
}

static const char *binOpStr(TokenKind op) {
  switch (op) {
  case TokenKind::Plus:
    return " + ";
  case TokenKind::Minus:
    return " - ";
  case TokenKind::Star:
    return " * ";
  case TokenKind::Slash:
    return " / ";
  case TokenKind::Percent:
    return " % ";
  case TokenKind::AndAnd:
    return " && ";
  case TokenKind::OrOr:
    return " || ";
  case TokenKind::Eq:
    return " == ";
  case TokenKind::Ne:
    return " != ";
  case TokenKind::Lt:
    return " < ";
  case TokenKind::Le:
    return " <= ";
  case TokenKind::Gt:
    return " > ";
  case TokenKind::Ge:
    return " >= ";
  case TokenKind::Amp:
    return " & ";
  case TokenKind::Pipe:
    return " | ";
  case TokenKind::Caret:
    return " ^ ";
  case TokenKind::Shl:
    return " << ";
  case TokenKind::Shr:
    return " >> ";
  default:
    return " /*op*/ ";
  }
}

static const char *assignOpStr(TokenKind op) {
  switch (op) {
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
  case TokenKind::ShlAssign:
    return "<<=";
  case TokenKind::ShrAssign:
    return ">>=";
  default:
    return "/*=*/";
  }
}

static void emitExpr(const Expr &e, std::ostream &os, int prec) {
  std::visit(
      overloaded{
          [&](const ExprIdent &node) { os << node.name; },
          [&](const ExprInt &node) { os << node.value; },
          [&](const ExprFloat &node) { os << node.value; },
          [&](const ExprStr &node) {
            os << '"';
            for (char c : node.value) {
              if (c == '\\' || c == '"')
                os << '\\' << c;
              else if (c == '\n')
                os << "\\n";
              else
                os << c;
            }
            os << '"';
          },
          [&](const ExprUnary &node) {
            switch (node.op) {
            case TokenKind::Minus:
              os << '-';
              break;
            case TokenKind::Bang:
              os << '!';
              break;
            case TokenKind::Tilde:
              os << '~';
              break;
            case TokenKind::Plus:
              os << '+';
              break;
            case TokenKind::PlusPlus:
              os << "++";
              break;
            case TokenKind::MinusMinus:
              os << "--";
              break;
            default:
              os << "/*unop*/";
              break;
            }
            emitExpr(*node.rhs, os, 12);
          },
          [&](const ExprBinary &node) {
            int p = binPrec(node.op);
            if (p < prec)
              os << '(';
            emitExpr(*node.lhs, os, p);
            os << binOpStr(node.op);
            emitExpr(*node.rhs, os, p + 1);
            if (p < prec)
              os << ')';
          },
          [&](const ExprAssign &node) {
            emitExpr(*node.lhs, os);
            os << ' ' << assignOpStr(node.op) << ' ';
            emitExpr(*node.rhs, os);
          },
          [&](const ExprCond &node) {
            emitExpr(*node.cond, os);
            os << " ? ";
            emitExpr(*node.thenE, os);
            os << " : ";
            emitExpr(*node.elseE, os);
          },
          [&](const ExprCall &node) {
            if (node.callee == "println") {
              os << "Console.WriteLine(";
              emitCallArgs(node.args, os);
              os << ")";
            } else {
              os << node.callee << '(';
              emitCallArgs(node.args, os);
              os << ')';
            }
          },
          [&](const ExprIndex &node) {
            emitExpr(*node.base, os);
            os << '[';
            emitExpr(*node.index, os);
            os << ']';
          },
          [&](const ExprMember &node) {
            emitExpr(*node.base, os);
            os << '.' << node.member;
          },
          [&](const ExprComptime &node) {
            // C# doesn't have comptime - just emit the inner expression
            emitExpr(*node.expr, os);
          },
          [&](const ExprAddressOf &node) {
            os << "&";
            emitExpr(*node.operand, os);
          },
          [&](const ExprDeref &node) {
            os << "*";
            emitExpr(*node.operand, os);
          },
          [&](const ExprCast &node) {
            os << "((" << emitType(node.target) << ")";
            emitExpr(*node.expr, os);
            os << ")";
          },
          [&](const ExprInitList &node) {
            os << "{ ";
            for (size_t i = 0; i < node.values.size(); ++i) {
              if (i)
                os << ", ";
              emitExpr(*node.values[i].value, os);
            }
            os << " }";
          },
      },
      e.v);
}

static void emitStmt(const Stmt &s, std::ostream &os, int ind);

static void emitForInit(const Stmt &s, std::ostream &os) {
  std::visit(
      overloaded{
          [&](const StmtDecl &ds) {
            os << emitType(ds.type) << " ";
            for (size_t i = 0; i < ds.declarators.size(); ++i) {
              if (i)
                os << ", ";
              os << ds.declarators[i].name;
              if (ds.declarators[i].init && *ds.declarators[i].init) {
                os << " = ";
                emitExpr(**ds.declarators[i].init, os);
              }
            }
          },
          [&](const StmtExpr &es) { emitExpr(*es.expr, os); },
          [&](const auto &) { /* other statement types not expected */ },
      },
      s.v);
}

static void emitStmt(const Stmt &s, std::ostream &os, int ind) {
  std::visit(
      overloaded{
          [&](const StmtExpr &node) {
            indent(os, ind);
            emitExpr(*node.expr, os);
            os << ";\n";
          },
          [&](const StmtReturn &node) {
            indent(os, ind);
            os << "return";
            if (node.expr) {
              os << ' ';
              emitExpr(**node.expr, os);
            }
            os << ";\n";
          },
          [&](const StmtDecl &node) {
            indent(os, ind);
            os << emitType(node.type) << " ";
            for (size_t i = 0; i < node.declarators.size(); ++i) {
              if (i)
                os << ", ";
              os << node.declarators[i].name;
              if (node.declarators[i].init && *node.declarators[i].init) {
                os << " = ";
                emitExpr(**node.declarators[i].init, os);
              }
            }
            os << ";\n";
          },
          [&](const StmtBlock &node) { emitBlock(node, os, ind); },
          [&](const StmtFor &node) {
            indent(os, ind);
            os << "for (";
            if (node.init) {
              emitForInit(**node.init, os);
            }
            os << "; ";
            if (node.cond)
              emitExpr(**node.cond, os);
            os << "; ";
            if (node.iter)
              emitExpr(**node.iter, os);
            os << ") ";
            emitStmt(*node.body, os, 0);
          },
          [&](const StmtIf &node) {
            indent(os, ind);
            os << "if (";
            emitExpr(*node.cond, os);
            os << ") ";
            emitStmt(*node.thenBranch, os, 0);
            if (node.elseBranch) {
              indent(os, ind);
              os << "else ";
              emitStmt(**node.elseBranch, os, 0);
            }
          },
          [&](const StmtWhile &node) {
            indent(os, ind);
            os << "while (";
            emitExpr(*node.cond, os);
            os << ") ";
            emitStmt(*node.body, os, 0);
          },
          [&](const StmtBreak &) {
            indent(os, ind);
            os << "break;\n";
          },
          [&](const StmtContinue &) {
            indent(os, ind);
            os << "continue;\n";
          },
          [&](const StmtAsm &) {
            indent(os, ind);
            os << "// inline assembly not supported in C#\n";
          },
          [&](const StmtSwitch &node) {
            indent(os, ind);
            os << "switch (";
            emitExpr(*node.cond, os);
            os << ") {\n";
            for (auto &c : node.cases) {
              for (auto &v : c.values) {
                indent(os, ind);
                os << "case ";
                emitExpr(*v, os);
                os << ":\n";
              }
              emitStmt(*c.body, os, ind + 4);
              indent(os, ind + 4);
              os << "break;\n";
            }
            if (node.defaultCase) {
              indent(os, ind);
              os << "default:\n";
              emitStmt(**node.defaultCase, os, ind + 4);
              indent(os, ind + 4);
              os << "break;\n";
            }
            indent(os, ind);
            os << "}\n";
          },
      },
      s.v);
}

static void emitBlock(const StmtBlock &blk, std::ostream &os, int ind) {
  indent(os, ind);
  os << "{\n";
  for (auto &st : blk.stmts)
    emitStmt(*st, os, ind + 4);
  indent(os, ind);
  os << "}\n";
}

class CsBackend : public CodegenBackend {
public:
  std::string_view name() const override { return "csharp"; }
  bool generate(const Program &prog, std::ostream &os, std::string &err,
                const CodegenOptions &) override {
    (void)err;
    os << "using System;\n\n";
    os << "public static class Program {\n";

    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      std::visit(
          overloaded{
              [&](const DeclImport &node) {
                os << "    // import ";
                for (size_t i = 0; i < node.path.size(); ++i) {
                  if (i)
                    os << '.';
                  os << node.path[i];
                }
                os << "\n";
              },
              [&](const DeclLink &node) {
                os << "    // link " << node.lib << "\n";
              },
              [&](const DeclStruct &node) {
                os << "    public struct " << node.name << " {\n";
                for (auto &f : node.fields) {
                  os << "        public " << emitType(f.type) << " ";
                  for (size_t i = 0; i < f.names.size(); ++i) {
                    if (i)
                      os << ", ";
                    os << f.names[i];
                  }
                  os << ";\n";
                }
                os << "    }\n";
              },
              [&](const DeclEnum &node) {
                os << "    public enum " << node.name << " {\n";
                for (auto &item : node.items) {
                  os << "        " << item.name;
                  if (item.value)
                    os << " = " << *item.value;
                  os << ",\n";
                }
                os << "    }\n";
              },
              [&](const DeclVar &node) {
                os << "    public static " << emitType(node.type) << " ";
                for (size_t i = 0; i < node.declarators.size(); ++i) {
                  if (i)
                    os << ", ";
                  os << node.declarators[i].name;
                  if (node.declarators[i].init && *node.declarators[i].init) {
                    os << " = ";
                    emitExpr(**node.declarators[i].init, os);
                  }
                }
                os << ";\n";
              },
              [&](const DeclFunc &node) {
                if (node.isExtern) {
                  os << "    "
                        "[System.Runtime.InteropServices.DllImport(\"__"
                        "Internal\")]\n";
                  os << "    public static extern " << emitType(node.ret) << " "
                     << node.name << "(";
                } else {
                  os << "    public static " << emitType(node.ret) << " "
                     << node.name << "(";
                }
                for (size_t i = 0; i < node.params.size(); ++i) {
                  if (i)
                    os << ", ";
                  os << emitType(node.params[i].type) << " "
                     << node.params[i].name;
                }
                os << ")";
                if (node.body) {
                  os << "\n";
                  emitBlock(*node.body, os, 4);
                } else {
                  os << ";\n";
                }
                os << "\n";
              },
              [&](const DeclImpl &) {
                // impl blocks handled differently in C#
              },
              [&](const DeclCast &) {
                // cast operators handled differently in C#
              },
          },
          d.v);
    }

    os << "}\n";
    return true;
  }
};

std::unique_ptr<CodegenBackend> create_backend_csharp() {
  return std::make_unique<CsBackend>();
}

} // namespace agc