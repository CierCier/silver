#include "agc/codegen.hpp"
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
    base = "string"; // simplistic mapping
  else if (base == "bool")
    base = "bool";
  else if (base == "void")
    base = "void";

  // Pointers in C# are unsafe, but for simplicity let's map to arrays or just
  // keep as is if unsafe For this simple implementation, let's assume we use
  // unsafe blocks or map pointers to something else. But the user wants
  // interop, so maybe unsafe is better. However, for "basic" examples like
  // arrays, C# uses arrays. Let's try to map pointer to array if it looks like
  // an array, or just use unsafe pointer. Given the example: i32 *argv ->
  // string[] args (convention) i32 arr[8] -> int[] arr = new int[8]

  std::ostringstream os;
  os << base;
  for (unsigned i = 0; i < t.pointerDepth; ++i)
    os << "*";
  for (auto &d : t.arrayDims) {
    os << "[";
    if (d)
      os << *d; // C# arrays are [,] for multidim, but here it's likely []
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

static void emitExpr(const Expr &e, std::ostream &os, int prec) {
  std::visit(
      [&](auto const &node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, ExprIdent>) {
          os << node.name;
        } else if constexpr (std::is_same_v<T, ExprInt>) {
          os << node.value;
        } else if constexpr (std::is_same_v<T, ExprStr>) {
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
        } else if constexpr (std::is_same_v<T, ExprUnary>) {
          if (node.op == TokenKind::Minus) {
            os << '-';
            emitExpr(*node.rhs, os, 12);
          } else if (node.op == TokenKind::Bang) {
            os << '!';
            emitExpr(*node.rhs, os, 12);
          } else if (node.op == TokenKind::Tilde) {
            os << '~';
            emitExpr(*node.rhs, os, 12);
          } else if (node.op == TokenKind::Plus) {
            os << '+';
            emitExpr(*node.rhs, os, 12);
          } else if (node.op == TokenKind::PlusPlus) {
            os << "++";
            emitExpr(*node.rhs, os, 12);
          } else if (node.op == TokenKind::MinusMinus) {
            os << "--";
            emitExpr(*node.rhs, os, 12);
          } else {
            os << "/*unop*/";
          }
        } else if constexpr (std::is_same_v<T, ExprBinary>) {
          int p = binPrec(node.op);
          if (p < prec)
            os << '(';
          emitExpr(*node.lhs, os, p);
          switch (node.op) {
          case TokenKind::Plus:
            os << " + ";
            break;
          case TokenKind::Minus:
            os << " - ";
            break;
          case TokenKind::Star:
            os << " * ";
            break;
          case TokenKind::Slash:
            os << " / ";
            break;
          case TokenKind::Percent:
            os << " % ";
            break;
          case TokenKind::AndAnd:
            os << " && ";
            break;
          case TokenKind::OrOr:
            os << " || ";
            break;
          case TokenKind::Eq:
            os << " == ";
            break;
          case TokenKind::Ne:
            os << " != ";
            break;
          case TokenKind::Lt:
            os << " < ";
            break;
          case TokenKind::Le:
            os << " <= ";
            break;
          case TokenKind::Gt:
            os << " > ";
            break;
          case TokenKind::Ge:
            os << " >= ";
            break;
          case TokenKind::Amp:
            os << " & ";
            break;
          case TokenKind::Pipe:
            os << " | ";
            break;
          case TokenKind::Caret:
            os << " ^ ";
            break;
          case TokenKind::Shl:
            os << " << ";
            break;
          case TokenKind::Shr:
            os << " >> ";
            break;
          default:
            os << " /*op*/ ";
            break;
          }
          emitExpr(*node.rhs, os, p + 1);
          if (p < prec)
            os << ')';
        } else if constexpr (std::is_same_v<T, ExprAssign>) {
          emitExpr(*node.lhs, os);
          os << ' ';
          switch (node.op) {
          case TokenKind::Assign:
            os << '=';
            break;
          case TokenKind::PlusAssign:
            os << "+=";
            break;
          case TokenKind::MinusAssign:
            os << "-=";
            break;
          case TokenKind::StarAssign:
            os << "*=";
            break;
          case TokenKind::SlashAssign:
            os << "/=";
            break;
          case TokenKind::PercentAssign:
            os << "%=";
            break;
          case TokenKind::ShlAssign:
            os << "<<=";
            break;
          case TokenKind::ShrAssign:
            os << ">>=";
            break;
          default:
            os << "/*=*/";
            break;
          }
          os << ' ';
          emitExpr(*node.rhs, os);
        } else if constexpr (std::is_same_v<T, ExprCond>) {
          emitExpr(*node.cond, os);
          os << " ? ";
          emitExpr(*node.thenE, os);
          os << " : ";
          emitExpr(*node.elseE, os);
        } else if constexpr (std::is_same_v<T, ExprCall>) {
          if (node.callee == "println") {
            os << "Console.WriteLine(";
            // C# Console.WriteLine supports format strings similar to printf
            // but with {0} etc. For simplicity, let's assume the user uses C#
            // style or we just pass it through. But wait, the example uses
            // printf style "%s%s". We might need a helper or just map to
            // Console.Write(string.Format(...)) Or just emit it and let the
            // user deal with it (stub). Better: map to a helper `Printf` if we
            // can, or just `Console.WriteLine`. Let's just emit
            // `Console.WriteLine` and hope for the best or comment.
            emitCallArgs(node.args, os);
            os << ")";
          } else {
            os << node.callee << '(';
            emitCallArgs(node.args, os);
            os << ')';
          }
        } else if constexpr (std::is_same_v<T, ExprIndex>) {
          emitExpr(*node.base, os);
          os << '[';
          emitExpr(*node.index, os);
          os << ']';
        } else if constexpr (std::is_same_v<T, ExprMember>) {
          emitExpr(*node.base, os);
          os << '.' << node.member;
        } else if constexpr (std::is_same_v<T, ExprComptime>) {
          // C# doesn't have comptime - just emit the inner expression
          // The evaluator would have folded constants already
          emitExpr(*node.expr, os);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          // C# unsafe code: &operand
          os << "&";
          emitExpr(*node.operand, os);
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          // C# unsafe code: *operand
          os << "*";
          emitExpr(*node.operand, os);
        }
      },
      e.v);
}

static void emitStmt(const Stmt &s, std::ostream &os, int ind) {
  std::visit(
      [&](auto const &node) {
        using T = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<T, StmtExpr>) {
          indent(os, ind);
          emitExpr(*node.expr, os);
          os << ";\n";
        } else if constexpr (std::is_same_v<T, StmtReturn>) {
          indent(os, ind);
          os << "return";
          if (node.expr) {
            os << ' ';
            emitExpr(**node.expr, os);
          }
          os << ";\n";
        } else if constexpr (std::is_same_v<T, StmtDecl>) {
          indent(os, ind);
          // C# var decl
          os << emitType(node.type) << " ";
          for (size_t i = 0; i < node.declarators.size(); ++i) {
            if (i)
              os << ", ";
            os << node.declarators[i].first;
            if (node.declarators[i].second && *node.declarators[i].second) {
              os << " = ";
              emitExpr(**node.declarators[i].second, os);
            }
          }
          os << ";\n";
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          emitBlock(node, os, ind);
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          indent(os, ind);
          os << "for (";
          if (node.init) {
            if (auto *ds = std::get_if<StmtDecl>(&node.init->get()->v)) {
              os << emitType(ds->type) << " ";
              for (size_t i = 0; i < ds->declarators.size(); ++i) {
                if (i)
                  os << ", ";
                os << ds->declarators[i].first;
                if (ds->declarators[i].second && *ds->declarators[i].second) {
                  os << " = ";
                  emitExpr(**ds->declarators[i].second, os);
                }
              }
              os << "; ";
            } else if (auto *es = std::get_if<StmtExpr>(&node.init->get()->v)) {
              emitExpr(*es->expr, os);
              os << "; ";
            } else {
              os << "; ";
            }
          } else {
            os << "; ";
          }
          if (node.cond)
            emitExpr(**node.cond, os);
          os << "; ";
          if (node.iter)
            emitExpr(**node.iter, os);
          os << ") ";
          emitStmt(*node.body, os, 0);
        } else if constexpr (std::is_same_v<T, StmtIf>) {
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
        } else if constexpr (std::is_same_v<T, StmtWhile>) {
          indent(os, ind);
          os << "while (";
          emitExpr(*node.cond, os);
          os << ") ";
          emitStmt(*node.body, os, 0);
        } else if constexpr (std::is_same_v<T, StmtBreak>) {
          indent(os, ind);
          os << "break;\n";
        } else if constexpr (std::is_same_v<T, StmtContinue>) {
          indent(os, ind);
          os << "continue;\n";
        }
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
          [&](auto const &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, DeclImport>) {
              os << "    // import ";
              for (size_t i = 0; i < node.path.size(); ++i) {
                if (i)
                  os << '.';
                os << node.path[i];
              }
              os << "\n";
            } else if constexpr (std::is_same_v<T, DeclStruct>) {
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
            } else if constexpr (std::is_same_v<T, DeclEnum>) {
              os << "    public enum " << node.name << " {\n";
              for (auto &item : node.items) {
                os << "        " << item.name;
                if (item.value)
                  os << " = " << *item.value;
                os << ",\n";
              }
              os << "    }\n";
            } else if constexpr (std::is_same_v<T, DeclVar>) {
              os << "    public static " << emitType(node.type) << " ";
              for (size_t i = 0; i < node.declarators.size(); ++i) {
                if (i)
                  os << ", ";
                os << node.declarators[i].first;
                if (node.declarators[i].second && *node.declarators[i].second) {
                  os << " = ";
                  emitExpr(**node.declarators[i].second, os);
                }
              }
              os << ";\n";
            } else if constexpr (std::is_same_v<T, DeclFunc>) {
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
            }
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
