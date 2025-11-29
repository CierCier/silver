#include "agc/backends/codegen_js.hpp"
#include <sstream>

namespace agc {

static void indent(std::ostream &os, int n) {
  for (int i = 0; i < n; ++i)
    os << ' ';
}

static std::string emitType(const TypeName &t) {
  // JS is dynamically typed; return a comment string for readability
  std::ostringstream os;
  os << t.name;
  for (unsigned i = 0; i < t.pointerDepth; ++i)
    os << '*';
  for (auto &d : t.arrayDims) {
    os << '[';
    if (d)
      os << *d;
    os << ']';
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
  case TokenKind::Eq:
  case TokenKind::Ne:
    return 3;
  case TokenKind::Lt:
  case TokenKind::Le:
  case TokenKind::Gt:
  case TokenKind::Ge:
    return 4;
  case TokenKind::Plus:
  case TokenKind::Minus:
    return 5;
  case TokenKind::Star:
  case TokenKind::Slash:
  case TokenKind::Percent:
    return 6;
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
            emitExpr(*node.rhs, os, 7);
          } else if (node.op == TokenKind::Bang) {
            os << '!';
            emitExpr(*node.rhs, os, 7);
          } else if (node.op == TokenKind::PlusPlus) {
            os << "(++";
            emitExpr(*node.rhs, os);
            os << ')';
          } else if (node.op == TokenKind::MinusMinus) {
            os << "(--";
            emitExpr(*node.rhs, os);
            os << ')';
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
            os << "console.log(";
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
          // JS doesn't have comptime - just emit the inner expression
          emitExpr(*node.expr, os);
        } else if constexpr (std::is_same_v<T, ExprAddressOf>) {
          // JS doesn't support address-of - emit as comment
          os << "/* &";
          emitExpr(*node.operand, os);
          os << " */";
        } else if constexpr (std::is_same_v<T, ExprDeref>) {
          // JS doesn't support dereference - emit as comment
          os << "/* *";
          emitExpr(*node.operand, os);
          os << " */";
        } else if constexpr (std::is_same_v<T, ExprCast>) {
          // JS doesn't have type casts - just emit the expression
          emitExpr(*node.expr, os);
        } else if constexpr (std::is_same_v<T, ExprInitList>) {
          // Emit as object/array literal
          os << '{';
          for (size_t i = 0; i < node.values.size(); ++i) {
            if (i)
              os << ", ";
            emitExpr(*node.values[i], os);
          }
          os << '}';
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
          os << "let /*" << emitType(node.type) << "*/ ";
          for (size_t i = 0; i < node.declarators.size(); ++i) {
            if (i)
              os << ", ";
            os << node.declarators[i].name;
          }
          os << ";\n"; // initializers omitted for brevity
        } else if constexpr (std::is_same_v<T, StmtBlock>) {
          emitBlock(node, os, ind);
        } else if constexpr (std::is_same_v<T, StmtFor>) {
          indent(os, ind);
          os << "for (";
          if (node.init) {
            // Only handle decl stmt init or expr stmt minimally
            if (auto *ds = std::get_if<StmtDecl>(&node.init->get()->v)) {
              os << "let /*" << emitType(ds->type) << "*/ ";
              for (size_t i = 0; i < ds->declarators.size(); ++i) {
                if (i)
                  os << ", ";
                os << ds->declarators[i].name;
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
    emitStmt(*st, os, ind + 2);
  indent(os, ind);
  os << "}\n";
}

class JsBackend : public CodegenBackend {
public:
  std::string_view name() const override { return "js"; }
  bool generate(const Program &prog, std::ostream &os, std::string &err,
                const CodegenOptions &) override {
    os << "// Transpiled by AGc (this is not reliable)\n\n";
    // Emit functions and top-level vars; ignore struct/enum for now (emit as
    // comments)
    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      std::visit(
          [&](auto const &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, DeclImport>) {
              os << "// import ";
              for (size_t i = 0; i < node.path.size(); ++i) {
                if (i)
                  os << '.';
                os << node.path[i];
              }
              os << "\n";
            } else if constexpr (std::is_same_v<T, DeclStruct>) {
              os << "// struct " << node.name << "\n";
            } else if constexpr (std::is_same_v<T, DeclEnum>) {
              os << "// enum " << node.name << "\n";
            } else if constexpr (std::is_same_v<T, DeclVar>) {
              os << "let /*" << emitType(node.type) << "*/ ";
              for (size_t i = 0; i < node.declarators.size(); ++i) {
                if (i)
                  os << ", ";
                os << node.declarators[i].name;
              }
              os << ";\n";
            } else if constexpr (std::is_same_v<T, DeclFunc>) {
              os << "function " << node.name << "(";
              for (size_t i = 0; i < node.params.size(); ++i) {
                if (i)
                  os << ", ";
                os << node.params[i].name;
              }
              os << ") ";
              if (node.body) {
                emitBlock(*node.body, os, 0);
              } else {
                os << "{}\n";
              }
              os << "\n";
            }
          },
          d.v);
    }
    return true;
  }
};

std::unique_ptr<CodegenBackend> create_backend_js() {
  return std::make_unique<JsBackend>();
}

} // namespace agc
