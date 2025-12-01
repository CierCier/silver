#include "agc/backends/codegen_js.hpp"
#include "agc/overloaded.hpp"
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
    return " === ";
  case TokenKind::Ne:
    return " !== ";
  case TokenKind::Lt:
    return " < ";
  case TokenKind::Le:
    return " <= ";
  case TokenKind::Gt:
    return " > ";
  case TokenKind::Ge:
    return " >= ";
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
  default:
    return "/*=*/";
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
  std::visit(overloaded{
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
                     emitExpr(*node.rhs, os, 7);
                     break;
                   case TokenKind::Bang:
                     os << '!';
                     emitExpr(*node.rhs, os, 7);
                     break;
                   case TokenKind::PlusPlus:
                     os << "(++";
                     emitExpr(*node.rhs, os);
                     os << ')';
                     break;
                   case TokenKind::MinusMinus:
                     os << "(--";
                     emitExpr(*node.rhs, os);
                     os << ')';
                     break;
                   default:
                     os << "/*unop*/";
                     break;
                   }
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
                     os << "console.log(";
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
                   // JS doesn't have comptime - just emit the inner expression
                   emitExpr(*node.expr, os);
                 },
                 [&](const ExprAddressOf &node) {
                   // JS doesn't support address-of - emit as comment
                   os << "/* &";
                   emitExpr(*node.operand, os);
                   os << " */";
                 },
                 [&](const ExprDeref &node) {
                   // JS doesn't support dereference - emit as comment
                   os << "/* *";
                   emitExpr(*node.operand, os);
                   os << " */";
                 },
                 [&](const ExprCast &node) {
                   // JS doesn't have type casts - just emit the expression
                   emitExpr(*node.expr, os);
                 },
                 [&](const ExprInitList &node) {
                   // Emit as object/array literal
                   os << '{';
                   for (size_t i = 0; i < node.values.size(); ++i) {
                     if (i)
                       os << ", ";
                     emitExpr(*node.values[i].value, os);
                   }
                   os << '}';
                 },
             },
             e.v);
}

static void emitStmt(const Stmt &s, std::ostream &os, int ind);

static void emitForInit(const Stmt &s, std::ostream &os) {
  std::visit(overloaded{
                 [&](const StmtDecl &ds) {
                   os << "let /*" << emitType(ds.type) << "*/ ";
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
  std::visit(overloaded{
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
                   os << "let /*" << emitType(node.type) << "*/ ";
                   for (size_t i = 0; i < node.declarators.size(); ++i) {
                     if (i)
                       os << ", ";
                     os << node.declarators[i].name;
                     if (node.declarators[i].init &&
                         *node.declarators[i].init) {
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
                   os << "// inline assembly not supported in JS\n";
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
                     emitStmt(*c.body, os, ind + 2);
                     indent(os, ind + 2);
                     os << "break;\n";
                   }
                   if (node.defaultCase) {
                     indent(os, ind);
                     os << "default:\n";
                     emitStmt(**node.defaultCase, os, ind + 2);
                     indent(os, ind + 2);
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
    emitStmt(*st, os, ind + 2);
  indent(os, ind);
  os << "}\n";
}

class JsBackend : public CodegenBackend {
public:
  std::string_view name() const override { return "js"; }
  bool generate(const Program &prog, std::ostream &os, std::string &err,
                const CodegenOptions &) override {
    (void)err;
    os << "// Transpiled by AGc (this is not reliable)\n\n";

    for (auto const &dptr : prog.decls) {
      const Decl &d = *dptr;
      std::visit(
          overloaded{
              [&](const DeclImport &node) {
                os << "// import ";
                for (size_t i = 0; i < node.path.size(); ++i) {
                  if (i)
                    os << '.';
                  os << node.path[i];
                }
                os << "\n";
              },
              [&](const DeclLink &node) {
                os << "// link " << node.lib << "\n";
              },
              [&](const DeclStruct &node) {
                os << "// struct " << node.name << "\n";
              },
              [&](const DeclEnum &node) {
                os << "const " << node.name << " = Object.freeze({\n";
                int64_t nextVal = 0;
                for (auto &item : node.items) {
                  int64_t val =
                      item.value ? static_cast<int64_t>(*item.value) : nextVal;
                  os << "  " << item.name << ": " << val << ",\n";
                  nextVal = val + 1;
                }
                os << "});\n\n";
              },
              [&](const DeclVar &node) {
                os << "let /*" << emitType(node.type) << "*/ ";
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
              },
              [&](const DeclImpl &) {
                // impl blocks handled inline in JS
              },
              [&](const DeclCast &) {
                // cast operators not applicable in JS
              },
              [&](const DeclTrait &) {
                // trait definitions are compile-time only
              },
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