#pragma once
#include "agc/ast.hpp"
#include "agc/token.hpp"
#include <vector>

#include "agc/diagnostics.hpp"

namespace agc {

class ParseError : public std::runtime_error {
public:
  DiagLoc loc;
  ParseError(const DiagLoc &l, const std::string &msg)
      : std::runtime_error(msg), loc(l) {}
};

class Parser {
public:
  explicit Parser(const std::vector<Token> &tokens) : toks(tokens) {}

  Program parseProgram();

private:
  const Token &peek(size_t off = 0) const {
    size_t i = pos + off;
    return i < toks.size() ? toks[i] : toks.back();
  }
  bool is(TokenKind k, size_t off = 0) const { return peek(off).kind == k; }
  bool match(TokenKind k) {
    if (is(k)) {
      ++pos;
      return true;
    }
    return false;
  }
  const Token &expect(TokenKind k, const char *msg);

  // Declarations
  DeclPtr parseExternal();
  DeclPtr parseImport();
  DeclPtr parseLink();
  DeclPtr parseStruct();
  DeclPtr parseEnum();
  DeclPtr parseDeclOrFunc(TypeName ty, DiagLoc loc, bool isExtern,
                          bool isStatic);

  // Types and declarators (simplified)
  TypeName parseType();
  void parseDeclaratorTail(TypeName &t, std::string &name);
  StmtPtr parseStmt();
  StmtPtr parseBlock();
  StmtPtr parseReturn();
  StmtPtr parseFor();
  StmtPtr parseIf();
  StmtPtr parseWhile();
  StmtPtr parseSwitch();
  StmtPtr parseAsm();
  StmtPtr parseDeclStmt(TypeName t, DiagLoc loc, bool isConst = false);

  // Expressions (Pratt parser)
  ExprPtr parseExpr();
  ExprPtr parseBinary(int minPrec);
  ExprPtr parsePrimary();

  int precedence(TokenKind op) const;

  const std::vector<Token> &toks;
  size_t pos{0};
};

} // namespace agc
