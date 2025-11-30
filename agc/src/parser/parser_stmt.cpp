#include "agc/parser.hpp"

namespace agc {

static ParseError parse_error(const Token &t, const char *msg) {
  return ParseError(t.loc, msg);
}

bool Parser::isGenericInstantiation() {
  int depth = 0;
  size_t i = 1; // start at < (offset 1 from current Identifier)
  if (!is(TokenKind::Lt, 1))
    return false;

  while (true) {
    const Token &t = peek(i);
    if (t.kind == TokenKind::End)
      return false;
    if (t.kind == TokenKind::Lt)
      depth++;
    else if (t.kind == TokenKind::Gt) {
      depth--;
      if (depth == 0) {
        const Token &next = peek(i + 1);
        return next.kind == TokenKind::Identifier ||
               next.kind == TokenKind::Star;
      }
    } else if (t.kind == TokenKind::Semicolon || t.kind == TokenKind::LBrace ||
               t.kind == TokenKind::RBrace || t.kind == TokenKind::Eq) {
      return false;
    }
    i++;
  }
}

StmtPtr Parser::parseBlock() {
  auto loc = peek().loc;
  expect(TokenKind::LBrace, "'{'");
  auto blk = StmtBlock{};
  while (!match(TokenKind::RBrace)) {
    if (is(TokenKind::End)) {
      diags.report(DiagLevel::Error, peek().loc, "expected '}'");
      break;
    }
    try {
      // try declaration: optional 'const', optional 'static', then type keyword
      // or identifier
      bool isConst = false;
      if (is(TokenKind::Kw_const) || is(TokenKind::Kw_static) ||
          is_type_keyword(peek().kind) ||
          (is(TokenKind::Identifier) &&
           (is(TokenKind::Identifier, 1) || is(TokenKind::Star, 1) ||
            isGenericInstantiation()))) {
        auto loc = peek().loc;
        if (match(TokenKind::Kw_const)) {
          isConst = true;
        }
        if (match(TokenKind::Kw_static)) {
          // storage class ignored in AST for now
        }
        TypeName t = parseType();
        blk.stmts.push_back(parseDeclStmt(std::move(t), loc, isConst));
      } else {
        blk.stmts.push_back(parseStmt());
      }
    } catch (const ParseError &e) {
      diags.report(DiagLevel::Error, e.loc, e.what());
      synchronize();
    }
  }
  auto s = std::make_unique<Stmt>();
  s->v = std::move(blk);
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseDeclStmt(TypeName t, DiagLoc loc, bool isConst) {
  std::vector<Declarator> decls;
  std::string name;

  DiagLoc varLoc = peek().loc;
  parseDeclaratorTail(t, name);
  std::optional<ExprPtr> init;
  if (match(TokenKind::Assign)) {
    init = parseExpr();
  }
  decls.push_back({std::move(name), varLoc, std::move(init)});
  while (match(TokenKind::Comma)) {
    std::string n2;
    TypeName t2 = t;
    DiagLoc varLoc2 = peek().loc;
    parseDeclaratorTail(t2, n2);
    std::optional<ExprPtr> init2;
    if (match(TokenKind::Assign)) {
      init2 = parseExpr();
    }
    decls.push_back({std::move(n2), varLoc2, std::move(init2)});
  }
  expect(TokenKind::Semicolon, "; expected after declaration");
  auto s = std::make_unique<Stmt>();
  s->v = StmtDecl{std::move(t), std::move(decls), isConst};
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseStmt() {
  if (match(TokenKind::LBrace)) {
    pos--;
    return parseBlock();
  }
  if (match(TokenKind::Kw_return)) {
    pos--;
    return parseReturn();
  }
  if (match(TokenKind::Kw_for)) {
    pos--;
    return parseFor();
  }
  if (match(TokenKind::Kw_if)) {
    pos--;
    return parseIf();
  }
  if (match(TokenKind::Kw_while)) {
    pos--;
    return parseWhile();
  }
  if (match(TokenKind::Kw_switch)) {
    pos--;
    return parseSwitch();
  }
  if (match(TokenKind::Kw_break)) {
    auto loc = toks[pos - 1].loc;
    expect(TokenKind::Semicolon, "; expected after break");
    auto s = std::make_unique<Stmt>();
    s->v = StmtBreak{};
    s->loc = loc;
    return s;
  }
  if (match(TokenKind::Kw_continue)) {
    auto loc = toks[pos - 1].loc;
    expect(TokenKind::Semicolon, "; expected after continue");
    auto s = std::make_unique<Stmt>();
    s->v = StmtContinue{};
    s->loc = loc;
    return s;
  }
  if (match(TokenKind::Kw_asm)) {
    pos--;
    return parseAsm();
  }

  // expression statement
  auto loc = peek().loc;
  auto e = parseExpr();
  expect(TokenKind::Semicolon, "; expected after expression");
  auto s = std::make_unique<Stmt>();
  s->v = StmtExpr{std::move(e)};
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseReturn() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_return, "return");
  StmtReturn r;
  if (!is(TokenKind::Semicolon))
    r.expr = parseExpr();
  expect(TokenKind::Semicolon, "; expected after return");
  auto s = std::make_unique<Stmt>();
  s->v = std::move(r);
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseFor() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_for, "for");
  expect(TokenKind::LParen, "'('");
  StmtFor f;
  // init: decl or expr or ';'
  if (!is(TokenKind::Semicolon)) {
    bool isConst = false;
    if (is(TokenKind::Kw_const) || is(TokenKind::Kw_static) ||
        is_type_keyword(peek().kind) ||
        (is(TokenKind::Identifier) &&
         (is(TokenKind::Identifier, 1) || is(TokenKind::Star, 1) ||
          isGenericInstantiation()))) {
      auto loc = peek().loc;
      if (match(TokenKind::Kw_const)) {
        isConst = true;
      }
      if (match(TokenKind::Kw_static)) {
        // ignore 'static' at for-init
      }
      TypeName t = parseType();
      f.init = parseDeclStmt(std::move(t), loc, isConst);
    } else {
      auto e = parseExpr();
      expect(TokenKind::Semicolon, "; after for init");
      auto s = std::make_unique<Stmt>();
      s->v = StmtExpr{std::move(e)};
      f.init = std::move(s);
    }
  } else {
    expect(TokenKind::Semicolon, ";");
  }
  // cond
  if (!is(TokenKind::Semicolon))
    f.cond = parseExpr();
  expect(TokenKind::Semicolon, "; after for cond");
  // iter
  if (!is(TokenKind::RParen))
    f.iter = parseExpr();
  expect(TokenKind::RParen, ") after for iter");
  f.body = parseStmt();
  auto s = std::make_unique<Stmt>();
  s->v = std::move(f);
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseIf() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_if, "if");
  expect(TokenKind::LParen, "'(' after if");
  auto cond = parseExpr();
  expect(TokenKind::RParen, "')' after if condition");
  auto thenBranch = parseStmt();
  std::optional<StmtPtr> elseBranch;
  if (match(TokenKind::Kw_else)) {
    elseBranch = parseStmt();
  }
  auto s = std::make_unique<Stmt>();
  s->v = StmtIf{std::move(cond), std::move(thenBranch), std::move(elseBranch)};
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseWhile() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_while, "while");
  expect(TokenKind::LParen, "'(' after while");
  auto cond = parseExpr();
  expect(TokenKind::RParen, "')' after while condition");
  auto body = parseStmt();
  auto s = std::make_unique<Stmt>();
  s->v = StmtWhile{std::move(cond), std::move(body)};
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseSwitch() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_switch, "switch");
  expect(TokenKind::LParen, "'(' after switch");
  auto cond = parseExpr();
  expect(TokenKind::RParen, "')' after switch condition");
  expect(TokenKind::LBrace, "'{'");

  std::vector<Case> cases;
  std::optional<StmtPtr> defaultCase;

  while (!match(TokenKind::RBrace)) {
    if (match(TokenKind::Kw_case)) {
      std::vector<ExprPtr> values;
      while (true) {
        values.push_back(parseExpr());
        if (!match(TokenKind::Comma))
          break;
      }
      expect(TokenKind::Colon, ": after case value");
      // Parse statements until next case/default/end
      StmtBlock body;
      while (!is(TokenKind::Kw_case) && !is(TokenKind::Kw_default) &&
             !is(TokenKind::RBrace)) {
        body.stmts.push_back(parseStmt());
      }
      auto s = std::make_unique<Stmt>();
      s->v = std::move(body);
      cases.push_back(Case{std::move(values), std::move(s)});
    } else if (match(TokenKind::Kw_default)) {
      expect(TokenKind::Colon, ": after default");
      StmtBlock body;
      while (!is(TokenKind::Kw_case) && !is(TokenKind::Kw_default) &&
             !is(TokenKind::RBrace)) {
        body.stmts.push_back(parseStmt());
      }
      auto s = std::make_unique<Stmt>();
      s->v = std::move(body);
      defaultCase = std::move(s);
    } else {
      throw parse_error(peek(), "expected case or default in switch");
    }
  }

  auto s = std::make_unique<Stmt>();
  s->v = StmtSwitch{std::move(cond), std::move(cases), std::move(defaultCase)};
  s->loc = loc;
  return s;
}

StmtPtr Parser::parseAsm() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_asm, "asm");
  expect(TokenKind::LParen, "(");
  std::string code = expect(TokenKind::String, "asm string").text;
  expect(TokenKind::RParen, ")");
  expect(TokenKind::Semicolon, ";");
  auto s = std::make_unique<Stmt>();
  s->v = StmtAsm{std::move(code), true};
  s->loc = loc;
  return s;
}

} // namespace agc
