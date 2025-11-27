#include "agc/parser.hpp"
#include <iostream>
#include <stdexcept>

namespace agc {

static std::runtime_error parse_error(const Token &t, const char *msg) {
  return std::runtime_error("parse error at line " +
                            std::to_string(t.loc.line) + ": " + msg);
}

const Token &Parser::expect(TokenKind k, const char *msg) {
  if (!is(k))
    throw parse_error(peek(), msg);
  return toks[pos++];
}

Program Parser::parseProgram() {
  Program p;
  while (!is(TokenKind::End)) {
    p.decls.push_back(parseExternal());
  }
  return p;
}

DeclPtr Parser::parseExternal() {
  bool isExtern = false, isStatic = false;
  if (match(TokenKind::Kw_import)) {
    pos--; // rewind to let parseImport consume the keyword and path
    return parseImport();
  }
  if (match(TokenKind::Kw_struct)) {
    pos--;
    return parseStruct();
  }
  if (match(TokenKind::Kw_enum)) {
    pos--;
    return parseEnum();
  }

  if (match(TokenKind::Kw_extern))
    isExtern = true;
  else if (match(TokenKind::Kw_static))
    isStatic = true;

  TypeName t = parseType();
  return parseDeclOrFunc(std::move(t), isExtern, isStatic);
}

DeclPtr Parser::parseImport() {
  expect(TokenKind::Kw_import, "expected 'import'");
  std::vector<std::string> path;
  path.push_back(expect(TokenKind::Identifier, "module id").text);
  while (match(TokenKind::Dot)) {
    path.push_back(expect(TokenKind::Identifier, "module id").text);
  }
  expect(TokenKind::Semicolon, "; expected after import");
  auto d = std::make_unique<Decl>();
  d->v = DeclImport{std::move(path)};
  return d;
}

TypeName Parser::parseType() {
  const Token &t = peek();
  TypeName ty;
  if (is_type_keyword(t.kind) || t.kind == TokenKind::Identifier) {
    ty.name = t.text;
    ++pos;
  } else {
    throw parse_error(t, "expected type name");
  }
  // pointer depth
  while (match(TokenKind::Star))
    ty.pointerDepth++;
  return ty;
}

void Parser::parseDeclaratorTail(TypeName &t, std::string &name) {
  // identifier
  name = expect(TokenKind::Identifier, "declarator name").text;
  // arrays (suffix form)
  while (match(TokenKind::LBracket)) {
    if (is(TokenKind::RBracket)) {
      expect(TokenKind::RBracket, "]");
      t.arrayDims.push_back(std::nullopt);
    } else {
      const Token &n = expect(TokenKind::Integer, "array size");
      t.arrayDims.push_back(std::stoull(n.text));
      expect(TokenKind::RBracket, "]");
    }
  }
}

DeclPtr Parser::parseStruct() {
  expect(TokenKind::Kw_struct, "expected 'struct'");
  std::string name = expect(TokenKind::Identifier, "struct name").text;
  expect(TokenKind::LBrace, "'{'");

  std::vector<StructField> fields;

  // Support two styles: semicolon-terminated lines; or comma-separated items
  // with optional trailing comma.
  while (!match(TokenKind::RBrace)) {
    // try semicolon-terminated member decl: Type declarator_list ';'
    TypeName mt = parseType();
    std::vector<std::string> names;
    std::string n;
    parseDeclaratorTail(mt, n);
    names.push_back(std::move(n));
    while (match(TokenKind::Comma)) {
      std::string n2;
      parseDeclaratorTail(mt, n2);
      names.push_back(std::move(n2));
    }
    if (match(TokenKind::Semicolon)) {
      fields.push_back(StructField{mt, std::move(names)});
    } else if (match(TokenKind::Comma)) {
      // comma-terminated field (alternate style)
      fields.push_back(StructField{mt, std::move(names)});
      // optional trailing comma chain: continue
    } else if (is(TokenKind::RBrace)) {
      fields.push_back(StructField{mt, std::move(names)});
      expect(TokenKind::RBrace, "'}'");
      break;
    } else {
      throw parse_error(peek(), "expected ';', ',' or '}' in struct");
    }
  }

  auto d = std::make_unique<Decl>();
  d->v = DeclStruct{std::move(name), std::move(fields)};
  return d;
}

DeclPtr Parser::parseEnum() {
  expect(TokenKind::Kw_enum, "expected 'enum'");
  std::string name = expect(TokenKind::Identifier, "enum name").text;
  expect(TokenKind::LBrace, "'{'");
  std::vector<EnumItem> items;
  while (!match(TokenKind::RBrace)) {
    std::string id;
    if (is(TokenKind::Identifier)) {
      id = expect(TokenKind::Identifier, "enumerator").text;
    } else if (is(TokenKind::Integer) && is(TokenKind::Identifier, 1)) {
      // allow names like 1D, 2D, etc.
      id = expect(TokenKind::Integer, "enum item number").text;
      id += expect(TokenKind::Identifier, "enum item suffix").text;
    } else {
      throw parse_error(peek(), "enumerator");
    }
    std::optional<uint64_t> val;
    if (match(TokenKind::Assign)) {
      const Token &n = expect(TokenKind::Integer, "enum value");
      val = std::stoull(n.text);
    }
    items.push_back(EnumItem{std::move(id), val});
    if (match(TokenKind::Semicolon)) {
      // continue
    } else if (is(TokenKind::RBrace)) {
      continue;
    } else {
      throw parse_error(peek(), "expected ';' or '}' after enum item");
    }
  }
  auto d = std::make_unique<Decl>();
  d->v = DeclEnum{std::move(name), std::move(items)};
  return d;
}

DeclPtr Parser::parseDeclOrFunc(TypeName ty, bool isExtern, bool isStatic) {
  std::string name;
  parseDeclaratorTail(ty, name);

  if (match(TokenKind::LParen)) {
    // function
    std::vector<Param> params;
    bool isVariadic = false;
    if (!match(TokenKind::RParen)) {
      while (true) {
        if (match(TokenKind::DotDotDot)) {
          isVariadic = true;
          // Variadic must be last
          if (!match(TokenKind::RParen)) {
             throw parse_error(peek(), "expected ')' after '...'");
          }
          break; // Done with params
        }

        TypeName pt = parseType();
        std::string pn;
        if (peek().kind == TokenKind::Identifier) {
          pn = expect(TokenKind::Identifier, "parameter name").text;
        }
        params.push_back(Param{std::move(pt), std::move(pn)});
        if (match(TokenKind::RParen))
          break;
        expect(TokenKind::Comma, ", or ) expected");
      }
    }
    std::optional<StmtBlock> body;
    if (!isExtern && !match(TokenKind::Semicolon)) {
      auto blk = parseBlock();
      if (auto *sb = std::get_if<StmtBlock>(&blk->v)) {
          body = std::move(*sb);
      } else {
          throw parse_error(peek(), "expected block");
      }
    } else {
      if (isExtern)
        expect(TokenKind::Semicolon, "; expected after extern declaration");
    }

    auto d = std::make_unique<Decl>();
    d->v = DeclFunc{std::move(ty), std::move(name), std::move(params),
                    std::move(body), isExtern, isVariadic};
    return d;
  } else {
    // variable declaration (could be global)
    std::vector<std::pair<std::string, std::optional<ExprPtr>>> decls;
    std::optional<ExprPtr> init;
    if (match(TokenKind::Assign)) {
      if (match(TokenKind::LBrace)) {
        int depth = 1;
        while (depth > 0) {
          if (match(TokenKind::LBrace))
            depth++;
          else if (match(TokenKind::RBrace))
            depth--;
          else
            ++pos;
        }
      } else {
        init = parseExpr();
      }
    }
    decls.emplace_back(std::move(name), std::move(init));
    while (match(TokenKind::Comma)) {
      std::string n2;
      TypeName ty2 = ty; // same base type
      parseDeclaratorTail(ty2, n2);
      std::optional<ExprPtr> init2;
      if (match(TokenKind::Assign))
        init2 = parseExpr();
      decls.emplace_back(std::move(n2), std::move(init2));
    }
    expect(TokenKind::Semicolon, "; expected after declaration");
    auto d = std::make_unique<Decl>();
    d->v = DeclVar{std::move(ty), std::move(decls), isExtern, isStatic};
    return d;
  }
}

StmtPtr Parser::parseBlock() {
  expect(TokenKind::LBrace, "'{'");
  auto blk = StmtBlock{};
  while (!match(TokenKind::RBrace)) {
    // try declaration: optional 'const', optional 'static', then type keyword
    // or identifier
    bool isConst = false;
    if (is(TokenKind::Kw_const) || is(TokenKind::Kw_static) ||
        is_type_keyword(peek().kind) ||
        (is(TokenKind::Identifier) &&
         (is(TokenKind::Identifier, 1) || is(TokenKind::Star, 1)))) {
      if (match(TokenKind::Kw_const)) {
        isConst = true;
      }
      if (match(TokenKind::Kw_static)) {
        // storage class ignored in AST for now
      }
      TypeName t = parseType();
      blk.stmts.push_back(parseDeclStmt(std::move(t), isConst));
    } else {
      blk.stmts.push_back(parseStmt());
    }
  }
  auto s = std::make_unique<Stmt>();
  s->v = std::move(blk);
  return s;
}

StmtPtr Parser::parseDeclStmt(TypeName t, bool isConst) {
  std::vector<std::pair<std::string, std::optional<ExprPtr>>> decls;
  std::string name;
  parseDeclaratorTail(t, name);
  std::optional<ExprPtr> init;
  if (match(TokenKind::Assign)) {
    if (match(TokenKind::LBrace)) {
      int depth = 1;
      while (depth > 0) {
        if (match(TokenKind::LBrace))
          depth++;
        else if (match(TokenKind::RBrace))
          depth--;
        else
          ++pos;
      }
    } else {
      init = parseExpr();
    }
  }
  decls.emplace_back(std::move(name), std::move(init));
  while (match(TokenKind::Comma)) {
    std::string n2;
    TypeName t2 = t;
    parseDeclaratorTail(t2, n2);
    std::optional<ExprPtr> init2;
    if (match(TokenKind::Assign)) {
      if (match(TokenKind::LBrace)) {
        int depth = 1;
        while (depth > 0) {
          if (match(TokenKind::LBrace))
            depth++;
          else if (match(TokenKind::RBrace))
            depth--;
          else
            ++pos;
        }
      } else {
        init2 = parseExpr();
      }
    }
    decls.emplace_back(std::move(n2), std::move(init2));
  }
  expect(TokenKind::Semicolon, "; expected after declaration");
  auto s = std::make_unique<Stmt>();
  s->v = StmtDecl{std::move(t), std::move(decls), isConst};
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
  if (match(TokenKind::Kw_break)) {
    expect(TokenKind::Semicolon, "; expected after break");
    auto s = std::make_unique<Stmt>();
    s->v = StmtBreak{};
    return s;
  }
  if (match(TokenKind::Kw_continue)) {
    expect(TokenKind::Semicolon, "; expected after continue");
    auto s = std::make_unique<Stmt>();
    s->v = StmtContinue{};
    return s;
  }
  if (match(TokenKind::Kw_asm)) {
    pos--;
    return parseAsm();
  }

  // expression statement
  auto e = parseExpr();
  expect(TokenKind::Semicolon, "; expected after expression");
  auto s = std::make_unique<Stmt>();
  s->v = StmtExpr{std::move(e)};
  return s;
}

StmtPtr Parser::parseReturn() {
  expect(TokenKind::Kw_return, "return");
  StmtReturn r;
  if (!is(TokenKind::Semicolon))
    r.expr = parseExpr();
  expect(TokenKind::Semicolon, "; expected after return");
  auto s = std::make_unique<Stmt>();
  s->v = std::move(r);
  return s;
}

StmtPtr Parser::parseFor() {
  expect(TokenKind::Kw_for, "for");
  expect(TokenKind::LParen, "'('");
  StmtFor f;
  // init: decl or expr or ';'
  if (!is(TokenKind::Semicolon)) {
    bool isConst = false;
    if (is(TokenKind::Kw_const) || is(TokenKind::Kw_static) ||
        is_type_keyword(peek().kind) ||
        (is(TokenKind::Identifier) &&
         (is(TokenKind::Identifier, 1) || is(TokenKind::Star, 1)))) {
      if (match(TokenKind::Kw_const)) {
        isConst = true;
      }
      if (match(TokenKind::Kw_static)) {
        // ignore 'static' at for-init
      }
      TypeName t = parseType();
      f.init = parseDeclStmt(std::move(t), isConst);
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
  return s;
}

StmtPtr Parser::parseIf() {
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
  return s;
}

StmtPtr Parser::parseWhile() {
  expect(TokenKind::Kw_while, "while");
  expect(TokenKind::LParen, "'(' after while");
  auto cond = parseExpr();
  expect(TokenKind::RParen, "')' after while condition");
  auto body = parseStmt();
  auto s = std::make_unique<Stmt>();
  s->v = StmtWhile{std::move(cond), std::move(body)};
  return s;
}

StmtPtr Parser::parseAsm() {
  expect(TokenKind::Kw_asm, "asm");
  expect(TokenKind::LParen, "(");
  std::string code = expect(TokenKind::String, "asm string").text;
  expect(TokenKind::RParen, ")");
  expect(TokenKind::Semicolon, ";");
  auto s = std::make_unique<Stmt>();
  s->v = StmtAsm{std::move(code), true};
  return s;
}

int Parser::precedence(TokenKind op) const {
  switch (op) {
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
    return -1;
  }
}

ExprPtr Parser::parsePrimary() {
  if (is(TokenKind::Identifier)) {
    std::string id = expect(TokenKind::Identifier, "id").text;
    // call or postfix parsing
    ExprPtr base = std::make_unique<Expr>();
    base->v = ExprIdent{std::move(id)};
    while (true) {
      if (match(TokenKind::LParen)) {
        std::vector<ExprPtr> args;
        if (!is(TokenKind::RParen)) {
          while (true) {
            args.push_back(parseExpr());
            if (match(TokenKind::Comma))
              continue;
            break;
          }
        }
        expect(TokenKind::RParen, ")");
        ExprPtr call = std::make_unique<Expr>();
        // if base is ident, extract name; otherwise unsupported for now
        std::string cname;
        if (auto *idp = std::get_if<ExprIdent>(&base->v))
          cname = idp->name;
        else
          cname = "<expr>";
        call->v = ExprCall{std::move(cname), std::move(args)};
        base = std::move(call);
      } else if (match(TokenKind::LBracket)) {
        auto idx = parseExpr();
        expect(TokenKind::RBracket, "]");
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprIndex{std::move(base), std::move(idx)};
        base = std::move(e);
      } else if (match(TokenKind::Dot)) {
        std::string mem = expect(TokenKind::Identifier, "member").text;
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprMember{std::move(base), std::move(mem), false};
        base = std::move(e);
      } else if (match(TokenKind::PlusPlus)) {
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprUnary{TokenKind::PlusPlus, std::move(base)};
        base = std::move(e);
      } else if (match(TokenKind::MinusMinus)) {
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprUnary{TokenKind::MinusMinus, std::move(base)};
        base = std::move(e);
      } else {
        break;
      }
    }
    return base;
  }
  if (is(TokenKind::Integer)) {
    uint64_t v = std::stoull(expect(TokenKind::Integer, "int").text);
    auto e = std::make_unique<Expr>();
    e->v = ExprInt{v};
    return e;
  }
  if (is(TokenKind::String)) {
    std::string s = expect(TokenKind::String, "string").text;
    auto e = std::make_unique<Expr>();
    e->v = ExprStr{std::move(s)};
    return e;
  }
  if (match(TokenKind::LParen)) {
    auto e = parseExpr();
    expect(TokenKind::RParen, ")");
    return e;
  }
  if (match(TokenKind::Minus)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Minus, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::Bang)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Bang, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::Tilde)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Tilde, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::Plus)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Plus, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::PlusPlus)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::PlusPlus, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::MinusMinus)) {
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::MinusMinus, std::move(rhs)};
    return e;
  }
  if (match(TokenKind::Kw_comptime)) {
    // comptime expr => compile-time evaluation marker
    auto inner = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprComptime{std::move(inner)};
    return e;
  }
  if (match(TokenKind::Amp)) {
    // Address-of operator
    auto operand = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprAddressOf{std::move(operand)};
    return e;
  }
  if (match(TokenKind::Star)) {
    // Dereference operator (unary *)
    auto operand = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprDeref{std::move(operand)};
    return e;
  }
  throw parse_error(peek(), "unexpected token in expression");
}

ExprPtr Parser::parseExpr(int minPrec) {
  auto lhs = parsePrimary();

  // ternary ?: (right-associative, low precedence)
  while (true) {
    if (match(TokenKind::Question)) {
      auto thenE = parseExpr();
      expect(TokenKind::Colon, ":");
      auto elseE = parseExpr();
      auto e = std::make_unique<Expr>();
      e->v = ExprCond{std::move(lhs), std::move(thenE), std::move(elseE)};
      lhs = std::move(e);
      continue;
    }
    // assignment (right-associative) including compound assignments
    if (is(TokenKind::Assign) || is(TokenKind::PlusAssign) ||
        is(TokenKind::MinusAssign) || is(TokenKind::StarAssign) ||
        is(TokenKind::SlashAssign) || is(TokenKind::PercentAssign)) {
      TokenKind aop = peek().kind;
      ++pos;
      auto rhs = parseExpr();
      auto e = std::make_unique<Expr>();
      e->v = ExprAssign{std::move(lhs), std::move(rhs), aop};
      lhs = std::move(e);
      continue;
    }
    // binary operators by precedence
    TokenKind op = peek().kind;
    int prec = precedence(op);
    if (prec < minPrec)
      break;
    ++pos; // consume op
    auto rhs = parseExpr(prec + 1);
    auto e = std::make_unique<Expr>();
    e->v = ExprBinary{op, std::move(lhs), std::move(rhs)};
    lhs = std::move(e);
  }
  return lhs;
}

} // namespace agc
