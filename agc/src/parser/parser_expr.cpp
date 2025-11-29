#include "agc/parser.hpp"

namespace agc {

static ParseError parse_error(const Token &t, const char *msg) {
  return ParseError(t.loc, msg);
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
    auto loc = peek().loc;
    std::string id = expect(TokenKind::Identifier, "id").text;
    // call or postfix parsing
    ExprPtr base = std::make_unique<Expr>();
    base->v = ExprIdent{std::move(id)};
    base->loc = loc;
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

        if (auto *idp = std::get_if<ExprIdent>(&base->v)) {
          auto call = std::make_unique<Expr>();
          call->loc = base->loc;
          call->v = ExprCall{std::move(idp->name), std::move(args)};
          base = std::move(call);
        } else {
          throw parse_error(peek(), "expected function name for call");
        }
      } else if (match(TokenKind::LBracket)) {
        auto idx = parseExpr();
        expect(TokenKind::RBracket, "]");
        auto loc = base->loc;
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprIndex{std::move(base), std::move(idx)};
        e->loc = loc;
        base = std::move(e);
      } else if (match(TokenKind::Dot)) {
        std::string mem = expect(TokenKind::Identifier, "member").text;
        auto loc = base->loc;
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprMember{std::move(base), std::move(mem), false};
        e->loc = loc;
        base = std::move(e);
      } else if (match(TokenKind::PlusPlus)) {
        auto loc = base->loc;
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprUnary{TokenKind::PlusPlus, std::move(base)};
        e->loc = loc;
        base = std::move(e);
      } else if (match(TokenKind::MinusMinus)) {
        auto loc = base->loc;
        ExprPtr e = std::make_unique<Expr>();
        e->v = ExprUnary{TokenKind::MinusMinus, std::move(base)};
        e->loc = loc;
        base = std::move(e);
      } else {
        break;
      }
    }
    return base;
  }
  if (is(TokenKind::Integer)) {
    auto loc = peek().loc;
    uint64_t v = std::stoull(expect(TokenKind::Integer, "int").text);
    auto e = std::make_unique<Expr>();
    e->v = ExprInt{v};
    e->loc = loc;
    return e;
  }
  if (is(TokenKind::Float)) {
    auto loc = peek().loc;
    double v = std::stod(expect(TokenKind::Float, "float").text);
    auto e = std::make_unique<Expr>();
    e->v = ExprFloat{v};
    e->loc = loc;
    return e;
  }
  if (is(TokenKind::String)) {
    auto loc = peek().loc;
    std::string s = expect(TokenKind::String, "string").text;
    auto e = std::make_unique<Expr>();
    e->v = ExprStr{std::move(s)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::LParen)) {
    // Check for C-style cast: (type) expr
    // We only support primitive types or types starting with keywords for now
    // to avoid ambiguity
    if (is_type_keyword(peek().kind)) {
      TypeName t = parseType();
      expect(TokenKind::RParen, ")");
      auto rhs = parsePrimary(); // Cast binds as unary
      auto e = std::make_unique<Expr>();
      e->v = ExprCast{std::move(rhs), std::move(t)};
      e->loc = e->loc; // fix loc
      return e;
    }

    auto e = parseExpr();
    expect(TokenKind::RParen, ")");
    return e;
  }
  if (match(TokenKind::Minus)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Minus, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Bang)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Bang, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Tilde)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Tilde, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Plus)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::Plus, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::PlusPlus)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::PlusPlus, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::MinusMinus)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprUnary{TokenKind::MinusMinus, std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Amp)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprAddressOf{std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Star)) {
    auto loc = toks[pos - 1].loc;
    auto rhs = parsePrimary();
    auto e = std::make_unique<Expr>();
    e->v = ExprDeref{std::move(rhs)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::Kw_comptime)) {
    auto loc = toks[pos - 1].loc;
    auto expr = parseExpr();
    auto e = std::make_unique<Expr>();
    e->v = ExprComptime{std::move(expr)};
    e->loc = loc;
    return e;
  }
  if (match(TokenKind::LBrace)) {
    auto loc = toks[pos - 1].loc;
    std::vector<ExprPtr> values;
    if (!is(TokenKind::RBrace)) {
      while (true) {
        values.push_back(parseExpr());
        if (match(TokenKind::Comma))
          continue;
        break;
      }
    }
    expect(TokenKind::RBrace, "}");
    auto e = std::make_unique<Expr>();
    e->v = ExprInitList{std::move(values)};
    e->loc = loc;
    return e;
  }

  throw parse_error(peek(), "expected expression");
}

ExprPtr Parser::parseBinary(int minPrec) {
  auto lhs = parsePrimary();
  while (true) {
    auto op = peek().kind;
    int prec = precedence(op);
    if (prec < minPrec)
      break;
    pos++; // consume op
    auto rhs = parseBinary(prec + 1);
    auto e = std::make_unique<Expr>();
    e->v = ExprBinary{op, std::move(lhs), std::move(rhs)};
    e->loc = e->loc; // fix loc?
    lhs = std::move(e);
  }
  return lhs;
}

ExprPtr Parser::parseExpr() {
  auto lhs = parseBinary(0);
  if (match(TokenKind::Assign)) {
    auto rhs = parseExpr();
    auto e = std::make_unique<Expr>();
    e->v = ExprAssign{std::move(lhs), std::move(rhs), TokenKind::Assign};
    e->loc = e->loc;
    return e;
  }
  if (match(TokenKind::Question)) {
    auto thenE = parseExpr();
    expect(TokenKind::Colon, ": expected in ternary");
    auto elseE = parseExpr();
    auto e = std::make_unique<Expr>();
    e->v = ExprCond{std::move(lhs), std::move(thenE), std::move(elseE)};
    e->loc = e->loc;
    return e;
  }
  return lhs;
}

} // namespace agc
