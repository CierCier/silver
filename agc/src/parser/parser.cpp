#include "agc/parser.hpp"

namespace agc {

static ParseError parse_error(const Token &t, const char *msg) {
  return ParseError(t.loc, msg);
}

const Token &Parser::expect(TokenKind k, const char *msg) {
  if (!is(k)) {
    // Check if we should report at the end of the previous token
    if (pos > 0 && k == TokenKind::Semicolon) {
      const Token &prev = toks[pos - 1];
      const Token &curr = peek();
      if (curr.loc.line > prev.loc.line) {
        // Construct a location at the end of prev
        DiagLoc loc = prev.loc;
        loc.col += prev.text.length();
        throw ParseError(loc, msg);
      }
    }
    throw parse_error(peek(), msg);
  }
  return toks[pos++];
}

Program Parser::parseProgram() {
  Program p;
  while (!is(TokenKind::End)) {
    try {
      p.decls.push_back(parseExternal());
    } catch (const ParseError &e) {
      diags.report(DiagLevel::Error, e.loc, e.what());
      synchronize();
    }
  }
  return p;
}

void Parser::synchronize() {
  pos++;
  while (!is(TokenKind::End)) {
    if (toks[pos - 1].kind == TokenKind::Semicolon)
      return;

    switch (peek().kind) {
    case TokenKind::Kw_struct:
    case TokenKind::Kw_if:
    case TokenKind::Kw_while:
    case TokenKind::Kw_return:
    case TokenKind::Kw_import:
    case TokenKind::Kw_extern:
    case TokenKind::RBrace:
      return;
    default:
      pos++;
    }
  }
}

DeclPtr Parser::parseExternal() {
  auto loc = peek().loc;
  bool isExtern = false, isStatic = false;
  if (match(TokenKind::Kw_import)) {
    pos--; // rewind to let parseImport consume the keyword and path
    return parseImport();
  }
  if (match(TokenKind::Kw_link)) {
    pos--;
    return parseLink();
  }
  if (match(TokenKind::Kw_struct)) {
    pos--;
    return parseStruct();
  }
  if (match(TokenKind::Kw_enum)) {
    pos--;
    return parseEnum();
  }
  if (match(TokenKind::Kw_impl)) {
    pos--;
    return parseImpl();
  }

  if (match(TokenKind::Kw_extern))
    isExtern = true;
  else if (match(TokenKind::Kw_static))
    isStatic = true;

  TypeName t = parseType();
  return parseDeclOrFunc(std::move(t), loc, isExtern, isStatic);
}

} // namespace agc
