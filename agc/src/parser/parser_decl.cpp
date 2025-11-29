#include "agc/parser.hpp"

namespace agc {

static ParseError parse_error(const Token &t, const char *msg) {
  return ParseError(t.loc, msg);
}

DeclPtr Parser::parseImport() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_import, "expected 'import'");
  std::vector<std::string> path;
  path.push_back(expect(TokenKind::Identifier, "module id").text);
  while (match(TokenKind::Dot)) {
    path.push_back(expect(TokenKind::Identifier, "module id").text);
  }
  expect(TokenKind::Semicolon, "; expected after import");
  auto d = std::make_unique<Decl>();
  d->v = DeclImport{std::move(path)};
  d->loc = loc;
  return d;
}

DeclPtr Parser::parseLink() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_link, "expected 'link'");
  std::string lib = expect(TokenKind::String, "library name").text;
  expect(TokenKind::Semicolon, "; expected after link");
  auto d = std::make_unique<Decl>();
  d->v = DeclLink{std::move(lib)};
  d->loc = loc;
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
  auto loc = peek().loc;
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
  d->loc = loc;
  return d;
}

DeclPtr Parser::parseEnum() {
  auto loc = peek().loc;
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
  d->loc = loc;
  return d;
}

DeclPtr Parser::parseDeclOrFunc(TypeName ty, DiagLoc loc, bool isExtern,
                                bool isStatic) {
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
    d->v = DeclFunc{std::move(ty),   std::move(name), std::move(params),
                    std::move(body), isExtern,        isVariadic};
    d->loc = loc;
    return d;
  } else {
    // variable declaration (could be global)
    std::vector<Declarator> decls;
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

    decls.push_back({std::move(name), loc, std::move(init)});

    while (match(TokenKind::Comma)) {
      std::string n2;
      TypeName ty2 = ty; // same base type

      DiagLoc varLoc = peek().loc;
      parseDeclaratorTail(ty2, n2);
      std::optional<ExprPtr> init2;
      if (match(TokenKind::Assign))
        init2 = parseExpr();
      decls.push_back({std::move(n2), varLoc, std::move(init2)});
    }
    expect(TokenKind::Semicolon, "; expected after declaration");
    auto d = std::make_unique<Decl>();
    d->v = DeclVar{std::move(ty), std::move(decls), isExtern, isStatic};
    d->loc = loc;
    return d;
  }
}

} // namespace agc
