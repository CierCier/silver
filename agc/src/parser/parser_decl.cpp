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
  // Generic arguments: Type<A, B>
  if (match(TokenKind::Lt)) {
    while (true) {
      ty.genericArgs.push_back(parseType());
      if (match(TokenKind::Gt))
        break;
      expect(TokenKind::Comma, "expected ',' or '>' in generic arguments");
    }
  }

  // pointer depth
  while (match(TokenKind::Star))
    ty.pointerDepth++;

  return ty;
}

void Parser::parseDeclaratorTail(TypeName &t, std::string &name) {
  // identifier - also accept 'drop', 'new', 'alloc', 'free' keywords as names
  // for methods
  if (is(TokenKind::Identifier)) {
    name = expect(TokenKind::Identifier, "declarator name").text;
  } else if (is(TokenKind::Kw_drop)) {
    name = peek().text;
    ++pos;
  } else if (is(TokenKind::Kw_new)) {
    name = peek().text;
    ++pos;
  } else if (is(TokenKind::Kw_alloc)) {
    name = peek().text;
    ++pos;
  } else if (is(TokenKind::Kw_free)) {
    name = peek().text;
    ++pos;
  } else {
    throw parse_error(peek(), "expected declarator name");
  }
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

// Parse a single attribute: @name or @name(arg1, arg2, ...)
Attribute Parser::parseAttribute() {
  expect(TokenKind::At, "expected '@'");

  // Accept either identifier or 'trait' keyword as attribute name
  std::string name;
  if (is(TokenKind::Kw_trait)) {
    name = peek().text;
    ++pos;
  } else {
    name = expect(TokenKind::Identifier, "attribute name").text;
  }

  std::vector<std::string> args;

  if (match(TokenKind::LParen)) {
    if (!is(TokenKind::RParen)) {
      do {
        // Accept identifiers or keywords as trait arguments
        // (copy, clone, drop, default, debug, new, alloc, free, etc.)
        const Token &t = peek();
        if (t.kind == TokenKind::Identifier ||
            t.kind == TokenKind::Kw_default || t.kind == TokenKind::Kw_drop ||
            t.kind == TokenKind::Kw_new || t.kind == TokenKind::Kw_alloc ||
            t.kind == TokenKind::Kw_free) {
          args.push_back(t.text);
          ++pos;
        } else {
          throw parse_error(t, "expected trait name");
        }
      } while (match(TokenKind::Comma));
    }
    expect(TokenKind::RParen, "expected ')' after attribute arguments");
  }

  return Attribute{std::move(name), std::move(args)};
}

// Parse multiple attributes: @attr1 @attr2(a, b) ...
std::vector<Attribute> Parser::parseAttributes() {
  std::vector<Attribute> attrs;
  while (is(TokenKind::At)) {
    attrs.push_back(parseAttribute());
  }
  return attrs;
}

DeclPtr Parser::parseStruct(std::vector<Attribute> attrs) {
  auto loc = peek().loc;
  expect(TokenKind::Kw_struct, "expected 'struct'");
  std::string name = expect(TokenKind::Identifier, "struct name").text;
  std::vector<std::string> genericParams;
  if (match(TokenKind::Lt)) {
    while (true) {
      genericParams.push_back(
          expect(TokenKind::Identifier, "generic parameter name").text);
      if (match(TokenKind::Gt))
        break;
      expect(TokenKind::Comma, "expected ',' or '>' in generic parameters");
    }
  }
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
  d->v = DeclStruct{std::move(name), std::move(fields),
                    std::move(genericParams), std::move(attrs)};
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

DeclPtr Parser::parseTrait() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_trait, "expected 'trait'");
  std::string name = expect(TokenKind::Identifier, "trait name").text;

  // Optional generic parameters: trait Clone<T> { ... }
  std::vector<std::string> genericParams;
  if (match(TokenKind::Lt)) {
    while (true) {
      genericParams.push_back(
          expect(TokenKind::Identifier, "generic parameter name").text);
      if (match(TokenKind::Gt))
        break;
      expect(TokenKind::Comma, "expected ',' or '>' in generic parameters");
    }
  }

  expect(TokenKind::LBrace, "expected '{'");

  std::vector<TraitMethod> methods;
  while (!match(TokenKind::RBrace)) {
    // Parse method signature: ReturnType name(params);
    TypeName retType = parseType();
    // Accept identifier or 'drop'/'new'/'alloc'/'free' keywords as method names
    std::string methodName;
    if (is(TokenKind::Identifier)) {
      methodName = expect(TokenKind::Identifier, "method name").text;
    } else if (is(TokenKind::Kw_drop)) {
      methodName = peek().text;
      ++pos;
    } else if (is(TokenKind::Kw_new)) {
      methodName = peek().text;
      ++pos;
    } else if (is(TokenKind::Kw_alloc)) {
      methodName = peek().text;
      ++pos;
    } else if (is(TokenKind::Kw_free)) {
      methodName = peek().text;
      ++pos;
    } else {
      throw parse_error(peek(), "expected method name");
    }

    expect(TokenKind::LParen, "expected '('");
    std::vector<Param> params;
    if (!is(TokenKind::RParen)) {
      do {
        TypeName pt = parseType();
        std::string pn;
        if (is(TokenKind::Identifier)) {
          pn = expect(TokenKind::Identifier, "parameter name").text;
        }
        params.push_back(Param{std::move(pt), std::move(pn)});
      } while (match(TokenKind::Comma));
    }
    expect(TokenKind::RParen, "expected ')'");
    expect(TokenKind::Semicolon, "expected ';' after method signature");

    methods.push_back(TraitMethod{std::move(retType), std::move(methodName),
                                  std::move(params)});
  }

  auto d = std::make_unique<Decl>();
  d->v =
      DeclTrait{std::move(name), std::move(genericParams), std::move(methods)};
  d->loc = loc;
  return d;
}

DeclPtr Parser::parseDeclOrFunc(TypeName ty, DiagLoc loc, bool isExtern,
                                bool isStatic) {
  std::string name;
  parseDeclaratorTail(ty, name);

  std::vector<std::string> genericParams;
  if (match(TokenKind::Lt)) {
    while (true) {
      genericParams.push_back(
          expect(TokenKind::Identifier, "generic parameter name").text);
      if (match(TokenKind::Gt))
        break;
      expect(TokenKind::Comma, "expected ',' or '>' in generic parameters");
    }
  }

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
    d->v = DeclFunc{std::move(ty),     std::move(name),         "",
                    std::move(params), std::move(body),         isExtern,
                    isVariadic,        std::move(genericParams)};
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

DeclPtr Parser::parseCast(bool isImplicit) {
  auto loc = peek().loc;
  expect(TokenKind::Kw_cast, "expected 'cast'");
  TypeName target = parseType();
  expect(TokenKind::LParen, "expected '('");
  std::vector<Param> params;
  if (!match(TokenKind::RParen)) {
    while (true) {
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
  auto blk = parseBlock();
  std::optional<StmtBlock> body;
  if (auto *sb = std::get_if<StmtBlock>(&blk->v)) {
    body = std::move(*sb);
  } else {
    throw parse_error(peek(), "expected block");
  }

  auto d = std::make_unique<Decl>();
  d->v = DeclCast{std::move(target), "", std::move(params), std::move(body),
                  isImplicit};
  d->loc = loc;
  return d;
}

DeclPtr Parser::parseImpl() {
  auto loc = peek().loc;
  expect(TokenKind::Kw_impl, "expected 'impl'");
  TypeName ty = parseType();
  expect(TokenKind::LBrace, "expected '{'");

  std::vector<DeclPtr> methods;
  while (!match(TokenKind::RBrace)) {
    if (match(TokenKind::Kw_implicit)) {
      methods.push_back(parseCast(true));
    } else if (is(TokenKind::Kw_cast)) {
      methods.push_back(parseCast(false));
    } else {
      // Parse method (function)
      // Methods look like functions but inside impl
      // We can reuse parseDeclOrFunc but need to handle it carefully
      // Actually, methods usually start with 'fn' or return type?
      // In Silver, functions start with return type.
      // So we parse type, then name...
      TypeName rt = parseType();
      methods.push_back(parseDeclOrFunc(rt, peek().loc, false, false));
    }
  }

  auto d = std::make_unique<Decl>();
  d->v = DeclImpl{std::move(ty), std::move(methods)};
  d->loc = loc;
  return d;
}

} // namespace agc
