#include "agc/lexer.hpp"
#include <cctype>

namespace agc {

static TokenKind keywordKind(std::string_view id) {
  if (id == "import")
    return TokenKind::Kw_import;
  if (id == "struct")
    return TokenKind::Kw_struct;
  if (id == "enum")
    return TokenKind::Kw_enum;
  if (id == "extern")
    return TokenKind::Kw_extern;
  if (id == "static")
    return TokenKind::Kw_static;
  if (id == "return")
    return TokenKind::Kw_return;
  if (id == "for")
    return TokenKind::Kw_for;
  if (id == "while")
    return TokenKind::Kw_while;
  if (id == "if")
    return TokenKind::Kw_if;
  if (id == "else")
    return TokenKind::Kw_else;
  if (id == "comptime")
    return TokenKind::Kw_comptime;
  if (id == "const")
    return TokenKind::Kw_const;
  if (id == "break")
    return TokenKind::Kw_break;
  if (id == "continue")
    return TokenKind::Kw_continue;
  if (id == "asm")
    return TokenKind::Kw_asm;
  if (id == "link")
    return TokenKind::Kw_link;
  if (id == "switch")
    return TokenKind::Kw_switch;
  if (id == "case")
    return TokenKind::Kw_case;
  if (id == "default")
    return TokenKind::Kw_default;
  if (id == "impl")
    return TokenKind::Kw_impl;
  if (id == "as")
    return TokenKind::Kw_as;
  if (id == "cast")
    return TokenKind::Kw_cast;
  if (id == "implicit")
    return TokenKind::Kw_implicit;
  if (id == "trait")
    return TokenKind::Kw_trait;
  if (id == "new")
    return TokenKind::Kw_new;
  if (id == "drop")
    return TokenKind::Kw_drop;
  if (id == "alloc")
    return TokenKind::Kw_alloc;
  if (id == "free")
    return TokenKind::Kw_free;

  if (id == "true")
    return TokenKind::Kw_true;
  if (id == "false")
    return TokenKind::Kw_false;

  if (id == "void")
    return TokenKind::Kw_void;
  if (id == "bool")
    return TokenKind::Kw_bool;
  if (id == "i8")
    return TokenKind::Kw_i8;
  if (id == "i16")
    return TokenKind::Kw_i16;
  if (id == "i32")
    return TokenKind::Kw_i32;
  if (id == "i64")
    return TokenKind::Kw_i64;
  if (id == "u8")
    return TokenKind::Kw_u8;
  if (id == "u16")
    return TokenKind::Kw_u16;
  if (id == "u32")
    return TokenKind::Kw_u32;
  if (id == "u64")
    return TokenKind::Kw_u64;
  if (id == "f32")
    return TokenKind::Kw_f32;
  if (id == "f64")
    return TokenKind::Kw_f64;
  if (id == "char")
    return TokenKind::Kw_char;
  if (id == "str")
    return TokenKind::Kw_str;

  return TokenKind::Identifier;
}

char Lexer::get() {
  char c = peek();
  if (c == '\0')
    return c;
  ++pos_;
  if (c == '\n')
    newline();
  else
    loc_.col++;
  return c;
}

void Lexer::newline() {
  loc_.line++;
  loc_.col = 1;
  loc_.file = filename_;
}

bool Lexer::match(char c) {
  if (peek() == c) {
    get();
    return true;
  }
  return false;
}

Token Lexer::make(TokenKind k, std::string s) {
  return Token{k, std::move(s), loc_};
}

void Lexer::skipSpacesAndComments() {
  while (true) {
    char c = peek();
    while (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
      get();
      c = peek();
    }
    if (c == '/' && peek(1) == '/') {
      while (c && c != '\n') {
        c = get();
      }
      continue;
    }
    if (c == '/' && peek(1) == '*') {
      get();
      get();
      int depth = 1;
      while (depth > 0) {
        char d = get();
        if (d == '\0')
          break;
        if (d == '/' && peek() == '*') {
          get();
          depth++;
        } else if (d == '*' && peek() == '/') {
          get();
          depth--;
        }
      }
      continue;
    }
    break;
  }
}

Token Lexer::lexIdentifierOrKeyword() {
  SourceLoc start = loc_;
  std::string s;
  char c = peek();
  while (std::isalnum(static_cast<unsigned char>(c)) || c == '_') {
    s.push_back(get());
    c = peek();
  }
  TokenKind k = keywordKind(s);
  return Token{k, std::move(s), start};
}

Token Lexer::lexNumber() {
  SourceLoc start = loc_;
  std::string s;
  char c = peek();
  bool isFloat = false;
  while (std::isdigit(static_cast<unsigned char>(c))) {
    s.push_back(get());
    c = peek();
  }
  if (c == '.') {
    isFloat = true;
    s.push_back(get());
    c = peek();
    while (std::isdigit(static_cast<unsigned char>(c))) {
      s.push_back(get());
      c = peek();
    }
  }
  return Token{isFloat ? TokenKind::Float : TokenKind::Integer, std::move(s),
               start};
}

Token Lexer::lexString() {
  SourceLoc start = loc_;
  std::string s;
  get(); // opening quote
  while (true) {
    char c = get();
    if (c == '\0' || c == '\n')
      break;
    if (c == '"')
      break;
    if (c == '\\') {
      char e = get();
      switch (e) {
      case 'n':
        s.push_back('\n');
        break;
      case 'r':
        s.push_back('\r');
        break;
      case 't':
        s.push_back('\t');
        break;
      case '0':
        s.push_back('\0');
        break;
      case '\\':
        s.push_back('\\');
        break;
      case '"':
        s.push_back('"');
        break;
      default:
        s.push_back(e);
        break;
      }
    } else {
      s.push_back(c);
    }
  }
  return Token{TokenKind::String, std::move(s), start};
}

Token Lexer::lexCharLiteral() {
  SourceLoc start = loc_;
  std::string s;
  get(); // opening single quote

  char c = peek();
  if (c == '\\') {
    // Escape sequence
    get(); // consume backslash
    char e = get();
    switch (e) {
    case 'n':
      s.push_back('\n');
      break;
    case 'r':
      s.push_back('\r');
      break;
    case 't':
      s.push_back('\t');
      break;
    case '0':
      s.push_back('\0');
      break;
    case '\\':
      s.push_back('\\');
      break;
    case '\'':
      s.push_back('\'');
      break;
    case 'x': {
      // Hex escape: \xNN
      char h1 = get();
      char h2 = get();
      auto hexDigit = [](char ch) -> int {
        if (ch >= '0' && ch <= '9')
          return ch - '0';
        if (ch >= 'a' && ch <= 'f')
          return 10 + (ch - 'a');
        if (ch >= 'A' && ch <= 'F')
          return 10 + (ch - 'A');
        return -1;
      };
      int v1 = hexDigit(h1);
      int v2 = hexDigit(h2);
      if (v1 >= 0 && v2 >= 0) {
        s.push_back(static_cast<char>((v1 << 4) | v2));
      }
      break;
    }
    case 'u': {
      // Unicode escape: \uNNNN (4 hex digits)
      uint32_t codepoint = 0;
      auto hexDigit = [](char ch) -> int {
        if (ch >= '0' && ch <= '9')
          return ch - '0';
        if (ch >= 'a' && ch <= 'f')
          return 10 + (ch - 'a');
        if (ch >= 'A' && ch <= 'F')
          return 10 + (ch - 'A');
        return -1;
      };
      for (int i = 0; i < 4; ++i) {
        char h = get();
        int v = hexDigit(h);
        if (v >= 0) {
          codepoint = (codepoint << 4) | v;
        }
      }
      // Encode as UTF-8
      if (codepoint <= 0x7F) {
        s.push_back(static_cast<char>(codepoint));
      } else if (codepoint <= 0x7FF) {
        s.push_back(static_cast<char>(0xC0 | (codepoint >> 6)));
        s.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
      } else {
        s.push_back(static_cast<char>(0xE0 | (codepoint >> 12)));
        s.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
        s.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
      }
      break;
    }
    case 'U': {
      // Extended Unicode escape: \UNNNNNNNN (8 hex digits)
      uint32_t codepoint = 0;
      auto hexDigit = [](char ch) -> int {
        if (ch >= '0' && ch <= '9')
          return ch - '0';
        if (ch >= 'a' && ch <= 'f')
          return 10 + (ch - 'a');
        if (ch >= 'A' && ch <= 'F')
          return 10 + (ch - 'A');
        return -1;
      };
      for (int i = 0; i < 8; ++i) {
        char h = get();
        int v = hexDigit(h);
        if (v >= 0) {
          codepoint = (codepoint << 4) | v;
        }
      }
      // Encode as UTF-8
      if (codepoint <= 0x7F) {
        s.push_back(static_cast<char>(codepoint));
      } else if (codepoint <= 0x7FF) {
        s.push_back(static_cast<char>(0xC0 | (codepoint >> 6)));
        s.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
      } else if (codepoint <= 0xFFFF) {
        s.push_back(static_cast<char>(0xE0 | (codepoint >> 12)));
        s.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
        s.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
      } else if (codepoint <= 0x10FFFF) {
        s.push_back(static_cast<char>(0xF0 | (codepoint >> 18)));
        s.push_back(static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F)));
        s.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
        s.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
      }
      break;
    }
    default:
      s.push_back(e);
      break;
    }
  } else if (c != '\'' && c != '\0' && c != '\n') {
    // Handle UTF-8 multi-byte characters directly
    // UTF-8 byte patterns:
    // 0xxxxxxx - 1 byte (ASCII)
    // 110xxxxx 10xxxxxx - 2 bytes
    // 1110xxxx 10xxxxxx 10xxxxxx - 3 bytes
    // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx - 4 bytes
    unsigned char first = static_cast<unsigned char>(get());
    s.push_back(static_cast<char>(first));

    int extraBytes = 0;
    if ((first & 0xE0) == 0xC0)
      extraBytes = 1; // 110xxxxx
    else if ((first & 0xF0) == 0xE0)
      extraBytes = 2; // 1110xxxx
    else if ((first & 0xF8) == 0xF0)
      extraBytes = 3; // 11110xxx

    for (int i = 0; i < extraBytes; ++i) {
      unsigned char cont = static_cast<unsigned char>(peek());
      if ((cont & 0xC0) == 0x80) { // 10xxxxxx
        s.push_back(static_cast<char>(get()));
      } else {
        break;
      }
    }
  }

  // Consume closing quote
  if (peek() == '\'') {
    get();
  }

  return Token{TokenKind::CharLiteral, std::move(s), start};
}

std::vector<Token> Lexer::lex() {
  std::vector<Token> out;
  while (true) {
    skipSpacesAndComments();
    char c = peek();
    if (!c)
      break;
    if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
      out.push_back(lexIdentifierOrKeyword());
      continue;
    }
    if (std::isdigit(static_cast<unsigned char>(c))) {
      out.push_back(lexNumber());
      continue;
    }
    if (c == '"') {
      out.push_back(lexString());
      continue;
    }
    if (c == '\'') {
      out.push_back(lexCharLiteral());
      continue;
    }

    SourceLoc start = loc_;
    auto emit = [&](TokenKind k, char ch) {
      get();
      out.push_back(Token{k, std::string(1, ch), start});
    };

    switch (c) {
    case '(':
      emit(TokenKind::LParen, c);
      break;
    case ')':
      emit(TokenKind::RParen, c);
      break;
    case '{':
      emit(TokenKind::LBrace, c);
      break;
    case '}':
      emit(TokenKind::RBrace, c);
      break;
    case '[':
      emit(TokenKind::LBracket, c);
      break;
    case ']':
      emit(TokenKind::RBracket, c);
      break;
    case ',':
      emit(TokenKind::Comma, c);
      break;
    case ';':
      emit(TokenKind::Semicolon, c);
      break;
    case ':':
      emit(TokenKind::Colon, c);
      break;
    case '.':
      get();
      if (match('.') && match('.')) {
        out.push_back(Token{TokenKind::DotDotDot, "...", start});
      } else {
        out.push_back(Token{TokenKind::Dot, ".", start});
      }
      break;
    case '?':
      emit(TokenKind::Question, c);
      break;
    case '+':
      get();
      if (match('+'))
        out.push_back(Token{TokenKind::PlusPlus, "++", start});
      else if (match('='))
        out.push_back(Token{TokenKind::PlusAssign, "+=", start});
      else
        out.push_back(Token{TokenKind::Plus, "+", start});
      break;
    case '-':
      get();
      if (match('>'))
        out.push_back(Token{TokenKind::Arrow, "->", start});
      else if (match('-'))
        out.push_back(Token{TokenKind::MinusMinus, "--", start});
      else if (match('='))
        out.push_back(Token{TokenKind::MinusAssign, "-=", start});
      else
        out.push_back(Token{TokenKind::Minus, "-", start});
      break;
    case '*':
      get();
      if (match('='))
        out.push_back(Token{TokenKind::StarAssign, "*=", start});
      else
        out.push_back(Token{TokenKind::Star, "*", start});
      break;
    case '/':
      get();
      if (match('='))
        out.push_back(Token{TokenKind::SlashAssign, "/=", start});
      else
        out.push_back(Token{TokenKind::Slash, "/", start});
      break;
    case '%':
      get();
      if (match('='))
        out.push_back(Token{TokenKind::PercentAssign, "%=", start});
      else
        out.push_back(Token{TokenKind::Percent, "%", start});
      break;
    case '&':
      get();
      if (match('&'))
        out.push_back(Token{TokenKind::AndAnd, "&&", start});
      else if (match('='))
        out.push_back(
            Token{TokenKind::Amp, "&=", start}); // treat &= as Amp for now
      else
        out.push_back(Token{TokenKind::Amp, "&", start});
      break;
    case '|':
      get();
      if (match('|'))
        out.push_back(Token{TokenKind::OrOr, "||", start});
      else
        out.push_back(Token{TokenKind::Pipe, "|", start});
      break;
    case '^':
      emit(TokenKind::Caret, c);
      break;
    case '~':
      emit(TokenKind::Tilde, c);
      break;
    case '!':
      get();
      if (match('='))
        out.push_back(Token{TokenKind::Ne, "!=", start});
      else
        out.push_back(Token{TokenKind::Bang, "!", start});
      break;
    case '=':
      get();
      if (match('='))
        out.push_back(Token{TokenKind::Eq, "==", start});
      else
        out.push_back(Token{TokenKind::Assign, "=", start});
      break;
    case '<':
      get();
      if (match('<')) {
        if (match('='))
          out.push_back(Token{TokenKind::ShlAssign, "<<=", start});
        else
          out.push_back(Token{TokenKind::Shl, "<<", start});
      } else if (match('='))
        out.push_back(Token{TokenKind::Le, "<=", start});
      else
        out.push_back(Token{TokenKind::Lt, "<", start});
      break;
    case '>':
      get();
      if (match('>')) {
        if (match('='))
          out.push_back(Token{TokenKind::ShrAssign, ">>=", start});
        else
          out.push_back(Token{TokenKind::Shr, ">>", start});
      } else if (match('='))
        out.push_back(Token{TokenKind::Ge, ">=", start});
      else
        out.push_back(Token{TokenKind::Gt, ">", start});
      break;
    case '@':
      emit(TokenKind::At, c);
      break;
    default:
      // unknown char, skip
      get();
      break;
    }
  }
  out.push_back(Token{TokenKind::End, "", loc_});
  return out;
}

} // namespace agc
