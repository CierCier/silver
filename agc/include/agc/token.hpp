#pragma once
#include "diagnostics.hpp"
#include <cstdint>
#include <string>

namespace agc {

enum class TokenKind : uint16_t {
  End,
  Identifier,
  Integer,
  Float,
  String,

  // Keywords
  Kw_import,
  Kw_struct,
  Kw_enum,
  Kw_extern,
  Kw_static,
  Kw_link,
  Kw_switch,
  Kw_case,
  Kw_default,
  Kw_impl,
  Kw_comptime,
  Kw_const,
  Kw_return,
  Kw_for,
  Kw_while,
  Kw_if,
  Kw_else,
  Kw_break,
  Kw_continue,
  Kw_asm,
  Kw_as,
  Kw_cast,
  Kw_implicit,
  Kw_trait,
  Kw_new,
  Kw_drop,
  Kw_alloc,
  Kw_free,
  Kw_true,
  Kw_false,

  // Primitive type keywords (treated as identifiers in AST typing)
  Kw_void,
  Kw_bool,
  Kw_i8,
  Kw_i16,
  Kw_i32,
  Kw_i64,
  Kw_u8,
  Kw_u16,
  Kw_u32,
  Kw_u64,
  Kw_f32,
  Kw_f64,
  Kw_char,
  Kw_str,

  // Operators / punctuation
  LParen,
  RParen,
  LBrace,
  RBrace,
  LBracket,
  RBracket,
  Comma,
  Semicolon,
  Colon,
  Dot,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  PlusPlus,
  MinusMinus,
  Amp,
  Pipe,
  Caret,
  Tilde,
  Bang,
  Assign,
  PlusAssign,
  MinusAssign,
  StarAssign,
  SlashAssign,
  PercentAssign,
  Shl,
  Shr,
  ShlAssign,
  ShrAssign,
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
  AndAnd,
  OrOr,
  Question,

  Arrow,
  DotDotDot,
  At,
};

using SourceLoc = DiagLoc;

struct Token {
  TokenKind kind{TokenKind::End};
  std::string text{}; // raw lexeme (unescaped for strings)
  SourceLoc loc{};
};

inline bool is_type_keyword(TokenKind k) {
  switch (k) {
  case TokenKind::Kw_void:
  case TokenKind::Kw_bool:
  case TokenKind::Kw_i8:
  case TokenKind::Kw_i16:
  case TokenKind::Kw_i32:
  case TokenKind::Kw_i64:
  case TokenKind::Kw_u8:
  case TokenKind::Kw_u16:
  case TokenKind::Kw_u32:
  case TokenKind::Kw_u64:
  case TokenKind::Kw_f32:
  case TokenKind::Kw_f64:
  case TokenKind::Kw_char:
  case TokenKind::Kw_str:
    return true;
  default:
    return false;
  }
}

} // namespace agc
