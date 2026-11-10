// External scanner for Silver block comments, which nest: `/* a /* b */ c */`.
#include "tree_sitter/parser.h"

enum TokenType { BLOCK_COMMENT };

void *tree_sitter_silver_external_scanner_create(void) { return NULL; }
void tree_sitter_silver_external_scanner_destroy(void *payload) { (void)payload; }
unsigned tree_sitter_silver_external_scanner_serialize(void *payload, char *buffer) {
  (void)payload;
  (void)buffer;
  return 0;
}
void tree_sitter_silver_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  (void)payload;
  (void)buffer;
  (void)length;
}

bool tree_sitter_silver_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  (void)payload;
  if (!valid_symbols[BLOCK_COMMENT]) return false;
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t' || lexer->lookahead == '\n' ||
         lexer->lookahead == '\r') {
    lexer->advance(lexer, true);
  }
  if (lexer->lookahead != '/') return false;
  lexer->advance(lexer, false);
  if (lexer->lookahead != '*') return false;
  lexer->advance(lexer, false);

  unsigned depth = 1;
  while (depth > 0) {
    if (lexer->eof(lexer)) return false;
    int32_t current = lexer->lookahead;
    lexer->advance(lexer, false);
    if (current == '/' && lexer->lookahead == '*') {
      lexer->advance(lexer, false);
      depth++;
    } else if (current == '*' && lexer->lookahead == '/') {
      lexer->advance(lexer, false);
      depth--;
    }
  }
  lexer->result_symbol = BLOCK_COMMENT;
  return true;
}
