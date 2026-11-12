// External scanner for Silver: nested block comments, `/* a /* b */ c */`,
// and the contextual keywords `shared` and `lock`, which are also ordinary
// names. `shared` is a keyword before a type (`shared i64 total`) and `lock`
// before a place and a block (`lock total {`, `lock self.hits as hits {`),
// as in the compiler's sync_sugar rewrite.
#include "tree_sitter/parser.h"

#include <stdbool.h>

enum TokenType { BLOCK_COMMENT, SHARED_KEYWORD, LOCK_KEYWORD };

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

static bool is_space(int32_t c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r'; }
static bool is_word_start(int32_t c) { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'; }
static bool is_word(int32_t c) { return is_word_start(c) || (c >= '0' && c <= '9'); }

static bool word_is(const char *word, const char *expected) {
  while (*word && *word == *expected) {
    word++;
    expected++;
  }
  return *word == *expected;
}

static void skip_space(TSLexer *lexer) {
  while (is_space(lexer->lookahead)) lexer->advance(lexer, false);
}

// Consumes the identifier at the lookahead into `word`. A word that does
// not fit is stored as "" so it matches no keyword.
static void read_word(TSLexer *lexer, char *word, unsigned size) {
  unsigned length = 0;
  while (is_word(lexer->lookahead)) {
    if (length + 1 < size) word[length] = (char)lexer->lookahead;
    length++;
    lexer->advance(lexer, false);
  }
  word[length + 1 < size ? length : 0] = '\0';
}

static bool scan_block_comment(TSLexer *lexer) {
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

// After `shared`: a type name follows, not an operator or `as`/`in`.
static bool type_follows(TSLexer *lexer) {
  skip_space(lexer);
  if (!is_word_start(lexer->lookahead)) return false;
  char word[3];
  read_word(lexer, word, sizeof word);
  return !word_is(word, "as") && !word_is(word, "in");
}

// After `lock`: a place (`total`, `self.hits`), an optional `as name`, then `{`.
static bool place_and_block_follow(TSLexer *lexer) {
  skip_space(lexer);
  if (!is_word_start(lexer->lookahead)) return false;
  while (!lexer->eof(lexer)) {
    int32_t c = lexer->lookahead;
    if (c == '{') return true;
    if (!is_word(c) && !is_space(c) && c != '.') return false;
    lexer->advance(lexer, false);
  }
  return false;
}

bool tree_sitter_silver_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  (void)payload;
  while (is_space(lexer->lookahead)) lexer->advance(lexer, true);
  if (lexer->lookahead == '/') return valid_symbols[BLOCK_COMMENT] && scan_block_comment(lexer);
  if (!valid_symbols[SHARED_KEYWORD] && !valid_symbols[LOCK_KEYWORD]) return false;
  if (lexer->lookahead != 's' && lexer->lookahead != 'l') return false;

  char word[8];
  read_word(lexer, word, sizeof word);
  lexer->mark_end(lexer);
  if (valid_symbols[SHARED_KEYWORD] && word_is(word, "shared") && type_follows(lexer)) {
    lexer->result_symbol = SHARED_KEYWORD;
    return true;
  }
  if (valid_symbols[LOCK_KEYWORD] && word_is(word, "lock") && place_and_block_follow(lexer)) {
    lexer->result_symbol = LOCK_KEYWORD;
    return true;
  }
  return false;
}
