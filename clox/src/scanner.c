#include "scanner.h"

#include <stdint.h>
#include <string.h>

typedef struct Scanner_t {
  char const *start;
  char const *current;
  int32_t line;
} Scanner;

static Scanner scanner;

void init_scanner(const char *source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}

static bool is_at_end() { return *scanner.current == '\0'; }

static Token make_token(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = scanner.current - scanner.start;
  token.line = scanner.line;
  return token;
}

static Token error_token(char const *message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = strlen(message);
  token.line = scanner.line;
  return token;
}

Token scan_token() {
  scanner.start = scanner.current;

  if (is_at_end()) {
    return make_token(TOKEN_EOF);
  }

  return error_token("Unexpected character.");
}
