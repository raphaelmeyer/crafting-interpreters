#include "compiler.h"

#include "scanner.h"

#include <stdio.h>

typedef struct Parser_t {
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode;
} Parser;

static Parser parser;
static Chunk *compiling_chunk;

static Chunk *current_chunk() { return compiling_chunk; }

static void error_at(Token const *token, char const *message) {
  if (parser.panic_mode) {
    return;
  }
  parser.panic_mode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.had_error = true;
}

// static void error(char const *message) { error_at(&parser.previous, message);
// }

static void error_at_current(char const *message) {
  error_at(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scan_token();
    if (parser.current.type != TOKEN_ERROR) {
      break;
    }

    error_at_current(parser.current.start);
  }
}

static void consume(TokenType type, char const *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  error_at_current(message);
}

static void emit_byte(uint8_t byte) {
  write_chunk(current_chunk(), byte, parser.previous.line);
}

// static void emit_bytes(uint8_t byte1, uint8_t byte2) {
//   emit_byte(byte1);
//   emit_byte(byte2);
// }

static void emit_return() { emit_byte(OP_RETURN); }

static void end_compiler() { emit_return(); }

static void expression() {}

bool compile(char const *source, Chunk *chunk) {
  init_scanner(source);
  compiling_chunk = chunk;

  parser.had_error = false;
  parser.panic_mode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");
  end_compiler();

  return !parser.had_error;
}
