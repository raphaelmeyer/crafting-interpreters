#include "compiler.h"

#include "scanner.h"

#include <stdio.h>
#include <stdlib.h>

typedef struct Parser_t {
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode;
} Parser;

typedef enum Precedence_t {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)();

typedef struct ParseRule_t {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

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

static void error(char const *message) { error_at(&parser.previous, message); }

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

static void emit_bytes(uint8_t byte1, uint8_t byte2) {
  emit_byte(byte1);
  emit_byte(byte2);
}

static void emit_return() { emit_byte(OP_RETURN); }

static uint8_t make_constant(Value value) {
  int32_t constant = add_constant(current_chunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emit_constant(Value value) {
  emit_bytes(OP_CONSTANT, make_constant(value));
}

static void end_compiler() { emit_return(); }

static void expression();
static void parse_precedence(Precedence precedence);
static ParseRule *get_rule(TokenType type);

static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number() {
  double value = strtod(parser.previous.start, NULL);
  emit_constant(value);
}

static void unary() {
  TokenType operator_type = parser.previous.type;

  parse_precedence(PREC_UNARY);

  switch (operator_type) {
  case TOKEN_MINUS:
    emit_byte((OP_NEGATE));
    break;
  default:
    return;
  }
}

static ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, NULL, PREC_TERM},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parse_precedence(Precedence precedence) {
  (void)precedence;

  advance();
  ParseFn prefix_rule = get_rule(parser.previous.type)->prefix;
  if (prefix_rule == NULL) {
    error("Expect expression.");
    return;
  }

  prefix_rule();
}

static ParseRule *get_rule(TokenType type) { return &rules[type]; }

static void expression() { parse_precedence(PREC_ASSIGNMENT); }

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
