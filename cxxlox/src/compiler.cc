#include "compiler.h"

#include "scanner.h"

#include <format>
#include <iostream>

namespace {

struct Parser {
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode;
};

Parser parser;

void error_at(Token const &token, std::string_view message) {
  if (parser.panic_mode) {
    return;
  }
  parser.panic_mode = true;

  std::cerr << std::format("[line {:d}] Error", token.line);

  if (token.type == TokenType::END) {
    std::cerr << " at end";
  } else if (token.type == TokenType::ERROR) {
    // Nothing
  } else {
    std::cerr << std::format(
        " at '{:s}'",
        std::string_view{token.start, token.start + token.length});
  }

  std::cerr << ": " << message << "\n";
  parser.had_error = true;
}

// void error(std::string_view message) { error_at(parser.previous, message); }

void error_at_current(std::string_view message) {
  error_at(parser.current, message);
}

void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scan_token();
    if (parser.current.type != TokenType::ERROR) {
      break;
    }

    error_at_current(parser.current.start);
  }
}

void consume(TokenType type, std::string_view message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  error_at_current(message);
}

void expression() {}

} // namespace

bool compile(std::string_view source, Chunk &) {
  init_scanner(source);

  parser.had_error = false;
  parser.panic_mode = false;

  advance();
  expression();
  consume(TokenType::END, "Expect end of expression.");
  return not parser.had_error;
}
