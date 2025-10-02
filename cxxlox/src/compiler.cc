#include "compiler.h"

#include "scanner.h"

#include <format>
#include <iostream>
#include <limits>
#include <map>

namespace {

struct Parser {
  Token current;
  Token previous;
  bool had_error;
  bool panic_mode;
};

enum class Precedence {
  NONE,
  ASSIGNMENT, // =
  OR,         // or
  AND,        // and
  EQUALITY,   // == !=
  COMPARISON, // < > <= >=
  TERM,       // + -
  FACTOR,     // * /
  UNARY,      // ! -
  CALL,       // . ()
  PRIMARY
};

using ParseFn = void (*)();

struct ParseRule {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
};

Parser parser;
Chunk *compiling_chunk;

Chunk *current_chunk() { return compiling_chunk; }

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

void error(std::string_view message) { error_at(parser.previous, message); }

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

void emit_byte(std::uint8_t byte) {
  write_chunk(*current_chunk(), byte, parser.previous.line);
}

void emit_byte(OpCode op_code) {
  emit_byte(static_cast<std::uint8_t>(op_code));
}

void emit_bytes(OpCode op_code, std::uint8_t byte) {
  emit_byte(op_code);
  emit_byte(byte);
}

void emit_return() { emit_byte(OpCode::RETURN); }

uint8_t make_constant(Value value) {
  auto const constant = add_constant(*current_chunk(), value);
  if (constant > std::numeric_limits<std::uint8_t>::max()) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return static_cast<uint8_t>(constant);
}

void emit_constant(Value value) {
  emit_bytes(OpCode::CONSTANT, make_constant(value));
}

void end_compiler() { emit_return(); }

void expression();
void parse_precedence(Precedence precedence);
ParseRule const &get_rule(TokenType type);

Precedence next_higher_precedence(Precedence precedence) {
  return static_cast<Precedence>(static_cast<std::int32_t>(precedence) + 1);
}

void binary() {
  auto const operator_type = parser.previous.type;
  auto const rule = get_rule(operator_type);
  parse_precedence(next_higher_precedence(rule.precedence));

  switch (operator_type) {
  case TokenType::PLUS:
    emit_byte(OpCode::ADD);
    break;
  case TokenType::MINUS:
    emit_byte(OpCode::SUBTRACT);
    break;
  case TokenType::STAR:
    emit_byte(OpCode::MULTIPLY);
    break;
  case TokenType::SLASH:
    emit_byte(OpCode::DIVIDE);
    break;
  default:
    return;
  }
}

void grouping() {
  expression();
  consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

void number() {
  auto const value = std::stod(
      std::string{parser.previous.start, parser.previous.length}, nullptr);
  emit_constant(value);
}

void unary() {
  auto const operator_type = parser.previous.type;

  // Compile the operand.
  parse_precedence(Precedence::UNARY);

  // Emit the operator instruction.
  switch (operator_type) {
  case TokenType::MINUS:
    emit_byte(OpCode::NEGATE);
    break;

  default:
    return;
  }
}

std::map<TokenType, ParseRule> const rules{
    {TokenType::LEFT_PAREN, {grouping, nullptr, Precedence::NONE}},
    {TokenType::MINUS, {unary, binary, Precedence::TERM}},
    {TokenType::NUMBER, {number, nullptr, Precedence::NONE}}};

void parse_precedence([[maybe_unused]] Precedence precedence) {}

ParseRule const &get_rule(TokenType type) { return rules.at(type); }

void expression() { parse_precedence(Precedence::ASSIGNMENT); }

} // namespace

bool compile(std::string_view source, Chunk &chunk) {
  init_scanner(source);
  compiling_chunk = &chunk;

  parser.had_error = false;
  parser.panic_mode = false;

  advance();
  expression();
  consume(TokenType::END, "Expect end of expression.");

  end_compiler();

  return not parser.had_error;
}
