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

// clang-format off
std::map<TokenType, ParseRule> const rules{
  {TokenType::LEFT_PAREN,    {grouping, nullptr,  Precedence::NONE}},
  {TokenType::RIGHT_PAREN,   {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::LEFT_BRACE,    {nullptr,  nullptr,  Precedence::NONE}}, 
  {TokenType::RIGHT_BRACE,   {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::COMMA,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::DOT,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::MINUS,         {unary,    binary,   Precedence::TERM}},
  {TokenType::PLUS,          {nullptr,  binary,   Precedence::TERM}},
  {TokenType::SEMICOLON,     {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::SLASH,         {nullptr,  binary,   Precedence::FACTOR}},
  {TokenType::STAR,          {nullptr,  binary,   Precedence::FACTOR}},
  {TokenType::BANG,          {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::BANG_EQUAL,    {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::EQUAL,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::EQUAL_EQUAL,   {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::GREATER,       {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::GREATER_EQUAL, {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::LESS,          {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::LESS_EQUAL,    {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::IDENTIFIER,    {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::STRING,        {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::NUMBER,        {number,   nullptr,  Precedence::NONE}},
  {TokenType::AND,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::CLASS,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::ELSE,          {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::FALSE,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::FOR,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::FUN,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::IF,            {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::NIL,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::OR,            {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::PRINT,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::RETURN,        {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::SUPER,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::THIS,          {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::TRUE,          {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::VAR,           {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::WHILE,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::ERROR,         {nullptr,  nullptr,  Precedence::NONE}},
  {TokenType::END,           {nullptr,  nullptr,  Precedence::NONE}}
};
// clang-format on

void parse_precedence(Precedence precedence) {
  advance();
  auto const prefix_rule = get_rule(parser.previous.type).prefix;
  if (not prefix_rule) {
    error("Expect expression.");
    return;
  }

  prefix_rule();

  while (precedence <= get_rule(parser.current.type).precedence) {
    advance();
    auto const infix_rule = get_rule(parser.previous.type).infix;
    infix_rule();
  }
}

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
