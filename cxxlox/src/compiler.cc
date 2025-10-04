#include "compiler.h"

#include "chunk.h"
#include "debug.h"
#include "scanner.h"

#include <format>
#include <functional>
#include <iostream>
#include <limits>
#include <map>

namespace {

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

class LoxCompiler final : public Compiler {
public:
  LoxCompiler(std::unique_ptr<Scanner> &&scanner_)
      : scanner{std::move(scanner_)} {}

  bool compile(std::string_view source, Chunk &chunk) override;

  void binary();
  void grouping();
  void number();
  void unary();

private:
  Chunk *current_chunk() { return compiling_chunk; }

  void error_at(Token const &token, std::string_view message);
  void error(std::string_view message);
  void error_at_current(std::string_view message);

  void advance();
  void consume(TokenType type, std::string_view message);

  void emit_byte(std::uint8_t byte);
  void emit_byte(OpCode op_code);
  void emit_bytes(OpCode op_code, std::uint8_t byte);
  void emit_return();
  uint8_t make_constant(Value value);
  void emit_constant(Value value);

  void end_compiler();

  void parse_precedence(Precedence precedence);
  Precedence next_higher_precedence(Precedence precedence);

  void expression();

private:
  struct Parser {
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
  };

  Parser parser{};
  Chunk *compiling_chunk{};

  std::unique_ptr<Scanner> scanner{};
};

using ParseFn = void (LoxCompiler::*)();

struct ParseRule {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
};

void LoxCompiler::error_at(Token const &token, std::string_view message) {
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

void LoxCompiler::error(std::string_view message) {
  error_at(parser.previous, message);
}

void LoxCompiler::error_at_current(std::string_view message) {
  error_at(parser.current, message);
}

void LoxCompiler::advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanner->scan_token();
    if (parser.current.type != TokenType::ERROR) {
      break;
    }

    error_at_current(parser.current.start);
  }
}

void LoxCompiler::consume(TokenType type, std::string_view message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  error_at_current(message);
}

void LoxCompiler::emit_byte(std::uint8_t byte) {
  current_chunk()->write(byte, parser.previous.line);
}

void LoxCompiler::emit_byte(OpCode op_code) {
  emit_byte(static_cast<std::uint8_t>(op_code));
}

void LoxCompiler::emit_bytes(OpCode op_code, std::uint8_t byte) {
  emit_byte(op_code);
  emit_byte(byte);
}

void LoxCompiler::emit_return() { emit_byte(OpCode::RETURN); }

uint8_t LoxCompiler::make_constant(Value value) {
  auto const constant = current_chunk()->add_constant(value);
  if (constant > std::numeric_limits<std::uint8_t>::max()) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return static_cast<uint8_t>(constant);
}

void LoxCompiler::emit_constant(Value value) {
  emit_bytes(OpCode::CONSTANT, make_constant(value));
}

void LoxCompiler::end_compiler() {
  emit_return();

  if (Debug::PRINT_CODE) {
    if (not parser.had_error) {
      disassemble_chunk(*current_chunk(), "code");
    }
  }
}

Precedence LoxCompiler::next_higher_precedence(Precedence precedence) {
  return static_cast<Precedence>(static_cast<std::int32_t>(precedence) + 1);
}

ParseRule const &get_rule(TokenType type);

void LoxCompiler::binary() {
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

void LoxCompiler::grouping() {
  expression();
  consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

void LoxCompiler::number() {
  auto const value = std::stod(
      std::string{parser.previous.start, parser.previous.length}, nullptr);
  emit_constant(value);
}

void LoxCompiler::unary() {
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

using L = LoxCompiler;

// clang-format off
std::map<TokenType, ParseRule> const rules{
  {TokenType::LEFT_PAREN,    {&L::grouping, nullptr,      Precedence::NONE}},
  {TokenType::RIGHT_PAREN,   {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::LEFT_BRACE,    {nullptr,      nullptr,      Precedence::NONE}}, 
  {TokenType::RIGHT_BRACE,   {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::COMMA,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::DOT,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::MINUS,         {&L::unary,    &L::binary,   Precedence::TERM}},
  {TokenType::PLUS,          {nullptr,      &L::binary,   Precedence::TERM}},
  {TokenType::SEMICOLON,     {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::SLASH,         {nullptr,      &L::binary,   Precedence::FACTOR}},
  {TokenType::STAR,          {nullptr,      &L::binary,   Precedence::FACTOR}},
  {TokenType::BANG,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::BANG_EQUAL,    {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::EQUAL,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::EQUAL_EQUAL,   {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::GREATER,       {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::GREATER_EQUAL, {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::LESS,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::LESS_EQUAL,    {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::IDENTIFIER,    {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::STRING,        {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::NUMBER,        {&L::number,   nullptr,      Precedence::NONE}},
  {TokenType::AND,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::CLASS,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::ELSE,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::FALSE,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::FOR,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::FUN,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::IF,            {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::NIL,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::OR,            {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::PRINT,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::RETURN,        {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::SUPER,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::THIS,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::TRUE,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::VAR,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::WHILE,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::ERROR,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::END,           {nullptr,      nullptr,      Precedence::NONE}}
};
// clang-format on

ParseRule const &get_rule(TokenType type) { return rules.at(type); }

void LoxCompiler::parse_precedence(Precedence precedence) {
  advance();
  auto const prefix_rule = get_rule(parser.previous.type).prefix;
  if (not prefix_rule) {
    error("Expect expression.");
    return;
  }

  std::invoke(prefix_rule, *this);

  while (precedence <= get_rule(parser.current.type).precedence) {
    advance();
    auto const infix_rule = get_rule(parser.previous.type).infix;
    std::invoke(infix_rule, *this);
  }
}

void LoxCompiler::expression() { parse_precedence(Precedence::ASSIGNMENT); }

bool LoxCompiler::compile(std::string_view source, Chunk &chunk) {
  scanner->init_scanner(source);
  compiling_chunk = &chunk;

  parser.had_error = false;
  parser.panic_mode = false;

  advance();
  expression();
  consume(TokenType::END, "Expect end of expression.");

  end_compiler();

  return not parser.had_error;
}

} // namespace

std::unique_ptr<Compiler> Compiler::create() {
  return std::make_unique<LoxCompiler>(Scanner::create());
}
