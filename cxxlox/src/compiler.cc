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
  void literal();
  void grouping();
  void number();
  void string();
  void variable();
  void unary();

private:
  Chunk *current_chunk() { return compiling_chunk; }

  void error_at(Token const &token, std::string_view message);
  void error(std::string_view message);
  void error_at_current(std::string_view message);

  void advance();
  void consume(TokenType type, std::string_view message);
  bool check(TokenType type);
  bool match(TokenType type);

  void emit_byte(std::uint8_t byte);
  void emit_byte(OpCode op_code);
  void emit_bytes(OpCode op_code, std::uint8_t byte);
  void emit_bytes(OpCode op_code_a, OpCode op_code_b);
  void emit_return();
  std::uint8_t make_constant(Value value);
  void emit_constant(Value value);

  void end_compiler();

  void parse_precedence(Precedence precedence);
  Precedence next_higher_precedence(Precedence precedence);

  std::uint8_t identifier_constant(Token const &name);
  std::uint8_t parse_variable(std::string_view error_message);
  void define_variable(std::uint8_t global);

  void expression();
  void declaration();
  void var_declaration();
  void statement();
  void expression_statement();
  void print_statement();

  void named_variable(Token const &name);

  void synchronize();

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

bool LoxCompiler::check(TokenType type) { return parser.current.type == type; }

bool LoxCompiler::match(TokenType type) {
  if (not check(type)) {
    return false;
  }
  advance();
  return true;
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

void LoxCompiler::emit_bytes(OpCode op_code_a, OpCode op_code_b) {
  emit_byte(op_code_a);
  emit_byte(op_code_b);
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
  case TokenType::BANG_EQUAL:
    emit_bytes(OpCode::EQUAL, OpCode::NOT);
    break;
  case TokenType::EQUAL_EQUAL:
    emit_byte(OpCode::EQUAL);
    break;
  case TokenType::GREATER:
    emit_byte(OpCode::GREATER);
    break;
  case TokenType::GREATER_EQUAL:
    emit_bytes(OpCode::LESS, OpCode::NOT);
    break;
  case TokenType::LESS:
    emit_byte(OpCode::LESS);
    break;
  case TokenType::LESS_EQUAL:
    emit_bytes(OpCode::GREATER, OpCode::NOT);
    break;
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

void LoxCompiler::literal() {
  switch (parser.previous.type) {
  case TokenType::FALSE:
    emit_byte(OpCode::FALSE);
    break;
  case TokenType::NIL:
    emit_byte(OpCode::NIL);
    break;
  case TokenType::TRUE:
    emit_byte(OpCode::TRUE);
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
  emit_constant(number_value(value));
}

void LoxCompiler::string() {
  emit_constant(string_value(
      std::string{parser.previous.start + 1, parser.previous.length - 2}));
}

void LoxCompiler::variable() { named_variable(parser.previous); }

void LoxCompiler::unary() {
  auto const operator_type = parser.previous.type;

  // Compile the operand.
  parse_precedence(Precedence::UNARY);

  // Emit the operator instruction.
  switch (operator_type) {
  case TokenType::BANG:
    emit_byte(OpCode::NOT);
    break;
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
  {TokenType::BANG,          {&L::unary,    nullptr,      Precedence::NONE}},
  {TokenType::BANG_EQUAL,    {nullptr,      &L::binary,   Precedence::EQUALITY}},
  {TokenType::EQUAL,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::EQUAL_EQUAL,   {nullptr,      &L::binary,   Precedence::EQUALITY}},
  {TokenType::GREATER,       {nullptr,      &L::binary,   Precedence::COMPARISON}},
  {TokenType::GREATER_EQUAL, {nullptr,      &L::binary,   Precedence::COMPARISON}},
  {TokenType::LESS,          {nullptr,      &L::binary,   Precedence::COMPARISON}},
  {TokenType::LESS_EQUAL,    {nullptr,      &L::binary,   Precedence::COMPARISON}},
  {TokenType::IDENTIFIER,    {&L::variable, nullptr,      Precedence::NONE}},
  {TokenType::STRING,        {&L::string,   nullptr,      Precedence::NONE}},
  {TokenType::NUMBER,        {&L::number,   nullptr,      Precedence::NONE}},
  {TokenType::AND,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::CLASS,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::ELSE,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::FALSE,         {&L::literal,  nullptr,      Precedence::NONE}},
  {TokenType::FOR,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::FUN,           {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::IF,            {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::NIL,           {&L::literal,  nullptr,      Precedence::NONE}},
  {TokenType::OR,            {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::PRINT,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::RETURN,        {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::SUPER,         {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::THIS,          {nullptr,      nullptr,      Precedence::NONE}},
  {TokenType::TRUE,          {&L::literal,  nullptr,      Precedence::NONE}},
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

std::uint8_t LoxCompiler::identifier_constant(Token const &name) {
  return make_constant(string_value({name.start, name.length}));
}

std::uint8_t LoxCompiler::parse_variable(std::string_view error_message) {
  consume(TokenType::IDENTIFIER, error_message);
  return identifier_constant(parser.previous);
}

void LoxCompiler::define_variable(std::uint8_t global) {
  emit_bytes(OpCode::DEFINE_GLOBAL, global);
}

void LoxCompiler::expression() { parse_precedence(Precedence::ASSIGNMENT); }

void LoxCompiler::expression_statement() {
  expression();
  consume(TokenType::SEMICOLON, "Expect ';' after expression.");
  emit_byte(OpCode::POP);
}

void LoxCompiler::print_statement() {
  expression();
  consume(TokenType::SEMICOLON, "Expect ';' after value.");
  emit_byte(OpCode::PRINT);
}

void LoxCompiler::declaration() {
  if (match(TokenType::VAR)) {
    var_declaration();
  } else {
    statement();
  }

  if (parser.panic_mode) {
    synchronize();
  }
}

void LoxCompiler::var_declaration() {
  auto const global = parse_variable("Expect variable name.");

  if (match(TokenType::EQUAL)) {
    expression();
  } else {
    emit_byte(OpCode::NIL);
  }

  consume(TokenType::SEMICOLON, "Expect ';' after variable declaration.");

  define_variable(global);
}

void LoxCompiler::statement() {
  if (match(TokenType::PRINT)) {
    print_statement();
  } else {
    expression_statement();
  }
}

void LoxCompiler::named_variable(Token const &name) {
  auto const arg = identifier_constant(name);
  emit_bytes(OpCode::GET_GLOBAL, arg);
}

void LoxCompiler::synchronize() {
  parser.panic_mode = false;

  while (parser.current.type != TokenType::END) {
    if (parser.previous.type == TokenType::SEMICOLON) {
      return;
    }

    switch (parser.current.type) {
    case TokenType::CLASS:
    case TokenType::FUN:
    case TokenType::VAR:
    case TokenType::FOR:
    case TokenType::IF:
    case TokenType::WHILE:
    case TokenType::PRINT:
    case TokenType::RETURN:
      return;

    default:
      break;
    }

    advance();
  }
}

bool LoxCompiler::compile(std::string_view source, Chunk &chunk) {
  scanner->init_scanner(source);
  compiling_chunk = &chunk;

  parser.had_error = false;
  parser.panic_mode = false;

  advance();

  while (not match(TokenType::END)) {
    declaration();
  }

  end_compiler();

  return not parser.had_error;
}

} // namespace

std::unique_ptr<Compiler> Compiler::create() {
  return std::make_unique<LoxCompiler>(Scanner::create());
}
