#include "compiler.h"

#include "chunk.h"
#include "debug.h"
#include "object.h"
#include "scanner.h"

#include <algorithm>
#include <format>
#include <functional>
#include <iostream>
#include <limits>
#include <map>
#include <optional>
#include <ranges>

namespace views = std::ranges::views;

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
  LoxCompiler(ObjList &objects_, std::unique_ptr<Scanner> &&scanner_,
              std::ostream &err_)
      : objects{objects_}, scanner{std::move(scanner_)}, err{err_} {}

  ObjHandle compile(std::string_view source) override;

  void binary(bool can_assign);
  void call(bool can_assign);
  void dot(bool can_assign);
  void literal(bool can_assign);
  void grouping(bool can_assign);
  void number(bool can_assign);
  void string(bool can_assign);
  void variable(bool can_assign);
  void unary(bool can_assign);
  void logical_and(bool can_assign);
  void logical_or(bool can_assign);

private:
  Chunk &current_chunk();
  Function &current_function();

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
  void emit_loop(std::size_t loop_start);
  std::size_t emit_jump(OpCode instruction);
  void emit_return();
  std::uint8_t make_constant(Value value);
  void emit_constant(Value value);
  void patch_jump(std::size_t offset);

  struct Context;
  enum class FunctionType;

  void init_compiler(Context *compiler, FunctionType type);
  ObjHandle end_compiler();

  void begin_scope();
  void end_scope();

  void parse_precedence(Precedence precedence);
  Precedence next_higher_precedence(Precedence precedence);

  std::uint8_t identifier_constant(Token const &name);
  bool identifier_equals(Token const &a, Token const &b);
  std::optional<std::uint8_t> resolve_local(Context const &compiler,
                                            Token const &name);
  std::uint8_t add_upvalue(Context &compiler, std::uint8_t index,
                           bool is_local);
  std::optional<std::uint8_t> resolve_upvalue(Context &compiler,
                                              Token const &name);
  void add_local(Token const &name);
  void declare_variable();
  std::uint8_t parse_variable(std::string_view error_message);
  void mark_initialized();
  void define_variable(std::uint8_t global);
  std::uint8_t argument_list();

  void expression();
  void block();
  void function(FunctionType type);
  void class_declaration();
  void declaration();
  void fun_declaration();
  void var_declaration();
  void statement();
  void expression_statement();
  void for_statement();
  void if_statement();
  void print_statement();
  void return_statement();
  void while_statement();

  void named_variable(Token const &name, bool can_assign);

  void synchronize();

private:
  struct Parser {
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
  };

  struct Local {
    Token name;
    std::optional<std::size_t> depth;
    bool is_captured;
  };

  struct Upvalue {
    std::uint8_t index;
    bool is_local;
  };

  enum class FunctionType {
    FUNCTION,
    SCRIPT,
  };

  constexpr static std::size_t const UINT8_COUNT =
      std::numeric_limits<std::uint8_t>::max() + 1;

  struct Context {
    Context *enclosing;
    ObjHandle function;
    FunctionType type;

    std::array<Local, UINT8_COUNT> locals;
    std::size_t local_count;
    std::array<Upvalue, UINT8_COUNT> upvalues;
    std::size_t scope_depth;
  };

  Parser parser{};
  Context *current{nullptr};

  ObjList &objects;
  std::unique_ptr<Scanner> scanner{};
  std::ostream &err;
};

using ParseFn = void (LoxCompiler::*)(bool);

struct ParseRule {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
};

Chunk &LoxCompiler::current_chunk() { return current_function().chunk; }

Function &LoxCompiler::current_function() {
  return std::get<Function>(current->function.lock()->data);
}

void LoxCompiler::error_at(Token const &token, std::string_view message) {
  if (parser.panic_mode) {
    return;
  }
  parser.panic_mode = true;

  err << std::format("[line {:d}] Error", token.line);

  if (token.type == TokenType::END) {
    err << " at end";
  } else if (token.type == TokenType::ERROR) {
    // Nothing
  } else {
    err << std::format(
        " at '{:s}'",
        std::string_view{token.start, token.start + token.length});
  }

  err << ": " << message << "\n";
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
  write_chunk(current_chunk(), byte, parser.previous.line);
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

void LoxCompiler::emit_loop(std::size_t loop_start) {
  emit_byte(OpCode::LOOP);

  std::size_t const offset = current_chunk().code.size() - loop_start + 2;
  if (offset > std::numeric_limits<std::uint16_t>::max()) {
    error("Loop body too large.");
  }

  emit_byte((offset >> 8) & 0xff);
  emit_byte(offset & 0xff);
}

std::size_t LoxCompiler::emit_jump(OpCode instruction) {
  emit_byte(instruction);
  emit_byte(0xff);
  emit_byte(0xff);
  return current_chunk().code.size() - 2;
}

void LoxCompiler::emit_return() {
  emit_byte(OpCode::NIL);
  emit_byte(OpCode::RETURN);
}

uint8_t LoxCompiler::make_constant(Value value) {
  auto const constant = add_constant(current_chunk(), value);
  if (constant > std::numeric_limits<std::uint8_t>::max()) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return static_cast<uint8_t>(constant);
}

void LoxCompiler::emit_constant(Value value) {
  emit_bytes(OpCode::CONSTANT, make_constant(value));
}

void LoxCompiler::patch_jump(std::size_t offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  std::size_t const jump = current_chunk().code.size() - offset - 2;

  if (jump > std::numeric_limits<std::uint16_t>::max()) {
    error("Too much code to jump over.");
  }

  current_chunk().code.at(offset) = (jump >> 8) & 0xff;
  current_chunk().code.at(offset + 1) = jump & 0xff;
}

void LoxCompiler::init_compiler(Context *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->type = type;
  compiler->local_count = 0;
  compiler->scope_depth = 0;
  compiler->function = new_function(objects);
  current = compiler;

  if (type != FunctionType::SCRIPT) {
    current_function().name =
        std::string{parser.previous.start, parser.previous.length};
  }

  Local &local = current->locals.at(current->local_count++);
  local.depth = 0;
  local.is_captured = false;
  local.name.start = "";
  local.name.length = 0;
}

ObjHandle LoxCompiler::end_compiler() {
  emit_return();
  ObjHandle function = current->function;

  if (Debug::PRINT_CODE) {
    if (not parser.had_error) {
      auto const &name = current_function().name;
      disassemble_chunk(current_chunk(), name.empty() ? "<script>" : name);
    }
  }

  current = current->enclosing;
  return function;
}

void LoxCompiler::begin_scope() { current->scope_depth++; }

void LoxCompiler::end_scope() {
  current->scope_depth--;

  while (current->local_count > 0 &&
         current->locals.at(current->local_count - 1)
             .depth
             .and_then([this](auto depth) {
               return std::optional{depth > current->scope_depth};
             })
             .value_or(false)) {
    if (current->locals.at(current->local_count - 1).is_captured) {
      emit_byte(OpCode::CLOSE_UPVALUE);
    } else {
      emit_byte(OpCode::POP);
    }
    current->local_count--;
  }
}

Precedence LoxCompiler::next_higher_precedence(Precedence precedence) {
  return static_cast<Precedence>(static_cast<std::int32_t>(precedence) + 1);
}

ParseRule const &get_rule(TokenType type);

void LoxCompiler::binary(bool) {
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

void LoxCompiler::call(bool) {
  auto const arg_count = argument_list();
  emit_bytes(OpCode::CALL, arg_count);
}

void LoxCompiler::literal(bool) {
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

void LoxCompiler::dot(bool can_assign) {
  consume(TokenType::IDENTIFIER, "Expect property name after '.'.");
  auto const name = identifier_constant(parser.previous);

  if (can_assign && match(TokenType::EQUAL)) {
    expression();
    emit_bytes(OpCode::SET_PROPERTY, name);
  } else {
    emit_bytes(OpCode::GET_PROPERTY, name);
  }
}

void LoxCompiler::grouping(bool) {
  expression();
  consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

void LoxCompiler::number(bool) {
  auto const value = std::stod(
      std::string{parser.previous.start, parser.previous.length}, nullptr);
  emit_constant(number_value(value));
}

void LoxCompiler::string(bool) {
  emit_constant(string_value(
      std::string{parser.previous.start + 1, parser.previous.length - 2}));
}

void LoxCompiler::variable(bool can_assign) {
  named_variable(parser.previous, can_assign);
}

void LoxCompiler::unary(bool) {
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

void LoxCompiler::logical_and(bool) {
  auto const end_jump = emit_jump(OpCode::JUMP_IF_FALSE);

  emit_byte(OpCode::POP);
  parse_precedence(Precedence::AND);

  patch_jump(end_jump);
}

void LoxCompiler::logical_or(bool) {
  auto const else_jump = emit_jump(OpCode::JUMP_IF_FALSE);
  auto const end_jump = emit_jump(OpCode::JUMP);

  patch_jump(else_jump);
  emit_byte(OpCode::POP);

  parse_precedence(Precedence::OR);
  patch_jump(end_jump);
}

using L = LoxCompiler;

// clang-format off
std::map<TokenType, ParseRule> const rules{
  {TokenType::LEFT_PAREN,    {&L::grouping, &L::call,         Precedence::CALL}},
  {TokenType::RIGHT_PAREN,   {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::LEFT_BRACE,    {nullptr,      nullptr,          Precedence::NONE}}, 
  {TokenType::RIGHT_BRACE,   {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::COMMA,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::DOT,           {nullptr,      &L::dot,          Precedence::CALL}},
  {TokenType::MINUS,         {&L::unary,    &L::binary,       Precedence::TERM}},
  {TokenType::PLUS,          {nullptr,      &L::binary,       Precedence::TERM}},
  {TokenType::SEMICOLON,     {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::SLASH,         {nullptr,      &L::binary,       Precedence::FACTOR}},
  {TokenType::STAR,          {nullptr,      &L::binary,       Precedence::FACTOR}},
  {TokenType::BANG,          {&L::unary,    nullptr,          Precedence::NONE}},
  {TokenType::BANG_EQUAL,    {nullptr,      &L::binary,       Precedence::EQUALITY}},
  {TokenType::EQUAL,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::EQUAL_EQUAL,   {nullptr,      &L::binary,       Precedence::EQUALITY}},
  {TokenType::GREATER,       {nullptr,      &L::binary,       Precedence::COMPARISON}},
  {TokenType::GREATER_EQUAL, {nullptr,      &L::binary,       Precedence::COMPARISON}},
  {TokenType::LESS,          {nullptr,      &L::binary,       Precedence::COMPARISON}},
  {TokenType::LESS_EQUAL,    {nullptr,      &L::binary,       Precedence::COMPARISON}},
  {TokenType::IDENTIFIER,    {&L::variable, nullptr,          Precedence::NONE}},
  {TokenType::STRING,        {&L::string,   nullptr,          Precedence::NONE}},
  {TokenType::NUMBER,        {&L::number,   nullptr,          Precedence::NONE}},
  {TokenType::AND,           {nullptr,      &L::logical_and,  Precedence::AND}},
  {TokenType::CLASS,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::ELSE,          {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::FALSE,         {&L::literal,  nullptr,          Precedence::NONE}},
  {TokenType::FOR,           {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::FUN,           {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::IF,            {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::NIL,           {&L::literal,  nullptr,          Precedence::NONE}},
  {TokenType::OR,            {nullptr,      &L::logical_or,   Precedence::OR}},
  {TokenType::PRINT,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::RETURN,        {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::SUPER,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::THIS,          {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::TRUE,          {&L::literal,  nullptr,          Precedence::NONE}},
  {TokenType::VAR,           {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::WHILE,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::ERROR,         {nullptr,      nullptr,          Precedence::NONE}},
  {TokenType::END,           {nullptr,      nullptr,          Precedence::NONE}}
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

  bool can_assign = precedence <= Precedence::ASSIGNMENT;
  std::invoke(prefix_rule, *this, can_assign);

  while (precedence <= get_rule(parser.current.type).precedence) {
    advance();
    auto const infix_rule = get_rule(parser.previous.type).infix;
    std::invoke(infix_rule, *this, can_assign);
  }

  if (can_assign && match(TokenType::EQUAL)) {
    error("Invalid assignment target.");
  }
}

std::uint8_t LoxCompiler::identifier_constant(Token const &name) {
  return make_constant(string_value({name.start, name.length}));
}

bool LoxCompiler::identifier_equals(Token const &a, Token const &b) {
  if (a.length != b.length) {
    return false;
  }
  return std::ranges::equal(a.start, a.start + a.length, b.start,
                            b.start + b.length);
}

std::optional<std::uint8_t> LoxCompiler::resolve_local(Context const &compiler,
                                                       Token const &name) {
  for (std::int32_t i = compiler.local_count - 1; i >= 0; --i) {
    auto const &local = compiler.locals.at(i);
    if (identifier_equals(name, local.name)) {
      if (not local.depth.has_value()) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }

  return {};
}

std::uint8_t LoxCompiler::add_upvalue(Context &compiler, std::uint8_t index,
                                      bool is_local) {
  auto &function = std::get<Function>(compiler.function.lock()->data);
  auto const upvalue_count = function.upvalue_count;

  for (std::size_t i = 0; i < upvalue_count; ++i) {
    auto const &upvalue = compiler.upvalues.at(i);
    if (upvalue.index == index && upvalue.is_local == is_local) {
      return i;
    }
  }

  if (upvalue_count == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return {};
  }

  compiler.upvalues.at(upvalue_count).is_local = is_local;
  compiler.upvalues.at(upvalue_count).index = index;
  return function.upvalue_count++;
}

std::optional<std::uint8_t> LoxCompiler::resolve_upvalue(Context &compiler,
                                                         Token const &name) {
  if (compiler.enclosing == nullptr) {
    return {};
  }

  auto const maybe_local = resolve_local(*compiler.enclosing, name);
  if (maybe_local.has_value()) {
    compiler.enclosing->locals.at(maybe_local.value()).is_captured = true;
    return add_upvalue(compiler, maybe_local.value(), true);
  }

  auto const maybe_upvalue = resolve_upvalue(*compiler.enclosing, name);
  if (maybe_upvalue.has_value()) {
    return add_upvalue(compiler, maybe_upvalue.value(), false);
  }

  return {};
}

void LoxCompiler::add_local(Token const &name) {
  if (current->local_count == current->locals.max_size()) {
    error("Too many local variables in function.");
    return;
  }

  auto const local = &current->locals.at(current->local_count);
  ++current->local_count;
  local->name = name;
  local->depth = std::nullopt;
  local->is_captured = false;
}

void LoxCompiler::declare_variable() {
  if (current->scope_depth == 0) {
    return;
  }

  auto const &name = parser.previous;

  auto last = current->locals.rbegin() +
              (current->locals.max_size() - current->local_count);
  for (auto local = last; local != current->locals.rend(); ++local) {
    if (local->depth.has_value() &&
        local->depth.value() < current->scope_depth) {
      break;
    }

    if (identifier_equals(name, local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  add_local(name);
}

std::uint8_t LoxCompiler::parse_variable(std::string_view error_message) {
  consume(TokenType::IDENTIFIER, error_message);

  declare_variable();
  if (current->scope_depth > 0) {
    return 0;
  }

  return identifier_constant(parser.previous);
}

void LoxCompiler::mark_initialized() {
  if (current->scope_depth == 0) {
    return;
  }
  current->locals.at(current->local_count - 1).depth = current->scope_depth;
}

void LoxCompiler::define_variable(std::uint8_t global) {
  if (current->scope_depth > 0) {
    mark_initialized();
    return;
  }

  emit_bytes(OpCode::DEFINE_GLOBAL, global);
}

std::uint8_t LoxCompiler::argument_list() {
  uint8_t arg_count = 0;
  if (not check(TokenType::RIGHT_PAREN)) {
    do {
      expression();
      if (arg_count == 255) {
        error("Can't have more than 255 arguments.");
      }
      arg_count++;
    } while (match(TokenType::COMMA));
  }
  consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments.");
  return arg_count;
}

void LoxCompiler::expression() { parse_precedence(Precedence::ASSIGNMENT); }

void LoxCompiler::block() {
  while (not check(TokenType::RIGHT_BRACE) && not check(TokenType::END)) {
    declaration();
  }

  consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
}

void LoxCompiler::function(FunctionType type) {
  Context compiler{};
  init_compiler(&compiler, type);
  begin_scope();

  consume(TokenType::LEFT_PAREN, "Expect '(' after function name.");
  if (not check(TokenType::RIGHT_PAREN)) {
    do {
      current_function().arity++;
      if (current_function().arity > 255) {
        error_at_current("Can't have more than 255 parameters.");
      }
      auto const constant = parse_variable("Expect parameter name.");
      define_variable(constant);
    } while (match(TokenType::COMMA));
  }
  consume(TokenType::RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TokenType::LEFT_BRACE, "Expect '{' before function body.");
  block();

  auto function = end_compiler();
  emit_bytes(OpCode::CLOSURE, make_constant(obj_value(function)));

  auto const upvalue_count =
      std::get<Function>(function.lock()->data).upvalue_count;
  for (auto const &upvalue : views::take(compiler.upvalues, upvalue_count)) {
    emit_byte(upvalue.is_local ? 1 : 0);
    emit_byte(upvalue.index);
  }
}

void LoxCompiler::expression_statement() {
  expression();
  consume(TokenType::SEMICOLON, "Expect ';' after expression.");
  emit_byte(OpCode::POP);
}

void LoxCompiler::for_statement() {
  begin_scope();
  consume(TokenType::LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TokenType::SEMICOLON)) {
    // No initializer
  } else if (match(TokenType::VAR)) {
    var_declaration();
  } else {
    expression_statement();
  }

  auto loop_start = current_chunk().code.size();
  std::optional<std::size_t> exit_jump{};
  if (not match(TokenType::SEMICOLON)) {
    expression();
    consume(TokenType::SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false
    exit_jump = emit_jump(OpCode::JUMP_IF_FALSE);
    emit_byte(OpCode::POP);
  }

  if (not match(TokenType::RIGHT_PAREN)) {
    auto const body_jump = emit_jump(OpCode::JUMP);
    auto const increment_start = current_chunk().code.size();
    expression();
    emit_byte(OpCode::POP);
    consume(TokenType::RIGHT_PAREN, "Expect ')' after for clauses.");

    emit_loop(loop_start);
    loop_start = increment_start;
    patch_jump(body_jump);
  }

  statement();
  emit_loop(loop_start);

  if (exit_jump.has_value()) {
    patch_jump(exit_jump.value());
    emit_byte(OpCode::POP);
  }

  end_scope();
}

void LoxCompiler::if_statement() {
  consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

  auto const then_jump = emit_jump(OpCode::JUMP_IF_FALSE);
  emit_byte(OpCode::POP);
  statement();

  auto const else_jump = emit_jump(OpCode::JUMP);

  patch_jump(then_jump);
  emit_byte(OpCode::POP);

  if (match(TokenType::ELSE)) {
    statement();
  }
  patch_jump(else_jump);
}

void LoxCompiler::print_statement() {
  expression();
  consume(TokenType::SEMICOLON, "Expect ';' after value.");
  emit_byte(OpCode::PRINT);
}

void LoxCompiler::return_statement() {
  if (current->type == FunctionType::SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TokenType::SEMICOLON)) {
    emit_return();
  } else {
    expression();
    consume(TokenType ::SEMICOLON, "Expect ';' after return value.");
    emit_byte(OpCode::RETURN);
  }
}

void LoxCompiler::while_statement() {
  auto const loop_start = current_chunk().code.size();
  consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

  auto const exit_jump = emit_jump(OpCode::JUMP_IF_FALSE);
  emit_byte(OpCode::POP);
  statement();
  emit_loop(loop_start);

  patch_jump(exit_jump);
  emit_byte(OpCode::POP);
}

void LoxCompiler::class_declaration() {
  consume(TokenType::IDENTIFIER, "Expect class name.");
  std::uint8_t name_constant = identifier_constant(parser.previous);
  declare_variable();

  emit_bytes(OpCode::CLASS, name_constant);
  define_variable(name_constant);

  consume(TokenType::LEFT_BRACE, "Expect '{' before class body.");
  consume(TokenType::RIGHT_BRACE, "Expect '}' after class body.");
}

void LoxCompiler::declaration() {
  if (match(TokenType::CLASS)) {
    class_declaration();
  } else if (match(TokenType::FUN)) {
    fun_declaration();
  } else if (match(TokenType::VAR)) {
    var_declaration();
  } else {
    statement();
  }

  if (parser.panic_mode) {
    synchronize();
  }
}

void LoxCompiler::fun_declaration() {
  auto const global = parse_variable("Expect function name.");
  mark_initialized();
  function(FunctionType::FUNCTION);
  define_variable(global);
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
  } else if (match(TokenType::FOR)) {
    for_statement();
  } else if (match(TokenType::IF)) {
    if_statement();
  } else if (match(TokenType::RETURN)) {
    return_statement();
  } else if (match(TokenType::WHILE)) {
    while_statement();
  } else if (match(TokenType::LEFT_BRACE)) {
    begin_scope();
    block();
    end_scope();
  } else {
    expression_statement();
  }
}

void LoxCompiler::named_variable(Token const &name, bool can_assign) {
  OpCode get_op{};
  OpCode set_op{};
  uint8_t arg{};

  if (auto maybe_arg = resolve_local(*current, name); maybe_arg.has_value()) {
    arg = maybe_arg.value();
    get_op = OpCode::GET_LOCAL;
    set_op = OpCode::SET_LOCAL;
  } else if (auto maybe_arg = resolve_upvalue(*current, name);
             maybe_arg.has_value()) {
    arg = maybe_arg.value();
    get_op = OpCode::GET_UPVALUE;
    set_op = OpCode::SET_UPVALUE;
  } else {
    arg = identifier_constant(name);
    get_op = OpCode::GET_GLOBAL;
    set_op = OpCode::SET_GLOBAL;
  }

  if (can_assign && match(TokenType::EQUAL)) {
    expression();
    emit_bytes(set_op, arg);
  } else {
    emit_bytes(get_op, arg);
  }
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

ObjHandle LoxCompiler::compile(std::string_view source) {
  scanner->init_scanner(source);
  Context compiler{};
  init_compiler(&compiler, FunctionType::SCRIPT);

  parser.had_error = false;
  parser.panic_mode = false;

  advance();

  while (not match(TokenType::END)) {
    declaration();
  }

  auto function = end_compiler();
  return parser.had_error ? ObjHandle{} : function;
}

} // namespace

std::unique_ptr<Compiler> Compiler::create(ObjList &objects,
                                           std::ostream &err) {
  return std::make_unique<LoxCompiler>(objects, Scanner::create(), err);
}
