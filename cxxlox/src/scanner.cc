#include "scanner.h"

namespace {

struct Scanner {
  std::string_view::const_iterator start;
  std::string_view::const_iterator current;
  std::string_view::const_iterator end;
  std::size_t line;
};

Scanner scanner;

bool is_at_end() { return scanner.current == scanner.end; }

char advance() {
  auto const c = *scanner.current;
  scanner.current++;
  return c;
}

bool match(char expected) {
  if (is_at_end()) {
    return false;
  }
  if (*scanner.current != expected) {
    return false;
  }
  scanner.current++;
  return true;
}

Token make_token(TokenType type) {
  return {.type = type,
          .start = scanner.start,
          .length = static_cast<std::size_t>(scanner.current - scanner.start),
          .line = scanner.line};
}

Token error_token(std::string_view message) {
  return {.type = TokenType::ERROR,
          .start = message.begin(),
          .length = message.size(),
          .line = scanner.line};
}

} // namespace

void init_scanner(std::string_view source) {
  scanner.start = source.begin();
  scanner.current = source.begin();
  scanner.end = source.end();
  scanner.line = 1;
}

Token scan_token() {
  scanner.start = scanner.current;

  if (is_at_end()) {
    return make_token(TokenType::END);
  }

  auto const c = advance();
  switch (c) {
  case '(':
    return make_token(TokenType::LEFT_PAREN);
  case ')':
    return make_token(TokenType::RIGHT_PAREN);
  case '{':
    return make_token(TokenType::LEFT_BRACE);
  case '}':
    return make_token(TokenType::RIGHT_BRACE);
  case ';':
    return make_token(TokenType::SEMICOLON);
  case ',':
    return make_token(TokenType::COMMA);
  case '.':
    return make_token(TokenType::DOT);
  case '-':
    return make_token(TokenType::MINUS);
  case '+':
    return make_token(TokenType::PLUS);
  case '/':
    return make_token(TokenType::SLASH);
  case '*':
    return make_token(TokenType::STAR);
  case '!':
    return make_token(match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
  case '=':
    return make_token(match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
  case '<':
    return make_token(match('=') ? TokenType::LESS_EQUAL : TokenType::LESS);
  case '>':
    return make_token(match('=') ? TokenType::GREATER_EQUAL
                                 : TokenType::GREATER);
  }

  return error_token("Unexpected character.");
}
