#include "scanner.h"

#include <algorithm>
#include <cctype>
#include <cstdint>

namespace {

struct Scanner {
  std::string_view::const_iterator start;
  std::string_view::const_iterator current;
  std::string_view::const_iterator end;
  std::size_t line;
};

Scanner scanner;

bool is_digit(char c) { return std::isdigit(c); }

bool is_alpha(char c) { return std::isalpha(c) || c == '_'; }

bool is_at_end() { return scanner.current == scanner.end; }

char advance() {
  auto const c = *scanner.current;
  scanner.current++;
  return c;
}

char peek() { return *scanner.current; }

char peek_next() {
  auto next = scanner.current + 1;
  if (next == scanner.end) {
    return '\0';
  }
  return *next;
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

void skip_whitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      advance();
      break;
    case '\n':
      scanner.line++;
      advance();
      break;
    case '/':
      if (peek_next() == '/') {
        while (peek() != '\n' && not is_at_end()) {
          advance();
        }
      } else {
        return;
      }
      break;
    default:
      return;
    }
  }
}

TokenType check_keyword(std::int32_t offset, std::int32_t length,
                        std::string_view rest, TokenType type) {
  if (scanner.current - scanner.start == offset + length &&
      std::equal(scanner.start + offset, scanner.current, rest.begin())) {
    return type;
  }

  return TokenType::IDENTIFIER;
}

TokenType identifier_type() {

  switch (*scanner.start) {
  case 'a':
    return check_keyword(1, 2, "nd", TokenType::AND);
  case 'c':
    return check_keyword(1, 4, "lass", TokenType::CLASS);
  case 'e':
    return check_keyword(1, 3, "lse", TokenType::ELSE);
  case 'f':
    if (scanner.current - scanner.start > 1) {
      switch (*(scanner.start + 1)) {
      case 'a':
        return check_keyword(2, 3, "lse", TokenType::FALSE);
      case 'o':
        return check_keyword(2, 1, "r", TokenType::FOR);
      case 'u':
        return check_keyword(2, 1, "n", TokenType::FUN);
      }
    }
    break;
  case 'i':
    return check_keyword(1, 1, "f", TokenType::IF);
  case 'n':
    return check_keyword(1, 2, "il", TokenType::NIL);
  case 'o':
    return check_keyword(1, 1, "r", TokenType::OR);
  case 'p':
    return check_keyword(1, 4, "rint", TokenType::PRINT);
  case 'r':
    return check_keyword(1, 5, "eturn", TokenType::RETURN);
  case 's':
    return check_keyword(1, 4, "uper", TokenType::SUPER);
  case 't':
    if (scanner.current - scanner.start > 1) {
      switch (*(scanner.start + 1)) {
      case 'h':
        return check_keyword(2, 2, "is", TokenType::THIS);
      case 'r':
        return check_keyword(2, 2, "ue", TokenType::TRUE);
      }
    }
    break;
  case 'v':
    return check_keyword(1, 2, "ar", TokenType::VAR);
  case 'w':
    return check_keyword(1, 4, "hile", TokenType::WHILE);
  }

  return TokenType::IDENTIFIER;
}

Token identifier() {
  while (is_alpha(peek()) || is_digit(peek())) {
    advance();
  }
  return make_token(identifier_type());
}

Token number() {
  while (is_digit(peek())) {
    advance();
  }

  if (peek() == '.' && is_digit(peek_next())) {
    advance();

    while (is_digit(peek())) {
      advance();
    }
  }

  return make_token(TokenType::NUMBER);
}

Token string() {
  while (peek() != '"' && not is_at_end()) {
    if (peek() == '\n') {
      scanner.line++;
    }
    advance();
  }

  if (is_at_end()) {
    return error_token("Unterminated string.");
  }

  advance();
  return make_token(TokenType::STRING);
}

} // namespace

void init_scanner(std::string_view source) {
  scanner.start = source.begin();
  scanner.current = source.begin();
  scanner.end = source.end();
  scanner.line = 1;
}

Token scan_token() {
  skip_whitespace();

  scanner.start = scanner.current;

  if (is_at_end()) {
    return make_token(TokenType::END);
  }

  auto const c = advance();
  if (is_alpha(c)) {
    return identifier();
  }
  if (is_digit(c)) {
    return number();
  }

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
  case '"':
    return string();
  }

  return error_token("Unexpected character.");
}
