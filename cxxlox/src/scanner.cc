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

  return error_token("Unexpected character.");
}
