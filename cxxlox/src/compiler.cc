#include "compiler.h"

#include "scanner.h"

#include <format>
#include <iostream>

void compile(std::string_view source) {
  init_scanner(source);
  std::size_t line = 0;
  for (;;) {
    Token token = scan_token();
    if (token.line != line) {
      std::cout << std::format("{:4} ", token.line);
      line = token.line;
    } else {
      std::cout << "   | ";
    }

    std::cout << std::format(
                     "{:2} '{}'", static_cast<uint32_t>(token.type),
                     std::string_view{token.start, token.start + token.length})
              << "\n";

    if (token.type == TokenType::END) {
      break;
    }
  }
}
