#include "scanner.h"

namespace {

struct Scanner {
  std::string_view::const_iterator start;
  std::string_view::const_iterator current;
  std::size_t line;
};

Scanner scanner;

} // namespace

void init_scanner(std::string_view source) {
  scanner.start = source.begin();
  scanner.current = source.begin();
  scanner.line = 1;
}
