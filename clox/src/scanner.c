#include "scanner.h"

#include <stdint.h>

typedef struct Scanner_t {
  char const *start;
  char const *current;
  int32_t line;
} Scanner;

static Scanner scanner;

void init_scanner(const char *source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}
