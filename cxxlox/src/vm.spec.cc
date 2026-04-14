#include <doctest/doctest.h>

#include "vm.h"

TEST_SUITE("vm") {

  auto const vm = VM::create();

  TEST_CASE("expression statement") {
    auto const result = vm->interpret("10 + 8 - 6 * 4 / 2;");
    REQUIRE(result == InterpretResult::OK);
  }
}
