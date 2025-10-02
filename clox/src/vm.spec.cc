#include <doctest/doctest.h>

#include "vm.h"

TEST_SUITE("vm") {

  TEST_CASE("expression") {

    init_vm();

    SUBCASE("negate") {
      auto const result = interpret("-1.25");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("add") {
      auto const result = interpret("1.25 + 0.75");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("subtract") {
      auto const result = interpret("1.25 - 0.75");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("arithmetic") {
      auto const result = interpret("(-4 + 3) / 2 + 1");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("invalid") {
      auto const result = interpret("3 +* 4");
      REQUIRE(result == INTERPRET_COMPILE_ERROR);
    }

    free_vm();
  }
}
