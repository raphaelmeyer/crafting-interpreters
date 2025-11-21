#include <doctest/doctest.h>

#include "vm.h"

TEST_SUITE("vm") {

  TEST_CASE("expression") {

    init_vm();

    SUBCASE("negate number") {
      auto const result = interpret("-1.25");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("add number") {
      auto const result = interpret("1.25 + 0.75");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("subtract number") {
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

    SUBCASE("concatenate strings") {
      auto const result = interpret("\"hello\" + \" \" + \"world\"");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("add incompatible types") {
      auto const result = interpret("42 + \"universe\"");
      REQUIRE(result == INTERPRET_RUNTIME_ERROR);
    }

    free_vm();
  }
}
