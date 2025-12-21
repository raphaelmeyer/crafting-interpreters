#include <doctest/doctest.h>

#include "vm.h"

TEST_SUITE("vm") {

  TEST_CASE("expression statement") {
    init_vm();

    SUBCASE("arithmetic") {
      auto const result = interpret("(1 + 2) * (3 / 4) > (5 - 6);");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("print statement") {

    init_vm();

    SUBCASE("negate number") {
      auto const result = interpret("print -1.25;");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("add number") {
      auto const result = interpret("print 1.25 + 0.75;");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("subtract number") {
      auto const result = interpret("print 1.25 - 0.75;");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("arithmetic") {
      auto const result = interpret("print (-4 + 3) / 2 + 1;");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("concatenate strings") {
      auto const result = interpret("print \"hello\" + \" \" + \"world\";");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("invalid statements") {

    init_vm();

    SUBCASE("invalid") {
      auto const result = interpret("3 +* 4;");
      REQUIRE(result == INTERPRET_COMPILE_ERROR);
    }

    SUBCASE("add incompatible types") {
      auto const result = interpret("42 + \"universe\";");
      REQUIRE(result == INTERPRET_RUNTIME_ERROR);
    }

    free_vm();
  }
}
