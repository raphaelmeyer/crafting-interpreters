#include <doctest/doctest.h>

#include "debug.h"
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

TEST_SUITE("garbage collection") {

  TEST_CASE("compiling constants") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("string literal") {
      auto const result = interpret(R"(var s = "hello"; print s;)");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("many unique constants force array growth") {
      auto const result = interpret(R"(
          var a = "a"; var b = "b"; var c = "c"; var d = "d";
          var e = "e"; var f = "f"; var g = "g"; var h = "h";
          var i = "i"; print a;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("function object") {
      auto const result = interpret(R"(
          fun square(x) { return x * x; }
          print square(4);
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("string concatenation") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("operands survive allocation") {
      auto const result = interpret(R"(
          var a = "hello";
          var b = " world";
          print a + b;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("chained concatenation") {
      auto const result = interpret(R"(print "a" + "b" + "c" + "d";)");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("string interning") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("equal literals are the same object") {
      auto const result = interpret(R"(
          var a = "hello";
          var b = "hello";
          print a == b;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("concatenated result equals literal") {
      auto const result = interpret(R"(
          var a = "hel" + "lo";
          var b = "hello";
          print a == b;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("new string survives table_set during interning") {
      auto const result = interpret(R"(
          var a = "apple";    var b = "banana";  var c = "cherry";
          var d = "date";     var e = "elder";   var f = "fig";
          var g = "grape";    var h = "honey";   var i = "kiwi";
          var j = "lemon";    var k = "mango";   var l = "nectar";
          var m = "orange";   var n = "papaya";  var o = "quince";
          var p = "raspberry";
          print a;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("string allocation") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("many concatenations") {
      auto const result = interpret(R"(
          var s = "";
          var i = 0;
          while (i < 20) { s = s + "x"; i = i + 1; }
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("temporary strings are reclaimed") {
      auto const result = interpret(R"(
          var i = 0;
          while (i < 10) {
            var tmp = "temp" + "string";
            i = i + 1;
          }
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("global variables") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("survive collection") {
      auto const result = interpret(R"(
          var a = "alive";
          var i = 0;
          while (i < 10) { var t = "dead"; i = i + 1; }
          print a;
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("open upvalues") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("captured stack variable survives while in scope") {
      auto const result = interpret(R"(
          fun outer() {
            var x = "captured";
            fun inner() { return x; }
            return inner();
          }
          print outer();
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }

  TEST_CASE("closed upvalues") {
    DEBUG_STRESS_GC = true;
    init_vm();

    SUBCASE("value moved off stack survives collection") {
      auto const result = interpret(R"(
          fun make_counter() {
            var count = 0;
            fun increment() { count = count + 1; return count; }
            return increment;
          }
          var counter = make_counter();
          counter(); counter(); print counter();
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    SUBCASE("multiple closures share closed upvalue") {
      auto const result = interpret(R"(
          fun make_adder(n) {
            fun add(x) { return n + x; }
            return add;
          }
          var add5 = make_adder(5);
          print add5(3);
      )");
      REQUIRE(result == INTERPRET_OK);
    }

    free_vm();
  }
}
