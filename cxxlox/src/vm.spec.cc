#include <doctest/doctest.h>

#include "vm.h"

#include <sstream>

namespace {

struct Captured {
  InterpretResult result;
  std::string out;
  std::string err;
};

Captured run(std::string_view source) {
  std::ostringstream out, err;
  auto result = VM::create(out, err)->interpret(source);
  return {result, out.str(), err.str()};
}

} // namespace

TEST_SUITE("vm") {

  TEST_CASE("expression statement") {
    auto const [result, out, err] = run("10 + 8 - 6 * 4 / 2;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out.empty());
  }

  TEST_CASE("var declaration") {
    auto const [result, out, err] = run("var x = 42; print x;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "42\n");
  }

  TEST_CASE("block statement") {
    auto const [result, out, err] =
        run("{ var x = 1; var y = 2; print x + y; }");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "3\n");
  }

  TEST_CASE("if statement - true branch") {
    auto const [result, out, err] = run("var x = 0; if (true) x = 1; print x;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "1\n");
  }

  TEST_CASE("if statement - else branch") {
    auto const [result, out, err] =
        run("var x = 0; if (false) x = 1; else x = 2; print x;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "2\n");
  }

  TEST_CASE("while statement") {
    auto const [result, out, err] =
        run("var i = 0; while (i < 3) i = i + 1; print i;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "3\n");
  }

  TEST_CASE("for statement") {
    auto const [result, out, err] =
        run("var s = 0; for (var i = 0; i < 3; i = i + 1) s = s + i; print s;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "3\n");
  }

  TEST_CASE("function declaration and call") {
    auto const [result, out, err] =
        run("fun add(a, b) { return a + b; } print add(1, 2);");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "3\n");
  }

  TEST_CASE("return statement") {
    auto const [result, out, err] =
        run("fun early() { return 1; return 2; } print early();");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "1\n");
  }

  TEST_CASE("class declaration") {
    auto const [result, out, err] = run("class Foo {} print Foo;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "Foo\n");
  }

  TEST_CASE("property set and get") {
    auto const [result, out, err] =
        run("class Foo {} var f = Foo(); f.x = 1; print f.x;");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "1\n");
  }

  TEST_CASE("print statement") {
    auto const [result, out, err] = run("print \"hello\";");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "hello\n");
  }

  TEST_CASE("closure captures upvalue") {
    auto const [result, out, err] = run(R"(
      fun make_counter() {
        var i = 0;
        fun increment() { i = i + 1; return i; }
        return increment;
      }
      var c = make_counter();
      print c();
      print c();
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "1\n2\n");
  }

  TEST_CASE("block shadowing") {
    auto const [result, out, err] = run(R"(
      var x = "outer";
      { var x = "inner"; print x; }
      print x;
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "inner\nouter\n");
  }

  TEST_CASE("runtime error - nil operand") {
    auto const [result, out, err] = run("nil + 1;");
    REQUIRE(result == InterpretResult::RUNTIME_ERROR);
    REQUIRE_FALSE(err.empty());
  }

  TEST_CASE("runtime error - arity mismatch") {
    auto const [result, out, err] = run("fun f(a) { return a; } f(1, 2);");
    REQUIRE(result == InterpretResult::RUNTIME_ERROR);
    REQUIRE_FALSE(err.empty());
  }

  TEST_CASE("compile error") {
    auto const [result, out, err] = run("var;");
    REQUIRE(result == InterpretResult::COMPILE_ERROR);
    REQUIRE_FALSE(err.empty());
  }

  TEST_CASE("runtime error - type mismatch") {
    auto const [result, out, err] = run("true + 1;");
    REQUIRE(result == InterpretResult::RUNTIME_ERROR);
    REQUIRE_FALSE(err.empty());
  }

  TEST_CASE("runtime error - undefined variable") {
    auto const [result, out, err] = run("x;");
    REQUIRE(result == InterpretResult::RUNTIME_ERROR);
    REQUIRE_FALSE(err.empty());
  }

  TEST_CASE("memory cycle - instance self-reference") {
    auto const [result, out, err] = run(R"(
      class Node {}
      var a = Node();
      a.self = a;
    )");
    REQUIRE(result == InterpretResult::OK);
  }

  TEST_CASE("memory cycle - mutual instance references") {
    auto const [result, out, err] = run(R"(
      class Node {}
      var a = Node();
      var b = Node();
      a.next = b;
      b.prev = a;
    )");
    REQUIRE(result == InterpretResult::OK);
  }

  TEST_CASE("memory cycle - closure captures itself via closed upvalue") {
    // When make() returns, the open upvalue for f is closed with the value
    // currently in f's stack slot (= ObjClosure(inner)), forming the cycle.
    auto const [result, out, err] = run(R"(
      fun make() {
        var f = nil;
        fun inner() { return f; }
        f = inner;
        return inner;
      }
      var c = make();
    )");
    REQUIRE(result == InterpretResult::OK);
  }

  TEST_CASE(
      "memory cycle - instance field holds closure that captures instance") {
    auto const [result, out, err] = run(R"(
      class Foo {}
      fun capture(o) {
        fun get() { return o; }
        return get;
      }
      var obj = Foo();
      obj.method = capture(obj);
    )");
    REQUIRE(result == InterpretResult::OK);
  }
}
