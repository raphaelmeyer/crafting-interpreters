#include <doctest/doctest.h>

#include "debug.h"
#include "vm.h"

#include <sstream>

namespace {

struct GcStressGuard {
  GcStressGuard() { Debug::GC_STRESS = true; }
  ~GcStressGuard() { Debug::GC_STRESS = false; }
};

struct Captured {
  InterpretResult result;
  std::string out;
  std::string err;
};

Captured run(std::string_view source) {
  GcStressGuard guard;
  std::ostringstream out, err;
  auto result = VM::create(out, err)->interpret(source);
  return {result, out.str(), err.str()};
}

} // namespace

TEST_SUITE("garbage collection") {

  TEST_CASE("globals survive collection") {
    auto const [result, out, err] = run(R"(
      var a = "hello";
      var b = "world";
      var c = a + " " + b;
      print c;
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "hello world\n");
  }

  TEST_CASE("stack values survive collection") {
    // Each concatenation allocates a new string, triggering GC under stress.
    // The intermediate strings on the stack must be treated as roots.
    auto const [result, out, err] = run(R"(
      fun build() {
        var s = "a";
        s = s + "b";
        s = s + "c";
        s = s + "d";
        return s;
      }
      print build();
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "abcd\n");
  }

  TEST_CASE("closed upvalue survives collection") {
    auto const [result, out, err] = run(R"(
      fun make_counter() {
        var i = 0;
        fun increment() {
          i = i + 1;
          return i;
        }
        return increment;
      }
      var c = make_counter();
      print c();
      print c();
      print c();
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "1\n2\n3\n");
  }

  TEST_CASE("instance fields survive collection") {
    auto const [result, out, err] = run(R"(
      class Box {}
      var b = Box();
      b.value = "kept";
      var dummy = "x" + "y" + "z";
      print b.value;
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "kept\n");
  }

  TEST_CASE("temporary objects collected without corrupting live data") {
    // Creates many short-lived instances in a loop; only the loop counter
    // and the final instance must survive.
    auto const [result, out, err] = run(R"(
      class Node {}
      var i = 0;
      while (i < 10) {
        var n = Node();
        n.x = i;
        i = i + 1;
      }
      print i;
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "10\n");
  }

  TEST_CASE("compiler roots: GC triggered during compilation") {
    // Under GC stress every allocation triggers a collection, including those
    // made while the compiler is still building ObjFunctions. This exercises
    // the compiler root marking path.
    auto const [result, out, err] = run(R"(
      fun outer() {
        fun middle() {
          fun inner() {
            return "deep";
          }
          return inner();
        }
        return middle();
      }
      print outer();
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "deep\n");
  }

  TEST_CASE("multiple closures sharing an upvalue survive collection") {
    auto const [result, out, err] = run(R"(
      fun make_pair() {
        var x = 0;
        fun get() { return x; }
        fun set(v) { x = v; }
        return get;
      }
      var get = make_pair();
      print get();
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE(out == "0\n");
  }

  TEST_CASE("class object survives collection") {
    auto const [result, out, err] = run(R"(
      class Animal {}
      var dummy = "x" + "y";
      var a = Animal();
      print a;
    )");
    REQUIRE(result == InterpretResult::OK);
    REQUIRE_FALSE(out.empty());
  }

  TEST_CASE("instance self-reference cycle collected without crash") {
    auto const [result, out, err] = run(R"(
      class Node {}
      var n = Node();
      n.self = n;
      n = nil;
    )");
    REQUIRE(result == InterpretResult::OK);
  }

  TEST_CASE("mutual reference cycle collected without crash") {
    auto const [result, out, err] = run(R"(
      class Node {}
      var a = Node();
      var b = Node();
      a.next = b;
      b.prev = a;
      a = nil;
      b = nil;
    )");
    REQUIRE(result == InterpretResult::OK);
  }
}
