#include <doctest/doctest.h>

#include "object.h"
#include "vm.h"

#include <string_view>

using namespace std::string_view_literals;

TEST_CASE("object") {
  init_vm();

  SUBCASE("string object") {
    auto const string = obj_value(copy_string("asdf", 4));

    REQUIRE(is_obj(string));
    REQUIRE(string.type == VAL_OBJ);

    REQUIRE(is_obj_type(string, OBJ_STRING));
    REQUIRE(string.as.obj->type == OBJ_STRING);

    REQUIRE(as_string(string)->obj.type == OBJ_STRING);
  }

  SUBCASE("string value") {
    auto const string = obj_value(copy_string("hello world", 5));

    REQUIRE(as_cstring(string) == "hello"sv);
    REQUIRE(as_string(string)->chars == "hello"sv);
    REQUIRE(as_string(string)->length == 5);
  }

  free_vm();
}
