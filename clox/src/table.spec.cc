#include <doctest/doctest.h>

#include "object.h"
#include "table.h"
#include "vm.h"

#include <string_view>

using namespace std::string_view_literals;

TEST_SUITE("hash table") {
  TEST_CASE("empty table") {
    Table table;
    init_table(&table);

    REQUIRE(table.count == 0);

    free_table(&table);
  }

  TEST_CASE("add values") {
    init_vm();

    Table table;
    init_table(&table);

    ObjString *nothing = copy_string("nothing", 7);
    table_set(&table, nothing, nil_value());

    ObjString *hello = copy_string("hello", 5);
    table_set(&table, hello, obj_value(copy_string("world", 5)));

    ObjString *pi = copy_string("pi", 2);
    table_set(&table, pi, number_value(3.14));

    REQUIRE(table.count == 3);

    Value nil = {};
    REQUIRE(table_get(&table, nothing, &nil));
    REQUIRE(is_nil(nil));

    Value string = {};
    REQUIRE(table_get(&table, hello, &string));
    REQUIRE(is_string(string));
    REQUIRE(as_cstring(string) == "world"sv);

    Value number = {};
    REQUIRE(table_get(&table, pi, &number));
    REQUIRE(is_number(number));
    REQUIRE(number.as.number == 3.14);

    free_table(&table);
    free_vm();
  }

  TEST_CASE("retrieve values") {
    init_vm();

    Table table;
    init_table(&table);

    ObjString *hello = copy_string("hello", 5);
    table_set(&table, hello, obj_value(copy_string("world", 5)));

    REQUIRE(table.count == 1);

    Value value = {};
    REQUIRE(table_get(&table, hello, &value));
    REQUIRE(is_string(value));
    REQUIRE(as_cstring(value) == "world"sv);

    ObjString *hell = copy_string("hell", 5);
    ObjString *helloo = copy_string("helloo", 5);

    REQUIRE_FALSE(table_get(&table, hell, &value));
    REQUIRE_FALSE(table_get(&table, helloo, &value));

    free_table(&table);
    free_vm();
  }

  TEST_CASE("copy table") {
    init_vm();

    Table table;
    init_table(&table);

    ObjString *truthy = copy_string("truthy", 6);
    table_set(&table, truthy, bool_value(true));

    ObjString *poiu = copy_string("poiu", 4);
    table_set(&table, poiu, obj_value(copy_string("qwer", 4)));

    ObjString *e = copy_string("e", 1);
    table_set(&table, e, number_value(2.718));

    Table copy;
    init_table(&copy);

    table_add_all(&table, &copy);

    REQUIRE(copy.count == 3);

    free_table(&copy);
    free_table(&table);
    free_vm();
  }
}
