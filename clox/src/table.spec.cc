#include <doctest/doctest.h>

#include "object.h"
#include "table.h"
#include "vm.h"

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
