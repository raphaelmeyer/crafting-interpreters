#include <doctest/doctest.h>

#include "table.h"

TEST_SUITE("hash table") {
  TEST_CASE("empty table") {
    Table table;
    init_table(&table);

    REQUIRE(table.count == 0);

    free_table(&table);
  }
}
