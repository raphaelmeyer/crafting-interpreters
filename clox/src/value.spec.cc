#include <doctest/doctest.h>

#include "value.h"

TEST_SUITE("value array") {

  TEST_CASE("initialized") {
    ValueArray values;
    init_value_array(&values);

    SUBCASE("empty after init") { REQUIRE(values.count == 0); }

    SUBCASE("contains added element") {
      write_value_array(&values, 42);

      REQUIRE(values.count == 1);
      REQUIRE(values.capacity >= 1);
      REQUIRE_EQ(values.values[0], 42);
    }

    SUBCASE("contains all added elements") {
      write_value_array(&values, 17);
      write_value_array(&values, 21);

      REQUIRE(values.count == 2);
      REQUIRE(values.capacity >= 2);
      REQUIRE_EQ(values.values[0], 17);
      REQUIRE_EQ(values.values[1], 21);
    }

    SUBCASE("grows capicity") {
      write_value_array(&values, 123);
      int32_t const capacity = values.capacity;

      for (int32_t i = values.count; i < capacity; ++i) {
        write_value_array(&values, 77);
      }

      REQUIRE(values.count == capacity);
      REQUIRE(values.capacity >= capacity);

      write_value_array(&values, 99);
      REQUIRE(values.count == capacity + 1);
      REQUIRE(values.capacity > capacity);
    }

    free_value_array(&values);

    SUBCASE("is re-initialized after free") { REQUIRE(values.count == 0); }
  }
}
