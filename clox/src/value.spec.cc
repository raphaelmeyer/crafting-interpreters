#include <doctest/doctest.h>

#include "value.h"

TEST_SUITE("value type") {
  TEST_CASE("boolean") {
    Value yes = bool_value(true);
    REQUIRE(is_bool(yes));
    REQUIRE_EQ(yes.type, VAL_BOOL);
    REQUIRE_EQ(yes.as.boolean, true);

    Value no = bool_value(false);
    REQUIRE_EQ(no.as.boolean, false);
  }

  TEST_CASE("nil") {
    Value nil = nil_value();
    REQUIRE(is_nil(nil));
    REQUIRE_EQ(nil.type, VAL_NIL);
  }

  TEST_CASE("number") {
    Value n = number_value(123);
    REQUIRE(is_number(n));
    REQUIRE_EQ(n.type, VAL_NUMBER);
    REQUIRE_EQ(n.as.number, 123);
  }
}

TEST_SUITE("value array") {

  TEST_CASE("initialized") {
    ValueArray values;
    init_value_array(&values);

    SUBCASE("empty after init") { REQUIRE(values.count == 0); }

    SUBCASE("contains added element") {
      write_value_array(&values, number_value(42));

      REQUIRE(values.count == 1);
      REQUIRE(values.capacity >= 1);
      REQUIRE_EQ(values.values[0].as.number, 42);
    }

    SUBCASE("contains all added elements") {
      write_value_array(&values, number_value(17));
      write_value_array(&values, number_value(21));

      REQUIRE(values.count == 2);
      REQUIRE(values.capacity >= 2);
      REQUIRE_EQ(values.values[0].as.number, 17);
      REQUIRE_EQ(values.values[1].as.number, 21);
    }

    SUBCASE("grows capicity") {
      write_value_array(&values, number_value(123));
      int32_t const capacity = values.capacity;

      for (int32_t i = values.count; i < capacity; ++i) {
        write_value_array(&values, number_value(77));
      }

      REQUIRE(values.count == capacity);
      REQUIRE(values.capacity >= capacity);

      write_value_array(&values, number_value(99));
      REQUIRE(values.count == capacity + 1);
      REQUIRE(values.capacity > capacity);
    }

    SUBCASE("mixed values") {
      write_value_array(&values, bool_value(true));
      write_value_array(&values, nil_value());
      write_value_array(&values, number_value(3.14));

      REQUIRE(values.count == 3);
      REQUIRE(is_bool(values.values[0]));
      REQUIRE(is_nil(values.values[1]));
      REQUIRE(is_number(values.values[2]));

      REQUIRE_EQ(values.values[0].as.boolean, true);
      REQUIRE_EQ(values.values[2].as.number, 3.14);
    }

    free_value_array(&values);

    SUBCASE("is re-initialized after free") { REQUIRE(values.count == 0); }
  }
}
