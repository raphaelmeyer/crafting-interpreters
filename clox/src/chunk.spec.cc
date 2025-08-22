#include <doctest/doctest.h>

#include "chunk.h"

TEST_SUITE("chunk") {

  TEST_CASE("initialized") {
    Chunk chunk;
    init_chunk(&chunk);

    SUBCASE("empty after init") { REQUIRE(chunk.count == 0); }

    SUBCASE("contains added element") {
      write_chunk(&chunk, OP_RETURN);

      REQUIRE(chunk.count == 1);
      REQUIRE(chunk.capacity >= 1);
      REQUIRE_EQ(chunk.code[0], OP_RETURN);
    }

    SUBCASE("contains all added elements") {
      write_chunk(&chunk, OP_CONSTANT);
      write_chunk(&chunk, OP_RETURN);

      REQUIRE(chunk.count == 2);
      REQUIRE(chunk.capacity >= 2);
      REQUIRE_EQ(chunk.code[0], OP_CONSTANT);
      REQUIRE_EQ(chunk.code[1], OP_RETURN);
    }

    SUBCASE("grows capicity") {
      write_chunk(&chunk, OP_RETURN);
      int32_t const capacity = chunk.capacity;

      for (int32_t i = chunk.count; i < capacity; ++i) {
        write_chunk(&chunk, OP_CONSTANT);
      }

      REQUIRE(chunk.count == capacity);
      REQUIRE(chunk.capacity >= capacity);

      write_chunk(&chunk, OP_RETURN);
      REQUIRE(chunk.count == capacity + 1);
      REQUIRE(chunk.capacity > capacity);
    }

    free_chunk(&chunk);

    SUBCASE("is re-initialized after free") { REQUIRE(chunk.count == 0); }
  }
}
