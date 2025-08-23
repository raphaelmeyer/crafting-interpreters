#include <doctest/doctest.h>

#include "chunk.h"

TEST_SUITE("chunk") {

  TEST_CASE("code") {
    Chunk chunk;
    init_chunk(&chunk);

    SUBCASE("empty after init") { REQUIRE(chunk.count == 0); }

    SUBCASE("contains added element") {
      write_chunk(&chunk, OP_RETURN, 0);

      REQUIRE(chunk.count == 1);
      REQUIRE(chunk.capacity >= 1);
      REQUIRE_EQ(chunk.code[0], OP_RETURN);
    }

    SUBCASE("contains all added elements") {
      write_chunk(&chunk, OP_CONSTANT, 0);
      write_chunk(&chunk, OP_RETURN, 0);

      REQUIRE(chunk.count == 2);
      REQUIRE(chunk.capacity >= 2);
      REQUIRE_EQ(chunk.code[0], OP_CONSTANT);
      REQUIRE_EQ(chunk.code[1], OP_RETURN);
    }

    SUBCASE("grows capicity") {
      write_chunk(&chunk, OP_RETURN, 0);
      int32_t const capacity = chunk.capacity;

      for (int32_t i = chunk.count; i < capacity; ++i) {
        write_chunk(&chunk, OP_CONSTANT, 0);
      }

      REQUIRE(chunk.count == capacity);
      REQUIRE(chunk.capacity >= capacity);

      write_chunk(&chunk, OP_RETURN, 0);
      REQUIRE(chunk.count == capacity + 1);
      REQUIRE(chunk.capacity > capacity);
    }

    free_chunk(&chunk);

    SUBCASE("is re-initialized after free") { REQUIRE(chunk.count == 0); }
  }

  TEST_CASE("line information") {
    Chunk chunk;
    init_chunk(&chunk);

    SUBCASE("store line information for each element") {
      write_chunk(&chunk, OP_CONSTANT, 101);
      write_chunk(&chunk, 64, 101);
      write_chunk(&chunk, OP_RETURN, 107);

      REQUIRE(chunk.lines[0] == 101);
      REQUIRE(chunk.lines[1] == 101);
      REQUIRE(chunk.lines[2] == 107);
    }

    free_chunk(&chunk);
  }

  TEST_CASE("constants") {
    Chunk chunk;
    init_chunk(&chunk);

    SUBCASE("contains added constant") {
      int32_t const a = add_constant(&chunk, 12);

      REQUIRE(chunk.constants.count == 1);
      REQUIRE(chunk.constants.values[a] == 12);
    }

    SUBCASE("contains all added constants") {
      int32_t const a = add_constant(&chunk, 23);
      int32_t const b = add_constant(&chunk, 11);
      int32_t const c = add_constant(&chunk, 29);

      REQUIRE(chunk.constants.count == 3);
      REQUIRE(chunk.constants.values[a] == 23);
      REQUIRE(chunk.constants.values[b] == 11);
      REQUIRE(chunk.constants.values[c] == 29);
    }

    free_chunk(&chunk);

    SUBCASE("clean up constants after free") {
      REQUIRE(chunk.constants.count == 0);
    }
  }
}
