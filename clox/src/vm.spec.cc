#include <doctest/doctest.h>

#include "chunk.h"
#include "vm.h"

TEST_SUITE("vm") {

  TEST_CASE("with chunk") {

    init_vm();

    Chunk chunk;
    init_chunk(&chunk);

    SUBCASE("negate") {
      int32_t const constant = add_constant(&chunk, 1.25);
      write_chunk(&chunk, OP_CONSTANT, 1);
      write_chunk(&chunk, constant, 1);

      write_chunk(&chunk, OP_RETURN, 2);
      _interpret(&chunk);
    }

    SUBCASE("add") {
      int32_t const a = add_constant(&chunk, 1.25);
      write_chunk(&chunk, OP_CONSTANT, 1);
      write_chunk(&chunk, a, 1);

      int32_t const b = add_constant(&chunk, 0.75);
      write_chunk(&chunk, OP_CONSTANT, 1);
      write_chunk(&chunk, b, 1);

      write_chunk(&chunk, OP_ADD, 1);

      write_chunk(&chunk, OP_RETURN, 2);
      _interpret(&chunk);
    }

    SUBCASE("subtract") {
      int32_t const a = add_constant(&chunk, 1.25);
      write_chunk(&chunk, OP_CONSTANT, 1);
      write_chunk(&chunk, a, 1);

      int32_t const b = add_constant(&chunk, 0.75);
      write_chunk(&chunk, OP_CONSTANT, 1);
      write_chunk(&chunk, b, 1);

      write_chunk(&chunk, OP_SUBTRACT, 1);

      write_chunk(&chunk, OP_RETURN, 2);
      _interpret(&chunk);
    }

    free_vm();
    free_chunk(&chunk);
  }
}
