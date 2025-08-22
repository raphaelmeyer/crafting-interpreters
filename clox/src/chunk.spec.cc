#include <doctest/doctest.h>

#include "chunk.h"

TEST_CASE("chunk") {
  Chunk chunk;
  init_chunk(&chunk);
  write_chunk(&chunk, OP_RETURN);
  free_chunk(&chunk);

  REQUIRE(chunk.capacity == 0);
}
