
#include "chunk.h"
#include "debug.h"

int main(int, const char *[]) {
  Chunk chunk;
  init_chunk(&chunk);

  const int32_t constant = add_constant(&chunk, 1.2);
  write_chunk(&chunk, OP_CONSTANT, 123);
  write_chunk(&chunk, constant, 123);

  write_chunk(&chunk, OP_RETURN, 123);

  disassemble_chunk(&chunk, "test chunk");

  free_chunk(&chunk);

  return 0;
}
