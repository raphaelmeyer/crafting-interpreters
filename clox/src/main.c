
#include "chunk.h"
#include "debug.h"

int main(int, const char *[]) {
  Chunk chunk;
  init_chunk(&chunk);
  write_chunk(&chunk, OP_RETURN);

  disassemble_chunk(&chunk, "test chunk");

  free_chunk(&chunk);

  return 0;
}
