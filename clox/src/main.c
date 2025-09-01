
#include "chunk.h"
#include "debug.h"
#include "vm.h"

#include <string.h>

int main(int argc, const char *argv[]) {
  int32_t i = 0;
  for (i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--debug") == 0) {
      DEBUG_TRACE_EXECUTION = true;
    }
  }

  init_vm();

  Chunk chunk;
  init_chunk(&chunk);

  int32_t constant = add_constant(&chunk, 1.2);
  write_chunk(&chunk, OP_CONSTANT, 123);
  write_chunk(&chunk, constant, 123);

  constant = add_constant(&chunk, 3.4);
  write_chunk(&chunk, OP_CONSTANT, 123);
  write_chunk(&chunk, constant, 123);

  write_chunk(&chunk, OP_ADD, 123);

  constant = add_constant(&chunk, 5.6);
  write_chunk(&chunk, OP_CONSTANT, 123);
  write_chunk(&chunk, constant, 123);

  write_chunk(&chunk, OP_DIVIDE, 123);

  write_chunk(&chunk, OP_NEGATE, 123);

  write_chunk(&chunk, OP_RETURN, 123);
  disassemble_chunk(&chunk, "test chunk");

  interpret(&chunk);
  free_vm();

  free_chunk(&chunk);

  return 0;
}
