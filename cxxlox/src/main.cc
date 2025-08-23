#include "chunk.h"
#include "debug.h"

int main() {
  Chunk chunk{};

  write_chunk(chunk, OpCode::RETURN);

  disassemble_chunk(chunk, "test chunk");

  return 0;
}
