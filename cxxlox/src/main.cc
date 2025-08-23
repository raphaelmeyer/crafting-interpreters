#include "chunk.h"
#include "debug.h"

int main() {
  Chunk chunk{};

  auto const constant = add_constant(chunk, 1.2);
  write_chunk(chunk, OpCode::CONSTANT);
  write_chunk(chunk, constant);

  write_chunk(chunk, OpCode::RETURN);

  disassemble_chunk(chunk, "test chunk");

  return 0;
}
