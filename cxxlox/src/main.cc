#include "chunk.h"

int main() {
  Chunk chunk{};

  write_chunk(chunk, OpCode::RETURN);

  return 0;
}
