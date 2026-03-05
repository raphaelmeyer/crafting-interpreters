#include "chunk.h"

void write_chunk(Chunk &chunk, uint8_t byte, std::int32_t line) {
  chunk.code.push_back(byte);
  chunk.lines.push_back(line);
}

std::size_t add_constant(Chunk &chunk, Value value) {
  chunk.constants.push_back(value);
  return chunk.constants.size() - 1;
}
