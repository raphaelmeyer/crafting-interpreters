#include "chunk.h"

void write_chunk(Chunk &chunk, OpCode op_code) {
  write_chunk(chunk, static_cast<std::uint8_t>(op_code));
}

void write_chunk(Chunk &chunk, std::uint8_t byte) {
  chunk.code.push_back(byte);
}

std::size_t add_constant(Chunk &chunk, Value value) {
  chunk.constants.push_back(value);
  return chunk.constants.size() - 1;
}
