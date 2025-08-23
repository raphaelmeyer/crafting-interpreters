#include "chunk.h"

void write_chunk(Chunk &chunk, OpCode op_code) {
  write_chunk(chunk, static_cast<std::uint8_t>(op_code));
}

void write_chunk(Chunk &chunk, std::uint8_t byte) {
  chunk.code.push_back(byte);
}
