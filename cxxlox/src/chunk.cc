#include "chunk.h"

void Chunk::write(uint8_t byte, std::int32_t line) {
  code.push_back(byte);
  lines.push_back(line);
}

std::size_t Chunk::add_constant(Value value) {
  constants.push_back(value);
  return constants.size() - 1;
}