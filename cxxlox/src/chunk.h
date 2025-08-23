#pragma once

#include <cstdint>
#include <vector>

enum class OpCode : std::uint8_t { RETURN };

struct Chunk {
  std::vector<std::uint8_t> code;
};

void write_chunk(Chunk &chunk, OpCode op_code);
void write_chunk(Chunk &chunk, std::uint8_t byte);
