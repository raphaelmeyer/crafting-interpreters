#pragma once

#include "value.h"

#include <cstdint>
#include <vector>

enum class OpCode : std::uint8_t { CONSTANT, RETURN };

struct Chunk {
  std::vector<std::uint8_t> code;
  std::vector<std::int32_t> lines;
  ValueArray constants;
};

void write_chunk(Chunk &chunk, OpCode op_code, std::int32_t line);
void write_chunk(Chunk &chunk, std::uint8_t byte, std::int32_t line);

std::size_t add_constant(Chunk &chunk, Value value);
