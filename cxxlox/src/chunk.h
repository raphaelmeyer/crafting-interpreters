#pragma once

#include "value.h"

#include <cstdint>
#include <vector>

enum class OpCode : std::uint8_t {
  CONSTANT,
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  NEGATE,
  RETURN
};

struct Chunk {
  std::vector<std::uint8_t> code;
  std::vector<std::int32_t> lines;
  ValueArray constants;

  using CodeIterator = decltype(Chunk::code)::const_iterator;

  void write(uint8_t byte, std::int32_t line);
  std::size_t add_constant(Value value);
};
