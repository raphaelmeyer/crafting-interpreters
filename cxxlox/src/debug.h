#pragma once

#include <string>

struct Debug {
  static bool TRACE_EXECUTION;
};

struct Chunk;

void disassemble_chunk(Chunk const &chunk, std::string name);
std::size_t disassemble_instruction(Chunk const &chunk, std::size_t offset);
