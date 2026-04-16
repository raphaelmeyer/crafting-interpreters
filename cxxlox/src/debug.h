#pragma once

#include <string>

struct Debug {
  static bool TRACE_EXECUTION;
  static bool PRINT_CODE;
  static bool GC_LOG;
  static bool GC_STRESS;
};

struct Chunk;

void disassemble_chunk(Chunk const &chunk, std::string name);
std::size_t disassemble_instruction(Chunk const &chunk, std::size_t offset);
