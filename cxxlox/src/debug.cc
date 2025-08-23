#include "debug.h"

#include "chunk.h"

#include <format>
#include <iostream>

namespace {

std::size_t disassemble_instruction(Chunk const &chunk, std::size_t offset);

std::size_t simple_instruction(std::string name, std::size_t offset);

std::size_t disassemble_instruction(Chunk const &chunk, std::size_t offset) {
  std::cout << std::format("{:04} ", offset);

  auto const instruction = static_cast<OpCode>(chunk.code.at(offset));
  switch (instruction) {
  case OpCode::RETURN:
    return simple_instruction("OP_RETURN", offset);
  default:
    std::cout << std::format("Unknown opcode {}\n",
                             static_cast<uint32_t>(instruction));
    return offset + 1;
  }
}

std::size_t simple_instruction(std::string name, std::size_t offset) {
  std::cout << name << "\n";
  return offset + 1;
}

} // namespace

void disassemble_chunk(Chunk const &chunk, std::string name) {
  std::cout << std::format("== {} ==\n", name);

  for (std::size_t offset = 0; offset < chunk.code.size();) {
    offset = disassemble_instruction(chunk, offset);
  }
}
