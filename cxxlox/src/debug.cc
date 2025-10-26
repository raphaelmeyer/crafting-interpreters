#include "debug.h"

#include "chunk.h"

#include <format>
#include <iostream>

namespace {

std::size_t simple_instruction(std::string name, std::size_t offset);
std::size_t constant_instruction(std::string name, Chunk const &chunk,
                                 std::size_t offset);

std::size_t simple_instruction(std::string name, std::size_t offset) {
  std::cout << name << "\n";
  return offset + 1;
}

std::size_t constant_instruction(std::string name, Chunk const &chunk,
                                 std::size_t offset) {
  auto const constant = chunk.code.at(offset + 1);
  std::cout << std::format("{:16} {:4} '", name, constant);
  print_value(chunk.constants.at(constant));
  std::cout << "'\n";

  return offset + 2;
}

} // namespace

bool Debug::TRACE_EXECUTION = false;
bool Debug::PRINT_CODE = false;

void disassemble_chunk(Chunk const &chunk, std::string name) {
  std::cout << std::format("== {} ==\n", name);

  for (std::size_t offset = 0; offset < chunk.code.size();) {
    offset = disassemble_instruction(chunk, offset);
  }
}

std::size_t disassemble_instruction(Chunk const &chunk, std::size_t offset) {
  std::cout << std::format("{:04} ", offset);

  if (offset > 0 && chunk.lines.at(offset) == chunk.lines.at(offset - 1)) {
    std::cout << "   | ";
  } else {
    std::cout << std::format("{:4} ", chunk.lines.at(offset));
  }

  auto const instruction = static_cast<OpCode>(chunk.code.at(offset));
  switch (instruction) {
  case OpCode::CONSTANT:
    return constant_instruction("OP_CONSTANT", chunk, offset);
  case OpCode::NIL:
    return simple_instruction("OP_NIL", offset);
  case OpCode::TRUE:
    return simple_instruction("OP_TRUE", offset);
  case OpCode::FALSE:
    return simple_instruction("OP_FALSE", offset);
  case OpCode::ADD:
    return simple_instruction("OP_ADD", offset);
  case OpCode::SUBTRACT:
    return simple_instruction("OP_SUBTRACT", offset);
  case OpCode::MULTIPLY:
    return simple_instruction("OP_MULTIPLY", offset);
  case OpCode::DIVIDE:
    return simple_instruction("OP_DIVIDE", offset);
  case OpCode::NEGATE:
    return simple_instruction("OP_NEGATE", offset);
  case OpCode::RETURN:
    return simple_instruction("OP_RETURN", offset);
  default:
    std::cout << std::format("Unknown opcode {:d}\n",
                             static_cast<uint8_t>(instruction));
    return offset + 1;
  }
}
