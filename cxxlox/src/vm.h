#pragma once

#include "chunk.h"

#include <array>
#include <string_view>

constexpr std::size_t const STACK_MAX = 256;

struct VM {
  Chunk const *chunk;
  std::uint8_t const *ip;
  std::array<Value, STACK_MAX> stack;
  Value *stack_top;
};

enum class InterpretResult { OK, COMPILE_ERROR, RUNTIME_ERROR };

void init_vm();
void free_vm();

InterpretResult interpret(std::string_view source);
