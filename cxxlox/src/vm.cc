#include "vm.h"

#include "debug.h"

#include <functional>
#include <iostream>

namespace {

VM vm{};

void reset_stack() { vm.stack_top = vm.stack.data(); }

void push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

Value pop() {
  vm.stack_top--;
  return *vm.stack_top;
}

template <typename T> T read_byte() { return static_cast<T>(*vm.ip++); }
Value read_constant() { return vm.chunk->constants[read_byte<std::size_t>()]; }

template <typename Op> void binary_op(Op op) {
  auto const b = pop();
  auto const a = pop();
  push(op(a, b));
}

InterpretResult run() {
  for (;;) {
    if (Debug::TRACE_EXECUTION) {
      std::cout << "          ";
      for (auto const *slot = vm.stack.data(); slot < vm.stack_top; ++slot) {
        std::cout << "[ ";
        print_value(*slot);
        std::cout << " ]";
      }
      std::cout << "\n";

      disassemble_instruction(
          *vm.chunk, static_cast<std::size_t>(vm.ip - vm.chunk->code.data()));
    }

    auto const instruction = read_byte<OpCode>();
    switch (instruction) {
    case OpCode::CONSTANT: {
      Value constant = read_constant();
      push(constant);
      break;
    }

    case OpCode::ADD: {
      binary_op(std::plus());
      break;
    }
    case OpCode::SUBTRACT: {
      binary_op(std::minus());
      break;
    }
    case OpCode::MULTIPLY: {
      binary_op(std::multiplies());
      break;
    }
    case OpCode::DIVIDE: {
      binary_op(std::divides());
      break;
    }

    case OpCode::NEGATE: {
      push(-pop());
      break;
    }

    case OpCode::RETURN: {
      print_value(pop());
      std::cout << "\n";
      return InterpretResult::OK;
    }

    default:
      return InterpretResult::RUNTIME_ERROR;
    }
  }
}

} // namespace

void init_vm() { reset_stack(); }

void free_vm() {}

InterpretResult interpret(Chunk const *chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code.data();
  return run();
}
