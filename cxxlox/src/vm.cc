#include "vm.h"

#include "compiler.h"
#include "debug.h"

#include <functional>
#include <iostream>

namespace {

class LoxVM final : public VM {
public:
  LoxVM() { init_vm(); }
  ~LoxVM() { free_vm(); }

  InterpretResult interpret(std::string_view source) override;

private:
  void init_vm();
  void free_vm();

  void reset_stack();
  void push(Value value);
  Value pop();

  std::uint8_t read_byte();
  Value read_constant();
  OpCode read_opcode();

  InterpretResult run();

  template <typename Op> void binary_op(Op op) {
    auto const b = pop();
    auto const a = pop();
    push(op(a, b));
  }

  constexpr static std::size_t const STACK_MAX = 256;

  struct Context {
    Chunk const *chunk;
    std::uint8_t const *ip;
    std::array<Value, STACK_MAX> stack;
    Value *stack_top;
  };

  Context vm;
};

void LoxVM::init_vm() { reset_stack(); }

void LoxVM::free_vm() {}

void LoxVM::reset_stack() { vm.stack_top = vm.stack.data(); }

void LoxVM::push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

Value LoxVM::pop() {
  vm.stack_top--;
  return *vm.stack_top;
}

std::uint8_t LoxVM::read_byte() { return *vm.ip++; }

Value LoxVM::read_constant() { return vm.chunk->constants[read_byte()]; }

OpCode LoxVM::read_opcode() { return static_cast<OpCode>(read_byte()); }

InterpretResult LoxVM::run() {
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

    auto const instruction = read_opcode();
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

InterpretResult LoxVM::interpret(std::string_view source) {
  Chunk chunk{};

  if (not compile(source, chunk)) {
    return InterpretResult::COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code.data();

  auto const result = run();

  return result;
}

} // namespace

std::unique_ptr<VM> VM::create() { return std::make_unique<LoxVM>(); }
