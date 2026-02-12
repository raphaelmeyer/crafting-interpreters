#include "vm.h"

#include "chunk.h"
#include "compiler.h"
#include "debug.h"

#include <format>
#include <functional>
#include <iostream>
#include <unordered_map>

namespace {

class LoxVM final : public VM {
public:
  LoxVM(std::unique_ptr<Compiler> &&compiler_)
      : compiler{std::move(compiler_)} {
    init_vm();
  }
  ~LoxVM() { free_vm(); }

  InterpretResult interpret(std::string_view source) override;

private:
  void init_vm();
  void free_vm();

  void reset_stack();
  void push(Value value);
  Value pop();
  Value const &peek(size_t distance) const;

  bool is_falsey(Value const &value) const;
  void concatenate();

  template <typename... Args>
  void runtime_error(std::format_string<Args...> format, Args &&...args) {
    std::cerr << std::format(format, args...) << "\n";

    auto const instruction = std::distance(vm.chunk->code.begin(), vm.ip) - 1;
    auto const line = vm.chunk->lines[instruction];
    std::cerr << std::format("[line {:d}] in script", line) << "\n";

    reset_stack();
  }

  std::uint8_t read_byte();
  std::uint16_t read_short();
  Value read_constant();
  OpCode read_opcode();
  std::string read_string();

  InterpretResult run();

  template <typename Type, typename Op>
  InterpretResult binary_op(Type value_type, Op op) {
    if (not is_number(peek(0)) || not is_number(peek(1))) {
      runtime_error("Operands must be numbers.");
      return InterpretResult::RUNTIME_ERROR;
    }
    auto const b = as_number(pop());
    auto const a = as_number(pop());
    push(value_type(op(a, b)));
    return InterpretResult::OK;
  }

  constexpr static std::size_t const STACK_MAX = 256;

  struct Context {
    Chunk const *chunk;
    Chunk::CodeIterator ip;
    std::array<Value, STACK_MAX> stack;
    using StackPointer = decltype(stack)::iterator;
    StackPointer stack_top;
    std::unordered_map<std::string, Value> globals;
  };

  Context vm{};
  std::unique_ptr<Compiler> compiler{Compiler::create()};
};

void LoxVM::init_vm() { reset_stack(); }

void LoxVM::free_vm() {}

void LoxVM::reset_stack() { vm.stack_top = vm.stack.begin(); }

void LoxVM::push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

Value LoxVM::pop() {
  vm.stack_top--;
  return *vm.stack_top;
}

Value const &LoxVM::peek(size_t distance) const {
  return *std::prev(vm.stack_top, 1 + distance);
}

bool LoxVM::is_falsey(Value const &value) const {
  return is_nil(value) || (is_bool(value) && not as_bool(value));
}

void LoxVM::concatenate() {
  auto const b = as_string(pop());
  auto const a = as_string(pop());

  push(string_value(a + b));
}

std::uint8_t LoxVM::read_byte() { return *vm.ip++; }

std::uint16_t LoxVM::read_short() {
  auto const high_byte = *vm.ip++;
  auto const low_byte = *vm.ip++;
  return static_cast<std::uint16_t>((high_byte << 8) | low_byte);
}

Value LoxVM::read_constant() { return vm.chunk->constants[read_byte()]; }

OpCode LoxVM::read_opcode() { return static_cast<OpCode>(read_byte()); }

std::string LoxVM::read_string() { return as_string(read_constant()); }

InterpretResult LoxVM::run() {
  for (;;) {
    if (Debug::TRACE_EXECUTION) {
      std::cout << "          ";
      for (auto slot = vm.stack.begin(); slot < vm.stack_top; ++slot) {
        std::cout << "[ ";
        print_value(*slot);
        std::cout << " ]";
      }
      std::cout << "\n";

      disassemble_instruction(*vm.chunk,
                              std::distance(vm.chunk->code.begin(), vm.ip));
    }

    auto const instruction = read_opcode();
    switch (instruction) {
    case OpCode::CONSTANT: {
      Value constant = read_constant();
      push(constant);
      break;
    }

    case OpCode::NIL:
      push(nil_value());
      break;
    case OpCode::TRUE:
      push(bool_value(true));
      break;
    case OpCode::FALSE:
      push(bool_value(false));
      break;

    case OpCode::POP:
      pop();
      break;

    case OpCode::GET_LOCAL: {
      auto const slot = read_byte();
      push(vm.stack.at(slot));
      break;
    }

    case OpCode::SET_LOCAL: {
      auto const slot = read_byte();
      vm.stack.at(slot) = peek(0);
      break;
    }

    case OpCode::GET_GLOBAL: {
      auto const name = read_string();
      if (not vm.globals.contains(name)) {
        runtime_error("Undefined variable '{}'.", name);
        return InterpretResult::RUNTIME_ERROR;
      }
      auto const value = vm.globals.at(name);
      push(value);
      break;
    }

    case OpCode::DEFINE_GLOBAL: {
      auto const name = read_string();
      vm.globals.insert_or_assign(name, peek(0));
      pop();
      break;
    }

    case OpCode::SET_GLOBAL: {
      auto const name = read_string();
      if (not vm.globals.contains(name)) {
        runtime_error("Undefined variable '{}'.", name);
        return InterpretResult::RUNTIME_ERROR;
      }
      vm.globals.at(name) = peek(0);
      break;
    }

    case OpCode::EQUAL: {
      auto const b = pop();
      auto const a = pop();
      push(bool_value(values_equal(a, b)));
      break;
    }

    case OpCode::GREATER: {
      auto const result = binary_op(bool_value, std::greater());
      if (result != InterpretResult::OK) {
        return result;
      }
      break;
    }
    case OpCode::LESS: {
      auto const result = binary_op(bool_value, std::less());
      if (result != InterpretResult::OK) {
        return result;
      }
      break;
    }

    case OpCode::ADD: {
      if (is_string(peek(0)) && is_string(peek(1))) {
        concatenate();
      } else if (is_number(peek(0)) && is_number(peek(1))) {
        auto const b = as_number(pop());
        auto const a = as_number(pop());
        push(a + b);
      } else {
        runtime_error("Operands must be two numbers or two strings.");
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::SUBTRACT: {
      auto const result = binary_op(number_value, std::minus());
      if (result != InterpretResult::OK) {
        return result;
      }
      break;
    }
    case OpCode::MULTIPLY: {
      auto const result = binary_op(number_value, std::multiplies());
      if (result != InterpretResult::OK) {
        return result;
      }
      break;
    }
    case OpCode::DIVIDE: {
      auto const result = binary_op(number_value, std::divides());
      if (result != InterpretResult::OK) {
        return result;
      }
      break;
    }

    case OpCode::NOT:
      push(bool_value(is_falsey(pop())));
      break;

    case OpCode::NEGATE: {
      if (not is_number(peek(0))) {
        runtime_error("Operand must be a number.");
        return InterpretResult::RUNTIME_ERROR;
      }
      push(number_value(-as_number(pop())));
      break;
    }

    case OpCode::PRINT: {
      print_value(pop());
      std::cout << "\n";
      break;
    }

    case OpCode::JUMP: {
      auto const offset = read_short();
      vm.ip += offset;
      break;
    }

    case OpCode::JUMP_IF_FALSE: {
      auto const offset = read_short();
      if (is_falsey(peek(0))) {
        vm.ip += offset;
      }
      break;
    }

    case OpCode::RETURN: {
      return InterpretResult::OK;
    }

    default:
      return InterpretResult::RUNTIME_ERROR;
    }
  }
}

InterpretResult LoxVM::interpret(std::string_view source) {
  Chunk chunk{};

  if (not compiler->compile(source, chunk)) {
    return InterpretResult::COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code.begin();

  auto const result = run();

  return result;
}

} // namespace

std::unique_ptr<VM> VM::create() {
  return std::make_unique<LoxVM>(Compiler::create());
}
