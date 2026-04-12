#include "vm.h"

#include "chunk.h"
#include "compiler.h"
#include "debug.h"

#include <chrono>
#include <format>
#include <functional>
#include <iostream>
#include <iterator>
#include <random>
#include <ranges>
#include <unordered_map>

namespace views = std::ranges::views;

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
  constexpr static std::size_t const UINT8_COUNT =
      std::numeric_limits<std::uint8_t>::max() + 1;

  constexpr static std::size_t const FRAMES_MAX = 64;
  constexpr static std::size_t const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

  using Stack = std::array<Value, STACK_MAX>;
  using StackPointer = Stack::iterator;

  struct CallFrame {
    ObjClosure closure;
    CodeIterator ip;
    StackPointer slots;
  };

  struct Context {
    std::array<CallFrame, FRAMES_MAX> frames;
    std::size_t frame_count;

    Stack stack;
    StackPointer stack_top;
    std::unordered_map<std::string, Value> globals;

    std::vector<ObjUpvalue> open_upvalues;
  };

  void init_vm();
  void free_vm();

  void reset_stack();
  void push(Value value);
  Value pop();
  Value const &peek(size_t distance) const;

  bool call(ObjClosure closure, std::size_t arg_count);
  bool call_value(Value const &callee, std::size_t arg_count);
  bool call_native(ObjNative native, std::size_t arg_count);
  ObjUpvalue capture_upvalue(StackPointer local);
  void close_upvalues(StackPointer last);

  bool is_falsey(Value const &value) const;
  void concatenate();

  template <typename... Args>
  void runtime_error(std::format_string<Args...> format, Args &&...args) {
    std::cerr << std::format(format, args...) << "\n";

    for (auto const &frame :
         views::reverse(views::take(vm.frames, vm.frame_count))) {
      auto const function = frame.closure->function;
      auto const instruction =
          std::distance(function->chunk.code.cbegin(), frame.ip) - 1;
      auto const line = frame.closure->function->chunk.lines.at(instruction);
      std::cerr << std::format("[line {:d}] in ", line);
      if (function->name.empty()) {
        std::cerr << "script" << "\n";
      } else {
        std::cerr << std::format("{}()\n", function->name);
      }
    }

    reset_stack();
  }

  void define_native(std::string name, std::size_t arity, NativeFn function) {
    push(name);
    push(new_native(arity, function));
    vm.globals.insert_or_assign(as_string(peek(1)), peek(0));
    pop();
    pop();
  }

  std::uint8_t read_byte(CallFrame &frame);
  std::uint16_t read_short(CallFrame &frame);
  Value read_constant(CallFrame &frame);
  OpCode read_opcode(CallFrame &frame);
  std::string read_string(CallFrame &frame);

  Value &to_value(ObjUpvalue &upvalue);

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

  std::random_device random_device{};
  std::mt19937 rng{random_device()};

  Context vm{};
  std::unique_ptr<Compiler> compiler{Compiler::create()};
};

void LoxVM::init_vm() {
  reset_stack();

  define_native("clock", 0, [](auto, auto) {
    std::chrono::duration<double> now =
        std::chrono::steady_clock::now().time_since_epoch();
    return now.count();
  });

  define_native("random", 2, [this](auto, auto args) {
    auto const from = as_number(args[0]);
    auto const to = as_number(args[1]);
    std::uniform_real_distribution<double> distribution{from, to};

    return distribution(rng);
  });
}

void LoxVM::free_vm() {}

void LoxVM::reset_stack() {
  vm.stack_top = vm.stack.begin();
  vm.frame_count = 0;
  vm.open_upvalues.clear();
}

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

bool LoxVM::call(ObjClosure closure, std::size_t arg_count) {
  if (arg_count != closure->function->arity) {
    runtime_error("Expected {} arguments but got {}.", closure->function->arity,
                  arg_count);
    return false;
  }

  if (vm.frame_count == FRAMES_MAX) {
    runtime_error("Stack overflow.");
    return false;
  }

  auto &frame = vm.frames.at(vm.frame_count++);
  frame.closure = closure;
  frame.ip = closure->function->chunk.code.begin();
  frame.slots = vm.stack_top - arg_count - 1;
  return true;
}

bool LoxVM::call_native(ObjNative native, std::size_t arg_count) {
  if (arg_count != native->arity) {
    runtime_error("Expected {} arguments but got {}.", native->arity,
                  arg_count);
    return false;
  }

  std::span<Value> args{std::prev(vm.stack_top, arg_count), arg_count};
  auto const result = native->function(arg_count, args);
  std::advance(vm.stack_top, -(arg_count + 1));
  push(result);
  return true;
}

bool LoxVM::call_value(Value const &callee, std::size_t arg_count) {
  if (is_class(callee)) {
    auto const klass = as_class(callee);
    auto slot = std::prev(vm.stack_top, arg_count + 1);
    *slot = new_instance(klass);
    return true;
  } else if (is_closure(callee)) {
    return call(as_closure(callee), arg_count);
  } else if (is_native(callee)) {
    return call_native(as_native(callee), arg_count);
  }

  runtime_error("Can only call functions and classes.");
  return false;
}

ObjUpvalue LoxVM::capture_upvalue(StackPointer local) {
  size_t const slot = std::distance(vm.stack.begin(), local);

  auto upvalue = vm.open_upvalues.begin();
  while (upvalue != vm.open_upvalues.end() &&
         (std::get<StackSlot>((*upvalue)->value).from_start > slot)) {
    ++upvalue;
  }

  if (upvalue != vm.open_upvalues.end() &&
      std::get<StackSlot>((*upvalue)->value).from_start == slot) {
    return *upvalue;
  }

  auto created_upvalue = new_upvalue(slot);
  vm.open_upvalues.insert(upvalue, created_upvalue);

  return created_upvalue;
}

void LoxVM::close_upvalues(StackPointer last) {
  std::size_t const last_slot = std::distance(vm.stack.begin(), last);
  auto open_upvalue = vm.open_upvalues.begin();
  while (open_upvalue != vm.open_upvalues.end()) {
    auto const slot = std::get<StackSlot>((*open_upvalue)->value).from_start;
    if (slot < last_slot) {
      break;
    }

    (*open_upvalue)->value = Closed{vm.stack.at(slot)};
    open_upvalue = vm.open_upvalues.erase(open_upvalue);
  }
}

bool LoxVM::is_falsey(Value const &value) const {
  return is_nil(value) || (is_bool(value) && not as_bool(value));
}

void LoxVM::concatenate() {
  auto const b = as_string(pop());
  auto const a = as_string(pop());

  push(string_value(a + b));
}

std::uint8_t LoxVM::read_byte(CallFrame &frame) { return *frame.ip++; }

std::uint16_t LoxVM::read_short(CallFrame &frame) {
  auto const high_byte = *frame.ip++;
  auto const low_byte = *frame.ip++;
  return static_cast<std::uint16_t>((high_byte << 8) | low_byte);
}

Value LoxVM::read_constant(CallFrame &frame) {
  return frame.closure->function->chunk.constants[read_byte(frame)];
}

OpCode LoxVM::read_opcode(CallFrame &frame) {
  return static_cast<OpCode>(read_byte(frame));
}

std::string LoxVM::read_string(CallFrame &frame) {
  return as_string(read_constant(frame));
}

Value &LoxVM::to_value(ObjUpvalue &upvalue) {
  if (std::holds_alternative<StackSlot>(upvalue->value)) {
    auto on_stack = std::get<StackSlot>(upvalue->value);
    return vm.stack.at(on_stack.from_start);
  } else {
    auto &closed = std::get<Closed>(upvalue->value);
    return closed.closed;
  }
}

InterpretResult LoxVM::run() {
  auto frame = vm.frames.begin() + vm.frame_count - 1;

  if (Debug::TRACE_EXECUTION) {
    std::cout << "== run vm ==" << "\n";
  }

  for (;;) {
    if (Debug::TRACE_EXECUTION) {
      std::cout << "          ";
      for (auto slot = vm.stack.begin(); slot < vm.stack_top; ++slot) {
        std::cout << "[ ";
        print_value(*slot);
        std::cout << " ]";
      }
      std::cout << "\n";

      disassemble_instruction(
          frame->closure->function->chunk,
          std::distance(frame->closure->function->chunk.code.cbegin(),
                        frame->ip));
    }

    auto const instruction = read_opcode(*frame);
    switch (instruction) {
    case OpCode::CONSTANT: {
      Value constant = read_constant(*frame);
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
      auto const slot = read_byte(*frame);
      push(*(frame->slots + slot));
      break;
    }

    case OpCode::SET_LOCAL: {
      auto const slot = read_byte(*frame);
      *(frame->slots + slot) = peek(0);
      break;
    }

    case OpCode::GET_GLOBAL: {
      auto const name = read_string(*frame);
      if (not vm.globals.contains(name)) {
        runtime_error("Undefined variable '{}'.", name);
        return InterpretResult::RUNTIME_ERROR;
      }
      auto const value = vm.globals.at(name);
      push(value);
      break;
    }

    case OpCode::DEFINE_GLOBAL: {
      auto const name = read_string(*frame);
      vm.globals.insert_or_assign(name, peek(0));
      pop();
      break;
    }

    case OpCode::SET_GLOBAL: {
      auto const name = read_string(*frame);
      if (not vm.globals.contains(name)) {
        runtime_error("Undefined variable '{}'.", name);
        return InterpretResult::RUNTIME_ERROR;
      }
      vm.globals.at(name) = peek(0);
      break;
    }

    case OpCode::GET_UPVALUE: {
      auto const slot = read_byte(*frame);
      push(to_value(frame->closure->upvalues.at(slot)));
      break;
    }

    case OpCode::SET_UPVALUE: {
      auto const slot = read_byte(*frame);
      to_value(frame->closure->upvalues.at(slot)) = peek(0);
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
      auto const offset = read_short(*frame);
      frame->ip += offset;
      break;
    }

    case OpCode::JUMP_IF_FALSE: {
      auto const offset = read_short(*frame);
      if (is_falsey(peek(0))) {
        frame->ip += offset;
      }
      break;
    }

    case OpCode::LOOP: {
      auto const offset = read_short(*frame);
      frame->ip -= offset;
      break;
    }

    case OpCode::CALL: {
      auto const arg_count = read_byte(*frame);
      if (not call_value(peek(arg_count), arg_count)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      frame = vm.frames.begin() + vm.frame_count - 1;
      break;
    }

    case OpCode::CLOSURE: {
      auto function = as_function(read_constant(*frame));
      auto closure = new_closure(function);
      push(closure);

      for (std::size_t i = 0; i < closure->upvalues.size(); ++i) {
        auto const is_local = read_byte(*frame);
        auto const index = read_byte(*frame);
        if (is_local) {
          closure->upvalues.at(i) =
              capture_upvalue(std::next(frame->slots, index));
        } else {
          closure->upvalues.at(i) = frame->closure->upvalues.at(index);
        }
      }

      break;
    }

    case OpCode::CLOSE_UPVALUE: {
      close_upvalues(std::prev(vm.stack_top));
      pop();
      break;
    }

    case OpCode::RETURN: {
      auto const result = pop();
      close_upvalues(frame->slots);
      vm.frame_count--;
      if (vm.frame_count == 0) {
        pop();
        return InterpretResult::OK;
      }

      vm.stack_top = frame->slots;
      push(result);
      frame = vm.frames.begin() + vm.frame_count - 1;
      break;
    }

    case OpCode::CLASS:
      push(new_class(read_string(*frame)));
      break;

    default:
      return InterpretResult::RUNTIME_ERROR;
    }
  }
}

InterpretResult LoxVM::interpret(std::string_view source) {
  ObjFunction function = compiler->compile(source);
  if (function == nullptr) {
    return InterpretResult::COMPILE_ERROR;
  }

  push(function);
  auto closure = new_closure(function);
  pop();
  push(closure);
  call(closure, 0);

  return run();
}

} // namespace

std::unique_ptr<VM> VM::create() {
  return std::make_unique<LoxVM>(Compiler::create());
}
