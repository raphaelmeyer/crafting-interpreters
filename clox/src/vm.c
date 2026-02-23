#include "vm.h"

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static VM vm;

static void reset_stack() {
  vm.stack_top = vm.stack;
  vm.frame_count = 0;
}

static void runtime_error(char const *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  CallFrame *frame = &vm.frames[vm.frame_count - 1];
  size_t instruction = frame->ip - frame->function->chunk.code - 1;
  int32_t line = frame->function->chunk.lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  reset_stack();
}

static void print_stack() {
  printf("          ");
  for (Value *slot = vm.stack; slot < vm.stack_top; ++slot) {
    printf("[ ");
    print_value(*slot);
    printf(" ]");
  }
  printf("\n");
}

static void push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

static Value pop() {
  vm.stack_top--;
  return *vm.stack_top;
}

static Value peek(int distance) { return vm.stack_top[-1 - distance]; }

static bool is_falsey(Value value) {
  return is_nil(value) || (is_bool(value) && !value.as.boolean);
}

static void concatenate() {
  ObjString *b = as_string(pop());
  ObjString *a = as_string(pop());

  size_t length = a->length + b->length;
  char *chars = allocate(sizeof(char), length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString *result = take_string(chars, length);
  push(obj_value(result));
}

void init_vm() {
  reset_stack();
  init_object_allocation(&vm);
  vm.objects = NULL;

  init_table(&vm.globals);
  init_table(&vm.strings);
}

void free_vm() {
  free_table(&vm.globals);
  free_table(&vm.strings);
  free_objects(vm.objects);
}

static inline uint8_t read_byte(CallFrame *frame) { return *frame->ip++; }

static inline uint16_t read_short(CallFrame *frame) {
  frame->ip += 2;
  return (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]);
}

static inline Value read_constant(CallFrame *frame) {
  return frame->function->chunk.constants.values[read_byte(frame)];
}

static inline ObjString *read_string(CallFrame *frame) {
  return as_string(read_constant(frame));
}

static Value subtract(double a, double b) { return number_value(a - b); }
static Value multiply(double a, double b) { return number_value(a * b); }
static Value divide(double a, double b) { return number_value(a / b); }
static Value greater(double a, double b) { return bool_value(a > b); }
static Value less(double a, double b) { return bool_value(a < b); }

static inline InterpretResult binary_op(Value (*op)(double, double)) {
  if (!is_number(peek(0)) || !is_number(peek(1))) {
    runtime_error("Operands must be numbers.");
    return INTERPRET_RUNTIME_ERROR;
  }
  double b = pop().as.number;
  double a = pop().as.number;
  push(op(a, b));

  return INTERPRET_OK;
}

static InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.frame_count - 1];

  for (;;) {
    if (DEBUG_TRACE_EXECUTION) {
      print_stack();
      disassemble_instruction(
          &frame->function->chunk,
          (int32_t)(frame->ip - frame->function->chunk.code));
    }

    uint8_t instruction;
    switch (instruction = read_byte(frame)) {

    case OP_CONSTANT: {
      Value const constant = read_constant(frame);
      push(constant);
      break;
    }

    case OP_NIL:
      push(nil_value());
      break;
    case OP_TRUE:
      push(bool_value(true));
      break;
    case OP_FALSE:
      push(bool_value(false));
      break;

    case OP_POP:
      pop();
      break;

    case OP_GET_LOCAL: {
      uint8_t slot = read_byte(frame);
      push(frame->slots[slot]);
      break;
    }

    case OP_SET_LOCAL: {
      uint8_t slot = read_byte(frame);
      frame->slots[slot] = peek(0);
      break;
    }

    case OP_GET_GLOBAL: {
      ObjString *name = read_string(frame);
      Value value;
      if (!table_get(&vm.globals, name, &value)) {
        runtime_error("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      push(value);
      break;
    }

    case OP_DEFINE_GLOBAL: {
      ObjString *const name = read_string(frame);
      table_set(&vm.globals, name, peek(0));
      pop();
      break;
    }

    case OP_SET_GLOBAL: {
      ObjString *name = read_string(frame);
      if (table_set(&vm.globals, name, peek(0))) {
        table_delete(&vm.globals, name);
        runtime_error("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }

    case OP_EQUAL: {
      Value b = pop();
      Value a = pop();
      push(bool_value(values_equal(a, b)));
      break;
    }

    case OP_GREATER: {
      InterpretResult result = binary_op(greater);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_LESS: {
      InterpretResult result = binary_op(less);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_ADD: {
      if (is_string(peek(0)) && is_string(peek(1))) {
        concatenate();
      } else if (is_number(peek(0)) && is_number(peek(1))) {
        const double b = pop().as.number;
        const double a = pop().as.number;
        push(number_value(a + b));
      } else {
        runtime_error("Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUBTRACT: {
      InterpretResult result = binary_op(subtract);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_MULTIPLY: {
      InterpretResult result = binary_op(multiply);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }
    case OP_DIVIDE: {
      InterpretResult result = binary_op(divide);
      if (result != INTERPRET_OK) {
        return result;
      }
      break;
    }

    case OP_NOT: {
      push(bool_value(is_falsey(pop())));
      break;
    }

    case OP_NEGATE: {
      if (!is_number(peek(0))) {
        runtime_error("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      push(number_value(-pop().as.number));
      break;
    }

    case OP_PRINT: {
      print_value(pop());
      printf("\n");
      break;
    }

    case OP_JUMP: {
      uint16_t offset = read_short(frame);
      frame->ip += offset;
      break;
    }

    case OP_JUMP_IF_FALSE: {
      uint16_t offset = read_short(frame);
      if (is_falsey(peek(0))) {
        frame->ip += offset;
      }
      break;
    }

    case OP_LOOP: {
      uint16_t offset = read_short(frame);
      frame->ip -= offset;
      break;
    }

    case OP_RETURN: {
      return INTERPRET_OK;
    }
    }
  }
}

InterpretResult interpret(char const *source) {
  ObjFunction *function = compile(source);
  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  push(obj_value(function));
  CallFrame *frame = &vm.frames[vm.frame_count++];
  frame->function = function;
  frame->ip = function->chunk.code;
  frame->slots = vm.stack;

  return run();
}
