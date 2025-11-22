#include "vm.h"

#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static VM vm;

static void reset_stack() { vm.stack_top = vm.stack; }

static void runtime_error(char const *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm.ip - vm.chunk->code - 1;
  int line = vm.chunk->lines[instruction];
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

void init_vm() { reset_stack(); }

void free_vm() {}

static inline uint8_t read_byte(VM *vm) { return *vm->ip++; }

static inline Value read_constant(VM *vm) {
  return vm->chunk->constants.values[read_byte(vm)];
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
  for (;;) {
    if (DEBUG_TRACE_EXECUTION) {
      print_stack();
      disassemble_instruction(vm.chunk, (int32_t)(vm.ip - vm.chunk->code));
    }

    uint8_t instruction;
    switch (instruction = read_byte(&vm)) {

    case OP_CONSTANT: {
      Value const constant = read_constant(&vm);
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

    case OP_RETURN: {
      print_value(pop());
      printf("\n");
      return INTERPRET_OK;
    }
    }
  }
}

InterpretResult interpret(char const *source) {
  Chunk chunk;
  init_chunk(&chunk);

  if (!compile(source, &chunk)) {
    free_chunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code;

  InterpretResult result = run();

  free_chunk(&chunk);
  return result;
}
