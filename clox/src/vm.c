#include "vm.h"

#include "debug.h"

#include <stdio.h>

static VM vm;

static void reset_stack() { vm.stack_top = vm.stack; }
static void print_stack() {
  printf("          ");
  for (Value *slot = vm.stack; slot < vm.stack_top; ++slot) {
    printf("[ ");
    print_value(*slot);
    printf(" ]");
  }
  printf("\n");
}

static void push(Value value);
static Value pop();

void init_vm() { reset_stack(); }

void free_vm() {}

static inline uint8_t read_byte(VM *vm) { return *vm->ip++; }

static inline Value read_constant(VM *vm) {
  return vm->chunk->constants.values[read_byte(vm)];
}

static inline Value add(Value a, Value b) { return a + b; }
static inline Value subtract(Value a, Value b) { return a - b; }
static inline Value multiply(Value a, Value b) { return a * b; }
static inline Value divide(Value a, Value b) { return a / b; }

static inline void binray_op(Value (*op)(Value, Value)) {
  double b = pop();
  double a = pop();
  push(op(a, b));
}

static InterpretResult run() {
  for (;;) {
    if (DEBUG_TRACE_EXECUTION) {
      print_stack();
      disassemble_instruction(vm.chunk, (int32_t)(vm.ip - vm.chunk->code));
    }

    uint8_t instruction;
    switch (instruction = read_byte(&vm)) {
    case OP_RETURN: {
      print_value(pop());
      printf("\n");
      return INTERPRET_OK;
    }

    case OP_NEGATE: {
      push(-pop());
      break;
    }

    case OP_ADD: {
      binray_op(add);
      break;
    }
    case OP_SUBTRACT: {
      binray_op(subtract);
      break;
    }
    case OP_MULTIPLY: {
      binray_op(multiply);
      break;
    }
    case OP_DIVIDE: {
      binray_op(divide);
      break;
    }

    case OP_CONSTANT: {
      Value const constant = read_constant(&vm);
      push(constant);
      break;
    }
    }
  }
}

InterpretResult interpret(Chunk const *chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code;
  return run();
}

void push(Value value) {
  *vm.stack_top = value;
  vm.stack_top++;
}

Value pop() {
  vm.stack_top--;
  return *vm.stack_top;
}
