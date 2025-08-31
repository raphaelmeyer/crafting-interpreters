#include "vm.h"

#include <stdio.h>

static VM vm;

void init_vm() {}

void free_vm() {}

static inline uint8_t read_byte(VM *vm) { return *vm->ip++; }

static inline Value read_constant(VM *vm) {
  return vm->chunk->constants.values[read_byte(vm)];
}

static InterpretResult run() {
  for (;;) {
    uint8_t instruction;
    switch (instruction = read_byte(&vm)) {
    case OP_RETURN: {
      return INTERPRET_OK;
    }

    case OP_CONSTANT: {
      Value const constant = read_constant(&vm);
      print_value(constant);
      printf("\n");
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
