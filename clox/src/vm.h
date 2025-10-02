#pragma once

#include "chunk.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

static constexpr size_t const STACK_MAX = 256;

typedef struct VM_t {
  Chunk const *chunk;
  uint8_t *ip;
  Value stack[STACK_MAX];
  Value *stack_top;
} VM;

typedef enum InterpretResult_t {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void init_vm();
void free_vm();
InterpretResult interpret(char const *source);

#ifdef __cplusplus
} // extern "C"
#endif
