#pragma once

#include "chunk.h"

typedef struct VM_t {
  Chunk const *chunk;
  uint8_t *ip;
} VM;

typedef enum InterpretResult_t {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void init_vm();
void free_vm();
InterpretResult interpret(Chunk const *chunk);
