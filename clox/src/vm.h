#pragma once

#include "chunk.h"
#include "object.h"
#include "table.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

static constexpr size_t const FRAMES_MAX = 64;
static constexpr size_t const STACK_MAX = 256;

typedef struct CallFrame_t {
  ObjFunction *function;
  uint8_t *ip;
  Value *slots;
} CallFrame;

typedef struct VM_t {
  CallFrame frames[FRAMES_MAX];
  size_t frame_count;

  Value stack[STACK_MAX];
  Value *stack_top;
  Table globals;
  Table strings;
  Obj *objects;
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
