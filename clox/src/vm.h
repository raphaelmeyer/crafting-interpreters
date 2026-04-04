#pragma once

#include "object.h"
#include "table.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

static constexpr const size_t UINT8_COUNT = UINT8_MAX + 1;
static constexpr size_t const FRAMES_MAX = 64;
static constexpr size_t const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

typedef struct CallFrame_t {
  ObjClosure *closure;
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
  ObjUpvalue *open_upvalues;

  size_t bytes_allocated;
  size_t next_gc;
  Obj *objects;
  size_t gray_count;
  size_t gray_capacity;
  Obj **gray_stack;
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
