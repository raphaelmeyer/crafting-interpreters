#pragma once

#include "value.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum OpCode_t {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_GLOBAL,
  OP_DEFINE_GLOBAL,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
  OP_RETURN,
} OpCode;

typedef struct Chunk_t {
  int32_t count;
  int32_t capacity;
  uint8_t *code;
  int32_t *lines;
  ValueArray constants;
} Chunk;

void init_chunk(Chunk *chunk);
void free_chunk(Chunk *chunk);
void write_chunk(Chunk *chunk, uint8_t byte, int32_t line);
int32_t add_constant(Chunk *chunk, Value value);

#ifdef __cplusplus
} // extern "C"
#endif
