#pragma once

#include "value.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum OpCode_t {
  OP_CONSTANT,
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
