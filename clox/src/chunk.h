#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum OpCode_t {
  OP_RETURN,
} OpCode;

typedef struct Chunk_t {
  int32_t count;
  int32_t capacity;
  uint8_t *code;
} Chunk;

void init_chunk(Chunk *chunk);
void free_chunk(Chunk *chunk);
void write_chunk(Chunk *chunk, uint8_t byte);

#ifdef __cplusplus
} // extern "C"
#endif
