#include "chunk.h"

#include "memory.h"

static inline int32_t grow_capacity(int32_t capacity) {
  return capacity < 8 ? 8 : capacity * 2;
}

static inline uint8_t *grow_array(uint8_t *array, int32_t old_count,
                                  int32_t new_count) {
  return (uint8_t *)reallocate(array, sizeof(uint8_t) * old_count,
                               sizeof(uint8_t) * new_count);
}

static inline void free_array(uint8_t *array, int32_t old_count) {
  reallocate(array, sizeof(uint8_t) * old_count, 0);
}

void init_chunk(Chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
}

void free_chunk(Chunk *chunk) {
  free_array(chunk->code, chunk->capacity);
  init_chunk(chunk);
}

void write_chunk(Chunk *chunk, uint8_t byte) {
  if (chunk->capacity < chunk->count + 1) {
    int32_t const old_capacity = chunk->capacity;
    chunk->capacity = grow_capacity(old_capacity);
    chunk->code = grow_array(chunk->code, old_capacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  chunk->count++;
}
