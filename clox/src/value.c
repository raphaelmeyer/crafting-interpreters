#include "value.h"

#include "memory.h"

static inline int32_t grow_capacity(int32_t capacity) {
  return capacity < 8 ? 8 : capacity * 2;
}

static inline Value *grow_array(Value *array, int32_t old_count,
                                int32_t new_count) {
  return (Value *)reallocate(array, sizeof(Value) * old_count,
                             sizeof(Value) * new_count);
}

static inline void free_array(Value *array, int32_t old_count) {
  reallocate(array, sizeof(Value) * old_count, 0);
}

void init_value_array(ValueArray *array) {
  array->values = NULL;
  array->capacity = 0;
  array->count = 0;
}

void free_value_array(ValueArray *array) {
  free_array(array->values, array->capacity);
  init_value_array(array);
}

void write_value_array(ValueArray *array, Value value) {
  if (array->capacity < array->count + 1) {
    int32_t const old_capacity = array->capacity;
    array->capacity = grow_capacity(old_capacity);
    array->values = grow_array(array->values, old_capacity, array->capacity);
  }

  array->values[array->count] = value;
  array->count++;
}
