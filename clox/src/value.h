#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef double Value;

typedef struct ValueArray_t {
  int32_t capacity;
  int32_t count;
  Value *values;
} ValueArray;

void init_value_array(ValueArray *array);
void write_value_array(ValueArray *array, Value value);
void free_value_array(ValueArray *array);

#ifdef __cplusplus
} // extern "C"
#endif
