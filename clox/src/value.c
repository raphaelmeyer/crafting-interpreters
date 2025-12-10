#include "value.h"

#include "memory.h"
#include "object.h"

#include <stdio.h>
#include <string.h>

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

void print_value(Value value) {
  switch (value.type) {
  case VAL_BOOL:
    printf(value.as.boolean ? "true" : "false");
    break;
  case VAL_NIL:
    printf("nil");
    break;
  case VAL_NUMBER:
    printf("%g", value.as.number);
    break;
  case VAL_OBJ:
    print_object(value);
    break;
  }
}

bool values_equal(Value a, Value b) {
  if (a.type != b.type) {
    return false;
  }
  switch (a.type) {
  case VAL_BOOL:
    return a.as.boolean == b.as.boolean;
  case VAL_NIL:
    return true;
  case VAL_NUMBER:
    return a.as.number == b.as.number;
  case VAL_OBJ: {
    return a.as.obj == b.as.obj;
  }

  default:
    return false;
  }
}
