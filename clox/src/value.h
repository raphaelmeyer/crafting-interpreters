#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum ValueType_t { VAL_BOOL, VAL_NIL, VAL_NUMBER } ValueType;

typedef struct Value_t {
  ValueType type;
  union Storage {
    bool boolean;
    double number;
  } as;
} Value;

typedef struct ValueArray_t {
  int32_t capacity;
  int32_t count;
  Value *values;
} ValueArray;

inline bool is_bool(Value const value) { return value.type == VAL_BOOL; }
inline bool is_nil(Value const value) { return value.type == VAL_NIL; }
inline bool is_number(Value const value) { return value.type == VAL_NUMBER; }

inline Value bool_value(bool value) {
  return (Value){VAL_BOOL, {.boolean = value}};
}

inline Value nil_value() { return (Value){VAL_NIL, {.number = 0}}; }

inline Value number_value(double value) {
  return (Value){VAL_NUMBER, {.number = value}};
}

void init_value_array(ValueArray *array);
void write_value_array(ValueArray *array, Value value);
void free_value_array(ValueArray *array);

void print_value(Value value);

#ifdef __cplusplus
} // extern "C"
#endif
