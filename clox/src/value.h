#pragma once

#include <stdint.h>
#include <string.h>

#define NAN_BOXING

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Obj_t Obj;
typedef struct ObjString_t ObjString;

#ifdef NAN_BOXING

typedef uint64_t Value;

static_assert(sizeof(Value) == sizeof(double));

constexpr uint64_t QNAN = 0x7ffc000000000000ull;
constexpr uint64_t SIGN_BIT = 0x8000000000000000ull;

constexpr uint64_t TAG_NIL = 0b01;
constexpr uint64_t TAG_TRUE = 0b10;
constexpr uint64_t TAG_FALSE = 0b11;

constexpr Value NIL_VALUE = (Value)(QNAN | TAG_NIL);
constexpr Value FALSE_VALUE = (Value)(QNAN | TAG_FALSE);
constexpr Value TRUE_VALUE = (Value)(QNAN | TAG_TRUE);

static inline Value number_to_value(double number) {
  Value value;
  memcpy(&value, &number, sizeof(double));
  return value;
}

static inline double value_to_number(Value value) {
  double number;
  memcpy(&number, &value, sizeof(Value));
  return number;
}

static inline bool is_number(Value value) { return (value & QNAN) != QNAN; }
static inline bool is_nil(Value value) { return value == NIL_VALUE; }
static inline bool is_bool(Value value) {
  return value == TRUE_VALUE || value == FALSE_VALUE;
}
static inline bool is_obj(Value value) {
  return (value & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
}

static inline Value number_value(double value) {
  return number_to_value(value);
}
static inline Value nil_value() { return NIL_VALUE; }
static inline Value bool_value(bool value) {
  return value ? TRUE_VALUE : FALSE_VALUE;
}
static inline Value obj_value(void *object) {
  return (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(object));
}

static inline double as_number(Value value) { return value_to_number(value); }
static inline bool as_bool(Value value) { return value == TRUE_VALUE; }
static inline Obj *as_obj(Value value) {
  return (Obj *)(uintptr_t)(value & (~(SIGN_BIT | QNAN)));
}

#else

typedef enum ValueType_t { VAL_BOOL, VAL_NIL, VAL_NUMBER, VAL_OBJ } ValueType;

typedef struct Value_t {
  ValueType type;
  union Storage {
    bool boolean;
    double number;
    Obj *obj;
  } as;
} Value;

inline bool is_bool(Value const value) { return value.type == VAL_BOOL; }
inline bool is_nil(Value const value) { return value.type == VAL_NIL; }
inline bool is_number(Value const value) { return value.type == VAL_NUMBER; }
inline bool is_obj(Value const value) { return value.type == VAL_OBJ; }

inline Value bool_value(bool value) {
  return (Value){VAL_BOOL, {.boolean = value}};
}

inline Value nil_value() { return (Value){VAL_NIL, {.number = 0}}; }

inline Value number_value(double value) {
  return (Value){VAL_NUMBER, {.number = value}};
}

inline Value obj_value(void *object) {
  return (Value){VAL_OBJ, {.obj = (Obj *)object}};
}

static inline bool as_bool(Value value) { return value.as.boolean; }
static inline double as_number(Value value) { return value.as.number; }
static inline Obj *as_obj(Value value) { return value.as.obj; }

#endif

typedef struct ValueArray_t {
  int32_t capacity;
  int32_t count;
  Value *values;
} ValueArray;

bool values_equal(Value a, Value b);
void init_value_array(ValueArray *array);
void write_value_array(ValueArray *array, Value value);
void free_value_array(ValueArray *array);

void print_value(Value value);

#ifdef __cplusplus
} // extern "C"
#endif
