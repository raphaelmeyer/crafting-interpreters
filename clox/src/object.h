#pragma once

#include "value.h"

#include <stddef.h>

typedef enum ObjType_t {
  OBJ_STRING,
} ObjType;

struct Obj_t {
  ObjType type;
};

typedef struct ObjString_t {
  Obj obj;
  size_t length;
  char const *chars;
} ObjString;

static inline ObjType obj_type(Value const value) { return value.as.obj->type; }

static inline bool is_obj_type(Value const value, ObjType type) {
  return is_obj(value) && value.as.obj->type == type;
}

static inline bool is_string(Value const value) {
  return is_obj_type(value, OBJ_STRING);
}

static inline ObjString *as_string(Value const value) {
  return (ObjString *)value.as.obj;
}

static inline char const *as_cstring(Value const value) {
  return ((ObjString *)value.as.obj)->chars;
}
