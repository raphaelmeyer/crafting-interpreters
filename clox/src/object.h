#pragma once

#include "value.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct VM_t VM;

typedef enum ObjType_t {
  OBJ_STRING,
} ObjType;

typedef struct Obj_t {
  ObjType type;
  Obj *next;
} Obj;

typedef struct ObjString_t {
  Obj obj;
  size_t length;
  char const *chars;
  uint32_t hash;
} ObjString;

ObjString *take_string(char *chars, size_t length);
ObjString *copy_string(char const *chars, size_t length);
void print_object(Value const value);
void init_object_allocation(VM *vm_instance);

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

#ifdef __cplusplus
} // extern "C"
#endif
