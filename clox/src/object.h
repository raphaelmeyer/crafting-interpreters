#pragma once

#include "chunk.h"
#include "table.h"
#include "value.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct VM_t VM;

typedef enum ObjType_t {
  OBJ_CLASS,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE,
} ObjType;

typedef struct Obj_t {
  ObjType type;
  bool is_marked;
  Obj *next;
} Obj;

typedef struct ObjFunction_t {
  Obj obj;
  int arity;
  size_t upvalue_count;
  Chunk chunk;
  ObjString *name;
} ObjFunction;

typedef Value (*NativeFn)(int arg_count, Value *args);

typedef struct ObjNative_t {
  Obj obj;
  NativeFn function;
} ObjNative;

typedef struct ObjString_t {
  Obj obj;
  size_t length;
  char const *chars;
  uint32_t hash;
} ObjString;

typedef struct ObjUpvalue_t {
  Obj obj;
  Value *location;
  Value closed;
  struct ObjUpvalue_t *next;
} ObjUpvalue;

typedef struct ObjClosure_t {
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  size_t upvalue_count;
} ObjClosure;

typedef struct ObjClass_t {
  Obj obj;
  ObjString *name;
} ObjClass;

typedef struct ObjInstance_t {
  Obj obj;
  ObjClass *klass;
  Table fields;
} ObjInstance;

ObjClass *new_class(ObjString *name);
ObjClosure *new_closure(ObjFunction *function);
ObjFunction *new_function();
ObjInstance *new_instance(ObjClass *klass);
ObjNative *new_native(NativeFn function);
ObjString *take_string(char *chars, size_t length);
ObjString *copy_string(char const *chars, size_t length);
ObjUpvalue *new_upvalue(Value *slot);
void print_object(Value const value);
void init_object_allocation(VM *vm_instance);

static inline ObjType obj_type(Value const value) { return value.as.obj->type; }

static inline bool is_obj_type(Value const value, ObjType type) {
  return is_obj(value) && value.as.obj->type == type;
}

static inline bool is_class(Value const value) {
  return is_obj_type(value, OBJ_CLASS);
}

static inline bool is_closure(Value const value) {
  return is_obj_type(value, OBJ_CLOSURE);
}

static inline bool is_function(Value const value) {
  return is_obj_type(value, OBJ_FUNCTION);
}

static inline bool is_instance(Value const value) {
  return is_obj_type(value, OBJ_INSTANCE);
}

static inline bool is_native(Value const value) {
  return is_obj_type(value, OBJ_NATIVE);
}

static inline bool is_string(Value const value) {
  return is_obj_type(value, OBJ_STRING);
}

static inline ObjClass *as_class(Value const value) {
  return (ObjClass *)value.as.obj;
}

static inline ObjClosure *as_closure(Value const value) {
  return (ObjClosure *)value.as.obj;
}

static inline ObjFunction *as_function(Value const value) {
  return (ObjFunction *)value.as.obj;
}

static inline ObjInstance *as_instance(Value const value) {
  return (ObjInstance *)value.as.obj;
}

static inline NativeFn as_native(Value const value) {
  return ((ObjNative *)value.as.obj)->function;
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
