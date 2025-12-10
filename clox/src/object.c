#include "object.h"

#include "memory.h"
#include "vm.h"

#include <stdio.h>
#include <string.h>

static VM *vm = NULL;

static inline void free_char_array(char *array, int32_t old_count) {
  reallocate(array, sizeof(Value) * old_count, 0);
}

static Obj *allocate_object(size_t size, ObjType type) {
  Obj *object = reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm->objects;
  vm->objects = object;
  return object;
}

static ObjString *allocate_string(char const *chars, size_t length,
                                  uint32_t hash) {
  ObjString *string =
      (ObjString *)allocate_object(sizeof(ObjString), OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
  table_set(&vm->strings, string, nil_value());
  return string;
}

static uint32_t hash_string(char const *key, size_t length) {
  uint32_t hash = 2166136261u;
  for (size_t i = 0; i < length; ++i) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

ObjString *take_string(char *chars, size_t length) {
  uint32_t hash = hash_string(chars, length);
  ObjString *interned = table_find_string(&vm->strings, chars, length, hash);
  if (interned != NULL) {
    free_char_array(chars, length + 1);
    return interned;
  }

  return allocate_string(chars, length, hash);
}

ObjString *copy_string(char const *chars, size_t length) {
  uint32_t hash = hash_string(chars, length);
  ObjString *interned = table_find_string(&vm->strings, chars, length, hash);
  if (interned != NULL) {
    return interned;
  }

  char *heap_chars = allocate(sizeof(char), length + 1);
  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return allocate_string(heap_chars, length, hash);
}

void print_object(Value const value) {
  switch (obj_type(value)) {
  case OBJ_STRING:
    printf("%s", as_cstring(value));
    break;
  }
}

void init_object_allocation(VM *vm_instance) { vm = vm_instance; }
