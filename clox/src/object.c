#include "object.h"

#include "memory.h"

#include <stdio.h>
#include <string.h>

static Obj *allocate_object(size_t size, ObjType type) {
  Obj *object = reallocate(NULL, 0, size);
  object->type = type;
  return object;
}

static ObjString *allocate_string(char const *chars, size_t length) {
  ObjString *string =
      (ObjString *)allocate_object(sizeof(ObjString), OBJ_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

ObjString *take_string(char const *chars, size_t length) {
  return allocate_string(chars, length);
}

ObjString *copy_string(char const *chars, size_t length) {
  char *heap_chars = allocate(sizeof(char), length + 1);
  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return allocate_string(heap_chars, length);
}

void print_object(Value const value) {
  switch (obj_type(value)) {
  case OBJ_STRING:
    printf("%s", as_cstring(value));
    break;
  }
}
