#include "memory.h"

#include <stdlib.h>

static void free_string(ObjString *string) {
  reallocate((char *)string->chars, sizeof(char) * (string->length + 1), 0);
  reallocate(string, sizeof(ObjString), 0);
}

static void free_function(ObjFunction *function) {
  reallocate(function, sizeof(ObjFunction), 0);
}

static void free_object(Obj *object) {
  switch (object->type) {
  case OBJ_STRING: {
    free_string((ObjString *)object);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    free_chunk(&function->chunk);
    free_function(function);
    break;
  }
  }
}

void *allocate(size_t item_size, size_t count) {
  return reallocate(NULL, 0, item_size * count);
}

void *reallocate(void *pointer, size_t, size_t new_size) {
  if (new_size == 0) {
    free(pointer);
    return NULL;
  }

  void *result = realloc(pointer, new_size);
  if (result == NULL) {
    exit(1);
  }

  return result;
}

void free_objects(Obj *head) {
  Obj *object = head;
  while (object != NULL) {
    Obj *next = object->next;
    free_object(object);
    object = next;
  }
}
