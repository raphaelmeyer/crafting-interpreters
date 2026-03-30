#include "memory.h"

#include "debug.h"

#include <stdio.h>
#include <stdlib.h>

static void free_item(size_t size, void *pointer) {
  reallocate(pointer, size, 0);
}

static void free_items(size_t item_size, void *pointer, size_t count) {
  reallocate(pointer, item_size * count, 0);
}

static void free_string(ObjString *string) {
  reallocate((char *)string->chars, sizeof(char) * (string->length + 1), 0);
  reallocate(string, sizeof(ObjString), 0);
}

static void free_object(Obj *object) {
  if (DEBUG_LOG_GC) {
    printf("%p free type %d\n", (void *)object, object->type);
  }

  switch (object->type) {

  case OBJ_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)object;
    free_items(sizeof(ObjUpvalue *), closure->upvalues, closure->upvalue_count);
    free_item(sizeof(ObjClosure), object);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    free_chunk(&function->chunk);
    free_item(sizeof(ObjFunction), object);
    break;
  }
  case OBJ_NATIVE: {
    free_item(sizeof(ObjNative), object);
    break;
  }
  case OBJ_STRING: {
    free_string((ObjString *)object);
    break;
  }
  case OBJ_UPVALUE: {
    free_item(sizeof(ObjUpvalue), object);
    break;
  }
  }
}

void collect_garbage() {
  if (DEBUG_LOG_GC) {
    printf("-- gc begin\n");
  }

  if (DEBUG_LOG_GC) {
    printf("-- gc end\n");
  }
}

void *allocate(size_t item_size, size_t count) {
  return reallocate(NULL, 0, item_size * count);
}

void *reallocate(void *pointer, size_t old_size, size_t new_size) {
  if (new_size > old_size) {
    if (DEBUG_STRESS_GC) {
      collect_garbage();
    }
  }

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
