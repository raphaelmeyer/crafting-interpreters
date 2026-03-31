#pragma once

#include "object.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

struct VM;

void *allocate(size_t item_size, size_t count);
void *reallocate(void *pointer, size_t old_size, size_t new_size);
void mark_object(Obj *object);
void mark_value(Value value);
void collect_garbage();
void free_objects(Obj *head);

void init_garbage_collector(VM *vm_instance);

static inline int32_t grow_capacity(int32_t capacity) {
  return capacity < 8 ? 8 : capacity * 2;
}

#ifdef __cplusplus
} // extern "C"
#endif
