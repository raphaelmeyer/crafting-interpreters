#pragma once

#include "object.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

void *allocate(size_t item_size, size_t count);
void *reallocate(void *pointer, size_t old_size, size_t new_size);
void free_objects(Obj *head);

#ifdef __cplusplus
} // extern "C"
#endif
