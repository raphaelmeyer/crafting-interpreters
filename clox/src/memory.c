#include "memory.h"

#include <stdlib.h>

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
