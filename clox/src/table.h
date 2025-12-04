#pragma once

#include "value.h"

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Entry_t {
  ObjString *key;
  Value value;
} Entry;

typedef struct Table_t {
  size_t count;
  size_t capacity;
  Entry *entries;
} Table;

void init_table(Table *table);
void free_table(Table *table);

#ifdef __cplusplus
} // extern "C"
#endif
