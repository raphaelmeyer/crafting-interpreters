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
bool table_get(Table const *table, ObjString const *key, Value *value);
bool table_set(Table *table, ObjString *key, Value value);
void table_add_all(Table const *from, Table *to);

#ifdef __cplusplus
} // extern "C"
#endif
