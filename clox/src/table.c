#include "table.h"

#include "memory.h"

static inline void free_array(Entry *array, int32_t old_count) {
  reallocate(array, sizeof(Value) * old_count, 0);
}

void init_table(Table *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void free_table(Table *table) {
  free_array(table->entries, table->capacity);
  init_table(table);
}
