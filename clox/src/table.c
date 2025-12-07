#include "table.h"

#include "memory.h"

static float const TABLE_MAX_LOAD = 0.75;

static inline void free_array(Entry *array, int32_t old_count) {
  reallocate(array, sizeof(Value) * old_count, 0);
}

static Entry *find_entry(Entry *entries, int capacity, ObjString const *key) {
  uint32_t index = key->hash % capacity;
  for (;;) {
    Entry *entry = &entries[index];
    if (entry->key == key || entry->key == NULL) {
      return entry;
    }

    index = (index + 1) % capacity;
  }
}

static void adjust_capacity(Table *table, size_t capacity) {
  Entry *entries = allocate(sizeof(Entry), capacity);
  for (size_t i = 0; i < capacity; ++i) {
    entries[i].key = NULL;
    entries[i].value = nil_value();
  }

  for (size_t i = 0; i < table->capacity; ++i) {
    Entry *entry = &table->entries[i];
    if (entry->key == NULL) {
      continue;
    }

    Entry *dest = find_entry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
  }

  free_array(table->entries, table->capacity);

  table->entries = entries;
  table->capacity = capacity;
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

bool table_get(Table const *table, ObjString const *key, Value *value) {
  if (table->count == 0) {
    return false;
  }

  Entry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL) {
    return false;
  }

  *value = entry->value;
  return true;
}

bool table_set(Table *table, ObjString *key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    size_t capacity = grow_capacity(table->capacity);
    adjust_capacity(table, capacity);
  }

  Entry *entry = find_entry(table->entries, table->capacity, key);
  bool is_new_key = entry->key == NULL;
  if (is_new_key) {
    table->count++;
  }

  entry->key = key;
  entry->value = value;
  return is_new_key;
}

void table_add_all(Table const *from, Table *to) {
  for (size_t i = 0; i < from->capacity; ++i) {
    Entry *entry = &from->entries[i];
    if (entry->key != NULL) {
      table_set(to, entry->key, entry->value);
    }
  }
}
