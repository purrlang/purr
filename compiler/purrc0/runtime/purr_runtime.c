#include "purr_runtime.h"
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/* --- Basic runtime --- */

void runtimeInit(void) {
  /* Future: scheduler, heap init */
}

void print_string(const char* s) { printf("%s\n", s); }
void print_i32(int32_t n)        { printf("%d\n", n); }
void print_i64(int64_t n)        { printf("%lld\n", (long long)n); }
void print_bool(_Bool b)         { printf("%s\n", b ? "true" : "false"); }

int32_t char_at(const char* s, int32_t index) {
  if (!s || index < 0) return -1;
  int i = 0;
  while (s[i]) { if (i == index) return (int32_t)(unsigned char)s[i]; i++; }
  return -1;
}

int64_t abs_i64(int64_t n) { return n < 0 ? -n : n; }

/* ============================================================================
   List Implementation
   ============================================================================ */

PurrList* purr_list_create(int64_t initial_capacity) {
  if (initial_capacity <= 0) initial_capacity = 10;

  PurrList* list = (PurrList*)malloc(sizeof(PurrList));
  if (!list) return NULL;

  list->data = (void**)malloc(sizeof(void*) * initial_capacity);
  if (!list->data) {
    free(list);
    return NULL;
  }

  list->capacity = initial_capacity;
  list->length = 0;
  return list;
}

void purr_list_free(PurrList* list) {
  if (!list) return;
  free(list->data);
  free(list);
}

static void purr_list_grow(PurrList* list) {
  int64_t new_capacity = list->capacity * 2;
  void** new_data = (void**)realloc(list->data, sizeof(void*) * new_capacity);

  if (!new_data) {
    /* Growth failed, but keep existing list intact */
    return;
  }

  list->data = new_data;
  list->capacity = new_capacity;
}

void purr_list_push(PurrList* list, void* element) {
  if (!list) return;

  if (list->length >= list->capacity) {
    purr_list_grow(list);
  }

  list->data[list->length++] = element;
}

void* purr_list_pop(PurrList* list) {
  if (!list || list->length == 0) return NULL;
  return list->data[--list->length];
}

void* purr_list_get(PurrList* list, int64_t index) {
  if (!list || index < 0 || index >= list->length) return NULL;
  return list->data[index];
}

void purr_list_set(PurrList* list, int64_t index, void* element) {
  if (!list || index < 0 || index >= list->length) return;
  list->data[index] = element;
}

int64_t purr_list_length(PurrList* list) {
  if (!list) return 0;
  return list->length;
}

void purr_list_clear(PurrList* list) {
  if (!list) return;
  list->length = 0;
}

/* ============================================================================
   Fixed Array Implementation
   ============================================================================ */

PurrFixed* purr_fixed_create(void* data, int64_t size) {
  PurrFixed* fixed = (PurrFixed*)malloc(sizeof(PurrFixed));
  if (!fixed) return NULL;

  fixed->data = data;
  fixed->length = size;
  return fixed;
}

void purr_fixed_free(PurrFixed* fixed) {
  if (!fixed) return;
  /* Note: we don't free the data since it's typically stack-allocated */
  free(fixed);
}

void* purr_fixed_get(PurrFixed* fixed, int64_t index) {
  if (!fixed || index < 0 || index >= fixed->length) return NULL;

  /* Assuming fixed->data points to an array of void* pointers */
  void** array = (void**)fixed->data;
  return array[index];
}

void purr_fixed_set(PurrFixed* fixed, int64_t index, void* element) {
  if (!fixed || index < 0 || index >= fixed->length) return;

  void** array = (void**)fixed->data;
  array[index] = element;
}

int64_t purr_fixed_length(PurrFixed* fixed) {
  if (!fixed) return 0;
  return fixed->length;
}

/* ============================================================================
   Slice Implementation
   ============================================================================ */

PurrSlice* purr_slice_create(void* data, int64_t start, int64_t length) {
  PurrSlice* slice = (PurrSlice*)malloc(sizeof(PurrSlice));
  if (!slice) return NULL;

  slice->data = data;
  slice->start = start;
  slice->length = length;
  return slice;
}

void purr_slice_free(PurrSlice* slice) {
  if (!slice) return;
  free(slice);
}

void* purr_slice_get(PurrSlice* slice, int64_t index) {
  if (!slice || index < 0 || index >= slice->length) return NULL;

  void** array = (void**)slice->data;
  return array[slice->start + index];
}

void purr_slice_set(PurrSlice* slice, int64_t index, void* element) {
  if (!slice || index < 0 || index >= slice->length) return;

  void** array = (void**)slice->data;
  array[slice->start + index] = element;
}

int64_t purr_slice_length(PurrSlice* slice) {
  if (!slice) return 0;
  return slice->length;
}

/* ============================================================================
   Map Implementation (Simple Hash Table)
   ============================================================================ */

static int64_t purr_map_find_slot(PurrMap* map, void* key,
                                   int (*key_hash)(void*),
                                   int (*key_equals)(void*, void*)) {
  if (!map || !key_hash || !key_equals) return -1;

  int64_t hash = key_hash(key);
  int64_t slot = hash % map->capacity;
  int64_t start_slot = slot;

  /* Linear probing to find the slot */
  while (map->entries[slot].used) {
    if (key_equals(map->entries[slot].key, key)) {
      return slot;  /* Found exact key */
    }
    slot = (slot + 1) % map->capacity;
    if (slot == start_slot) {
      /* Table is full, no empty slot found */
      return -1;
    }
  }

  return slot;  /* Found empty slot */
}

static void purr_map_resize(PurrMap* map,
                            int (*key_hash)(void*),
                            int (*key_equals)(void*, void*)) {
  if (!map) return;

  int64_t old_capacity = map->capacity;
  PurrMapEntry* old_entries = map->entries;

  /* Double the capacity */
  map->capacity *= 2;
  map->entries = (PurrMapEntry*)calloc(map->capacity, sizeof(PurrMapEntry));
  if (!map->entries) {
    /* Resize failed, restore old state */
    map->capacity = old_capacity;
    map->entries = old_entries;
    return;
  }

  map->count = 0;

  /* Rehash all entries into the new table */
  for (int64_t i = 0; i < old_capacity; i++) {
    if (old_entries[i].used) {
      purr_map_insert(map, old_entries[i].key, old_entries[i].value,
                      key_hash, key_equals);
    }
  }

  free(old_entries);
}

PurrMap* purr_map_create(int64_t initial_capacity) {
  if (initial_capacity <= 0) initial_capacity = 16;

  PurrMap* map = (PurrMap*)malloc(sizeof(PurrMap));
  if (!map) return NULL;

  map->entries = (PurrMapEntry*)calloc(initial_capacity, sizeof(PurrMapEntry));
  if (!map->entries) {
    free(map);
    return NULL;
  }

  map->capacity = initial_capacity;
  map->count = 0;
  return map;
}

void purr_map_free(PurrMap* map) {
  if (!map) return;
  free(map->entries);
  free(map);
}

void purr_map_insert(PurrMap* map, void* key, void* value,
                     int (*key_hash)(void*),
                     int (*key_equals)(void*, void*)) {
  if (!map || !key || !key_hash || !key_equals) return;

  /* Check if we need to resize (load factor > 0.75) */
  if (map->count * 4 > map->capacity * 3) {
    purr_map_resize(map, key_hash, key_equals);
  }

  int64_t slot = purr_map_find_slot(map, key, key_hash, key_equals);
  if (slot < 0) return;  /* Insertion failed */

  int is_new = !map->entries[slot].used;

  map->entries[slot].key = key;
  map->entries[slot].value = value;
  map->entries[slot].used = 1;

  if (is_new) {
    map->count++;
  }
}

void* purr_map_get(PurrMap* map, void* key,
                   int (*key_hash)(void*),
                   int (*key_equals)(void*, void*)) {
  if (!map || !key || !key_hash || !key_equals) return NULL;

  int64_t slot = purr_map_find_slot(map, key, key_hash, key_equals);
  if (slot < 0 || !map->entries[slot].used) return NULL;

  return map->entries[slot].value;
}

void purr_map_remove(PurrMap* map, void* key,
                     int (*key_hash)(void*),
                     int (*key_equals)(void*, void*)) {
  if (!map || !key || !key_hash || !key_equals) return;

  int64_t slot = purr_map_find_slot(map, key, key_hash, key_equals);
  if (slot < 0 || !map->entries[slot].used) return;

  map->entries[slot].used = 0;
  map->count--;
}

int64_t purr_map_count(PurrMap* map) {
  if (!map) return 0;
  return map->count;
}

void purr_map_clear(PurrMap* map) {
  if (!map) return;

  for (int64_t i = 0; i < map->capacity; i++) {
    map->entries[i].used = 0;
  }

  map->count = 0;
}

/* ============================================================================
   Purr-level API wrappers
   ============================================================================ */

/* --- list<T> wrappers --- */

PurrList* list_new(void) {
  return purr_list_create(16);
}

void list_append(PurrList* l, int64_t elem) {
  purr_list_push(l, (void*)(intptr_t)elem);
}

int64_t list_get(PurrList* l, int64_t idx) {
  return (int64_t)(intptr_t)purr_list_get(l, idx);
}

int64_t list_length(PurrList* l) {
  return purr_list_length(l);
}

void list_set(PurrList* l, int64_t idx, int64_t elem) {
  purr_list_set(l, idx, (void*)(intptr_t)elem);
}

/* --- String helpers for map hashing --- */

int purr_str_hash(void* key) {
  const char* s = (const char*)key;
  unsigned long h = 5381;
  int c;
  while ((c = (unsigned char)*s++)) {
    h = ((h << 5) + h) + c;  /* djb2: h * 33 + c */
  }
  return (int)(h & 0x7fffffff);
}

int purr_str_equals(void* a, void* b) {
  return strcmp((const char*)a, (const char*)b) == 0;
}

/* --- map<string, T> wrappers --- */

PurrMap* map_new(void) {
  return purr_map_create(16);
}

void map_set_str(PurrMap* m, const char* k, int64_t v) {
  purr_map_insert(m, (void*)k, (void*)(intptr_t)v,
                  purr_str_hash, purr_str_equals);
}

int64_t map_get_str(PurrMap* m, const char* k) {
  void* val = purr_map_get(m, (void*)k, purr_str_hash, purr_str_equals);
  return (int64_t)(intptr_t)val;
}

_Bool map_has_str(PurrMap* m, const char* k) {
  return purr_map_get(m, (void*)k, purr_str_hash, purr_str_equals) != NULL;
}

/* --- M5: test assertion helpers --- */

void expect_eq_i32(int32_t actual, int32_t expected) {
  if (actual != expected) {
    printf("FAIL: expected %d got %d\n", expected, actual);
  }
}

void expect_eq_i64(int64_t actual, int64_t expected) {
  if (actual != expected) {
    printf("FAIL: expected %lld got %lld\n", (long long)expected, (long long)actual);
  }
}

void expect_true(_Bool cond) {
  if (!cond) printf("FAIL: expected true\n");
}

void expect_false(_Bool cond) {
  if (cond) printf("FAIL: expected false\n");
}

/* --- option<T> helpers --- */

_Bool is_some(int64_t opt)  { return opt != 0; }
_Bool is_none(int64_t opt)  { return opt == 0; }
int64_t unwrap(int64_t opt) { return opt; }  /* caller ensures non-nil */

/* --- result<T,E> helpers --- */

_Bool   is_ok(int64_t res)     { return res != PURR_ERR_SENTINEL; }
int64_t unwrap_ok(int64_t res) { return res; }
