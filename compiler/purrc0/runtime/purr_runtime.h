#ifndef PURR_RUNTIME_H
#define PURR_RUNTIME_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* --- Basic runtime --- */
void runtimeInit(void);
void print_string(const char* s);
void print_i32(int32_t n);
void print_i64(int64_t n);
void print_bool(_Bool b);
int32_t char_at(const char* s, int32_t index);
int64_t abs_i64(int64_t n);

/* ============================================================================
   M9 Container Runtime Support
   Provides C implementations for Purr's container types:
   - list<T>: Dynamic arrays with push/pop/get/set
   - map<K,V>: Hash table for key-value pairs
   - fixed<T,N>: Fixed-size arrays at compile time
   - slice<T>: View into array (start, length)
   ============================================================================ */

/* --- List Implementation (Dynamic Array) --- */

typedef struct {
  void** data;        /* Array of elements */
  int64_t capacity;   /* Allocated capacity */
  int64_t length;     /* Current length */
} PurrList;

/* Create a new list with initial capacity */
PurrList* purr_list_create(int64_t initial_capacity);

/* Free a list and all its elements */
void purr_list_free(PurrList* list);

/* Push an element to the end of the list */
void purr_list_push(PurrList* list, void* element);

/* Pop an element from the end of the list */
void* purr_list_pop(PurrList* list);

/* Get element at index */
void* purr_list_get(PurrList* list, int64_t index);

/* Set element at index */
void purr_list_set(PurrList* list, int64_t index, void* element);

/* Get the length of the list */
int64_t purr_list_length(PurrList* list);

/* Clear the list (remove all elements) */
void purr_list_clear(PurrList* list);

/* --- Fixed Array Implementation --- */

typedef struct {
  void* data;         /* Pointer to fixed array data */
  int64_t length;     /* Always N (fixed size) */
} PurrFixed;

/* Create a fixed-size array (must be called with preallocated data) */
PurrFixed* purr_fixed_create(void* data, int64_t size);

/* Free a fixed array */
void purr_fixed_free(PurrFixed* fixed);

/* Get element at index */
void* purr_fixed_get(PurrFixed* fixed, int64_t index);

/* Set element at index */
void purr_fixed_set(PurrFixed* fixed, int64_t index, void* element);

/* Get the length of the fixed array */
int64_t purr_fixed_length(PurrFixed* fixed);

/* --- Slice Implementation (Array View) --- */

typedef struct {
  void* data;         /* Pointer to base data */
  int64_t start;      /* Start index */
  int64_t length;     /* Number of elements */
} PurrSlice;

/* Create a slice from existing data */
PurrSlice* purr_slice_create(void* data, int64_t start, int64_t length);

/* Free a slice (doesn't free underlying data) */
void purr_slice_free(PurrSlice* slice);

/* Get element at index in slice */
void* purr_slice_get(PurrSlice* slice, int64_t index);

/* Set element at index in slice */
void purr_slice_set(PurrSlice* slice, int64_t index, void* element);

/* Get the length of the slice */
int64_t purr_slice_length(PurrSlice* slice);

/* --- Map Implementation (Hash Table) --- */

typedef struct {
  void* key;
  void* value;
  int used;           /* 1 if entry is used, 0 if empty */
} PurrMapEntry;

typedef struct {
  PurrMapEntry* entries;  /* Hash table entries */
  int64_t capacity;       /* Number of slots */
  int64_t count;          /* Number of entries */
} PurrMap;

/* Create a new map with initial capacity */
PurrMap* purr_map_create(int64_t initial_capacity);

/* Free a map and all its entries */
void purr_map_free(PurrMap* map);

/* Insert or update a key-value pair */
void purr_map_insert(PurrMap* map, void* key, void* value,
                     int (*key_hash)(void*),
                     int (*key_equals)(void*, void*));

/* Get a value by key (returns NULL if not found) */
void* purr_map_get(PurrMap* map, void* key,
                   int (*key_hash)(void*),
                   int (*key_equals)(void*, void*));

/* Remove a key-value pair */
void purr_map_remove(PurrMap* map, void* key,
                     int (*key_hash)(void*),
                     int (*key_equals)(void*, void*));

/* Get the number of entries in the map */
int64_t purr_map_count(PurrMap* map);

/* Clear the map (remove all entries) */
void purr_map_clear(PurrMap* map);

/* ============================================================================
   Purr-level container API wrappers
   These are called directly by generated code.
   ============================================================================ */

/* --- list<T> wrappers (elements stored as int64_t cast through void*) --- */
PurrList* list_new(void);
void      list_append(PurrList* l, int64_t elem);
int64_t   list_get(PurrList* l, int64_t idx);
int64_t   list_length(PurrList* l);
void      list_set(PurrList* l, int64_t idx, int64_t elem);

/* --- map<string, T> wrappers (string keys, int64_t values) --- */
PurrMap*  map_new(void);
void      map_set_str(PurrMap* m, const char* k, int64_t v);
int64_t   map_get_str(PurrMap* m, const char* k);
_Bool     map_has_str(PurrMap* m, const char* k);

/* String hash/equals helpers used internally by map wrappers */
int purr_str_hash(void* key);
int purr_str_equals(void* a, void* b);

/* --- M5: test assertion helpers --- */
void expect_eq_i32(int32_t actual, int32_t expected);
void expect_eq_i64(int64_t actual, int64_t expected);
void expect_true(_Bool cond);
void expect_false(_Bool cond);

/* --- option<T> helpers (option represented as int64_t; 0 = None, non-zero = Some) --- */
_Bool   is_some(int64_t opt);
_Bool   is_none(int64_t opt);
int64_t unwrap(int64_t opt);

/* --- result<T,E> helpers (positive = Ok value, INT64_MIN = Err sentinel) --- */
#define PURR_ERR_SENTINEL INT64_MIN
_Bool   is_ok(int64_t res);
int64_t unwrap_ok(int64_t res);

/* ============================================================================
   M10.5: Instrumentation API for Benchmarking
   Tracks allocations and performance metrics during benchmark runs.
   ============================================================================ */

/* Instrumentation counter structure */
typedef struct {
  int64_t alloc_count;      /* Number of allocations */
  int64_t bytes_allocated;  /* Total bytes allocated */
  int64_t message_count;    /* Number of messages dispatched (for actors) */
  int64_t scheduler_steps;  /* Number of scheduler steps (for actors) */
} PurrInstrCounters;

/* Get current instrumentation counters */
PurrInstrCounters get_instr_counters(void);

/* Reset instrumentation counters */
void reset_instr_counters(void);

#endif /* PURR_RUNTIME_H */
