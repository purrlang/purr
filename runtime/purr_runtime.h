#ifndef PURR_RUNTIME_H
#define PURR_RUNTIME_H

#include <stdint.h>
#include <stdbool.h>

void runtimeInit(void);
void print_string(const char* s);
void print_i32(int32_t n);
void print_i64(int64_t n);
void print_bool(_Bool b);
/* M7+: Backfill utility functions */
int32_t char_at(const char* s, int32_t index);
int64_t abs_i64(int64_t n);

#endif
