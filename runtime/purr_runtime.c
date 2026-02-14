#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "purr_runtime.h"

void runtimeInit(void) {
    /* Future: scheduler, heap init */
}

void print_string(const char* s) {
    printf("%s\n", s);
}

void print_i32(int32_t n) {
    printf("%d\n", n);
}

void print_i64(int64_t n) {
    printf("%lld\n", n);
}

void print_bool(_Bool b) {
    printf("%s\n", b ? "true" : "false");
}
/* M7+: Backfill utility functions */
int32_t char_at(const char* s, int32_t index) {
    if (s == NULL || index < 0) {
        return -1;  /* Error: null string or negative index */
    }
    /* Return the character at index as ASCII value, or -1 if out of bounds */
    int i = 0;
    while (s[i] != '\0') {
        if (i == index) {
            return (int32_t)(unsigned char)s[i];
        }
        i++;
    }
    return -1;  /* Out of bounds */
}

int64_t abs_i64(int64_t n) {
    return n < 0 ? -n : n;
}