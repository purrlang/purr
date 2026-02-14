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
