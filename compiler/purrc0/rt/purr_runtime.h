#pragma once
#include <stdint.h>
#include <stddef.h>

typedef struct {
    const char* ptr;
    int64_t len;
} purr_string;

void purr_core_print(purr_string s);
