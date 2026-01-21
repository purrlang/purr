#include "purr_runtime.h"
#include <stdio.h>

void purr_core_print(purr_string s) {
    fwrite(s.ptr, 1, (size_t)s.len, stdout);
    fputc('\n', stdout);
}
