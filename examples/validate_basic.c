#include "purr_runtime.h"
#include <stdint.h>
#include <stdbool.h>

int32_t Main_on_start(void);

int32_t Main_on_start(void) {
    print_string("Hello, World!");
    return;
}

int main(void) {
    runtimeInit();
    Main_on_start();
    return 0;
}
