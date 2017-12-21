#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "utils.h"

int32_t printBool(bool a) {
    printf("%s (bool)\n", a ? "true" : "false");
    return 0;
}

int32_t printArray(void *a) {
    printf("This is where printing an array would be good\n");
    return 0;
}