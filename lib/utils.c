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