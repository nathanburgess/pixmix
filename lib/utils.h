#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#ifndef __UTILS_H__
#define __UTILS_H__

#define INT 0
#define FLOAT 1
#define BOOL 2
#define STRING 3
#define IMAGE 4

int32_t printBool(bool a);

struct Image {
	int32_t a;
};

#endif