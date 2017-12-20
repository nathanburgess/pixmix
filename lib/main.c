#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "utils.h"

int main() {

    struct Image i;

    i.a = 5;

    printf("Image: %d\n", i.a);

    return 0;
}