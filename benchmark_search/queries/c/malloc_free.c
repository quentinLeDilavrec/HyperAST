#include <stdlib.h>

void f () {
    void* v = malloc(42);
    free(v);
}

void g () {
    void* v = malloc(42);
    free(v);
    free(v2);
}
