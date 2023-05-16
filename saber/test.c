#include <stdio.h>
#include <stdlib.h>

typedef struct {
    void* mem;
    int capacity;
    int offset;
} Memory;

void* storeM(Memory* m, void* a, size_t size) 
{
    if (m->capacity < m->offset+size) 
        // whatever makes sense here
        exit(1234);
    char* ptr = &(m->mem)[m->offset];
    m->offset += size;
    *ptr = a;
    return ptr;
}

Memory* allocM(size_t size) {
    size_t ms = sizeof(Memory);
    Memory* out = (Memory*)malloc(ms);
    out->mem = malloc(size);
    out->offset = 0;
    out->capacity = size;
    return out;
}

void freeM(Memory* m) {
    free(m->mem);
    free(m);
}

typedef struct {
    Memory* mem;
    int val;
} MemAndInt;

MemAndInt capacityM(Memory* m) {
    MemAndInt out;
    out.mem = m;
    out.val = m->capacity;
    return out;
}

typedef struct {
    int a;
    int b;
    int c;
} Blob;

Blob* mkBlob() {
    Blob* out = (Blob*)malloc(sizeof(Blob));
    out->a = 30;
    out->b = 50;
    out->c = 70;
    return out;
}

int main() {
    Memory* m = allocM(100);
    char* x = storeM(m, mkBlob(), sizeof(Blob));
    char* y = storeM(m, mkBlob, sizeof(Blob));
    printf("%d\n", m->offset);
    freeM(m);
    return 0;
}

