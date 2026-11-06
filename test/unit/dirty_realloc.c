/*
 * dirty_realloc.c - realloc() that fills the memory it adds with 0x01
 *
 * Linked into the C unit tests, with the sources under test compiled with
 * -Drealloc=dirty_realloc. Whether the real realloc() returns clean or dirty
 * memory depends on the state of the heap; this makes it dirty every time,
 * and in a way that reads as a CONST_UTF8 entry.
 */

/* (The real realloc() is wanted here, not the replacement) */
#undef realloc

#include <stdlib.h>
#include <string.h>

#ifdef __APPLE__
#include <malloc/malloc.h>
#define USABLE_SIZE(p) malloc_size(p)
#else
#include <malloc.h>
#define USABLE_SIZE(p) malloc_usable_size(p)
#endif

void *dirty_realloc(void *ptr, size_t size);

void *dirty_realloc(void *ptr, size_t size)
{
    size_t old = ptr ? (size_t)USABLE_SIZE(ptr) : 0;
    void *grown = realloc(ptr, size);
    if (grown && size > old) {
        memset((unsigned char *)grown + old, 0x01, size - old);
    }
    return grown;
}
