/*
 * ConstPoolGrowTest.c - the constant pool must leave no slot uninitialised
 *
 * When cp_add_raw_entry() (indy.c) grows the pool it must clear the new
 * entries as cp_add_entry() does. A long or double takes a second slot that
 * is never written, so a slot that came from realloc() uncleared holds
 * whatever the heap held: if that looks like a CONST_UTF8 entry,
 * const_pool_free() frees the leftover bytes as a string.
 *
 * The sources under test are compiled with realloc() replaced by
 * dirty_realloc(), which fills the memory it adds with bytes that read as a
 * CONST_UTF8 entry. The test grows a pool through cp_add_raw_entry(), adds a
 * long and a double, and checks that every slot the pool does not set is
 * zero. It is built and run by test/run-tests.sh.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "constpool.h"
#include "indy.h"

static int failures = 0;

static void expect_zero(const const_pool_t *cp, uint16_t index, const char *what)
{
    if (cp->entries[index].type != 0) {
        if (failures < 5) {
            printf("slot %u (%s) has type %d, expected 0\n", (unsigned)index, what,
                   (int)cp->entries[index].type);
        }
        failures++;
    }
}

int main(void)
{
    const_pool_t *cp = const_pool_new();
    if (!cp) {
        return 2;
    }

    /* Fill the pool to its capacity of 256 entries with ordinary entries */
    char name[32];
    int n = 0;
    while (cp->count < cp->capacity) {
        snprintf(name, sizeof(name), "u%d", n++);
        cp_add_utf8_len(cp, name, strlen(name));
    }

    /* The next entry is added by cp_add_raw_entry(), which grows the pool */
    uint16_t before = cp->capacity;
    uint16_t mh = cp_add_method_handle(cp, REF_invokeStatic, 1);
    if (mh == 0 || cp->capacity <= before) {
        printf("the pool did not grow through cp_add_raw_entry()\n");
        return 2;
    }

    uint16_t lng = cp_add_long(cp, 42);
    uint16_t dbl = cp_add_double(cp, 1.5);
    expect_zero(cp, (uint16_t)(lng + 1), "second slot of a long");
    expect_zero(cp, (uint16_t)(dbl + 1), "second slot of a double");
    for (uint16_t i = cp->count; i < cp->capacity; i++) {
        expect_zero(cp, i, "unused tail");
    }

    /* (With the slots dirty this is where a worker process aborts) */
    const_pool_free(cp);
    if (failures == 0) {
        printf("ok\n");
    }
    return failures == 0 ? 0 : 1;
}
