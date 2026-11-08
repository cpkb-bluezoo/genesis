/*
 * JmodsTest.c
 * Test driver for the jmods reader
 * Copyright (C) 2026 Chris Burdess <dog@gnu.org>
 *
 * This file is part of genesis.
 *
 * genesis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * genesis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/*
 * JmodsTest JAVA_HOME MODULE...
 *
 * Reads the named modules of a JDK that has both its jmods directory and its
 * lib/modules image, once from each, and checks that the two models hold the
 * same classes in the same modules, with the same bytes and the same
 * module-info. Prints each difference; exits 1 if there are any.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "jimage.h"
#include "jmods.h"

typedef struct compare_state
{
    platform_image_t *from;     /* The model being walked */
    platform_image_t *other;    /* The model it is compared with */
    const char *from_label;
    const char *other_label;
    size_t checked;
    int differences;
} compare_state_t;

static void difference(compare_state_t *st, const char *what, const char *name)
{
    if (st->differences++ < 10) {
        printf("%s: %s (%s)\n", name, what, st->from_label);
    }
}

static void compare_bytes(compare_state_t *st, const char *name, const platform_class_t *c,
                          const platform_class_t *o)
{
    size_t a_size = 0, b_size = 0;
    uint8_t *a = platform_image_read_class(st->from, name, &a_size);
    uint8_t *b = platform_image_read_class(st->other, name, &b_size);
    if (!a || !b) {
        difference(st, "cannot be read", name);
    } else if (a_size != b_size || memcmp(a, b, a_size) != 0) {
        difference(st, "differs in content", name);
    } else if (strcmp(st->from->module_names[c->module], st->other->module_names[o->module]) != 0) {
        difference(st, "is in a different module", name);
    }
    free(a);
    free(b);
}

/* Classes that jlink writes or rewrites in java.base when it makes the image: the
 * method handle holders, their generated species, and the table of system modules */
static bool made_by_jlink(const char *name)
{
    size_t n = strlen(name);
    return (n > 7 && strcmp(name + n - 7, "$Holder") == 0)
            || strstr(name, "$Species_") != NULL
            || strncmp(name, "jdk/internal/module/SystemModules", 33) == 0;
}

static void compare_class(const char *name, const platform_class_t *c, void *user_data)
{
    compare_state_t *st = (compare_state_t *)user_data;
    const platform_class_t *o = platform_image_find_class(st->other, name);
    if (made_by_jlink(name)) {
        return;
    }
    st->checked++;
    if (!o) {
        difference(st, "is missing from the other", name);
    } else {
        compare_bytes(st, name, c, o);
    }
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: JmodsTest JAVA_HOME MODULE...\n");
        return 2;
    }
    char path[1024];
    snprintf(path, sizeof(path), "%s/jmods", argv[1]);
    platform_image_t *jmods = jmods_load(path, (const char *const *)&argv[2]);
    snprintf(path, sizeof(path), "%s/lib/modules", argv[1]);
    platform_image_t *image = jimage_load(path, (const char *const *)&argv[2]);
    if (!jmods || !image) {
        fprintf(stderr, "cannot read both\n");
        return 2;
    }

    compare_state_t a = { jmods, image, "jmods", "jimage", 0, 0 };
    compare_state_t b = { image, jmods, "jimage", "jmods", 0, 0 };
    platform_image_foreach_class(jmods, compare_class, &a);
    platform_image_foreach_class(image, compare_class, &b);

    /* The module-info of each module */
    for (int m = 0; m < jmods->module_count; m++) {
        const char *module = jmods->module_names[m];
        size_t x = 0, y = 0;
        uint8_t *p = platform_image_read_module_info(jmods, module, &x);
        uint8_t *q = platform_image_read_module_info(image, module, &y);
        if (!p || !q) {
            printf("%s: module-info cannot be read\n", module);
            a.differences++;
        } else if (x != y || memcmp(p, q, x) != 0) {
            printf("%s: module-info differs\n", module);
            a.differences++;
        }
        free(p);
        free(q);
    }

    printf("%zu classes in jmods, %zu in the image, %d differences\n", a.checked, b.checked,
           a.differences + b.differences);
    int rc = (a.differences + b.differences || a.checked != b.checked || a.checked < 1000) ? 1 : 0;
    platform_image_free(jmods);
    platform_image_free(image);
    return rc;
}
