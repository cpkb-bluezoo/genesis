/*
 * PlatImageTest.c
 * Test of the platform model
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
 * The platform model on its own: lookups, packages, growth beyond what the reader
 * reserved, and which of two classes with the same name is found.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "platimage.h"

static int failures;

static void check(bool ok, const char *what)
{
    if (!ok) {
        printf("FAILED: %s\n", what);
        failures++;
    }
}

static void count_class(const char *name, const platform_class_t *c, void *user_data)
{
    (void)name;
    (void)c;
    (*(size_t *)user_data)++;
}

int main(void)
{
    platform_image_t *img = platform_image_new();
    int first = platform_image_add_module(img, "first.module");
    int second = platform_image_add_module(img, "second.module");
    check(first == 0 && second == 1 && platform_image_add_module(img, "first.module") == 0,
          "modules are numbered once");

    /* The reader reserves room for 10; 5000 go in */
    check(platform_image_reserve(img, 10), "reserve");
    char name[64];
    bool added = true;
    for (int i = 0; i < 5000; i++) {
        platform_class_t c = {0};
        c.size = (uint32_t)i;
        snprintf(name, sizeof(name), "pkg%d/sub/Class%d", i % 40, i);
        added = platform_image_add_class(img, name, (uint32_t)(i % 2), &c) && added;
    }
    check(added, "the model grows past what was reserved");

    size_t listed = 0;
    platform_image_foreach_class(img, count_class, &listed);
    check(listed == 5000 && img->class_count == 5000, "every class is there once");

    bool found = true;
    for (int i = 0; i < 5000; i++) {
        snprintf(name, sizeof(name), "pkg%d/sub/Class%d", i % 40, i);
        const platform_class_t *c = platform_image_find_class(img, name);
        found = found && c && c->size == (uint32_t)i && c->module == (uint32_t)(i % 2);
    }
    check(found, "every class is found with what was recorded for it");
    check(!platform_image_find_class(img, "pkg1/sub/Class4000x"), "a name that is not there");
    check(!platform_image_find_class(img, "pkg1/sub/Class"), "a prefix of one that is");

    check(platform_image_package_module(img, "pkg3/sub") != NULL
          && strcmp(platform_image_package_module(img, "pkg3/sub"), "second.module") == 0,
          "a package is in the module of its first class (class 3 is in module 1)");
    check(!platform_image_package_module(img, "pkg3"), "a package with no classes of its own");
    check(img->package_count == 40, "forty packages");

    /* The class that is added first is the one that is found: the order of the modules is the
     * order of the search */
    platform_class_t early = {0}, late = {0};
    early.size = 1;
    late.size = 2;
    platform_image_add_class(img, "dup/Same", 0, &early);
    platform_image_add_class(img, "dup/Same", 1, &late);
    const platform_class_t *same = platform_image_find_class(img, "dup/Same");
    check(same && same->size == 1 && same->module == 0, "of two classes with one name, the first");

    /* A class with no package */
    platform_class_t bare = {0};
    platform_image_add_class(img, "Bare", 0, &bare);
    check(platform_image_find_class(img, "Bare") && platform_image_package_module(img, ""),
          "a class in the unnamed package");

    platform_image_free(img);
    if (failures == 0) {
        printf("ok\n");
    }
    return failures ? 1 : 0;
}
