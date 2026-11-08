/*
 * JImageTest.c
 * Test driver for the jimage reader
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
 * Driver for the jimage reader, run by test/run-tests.sh against the JDK's own
 * "jimage" tool:
 *
 *   JImageTest list IMAGE [MODULE...]    "module internal/name" per class
 *   JImageTest read IMAGE NAME           the class file, on stdout
 *   JImageTest modinfo IMAGE MODULE      the module-info.class, on stdout
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jimage.h"

static void print_class(const char *name, void *value, void *user_data)
{
    platform_image_t *img = (platform_image_t *)user_data;
    const platform_class_t *c = (const platform_class_t *)value;
    printf("%s %s\n", img->module_names[c->module], name);
}

static int write_bytes(uint8_t *data, size_t size)
{
    if (!data) {
        fprintf(stderr, "not found or unreadable\n");
        return 1;
    }
    fwrite(data, 1, size, stdout);
    free(data);
    return 0;
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "usage: JImageTest list|read|modinfo IMAGE [ARG...]\n");
        return 2;
    }
    const char *const *modules = NULL;
    if (strcmp(argv[1], "list") == 0 && argc > 3) {
        modules = (const char *const *)&argv[3];
        argv[argc] = NULL;
    }
    platform_image_t *img = jimage_load(argv[2], modules);
    if (!img) {
        return 1;
    }
    int rc = 0;
    size_t size = 0;
    if (strcmp(argv[1], "list") == 0) {
        hashtable_foreach(img->classes, print_class, img);
    } else if (strcmp(argv[1], "read") == 0 && argc > 3) {
        uint8_t *data = platform_image_read_class(img, argv[3], &size);
        rc = write_bytes(data, size);
    } else if (strcmp(argv[1], "modinfo") == 0 && argc > 3) {
        uint8_t *data = platform_image_read_module_info(img, argv[3], &size);
        rc = write_bytes(data, size);
    } else {
        rc = 2;
    }
    platform_image_free(img);
    return rc;
}
