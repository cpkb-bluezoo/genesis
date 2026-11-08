/*
 * jmods.c
 * Reader of the JDK's jmods directory
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "classpath.h"
#include "jmods.h"

/*
 * A .jmod is a zip archive behind a four-byte header, "JM" 1 0. Its classes are the
 * members "classes/java/lang/String.class".
 */
#define JMOD_HEADER_SIZE    4
#define CLASSES_PREFIX      "classes/"
#define CLASSES_PREFIX_LEN  8
#define CLASS_SUFFIX_LEN    6       /* ".class" */

typedef struct jmods_store
{
    int *fds;                   /* The open .jmod of each module of the model, by index */
    int count;
} jmods_store_t;

typedef struct enter_state
{
    platform_image_t *img;
    uint32_t module;
    bool failed;
} enter_state_t;

static bool enter_member(const char *member, size_t length, const zip_member_t *m, void *user_data)
{
    enter_state_t *st = (enter_state_t *)user_data;
    if (length < CLASSES_PREFIX_LEN + CLASS_SUFFIX_LEN + 1
            || memcmp(member, CLASSES_PREFIX, CLASSES_PREFIX_LEN) != 0
            || memcmp(member + length - CLASS_SUFFIX_LEN, ".class", CLASS_SUFFIX_LEN) != 0) {
        return true;
    }
    const char *name = member + CLASSES_PREFIX_LEN;
    size_t name_length = length - CLASSES_PREFIX_LEN - CLASS_SUFFIX_LEN;
    if (name_length > 9 && memcmp(name, "META-INF/", 9) == 0) {
        return true;
    }

    platform_class_t c = {0};
    c.offset = m->local_header_offset;
    c.stored_size = m->compressed_size;
    c.size = m->uncompressed_size;
    c.method = (uint8_t)m->method;
    if (name_length == 11 && memcmp(name, "module-info", 11) == 0) {
        platform_image_set_module_info(st->img, st->module, &c);
    } else if (!platform_image_add_class_n(st->img, name, name_length, st->module, &c)) {
        st->failed = true;
        return false;
    }
    return true;
}

static uint8_t *jmods_store_read(platform_image_t *img, const char *name,
                                 const platform_class_t *c, size_t *size)
{
    (void)name;     /* The class is found by where it is */
    jmods_store_t *store = (jmods_store_t *)img->store.data;
    if ((int)c->module >= store->count || store->fds[c->module] < 0) {
        return NULL;
    }
    zip_member_t m;
    m.local_header_offset = (uint32_t)c->offset;
    m.compressed_size = c->stored_size;
    m.uncompressed_size = c->size;
    m.method = c->method;
    return zip_read_member(store->fds[c->module], JMOD_HEADER_SIZE, &m, size);
}

static void jmods_store_close(platform_image_t *img)
{
    jmods_store_t *store = (jmods_store_t *)img->store.data;
    if (store) {
        for (int i = 0; i < store->count; i++) {
            if (store->fds[i] >= 0) {
                close(store->fds[i]);
            }
        }
        free(store->fds);
        free(store);
        img->store.data = NULL;
    }
}

/* Open a .jmod: its descriptor, or -1 if it is not there or not a .jmod */
static int open_jmod(const char *path)
{
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        return -1;
    }
    uint8_t header[JMOD_HEADER_SIZE];
    if (pread(fd, header, sizeof(header), 0) != (ssize_t)sizeof(header)
            || header[0] != 'J' || header[1] != 'M' || header[2] != 1 || header[3] != 0) {
        fprintf(stderr, "warning: failed to open JMOD: %s\n", path);
        close(fd);
        return -1;
    }
    return fd;
}

platform_image_t *jmods_load(const char *dir, const char *const *modules)
{
    int wanted = 0;
    while (modules[wanted]) {
        wanted++;
    }

    platform_image_t *img = platform_image_new();
    jmods_store_t *store = calloc(1, sizeof(jmods_store_t));
    int *fds = malloc((wanted ? (size_t)wanted : 1) * sizeof(int));
    if (!img || !store || !fds) {
        free(store);
        free(fds);
        platform_image_free(img);
        return NULL;
    }
    store->fds = fds;
    img->store.read = jmods_store_read;
    img->store.close = jmods_store_close;
    img->store.data = store;

    /* Open the files that are there, and see how many members they have between them */
    int opened = 0;
    size_t total = 0;
    for (int i = 0; i < wanted; i++) {
        char path[1024];
        int len = snprintf(path, sizeof(path), "%s/%s.jmod", dir, modules[i]);
        if (len < 0 || (size_t)len >= sizeof(path)) {
            continue;
        }
        int fd = open_jmod(path);
        size_t count = 0;
        if (fd < 0) {
            continue;
        }
        if (zip_member_count(fd, JMOD_HEADER_SIZE, &count) != 0
                || platform_image_add_module(img, modules[i]) != opened) {
            fprintf(stderr, "warning: failed to open JMOD: %s\n", path);
            close(fd);
            continue;
        }
        fds[opened++] = fd;
        store->count = opened;
        total += count;
    }
    if (opened == 0 || !platform_image_reserve(img, total)) {
        platform_image_free(img);
        return NULL;
    }

    for (int m = 0; m < opened; m++) {
        enter_state_t st = { img, (uint32_t)m, false };
        if (zip_scan_members(fds[m], JMOD_HEADER_SIZE, enter_member, &st) != 0 || st.failed) {
            platform_image_free(img);
            return NULL;
        }
    }
    return img;
}
