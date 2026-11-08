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
#include <unistd.h>
#include "classpath.h"
#include "jmods.h"

/* The classes of a .jmod are the members "classes/java/lang/String.class" of its zip file */
#define CLASSES_PREFIX      "classes/"
#define CLASSES_PREFIX_LEN  8

typedef struct jmods_store
{
    void **handles;             /* The open .jmod of each module of the model, by index */
    int count;
} jmods_store_t;

/* The internal name of the class that a member of a .jmod holds ("java/lang/String"), copied
 * into name; false if it is not a class */
static bool member_class_name(const char *member, char *name, size_t room)
{
    size_t length = strlen(member);
    if (strncmp(member, CLASSES_PREFIX, CLASSES_PREFIX_LEN) != 0 || length < CLASSES_PREFIX_LEN + 7
            || strcmp(member + length - 6, ".class") != 0) {
        return false;
    }
    length -= CLASSES_PREFIX_LEN + 6;
    if (length >= room || strncmp(member + CLASSES_PREFIX_LEN, "META-INF/", 9) == 0) {
        return false;
    }
    memcpy(name, member + CLASSES_PREFIX_LEN, length);
    name[length] = '\0';
    return true;
}

static void count_member(const char *member, uint32_t size, void *user_data)
{
    (void)size;
    char name[1024];
    if (member_class_name(member, name, sizeof(name))) {
        (*(size_t *)user_data)++;
    }
}

typedef struct enter_state
{
    platform_image_t *img;
    uint32_t module;
    bool failed;
} enter_state_t;

static void enter_member(const char *member, uint32_t size, void *user_data)
{
    enter_state_t *st = (enter_state_t *)user_data;
    char name[1024];
    if (st->failed || !member_class_name(member, name, sizeof(name))) {
        return;
    }
    platform_class_t c = {0};
    c.size = size;
    if (strcmp(name, "module-info") == 0) {
        platform_image_set_module_info(st->img, st->module, &c);
    } else if (!platform_image_add_class(st->img, name, st->module, &c)) {
        st->failed = true;
    }
}

static uint8_t *jmods_store_read(platform_image_t *img, const char *name,
                                 const platform_class_t *c, size_t *size)
{
    jmods_store_t *store = (jmods_store_t *)img->store.data;
    if ((int)c->module >= store->count || !store->handles[c->module]) {
        return NULL;
    }
    return jmod_read_class(store->handles[c->module], name, size);
}

static void jmods_store_close(platform_image_t *img)
{
    jmods_store_t *store = (jmods_store_t *)img->store.data;
    if (store) {
        for (int i = 0; i < store->count; i++) {
            if (store->handles[i]) {
                jmod_close(store->handles[i]);
            }
        }
        free(store->handles);
        free(store);
        img->store.data = NULL;
    }
}

platform_image_t *jmods_load(const char *dir, const char *const *modules)
{
    int wanted = 0;
    while (modules[wanted]) {
        wanted++;
    }

    platform_image_t *img = platform_image_new();
    jmods_store_t *store = calloc(1, sizeof(jmods_store_t));
    void **handles = calloc(wanted ? (size_t)wanted : 1, sizeof(void *));
    if (!img || !store || !handles) {
        free(store);
        free(handles);
        platform_image_free(img);
        return NULL;
    }
    store->handles = handles;
    img->store.read = jmods_store_read;
    img->store.close = jmods_store_close;
    img->store.data = store;

    /* Open the files, to know how many classes there will be */
    int opened = 0;
    size_t total = 0;
    for (int i = 0; i < wanted; i++) {
        char path[1024];
        int len = snprintf(path, sizeof(path), "%s/%s.jmod", dir, modules[i]);
        if (len < 0 || (size_t)len >= sizeof(path) || access(path, R_OK) != 0) {
            continue;
        }
        void *handle = jmod_open(path);
        if (!handle) {
            fprintf(stderr, "warning: failed to open JMOD: %s\n", path);
            continue;
        }
        int m = platform_image_add_module(img, modules[i]);
        if (m < 0 || m != opened) {
            jmod_close(handle);
            platform_image_free(img);
            return NULL;
        }
        handles[opened++] = handle;
        store->count = opened;
        jar_foreach_entry(handle, count_member, &total);
    }
    if (opened == 0 || !platform_image_reserve(img, total)) {
        platform_image_free(img);
        return NULL;
    }

    for (int m = 0; m < opened; m++) {
        enter_state_t st = { img, (uint32_t)m, false };
        jar_foreach_entry(handles[m], enter_member, &st);
        if (st.failed) {
            platform_image_free(img);
            return NULL;
        }
    }
    return img;
}
