/*
 * platimage.c
 * Model of the classes of the Java platform (the JDK modules)
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

#include <stdlib.h>
#include <string.h>
#include "platimage.h"

#define INITIAL_PACKAGE_SLOTS   2048

/* FNV-1a */
static uint32_t name_hash(const char *name, size_t length)
{
    uint32_t h = 2166136261u;
    for (size_t i = 0; i < length; i++) {
        h = (h ^ (uint8_t)name[i]) * 16777619u;
    }
    return h;
}

static size_t power_of_two_for(size_t n)
{
    size_t size = 64;
    while (size < n) {
        size *= 2;
    }
    return size;
}

platform_image_t *platform_image_new(void)
{
    platform_image_t *img = calloc(1, sizeof(platform_image_t));
    if (!img) {
        return NULL;
    }
    img->package_capacity = INITIAL_PACKAGE_SLOTS / 2;
    img->packages = malloc(img->package_capacity * sizeof(platform_package_t));
    img->package_slots = calloc(INITIAL_PACKAGE_SLOTS, sizeof(uint32_t));
    img->package_slot_mask = INITIAL_PACKAGE_SLOTS - 1;
    if (!img->packages || !img->package_slots) {
        platform_image_free(img);
        return NULL;
    }
    return img;
}

void platform_image_free(platform_image_t *img)
{
    if (!img) {
        return;
    }
    if (img->store.close) {
        img->store.close(img);
    }
    for (int i = 0; i < img->module_count; i++) {
        free(img->module_names[i]);
    }
    free(img->module_names);
    free(img->module_info);
    free(img->classes);
    free(img->class_slots);
    free(img->packages);
    free(img->package_slots);
    free(img->names);
    free(img);
}

int platform_image_find_module(const platform_image_t *img, const char *name)
{
    for (int i = 0; i < img->module_count; i++) {
        if (strcmp(img->module_names[i], name) == 0) {
            return i;
        }
    }
    return -1;
}

int platform_image_add_module(platform_image_t *img, const char *name)
{
    int existing = platform_image_find_module(img, name);
    if (existing >= 0) {
        return existing;
    }
    char **names = realloc(img->module_names, (size_t)(img->module_count + 1) * sizeof(char *));
    if (!names) {
        return -1;
    }
    img->module_names = names;
    platform_class_t *infos = realloc(img->module_info,
                                      (size_t)(img->module_count + 1) * sizeof(platform_class_t));
    if (!infos) {
        return -1;
    }
    img->module_info = infos;
    char *copy = strdup(name);
    if (!copy) {
        return -1;
    }
    memset(&infos[img->module_count], 0, sizeof(platform_class_t));
    names[img->module_count] = copy;
    return img->module_count++;
}

bool platform_image_reserve(platform_image_t *img, size_t n)
{
    free(img->classes);
    free(img->class_slots);
    img->class_count = 0;
    img->classes = calloc(n ? n : 1, sizeof(platform_class_t));
    size_t slots = power_of_two_for(n * 2);
    img->class_slots = calloc(slots, sizeof(uint32_t));
    img->class_slot_mask = slots - 1;
    img->class_capacity = (img->classes && img->class_slots) ? n : 0;
    if (img->class_capacity && img->names_capacity < n * 40) {
        char *names = realloc(img->names, n * 40);
        if (names) {
            img->names = names;
            img->names_capacity = n * 40;
        }
    }
    return img->classes && img->class_slots;
}

void platform_image_set_module_info(platform_image_t *img, uint32_t module,
                                    const platform_class_t *c)
{
    img->module_info[module] = *c;
    img->module_info[module].module = module;
}

/* Copy a name into the model's names; its offset, or UINT32_MAX if out of memory */
static uint32_t store_name(platform_image_t *img, const char *name, size_t length)
{
    if (img->names_size + length + 1 > img->names_capacity) {
        size_t capacity = img->names_capacity ? img->names_capacity * 2 : 4096;
        while (capacity < img->names_size + length + 1) {
            capacity *= 2;
        }
        char *names = realloc(img->names, capacity);
        if (!names) {
            return UINT32_MAX;
        }
        img->names = names;
        img->names_capacity = capacity;
    }
    uint32_t at = (uint32_t)img->names_size;
    memcpy(img->names + at, name, length);
    img->names[at + length] = '\0';
    img->names_size += length + 1;
    return at;
}

/* Make a package known, if it is not, as a package of module */
static bool note_package(platform_image_t *img, const char *name, size_t length, uint32_t module)
{
    /* Classes of a package tend to come together */
    if (img->have_last_package && img->last_package_length == length
            && memcmp(img->names + img->last_package_offset, name, length) == 0) {
        return true;
    }

    uint32_t hash = name_hash(name, length);
    size_t i = hash & img->package_slot_mask;
    while (img->package_slots[i]) {
        const platform_package_t *p = &img->packages[img->package_slots[i] - 1];
        if (p->hash == hash && p->name_length == length
                && memcmp(img->names + p->name_offset, name, length) == 0) {
            img->last_package_offset = p->name_offset;
            img->last_package_length = (uint32_t)length;
            img->have_last_package = true;
            return true;
        }
        i = (i + 1) & img->package_slot_mask;
    }

    if (img->package_count == img->package_capacity) {
        size_t capacity = img->package_capacity * 2;
        platform_package_t *grown = realloc(img->packages, capacity * sizeof(platform_package_t));
        if (!grown) {
            return false;
        }
        img->packages = grown;
        img->package_capacity = capacity;
    }
    if ((img->package_count + 1) * 2 > img->package_slot_mask + 1) {
        size_t slots = (img->package_slot_mask + 1) * 2;
        uint32_t *table = calloc(slots, sizeof(uint32_t));
        if (!table) {
            return false;
        }
        for (size_t k = 0; k < img->package_count; k++) {
            size_t j = img->packages[k].hash & (slots - 1);
            while (table[j]) {
                j = (j + 1) & (slots - 1);
            }
            table[j] = (uint32_t)k + 1;
        }
        free(img->package_slots);
        img->package_slots = table;
        img->package_slot_mask = slots - 1;
        i = hash & img->package_slot_mask;
        while (img->package_slots[i]) {
            i = (i + 1) & img->package_slot_mask;
        }
    }
    uint32_t at = store_name(img, name, length);
    if (at == UINT32_MAX) {
        return false;
    }
    platform_package_t *p = &img->packages[img->package_count];
    p->hash = hash;
    p->name_offset = at;
    p->name_length = (uint32_t)length;
    p->module = module;
    img->package_slots[i] = (uint32_t)++img->package_count;
    img->last_package_offset = at;
    img->last_package_length = (uint32_t)length;
    img->have_last_package = true;
    return true;
}

/* Room for more classes than the reader said there would be */
static bool grow_classes(platform_image_t *img)
{
    size_t capacity = img->class_capacity + img->class_capacity / 2 + 64;
    platform_class_t *grown = realloc(img->classes, capacity * sizeof(platform_class_t));
    if (!grown) {
        return false;
    }
    img->classes = grown;
    img->class_capacity = capacity;

    size_t slots = img->class_slot_mask + 1;
    if (capacity * 2 > slots) {
        while (capacity * 2 > slots) {
            slots *= 2;
        }
        uint32_t *table = calloc(slots, sizeof(uint32_t));
        if (!table) {
            return false;
        }
        for (size_t k = 0; k < img->class_count; k++) {
            size_t j = img->classes[k].hash & (slots - 1);
            while (table[j]) {
                j = (j + 1) & (slots - 1);
            }
            table[j] = (uint32_t)k + 1;
        }
        free(img->class_slots);
        img->class_slots = table;
        img->class_slot_mask = slots - 1;
    }
    return true;
}

bool platform_image_add_class_n(platform_image_t *img, const char *internal_name, size_t length,
                                uint32_t module, const platform_class_t *c)
{
    if (length > UINT16_MAX) {
        return false;
    }
    uint32_t hash = name_hash(internal_name, length);
    size_t i = hash & img->class_slot_mask;
    while (img->class_slots[i]) {
        platform_class_t *existing = &img->classes[img->class_slots[i] - 1];
        if (existing->hash == hash && existing->name_length == length
                && memcmp(img->names + existing->name_offset, internal_name, length) == 0) {
            /* The same class again: the module that is searched first has it */
            return true;
        }
        i = (i + 1) & img->class_slot_mask;
    }
    if (img->class_count >= img->class_capacity) {
        if (!grow_classes(img)) {
            return false;
        }
        /* The table may have been rebuilt: find the free slot again */
        i = hash & img->class_slot_mask;
        while (img->class_slots[i]) {
            i = (i + 1) & img->class_slot_mask;
        }
    }

    uint32_t at = store_name(img, internal_name, length);
    if (at == UINT32_MAX) {
        return false;
    }
    platform_class_t *slot = &img->classes[img->class_count++];
    *slot = *c;
    slot->module = module;
    slot->hash = hash;
    slot->name_offset = at;
    slot->name_length = (uint16_t)length;
    img->class_slots[i] = (uint32_t)img->class_count;

    size_t package_length = length;
    while (package_length > 0 && internal_name[package_length - 1] != '/') {
        package_length--;
    }
    package_length = package_length ? package_length - 1 : 0;
    return note_package(img, internal_name, package_length, module);
}

bool platform_image_add_class(platform_image_t *img, const char *internal_name,
                              uint32_t module, const platform_class_t *c)
{
    return platform_image_add_class_n(img, internal_name, strlen(internal_name), module, c);
}

const platform_class_t *platform_image_find_class(const platform_image_t *img,
                                                  const char *internal_name)
{
    if (!img->class_slots) {
        return NULL;
    }
    size_t length = strlen(internal_name);
    uint32_t hash = name_hash(internal_name, length);
    for (size_t i = hash & img->class_slot_mask; img->class_slots[i];
            i = (i + 1) & img->class_slot_mask) {
        const platform_class_t *c = &img->classes[img->class_slots[i] - 1];
        if (c->hash == hash && c->name_length == length
                && memcmp(img->names + c->name_offset, internal_name, length) == 0) {
            return c;
        }
    }
    return NULL;
}

const char *platform_image_package_module(const platform_image_t *img, const char *package)
{
    size_t length = strlen(package);
    uint32_t hash = name_hash(package, length);
    for (size_t i = hash & img->package_slot_mask; img->package_slots[i];
            i = (i + 1) & img->package_slot_mask) {
        const platform_package_t *p = &img->packages[img->package_slots[i] - 1];
        if (p->hash == hash && p->name_length == length
                && memcmp(img->names + p->name_offset, package, length) == 0) {
            return img->module_names[p->module];
        }
    }
    return NULL;
}

void platform_image_foreach_class(const platform_image_t *img, platform_class_fn fn,
                                  void *user_data)
{
    for (size_t i = 0; i < img->class_count; i++) {
        fn(img->names + img->classes[i].name_offset, &img->classes[i], user_data);
    }
}

void platform_image_foreach_package(const platform_image_t *img, platform_package_fn fn,
                                    void *user_data)
{
    for (size_t i = 0; i < img->package_count; i++) {
        fn(img->names + img->packages[i].name_offset, img->module_names[img->packages[i].module],
           user_data);
    }
}

uint8_t *platform_image_read_class(platform_image_t *img, const char *internal_name,
                                   size_t *size)
{
    const platform_class_t *c = platform_image_find_class(img, internal_name);
    if (!c || !img->store.read) {
        return NULL;
    }
    return img->store.read(img, internal_name, c, size);
}

uint8_t *platform_image_read_module_info(platform_image_t *img, const char *module,
                                         size_t *size)
{
    int m = platform_image_find_module(img, module);
    if (m < 0 || img->module_info[m].size == 0 || !img->store.read) {
        return NULL;
    }
    return img->store.read(img, "module-info", &img->module_info[m], size);
}
