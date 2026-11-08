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

platform_image_t *platform_image_new(void)
{
    platform_image_t *img = calloc(1, sizeof(platform_image_t));
    if (!img) {
        return NULL;
    }
    img->classes = hashtable_new_sized(4096);
    img->packages = hashtable_new_sized(512);
    if (!img->classes || !img->packages) {
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
    free(img->class_table);
    if (img->classes) {
        hashtable_free(img->classes);
    }
    if (img->packages) {
        hashtable_free(img->packages);
    }
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
    free(img->class_table);
    img->class_table = calloc(n ? n : 1, sizeof(platform_class_t));
    img->class_count = 0;
    img->class_capacity = img->class_table ? n : 0;
    return img->class_table != NULL;
}

void platform_image_set_module_info(platform_image_t *img, uint32_t module,
                                    const platform_class_t *c)
{
    img->module_info[module] = *c;
    img->module_info[module].module = module;
}

bool platform_image_add_class(platform_image_t *img, const char *internal_name,
                              uint32_t module, const platform_class_t *c)
{
    /* A class is kept in the model's own table, whose address the hashtable
     * holds: the table is sized up front by the reader (platform_image_reserve) */
    if (img->class_count >= img->class_capacity) {
        return false;
    }
    platform_class_t *slot = &img->class_table[img->class_count++];
    *slot = *c;
    slot->module = module;
    hashtable_insert(img->classes, internal_name, slot);

    const char *slash = strrchr(internal_name, '/');
    size_t length = slash ? (size_t)(slash - internal_name) : 0;
    char package[1024];
    if (length >= sizeof(package)) {
        return false;
    }
    memcpy(package, internal_name, length);
    package[length] = '\0';
    if (!hashtable_contains(img->packages, package)) {
        hashtable_insert(img->packages, package, (void *)(intptr_t)(module + 1));
    }
    return true;
}

const platform_class_t *platform_image_find_class(const platform_image_t *img,
                                                  const char *internal_name)
{
    return (const platform_class_t *)hashtable_lookup(img->classes, internal_name);
}

const char *platform_image_package_module(const platform_image_t *img, const char *package)
{
    intptr_t m = (intptr_t)hashtable_lookup(img->packages, package);
    return m > 0 ? img->module_names[m - 1] : NULL;
}

uint8_t *platform_image_read_class(platform_image_t *img, const char *internal_name,
                                   size_t *size)
{
    const platform_class_t *c = platform_image_find_class(img, internal_name);
    if (!c || !img->store.read) {
        return NULL;
    }
    return img->store.read(img, c, size);
}

uint8_t *platform_image_read_module_info(platform_image_t *img, const char *module,
                                         size_t *size)
{
    int m = platform_image_find_module(img, module);
    if (m < 0 || img->module_info[m].size == 0 || !img->store.read) {
        return NULL;
    }
    return img->store.read(img, &img->module_info[m], size);
}
