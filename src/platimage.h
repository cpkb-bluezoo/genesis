/*
 * platimage.h
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

#ifndef PLATIMAGE_H
#define PLATIMAGE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include "util.h"

/*
 * The classes of the Java platform, as genesis sees them: which modules there
 * are, which package each one holds, and where every class lives. The model is
 * filled in once by a reader of the JDK's own storage (the jimage runtime image
 * "lib/modules", see jimage.h) and is read-only after that, so any number of
 * threads may look classes up and read them at once.
 *
 * The model owns its data. It does not keep the file it was read from mapped;
 * a class's bytes are read from the backing store when asked for.
 */

typedef struct platform_image platform_image_t;

/* Where one class is, and how to get its bytes from the store behind the model */
typedef struct platform_class
{
    uint32_t module;            /* Index into the model's modules */
    uint64_t offset;            /* Position of the content in the store */
    uint32_t stored_size;       /* Bytes in the store; 0 when stored uncompressed */
    uint32_t size;              /* Bytes of the class file itself */
} platform_class_t;

/* The store behind a model */
typedef struct platform_store
{
    /* The class file of c, malloc'd, *size bytes - NULL if it cannot be read */
    uint8_t *(*read)(platform_image_t *img, const platform_class_t *c, size_t *size);
    void (*close)(platform_image_t *img);
    void *data;                 /* The reader's own state */
} platform_store_t;

struct platform_image
{
    int module_count;
    char **module_names;
    platform_class_t *module_info;  /* module-info.class of each module (size 0: none) */

    hashtable_t *classes;       /* "java/lang/String" -> platform_class_t * */
    hashtable_t *packages;      /* "java/lang" -> (void *)(module index + 1) */
    platform_class_t *class_table;  /* Backing array of the classes hashtable */
    size_t class_count;
    size_t class_capacity;

    platform_store_t store;
};

/* An empty model; the readers fill it in */
platform_image_t *platform_image_new(void);
void platform_image_free(platform_image_t *img);

/* Index of the module called name, adding it if it is new; -1 if out of memory */
int platform_image_add_module(platform_image_t *img, const char *name);

/* The module with this name, or -1 */
int platform_image_find_module(const platform_image_t *img, const char *name);

/* Make room for n classes in all. The classes are kept in one table that the
 * lookup refers into, so the reader says how many there will be before it adds any. */
bool platform_image_reserve(platform_image_t *img, size_t n);

/* Record the module-info.class of a module */
void platform_image_set_module_info(platform_image_t *img, uint32_t module,
                                    const platform_class_t *c);

/* Record a class (internal name, "java/lang/String") of module, which holds
 * c. Returns false if out of memory. */
bool platform_image_add_class(platform_image_t *img, const char *internal_name,
                              uint32_t module, const platform_class_t *c);

/* The class with this internal name, or NULL */
const platform_class_t *platform_image_find_class(const platform_image_t *img,
                                                  const char *internal_name);

/* Whether a package ("java/lang") has classes, and the module it belongs to
 * (its name, or NULL) */
const char *platform_image_package_module(const platform_image_t *img, const char *package);

/* The bytes of a class (malloc'd; *size set), or NULL if it is not there or
 * cannot be read. module-info is the module-info.class of a module. */
uint8_t *platform_image_read_class(platform_image_t *img, const char *internal_name,
                                   size_t *size);
uint8_t *platform_image_read_module_info(platform_image_t *img, const char *module,
                                         size_t *size);

#endif /* PLATIMAGE_H */
