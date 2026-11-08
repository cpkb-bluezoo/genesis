/*
 * jimage.c
 * Reader of the JDK runtime image (lib/modules)
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
#include <sys/mman.h>
#include <sys/stat.h>
#include "jimage.h"

/*
 * The layout of the file (jdk.internal.jimage in the JDK, ImageHeader and
 * ImageLocation): a header of seven 32-bit words in the byte order of the
 * machine that wrote it, then four tables, then the resources.
 *
 *   header      magic 0xCAFEDADA, version (major << 16 | minor), flags,
 *               resource count, table length, locations size, strings size
 *   redirect    table length words - the perfect hash for finding a name; not
 *               needed here, the model is built by walking the image
 *   offsets     table length words: where each location starts in "locations"
 *   locations   per resource a run of attributes, each one a byte holding the
 *               kind (high five bits) and the length of the value minus one (low
 *               three bits), then the value in big-endian: module, parent, base
 *               and extension (offsets into "strings"), the offset of the content
 *               from the end of the index, and its compressed and uncompressed
 *               sizes; a byte of 0 ends the run
 *   strings     NUL-terminated modified UTF-8
 *
 * A resource named /java.base/java/lang/Object.class is module "java.base",
 * parent "java/lang", base "Object", extension "class".
 */

#define JIMAGE_MAGIC        0xCAFEDADAu
#define JIMAGE_HEADER_SIZE  28

enum
{
    ATTR_END = 0,
    ATTR_MODULE,
    ATTR_PARENT,
    ATTR_BASE,
    ATTR_EXTENSION,
    ATTR_OFFSET,
    ATTR_COMPRESSED,
    ATTR_UNCOMPRESSED,
    ATTR_COUNT
};

typedef struct jimage_store
{
    int fd;
    uint64_t index_size;        /* The content offsets count from here */
    uint64_t file_size;
} jimage_store_t;

static uint32_t read_u32(const uint8_t *p, bool swap)
{
    uint32_t v;
    memcpy(&v, p, sizeof(v));
    return swap ? __builtin_bswap32(v) : v;
}

/* The location starting at p (limit bytes left): its attributes. False if it is malformed. */
static bool decode_location(const uint8_t *p, size_t limit, uint64_t attrs[ATTR_COUNT])
{
    memset(attrs, 0, ATTR_COUNT * sizeof(uint64_t));
    size_t i = 0;
    while (i < limit) {
        unsigned data = p[i++];
        if (data <= 0x7) {
            return true;
        }
        unsigned kind = data >> 3;
        size_t length = (data & 0x7) + 1;
        if (kind >= ATTR_COUNT || i + length > limit) {
            return false;
        }
        uint64_t value = 0;
        for (size_t j = 0; j < length; j++) {
            value = (value << 8) | p[i++];
        }
        attrs[kind] = value;
    }
    return false;
}

/* The string at offset in the strings table, or NULL if it is not inside it */
static const char *string_at(const uint8_t *strings, size_t strings_size, uint64_t offset)
{
    if (offset >= strings_size) {
        return NULL;
    }
    size_t room = strings_size - (size_t)offset;
    const char *s = (const char *)strings + offset;
    return memchr(s, '\0', room) ? s : NULL;
}

static bool wanted_module(const char *name, const char *const *modules)
{
    if (!modules) {
        return true;
    }
    for (; *modules; modules++) {
        if (strcmp(*modules, name) == 0) {
            return true;
        }
    }
    return false;
}

static uint8_t *jimage_store_read(platform_image_t *img, const platform_class_t *c, size_t *size)
{
    jimage_store_t *store = (jimage_store_t *)img->store.data;
    if (c->stored_size != 0) {
        fprintf(stderr, "error: compressed jimage resources are not supported yet\n");
        return NULL;
    }
    uint64_t start = store->index_size + c->offset;
    if (start > store->file_size || c->size > store->file_size - start) {
        return NULL;
    }
    uint8_t *data = malloc(c->size ? c->size : 1);
    if (!data) {
        return NULL;
    }
    size_t done = 0;
    while (done < c->size) {
        ssize_t n = pread(store->fd, data + done, c->size - done, (off_t)(start + done));
        if (n <= 0) {
            free(data);
            return NULL;
        }
        done += (size_t)n;
    }
    if (size) {
        *size = c->size;
    }
    return data;
}

static void jimage_store_close(platform_image_t *img)
{
    jimage_store_t *store = (jimage_store_t *)img->store.data;
    if (store) {
        if (store->fd >= 0) {
            close(store->fd);
        }
        free(store);
        img->store.data = NULL;
    }
}

/* Walk the index and enter what it holds in the model */
static bool jimage_index_to_model(platform_image_t *img, const uint8_t *index, uint64_t table_length,
                                  uint64_t locations_size, uint64_t strings_size, bool swap,
                                  const char *const *modules)
{
    const uint8_t *offsets = index + JIMAGE_HEADER_SIZE + table_length * 4;
    const uint8_t *locations = offsets + table_length * 4;
    const uint8_t *strings = locations + locations_size;

    if (!platform_image_reserve(img, (size_t)table_length)) {
        return false;
    }

    char name[1024];
    for (uint64_t n = 0; n < table_length; n++) {
        uint32_t at = read_u32(offsets + n * 4, swap);
        uint64_t attrs[ATTR_COUNT];
        if (at >= locations_size || !decode_location(locations + at, locations_size - at, attrs)) {
            fprintf(stderr, "error: jimage location table is corrupt\n");
            return false;
        }
        if (attrs[ATTR_MODULE] == 0) {
            continue;       /* Not a resource of a module */
        }
        const char *module = string_at(strings, strings_size, attrs[ATTR_MODULE]);
        const char *base = string_at(strings, strings_size, attrs[ATTR_BASE]);
        const char *ext = attrs[ATTR_EXTENSION]
                ? string_at(strings, strings_size, attrs[ATTR_EXTENSION]) : NULL;
        if (!module || !base || !ext || strcmp(ext, "class") != 0) {
            continue;
        }
        /* The image describes itself under the module names "modules" and "packages" */
        if (strcmp(module, "modules") == 0 || strcmp(module, "packages") == 0
                || !wanted_module(module, modules)) {
            continue;
        }
        const char *parent = attrs[ATTR_PARENT]
                ? string_at(strings, strings_size, attrs[ATTR_PARENT]) : NULL;
        if (attrs[ATTR_PARENT] && !parent) {
            fprintf(stderr, "error: jimage location table is corrupt\n");
            return false;
        }

        int m = platform_image_add_module(img, module);
        if (m < 0) {
            return false;
        }
        platform_class_t c = {0};
        c.offset = attrs[ATTR_OFFSET];
        c.stored_size = (uint32_t)attrs[ATTR_COMPRESSED];
        c.size = (uint32_t)attrs[ATTR_UNCOMPRESSED];
        if (c.stored_size == 0 && attrs[ATTR_UNCOMPRESSED] > UINT32_MAX) {
            continue;
        }

        if (!parent && strcmp(base, "module-info") == 0) {
            platform_image_set_module_info(img, (uint32_t)m, &c);
            continue;
        }
        int len = snprintf(name, sizeof(name), "%s%s%s", parent ? parent : "",
                           parent ? "/" : "", base);
        if (len < 0 || (size_t)len >= sizeof(name)) {
            continue;
        }
        if (!platform_image_add_class(img, name, (uint32_t)m, &c)) {
            return false;
        }
    }
    return true;
}

platform_image_t *jimage_load(const char *path, const char *const *modules)
{
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        fprintf(stderr, "error: cannot open %s\n", path);
        return NULL;
    }
    struct stat st;
    uint8_t header[JIMAGE_HEADER_SIZE];
    if (fstat(fd, &st) != 0 || st.st_size < JIMAGE_HEADER_SIZE
            || pread(fd, header, sizeof(header), 0) != (ssize_t)sizeof(header)) {
        fprintf(stderr, "error: %s is not a jimage file\n", path);
        close(fd);
        return NULL;
    }

    /* The file is in the byte order of the machine that wrote it */
    bool swap = false;
    if (read_u32(header, false) != JIMAGE_MAGIC) {
        if (read_u32(header, true) != JIMAGE_MAGIC) {
            fprintf(stderr, "error: %s is not a jimage file\n", path);
            close(fd);
            return NULL;
        }
        swap = true;
    }
    uint32_t version = read_u32(header + 4, swap);
    if ((version >> 16) != 1) {
        fprintf(stderr, "error: %s is jimage version %u.%u, which is not supported\n", path,
                version >> 16, version & 0xFFFF);
        close(fd);
        return NULL;
    }
    uint64_t table_length = read_u32(header + 16, swap);
    uint64_t locations_size = read_u32(header + 20, swap);
    uint64_t strings_size = read_u32(header + 24, swap);
    uint64_t index_size = JIMAGE_HEADER_SIZE + table_length * 8 + locations_size + strings_size;
    if (index_size > (uint64_t)st.st_size) {
        fprintf(stderr, "error: %s is truncated\n", path);
        close(fd);
        return NULL;
    }

    /* Map only the index, and only while it is read into the model */
    void *map = mmap(NULL, (size_t)index_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (map == MAP_FAILED) {
        fprintf(stderr, "error: cannot map %s\n", path);
        close(fd);
        return NULL;
    }

    platform_image_t *img = platform_image_new();
    jimage_store_t *store = calloc(1, sizeof(jimage_store_t));
    if (!img || !store) {
        free(store);
        platform_image_free(img);
        munmap(map, (size_t)index_size);
        close(fd);
        return NULL;
    }
    store->fd = fd;
    store->index_size = index_size;
    store->file_size = (uint64_t)st.st_size;
    img->store.read = jimage_store_read;
    img->store.close = jimage_store_close;
    img->store.data = store;

    bool ok = jimage_index_to_model(img, (const uint8_t *)map, table_length, locations_size,
                                    strings_size, swap, modules);
    munmap(map, (size_t)index_size);
    if (!ok) {
        platform_image_free(img);
        return NULL;
    }
    return img;
}
