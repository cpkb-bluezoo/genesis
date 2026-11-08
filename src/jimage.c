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
#include <zlib.h>
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
    bool swap;                  /* The file is in the other byte order to this machine's */
    uint8_t *strings;           /* The strings table, which decompression refers to */
    size_t strings_size;
} jimage_store_t;

static uint32_t read_u32(const uint8_t *p, bool swap)
{
    uint32_t v;
    memcpy(&v, p, sizeof(v));
    return swap ? __builtin_bswap32(v) : v;
}

static uint64_t read_u64(const uint8_t *p, bool swap)
{
    uint64_t v;
    memcpy(&v, p, sizeof(v));
    return swap ? __builtin_bswap64(v) : v;
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

/*
 * Compressed resources. A resource that was compressed when the image was made
 * is stored as a layer: a 29-byte header (magic 0xCAFEFAFA, the compressed and
 * uncompressed size as 64-bit words, the offset in the strings table of the
 * name of the compressor, a word of padding, a byte that says whether this is
 * the last layer) and then the content. Taking the layer off may give another
 * layer, and so on until the bytes no longer start with the magic. The
 * compressors are "zip" (zlib) and "compact-cp", which leaves out of a class
 * file the strings that the image holds anyway.
 */

#define LAYER_MAGIC         0xCAFEFAFAu
#define LAYER_HEADER_SIZE   29
#define MAX_RESOURCE_SIZE   (1u << 30)

/* A growing buffer */
typedef struct buffer
{
    uint8_t *data;
    size_t size;
    size_t capacity;
    bool failed;
} buffer_t;

static void buffer_put(buffer_t *b, const void *bytes, size_t n)
{
    if (b->failed || n == 0) {
        return;
    }
    if (b->size + n > b->capacity) {
        size_t capacity = b->capacity ? b->capacity : 1024;
        while (capacity < b->size + n) {
            capacity *= 2;
        }
        uint8_t *grown = realloc(b->data, capacity);
        if (!grown) {
            b->failed = true;
            return;
        }
        b->data = grown;
        b->capacity = capacity;
    }
    memcpy(b->data + b->size, bytes, n);
    b->size += n;
}

static void buffer_put_u8(buffer_t *b, unsigned v)
{
    uint8_t byte = (uint8_t)v;
    buffer_put(b, &byte, 1);
}

static void buffer_put_u16(buffer_t *b, unsigned v)
{
    uint8_t bytes[2] = { (uint8_t)(v >> 8), (uint8_t)v };
    buffer_put(b, bytes, 2);
}

/* A cursor over the bytes of a layer */
typedef struct cursor
{
    const uint8_t *p;
    size_t left;
    bool failed;
} cursor_t;

static const uint8_t *cursor_take(cursor_t *c, size_t n)
{
    if (c->failed || n > c->left) {
        c->failed = true;
        return NULL;
    }
    const uint8_t *at = c->p;
    c->p += n;
    c->left -= n;
    return at;
}

static unsigned cursor_u8(cursor_t *c)
{
    const uint8_t *p = cursor_take(c, 1);
    return p ? p[0] : 0;
}

static unsigned cursor_u16(cursor_t *c)
{
    const uint8_t *p = cursor_take(c, 2);
    return p ? (unsigned)(p[0] << 8 | p[1]) : 0;
}

/* An index into the strings table as compact-cp writes it. A first byte with
 * the top bit set holds the number of bytes of the index (bits 5 and 6) and its
 * high bits (the low five); otherwise the index is four bytes, big-endian. */
static uint32_t cursor_index(cursor_t *c)
{
    unsigned header = cursor_u8(c);
    size_t length = 4;
    uint32_t value = header;
    if (header & 0x80) {
        length = (header >> 5) & 3;
        value = header & 0x1f;
    }
    for (size_t i = 1; i < length; i++) {
        value = (value << 8) | cursor_u8(c);
    }
    return value;
}

/* The size of the body of a constant pool entry of each tag, after the tag; 0 where it varies or the tag is not valid */
static const uint8_t constant_sizes[21] = {
    0, 0, 0, 4, 4, 8, 8, 2, 2, 4, 4, 4, 4, 0, 0, 3, 2, 4, 4, 2, 2
};

#define TAG_UTF8                1
#define TAG_LONG                5
#define TAG_DOUBLE              6
#define TAG_EXTERNAL_STRING     23      /* A Utf8 whose text is a string of the image */
#define TAG_EXTERNAL_DESCRIPTOR 25      /* A descriptor whose type names are strings of the image */

/* A Utf8 constant with this text */
static void put_utf8(buffer_t *out, const char *text, size_t length, bool *bad)
{
    if (length > 0xFFFF) {
        *bad = true;
        return;
    }
    buffer_put_u8(out, TAG_UTF8);
    buffer_put_u16(out, (unsigned)length);
    buffer_put(out, text, length);
}

/* A descriptor with the package and class names of its types left out ("(L;L;)V"),
 * followed by the list of indexes of those names, a package then a class for each L */
static void put_descriptor(buffer_t *out, const uint8_t *strings, size_t strings_size,
                           cursor_t *c, bool *bad)
{
    const char *desc = string_at(strings, strings_size, cursor_index(c));
    size_t list_length = cursor_index(c);
    const uint8_t *list = cursor_take(c, list_length);
    if (!desc || !list) {
        *bad = true;
        return;
    }
    cursor_t names = { list, list_length, false };
    buffer_t text = {0};
    for (const char *d = desc; *d; d++) {
        buffer_put_u8(&text, (unsigned char)*d);
        if (*d != 'L') {
            continue;
        }
        const char *package = string_at(strings, strings_size, cursor_index(&names));
        const char *name = string_at(strings, strings_size, cursor_index(&names));
        if (names.failed || !package || !name) {
            *bad = true;
            break;
        }
        if (*package) {
            buffer_put(&text, package, strlen(package));
            buffer_put_u8(&text, '/');
        }
        buffer_put(&text, name, strlen(name));
    }
    if (!*bad && !text.failed) {
        put_utf8(out, (const char *)text.data, text.size, bad);
    } else {
        *bad = true;
    }
    free(text.data);
}

/* A class file whose constant pool refers to the strings table for some of its
 * Utf8 constants, put back as a plain class file */
static uint8_t *decompress_compact_cp(const uint8_t *in, size_t in_size, const uint8_t *strings,
                                      size_t strings_size, size_t *out_size)
{
    cursor_t c = { in, in_size, false };
    buffer_t out = {0};
    bool bad = false;

    const uint8_t *header = cursor_take(&c, 8);        /* Magic, minor, major */
    buffer_put(&out, header, header ? 8 : 0);
    unsigned count = cursor_u16(&c);
    buffer_put_u16(&out, count);
    for (unsigned i = 1; i < count && !c.failed && !out.failed && !bad; i++) {
        unsigned tag = cursor_u8(&c);
        if (tag == TAG_UTF8) {
            unsigned length = cursor_u16(&c);
            const uint8_t *text = cursor_take(&c, length);
            if (text) {
                put_utf8(&out, (const char *)text, length, &bad);
            }
        } else if (tag == TAG_EXTERNAL_STRING) {
            const char *text = string_at(strings, strings_size, cursor_index(&c));
            if (text) {
                put_utf8(&out, text, strlen(text), &bad);
            } else {
                bad = true;
            }
        } else if (tag == TAG_EXTERNAL_DESCRIPTOR) {
            put_descriptor(&out, strings, strings_size, &c, &bad);
        } else if (tag < sizeof(constant_sizes) && constant_sizes[tag]) {
            const uint8_t *body = cursor_take(&c, constant_sizes[tag]);
            buffer_put_u8(&out, tag);
            buffer_put(&out, body, body ? constant_sizes[tag] : 0);
            if (tag == TAG_LONG || tag == TAG_DOUBLE) {
                i++;        /* A long or double takes two slots */
            }
        } else {
            bad = true;
        }
    }
    if (!c.failed && !bad) {
        buffer_put(&out, c.p, c.left);      /* The rest of the class file as it was */
    }
    if (c.failed || bad || out.failed) {
        free(out.data);
        return NULL;
    }
    *out_size = out.size;
    return out.data;
}

static uint8_t *decompress_zip(const uint8_t *in, size_t in_size, size_t original_size,
                               size_t *out_size)
{
    uint8_t *out = malloc(original_size ? original_size : 1);
    if (!out) {
        return NULL;
    }
    z_stream zs;
    memset(&zs, 0, sizeof(zs));
    if (inflateInit(&zs) != Z_OK) {
        free(out);
        return NULL;
    }
    zs.next_in = (Bytef *)in;
    zs.avail_in = (uInt)in_size;
    zs.next_out = out;
    zs.avail_out = (uInt)original_size;
    int rc = inflate(&zs, Z_FINISH);
    size_t produced = original_size - zs.avail_out;
    inflateEnd(&zs);
    if (rc != Z_STREAM_END || produced != original_size) {
        free(out);
        return NULL;
    }
    *out_size = produced;
    return out;
}

/* Take the compression layers off a resource. Takes over data (malloc'd, *size bytes)
 * and returns the bytes under the layers, or NULL. */
static uint8_t *remove_layers(const jimage_store_t *store, uint8_t *data, size_t *size)
{
    while (*size >= LAYER_HEADER_SIZE && read_u32(data, store->swap) == LAYER_MAGIC) {
        uint64_t original = read_u64(data + 12, store->swap);
        const char *name = string_at(store->strings, store->strings_size,
                                     read_u32(data + 20, store->swap));
        const uint8_t *body = data + LAYER_HEADER_SIZE;
        size_t body_size = *size - LAYER_HEADER_SIZE;
        uint8_t *under = NULL;
        size_t under_size = 0;

        if (!name || original > MAX_RESOURCE_SIZE) {
            fprintf(stderr, "error: jimage compressed resource is corrupt\n");
        } else if (strcmp(name, "zip") == 0) {
            under = decompress_zip(body, body_size, (size_t)original, &under_size);
        } else if (strcmp(name, "compact-cp") == 0) {
            under = decompress_compact_cp(body, body_size, store->strings, store->strings_size,
                                          &under_size);
        } else {
            fprintf(stderr, "error: jimage resource compressed with unknown plugin \"%s\"\n", name);
        }
        free(data);
        if (!under) {
            return NULL;
        }
        data = under;
        *size = under_size;
    }
    return data;
}

static uint8_t *jimage_store_read(platform_image_t *img, const platform_class_t *c, size_t *size)
{
    jimage_store_t *store = (jimage_store_t *)img->store.data;
    size_t stored = c->stored_size ? c->stored_size : c->size;
    uint64_t start = store->index_size + c->offset;
    if (start > store->file_size || stored > store->file_size - start) {
        return NULL;
    }
    uint8_t *data = malloc(stored ? stored : 1);
    if (!data) {
        return NULL;
    }
    size_t done = 0;
    while (done < stored) {
        ssize_t n = pread(store->fd, data + done, stored - done, (off_t)(start + done));
        if (n <= 0) {
            free(data);
            return NULL;
        }
        done += (size_t)n;
    }
    size_t length = stored;
    if (c->stored_size != 0) {
        data = remove_layers(store, data, &length);
        if (data && length != c->size) {
            fprintf(stderr, "error: jimage compressed resource has the wrong size\n");
            free(data);
            data = NULL;
        }
    }
    if (data && size) {
        *size = length;
    }
    return data;
}

static void jimage_store_close(platform_image_t *img)
{
    jimage_store_t *store = (jimage_store_t *)img->store.data;
    if (store) {
        free(store->strings);
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
        if (strcmp(module, "modules") == 0 || strcmp(module, "packages") == 0) {
            continue;
        }
        /* Every module's module-info is kept, whether or not its classes are: what a
         * module exports can be asked of one that is not on the class path */
        bool is_module_info = attrs[ATTR_PARENT] == 0 && strcmp(base, "module-info") == 0;
        if (!is_module_info && !wanted_module(module, modules)) {
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

        if (is_module_info) {
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

    store->swap = swap;
    store->strings_size = (size_t)strings_size;
    store->strings = malloc(strings_size ? strings_size : 1);
    if (store->strings) {
        memcpy(store->strings, (const uint8_t *)map + index_size - strings_size, strings_size);
    }
    bool ok = store->strings && jimage_index_to_model(img, (const uint8_t *)map, table_length,
                                                      locations_size, strings_size, swap, modules);
    munmap(map, (size_t)index_size);
    if (!ok) {
        platform_image_free(img);
        return NULL;
    }
    return img;
}
