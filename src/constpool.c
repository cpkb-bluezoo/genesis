/*
 * constpool.c
 * Constant pool management for the genesis Java compiler
 * Copyright (C) 2016, 2020 Chris Burdess <dog@gnu.org>
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
#include "constpool.h"

/* ========================================================================
 * Constant Pool Implementation
 * ======================================================================== */

const_pool_t *const_pool_new(void)
{
    const_pool_t *cp = calloc(1, sizeof(const_pool_t));
    if (!cp) {
        return NULL;
    }
    
    cp->capacity = 256;
    cp->entries = calloc(cp->capacity, sizeof(const_pool_entry_t));
    if (!cp->entries) {
        free(cp);
        return NULL;
    }
    
    /* Index 0 is unused per JVM spec */
    cp->count = 1;
    cp->utf8_cache = hashtable_new();
    
    return cp;
}

void const_pool_free(const_pool_t *cp)
{
    if (!cp) {
        return;
    }
    
    /* Free UTF8 strings */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_UTF8) {
            /* Don't double-free: set to NULL after freeing */
            if (cp->entries[i].data.utf8) {
                free(cp->entries[i].data.utf8);
                cp->entries[i].data.utf8 = NULL;
            }
        }
    }
    
    free(cp->entries);
    free(cp->class_by_name);
    hashtable_free(cp->utf8_cache);
    free(cp);
}

/**
 * Allocate a new entry in the constant pool.
 */
static uint16_t cp_add_entry(const_pool_t *cp)
{
    if (cp->count >= cp->capacity) {
        size_t old_capacity = cp->capacity;
        cp->capacity *= 2;
        cp->entries = realloc(cp->entries, cp->capacity * sizeof(const_pool_entry_t));
        /* Zero new entries to avoid uninitialized memory issues */
        memset(&cp->entries[old_capacity], 0, 
               (cp->capacity - old_capacity) * sizeof(const_pool_entry_t));
    }
    return cp->count++;
}

uint16_t cp_add_utf8_len(const_pool_t *cp, const char *str, size_t len)
{
    if (!cp || !str) {
        return 0;
    }

    /* The dedup cache (cp->utf8_cache) is a plain hashtable_t keyed by
     * 'const char *' - it hashes/compares that key as an ordinary
     * NUL-terminated C string, so it can only safely represent a value
     * with no embedded NUL byte of its own. When len != strlen(str),
     * the value has an embedded NUL (a JLS 3.10.6 octal escape, e.g.
     * "\0alice\0s3cret") - skip the cache entirely for it rather than
     * risk an incorrect collision (e.g. matching a previously-added
     * empty string, since both hash/compare as "" up to the first NUL).
     * This is a rare edge case; the only cost of skipping dedup for it
     * is a possible duplicate (but individually correct) UTF8 entry,
     * which the classfile format explicitly permits. */
    bool has_embedded_nul = (len != strlen(str));

    if (!has_embedded_nul) {
        /* Check cache first for deduplication */
        void *cached = hashtable_lookup(cp->utf8_cache, str);
        if (cached) {
            return (uint16_t)(uintptr_t)cached;
        }
    }

    char *copy = malloc(len + 1);
    if (!copy) {
        return 0;
    }
    memcpy(copy, str, len);
    copy[len] = '\0';

    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_UTF8;
    cp->entries[index].data.utf8 = copy;
    cp->entries[index].utf8_len = (uint16_t)len;

    if (!has_embedded_nul) {
        /* Cache it for future lookups */
        hashtable_insert(cp->utf8_cache, str, (void *)(uintptr_t)index);
    }

    return index;
}

uint16_t cp_add_utf8(const_pool_t *cp, const char *str)
{
    if (!cp || !str) {
        return 0;
    }
    return cp_add_utf8_len(cp, str, strlen(str));
}

size_t cp_utf8_modified_length(const char *bytes, size_t len)
{
    size_t extra = 0;
    for (size_t i = 0; i < len; i++) {
        if (bytes[i] == '\0') {
            extra++;  /* 0x00 -> 0xC0 0x80: one extra byte */
        }
    }
    return len + extra;
}

void cp_utf8_modified_write(uint8_t **p, const char *bytes, size_t len)
{
    uint8_t *out = *p;
    for (size_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)bytes[i];
        if (c == 0) {
            *out++ = 0xC0;
            *out++ = 0x80;
        } else {
            *out++ = c;
        }
    }
    *p = out;
}

uint16_t cp_add_integer(const_pool_t *cp, int32_t value)
{
    if (!cp) {
        return 0;
    }
    
    /* Check for existing entry with same value (deduplication) */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_INTEGER && 
            cp->entries[i].data.integer == value) {
            return i;
        }
    }
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_INTEGER;
    cp->entries[index].data.integer = value;
    return index;
}

uint16_t cp_add_float(const_pool_t *cp, float value)
{
    if (!cp) {
        return 0;
    }

    /* Deduplicate (mirrors cp_add_integer) - callers that need the same
     * constant twice across separate passes (e.g. an annotation element's
     * value pre-added before the constant pool is serialized, then
     * looked up again while writing the actual attribute) must get back
     * the same index each time, not a second entry added too late to be
     * serialized. */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_FLOAT &&
            cp->entries[i].data.float_val == value) {
            return i;
        }
    }

    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_FLOAT;
    cp->entries[index].data.float_val = value;
    return index;
}

uint16_t cp_add_long(const_pool_t *cp, int64_t value)
{
    if (!cp) {
        return 0;
    }

    /* Deduplicate - see cp_add_float() for why this matters, not just for
     * space. */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_LONG &&
            cp->entries[i].data.long_val == value) {
            return i;
        }
    }

    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_LONG;
    cp->entries[index].data.long_val = value;

    /* Long and Double take two constant pool slots */
    cp_add_entry(cp);

    return index;
}

uint16_t cp_add_double(const_pool_t *cp, double value)
{
    if (!cp) {
        return 0;
    }

    /* Deduplicate - see cp_add_float() for why this matters, not just for
     * space. */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_DOUBLE &&
            cp->entries[i].data.double_val == value) {
            return i;
        }
    }

    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_DOUBLE;
    cp->entries[index].data.double_val = value;

    /* Long and Double take two constant pool slots */
    cp_add_entry(cp);

    return index;
}

uint16_t cp_add_class(const_pool_t *cp, const char *name)
{
    if (!cp || !name) {
        return 0;
    }
    
    uint16_t name_index = cp_add_utf8(cp, name);
    
    /* Reuse the existing CONST_CLASS entry for this name, if there is one.
     * Class entries are only ever created here, one per name, so a table
     * from UTF8 index to class index finds it directly. (This function is
     * called for every class reference in the code and every reference
     * type pushed in a stack map frame; searching the whole pool each
     * time was a noticeable part of code generation.) */
    if ((size_t)name_index >= cp->class_by_name_size) {
        size_t new_size = cp->class_by_name_size ? cp->class_by_name_size : 256;
        while (new_size <= (size_t)name_index) {
            new_size *= 2;
        }
        uint16_t *table = realloc(cp->class_by_name, new_size * sizeof(uint16_t));
        if (!table) {
            return 0;
        }
        memset(table + cp->class_by_name_size, 0,
               (new_size - cp->class_by_name_size) * sizeof(uint16_t));
        cp->class_by_name = table;
        cp->class_by_name_size = new_size;
    }
    if (cp->class_by_name[name_index]) {
        return cp->class_by_name[name_index];  /* Return existing entry */
    }
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_CLASS;
    cp->entries[index].data.class_index = name_index;
    cp->class_by_name[name_index] = index;
    return index;
}

uint16_t cp_add_string_len(const_pool_t *cp, const char *str, size_t len)
{
    if (!cp || !str) {
        return 0;
    }

    uint16_t utf8_index = cp_add_utf8_len(cp, str, len);

    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_STRING;
    cp->entries[index].data.string_index = utf8_index;
    return index;
}

uint16_t cp_add_string(const_pool_t *cp, const char *str)
{
    if (!cp || !str) {
        return 0;
    }
    return cp_add_string_len(cp, str, strlen(str));
}

uint16_t cp_add_name_and_type(const_pool_t *cp, const char *name, const char *descriptor)
{
    if (!cp || !name || !descriptor) {
        return 0;
    }
    
    uint16_t name_index = cp_add_utf8(cp, name);
    uint16_t desc_index = cp_add_utf8(cp, descriptor);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_NAME_AND_TYPE;
    cp->entries[index].data.name_type.name_index = name_index;
    cp->entries[index].data.name_type.descriptor_index = desc_index;
    return index;
}

uint16_t cp_add_fieldref(const_pool_t *cp, const char *class_name,
                          const char *name, const char *descriptor)
{
    if (!cp) {
        return 0;
    }
    
    uint16_t class_index = cp_add_class(cp, class_name);
    uint16_t name_type_index = cp_add_name_and_type(cp, name, descriptor);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_FIELDREF;
    cp->entries[index].data.ref.class_index = class_index;
    cp->entries[index].data.ref.name_type_index = name_type_index;
    return index;
}

uint16_t cp_add_methodref(const_pool_t *cp, const char *class_name,
                           const char *name, const char *descriptor)
{
    if (!cp) {
        return 0;
    }
    
    uint16_t class_index = cp_add_class(cp, class_name);
    uint16_t name_type_index = cp_add_name_and_type(cp, name, descriptor);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_METHODREF;
    cp->entries[index].data.ref.class_index = class_index;
    cp->entries[index].data.ref.name_type_index = name_type_index;
    return index;
}

uint16_t cp_add_interface_methodref(const_pool_t *cp, const char *class_name,
                                     const char *name, const char *descriptor)
{
    if (!cp) {
        return 0;
    }
    
    uint16_t class_index = cp_add_class(cp, class_name);
    uint16_t name_type_index = cp_add_name_and_type(cp, name, descriptor);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_INTERFACE_METHODREF;
    cp->entries[index].data.ref.class_index = class_index;
    cp->entries[index].data.ref.name_type_index = name_type_index;
    return index;
}

uint16_t cp_add_module(const_pool_t *cp, const char *name)
{
    if (!cp || !name) {
        return 0;
    }
    
    uint16_t name_index = cp_add_utf8(cp, name);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_MODULE;
    cp->entries[index].data.class_index = name_index;  /* Reuse class_index field */
    return index;
}

uint16_t cp_add_package(const_pool_t *cp, const char *name)
{
    if (!cp || !name) {
        return 0;
    }
    
    uint16_t name_index = cp_add_utf8(cp, name);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_PACKAGE;
    cp->entries[index].data.class_index = name_index;  /* Reuse class_index field */
    return index;
}

