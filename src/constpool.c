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

uint16_t cp_add_utf8(const_pool_t *cp, const char *str)
{
    if (!cp || !str) {
        return 0;
    }
    
    /* Check cache first for deduplication */
    void *cached = hashtable_lookup(cp->utf8_cache, str);
    if (cached) {
        return (uint16_t)(uintptr_t)cached;
    }
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_UTF8;
    cp->entries[index].data.utf8 = strdup(str);
    
    /* Cache it for future lookups */
    hashtable_insert(cp->utf8_cache, str, (void *)(uintptr_t)index);
    
    return index;
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
    
    /* Search for existing CONST_CLASS with same name_index to avoid duplicates */
    for (uint16_t i = 1; i < cp->count; i++) {
        if (cp->entries[i].type == CONST_CLASS && 
            cp->entries[i].data.class_index == name_index) {
            return i;  /* Return existing entry */
        }
    }
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_CLASS;
    cp->entries[index].data.class_index = name_index;
    return index;
}

uint16_t cp_add_string(const_pool_t *cp, const char *str)
{
    if (!cp || !str) {
        return 0;
    }
    
    uint16_t utf8_index = cp_add_utf8(cp, str);
    
    uint16_t index = cp_add_entry(cp);
    cp->entries[index].type = CONST_STRING;
    cp->entries[index].data.string_index = utf8_index;
    return index;
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

