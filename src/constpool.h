/*
 * constpool.h
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

#ifndef CONSTPOOL_H
#define CONSTPOOL_H

#include <stdint.h>
#include "util.h"

/*
 * Constant Pool Tag Types (class file constant pool)
 */
typedef enum const_pool_tag
{
    CONST_UTF8               = 1,
    CONST_INTEGER            = 3,
    CONST_FLOAT              = 4,
    CONST_LONG               = 5,
    CONST_DOUBLE             = 6,
    CONST_CLASS              = 7,
    CONST_STRING             = 8,
    CONST_FIELDREF           = 9,
    CONST_METHODREF          = 10,
    CONST_INTERFACE_METHODREF = 11,
    CONST_NAME_AND_TYPE      = 12,
    CONST_METHOD_HANDLE      = 15,
    CONST_METHOD_TYPE        = 16,
    CONST_DYNAMIC            = 17,
    CONST_INVOKE_DYNAMIC     = 18,
    CONST_MODULE             = 19,
    CONST_PACKAGE            = 20
} const_pool_tag_t;

/*
 * Constant Pool Entry
 */
typedef struct const_pool_entry
{
    const_pool_tag_t type;
    union {
        char *utf8;                 /* CONST_UTF8 */
        int32_t integer;            /* CONST_INTEGER */
        float float_val;            /* CONST_FLOAT */
        int64_t long_val;           /* CONST_LONG */
        double double_val;          /* CONST_DOUBLE */
        uint16_t class_index;       /* CONST_CLASS, CONST_STRING, CONST_METHOD_TYPE - index to UTF8 */
        uint16_t string_index;      /* CONST_STRING - index to UTF8 (alias) */
        struct {
            uint16_t class_index;
            uint16_t name_type_index;
        } ref;                      /* CONST_FIELDREF, CONST_METHODREF, CONST_INTERFACE_METHODREF,
                                       CONST_METHOD_HANDLE (kind, ref), CONST_INVOKE_DYNAMIC (bsm, nat) */
        struct {
            uint16_t name_index;
            uint16_t descriptor_index;
        } name_type;                /* CONST_NAME_AND_TYPE */
    } data;
} const_pool_entry_t;

/*
 * Constant Pool Builder
 */
typedef struct const_pool
{
    const_pool_entry_t *entries;
    uint16_t count;
    uint16_t capacity;
    hashtable_t *utf8_cache;        /* Cache for UTF8 deduplication */
} const_pool_t;

/* ========================================================================
 * Constant Pool Management
 * ======================================================================== */

/**
 * Create a new constant pool.
 * Index 0 is reserved (unused per JVM spec).
 *
 * @return New constant pool, or NULL on allocation failure
 */
const_pool_t *const_pool_new(void);

/**
 * Free a constant pool and all its entries.
 *
 * @param cp Constant pool to free
 */
void const_pool_free(const_pool_t *cp);

/**
 * Add a UTF8 string entry to the constant pool.
 * Duplicate strings are deduplicated via caching.
 *
 * @param cp  Constant pool
 * @param str UTF8 string value
 * @return    Constant pool index, or 0 on error
 */
uint16_t cp_add_utf8(const_pool_t *cp, const char *str);

/**
 * Add an integer constant to the constant pool.
 *
 * @param cp    Constant pool
 * @param value Integer value
 * @return      Constant pool index, or 0 on error
 */
uint16_t cp_add_integer(const_pool_t *cp, int32_t value);

/**
 * Add a float constant to the constant pool.
 *
 * @param cp    Constant pool
 * @param value Float value
 * @return      Constant pool index, or 0 on error
 */
uint16_t cp_add_float(const_pool_t *cp, float value);

/**
 * Add a long constant to the constant pool.
 * Note: Long entries occupy two constant pool slots.
 *
 * @param cp    Constant pool
 * @param value Long value
 * @return      Constant pool index, or 0 on error
 */
uint16_t cp_add_long(const_pool_t *cp, int64_t value);

/**
 * Add a double constant to the constant pool.
 * Note: Double entries occupy two constant pool slots.
 *
 * @param cp    Constant pool
 * @param value Double value
 * @return      Constant pool index, or 0 on error
 */
uint16_t cp_add_double(const_pool_t *cp, double value);

/**
 * Add a class reference to the constant pool.
 * Creates the required UTF8 entry for the class name.
 *
 * @param cp   Constant pool
 * @param name Internal class name (e.g., "java/lang/Object")
 * @return     Constant pool index, or 0 on error
 */
uint16_t cp_add_class(const_pool_t *cp, const char *name);

/**
 * Add a string constant to the constant pool.
 * Creates the required UTF8 entry for the string value.
 *
 * @param cp  Constant pool
 * @param str String value
 * @return    Constant pool index, or 0 on error
 */
uint16_t cp_add_string(const_pool_t *cp, const char *str);

/**
 * Add a NameAndType entry to the constant pool.
 * Creates the required UTF8 entries for name and descriptor.
 *
 * @param cp         Constant pool
 * @param name       Member name
 * @param descriptor Member type descriptor
 * @return           Constant pool index, or 0 on error
 */
uint16_t cp_add_name_and_type(const_pool_t *cp, const char *name,
                               const char *descriptor);

/**
 * Add a field reference to the constant pool.
 * Creates the required Class and NameAndType entries.
 *
 * @param cp         Constant pool
 * @param class_name Internal class name
 * @param name       Field name
 * @param descriptor Field type descriptor
 * @return           Constant pool index, or 0 on error
 */
uint16_t cp_add_fieldref(const_pool_t *cp, const char *class_name,
                          const char *name, const char *descriptor);

/**
 * Add a method reference to the constant pool.
 * Creates the required Class and NameAndType entries.
 *
 * @param cp         Constant pool
 * @param class_name Internal class name
 * @param name       Method name
 * @param descriptor Method type descriptor
 * @return           Constant pool index, or 0 on error
 */
uint16_t cp_add_methodref(const_pool_t *cp, const char *class_name,
                           const char *name, const char *descriptor);

/**
 * Add an interface method reference to the constant pool.
 * Creates the required Class and NameAndType entries.
 *
 * @param cp         Constant pool
 * @param class_name Internal interface name
 * @param name       Method name
 * @param descriptor Method type descriptor
 * @return           Constant pool index, or 0 on error
 */
uint16_t cp_add_interface_methodref(const_pool_t *cp, const char *class_name,
                                     const char *name, const char *descriptor);

/**
 * Add a module reference to the constant pool (Java 9+).
 * Creates the required UTF8 entry for the module name.
 *
 * @param cp   Constant pool
 * @param name Module name (e.g., "java.base")
 * @return     Constant pool index, or 0 on error
 */
uint16_t cp_add_module(const_pool_t *cp, const char *name);

/**
 * Add a package reference to the constant pool (Java 9+).
 * Creates the required UTF8 entry for the package name.
 *
 * @param cp   Constant pool
 * @param name Internal package name (e.g., "java/lang")
 * @return     Constant pool index, or 0 on error
 */
uint16_t cp_add_package(const_pool_t *cp, const char *name);

#endif /* CONSTPOOL_H */

