/*
 * type.h
 * Type system definitions for the genesis Java compiler
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

#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include "util.h"

/* Forward declarations */
typedef struct symbol symbol_t;

/* ========================================================================
 * Type Kinds
 * ======================================================================== */

typedef enum type_kind
{
    TYPE_VOID,
    TYPE_BOOLEAN,
    TYPE_BYTE,
    TYPE_CHAR,
    TYPE_SHORT,
    TYPE_INT,
    TYPE_LONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_NULL,              /* The null type */
    TYPE_CLASS,             /* Reference type (class/interface) */
    TYPE_ARRAY,             /* Array type */
    TYPE_TYPEVAR,           /* Type variable (generics) */
    TYPE_WILDCARD,          /* Wildcard type (?, ? extends T, ? super T) */
    TYPE_UNKNOWN,           /* Unresolved/error type */
} type_kind_t;

/* ========================================================================
 * Type Structure
 * ======================================================================== */

typedef struct type type_t;

struct type
{
    type_kind_t kind;
    union {
        /* For TYPE_CLASS */
        struct {
            char *name;             /* Fully qualified name */
            symbol_t *symbol;       /* Resolved class symbol */
            slist_t *type_args;     /* Generic type arguments (list of type_t*) */
        } class_type;
        
        /* For TYPE_ARRAY */
        struct {
            type_t *element_type;   /* Element type */
            int dimensions;         /* Number of dimensions */
        } array_type;
        
        /* For TYPE_TYPEVAR */
        struct {
            char *name;             /* Type variable name */
            type_t *bound;          /* Upper bound (extends) */
        } type_var;
        
        /* For TYPE_WILDCARD */
        struct {
            int bound_kind;         /* 0=unbounded, 1=extends, -1=super */
            type_t *bound;          /* Bound type (NULL if unbounded) */
        } wildcard;
    } data;
};

/* ========================================================================
 * Type Construction
 * ======================================================================== */

/**
 * Create a new primitive type.
 */
type_t *type_new_primitive(type_kind_t kind);

/**
 * Create a new class/reference type.
 */
type_t *type_new_class(const char *name);

/**
 * Create a new array type.
 */
type_t *type_new_array(type_t *element, int dimensions);

/**
 * Create a new wildcard type.
 * @param bound_kind  0=unbounded (?), 1=extends (? extends T), -1=super (? super T)
 * @param bound       The bound type (NULL for unbounded)
 */
type_t *type_new_wildcard(int bound_kind, type_t *bound);

/**
 * Free a type (careful with singletons - built-in types are not freed).
 */
void type_free(type_t *type);

/* ========================================================================
 * Built-in Types (Singletons)
 * ======================================================================== */

type_t *type_void(void);
type_t *type_boolean(void);
type_t *type_byte(void);
type_t *type_char(void);
type_t *type_short(void);
type_t *type_int(void);
type_t *type_long(void);
type_t *type_float(void);
type_t *type_double(void);
type_t *type_null(void);
type_t *type_string(void);
type_t *type_object(void);

/* ========================================================================
 * Type Utilities
 * ======================================================================== */

/**
 * Convert type to string representation.
 * @return Allocated string (caller must free)
 */
char *type_to_string(type_t *type);

/**
 * Check if two types are equal.
 */
bool type_equals(type_t *a, type_t *b);

/**
 * Check if source type is assignable to target type.
 * Handles widening conversions, subtyping, and autoboxing.
 */
bool type_assignable(type_t *target, type_t *source);

/**
 * Check if type is numeric (byte, short, int, long, float, double, char).
 */
bool type_is_numeric(type_t *type);

/**
 * Check if type is integral (byte, short, int, long, char).
 */
bool type_is_integral(type_t *type);

/**
 * Check if type is a reference type (class, array, or null).
 */
bool type_is_reference(type_t *type);

/* ========================================================================
 * Autoboxing/Unboxing Support
 * ======================================================================== */

/**
 * Get the wrapper class name for a primitive type.
 * @return Fully qualified wrapper class name, or NULL if not a primitive
 */
const char *get_wrapper_class(type_kind_t kind);

/**
 * Get the primitive type for a wrapper class.
 * Handles dot notation (java.lang.Integer), internal format (java/lang/Integer),
 * and simple names (Integer).
 * @return Primitive type kind, or TYPE_UNKNOWN if not a wrapper class
 */
type_kind_t get_primitive_for_wrapper(const char *class_name);

/**
 * Check if target type can be assigned from source via boxing.
 * (source is primitive, target is wrapper)
 */
bool type_needs_boxing(type_t *target, type_t *source);

/**
 * Check if target type can be assigned from source via unboxing.
 * (source is wrapper, target is primitive)
 */
bool type_needs_unboxing(type_t *target, type_t *source);

/* ========================================================================
 * JVM Type Codes
 * ======================================================================== */

/**
 * Get the JVM array type code (atype) for NEWARRAY instruction.
 * These are the T_* constants from the JVM spec:
 *   T_BOOLEAN=4, T_CHAR=5, T_FLOAT=6, T_DOUBLE=7,
 *   T_BYTE=8, T_SHORT=9, T_INT=10, T_LONG=11
 * 
 * @return atype code (4-11), or -1 if not a primitive type
 */
int type_kind_to_atype(type_kind_t kind);

/**
 * Get the JVM array type code from a primitive type name string.
 * Handles "boolean", "byte", "char", "short", "int", "long", "float", "double".
 * 
 * @return atype code (4-11), or -1 if not a primitive type name
 */
int type_name_to_atype(const char *type_name);

#endif /* TYPE_H */

