/*
 * type.c
 * Type system implementation for the genesis Java compiler
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
#include <stdio.h>
#include <pthread.h>
#include "type.h"

/* ========================================================================
 * Canonical Type Cache (Thread-Safe)
 * ========================================================================
 * Like javac's Symtab.classes map, we maintain a single canonical instance
 * for each class type. This ensures type identity comparison works correctly
 * in parallel compilation.
 */

#define TYPE_CACHE_SIZE 4096

typedef struct type_cache_entry {
    char *name;
    type_t *type;
    struct type_cache_entry *next;
} type_cache_entry_t;

static type_cache_entry_t *g_type_cache[TYPE_CACHE_SIZE];
static pthread_mutex_t g_type_cache_mutex = PTHREAD_MUTEX_INITIALIZER;
static int g_type_cache_enabled = 0;

/* Simple hash function for strings */
static unsigned int hash_string(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash;
}

/* Enable the canonical type cache (call before parallel compilation) */
void type_cache_enable(void) {
    g_type_cache_enabled = 1;
}

/* Disable the canonical type cache */
void type_cache_disable(void) {
    g_type_cache_enabled = 0;
}

/* Look up or create a canonical class type */
static type_t *type_cache_get_or_create(const char *name) {
    if (!name) return NULL;
    
    unsigned int hash = hash_string(name) % TYPE_CACHE_SIZE;
    
    pthread_mutex_lock(&g_type_cache_mutex);
    
    /* Look for existing entry */
    type_cache_entry_t *entry = g_type_cache[hash];
    while (entry) {
        if (strcmp(entry->name, name) == 0) {
            type_t *found = entry->type;
            pthread_mutex_unlock(&g_type_cache_mutex);
            return found;
        }
        entry = entry->next;
    }
    
    /* Not found - create new type and cache it */
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        pthread_mutex_unlock(&g_type_cache_mutex);
        return NULL;
    }
    type->kind = TYPE_CLASS;
    type->data.class_type.name = strdup(name);
    
    /* Add to cache */
    entry = calloc(1, sizeof(type_cache_entry_t));
    if (entry) {
        entry->name = strdup(name);
        entry->type = type;
        entry->next = g_type_cache[hash];
        g_type_cache[hash] = entry;
    }
    
    pthread_mutex_unlock(&g_type_cache_mutex);
    return type;
}

/* symbol_t is forward-declared in type.h */

/* We need access to symbol internals for type_assignable subtype checking.
 * This creates a dependency on the symbol structure, but it's necessary
 * for proper subtyping. We declare just what we need here. */
typedef enum symbol_kind {
    SYM_PACKAGE,
    SYM_CLASS,
    SYM_INTERFACE,
    SYM_ENUM,
    SYM_ANNOTATION,
    SYM_FIELD,
    SYM_METHOD,
    SYM_CONSTRUCTOR,
    SYM_PARAMETER,
    SYM_LOCAL_VAR,
    SYM_TYPE_PARAM,
} symbol_kind_t;

/* Minimal symbol access for subtype checking */
extern symbol_kind_t symbol_get_kind(symbol_t *sym);
extern const char *symbol_get_qualified_name(symbol_t *sym);
extern symbol_t *symbol_get_superclass(symbol_t *sym);
extern const char *symbol_get_unresolved_superclass(symbol_t *sym);
extern slist_t *symbol_get_interfaces(symbol_t *sym);

/* ========================================================================
 * Built-in Types (Singletons)
 * ======================================================================== */

static type_t *builtin_void = NULL;
static type_t *builtin_boolean = NULL;
static type_t *builtin_byte = NULL;
static type_t *builtin_char = NULL;
static type_t *builtin_short = NULL;
static type_t *builtin_int = NULL;
static type_t *builtin_long = NULL;
static type_t *builtin_float = NULL;
static type_t *builtin_double = NULL;
static type_t *builtin_null = NULL;
static type_t *builtin_string = NULL;
static type_t *builtin_object = NULL;

type_t *type_void(void)
{
    if (!builtin_void) {
        builtin_void = type_new_primitive(TYPE_VOID);
    }
    return builtin_void;
}

type_t *type_boolean(void)
{
    if (!builtin_boolean) {
        builtin_boolean = type_new_primitive(TYPE_BOOLEAN);
    }
    return builtin_boolean;
}

type_t *type_byte(void)
{
    if (!builtin_byte) {
        builtin_byte = type_new_primitive(TYPE_BYTE);
    }
    return builtin_byte;
}

type_t *type_char(void)
{
    if (!builtin_char) {
        builtin_char = type_new_primitive(TYPE_CHAR);
    }
    return builtin_char;
}

type_t *type_short(void)
{
    if (!builtin_short) {
        builtin_short = type_new_primitive(TYPE_SHORT);
    }
    return builtin_short;
}

type_t *type_int(void)
{
    if (!builtin_int) {
        builtin_int = type_new_primitive(TYPE_INT);
    }
    return builtin_int;
}

type_t *type_long(void)
{
    if (!builtin_long) {
        builtin_long = type_new_primitive(TYPE_LONG);
    }
    return builtin_long;
}

type_t *type_float(void)
{
    if (!builtin_float) {
        builtin_float = type_new_primitive(TYPE_FLOAT);
    }
    return builtin_float;
}

type_t *type_double(void)
{
    if (!builtin_double) {
        builtin_double = type_new_primitive(TYPE_DOUBLE);
    }
    return builtin_double;
}

type_t *type_null(void)
{
    if (!builtin_null) {
        builtin_null = type_new_primitive(TYPE_NULL);
    }
    return builtin_null;
}

type_t *type_string(void)
{
    if (!builtin_string) {
        builtin_string = type_new_class("java.lang.String");
    }
    return builtin_string;
}

type_t *type_object(void)
{
    if (!builtin_object) {
        builtin_object = type_new_class("java.lang.Object");
    }
    return builtin_object;
}

/* ========================================================================
 * Type Construction
 * ======================================================================== */

type_t *type_new_primitive(type_kind_t kind)
{
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = kind;
    return type;
}

type_t *type_new_class(const char *name)
{
    /* Always create fresh type instances. 
     * Type comparison in type_equals() uses name comparison, not identity.
     * The canonical cache is used for symbol sharing, not type identity. */
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = TYPE_CLASS;
    type->data.class_type.name = strdup(name);
    return type;
}

/**
 * Get or create a canonical symbol holder for a class name.
 * Used in parallel mode to share symbol references across threads.
 */
type_t *type_get_canonical(const char *name)
{
    if (g_type_cache_enabled) {
        return type_cache_get_or_create(name);
    }
    return type_new_class(name);
}

type_t *type_new_array(type_t *element, int dimensions)
{
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = TYPE_ARRAY;
    type->data.array_type.element_type = element;
    type->data.array_type.dimensions = dimensions;
    return type;
}

type_t *type_new_wildcard(int bound_kind, type_t *bound)
{
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = TYPE_WILDCARD;
    type->data.wildcard.bound_kind = bound_kind;
    type->data.wildcard.bound = bound;
    return type;
}

type_t *type_new_intersection(slist_t *types)
{
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = TYPE_INTERSECTION;
    type->data.intersection.types = types;
    return type;
}

void type_free(type_t *type)
{
    if (!type) {
        return;
    }
    
    /* Don't free built-in singletons */
    if (type == builtin_void || type == builtin_boolean ||
        type == builtin_byte || type == builtin_char ||
        type == builtin_short || type == builtin_int ||
        type == builtin_long || type == builtin_float ||
        type == builtin_double || type == builtin_null ||
        type == builtin_string || type == builtin_object) {
        return;
    }
    
    switch (type->kind) {
        case TYPE_CLASS:
            free(type->data.class_type.name);
            /* Don't free type_args - they're shared */
            break;
        case TYPE_ARRAY:
            /* Don't free element_type - might be shared */
            break;
        case TYPE_TYPEVAR:
            free(type->data.type_var.name);
            break;
        default:
            break;
    }
    
    free(type);
}

/* ========================================================================
 * Type Utilities
 * ======================================================================== */

char *type_to_string(type_t *type)
{
    if (!type) {
        return strdup("<null>");
    }
    
    switch (type->kind) {
        case TYPE_VOID:     return strdup("void");
        case TYPE_BOOLEAN:  return strdup("boolean");
        case TYPE_BYTE:     return strdup("byte");
        case TYPE_CHAR:     return strdup("char");
        case TYPE_SHORT:    return strdup("short");
        case TYPE_INT:      return strdup("int");
        case TYPE_LONG:     return strdup("long");
        case TYPE_FLOAT:    return strdup("float");
        case TYPE_DOUBLE:   return strdup("double");
        case TYPE_NULL:     return strdup("null");
        case TYPE_UNKNOWN:  return strdup("<unknown>");
        
        case TYPE_CLASS:
            {
                const char *name = type->data.class_type.name;
                slist_t *type_args = type->data.class_type.type_args;
                
                if (!type_args) {
                    return strdup(name);
                }
                
                /* Build string like "List<String, Integer>" */
                size_t total_len = strlen(name) + 2;  /* "Name<>" */
                for (slist_t *a = type_args; a; a = a->next) {
                    char *ts = type_to_string((type_t *)a->data);
                    total_len += strlen(ts) + 2;  /* ", " separator */
                    free(ts);
                }
                
                char *result = malloc(total_len + 1);
                strcpy(result, name);
                strcat(result, "<");
                
                bool first = true;
                for (slist_t *a = type_args; a; a = a->next) {
                    if (!first) {
                        strcat(result, ", ");
                    }
                    char *ts = type_to_string((type_t *)a->data);
                    strcat(result, ts);
                    free(ts);
                    first = false;
                }
                strcat(result, ">");
                return result;
            }
        
        case TYPE_ARRAY:
            {
                char *elem = type_to_string(type->data.array_type.element_type);
                size_t len = strlen(elem) + type->data.array_type.dimensions * 2 + 1;
                char *result = malloc(len);
                strcpy(result, elem);
                for (int i = 0; i < type->data.array_type.dimensions; i++) {
                    strcat(result, "[]");
                }
                free(elem);
                return result;
            }
        
        case TYPE_TYPEVAR:
            return strdup(type->data.type_var.name);
        
        case TYPE_WILDCARD:
            {
                if (type->data.wildcard.bound_kind == 0) {
                    return strdup("?");
                } else if (type->data.wildcard.bound_kind == 1 && type->data.wildcard.bound) {
                    char *bound_str = type_to_string(type->data.wildcard.bound);
                    size_t len = strlen(bound_str) + 12;
                    char *result = malloc(len);
                    snprintf(result, len, "? extends %s", bound_str);
                    free(bound_str);
                    return result;
                } else if (type->data.wildcard.bound_kind == -1 && type->data.wildcard.bound) {
                    char *bound_str = type_to_string(type->data.wildcard.bound);
                    size_t len = strlen(bound_str) + 10;
                    char *result = malloc(len);
                    snprintf(result, len, "? super %s", bound_str);
                    free(bound_str);
                    return result;
                }
                return strdup("?");
            }
        
        case TYPE_INTERSECTION:
            {
                /* Build string like "Type1 & Type2 & Type3" */
                slist_t *types = type->data.intersection.types;
                if (!types) {
                    return strdup("<empty intersection>");
                }
                
                /* Calculate total length needed */
                size_t total_len = 0;
                for (slist_t *t = types; t; t = t->next) {
                    char *ts = type_to_string((type_t *)t->data);
                    total_len += strlen(ts) + 3;  /* " & " separator */
                    free(ts);
                }
                
                char *result = malloc(total_len + 1);
                result[0] = '\0';
                
                bool first = true;
                for (slist_t *t = types; t; t = t->next) {
                    if (!first) {
                        strcat(result, " & ");
                    }
                    char *ts = type_to_string((type_t *)t->data);
                    strcat(result, ts);
                    free(ts);
                    first = false;
                }
                return result;
            }
        
        default:
            return strdup("<error>");
    }
}

/* Helper to normalize class names for comparison.
 * Handles simple name vs java.lang.* fully qualified name.
 * Also handles nested class names where '.' and '$' are equivalent separators.
 * e.g., "Outer.Inner.InnerInner" should equal "Outer$Inner$InnerInner" */
static bool class_names_equal(const char *name1, const char *name2)
{
    if (!name1 || !name2) {
        return name1 == name2;
    }
    
    /* Exact match */
    if (strcmp(name1, name2) == 0) {
        return true;
    }
    
    /* Try comparing with $ and . treated as equivalent.
     * First, find where the package ends and class name begins.
     * Package uses only '.', nested classes use either '.' or '$'.
     * We need to normalize nested class separators. */
    size_t len1 = strlen(name1);
    size_t len2 = strlen(name2);
    
    if (len1 == len2) {
        /* Same length - check if they differ only in $ vs . for inner classes */
        bool match = true;
        for (size_t i = 0; i < len1; i++) {
            char c1 = name1[i];
            char c2 = name2[i];
            if (c1 != c2) {
                /* Allow $ and . to be equivalent separators */
                if ((c1 == '$' || c1 == '.') && (c2 == '$' || c2 == '.')) {
                    continue;
                }
                match = false;
                break;
            }
        }
        if (match) {
            return true;
        }
    }
    
    /* Check if one is java.lang.X and other is just X */
    const char *simple1 = strrchr(name1, '.');
    const char *dollar1 = strrchr(name1, '$');
    if (dollar1 > simple1) {
        simple1 = dollar1;
    }
    
    const char *simple2 = strrchr(name2, '.');
    const char *dollar2 = strrchr(name2, '$');
    if (dollar2 > simple2) {
        simple2 = dollar2;
    }
    
    if (simple1) {
        simple1++;
    } else {
        simple1 = name1;
    }
    if (simple2) {
        simple2++;
    } else {
        simple2 = name2;
    }
    
    /* If simple names match, check if qualified is java.lang.* */
    if (strcmp(simple1, simple2) == 0) {
        if (strncmp(name1, "java.lang.", 10) == 0 && strcmp(name1 + 10, simple2) == 0) {
            return true;
        }
        if (strncmp(name2, "java.lang.", 10) == 0 && strcmp(name2 + 10, simple1) == 0) {
            return true;
        }
    }
    
    return false;
}

/**
 * Get the total dimensions and base element type for an array type.
 * Handles both flat (dims=2, elem=byte) and nested (dims=1, elem=byte[]) representations.
 */
static void get_array_info(type_t *arr, int *total_dims, type_t **base_elem)
{
    *total_dims = 0;
    *base_elem = arr;
    
    while (*base_elem && (*base_elem)->kind == TYPE_ARRAY) {
        *total_dims += (*base_elem)->data.array_type.dimensions;
        *base_elem = (*base_elem)->data.array_type.element_type;
    }
}

bool type_equals(type_t *a, type_t *b)
{
    if (a == b) {
        return true;
    }
    if (!a || !b) {
        return false;
    }
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case TYPE_CLASS:
            return class_names_equal(a->data.class_type.name, b->data.class_type.name);
        
        case TYPE_ARRAY: {
            /* Handle different array representations:
             * byte[][] can be either (dims=2, elem=byte) or (dims=1, elem=byte[])
             * Normalize both to total dimensions and base element type */
            int dims_a, dims_b;
            type_t *base_a, *base_b;
            get_array_info(a, &dims_a, &base_a);
            get_array_info(b, &dims_b, &base_b);
            return dims_a == dims_b && type_equals(base_a, base_b);
        }
        
        case TYPE_TYPEVAR:
            return strcmp(a->data.type_var.name, b->data.type_var.name) == 0;
        
        default:
            /* Primitives - same kind means equal */
            return true;
    }
}

bool type_is_numeric(type_t *type)
{
    if (!type) {
        return false;
    }
    switch (type->kind) {
        case TYPE_BYTE:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
        case TYPE_FLOAT:
        case TYPE_DOUBLE:
        case TYPE_CHAR:
            return true;
        default:
            return false;
    }
}

bool type_is_integral(type_t *type)
{
    if (!type) {
        return false;
    }
    switch (type->kind) {
        case TYPE_BYTE:
        case TYPE_SHORT:
        case TYPE_INT:
        case TYPE_LONG:
        case TYPE_CHAR:
            return true;
        default:
            return false;
    }
}

bool type_is_reference(type_t *type)
{
    if (!type) {
        return false;
    }
    return type->kind == TYPE_CLASS || 
           type->kind == TYPE_ARRAY ||
           type->kind == TYPE_NULL ||
           type->kind == TYPE_TYPEVAR ||
           type->kind == TYPE_WILDCARD;
}

/* ========================================================================
 * Autoboxing/Unboxing Support
 * ======================================================================== */

const char *get_wrapper_class(type_kind_t kind)
{
    switch (kind) {
        case TYPE_INT:     return "java.lang.Integer";
        case TYPE_LONG:    return "java.lang.Long";
        case TYPE_DOUBLE:  return "java.lang.Double";
        case TYPE_FLOAT:   return "java.lang.Float";
        case TYPE_BYTE:    return "java.lang.Byte";
        case TYPE_SHORT:   return "java.lang.Short";
        case TYPE_CHAR:    return "java.lang.Character";
        case TYPE_BOOLEAN: return "java.lang.Boolean";
        default:           return NULL;
    }
}

/**
 * Get the boxed (wrapper) type for a primitive type.
 * Returns a new TYPE_CLASS for the wrapper, or the original type if not primitive.
 */
type_t *type_boxed(type_t *type)
{
    if (!type) {
        return type;
    }
    
    const char *wrapper = get_wrapper_class(type->kind);
    if (wrapper) {
        return type_new_class(wrapper);
    }
    
    return type;
}

type_kind_t get_primitive_for_wrapper(const char *class_name)
{
    if (!class_name) {
        return TYPE_UNKNOWN;
    }
    
    if (strcmp(class_name, "java.lang.Integer") == 0 ||
        strcmp(class_name, "java/lang/Integer") == 0 ||
        strcmp(class_name, "Integer") == 0) {
        return TYPE_INT;
    }
    if (strcmp(class_name, "java.lang.Long") == 0 ||
        strcmp(class_name, "java/lang/Long") == 0 ||
        strcmp(class_name, "Long") == 0) {
        return TYPE_LONG;
    }
    if (strcmp(class_name, "java.lang.Double") == 0 ||
        strcmp(class_name, "java/lang/Double") == 0 ||
        strcmp(class_name, "Double") == 0) {
        return TYPE_DOUBLE;
    }
    if (strcmp(class_name, "java.lang.Float") == 0 ||
        strcmp(class_name, "java/lang/Float") == 0 ||
        strcmp(class_name, "Float") == 0) {
        return TYPE_FLOAT;
    }
    if (strcmp(class_name, "java.lang.Byte") == 0 ||
        strcmp(class_name, "java/lang/Byte") == 0 ||
        strcmp(class_name, "Byte") == 0) {
        return TYPE_BYTE;
    }
    if (strcmp(class_name, "java.lang.Short") == 0 ||
        strcmp(class_name, "java/lang/Short") == 0 ||
        strcmp(class_name, "Short") == 0) {
        return TYPE_SHORT;
    }
    if (strcmp(class_name, "java.lang.Character") == 0 ||
        strcmp(class_name, "java/lang/Character") == 0 ||
        strcmp(class_name, "Character") == 0) {
        return TYPE_CHAR;
    }
    if (strcmp(class_name, "java.lang.Boolean") == 0 ||
        strcmp(class_name, "java/lang/Boolean") == 0 ||
        strcmp(class_name, "Boolean") == 0) {
        return TYPE_BOOLEAN;
    }
    
    return TYPE_UNKNOWN;
}

bool type_needs_boxing(type_t *target, type_t *source)
{
    if (!target || !source) {
        return false;
    }
    if (target->kind != TYPE_CLASS) {
        return false;
    }
    if (!type_is_numeric(source) && source->kind != TYPE_BOOLEAN) {
        return false;
    }
    
    const char *wrapper = get_wrapper_class(source->kind);
    if (!wrapper) {
        return false;
    }
    
    if (!target->data.class_type.name) {
        return false;
    }
    
    return strcmp(target->data.class_type.name, wrapper) == 0 ||
           strcmp(target->data.class_type.name, wrapper + 10) == 0;  /* Skip "java.lang." */
}

bool type_needs_unboxing(type_t *target, type_t *source)
{
    if (!target || !source) {
        return false;
    }
    if (source->kind != TYPE_CLASS) {
        return false;
    }
    if (!type_is_numeric(target) && target->kind != TYPE_BOOLEAN) {
        return false;
    }
    if (!source->data.class_type.name) {
        return false;
    }
    
    type_kind_t prim = get_primitive_for_wrapper(source->data.class_type.name);
    if (prim == TYPE_UNKNOWN) {
        return false;
    }
    
    /* Check if target matches the unboxed type (with widening allowed) */
    if (target->kind == prim) {
        return true;
    }
    
    /* Allow widening after unboxing (e.g., Integer -> long) */
    if (type_is_numeric(target)) {
        int target_rank = 0, prim_rank = 0;
        switch (target->kind) {
            case TYPE_DOUBLE: target_rank = 6; break;
            case TYPE_FLOAT:  target_rank = 5; break;
            case TYPE_LONG:   target_rank = 4; break;
            case TYPE_INT:    target_rank = 3; break;
            case TYPE_SHORT:  target_rank = 2; break;
            case TYPE_CHAR:   target_rank = 2; break;
            case TYPE_BYTE:   target_rank = 1; break;
            default: break;
        }
        switch (prim) {
            case TYPE_DOUBLE: prim_rank = 6; break;
            case TYPE_FLOAT:  prim_rank = 5; break;
            case TYPE_LONG:   prim_rank = 4; break;
            case TYPE_INT:    prim_rank = 3; break;
            case TYPE_SHORT:  prim_rank = 2; break;
            case TYPE_CHAR:   prim_rank = 2; break;
            case TYPE_BYTE:   prim_rank = 1; break;
            default: break;
        }
        return target_rank >= prim_rank;
    }
    
    return false;
}

/* ========================================================================
 * Type Assignment Compatibility
 * ======================================================================== */

/* External function to lazily resolve interfaces for a symbol */
extern void ensure_interfaces_resolved(void *sem, symbol_t *sym);

/**
 * Recursively check if a symbol (class or interface) implements or extends
 * a target interface.
 * 
 * @param sem The semantic analyzer context (can be NULL)
 * @param sym The symbol to check
 * @param target_sym The target interface symbol (can be NULL)
 * @param target_name The target interface name (used if target_sym is NULL)
 * @param depth Recursion depth limit to prevent infinite loops
 * @return true if sym implements/extends the target
 */
static bool symbol_implements_interface_with_sem(void *sem, symbol_t *sym, symbol_t *target_sym, 
                                        const char *target_name, int depth)
{
    if (!sym || depth > 20) {
        return false;  /* Limit depth to prevent infinite recursion */
    }
    
    /* First check if sym itself IS the target (for class extension, not just interface implementation) */
    if (sym == target_sym) {
        return true;
    }
    const char *sym_name = symbol_get_qualified_name(sym);
    if (sym_name && target_name && strcmp(sym_name, target_name) == 0) {
        return true;
    }
    
    /* Ensure interfaces are resolved for this symbol */
    if (sem) {
        ensure_interfaces_resolved(sem, sym);
    }
    
    /* Check direct interfaces of this symbol */
    slist_t *ifaces = symbol_get_interfaces(sym);
    
    while (ifaces) {
        symbol_t *iface = (symbol_t *)ifaces->data;
        if (!iface) {
            ifaces = ifaces->next;
            continue;
        }
        
        /* Ensure the interface's own interfaces are resolved */
        if (sem) {
            ensure_interfaces_resolved(sem, iface);
        }
        
        /* Direct match by symbol pointer */
        if (iface == target_sym) {
            return true;
        }
        
        /* Match by qualified name */
        const char *iface_name = symbol_get_qualified_name(iface);
        if (iface_name && target_name && strcmp(iface_name, target_name) == 0) {
            return true;
        }
        
        /* Recursively check if this interface extends the target */
        if (symbol_implements_interface_with_sem(sem, iface, target_sym, target_name, depth + 1)) {
            return true;
        }
        
        ifaces = ifaces->next;
    }
    
    /* Check superclass hierarchy */
    symbol_t *super = symbol_get_superclass(sym);
    
    /* If superclass is wrong (Object when it should be something else),
     * try to resolve superclass from the unresolved name */
    const char *unresolved_super = symbol_get_unresolved_superclass(sym);
    if (unresolved_super && sem && symbol_get_kind(sym) != SYM_INTERFACE) {
        /* We have an unresolved superclass name - resolve it now */
        const char *super_name = super ? symbol_get_qualified_name(super) : NULL;
        /* Only replace if current super is Object (default) but we have a real superclass name */
        if (!super || (super_name && strcmp(super_name, "java.lang.Object") == 0)) {
            extern symbol_t *load_external_class(void *sem, const char *name);
            symbol_t *real_super = NULL;
            
            /* Try direct load first (for fully qualified names) */
            real_super = load_external_class(sem, unresolved_super);
            
            /* Try common package prefixes for gumdrop classes */
            if (!real_super && !strchr(unresolved_super, '.')) {
                static const char *prefixes[] = {
                    "org.bluezoo.gumdrop.http.",
                    "org.bluezoo.gumdrop.",
                    "org.bluezoo.gumdrop.servlet.",
                    "org.bluezoo.gumdrop.pop3.",
                    "org.bluezoo.gumdrop.smtp.",
                    "org.bluezoo.gumdrop.ftp.",
                    "org.bluezoo.gumdrop.imap.",
                    NULL
                };
                for (int i = 0; prefixes[i] && !real_super; i++) {
                    char buf[512];
                    snprintf(buf, sizeof(buf), "%s%s", prefixes[i], unresolved_super);
                    real_super = load_external_class(sem, buf);
                }
            }
            
            if (real_super) {
                super = real_super;
                /* Update the symbol's superclass for future lookups */
                extern void symbol_set_superclass(symbol_t *sym, symbol_t *super);
                symbol_set_superclass(sym, real_super);
            }
        }
    }
    /* If still no super and this isn't Object or an interface, default to Object */
    if (!super && symbol_get_kind(sym) != SYM_INTERFACE) {
        const char *sym_name = symbol_get_qualified_name(sym);
        if (!sym_name || strcmp(sym_name, "java.lang.Object") != 0) {
            extern symbol_t *load_external_class(void *sem, const char *name);
            if (sem) {
                super = load_external_class(sem, "java.lang.Object");
            }
        }
    }
    
    if (super && symbol_implements_interface_with_sem(sem, super, target_sym, target_name, depth + 1)) {
        return true;
    }
    
    return false;
}

/**
 * Wrapper for backward compatibility - gets current semantic context.
 */
static bool symbol_implements_interface(symbol_t *sym, symbol_t *target_sym, 
                                        const char *target_name, int depth)
{
    extern void *get_current_semantic(void);
    void *sem = get_current_semantic();
    return symbol_implements_interface_with_sem(sem, sym, target_sym, target_name, depth);
}

bool type_assignable(type_t *target, type_t *source)
{
    if (!target || !source) {
        return false;
    }
    
    /* Same type */
    if (type_equals(target, source)) {
        return true;
    }
    
    /* null assignable to any reference type */
    if (source->kind == TYPE_NULL && type_is_reference(target)) {
        return true;
    }
    
    /* Numeric widening conversions */
    if (type_is_numeric(target) && type_is_numeric(source)) {
        /* byte -> short -> int -> long -> float -> double */
        /* char -> int -> long -> float -> double */
        int target_rank = 0, source_rank = 0;
        
        switch (target->kind) {
            case TYPE_DOUBLE: target_rank = 6; break;
            case TYPE_FLOAT:  target_rank = 5; break;
            case TYPE_LONG:   target_rank = 4; break;
            case TYPE_INT:    target_rank = 3; break;
            case TYPE_SHORT:  target_rank = 2; break;
            case TYPE_CHAR:   target_rank = 2; break;
            case TYPE_BYTE:   target_rank = 1; break;
            default: break;
        }
        
        switch (source->kind) {
            case TYPE_DOUBLE: source_rank = 6; break;
            case TYPE_FLOAT:  source_rank = 5; break;
            case TYPE_LONG:   source_rank = 4; break;
            case TYPE_INT:    source_rank = 3; break;
            case TYPE_SHORT:  source_rank = 2; break;
            case TYPE_CHAR:   source_rank = 2; break;
            case TYPE_BYTE:   source_rank = 1; break;
            default: break;
        }
        
        return target_rank >= source_rank;
    }
    
    /* Auto-boxing: primitive to Object or wrapper class */
    if (target->kind == TYPE_CLASS && 
        (source->kind >= TYPE_BOOLEAN && source->kind <= TYPE_DOUBLE)) {
        const char *target_name = target->data.class_type.name;
        if (!target_name) {
            return false;
        }
        
        /* Any primitive can be boxed and widened to Object */
        if (strcmp(target_name, "java.lang.Object") == 0) {
            return true;
        }
        
        /* Check primitive-to-wrapper compatibility */
        const char *expected_wrapper = NULL;
        switch (source->kind) {
            case TYPE_BOOLEAN: expected_wrapper = "java.lang.Boolean"; break;
            case TYPE_BYTE:    expected_wrapper = "java.lang.Byte"; break;
            case TYPE_CHAR:    expected_wrapper = "java.lang.Character"; break;
            case TYPE_SHORT:   expected_wrapper = "java.lang.Short"; break;
            case TYPE_INT:     expected_wrapper = "java.lang.Integer"; break;
            case TYPE_LONG:    expected_wrapper = "java.lang.Long"; break;
            case TYPE_FLOAT:   expected_wrapper = "java.lang.Float"; break;
            case TYPE_DOUBLE:  expected_wrapper = "java.lang.Double"; break;
            default: break;
        }
        
        if (expected_wrapper && strcmp(target_name, expected_wrapper) == 0) {
            return true;
        }
        
        /* Also allow widening after boxing: int -> Long, Integer -> Number, etc. */
        /* For simplicity, just allow boxing to Number for numeric types */
        if (type_is_numeric(source)) {
            if (strcmp(target_name, "java.lang.Number") == 0) {
                return true;
            }
        }
    }
    
    /* Reference type widening (subtyping) */
    if (target->kind == TYPE_CLASS && source->kind == TYPE_CLASS) {
        /* Object is assignable from any class */
        if (strcmp(target->data.class_type.name, "java.lang.Object") == 0) {
            return true;
        }
        
        symbol_t *target_sym = target->data.class_type.symbol;
        symbol_t *source_sym = source->data.class_type.symbol;
        
        /* Get target name for comparison */
        const char *target_name = target_sym ? symbol_get_qualified_name(target_sym) 
                                              : target->data.class_type.name;
        
        /* Fallback: check by name when symbols aren't loaded.
         * This handles wrapper classes like Double, Integer, Long which
         * extend Number but may not have their symbols loaded. */
        if (!source_sym && source->data.class_type.name && target_name) {
            const char *source_name = source->data.class_type.name;
            /* java.lang wrapper classes that extend Number */
            if (strcmp(target_name, "java.lang.Number") == 0) {
                if (strcmp(source_name, "java.lang.Double") == 0 ||
                    strcmp(source_name, "java.lang.Float") == 0 ||
                    strcmp(source_name, "java.lang.Integer") == 0 ||
                    strcmp(source_name, "java.lang.Long") == 0 ||
                    strcmp(source_name, "java.lang.Short") == 0 ||
                    strcmp(source_name, "java.lang.Byte") == 0 ||
                    strcmp(source_name, "java.math.BigInteger") == 0 ||
                    strcmp(source_name, "java.math.BigDecimal") == 0) {
                    return true;
                }
            }
            /* Common wrapper/interface relationships */
            if (strcmp(target_name, "java.lang.Comparable") == 0) {
                /* All wrapper types implement Comparable */
                if (strcmp(source_name, "java.lang.Double") == 0 ||
                    strcmp(source_name, "java.lang.Float") == 0 ||
                    strcmp(source_name, "java.lang.Integer") == 0 ||
                    strcmp(source_name, "java.lang.Long") == 0 ||
                    strcmp(source_name, "java.lang.Short") == 0 ||
                    strcmp(source_name, "java.lang.Byte") == 0 ||
                    strcmp(source_name, "java.lang.String") == 0 ||
                    strcmp(source_name, "java.lang.Boolean") == 0 ||
                    strcmp(source_name, "java.lang.Character") == 0) {
                    return true;
                }
            }
        }
        
        /* Try to load source symbol if not present */
        if (!source_sym && source->data.class_type.name) {
            /* Use the semantic analyzer's class loading if available */
            extern symbol_t *load_external_class(void *sem, const char *name);
            extern void *get_current_semantic(void);
            void *sem = get_current_semantic();
            if (sem) {
                source_sym = load_external_class(sem, source->data.class_type.name);
                if (source_sym) {
                    /* Cache the symbol in the type for future use */
                    source->data.class_type.symbol = source_sym;
                }
            }
        }
        
        /* Before checking interfaces, ensure the full superclass chain is resolved.
         * This is critical for parallel compilation where intermediate classes
         * may not have their superclass links fully populated yet. */
        if (source_sym) {
            extern void *get_current_semantic(void);
            extern symbol_t *load_external_class(void *sem, const char *name);
            void *sem = get_current_semantic();
            if (sem) {
                symbol_t *s = source_sym;
                int depth = 0;
                while (s && depth < 30) {
                    /* If no superclass but not Object/Interface, try to resolve */
                    if (!symbol_get_superclass(s) && symbol_get_kind(s) != SYM_INTERFACE) {
                        const char *s_name = symbol_get_qualified_name(s);
                        if (!s_name || strcmp(s_name, "java.lang.Object") != 0) {
                            const char *unresolved = symbol_get_unresolved_superclass(s);
                            symbol_t *super_sym = NULL;
                            if (unresolved) {
                                super_sym = load_external_class(sem, unresolved);
                            }
                            if (!super_sym) {
                                super_sym = load_external_class(sem, "java.lang.Object");
                            }
                            if (super_sym) {
                                /* Note: This modifies shared state, but the check is atomic enough */
                                extern void symbol_set_superclass(symbol_t *sym, symbol_t *super);
                                symbol_set_superclass(s, super_sym);
                            }
                        }
                    }
                    /* Also ensure interfaces are resolved */
                    extern void ensure_interfaces_resolved(void *sem, symbol_t *sym);
                    ensure_interfaces_resolved(sem, s);
                    s = symbol_get_superclass(s);
                    depth++;
                }
            }
        }
        
        /* Use recursive helper to check full interface/class hierarchy */
        if (source_sym && symbol_implements_interface(source_sym, target_sym, target_name, 0)) {
            return true;
        }
        
        
        /* Also check if source class extends target class directly */
        if (source_sym) {
            symbol_t *super = symbol_get_superclass(source_sym);
            int depth = 0;
            
            if (getenv("GENESIS_DEBUG_SUBTYPE")) {
                const char *src_name = symbol_get_qualified_name(source_sym);
                fprintf(stderr, "DEBUG subtype: checking superclass chain for '%s' (sym=%p)\n", 
                        src_name ? src_name : "(null)", (void*)source_sym);
                fprintf(stderr, "  target='%s', initial super=%p\n", 
                        target_name ? target_name : "(null)", (void*)super);
            }
            
            while (super && depth < 50) {  /* Limit depth to prevent infinite loops */
                const char *super_name = symbol_get_qualified_name(super);
                
                if (getenv("GENESIS_DEBUG_SUBTYPE")) {
                    fprintf(stderr, "  depth=%d super='%s' (sym=%p)\n", depth, 
                            super_name ? super_name : "(null)", (void*)super);
                }
                
                if (super == target_sym) {
                    return true;  /* Source extends target class */
                }
                if (super_name && target_name && strcmp(super_name, target_name) == 0) {
                    return true;
                }
                
                /* If superclass has unresolved superclass, try to load it */
                symbol_t *next_super = symbol_get_superclass(super);
                if (!next_super && super_name && strcmp(super_name, "java.lang.Object") != 0) {
                    /* Try to load the superclass's superclass from semantic context */
                    extern symbol_t *load_external_class(void *sem, const char *name);
                    extern void *get_current_semantic(void);
                    void *sem = get_current_semantic();
                    if (sem) {
                        /* Re-load this symbol to ensure its superclass chain is populated */
                        symbol_t *reloaded = load_external_class(sem, super_name);
                        if (reloaded && reloaded != super) {
                            next_super = symbol_get_superclass(reloaded);
                            if (getenv("GENESIS_DEBUG_SUBTYPE")) {
                                fprintf(stderr, "    reloaded '%s' -> next_super=%p\n", 
                                        super_name, (void*)next_super);
                            }
                        }
                    }
                }
                
                super = next_super;
                depth++;
            }
            
            if (getenv("GENESIS_DEBUG_SUBTYPE")) {
                fprintf(stderr, "  superclass chain did NOT match target\n");
            }
        }
    }
    
    /* Array subtyping */
    if (target->kind == TYPE_ARRAY && source->kind == TYPE_ARRAY) {
        /* Arrays are covariant for reference element types */
        if (target->data.array_type.dimensions == source->data.array_type.dimensions) {
            type_t *te = target->data.array_type.element_type;
            type_t *se = source->data.array_type.element_type;
            if (type_is_reference(te) && type_is_reference(se)) {
                return type_assignable(te, se);
            }
        }
        /* Multi-dimensional array to Object[]: T[][] is assignable to Object[]
         * because T[] is assignable to Object */
        else if (target->data.array_type.dimensions == 1 &&
                 source->data.array_type.dimensions > 1) {
            type_t *te = target->data.array_type.element_type;
            /* Check if target element is Object */
            if (te && te->kind == TYPE_CLASS && 
                strcmp(te->data.class_type.name, "java.lang.Object") == 0) {
                return true;  /* Any array[][] is assignable to Object[] */
            }
        }
    }
    
    /* Any array is also assignable to Object (not just Object[]) */
    if (target->kind == TYPE_CLASS && source->kind == TYPE_ARRAY) {
        if (strcmp(target->data.class_type.name, "java.lang.Object") == 0) {
            return true;
        }
    }
    
    /* Autoboxing: primitive -> wrapper (e.g., int -> Integer) */
    if (type_needs_boxing(target, source)) {
        return true;
    }
    
    /* Auto-unboxing: wrapper -> primitive (e.g., Integer -> int) */
    if (type_needs_unboxing(target, source)) {
        return true;
    }
    
    /* Type variable handling (generics):
     * A type variable T is assignable to its bound type.
     * Source of type variable T is assignable to target if T's bound is assignable. */
    if (source->kind == TYPE_TYPEVAR) {
        /* Use the bound for assignment checking */
        type_t *bound = source->data.type_var.bound;
        if (bound) {
            return type_assignable(target, bound);
        }
        /* Unbounded type var treated as Object */
        type_t obj = { .kind = TYPE_CLASS, .data.class_type.name = "java.lang.Object" };
        return type_assignable(target, &obj);
    }
    
    /* Target is type variable - anything assignable to the bound is assignable to T */
    if (target->kind == TYPE_TYPEVAR) {
        type_t *bound = target->data.type_var.bound;
        if (bound) {
            return type_assignable(bound, source);
        }
        /* Unbounded type var accepts any reference type.
         * Also accept primitives since they will be autoboxed. */
        if (type_is_reference(source)) {
            return true;
        }
        /* Accept primitives that can be boxed */
        const char *wrapper = get_wrapper_class(source->kind);
        return wrapper != NULL;
    }
    
    /* Wildcard type handling:
     * For ? extends T (covariant): can read as T, source must be subtype of T
     * For ? super T (contravariant): can write T, target must be supertype of T
     * For ? (unbounded): same as ? extends Object */
    if (target->kind == TYPE_WILDCARD) {
        if (target->data.wildcard.bound_kind >= 0) {
            /* Unbounded (?) or upper bounded (? extends T) */
            type_t *bound = target->data.wildcard.bound;
            if (bound) {
                /* Source must be assignable to the bound */
                return type_assignable(bound, source);
            }
            /* Unbounded wildcard accepts any reference type */
            return type_is_reference(source);
        } else {
            /* Lower bounded (? super T) */
            type_t *bound = target->data.wildcard.bound;
            if (bound) {
                /* The bound must be assignable to source (contravariant) */
                return type_assignable(source, bound);
            }
            return true;
        }
    }
    
    if (source->kind == TYPE_WILDCARD) {
        /* Source is wildcard - use its upper bound for assignment */
        if (source->data.wildcard.bound_kind >= 0 && source->data.wildcard.bound) {
            return type_assignable(target, source->data.wildcard.bound);
        }
        /* Unbounded or lower-bounded wildcard produces Object */
        type_t obj = { .kind = TYPE_CLASS, .data.class_type.name = "java.lang.Object" };
        return type_assignable(target, &obj);
    }
    
    /* Intersection type handling:
     * An intersection type T1 & T2 & ... is assignable to a target
     * if ALL component types are assignable to target (typically just Object).
     * Conversely, source is assignable TO intersection if assignable to ALL components. */
    if (source->kind == TYPE_INTERSECTION) {
        /* Intersection -> target: 
         * The intersection is a subtype of each component,
         * so it's assignable to target if ANY component is assignable to target */
        slist_t *types = source->data.intersection.types;
        for (slist_t *t = types; t; t = t->next) {
            type_t *component = (type_t *)t->data;
            if (type_assignable(target, component)) {
                return true;
            }
        }
        return false;
    }
    
    if (target->kind == TYPE_INTERSECTION) {
        /* source -> Intersection:
         * Source must be assignable to ALL components of the intersection */
        slist_t *types = target->data.intersection.types;
        for (slist_t *t = types; t; t = t->next) {
            type_t *component = (type_t *)t->data;
            if (!type_assignable(component, source)) {
                return false;
            }
        }
        return true;
    }
    
    return false;
}

/* ========================================================================
 * JVM Type Codes
 * ======================================================================== */

int type_kind_to_atype(type_kind_t kind)
{
    switch (kind) {
        case TYPE_BOOLEAN: return 4;   /* T_BOOLEAN */
        case TYPE_CHAR:    return 5;   /* T_CHAR */
        case TYPE_FLOAT:   return 6;   /* T_FLOAT */
        case TYPE_DOUBLE:  return 7;   /* T_DOUBLE */
        case TYPE_BYTE:    return 8;   /* T_BYTE */
        case TYPE_SHORT:   return 9;   /* T_SHORT */
        case TYPE_INT:     return 10;  /* T_INT */
        case TYPE_LONG:    return 11;  /* T_LONG */
        default:           return -1;  /* Not a primitive */
    }
}

int type_name_to_atype(const char *type_name)
{
    if (!type_name) {
        return -1;
    }
    
    if (strcmp(type_name, "boolean") == 0) {
        return 4;   /* T_BOOLEAN */
    }
    if (strcmp(type_name, "char") == 0) {
        return 5;   /* T_CHAR */
    }
    if (strcmp(type_name, "float") == 0) {
        return 6;   /* T_FLOAT */
    }
    if (strcmp(type_name, "double") == 0) {
        return 7;   /* T_DOUBLE */
    }
    if (strcmp(type_name, "byte") == 0) {
        return 8;   /* T_BYTE */
    }
    if (strcmp(type_name, "short") == 0) {
        return 9;   /* T_SHORT */
    }
    if (strcmp(type_name, "int") == 0) {
        return 10;  /* T_INT */
    }
    if (strcmp(type_name, "long") == 0) {
        return 11;  /* T_LONG */
    }
    
    return -1;  /* Not a primitive type name */
}

