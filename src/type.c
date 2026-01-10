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
#include "type.h"

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
    type_t *type = calloc(1, sizeof(type_t));
    if (!type) {
        return NULL;
    }
    type->kind = TYPE_CLASS;
    type->data.class_type.name = strdup(name);
    return type;
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
            return strdup(type->data.class_type.name);
        
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
        
        default:
            return strdup("<error>");
    }
}

/* Helper to normalize class names for comparison.
 * Handles simple name vs java.lang.* fully qualified name. */
static bool class_names_equal(const char *name1, const char *name2)
{
    if (!name1 || !name2) {
        return name1 == name2;
    }
    
    /* Exact match */
    if (strcmp(name1, name2) == 0) {
        return true;
    }
    
    /* Check if one is java.lang.X and other is just X */
    const char *simple1 = strrchr(name1, '.');
    const char *simple2 = strrchr(name2, '.');
    
    if (simple1) simple1++; else simple1 = name1;
    if (simple2) simple2++; else simple2 = name2;
    
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
        
        case TYPE_ARRAY:
            return a->data.array_type.dimensions == b->data.array_type.dimensions &&
                   type_equals(a->data.array_type.element_type, 
                              b->data.array_type.element_type);
        
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

/**
 * Recursively check if a symbol (class or interface) implements or extends
 * a target interface.
 * 
 * @param sym The symbol to check
 * @param target_sym The target interface symbol (can be NULL)
 * @param target_name The target interface name (used if target_sym is NULL)
 * @param depth Recursion depth limit to prevent infinite loops
 * @return true if sym implements/extends the target
 */
static bool symbol_implements_interface(symbol_t *sym, symbol_t *target_sym, 
                                        const char *target_name, int depth)
{
    if (!sym || depth > 20) {
        return false;  /* Limit depth to prevent infinite recursion */
    }
    
    /* Check direct interfaces of this symbol */
    slist_t *ifaces = symbol_get_interfaces(sym);
    while (ifaces) {
        symbol_t *iface = (symbol_t *)ifaces->data;
        if (!iface) {
            ifaces = ifaces->next;
            continue;
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
        if (symbol_implements_interface(iface, target_sym, target_name, depth + 1)) {
            return true;
        }
        
        ifaces = ifaces->next;
    }
    
    /* Check superclass hierarchy */
    symbol_t *super = symbol_get_superclass(sym);
    if (super && symbol_implements_interface(super, target_sym, target_name, depth + 1)) {
        return true;
    }
    
    return false;
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
        if (!target_name) return false;
        
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
        
        /* Use recursive helper to check full interface/class hierarchy */
        if (source_sym && symbol_implements_interface(source_sym, target_sym, target_name, 0)) {
            return true;
        }
        
        /* Also check if source class extends target class directly */
        if (source_sym) {
            symbol_t *super = symbol_get_superclass(source_sym);
            while (super) {
                if (super == target_sym) {
                    return true;  /* Source extends target class */
                }
                const char *super_name = symbol_get_qualified_name(super);
                if (super_name && target_name && strcmp(super_name, target_name) == 0) {
                    return true;
                }
                super = symbol_get_superclass(super);
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
        /* Unbounded type var accepts any reference type */
        return type_is_reference(source);
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
    if (!type_name) return -1;
    
    if (strcmp(type_name, "boolean") == 0) return 4;   /* T_BOOLEAN */
    if (strcmp(type_name, "char") == 0)    return 5;   /* T_CHAR */
    if (strcmp(type_name, "float") == 0)   return 6;   /* T_FLOAT */
    if (strcmp(type_name, "double") == 0)  return 7;   /* T_DOUBLE */
    if (strcmp(type_name, "byte") == 0)    return 8;   /* T_BYTE */
    if (strcmp(type_name, "short") == 0)   return 9;   /* T_SHORT */
    if (strcmp(type_name, "int") == 0)     return 10;  /* T_INT */
    if (strcmp(type_name, "long") == 0)    return 11;  /* T_LONG */
    
    return -1;  /* Not a primitive type name */
}

