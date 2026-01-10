/*
 * codegen_expr.c
 * Expression bytecode generation for the JVM
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

#include "codegen_internal.h"
#include "classpath.h"

/* ========================================================================
 * REFACTORING NOTE: This file (7600+ lines) should be split into:
 * 
 * 1. codegen_expr.c    - Main dispatcher (codegen_expr, codegen_expression)
 *                        + simple expression handlers (literal, identifier)
 * 2. codegen_call.c    - Method call handling (codegen_method_call ~1000 lines)
 *                        + constructor calls + related helpers
 * 3. codegen_binary.c  - Binary operations (codegen_binary_expr ~1000 lines)
 *                        + string concatenation
 * 4. codegen_array.c   - Array creation/initialization (~500 lines)
 *                        (codegen_new_array, codegen_array_init)
 * 5. codegen_boxing.c  - Boxing/unboxing helpers (~200 lines)
 * 
 * Dependencies to consider:
 * - Helper functions like resolve_java_lang_class, class_to_internal_name
 * - Forward declarations for circular dependencies
 * - codegen_internal.h may need updating for shared declarations
 * ======================================================================== */

/* ========================================================================
 * Helper Functions
 * ======================================================================== */

/**
 * Convert a field descriptor to a class name suitable for stackmap.
 * - "Ljava/io/PrintStream;" -> "java/io/PrintStream"
 * - "[Ljava/lang/String;" -> "[Ljava/lang/String;" (arrays keep descriptor)
 * - "I", "J", etc. -> NULL (primitives)
 * Returns a newly allocated string that must be freed, or NULL for primitives.
 */
static char *descriptor_to_class_name(const char *descriptor)
{
    if (!descriptor || !descriptor[0]) return NULL;
    
    if (descriptor[0] == '[') {
        /* Array type - use descriptor as-is */
        return strdup(descriptor);
    } else if (descriptor[0] == 'L') {
        /* Object type - strip L prefix and ; suffix */
        size_t len = strlen(descriptor);
        if (len < 3) return NULL;  /* At least "L;" */
        char *name = malloc(len - 1);  /* -2 for L and ;, +1 for null */
        memcpy(name, descriptor + 1, len - 2);
        name[len - 2] = '\0';
        return name;
    }
    
    /* Primitive type */
    return NULL;
}

/**
 * Push an object type to the stackmap based on a field descriptor.
 */
static void mg_push_object_from_descriptor(method_gen_t *mg, const char *descriptor)
{
    if (!mg || !descriptor) return;
    
    char *class_name = descriptor_to_class_name(descriptor);
    if (class_name) {
        mg_push_object(mg, class_name);
        free(class_name);
    } else {
        /* Fallback - shouldn't happen for L/[ types */
        mg_push_object(mg, "java/lang/Object");
    }
}

/**
 * Look up a method in a class and its superclass chain.
 * Returns the method symbol and sets *owner_class to the class where it was found.
 */
/**
 * Look up a method by name, trying both direct lookup and "()" suffix.
 * Methods loaded from classfiles use "()" suffix to avoid field name collisions.
 */
static symbol_t *lookup_method_by_name(scope_t *scope, const char *method_name)
{
    if (!scope) {
        return NULL;
    }
    
    /* Use scope_lookup_method which handles all key formats:
     * - Direct name (legacy)
     * - name() (old classfile format)
     * - name(descriptor) (new classfile format)
     * - name(N) (source-defined with param count)
     */
    return scope_lookup_method(scope, method_name);
}

/**
 * SAM search context for finding the single abstract method in a functional interface.
 */
typedef struct mref_sam_search {
    symbol_t *sam;
    int abstract_count;
} mref_sam_search_t;

static void mref_find_sam_fn(const char *key, void *value, void *user_data)
{
    (void)key;
    mref_sam_search_t *search = (mref_sam_search_t *)user_data;
    symbol_t *sym = (symbol_t *)value;
    if (sym && sym->kind == SYM_METHOD &&
        !(sym->modifiers & MOD_STATIC) &&
        !(sym->modifiers & MOD_DEFAULT)) {
        /* This is an abstract instance method (interface methods are implicitly abstract) */
        search->abstract_count++;
        if (search->abstract_count == 1) {
            search->sam = sym;
        } else {
            /* More than one - not a functional interface */
            search->sam = NULL;
        }
    }
}

/* Forward declaration for recursive interface search */
static symbol_t *lookup_method_in_interfaces(symbol_t *iface, const char *method_name, 
                                              symbol_t **owner_class);

static symbol_t *lookup_method_in_hierarchy(symbol_t *class_sym, const char *method_name, 
                                            symbol_t **owner_class)
{
    symbol_t *current = class_sym;
    while (current) {
        if (current->data.class_data.members) {
            symbol_t *method = lookup_method_by_name(current->data.class_data.members, method_name);
            if (method) {
                if (owner_class) {
                    *owner_class = current;
                }
                return method;
            }
        }
        
        /* Search implemented interfaces for default methods (recursively) */
        slist_t *interfaces = current->data.class_data.interfaces;
        while (interfaces) {
            symbol_t *iface = (symbol_t *)interfaces->data;
            if (iface) {
                symbol_t *method = lookup_method_in_interfaces(iface, method_name, owner_class);
                if (method) {
                    return method;
                }
            }
            interfaces = interfaces->next;
        }
        
        /* Move to superclass */
        current = current->data.class_data.superclass;
    }
    return NULL;
}

/**
 * Recursively search an interface and its super-interfaces for a method.
 * Handles interface hierarchy like Collection -> List -> ArrayList.
 */
static symbol_t *lookup_method_in_interfaces(symbol_t *iface, const char *method_name, 
                                              symbol_t **owner_class)
{
    if (!iface) return NULL;
    
    /* Check direct members of this interface */
    if (iface->data.class_data.members) {
        symbol_t *method = lookup_method_by_name(iface->data.class_data.members, method_name);
        if (method && (method->modifiers & MOD_DEFAULT)) {
            /* Found a default method in this interface */
            if (owner_class) {
                *owner_class = iface;
            }
            return method;
        }
    }
    
    /* Recursively search super-interfaces */
    slist_t *super_ifaces = iface->data.class_data.interfaces;
    while (super_ifaces) {
        symbol_t *super_iface = (symbol_t *)super_ifaces->data;
        if (super_iface) {
            symbol_t *method = lookup_method_in_interfaces(super_iface, method_name, owner_class);
            if (method) {
                return method;
            }
        }
        super_ifaces = super_ifaces->next;
    }
    
    return NULL;
}

/**
 * Resolve a simple class name to its fully qualified internal name.
 * Handles common java.lang classes that may be used without import.
 */
const char *resolve_java_lang_class(const char *name)
{
    if (!name) {
        return "java/lang/Object";
    }
    
    /* If already contains a package separator, use as-is */
    if (strchr(name, '.') || strchr(name, '/')) {
        return name;
    }
    
    /* Check for common java.lang classes */
    static const char *java_lang_classes[] = {
        "Object",
        "String",
        "StringBuilder",
        "StringBuffer",
        "Integer",
        "Long",
        "Float",
        "Double",
        "Boolean",
        "Byte",
        "Short",
        "Character",
        "Number",
        "Math",
        "System",
        "Class",
        "ClassLoader",
        "Thread",
        "Runnable",
        "Enum",
        "Iterable",
        "Comparable",
        "Cloneable",
        /* Exceptions and Errors */
        "Throwable",
        "Exception",
        "RuntimeException",
        "Error",
        "ArithmeticException",
        "ArrayIndexOutOfBoundsException",
        "ArrayStoreException",
        "ClassCastException",
        "ClassNotFoundException",
        "CloneNotSupportedException",
        "IllegalAccessException",
        "IllegalArgumentException",
        "IllegalMonitorStateException",
        "IllegalStateException",
        "IllegalThreadStateException",
        "IndexOutOfBoundsException",
        "InstantiationException",
        "InterruptedException",
        "NegativeArraySizeException",
        "NoSuchFieldException",
        "NoSuchMethodException",
        "NullPointerException",
        "NumberFormatException",
        "SecurityException",
        "StringIndexOutOfBoundsException",
        "UnsupportedOperationException",
        "AssertionError",
        "LinkageError",
        "OutOfMemoryError",
        "StackOverflowError",
        "VirtualMachineError",
        NULL
    };
    
    for (const char **c = java_lang_classes; *c; c++) {
        if (strcmp(name, *c) == 0) {
            static char buf[128];
            snprintf(buf, sizeof(buf), "java/lang/%s", name);
            return buf;
        }
    }
    
    /* Not a known java.lang class - return as-is */
    return name;
}

/* ========================================================================
 * Autoboxing/Unboxing Code Generation
 * ======================================================================== */

/**
 * Emit bytecode to box a primitive value on top of the stack.
 * Uses Integer.valueOf(), Long.valueOf(), etc.
 */
bool emit_boxing(method_gen_t *mg, const_pool_t *cp, type_kind_t prim_kind)
{
    const char *wrapper_class;
    const char *descriptor;
    
    switch (prim_kind) {
        case TYPE_INT:
            wrapper_class = "java/lang/Integer";
            descriptor = "(I)Ljava/lang/Integer;";
            break;
        case TYPE_LONG:
            wrapper_class = "java/lang/Long";
            descriptor = "(J)Ljava/lang/Long;";
            break;
        case TYPE_DOUBLE:
            wrapper_class = "java/lang/Double";
            descriptor = "(D)Ljava/lang/Double;";
            break;
        case TYPE_FLOAT:
            wrapper_class = "java/lang/Float";
            descriptor = "(F)Ljava/lang/Float;";
            break;
        case TYPE_BYTE:
            wrapper_class = "java/lang/Byte";
            descriptor = "(B)Ljava/lang/Byte;";
            break;
        case TYPE_SHORT:
            wrapper_class = "java/lang/Short";
            descriptor = "(S)Ljava/lang/Short;";
            break;
        case TYPE_CHAR:
            wrapper_class = "java/lang/Character";
            descriptor = "(C)Ljava/lang/Character;";
            break;
        case TYPE_BOOLEAN:
            wrapper_class = "java/lang/Boolean";
            descriptor = "(Z)Ljava/lang/Boolean;";
            break;
        default:
            fprintf(stderr, "codegen: cannot box type %d\n", prim_kind);
            return false;
    }
    
    uint16_t methodref = cp_add_methodref(cp, wrapper_class, "valueOf", descriptor);
    bc_emit(mg->code, OP_INVOKESTATIC);
    bc_emit_u2(mg->code, methodref);
    
    /* Stack: primitive -> reference (adjusting for wide types) */
    if (prim_kind == TYPE_LONG || prim_kind == TYPE_DOUBLE) {
        mg_pop_typed(mg, 1);  /* Was 2 slots, now 1 */
    }
    
    return true;
}

/**
 * Emit bytecode to unbox a wrapper object on top of the stack.
 * Uses intValue(), longValue(), etc.
 */
bool emit_unboxing(method_gen_t *mg, const_pool_t *cp, type_kind_t target_prim, const char *wrapper_class)
{
    const char *method;
    const char *descriptor;
    int stack_adjust = 0;  /* Amount to push after unboxing */
    
    switch (target_prim) {
        case TYPE_INT:
            method = "intValue";
            descriptor = "()I";
            break;
        case TYPE_LONG:
            method = "longValue";
            descriptor = "()J";
            stack_adjust = 1;  /* 1 slot -> 2 slots */
            break;
        case TYPE_DOUBLE:
            method = "doubleValue";
            descriptor = "()D";
            stack_adjust = 1;  /* 1 slot -> 2 slots */
            break;
        case TYPE_FLOAT:
            method = "floatValue";
            descriptor = "()F";
            break;
        case TYPE_BYTE:
            method = "byteValue";
            descriptor = "()B";
            break;
        case TYPE_SHORT:
            method = "shortValue";
            descriptor = "()S";
            break;
        case TYPE_CHAR:
            /* Character uses charValue() */
            method = "charValue";
            descriptor = "()C";
            break;
        case TYPE_BOOLEAN:
            method = "booleanValue";
            descriptor = "()Z";
            break;
        default:
            fprintf(stderr, "codegen: cannot unbox to type %d\n", target_prim);
            return false;
    }
    
    uint16_t methodref = cp_add_methodref(cp, wrapper_class, method, descriptor);
    bc_emit(mg->code, OP_INVOKEVIRTUAL);
    bc_emit_u2(mg->code, methodref);
    
    if (stack_adjust > 0) {
        mg_push(mg, stack_adjust);
    }
    
    return true;
}

/**
 * Emit boxing if expression needs to be boxed to match target type.
 * Returns true if boxing was emitted, false otherwise.
 */
bool emit_boxing_if_needed(method_gen_t *mg, const_pool_t *cp, type_t *target, type_t *source)
{
    if (type_needs_boxing(target, source)) {
        return emit_boxing(mg, cp, source->kind);
    }
    return true;  /* No boxing needed, but not an error */
}

/**
 * Emit unboxing if expression needs to be unboxed to match target type.
 * Returns true if unboxing was emitted, false otherwise.
 */
bool emit_unboxing_if_needed(method_gen_t *mg, const_pool_t *cp, type_t *target, type_t *source)
{
    if (type_needs_unboxing(target, source) && source->data.class_type.name) {
        char *internal = class_to_internal_name(source->data.class_type.name);
        bool result = emit_unboxing(mg, cp, target->kind, internal);
        free(internal);
        return result;
    }
    return true;  /* No unboxing needed, but not an error */
}

/**
 * Get the type kind of an argument expression, considering array access.
 * Used for slot counting and overload resolution.
 */
static type_kind_t get_arg_type_kind(method_gen_t *mg, ast_node_t *arg)
{
    if (!arg) return TYPE_INT;
    
    /* Check semantic type first */
    if (arg->sem_type) {
        return arg->sem_type->kind;
    }
    
    /* Handle array access - look up element type from array */
    if (arg->type == AST_ARRAY_ACCESS) {
        slist_t *children = arg->data.node.children;
        if (children) {
            ast_node_t *array_expr = (ast_node_t *)children->data;
            
            /* Check array's semantic type */
            if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    return elem_type->kind;
                }
            }
            
            /* Try local array tracking */
            if (array_expr->type == AST_IDENTIFIER && mg) {
                const char *arr_name = array_expr->data.leaf.name;
                if (mg_local_is_array(mg, arr_name)) {
                    return mg_local_array_elem_kind(mg, arr_name);
                }
            }
        }
    }
    
    /* Handle identifiers - check local variable type */
    if (arg->type == AST_IDENTIFIER && mg) {
        const char *name = arg->data.leaf.name;
        return mg_get_local_type(mg, name);
    }
    
    return TYPE_INT;  /* Default to int */
}

/**
 * Calculate the JVM slot count for an argument list.
 * Long and double take 2 slots, everything else takes 1.
 * This is needed for the invokeinterface count byte.
 */
static int calculate_arg_slot_count(method_gen_t *mg, slist_t *args)
{
    int count = 0;
    for (slist_t *node = args; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        type_kind_t kind = get_arg_type_kind(mg, arg);
        if (kind == TYPE_LONG || kind == TYPE_DOUBLE) {
            count += 2;
        } else {
            count += 1;
        }
    }
    return count;
}

/**
 * Get the type kind of an expression for opcode selection.
 * Used to determine which variant of arithmetic opcodes to use.
 * For wrapper types (Integer, Long, etc.), returns the underlying primitive.
 */
type_kind_t get_expr_type_kind(method_gen_t *mg, ast_node_t *expr)
{
    if (!expr) {
        return TYPE_INT;
    }
    
    /* Check semantic type first */
    if (expr->sem_type) {
        /* Handle wrapper types - return underlying primitive for arithmetic */
        if (expr->sem_type->kind == TYPE_CLASS && expr->sem_type->data.class_type.name) {
            type_kind_t prim = get_primitive_for_wrapper(expr->sem_type->data.class_type.name);
            if (prim != TYPE_UNKNOWN) {
                return prim;
            }
        }
        return expr->sem_type->kind;
    }
    
    /* For identifiers, check local variable type or field type */
    if (expr->type == AST_IDENTIFIER) {
        const char *name = expr->data.leaf.name;
        type_kind_t kind = mg_get_local_type(mg, name);
        if (kind != TYPE_INT || !mg) {
            return kind;
        }
        /* Not found as local - check if it's a field */
        if (mg->class_gen) {
            field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
            if (field && field->descriptor) {
                switch (field->descriptor[0]) {
                    case 'J': return TYPE_LONG;
                    case 'F': return TYPE_FLOAT;
                    case 'D': return TYPE_DOUBLE;
                    case 'Z': return TYPE_BOOLEAN;
                    case 'B': return TYPE_BYTE;
                    case 'C': return TYPE_CHAR;
                    case 'S': return TYPE_SHORT;
                    case 'L': 
                    case '[': return TYPE_CLASS;
                    default: return TYPE_INT;
                }
            }
        }
        return kind;
    }
    
    /* For literals, check the literal type */
    if (expr->type == AST_LITERAL) {
        switch (expr->data.leaf.token_type) {
            case TOK_LONG_LITERAL:
                return TYPE_LONG;
            case TOK_FLOAT_LITERAL:
                return TYPE_FLOAT;
            case TOK_DOUBLE_LITERAL:
                return TYPE_DOUBLE;
            case TOK_TRUE:
            case TOK_FALSE:
                return TYPE_BOOLEAN;
            case TOK_NULL:
                return TYPE_NULL;
            case TOK_STRING_LITERAL:
                return TYPE_CLASS;  /* String is a reference type */
            case TOK_CHAR_LITERAL:
                return TYPE_CHAR;
            default:
                return TYPE_INT;
        }
    }
    
    /* For parenthesized expressions, look through to the inner expression */
    if (expr->type == AST_PARENTHESIZED) {
        slist_t *children = expr->data.node.children;
        if (children) {
            return get_expr_type_kind(mg, (ast_node_t *)children->data);
        }
    }
    
    /* For binary expressions, determine from operands (use wider type) */
    if (expr->type == AST_BINARY_EXPR) {
        slist_t *children = expr->data.node.children;
        if (children && children->next) {
            type_kind_t left_kind = get_expr_type_kind(mg, (ast_node_t *)children->data);
            type_kind_t right_kind = get_expr_type_kind(mg, (ast_node_t *)children->next->data);
            
            /* Type widening: double > float > long > int */
            if (left_kind == TYPE_DOUBLE || right_kind == TYPE_DOUBLE) {
                return TYPE_DOUBLE;
            }
            if (left_kind == TYPE_FLOAT || right_kind == TYPE_FLOAT) {
                return TYPE_FLOAT;
            }
            if (left_kind == TYPE_LONG || right_kind == TYPE_LONG) {
                return TYPE_LONG;
            }
            return TYPE_INT;
        }
    }
    
    /* For method calls, look up method return type */
    if (expr->type == AST_METHOD_CALL && mg) {
        const char *method_name = expr->data.node.name;
        slist_t *children = expr->data.node.children;
        
        /* Check if first child is a receiver */
        if (children) {
            ast_node_t *first = (ast_node_t *)children->data;
            symbol_t *receiver_class = NULL;
            
            /* Get receiver's class/interface symbol */
            if (first->type == AST_IDENTIFIER) {
                const char *recv_name = first->data.leaf.name;
                /* Check local variable type */
                const char *local_class = mg_local_class_name(mg, recv_name);
                if (local_class && mg->class_gen && mg->class_gen->sem) {
                    /* Convert internal name to qualified name */
                    char *qualified = strdup(local_class);
                    for (char *p = qualified; *p; p++) {
                        if (*p == '/') *p = '.';
                    }
                    type_t *class_type = hashtable_lookup(mg->class_gen->sem->types, qualified);
                    if (class_type && class_type->kind == TYPE_CLASS) {
                        receiver_class = class_type->data.class_type.symbol;
                    }
                    free(qualified);
                }
            } else if (first->sem_type && first->sem_type->kind == TYPE_CLASS) {
                receiver_class = first->sem_type->data.class_type.symbol;
            }
            
            /* Look up method in receiver's class/interface hierarchy */
            if (receiver_class) {
                symbol_t *method_sym = lookup_method_in_hierarchy(receiver_class, method_name, NULL);
                if (method_sym && method_sym->type) {
                    return method_sym->type->kind;
                }
            }
        }
        
        /* Check method in current class */
        if (mg->class_gen && mg->class_gen->class_sym) {
            symbol_t *class_sym = mg->class_gen->class_sym;
            if (class_sym->data.class_data.members) {
                symbol_t *method_sym = scope_lookup_local(
                    class_sym->data.class_data.members, method_name);
                if (method_sym && method_sym->type) {
                    return method_sym->type->kind;
                }
            }
        }
    }
    
    /* Default to int */
    return TYPE_INT;
}

/* ========================================================================
 * Literal Code Generation
 * ======================================================================== */

static bool codegen_literal(method_gen_t *mg, ast_node_t *lit, const_pool_t *cp)
{
    if (!lit || lit->type != AST_LITERAL) {
        return false;
    }
    
    token_type_t tok_type = lit->data.leaf.token_type;
    
    switch (tok_type) {
        case TOK_INTEGER_LITERAL:
            {
                long long val = lit->data.leaf.value.int_val;
                if (val >= -1 && val <= 5) {
                    bc_emit(mg->code, OP_ICONST_0 + (int)val);
                } else if (val >= -128 && val <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_s1(mg->code, (int8_t)val);
                } else if (val >= -32768 && val <= 32767) {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_s2(mg->code, (int16_t)val);
                } else {
                    uint16_t idx = cp_add_integer(cp, (int32_t)val);
                    if (idx <= 255) {
                        bc_emit(mg->code, OP_LDC);
                        bc_emit_u1(mg->code, (uint8_t)idx);
                    } else {
                        bc_emit(mg->code, OP_LDC_W);
                        bc_emit_u2(mg->code, idx);
                    }
                }
                mg_push_int(mg);
                return true;
            }
        
        case TOK_LONG_LITERAL:
            {
                long long val = lit->data.leaf.value.int_val;
                if (val == 0) {
                    bc_emit(mg->code, OP_LCONST_0);
                } else if (val == 1) {
                    bc_emit(mg->code, OP_LCONST_1);
                } else {
                    uint16_t idx = cp_add_long(cp, val);
                    bc_emit(mg->code, OP_LDC2_W);
                    bc_emit_u2(mg->code, idx);
                }
                mg_push_long(mg);
                return true;
            }
        
        case TOK_FLOAT_LITERAL:
            {
                double val = lit->data.leaf.value.float_val;
                if (val == 0.0f) {
                    bc_emit(mg->code, OP_FCONST_0);
                } else if (val == 1.0f) {
                    bc_emit(mg->code, OP_FCONST_1);
                } else if (val == 2.0f) {
                    bc_emit(mg->code, OP_FCONST_2);
                } else {
                    uint16_t idx = cp_add_float(cp, (float)val);
                    if (idx <= 255) {
                        bc_emit(mg->code, OP_LDC);
                        bc_emit_u1(mg->code, (uint8_t)idx);
                    } else {
                        bc_emit(mg->code, OP_LDC_W);
                        bc_emit_u2(mg->code, idx);
                    }
                }
                mg_push_float(mg);
                return true;
            }
        
        case TOK_DOUBLE_LITERAL:
            {
                double val = lit->data.leaf.value.float_val;
                if (val == 0.0) {
                    bc_emit(mg->code, OP_DCONST_0);
                } else if (val == 1.0) {
                    bc_emit(mg->code, OP_DCONST_1);
                } else {
                    uint16_t idx = cp_add_double(cp, val);
                    bc_emit(mg->code, OP_LDC2_W);
                    bc_emit_u2(mg->code, idx);
                }
                mg_push_double(mg);
                return true;
            }
        
        case TOK_CHAR_LITERAL:
            {
                /* Char is stored as str_val, get first char value */
                const char *str = lit->data.leaf.value.str_val;
                int val = str && str[0] ? (unsigned char)str[0] : 0;
                if (val >= 0 && val <= 5) {
                    bc_emit(mg->code, OP_ICONST_0 + val);
                } else if (val <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_s1(mg->code, (int8_t)val);
                } else {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_s2(mg->code, (int16_t)val);
                }
                mg_push_int(mg);
                return true;
            }
        
        case TOK_STRING_LITERAL:
        case TOK_TEXT_BLOCK:
            {
                const char *str = lit->data.leaf.value.str_val;
                if (!str) {
                    str = lit->data.leaf.name;
                }
                uint16_t idx = cp_add_string(cp, str ? str : "");
                if (idx <= 255) {
                    bc_emit(mg->code, OP_LDC);
                    bc_emit_u1(mg->code, (uint8_t)idx);
                } else {
                    bc_emit(mg->code, OP_LDC_W);
                    bc_emit_u2(mg->code, idx);
                }
                mg_push_object(mg, "java/lang/String");
                return true;
            }
        
        case TOK_TRUE:
            bc_emit(mg->code, OP_ICONST_1);
            mg_push_int(mg);
            return true;
        
        case TOK_FALSE:
            bc_emit(mg->code, OP_ICONST_0);
            mg_push_int(mg);
            return true;
        
        case TOK_NULL:
            bc_emit(mg->code, OP_ACONST_NULL);
            mg_push_null(mg);
            return true;
        
        default:
            return false;
    }
}

/* ========================================================================
 * Identifier Code Generation
 * ======================================================================== */

static bool codegen_identifier(method_gen_t *mg, ast_node_t *ident)
{
    if (!ident || ident->type != AST_IDENTIFIER) {
        return false;
    }
    
    const char *name = ident->data.leaf.name;
    
    /* Check if this identifier is a class reference (e.g., "java" in java.util.Objects, or "Objects" after import).
     * In that case, sem_symbol is set to a class/interface/enum symbol by semantic analysis, and we should NOT
     * generate any code - the parent AST_METHOD_CALL or AST_FIELD_ACCESS will handle the static access. */
    if (ident->sem_symbol && 
        (ident->sem_symbol->kind == SYM_CLASS ||
         ident->sem_symbol->kind == SYM_INTERFACE ||
         ident->sem_symbol->kind == SYM_ENUM)) {
        /* This is a class reference, not a variable - no code to generate */
        return true;
    }
    
    /* First, check if it's a local variable */
    local_var_info_t *info = (local_var_info_t *)hashtable_lookup(mg->locals, name);
    if (info) {
        /* Use helper that selects correct opcode based on type */
        mg_emit_load_local(mg, info->slot, info->kind);
        
        /* For lambda parameters, the JVM type is erased (Object), but the semantic type
         * is the actual type (e.g., String). We need to checkcast before using it. */
        if (info->is_ref && ident->sem_type && ident->sem_type->kind == TYPE_CLASS) {
            const char *semantic_name = ident->sem_type->data.class_type.name;
            /* Only cast if semantic type is more specific than Object */
            if (semantic_name && strcmp(semantic_name, "java.lang.Object") != 0) {
                uint16_t cast_idx = cp_add_class(mg->cp, class_to_internal_name(semantic_name));
                bc_emit(mg->code, OP_CHECKCAST);
                bc_emit_u2(mg->code, cast_idx);
                /* checkcast doesn't change stack depth, it just narrows the type.
                 * The stack tracking already has the object pushed, so we just
                 * need to update the type info if needed for verification. */
            }
        }
        return true;
    }
    
    /* Check if it's a captured variable in a local/anonymous class */
    if (mg->class_gen && (mg->class_gen->is_local_class || mg->class_gen->is_anonymous_class) && 
        mg->class_gen->captured_field_refs) {
        void *ref_ptr = hashtable_lookup(mg->class_gen->captured_field_refs, name);
        if (ref_ptr) {
            uint16_t field_ref = (uint16_t)(uintptr_t)ref_ptr;
            
            /* Look up the captured field to get its descriptor */
            char val_field_name[256];
            snprintf(val_field_name, sizeof(val_field_name), "val$%s", name);
            field_gen_t *captured_field = hashtable_lookup(mg->class_gen->field_map, val_field_name);
            
            /* Load 'this' */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push_object(mg, mg->class_gen->internal_name);
            
            /* Get the captured value from val$xxx field */
            bc_emit(mg->code, OP_GETFIELD);
            bc_emit_u2(mg->code, field_ref);
            /* getfield pops ref, pushes value with proper type tracking */
            mg_pop_typed(mg, 1);  /* Pop the object reference */
            if (captured_field && captured_field->descriptor) {
                switch (captured_field->descriptor[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L': 
                    case '[': mg_push_object_from_descriptor(mg, captured_field->descriptor); break;
                    default:  mg_push_int(mg); break;
                }
            } else {
                /* Fallback to Object type if descriptor unknown */
                mg_push_object(mg, "java/lang/Object");
            }
            
            return true;
        }
    }
    
    /* Not a local - check if it's a field */
    if (mg->class_gen) {
        field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
        if (field) {
            /* Check if it's a static field */
            if (field->access_flags & ACC_STATIC) {
                /* Static field - use getstatic */
                uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                     field->name, field->descriptor);
                bc_emit(mg->code, OP_GETSTATIC);
                bc_emit_u2(mg->code, fieldref);
                /* Push with proper type tracking based on descriptor */
                switch (field->descriptor[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L': 
                    case '[': mg_push_object_from_descriptor(mg, field->descriptor); break;
                    default:  mg_push_int(mg); break;
                }
                return true;
            } else if (!mg->is_static) {
                /* Instance field - use getfield */
                /* Load 'this' first */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push_object(mg, mg->class_gen->internal_name);
                
                /* Emit getfield */
                uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                     field->name, field->descriptor);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, fieldref);
                
                /* getfield pops object ref, pushes field value */
                mg_pop_typed(mg, 1);  /* Pop the object reference */
                switch (field->descriptor[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L': 
                    case '[': mg_push_object_from_descriptor(mg, field->descriptor); break;
                    default:  mg_push_int(mg); break;
                }
                return true;
            }
            /* Instance field access in static context - error handled below */
        }
        
        /* Check enclosing class for nested classes */
        symbol_t *class_sym = mg->class_gen->class_sym;
        if (class_sym && class_sym->data.class_data.enclosing_class) {
            symbol_t *enclosing = class_sym->data.class_data.enclosing_class;
            if (enclosing->data.class_data.members) {
                symbol_t *outer_field = scope_lookup_local(
                    enclosing->data.class_data.members, name);
                if (outer_field && outer_field->kind == SYM_FIELD) {
                    char *outer_internal = class_to_internal_name(enclosing->qualified_name);
                    char *field_desc = type_to_descriptor(outer_field->type);
                    
                    if (outer_field->modifiers & MOD_STATIC) {
                        /* Static field in enclosing class - use getstatic */
                        uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                             name, field_desc);
                        bc_emit(mg->code, OP_GETSTATIC);
                        bc_emit_u2(mg->code, fieldref);
                        /* Push with proper type tracking */
                        switch (field_desc[0]) {
                            case 'J': mg_push_long(mg); break;
                            case 'D': mg_push_double(mg); break;
                            case 'F': mg_push_float(mg); break;
                            case 'L': 
                            case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                            default:  mg_push_int(mg); break;
                        }
                    } else if (mg->class_gen->is_inner_class) {
                        /* Instance field in enclosing class - access via this$0 */
                        /* First load this$0 */
                        bc_emit(mg->code, OP_ALOAD_0);
                        mg_push_object(mg, mg->class_gen->internal_name);
                        bc_emit(mg->code, OP_GETFIELD);
                        bc_emit_u2(mg->code, mg->class_gen->this_dollar_zero_ref);
                        /* getfield pops 'this', pushes outer instance - update types */
                        mg_pop_typed(mg, 1);
                        mg_push_object(mg, outer_internal);
                        
                        /* Now get the field from outer instance */
                        uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                             name, field_desc);
                        bc_emit(mg->code, OP_GETFIELD);
                        bc_emit_u2(mg->code, fieldref);
                        /* getfield pops outer instance, pushes field value */
                        mg_pop_typed(mg, 1);
                        switch (field_desc[0]) {
                            case 'J': mg_push_long(mg); break;
                            case 'D': mg_push_double(mg); break;
                            case 'F': mg_push_float(mg); break;
                            case 'L': 
                            case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                            default:  mg_push_int(mg); break;
                        }
                    } else {
                        /* Instance field in enclosing class but this is a static nested class */
                        free(outer_internal);
                        free(field_desc);
                        fprintf(stderr, "codegen: cannot access instance field '%s' of enclosing class from static context\n", name);
                        return false;
                    }
                    
                    free(outer_internal);
                    free(field_desc);
                    return true;
                }
            }
        }
    }
    
    /* Check if this is a statically imported field */
    if (ident->sem_symbol && ident->sem_symbol->kind == SYM_FIELD &&
        (ident->sem_symbol->modifiers & MOD_STATIC)) {
        symbol_t *field_sym = ident->sem_symbol;
        /* Get the class from the symbol's scope owner */
        symbol_t *class_sym = field_sym->scope ? field_sym->scope->owner : NULL;
        
        if (class_sym && class_sym->qualified_name) {
            char *class_internal = class_to_internal_name(class_sym->qualified_name);
            char *field_desc = type_to_descriptor(field_sym->type);
            
            uint16_t fieldref = cp_add_fieldref(mg->cp, class_internal, name, field_desc);
            bc_emit(mg->code, OP_GETSTATIC);
            bc_emit_u2(mg->code, fieldref);
            
            /* Push with proper type tracking */
            if (field_sym->type) {
                switch (field_sym->type->kind) {
                    case TYPE_LONG:   mg_push_long(mg); break;
                    case TYPE_DOUBLE: mg_push_double(mg); break;
                    case TYPE_FLOAT:  mg_push_float(mg); break;
                    case TYPE_CLASS:
                    case TYPE_ARRAY:  mg_push_object_from_descriptor(mg, field_desc); break;
                    default:          mg_push_int(mg); break;
                }
            } else {
                mg_push_int(mg);
            }
            
            free(class_internal);
            free(field_desc);
            return true;
        }
    }
    
    fprintf(stderr, "codegen: cannot resolve identifier: %s\n", name);
    return false;
}

/* ========================================================================
 * Field Access Code Generation
 * ======================================================================== */

/**
 * Check if an identifier refers to a known class name (for static field access).
 * Returns the internal class name if found, NULL otherwise.
 */
static const char *resolve_class_name(method_gen_t *mg, const char *name)
{
    /* Check for well-known JDK classes */
    if (strcmp(name, "System") == 0) {
        return "java/lang/System";
    }
    if (strcmp(name, "Math") == 0) {
        return "java/lang/Math";
    }
    if (strcmp(name, "Integer") == 0) {
        return "java/lang/Integer";
    }
    if (strcmp(name, "Long") == 0) {
        return "java/lang/Long";
    }
    if (strcmp(name, "Double") == 0) {
        return "java/lang/Double";
    }
    if (strcmp(name, "Float") == 0) {
        return "java/lang/Float";
    }
    if (strcmp(name, "Boolean") == 0) {
        return "java/lang/Boolean";
    }
    if (strcmp(name, "String") == 0) {
        return "java/lang/String";
    }
    if (strcmp(name, "Object") == 0) {
        return "java/lang/Object";
    }
    if (strcmp(name, "Arrays") == 0) {
        return "java/util/Arrays";
    }
    if (strcmp(name, "Collections") == 0) {
        return "java/util/Collections";
    }
    
    /* Check if it's a class in the current compilation unit */
    if (mg->class_gen && mg->class_gen->class_sym) {
        /* Check if it's the current class */
        if (strcmp(name, mg->class_gen->class_sym->name) == 0) {
            return mg->class_gen->internal_name;
        }
        
        /* Check if it's a nested class/enum in the current class */
        symbol_t *class_sym = mg->class_gen->class_sym;
        if (class_sym->data.class_data.members) {
            symbol_t *nested = scope_lookup_local(class_sym->data.class_data.members, name);
            if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                          nested->kind == SYM_ENUM)) {
                /* Build internal name: OuterClass$NestedClass */
                static char nested_internal[256];
                snprintf(nested_internal, sizeof(nested_internal), "%s$%s",
                         mg->class_gen->internal_name, name);
                return nested_internal;
            }
        }
        /* TODO: Check imports */
    }
    
    return NULL;
}

/**
 * Get the field descriptor for a well-known static field.
 * Returns the descriptor string or NULL if not found.
 */
static const char *get_known_static_field_descriptor(const char *class_name, const char *field_name)
{
    /* java.lang.System */
    if (strcmp(class_name, "java/lang/System") == 0) {
        if (strcmp(field_name, "out") == 0) {
            return "Ljava/io/PrintStream;";
        }
        if (strcmp(field_name, "err") == 0) {
            return "Ljava/io/PrintStream;";
        }
        if (strcmp(field_name, "in") == 0) {
            return "Ljava/io/InputStream;";
        }
    }
    
    /* TODO: Add more well-known fields as needed */
    
    return NULL;
}

/**
 * Get the class type for a well-known static field.
 * Returns the internal class name of the field's type, or NULL.
 */
static const char *get_known_static_field_type_class(const char *class_name, const char *field_name)
{
    if (strcmp(class_name, "java/lang/System") == 0) {
        if (strcmp(field_name, "out") == 0 || strcmp(field_name, "err") == 0) {
            return "java/io/PrintStream";
        }
        if (strcmp(field_name, "in") == 0) {
            return "java/io/InputStream";
        }
    }
    
    return NULL;
}

/**
 * Generate code for field access (obj.field or Class.staticField).
 * Handles both instance and static field access, including chained access.
 */
static bool codegen_field_access(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_FIELD_ACCESS) {
        return false;
    }
    
    const char *field_name = expr->data.node.name;
    
    /* Check if this field access is actually a FQN class reference (e.g., java.util.Objects).
     * In that case, sem_symbol is set to a class symbol by semantic analysis, and we should NOT
     * generate any code - the parent AST_METHOD_CALL will handle the static method invocation.
     * EXCEPTION: if field_name is "class", this is a class literal and we MUST generate code. */
    if (expr->sem_symbol && 
        (expr->sem_symbol->kind == SYM_CLASS || 
         expr->sem_symbol->kind == SYM_INTERFACE ||
         expr->sem_symbol->kind == SYM_ENUM) &&
        (!field_name || strcmp(field_name, "class") != 0)) {
        /* This is a class reference, not a field access - no code to generate */
        return true;
    }
    if (!field_name) {
        fprintf(stderr, "codegen: field access without field name\n");
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    if (!children) {
        fprintf(stderr, "codegen: field access without receiver\n");
        return false;
    }
    
    ast_node_t *receiver = (ast_node_t *)children->data;
    
    /* Check for class literal (Type.class) */
    if (field_name && strcmp(field_name, "class") == 0) {
        /* For reference types: ldc <class constant>
         * For primitive types: getstatic <WrapperType>.TYPE */
        
        if (receiver->type == AST_PRIMITIVE_TYPE) {
            /* Primitive class literal: boolean.class -> Boolean.TYPE, etc. */
            const char *prim_name = receiver->data.leaf.name;
            const char *wrapper_class = NULL;
            
            if (strcmp(prim_name, "boolean") == 0) wrapper_class = "java/lang/Boolean";
            else if (strcmp(prim_name, "byte") == 0) wrapper_class = "java/lang/Byte";
            else if (strcmp(prim_name, "char") == 0) wrapper_class = "java/lang/Character";
            else if (strcmp(prim_name, "short") == 0) wrapper_class = "java/lang/Short";
            else if (strcmp(prim_name, "int") == 0) wrapper_class = "java/lang/Integer";
            else if (strcmp(prim_name, "long") == 0) wrapper_class = "java/lang/Long";
            else if (strcmp(prim_name, "float") == 0) wrapper_class = "java/lang/Float";
            else if (strcmp(prim_name, "double") == 0) wrapper_class = "java/lang/Double";
            else if (strcmp(prim_name, "void") == 0) wrapper_class = "java/lang/Void";
            
            if (wrapper_class) {
                uint16_t fieldref = cp_add_fieldref(cp, wrapper_class, "TYPE", "Ljava/lang/Class;");
                bc_emit(mg->code, OP_GETSTATIC);
                bc_emit_u2(mg->code, fieldref);
                mg_push_object(mg, "java/lang/Class");
                return true;
            }
        } else if (receiver->type == AST_CLASS_TYPE || receiver->type == AST_IDENTIFIER) {
            /* Reference type class literal: String.class -> ldc <String class> */
            const char *class_name = NULL;
            
            if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
                class_name = receiver->sem_type->data.class_type.name;
            }
            if (!class_name) {
                class_name = receiver->type == AST_IDENTIFIER ? 
                    receiver->data.leaf.name : receiver->data.node.name;
            }
            
            if (class_name) {
                char *internal = class_to_internal_name(class_name);
                uint16_t class_idx = cp_add_class(cp, internal);
                
                /* Use ldc_w for class constants (can be large) */
                bc_emit(mg->code, OP_LDC_W);
                bc_emit_u2(mg->code, class_idx);
                mg_push_object(mg, "java/lang/Class");
                free(internal);
                return true;
            }
        } else if (receiver->type == AST_ARRAY_TYPE) {
            /* Array type class literal: int[].class, String[].class */
            char *desc = ast_type_to_descriptor(receiver);
            uint16_t class_idx = cp_add_class(cp, desc);
            
            bc_emit(mg->code, OP_LDC_W);
            bc_emit_u2(mg->code, class_idx);
            mg_push_object(mg, "java/lang/Class");
            free(desc);
            return true;
        }
    }
    
    /* Check for array.length access */
    if (strcmp(field_name, "length") == 0) {
        /* Check if receiver is an array type */
        bool is_array = false;
        if (receiver->sem_type && receiver->sem_type->kind == TYPE_ARRAY) {
            is_array = true;
        } else if (receiver->type == AST_IDENTIFIER) {
            /* Check local variable for array type marker */
            const char *name = receiver->data.leaf.name;
            if (mg_local_is_array(mg, name)) {
                is_array = true;
            }
        } else if (receiver->type == AST_NEW_ARRAY) {
            is_array = true;
        } else if (receiver->type == AST_ARRAY_ACCESS) {
            /* Result of array access could be an array (multi-dimensional) 
             * Check by looking at the array expression and counting access depth */
            int depth = 0;
            ast_node_t *base = receiver;
            while (base && base->type == AST_ARRAY_ACCESS) {
                depth++;
                base = (ast_node_t *)base->data.node.children->data;
            }
            /* Now base is the actual array variable */
            if (base && base->type == AST_IDENTIFIER) {
                const char *arr_name = base->data.leaf.name;
                if (mg_local_is_array(mg, arr_name)) {
                    int dims = mg_local_array_dims(mg, arr_name);
                    /* If we've accessed fewer times than dimensions, result is still an array */
                    if (depth < dims) {
                        is_array = true;
                    }
                }
            }
        }
        
        if (is_array) {
            /* Generate array reference */
            if (!codegen_expr(mg, receiver, cp)) {
                return false;
            }
            
            /* Emit arraylength instruction */
            bc_emit(mg->code, OP_ARRAYLENGTH);
            /* Stack: arrayref -> int (length) - net effect 0 */
            return true;
        }
    }
    
    /* Check if this is a static field access (receiver is a class name) */
    if (receiver->type == AST_IDENTIFIER) {
        const char *recv_name = receiver->data.leaf.name;
        const char *class_name = resolve_class_name(mg, recv_name);
        
        /* Also check if semantic analysis resolved this to a class/enum symbol */
        if (!class_name && receiver->sem_symbol &&
            (receiver->sem_symbol->kind == SYM_CLASS ||
             receiver->sem_symbol->kind == SYM_INTERFACE ||
             receiver->sem_symbol->kind == SYM_ENUM)) {
            /* External class reference - use the qualified name */
            static char external_class_name[256];
            if (receiver->sem_symbol->qualified_name) {
                char *internal = class_to_internal_name(receiver->sem_symbol->qualified_name);
                strncpy(external_class_name, internal, sizeof(external_class_name) - 1);
                external_class_name[sizeof(external_class_name) - 1] = '\0';
                free(internal);
                class_name = external_class_name;
            }
        }
        
        if (class_name) {
            /* Static field access */
            const char *field_desc = get_known_static_field_descriptor(class_name, field_name);
            
            if (!field_desc) {
                /* Check if it's a field in the current class */
                if (mg->class_gen && strcmp(class_name, mg->class_gen->internal_name) == 0) {
                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
                    if (field) {
                        field_desc = field->descriptor;
                    }
                }
            }
            
            if (!field_desc && mg->class_gen && mg->class_gen->class_sym) {
                /* Check if it's a nested class/enum - look up the field from symbol table */
                symbol_t *class_sym = mg->class_gen->class_sym;
                if (class_sym->data.class_data.members) {
                    symbol_t *nested = scope_lookup_local(class_sym->data.class_data.members, recv_name);
                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                  nested->kind == SYM_ENUM)) {
                        /* Look up field in nested type */
                        if (nested->data.class_data.members) {
                            symbol_t *field_sym = scope_lookup_local(
                                nested->data.class_data.members, field_name);
                            if (field_sym && field_sym->kind == SYM_FIELD) {
                                static char field_desc_buf[256];
                                /* For enum constants, the type is the enum type itself */
                                if (field_sym->type && field_sym->type->kind == TYPE_CLASS) {
                                    char *internal = class_to_internal_name(field_sym->type->data.class_type.name);
                                    snprintf(field_desc_buf, sizeof(field_desc_buf), "L%s;", internal);
                                    free(internal);
                                    field_desc = field_desc_buf;
                                } else if (field_sym->type) {
                                    char *desc = type_to_descriptor(field_sym->type);
                                    strncpy(field_desc_buf, desc, sizeof(field_desc_buf) - 1);
                                    field_desc_buf[sizeof(field_desc_buf) - 1] = '\0';
                                    free(desc);
                                    field_desc = field_desc_buf;
                                }
                            }
                        }
                    }
                }
            }
            
            /* Check if receiver->sem_symbol points to an external class */
            if (!field_desc && receiver->sem_symbol &&
                (receiver->sem_symbol->kind == SYM_CLASS ||
                 receiver->sem_symbol->kind == SYM_INTERFACE ||
                 receiver->sem_symbol->kind == SYM_ENUM)) {
                symbol_t *ext_class = receiver->sem_symbol;
                if (ext_class->data.class_data.members) {
                    symbol_t *field_sym = scope_lookup_local(
                        ext_class->data.class_data.members, field_name);
                    if (field_sym && field_sym->kind == SYM_FIELD) {
                        static char ext_field_desc_buf[256];
                        if (field_sym->type && field_sym->type->kind == TYPE_CLASS) {
                            char *internal = class_to_internal_name(field_sym->type->data.class_type.name);
                            snprintf(ext_field_desc_buf, sizeof(ext_field_desc_buf), "L%s;", internal);
                            free(internal);
                            field_desc = ext_field_desc_buf;
                        } else if (field_sym->type) {
                            char *desc = type_to_descriptor(field_sym->type);
                            strncpy(ext_field_desc_buf, desc, sizeof(ext_field_desc_buf) - 1);
                            ext_field_desc_buf[sizeof(ext_field_desc_buf) - 1] = '\0';
                            free(desc);
                            field_desc = ext_field_desc_buf;
                        }
                    }
                }
            }
            
            if (!field_desc) {
                fprintf(stderr, "codegen: cannot resolve static field: %s.%s\n", 
                        recv_name, field_name);
                return false;
            }
            
            /* Emit getstatic */
            uint16_t fieldref = cp_add_fieldref(cp, class_name, field_name, field_desc);
            bc_emit(mg->code, OP_GETSTATIC);
            bc_emit_u2(mg->code, fieldref);
            /* Push with proper type tracking */
            switch (field_desc[0]) {
                case 'J': mg_push_long(mg); break;
                case 'D': mg_push_double(mg); break;
                case 'F': mg_push_float(mg); break;
                case 'L': 
                case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                default:  mg_push_int(mg); break;
            }
            
            return true;
        }
        
        /* Not a class name - check if it's a local variable */
        local_var_info_t *slot_info = (local_var_info_t *)hashtable_lookup(mg->locals, recv_name);
        if (slot_info) {
            /* Local variable - instance field access */
            
            /* Load the local variable (object reference) */
            mg_emit_load_local(mg, slot_info->slot, TYPE_CLASS);
            
            /* Now we need to get the field from this object */
            /* Try to determine the field type from semantic info */
            const char *obj_class = "java/lang/Object";
            const char *field_desc = "I";  /* Default to int for primitives */
            
            /* First check if we have the class name stored for this local */
            const char *local_class = mg_local_class_name(mg, recv_name);
            if (local_class) {
                obj_class = local_class;
            }
            
            /* Use semantic type info if available */
            if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
                if (receiver->sem_type->data.class_type.name) {
                    char *internal = class_to_internal_name(receiver->sem_type->data.class_type.name);
                    obj_class = internal;
                    /* Note: we leak this memory, but it's small */
                }
                
                /* Look up the field in the class to get its type */
                symbol_t *class_sym = receiver->sem_type->data.class_type.symbol;
                if (class_sym && class_sym->data.class_data.members) {
                    symbol_t *field_sym = scope_lookup_local(class_sym->data.class_data.members, field_name);
                    if (field_sym && field_sym->kind == SYM_FIELD && field_sym->type) {
                        field_desc = type_to_descriptor(field_sym->type);
                    }
                }
            }
            
            /* If we have the local class but no semantic info, look up via semantic analyzer */
            if (local_class && !receiver->sem_type && mg->class_gen && mg->class_gen->sem) {
                /* Try to find the class symbol by name */
                type_t *local_type = hashtable_lookup(mg->class_gen->sem->types, recv_name);
                if (!local_type) {
                    /* Try looking up by the class name */
                    char *dotted = strdup(local_class);
                    for (char *p = dotted; *p; p++) {
                        if (*p == '/') *p = '.';
                    }
                    local_type = hashtable_lookup(mg->class_gen->sem->types, dotted);
                    free(dotted);
                }
                if (local_type && local_type->kind == TYPE_CLASS && local_type->data.class_type.symbol) {
                    symbol_t *class_sym = local_type->data.class_type.symbol;
                    if (class_sym->data.class_data.members) {
                        symbol_t *field_sym = scope_lookup_local(class_sym->data.class_data.members, field_name);
                        if (field_sym && field_sym->kind == SYM_FIELD && field_sym->type) {
                            field_desc = type_to_descriptor(field_sym->type);
                        }
                    }
                }
            }
            
            uint16_t fieldref = cp_add_fieldref(cp, obj_class, field_name, field_desc);
            bc_emit(mg->code, OP_GETFIELD);
            bc_emit_u2(mg->code, fieldref);
            /* getfield pops ref, pushes value with proper type tracking */
            mg_pop_typed(mg, 1);  /* Pop the object reference */
            switch (field_desc[0]) {
                case 'J': mg_push_long(mg); break;
                case 'D': mg_push_double(mg); break;
                case 'F': mg_push_float(mg); break;
                case 'L': 
                case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                default:  mg_push_int(mg); break;
            }
            
            return true;
        }
    }
    
    /* Handle chained field access (e.g., a.b.c) or field access on expression result */
    /* The receiver could be another field access, method call, etc. */
    if (receiver->type == AST_FIELD_ACCESS) {
        /* Recursive field access - generate code for the receiver first */
        if (!codegen_field_access(mg, receiver, cp)) {
            return false;
        }
        
        /* Now the receiver object is on the stack */
        /* We need to determine the class and field type */
        const char *recv_class = NULL;
        const char *field_desc = NULL;
        
        /* Try to get type info from semantic analysis */
        if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
            if (receiver->sem_type->data.class_type.name) {
                recv_class = class_to_internal_name(receiver->sem_type->data.class_type.name);
            }
        }
        
        /* Check if the receiver was a known static field */
        if (!recv_class && receiver->data.node.children) {
            ast_node_t *recv_recv = (ast_node_t *)receiver->data.node.children->data;
            if (recv_recv->type == AST_IDENTIFIER) {
                const char *class_name = resolve_class_name(mg, recv_recv->data.leaf.name);
                if (class_name) {
                    recv_class = get_known_static_field_type_class(class_name, receiver->data.node.name);
                }
            }
        }
        
        if (!recv_class) {
            recv_class = "java/lang/Object";
        }
        
        /* Default field descriptor - assume Object reference */
        field_desc = "Ljava/lang/Object;";
        
        /* Emit getfield */
        uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
        bc_emit(mg->code, OP_GETFIELD);
        bc_emit_u2(mg->code, fieldref);
        /* getfield pops ref, pushes value with proper type tracking */
        mg_pop_typed(mg, 1);  /* Pop the object reference */
        mg_push_object_from_descriptor(mg, field_desc);  /* Push the field value (Object type) */
        
        return true;
    }
    
    /* Handle this.field */
    if (receiver->type == AST_THIS_EXPR) {
        /* Load 'this' */
        bc_emit(mg->code, OP_ALOAD_0);
        mg_push_object(mg, mg->class_gen ? mg->class_gen->internal_name : "java/lang/Object");
        
        /* Check if field exists in current class */
        if (mg->class_gen) {
            field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
            if (field) {
                uint16_t fieldref = cp_add_fieldref(cp, mg->class_gen->internal_name,
                                                     field->name, field->descriptor);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, fieldref);
                /* getfield pops ref, pushes value with proper type tracking */
                mg_pop_typed(mg, 1);  /* Pop the object reference */
                switch (field->descriptor[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L': 
                    case '[': mg_push_object_from_descriptor(mg, field->descriptor); break;
                    default:  mg_push_int(mg); break;
                }
                return true;
            }
        }
        
        fprintf(stderr, "codegen: cannot resolve field: this.%s\n", field_name);
        return false;
    }
    
    /* General case: receiver is some other expression */
    /* Generate code for the receiver, then access the field */
    if (!codegen_expr(mg, receiver, cp)) {
        return false;
    }
    
    /* Receiver is now on stack - emit getfield */
    const char *recv_class = "java/lang/Object";
    const char *field_desc = "Ljava/lang/Object;";
    
    if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
        if (receiver->sem_type->data.class_type.name) {
            recv_class = class_to_internal_name(receiver->sem_type->data.class_type.name);
        }
    }
    
    uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
    bc_emit(mg->code, OP_GETFIELD);
    bc_emit_u2(mg->code, fieldref);
    /* getfield pops ref, pushes value - net 0 */
    
    return true;
}

/* ========================================================================
 * String Concatenation
 * ======================================================================== */

/**
 * Check if an expression evaluates to a String type.
 */
bool is_string_type(ast_node_t *expr)
{
    if (!expr) {
        return false;
    }
    
    /* String literal */
    if (expr->type == AST_LITERAL && expr->data.leaf.token_type == TOK_STRING_LITERAL) {
        return true;
    }
    
    /* Check resolved type from semantic analysis */
    if (expr->sem_type) {
        if (expr->sem_type->kind == TYPE_CLASS) {
            const char *name = expr->sem_type->data.class_type.name;
            if (name && (strcmp(name, "String") == 0 || 
                        strcmp(name, "java.lang.String") == 0)) {
                return true;
            }
        }
    }
    
    /* Method call returning String - check method name heuristically for now */
    if (expr->type == AST_METHOD_CALL) {
        const char *method_name = expr->data.node.name;
        if (method_name && strcmp(method_name, "toString") == 0) {
            return true;
        }
    }
    
    /* String concatenation produces String */
    if (expr->type == AST_BINARY_EXPR && expr->data.node.name &&
        strcmp(expr->data.node.name, "+") == 0) {
        slist_t *children = expr->data.node.children;
        if (children && children->next) {
            if (is_string_type((ast_node_t *)children->data) ||
                is_string_type((ast_node_t *)children->next->data)) {
                return true;
            }
        }
    }
    
    return false;
}

/**
 * Get the appropriate StringBuilder.append() method descriptor for a type.
 */
static const char *get_append_descriptor(method_gen_t *mg, ast_node_t *expr)
{
    if (!expr) {
        return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
    }
    
    /* Check for literal types */
    if (expr->type == AST_LITERAL) {
        switch (expr->data.leaf.token_type) {
            case TOK_STRING_LITERAL:
                return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
            case TOK_INTEGER_LITERAL:
                return "(I)Ljava/lang/StringBuilder;";
            case TOK_LONG_LITERAL:
                return "(J)Ljava/lang/StringBuilder;";
            case TOK_FLOAT_LITERAL:
                return "(F)Ljava/lang/StringBuilder;";
            case TOK_DOUBLE_LITERAL:
                return "(D)Ljava/lang/StringBuilder;";
            case TOK_CHAR_LITERAL:
                return "(C)Ljava/lang/StringBuilder;";
            case TOK_TRUE:
            case TOK_FALSE:
                return "(Z)Ljava/lang/StringBuilder;";
            default:
                break;
        }
    }
    
    /* Check resolved type */
    if (expr->sem_type) {
        if (expr->sem_type->kind == TYPE_CLASS) {
            const char *name = expr->sem_type->data.class_type.name;
            if (name && (strcmp(name, "String") == 0 || 
                        strcmp(name, "java.lang.String") == 0)) {
                return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
            }
        } else if (expr->sem_type->kind == TYPE_INT) {
            return "(I)Ljava/lang/StringBuilder;";
        } else if (expr->sem_type->kind == TYPE_LONG) {
            return "(J)Ljava/lang/StringBuilder;";
        } else if (expr->sem_type->kind == TYPE_FLOAT) {
            return "(F)Ljava/lang/StringBuilder;";
        } else if (expr->sem_type->kind == TYPE_DOUBLE) {
            return "(D)Ljava/lang/StringBuilder;";
        } else if (expr->sem_type->kind == TYPE_BOOLEAN) {
            return "(Z)Ljava/lang/StringBuilder;";
        } else if (expr->sem_type->kind == TYPE_CHAR) {
            return "(C)Ljava/lang/StringBuilder;";
        }
    }
    
    /* Check if it's a string type */
    if (is_string_type(expr)) {
        return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
    }
    
    /* Check identifier type from local variable tracking */
    if (expr->type == AST_IDENTIFIER) {
        const char *name = expr->data.leaf.name;
        
        /* Use mg_get_local_type to check local variable type */
        type_kind_t local_type = mg_get_local_type(mg, name);
        switch (local_type) {
            case TYPE_LONG: return "(J)Ljava/lang/StringBuilder;";
            case TYPE_FLOAT: return "(F)Ljava/lang/StringBuilder;";
            case TYPE_DOUBLE: return "(D)Ljava/lang/StringBuilder;";
            case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
            case TYPE_CHAR: return "(C)Ljava/lang/StringBuilder;";
            case TYPE_CLASS:
            case TYPE_ARRAY:
                return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
            case TYPE_INT:
            case TYPE_SHORT:
            case TYPE_BYTE:
                return "(I)Ljava/lang/StringBuilder;";
            default:
                break;
        }
        
        /* Check if it's a reference type */
        if (mg && name && mg_local_is_ref(mg, name)) {
            return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
        }
        
        /* Check semantic type info */
        if (expr->sem_type) {
            switch (expr->sem_type->kind) {
                case TYPE_INT: return "(I)Ljava/lang/StringBuilder;";
                case TYPE_LONG: return "(J)Ljava/lang/StringBuilder;";
                case TYPE_FLOAT: return "(F)Ljava/lang/StringBuilder;";
                case TYPE_DOUBLE: return "(D)Ljava/lang/StringBuilder;";
                case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
                case TYPE_CHAR: return "(C)Ljava/lang/StringBuilder;";
                case TYPE_CLASS:
                case TYPE_ARRAY:
                    return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
                default:
                    break;
            }
        }
        /* Default to int for unknown identifiers (common case) */
        return "(I)Ljava/lang/StringBuilder;";
    }
    
    /* For binary expressions, determine from operand types */
    if (expr->type == AST_BINARY_EXPR) {
        type_kind_t expr_type = get_expr_type_kind(mg, expr);
        switch (expr_type) {
            case TYPE_LONG: return "(J)Ljava/lang/StringBuilder;";
            case TYPE_FLOAT: return "(F)Ljava/lang/StringBuilder;";
            case TYPE_DOUBLE: return "(D)Ljava/lang/StringBuilder;";
            default: return "(I)Ljava/lang/StringBuilder;";
        }
    }
    
    /* For field access, look up field type */
    if (expr->type == AST_FIELD_ACCESS) {
        /* First check semantic type if available */
        if (expr->sem_type) {
            switch (expr->sem_type->kind) {
                case TYPE_INT:
                case TYPE_BYTE:
                case TYPE_SHORT:
                    return "(I)Ljava/lang/StringBuilder;";
                case TYPE_LONG:
                    return "(J)Ljava/lang/StringBuilder;";
                case TYPE_FLOAT:
                    return "(F)Ljava/lang/StringBuilder;";
                case TYPE_DOUBLE:
                    return "(D)Ljava/lang/StringBuilder;";
                case TYPE_BOOLEAN:
                    return "(Z)Ljava/lang/StringBuilder;";
                case TYPE_CHAR:
                    return "(C)Ljava/lang/StringBuilder;";
                case TYPE_CLASS:
                    if (expr->sem_type->data.class_type.name) {
                        const char *name = expr->sem_type->data.class_type.name;
                        if (strcmp(name, "String") == 0 ||
                            strcmp(name, "java.lang.String") == 0 ||
                            strcmp(name, "java/lang/String") == 0) {
                            return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                        }
                    }
                    return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
                default:
                    break;
            }
        }
        
        /* Try to look up field type from receiver class */
        const char *field_name = expr->data.node.name;
        slist_t *children = expr->data.node.children;
        if (children && field_name) {
            ast_node_t *receiver = (ast_node_t *)children->data;
            /* Get receiver's type to find the field */
            if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
                symbol_t *class_sym = receiver->sem_type->data.class_type.symbol;
                if (class_sym && class_sym->data.class_data.members) {
                    symbol_t *field_sym = scope_lookup_local(
                        class_sym->data.class_data.members, field_name);
                    if (field_sym && field_sym->type) {
                        switch (field_sym->type->kind) {
                            case TYPE_INT:
                            case TYPE_BYTE:
                            case TYPE_SHORT:
                                return "(I)Ljava/lang/StringBuilder;";
                            case TYPE_LONG:
                                return "(J)Ljava/lang/StringBuilder;";
                            case TYPE_FLOAT:
                                return "(F)Ljava/lang/StringBuilder;";
                            case TYPE_DOUBLE:
                                return "(D)Ljava/lang/StringBuilder;";
                            case TYPE_BOOLEAN:
                                return "(Z)Ljava/lang/StringBuilder;";
                            case TYPE_CHAR:
                                return "(C)Ljava/lang/StringBuilder;";
                            default:
                                break;
                        }
                    }
                }
            }
        }
        /* Default for field access without type info - try int first as common case */
        return "(I)Ljava/lang/StringBuilder;";
    }
    
    /* For method calls, check return type */
    if (expr->type == AST_METHOD_CALL) {
        type_kind_t ret_type = get_expr_type_kind(mg, expr);
        switch (ret_type) {
            case TYPE_LONG: return "(J)Ljava/lang/StringBuilder;";
            case TYPE_FLOAT: return "(F)Ljava/lang/StringBuilder;";
            case TYPE_DOUBLE: return "(D)Ljava/lang/StringBuilder;";
            case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
            case TYPE_CHAR: return "(C)Ljava/lang/StringBuilder;";
            case TYPE_CLASS:
            case TYPE_ARRAY:
                return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
            default:
                return "(I)Ljava/lang/StringBuilder;";
        }
    }
    
    /* For array access, determine element type */
    if (expr->type == AST_ARRAY_ACCESS) {
        type_kind_t elem_type = get_arg_type_kind(mg, expr);
        switch (elem_type) {
            case TYPE_LONG: return "(J)Ljava/lang/StringBuilder;";
            case TYPE_FLOAT: return "(F)Ljava/lang/StringBuilder;";
            case TYPE_DOUBLE: return "(D)Ljava/lang/StringBuilder;";
            case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
            case TYPE_CHAR: return "(C)Ljava/lang/StringBuilder;";
            case TYPE_CLASS:
            case TYPE_ARRAY:
                return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
            default:
                return "(I)Ljava/lang/StringBuilder;";
        }
    }
    
    return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
}

/**
 * Generate string concatenation using StringBuilder.
 * Collects all parts of a concat chain and generates:
 *   new StringBuilder().append(a).append(b)...toString()
 */
static bool codegen_string_concat(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    /* Collect all parts of the concatenation */
    slist_t *parts = NULL;
    slist_t *parts_tail = NULL;
    
    /* Flatten the concatenation tree */
    slist_t *stack = slist_new(expr);
    while (stack) {
        ast_node_t *node = (ast_node_t *)stack->data;
        slist_t *old = stack;
        stack = stack->next;
        free(old);
        
        if (node->type == AST_BINARY_EXPR && 
            node->data.node.name && strcmp(node->data.node.name, "+") == 0) {
            slist_t *children = node->data.node.children;
            if (children && children->next) {
                ast_node_t *left = (ast_node_t *)children->data;
                ast_node_t *right = (ast_node_t *)children->next->data;
                
                /* Check if either is a string */
                if (is_string_type(left) || is_string_type(right)) {
                    /* Push right first (so left is processed first) */
                    stack = slist_prepend(stack, right);
                    stack = slist_prepend(stack, left);
                    continue;
                }
            }
        }
        
        /* Not a string concat - add to parts list */
        if (!parts) {
            parts = slist_new(node);
            parts_tail = parts;
        } else {
            parts_tail = slist_append(parts_tail, node);
        }
    }
    
    /* Generate: new StringBuilder() */
    uint16_t sb_class = cp_add_class(cp, "java/lang/StringBuilder");
    bc_emit(mg->code, OP_NEW);
    bc_emit_u2(mg->code, sb_class);
    mg_push_null(mg);  /* Uninitialized object reference */
    
    bc_emit(mg->code, OP_DUP);
    mg_push_null(mg);  /* Duplicated object reference */
    
    uint16_t init_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "<init>", "()V");
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    mg_pop_typed(mg, 1);
    
    /* Generate append calls for each part */
    for (slist_t *node = parts; node; node = node->next) {
        ast_node_t *part = (ast_node_t *)node->data;
        
        /* Generate the part expression */
        if (!codegen_expr(mg, part, cp)) {
            slist_free(parts);
            return false;
        }
        
        /* Call appropriate append method */
        const char *append_desc = get_append_descriptor(mg, part);
        uint16_t append_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "append", append_desc);
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, append_ref);
        
        /* Stack: [StringBuilder, arg] -> [StringBuilder]
         * append consumes this+arg and returns StringBuilder
         * Long (J) and Double (D) args take 2 slots, others take 1 */
        type_kind_t part_type = get_expr_type_kind(mg, part);
        if (part_type == TYPE_LONG || part_type == TYPE_DOUBLE) {
            mg_pop_typed(mg, 2);  /* 2-slot arg */
        } else {
            mg_pop_typed(mg, 1);  /* 1-slot arg */
        }
    }
    
    /* Generate: toString() */
    uint16_t toString_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
    bc_emit(mg->code, OP_INVOKEVIRTUAL);
    bc_emit_u2(mg->code, toString_ref);
    /* Stack: StringBuilder -> String (same size, different type)
     * Pop StringBuilder, push String for correct type tracking */
    mg_pop_typed(mg, 1);
    mg_push_object(mg, "java/lang/String");
    
    slist_free(parts);
    return true;
}

/* ========================================================================
 * Binary Expression Code Generation
 * ======================================================================== */

static bool codegen_binary_expr(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_BINARY_EXPR) {
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    if (!children || !children->next) {
        return false;
    }
    
    ast_node_t *left = (ast_node_t *)children->data;
    ast_node_t *right = (ast_node_t *)children->next->data;
    token_type_t op = expr->data.node.op_token;
    
    /* Handle short-circuit logical operators (&&, ||) - must be done before
     * evaluating both operands to implement proper short-circuit behavior */
    if (op == TOK_AND || op == TOK_OR) {
        /* Logical AND (&&): if left is false, skip right and push 0
         * Logical OR (||): if left is true, skip right and push 1
         *
         * Pattern for && (short-circuit):
         *   eval left
         *   ifeq push_false     ; if left == 0, skip to push_false
         *   eval right
         *   ifeq push_false     ; if right == 0, skip to push_false
         *   iconst_1            ; both true, push 1
         *   goto end
         * push_false:
         *   iconst_0            ; push 0
         * end:
         *
         * Pattern for || (short-circuit):
         *   eval left
         *   ifne push_true      ; if left != 0, skip to push_true
         *   eval right
         *   ifne push_true      ; if right != 0, skip to push_true
         *   iconst_0            ; both false, push 0
         *   goto end
         * push_true:
         *   iconst_1            ; push 1
         * end:
         */
        
        /* Evaluate left operand */
        if (!codegen_expr(mg, left, cp)) {
            return false;
        }
        
        /* Emit conditional branch */
        size_t branch1_pos = mg->code->length;
        if (op == TOK_AND) {
            bc_emit(mg->code, OP_IFEQ);  /* branch if left == 0 */
        } else {
            bc_emit(mg->code, OP_IFNE);  /* branch if left != 0 */
        }
        bc_emit_u2(mg->code, 0);  /* placeholder offset */
        mg_pop_typed(mg, 1);  /* left operand consumed by branch */
        
        /* Evaluate right operand */
        if (!codegen_expr(mg, right, cp)) {
            return false;
        }
        
        /* Emit second conditional branch */
        size_t branch2_pos = mg->code->length;
        if (op == TOK_AND) {
            bc_emit(mg->code, OP_IFEQ);  /* branch if right == 0 */
        } else {
            bc_emit(mg->code, OP_IFNE);  /* branch if right != 0 */
        }
        bc_emit_u2(mg->code, 0);  /* placeholder offset */
        mg_pop_typed(mg, 1);  /* right operand consumed by branch */
        
        /* Emit "both conditions met" result */
        if (op == TOK_AND) {
            bc_emit(mg->code, OP_ICONST_1);  /* && true: both non-zero */
        } else {
            bc_emit(mg->code, OP_ICONST_0);  /* || false: both zero */
        }
        mg_push_int(mg);
        
        /* Emit goto to skip the short-circuit result */
        size_t goto_pos = mg->code->length;
        bc_emit(mg->code, OP_GOTO);
        bc_emit_u2(mg->code, 0);  /* placeholder offset */
        
        /* Short-circuit target: branches arrive here with empty stack */
        size_t short_circuit_pos = mg->code->length;
        
        /* Pop the result from the non-short-circuit path for correct frame recording */
        mg_pop_typed(mg, 1);
        
        /* Record frame at short-circuit target (target of branch1 and branch2) */
        mg_record_frame(mg);
        
        if (op == TOK_AND) {
            bc_emit(mg->code, OP_ICONST_0);  /* && short-circuit: left was false */
        } else {
            bc_emit(mg->code, OP_ICONST_1);  /* || short-circuit: left was true */
        }
        mg_push_int(mg);
        
        /* end label position - record frame (target of goto) */
        size_t end_pos = mg->code->length;
        mg_record_frame(mg);
        
        /* Patch branch offsets */
        int16_t branch1_offset = (int16_t)(short_circuit_pos - branch1_pos);
        bc_patch_u2(mg->code, branch1_pos + 1, (uint16_t)branch1_offset);
        
        int16_t branch2_offset = (int16_t)(short_circuit_pos - branch2_pos);
        bc_patch_u2(mg->code, branch2_pos + 1, (uint16_t)branch2_offset);
        
        int16_t goto_offset = (int16_t)(end_pos - goto_pos);
        bc_patch_u2(mg->code, goto_pos + 1, (uint16_t)goto_offset);
        
        return true;
    }
    
    /* Check for string concatenation */
    if (op == TOK_PLUS) {
        if (is_string_type(left) || is_string_type(right)) {
            return codegen_string_concat(mg, expr, cp);
        }
    }
    
    /* Determine operand types and result type for widening */
    type_kind_t left_type = get_expr_type_kind(mg, left);
    type_kind_t right_type = get_expr_type_kind(mg, right);
    type_kind_t op_type = left_type;
    
    /* Use the wider type (type promotion): double > float > long > int */
    if (right_type == TYPE_DOUBLE || op_type == TYPE_DOUBLE) {
        op_type = TYPE_DOUBLE;
    } else if (right_type == TYPE_FLOAT || op_type == TYPE_FLOAT) {
        op_type = TYPE_FLOAT;
    } else if (right_type == TYPE_LONG || op_type == TYPE_LONG) {
        op_type = TYPE_LONG;
    }
    
    /* Generate left operand */
    if (!codegen_expr(mg, left, cp)) {
        return false;
    }
    
    /* Auto-unbox left operand if it's a wrapper type */
    if (left->sem_type && left->sem_type->kind == TYPE_CLASS && 
        left->sem_type->data.class_type.name) {
        type_kind_t prim = get_primitive_for_wrapper(left->sem_type->data.class_type.name);
        if (prim != TYPE_UNKNOWN) {
            char *internal = class_to_internal_name(left->sem_type->data.class_type.name);
            emit_unboxing(mg, cp, prim, internal);
            free(internal);
        }
    }
    
    /* Emit widening conversion for left operand if needed */
    if (left_type != op_type) {
        switch (left_type) {
            case TYPE_INT:
            case TYPE_CHAR:
            case TYPE_SHORT:
            case TYPE_BYTE:
                /* int/char/short/byte -> wider type */
                switch (op_type) {
                    case TYPE_LONG:
                        bc_emit(mg->code, OP_I2L);
                        mg_pop_typed(mg, 1);
                        mg_push_long(mg);
                        break;
                    case TYPE_FLOAT:
                        bc_emit(mg->code, OP_I2F);
                        mg_pop_typed(mg, 1);
                        mg_push_float(mg);
                        break;
                    case TYPE_DOUBLE:
                        bc_emit(mg->code, OP_I2D);
                        mg_pop_typed(mg, 1);
                        mg_push_double(mg);
                        break;
                    default: break;
                }
                break;
            case TYPE_LONG:
                /* long -> wider type */
                switch (op_type) {
                    case TYPE_FLOAT:
                        bc_emit(mg->code, OP_L2F);
                        mg_pop_typed(mg, 2);
                        mg_push_float(mg);
                        break;
                    case TYPE_DOUBLE:
                        bc_emit(mg->code, OP_L2D);
                        mg_pop_typed(mg, 2);
                        mg_push_double(mg);
                        break;
                    default: break;
                }
                break;
            case TYPE_FLOAT:
                /* float -> double */
                if (op_type == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_F2D);
                    mg_pop_typed(mg, 1);
                    mg_push_double(mg);
                }
                break;
            default:
                break;
        }
    }
    
    /* Generate right operand */
    if (!codegen_expr(mg, right, cp)) {
        return false;
    }
    
    /* Auto-unbox right operand if it's a wrapper type */
    if (right->sem_type && right->sem_type->kind == TYPE_CLASS &&
        right->sem_type->data.class_type.name) {
        type_kind_t prim = get_primitive_for_wrapper(right->sem_type->data.class_type.name);
        if (prim != TYPE_UNKNOWN) {
            char *internal = class_to_internal_name(right->sem_type->data.class_type.name);
            emit_unboxing(mg, cp, prim, internal);
            free(internal);
        }
    }
    
    /* Emit widening conversion for right operand if needed */
    if (right_type != op_type) {
        switch (right_type) {
            case TYPE_INT:
            case TYPE_CHAR:
            case TYPE_SHORT:
            case TYPE_BYTE:
                /* int/char/short/byte -> wider type */
                switch (op_type) {
                    case TYPE_LONG:
                        bc_emit(mg->code, OP_I2L);
                        mg_pop_typed(mg, 1);
                        mg_push_long(mg);
                        break;
                    case TYPE_FLOAT:
                        bc_emit(mg->code, OP_I2F);
                        mg_pop_typed(mg, 1);
                        mg_push_float(mg);
                        break;
                    case TYPE_DOUBLE:
                        bc_emit(mg->code, OP_I2D);
                        mg_pop_typed(mg, 1);
                        mg_push_double(mg);
                        break;
                    default: break;
                }
                break;
            case TYPE_LONG:
                /* long -> wider type */
                switch (op_type) {
                    case TYPE_FLOAT:
                        bc_emit(mg->code, OP_L2F);
                        mg_pop_typed(mg, 2);
                        mg_push_float(mg);
                        break;
                    case TYPE_DOUBLE:
                        bc_emit(mg->code, OP_L2D);
                        mg_pop_typed(mg, 2);
                        mg_push_double(mg);
                        break;
                    default: break;
                }
                break;
            case TYPE_FLOAT:
                /* float -> double */
                if (op_type == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_F2D);
                    mg_pop_typed(mg, 1);
                    mg_push_double(mg);
                }
                break;
            default:
                break;
        }
    }
    
    /* Generate operation using switch on operator token */
    switch (op) {
        /* Arithmetic operators - type-aware */
        case TOK_PLUS:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LADD); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FADD); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DADD); break;
                default:          bc_emit(mg->code, OP_IADD); break;
            }
            break;
        case TOK_MINUS:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LSUB); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FSUB); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DSUB); break;
                default:          bc_emit(mg->code, OP_ISUB); break;
            }
            break;
        case TOK_STAR:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LMUL); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FMUL); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DMUL); break;
                default:          bc_emit(mg->code, OP_IMUL); break;
            }
            break;
        case TOK_SLASH:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LDIV); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FDIV); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DDIV); break;
                default:          bc_emit(mg->code, OP_IDIV); break;
            }
            break;
        case TOK_MOD:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LREM); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FREM); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DREM); break;
                default:          bc_emit(mg->code, OP_IREM); break;
            }
            break;
        
        /* Bitwise operators (int and long only) */
        case TOK_BITAND:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LAND : OP_IAND);
            break;
        case TOK_BITOR:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LOR : OP_IOR);
            break;
        case TOK_CARET:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LXOR : OP_IXOR);
            break;
        case TOK_LSHIFT:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LSHL : OP_ISHL);
            break;
        case TOK_RSHIFT:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LSHR : OP_ISHR);
            break;
        case TOK_URSHIFT:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LUSHR : OP_IUSHR);
            break;
        
        /* Comparison operators - produce boolean (0 or 1) result */
        case TOK_EQ:
        case TOK_NE:
        case TOK_LT:
        case TOK_GT:
        case TOK_LE:
        case TOK_GE:
            {
                /* Check if this is a reference comparison (including null) */
                bool is_ref_compare = false;
                bool left_is_null = false;
                bool right_is_null = false;
                
                /* Check for null literals */
                if (left->type == AST_LITERAL && 
                    left->data.leaf.token_type == TOK_NULL) {
                    left_is_null = true;
                    is_ref_compare = true;
                }
                if (right->type == AST_LITERAL && 
                    right->data.leaf.token_type == TOK_NULL) {
                    right_is_null = true;
                    is_ref_compare = true;
                }
                
                /* Check for reference types (only for == and !=) */
                if ((op == TOK_EQ || op == TOK_NE) && !is_ref_compare) {
                    /* Check left operand type */
                    if (left->type == AST_IDENTIFIER) {
                        const char *name = left->data.leaf.name;
                        if (mg_local_is_ref(mg, name)) {
                            is_ref_compare = true;
                        }
                    } else if (left->type == AST_NEW_OBJECT || left->type == AST_NEW_ARRAY ||
                               left->type == AST_THIS_EXPR || is_string_type(left)) {
                        is_ref_compare = true;
                    } else if (left->type == AST_METHOD_CALL || left->type == AST_FIELD_ACCESS ||
                               left->type == AST_ARRAY_ACCESS) {
                        /* These could be reference types - check sem_type */
                        if (left->sem_type && (left->sem_type->kind == TYPE_CLASS ||
                            left->sem_type->kind == TYPE_ARRAY ||
                            left->sem_type->kind == TYPE_NULL)) {
                            is_ref_compare = true;
                        }
                    }
                    
                    /* Check right operand type */
                    if (right->type == AST_IDENTIFIER) {
                        const char *name = right->data.leaf.name;
                        if (mg_local_is_ref(mg, name)) {
                            is_ref_compare = true;
                        }
                    } else if (right->type == AST_NEW_OBJECT || right->type == AST_NEW_ARRAY ||
                               right->type == AST_THIS_EXPR || is_string_type(right)) {
                        is_ref_compare = true;
                    } else if (right->type == AST_METHOD_CALL || right->type == AST_FIELD_ACCESS ||
                               right->type == AST_ARRAY_ACCESS) {
                        if (right->sem_type && (right->sem_type->kind == TYPE_CLASS ||
                            right->sem_type->kind == TYPE_ARRAY ||
                            right->sem_type->kind == TYPE_NULL)) {
                            is_ref_compare = true;
                        }
                    }
                }
                
                /* Pattern: if_<cmp><negated> push_zero; iconst_1; goto end; push_zero: iconst_0; end: */
                
                /* Determine the negated branch condition */
                /* We branch to push_zero if condition is FALSE */
                uint8_t branch_op;
                
                if (is_ref_compare && (op == TOK_EQ || op == TOK_NE)) {
                    /* Reference comparison */
                    if (right_is_null && !left_is_null) {
                        /* x == null or x != null: use ifnull/ifnonnull (single operand) */
                        /* Pop the null from stack first - we only need left operand */
                        mg_pop_typed(mg, 1);  /* Adjust for the null we're not using */
                        
                        /* Rewind: we need to regenerate without the right operand */
                        /* Actually, both operands are already on stack. Use if_acmp instead */
                        /* For simplicity, just use if_acmpeq/if_acmpne */
                        branch_op = (op == TOK_EQ) ? OP_IF_ACMPNE : OP_IF_ACMPEQ;
                        mg_push_null(mg);  /* Undo the pop - operands are both on stack (null for references) */
                    } else if (left_is_null && !right_is_null) {
                        /* null == x: same as x == null */
                        branch_op = (op == TOK_EQ) ? OP_IF_ACMPNE : OP_IF_ACMPEQ;
                    } else {
                        /* General reference comparison */
                        branch_op = (op == TOK_EQ) ? OP_IF_ACMPNE : OP_IF_ACMPEQ;
                    }
                } else if (op_type == TYPE_LONG) {
                    /* Long comparison: emit lcmp first
                     * lcmp pops 4 slots (2 longs), pushes 1 int
                     * Then ifXX pops 1, we push 1 result = net -3 from 4 input slots
                     * The mg_pop(1) at the end provides -1, so we add -2 more here */
                    bc_emit(mg->code, OP_LCMP);
                    mg_pop_typed(mg, 2);  /* Extra adjustment for long (beyond the -1 at end) */
                    
                    /* Then use single-operand branch on result (-1/0/1) */
                    switch (op) {
                        case TOK_EQ: branch_op = OP_IFNE; break;  /* branch if result != 0 */
                        case TOK_NE: branch_op = OP_IFEQ; break;  /* branch if result == 0 */
                        case TOK_LT: branch_op = OP_IFGE; break;  /* branch if result >= 0 */
                        case TOK_GE: branch_op = OP_IFLT; break;  /* branch if result < 0 */
                        case TOK_GT: branch_op = OP_IFLE; break;  /* branch if result <= 0 */
                        case TOK_LE: branch_op = OP_IFGT; break;  /* branch if result > 0 */
                        default: branch_op = OP_IFNE; break;
                    }
                } else if (op_type == TYPE_FLOAT) {
                    /* Float comparison: use fcmpg for </<=, fcmpl for >/>=
                     * fcmpg: NaN produces 1 (makes < and <= false)
                     * fcmpl: NaN produces -1 (makes > and >= false)
                     * fcmp pops 2 floats, pushes 1 int; net -1 handled by mg_pop at end */
                    if (op == TOK_GT || op == TOK_GE) {
                        bc_emit(mg->code, OP_FCMPL);
                    } else {
                        bc_emit(mg->code, OP_FCMPG);
                    }
                    /* No extra stack adjustment - same as int (2 slots in, 1 out = -1) */
                    
                    /* Then use single-operand branch on result (-1/0/1) */
                    switch (op) {
                        case TOK_EQ: branch_op = OP_IFNE; break;
                        case TOK_NE: branch_op = OP_IFEQ; break;
                        case TOK_LT: branch_op = OP_IFGE; break;
                        case TOK_GE: branch_op = OP_IFLT; break;
                        case TOK_GT: branch_op = OP_IFLE; break;
                        case TOK_LE: branch_op = OP_IFGT; break;
                        default: branch_op = OP_IFNE; break;
                    }
                } else if (op_type == TYPE_DOUBLE) {
                    /* Double comparison: use dcmpg for </<=, dcmpl for >/>=
                     * dcmpg: NaN produces 1 (makes < and <= false)
                     * dcmpl: NaN produces -1 (makes > and >= false)
                     * dcmp pops 4 slots (2 doubles), pushes 1 int = net -3
                     * The mg_pop(1) at the end provides -1, so we add -2 more here */
                    if (op == TOK_GT || op == TOK_GE) {
                        bc_emit(mg->code, OP_DCMPL);
                    } else {
                        bc_emit(mg->code, OP_DCMPG);
                    }
                    mg_pop_typed(mg, 2);  /* Extra adjustment for double (beyond the -1 at end) */
                    
                    /* Then use single-operand branch on result (-1/0/1) */
                    switch (op) {
                        case TOK_EQ: branch_op = OP_IFNE; break;
                        case TOK_NE: branch_op = OP_IFEQ; break;
                        case TOK_LT: branch_op = OP_IFGE; break;
                        case TOK_GE: branch_op = OP_IFLT; break;
                        case TOK_GT: branch_op = OP_IFLE; break;
                        case TOK_LE: branch_op = OP_IFGT; break;
                        default: branch_op = OP_IFNE; break;
                    }
                } else {
                    /* Integer/byte/char/short comparison */
                    switch (op) {
                        case TOK_EQ: branch_op = OP_IF_ICMPNE; break;  /* branch if NOT equal */
                        case TOK_NE: branch_op = OP_IF_ICMPEQ; break;  /* branch if equal */
                        case TOK_LT: branch_op = OP_IF_ICMPGE; break;  /* branch if NOT less than */
                        case TOK_GE: branch_op = OP_IF_ICMPLT; break;  /* branch if less than */
                        case TOK_GT: branch_op = OP_IF_ICMPLE; break;  /* branch if NOT greater than */
                        case TOK_LE: branch_op = OP_IF_ICMPGT; break;  /* branch if greater than */
                        default: branch_op = OP_IF_ICMPNE; break;
                    }
                }
                
                /* Emit branch instruction
                 * Pattern: branch +7 → iconst_1 → goto +4 → iconst_0 → ...
                 * Offset 7 = branch (3 bytes) + iconst_1 (1) + goto (3) = 7 to reach iconst_0
                 */
                bc_emit(mg->code, branch_op);
                bc_emit_u2(mg->code, 7);
                
                /* Pop operands before branch - they're consumed by the comparison */
                mg_pop_typed(mg, 2);
                
                /* Emit: iconst_1 (condition was true) */
                bc_emit(mg->code, OP_ICONST_1);
                mg_push_int(mg);
                
                /* Emit: goto +4 (skip iconst_0) */
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 4);  /* offset to end */
                
                /* Record frame at iconst_0 (branch target from if_icmpXX) */
                /* Pop the iconst_1 that the other path pushed, for frame recording */
                mg_pop_typed(mg, 1);
                mg_record_frame(mg);
                
                /* iconst_0 (condition was false) */
                bc_emit(mg->code, OP_ICONST_0);
                mg_push_int(mg);
                
                /* Record frame at end (goto target from iconst_1 path) */
                mg_record_frame(mg);
                
                /* Result is on stack */
                return true;
            }
        
        default:
            /* Unknown operator */
            fprintf(stderr, "codegen: unknown binary operator: %s (token %d)\n", 
                    expr->data.node.name ? expr->data.node.name : "?", op);
            return false;
    }
    
    mg_pop_typed(mg, 1);  /* Two operands -> one result */
    return true;
}

/* ========================================================================
 * Method Descriptor Building
 * ======================================================================== */

/*
 * Infer the type descriptor for an expression argument
 */
static const char *infer_arg_descriptor(ast_node_t *arg)
{
    if (!arg) return "I";
    
    switch (arg->type) {
        case AST_LITERAL:
            switch (arg->data.leaf.token_type) {
                case TOK_STRING_LITERAL:
                    return "Ljava/lang/String;";
                case TOK_INTEGER_LITERAL:
                    return "I";
                case TOK_LONG_LITERAL:
                    return "J";
                case TOK_FLOAT_LITERAL:
                    return "F";
                case TOK_DOUBLE_LITERAL:
                    return "D";
                case TOK_TRUE:
                case TOK_FALSE:
                    return "Z";
                case TOK_CHAR_LITERAL:
                    return "C";
                case TOK_NULL:
                    return "Ljava/lang/Object;";
                default:
                    return "I";
            }
        case AST_NEW_OBJECT:
            /* Get class type from first child */
            if (arg->data.node.children) {
                ast_node_t *type_node = (ast_node_t *)arg->data.node.children->data;
                if (type_node->type == AST_CLASS_TYPE) {
                    static char class_desc[256];
                    const char *name = resolve_java_lang_class(type_node->data.node.name);
                    snprintf(class_desc, sizeof(class_desc), "L%s;", name);
                    /* Convert dots to slashes */
                    for (char *p = class_desc + 1; *p != ';'; p++) {
                        if (*p == '.') *p = '/';
                    }
                    return class_desc;
                }
            }
            return "Ljava/lang/Object;";
        case AST_NEW_ARRAY:
            /* Check sem_type for proper array descriptor */
            if (arg->sem_type && arg->sem_type->kind == TYPE_ARRAY) {
                static char new_arr_desc[256];
                char *desc = type_to_descriptor(arg->sem_type);
                strncpy(new_arr_desc, desc, sizeof(new_arr_desc) - 1);
                new_arr_desc[sizeof(new_arr_desc) - 1] = '\0';
                free(desc);
                return new_arr_desc;
            }
            return "[Ljava/lang/Object;";
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
            return "Ljava/lang/Object;";  /* TODO: use current class type */
        case AST_CLASS_LITERAL:
            return "Ljava/lang/Class;";
        case AST_BINARY_EXPR:
            /* Check if this is string concatenation */
            if (arg->data.node.name && strcmp(arg->data.node.name, "+") == 0) {
                slist_t *children = arg->data.node.children;
                if (children) {
                    ast_node_t *left = (ast_node_t *)children->data;
                    ast_node_t *right = children->next ? (ast_node_t *)children->next->data : NULL;
                    
                    /* If either operand is a string, result is string */
                    const char *left_desc = infer_arg_descriptor(left);
                    if (strcmp(left_desc, "Ljava/lang/String;") == 0) {
                        return "Ljava/lang/String;";
                    }
                    if (right) {
                        const char *right_desc = infer_arg_descriptor(right);
                        if (strcmp(right_desc, "Ljava/lang/String;") == 0) {
                            return "Ljava/lang/String;";
                        }
                    }
                }
            }
            /* Fall through to check sem_type */
            if (arg->sem_type) {
                switch (arg->sem_type->kind) {
                    case TYPE_INT:
                    case TYPE_BYTE:
                    case TYPE_SHORT:
                    case TYPE_CHAR:
                    case TYPE_BOOLEAN:
                        return "I";
                    case TYPE_LONG:
                        return "J";
                    case TYPE_FLOAT:
                        return "F";
                    case TYPE_DOUBLE:
                        return "D";
                    case TYPE_CLASS:
                        if (strcmp(arg->sem_type->data.class_type.name, "java.lang.String") == 0) {
                            return "Ljava/lang/String;";
                        }
                        return "Ljava/lang/Object;";
                    default:
                        break;
                }
            }
            return "I";  /* Default for numeric binary operations */
        default:
            /* Check sem_type if available */
            if (arg->sem_type) {
                switch (arg->sem_type->kind) {
                    case TYPE_INT:
                    case TYPE_BYTE:
                    case TYPE_SHORT:
                    case TYPE_CHAR:
                    case TYPE_BOOLEAN:
                        return "I";
                    case TYPE_LONG:
                        return "J";
                    case TYPE_FLOAT:
                        return "F";
                    case TYPE_DOUBLE:
                        return "D";
                    case TYPE_CLASS:
                        if (arg->sem_type->data.class_type.symbol &&
                            arg->sem_type->data.class_type.symbol->qualified_name) {
                            static char sem_desc[256];
                            const char *qn = arg->sem_type->data.class_type.symbol->qualified_name;
                            snprintf(sem_desc, sizeof(sem_desc), "L%s;", qn);
                            for (char *p = sem_desc + 1; *p != ';'; p++) {
                                if (*p == '.') *p = '/';
                            }
                            return sem_desc;
                        }
                        return "Ljava/lang/Object;";
                    case TYPE_ARRAY:
                        {
                            /* Use type_to_descriptor for proper array element type */
                            static char arr_desc[256];
                            char *desc = type_to_descriptor(arg->sem_type);
                            strncpy(arr_desc, desc, sizeof(arr_desc) - 1);
                            arr_desc[sizeof(arr_desc) - 1] = '\0';
                            free(desc);
                            return arr_desc;
                        }
                    default:
                        return "I";
                }
            }
            return "I";
    }
}

/*
 * Build a method descriptor from AST parameter types
 * Returns something like "(II)I" for a method taking two ints and returning int
 */
static char *build_method_descriptor(slist_t *args, ast_node_t *return_type)
{
    string_t *desc = string_new("(");
    
    /* Add parameter types - infer from expression types */
    for (slist_t *node = args; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        const char *arg_desc = infer_arg_descriptor(arg);
        string_append(desc, arg_desc);
    }
    
    string_append_c(desc, ')');
    
    /* Add return type */
    if (return_type) {
        char *ret = ast_type_to_descriptor(return_type);
        string_append(desc, ret);
        free(ret);
    } else {
        string_append(desc, "I");  /* Default to int */
    }
    
    return string_free(desc, false);
}

/*
 * Build a method descriptor from args and a type_t return type
 * Used for methods loaded from classfiles where we have type info but no AST
 */
static char *build_method_descriptor_with_type(slist_t *args, type_t *return_type)
{
    string_t *desc = string_new("(");
    
    /* Add parameter types - infer from expression types */
    for (slist_t *node = args; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        const char *arg_desc = infer_arg_descriptor(arg);
        string_append(desc, arg_desc);
    }
    
    string_append_c(desc, ')');
    
    /* Add return type from type_t */
    if (return_type) {
        char *ret = type_to_descriptor(return_type);
        string_append(desc, ret);
        free(ret);
    } else {
        string_append(desc, "I");  /* Default to int */
    }
    
    return string_free(desc, false);
}

/*
 * Build a method descriptor from a method symbol
 * Uses the symbol's parameter list and return type for accurate descriptor
 */
static char *build_method_descriptor_from_symbol(symbol_t *method_sym)
{
    if (!method_sym || method_sym->kind != SYM_METHOD) {
        return strdup("()I");  /* Default fallback */
    }
    
    string_t *desc = string_new("(");
    
    /* Add parameter types from symbol */
    slist_t *params = method_sym->data.method_data.parameters;
    for (slist_t *node = params; node; node = node->next) {
        symbol_t *param = (symbol_t *)node->data;
        if (param && param->type) {
            char *param_desc = type_to_descriptor(param->type);
            string_append(desc, param_desc);
            free(param_desc);
        } else {
            string_append(desc, "I");  /* Default to int */
        }
    }
    
    string_append_c(desc, ')');
    
    /* Add return type from symbol */
    if (method_sym->type) {
        char *ret_desc = type_to_descriptor(method_sym->type);
        string_append(desc, ret_desc);
        free(ret_desc);
    } else {
        string_append(desc, "V");  /* Default to void */
    }
    
    return string_free(desc, false);
}

/* ========================================================================
 * Method Call Code Generation
 * ======================================================================== */

/**
 * Get the class type for a PrintStream method parameter.
 * Returns the method descriptor for println/print methods.
 */
static const char *get_print_method_descriptor(method_gen_t *mg, const char *method_name, ast_node_t *arg)
{
    if (strcmp(method_name, "println") != 0 && strcmp(method_name, "print") != 0) {
        return NULL;
    }
    
    /* No arguments - println() */
    if (!arg) {
        return "()V";
    }
    
    /* Check argument type */
    if (arg->type == AST_LITERAL) {
        switch (arg->data.leaf.token_type) {
            case TOK_STRING_LITERAL:
            case TOK_TEXT_BLOCK:
                return "(Ljava/lang/String;)V";
            case TOK_INTEGER_LITERAL:
                return "(I)V";
            case TOK_LONG_LITERAL:
                return "(J)V";
            case TOK_FLOAT_LITERAL:
                return "(F)V";
            case TOK_DOUBLE_LITERAL:
                return "(D)V";
            case TOK_CHAR_LITERAL:
                return "(C)V";
            case TOK_TRUE:
            case TOK_FALSE:
                return "(Z)V";
            default:
                break;
        }
    }
    
    /* Check semantic type */
    if (arg->sem_type) {
        switch (arg->sem_type->kind) {
            case TYPE_INT:
            case TYPE_BYTE:
            case TYPE_SHORT:
                return "(I)V";
            case TYPE_LONG:
                return "(J)V";
            case TYPE_FLOAT:
                return "(F)V";
            case TYPE_DOUBLE:
                return "(D)V";
            case TYPE_BOOLEAN:
                return "(Z)V";
            case TYPE_CHAR:
                return "(C)V";
            case TYPE_CLASS:
                if (arg->sem_type->data.class_type.name) {
                    if (strcmp(arg->sem_type->data.class_type.name, "String") == 0 ||
                        strcmp(arg->sem_type->data.class_type.name, "java.lang.String") == 0 ||
                        strcmp(arg->sem_type->data.class_type.name, "java/lang/String") == 0) {
                        return "(Ljava/lang/String;)V";
                    }
                }
                return "(Ljava/lang/Object;)V";
            default:
                break;
        }
    }
    
    /* Check if it's a string expression */
    if (is_string_type(arg)) {
        return "(Ljava/lang/String;)V";
    }
    
    /* For identifiers, check if it's a reference type in local_refs */
    if (arg->type == AST_IDENTIFIER && mg) {
        const char *name = arg->data.leaf.name;
        
        /* Check local variable type */
        type_kind_t local_type = mg_get_local_type(mg, name);
        switch (local_type) {
            case TYPE_LONG: return "(J)V";
            case TYPE_FLOAT: return "(F)V";
            case TYPE_DOUBLE: return "(D)V";
            case TYPE_BOOLEAN: return "(Z)V";
            case TYPE_CHAR: return "(C)V";
            case TYPE_CLASS:
            case TYPE_ARRAY:
                /* Check for String type */
                if (mg_local_class_name(mg, name)) {
                    const char *class_name = mg_local_class_name(mg, name);
                    if (strcmp(class_name, "java/lang/String") == 0) {
                        return "(Ljava/lang/String;)V";
                    }
                }
                return "(Ljava/lang/Object;)V";
            default:
                return "(I)V";
        }
    }
    
    /* Handle array access: arr[index] */
    if (arg->type == AST_ARRAY_ACCESS && mg) {
        slist_t *children = arg->data.node.children;
        if (children) {
            ast_node_t *array_expr = (ast_node_t *)children->data;
            
            /* Check the array's semantic type */
            if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    switch (elem_type->kind) {
                        case TYPE_INT:
                        case TYPE_BYTE:
                        case TYPE_SHORT:
                            return "(I)V";
                        case TYPE_LONG:
                            return "(J)V";
                        case TYPE_FLOAT:
                            return "(F)V";
                        case TYPE_DOUBLE:
                            return "(D)V";
                        case TYPE_BOOLEAN:
                            return "(Z)V";
                        case TYPE_CHAR:
                            return "(C)V";
                        case TYPE_CLASS:
                            if (elem_type->data.class_type.name) {
                                if (strcmp(elem_type->data.class_type.name, "String") == 0 ||
                                    strcmp(elem_type->data.class_type.name, "java.lang.String") == 0 ||
                                    strcmp(elem_type->data.class_type.name, "java/lang/String") == 0) {
                                    return "(Ljava/lang/String;)V";
                                }
                            }
                            return "(Ljava/lang/Object;)V";
                        default:
                            break;
                    }
                }
            }
            
            /* If no semantic type, try to get type from local array tracking */
            if (array_expr->type == AST_IDENTIFIER) {
                const char *arr_name = array_expr->data.leaf.name;
                if (mg_local_is_array(mg, arr_name)) {
                    type_kind_t elem_kind = mg_local_array_elem_kind(mg, arr_name);
                    switch (elem_kind) {
                        case TYPE_INT:
                        case TYPE_BYTE:
                        case TYPE_SHORT:
                            return "(I)V";
                        case TYPE_LONG:
                            return "(J)V";
                        case TYPE_FLOAT:
                            return "(F)V";
                        case TYPE_DOUBLE:
                            return "(D)V";
                        case TYPE_BOOLEAN:
                            return "(Z)V";
                        case TYPE_CHAR:
                            return "(C)V";
                        case TYPE_CLASS: {
                            /* For object arrays, check element class name */
                            const char *elem_class = mg_local_array_elem_class(mg, arr_name);
                            if (elem_class) {
                                if (strcmp(elem_class, "java/lang/String") == 0) {
                                    return "(Ljava/lang/String;)V";
                                }
                            }
                            return "(Ljava/lang/Object;)V";
                        }
                        default:
                            break;
                    }
                }
            }
        }
        /* Default to int for array access */
        return "(I)V";
    }
    
    /* Default to int for unknown */
    return "(I)V";
}

/**
 * Get the type class of a field access receiver.
 * For System.out, returns "java/io/PrintStream".
 */
static const char *get_field_access_type_class(ast_node_t *field_access)
{
    if (!field_access || field_access->type != AST_FIELD_ACCESS) {
        return NULL;
    }
    
    slist_t *children = field_access->data.node.children;
    if (!children) {
        return NULL;
    }
    
    ast_node_t *recv = (ast_node_t *)children->data;
    const char *field_name = field_access->data.node.name;
    
    /* Check for System.out / System.err */
    if (recv->type == AST_IDENTIFIER) {
        const char *recv_name = recv->data.leaf.name;
        if (strcmp(recv_name, "System") == 0) {
            if (strcmp(field_name, "out") == 0 || strcmp(field_name, "err") == 0) {
                return "java/io/PrintStream";
            }
            if (strcmp(field_name, "in") == 0) {
                return "java/io/InputStream";
            }
        }
    }
    
    /* Check semantic type */
    if (field_access->sem_type && field_access->sem_type->kind == TYPE_CLASS) {
        if (field_access->sem_type->data.class_type.name) {
            return class_to_internal_name(field_access->sem_type->data.class_type.name);
        }
    }
    
    return NULL;
}

static bool codegen_method_call(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_METHOD_CALL) {
        return false;
    }
    
    const char *method_name = expr->data.node.name;
    if (!method_name) {
        fprintf(stderr, "codegen: method call without name\n");
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    
    bool is_static = false;
    bool is_interface_call = false;  /* Track if receiver is an interface type */
    const char *target_class = NULL;
    ast_node_t *receiver = NULL;
    slist_t *args = children;
    bool is_void_return = false;
    char *custom_descriptor = NULL;
    
    /* FIRST: Check if first child is a field access (e.g., System.out.println) */
    if (children) {
        ast_node_t *first = (ast_node_t *)children->data;
        if (first->type == AST_FIELD_ACCESS) {
            /* Check if the field access is actually a class reference (FQN like java.util.Objects) */
            if (first->sem_symbol && 
                (first->sem_symbol->kind == SYM_CLASS || 
                 first->sem_symbol->kind == SYM_INTERFACE ||
                 first->sem_symbol->kind == SYM_ENUM)) {
                /* This is a static method call on a class (java.util.Objects.requireNonNull) */
                /* Don't set receiver - it's not an instance, just a class reference */
                args = children->next;
                is_static = true;
                symbol_t *class_sym = first->sem_symbol;
                target_class = class_sym->qualified_name ?
                    class_to_internal_name(class_sym->qualified_name) :
                    class_to_internal_name(class_sym->name);
                is_interface_call = (class_sym->kind == SYM_INTERFACE);
                
                /* Handle synthetic enum methods values() and valueOf() */
                if (class_sym->kind == SYM_ENUM) {
                    if (strcmp(method_name, "values") == 0) {
                        /* values() returns EnumType[] */
                        char desc[256];
                        snprintf(desc, sizeof(desc), "()[L%s;", target_class);
                        custom_descriptor = strdup(desc);
                    } else if (strcmp(method_name, "valueOf") == 0) {
                        /* valueOf(String) returns EnumType */
                        char desc[256];
                        snprintf(desc, sizeof(desc), "(Ljava/lang/String;)L%s;", target_class);
                        custom_descriptor = strdup(desc);
                    }
                }
                
                /* Look up the method in the class - store in semantic symbol for later use */
                if (class_sym->data.class_data.members) {
                    /* Count arguments for overload resolution */
                    int arg_count = 0;
                    for (slist_t *a = args; a; a = a->next) arg_count++;
                    
                    symbol_t *fqn_method = scope_lookup_method_with_args(
                        class_sym->data.class_data.members, method_name, arg_count);
                    
                    /* Check superclass chain if not found */
                    if (!fqn_method || fqn_method->kind != SYM_METHOD) {
                        symbol_t *super = class_sym->data.class_data.superclass;
                        while (super && (!fqn_method || fqn_method->kind != SYM_METHOD)) {
                            if (super->data.class_data.members) {
                                fqn_method = scope_lookup_method_with_args(
                                    super->data.class_data.members, method_name, arg_count);
                            }
                            super = super->data.class_data.superclass;
                        }
                    }
                    /* Store method for later use */
                    if (fqn_method && fqn_method->kind == SYM_METHOD) {
                        expr->sem_symbol = fqn_method;
                    }
                }
            } else {
                /* Regular field access (e.g., System.out.println) */
                receiver = first;
                args = children->next;
                target_class = get_field_access_type_class(first);
                
                /* Check if the field's type is an interface */
                if (first->sem_type && first->sem_type->kind == TYPE_CLASS &&
                    first->sem_type->data.class_type.symbol) {
                    is_interface_call = (first->sem_type->data.class_type.symbol->kind == SYM_INTERFACE);
                }
                
                /* Check for PrintStream.println/print */
                if (target_class && strcmp(target_class, "java/io/PrintStream") == 0) {
                    ast_node_t *arg = args ? (ast_node_t *)args->data : NULL;
                    const char *print_desc = get_print_method_descriptor(mg, method_name, arg);
                    if (print_desc) {
                        custom_descriptor = strdup(print_desc);
                        is_void_return = true;
                    }
                }
            }
        }
    }
    
    /* Determine receiver and arguments - check if first child is a receiver */
    /* Skip if we've already determined this is a static call via FQN (is_static already set) */
    symbol_t *method_sym = NULL;
    
    /* PREFER semantic analysis result - it has proper overload resolution */
    if (expr->sem_symbol && expr->sem_symbol->kind == SYM_METHOD) {
        method_sym = expr->sem_symbol;
    }
    
    if (!receiver && !is_static && children) {
        ast_node_t *first = (ast_node_t *)children->data;
        
        if (first->type == AST_THIS_EXPR) {
            /* Explicit this.method() */
            receiver = first;
            args = children->next;
            /* Look up method in current class (only if semantic analysis didn't resolve it) */
            if (!method_sym && mg->class_gen && mg->class_gen->class_sym) {
                symbol_t *class_sym = mg->class_gen->class_sym;
                if (class_sym->data.class_data.members) {
                    method_sym = scope_lookup_local(
                        class_sym->data.class_data.members, method_name);
                    if (method_sym && method_sym->kind == SYM_METHOD) {
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        target_class = mg->class_gen->internal_name;
                        is_interface_call = (class_sym->kind == SYM_INTERFACE);
                    }
                }
            }
        } else if (first->type == AST_IDENTIFIER) {
            /* Could be a local variable receiver (obj.method()), 
             * a class/enum name for static call (Day.values()), 
             * or just the first arg */
            const char *name = first->data.leaf.name;
            
            /* First check if it's a class/enum name for static method call */
            bool is_class_ref = false;
            
            /* Check if semantic analysis already resolved this as a class reference */
            if (first->sem_symbol && 
                (first->sem_symbol->kind == SYM_CLASS ||
                 first->sem_symbol->kind == SYM_INTERFACE ||
                 first->sem_symbol->kind == SYM_ENUM)) {
                /* It's a class reference resolved via import (e.g., Objects after import java.util.Objects) */
                is_class_ref = true;
                is_static = true;
                is_interface_call = (first->sem_symbol->kind == SYM_INTERFACE);
                args = children->next;  /* Skip the class reference */
                
                symbol_t *class_sym = first->sem_symbol;
                target_class = class_sym->qualified_name ?
                    class_to_internal_name(class_sym->qualified_name) :
                    class_to_internal_name(class_sym->name);
                
                /* Handle synthetic enum methods values() and valueOf() */
                if (class_sym->kind == SYM_ENUM) {
                    if (strcmp(method_name, "values") == 0) {
                        /* values() returns EnumType[] */
                        char desc[256];
                        snprintf(desc, sizeof(desc), "()[L%s;", target_class);
                        custom_descriptor = strdup(desc);
                    } else if (strcmp(method_name, "valueOf") == 0) {
                        /* valueOf(String) returns EnumType */
                        char desc[256];
                        snprintf(desc, sizeof(desc), "(Ljava/lang/String;)L%s;", target_class);
                        custom_descriptor = strdup(desc);
                    }
                }
                
                /* Look up the method in the class (only if not already resolved) */
                if (!method_sym && class_sym->data.class_data.members) {
                    symbol_t *found_method = lookup_method_in_hierarchy(
                        class_sym, method_name, NULL);
                    if (found_method && found_method->kind == SYM_METHOD) {
                        method_sym = found_method;
                    }
                }
            }
            
            if (!is_class_ref && mg->class_gen && mg->class_gen->class_sym) {
                symbol_t *class_sym = mg->class_gen->class_sym;
                /* Check nested types in current class */
                if (class_sym->data.class_data.members) {
                    symbol_t *nested = scope_lookup_local(
                        class_sym->data.class_data.members, name);
                    if (nested && (nested->kind == SYM_CLASS || 
                                  nested->kind == SYM_INTERFACE ||
                                  nested->kind == SYM_ENUM)) {
                        /* It's a static call on a nested type */
                        is_class_ref = true;
                        is_static = true;
                        is_interface_call = (nested->kind == SYM_INTERFACE);
                        args = children->next;  /* Skip the class reference */
                        
                        /* Build the internal class name */
                        char nested_internal[256];
                        snprintf(nested_internal, sizeof(nested_internal), "%s$%s",
                                 mg->class_gen->internal_name, name);
                        target_class = strdup(nested_internal);
                        
                        /* For enums, values() and valueOf() are synthetic - generate descriptor */
                        if (nested->kind == SYM_ENUM) {
                            if (strcmp(method_name, "values") == 0) {
                                /* values() returns EnumType[] */
                                char desc[256];
                                snprintf(desc, sizeof(desc), "()[L%s;", nested_internal);
                                custom_descriptor = strdup(desc);
                            } else if (strcmp(method_name, "valueOf") == 0) {
                                /* valueOf(String) returns EnumType */
                                char desc[256];
                                snprintf(desc, sizeof(desc), "(Ljava/lang/String;)L%s;", nested_internal);
                                custom_descriptor = strdup(desc);
                            }
                        } else if (!method_sym && nested->data.class_data.members) {
                            method_sym = scope_lookup_local(
                                nested->data.class_data.members, method_name);
                        }
                    }
                }
            }
            
            /* Check for external class reference (e.g., System.exit(), Math.abs()) */
            if (!is_class_ref) {
                const char *ext_class = resolve_class_name(mg, name);
                if (ext_class && mg->class_gen && mg->class_gen->sem) {
                    /* It's a known class name - load it and look up the method */
                    char *qualified = strdup(ext_class);
                    for (char *p = qualified; *p; p++) {
                        if (*p == '/') {
                            *p = '.';
                        }
                    }
                    
                    classfile_t *cf = classpath_load_class(
                        mg->class_gen->sem->classpath, qualified);
                    if (cf) {
                        symbol_t *ext_class_sym = symbol_from_classfile(
                            mg->class_gen->sem, cf);
                        if (ext_class_sym && ext_class_sym->data.class_data.members) {
                            symbol_t *found_method = lookup_method_in_hierarchy(
                                ext_class_sym, method_name, NULL);
                            if (found_method && found_method->kind == SYM_METHOD &&
                                (found_method->modifiers & MOD_STATIC)) {
                                /* Found a static method in the external class */
                                is_class_ref = true;
                                is_static = true;
                                args = children->next;  /* Skip the class reference */
                                target_class = strdup(ext_class);
                                if (!method_sym) method_sym = found_method;
                            }
                        }
                    }
                    free(qualified);
                }
            }
            
            /* Check types cache for classes loaded from sourcepath */
            if (!is_class_ref && mg->class_gen && mg->class_gen->sem) {
                type_t *class_type = hashtable_lookup(mg->class_gen->sem->types, name);
                if (class_type && class_type->kind == TYPE_CLASS && 
                    class_type->data.class_type.symbol) {
                    symbol_t *src_class_sym = class_type->data.class_type.symbol;
                    if (src_class_sym->data.class_data.members) {
                        symbol_t *found_method = lookup_method_in_hierarchy(
                            src_class_sym, method_name, NULL);
                        if (found_method && found_method->kind == SYM_METHOD &&
                            (found_method->modifiers & MOD_STATIC)) {
                            /* Found a static method in the source-loaded class */
                            is_class_ref = true;
                            is_static = true;
                            args = children->next;  /* Skip the class reference */
                            /* Convert qualified name to internal format */
                            target_class = src_class_sym->qualified_name ?
                                class_to_internal_name(src_class_sym->qualified_name) :
                                class_to_internal_name(name);
                            if (!method_sym) method_sym = found_method;
                        }
                    }
                }
            }
            
            local_var_info_t *local_info = (local_var_info_t *)hashtable_lookup(mg->locals, name);
            if (!is_class_ref && local_info) {
                /* It's a local variable - check if it has a class type */
                type_t *recv_type = first->sem_type;
                symbol_t *recv_class_sym = NULL;
                
                /* Handle type variables by using their bound type */
                if (recv_type && recv_type->kind == TYPE_TYPEVAR && recv_type->data.type_var.bound) {
                    recv_type = recv_type->data.type_var.bound;
                }
                
                if (recv_type && recv_type->kind == TYPE_CLASS && recv_type->data.class_type.symbol) {
                    recv_class_sym = recv_type->data.class_type.symbol;
                } else {
                    /* sem_type not available - try to get class from local tracking */
                    const char *local_class = mg_local_class_name(mg, name);
                    if (local_class && mg->class_gen && mg->class_gen->sem) {
                        /* Look up the class symbol by name */
                        /* Convert internal name (com/example/Foo) back to qualified (com.example.Foo) */
                        char *qualified = strdup(local_class);
                        for (char *p = qualified; *p; p++) {
                            if (*p == '/') *p = '.';
                        }
                        
                        /* Load from classpath to get full class hierarchy */
                        classfile_t *cf = classpath_load_class(mg->class_gen->sem->classpath, qualified);
                        if (cf) {
                            symbol_t *loaded_sym = symbol_from_classfile(mg->class_gen->sem, cf);
                            if (loaded_sym) {
                                recv_class_sym = loaded_sym;
                            }
                        }
                        
                        /* Fall back to types cache if classpath load failed */
                        if (!recv_class_sym) {
                            type_t *class_type = hashtable_lookup(mg->class_gen->sem->types, qualified);
                            if (class_type && class_type->kind == TYPE_CLASS) {
                                recv_class_sym = class_type->data.class_type.symbol;
                            }
                        }
                        free(qualified);
                    }
                }
                
                if (recv_class_sym) {
                    /* Look up method in the receiver's class and superclasses */
                    symbol_t *owner_class = NULL;
                    symbol_t *found_method = lookup_method_in_hierarchy(
                        recv_class_sym, method_name, &owner_class);
                    if (found_method) {
                        /* Method found in receiver's class or superclass */
                        receiver = first;
                        args = children->next;
                        /* Prefer semantic analysis result for correct overload */
                        if (!method_sym) method_sym = found_method;
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        /* Use the class where method is declared for target */
                        target_class = owner_class->qualified_name ?
                            class_to_internal_name(owner_class->qualified_name) :
                            mg->class_gen->internal_name;
                        is_interface_call = (owner_class->kind == SYM_INTERFACE);
                    }
                }
                
                /* Final fallback: check if method exists in current class */
                /* Only applies when identifier could be a receiver (has class type) */
                /* If first arg is primitive, don't treat it as receiver - it's just an arg */
            }
            
            /* Check if identifier is a field reference (static or instance field) */
            if (!receiver && !is_class_ref && !is_static && mg->class_gen && mg->class_gen->class_sym) {
                symbol_t *class_sym = mg->class_gen->class_sym;
                symbol_t *field_sym = NULL;
                
                /* Look for field in current class and superclasses */
                symbol_t *search_class = class_sym;
                while (search_class && !field_sym) {
                    if (search_class->data.class_data.members) {
                        field_sym = scope_lookup_local(search_class->data.class_data.members, name);
                        if (field_sym && field_sym->kind != SYM_FIELD) {
                            field_sym = NULL;  /* Not a field */
                        }
                    }
                    search_class = search_class->data.class_data.superclass;
                }
                
                if (field_sym && field_sym->type && field_sym->type->kind == TYPE_CLASS) {
                    /* It's a field with a class type - it can be a receiver */
                    symbol_t *recv_class_sym = field_sym->type->data.class_type.symbol;
                    
                    /* Try to load the class if symbol not set */
                    if (!recv_class_sym && field_sym->type->data.class_type.name && mg->class_gen->sem) {
                        recv_class_sym = load_external_class(mg->class_gen->sem, 
                            field_sym->type->data.class_type.name);
                        if (recv_class_sym) {
                            field_sym->type->data.class_type.symbol = recv_class_sym;
                        }
                    }
                    
                    if (recv_class_sym) {
                        symbol_t *owner_class = NULL;
                        symbol_t *found_method = lookup_method_in_hierarchy(
                            recv_class_sym, method_name, &owner_class);
                        if (found_method && found_method->kind == SYM_METHOD) {
                            receiver = first;
                            args = children->next;
                            /* Prefer semantic analysis result for correct overload */
                            if (!method_sym) method_sym = found_method;
                            is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                            target_class = owner_class && owner_class->qualified_name ?
                                class_to_internal_name(owner_class->qualified_name) :
                                class_to_internal_name(recv_class_sym->qualified_name);
                            is_interface_call = owner_class && (owner_class->kind == SYM_INTERFACE);
                        }
                    }
                }
            }
        } else if (first->type == AST_METHOD_CALL) {
            /* Method call as receiver: obj.method1().method2() */
            /* The result of method1() is the receiver for method2() */
            type_t *recv_type = first->sem_type;
            
            /* Handle type variables by using their bound type */
            if (recv_type && recv_type->kind == TYPE_TYPEVAR && recv_type->data.type_var.bound) {
                recv_type = recv_type->data.type_var.bound;
            }
            
            if (recv_type && recv_type->kind == TYPE_CLASS) {
                symbol_t *recv_class_sym = recv_type->data.class_type.symbol;
                
                /* If symbol not set, try to load the class externally */
                /* This happens for types like String from type_string() */
                if (!recv_class_sym && recv_type->data.class_type.name &&
                    mg->class_gen && mg->class_gen->sem) {
                    recv_class_sym = load_external_class(mg->class_gen->sem,
                        recv_type->data.class_type.name);
                }
                
                if (recv_class_sym && recv_class_sym->data.class_data.members) {
                    /* Count arguments for overload resolution (receiver is first) */
                    int arg_count = 0;
                    for (slist_t *a = children->next; a; a = a->next) arg_count++;
                    
                    symbol_t *found_method = scope_lookup_method_with_args(
                        recv_class_sym->data.class_data.members, method_name, arg_count);
                    
                    /* Check superclass chain if not found */
                    if (!found_method || found_method->kind != SYM_METHOD) {
                        symbol_t *super = recv_class_sym->data.class_data.superclass;
                        while (super && (!found_method || found_method->kind != SYM_METHOD)) {
                            if (super->data.class_data.members) {
                                found_method = scope_lookup_method_with_args(
                                    super->data.class_data.members, method_name, arg_count);
                            }
                            super = super->data.class_data.superclass;
                        }
                    }
                    
                    if (found_method && found_method->kind == SYM_METHOD) {
                        receiver = first;
                        args = children->next;
                        /* Prefer semantic analysis result for correct overload */
                        if (!method_sym) method_sym = found_method;
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        target_class = recv_class_sym->qualified_name ?
                            class_to_internal_name(recv_class_sym->qualified_name) :
                            mg->class_gen->internal_name;
                        is_interface_call = (recv_class_sym->kind == SYM_INTERFACE);
                    }
                }
            } else if (mg->class_gen && mg->class_gen->class_sym) {
                /* sem_type not available - fallback to current class */
                symbol_t *class_sym = mg->class_gen->class_sym;
                if (class_sym->data.class_data.members) {
                    symbol_t *found_method = scope_lookup_local(
                        class_sym->data.class_data.members, method_name);
                    if (found_method && found_method->kind == SYM_METHOD) {
                        receiver = first;
                        args = children->next;
                        /* Prefer semantic analysis result for correct overload */
                        if (!method_sym) method_sym = found_method;
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        target_class = mg->class_gen->internal_name;
                    }
                }
            }
        } else if ((first->type == AST_LITERAL ||
                    first->type == AST_NEW_OBJECT ||
                    first->type == AST_ARRAY_ACCESS ||
                    first->type == AST_PARENTHESIZED ||
                    first->type == AST_CAST_EXPR ||
                    first->type == AST_CONDITIONAL_EXPR ||
                    first->type == AST_CLASS_LITERAL) &&
                   (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER)) {
            /* Literal or expression as receiver: "abc".trim(), new String().length() */
            /* Also handles: String.class.getMethod("name"), chained method calls */
            /* Only applies when there's an explicit receiver (dot notation) */
            /* Use the method resolved by semantic analysis */
            if (expr->sem_symbol && expr->sem_symbol->kind == SYM_METHOD) {
                method_sym = expr->sem_symbol;
                receiver = first;
                args = children->next;
                is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                
                /* Get the class that owns this method */
                symbol_t *owner_class = method_sym->scope ? method_sym->scope->owner : NULL;
                if (owner_class && owner_class->qualified_name) {
                    target_class = class_to_internal_name(owner_class->qualified_name);
                } else if (first->sem_type && first->sem_type->kind == TYPE_CLASS && 
                           first->sem_type->data.class_type.name) {
                    /* Get class from the receiver's semantic type */
                    target_class = class_to_internal_name(first->sem_type->data.class_type.name);
                }
            }
        }
    }
    
    /* If no explicit receiver found, check for method in current class.
     * Even if method_sym is already set from semantic analysis, we need to
     * set target_class, is_static, and args appropriately. */
    if (!receiver && mg->class_gen && mg->class_gen->class_sym) {
        symbol_t *class_sym = mg->class_gen->class_sym;
        
        /* Special case: synthetic enum methods values() and valueOf() */
        if (!method_sym && class_sym->kind == SYM_ENUM) {
            if (strcmp(method_name, "values") == 0) {
                /* values() is a synthetic static method returning EnumType[] */
                is_static = true;
                target_class = mg->class_gen->internal_name;
                args = children;
                /* Build custom descriptor for values() */
                char desc[256];
                snprintf(desc, sizeof(desc), "()[L%s;", mg->class_gen->internal_name);
                custom_descriptor = strdup(desc);
            } else if (strcmp(method_name, "valueOf") == 0) {
                /* valueOf(String) is a synthetic static method returning EnumType */
                is_static = true;
                target_class = mg->class_gen->internal_name;
                args = children;
                /* Build custom descriptor for valueOf(String) */
                char desc[256];
                snprintf(desc, sizeof(desc), "(Ljava/lang/String;)L%s;", mg->class_gen->internal_name);
                custom_descriptor = strdup(desc);
            }
        }
        
        /* If method_sym is set (from semantic analysis), use it but set context */
        if (method_sym && !target_class) {
            is_static = (method_sym->modifiers & MOD_STATIC) != 0;
            
            /* Check if this is a static import (method from different class) */
            symbol_t *owner_class = method_sym->scope ? method_sym->scope->owner : NULL;
            if (owner_class && owner_class->qualified_name) {
                /* Method belongs to a different class (e.g., static import) */
                target_class = class_to_internal_name(owner_class->qualified_name);
                is_interface_call = (owner_class->kind == SYM_INTERFACE);
            } else {
                /* Method belongs to current class */
                target_class = mg->class_gen->internal_name;
                is_interface_call = (class_sym->kind == SYM_INTERFACE);
            }
            /* All children are arguments (no explicit receiver) */
            args = children;
        }
        else if (!is_static && !method_sym && class_sym->data.class_data.members) {
            symbol_t *found_method = scope_lookup_local(
                class_sym->data.class_data.members, method_name);
            if (found_method && found_method->kind == SYM_METHOD) {
                method_sym = found_method;
                is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                target_class = mg->class_gen->internal_name;
                /* If current class is an interface, use invokeinterface for instance methods */
                is_interface_call = (class_sym->kind == SYM_INTERFACE);
                
                /* For static methods or implicit this in instance methods */
                if (is_static || !mg->is_static) {
                    /* All children are arguments */
                    args = children;
                }
            }
        }
    }
    
    /* Check for statically imported method */
    char *static_import_class = NULL;  /* Track allocation for cleanup */
    if (!method_sym && !receiver && expr->sem_symbol && 
        expr->sem_symbol->kind == SYM_METHOD &&
        (expr->sem_symbol->modifiers & MOD_STATIC)) {
        method_sym = expr->sem_symbol;
        is_static = true;
        args = children;  /* All children are arguments */
        
        /* Get the class that owns this method from its scope */
        symbol_t *import_class_sym = method_sym->scope ? method_sym->scope->owner : NULL;
        if (import_class_sym && import_class_sym->qualified_name) {
            static_import_class = class_to_internal_name(import_class_sym->qualified_name);
            target_class = static_import_class;
        }
    }
    
    /* Generate receiver (for instance calls) */
    if (!is_static) {
        if (receiver) {
            /* Explicit receiver */
            if (!codegen_expr(mg, receiver, cp)) {
                if (custom_descriptor) free(custom_descriptor);
                if (static_import_class) free(static_import_class);
                return false;
            }
        } else if (!mg->is_static) {
            /* Implicit 'this' - only valid in instance methods */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push_null(mg);  /* Object reference */
        } else {
            /* Error: trying to call instance method without receiver in static context */
            fprintf(stderr, "codegen: cannot call instance method '%s' without receiver in static context\n", method_name);
            if (custom_descriptor) free(custom_descriptor);
            if (static_import_class) free(static_import_class);
            return false;
        }
    }
    
    /* Check if this is a varargs call */
    bool is_varargs_method = method_sym && (method_sym->modifiers & MOD_VARARGS);
    int fixed_param_count = 0;
    symbol_t *varargs_param = NULL;
    
    if (is_varargs_method && method_sym->data.method_data.parameters) {
        /* Count fixed parameters (all except last) */
        slist_t *p = method_sym->data.method_data.parameters;
        while (p) {
            symbol_t *param = (symbol_t *)p->data;
            if (!p->next) {
                varargs_param = param;  /* Last param is varargs */
            } else {
                fixed_param_count++;
            }
            p = p->next;
        }
    }
    
    /* Generate arguments with autoboxing/unboxing */
    slist_t *param_node = method_sym ? method_sym->data.method_data.parameters : NULL;
    int arg_index = 0;
    
    for (slist_t *node = args; node; node = node->next, arg_index++) {
        ast_node_t *arg = (ast_node_t *)node->data;
        
        /* Check if we've hit the varargs position */
        if (is_varargs_method && arg_index == fixed_param_count && varargs_param) {
            /* Count remaining arguments */
            int varargs_count = 0;
            for (slist_t *n = node; n; n = n->next) varargs_count++;
            
            /* Get element type from varargs array type */
            type_t *elem_type = NULL;
            if (varargs_param->type && varargs_param->type->kind == TYPE_ARRAY) {
                elem_type = varargs_param->type->data.array_type.element_type;
            }
            
            /* Check for array-to-varargs conversion:
             * If there's exactly one argument at the varargs position and it's
             * already an array of the compatible type, pass it directly */
            if (varargs_count == 1) {
                bool is_array_arg = false;
                const char *arg_elem_class = NULL;
                type_kind_t arg_elem_kind = TYPE_VOID;
                
                /* Check if argument is an identifier referencing an array local */
                if (arg->type == AST_IDENTIFIER && arg->data.leaf.name) {
                    const char *var_name = arg->data.leaf.name;
                    if (mg_local_is_array(mg, var_name)) {
                        is_array_arg = true;
                        arg_elem_class = mg_local_array_elem_class(mg, var_name);
                        arg_elem_kind = mg_local_array_elem_kind(mg, var_name);
                    }
                }
                /* Also check sem_type for other array expressions (field access, etc.) */
                else if (arg->sem_type && arg->sem_type->kind == TYPE_ARRAY) {
                    is_array_arg = true;
                    type_t *arg_elem = arg->sem_type->data.array_type.element_type;
                    if (arg_elem) {
                        arg_elem_kind = arg_elem->kind;
                        if (arg_elem->kind == TYPE_CLASS) {
                            arg_elem_class = arg_elem->data.class_type.name;
                        }
                    }
                }
                
                if (is_array_arg && elem_type) {
                    bool compatible = false;
                    
                    if (elem_type->kind == TYPE_CLASS) {
                        /* Object[] is compatible with any reference array */
                        if (strcmp(elem_type->data.class_type.name, "java.lang.Object") == 0) {
                            compatible = (arg_elem_kind == TYPE_CLASS);
                        }
                        /* Same class type */
                        else if (arg_elem_class) {
                            const char *expected = elem_type->data.class_type.name;
                            /* Handle qualified vs simple names */
                            const char *simple = strrchr(expected, '.');
                            if (simple) simple++; else simple = expected;
                            const char *arg_simple = strrchr(arg_elem_class, '/');
                            if (arg_simple) arg_simple++; else {
                                arg_simple = strrchr(arg_elem_class, '.');
                                if (arg_simple) arg_simple++; else arg_simple = arg_elem_class;
                            }
                            compatible = (strcmp(simple, arg_simple) == 0 ||
                                         strcmp(expected, arg_elem_class) == 0);
                        }
                    } else {
                        /* Primitive array - check exact type match */
                        compatible = (elem_type->kind == arg_elem_kind);
                    }
                    
                    if (compatible) {
                        /* Pass array directly - just generate the expression */
                        if (!codegen_expr(mg, arg, cp)) {
                            if (custom_descriptor) free(custom_descriptor);
                            if (static_import_class) free(static_import_class);
                            return false;
                        }
                        break;  /* Done with arguments */
                    }
                }
            }
            
            /* Create array: push size */
            if (varargs_count <= 5) {
                bc_emit(mg->code, OP_ICONST_0 + varargs_count);
            } else if (varargs_count <= 127) {
                bc_emit(mg->code, OP_BIPUSH);
                bc_emit_u1(mg->code, varargs_count);
            } else {
                bc_emit(mg->code, OP_SIPUSH);
                bc_emit_u2(mg->code, varargs_count);
            }
            mg_push_int(mg);  /* Array size is an integer */
            
            /* Create the array */
            if (elem_type && elem_type->kind == TYPE_CLASS) {
                char *internal = class_to_internal_name(elem_type->data.class_type.name);
                uint16_t class_ref = cp_add_class(cp, internal);
                bc_emit(mg->code, OP_ANEWARRAY);
                bc_emit_u2(mg->code, class_ref);
                free(internal);
            } else if (elem_type && type_kind_to_atype(elem_type->kind) >= 0) {
                /* Primitive array - use NEWARRAY */
                bc_emit(mg->code, OP_NEWARRAY);
                bc_emit_u1(mg->code, (uint8_t)type_kind_to_atype(elem_type->kind));
            } else {
                /* Default to Object[] */
                uint16_t obj_ref = cp_add_class(cp, "java/lang/Object");
                bc_emit(mg->code, OP_ANEWARRAY);
                bc_emit_u2(mg->code, obj_ref);
            }
            /* Array is now on stack (replaced size) */
            
            /* Store each varargs element */
            int va_idx = 0;
            for (slist_t *va_node = node; va_node; va_node = va_node->next, va_idx++) {
                ast_node_t *va_arg = (ast_node_t *)va_node->data;
                
                /* Dup array ref */
                bc_emit(mg->code, OP_DUP);
                mg_push_null(mg);  /* Duplicated array reference */
                
                /* Push index */
                if (va_idx <= 5) {
                    bc_emit(mg->code, OP_ICONST_0 + va_idx);
                } else if (va_idx <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_u1(mg->code, va_idx);
                } else {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_u2(mg->code, va_idx);
                }
                mg_push_int(mg);  /* Array index is an integer */
                
                /* Generate value */
                if (!codegen_expr(mg, va_arg, cp)) {
                    if (custom_descriptor) free(custom_descriptor);
                    if (static_import_class) free(static_import_class);
                    return false;
                }
                
                /* Box primitive if needed for Object[] */
                type_kind_t va_kind = get_expr_type_kind(mg, va_arg);
                if (va_arg->sem_type) va_kind = va_arg->sem_type->kind;
                
                if (elem_type && elem_type->kind == TYPE_CLASS &&
                    va_kind >= TYPE_BOOLEAN && va_kind <= TYPE_DOUBLE) {
                    emit_boxing(mg, cp, va_kind);
                }
                
                /* Store into array */
                if (elem_type && elem_type->kind >= TYPE_BOOLEAN && 
                    elem_type->kind <= TYPE_DOUBLE) {
                    uint8_t store_op = OP_AASTORE;
                    switch (elem_type->kind) {
                        case TYPE_BYTE:
                        case TYPE_BOOLEAN: store_op = OP_BASTORE; break;
                        case TYPE_CHAR:    store_op = OP_CASTORE; break;
                        case TYPE_SHORT:   store_op = OP_SASTORE; break;
                        case TYPE_INT:     store_op = OP_IASTORE; break;
                        case TYPE_LONG:    store_op = OP_LASTORE; break;
                        case TYPE_FLOAT:   store_op = OP_FASTORE; break;
                        case TYPE_DOUBLE:  store_op = OP_DASTORE; break;
                        default: store_op = OP_AASTORE;
                    }
                    bc_emit(mg->code, store_op);
                } else {
                    bc_emit(mg->code, OP_AASTORE);
                }
                mg_pop_typed(mg, 3);  /* Pop array, index, value */
            }
            
            /* Skip remaining args since we processed them */
            break;
        }
        
        /* Regular argument (not varargs) */
        if (!codegen_expr(mg, arg, cp)) {
            if (custom_descriptor) free(custom_descriptor);
            if (static_import_class) free(static_import_class);
            return false;
        }
        
        /* Check if boxing/unboxing needed for this argument */
        if (param_node) {
            symbol_t *param = (symbol_t *)param_node->data;
            if (param && param->type) {
                /* Get the argument's type - prefer sem_type, fallback to inferred */
                type_kind_t arg_kind = TYPE_UNKNOWN;
                const char *arg_class_name = NULL;
                
                if (arg->sem_type) {
                    arg_kind = arg->sem_type->kind;
                    if (arg_kind == TYPE_CLASS && arg->sem_type->data.class_type.name) {
                        arg_class_name = arg->sem_type->data.class_type.name;
                    }
                } else {
                    /* Fallback: infer from expression */
                    arg_kind = get_expr_type_kind(mg, arg);
                    if (arg->type == AST_IDENTIFIER) {
                        arg_class_name = mg_local_class_name(mg, arg->data.leaf.name);
                    }
                }
                
                /* Check if arg_kind is a primitive type */
                bool arg_is_primitive = (arg_kind >= TYPE_BOOLEAN && arg_kind <= TYPE_DOUBLE);
                bool param_is_primitive = (param->type->kind >= TYPE_BOOLEAN && 
                                           param->type->kind <= TYPE_DOUBLE);
                
                /* Boxing: primitive arg -> wrapper param */
                if (param->type->kind == TYPE_CLASS && param->type->data.class_type.name &&
                    arg_is_primitive) {
                    type_kind_t target_prim = get_primitive_for_wrapper(param->type->data.class_type.name);
                    if (target_prim != TYPE_UNKNOWN) {
                        emit_boxing(mg, cp, arg_kind);
                    }
                }
                /* Unboxing: wrapper arg -> primitive param */
                else if (param_is_primitive && arg_kind == TYPE_CLASS && arg_class_name) {
                    type_kind_t unbox_to = get_primitive_for_wrapper(arg_class_name);
                    if (unbox_to != TYPE_UNKNOWN && unbox_to == param->type->kind) {
                        char *internal = class_to_internal_name(arg_class_name);
                        emit_unboxing(mg, cp, param->type->kind, internal);
                        free(internal);
                    }
                }
            }
            param_node = param_node->next;
        }
    }
    
    /* Handle case where varargs method is called with no varargs arguments */
    if (is_varargs_method && varargs_param) {
        /* Count actual arguments */
        int arg_count = 0;
        for (slist_t *n = args; n; n = n->next) arg_count++;
        
        /* If we have exactly the fixed params (no varargs provided), create empty array */
        if (arg_count == fixed_param_count) {
            /* Get element type from varargs array type */
            type_t *elem_type = NULL;
            if (varargs_param->type && varargs_param->type->kind == TYPE_ARRAY) {
                elem_type = varargs_param->type->data.array_type.element_type;
            }
            
            /* Push 0 (empty array size) */
            bc_emit(mg->code, OP_ICONST_0);
            mg_push_int(mg);
            
            /* Create the empty array */
            if (elem_type && elem_type->kind == TYPE_CLASS) {
                char *internal = class_to_internal_name(elem_type->data.class_type.name);
                uint16_t class_ref = cp_add_class(cp, internal);
                bc_emit(mg->code, OP_ANEWARRAY);
                bc_emit_u2(mg->code, class_ref);
                free(internal);
            } else if (elem_type && type_kind_to_atype(elem_type->kind) >= 0) {
                bc_emit(mg->code, OP_NEWARRAY);
                bc_emit_u1(mg->code, (uint8_t)type_kind_to_atype(elem_type->kind));
            } else {
                uint16_t obj_ref = cp_add_class(cp, "java/lang/Object");
                bc_emit(mg->code, OP_ANEWARRAY);
                bc_emit_u2(mg->code, obj_ref);
            }
            /* Array replaces size on stack - no net change */
        }
    }
    
    /* Build method descriptor */
    char *descriptor;
    if (custom_descriptor) {
        descriptor = custom_descriptor;
    } else if (method_sym && method_sym->data.method_data.descriptor) {
        /* Use stored descriptor from classfile (most accurate for external methods) */
        descriptor = strdup(method_sym->data.method_data.descriptor);
        is_void_return = (method_sym->type && method_sym->type->kind == TYPE_VOID);
    } else if (method_sym && method_sym->type && method_sym->data.method_data.parameters) {
        /* Build descriptor from method symbol's parameter info (for source-defined methods) */
        descriptor = build_method_descriptor_from_symbol(method_sym);
        is_void_return = (method_sym->type->kind == TYPE_VOID);
    } else if (method_sym && method_sym->type) {
        /* Method from classfile without descriptor - use args + known return type */
        descriptor = build_method_descriptor_with_type(args, method_sym->type);
        is_void_return = (method_sym->type->kind == TYPE_VOID);
    } else {
        descriptor = build_method_descriptor(args, NULL);
    }
    
    /* Get method reference */
    if (!target_class) {
        target_class = mg->class_gen ? mg->class_gen->internal_name : "java/lang/Object";
    }
    
    /* Use interface methodref for interface calls (both instance and static) */
    uint16_t methodref;
    if (is_interface_call) {
        methodref = cp_add_interface_methodref(cp, target_class, method_name, descriptor);
    } else {
        methodref = cp_add_methodref(cp, target_class, method_name, descriptor);
    }
    free(descriptor);
    
    /* Emit invoke instruction */
    if (is_static) {
        bc_emit(mg->code, OP_INVOKESTATIC);
        bc_emit_u2(mg->code, methodref);
        /* Calling static interface method requires class file version 52 */
        if (is_interface_call && mg->class_gen) {
            mg->class_gen->has_default_methods = true;
        }
    } else if (is_interface_call) {
        /* invokeinterface has 5 bytes: opcode + index(2) + count(1) + zero(1) */
        bc_emit(mg->code, OP_INVOKEINTERFACE);
        bc_emit_u2(mg->code, methodref);
        /* count = argument slots + 1 (for receiver) */
        int slot_count = calculate_arg_slot_count(mg, args) + 1;
        bc_emit_u1(mg->code, (uint8_t)slot_count);
        bc_emit_u1(mg->code, 0);  /* Must be zero */
    } else {
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, methodref);
    }
    
    /* Check if we need a checkcast for generic return types.
     * When a method returns a type parameter (e.g., T in Supplier<T>.get()),
     * the actual bytecode returns Object. If semantic analysis determined a
     * more specific type (e.g., String), we need to emit a checkcast. */
    if (!is_void_return && expr->sem_type && expr->sem_type->kind == TYPE_CLASS) {
        bool needs_checkcast = false;
        const char *cast_target = NULL;
        
        /* Check if method's declared return type is a type variable */
        if (method_sym && method_sym->type && method_sym->type->kind == TYPE_TYPEVAR) {
            /* Method returns a type parameter - check if sem_type gives us the actual type */
            cast_target = expr->sem_type->data.class_type.name;
            if (cast_target && strcmp(cast_target, "java.lang.Object") != 0) {
                needs_checkcast = true;
            }
        }
        
        if (needs_checkcast && cast_target) {
            uint16_t class_idx = cp_add_class(cp, class_to_internal_name(cast_target));
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, class_idx);
        }
    }
    
    /* Update stack: pop receiver (if any) and args, push return value */
    /* Use slot count for args (long/double take 2 slots) */
    int arg_slots;
    if (is_varargs_method && varargs_param) {
        /* For varargs, we pushed: fixed args + 1 array (regardless of varargs count) */
        /* Count fixed parameter slots */
        arg_slots = 0;
        int idx = 0;
        for (slist_t *n = args; n && idx < fixed_param_count; n = n->next, idx++) {
            ast_node_t *arg = (ast_node_t *)n->data;
            type_kind_t kind = get_arg_type_kind(mg, arg);
            arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
        }
        /* Add 1 for the varargs array */
        arg_slots += 1;
    } else {
        arg_slots = calculate_arg_slot_count(mg, args);
    }
    mg_pop_typed(mg, arg_slots + (is_static ? 0 : 1));
    if (!is_void_return) {
        /* Push return value with correct type for stackmap tracking */
        if (method_sym && method_sym->type) {
            type_kind_t ret_kind = method_sym->type->kind;
            switch (ret_kind) {
                case TYPE_LONG:   mg_push_long(mg); break;
                case TYPE_DOUBLE: mg_push_double(mg); break;
                case TYPE_FLOAT:  mg_push_float(mg); break;
                case TYPE_CLASS:
                case TYPE_ARRAY:  mg_push_null(mg); break;
                default:          mg_push_int(mg); break;  /* boolean, byte, char, short, int */
            }
        } else {
            /* Unknown return type - assume int */
            mg_push_int(mg);
        }
    }
    
    if (static_import_class) free(static_import_class);
    return true;
}

/* ========================================================================
 * Explicit Constructor Invocation (this() and super())
 * ======================================================================== */

static bool codegen_explicit_ctor_call(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_EXPLICIT_CTOR_CALL) {
        return false;
    }
    
    const char *call_type = expr->data.node.name;
    bool is_this_call = (strcmp(call_type, "this") == 0);
    
    /* Determine target class for the constructor */
    const char *target_class = NULL;
    
    if (is_this_call) {
        /* this() - call another constructor in this class */
        target_class = mg->class_gen->internal_name;
    } else {
        /* super() - call superclass constructor */
        if (mg->class_gen->superclass) {
            target_class = mg->class_gen->superclass;
        } else {
            target_class = "java/lang/Object";
        }
    }
    
    /* Push 'this' reference */
    bc_emit(mg->code, OP_ALOAD_0);
    mg_push_null(mg);  /* Object reference */
    
    /* Generate constructor arguments */
    int arg_count = 0;
    for (slist_t *node = expr->data.node.children; node; node = node->next) {
        if (!codegen_expr(mg, (ast_node_t *)node->data, cp)) {
            return false;
        }
        arg_count++;
    }
    
    /* Build constructor descriptor */
    char *descriptor = build_method_descriptor(expr->data.node.children, NULL);
    /* Ensure descriptor has void return */
    size_t len = strlen(descriptor);
    if (len > 0 && descriptor[len - 1] != 'V') {
        descriptor[len - 1] = 'V';
    }
    
    /* Emit: invokespecial <init> */
    uint16_t init_ref = cp_add_methodref(cp, target_class, "<init>", descriptor);
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    
    free(descriptor);
    
    /* invokespecial consumes object ref and args, leaves nothing */
    mg_pop_typed(mg, arg_count + 1);
    
    return true;
}

/* ========================================================================
 * Object Creation Code Generation
 * ======================================================================== */

static bool codegen_new_object(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_NEW_OBJECT) {
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    if (!children) {
        fprintf(stderr, "codegen: new without type\n");
        return false;
    }
    
    /* Check if this is an anonymous class (has sem_symbol set) */
    bool is_anonymous_class = false;
    symbol_t *anon_sym = expr->sem_symbol;
    if (anon_sym && anon_sym->kind == SYM_CLASS && 
        anon_sym->data.class_data.is_anonymous_class) {
        is_anonymous_class = true;
        
        /* Add anonymous class to list for later compilation */
        if (mg->class_gen) {
            if (!mg->class_gen->anonymous_classes) {
                mg->class_gen->anonymous_classes = slist_new(anon_sym);
            } else {
                /* Check if already added (in case of multiple instantiations) */
                bool already_added = false;
                for (slist_t *ac = mg->class_gen->anonymous_classes; ac; ac = ac->next) {
                    if (ac->data == anon_sym) {
                        already_added = true;
                        break;
                    }
                }
                if (!already_added) {
                    slist_append(mg->class_gen->anonymous_classes, anon_sym);
                }
            }
        }
    }
    
    /* First child is the class type */
    ast_node_t *type_node = (ast_node_t *)children->data;
    const char *class_name = NULL;
    char *internal_name = NULL;
    
    if (is_anonymous_class) {
        /* For anonymous classes, use the synthetic class name */
        class_name = anon_sym->qualified_name;
        internal_name = class_to_internal_name(class_name);
    } else if (type_node->type == AST_CLASS_TYPE) {
        class_name = type_node->data.node.name;
        
        /* Check if semantic analysis resolved this type */
        if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
            /* First try symbol's qualified name (handles nested classes) */
            if (type_node->sem_type->data.class_type.symbol &&
                type_node->sem_type->data.class_type.symbol->qualified_name) {
                const char *qname = type_node->sem_type->data.class_type.symbol->qualified_name;
                internal_name = class_to_internal_name(qname);
            }
            /* If no symbol, try the type's name directly (for imported classes) */
            else if (type_node->sem_type->data.class_type.name) {
                const char *type_name = type_node->sem_type->data.class_type.name;
                internal_name = class_to_internal_name(type_name);
            }
        }
        
        /* If not resolved, check if it's a nested class of the current class */
        if (!internal_name && mg->class_gen && mg->class_gen->class_sym) {
            symbol_t *class_sym = mg->class_gen->class_sym;
            if (class_sym->data.class_data.members) {
                symbol_t *nested = scope_lookup_local(
                    class_sym->data.class_data.members, class_name);
                if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE)) {
                    internal_name = class_to_internal_name(nested->qualified_name);
                }
            }
        }
    }
    
    if (!class_name) {
        fprintf(stderr, "codegen: new without class name\n");
        return false;
    }
    
    /* Fall back to standard resolution if not already resolved */
    if (!internal_name) {
        const char *resolved_name = resolve_java_lang_class(class_name);
        internal_name = class_to_internal_name(resolved_name);
    }
    
    /* Emit: new <class> */
    uint16_t class_ref = cp_add_class(cp, internal_name);
    bc_emit(mg->code, OP_NEW);
    bc_emit_u2(mg->code, class_ref);
    mg_push_null(mg);  /* Uninitialized object reference */
    
    /* Emit: dup (we need two refs: one for invokespecial, one to keep) */
    bc_emit(mg->code, OP_DUP);
    mg_push_null(mg);  /* Duplicated object reference */
    
    /* Check if the target class is an inner class (non-static nested class) */
    /* We need to pass the outer instance as first constructor argument */
    bool is_inner_class = false;
    bool is_local_class = false;
    char *outer_internal = NULL;
    slist_t *captured_vars = NULL;  /* Captured variables for local/anonymous classes */
    
    /* For anonymous classes, use the anonymous class symbol */
    symbol_t *target_sym = is_anonymous_class ? anon_sym : 
        (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS ?
         type_node->sem_type->data.class_type.symbol : NULL);
    
    if (target_sym) {
        /* Check for captured variables (local or anonymous class) */
        if (target_sym->data.class_data.is_local_class || 
            target_sym->data.class_data.is_anonymous_class) {
            is_local_class = true;
            captured_vars = target_sym->data.class_data.captured_vars;
        }
        
        if (target_sym->data.class_data.enclosing_class &&
            !(target_sym->modifiers & MOD_STATIC)) {
            /* This is an inner/anonymous class - need to pass enclosing instance */
            is_inner_class = true;
            symbol_t *enclosing = target_sym->data.class_data.enclosing_class;
            outer_internal = class_to_internal_name(enclosing->qualified_name);
            
            /* For now, assume the enclosing class is either:
             * 1. The current class (load 'this')
             * 2. An ancestor of the current class (load this$0 chain)
             * Simple case: enclosing is current class */
            if (mg->class_gen && mg->class_gen->class_sym) {
                if (mg->class_gen->class_sym == enclosing) {
                    /* Creating inner class of our own class - use 'this' */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push_null(mg);  /* Object reference */
                } else if (mg->class_gen->is_inner_class &&
                           mg->class_gen->class_sym->data.class_data.enclosing_class == enclosing) {
                    /* Creating inner class of our outer class - use this$0 */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push_null(mg);  /* Object reference */
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, mg->class_gen->this_dollar_zero_ref);
                    /* Stack unchanged: popped this, pushed outer */
                } else {
                    /* Fallback: use 'this' - may be wrong for complex cases */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push_null(mg);  /* Object reference */
                }
            }
        }
    }
    
    /* For local classes, push captured variable values */
    int captured_count = 0;
    if (is_local_class && captured_vars) {
        for (slist_t *cap = captured_vars; cap; cap = cap->next) {
            symbol_t *var_sym = (symbol_t *)cap->data;
            if (!var_sym || !var_sym->name) continue;
            
            /* Load the captured variable from the enclosing method's scope */
            /* It should be a local variable in our current method context */
            local_var_info_t *cap_info = (local_var_info_t *)hashtable_lookup(mg->locals, var_sym->name);
            if (cap_info) {
                uint16_t slot = cap_info->slot;
                type_kind_t kind = TYPE_CLASS;
                if (var_sym->type) {
                    kind = var_sym->type->kind;
                }
                
                /* Emit appropriate load instruction */
                if (kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_LLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_long(mg);
                } else if (kind == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_DLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_double(mg);
                } else if (kind == TYPE_FLOAT) {
                    bc_emit(mg->code, OP_FLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_float(mg);
                } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY) {
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_null(mg);  /* Object reference */
                } else {
                    /* Integer types */
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_int(mg);
                }
                captured_count++;
            }
        }
    }
    
    /* Generate constructor arguments */
    /* Skip AST_BLOCK if this is an anonymous class (the block is the class body, not an argument) */
    int arg_count = 0;
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        
        /* Skip anonymous class body block */
        if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) {
            break;
        }
        
        if (!codegen_expr(mg, arg, cp)) {
            free(internal_name);
            if (outer_internal) free(outer_internal);
            return false;
        }
        arg_count++;
    }
    
    /* Build constructor descriptor */
    string_t *desc = string_new("(");
    
    /* For inner classes, add outer instance as first parameter */
    if (is_inner_class && outer_internal) {
        string_append(desc, "L");
        string_append(desc, outer_internal);
        string_append(desc, ";");
        arg_count++;  /* Count the implicit outer argument */
    }
    
    /* For local classes, add captured variable types */
    if (is_local_class && captured_vars) {
        for (slist_t *cap = captured_vars; cap; cap = cap->next) {
            symbol_t *var_sym = (symbol_t *)cap->data;
            if (var_sym && var_sym->type) {
                char *cap_desc = type_to_descriptor(var_sym->type);
                string_append(desc, cap_desc);
                free(cap_desc);
            }
        }
        arg_count += captured_count;  /* Count captured variables */
    }
    
    /* Add explicit argument types */
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        
        /* Skip anonymous class body block */
        if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) {
            break;
        }
        
        const char *arg_desc = infer_arg_descriptor(arg);
        string_append(desc, arg_desc);
    }
    
    string_append(desc, ")V");
    char *descriptor = string_free(desc, false);
    
    if (outer_internal) free(outer_internal);
    
    /* Emit: invokespecial <init> */
    uint16_t init_ref = cp_add_methodref(cp, internal_name, "<init>", descriptor);
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    
    free(descriptor);
    free(internal_name);
    
    /* invokespecial consumes object ref and args, leaves nothing */
    /* But we have a dup'd reference still on stack */
    /* Calculate stack slots consumed (long/double take 2 slots) */
    int arg_slots = 0;
    if (is_inner_class) arg_slots++;  /* Outer instance */
    if (is_local_class && captured_vars) {
        for (slist_t *cap = captured_vars; cap; cap = cap->next) {
            symbol_t *var_sym = (symbol_t *)cap->data;
            if (var_sym && var_sym->type) {
                type_kind_t kind = var_sym->type->kind;
                arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
            } else {
                arg_slots++;
            }
        }
    }
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) break;
        type_kind_t kind = get_expr_type_kind(mg, arg);
        arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
    }
    mg_pop_typed(mg, arg_slots + 1);  /* +1 for object reference */
    
    return true;
}

/* ========================================================================
 * Array Creation Code Generation
 * ======================================================================== */

/**
 * Get the array store opcode for the given element type.
 */
static uint8_t get_array_store_opcode(type_kind_t kind)
{
    switch (kind) {
        case TYPE_BOOLEAN:
        case TYPE_BYTE:
            return OP_BASTORE;
        case TYPE_CHAR:
            return OP_CASTORE;
        case TYPE_SHORT:
            return OP_SASTORE;
        case TYPE_INT:
            return OP_IASTORE;
        case TYPE_LONG:
            return OP_LASTORE;
        case TYPE_FLOAT:
            return OP_FASTORE;
        case TYPE_DOUBLE:
            return OP_DASTORE;
        case TYPE_CLASS:
        case TYPE_ARRAY:
        default:
            return OP_AASTORE;
    }
}

/**
 * Generate code for a standalone array initializer: {1, 2, 3} or {{1,2}, {3,4}}
 * Uses sem_type to determine the target array type.
 */
static bool codegen_array_init(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_ARRAY_INIT) {
        return false;
    }
    
    /* Get the target type from sem_type */
    type_t *arr_type = expr->sem_type;
    if (!arr_type || arr_type->kind != TYPE_ARRAY) {
        fprintf(stderr, "codegen: array initializer without target type\n");
        return false;
    }
    
    /* Count elements */
    int elem_count = 0;
    for (slist_t *node = expr->data.node.children; node; node = node->next) {
        elem_count++;
    }
    
    /* Get element type info */
    type_t *elem_type = arr_type->data.array_type.element_type;
    int arr_dims = arr_type->data.array_type.dimensions;
    type_kind_t elem_kind = elem_type ? elem_type->kind : TYPE_INT;
    
    /* Push array size */
    if (elem_count <= 5) {
        bc_emit(mg->code, OP_ICONST_0 + elem_count);
    } else if (elem_count <= 127) {
        bc_emit(mg->code, OP_BIPUSH);
        bc_emit_u1(mg->code, (uint8_t)elem_count);
    } else {
        bc_emit(mg->code, OP_SIPUSH);
        bc_emit_u2(mg->code, (uint16_t)elem_count);
    }
    mg_push_int(mg);  /* Array size is an integer */
    
    /* Create the array - type depends on dimensions and element type */
    if (arr_dims > 1) {
        /* Multi-dimensional: elements are arrays, use anewarray with inner array type */
        /* Build descriptor for the inner array type (one less dimension) */
        char *elem_desc;
        if (elem_kind == TYPE_BOOLEAN) elem_desc = strdup("Z");
        else if (elem_kind == TYPE_BYTE) elem_desc = strdup("B");
        else if (elem_kind == TYPE_CHAR) elem_desc = strdup("C");
        else if (elem_kind == TYPE_SHORT) elem_desc = strdup("S");
        else if (elem_kind == TYPE_INT) elem_desc = strdup("I");
        else if (elem_kind == TYPE_LONG) elem_desc = strdup("J");
        else if (elem_kind == TYPE_FLOAT) elem_desc = strdup("F");
        else if (elem_kind == TYPE_DOUBLE) elem_desc = strdup("D");
        else if (elem_kind == TYPE_CLASS && elem_type && elem_type->data.class_type.name) {
            char *internal = class_to_internal_name(elem_type->data.class_type.name);
            size_t len = strlen(internal) + 3;
            elem_desc = malloc(len);
            snprintf(elem_desc, len, "L%s;", internal);
            free(internal);
        } else {
            elem_desc = strdup("Ljava/lang/Object;");
        }
        
        /* Inner array descriptor: [[[...elem_desc (dims-1 brackets) */
        size_t desc_len = strlen(elem_desc) + arr_dims;  /* arr_dims - 1 brackets */
        char *inner_desc = malloc(desc_len);
        memset(inner_desc, '[', arr_dims - 1);
        strcpy(inner_desc + arr_dims - 1, elem_desc);
        free(elem_desc);
        
        uint16_t class_ref = cp_add_class(cp, inner_desc);
        free(inner_desc);
        
        bc_emit(mg->code, OP_ANEWARRAY);
        bc_emit_u2(mg->code, class_ref);
    } else if (elem_kind == TYPE_CLASS) {
        /* Single dimension reference array */
        const char *class_name = elem_type && elem_type->data.class_type.name 
            ? elem_type->data.class_type.name : "java/lang/Object";
        char *internal_name = class_to_internal_name(class_name);
        uint16_t class_ref = cp_add_class(cp, internal_name);
        free(internal_name);
        
        bc_emit(mg->code, OP_ANEWARRAY);
        bc_emit_u2(mg->code, class_ref);
    } else {
        /* Single dimension primitive array */
        const char *prim_name = NULL;
        switch (elem_kind) {
            case TYPE_BOOLEAN: prim_name = "boolean"; break;
            case TYPE_BYTE: prim_name = "byte"; break;
            case TYPE_CHAR: prim_name = "char"; break;
            case TYPE_SHORT: prim_name = "short"; break;
            case TYPE_INT: prim_name = "int"; break;
            case TYPE_LONG: prim_name = "long"; break;
            case TYPE_FLOAT: prim_name = "float"; break;
            case TYPE_DOUBLE: prim_name = "double"; break;
            default: prim_name = "int"; break;
        }
        int atype = type_name_to_atype(prim_name);
        if (atype < 0) {
            fprintf(stderr, "codegen: unknown primitive type for array\n");
            return false;
        }
        bc_emit(mg->code, OP_NEWARRAY);
        bc_emit_u1(mg->code, (uint8_t)atype);
    }
    /* Stack: size -> arrayref (no net change) */
    
    /* Populate the array with initializer values */
    int index = 0;
    for (slist_t *node = expr->data.node.children; node; node = node->next) {
        ast_node_t *elem_expr = (ast_node_t *)node->data;
        
        /* Duplicate array reference */
        bc_emit(mg->code, OP_DUP);
        mg_push_null(mg);  /* Duplicated array reference */
        
        /* Push index */
        if (index <= 5) {
            bc_emit(mg->code, OP_ICONST_0 + index);
        } else if (index <= 127) {
            bc_emit(mg->code, OP_BIPUSH);
            bc_emit_u1(mg->code, (uint8_t)index);
        } else {
            bc_emit(mg->code, OP_SIPUSH);
            bc_emit_u2(mg->code, (uint16_t)index);
        }
        mg_push_int(mg);  /* Array index is an integer */
        
        /* Generate element value */
        if (elem_expr->type == AST_ARRAY_INIT) {
            /* Nested array initializer - set its sem_type */
            if (arr_dims > 1 && elem_type) {
                elem_expr->sem_type = type_new_array(elem_type, arr_dims - 1);
            }
            if (!codegen_array_init(mg, elem_expr, cp)) {
                return false;
            }
        } else {
            if (!codegen_expr(mg, elem_expr, cp)) {
                return false;
            }
        }
        
        /* Store element */
        uint8_t store_op;
        if (arr_dims > 1) {
            store_op = OP_AASTORE;  /* Sub-arrays are references */
        } else {
            store_op = get_array_store_opcode(elem_kind);
        }
        bc_emit(mg->code, store_op);
        
        /* Stack: arrayref, index, value -> (net effect: -3) */
        mg_pop_typed(mg, 3);
        if (elem_kind == TYPE_LONG || elem_kind == TYPE_DOUBLE) {
            mg_pop_typed(mg, 1);  /* Wide types */
        }
        
        index++;
    }
    
    /* Array reference is still on stack */
    return true;
}

/**
 * Generate code for array creation: new Type[size] or new Type[]{...}
 * JVM opcodes:
 *   - newarray <atype> for primitive arrays
 *   - anewarray <class> for reference arrays
 *   - multianewarray <class> <dims> for multi-dimensional
 */
static bool codegen_new_array(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_NEW_ARRAY) {
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    if (!children) {
        fprintf(stderr, "codegen: new array without type\n");
        return false;
    }
    
    /* First child is the element type */
    ast_node_t *type_node = (ast_node_t *)children->data;
    
    /* Check for array initializer and count dimension expressions */
    ast_node_t *array_init = NULL;
    int dim_count = 0;
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *child = (ast_node_t *)node->data;
        if (child->type == AST_ARRAY_INIT) {
            array_init = child;
        } else {
            dim_count++;
        }
    }
    
    /* Handle array initializer: new int[]{1, 2, 3} */
    if (array_init && dim_count == 0) {
        /* Count elements in initializer */
        int elem_count = 0;
        for (slist_t *node = array_init->data.node.children; node; node = node->next) {
            elem_count++;
        }
        
        /* Determine element type kind for store opcode */
        type_kind_t elem_kind = TYPE_INT;
        bool is_primitive = (type_node->type == AST_PRIMITIVE_TYPE);
        if (is_primitive) {
            const char *prim_name = type_node->data.leaf.name;
            if (strcmp(prim_name, "boolean") == 0) {
                elem_kind = TYPE_BOOLEAN;
            } else if (strcmp(prim_name, "byte") == 0) {
                elem_kind = TYPE_BYTE;
            } else if (strcmp(prim_name, "char") == 0) {
                elem_kind = TYPE_CHAR;
            } else if (strcmp(prim_name, "short") == 0) {
                elem_kind = TYPE_SHORT;
            } else if (strcmp(prim_name, "int") == 0) {
                elem_kind = TYPE_INT;
            } else if (strcmp(prim_name, "long") == 0) {
                elem_kind = TYPE_LONG;
            } else if (strcmp(prim_name, "float") == 0) {
                elem_kind = TYPE_FLOAT;
            } else if (strcmp(prim_name, "double") == 0) {
                elem_kind = TYPE_DOUBLE;
            }
        } else {
            elem_kind = TYPE_CLASS;
        }
        
        /* Push array size */
        if (elem_count <= 5) {
            bc_emit(mg->code, OP_ICONST_0 + elem_count);
        } else if (elem_count <= 127) {
            bc_emit(mg->code, OP_BIPUSH);
            bc_emit_u1(mg->code, (uint8_t)elem_count);
        } else {
            bc_emit(mg->code, OP_SIPUSH);
            bc_emit_u2(mg->code, (uint16_t)elem_count);
        }
        mg_push_int(mg);  /* Array size is an integer */
        
        /* Create the array */
        if (is_primitive) {
            const char *prim_name = type_node->data.leaf.name;
            int atype = type_name_to_atype(prim_name);
            if (atype < 0) {
                fprintf(stderr, "codegen: unknown primitive type for array: %s\n", prim_name);
                return false;
            }
            bc_emit(mg->code, OP_NEWARRAY);
            bc_emit_u1(mg->code, (uint8_t)atype);
        } else {
            const char *class_name = type_node->data.node.name;
            if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                class_name = type_node->sem_type->data.class_type.name;
            }
            char *internal_name = class_to_internal_name(class_name);
            uint16_t class_ref = cp_add_class(cp, internal_name);
            free(internal_name);
            bc_emit(mg->code, OP_ANEWARRAY);
            bc_emit_u2(mg->code, class_ref);
        }
        /* Stack: size -> arrayref (no net change) */
        
        /* Now populate the array with initializer values */
        /* For each element: dup arrayref, push index, push value, store */
        int index = 0;
        for (slist_t *node = array_init->data.node.children; node; node = node->next) {
            ast_node_t *elem_expr = (ast_node_t *)node->data;
            
            /* Duplicate array reference */
            bc_emit(mg->code, OP_DUP);
            mg_push_null(mg);  /* Duplicated array reference */
            
            /* Push index */
            if (index <= 5) {
                bc_emit(mg->code, OP_ICONST_0 + index);
            } else if (index <= 127) {
                bc_emit(mg->code, OP_BIPUSH);
                bc_emit_u1(mg->code, (uint8_t)index);
            } else {
                bc_emit(mg->code, OP_SIPUSH);
                bc_emit_u2(mg->code, (uint16_t)index);
            }
            mg_push_int(mg);  /* Array index is an integer */
            
            /* Generate element value */
            if (!codegen_expr(mg, elem_expr, cp)) {
                return false;
            }
            
            /* Store to array */
            switch (elem_kind) {
                case TYPE_BOOLEAN:
                case TYPE_BYTE:
                    bc_emit(mg->code, OP_BASTORE);
                    break;
                case TYPE_CHAR:
                    bc_emit(mg->code, OP_CASTORE);
                    break;
                case TYPE_SHORT:
                    bc_emit(mg->code, OP_SASTORE);
                    break;
                case TYPE_INT:
                    bc_emit(mg->code, OP_IASTORE);
                    break;
                case TYPE_LONG:
                    bc_emit(mg->code, OP_LASTORE);
                    break;
                case TYPE_FLOAT:
                    bc_emit(mg->code, OP_FASTORE);
                    break;
                case TYPE_DOUBLE:
                    bc_emit(mg->code, OP_DASTORE);
                    break;
                case TYPE_CLASS:
                case TYPE_ARRAY:
                    bc_emit(mg->code, OP_AASTORE);
                    break;
                default:
                    bc_emit(mg->code, OP_IASTORE);
                    break;
            }
            /* Store consumes arrayref, index, value */
            mg_pop_typed(mg, 3);
            
            index++;
        }
        
        /* Array reference is left on stack */
        return true;
    }
    
    if (dim_count == 0) {
        fprintf(stderr, "codegen: new array without dimensions or initializer\n");
        return false;
    }
    
    /* Generate dimension expressions */
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *dim_expr = (ast_node_t *)node->data;
        if (dim_expr->type != AST_ARRAY_INIT) {
            if (!codegen_expr(mg, dim_expr, cp)) {
                return false;
            }
        }
    }
    
    if (dim_count == 1) {
        /* Single-dimensional array */
        if (type_node->type == AST_PRIMITIVE_TYPE) {
            /* Primitive array: use newarray */
            const char *prim_name = type_node->data.leaf.name;
            int atype = type_name_to_atype(prim_name);
            if (atype < 0) {
                fprintf(stderr, "codegen: unknown primitive type for array: %s\n", prim_name);
                return false;
            }
            
            bc_emit(mg->code, OP_NEWARRAY);
            bc_emit_u1(mg->code, (uint8_t)atype);
            /* Stack: size -> arrayref (no net change) */
        } else if (type_node->type == AST_CLASS_TYPE) {
            /* Reference array: use anewarray */
            const char *class_name = type_node->data.node.name;
            
            /* Use resolved type if available */
            if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                class_name = type_node->sem_type->data.class_type.name;
            }
            
            char *internal_name = class_to_internal_name(class_name);
            uint16_t class_ref = cp_add_class(cp, internal_name);
            free(internal_name);
            
            bc_emit(mg->code, OP_ANEWARRAY);
            bc_emit_u2(mg->code, class_ref);
            /* Stack: size -> arrayref (no net change) */
        } else {
            fprintf(stderr, "codegen: unsupported array element type\n");
            return false;
        }
    } else {
        /* Multi-dimensional array: use multianewarray */
        /* Build the array type descriptor */
        char *elem_desc;
        if (type_node->type == AST_PRIMITIVE_TYPE) {
            elem_desc = ast_type_to_descriptor(type_node);
        } else {
            elem_desc = ast_type_to_descriptor(type_node);
        }
        
        /* Create array type: [[...elem_desc */
        size_t desc_len = strlen(elem_desc) + dim_count + 1;
        char *array_desc = malloc(desc_len);
        memset(array_desc, '[', dim_count);
        strcpy(array_desc + dim_count, elem_desc);
        free(elem_desc);
        
        /* For multianewarray, we need a class reference to the array type */
        /* Remove the 'L' prefix and ';' suffix if present for class lookup */
        char *class_name;
        if (array_desc[dim_count] == 'L') {
            /* Reference type array - use full descriptor */
            class_name = strdup(array_desc);
        } else {
            /* Primitive type array - use full descriptor */
            class_name = strdup(array_desc);
        }
        
        uint16_t array_class = cp_add_class(cp, class_name);
        free(array_desc);
        free(class_name);
        
        bc_emit(mg->code, OP_MULTIANEWARRAY);
        bc_emit_u2(mg->code, array_class);
        bc_emit_u1(mg->code, (uint8_t)dim_count);
        
        /* Stack: dim1, dim2, ... dimN -> arrayref */
        mg_pop_typed(mg, dim_count - 1);  /* Consumes N dims, pushes 1 ref */
    }
    
    return true;
}

/* ========================================================================
 * Assignment Code Generation
 * ======================================================================== */

/**
 * Emit the opcode for a compound assignment operation based on the operator token.
 * Returns true if the opcode was emitted, false if unknown operator.
 */
static bool emit_compound_op(method_gen_t *mg, token_type_t op)
{
    switch (op) {
        case TOK_PLUS_ASSIGN:   bc_emit(mg->code, OP_IADD); break;
        case TOK_MINUS_ASSIGN:  bc_emit(mg->code, OP_ISUB); break;
        case TOK_STAR_ASSIGN:   bc_emit(mg->code, OP_IMUL); break;
        case TOK_SLASH_ASSIGN:  bc_emit(mg->code, OP_IDIV); break;
        case TOK_MOD_ASSIGN:    bc_emit(mg->code, OP_IREM); break;
        case TOK_AND_ASSIGN:    bc_emit(mg->code, OP_IAND); break;
        case TOK_OR_ASSIGN:     bc_emit(mg->code, OP_IOR);  break;
        case TOK_XOR_ASSIGN:    bc_emit(mg->code, OP_IXOR); break;
        case TOK_LSHIFT_ASSIGN: bc_emit(mg->code, OP_ISHL); break;
        case TOK_RSHIFT_ASSIGN: bc_emit(mg->code, OP_ISHR); break;
        case TOK_URSHIFT_ASSIGN: bc_emit(mg->code, OP_IUSHR); break;
        default: return false;
    }
    return true;
}

static bool codegen_assignment(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr || expr->type != AST_ASSIGNMENT_EXPR) {
        return false;
    }
    
    slist_t *children = expr->data.node.children;
    if (!children || !children->next) {
        return false;
    }
    
    ast_node_t *target = (ast_node_t *)children->data;
    ast_node_t *value = (ast_node_t *)children->next->data;
    token_type_t op = expr->data.node.op_token;
    
    /* For compound assignments (+=, -=, etc.), we need to load the old value first */
    bool compound = (op != TOK_ASSIGN);
    
    if (target->type == AST_IDENTIFIER) {
        const char *name = target->data.leaf.name;
        
        /* Check if it's a local variable */
        local_var_info_t *local_info = (local_var_info_t *)hashtable_lookup(mg->locals, name);
        
        if (local_info) {
            /* Local variable assignment */
            uint16_t slot = local_info->slot;
            type_kind_t kind = local_info->kind;
            
            if (compound) {
                /* Load current value */
                mg_emit_load_local(mg, slot, kind);
            }
            
            /* Generate right-hand side */
            if (!codegen_expr(mg, value, cp)) {
                return false;
            }
            
            if (compound) {
                /* Apply compound operation */
                emit_compound_op(mg, op);
                mg_pop_typed(mg, 1);  /* Operation consumes one operand */
            }
            
            /* Autoboxing/unboxing for simple assignment (not compound) */
            if (!compound && target->sem_type && value->sem_type) {
                /* Check if boxing needed (primitive -> wrapper) */
                if (type_needs_boxing(target->sem_type, value->sem_type)) {
                    emit_boxing(mg, cp, value->sem_type->kind);
                }
                /* Check if unboxing needed (wrapper -> primitive) */
                else if (type_needs_unboxing(target->sem_type, value->sem_type)) {
                    char *internal = class_to_internal_name(value->sem_type->data.class_type.name);
                    emit_unboxing(mg, cp, target->sem_type->kind, internal);
                    free(internal);
                }
            }
            
            /* Store to local variable */
            mg_emit_store_local(mg, slot, kind);
            
            /* Update stackmap for this local variable slot */
            if (mg->stackmap) {
                switch (kind) {
                    case TYPE_LONG:   stackmap_set_local_long(mg->stackmap, slot); break;
                    case TYPE_DOUBLE: stackmap_set_local_double(mg->stackmap, slot); break;
                    case TYPE_FLOAT:  stackmap_set_local_float(mg->stackmap, slot); break;
                    case TYPE_CLASS:
                    case TYPE_ARRAY:
                        if (local_info->class_name) {
                            stackmap_set_local_object(mg->stackmap, slot, mg->cp, local_info->class_name);
                        }
                        break;
                    default:          stackmap_set_local_int(mg->stackmap, slot); break;
                }
            }
            return true;
        }
        
        /* Check if it's a field */
        if (mg->class_gen) {
            field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
            if (field) {
                /* Check if it's a static field */
                if (field->access_flags & ACC_STATIC) {
                    /* Static field assignment */
                    if (compound) {
                        /* Load current value first */
                        uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                             field->name, field->descriptor);
                        bc_emit(mg->code, OP_GETSTATIC);
                        bc_emit_u2(mg->code, fieldref);
                        /* Push with proper type tracking */
                        switch (field->descriptor[0]) {
                            case 'J': mg_push_long(mg); break;
                            case 'D': mg_push_double(mg); break;
                            case 'F': mg_push_float(mg); break;
                            case 'L': 
                            case '[': mg_push_object_from_descriptor(mg, field->descriptor); break;
                            default:  mg_push_int(mg); break;
                        }
                    }
                    
                    /* Generate right-hand side */
                    if (!codegen_expr(mg, value, cp)) {
                        return false;
                    }
                    
                    if (compound) {
                        /* Apply compound operation */
                        emit_compound_op(mg, op);
                        mg_pop_typed(mg, 1);
                    }
                    
                    /* Store to static field */
                    uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                         field->name, field->descriptor);
                    bc_emit(mg->code, OP_PUTSTATIC);
                    bc_emit_u2(mg->code, fieldref);
                    mg_pop_typed(mg, 1);
                    return true;
                } else if (!mg->is_static) {
                    /* Instance field assignment: this.field = value */
                    
                    /* Load 'this' */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push_null(mg);  /* Object reference */
                    
                    if (compound) {
                        /* Duplicate 'this' for getfield, then load current value */
                        bc_emit(mg->code, OP_DUP);
                        mg_push(mg, 1);
                        
                        uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                             field->name, field->descriptor);
                        bc_emit(mg->code, OP_GETFIELD);
                        bc_emit_u2(mg->code, fieldref);
                        /* getfield pops ref, pushes value - net 0 */
                    }
                    
                    /* Generate right-hand side */
                    if (!codegen_expr(mg, value, cp)) {
                        return false;
                    }
                    
                    if (compound) {
                        /* Apply compound operation */
                        emit_compound_op(mg, op);
                        mg_pop_typed(mg, 1);  /* Operation consumes one operand */
                    }
                    
                    /* Store to field: putfield pops object ref and value */
                    uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                         field->name, field->descriptor);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, fieldref);
                    /* putfield consumes ref (1) + value (1 or 2 slots) */
                    if (field->descriptor[0] == 'J' || field->descriptor[0] == 'D') {
                        mg_pop_typed(mg, 3);  /* ref=1 + long/double=2 */
                    } else {
                        mg_pop_typed(mg, 2);  /* ref=1 + other=1 */
                    }
                    return true;
                }
                /* Instance field in static context - error handled below */
            }
        }
        
        fprintf(stderr, "codegen: cannot resolve identifier for assignment: %s\n", name);
        return false;
    }
    
    /* Handle field access assignment (obj.field = value) */
    if (target->type == AST_FIELD_ACCESS) {
        const char *field_name = target->data.node.name;
        slist_t *target_children = target->data.node.children;
        
        if (!target_children || !field_name) {
            fprintf(stderr, "codegen: malformed field access assignment\n");
            return false;
        }
        
        ast_node_t *receiver = (ast_node_t *)target_children->data;
        
        /* Check if this is a static field assignment (receiver is a class name) */
        if (receiver->type == AST_IDENTIFIER) {
            const char *recv_name = receiver->data.leaf.name;
            const char *class_name = resolve_class_name(mg, recv_name);
            
            if (class_name) {
                /* Static field assignment */
                const char *field_desc = get_known_static_field_descriptor(class_name, field_name);
                
                if (!field_desc && mg->class_gen && strcmp(class_name, mg->class_gen->internal_name) == 0) {
                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
                    if (field) {
                        field_desc = field->descriptor;
                    }
                }
                
                if (!field_desc) {
                    fprintf(stderr, "codegen: cannot resolve static field for assignment: %s.%s\n", 
                            recv_name, field_name);
                    return false;
                }
                
                /* Generate value */
                if (!codegen_expr(mg, value, cp)) {
                    return false;
                }
                
                /* Emit putstatic */
                uint16_t fieldref = cp_add_fieldref(cp, class_name, field_name, field_desc);
                bc_emit(mg->code, OP_PUTSTATIC);
                bc_emit_u2(mg->code, fieldref);
                mg_pop_typed(mg, 1);
                
                return true;
            }
        }
        
        /* Instance field assignment */
        /* Generate receiver */
        if (!codegen_expr(mg, receiver, cp)) {
            return false;
        }
        
        /* Generate value */
        if (!codegen_expr(mg, value, cp)) {
            return false;
        }
        
        /* Determine field class and descriptor */
        const char *recv_class = "java/lang/Object";
        const char *field_desc = "I";  /* Default to int */
        
        /* Try to get receiver class from semantic info */
        if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
            if (receiver->sem_type->data.class_type.name) {
                recv_class = class_to_internal_name(receiver->sem_type->data.class_type.name);
            }
            
            /* Look up field descriptor from receiver's class symbol */
            symbol_t *class_sym = receiver->sem_type->data.class_type.symbol;
            if (class_sym && class_sym->data.class_data.members) {
                symbol_t *field_sym = scope_lookup_local(class_sym->data.class_data.members, field_name);
                if (field_sym && field_sym->type) {
                    char *desc = type_to_descriptor(field_sym->type);
                    if (desc) {
                        field_desc = desc;
                    }
                }
            }
        }
        
        /* For this.field, use current class */
        if (receiver->type == AST_THIS_EXPR && mg->class_gen) {
            recv_class = mg->class_gen->internal_name;
            field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
            if (field) {
                field_desc = field->descriptor;
            }
        }
        
        /* Emit putfield */
        uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
        bc_emit(mg->code, OP_PUTFIELD);
        bc_emit_u2(mg->code, fieldref);
        /* putfield consumes ref (1) + value (1 or 2 slots) */
        if (field_desc && (field_desc[0] == 'J' || field_desc[0] == 'D')) {
            mg_pop_typed(mg, 3);  /* ref=1 + long/double=2 */
        } else {
            mg_pop_typed(mg, 2);  /* ref=1 + other=1 */
        }
        
        return true;
    }
    
    /* Handle array element assignment: arr[index] = value */
    if (target->type == AST_ARRAY_ACCESS) {
        slist_t *target_children = target->data.node.children;
        if (!target_children || !target_children->next) {
            fprintf(stderr, "codegen: malformed array access assignment\n");
            return false;
        }
        
        ast_node_t *array_expr = (ast_node_t *)target_children->data;
        ast_node_t *index_expr = (ast_node_t *)target_children->next->data;
        
        /* Generate array reference */
        if (!codegen_expr(mg, array_expr, cp)) {
            return false;
        }
        
        /* Generate index */
        if (!codegen_expr(mg, index_expr, cp)) {
            return false;
        }
        
        /* For compound assignment, we need to load the current value first */
        if (compound) {
            /* Stack: arrayref, index */
            /* Need: arrayref, index, arrayref, index for load, then value for store */
            bc_emit(mg->code, OP_DUP2);  /* Duplicate arrayref and index */
            /* Push the duplicated types: arrayref (null) and index (int) */
            mg_push_null(mg);
            mg_push_int(mg);
            
            /* Load current value */
            type_kind_t elem_kind = TYPE_INT;
            if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    elem_kind = elem_type->kind;
                }
            } else if (array_expr->type == AST_IDENTIFIER) {
                const char *arr_name = array_expr->data.leaf.name;
                if (mg_local_is_array(mg, arr_name)) {
                    elem_kind = mg_local_array_elem_kind(mg, arr_name);
                }
            }
            
            switch (elem_kind) {
                case TYPE_BOOLEAN:
                case TYPE_BYTE:
                    bc_emit(mg->code, OP_BALOAD);
                    break;
                case TYPE_CHAR:
                    bc_emit(mg->code, OP_CALOAD);
                    break;
                case TYPE_SHORT:
                    bc_emit(mg->code, OP_SALOAD);
                    break;
                case TYPE_INT:
                    bc_emit(mg->code, OP_IALOAD);
                    break;
                case TYPE_LONG:
                    bc_emit(mg->code, OP_LALOAD);
                    mg_push(mg, 1);
                    break;
                case TYPE_FLOAT:
                    bc_emit(mg->code, OP_FALOAD);
                    break;
                case TYPE_DOUBLE:
                    bc_emit(mg->code, OP_DALOAD);
                    mg_push(mg, 1);
                    break;
                case TYPE_CLASS:
                case TYPE_ARRAY:
                    bc_emit(mg->code, OP_AALOAD);
                    break;
                default:
                    bc_emit(mg->code, OP_IALOAD);
                    break;
            }
            mg_pop_typed(mg, 1);  /* Load consumed arrayref, index; pushed value */
        }
        
        /* Generate value expression */
        if (!codegen_expr(mg, value, cp)) {
            return false;
        }
        
        if (compound) {
            /* Apply compound operation */
            emit_compound_op(mg, op);
            mg_pop_typed(mg, 1);  /* Operation consumes one operand */
        }
        
        /* Determine element type and emit appropriate store opcode */
        type_kind_t store_elem_kind = TYPE_INT;
        if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
            type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
            if (elem_type) {
                store_elem_kind = elem_type->kind;
            }
        } else if (array_expr->type == AST_IDENTIFIER) {
            const char *arr_name = array_expr->data.leaf.name;
            if (mg_local_is_array(mg, arr_name)) {
                store_elem_kind = mg_local_array_elem_kind(mg, arr_name);
            }
        }
        
        switch (store_elem_kind) {
            case TYPE_BOOLEAN:
            case TYPE_BYTE:
                bc_emit(mg->code, OP_BASTORE);
                break;
            case TYPE_CHAR:
                bc_emit(mg->code, OP_CASTORE);
                break;
            case TYPE_SHORT:
                bc_emit(mg->code, OP_SASTORE);
                break;
            case TYPE_INT:
                bc_emit(mg->code, OP_IASTORE);
                break;
            case TYPE_LONG:
                bc_emit(mg->code, OP_LASTORE);
                mg_pop_typed(mg, 1);  /* Long takes 2 slots */
                break;
            case TYPE_FLOAT:
                bc_emit(mg->code, OP_FASTORE);
                break;
            case TYPE_DOUBLE:
                bc_emit(mg->code, OP_DASTORE);
                mg_pop_typed(mg, 1);  /* Double takes 2 slots */
                break;
            case TYPE_CLASS:
            case TYPE_ARRAY:
                bc_emit(mg->code, OP_AASTORE);
                break;
            default:
                bc_emit(mg->code, OP_IASTORE);
                break;
        }
        
        /* Stack: arrayref, index, value -> (empty) */
        mg_pop_typed(mg, 3);
        return true;
    }
    
    fprintf(stderr, "codegen: unsupported assignment target: %s\n",
            ast_type_name(target->type));
    return false;
}

/* ========================================================================
 * Pattern Matching Switch Expression (Java 21+)
 * ======================================================================== */

/**
 * Generate code for switch expression with type patterns.
 * Uses instanceof checks instead of lookupswitch.
 */
static bool codegen_pattern_switch_expr(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    slist_t *children = expr->data.node.children;
    
    if (!children) {
        return false;
    }
    
    /* Generate selector and store in local variable */
    ast_node_t *selector = (ast_node_t *)children->data;
    if (!codegen_expr(mg, selector, cp)) {
        return false;
    }
    
    /* Save slot counter and stackmap state before pattern switch.
     * Pattern variables are local to each case body and should not
     * persist to subsequent cases or after the switch. */
    uint16_t switch_saved_slot = mg->next_slot;
    uint16_t switch_saved_locals = 0;
    if (mg->stackmap) {
        switch_saved_locals = mg_save_locals_count(mg);
    }
    
    /* Allocate local for selector */
    uint16_t selector_slot = mg->next_slot++;
    if (mg->next_slot > mg->max_locals) {
        mg->max_locals = mg->next_slot;
    }
    bc_emit(mg->code, OP_ASTORE);
    bc_emit_u1(mg->code, (uint8_t)selector_slot);
    mg_pop_typed(mg, 1);
    
    /* Update stackmap to track the selector local */
    if (mg->stackmap) {
        stackmap_set_local_object(mg->stackmap, selector_slot, mg->cp, "java/lang/Object");
    }
    
    /* Track goto positions to patch at the end */
    slist_t *end_gotos = NULL;
    ast_node_t *default_rule = NULL;
    
    /* Process each rule */
    for (slist_t *node = children->next; node; node = node->next) {
        ast_node_t *rule = (ast_node_t *)node->data;
        if (rule->type != AST_SWITCH_RULE) continue;
        
        /* Check for default */
        if (rule->data.node.name && strcmp(rule->data.node.name, "default") == 0) {
            default_rule = rule;
            continue;
        }
        
        /* Get pattern and body from rule */
        slist_t *rule_children = rule->data.node.children;
        ast_node_t *pattern = NULL;
        ast_node_t *guard = NULL;
        ast_node_t *body = NULL;
        
        for (slist_t *rc = rule_children; rc; rc = rc->next) {
            ast_node_t *child = (ast_node_t *)rc->data;
            if (child->type == AST_TYPE_PATTERN) {
                pattern = child;
            } else if (child->type == AST_RECORD_PATTERN) {
                pattern = child;
            } else if (child->type == AST_UNNAMED_PATTERN) {
                /* Unnamed pattern: case _ -> matches anything
                 * This is essentially the default case in pattern switch.
                 * Mark it as default so we don't generate null fallback. */
                default_rule = rule;
                continue;  /* Will be generated after all other cases */
            } else if (child->type == AST_GUARDED_PATTERN) {
                slist_t *gp_children = child->data.node.children;
                if (gp_children) {
                    pattern = (ast_node_t *)gp_children->data;
                    if (gp_children->next) {
                        guard = (ast_node_t *)gp_children->next->data;
                    }
                }
            } else if (child->type == AST_LITERAL && 
                       child->data.leaf.token_type == TOK_NULL) {
                /* null pattern - check for null */
                bc_emit(mg->code, OP_ALOAD);
                bc_emit_u1(mg->code, (uint8_t)selector_slot);
                mg_push(mg, 1);
                
                /* if (selector == null) */
                size_t ifnonnull_pos = mg->code->length;
                bc_emit(mg->code, OP_IFNONNULL);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);
                
                /* Get the body - it's the last child */
                body = child;  /* Will be overwritten below */
                for (slist_t *bc = rule_children; bc; bc = bc->next) {
                    body = (ast_node_t *)bc->data;
                }
                
                /* Generate body */
                mg_record_frame(mg);
                if (body->type == AST_BLOCK) {
                    codegen_statement(mg, body);
                } else {
                    codegen_expr(mg, body, cp);
                }
                
                /* Jump to end */
                size_t *goto_pos = malloc(sizeof(size_t));
                *goto_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);
                if (!end_gotos) {
                    end_gotos = slist_new(goto_pos);
                } else {
                    slist_append(end_gotos, goto_pos);
                }
                
                /* Patch ifnonnull */
                uint16_t skip_offset = (uint16_t)(mg->code->length - ifnonnull_pos);
                bc_patch_u2(mg->code, ifnonnull_pos + 1, skip_offset);
                mg_record_frame(mg);
                
                continue;
            }
            body = child;  /* Last child is the body */
        }
        
        if (!pattern) continue;
        
        /* Get pattern type and variable */
        const char *var_name = pattern->data.node.name;
        slist_t *pattern_children = pattern->data.node.children;
        if (!pattern_children) continue;
        
        /* Get type internal name from pattern's semantic type (set by semantic pass) */
        char *type_internal = NULL;
        
        if (pattern->sem_type && pattern->sem_type->kind == TYPE_CLASS) {
            type_internal = class_to_internal_name(pattern->sem_type->data.class_type.name);
        } else {
            /* Fallback: try to get from type node */
            ast_node_t *type_node = (ast_node_t *)pattern_children->data;
            if (type_node->type == AST_CLASS_TYPE) {
                const char *type_name = type_node->data.node.name;
                if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                    type_name = type_node->sem_type->data.class_type.name;
                }
                type_internal = class_to_internal_name(type_name);
            }
        }
        
        if (!type_internal) continue;
        
        /* Save stackmap state before pattern binding - each case should
         * start with the same state (only selector in scope) */
        stackmap_state_t *case_entry_state = NULL;
        if (mg->stackmap) {
            case_entry_state = stackmap_save_state(mg->stackmap);
        }
        uint16_t case_saved_slot = mg->next_slot;
        
        /* instanceof check: if (selector instanceof Type) */
        bc_emit(mg->code, OP_ALOAD);
        bc_emit_u1(mg->code, (uint8_t)selector_slot);
        mg_push(mg, 1);
        
        uint16_t type_class = cp_add_class(cp, type_internal);
        bc_emit(mg->code, OP_INSTANCEOF);
        bc_emit_u2(mg->code, type_class);
        /* Stack: int (0 or 1) */
        
        size_t ifeq_pos = mg->code->length;
        bc_emit(mg->code, OP_IFEQ);  /* Jump if not instanceof */
        bc_emit_u2(mg->code, 0);  /* Placeholder */
        mg_pop_typed(mg, 1);
        
        /* Record frame for TRUE path (pattern matched, entering case body).
         * This is the target of the fall-through from the ifeq when instanceof returns true.
         * At this point, stack is empty and we're about to set up pattern variables. */
        mg_record_frame(mg);
        
        uint16_t var_slot = 0;
        
        if (pattern->type == AST_RECORD_PATTERN) {
            /* Record pattern: extract components using accessor methods */
            /* First, checkcast and store in a temporary */
            uint16_t record_slot = mg->next_slot++;
            if (mg->next_slot > mg->max_locals) {
                mg->max_locals = mg->next_slot;
            }
            
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)selector_slot);
            mg_push(mg, 1);
            
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, type_class);
            
            bc_emit(mg->code, OP_ASTORE);
            bc_emit_u1(mg->code, (uint8_t)record_slot);
            mg_pop_typed(mg, 1);
            
            /* Get record symbol to find component accessors */
            symbol_t *record_sym = NULL;
            if (pattern->sem_type && pattern->sem_type->kind == TYPE_CLASS) {
                record_sym = pattern->sem_type->data.class_type.symbol;
            }
            
            /* Process component patterns (skip first child which is the type) */
            int comp_index = 0;
            for (slist_t *comp = pattern_children->next; comp; comp = comp->next, comp_index++) {
                ast_node_t *comp_pattern = (ast_node_t *)comp->data;
                
                if (comp_pattern->type == AST_UNNAMED_PATTERN) {
                    /* Unnamed component - no variable to extract */
                    continue;
                }
                
                if (comp_pattern->type == AST_TYPE_PATTERN) {
                    const char *comp_var_name = comp_pattern->data.node.name;
                    
                    if (!comp_var_name) {
                        /* Unnamed type pattern (Type _) - no variable to extract */
                        continue;
                    }
                    
                    /* Get component type for accessor return type */
                    type_t *comp_type = comp_pattern->sem_type;
                    
                    
                    /* Find accessor method name from record components */
                    const char *accessor_name = NULL;
                    const char *accessor_desc = NULL;
                    if (record_sym && record_sym->ast) {
                        ast_node_t *record_decl = record_sym->ast;
                        slist_t *record_children = record_decl->data.node.children;
                        int param_idx = 0;
                        for (slist_t *rc = record_children; rc; rc = rc->next) {
                            ast_node_t *child = (ast_node_t *)rc->data;
                            if (child->type == AST_PARAMETER) {
                                if (param_idx == comp_index) {
                                    accessor_name = child->data.node.name;
                                    /* Determine accessor descriptor from component type */
                                    slist_t *param_children = child->data.node.children;
                                    if (param_children) {
                                        ast_node_t *param_type = (ast_node_t *)param_children->data;
                                        if (param_type->sem_type) {
                                            char *ret_desc = type_to_descriptor(param_type->sem_type);
                                            accessor_desc = ret_desc;
                                        }
                                    }
                                    break;
                                }
                                param_idx++;
                            }
                        }
                    }
                    
                    /* Fallback for external records or when param type isn't resolved:
                     * use pattern variable name as accessor name.
                     * This works when pattern uses same names as record components. */
                    if (!accessor_name && comp_var_name) {
                        accessor_name = comp_var_name;
                    }
                    /* If accessor_desc wasn't set from record component, derive from pattern type */
                    if (!accessor_desc && comp_type) {
                        accessor_desc = type_to_descriptor(comp_type);
                    }
                    
                    if (accessor_name && accessor_desc) {
                        /* Allocate local for component variable */
                        uint16_t comp_slot = mg->next_slot++;
                        if (mg->next_slot > mg->max_locals) {
                            mg->max_locals = mg->next_slot;
                        }
                        
                        /* Generate: record.accessor() */
                        bc_emit(mg->code, OP_ALOAD);
                        bc_emit_u1(mg->code, (uint8_t)record_slot);
                        mg_push(mg, 1);
                        
                        char desc_buf[256];
                        snprintf(desc_buf, sizeof(desc_buf), "()%s", accessor_desc);
                        uint16_t accessor_ref = cp_add_methodref(cp, type_internal, accessor_name, desc_buf);
                        bc_emit(mg->code, OP_INVOKEVIRTUAL);
                        bc_emit_u2(mg->code, accessor_ref);
                        /* Stack: component value */
                        
                        /* Store in local */
                        type_kind_t kind = TYPE_CLASS;
                        if (comp_type) kind = comp_type->kind;
                        
                        if (kind == TYPE_INT || kind == TYPE_BOOLEAN || 
                            kind == TYPE_CHAR || kind == TYPE_SHORT || kind == TYPE_BYTE) {
                            bc_emit(mg->code, OP_ISTORE);
                        } else if (kind == TYPE_LONG) {
                            bc_emit(mg->code, OP_LSTORE);
                        } else if (kind == TYPE_FLOAT) {
                            bc_emit(mg->code, OP_FSTORE);
                        } else if (kind == TYPE_DOUBLE) {
                            bc_emit(mg->code, OP_DSTORE);
                        } else {
                            bc_emit(mg->code, OP_ASTORE);
                        }
                        bc_emit_u1(mg->code, (uint8_t)comp_slot);
                        mg_pop_typed(mg, 1);
                        
                        /* Update stackmap for primitive types */
                        if (mg->stackmap) {
                            if (kind == TYPE_INT || kind == TYPE_BOOLEAN || 
                                kind == TYPE_CHAR || kind == TYPE_SHORT || kind == TYPE_BYTE) {
                                stackmap_set_local_int(mg->stackmap, comp_slot);
                            } else if (kind == TYPE_LONG) {
                                stackmap_set_local_long(mg->stackmap, comp_slot);
                            } else if (kind == TYPE_FLOAT) {
                                stackmap_set_local_float(mg->stackmap, comp_slot);
                            } else if (kind == TYPE_DOUBLE) {
                                stackmap_set_local_double(mg->stackmap, comp_slot);
                            } else {
                                stackmap_set_local_object(mg->stackmap, comp_slot, mg->cp, 
                                    comp_type && comp_type->data.class_type.symbol ? 
                                    comp_type->data.class_type.symbol->qualified_name : "java/lang/Object");
                            }
                        }
                        
                        /* Store in locals table for body access - always register */
                        local_var_info_t *info = local_var_info_new(comp_slot, kind);
                        hashtable_insert(mg->locals, comp_var_name, info);
                    }
                }
                /* Nested record patterns would need recursive handling */
            }
        } else {
            /* Type pattern: only allocate local if variable is named */
            if (var_name) {
                /* Allocate local for pattern variable and checkcast */
                var_slot = mg->next_slot++;
                if (mg->next_slot > mg->max_locals) {
                    mg->max_locals = mg->next_slot;
                }
                
                bc_emit(mg->code, OP_ALOAD);
                bc_emit_u1(mg->code, (uint8_t)selector_slot);
                mg_push(mg, 1);
                
                bc_emit(mg->code, OP_CHECKCAST);
                bc_emit_u2(mg->code, type_class);
                
                bc_emit(mg->code, OP_ASTORE);
                bc_emit_u1(mg->code, (uint8_t)var_slot);
                mg_pop_typed(mg, 1);
                
                /* Track pattern variable in stackmap */
                if (mg->stackmap) {
                    stackmap_set_local_object(mg->stackmap, var_slot, mg->cp, type_internal);
                }
            }
            /* Else: unnamed type pattern - just do the instanceof check, no variable */
            
            /* Store pattern variable info for the body (only if named) */
            if (var_name && pattern->sem_symbol) {
                local_var_info_t *info = local_var_info_new(var_slot, TYPE_CLASS);
                if (type_internal) {
                    info->class_name = strdup(type_internal);
                }
                hashtable_insert(mg->locals, var_name, info);
            }
        }
        
        /* Handle guard if present */
        if (guard) {
            codegen_expr(mg, guard, cp);
            size_t guard_ifeq_pos = mg->code->length;
            bc_emit(mg->code, OP_IFEQ);
            bc_emit_u2(mg->code, 0);
            mg_pop_typed(mg, 1);
            
            /* Generate body */
            if (body->type == AST_BLOCK) {
                codegen_statement(mg, body);
            } else {
                codegen_expr(mg, body, cp);
            }
            
            /* Jump to end */
            size_t *goto_pos = malloc(sizeof(size_t));
            *goto_pos = mg->code->length;
            bc_emit(mg->code, OP_GOTO);
            bc_emit_u2(mg->code, 0);
            if (!end_gotos) {
                end_gotos = slist_new(goto_pos);
            } else {
                slist_append(end_gotos, goto_pos);
            }
            
            /* Patch guard ifeq */
            uint16_t guard_skip = (uint16_t)(mg->code->length - guard_ifeq_pos);
            bc_patch_u2(mg->code, guard_ifeq_pos + 1, guard_skip);
            mg_record_frame(mg);
        } else {
            /* Generate body */
            if (body->type == AST_BLOCK) {
                codegen_statement(mg, body);
            } else {
                codegen_expr(mg, body, cp);
            }
            
            /* Jump to end */
            size_t *goto_pos = malloc(sizeof(size_t));
            *goto_pos = mg->code->length;
            bc_emit(mg->code, OP_GOTO);
            bc_emit_u2(mg->code, 0);
            if (!end_gotos) {
                end_gotos = slist_new(goto_pos);
            } else {
                slist_append(end_gotos, goto_pos);
            }
        }
        
        /* Patch ifeq to skip this case */
        uint16_t skip_offset = (uint16_t)(mg->code->length - ifeq_pos);
        bc_patch_u2(mg->code, ifeq_pos + 1, skip_offset);
        
        /* Restore stackmap state and slot counter to case entry state BEFORE recording frame.
         * This ensures the next case starts with the same state.
         * The ifeq target (when pattern doesn't match) should have empty stack
         * and the same locals as before the pattern binding. */
        mg->next_slot = case_saved_slot;
        if (case_entry_state && mg->stackmap) {
            stackmap_restore_state(mg->stackmap, case_entry_state);
        }
        stackmap_state_free(case_entry_state);
        
        /* Record frame for next case entry - this is the ifeq FALSE target.
         * The locals should NOT include the pattern variables since the pattern didn't match. */
        mg_record_frame(mg);
        
        free(type_internal);
    }
    
    /* Generate default case */
    if (default_rule) {
        slist_t *rule_children = default_rule->data.node.children;
        ast_node_t *body = NULL;
        for (slist_t *rc = rule_children; rc; rc = rc->next) {
            body = (ast_node_t *)rc->data;
        }
        if (body) {
            if (body->type == AST_BLOCK) {
                codegen_statement(mg, body);
            } else {
                codegen_expr(mg, body, cp);
            }
        }
    } else {
        /* No default - push null as result */
        bc_emit(mg->code, OP_ACONST_NULL);
        mg_push(mg, 1);
    }
    
    /* Record frame at end of switch (join point for all gotos) */
    if (end_gotos) {
        mg_record_frame(mg);
    }
    
    /* Patch all end gotos */
    size_t end_pos = mg->code->length;
    for (slist_t *g = end_gotos; g; g = g->next) {
        size_t *pos = (size_t *)g->data;
        uint16_t offset = (uint16_t)(end_pos - *pos);
        bc_patch_u2(mg->code, *pos + 1, offset);
        free(pos);
    }
    slist_free(end_gotos);
    
    /* Restore slot counter to before switch - selector and pattern variables
     * are no longer in scope after the switch expression */
    mg->next_slot = switch_saved_slot;
    if (switch_saved_locals > 0) {
        mg_restore_locals_count(mg, switch_saved_locals);
    }
    
    /* Result is on stack */
    return true;
}

/* ========================================================================
 * Main Expression Code Generation Entry Point
 * ======================================================================== */

bool codegen_expr(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr) {
        return false;
    }
    
    switch (expr->type) {
        case AST_LITERAL:
            return codegen_literal(mg, expr, cp);
        
        case AST_IDENTIFIER:
            return codegen_identifier(mg, expr);
        
        case AST_BINARY_EXPR:
            return codegen_binary_expr(mg, expr, cp);
        
        case AST_UNARY_EXPR:
            {
                slist_t *children = expr->data.node.children;
                if (!children) {
                    return false;
                }
                
                token_type_t op = expr->data.node.op_token;
                ast_node_t *operand = (ast_node_t *)children->data;
                
                switch (op) {
                    case TOK_MINUS:
                        /* Unary minus: negate */
                        if (!codegen_expr(mg, operand, cp)) {
                            return false;
                        }
                        bc_emit(mg->code, OP_INEG);
                        return true;
                    
                    case TOK_PLUS:
                        /* Unary plus: no-op */
                        return codegen_expr(mg, operand, cp);
                    
                    case TOK_NOT:
                        /* Logical not: if val != 0 push 0, else push 1 */
                        if (!codegen_expr(mg, operand, cp)) {
                            return false;
                        }
                        /* ifeq consumes the operand */
                        bc_emit(mg->code, OP_IFEQ);
                        bc_emit_u2(mg->code, 7);  /* Skip to iconst_1 */
                        mg_pop_typed(mg, 1);  /* Consumed by ifeq */
                        
                        bc_emit(mg->code, OP_ICONST_0);  /* Push 0 (true became false) */
                        mg_push_int(mg);
                        
                        bc_emit(mg->code, OP_GOTO);
                        bc_emit_u2(mg->code, 4);  /* Skip to end */
                        
                        /* Record frame at iconst_1 (ifeq target) */
                        mg_pop_typed(mg, 1);  /* Pop the iconst_0 for frame recording */
                        mg_record_frame(mg);
                        
                        bc_emit(mg->code, OP_ICONST_1);  /* Push 1 (false became true) */
                        mg_push_int(mg);
                        
                        /* Record frame at end (goto target) */
                        mg_record_frame(mg);
                        
                        /* Net stack effect: 0 (consumed 1, pushed 1) */
                        return true;
                    
                    case TOK_TILDE:
                        /* Bitwise not: xor with -1 */
                        if (!codegen_expr(mg, operand, cp)) {
                            return false;
                        }
                        bc_emit(mg->code, OP_ICONST_M1);
                        bc_emit(mg->code, OP_IXOR);
                        mg_push(mg, 1);
                        mg_pop_typed(mg, 1);
                        return true;
                    
                    case TOK_INC:
                    case TOK_DEC:
                        {
                            /* Pre/post increment/decrement */
                            const char *op_name = expr->data.node.name;
                            bool is_post = (op_name && strstr(op_name, "post") != NULL);
                            bool is_inc = (op == TOK_INC);
                            int delta = is_inc ? 1 : -1;
                            
                            if (operand->type == AST_IDENTIFIER) {
                                /* Local variable increment/decrement */
                                const char *name = operand->data.leaf.name;
                                local_var_info_t *inc_info = (local_var_info_t *)hashtable_lookup(mg->locals, name);
                                
                                if (inc_info) {
                                    uint16_t slot = inc_info->slot;
                                    
                                    if (is_post) {
                                        /* Post: load old value, then increment */
                                        /* iload slot; iinc slot, delta */
                                        if (slot <= 3) {
                                            bc_emit(mg->code, OP_ILOAD_0 + slot);
                                        } else {
                                            bc_emit(mg->code, OP_ILOAD);
                                            bc_emit_u1(mg->code, (uint8_t)slot);
                                        }
                                        mg_push(mg, 1);
                                        bc_emit(mg->code, OP_IINC);
                                        bc_emit_u1(mg->code, (uint8_t)slot);
                                        bc_emit_s1(mg->code, (int8_t)delta);
                                    } else {
                                        /* Pre: increment first, then load new value */
                                        /* iinc slot, delta; iload slot */
                                        bc_emit(mg->code, OP_IINC);
                                        bc_emit_u1(mg->code, (uint8_t)slot);
                                        bc_emit_s1(mg->code, (int8_t)delta);
                                        if (slot <= 3) {
                                            bc_emit(mg->code, OP_ILOAD_0 + slot);
                                        } else {
                                            bc_emit(mg->code, OP_ILOAD);
                                            bc_emit_u1(mg->code, (uint8_t)slot);
                                        }
                                        mg_push(mg, 1);
                                    }
                                    return true;
                                }
                                
                                /* Check for instance field */
                                if (mg->class_gen && !mg->is_static) {
                                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
                                    if (field) {
                                        /* Instance field: this.field++ or ++this.field */
                                        uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                                             field->name, field->descriptor);
                                        
                                        if (is_post) {
                                            /* Post: aload_0, dup, getfield, dup_x1, iconst_1, iadd/isub, putfield */
                                            /* Stack trace: [] -> [this] -> [this,this] -> [this,old] -> [old,this,old] -> [old,this,old,1] -> [old,this,new] -> [old] */
                                            bc_emit(mg->code, OP_ALOAD_0);
                                            mg_push_null(mg);  /* [this] - object reference */
                                            bc_emit(mg->code, OP_DUP);
                                            mg_push_null(mg);  /* [this,this] - duplicated object ref */
                                            bc_emit(mg->code, OP_GETFIELD);
                                            bc_emit_u2(mg->code, fieldref);
                                            /* getfield: pop ref, push value - net 0: [this,old] */
                                            bc_emit(mg->code, OP_DUP_X1);
                                            mg_push_int(mg);  /* [old,this,old] - duplicated int */
                                            bc_emit(mg->code, OP_ICONST_1);
                                            mg_push_int(mg);  /* [old,this,old,1] */
                                            bc_emit(mg->code, is_inc ? OP_IADD : OP_ISUB);
                                            mg_pop_typed(mg, 1);   /* [old,this,new] */
                                            bc_emit(mg->code, OP_PUTFIELD);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_pop_typed(mg, 2);   /* [old] - putfield consumes ref and value */
                                            /* Result: old value on stack, stack_depth = 1 */
                                        } else {
                                            /* Pre: aload_0, dup, getfield, iconst_1, iadd/isub, dup_x1, putfield */
                                            /* Stack trace: [] -> [this] -> [this,this] -> [this,old] -> [this,old,1] -> [this,new] -> [new,this,new] -> [new] */
                                            bc_emit(mg->code, OP_ALOAD_0);
                                            mg_push_null(mg);  /* [this] - object reference */
                                            bc_emit(mg->code, OP_DUP);
                                            mg_push_null(mg);  /* [this,this] - duplicated object ref */
                                            bc_emit(mg->code, OP_GETFIELD);
                                            bc_emit_u2(mg->code, fieldref);
                                            /* getfield: pop ref, push value - net 0: [this,old] */
                                            bc_emit(mg->code, OP_ICONST_1);
                                            mg_push(mg, 1);  /* [this,old,1] */
                                            bc_emit(mg->code, is_inc ? OP_IADD : OP_ISUB);
                                            mg_pop_typed(mg, 1);   /* [this,new] */
                                            bc_emit(mg->code, OP_DUP_X1);
                                            mg_push(mg, 1);  /* [new,this,new] */
                                            bc_emit(mg->code, OP_PUTFIELD);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_pop_typed(mg, 2);   /* [new] - putfield consumes ref and value */
                                            /* Result: new value on stack, stack_depth = 1 */
                                        }
                                        return true;
                                    }
                                }
                                
                                fprintf(stderr, "codegen: cannot resolve variable for increment/decrement: %s\n", name);
                                return false;
                            }
                            
                            /* TODO: Handle field access (obj.field++) and array access (arr[i]++) */
                            fprintf(stderr, "codegen: increment/decrement on non-identifier not yet implemented\n");
                            return false;
                        }
                    
                    default:
                        fprintf(stderr, "codegen: unknown unary operator: %s (token %d)\n", 
                                expr->data.node.name ? expr->data.node.name : "?", op);
                        return false;
                }
            }
        
        case AST_ASSIGNMENT_EXPR:
            return codegen_assignment(mg, expr, cp);
        
        case AST_THIS_EXPR:
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push_null(mg);  /* 'this' is an object reference */
            return true;
        
        case AST_SUPER_EXPR:
            /* 'super' is also 'this' - just a marker for invokespecial to superclass */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push_null(mg);  /* 'super' reference (same as 'this') */
            return true;
        
        case AST_METHOD_CALL:
            return codegen_method_call(mg, expr, cp);
        
        case AST_EXPLICIT_CTOR_CALL:
            return codegen_explicit_ctor_call(mg, expr, cp);
        
        case AST_NEW_OBJECT:
            return codegen_new_object(mg, expr, cp);
        
        case AST_NEW_ARRAY:
            return codegen_new_array(mg, expr, cp);
        
        case AST_ARRAY_INIT:
            return codegen_array_init(mg, expr, cp);
        
        case AST_ARRAY_ACCESS:
            {
                /* Array access: arr[index] */
                slist_t *children = expr->data.node.children;
                if (!children || !children->next) {
                    fprintf(stderr, "codegen: malformed array access\n");
                    return false;
                }
                
                ast_node_t *array_expr = (ast_node_t *)children->data;
                ast_node_t *index_expr = (ast_node_t *)children->next->data;
                
                /* Generate array reference */
                if (!codegen_expr(mg, array_expr, cp)) {
                    return false;
                }
                
                /* Generate index */
                if (!codegen_expr(mg, index_expr, cp)) {
                    return false;
                }
                
                /* Determine element type and emit appropriate load opcode
                 * For multi-dimensional arrays, we need to track access depth
                 * to determine if the result is still an array reference or
                 * a scalar value.
                 */
                type_kind_t elem_kind = TYPE_INT;  /* Default to int */
                bool is_multi_dim_intermediate = false;
                
                if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                    type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                    if (elem_type) {
                        elem_kind = elem_type->kind;
                    }
                    /* Check if this is a multi-dim array with more dimensions */
                    if (array_expr->sem_type->data.array_type.dimensions > 1) {
                        is_multi_dim_intermediate = true;
                    }
                } else if (array_expr->type == AST_IDENTIFIER) {
                    /* Look up local variable's array info */
                    const char *arr_name = array_expr->data.leaf.name;
                    if (mg_local_is_array(mg, arr_name)) {
                        int dims = mg_local_array_dims(mg, arr_name);
                        if (dims > 1) {
                            /* This access yields another array */
                            is_multi_dim_intermediate = true;
                        } else {
                            elem_kind = mg_local_array_elem_kind(mg, arr_name);
                        }
                    }
                } else if (array_expr->type == AST_ARRAY_ACCESS) {
                    /* Chained array access - find base array and count depth */
                    int depth = 1;  /* Current access */
                    ast_node_t *base = array_expr;
                    while (base && base->type == AST_ARRAY_ACCESS) {
                        depth++;
                        base = (ast_node_t *)base->data.node.children->data;
                    }
                    if (base && base->type == AST_IDENTIFIER) {
                        const char *arr_name = base->data.leaf.name;
                        if (mg_local_is_array(mg, arr_name)) {
                            int dims = mg_local_array_dims(mg, arr_name);
                            if (depth < dims) {
                                /* Still have more dimensions, result is an array */
                                is_multi_dim_intermediate = true;
                            } else {
                                elem_kind = mg_local_array_elem_kind(mg, arr_name);
                            }
                        }
                    }
                }
                
                /* Emit array load instruction */
                if (is_multi_dim_intermediate) {
                    /* Loading a sub-array (object reference) */
                    bc_emit(mg->code, OP_AALOAD);
                } else {
                    switch (elem_kind) {
                        case TYPE_BOOLEAN:
                        case TYPE_BYTE:
                            bc_emit(mg->code, OP_BALOAD);
                            break;
                        case TYPE_CHAR:
                            bc_emit(mg->code, OP_CALOAD);
                            break;
                        case TYPE_SHORT:
                            bc_emit(mg->code, OP_SALOAD);
                            break;
                        case TYPE_INT:
                            bc_emit(mg->code, OP_IALOAD);
                            break;
                        case TYPE_LONG:
                            bc_emit(mg->code, OP_LALOAD);
                            mg_push(mg, 1);  /* Long takes 2 slots */
                            break;
                        case TYPE_FLOAT:
                            bc_emit(mg->code, OP_FALOAD);
                            break;
                        case TYPE_DOUBLE:
                            bc_emit(mg->code, OP_DALOAD);
                            mg_push(mg, 1);  /* Double takes 2 slots */
                            break;
                        case TYPE_CLASS:
                        case TYPE_ARRAY:
                            bc_emit(mg->code, OP_AALOAD);
                            break;
                        default:
                            bc_emit(mg->code, OP_IALOAD);  /* Default to int */
                            break;
                    }
                }
                
                /* Stack: arrayref, index -> value (net effect: -1) */
                mg_pop_typed(mg, 1);
                return true;
            }
        
        case AST_FIELD_ACCESS:
            return codegen_field_access(mg, expr, cp);
        
        case AST_INSTANCEOF_EXPR:
            {
                /* instanceof: expr instanceof Type */
                slist_t *children = expr->data.node.children;
                if (!children || !children->next) {
                    fprintf(stderr, "codegen: malformed instanceof expression\n");
                    return false;
                }
                
                ast_node_t *object_expr = (ast_node_t *)children->data;
                ast_node_t *type_node = (ast_node_t *)children->next->data;
                
                /* Generate the object expression */
                if (!codegen_expr(mg, object_expr, cp)) {
                    return false;
                }
                
                /* Get the class name for instanceof */
                const char *class_name = NULL;
                if (type_node->type == AST_CLASS_TYPE) {
                    class_name = type_node->data.node.name;
                } else if (type_node->type == AST_PRIMITIVE_TYPE) {
                    /* instanceof doesn't work with primitives */
                    fprintf(stderr, "codegen: instanceof not supported for primitive types\n");
                    return false;
                }
                
                if (!class_name) {
                    fprintf(stderr, "codegen: instanceof without class type\n");
                    return false;
                }
                
                /* Resolve to internal name */
                const char *resolved = resolve_java_lang_class(class_name);
                char *internal_name = class_to_internal_name(resolved);
                uint16_t class_index = cp_add_class(cp, internal_name);
                free(internal_name);
                
                /* Emit instanceof instruction */
                bc_emit(mg->code, OP_INSTANCEOF);
                bc_emit_u2(mg->code, class_index);
                /* Stack: objectref -> int (0 or 1) - net effect 0 */
                
                return true;
            }
        
        case AST_CAST_EXPR:
            {
                /* Cast expression: (Type) expr */
                slist_t *children = expr->data.node.children;
                if (!children || !children->next) {
                    fprintf(stderr, "codegen: malformed cast expression\n");
                    return false;
                }
                
                ast_node_t *type_node = (ast_node_t *)children->data;
                ast_node_t *operand = (ast_node_t *)children->next->data;
                
                /* Determine source and target types */
                type_kind_t target_kind = TYPE_UNKNOWN;
                const char *target_class = NULL;
                
                if (type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = type_node->data.leaf.name;
                    if (strcmp(prim_name, "int") == 0) target_kind = TYPE_INT;
                    else if (strcmp(prim_name, "long") == 0) target_kind = TYPE_LONG;
                    else if (strcmp(prim_name, "float") == 0) target_kind = TYPE_FLOAT;
                    else if (strcmp(prim_name, "double") == 0) target_kind = TYPE_DOUBLE;
                    else if (strcmp(prim_name, "byte") == 0) target_kind = TYPE_BYTE;
                    else if (strcmp(prim_name, "short") == 0) target_kind = TYPE_SHORT;
                    else if (strcmp(prim_name, "char") == 0) target_kind = TYPE_CHAR;
                    else if (strcmp(prim_name, "boolean") == 0) target_kind = TYPE_BOOLEAN;
                } else if (type_node->type == AST_CLASS_TYPE) {
                    target_kind = TYPE_CLASS;
                    /* Use sem_type if resolved, otherwise fall back to AST name */
                    if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                        /* Try symbol's qualified name first, then type's name */
                        if (type_node->sem_type->data.class_type.symbol &&
                            type_node->sem_type->data.class_type.symbol->qualified_name) {
                            target_class = type_node->sem_type->data.class_type.symbol->qualified_name;
                        } else if (type_node->sem_type->data.class_type.name) {
                            target_class = type_node->sem_type->data.class_type.name;
                        } else {
                            target_class = type_node->data.node.name;
                        }
                    } else {
                        target_class = type_node->data.node.name;
                    }
                } else if (type_node->type == AST_ARRAY_TYPE) {
                    target_kind = TYPE_ARRAY;
                    /* TODO: Handle array type descriptor */
                }
                
                /* Generate the operand expression */
                if (!codegen_expr(mg, operand, cp)) {
                    return false;
                }
                
                /* Determine source type from operand's semantic type */
                type_kind_t source_kind = TYPE_UNKNOWN;
                if (operand->sem_type) {
                    source_kind = operand->sem_type->kind;
                }
                
                /* Reference cast: use checkcast */
                if (target_kind == TYPE_CLASS || target_kind == TYPE_ARRAY) {
                    if (target_class) {
                        const char *resolved = resolve_java_lang_class(target_class);
                        char *internal_name = class_to_internal_name(resolved);
                        uint16_t class_index = cp_add_class(cp, internal_name);
                        free(internal_name);
                        
                        bc_emit(mg->code, OP_CHECKCAST);
                        bc_emit_u2(mg->code, class_index);
                        /* Stack: objectref -> objectref (no change) */
                    }
                    return true;
                }
                
                /* Primitive cast: emit conversion instruction */
                /* Determine source type - default to int if unknown */
                if (source_kind == TYPE_UNKNOWN) {
                    source_kind = TYPE_INT;  /* Assume int for unknown types */
                }
                
                /* No conversion needed if same type */
                if (source_kind == target_kind) {
                    return true;
                }
                
                /* Emit conversion opcodes */
                switch (source_kind) {
                    case TYPE_INT:
                    case TYPE_BYTE:
                    case TYPE_SHORT:
                    case TYPE_CHAR:
                    case TYPE_BOOLEAN:
                        switch (target_kind) {
                            case TYPE_LONG:   bc_emit(mg->code, OP_I2L); mg_push(mg, 1); break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_I2F); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_I2D); mg_push(mg, 1); break;
                            case TYPE_BYTE:   bc_emit(mg->code, OP_I2B); break;
                            case TYPE_SHORT:  bc_emit(mg->code, OP_I2S); break;
                            case TYPE_CHAR:   bc_emit(mg->code, OP_I2C); break;
                            default: break;  /* int to int: no-op */
                        }
                        break;
                    case TYPE_LONG:
                        switch (target_kind) {
                            case TYPE_INT:
                            case TYPE_BYTE:
                            case TYPE_SHORT:
                            case TYPE_CHAR:
                                bc_emit(mg->code, OP_L2I);
                                mg_pop_typed(mg, 1);
                                if (target_kind == TYPE_BYTE) bc_emit(mg->code, OP_I2B);
                                else if (target_kind == TYPE_SHORT) bc_emit(mg->code, OP_I2S);
                                else if (target_kind == TYPE_CHAR) bc_emit(mg->code, OP_I2C);
                                break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_L2F); mg_pop_typed(mg, 1); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_L2D); break;
                            default: break;
                        }
                        break;
                    case TYPE_FLOAT:
                        switch (target_kind) {
                            case TYPE_INT:
                            case TYPE_BYTE:
                            case TYPE_SHORT:
                            case TYPE_CHAR:
                                bc_emit(mg->code, OP_F2I);
                                if (target_kind == TYPE_BYTE) bc_emit(mg->code, OP_I2B);
                                else if (target_kind == TYPE_SHORT) bc_emit(mg->code, OP_I2S);
                                else if (target_kind == TYPE_CHAR) bc_emit(mg->code, OP_I2C);
                                break;
                            case TYPE_LONG:   bc_emit(mg->code, OP_F2L); mg_push(mg, 1); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_F2D); mg_push(mg, 1); break;
                            default: break;
                        }
                        break;
                    case TYPE_DOUBLE:
                        switch (target_kind) {
                            case TYPE_INT:
                            case TYPE_BYTE:
                            case TYPE_SHORT:
                            case TYPE_CHAR:
                                bc_emit(mg->code, OP_D2I);
                                mg_pop_typed(mg, 1);
                                if (target_kind == TYPE_BYTE) bc_emit(mg->code, OP_I2B);
                                else if (target_kind == TYPE_SHORT) bc_emit(mg->code, OP_I2S);
                                else if (target_kind == TYPE_CHAR) bc_emit(mg->code, OP_I2C);
                                break;
                            case TYPE_LONG:   bc_emit(mg->code, OP_D2L); break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_D2F); mg_pop_typed(mg, 1); break;
                            default: break;
                        }
                        break;
                    default:
                        /* Reference to primitive or unknown - just leave on stack */
                        break;
                }
                
                return true;
            }
        
        case AST_PARENTHESIZED:
            {
                /* Parenthesized expression: just evaluate the inner expression */
                slist_t *children = expr->data.node.children;
                if (!children) {
                    fprintf(stderr, "codegen: empty parenthesized expression\n");
                    return false;
                }
                return codegen_expr(mg, (ast_node_t *)children->data, cp);
            }
        
        case AST_CONDITIONAL_EXPR:
            {
                /* Ternary operator: condition ? then_expr : else_expr */
                slist_t *children = expr->data.node.children;
                if (!children || !children->next || !children->next->next) {
                    fprintf(stderr, "codegen: malformed ternary expression\n");
                    return false;
                }
                
                ast_node_t *condition = (ast_node_t *)children->data;
                ast_node_t *then_expr = (ast_node_t *)children->next->data;
                ast_node_t *else_expr = (ast_node_t *)children->next->next->data;
                
                /* Generate condition */
                if (!codegen_expr(mg, condition, cp)) {
                    return false;
                }
                
                /* Save stack state before branching */
                int saved_stack_depth = mg->stack_depth;
                int saved_stackmap_stack_size = mg->stackmap ? mg->stackmap->current_stack_size : 0;
                
                /* ifeq else_branch (jump if condition is false/0) */
                size_t ifeq_pos = mg->code->length;
                bc_emit(mg->code, OP_IFEQ);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);  /* Condition consumed */
                
                /* Generate then branch */
                if (!codegen_expr(mg, then_expr, cp)) {
                    return false;
                }
                
                /* goto end (skip else branch) */
                size_t goto_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                
                /* Patch ifeq to jump here (else branch) */
                uint16_t else_offset = (uint16_t)(mg->code->length - ifeq_pos);
                bc_patch_u2(mg->code, ifeq_pos + 1, else_offset);
                
                /* Restore stack state for else branch (same state as after condition consumed) */
                mg->stack_depth = saved_stack_depth - 1;  /* After consuming condition */
                if (mg->stackmap && saved_stackmap_stack_size > 0) {
                    mg->stackmap->current_stack_size = saved_stackmap_stack_size - 1;
                }
                
                /* Record frame at else branch target */
                mg_record_frame(mg);
                
                /* Generate else branch */
                if (!codegen_expr(mg, else_expr, cp)) {
                    return false;
                }
                
                /* Patch goto to jump here (end) */
                uint16_t end_offset = (uint16_t)(mg->code->length - goto_pos);
                bc_patch_u2(mg->code, goto_pos + 1, end_offset);
                
                /* Record frame at merge point */
                mg_record_frame(mg);
                
                /* Result is on stack (either from then or else branch) */
                return true;
            }
        
        case AST_SWITCH_EXPR:
            {
                /* Switch expression (Java 12+): evaluates to a value */
                slist_t *children = expr->data.node.children;
                if (!children) {
                    fprintf(stderr, "codegen: empty switch expression\n");
                    return false;
                }
                
                /* Check if this switch has type patterns - if so, use instanceof chain */
                bool has_type_patterns = false;
                for (slist_t *node = children->next; node; node = node->next) {
                    ast_node_t *rule = (ast_node_t *)node->data;
                    if (rule->type == AST_SWITCH_RULE) {
                        slist_t *rule_children = rule->data.node.children;
                        for (slist_t *rc = rule_children; rc; rc = rc->next) {
                            ast_node_t *child = (ast_node_t *)rc->data;
                            if (child->type == AST_TYPE_PATTERN || 
                                child->type == AST_GUARDED_PATTERN ||
                                child->type == AST_UNNAMED_PATTERN ||
                                child->type == AST_RECORD_PATTERN) {
                                has_type_patterns = true;
                                break;
                            }
                        }
                        if (has_type_patterns) break;
                    }
                }
                
                if (has_type_patterns) {
                    /* Generate instanceof chain for type patterns */
                    return codegen_pattern_switch_expr(mg, expr, cp);
                }
                
                /* Generate selector expression */
                ast_node_t *selector = (ast_node_t *)children->data;
                if (!codegen_expr(mg, selector, cp)) {
                    return false;
                }
                
                /* Count case rules (excluding default) */
                /* The body is the LAST child of each rule. All other children are case constants. */
                int num_cases = 0;
                for (slist_t *node = children->next; node; node = node->next) {
                    ast_node_t *rule = (ast_node_t *)node->data;
                    if (rule->type == AST_SWITCH_RULE) {
                        if (!(rule->data.node.name && 
                              strcmp(rule->data.node.name, "default") == 0)) {
                            /* Count children minus 1 (last child is the body) */
                            slist_t *rule_children = rule->data.node.children;
                            int child_count = 0;
                            for (slist_t *rc = rule_children; rc; rc = rc->next) {
                                child_count++;
                            }
                            num_cases += (child_count > 0) ? (child_count - 1) : 0;
                        }
                    }
                }
                
                /* Check if this is an enum switch */
                bool is_enum_switch = (selector->sem_type && 
                                       selector->sem_type->kind == TYPE_CLASS &&
                                       selector->sem_type->data.class_type.symbol &&
                                       selector->sem_type->data.class_type.symbol->kind == SYM_ENUM);
                
                /* For enum switch, call ordinal() */
                if (is_enum_switch) {
                    uint16_t methodref = cp_add_methodref(cp, "java/lang/Enum", "ordinal", "()I");
                    bc_emit(mg->code, OP_INVOKEVIRTUAL);
                    bc_emit_u2(mg->code, methodref);
                }
                
                /* Emit lookupswitch */
                size_t switch_pos = mg->code->length;
                bc_emit(mg->code, OP_LOOKUPSWITCH);
                mg_pop_typed(mg, 1);  /* Selector consumed */
                
                /* Pad to 4-byte alignment */
                while ((mg->code->length) % 4 != 0) {
                    bc_emit_u1(mg->code, 0);
                }
                
                /* Default offset placeholder */
                size_t default_offset_pos = mg->code->length;
                bc_emit_u4(mg->code, 0);
                
                /* Number of pairs */
                bc_emit_u4(mg->code, (uint32_t)num_cases);
                
                /* Collect case values and emit sorted pairs */
                int32_t *case_values = calloc(num_cases, sizeof(int32_t));
                size_t *case_offset_positions = calloc(num_cases, sizeof(size_t));
                ast_node_t **case_rules = calloc(num_cases, sizeof(ast_node_t *));
                int case_idx = 0;
                
                for (slist_t *node = children->next; node; node = node->next) {
                    ast_node_t *rule = (ast_node_t *)node->data;
                    if (rule->type == AST_SWITCH_RULE) {
                        if (rule->data.node.name && 
                            strcmp(rule->data.node.name, "default") == 0) {
                            continue;  /* Skip default */
                        }
                        
                        /* All children except the last are case constants */
                        slist_t *rule_children = rule->data.node.children;
                        for (slist_t *rc = rule_children; rc && rc->next; rc = rc->next) {
                            ast_node_t *child = (ast_node_t *)rc->data;
                            
                            if (child->type == AST_LITERAL) {
                                case_values[case_idx] = (int32_t)child->data.leaf.value.int_val;
                            } else if (child->type == AST_IDENTIFIER && is_enum_switch) {
                                case_values[case_idx] = (int32_t)child->data.leaf.value.int_val;
                            }
                            case_rules[case_idx] = rule;
                            case_idx++;
                        }
                    }
                }
                
                /* Sort by value */
                for (int i = 0; i < num_cases - 1; i++) {
                    for (int j = 0; j < num_cases - i - 1; j++) {
                        if (case_values[j] > case_values[j + 1]) {
                            int32_t tmp_val = case_values[j];
                            case_values[j] = case_values[j + 1];
                            case_values[j + 1] = tmp_val;
                            ast_node_t *tmp_rule = case_rules[j];
                            case_rules[j] = case_rules[j + 1];
                            case_rules[j + 1] = tmp_rule;
                        }
                    }
                }
                
                /* Emit sorted pairs with offset placeholders */
                for (int i = 0; i < num_cases; i++) {
                    bc_emit_u4(mg->code, (uint32_t)case_values[i]);
                    case_offset_positions[i] = mg->code->length;
                    bc_emit_u4(mg->code, 0);
                }
                
                /* Track gotos to patch at end */
                slist_t *end_gotos = NULL;
                
                /* Find default rule */
                ast_node_t *default_rule = NULL;
                for (slist_t *node = children->next; node; node = node->next) {
                    ast_node_t *rule = (ast_node_t *)node->data;
                    if (rule->type == AST_SWITCH_RULE &&
                        rule->data.node.name && 
                        strcmp(rule->data.node.name, "default") == 0) {
                        default_rule = rule;
                        break;
                    }
                }
                
                /* Generate code for each unique rule */
                ast_node_t *last_rule = NULL;
                size_t default_code_pos = 0;
                
                for (int i = 0; i < num_cases; i++) {
                    if (case_rules[i] == last_rule) {
                        /* Same rule as previous - just patch offset to same location */
                        continue;
                    }
                    
                    size_t code_pos = mg->code->length;
                    mg_record_frame(mg);
                    
                    /* Patch all cases pointing to this rule */
                    for (int j = i; j < num_cases; j++) {
                        if (case_rules[j] == case_rules[i]) {
                            int32_t offset = (int32_t)(code_pos - switch_pos);
                            mg->code->code[case_offset_positions[j] + 0] = (offset >> 24) & 0xFF;
                            mg->code->code[case_offset_positions[j] + 1] = (offset >> 16) & 0xFF;
                            mg->code->code[case_offset_positions[j] + 2] = (offset >> 8) & 0xFF;
                            mg->code->code[case_offset_positions[j] + 3] = offset & 0xFF;
                        }
                    }
                    
                    last_rule = case_rules[i];
                    
                    /* Generate rule body - the body is the last child of the rule */
                    slist_t *rule_children = case_rules[i]->data.node.children;
                    ast_node_t *body = NULL;
                    for (slist_t *rc = rule_children; rc; rc = rc->next) {
                        body = (ast_node_t *)rc->data;
                    }
                    
                    if (body) {
                        if (body->type == AST_BLOCK) {
                            /* Block with yield - set up yield patch context */
                            slist_t *saved_yield = mg->yield_patches;
                            mg->yield_patches = NULL;
                            
                            /* Generate block statements */
                            slist_t *stmts = body->data.node.children;
                            for (slist_t *s = stmts; s; s = s->next) {
                                if (!codegen_statement(mg, (ast_node_t *)s->data)) {
                                    free(case_values);
                                    free(case_offset_positions);
                                    free(case_rules);
                                    slist_free(end_gotos);
                                    return false;
                                }
                            }
                            
                            /* Collect yield patches as end gotos */
                            for (slist_t *yp = mg->yield_patches; yp; yp = yp->next) {
                                end_gotos = slist_prepend(end_gotos, yp->data);
                            }
                            slist_free(mg->yield_patches);
                            mg->yield_patches = saved_yield;
                        } else if (body->type == AST_THROW_STMT) {
                            /* throw statement */
                            if (!codegen_statement(mg, body)) {
                                free(case_values);
                                free(case_offset_positions);
                                free(case_rules);
                                slist_free(end_gotos);
                                return false;
                            }
                            /* No need for goto - throw doesn't fall through */
                            continue;
                        } else {
                            /* Expression - generate it */
                            if (!codegen_expr(mg, body, cp)) {
                                free(case_values);
                                free(case_offset_positions);
                                free(case_rules);
                                slist_free(end_gotos);
                                return false;
                            }
                            
                            /* Emit goto to end */
                            size_t goto_pos = mg->code->length;
                            bc_emit(mg->code, OP_GOTO);
                            bc_emit_u2(mg->code, 0);
                            end_gotos = slist_prepend(end_gotos, (void *)(uintptr_t)goto_pos);
                            mg_pop_typed(mg, 1);  /* Result will be restored at merge point */
                        }
                    }
                }
                
                /* Generate default if present */
                if (default_rule) {
                    default_code_pos = mg->code->length;
                    mg_record_frame(mg);
                    
                    /* The body is the last (and only for default) child */
                    slist_t *rule_children = default_rule->data.node.children;
                    ast_node_t *body = NULL;
                    for (slist_t *rc = rule_children; rc; rc = rc->next) {
                        body = (ast_node_t *)rc->data;
                    }
                    
                    if (body) {
                        if (body->type == AST_BLOCK) {
                            slist_t *saved_yield = mg->yield_patches;
                            mg->yield_patches = NULL;
                            
                            slist_t *stmts = body->data.node.children;
                            for (slist_t *s = stmts; s; s = s->next) {
                                if (!codegen_statement(mg, (ast_node_t *)s->data)) {
                                    free(case_values);
                                    free(case_offset_positions);
                                    free(case_rules);
                                    slist_free(end_gotos);
                                    return false;
                                }
                            }
                            
                            for (slist_t *yp = mg->yield_patches; yp; yp = yp->next) {
                                end_gotos = slist_prepend(end_gotos, yp->data);
                            }
                            slist_free(mg->yield_patches);
                            mg->yield_patches = saved_yield;
                        } else if (body->type == AST_THROW_STMT) {
                            if (!codegen_statement(mg, body)) {
                                free(case_values);
                                free(case_offset_positions);
                                free(case_rules);
                                slist_free(end_gotos);
                                return false;
                            }
                        } else {
                            if (!codegen_expr(mg, body, cp)) {
                                free(case_values);
                                free(case_offset_positions);
                                free(case_rules);
                                slist_free(end_gotos);
                                return false;
                            }
                            size_t goto_pos = mg->code->length;
                            bc_emit(mg->code, OP_GOTO);
                            bc_emit_u2(mg->code, 0);
                            end_gotos = slist_prepend(end_gotos, (void *)(uintptr_t)goto_pos);
                            mg_pop_typed(mg, 1);
                        }
                    }
                }
                
                /* Patch default offset */
                size_t switch_end = mg->code->length;
                int32_t default_offset;
                if (default_code_pos != 0) {
                    default_offset = (int32_t)(default_code_pos - switch_pos);
                } else {
                    default_offset = (int32_t)(switch_end - switch_pos);
                }
                mg->code->code[default_offset_pos + 0] = (default_offset >> 24) & 0xFF;
                mg->code->code[default_offset_pos + 1] = (default_offset >> 16) & 0xFF;
                mg->code->code[default_offset_pos + 2] = (default_offset >> 8) & 0xFF;
                mg->code->code[default_offset_pos + 3] = default_offset & 0xFF;
                
                /* Patch all gotos to end */
                mg_record_frame(mg);
                for (slist_t *g = end_gotos; g; g = g->next) {
                    size_t goto_pos = (size_t)(uintptr_t)g->data;
                    int16_t offset = (int16_t)(switch_end - goto_pos);
                    mg->code->code[goto_pos + 1] = (offset >> 8) & 0xFF;
                    mg->code->code[goto_pos + 2] = offset & 0xFF;
                }
                
                slist_free(end_gotos);
                free(case_values);
                free(case_offset_positions);
                free(case_rules);
                
                /* Result is on stack from whichever branch was taken */
                mg_push(mg, 1);
                
                return true;
            }
        
        case AST_LAMBDA_EXPR:
            {
                /* Lambda expression - generate invokedynamic call site */
                
                /* Get lambda info from semantic analysis */
                type_t *func_interface = expr->sem_type;
                if (!func_interface || func_interface->kind != TYPE_CLASS) {
                    fprintf(stderr, "codegen: lambda without target type\n");
                    return false;
                }
                
                const char *sam_name = expr->data.node.name;  /* Stored by semantic analysis */
                symbol_t *sam = expr->sem_symbol;  /* SAM method symbol */
                if (!sam_name || !sam) {
                    fprintf(stderr, "codegen: lambda without SAM info\n");
                    return false;
                }
                
                /* Get functional interface info */
                symbol_t *iface_sym = func_interface->data.class_type.symbol;
                if (!iface_sym) {
                    fprintf(stderr, "codegen: lambda without interface symbol\n");
                    return false;
                }
                
                class_gen_t *cg = mg->class_gen;
                
                /* Ensure bootstrap methods table exists */
                if (!cg->bootstrap_methods) {
                    cg->bootstrap_methods = bootstrap_methods_new();
                }
                
                /* Get or create the LambdaMetafactory bootstrap method */
                int metafactory_idx = cg_ensure_lambda_metafactory(cg);
                if (metafactory_idx < 0) {
                    fprintf(stderr, "codegen: failed to set up LambdaMetafactory\n");
                    return false;
                }
                
                /* Determine if this lambda captures 'this' */
                bool captures_this = expr->lambda_captures_this;
                slist_t *captures = expr->lambda_captures;
                
                /* Generate unique lambda method name */
                char lambda_method_name[256];
                const char *enclosing_name = mg->method ? mg->method->name : "main";
                snprintf(lambda_method_name, sizeof(lambda_method_name), 
                         "lambda$%s$%d", enclosing_name, cg->lambda_counter++);
                
                /* Build the lambda method descriptor:
                 * If captures this: instance method, params are SAM params
                 * If captures locals: static method, params are captures + SAM params */
                string_t *impl_desc = string_new("(");
                
                /* Add captured variable types to descriptor (for static lambdas) */
                if (!captures_this) {
                    for (slist_t *cap = captures; cap; cap = cap->next) {
                        symbol_t *var_sym = (symbol_t *)cap->data;
                        if (var_sym && var_sym->type) {
                            char *cap_desc = type_to_descriptor(var_sym->type);
                            string_append(impl_desc, cap_desc);
                            free(cap_desc);
                        }
                    }
                }
                
                /* Add SAM parameters to implementation method */
                slist_t *sam_params = sam->data.method_data.parameters;
                for (slist_t *p = sam_params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type) {
                        char *param_desc = type_to_descriptor(param->type);
                        string_append(impl_desc, param_desc);
                        free(param_desc);
                    }
                }
                
                string_append(impl_desc, ")");
                
                /* Add return type */
                if (sam->type) {
                    char *ret_desc = type_to_descriptor(sam->type);
                    string_append(impl_desc, ret_desc);
                    free(ret_desc);
                } else {
                    string_append(impl_desc, "V");
                }
                
                /* Build SAM descriptor (for bootstrap arguments) */
                char *sam_descriptor = method_to_descriptor(sam);
                
                /* Generate the synthetic lambda$N method */
                method_info_gen_t *lambda_method = calloc(1, sizeof(method_info_gen_t));
                lambda_method->access_flags = ACC_PRIVATE | ACC_SYNTHETIC;
                if (!captures_this) {
                    lambda_method->access_flags |= ACC_STATIC;
                }
                lambda_method->name_index = cp_add_utf8(cg->cp, lambda_method_name);
                lambda_method->descriptor_index = cp_add_utf8(cg->cp, impl_desc->str);
                
                /* Create method generator for lambda body */
                method_gen_t *lambda_mg = calloc(1, sizeof(method_gen_t));
                lambda_mg->code = bytecode_new();
                lambda_mg->cp = cg->cp;
                lambda_mg->class_gen = cg;
                lambda_mg->locals = hashtable_new();
                lambda_mg->is_static = !captures_this;
                lambda_mg->method = sam;  /* Use SAM for type info */
                
                /* Initialize StackMapTable tracking for lambda method */
                lambda_mg->stackmap = stackmap_new();
                if (lambda_mg->stackmap && cg->internal_name) {
                    stackmap_init_method(lambda_mg->stackmap, lambda_mg, 
                                        lambda_mg->is_static, cg->internal_name);
                }
                
                /* Set up local variable slots */
                uint16_t slot = 0;
                
                if (captures_this) {
                    /* Slot 0 is 'this' */
                    slot = 1;
                }
                
                /* Allocate slots for captured variables (for static lambdas) */
                if (!captures_this) {
                    for (slist_t *cap = captures; cap; cap = cap->next) {
                        symbol_t *var_sym = (symbol_t *)cap->data;
                        if (var_sym && var_sym->name && var_sym->type) {
                            local_var_info_t *info = local_var_info_new(slot, var_sym->type->kind);
                            info->is_ref = (var_sym->type->kind == TYPE_CLASS || 
                                           var_sym->type->kind == TYPE_ARRAY);
                            hashtable_insert(lambda_mg->locals, var_sym->name, info);
                            
                            int size = (var_sym->type->kind == TYPE_LONG || 
                                       var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                            slot += size;
                        }
                    }
                }
                
                /* Allocate slots for SAM parameters */
                slist_t *children = expr->data.node.children;
                ast_node_t *params_node = children ? (ast_node_t *)children->data : NULL;
                slist_t *sam_param = sam_params;
                
                /* Handle lambda parameters based on AST structure */
                if (params_node && params_node->type == AST_IDENTIFIER && sam_param) {
                    /* Single unparenthesized parameter */
                    symbol_t *sp = (symbol_t *)sam_param->data;
                    type_kind_t kind = sp && sp->type ? sp->type->kind : TYPE_CLASS;
                    /* For wildcards, typevars, and unknowns, treat as reference type */
                    if (kind == TYPE_WILDCARD || kind == TYPE_TYPEVAR || kind == TYPE_UNKNOWN) {
                        kind = TYPE_CLASS;
                    }
                    local_var_info_t *info = local_var_info_new(slot, kind);
                    info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                    hashtable_insert(lambda_mg->locals, params_node->data.leaf.name, info);
                    
                    int size = (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
                    slot += size;
                } else if (params_node && params_node->type == AST_BINARY_EXPR) {
                    /* Multiple comma-separated parameters (not wrapped in PARENTHESIZED) */
                    ast_node_t *param_list[16];
                    int param_count = 0;
                    
                    ast_node_t *stack[32];
                    int stack_top = 0;
                    stack[stack_top++] = params_node;
                    
                    while (stack_top > 0) {
                        ast_node_t *node = stack[--stack_top];
                        if (!node) continue;
                        
                        if (node->type == AST_IDENTIFIER) {
                            if (param_count < 16) {
                                param_list[param_count++] = node;
                            }
                        } else if (node->type == AST_BINARY_EXPR) {
                            slist_t *bc = node->data.node.children;
                            if (bc && bc->next && stack_top < 32) {
                                stack[stack_top++] = (ast_node_t *)bc->next->data;
                            }
                            if (bc && stack_top < 32) {
                                stack[stack_top++] = (ast_node_t *)bc->data;
                            }
                        }
                    }
                    
                    slist_t *sp_node = sam_param;
                    for (int i = 0; i < param_count && sp_node; i++) {
                        ast_node_t *param_id = param_list[i];
                        symbol_t *sp = (symbol_t *)sp_node->data;
                        type_kind_t kind = sp && sp->type ? sp->type->kind : TYPE_CLASS;
                        local_var_info_t *info = local_var_info_new(slot, kind);
                        info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                        hashtable_insert(lambda_mg->locals, param_id->data.leaf.name, info);
                        
                        int size = (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
                        slot += size;
                        sp_node = sp_node->next;
                    }
                } else if (params_node && params_node->type == AST_PARENTHESIZED) {
                    /* Parenthesized parameter(s) - can be single or multiple with comma */
                    slist_t *inner_children = params_node->data.node.children;
                    if (inner_children) {
                        ast_node_t *inner = (ast_node_t *)inner_children->data;
                        if (inner && inner->type == AST_IDENTIFIER && sam_param) {
                            /* Single parenthesized parameter */
                            symbol_t *sp = (symbol_t *)sam_param->data;
                            type_kind_t kind = sp && sp->type ? sp->type->kind : TYPE_CLASS;
                            local_var_info_t *info = local_var_info_new(slot, kind);
                            info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                            hashtable_insert(lambda_mg->locals, inner->data.leaf.name, info);
                            
                            int size = (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
                            slot += size;
                        } else if (inner && inner->type == AST_BINARY_EXPR) {
                            /* Multiple comma-separated parameters inside parens */
                            ast_node_t *param_list[16];
                            int param_count = 0;
                            
                            ast_node_t *stack[32];
                            int stack_top = 0;
                            stack[stack_top++] = inner;
                            
                            while (stack_top > 0) {
                                ast_node_t *node = stack[--stack_top];
                                if (!node) continue;
                                
                                if (node->type == AST_IDENTIFIER) {
                                    if (param_count < 16) {
                                        param_list[param_count++] = node;
                                    }
                                } else if (node->type == AST_BINARY_EXPR) {
                                    slist_t *bc = node->data.node.children;
                                    if (bc && bc->next && stack_top < 32) {
                                        stack[stack_top++] = (ast_node_t *)bc->next->data;
                                    }
                                    if (bc && stack_top < 32) {
                                        stack[stack_top++] = (ast_node_t *)bc->data;
                                    }
                                }
                            }
                            
                            slist_t *sp_node = sam_param;
                            for (int i = 0; i < param_count && sp_node; i++) {
                                ast_node_t *param_id = param_list[i];
                                symbol_t *sp = (symbol_t *)sp_node->data;
                                type_kind_t kind = sp && sp->type ? sp->type->kind : TYPE_CLASS;
                                local_var_info_t *info = local_var_info_new(slot, kind);
                                info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                                hashtable_insert(lambda_mg->locals, param_id->data.leaf.name, info);
                                
                                int size = (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
                                slot += size;
                                sp_node = sp_node->next;
                            }
                        }
                    }
                } else if (params_node && params_node->type == AST_LAMBDA_PARAMS) {
                    /* Typed lambda parameters: (Type name, ...) or (var name, ...) */
                    slist_t *sp_node = sam_param;
                    for (slist_t *node = params_node->data.node.children; node; node = node->next) {
                        ast_node_t *param = (ast_node_t *)node->data;
                        if (param->type != AST_PARAMETER) continue;
                        
                        const char *param_name = param->data.node.name;
                        type_kind_t kind = TYPE_CLASS;
                        
                        /* Get type from semantic analysis or SAM */
                        if (param->sem_type) {
                            kind = param->sem_type->kind;
                        } else if (sp_node) {
                            symbol_t *sp = (symbol_t *)sp_node->data;
                            if (sp && sp->type) {
                                kind = sp->type->kind;
                            }
                        }
                        
                        local_var_info_t *info = local_var_info_new(slot, kind);
                        info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                        hashtable_insert(lambda_mg->locals, param_name, info);
                        
                        int size = (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
                        slot += size;
                        
                        if (sp_node) {
                            sp_node = sp_node->next;
                        }
                    }
                }
                
                lambda_mg->next_slot = slot;
                lambda_mg->max_locals = slot;
                
                /* Generate lambda body bytecode */
                ast_node_t *body = children && children->next ? 
                                   (ast_node_t *)children->next->data : NULL;
                if (body) {
                    if (body->type == AST_BLOCK) {
                        /* Block body - compile statements */
                        codegen_statement(lambda_mg, body);
                        
                        /* Add return instruction if block doesn't end with one */
                        /* For void-returning lambdas (e.g., Consumer), add RETURN */
                        /* For value-returning lambdas, the block should have explicit returns */
                        if (lambda_mg->code->length == 0 || 
                            (lambda_mg->last_opcode != OP_RETURN &&
                             lambda_mg->last_opcode != OP_IRETURN &&
                             lambda_mg->last_opcode != OP_LRETURN &&
                             lambda_mg->last_opcode != OP_FRETURN &&
                             lambda_mg->last_opcode != OP_DRETURN &&
                             lambda_mg->last_opcode != OP_ARETURN &&
                             lambda_mg->last_opcode != OP_ATHROW)) {
                            if (sam->type && sam->type->kind == TYPE_VOID) {
                                bc_emit_u1(lambda_mg->code, OP_RETURN);
                            } else if (sam->type) {
                                /* Non-void return type but no explicit return - this is an error,
                                 * but emit a return to avoid verifier errors */
                                switch (sam->type->kind) {
                                    case TYPE_INT:
                                    case TYPE_BOOLEAN:
                                    case TYPE_BYTE:
                                    case TYPE_CHAR:
                                    case TYPE_SHORT:
                                        bc_emit_u1(lambda_mg->code, OP_ICONST_0);
                                        bc_emit_u1(lambda_mg->code, OP_IRETURN);
                                        break;
                                    case TYPE_LONG:
                                        bc_emit_u1(lambda_mg->code, OP_LCONST_0);
                                        bc_emit_u1(lambda_mg->code, OP_LRETURN);
                                        break;
                                    case TYPE_FLOAT:
                                        bc_emit_u1(lambda_mg->code, OP_FCONST_0);
                                        bc_emit_u1(lambda_mg->code, OP_FRETURN);
                                        break;
                                    case TYPE_DOUBLE:
                                        bc_emit_u1(lambda_mg->code, OP_DCONST_0);
                                        bc_emit_u1(lambda_mg->code, OP_DRETURN);
                                        break;
                                    default:
                                        bc_emit_u1(lambda_mg->code, OP_ACONST_NULL);
                                        bc_emit_u1(lambda_mg->code, OP_ARETURN);
                                        break;
                                }
                            } else {
                                bc_emit_u1(lambda_mg->code, OP_RETURN);
                            }
                        }
                    } else {
                        /* Expression body - evaluate and return */
                        codegen_expr(lambda_mg, body, cp);
                        
                        /* Emit appropriate return instruction */
                        if (sam->type) {
                            switch (sam->type->kind) {
                                case TYPE_VOID:
                                    bc_emit_u1(lambda_mg->code, OP_RETURN);
                                    break;
                                case TYPE_INT:
                                case TYPE_BOOLEAN:
                                case TYPE_BYTE:
                                case TYPE_CHAR:
                                case TYPE_SHORT:
                                    bc_emit_u1(lambda_mg->code, OP_IRETURN);
                                    mg_pop(lambda_mg, 1);
                                    break;
                                case TYPE_LONG:
                                    bc_emit_u1(lambda_mg->code, OP_LRETURN);
                                    mg_pop(lambda_mg, 2);
                                    break;
                                case TYPE_FLOAT:
                                    bc_emit_u1(lambda_mg->code, OP_FRETURN);
                                    mg_pop(lambda_mg, 1);
                                    break;
                                case TYPE_DOUBLE:
                                    bc_emit_u1(lambda_mg->code, OP_DRETURN);
                                    mg_pop(lambda_mg, 2);
                                    break;
                                default:
                                    bc_emit_u1(lambda_mg->code, OP_ARETURN);
                                    mg_pop(lambda_mg, 1);
                                    break;
                            }
                        } else {
                            bc_emit_u1(lambda_mg->code, OP_ARETURN);
                            mg_pop(lambda_mg, 1);
                        }
                    }
                }
                
                /* Finalize lambda method - set max_stack and max_locals */
                lambda_mg->code->max_stack = lambda_mg->max_stack > 0 ? lambda_mg->max_stack : 1;
                lambda_mg->code->max_locals = lambda_mg->max_locals > 0 ? lambda_mg->max_locals : slot;
                
                lambda_method->code = lambda_mg->code;
                lambda_mg->code = NULL;  /* Transfer ownership */
                
                /* Transfer stackmap to the method info */
                lambda_method->stackmap = lambda_mg->stackmap;
                lambda_mg->stackmap = NULL;  /* Transfer ownership */
                
                if (!cg->methods) {
                    cg->methods = slist_new(lambda_method);
                } else {
                    slist_append(cg->methods, lambda_method);
                }
                
                /* Free the method generator locals */
                hashtable_free(lambda_mg->locals);
                free(lambda_mg);
                
                /* Now generate the invokedynamic call site */
                
                /* Build invocation type descriptor: (captures)LFunctionalInterface; */
                string_t *invoke_desc = string_new("(");
                
                /* Add capture types */
                if (captures_this) {
                    /* Capture this */
                    string_append(invoke_desc, "L");
                    string_append(invoke_desc, cg->internal_name);
                    string_append(invoke_desc, ";");
                }
                for (slist_t *cap = captures; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (var_sym && var_sym->type) {
                        char *cap_desc = type_to_descriptor(var_sym->type);
                        string_append(invoke_desc, cap_desc);
                        free(cap_desc);
                    }
                }
                
                string_append(invoke_desc, ")L");
                char *iface_internal = class_to_internal_name(iface_sym->qualified_name ? 
                                                               iface_sym->qualified_name : 
                                                               iface_sym->name);
                string_append(invoke_desc, iface_internal);
                string_append(invoke_desc, ";");
                
                /* Create name_and_type for invokedynamic */
                uint16_t indy_nat = cp_add_name_and_type(cg->cp, sam_name, invoke_desc->str);
                
                /* Create method type entries for bootstrap args */
                uint16_t sam_mt = cp_add_method_type(cg->cp, sam_descriptor);
                uint16_t spec_mt = cp_add_method_type(cg->cp, sam_descriptor);
                
                /* Create method handle for implementation method */
                uint16_t impl_ref;
                if (captures_this) {
                    impl_ref = cp_add_methodref(cg->cp, cg->internal_name, 
                                                lambda_method_name, impl_desc->str);
                } else {
                    impl_ref = cp_add_methodref(cg->cp, cg->internal_name,
                                                lambda_method_name, impl_desc->str);
                }
                
                method_handle_kind_t mh_kind = captures_this ? REF_invokeSpecial : REF_invokeStatic;
                uint16_t impl_mh = cp_add_method_handle(cg->cp, mh_kind, impl_ref);
                
                /* Add bootstrap method with arguments */
                uint16_t bsm_args[3] = { sam_mt, impl_mh, spec_mt };
                uint16_t metafactory_mh = cg->bootstrap_methods->methods[metafactory_idx].method_handle_index;
                int bsm_idx = bootstrap_methods_add(cg->bootstrap_methods, metafactory_mh, bsm_args, 3);
                if (bsm_idx < 0) {
                    fprintf(stderr, "codegen: failed to add bootstrap method\n");
                    string_free(impl_desc, true);
                    string_free(invoke_desc, true);
                    free(sam_descriptor);
                    free(iface_internal);
                    return false;
                }
                
                /* Create InvokeDynamic constant pool entry */
                uint16_t indy_idx = cp_add_invoke_dynamic(cg->cp, (uint16_t)bsm_idx, indy_nat);
                
                /* Push captured values onto stack */
                if (captures_this) {
                    bc_emit_u1(mg->code, OP_ALOAD_0);
                    mg_push_null(mg);  /* Object reference */
                }
                for (slist_t *cap = captures; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (var_sym && var_sym->name) {
                        uint16_t var_slot = mg_get_local(mg, var_sym->name);
                        type_kind_t kind = var_sym->type ? var_sym->type->kind : TYPE_CLASS;
                        mg_emit_load_local(mg, var_slot, kind);
                    }
                }
                
                /* Emit invokedynamic instruction */
                mg_emit_invokedynamic(mg, indy_idx);
                
                /* Clean up */
                string_free(impl_desc, true);
                string_free(invoke_desc, true);
                free(sam_descriptor);
                free(iface_internal);
                
                /* invokedynamic leaves functional interface instance on stack */
                /* Adjust stack: pushed captures, got 1 reference back */
                int capture_count = captures_this ? 1 : 0;
                for (slist_t *cap = captures; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (var_sym && var_sym->type) {
                        int size = (var_sym->type->kind == TYPE_LONG || 
                                   var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                        capture_count += size;
                    }
                }
                mg_pop_typed(mg, capture_count);  /* Pop captures */
                mg_push(mg, 1);  /* Push result */
                
                cg->uses_invokedynamic = true;
                return true;
            }
        
        case AST_METHOD_REF:
            {
                /* Method reference - uses invokedynamic like lambda
                 * 
                 * Semantic analysis has set:
                 *   - sem_type: the target functional interface type
                 *   - sem_symbol: the resolved method/constructor
                 *   - children[0]: target type/expression
                 *   - data.node.name: method name (or "new")
                 *   - modifiers: flags for ref kind (STATIC, PRIVATE=bound, ABSTRACT=ctor)
                 */
                
                type_t *func_interface = expr->sem_type;
                if (!func_interface || func_interface->kind != TYPE_CLASS) {
                    fprintf(stderr, "codegen: method reference without target type\n");
                    return false;
                }
                
                symbol_t *resolved_method = expr->sem_symbol;
                
                slist_t *children = expr->data.node.children;
                if (!children) {
                    fprintf(stderr, "codegen: method reference without target\n");
                    return false;
                }
                ast_node_t *target_node = (ast_node_t *)children->data;
                const char *method_name = expr->data.node.name;
                
                /* Check for array constructor reference (Type[]::new) */
                bool is_array_constructor = (expr->data.node.flags & MOD_NATIVE) != 0;
                
                if (!resolved_method && !is_array_constructor) {
                    fprintf(stderr, "codegen: method reference without resolved method\n");
                    return false;
                }
                
                symbol_t *iface_sym = func_interface->data.class_type.symbol;
                if (!iface_sym) {
                    fprintf(stderr, "codegen: method reference without interface symbol\n");
                    return false;
                }
                
                /* Get SAM from functional interface using hashtable_foreach */
                symbol_t *sam = NULL;
                if (iface_sym->data.class_data.members && 
                    iface_sym->data.class_data.members->symbols) {
                    mref_sam_search_t search = { NULL, 0 };
                    hashtable_foreach(iface_sym->data.class_data.members->symbols, 
                                      mref_find_sam_fn, &search);
                    if (search.abstract_count == 1) {
                        sam = search.sam;
                    }
                }
                if (!sam) {
                    fprintf(stderr, "codegen: cannot find SAM in functional interface\n");
                    return false;
                }
                
                class_gen_t *cg = mg->class_gen;
                
                /* Ensure bootstrap methods table exists */
                if (!cg->bootstrap_methods) {
                    cg->bootstrap_methods = bootstrap_methods_new();
                }
                
                /* Get or create the LambdaMetafactory bootstrap method */
                int metafactory_idx = cg_ensure_lambda_metafactory(cg);
                if (metafactory_idx < 0) {
                    fprintf(stderr, "codegen: failed to set up LambdaMetafactory\n");
                    return false;
                }
                
                /* Determine method reference kind from flags set by semantic analysis */
                bool is_constructor = (expr->data.node.flags & MOD_ABSTRACT) != 0;
                bool is_static = (expr->data.node.flags & MOD_STATIC) != 0;
                bool is_bound = (expr->data.node.flags & MOD_PRIVATE) != 0;
                
                /* Handle array constructor references specially */
                if (is_array_constructor) {
                    /* Array constructor reference: Type[]::new
                     * Generate a synthetic method that creates the array, then use invokedynamic */
                    
                    /* Get the array type from target_node->sem_type */
                    type_t *array_type = target_node->sem_type;
                    if (!array_type || array_type->kind != TYPE_ARRAY) {
                        fprintf(stderr, "codegen: array constructor ref without array type\n");
                        return false;
                    }
                    
                    /* Get element type */
                    type_t *elem_type = array_type->data.array_type.element_type;
                    char *elem_desc = type_to_descriptor(elem_type);
                    
                    /* Build array type descriptor */
                    char array_desc[256];
                    snprintf(array_desc, sizeof(array_desc), "[%s", elem_desc);
                    
                    /* Generate unique synthetic method name */
                    char synth_method_name[256];
                    const char *enclosing_name = mg->method ? mg->method->name : "main";
                    snprintf(synth_method_name, sizeof(synth_method_name), 
                             "lambda$%s$%d", enclosing_name, cg->lambda_counter++);
                    
                    /* Build synthetic method descriptor: (I)[<ElementType> */
                    char synth_desc[256];
                    snprintf(synth_desc, sizeof(synth_desc), "(I)%s", array_desc);
                    
                    /* Generate the synthetic array factory method */
                    method_info_gen_t *synth_method = calloc(1, sizeof(method_info_gen_t));
                    synth_method->access_flags = ACC_PRIVATE | ACC_SYNTHETIC | ACC_STATIC;
                    synth_method->name_index = cp_add_utf8(cg->cp, synth_method_name);
                    synth_method->descriptor_index = cp_add_utf8(cg->cp, synth_desc);
                    
                    /* Create method generator for synthetic method */
                    method_gen_t *synth_mg = calloc(1, sizeof(method_gen_t));
                    synth_mg->code = bytecode_new();
                    synth_mg->cp = cg->cp;
                    synth_mg->class_gen = cg;
                    synth_mg->locals = hashtable_new();
                    synth_mg->is_static = true;
                    
                    /* Slot 0 is the int size parameter */
                    local_var_info_t *size_info = local_var_info_new(0, TYPE_INT);
                    hashtable_insert(synth_mg->locals, "size", size_info);
                    synth_mg->code->max_locals = 1;
                    
                    /* Generate array creation bytecode:
                     * iload_0      ; load size
                     * anewarray/newarray
                     * areturn
                     */
                    bc_emit_u1(synth_mg->code, OP_ILOAD_0);
                    
                    if (elem_type->kind == TYPE_CLASS || elem_type->kind == TYPE_ARRAY) {
                        /* Object array: anewarray */
                        char *elem_internal;
                        if (elem_type->kind == TYPE_CLASS) {
                            const char *elem_name = elem_type->data.class_type.name;
                            if (elem_type->data.class_type.symbol) {
                                elem_name = elem_type->data.class_type.symbol->qualified_name ?
                                           elem_type->data.class_type.symbol->qualified_name :
                                           elem_type->data.class_type.symbol->name;
                            }
                            elem_internal = class_to_internal_name(elem_name);
                        } else {
                            /* Nested array type - use the full descriptor */
                            elem_internal = strdup(elem_desc);
                        }
                        uint16_t class_idx = cp_add_class(cg->cp, elem_internal);
                        bc_emit_u1(synth_mg->code, OP_ANEWARRAY);
                        bc_emit_u2(synth_mg->code, class_idx);
                        free(elem_internal);
                    } else {
                        /* Primitive array: newarray */
                        bc_emit_u1(synth_mg->code, OP_NEWARRAY);
                        int atype = type_kind_to_atype(elem_type->kind);
                        if (atype < 0) atype = 10;  /* Default to T_INT */
                        bc_emit_u1(synth_mg->code, atype);
                    }
                    
                    bc_emit_u1(synth_mg->code, OP_ARETURN);
                    synth_mg->code->max_stack = 1;
                    
                    /* Finalize synthetic method */
                    synth_method->code = synth_mg->code;
                    synth_mg->code = NULL;
                    
                    /* Add to class methods */
                    if (!cg->methods) {
                        cg->methods = slist_new(synth_method);
                    } else {
                        slist_append(cg->methods, synth_method);
                    }
                    
                    method_gen_free(synth_mg);
                    
                    /* Now create the invokedynamic call site pointing to the synthetic method */
                    char *iface_internal = class_to_internal_name(
                        iface_sym->qualified_name ? iface_sym->qualified_name : iface_sym->name);
                    char *sam_descriptor = method_to_descriptor(sam);
                    
                    /* Create methodref for synthetic method */
                    uint16_t synth_ref = cp_add_methodref(cg->cp, cg->internal_name,
                                                          synth_method_name, synth_desc);
                    
                    /* Create method handle for synthetic method */
                    uint16_t impl_handle = cp_add_method_handle(cg->cp, REF_invokeStatic, synth_ref);
                    
                    /* Build invocation descriptor: ()L<FunctionalInterface>; */
                    char invoke_desc[256];
                    snprintf(invoke_desc, sizeof(invoke_desc), "()L%s;", iface_internal);
                    
                    /* Create name_and_type for SAM */
                    uint16_t nat_index = cp_add_name_and_type(cg->cp, sam->name, invoke_desc);
                    
                    /* Bootstrap arguments */
                    uint16_t erased_type = cp_add_method_type(cg->cp, sam_descriptor);
                    uint16_t specialized_type = cp_add_method_type(cg->cp, sam_descriptor);
                    
                    uint16_t bsm_args[3] = { erased_type, impl_handle, specialized_type };
                    int bsm_idx = bootstrap_methods_add(cg->bootstrap_methods,
                        cg->bootstrap_methods->methods[metafactory_idx].method_handle_index,
                        bsm_args, 3);
                    
                    /* Create InvokeDynamic entry */
                    uint16_t indy_index = cp_add_invoke_dynamic(cg->cp, (uint16_t)bsm_idx, nat_index);
                    
                    /* Emit invokedynamic */
                    bc_emit_u1(mg->code, OP_INVOKEDYNAMIC);
                    bc_emit_u2(mg->code, indy_index);
                    bc_emit_u1(mg->code, 0);
                    bc_emit_u1(mg->code, 0);
                    
                    mg_push(mg, 1);
                    
                    free(elem_desc);
                    free(iface_internal);
                    free(sam_descriptor);
                    
                    cg->uses_invokedynamic = true;
                    return true;
                }
                
                /* Get target class info */
                type_t *target_type = target_node->sem_type;
                symbol_t *target_class_sym = NULL;
                if (target_type && target_type->kind == TYPE_CLASS) {
                    target_class_sym = target_type->data.class_type.symbol;
                }
                if (!target_class_sym) {
                    fprintf(stderr, "codegen: method reference without target class\n");
                    return false;
                }
                
                /* Build internal class name */
                char *target_internal = class_to_internal_name(
                    target_class_sym->qualified_name ? 
                    target_class_sym->qualified_name : target_class_sym->name);
                char *iface_internal = class_to_internal_name(
                    iface_sym->qualified_name ? iface_sym->qualified_name : iface_sym->name);
                
                /* Build SAM descriptor */
                char *sam_descriptor = method_to_descriptor(sam);
                
                /* Build the method descriptor for the referenced method
                 * For constructors, the descriptor must have void return type */
                char *method_descriptor;
                if (is_constructor) {
                    /* Constructor descriptor: params from resolved_method but return type is void */
                    string_t *desc = string_new("(");
                    for (slist_t *node = resolved_method->data.method_data.parameters; node; node = node->next) {
                        symbol_t *param = (symbol_t *)node->data;
                        if (param && param->type) {
                            char *param_desc = type_to_descriptor(param->type);
                            string_append(desc, param_desc);
                            free(param_desc);
                        }
                    }
                    string_append(desc, ")V");  /* Constructor always returns void */
                    method_descriptor = string_free(desc, false);
                } else {
                    method_descriptor = method_to_descriptor(resolved_method);
                }
                
                /* For bound method references, we need to push the receiver first */
                if (is_bound) {
                    codegen_expr(mg, target_node, cp);
                }
                
                /* Build invocation type descriptor:
                 * - Static: ()L<FunctionalInterface>;
                 * - Bound: (L<TargetClass>;)L<FunctionalInterface>;  
                 * - Unbound/ctor: ()L<FunctionalInterface>;
                 */
                char invoke_desc[1024];
                if (is_bound) {
                    snprintf(invoke_desc, sizeof(invoke_desc), "(L%s;)L%s;",
                             target_internal, iface_internal);
                } else {
                    snprintf(invoke_desc, sizeof(invoke_desc), "()L%s;", iface_internal);
                }
                
                /* Determine method handle kind */
                method_handle_kind_t handle_kind;
                const char *actual_method_name = method_name;
                
                if (is_constructor) {
                    handle_kind = REF_newInvokeSpecial;
                    actual_method_name = "<init>";
                } else if (is_static) {
                    handle_kind = REF_invokeStatic;
                } else {
                    /* For both bound and unbound instance methods, use invokeVirtual */
                    handle_kind = REF_invokeVirtual;
                }
                
                /* Create method reference in constant pool */
                uint16_t method_ref = cp_add_methodref(cp, target_internal,
                                                        actual_method_name,
                                                        method_descriptor);
                if (method_ref == 0) {
                    fprintf(stderr, "codegen: failed to add method ref to constant pool\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Create method handle for the implementation */
                uint16_t impl_handle = cp_add_method_handle(cp, handle_kind, method_ref);
                if (impl_handle == 0) {
                    fprintf(stderr, "codegen: failed to add method handle\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Create name_and_type entry for SAM */
                uint16_t nat_index = cp_add_name_and_type(cp, sam->name, invoke_desc);
                if (nat_index == 0) {
                    fprintf(stderr, "codegen: failed to add name_and_type\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Bootstrap arguments for LambdaMetafactory.metafactory:
                 * 1. MethodType - erased SAM descriptor
                 * 2. MethodHandle - implementation method handle
                 * 3. MethodType - specialized SAM descriptor
                 */
                uint16_t erased_type = cp_add_method_type(cp, sam_descriptor);
                uint16_t specialized_type = cp_add_method_type(cp, sam_descriptor);
                if (erased_type == 0 || specialized_type == 0) {
                    fprintf(stderr, "codegen: failed to add method types\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Add bootstrap method with arguments */
                uint16_t bsm_args[3] = { erased_type, impl_handle, specialized_type };
                int bsm_idx = bootstrap_methods_add(cg->bootstrap_methods,
                    cg->bootstrap_methods->methods[metafactory_idx].method_handle_index,
                    bsm_args, 3);
                if (bsm_idx < 0) {
                    fprintf(stderr, "codegen: failed to add bootstrap method\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Create InvokeDynamic constant pool entry */
                uint16_t indy_index = cp_add_invoke_dynamic(cp, (uint16_t)bsm_idx, nat_index);
                if (indy_index == 0) {
                    fprintf(stderr, "codegen: failed to add invokedynamic entry\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    return false;
                }
                
                /* Emit invokedynamic instruction */
                bc_emit_u1(mg->code, OP_INVOKEDYNAMIC);
                bc_emit_u2(mg->code, indy_index);
                bc_emit_u1(mg->code, 0);  /* Reserved bytes */
                bc_emit_u1(mg->code, 0);
                
                /* Stack effect: for bound, we consumed receiver (-1), produce interface (+1) = 0
                 * For unbound/static/ctor, we just produce interface (+1) = +1 */
                if (is_bound) {
                    /* Already consumed receiver, just produced interface - net 0 but we pushed it earlier */
                    /* mg_pop already done by consuming receiver; result already on stack */
                } else {
                    mg_push(mg, 1);
                }
                
                free(target_internal);
                free(iface_internal);
                free(sam_descriptor);
                free(method_descriptor);
                
                cg->uses_invokedynamic = true;
                return true;
            }
        
        case AST_CLASS_LITERAL:
            {
                /* Type.class - primitive types use WrapperClass.TYPE, reference types use ldc */
                slist_t *children = expr->data.node.children;
                if (!children) {
                    fprintf(stderr, "codegen: AST_CLASS_LITERAL has no type child\n");
                    return false;
                }
                ast_node_t *type_node = (ast_node_t *)children->data;
                
                /* Check for primitive types - need to use WrapperClass.TYPE */
                if (type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = type_node->data.leaf.name;
                    const char *wrapper_class = NULL;
                    
                    if (strcmp(prim_name, "boolean") == 0) wrapper_class = "java/lang/Boolean";
                    else if (strcmp(prim_name, "byte") == 0) wrapper_class = "java/lang/Byte";
                    else if (strcmp(prim_name, "char") == 0) wrapper_class = "java/lang/Character";
                    else if (strcmp(prim_name, "short") == 0) wrapper_class = "java/lang/Short";
                    else if (strcmp(prim_name, "int") == 0) wrapper_class = "java/lang/Integer";
                    else if (strcmp(prim_name, "long") == 0) wrapper_class = "java/lang/Long";
                    else if (strcmp(prim_name, "float") == 0) wrapper_class = "java/lang/Float";
                    else if (strcmp(prim_name, "double") == 0) wrapper_class = "java/lang/Double";
                    else if (strcmp(prim_name, "void") == 0) wrapper_class = "java/lang/Void";
                    
                    if (wrapper_class) {
                        /* Use getstatic WrapperClass.TYPE */
                        uint16_t field_ref = cp_add_fieldref(cp, wrapper_class, "TYPE", "Ljava/lang/Class;");
                        bc_emit(mg->code, OP_GETSTATIC);
                        bc_emit_u2(mg->code, field_ref);
                        mg_push_object(mg, "java/lang/Class");
                        return true;
                    }
                }
                
                /* Reference type or array - use ldc with CONSTANT_Class */
                char *descriptor = ast_type_to_descriptor(type_node);
                
                /* For class literals, convert descriptor to internal form
                 * e.g., "Ljava/lang/String;" becomes "java/lang/String" */
                char *class_name;
                if (descriptor[0] == 'L' && descriptor[strlen(descriptor) - 1] == ';') {
                    /* Reference type - strip L and ; */
                    class_name = strndup(descriptor + 1, strlen(descriptor) - 2);
                } else {
                    /* Array type - use descriptor as-is (e.g., "[I" or "[Ljava/lang/String;") */
                    class_name = strdup(descriptor);
                }
                
                uint16_t class_idx = cp_add_class(cp, class_name);
                
                /* Use ldc or ldc_w depending on index */
                if (class_idx < 256) {
                    bc_emit(mg->code, OP_LDC);
                    bc_emit_u1(mg->code, (uint8_t)class_idx);
                } else {
                    bc_emit(mg->code, OP_LDC_W);
                    bc_emit_u2(mg->code, class_idx);
                }
                mg_push_object(mg, "java/lang/Class");
                
                free(class_name);
                free(descriptor);
                return true;
            }
        
        /* TODO: Implement other expression types */
        default:
            fprintf(stderr, "codegen: unhandled expression type: %s\n",
                    ast_type_name(expr->type));
            return false;
    }
}

bool codegen_expression(method_gen_t *mg, ast_node_t *expr)
{
    return codegen_expr(mg, expr, mg->cp);
}

