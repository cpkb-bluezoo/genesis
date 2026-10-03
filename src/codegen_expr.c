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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
    if (!descriptor || !descriptor[0]) {
        return NULL;
    }
    
    if (descriptor[0] == '[') {
        /* Array type - use descriptor as-is */
        return strdup(descriptor);
    } else if (descriptor[0] == 'L') {
        /* Object type - strip L prefix and ; suffix */
        size_t len = strlen(descriptor);
        if (len < 3) {
            return NULL;
        }  /* At least "L;" */
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
    if (!mg || !descriptor) {
        return;
    }
    
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
 * Build a JVM array-type descriptor ("[[I", "[Ljava/lang/String;", ...) from
 * a base (element) type kind/class and a dimension count. Used to give
 * array-element-load codegen a correctly-typed stackmap replacement when the
 * load result is itself a sub-array (jagged/multi-dimensional access).
 */
static char *build_array_descriptor(type_kind_t base_kind, const char *base_class, int dims)
{
    char *heap_base = NULL;
    const char *base_desc;

    switch (base_kind) {
        case TYPE_BOOLEAN: base_desc = "Z"; break;
        case TYPE_BYTE:    base_desc = "B"; break;
        case TYPE_CHAR:    base_desc = "C"; break;
        case TYPE_SHORT:   base_desc = "S"; break;
        case TYPE_INT:     base_desc = "I"; break;
        case TYPE_LONG:    base_desc = "J"; break;
        case TYPE_FLOAT:   base_desc = "F"; break;
        case TYPE_DOUBLE:  base_desc = "D"; break;
        case TYPE_CLASS:
            {
                char *internal = class_to_internal_name(base_class ? base_class : "java.lang.Object");
                size_t len = strlen(internal) + 3;
                heap_base = malloc(len);
                snprintf(heap_base, len, "L%s;", internal);
                free(internal);
                base_desc = heap_base;
            }
            break;
        default:
            base_desc = "Ljava/lang/Object;";
            break;
    }

    if (dims < 1) {
        dims = 1;
    }
    size_t len = strlen(base_desc) + (size_t)dims + 1;
    char *desc = malloc(len);
    char *p = desc;
    for (int i = 0; i < dims; i++) {
        *p++ = '[';
    }
    strcpy(p, base_desc);
    free(heap_base);
    return desc;
}

/**
 * Emit bytecode to load the nearest enclosing instance of type target_owner,
 * walking the this$0 chain from mg's current class as many levels as needed.
 * Defined below (with full doc comment); forward-declared here so earlier
 * call sites in this file (e.g. codegen_identifier's enclosing-field-read
 * path) can use it too.
 */
static void codegen_load_enclosing_this(method_gen_t *mg, const_pool_t *cp, symbol_t *target_owner);

/**
 * Find (or register) the synthetic static accessor 'enclosing' must emit so
 * a nested class can read 'field' (declared in the superclass
 * 'field_owner') without a direct getfield - see pending_field_accessor_t's
 * own comment (genesis.h) for why this is needed at all. Idempotent: the
 * discovery pass and the real codegen pass both call this for the same
 * access, and must agree on the same name, so a repeat lookup for the same
 * field just returns the already-assigned one.
 */
static const char *get_or_create_field_accessor(symbol_t *enclosing, symbol_t *field_owner,
                                                 symbol_t *field)
{
    int index = 0;
    for (slist_t *p = enclosing->data.class_data.pending_field_accessors; p; p = p->next, index++) {
        pending_field_accessor_t *acc = (pending_field_accessor_t *)p->data;
        if (acc->field == field) {
            return acc->accessor_name;
        }
    }

    pending_field_accessor_t *acc = calloc(1, sizeof(pending_field_accessor_t));
    acc->field = field;
    acc->field_owner = field_owner;
    char name_buf[32];
    snprintf(name_buf, sizeof(name_buf), "access$%d", index);
    acc->accessor_name = strdup(name_buf);

    if (!enclosing->data.class_data.pending_field_accessors) {
        enclosing->data.class_data.pending_field_accessors = slist_new(acc);
    } else {
        slist_append(enclosing->data.class_data.pending_field_accessors, acc);
    }
    return acc->accessor_name;
}

/**
 * Emit ++/-- on an instance field of the current object (this.field).
 * field_owner_internal is the class that declares the field (may be a superclass).
 */
static bool codegen_this_instance_field_incdec(method_gen_t *mg, const_pool_t *cp,
                                               const char *field_owner_internal,
                                               const char *field_name, const char *field_desc,
                                               bool is_post, bool is_inc)
{
    uint8_t add_op = OP_IADD;
    uint8_t sub_op = OP_ISUB;
    uint8_t const1_op = OP_ICONST_1;
    bool is_wide = false;

    switch (field_desc[0]) {
    case 'J':
        add_op = OP_LADD;
        sub_op = OP_LSUB;
        const1_op = OP_LCONST_1;
        is_wide = true;
        break;
    case 'D':
        add_op = OP_DADD;
        sub_op = OP_DSUB;
        const1_op = OP_DCONST_1;
        is_wide = true;
        break;
    case 'F':
        add_op = OP_FADD;
        sub_op = OP_FSUB;
        const1_op = OP_FCONST_1;
        break;
    default:
        break;
    }

    uint16_t fieldref = cp_add_fieldref(cp, field_owner_internal, field_name, field_desc);

    bc_emit(mg->code, OP_ALOAD_0);
    mg_push_object(mg, mg->class_gen->internal_name);

    if (is_post) {
        bc_emit(mg->code, OP_DUP);
        mg_push_object(mg, mg->class_gen->internal_name);
        bc_emit(mg->code, OP_GETFIELD);
        bc_emit_u2(mg->code, fieldref);
        mg_pop_typed(mg, 1);
        if (is_wide) {
            mg_push_long(mg);
        } else {
            mg_push_int(mg);
        }
        if (is_wide) {
            bc_emit(mg->code, OP_DUP2_X1);
            mg_push(mg, 2);
        } else {
            bc_emit(mg->code, OP_DUP_X1);
            mg_push_int(mg);
        }
        bc_emit(mg->code, const1_op);
        mg_push(mg, is_wide ? 2 : 1);
        bc_emit(mg->code, is_inc ? add_op : sub_op);
        mg_pop_typed(mg, is_wide ? 2 : 1);
        bc_emit(mg->code, OP_PUTFIELD);
        bc_emit_u2(mg->code, fieldref);
        mg_pop_typed(mg, is_wide ? 3 : 2);
    } else {
        bc_emit(mg->code, OP_DUP);
        mg_push_object(mg, mg->class_gen->internal_name);
        bc_emit(mg->code, OP_GETFIELD);
        bc_emit_u2(mg->code, fieldref);
        mg_pop_typed(mg, 1);
        if (is_wide) {
            mg_push_long(mg);
        } else {
            mg_push_int(mg);
        }
        bc_emit(mg->code, const1_op);
        mg_push(mg, is_wide ? 2 : 1);
        bc_emit(mg->code, is_inc ? add_op : sub_op);
        mg_pop_typed(mg, is_wide ? 2 : 1);
        if (is_wide) {
            bc_emit(mg->code, OP_DUP2_X1);
            mg_push(mg, 2);
        } else {
            bc_emit(mg->code, OP_DUP_X1);
            mg_push_int(mg);
        }
        bc_emit(mg->code, OP_PUTFIELD);
        bc_emit_u2(mg->code, fieldref);
        mg_pop_typed(mg, is_wide ? 3 : 2);
    }
    return true;
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
 * Finds both default methods and abstract interface methods.
 */
static symbol_t *lookup_method_in_interfaces(symbol_t *iface, const char *method_name, 
                                              symbol_t **owner_class)
{
    if (!iface) {
        return NULL;
    }
    
    /* Check direct members of this interface */
    if (iface->data.class_data.members) {
        symbol_t *method = lookup_method_by_name(iface->data.class_data.members, method_name);
        if (method && method->kind == SYM_METHOD) {
            /* Found a method in this interface (default or abstract) */
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
            static __thread char buf[128];
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

    /* invokestatic here always consumes exactly the primitive value on top
     * of the stack (1 or 2 words depending on category) and pushes the
     * boxed wrapper reference (1 word) - pop the primitive's stackmap
     * entry/entries and push a properly-typed wrapper-class replacement
     * via mg_push_object(), rather than leaving the primitive's type
     * lingering in mg->stackmap (or, for long/double, only correcting the
     * word count via mg_pop_typed() without ever replacing the TYPE).
     * Mirrors the equivalent fix already applied to emit_unboxing() above.
     * Without this, a later branch (e.g. a ternary building a further
     * call argument) while the boxed value still sits deeper on the
     * stack computes its StackMapTable frame from the stale primitive
     * type: VerifyError "Inconsistent stackmap frames ... Type
     * 'java/lang/Integer' ... is not assignable to integer". Confirmed
     * against gumdrop's own
     * ContentTypeParser.processRawParamsFromSlices()'s
     * "ranges.put(index, new int[] {..., quoted ? 1 : 0})" on a
     * TreeMap<Integer, int[]>. */
    mg_pop_typed(mg, (prim_kind == TYPE_LONG || prim_kind == TYPE_DOUBLE) ? 2 : 1);
    mg_push_object(mg, wrapper_class);

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

    switch (target_prim) {
        case TYPE_INT:
            method = "intValue";
            descriptor = "()I";
            break;
        case TYPE_LONG:
            method = "longValue";
            descriptor = "()J";
            break;
        case TYPE_DOUBLE:
            method = "doubleValue";
            descriptor = "()D";
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

    /* invokevirtual here always consumes exactly the wrapper reference on
     * top of the stack (1 word, 1 stackmap entry) and pushes the unboxed
     * primitive result - pop that one entry and push a properly-typed
     * replacement via the type-aware mg_push_*() helpers (which update
     * both mg->stack_depth and mg->stackmap together), rather than
     * blindly bumping the raw stack_depth counter alone. Without this,
     * the wrapper's own stackmap entry was left in place uncorrected -
     * for a category-1 result (int/float/etc) its wrong TYPE (still the
     * wrapper class) lingered; for a category-2 result (long/double) one
     * of its two required tracking slots was never even added, since the
     * old code's raw mg_push() call touched only the word counter, never
     * mg->stackmap - corrupting every stack-map frame subsequently
     * recorded in the same method (VerifyError: "Inconsistent stackmap
     * frames"/"StackMapTable error"). */
    mg_pop_typed(mg, 1);
    switch (target_prim) {
        case TYPE_LONG:   mg_push_long(mg); break;
        case TYPE_DOUBLE: mg_push_double(mg); break;
        case TYPE_FLOAT:  mg_push_float(mg); break;
        default:          mg_push_int(mg); break;  /* int/byte/short/char/boolean */
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
    if (!arg) {
        return TYPE_INT;
    }
    
    /* Check semantic type first */
    if (arg->sem_type) {
        return arg->sem_type->kind;
    }
    
    /* Handle literals - they may not have sem_type set */
    if (arg->type == AST_LITERAL) {
        switch (arg->data.leaf.token_type) {
            case TOK_LONG_LITERAL:
                return TYPE_LONG;
            case TOK_FLOAT_LITERAL:
                return TYPE_FLOAT;
            case TOK_DOUBLE_LITERAL:
                return TYPE_DOUBLE;
            case TOK_TRUE:
            case TOK_FALSE:
                return TYPE_BOOLEAN;
            case TOK_CHAR_LITERAL:
                return TYPE_CHAR;
            case TOK_NULL:
                return TYPE_NULL;
            case TOK_STRING_LITERAL:
            case TOK_TEXT_BLOCK:
                return TYPE_CLASS;
            default:
                return TYPE_INT;
        }
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
    
    /* Handle method calls - check return type from sem_symbol */
    if (arg->type == AST_METHOD_CALL) {
        if (arg->sem_symbol && arg->sem_symbol->kind == SYM_METHOD && arg->sem_symbol->type) {
            return arg->sem_symbol->type->kind;
        }
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

    /* AST_THIS_EXPR nodes carry no sem_type (confirmed by an existing
     * comment elsewhere in this file, at the receiver-type-inference call
     * site for field access) - so without this case, "this" fell through
     * every other branch below straight to the function's own final
     * "default to TYPE_INT" fallback, same as an unresolvable expression.
     * A caller deciding whether an argument needs boxing for a reference-
     * typed parameter (e.g. Map.put(key, this)) then wrongly treated
     * "this" as a primitive int needing Integer.valueOf() before the
     * call - producing an invokestatic to Integer.valueOf(I) right
     * before passing an object reference, rejected by the verifier
     * (VerifyError: "Bad type on operand stack", the reference type not
     * assignable to int). Confirmed against gumdrop's own
     * DNSResolverIpv6FallbackTest, whose test-only anonymous
     * DnsClientTransport implementation does exactly
     * "harness.transports.put(server.getHostAddress(), this);" inside
     * its own open() override. */
    if (expr->type == AST_THIS_EXPR) {
        return TYPE_CLASS;
    }

    /* A chained/nested assignment used as a VALUE, e.g. the inner
     * "h = compute()" in "hex = h = compute();" - per JLS 15.26, an
     * assignment expression's own type/value is that of its target
     * (String here, from local "h"'s declared type), not whatever the
     * RHS happens to be. Without this, an AST_ASSIGNMENT_EXPR node
     * reaches none of the cases below (it's not AST_IDENTIFIER, and
     * nothing else here recognizes it) and fell straight through to
     * this function's own final "default to TYPE_INT" fallback -
     * wrongly telling a caller deciding whether to box the value for a
     * reference-typed target (e.g. the OUTER assignment's own
     * coerce_value_to_descriptor(), storing into a String field) that
     * it's a primitive int needing Integer.valueOf() first: an
     * invokestatic to Integer.valueOf(I) immediately before storing an
     * actual String reference (VerifyError: "Bad type on operand stack
     * ... not assignable to integer" - Integer.valueOf(int)'s own
     * parameter, not the field store, since coerce_stack_value's boxing
     * call itself needs an int on the stack that was never really
     * there). Confirmed against gumdrop's own TraceId.toHexString(),
     * whose double-checked-locking "hex = h = ByteArrays.toHexString
     * (bytes);" is exactly this shape. */
    if (expr->type == AST_ASSIGNMENT_EXPR) {
        slist_t *children = expr->data.node.children;
        if (children) {
            return get_expr_type_kind(mg, (ast_node_t *)children->data);
        }
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
        token_type_t op = expr->data.node.op_token;
        
        /* Logical operators (&&, ||) always produce boolean */
        if (op == TOK_AND || op == TOK_OR) {
            return TYPE_BOOLEAN;
        }
        
        /* Comparison operators (==, !=, <, >, <=, >=) always produce boolean */
        if (op == TOK_EQ || op == TOK_NE || op == TOK_LT || 
            op == TOK_GT || op == TOK_LE || op == TOK_GE) {
            return TYPE_BOOLEAN;
        }
        
        slist_t *children = expr->data.node.children;
        if (children && children->next) {
            ast_node_t *left = (ast_node_t *)children->data;
            ast_node_t *right = (ast_node_t *)children->next->data;
            
            /* String concatenation: if either operand is String and op is +, result is String */
            if (op == TOK_PLUS) {
                if (is_string_type(left) || is_string_type(right)) {
                    return TYPE_CLASS;  /* String is a reference type */
                }
            }
            
            type_kind_t left_kind = get_expr_type_kind(mg, left);
            type_kind_t right_kind = get_expr_type_kind(mg, right);
            
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
    
    /* For array access, get element type from array's type */
    if (expr->type == AST_ARRAY_ACCESS) {
        slist_t *children = expr->data.node.children;
        if (children) {
            ast_node_t *array_expr = (ast_node_t *)children->data;
            /* Check array's sem_type for element type */
            if (array_expr && array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    return elem_type->kind;
                }
            }
            /* Fallback: check local variable info */
            if (array_expr && array_expr->type == AST_IDENTIFIER && mg) {
                const char *arr_name = array_expr->data.leaf.name;
                type_kind_t elem_kind = mg_local_array_elem_kind(mg, arr_name);
                if (elem_kind != TYPE_UNKNOWN) {
                    return elem_kind;
                }
            }
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
                        if (*p == '/') {
                            *p = '.';
                        }
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
    
    /* Class literals (Type.class) are always reference type (java.lang.Class) */
    if (expr->type == AST_CLASS_LITERAL) {
        return TYPE_CLASS;
    }
    
    /* Conditional expression: type is common type of both branches */
    if (expr->type == AST_CONDITIONAL_EXPR) {
        slist_t *children = expr->data.node.children;
        if (children && children->next && children->next->next) {
            ast_node_t *then_expr = (ast_node_t *)children->next->data;
            ast_node_t *else_expr = (ast_node_t *)children->next->next->data;
            
            type_kind_t then_kind = get_expr_type_kind(mg, then_expr);
            type_kind_t else_kind = get_expr_type_kind(mg, else_expr);
            
            /* If either branch is a reference type, result is reference type */
            if (then_kind == TYPE_CLASS || then_kind == TYPE_ARRAY ||
                then_kind == TYPE_NULL || then_kind == TYPE_TYPEVAR) {
                return TYPE_CLASS;
            }
            if (else_kind == TYPE_CLASS || else_kind == TYPE_ARRAY ||
                else_kind == TYPE_NULL || else_kind == TYPE_TYPEVAR) {
                return TYPE_CLASS;
            }
            
            /* Both primitives - use wider type */
            if (then_kind == TYPE_DOUBLE || else_kind == TYPE_DOUBLE) {
                return TYPE_DOUBLE;
            }
            if (then_kind == TYPE_FLOAT || else_kind == TYPE_FLOAT) {
                return TYPE_FLOAT;
            }
            if (then_kind == TYPE_LONG || else_kind == TYPE_LONG) {
                return TYPE_LONG;
            }
            return then_kind;
        }
    }
    
    /* Cast expressions have the cast target type */
    if (expr->type == AST_CAST_EXPR) {
        slist_t *children = expr->data.node.children;
        if (children) {
            ast_node_t *type_node = (ast_node_t *)children->data;
            if (type_node->type == AST_PRIMITIVE_TYPE) {
                const char *prim = type_node->data.leaf.name;
                if (strcmp(prim, "long") == 0) {
                    return TYPE_LONG;
                }
                if (strcmp(prim, "double") == 0) {
                    return TYPE_DOUBLE;
                }
                if (strcmp(prim, "float") == 0) {
                    return TYPE_FLOAT;
                }
                if (strcmp(prim, "byte") == 0) {
                    return TYPE_BYTE;
                }
                if (strcmp(prim, "short") == 0) {
                    return TYPE_SHORT;
                }
                if (strcmp(prim, "char") == 0) {
                    return TYPE_CHAR;
                }
                if (strcmp(prim, "boolean") == 0) {
                    return TYPE_BOOLEAN;
                }
                return TYPE_INT;
            } else {
                return TYPE_CLASS;  /* Reference cast */
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
                int val = (int)char_literal_value(str);
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
                size_t len;
                if (str) {
                    /* str_len (not strlen(str)) is the literal's true byte
                     * length - it may contain an embedded NUL of its own
                     * (JLS 3.10.6 octal escape, e.g. "\0alice\0s3cret"),
                     * which strlen() would stop at short. */
                    len = lit->data.leaf.str_len;
                } else {
                    str = lit->data.leaf.name;
                    len = str ? strlen(str) : 0;
                }
                uint16_t idx = cp_add_string_len(cp, str ? str : "", str ? len : 0);
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

/**
 * Find the AST_VAR_DECLARATOR (and, through it, the initializer
 * expression) for a specific field name within an AST_FIELD_DECL node -
 * needed because field_gen_t only keeps a pointer to the whole
 * declaration (which may declare several fields at once, e.g. "static
 * final int A = 1, B = 2;"), not to the one declarator matching a given
 * field_gen_t entry.
 */
static ast_node_t *find_field_var_declarator(ast_node_t *field_decl, const char *name)
{
    if (!field_decl || !name) {
        return NULL;
    }
    for (slist_t *c = field_decl->data.node.children; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (child->type == AST_VAR_DECLARATOR && child->data.node.name &&
            strcmp(child->data.node.name, name) == 0) {
            return child;
        }
    }
    return NULL;
}

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
        
        /* For reference types, update stackmap to reflect actual type.
         * mg_emit_load_local pushes 'null' type for simplicity, but we need
         * the actual type for correct stackmap frames at branch merge points. */
        if (info->is_ref && ident->sem_type && ident->sem_type->kind == TYPE_CLASS) {
            const char *semantic_name = ident->sem_type->data.class_type.name;
            if (semantic_name) {
                /* Update stackmap: pop the 'null' type, push the actual object type */
                if (mg->stackmap) {
                    stackmap_pop(mg->stackmap, 1);
                    stackmap_push_object(mg->stackmap, mg->cp, class_to_internal_name(semantic_name));
                }
                
                /* For lambda parameters, the JVM type is erased (Object), but the semantic type
                 * is the actual type (e.g., String). We need to checkcast before using it. */
                if (strcmp(semantic_name, "java.lang.Object") != 0) {
                uint16_t cast_idx = cp_add_class(mg->cp, class_to_internal_name(semantic_name));
                bc_emit(mg->code, OP_CHECKCAST);
                bc_emit_u2(mg->code, cast_idx);
                    /* checkcast doesn't change stack depth or type */
                }
            }
        } else if (info->is_ref && ident->sem_type && ident->sem_type->kind == TYPE_ARRAY) {
            /* For array types, update stackmap with array descriptor */
            char *array_desc = type_to_descriptor(ident->sem_type);
            if (array_desc && mg->stackmap) {
                stackmap_pop(mg->stackmap, 1);
                mg_push_object_from_descriptor(mg, array_desc);
                mg->stack_depth--;  /* mg_push_object_from_descriptor increments, but we already pushed */
            }
            if (array_desc) {
                free(array_desc);
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
                /* Compile-time constant inlining (JLS 4.12.4/13.1): a
                 * `static final` field of a primitive or String type,
                 * initialized with a literal, is a genuine compile-time
                 * constant - real javac inlines its value at every use
                 * site instead of emitting a runtime field read, which
                 * sidesteps any dependence on the declaring class's own
                 * <clinit> field-initializer ORDER (a constant declared
                 * LATER in the same class is still safely usable from an
                 * EARLIER field initializer, since there is no runtime
                 * read of the field at all - the value is baked in at
                 * compile time). Without this, an unqualified reference
                 * to such a constant from an EARLIER static field
                 * initializer in the same class got a literal GETSTATIC,
                 * which DOES observe declaration order - the constant's
                 * own assignment hadn't run yet, so the read silently
                 * returned the field's default value (null for a
                 * String, 0/false for a primitive) instead of its real
                 * one. Confirmed against gumdrop's own
                 * DnsServerCapabilityCache, whose "WELL_KNOWN =
                 * wellKnownResolvers()" field initializer (early in the
                 * class) calls a method reading "DOH_PATH" (a static
                 * final String declared LATER in the same file) -
                 * producing no VerifyError at all, just a plain wrong
                 * runtime value (every well-known public resolver
                 * silently reported as NOT supporting DoH). */
                if ((field->access_flags & ACC_FINAL) && field->ast) {
                    ast_node_t *decl = find_field_var_declarator(field->ast, name);
                    ast_node_t *init_expr = (decl && decl->data.node.children) ?
                        (ast_node_t *)decl->data.node.children->data : NULL;
                    if (init_expr && init_expr->type == AST_LITERAL &&
                        codegen_literal(mg, init_expr, mg->cp)) {
                        /* The literal's own natural type (e.g. an int
                         * literal like "3000") may be narrower than the
                         * field's own DECLARED type (e.g. "static final
                         * long ... = 3000;", legal per JLS 5.2's implicit
                         * widening at the point of assignment) - unlike
                         * the getstatic path just below, which always got
                         * this right for free (a field read's pushed type
                         * comes from the field's own descriptor, not its
                         * initializer), inlining the bare literal bypasses
                         * that and needs the same widening applied
                         * explicitly. Missing this regressed gumdrop's
                         * own DnsResolver.DDR_TIMEOUT_MS ("private static
                         * final long DDR_TIMEOUT_MS = 3000;", passed to an
                         * interface method's own long parameter) the
                         * moment the constant-inlining fix above landed
                         * (VerifyError: "Bad type on operand stack", int
                         * not assignable to long_2nd). */
                        type_kind_t field_kind;
                        switch (field->descriptor[0]) {
                            case 'J': field_kind = TYPE_LONG; break;
                            case 'D': field_kind = TYPE_DOUBLE; break;
                            case 'F': field_kind = TYPE_FLOAT; break;
                            case 'Z': field_kind = TYPE_BOOLEAN; break;
                            case 'B': field_kind = TYPE_BYTE; break;
                            case 'C': field_kind = TYPE_CHAR; break;
                            case 'S': field_kind = TYPE_SHORT; break;
                            case 'I': field_kind = TYPE_INT; break;
                            default:  field_kind = TYPE_UNKNOWN; break;
                        }
                        if (field_kind != TYPE_UNKNOWN) {
                            type_kind_t lit_kind = get_expr_type_kind(mg, init_expr);
                            if (lit_kind != field_kind) {
                                coerce_stack_value(mg, mg->cp, lit_kind, NULL, field_kind, NULL);
                            }
                        }
                        return true;
                    }
                }
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

        /* Check if this is an inherited field from OUR OWN superclass chain
         * (set by semantic analysis) BEFORE checking enclosing classes below -
         * JLS 6.5.6.1's member-lookup precedence considers a class's own
         * inherited members before an enclosing scope's members, and doing
         * this the other way round breaks whenever an enclosing class is
         * ALSO this class's own superclass (an enum constant's own
         * constant-specific class body, e.g. "P(2) { int twice() { return
         * n * 2; } }", is compiled as an anonymous subclass of the enum
         * itself - so the enum is simultaneously both its enclosing_class,
         * needed for NestHost/InnerClasses attribute generation, and its
         * superclass, the real reason its own inherited field "n" is
         * accessible at all here). The "enclosing classes" search below
         * finds "n" via `enclosing` too (same symbol), but requires an
         * outer-instance this$0 walk to reach it - which a static nested
         * class (this one always is; enum constants have no real lexical
         * enclosing instance) can never have, hard-failing codegen with
         * "cannot access instance field ... from static context" even
         * though plain inheritance (a bare GETFIELD on `this`, no outer
         * instance needed at all) trivially reaches the same field.
         * Confirmed against gumdrop-parity work on exactly this shape. */
        if (ident->sem_symbol && ident->sem_symbol->kind == SYM_FIELD &&
            !(ident->sem_symbol->modifiers & MOD_STATIC) && !mg->is_static) {
            symbol_t *field_sym = ident->sem_symbol;
            /* Find which class the field belongs to by checking superclass chain */
            symbol_t *field_class = NULL;
            if (mg->class_gen && mg->class_gen->class_sym) {
                symbol_t *search = mg->class_gen->class_sym->data.class_data.superclass;
                while (search) {
                    if (search->data.class_data.members &&
                        scope_lookup_local(search->data.class_data.members, name) == field_sym) {
                        field_class = search;
                        break;
                    }
                    search = search->data.class_data.superclass;
                }
            }

            if (field_class && field_class->qualified_name) {
                char *class_internal = class_to_internal_name(field_class->qualified_name);
                char *field_desc = type_to_descriptor(field_sym->type);

                /* Load 'this' first */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push_object(mg, mg->class_gen->internal_name);

                /* Emit getfield with superclass as owner */
                uint16_t fieldref = cp_add_fieldref(mg->cp, class_internal, name, field_desc);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, fieldref);

                /* getfield pops object ref, pushes field value */
                mg_pop_typed(mg, 1);
                switch (field_desc[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L':
                    case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                    default:  mg_push_int(mg); break;
                }

                free(class_internal);
                free(field_desc);
                return true;
            }
        }

        /* Check all enclosing classes for nested/local/anonymous classes.
         * Also check the superclass chain of each enclosing class for inherited fields. */
        symbol_t *class_sym = mg->class_gen->class_sym;
        symbol_t *enclosing = class_sym ? class_sym->data.class_data.enclosing_class : NULL;
        while (enclosing) {
            /* Check enclosing class and its superclass chain */
            symbol_t *search = enclosing;
            while (search) {
                if (search->data.class_data.members) {
                    symbol_t *outer_field = scope_lookup_local(
                        search->data.class_data.members, name);
                    if (outer_field && outer_field->kind == SYM_FIELD) {
                        /* Inner classes can access any member (including private) of their
                         * immediate enclosing class, and public/protected members from superclasses.
                         * For Java 11+, NestHost/NestMembers attributes allow the JVM to verify this. */
                        bool accessible = (search == enclosing) ||  /* All members from immediate enclosing */
                                          (outer_field->modifiers & (MOD_PUBLIC | MOD_PROTECTED));  /* Or inherited public/protected */
                        if (accessible) {
                        /* Use the enclosing class as the receiver, but the superclass as the field owner */
                        char *outer_internal = class_to_internal_name(enclosing->qualified_name);
                        char *field_owner_internal = class_to_internal_name(search->qualified_name);
                        char *field_desc = type_to_descriptor(outer_field->type);
                        
                        if (outer_field->modifiers & MOD_STATIC) {
                            /* Static field - use getstatic with actual declaring class */
                            uint16_t fieldref = cp_add_fieldref(mg->cp, field_owner_internal,
                                                                 name, field_desc);
                            bc_emit(mg->code, OP_GETSTATIC);
                            bc_emit_u2(mg->code, fieldref);
                            switch (field_desc[0]) {
                                case 'J': mg_push_long(mg); break;
                                case 'D': mg_push_double(mg); break;
                                case 'F': mg_push_float(mg); break;
                                case 'L': 
                                case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                                default:  mg_push_int(mg); break;
                            }
                            free(outer_internal);
                            free(field_owner_internal);
                            free(field_desc);
                            return true;
                        } else if (mg->class_gen->is_inner_class) {
                            /* Instance field - access via this$0, walking the full
                             * this$0 chain (not just one hop) since 'enclosing' may be
                             * two or more levels of anonymous/inner/local class away
                             * from the current class (e.g. a class nested inside an
                             * anonymous class nested inside another anonymous class). */
                            codegen_load_enclosing_this(mg, mg->cp, enclosing);
                            mg_pop_typed(mg, 1);
                            mg_push_object(mg, outer_internal);

                            /* JVMS 5.4.4: a protected field declared in a
                             * superclass in a DIFFERENT runtime package can only
                             * be read directly by a getfield whose own
                             * containing class is itself a subclass of the
                             * declaring class - true of 'enclosing' but NOT of
                             * the class actually emitting this instruction
                             * (mg->class_gen, a nested class OF 'enclosing',
                             * never itself a subclass of the far-away
                             * superclass). Emitting the getfield directly threw
                             * "IllegalAccessError: failed to access class ..."
                             * at runtime despite this being perfectly legal
                             * Java (JLS 6.6.2). Route through a synthetic
                             * accessor defined on 'enclosing' instead, exactly
                             * like real javac's own access$NNN bridge methods -
                             * confirmed against gumdrop's own
                             * WebSocketClientProtocolHandler$ClientWebSocketTransport
                             * reading the inherited "protected Endpoint
                             * endpoint" field declared on
                             * HttpClientProtocolHandler (a different package). */
                            bool needs_bridge = false;
                            if ((outer_field->modifiers & MOD_PROTECTED) &&
                                !(outer_field->modifiers & MOD_PUBLIC)) {
                                char *acc_pkg = get_package_name(class_sym->qualified_name);
                                char *owner_pkg = get_package_name(search->qualified_name);
                                bool same_package = acc_pkg && owner_pkg &&
                                    strcmp(acc_pkg, owner_pkg) == 0;
                                free(acc_pkg);
                                free(owner_pkg);
                                bool current_is_subtype = false;
                                for (symbol_t *s = class_sym; s; s = s->data.class_data.superclass) {
                                    if (s == search) { current_is_subtype = true; break; }
                                }
                                needs_bridge = !same_package && !current_is_subtype;
                            }

                            if (needs_bridge) {
                                const char *accessor_name =
                                    get_or_create_field_accessor(enclosing, search, outer_field);
                                size_t adesc_len = strlen(outer_internal) + strlen(field_desc) + 5;
                                char *accessor_desc = malloc(adesc_len);
                                snprintf(accessor_desc, adesc_len, "(L%s;)%s", outer_internal, field_desc);
                                uint16_t methodref = cp_add_methodref(mg->cp, outer_internal,
                                                                       accessor_name, accessor_desc);
                                bc_emit(mg->code, OP_INVOKESTATIC);
                                bc_emit_u2(mg->code, methodref);
                                free(accessor_desc);
                            } else {
                                /* Get the field using the declaring superclass as owner */
                                uint16_t fieldref = cp_add_fieldref(mg->cp, field_owner_internal,
                                                                     name, field_desc);
                                bc_emit(mg->code, OP_GETFIELD);
                                bc_emit_u2(mg->code, fieldref);
                            }
                            mg_pop_typed(mg, 1);
                            switch (field_desc[0]) {
                                case 'J': mg_push_long(mg); break;
                                case 'D': mg_push_double(mg); break;
                                case 'F': mg_push_float(mg); break;
                                case 'L': 
                                case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                                default:  mg_push_int(mg); break;
                            }
                            free(outer_internal);
                            free(field_owner_internal);
                            free(field_desc);
                            return true;
                        } else {
                            free(outer_internal);
                            free(field_owner_internal);
                            free(field_desc);
                            fprintf(stderr, "codegen: cannot access instance field '%s' of enclosing class from static context\n", name);
                            return false;
                        }
                        }
                    }
                }
                search = search->data.class_data.superclass;
            }
            enclosing = enclosing->data.class_data.enclosing_class;
        }
    }

    /* Check if this is a statically imported field */
    if (ident->sem_symbol && ident->sem_symbol->kind == SYM_FIELD &&
        (ident->sem_symbol->modifiers & MOD_STATIC)) {
        symbol_t *field_sym = ident->sem_symbol;
        /* Get the class from the symbol's scope owner */
        symbol_t *class_sym = field_sym->scope ? field_sym->scope->owner : NULL;
        const char *owner_name = NULL;
        if (class_sym) {
            owner_name = class_sym->qualified_name ? class_sym->qualified_name : class_sym->name;
        }
        
        if (owner_name) {
            char *class_internal = class_to_internal_name(owner_name);
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

    /* Interface constants (static final fields on implemented interfaces). */
    if (mg->class_gen && mg->class_gen->class_sym) {
        semantic_t *sem = mg->class_gen->sem;
        symbol_t *check_class = mg->class_gen->class_sym;
        while (check_class) {
            for (slist_t *iface_node = check_class->data.class_data.interfaces;
                 iface_node; iface_node = iface_node->next) {
                symbol_t *iface = (symbol_t *)iface_node->data;
                if (!iface) {
                    continue;
                }
                if (sem && sem->shared_registry) {
                    const char *qname = iface->qualified_name ? iface->qualified_name : iface->name;
                    if (qname) {
                        symbol_t *reg_iface = type_registry_lookup(sem->shared_registry, qname);
                        if (reg_iface) {
                            iface = reg_iface;
                        }
                    }
                }
                if (!iface->data.class_data.members) {
                    continue;
                }
                symbol_t *field = scope_lookup_local(iface->data.class_data.members, name);
                if (field && field->kind == SYM_FIELD &&
                    (field->modifiers & MOD_STATIC) && field->type) {
                    const char *owner_name = iface->qualified_name ? iface->qualified_name : iface->name;
                    if (owner_name) {
                        char *class_internal = class_to_internal_name(owner_name);
                        char *field_desc = type_to_descriptor(field->type);
                        uint16_t fieldref = cp_add_fieldref(mg->cp, class_internal, name, field_desc);
                        bc_emit(mg->code, OP_GETSTATIC);
                        bc_emit_u2(mg->code, fieldref);
                        switch (field->type->kind) {
                            case TYPE_LONG:   mg_push_long(mg); break;
                            case TYPE_DOUBLE: mg_push_double(mg); break;
                            case TYPE_FLOAT:  mg_push_float(mg); break;
                            case TYPE_CLASS:
                            case TYPE_ARRAY:  mg_push_object_from_descriptor(mg, field_desc); break;
                            default:          mg_push_int(mg); break;
                        }
                        free(class_internal);
                        free(field_desc);
                        return true;
                    }
                }
            }
            check_class = check_class->data.class_data.enclosing_class;
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
                static __thread char nested_internal[256];
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
 * Look up a field by name in a class's own members, walking UP its
 * superclass chain if not found directly on `class_sym` itself.
 *
 * scope_lookup_local() alone only ever checks a single class's own
 * members scope, never its ancestors - fine when a field is declared
 * directly on the receiver's own static type, but wrong for an INHERITED
 * field (declared on a superclass instead). codegen_field_access() below
 * has several receiver shapes (a plain local variable, and the general
 * "receiver is some other expression" fallback) that each independently
 * re-derive a field's own type here for codegen purposes (to build the
 * GETFIELD's descriptor and the pushed-value type for stack tracking);
 * each needs this same walk, mirroring the analogous superclass walk
 * semantic.c's get_expression_type() AST_FIELD_ACCESS case already does
 * when resolving a field access expression's type during semantic
 * analysis. Without it, an inherited field (e.g. `session`, declared on
 * `LinkImpl`, accessed via a `ReceiverImpl`-typed expression from a third,
 * unrelated file) is silently not found here, and the caller falls back
 * to a hardcoded `Ljava/lang/Object;` descriptor - wrong for anything but
 * a literal Object-typed field, and mismatched against the correct
 * target class a subsequent method call on the field's value would use
 * (resolved separately - and correctly, since it already walks
 * superclasses - by semantic.c) once the verifier compares the two.
 */
static symbol_t *lookup_field_with_superclass(symbol_t *class_sym, const char *field_name)
{
    for (symbol_t *search_class = class_sym; search_class;
         search_class = search_class->data.class_data.superclass) {
        if (!search_class->data.class_data.members) {
            continue;
        }
        symbol_t *field_sym = scope_lookup_local(search_class->data.class_data.members, field_name);
        if (field_sym && field_sym->kind == SYM_FIELD) {
            return field_sym;
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
            
            if (strcmp(prim_name, "boolean") == 0) {
                wrapper_class = "java/lang/Boolean";
            } else if (strcmp(prim_name, "byte") == 0) {
                wrapper_class = "java/lang/Byte";
            } else if (strcmp(prim_name, "char") == 0) {
                wrapper_class = "java/lang/Character";
            } else if (strcmp(prim_name, "short") == 0) {
                wrapper_class = "java/lang/Short";
            } else if (strcmp(prim_name, "int") == 0) {
                wrapper_class = "java/lang/Integer";
            } else if (strcmp(prim_name, "long") == 0) {
                wrapper_class = "java/lang/Long";
            } else if (strcmp(prim_name, "float") == 0) {
                wrapper_class = "java/lang/Float";
            } else if (strcmp(prim_name, "double") == 0) {
                wrapper_class = "java/lang/Double";
            } else if (strcmp(prim_name, "void") == 0) {
                wrapper_class = "java/lang/Void";
            }
            
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
            /* Stack: arrayref -> int (length) - net word-count effect 0, but
             * mg->stackmap's own tracked TYPE for this slot must change from
             * the array's reference type to integer - a raw mg_pop(1)/
             * mg_push(1) (or no update at all, as here previously) leaves
             * the array's own type sitting on top of mg->stackmap's simulated
             * stack. Invisible in straight-line code (nothing ever reads
             * that stale type back out), but a real bug the moment a
             * stackmap frame is recorded while the length is still on the
             * stack - e.g. as one operand of a ternary merged with another
             * int-typed branch (VerifyError: "Type integer ... is not
             * assignable to '[B'"). Confirmed against gumdrop's own
             * Encoder.encode() (org.bluezoo.gumdrop.http.hpack), whose
             * "useHuffman ? hname.length : rname.length" hits exactly this. */
            mg_pop_typed(mg, 1);
            mg_push_int(mg);
            return true;
        }
    }
    
    /* Check for qualified 'this' (ClassName.this) - enclosing instance access */
    if (strcmp(field_name, "this") == 0 && receiver->type == AST_IDENTIFIER) {
        const char *enclosing_class_name = receiver->data.leaf.name;

        /* EnclosingType.this in an instance method of EnclosingType is just this */
        if (mg->class_gen && mg->class_gen->class_sym && !mg->is_static) {
            symbol_t *cur = mg->class_gen->class_sym;
            bool same_class = false;
            if (cur->name && strcmp(cur->name, enclosing_class_name) == 0) {
                same_class = true;
            } else if (cur->qualified_name) {
                const char *simple = cur->qualified_name;
                const char *dot = strrchr(cur->qualified_name, '.');
                if (dot) {
                    simple = dot + 1;
                }
                if (strcmp(simple, enclosing_class_name) == 0) {
                    same_class = true;
                }
            }
            if (receiver->sem_symbol && receiver->sem_symbol == cur) {
                same_class = true;
            }
            if (same_class) {
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push_object(mg, mg->class_gen->internal_name);
                return true;
            }
        }

        if (mg->class_gen && mg->class_gen->this_dollar_zero_ref &&
            mg->class_gen->class_sym) {
            symbol_t *enc = mg->class_gen->class_sym->data.class_data.enclosing_class;
            if (enc && enc->name && strcmp(enc->name, enclosing_class_name) == 0) {
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push_object(mg, mg->class_gen->internal_name);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, mg->class_gen->this_dollar_zero_ref);
                mg_pop_typed(mg, 1);
                if (enc->qualified_name) {
                    char *internal = class_to_internal_name(enc->qualified_name);
                    mg_push_object(mg, internal);
                    free(internal);
                }
                return true;
            }
        }
        
        /* Find the enclosing class in the chain and load the appropriate this$N */
        if (mg->class_gen && (mg->class_gen->is_inner_class ||
                               mg->class_gen->is_local_class ||
                               mg->class_gen->is_anonymous_class ||
                               (mg->class_gen->class_sym &&
                                mg->class_gen->class_sym->data.class_data.enclosing_class))) {
            
            /* Start with 'this' (aload_0) */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push_object(mg, mg->class_gen->internal_name);
            
            /* Traverse the enclosing class chain to find the right class */
            symbol_t *current_class = mg->class_gen->class_sym;
            int depth = 0;
            
            while (current_class) {
                /* Check if this is the target class */
                if (current_class->name && strcmp(current_class->name, enclosing_class_name) == 0) {
                    /* Found it - we've loaded the right enclosing instance */
                    /* Fix stack tracking - we should have the enclosing class type */
                    if (current_class->qualified_name) {
                        char *internal = class_to_internal_name(current_class->qualified_name);
                        /* Pop the current tracking and push the correct type.
                         * Must be the type-aware mg_pop_typed(), not plain
                         * mg_pop() - see the identical fix and full
                         * explanation at this same function's multi-hop
                         * GETFIELD loop below. */
                        mg_pop_typed(mg, 1);
                        mg_push_object(mg, internal);
                        free(internal);
                    }
                    return true;
                }
                
                /* Move to enclosing class */
                symbol_t *enclosing = current_class->data.class_data.enclosing_class;
                if (!enclosing) {
                    break;
                }
                
                /* Load the next hop's synthetic outer-instance field. Real
                 * javac (and genesis's own field-creation code, e.g.
                 * class_gen_new()'s "this0->name = strdup(\"this$0\")")
                 * always names this field "this$0" on EVERY class that has
                 * one - there is no "this$1"/"this$2" convention; reaching
                 * an ancestor two levels up means chaining TWO separate
                 * "this$0" getfields, one per class, each named "this$0"
                 * on its own class. Using "this$%d" with an incrementing
                 * depth here instead produced a getfield naming a
                 * NON-EXISTENT field ("this$1") the moment this loop ran a
                 * second iteration: NoSuchFieldError at runtime, for any
                 * anonymous/inner class nested two or more lexical levels
                 * deep referring to a non-immediate enclosing instance
                 * (e.g. "Outer.this" from inside an anonymous class nested
                 * inside another anonymous class). Confirmed against
                 * gumdrop's own ServletWebConnection, whose constructor
                 * nests exactly this shape. */
                (void)depth;
                const char *this_field = "this$0";

                char *current_internal = class_to_internal_name(
                    current_class->qualified_name ? current_class->qualified_name : current_class->name);
                char *enclosing_internal = class_to_internal_name(
                    enclosing->qualified_name ? enclosing->qualified_name : enclosing->name);
                
                char field_desc[256];
                snprintf(field_desc, sizeof(field_desc), "L%s;", enclosing_internal);
                
                uint16_t fieldref = cp_add_fieldref(cp, current_internal, this_field, field_desc);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, fieldref);
                
                /* Update stack tracking. Must be the type-aware
                 * mg_pop_typed(), not plain mg_pop() - the latter only
                 * adjusts mg->stack_depth (the abstract counter used for
                 * max_stack), never mg->stackmap's own operand-stack type
                 * array, which only mg_pop_typed()/the "_typed" push
                 * helpers touch. Using plain mg_pop() here left mg->stack_depth
                 * correct (net 0 per hop, matching the GETFIELD above,
                 * which just replaces the top stack slot) but left ONE
                 * STALE entry in mg->stackmap's type array per hop - for
                 * an enclosing instance reached via two or more hops (e.g.
                 * "Outer.this" from an anonymous class nested inside
                 * another anonymous class), those stale entries corrupted
                 * any StackMapTable frame recorded at a later branch
                 * target in the same expression: "ClassFormatError:
                 * StackMapTable format error: bad type array size". A
                 * single hop (the immediate-enclosing-class fast path a
                 * few lines above this loop) already used the correct
                 * mg_pop_typed() - only this general, multi-hop loop had
                 * the bug, which is why nesting only one level deep never
                 * surfaced it. */
                mg_pop_typed(mg, 1);  /* Pop current class */
                mg_push_object(mg, enclosing_internal);  /* Push enclosing class */
                
                free(current_internal);
                free(enclosing_internal);
                
                current_class = enclosing;
                depth++;
            }
            
            fprintf(stderr, "codegen: cannot find enclosing class '%s' in class chain\n", 
                    enclosing_class_name);
            return false;
        }

        /* Fallback for non-inner class context - this shouldn't happen */
        fprintf(stderr, "codegen: qualified 'this' used in non-inner class context\n");
        return false;
    }

    /* Enum constant on a nested enum: Outer.Nested.CONST */
    if (receiver->type == AST_FIELD_ACCESS) {
        slist_t *rch = receiver->data.node.children;
        const char *nested_simple = receiver->data.node.name;
        if (rch && nested_simple) {
            ast_node_t *outer_recv = (ast_node_t *)rch->data;
            if (outer_recv->type == AST_IDENTIFIER) {
                symbol_t *outer_sym = outer_recv->sem_symbol;
                if (!outer_sym && mg->class_gen && mg->class_gen->sem) {
                    const char *outer_internal = resolve_class_name(mg, outer_recv->data.leaf.name);
                    if (outer_internal) {
                        char qualified[512];
                        snprintf(qualified, sizeof(qualified), "%s", outer_internal);
                        for (char *p = qualified; *p; p++) {
                            if (*p == '/') {
                                *p = '.';
                            }
                        }
                        outer_sym = load_external_class(mg->class_gen->sem, qualified);
                    }
                }
                if (outer_sym && outer_sym->data.class_data.members) {
                    symbol_t *nested_sym = scope_lookup_local(
                        outer_sym->data.class_data.members, nested_simple);
                    if (nested_sym && nested_sym->kind == SYM_ENUM &&
                        nested_sym->data.class_data.members) {
                        symbol_t *const_sym = scope_lookup_local(
                            nested_sym->data.class_data.members, field_name);
                        if (const_sym && const_sym->kind == SYM_FIELD &&
                            (const_sym->modifiers & MOD_STATIC) && const_sym->type) {
                            char *owner_internal = class_to_internal_name(
                                nested_sym->qualified_name ? nested_sym->qualified_name
                                                             : nested_sym->name);
                            char *field_desc = type_to_descriptor(const_sym->type);
                            uint16_t fieldref = cp_add_fieldref(cp, owner_internal,
                                                                field_name, field_desc);
                            bc_emit(mg->code, OP_GETSTATIC);
                            bc_emit_u2(mg->code, fieldref);
                            switch (field_desc[0]) {
                            case 'J': mg_push_long(mg); break;
                            case 'D': mg_push_double(mg); break;
                            case 'F': mg_push_float(mg); break;
                            case 'L':
                            case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                            default: mg_push_int(mg); break;
                            }
                            free(owner_internal);
                            free(field_desc);
                            return true;
                        }
                    }
                }
            }
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
            /* Thread-local to avoid race conditions in parallel compilation */
            static __thread char external_class_name[256];
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
                                static __thread char field_desc_buf[256];
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
                        static __thread char ext_field_desc_buf[256];
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
                
                /* Look up the field in the class to get its type - walking
                 * the superclass chain too, since it may be inherited. */
                symbol_t *class_sym = receiver->sem_type->data.class_type.symbol;
                if (class_sym) {
                    symbol_t *field_sym = lookup_field_with_superclass(class_sym, field_name);
                    if (field_sym && field_sym->type) {
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
                        if (*p == '/') {
                            *p = '.';
                        }
                    }
                    local_type = hashtable_lookup(mg->class_gen->sem->types, dotted);
                    free(dotted);
                }
                if (local_type && local_type->kind == TYPE_CLASS && local_type->data.class_type.symbol) {
                    symbol_t *class_sym = local_type->data.class_type.symbol;
                    symbol_t *field_sym = lookup_field_with_superclass(class_sym, field_name);
                    if (field_sym && field_sym->type) {
                        field_desc = type_to_descriptor(field_sym->type);
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
        /* Check if the receiver is a class/enum reference (e.g., ElementDeclaration.ContentType).
         * If so, this is a static field access - emit getstatic, don't generate receiver code. */
        if (receiver->sem_symbol &&
            (receiver->sem_symbol->kind == SYM_CLASS ||
             receiver->sem_symbol->kind == SYM_INTERFACE ||
             receiver->sem_symbol->kind == SYM_ENUM)) {
            /* Static field access on a class/enum */
            symbol_t *class_sym = receiver->sem_symbol;
            char *class_internal = class_to_internal_name(class_sym->qualified_name);
            const char *field_desc = NULL;
            
            /* Look up the field in the class to get its descriptor -
             * walking the superclass chain too, since a static field can
             * be inherited just like an instance field. */
            {
                symbol_t *field_sym = lookup_field_with_superclass(class_sym, field_name);
                if (field_sym && field_sym->type) {
                    field_desc = type_to_descriptor(field_sym->type);
                }
            }

            if (!field_desc) {
                /* Default to the enum type itself (common for enum constants) */
                field_desc = malloc(strlen(class_internal) + 3);
                sprintf((char *)field_desc, "L%s;", class_internal);
            }
            
            /* Emit getstatic */
            uint16_t fieldref = cp_add_fieldref(cp, class_internal, field_name, field_desc);
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
            
            free(class_internal);
            return true;
        }
        
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

        /* The field's own descriptor - use this expression's own sem_type
         * (semantic analysis already resolves and substitutes it correctly,
         * same as it does for a single-level field access), not a
         * hardcoded Object. Without this, a chained access (a.b.c) always
         * treated .c as an Object field regardless of its real declared
         * type (e.g. boolean, TreeMap<...>), leaving the wrong descriptor
         * in the classfile and the verifier rejecting whatever used the
         * result as anything other than a plain reference. */
        char *owned_field_desc = NULL;
        if (expr->sem_type) {
            owned_field_desc = type_to_descriptor(expr->sem_type);
        }
        field_desc = owned_field_desc ? owned_field_desc : "Ljava/lang/Object;";

        /* Emit getfield */
        uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
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

        free(owned_field_desc);
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

            /* Not declared by this class: a field it inherits (field_map
             * only holds the class's own fields). Reached for the receiver
             * of a chained access, "this.kind.extension" with kind declared
             * by a superclass - the single-level "this.kind" is handled
             * before it gets here. The field is named through this class,
             * as the assignment path does for "this.field = ..."; the JVM
             * resolves it up the superclass chain. */
            symbol_t *inherited = mg->class_gen->class_sym ?
                lookup_field_with_superclass(mg->class_gen->class_sym, field_name) : NULL;
            char *inherited_desc = (inherited && inherited->type) ?
                type_to_descriptor(inherited->type) : NULL;
            if (inherited_desc) {
                uint16_t fieldref = cp_add_fieldref(cp, mg->class_gen->internal_name,
                                                     field_name, inherited_desc);
                if (inherited->modifiers & MOD_STATIC) {
                    /* A static field reached through "this": the reference
                     * is not used */
                    bc_emit(mg->code, OP_POP);
                    mg_pop_typed(mg, 1);
                    bc_emit(mg->code, OP_GETSTATIC);
                    bc_emit_u2(mg->code, fieldref);
                } else {
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, fieldref);
                    mg_pop_typed(mg, 1);  /* Pop the object reference */
                }
                switch (inherited_desc[0]) {
                    case 'J': mg_push_long(mg); break;
                    case 'D': mg_push_double(mg); break;
                    case 'F': mg_push_float(mg); break;
                    case 'L':
                    case '[': mg_push_object_from_descriptor(mg, inherited_desc); break;
                    default:  mg_push_int(mg); break;
                }
                free(inherited_desc);
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
    symbol_t *recv_class_sym = NULL;
    
    /* Get receiver type - for cast expressions, sem_type is the cast target type */
    type_t *recv_type = receiver->sem_type;
    
    
    if (recv_type && recv_type->kind == TYPE_CLASS) {
        if (recv_type->data.class_type.name) {
            recv_class = class_to_internal_name(recv_type->data.class_type.name);
        }
        recv_class_sym = recv_type->data.class_type.symbol;
        
        /* If no symbol, try to load externally */
        if (!recv_class_sym && recv_type->data.class_type.name && mg->class_gen && mg->class_gen->sem) {
            recv_class_sym = load_external_class(mg->class_gen->sem, recv_type->data.class_type.name);
        }
    }
    
    /* Look up the actual field descriptor - walking the superclass chain
     * too, since the field may be inherited rather than declared directly
     * on the receiver's own static type (e.g. a field declared on a
     * superclass in one file, accessed via a subclass-typed expression in
     * a third, unrelated file - see lookup_field_with_superclass()). */
    if (recv_class_sym) {
        symbol_t *field_sym = lookup_field_with_superclass(recv_class_sym, field_name);
        if (field_sym && field_sym->type) {
            field_desc = type_to_descriptor(field_sym->type);
        }
    }

    uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
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
    
    /* The resolved type is authoritative when semantic analysis provided one.
     * A wrapper (Integer, Character, ...) is a reference and is appended as an
     * Object, exactly like javac: the primitive overloads would need the value
     * unboxed, which is not what is on the stack. */
    if (expr->sem_type) {
        switch (expr->sem_type->kind) {
            case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
            case TYPE_CHAR:    return "(C)Ljava/lang/StringBuilder;";
            case TYPE_BYTE:
            case TYPE_SHORT:
            case TYPE_INT:     return "(I)Ljava/lang/StringBuilder;";
            case TYPE_LONG:    return "(J)Ljava/lang/StringBuilder;";
            case TYPE_FLOAT:   return "(F)Ljava/lang/StringBuilder;";
            case TYPE_DOUBLE:  return "(D)Ljava/lang/StringBuilder;";
            case TYPE_CLASS: {
                const char *name = expr->sem_type->data.class_type.name;
                if (name && (strcmp(name, "String") == 0 ||
                             strcmp(name, "java.lang.String") == 0)) {
                    return "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                }
                return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
            }
            case TYPE_ARRAY:
            case TYPE_TYPEVAR:
            case TYPE_NULL:
                return "(Ljava/lang/Object;)Ljava/lang/StringBuilder;";
            default:
                break;  /* unknown: fall back to the heuristics below */
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
            case TYPE_BOOLEAN: return "(Z)Ljava/lang/StringBuilder;";
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
    
    /* Handle parenthesized expressions - unwrap and recurse */
    if (expr->type == AST_PARENTHESIZED) {
        slist_t *children = expr->data.node.children;
        if (children && children->data) {
            return get_append_descriptor(mg, (ast_node_t *)children->data);
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
    uint16_t sb_new_offset = (uint16_t)mg->code->length;
    bc_emit(mg->code, OP_NEW);
    bc_emit_u2(mg->code, sb_class);
    mg_push_uninitialized(mg, sb_new_offset);  /* Uninitialized object reference */
    
    bc_emit(mg->code, OP_DUP);
    mg_push_uninitialized(mg, sb_new_offset);  /* Duplicated uninitialized reference */
    
    uint16_t init_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "<init>", "()V");
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    mg_pop_typed(mg, 1);  /* One of the dup'd refs is consumed by <init> */
    
    /* Mark the uninitialized StringBuilder as initialized in stackmap */
    if (mg->stackmap) {
        stackmap_init_object(mg->stackmap, sb_new_offset, mg->cp, "java/lang/StringBuilder");
    }
    
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
         * append consumes this+arg and returns StringBuilder. Only the
         * primitive long (J) and double (D) overloads take a 2-slot argument;
         * a Long or Double wrapper is a reference and takes one. Decide from
         * the overload actually called, not from the operand's unboxed kind. */
        if (strncmp(append_desc, "(J)", 3) == 0 || strncmp(append_desc, "(D)", 3) == 0) {
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

/**
 * Generate code for a boolean condition that branches to a not-yet-known
 * target when `expr` evaluates false, WITHOUT ever materializing an
 * intermediate 0/1 value on the stack for a top-level chain of `&&`
 * operators - each pending branch instruction's offset (still needing a
 * backpatch once the caller knows the real target) is prepended onto
 * *false_positions, mirroring the existing break_offsets pattern
 * (mg_add_break_to_context/mg_pop_loop in codegen.c).
 *
 * This exists because the generic codegen_binary_expr()'s TOK_AND/TOK_OR
 * handling materializes a single 0/1 value at a merge point shared by
 * BOTH the "short-circuited false" and "both true" edges - correct when
 * the expression's own VALUE is genuinely needed afterward (its two
 * edges are indistinguishable from that point on), but wrong for a
 * condition used directly by a control-flow statement (if/while/for),
 * where the "both true" edge (entering the loop body/then-branch) must
 * stay entirely separate from the "false" edge: once a local's tracked
 * type is (correctly) degraded at that shared merge point - e.g. for
 * `while (guard && (x = next()) != null) { use(x); }`, `x`'s tracked
 * type must become the safe common type across both edges - the
 * verifier treats that recorded frame as authoritative for ALL code
 * reached from it afterward, including the loop body, even along the
 * edge where `x` really was just assigned a real, narrower type. Direct
 * per-operand branching (this function) sidesteps the shared merge
 * entirely, so the loop body is reached only via the "both true" edge,
 * with every operand's own side effects (like `x`'s assignment) intact.
 *
 * Only a top-level `&&` chain is special-cased (recursively, through any
 * number of nested TOK_AND and AST_PARENTHESIZED wrappers); anything
 * else (`||`, `!`, a plain comparison, a method call, ...) falls back to
 * the ordinary value-based codegen_expr() plus a single ifeq, exactly
 * matching the behavior callers had before this function existed. This
 * covers the common "guard && (assignment) != null" loop idiom without
 * the larger, riskier change of a fully general jump-code condition
 * compiler for `||`/`!` as well.
 */
bool codegen_condition_and_chain_false_branch(method_gen_t *mg, const_pool_t *cp,
                                               ast_node_t *expr, slist_t **false_positions)
{
    while (expr->type == AST_PARENTHESIZED) {
        slist_t *inner = expr->data.node.children;
        if (!inner) {
            return false;
        }
        expr = (ast_node_t *)inner->data;
    }

    if (expr->type == AST_BINARY_EXPR && expr->data.node.op_token == TOK_AND) {
        slist_t *children = expr->data.node.children;
        if (!children || !children->next) {
            return false;
        }
        ast_node_t *left = (ast_node_t *)children->data;
        ast_node_t *right = (ast_node_t *)children->next->data;
        if (!codegen_condition_and_chain_false_branch(mg, cp, left, false_positions)) {
            return false;
        }
        return codegen_condition_and_chain_false_branch(mg, cp, right, false_positions);
    }

    /* Base case: an arbitrary boolean-valued expression - evaluate it and
     * branch on its own value, exactly as the pre-existing generic path
     * already did for a while/if/for condition. */
    if (!codegen_expr(mg, expr, cp)) {
        return false;
    }

    if (expr->sem_type && expr->sem_type->kind == TYPE_CLASS &&
        expr->sem_type->data.class_type.name) {
        type_kind_t prim = get_primitive_for_wrapper(expr->sem_type->data.class_type.name);
        if (prim == TYPE_BOOLEAN) {
            char *internal = class_to_internal_name(expr->sem_type->data.class_type.name);
            emit_unboxing(mg, cp, prim, internal);
            free(internal);
        }
    }

    size_t branch_pos = mg->code->length;
    bc_emit(mg->code, OP_IFEQ);
    bc_emit_u2(mg->code, 0);  /* placeholder offset, patched by the caller */
    mg_pop_typed(mg, 1);

    pending_condition_branch_t *pending = malloc(sizeof(*pending));
    pending->branch_pos = branch_pos;
    pending->state = mg->stackmap ? stackmap_save_state(mg->stackmap) : NULL;

    /* slist_append() returns the newly-appended TAIL node, not the list's
     * head - only adopt it as *false_positions when the list was
     * previously empty (that first node IS the head); on every later
     * call the existing head must be kept as-is, or earlier entries
     * become unreachable from the caller's own list pointer. */
    slist_t *new_node = slist_append(*false_positions, pending);
    if (!*false_positions) {
        *false_positions = new_node;
    }
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
        
        /* Auto-unbox left operand if it's a Boolean wrapper */
        if (left->sem_type && left->sem_type->kind == TYPE_CLASS && 
            left->sem_type->data.class_type.name) {
            type_kind_t prim = get_primitive_for_wrapper(left->sem_type->data.class_type.name);
            if (prim == TYPE_BOOLEAN) {
                char *internal = class_to_internal_name(left->sem_type->data.class_type.name);
                emit_unboxing(mg, cp, prim, internal);
                free(internal);
            }
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

        /* Snapshot local-variable tracking as it stood before the right
         * operand runs - branch1 (left-operand-false) reaches the
         * short-circuit target WITHOUT ever running the right operand, so
         * any local the right operand assigns (e.g. `x` in
         * `cond && (x = expr()) != null`) is genuinely still whatever it
         * was here on that edge - not the real type the assignment gives
         * it on the other (branch2) edge. */
        stackmap_state_t *pre_right_state = mg->stackmap ? stackmap_save_state(mg->stackmap) : NULL;

        /* Evaluate right operand */
        if (!codegen_expr(mg, right, cp)) {
            stackmap_state_free(pre_right_state);
            return false;
        }
        
        /* Auto-unbox right operand if it's a Boolean wrapper */
        if (right->sem_type && right->sem_type->kind == TYPE_CLASS && 
            right->sem_type->data.class_type.name) {
            type_kind_t prim = get_primitive_for_wrapper(right->sem_type->data.class_type.name);
            if (prim == TYPE_BOOLEAN) {
                char *internal = class_to_internal_name(right->sem_type->data.class_type.name);
                emit_unboxing(mg, cp, prim, internal);
                free(internal);
            }
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

        /* branch1 also targets this same position, arriving with the
         * right operand never run - restore locals to that pre-right-
         * operand state (any local the right operand assigned reverts to
         * its prior, possibly-unassigned tracked type) before recording
         * the frame both branches share. Only locals are restored - the
         * stack was already corrected to empty by the pop above, which
         * matches both edges (branch1's own ifeq also consumed its
         * operand off the stack). */
        if (pre_right_state && mg->stackmap) {
            stackmap_restore_locals_only(mg->stackmap, pre_right_state);
        }

        /* Record frame at short-circuit target (target of branch1 and branch2) */
        mg_record_frame(mg);
        stackmap_state_free(pre_right_state);
        
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
    
    /* Check if this is a null comparison - don't auto-unbox for these */
    bool is_null_comparison = (op == TOK_EQ || op == TOK_NE) &&
        ((left->type == AST_LITERAL && left->data.leaf.token_type == TOK_NULL) ||
         (right->type == AST_LITERAL && right->data.leaf.token_type == TOK_NULL));

    /* Per JLS 15.21, "==" / "!=" between two operands that are BOTH of
     * reference type - including two WRAPPER types, e.g.
     * "Boolean.TRUE == someBooleanField" - is a REFERENCE comparison
     * (object identity), not a numeric one; unboxing only applies when
     * exactly one side is a genuine primitive (JLS 15.21's other case).
     * Without this check, the unconditional auto-unbox below fired for
     * EACH operand independently, based solely on that operand's own
     * type, unboxing BOTH sides of a wrapper-vs-wrapper "==" - the
     * is_ref_compare decision further below (unaffected by this, since it
     * reads static sem_type, not what was actually pushed) then still
     * correctly chose IF_ACMPEQ/NE for what were now two unboxed ints on
     * the stack: VerifyError "Bad type on operand stack ... not
     * assignable to reference type". Confirmed against gumdrop's own
     * Request.isRequestedSessionIdFromCookie() (org.bluezoo.gumdrop.
     * servlet): "Boolean.TRUE == sessionType". */
    bool left_is_refish = left->sem_type && (left->sem_type->kind == TYPE_CLASS ||
        left->sem_type->kind == TYPE_ARRAY || left->sem_type->kind == TYPE_NULL);
    bool right_is_refish = right->sem_type && (right->sem_type->kind == TYPE_CLASS ||
        right->sem_type->kind == TYPE_ARRAY || right->sem_type->kind == TYPE_NULL);
    bool suppress_unboxing_for_ref_eq = (op == TOK_EQ || op == TOK_NE) &&
        left_is_refish && right_is_refish;
    
    /* Determine operand types and result type for widening.
     * Shifts are not subject to binary numeric promotion (JLS 15.19): each
     * operand is promoted independently, the shift count (right) always
     * ending up int-shaped, and the result type is the LEFT operand's
     * promoted type alone. Combining left_type and right_type into a shared
     * op_type, as every other arithmetic operator does, would wrongly widen
     * an int shift count to match a long left operand, producing LSHL with
     * a long count where the JVM expects an int. */
    bool is_shift = (op == TOK_LSHIFT || op == TOK_RSHIFT || op == TOK_URSHIFT);
    type_kind_t left_type = get_expr_type_kind(mg, left);
    type_kind_t right_type = get_expr_type_kind(mg, right);
    type_kind_t op_type = left_type;

    if (!is_shift) {
        /* Use the wider type (type promotion): double > float > long > int */
        if (right_type == TYPE_DOUBLE || op_type == TYPE_DOUBLE) {
            op_type = TYPE_DOUBLE;
        } else if (right_type == TYPE_FLOAT || op_type == TYPE_FLOAT) {
            op_type = TYPE_FLOAT;
        } else if (right_type == TYPE_LONG || op_type == TYPE_LONG) {
            op_type = TYPE_LONG;
        }
    } else if (op_type != TYPE_LONG) {
        /* byte/short/char/int all widen to int for the shift opcode */
        op_type = TYPE_INT;
    }
    
    /* Generate left operand */
    if (!codegen_expr(mg, left, cp)) {
        return false;
    }
    
    /* Auto-unbox left operand if it's a wrapper type (but not for null
     * comparisons, nor a wrapper-vs-wrapper "=="/"!=" - see
     * suppress_unboxing_for_ref_eq's own comment above). */
    if (!is_null_comparison && !suppress_unboxing_for_ref_eq &&
        left->sem_type && left->sem_type->kind == TYPE_CLASS &&
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
    
    /* Auto-unbox right operand if it's a wrapper type (but not for null
     * comparisons, nor a wrapper-vs-wrapper "=="/"!=" - see
     * suppress_unboxing_for_ref_eq's own comment above). */
    if (!is_null_comparison && !suppress_unboxing_for_ref_eq &&
        right->sem_type && right->sem_type->kind == TYPE_CLASS &&
        right->sem_type->data.class_type.name) {
        type_kind_t prim = get_primitive_for_wrapper(right->sem_type->data.class_type.name);
        if (prim != TYPE_UNKNOWN) {
            char *internal = class_to_internal_name(right->sem_type->data.class_type.name);
            emit_unboxing(mg, cp, prim, internal);
            free(internal);
        }
    }
    
    /* A `long`-typed shift count (e.g. "1L << someLongVariable") is legal
     * Java - JLS 15.19 does not require the shift count itself to be int,
     * only its LOW-ORDER bits are ever used (6 for a long shift, 5 for an
     * int shift) - but ISHL/LSHL/ISHR/LSHR/IUSHR/LUSHR all require that
     * count as a single-word INT on the operand stack regardless of the
     * left operand's own width. right_type/op_type's shared computation
     * above only widens for non-shift operators (see is_shift's own
     * comment on op_type), so a genuinely 2-word `long` shift count was
     * left as-is: LSHL then saw a long_2nd where it needs an int,
     * VerifyError "Bad type on operand stack ... long_2nd ... not
     * assignable to integer". Narrow it down with L2I - real javac does
     * the same (masking to the relevant low bits is the shift opcode's
     * own job at runtime, not something that needs doing here). Confirmed
     * against gumdrop's own DtlsReplayWindow.mayAccept(): "1L << delta"
     * where "long delta = highestSeq - combinedSeq;". */
    if (is_shift && right_type == TYPE_LONG) {
        bc_emit(mg->code, OP_L2I);
        mg_pop_typed(mg, 2);
        mg_push_int(mg);
    }

    /* Emit widening conversion for right operand if needed. Never for a
     * shift: the count is not part of the promotion that decided op_type
     * (see above) and must stay int-shaped on the stack. */
    if (!is_shift && right_type != op_type) {
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
                    bool left_is_ref = false;
                    bool left_is_wrapper = false;
                    if (left->type == AST_IDENTIFIER) {
                        const char *name = left->data.leaf.name;
                        if (mg_local_is_ref(mg, name)) {
                            left_is_ref = true;
                            /* Check if it's a wrapper type (e.g., Integer parameter) */
                            if (left->sem_type && left->sem_type->kind == TYPE_CLASS &&
                                left->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(left->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                left_is_wrapper = true;
                            }
                        } else if (left->sem_type && (left->sem_type->kind == TYPE_CLASS ||
                                   left->sem_type->kind == TYPE_ARRAY || left->sem_type->kind == TYPE_NULL)) {
                            /* mg_local_is_ref() only knows about tracked LOCAL
                             * variables/parameters - it has no entry at all for
                             * a bare identifier that's actually an instance or
                             * static FIELD (e.g. "version" referring to
                             * "this.version" via Java's implicit unqualified
                             * field access), so it returns false and left this
                             * branch never reached for one. Fall back to the
                             * expression's own resolved sem_type here, exactly
                             * like the AST_METHOD_CALL/AST_FIELD_ACCESS/
                             * AST_ARRAY_ACCESS/AST_CAST_EXPR branch below
                             * already does - without it, "version !=
                             * initialVersion" (both enum-typed instance
                             * fields) wrongly used an int comparison
                             * (IF_ICMPNE) on two REFERENCES: VerifyError "Bad
                             * type on operand stack ... is not assignable to
                             * integer". Confirmed against gumdrop's own
                             * QuicConnection.canAdoptVersion(). */
                            left_is_ref = true;
                            if (left->sem_type->kind == TYPE_CLASS &&
                                left->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(left->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                left_is_wrapper = true;
                            }
                        }
                    } else if (left->type == AST_NEW_OBJECT || left->type == AST_NEW_ARRAY ||
                               left->type == AST_THIS_EXPR || is_string_type(left)) {
                        left_is_ref = true;
                    } else if (left->type == AST_METHOD_CALL || left->type == AST_FIELD_ACCESS ||
                               left->type == AST_ARRAY_ACCESS || left->type == AST_CAST_EXPR) {
                        /* These could be reference types - check sem_type */
                        if (left->sem_type && (left->sem_type->kind == TYPE_CLASS ||
                            left->sem_type->kind == TYPE_ARRAY ||
                            left->sem_type->kind == TYPE_NULL)) {
                            left_is_ref = true;
                            /* Check if it's a wrapper type */
                            if (left->sem_type->kind == TYPE_CLASS && 
                                left->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(left->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                left_is_wrapper = true;
                            }
                        }
                    }
                    
                    /* Check right operand type */
                    bool right_is_ref = false;
                    bool right_is_wrapper = false;
                    if (right->type == AST_IDENTIFIER) {
                        const char *name = right->data.leaf.name;
                        if (mg_local_is_ref(mg, name)) {
                            right_is_ref = true;
                            /* Check if it's a wrapper type (e.g., Integer parameter) */
                            if (right->sem_type && right->sem_type->kind == TYPE_CLASS &&
                                right->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(right->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                right_is_wrapper = true;
                            }
                        } else if (right->sem_type && (right->sem_type->kind == TYPE_CLASS ||
                                   right->sem_type->kind == TYPE_ARRAY || right->sem_type->kind == TYPE_NULL)) {
                            /* Same fallback as the left operand above, for the
                             * same reason - a bare identifier referring to an
                             * instance/static FIELD (not a tracked local). */
                            right_is_ref = true;
                            if (right->sem_type->kind == TYPE_CLASS &&
                                right->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(right->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                right_is_wrapper = true;
                            }
                        }
                    } else if (right->type == AST_NEW_OBJECT || right->type == AST_NEW_ARRAY ||
                               right->type == AST_THIS_EXPR || is_string_type(right)) {
                        right_is_ref = true;
                    } else if (right->type == AST_METHOD_CALL || right->type == AST_FIELD_ACCESS ||
                               right->type == AST_ARRAY_ACCESS || right->type == AST_CAST_EXPR) {
                        if (right->sem_type && (right->sem_type->kind == TYPE_CLASS ||
                            right->sem_type->kind == TYPE_ARRAY ||
                            right->sem_type->kind == TYPE_NULL)) {
                            right_is_ref = true;
                            /* Check if it's a wrapper type */
                            if (right->sem_type->kind == TYPE_CLASS && 
                                right->sem_type->data.class_type.name &&
                                get_primitive_for_wrapper(right->sem_type->data.class_type.name) != TYPE_UNKNOWN) {
                                right_is_wrapper = true;
                            }
                        }
                    }
                    
                    /* Check if one side is primitive and other is wrapper - do unboxing compare */
                    bool left_is_prim = (left->type == AST_LITERAL && 
                                        left->data.leaf.token_type >= TOK_INTEGER_LITERAL &&
                                        left->data.leaf.token_type <= TOK_DOUBLE_LITERAL) ||
                                       (left->sem_type && left->sem_type->kind >= TYPE_BOOLEAN &&
                                        left->sem_type->kind <= TYPE_DOUBLE);
                    bool right_is_prim = (right->type == AST_LITERAL && 
                                         right->data.leaf.token_type >= TOK_INTEGER_LITERAL &&
                                         right->data.leaf.token_type <= TOK_DOUBLE_LITERAL) ||
                                        (right->sem_type && right->sem_type->kind >= TYPE_BOOLEAN &&
                                         right->sem_type->kind <= TYPE_DOUBLE);
                    
                    /* If one is primitive and other is wrapper, use primitive comparison.
                     * Unboxing is already handled by emit_unboxing calls above (lines ~2300 and ~2375). */
                    if ((left_is_prim && right_is_wrapper) || (right_is_prim && left_is_wrapper)) {
                        is_ref_compare = false;  /* Use integer comparison (wrapper was already unboxed) */
                    } else if (left_is_ref || right_is_ref) {
                        is_ref_compare = true;
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
    
    /* Reaching here means an arithmetic, bitwise or shift op was emitted
     * above. Both operands are still tracked at their pushed sizes (which,
     * except for a shift's right operand, is op_type's size); the operator
     * consumes both and leaves one op_type-sized result. Popping a fixed 1
     * slot here, regardless of size, undercounted a long/double result by
     * exactly the extra slot its operands occupied, leaving the tracked
     * stack permanently too deep and eventually emitting a spurious pop of
     * an empty real stack at the enclosing statement. */
    int result_slots = (op_type == TYPE_LONG || op_type == TYPE_DOUBLE) ? 2 : 1;
    int operand_slots = is_shift ? result_slots + 1 : result_slots * 2;
    mg_pop_typed(mg, operand_slots);
    switch (op_type) {
        case TYPE_LONG:   mg_push_long(mg); break;
        case TYPE_DOUBLE: mg_push_double(mg); break;
        case TYPE_FLOAT:  mg_push_float(mg); break;
        default:          mg_push_int(mg); break;
    }
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
    if (!arg) {
        return "I";
    }
    
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
            /* First check if sem_type is available (resolved by semantic analysis) */
            if (arg->sem_type && arg->sem_type->kind == TYPE_CLASS) {
                static __thread char new_obj_desc[256];
                char *desc = type_to_descriptor(arg->sem_type);
                strncpy(new_obj_desc, desc, sizeof(new_obj_desc) - 1);
                new_obj_desc[sizeof(new_obj_desc) - 1] = '\0';
                free(desc);
                return new_obj_desc;
            }
            /* Fall back: get class type from first child */
            if (arg->data.node.children) {
                ast_node_t *type_node = (ast_node_t *)arg->data.node.children->data;
                /* Check type node's sem_type first */
                if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                    static __thread char type_desc[256];
                    char *desc = type_to_descriptor(type_node->sem_type);
                    strncpy(type_desc, desc, sizeof(type_desc) - 1);
                    type_desc[sizeof(type_desc) - 1] = '\0';
                    free(desc);
                    return type_desc;
                }
                /* Last resort: use simple name with java.lang resolution */
                if (type_node->type == AST_CLASS_TYPE) {
                    static __thread char class_desc[256];
                    const char *name = resolve_java_lang_class(type_node->data.node.name);
                    snprintf(class_desc, sizeof(class_desc), "L%s;", name);
                    /* Convert dots to slashes */
                    for (char *p = class_desc + 1; *p != ';'; p++) {
                        if (*p == '.') {
                            *p = '/';
                        }
                    }
                    return class_desc;
                }
            }
            return "Ljava/lang/Object;";
        case AST_NEW_ARRAY:
            /* Check sem_type for proper array descriptor */
            if (arg->sem_type && arg->sem_type->kind == TYPE_ARRAY) {
                static __thread char new_arr_desc[256];
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
                            static __thread char sem_desc[256];
                            const char *qn = arg->sem_type->data.class_type.symbol->qualified_name;
                            snprintf(sem_desc, sizeof(sem_desc), "L%s;", qn);
                            for (char *p = sem_desc + 1; *p != ';'; p++) {
                                if (*p == '.') {
                                    *p = '/';
                                }
                            }
                            return sem_desc;
                        }
                        return "Ljava/lang/Object;";
                    case TYPE_ARRAY:
                        {
                            /* Use type_to_descriptor for proper array element type */
                            static __thread char arr_desc[256];
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

/**
 * Emit a widening primitive conversion (JLS 5.1.2) from from_kind to to_kind,
 * updating the tracked stack. A no-op if to_kind is not actually wider (e.g.
 * both are int-shaped, or to_kind is narrower).
 */
void emit_widen_primitive(method_gen_t *mg, type_kind_t from_kind, type_kind_t to_kind)
{
    switch (from_kind) {
        case TYPE_BYTE:
        case TYPE_SHORT:
        case TYPE_CHAR:
        case TYPE_INT:
            switch (to_kind) {
                case TYPE_LONG:   bc_emit(mg->code, OP_I2L); mg_pop_typed(mg, 1); mg_push_long(mg); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_I2F); mg_pop_typed(mg, 1); mg_push_float(mg); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_I2D); mg_pop_typed(mg, 1); mg_push_double(mg); break;
                default: break;
            }
            break;
        case TYPE_LONG:
            switch (to_kind) {
                case TYPE_FLOAT:  bc_emit(mg->code, OP_L2F); mg_pop_typed(mg, 2); mg_push_float(mg); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_L2D); mg_pop_typed(mg, 2); mg_push_double(mg); break;
                default: break;
            }
            break;
        case TYPE_FLOAT:
            if (to_kind == TYPE_DOUBLE) {
                bc_emit(mg->code, OP_F2D);
                mg_pop_typed(mg, 1);
                mg_push_double(mg);
            }
            break;
        default:
            break;
    }
}

/**
 * Convert a value already on top of the stack, of kind/class (from_kind,
 * from_class), to (to_kind, to_class), as an assignment conversion (JLS 5.2):
 * unboxing (optionally followed by widening), widening alone, or boxing.
 * When from_kind is primitive and to_kind is a non-primitive TYPE_CLASS/
 * TYPE_TYPEVAR target, to_class is NOT used to gate whether to box - the
 * only way Java lets a primitive value reach a reference-typed context at
 * all is a boxing conversion, optionally followed by a widening reference
 * conversion (JLS 5.1.7/5.2), so if semantic analysis already accepted the
 * assignment/return/call, the target can only be the exact wrapper, a
 * supertype the boxed value widens to (Object, Number, Comparable,
 * Serializable, ...), or a type variable's erasure - boxing is always the
 * right move. (Earlier this only recognized the exact wrapper class or
 * Object as boxable, silently leaving the value as a bare primitive for
 * any other legal target - e.g. "Number f() { return 1; }" - and emitting
 * ARETURN on an int: VerifyError "Bad type on operand stack ... integer
 * ... not assignable to reference type".) A mismatch where from_kind is
 * ALSO non-primitive (two unrelated reference types) is left alone, since
 * that is either already a reference conversion needing no bytecode, or a
 * genuine type error that semantic analysis, not codegen, should have
 * reported.
 */
void coerce_stack_value(method_gen_t *mg, const_pool_t *cp,
                               type_kind_t from_kind, const char *from_class,
                               type_kind_t to_kind, const char *to_class)
{
    /* No longer consulted: boxing a primitive is always correct once
     * to_kind is a non-primitive target (see the doc comment above).
     * Kept in the signature since callers already have it on hand and a
     * future to_kind==TYPE_ARRAY/TYPE_CLASS distinction may want it again. */
    (void)to_class;
    bool from_is_primitive = (from_kind >= TYPE_BOOLEAN && from_kind <= TYPE_DOUBLE);
    bool to_is_primitive = (to_kind >= TYPE_BOOLEAN && to_kind <= TYPE_DOUBLE);

    if (to_is_primitive && from_kind == TYPE_CLASS && from_class) {
        type_kind_t unboxed = get_primitive_for_wrapper(from_class);
        if (unboxed != TYPE_UNKNOWN) {
            char *internal = class_to_internal_name(from_class);
            emit_unboxing(mg, cp, unboxed, internal);
            free(internal);
            from_kind = unboxed;
            from_is_primitive = true;
        }
    }

    if (to_is_primitive && from_is_primitive) {
        if (from_kind != to_kind) {
            emit_widen_primitive(mg, from_kind, to_kind);
        }
        return;
    }

    if (!to_is_primitive && from_is_primitive && to_kind != TYPE_UNKNOWN) {
        emit_boxing(mg, cp, from_kind);
    }
}

/**
 * The kind and, for a reference type, the internal class name (a pointer
 * into desc, valid only as long as desc is; not a copy) of a JVM field or
 * array-element descriptor such as "I", "J" or "Ljava/lang/Object;".
 * out_class is left NULL for a primitive or array descriptor.
 */
void descriptor_kind_and_class(const char *desc, type_kind_t *out_kind,
                                      char *class_buf, size_t class_buf_size)
{
    *out_kind = TYPE_INT;
    if (class_buf && class_buf_size) {
        class_buf[0] = '\0';
    }
    if (!desc || !desc[0]) {
        return;
    }
    switch (desc[0]) {
        case 'Z': *out_kind = TYPE_BOOLEAN; break;
        case 'B': *out_kind = TYPE_BYTE;    break;
        case 'C': *out_kind = TYPE_CHAR;    break;
        case 'S': *out_kind = TYPE_SHORT;   break;
        case 'I': *out_kind = TYPE_INT;     break;
        case 'J': *out_kind = TYPE_LONG;    break;
        case 'F': *out_kind = TYPE_FLOAT;   break;
        case 'D': *out_kind = TYPE_DOUBLE;  break;
        case '[': *out_kind = TYPE_ARRAY;   break;
        case 'L':
            *out_kind = TYPE_CLASS;
            if (class_buf && class_buf_size) {
                size_t len = strlen(desc) - 2;  /* strip leading L and trailing ; */
                if (len >= class_buf_size) {
                    len = class_buf_size - 1;
                }
                memcpy(class_buf, desc + 1, len);
                class_buf[len] = '\0';
            }
            break;
        default: break;
    }
}

/**
 * The kind and, for a class type, the name of an already-generated value
 * value, preferring its resolved semantic type and falling back to the
 * heuristics codegen otherwise uses when that is unavailable.
 */
void value_kind_and_class(method_gen_t *mg, ast_node_t *value,
                                 type_kind_t *out_kind, const char **out_class)
{
    *out_class = NULL;
    if (value->sem_type) {
        *out_kind = value->sem_type->kind;
        if (*out_kind == TYPE_CLASS) {
            *out_class = value->sem_type->data.class_type.name;
        }
        return;
    }
    *out_kind = get_expr_type_kind(mg, value);
    if (value->type == AST_IDENTIFIER) {
        *out_class = mg_local_class_name(mg, value->data.leaf.name);
    }
}

/**
 * Convert an already-generated value to the type named by a JVM field or
 * array-element descriptor (see coerce_stack_value). Used at every simple
 * (non-compound) assignment target: local variable, field, array element
 * and variable declaration initializer.
 */
void coerce_value_to_descriptor(method_gen_t *mg, const_pool_t *cp,
                                       ast_node_t *value, const char *desc)
{
    type_kind_t to_kind;
    char class_buf[512];
    descriptor_kind_and_class(desc, &to_kind, class_buf, sizeof(class_buf));

    type_kind_t from_kind;
    const char *from_class;
    value_kind_and_class(mg, value, &from_kind, &from_class);

    coerce_stack_value(mg, cp, from_kind, from_class,
                       to_kind, class_buf[0] ? class_buf : NULL);
}

/**
 * Convert an already-generated argument to its parameter type: box a primitive
 * passed to a wrapper, Object or type-variable parameter, unbox a wrapper
 * passed to a primitive parameter, and widen primitives (int -> long etc.).
 * Shared by method calls and constructor calls.
 *
 * descriptor_from_args is true when the call's descriptor was built from the
 * argument types themselves; nothing is then boxed, as the descriptor already
 * matches what was pushed.
 */
static void coerce_arg_to_param(method_gen_t *mg, const_pool_t *cp, ast_node_t *arg,
                                symbol_t *param, bool descriptor_from_args)
{
    if (!param || !param->type) {
        return;
    }
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
        
        /* Boxing: primitive arg -> wrapper param or type variable (erased to Object)
         * Skip boxing if custom_descriptor is set - it's already built from arg types
         * and the method_sym may not match the actual descriptor being used. */
        if (!descriptor_from_args && param->type->kind == TYPE_TYPEVAR && arg_is_primitive) {
            /* Type variable erases to Object at runtime - must box */
            emit_boxing(mg, cp, arg_kind);
        }
        else if (!descriptor_from_args && param->type->kind == TYPE_CLASS && 
            param->type->data.class_type.name && arg_is_primitive) {
            /* Only box if param is specifically a wrapper type or Object */
            const char *param_name = param->type->data.class_type.name;
            type_kind_t target_prim = get_primitive_for_wrapper(param_name);
            if (target_prim != TYPE_UNKNOWN || 
                strcmp(param_name, "java.lang.Object") == 0 ||
                strcmp(param_name, "Object") == 0) {
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
        /* Widening primitive conversion: int -> long, int -> float, etc. */
        else if (arg_is_primitive && param_is_primitive && arg_kind != param->type->kind) {
            /* Apply widening conversion if needed */
            switch (arg_kind) {
                case TYPE_INT:
                case TYPE_CHAR:
                case TYPE_SHORT:
                case TYPE_BYTE:
                    switch (param->type->kind) {
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
                        default:
                            break;
                    }
                    break;
                case TYPE_LONG:
                    switch (param->type->kind) {
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
                        default:
                            break;
                    }
                    break;
                case TYPE_FLOAT:
                    if (param->type->kind == TYPE_DOUBLE) {
                        bc_emit(mg->code, OP_F2D);
                        mg_pop_typed(mg, 1);
                        mg_push_double(mg);
                    }
                    break;
                default:
                    break;
            }
        }
}

/**
 * Emit bytecode to load the nearest enclosing instance of type
 * target_owner, walking the this$0 chain from mg's current class as many
 * levels as needed. Shared by "outer.new Inner()"'s implicit (non-
 * explicit) outer-instance case and by an unqualified instance method
 * call that resolves to an enclosing class rather than the current class
 * or one of its superclasses (e.g. calling an outer class's method with
 * no explicit qualifier from inside a non-static inner class).
 */
static void codegen_load_enclosing_this(method_gen_t *mg, const_pool_t *cp, symbol_t *target_owner)
{
    if (!mg->class_gen || !mg->class_gen->class_sym) {
        return;
    }

    /* Start with 'this' */
    bc_emit(mg->code, OP_ALOAD_0);
    if (mg->class_gen->internal_name) {
        mg_push_object(mg, mg->class_gen->internal_name);
    } else {
        mg_push_null(mg);
    }

    /* Traverse the enclosing class chain to find how many levels deep */
    symbol_t *current = mg->class_gen->class_sym;
    bool first_iteration = true;

    while (current && current != target_owner) {
        symbol_t *cur_enclosing = current->data.class_data.enclosing_class;
        if (!cur_enclosing) {
            break;
        }

        /* Get this$0 from current class to get to cur_enclosing */
        char *cur_internal = class_to_internal_name(current->qualified_name);
        char *enc_internal = class_to_internal_name(cur_enclosing->qualified_name);
        size_t desc_len = strlen(enc_internal) + 3;
        char *desc = malloc(desc_len);
        snprintf(desc, desc_len, "L%s;", enc_internal);

        /* Use the already-computed this$0 ref for the first iteration if available */
        uint16_t this0_ref;
        if (first_iteration && mg->class_gen->this_dollar_zero_ref) {
            this0_ref = mg->class_gen->this_dollar_zero_ref;
        } else {
            this0_ref = cp_add_fieldref(cp, cur_internal, "this$0", desc);
        }

        bc_emit(mg->code, OP_GETFIELD);
        bc_emit_u2(mg->code, this0_ref);
        /* Word count unchanged (popped old, pushed enclosing), but
         * mg->stackmap's own tracked TYPE for this slot must change from
         * the inner class to the enclosing class - a raw "no update at
         * all" (as here previously) leaves the INNER class's own type
         * sitting on top of mg->stackmap's simulated stack. Invisible
         * whenever this value is consumed immediately (e.g. as a direct
         * method-call receiver with nothing else pending), but a real bug
         * the moment a stackmap frame is recorded while it's still on the
         * stack UNDERNEATH something else being evaluated - e.g. as the
         * receiver of an outer method call whose argument is itself a
         * ternary (VerifyError: "Type Pop3ProtocolHandler ... is not
         * assignable to Pop3ProtocolHandler$23"). Confirmed against
         * gumdrop's own Pop3ProtocolHandler's RETR-offload failure
         * callback: "recordSessionException(error instanceof Exception ?
         * (Exception) error : new Exception(error))" from within an
         * anonymous StorageExecutor.Callback. */
        mg_pop_typed(mg, 1);
        mg_push_object(mg, enc_internal);

        free(cur_internal);
        free(enc_internal);
        free(desc);

        /* If we found the target enclosing class, we're done */
        if (cur_enclosing == target_owner) {
            break;
        }

        current = cur_enclosing;
        first_iteration = false;
    }
}

/**
 * Compute the type each individual argument at a varargs position must
 * have, given the varargs parameter's own full declared type (e.g. for
 * `void m(byte[]... parts)`, the parameter's own declared type is
 * "byte[][]" - dimensions=2, base element_type=byte - but each actual
 * argument passed for it is a single "byte[]" - dimensions=1). genesis
 * represents an array type as a dimension count plus a single base
 * element type (not a chain of nested TYPE_ARRAY types), so simply
 * reading ->data.array_type.element_type directly (as every call site
 * needing this used to do) strips ALL the way down to the base scalar
 * type regardless of how many dimensions the varargs parameter itself
 * has - correct only for the common "T... parts" case (dimensions=1,
 * base=T), but wrong whenever T is itself an array (dimensions>=2):
 * the varargs array's own synthetic-array-store codegen then picked a
 * primitive store opcode (e.g. BASTORE, matching the base scalar type)
 * for what are actually array-reference elements, rejected by the
 * verifier the moment a real array reference reached that store.
 */
/**
 * Erase a (possibly bounded) type variable to its runtime array-component
 * type, same as javac's own erasure: a bound's own bound if the bound is
 * itself a type variable, or java.lang.Object if unbounded. Every other
 * kind is returned unchanged.
 *
 * Needed because varargs_element_type() below returns the varargs
 * parameter's own declared element type verbatim - for a JDK generic
 * varargs method like `EnumSet.of(E first, E... rest)` (E declared as
 * `<E extends Enum<E>>`), that element type is TYPE_TYPEVAR, which none
 * of this element type's callers' TYPE_CLASS/TYPE_ARRAY/primitive
 * branches match. Left un-erased, the synthetic varargs array got built
 * (and stackmap-tracked) as the generic "[Ljava/lang/Object;" fallback -
 * wrong whenever the type variable has a real bound, since the call
 * site's own invokestatic/invokevirtual descriptor is fixed to the
 * ERASED bound (e.g. "[Ljava/lang/Enum;" for EnumSet.of), not Object.
 * Confirmed against gumdrop's own BasicRealm's
 * `EnumSet.of(SaslMechanism.PLAIN, ...)` (VerifyError: "Bad type on
 * operand stack", "[Ljava/lang/Object;" not assignable to
 * "[Ljava/lang/Enum;").
 */
static type_t *erase_typevar_for_array(type_t *t)
{
    while (t && t->kind == TYPE_TYPEVAR) {
        t = t->data.type_var.bound;
    }
    return t ? t : type_new_class("java.lang.Object");
}

static type_t *varargs_element_type(type_t *varargs_param_type)
{
    if (!varargs_param_type || varargs_param_type->kind != TYPE_ARRAY) {
        return NULL;
    }
    int dims = varargs_param_type->data.array_type.dimensions;
    type_t *base = erase_typevar_for_array(varargs_param_type->data.array_type.element_type);
    if (dims <= 1) {
        return base;
    }
    return type_new_array(base, dims - 1);
}

/**
 * Number of array dimensions of "t", whichever way it is represented: a
 * flat TYPE_ARRAY (dimensions=N over a scalar base) or nested ones (an
 * array whose element type is itself an array). Stores the non-array
 * base type in *base when base is not NULL.
 */
static int array_total_dims(type_t *t, type_t **base)
{
    int dims = 0;
    while (t && t->kind == TYPE_ARRAY) {
        int d = t->data.array_type.dimensions;
        dims += d > 0 ? d : 1;
        t = t->data.array_type.element_type;
    }
    if (base) {
        *base = t;
    }
    return dims;
}

/**
 * True if an argument of array type "arg_type" can be passed AS the
 * whole varargs array for a parameter of type "param_type", rather than
 * being wrapped as one element of it: same number of dimensions, and a
 * base type that is the same primitive or an assignable reference type.
 */
static bool array_passes_as_varargs(type_t *param_type, type_t *arg_type)
{
    type_t *param_base = NULL;
    type_t *arg_base = NULL;
    int param_dims = array_total_dims(param_type, &param_base);
    int arg_dims = array_total_dims(arg_type, &arg_base);
    if (param_dims == 0 || param_dims != arg_dims || !param_base || !arg_base) {
        return false;
    }
    param_base = erase_typevar_for_array(param_base);
    if (param_base->kind >= TYPE_BOOLEAN && param_base->kind <= TYPE_DOUBLE) {
        return arg_base->kind == param_base->kind;
    }
    if (arg_base->kind >= TYPE_BOOLEAN && arg_base->kind <= TYPE_DOUBLE) {
        return false;
    }
    return type_assignable(param_base, arg_base);
}

/**
 * Generate the single array argument for a call's varargs position.
 *
 * "node" is the list node of the first argument at the varargs position,
 * or NULL when the call supplies none (an empty array is pushed). With
 * skip_trailing_block set, a final AST_BLOCK is an anonymous class body
 * ("new T(a, b) { ... }"), not an argument.
 *
 * Shared by method calls, constructor calls and enum constant
 * construction (codegen.c). The constructor path
 * used to carry its own, much weaker copy of this logic: it built an
 * Object[] for ANY non-class element type (so "new C(1, 2, 3)" against
 * "C(int... xs)" stored raw ints with AASTORE - VerifyError), and only
 * passed an existing array straight through for String/Object elements.
 *
 * Leaves exactly one array reference on the stack.
 */
bool codegen_varargs_tail(method_gen_t *mg, const_pool_t *cp, symbol_t *varargs_param,
                          slist_t *node, bool skip_trailing_block)
{
    /* Count the arguments at the varargs position */
    int varargs_count = 0;
    for (slist_t *n = node; n; n = n->next) {
        ast_node_t *a = (ast_node_t *)n->data;
        if (skip_trailing_block && a && a->type == AST_BLOCK && !n->next) {
            break;
        }
        varargs_count++;
    }
    ast_node_t *arg = node ? (ast_node_t *)node->data : NULL;

    /* Get element type from varargs array type */
    type_t *elem_type = varargs_element_type(varargs_param->type);

    /* Check for array-to-varargs conversion:
     * If there's exactly one argument at the varargs position and it's
     * already an array of the compatible type, pass it directly */
    if (varargs_count == 1 && arg) {
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
            /* Also check sem_type for identifiers - important for method return values etc. */
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
        }
        /* Check for new Type[] or new Type[]{...} expression */
        else if (arg->type == AST_NEW_ARRAY) {
            is_array_arg = true;
            /* Get element type from AST_NEW_ARRAY's first child (type node) */
            slist_t *arr_children = arg->data.node.children;
            if (arr_children) {
                ast_node_t *elem_type_node = (ast_node_t *)arr_children->data;
                if (elem_type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = elem_type_node->data.leaf.name;
                    if (strcmp(prim_name, "int") == 0) {
                        arg_elem_kind = TYPE_INT;
                    } else if (strcmp(prim_name, "long") == 0) {
                        arg_elem_kind = TYPE_LONG;
                    } else if (strcmp(prim_name, "double") == 0) {
                        arg_elem_kind = TYPE_DOUBLE;
                    } else if (strcmp(prim_name, "float") == 0) {
                        arg_elem_kind = TYPE_FLOAT;
                    } else if (strcmp(prim_name, "boolean") == 0) {
                        arg_elem_kind = TYPE_BOOLEAN;
                    } else if (strcmp(prim_name, "byte") == 0) {
                        arg_elem_kind = TYPE_BYTE;
                    } else if (strcmp(prim_name, "char") == 0) {
                        arg_elem_kind = TYPE_CHAR;
                    } else if (strcmp(prim_name, "short") == 0) {
                        arg_elem_kind = TYPE_SHORT;
                    }
                } else if (elem_type_node->type == AST_CLASS_TYPE || 
                           elem_type_node->type == AST_IDENTIFIER) {
                    arg_elem_kind = TYPE_CLASS;
                    arg_elem_class = elem_type_node->data.node.name ? 
                        elem_type_node->data.node.name : elem_type_node->data.leaf.name;
                }
            }
            /* Also check sem_type if available */
            if (arg->sem_type && arg->sem_type->kind == TYPE_ARRAY) {
                type_t *arg_elem = arg->sem_type->data.array_type.element_type;
                if (arg_elem) {
                    arg_elem_kind = arg_elem->kind;
                    if (arg_elem->kind == TYPE_CLASS) {
                        arg_elem_class = arg_elem->data.class_type.name;
                    }
                }
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
        
        /* A real resolved type_t for the argument's own element
         * type, when available (semantic analysis annotates most
         * expressions with sem_type regardless of which branch
         * above actually set is_array_arg/arg_elem_class) - used
         * below for a genuine assignability check (does the
         * argument's element type IMPLEMENT/EXTEND the varargs
         * parameter's element type), not just an exact-name
         * match. Without this, passing a "StandardOpenOption[]"
         * array directly to a "OpenOption... options" varargs
         * parameter (StandardOpenOption implements OpenOption -
         * exactly java.nio.file.channels.FileChannel.open()'s
         * own signature) never counted as "compatible" (the
         * class-name strings "StandardOpenOption" and
         * "OpenOption" are simply different), so the array got
         * wrapped as a single vararg element instead of passed
         * through - "ArrayStoreException:
         * [Ljava.nio.file.StandardOpenOption;" the moment the
         * call actually ran (storing the whole array into a
         * slot that expects one OpenOption). Confirmed against
         * gumdrop's own BasicFTPFileSystem.openForWriting()'s
         * "FileChannel.open(filePath, options)". */
        type_t *arg_elem_type_full = (arg->sem_type && arg->sem_type->kind == TYPE_ARRAY) ?
            arg->sem_type->data.array_type.element_type : NULL;

        if (is_array_arg && elem_type) {
            bool compatible = false;

            /* Handle type variable (e.g., T in Stream.of(T...)) - any reference array is compatible */
            if (elem_type->kind == TYPE_TYPEVAR) {
                /* Type variable accepts any reference type */
                compatible = (arg_elem_kind == TYPE_CLASS);
            }
            else if (elem_type->kind == TYPE_CLASS) {
                /* Object[] is compatible with any reference array */
                if (strcmp(elem_type->data.class_type.name, "java.lang.Object") == 0) {
                    compatible = (arg_elem_kind == TYPE_CLASS);
                }
                /* Same class type */
                else if (arg_elem_class) {
                    const char *expected = elem_type->data.class_type.name;
                    /* Handle qualified vs simple names */
                    const char *simple = strrchr(expected, '.');
                    if (simple) {
                        simple++;
                    } else  {
                        simple = expected;
                    }
                    const char *arg_simple = strrchr(arg_elem_class, '/');
                    if (arg_simple) {
                        arg_simple++;
                    } else {
                        arg_simple = strrchr(arg_elem_class, '.');
                        if (arg_simple) {
                            arg_simple++;
                        } else  {
                            arg_simple = arg_elem_class;
                        }
                    }
                    compatible = (strcmp(simple, arg_simple) == 0 ||
                                 strcmp(expected, arg_elem_class) == 0);
                }
                /* Exact name match failed - fall back to a real
                 * assignability check (does the argument's
                 * element type implement/extend the parameter's
                 * element type), when a resolved type_t for it
                 * is available. See arg_elem_type_full's own
                 * comment above for why this is needed at all. */
                if (!compatible && arg_elem_type_full) {
                    compatible = type_assignable(elem_type, arg_elem_type_full);
                }
            } else if (elem_type->kind == TYPE_ARRAY) {
                /* The element type is itself an array ("byte[]...
                 * parts"): the argument passes straight through
                 * only when it is a whole array OF those elements
                 * (a byte[][]), i.e. one more dimension than the
                 * element type over a compatible base type. A
                 * single element (a byte[]) still has to be
                 * wrapped. This case used to fall into the
                 * primitive comparison below, which never matched,
                 * so a byte[][] handed to "byte[]... parts" was
                 * wrapped a second time into a one-element array
                 * and stored where a byte[] was expected
                 * (ArrayStoreException: [[B). */
                compatible = arg->sem_type &&
                    array_passes_as_varargs(varargs_param->type, arg->sem_type);
            } else {
                /* Primitive array - check exact type match. An
                 * argument with more than one dimension is an
                 * array of arrays, never an array of this
                 * primitive. */
                compatible = (elem_type->kind == arg_elem_kind) &&
                    (!arg->sem_type || array_total_dims(arg->sem_type, NULL) == 1);
            }
            
            if (compatible) {
                /* Pass array directly - just generate the expression */
                return codegen_expr(mg, arg, cp);
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
    
    /* Build array type string for stackmap tracking BEFORE array creation */
    char *array_type_str = NULL;
    if (elem_type && elem_type->kind == TYPE_CLASS) {
        char *internal = class_to_internal_name(elem_type->data.class_type.name);
        size_t len = strlen(internal) + 4;  /* "[L" + name + ";" + null */
        array_type_str = malloc(len);
        snprintf(array_type_str, len, "[L%s;", internal);
        free(internal);
    } else if (elem_type && elem_type->kind == TYPE_ARRAY) {
        /* e.g. "byte[]... parts" - each element is itself an
         * array ("byte[]"), so the synthetic array being built
         * here is "byte[][]" ("[[B"), not the generic
         * "[Ljava/lang/Object;" fallback below (which isn't
         * assignable to the method's real, exact parameter
         * type). */
        char *elem_desc = type_to_descriptor(elem_type);
        size_t len = strlen(elem_desc) + 2;  /* "[" + desc + null */
        array_type_str = malloc(len);
        snprintf(array_type_str, len, "[%s", elem_desc);
        free(elem_desc);
    } else if (elem_type && type_kind_to_atype(elem_type->kind) >= 0) {
        /* Primitive element ("int... values" builds an "[I") */
        char *elem_desc = type_to_descriptor(elem_type);
        size_t len = strlen(elem_desc) + 2;
        array_type_str = malloc(len);
        snprintf(array_type_str, len, "[%s", elem_desc);
        free(elem_desc);
    } else {
        array_type_str = strdup("[Ljava/lang/Object;");
    }

    /* Create the array */
    if (elem_type && elem_type->kind == TYPE_CLASS) {
        char *internal = class_to_internal_name(elem_type->data.class_type.name);
        uint16_t class_ref = cp_add_class(cp, internal);
        bc_emit(mg->code, OP_ANEWARRAY);
        bc_emit_u2(mg->code, class_ref);
        free(internal);
    } else if (elem_type && elem_type->kind == TYPE_ARRAY) {
        /* ANEWARRAY's class constant for an array element type is
         * the element's own full descriptor ("[B"), not an
         * unwrapped internal name (JVMS 4.4.1). */
        char *elem_desc = type_to_descriptor(elem_type);
        uint16_t class_ref = cp_add_class(cp, elem_desc);
        bc_emit(mg->code, OP_ANEWARRAY);
        bc_emit_u2(mg->code, class_ref);
        free(elem_desc);
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
    /* Array is now on stack (replaced size).
     * Update stackmap: pop int (size), push array type */
    mg_pop_typed(mg, 1);  /* Pop the int size from stackmap */
    mg_push_object(mg, array_type_str);  /* Push array type */
    
    /* Store each varargs element */
    int va_idx = 0;
    for (slist_t *va_node = node; va_node && va_idx < varargs_count;
         va_node = va_node->next, va_idx++) {
        ast_node_t *va_arg = (ast_node_t *)va_node->data;
        
        /* Dup array ref */
        bc_emit(mg->code, OP_DUP);
        mg_push_object(mg, array_type_str);  /* Duplicated array reference */
        
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
            free(array_type_str);
            return false;
        }
        
        /* Box primitive if needed for Object[] */
        type_kind_t va_kind = get_expr_type_kind(mg, va_arg);
        if (va_arg->sem_type) {
            va_kind = va_arg->sem_type->kind;
        }

        /* elem_type->kind == TYPE_CLASS alone missed a generic
         * varargs parameter (e.g. "<T> List<T> asList(T... a)",
         * matching java.util.Arrays.asList - gumdrop calls it with
         * mixed int/String arguments) whose own element type is a
         * bare type variable (TYPE_TYPEVAR), which - after erasure -
         * is exactly as much a reference array as TYPE_CLASS is; the
         * "Create the array" logic just above already treats it
         * that way (falling through to its own "Default to
         * Object[]" ANEWARRAY branch), but this boxing check never
         * matched it, so an int literal argument got AASTORE'd
         * unboxed (VerifyError: "Bad type on operand stack",
         * "Type integer ... is not assignable to 'java/lang/Object'").
         * The correct test mirrors the array-creation logic itself:
         * box exactly when the array being built is NOT one of the
         * primitive-element arrays created via NEWARRAY above. */
        bool va_array_is_primitive = elem_type && type_kind_to_atype(elem_type->kind) >= 0;
        if (!va_array_is_primitive &&
            va_kind >= TYPE_BOOLEAN && va_kind <= TYPE_DOUBLE) {
            emit_boxing(mg, cp, va_kind);
        } else if (va_array_is_primitive && va_kind >= TYPE_BOOLEAN && va_kind <= TYPE_DOUBLE &&
                   va_kind != elem_type->kind) {
            /* Widen a narrower primitive argument to the varargs
             * array's own declared primitive element type (JLS
             * 5.1.2) - e.g. an int literal argument like "1" or
             * "1000" passed for a "double... buckets" parameter
             * (Arrays.asList-style mixed literals, matching
             * gumdrop's own DoubleHistogram.Builder.
             * setExplicitBuckets(0.5, 1, 2, 5, ..., 1000)) leaves a
             * plain int on the stack, never widened to double -
             * the subsequent DASTORE (selected from elem_type,
             * correctly DOUBLE) then rejected it (VerifyError:
             * "Bad type on operand stack", "Type integer ... is
             * not assignable to double"). coerce_stack_value()
             * already implements exactly this widening for
             * ordinary (non-varargs) arguments via
             * coerce_arg_to_param() below - reuse it here instead
             * of duplicating the widen-opcode selection logic. */
            coerce_stack_value(mg, cp, va_kind, NULL, elem_type->kind, NULL);
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
            if (elem_type->kind == TYPE_LONG || elem_type->kind == TYPE_DOUBLE) {
                /* A long/double value occupies an extra word beyond
                 * the uniform "1 word" the shared pop below
                 * accounts for - mirrors the identical extra pop
                 * already done for LASTORE/DASTORE in ordinary
                 * (non-varargs) array-element assignment codegen
                 * a little later in this file. Without it,
                 * mg->stack_depth (and the stackmap's own mirrored
                 * word count, both tracked in real JVM words, not
                 * per-value entries - confirmed via
                 * stackmap_push_long/double(), which each push
                 * TWO entries) under-popped by one word per wide
                 * varargs element, drifting further out of sync
                 * with the actual bytecode on every iteration of
                 * a multi-element wide-typed varargs array (e.g.
                 * a "double... buckets" call with several
                 * elements) until a later stack-depth-sensitive
                 * check (an ifeq's own stack-size verification)
                 * finally caught the accumulated mismatch. */
                mg_pop_typed(mg, 1);
            }
        } else {
            bc_emit(mg->code, OP_AASTORE);
        }
        mg_pop_typed(mg, 3);  /* Pop array, index, value */
    }
    
    free(array_type_str);
    return true;
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
    /* Track if receiver is an interface type (needs invokeinterface, not
     * invokevirtual). Every one of this function's own
     * "is_interface_call = (...)" assignments below checks kind ==
     * SYM_INTERFACE || kind == SYM_ANNOTATION - an annotation type
     * declaration is ALWAYS, unconditionally an interface at the JVM
     * level too (JLS 9.6), but genesis represents it as its own distinct
     * SYM_ANNOTATION symbol kind rather than SYM_INTERFACE. Missing the
     * SYM_ANNOTATION half anywhere here emits an illegal invokevirtual
     * for a call on an annotation-typed receiver/element method (e.g.
     * "marker.value()") instead of invokeinterface: IncompatibleClassChangeError
     * ("Found interface X, but class was expected") at runtime. Confirmed
     * against GitHub issue #1's own nested-annotation-reflection repro. */
    bool is_interface_call = false;
    bool use_invokespecial = false;  /* For super.method() and K.super.method() calls */
    const char *target_class = NULL;
    ast_node_t *receiver = NULL;
    slist_t *args = children;
    bool is_void_return = false;
    char *custom_descriptor = NULL;
    symbol_t *method_sym = NULL;  /* Resolved method symbol */
    /* Set when an unqualified instance-method call resolves to an
     * ENCLOSING class's method (not our own, not an inherited one) - see
     * the matching comment where this is set, further down. */
    symbol_t *implicit_call_enclosing_owner = NULL;

    /* Check if this is an explicit receiver call (obj.method()) vs implicit (method(args)) */
    bool has_explicit_receiver = (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER) != 0;
    
    /* Handle array.clone() specially - arrays have covariant clone() */
    if (has_explicit_receiver && children && strcmp(method_name, "clone") == 0) {
        ast_node_t *first = (ast_node_t *)children->data;
        type_t *recv_type = first->sem_type;
        
        if (!recv_type) {
            recv_type = get_expression_type(mg->class_gen->sem, first);
        }
        
        if (recv_type && recv_type->kind == TYPE_ARRAY) {
            /* Generate receiver expression */
            if (!codegen_expr(mg, first, cp)) {
                return false;
            }
            
            /* Generate: invokevirtual [ArrayType].clone()Ljava/lang/Object; */
            char *array_desc = type_to_descriptor(recv_type);
            uint16_t methodref = cp_add_methodref(cp, array_desc, "clone", "()Ljava/lang/Object;");
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, methodref);
            
            /* Stack: pop array ref, push Object */
            mg_pop_typed(mg, 1);
            mg_push_object(mg, "java/lang/Object");
            
            /* Generate: checkcast [ArrayType] */
            uint16_t class_index = cp_add_class(cp, array_desc);
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, class_index);
            
            /* Stack: Object becomes array type */
            mg_pop_typed(mg, 1);
            mg_push_object(mg, array_desc);
            
            free(array_desc);
            return true;
        }
    }
    
    /* The other java.lang.Object methods on an array receiver: an array's
     * only own method is clone() (above); getClass/hashCode/equals/toString
     * are Object's, so the owner is java/lang/Object - never the array's
     * element class, and never absent for a primitive array. */
    if (has_explicit_receiver && children &&
        (strcmp(method_name, "getClass") == 0 || strcmp(method_name, "hashCode") == 0 ||
         strcmp(method_name, "toString") == 0 || strcmp(method_name, "equals") == 0)) {
        ast_node_t *first = (ast_node_t *)children->data;
        type_t *recv_type = first->sem_type;
        if (!recv_type) {
            recv_type = get_expression_type(mg->class_gen->sem, first);
        }
        bool is_equals = strcmp(method_name, "equals") == 0;
        if (recv_type && recv_type->kind == TYPE_ARRAY &&
            (is_equals ? (children->next && !children->next->next) : !children->next)) {
            if (!codegen_expr(mg, first, cp)) {
                return false;
            }
            const char *desc;
            if (is_equals) {
                if (!codegen_expr(mg, (ast_node_t *)children->next->data, cp)) {
                    return false;
                }
                desc = "(Ljava/lang/Object;)Z";
            } else if (strcmp(method_name, "getClass") == 0) {
                desc = "()Ljava/lang/Class;";
            } else if (strcmp(method_name, "hashCode") == 0) {
                desc = "()I";
            } else {
                desc = "()Ljava/lang/String;";
            }
            uint16_t methodref = cp_add_methodref(cp, "java/lang/Object", method_name, desc);
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, methodref);
            mg_pop_typed(mg, is_equals ? 2 : 1);
            if (strcmp(method_name, "getClass") == 0) {
                mg_push_object(mg, "java/lang/Class");
            } else if (strcmp(method_name, "toString") == 0) {
                mg_push_object(mg, "java/lang/String");
            } else {
                mg_push_int(mg);
            }
            return true;
        }
    }

    /* FIRST: Check if first child is a field access (e.g., System.out.println) 
     * Only do this if there's an explicit receiver - otherwise first child is just an argument */
    if (has_explicit_receiver && children) {
        ast_node_t *first = (ast_node_t *)children->data;
        if (first->type == AST_FIELD_ACCESS) {
            /* Check for qualified super call (K.super.m()) */
            const char *field_name = first->data.node.name;
            bool handled_qualified_super = false;
            
            if (field_name && strcmp(field_name, "super") == 0) {
                /* This is a qualified super call: K.super.m() 
                 * The child of the field access is the qualifying type (K) */
                slist_t *fa_children = first->data.node.children;
                if (fa_children) {
                    ast_node_t *qualifier = (ast_node_t *)fa_children->data;
                    const char *qualifier_name = NULL;
                    
                    if (qualifier->type == AST_IDENTIFIER) {
                        qualifier_name = qualifier->data.leaf.name;
                    }
                    
                    if (qualifier_name && mg->class_gen) {
                        /* Look up the qualifying interface/class */
                        symbol_t *qualifier_sym = NULL;
                        
                        /* First check if semantic analysis resolved it */
                        if (qualifier->sem_symbol) {
                            qualifier_sym = qualifier->sem_symbol;
                        }
                        
                        /* Check types cache */
                        if (!qualifier_sym && mg->class_gen->sem) {
                            type_t *q_type = hashtable_lookup(mg->class_gen->sem->types, qualifier_name);
                            if (q_type && q_type->kind == TYPE_CLASS) {
                                qualifier_sym = q_type->data.class_type.symbol;
                            }
                        }
                        
                        /* If not found in types cache, check nested types in current class */
                        if (!qualifier_sym && mg->class_gen->class_sym &&
                            mg->class_gen->class_sym->data.class_data.members) {
                            qualifier_sym = scope_lookup_local(
                                mg->class_gen->class_sym->data.class_data.members, qualifier_name);
                        }
                        
                        /* Also check enclosing class hierarchy (for nested classes) */
                        if (!qualifier_sym && mg->class_gen->class_sym) {
                            symbol_t *enc = mg->class_gen->class_sym->data.class_data.enclosing_class;
                            while (!qualifier_sym && enc) {
                                if (enc->data.class_data.members) {
                                    qualifier_sym = scope_lookup_local(
                                        enc->data.class_data.members, qualifier_name);
                                }
                                enc = enc->data.class_data.enclosing_class;
                            }
                        }
                        
                        /* Superclass chain (e.g. Mailbox.super.search in a subclass) */
                        if (!qualifier_sym && mg->class_gen->class_sym) {
                            symbol_t *super = mg->class_gen->class_sym->data.class_data.superclass;
                            while (super && !qualifier_sym) {
                                if (super->name && strcmp(super->name, qualifier_name) == 0) {
                                    qualifier_sym = super;
                                } else if (super->qualified_name) {
                                    const char *simple = super->qualified_name;
                                    const char *last_dot = strrchr(super->qualified_name, '.');
                                    if (last_dot) {
                                        simple = last_dot + 1;
                                    }
                                    if (strcmp(simple, qualifier_name) == 0) {
                                        qualifier_sym = super;
                                    }
                                }
                                super = super->data.class_data.superclass;
                            }
                        }

                        /* Also check implemented interfaces for qualifying interface */
                        if (!qualifier_sym && mg->class_gen->class_sym) {
                            slist_t *ifaces = mg->class_gen->class_sym->data.class_data.interfaces;
                            for (slist_t *i = ifaces; i && !qualifier_sym; i = i->next) {
                                symbol_t *iface = (symbol_t *)i->data;
                                if (iface && iface->name && strcmp(iface->name, qualifier_name) == 0) {
                                    qualifier_sym = iface;
                                }
                                if (!qualifier_sym && iface && iface->qualified_name) {
                                    const char *simple = iface->qualified_name;
                                    const char *last_dot = strrchr(iface->qualified_name, '.');
                                    if (last_dot) {
                                        simple = last_dot + 1;
                                    }
                                    if (strcmp(simple, qualifier_name) == 0) {
                                        qualifier_sym = iface;
                                    }
                                }
                            }
                        }

                        /* Same-package interface/class (e.g. AuthenticatedHandler in ftp.server) */
                        if (!qualifier_sym && qualifier_name && mg->class_gen->class_sym &&
                            mg->class_gen->class_sym->qualified_name && mg->class_gen->sem) {
                            const char *cur_q = mg->class_gen->class_sym->qualified_name;
                            const char *dot = strrchr(cur_q, '.');
                            if (dot) {
                                char fq[512];
                                snprintf(fq, sizeof(fq), "%.*s.%s",
                                         (int)(dot - cur_q), cur_q, qualifier_name);
                                qualifier_sym = load_external_class(mg->class_gen->sem, fq);
                            }
                        }
                        
                        if (qualifier_sym) {
                            /* Found the qualifying interface/class */
                            target_class = qualifier_sym->qualified_name ?
                                class_to_internal_name(qualifier_sym->qualified_name) :
                                class_to_internal_name(qualifier_sym->name);
                            
                            /* Use invokespecial for qualified super calls */
                            use_invokespecial = true;
                            is_interface_call = (qualifier_sym->kind == SYM_INTERFACE || qualifier_sym->kind == SYM_ANNOTATION);
                            
                            /* Skip the receiver in argument list */
                            args = children->next;
                            
                            /* Look up method in the qualifying interface */
                            if (qualifier_sym->data.class_data.members) {
                                method_sym = lookup_method_in_hierarchy(qualifier_sym, method_name, NULL);
                            }
                            
                            /* Mark as handled - don't fall through to other field access handling */
                            handled_qualified_super = true;
                        }
                    }
                }
                if (!handled_qualified_super && field_name &&
                    strcmp(field_name, "super") == 0) {
                    /* K.super.m() but qualifier not resolved above: still not a field load */
                    args = children->next;
                    use_invokespecial = true;
                }
            }
            
            /* Check if the field access is actually a class reference (FQN like java.util.Objects) */
            if (!handled_qualified_super && first->sem_symbol && 
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
                is_interface_call = (class_sym->kind == SYM_INTERFACE || class_sym->kind == SYM_ANNOTATION);
                
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
                
                /* Look up the method in the class - store in semantic symbol for later use.
                 * Only as a FALLBACK: semantic analysis's AST_FIELD_ACCESS receiver
                 * handling (semantic.c) already resolves this exact call shape
                 * (ClassName.staticMethod(...) via a fully-qualified-name receiver)
                 * through find_best_method_by_types(), which does real type-based
                 * overload resolution (including preferring a static varargs method
                 * over same-arity instance overloads). scope_lookup_method_with_args()
                 * here does no such thing - it matches by raw argument COUNT alone,
                 * with no static/instance filtering and no type checking, and simply
                 * returns whichever same-arity overload it meets first in hashtable
                 * bucket order. Unconditionally overwriting expr->sem_symbol with its
                 * result therefore could - and for java.text.MessageFormat.format(...)
                 * did - clobber an already-correct resolution with a wrong one (e.g.
                 * picking MessageFormat's instance format(Object[],StringBuffer,
                 * FieldPosition) over the intended static format(String,Object...)
                 * varargs method, since both have arity 3 and the varargs call only
                 * has an "apparent" arity of 3 after argument collapse), producing an
                 * invokestatic with the wrong owner method's descriptor
                 * (VerifyError: Bad type on operand stack). Only fall back to the
                 * naive lookup when semantic analysis didn't already resolve one. */
                if (!expr->sem_symbol && class_sym->data.class_data.members) {
                    /* Count arguments for overload resolution */
                    int arg_count = 0;
                    for (slist_t *a = args; a; a = a->next) {
                        arg_count++;
                    }

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
            } else if (!handled_qualified_super &&
                       !(field_name && strcmp(field_name, "super") == 0)) {
                /* Regular field access (e.g., System.out.println) */
                receiver = first;
                args = children->next;
                target_class = get_field_access_type_class(first);
                
                /* Check if the field's type is an interface */
                if (first->sem_type && first->sem_type->kind == TYPE_CLASS &&
                    first->sem_type->data.class_type.symbol) {
                    is_interface_call = (first->sem_type->data.class_type.symbol->kind == SYM_INTERFACE || first->sem_type->data.class_type.symbol->kind == SYM_ANNOTATION);
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
    
    /* PREFER semantic analysis result - it has proper overload resolution */
    if (expr->sem_symbol && expr->sem_symbol->kind == SYM_METHOD) {
        method_sym = expr->sem_symbol;
    }
    
    if (!receiver && !is_static && has_explicit_receiver && children) {
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
                        is_interface_call = (class_sym->kind == SYM_INTERFACE || class_sym->kind == SYM_ANNOTATION);
                    }
                }
            }
        } else if (first->type == AST_SUPER_EXPR) {
            /* super.method() - call superclass method with invokespecial */
            receiver = first;
            args = children->next;
            use_invokespecial = true;
            
            /* Get superclass and look up method there */
            if (mg->class_gen) {
                target_class = mg->class_gen->superclass ? 
                    mg->class_gen->superclass : "java/lang/Object";
                
                /* Look up method in superclass */
                if (!method_sym && mg->class_gen->class_sym) {
                    symbol_t *super_sym = mg->class_gen->class_sym->data.class_data.superclass;
                    if (super_sym && super_sym->data.class_data.members) {
                        method_sym = lookup_method_in_hierarchy(super_sym, method_name, NULL);
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
                is_interface_call = (first->sem_symbol->kind == SYM_INTERFACE || first->sem_symbol->kind == SYM_ANNOTATION);
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
                        is_interface_call = (nested->kind == SYM_INTERFACE || nested->kind == SYM_ANNOTATION);
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
                                if (!method_sym) {
                                    method_sym = found_method;
                                }
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
                            if (!method_sym) {
                                method_sym = found_method;
                            }
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
                
                if (recv_type && recv_type->kind == TYPE_CLASS) {
                    recv_class_sym = recv_type->data.class_type.symbol;
                    
                    /* If symbol not set, try to load the class externally */
                    if (!recv_class_sym && recv_type->data.class_type.name && 
                        mg->class_gen && mg->class_gen->sem) {
                        recv_class_sym = load_external_class(mg->class_gen->sem,
                            recv_type->data.class_type.name);
                    }
                }
                
                if (!recv_class_sym) {
                    /* sem_type not available - try to get class from local tracking */
                    const char *local_class = mg_local_class_name(mg, name);
                    if (local_class && mg->class_gen && mg->class_gen->sem) {
                        /* Look up the class symbol by name */
                        /* Convert internal name (com/example/Foo) back to qualified (com.example.Foo) */
                        char *qualified = strdup(local_class);
                        for (char *p = qualified; *p; p++) {
                            if (*p == '/') {
                                *p = '.';
                            }
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
                    
                    /* For interface types, also check Object for methods like hashCode, equals 
                     * since all interface instances are also Object instances */
                    if (!found_method && recv_class_sym->kind == SYM_INTERFACE && 
                        mg->class_gen && mg->class_gen->sem) {
                        symbol_t *object_sym = load_external_class(mg->class_gen->sem, "java.lang.Object");
                        if (object_sym) {
                            found_method = lookup_method_in_hierarchy(object_sym, method_name, &owner_class);
                        }
                    }
                    
                    if (found_method) {
                        /* Method found in receiver's class or superclass */
                        receiver = first;
                        args = children->next;
                        /* Prefer semantic analysis result for correct overload */
                        if (!method_sym) {
                            method_sym = found_method;
                        }
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        /* Reference the RECEIVER's own static type in the
                         * invoke's constant-pool entry, not owner_class (the
                         * class that actually DECLARES the method, found by
                         * walking the superclass chain) - matching what
                         * javac always does. The JVM resolves invokevirtual/
                         * invokeinterface through the full runtime hierarchy
                         * regardless of which valid ancestor class the
                         * symbolic reference names, so recv_class_sym is
                         * always a safe, correct choice when known - and
                         * using owner_class instead breaks the moment a
                         * method is inherited (not overridden) from an
                         * INACCESSIBLE ancestor while the receiver's own
                         * type is accessible, e.g. StringBuilder.setLength()
                         * is only ever declared on the package-private
                         * java.lang.AbstractStringBuilder - genesis emitted
                         * "invokevirtual AbstractStringBuilder.setLength"
                         * from calling code outside java.lang, which is
                         * illegal even though setLength() itself is public
                         * (IllegalAccessError: "failed to access class
                         * java.lang.AbstractStringBuilder"), confirmed
                         * against gumdrop's own SaslUtils.parseDigestParams()
                         * ("key.setLength(0); value.setLength(0);"). */
                        symbol_t *target_owner = recv_class_sym ? recv_class_sym : owner_class;
                        target_class = target_owner->qualified_name ?
                            class_to_internal_name(target_owner->qualified_name) :
                            mg->class_gen->internal_name;
                        is_interface_call = (target_owner->kind == SYM_INTERFACE || target_owner->kind == SYM_ANNOTATION);
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
                
                /* A field of type-variable type (N extends Number) dispatches on its bound */
                type_t *field_recv_type = field_sym ? field_sym->type : NULL;
                if (field_recv_type && field_recv_type->kind == TYPE_TYPEVAR &&
                    field_recv_type->data.type_var.bound) {
                    field_recv_type = field_recv_type->data.type_var.bound;
                }
                
                if (field_recv_type && field_recv_type->kind == TYPE_CLASS) {
                    /* It's a field with a class type - it can be a receiver */
                    symbol_t *recv_class_sym = field_recv_type->data.class_type.symbol;
                    
                    /* Try to load the class if symbol not set */
                    if (!recv_class_sym && field_recv_type->data.class_type.name && mg->class_gen->sem) {
                        recv_class_sym = load_external_class(mg->class_gen->sem, 
                            field_recv_type->data.class_type.name);
                        if (recv_class_sym) {
                            field_recv_type->data.class_type.symbol = recv_class_sym;
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
                            if (!method_sym) {
                                method_sym = found_method;
                            }
                            is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                            /* Reference the FIELD's own declared type in the
                             * invoke's constant-pool entry, not owner_class
                             * (the class that actually DECLARES the method) -
                             * same fix, and same reasoning, as the plain
                             * local-variable receiver case above: javac
                             * always binds an invoke to the receiver
                             * expression's static type, and using the
                             * actual (possibly less accessible) declaring
                             * ancestor instead breaks the moment a method is
                             * inherited but not overridden from an
                             * INACCESSIBLE ancestor - e.g. an implicit-this
                             * field access to a StringBuilder field calling
                             * setLength() (declared only on the package-
                             * private java.lang.AbstractStringBuilder).
                             * Confirmed against gumdrop's own
                             * FtpProtocolHandler.resetLineState()'s
                             * "argsBuilder.setLength(0);", argsBuilder being
                             * an instance field. */
                            symbol_t *target_owner = recv_class_sym ? recv_class_sym : owner_class;
                            target_class = target_owner->qualified_name ?
                                class_to_internal_name(target_owner->qualified_name) :
                                class_to_internal_name(recv_class_sym->qualified_name);
                            is_interface_call = (target_owner->kind == SYM_INTERFACE || target_owner->kind == SYM_ANNOTATION);
                        }
                    }
                }
            }
        } else if (first->type == AST_METHOD_CALL) {
            /* Method call as receiver: obj.method1().method2() */
            /* The result of method1() is the receiver for method2() */
            type_t *recv_type = first->sem_type;
            
            /* If sem_type not set, try to compute it from semantic analysis */
            if (!recv_type && mg->class_gen && mg->class_gen->sem) {
                recv_type = get_expression_type(mg->class_gen->sem, first);
            }
            
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
                    /* Save original receiver class - this is what we use for target_class
                     * even if the method is found in a superinterface */
                    symbol_t *original_recv_class = recv_class_sym;
                    
                    /* Count arguments for overload resolution (receiver is first) */
                    int arg_count = 0;
                    for (slist_t *a = children->next; a; a = a->next) {
                        arg_count++;
                    }
                    
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
                    
                    /* Check interface hierarchy if not found (including superinterfaces for interface types) */
                    if (!found_method || found_method->kind != SYM_METHOD) {
                        symbol_t *iface_owner = NULL;
                        found_method = lookup_method_in_interfaces(recv_class_sym, method_name, &iface_owner);
                        /* Note: we don't update recv_class_sym here anymore.
                         * When calling method on a concrete class, target should be the class,
                         * not the interface where the method is declared. The JVM handles dispatch. */
                    }
                    
                    if (found_method && found_method->kind == SYM_METHOD) {
                        receiver = first;
                        args = children->next;
                        /* Prefer semantic analysis result for correct overload */
                        if (!method_sym) {
                            method_sym = found_method;
                        }
                        is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                        /* Use original receiver class for target, not the interface where method was found */
                        target_class = original_recv_class->qualified_name ?
                            class_to_internal_name(original_recv_class->qualified_name) :
                            mg->class_gen->internal_name;
                        /* Only use interface call if receiver itself is an interface type */
                        is_interface_call = (original_recv_class->kind == SYM_INTERFACE || original_recv_class->kind == SYM_ANNOTATION);
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
                        if (!method_sym) {
                            method_sym = found_method;
                        }
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
                
                /* For instance method calls, prefer receiver's type as target class.
                 * This ensures we use invokevirtual on the receiver class, not the
                 * interface where the method might be declared (default methods).
                 * The JVM handles method dispatch correctly. */
                if (!is_static && first->sem_type && first->sem_type->kind == TYPE_CLASS && 
                           first->sem_type->data.class_type.name) {
                    /* Use receiver's type as target - correct for virtual dispatch */
                    target_class = class_to_internal_name(first->sem_type->data.class_type.name);
                    symbol_t *recv_sym = first->sem_type->data.class_type.symbol;
                    is_interface_call = (recv_sym && (recv_sym->kind == SYM_INTERFACE || recv_sym->kind == SYM_ANNOTATION));
                } else {
                    /* Static method or no receiver type - use method's owner */
                    symbol_t *owner_class = method_sym->scope ? method_sym->scope->owner : NULL;
                    if (owner_class && owner_class->qualified_name) {
                        target_class = class_to_internal_name(owner_class->qualified_name);
                        is_interface_call = (owner_class->kind == SYM_INTERFACE || owner_class->kind == SYM_ANNOTATION);
                    }
                }
            }
        }
    }

    /* Explicit receiver expression (e.g. obj.m1().m2()) when semantic analysis
     * already resolved the callee but type-based receiver discovery failed. */
    if (!receiver && !is_static && has_explicit_receiver && children &&
        (!method_sym || !(method_sym->modifiers & MOD_STATIC))) {
        ast_node_t *first = (ast_node_t *)children->data;
        switch (first->type) {
        case AST_METHOD_CALL:
        case AST_FIELD_ACCESS:
            if (first->data.node.name &&
                strcmp(first->data.node.name, "super") == 0) {
                break;
            }
            receiver = first;
            args = children->next;
            break;
        case AST_IDENTIFIER:
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
        case AST_NEW_OBJECT:
        case AST_ARRAY_ACCESS:
        case AST_PARENTHESIZED:
        case AST_CAST_EXPR:
        case AST_CONDITIONAL_EXPR:
        case AST_CLASS_LITERAL:
            receiver = first;
            args = children->next;
            if (!target_class) {
                type_t *recv_type = first->sem_type;
                if (!recv_type && mg->class_gen && mg->class_gen->sem) {
                    recv_type = get_expression_type(mg->class_gen->sem, first);
                }
                if (recv_type && recv_type->kind == TYPE_TYPEVAR &&
                    recv_type->data.type_var.bound) {
                    recv_type = recv_type->data.type_var.bound;
                }
                if (recv_type && recv_type->kind == TYPE_CLASS &&
                    recv_type->data.class_type.name) {
                    target_class = class_to_internal_name(recv_type->data.class_type.name);
                    symbol_t *recv_sym = recv_type->data.class_type.symbol;
                    is_interface_call = (recv_sym && (recv_sym->kind == SYM_INTERFACE || recv_sym->kind == SYM_ANNOTATION));
                }
            }
            break;
        default:
            break;
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
                is_interface_call = (owner_class->kind == SYM_INTERFACE || owner_class->kind == SYM_ANNOTATION);

                /* An unqualified instance-method call resolved to a class
                 * other than our own is either an INHERITED method
                 * (reachable by walking our own superclass chain - still
                 * invoked on plain "this") or an enclosing instance's
                 * method (an inner class calling an outer method with no
                 * explicit qualifier, e.g. "shutdown();" from inside a
                 * non-static inner class, resolved by semantic analysis
                 * via the enclosing-class chain, not inheritance) - which
                 * needs the SAME instance actually walked at runtime, not
                 * "this". Distinguish the two so the receiver-loading
                 * code below knows to walk this$0 instead of emitting a
                 * bare aload_0, which would push the wrong (inner) object
                 * as the receiver (VerifyError: "Bad type on operand
                 * stack", the invoked method's owner type not assignable
                 * from the inner class pushed). */
                if (!is_static && owner_class != class_sym) {
                    bool is_inherited = false;
                    for (symbol_t *s = class_sym->data.class_data.superclass; s;
                         s = s->data.class_data.superclass) {
                        if (s == owner_class) {
                            is_inherited = true;
                            break;
                        }
                    }
                    if (!is_inherited) {
                        /* owner_class might not be a LEXICALLY enclosing
                         * class at all - it could be a superclass of one
                         * instead (e.g. an anonymous class defined inside
                         * an instance method of class B extends A, calling
                         * A's own inherited method unqualified: the
                         * method's declaring symbol is A, but A is never
                         * itself in the this$0 chain here - B is, and B
                         * IS-A A). codegen_load_enclosing_this() walks the
                         * this$0 chain by exact enclosing-class identity,
                         * so asking it for a class that isn't actually
                         * anywhere in that chain makes it walk straight
                         * past the real target (B) all the way to the
                         * outermost enclosing class instead: VerifyError
                         * "Bad type on operand stack", the outermost
                         * class's own type not assignable to A. Walk the
                         * enclosing chain looking for the nearest class
                         * that either IS owner_class or INHERITS from it,
                         * and target that class instead. Confirmed against
                         * gumdrop's own CertificateCompressor.BrotliStream
                         * (extends Decompressor), whose constructor's
                         * anonymous BrotliDefaultHandler calls "emit(data)"
                         * unqualified - Decompressor.emit(), inherited by
                         * BrotliStream, never lexically enclosing anything
                         * here; BrotliStream itself is. */
                        symbol_t *walk_target = NULL;
                        for (symbol_t *enc = class_sym->data.class_data.enclosing_class; enc;
                             enc = enc->data.class_data.enclosing_class) {
                            if (enc == owner_class) {
                                walk_target = enc;
                                break;
                            }
                            bool enc_inherits = false;
                            for (symbol_t *s = enc->data.class_data.superclass; s;
                                 s = s->data.class_data.superclass) {
                                if (s == owner_class) {
                                    enc_inherits = true;
                                    break;
                                }
                            }
                            if (enc_inherits) {
                                walk_target = enc;
                                break;
                            }
                        }
                        implicit_call_enclosing_owner = walk_target ? walk_target : owner_class;
                    }
                }
            } else {
                /* Method belongs to current class */
                target_class = mg->class_gen->internal_name;
                is_interface_call = (class_sym->kind == SYM_INTERFACE || class_sym->kind == SYM_ANNOTATION);
            }
            /* All children are arguments only when there is no explicit receiver */
            if (!receiver) {
                args = children;
            }
        }
        else if (!is_static && !method_sym && class_sym->data.class_data.members) {
            /* Use scope_lookup_method which handles method overloads correctly */
            symbol_t *found_method = scope_lookup_method(
                class_sym->data.class_data.members, method_name);
            if (found_method && found_method->kind == SYM_METHOD) {
                method_sym = found_method;
                is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                target_class = mg->class_gen->internal_name;
                /* If current class is an interface, use invokeinterface for instance methods */
                is_interface_call = (class_sym->kind == SYM_INTERFACE || class_sym->kind == SYM_ANNOTATION);
                
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
                if (custom_descriptor) {
                    free(custom_descriptor);
                }
                if (static_import_class) {
                    free(static_import_class);
                }
                return false;
            }
        } else if (implicit_call_enclosing_owner) {
            /* Unqualified call to an enclosing class's instance method
             * (e.g. "shutdown();" from inside a non-static inner class,
             * meaning the outer class's method, not our own) - walk the
             * this$0 chain to load the actual enclosing instance the
             * method must be invoked on, not our own "this". */
            codegen_load_enclosing_this(mg, cp, implicit_call_enclosing_owner);
        } else if (!mg->is_static || use_invokespecial) {
            /* Implicit 'this' (including K.super.m() in instance methods) */
            bc_emit(mg->code, OP_ALOAD_0);
            /* Push 'this' with actual class type for stackmap */
            if (mg->class_gen && mg->class_gen->internal_name) {
                mg_push_object(mg, mg->class_gen->internal_name);
            } else {
                mg_push_null(mg);
            }
        } else {
            /* Error: trying to call instance method without receiver in static context */
            fprintf(stderr, "codegen: cannot call instance method '%s' without receiver in static context\n", method_name);
            if (custom_descriptor) {
                free(custom_descriptor);
            }
            if (static_import_class) {
                free(static_import_class);
            }
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
            /* Everything from here on goes into the varargs array */
            if (!codegen_varargs_tail(mg, cp, varargs_param, node, false)) {
                if (custom_descriptor) {
                    free(custom_descriptor);
                }
                if (static_import_class) {
                    free(static_import_class);
                }
                return false;
            }

            /* Skip remaining args since we processed them */
            break;
        }
        
        /* Regular argument (not varargs) */
        if (!codegen_expr(mg, arg, cp)) {
            if (custom_descriptor) {
                free(custom_descriptor);
            }
            if (static_import_class) {
                free(static_import_class);
            }
            return false;
        }
        
        /* Check if boxing/unboxing needed for this argument */
        if (param_node) {
            coerce_arg_to_param(mg, cp, arg, (symbol_t *)param_node->data,
                                custom_descriptor != NULL);
            param_node = param_node->next;
        }
    }
    
    /* Handle case where varargs method is called with no varargs arguments */
    if (is_varargs_method && varargs_param) {
        /* Count actual arguments */
        int arg_count = 0;
        for (slist_t *n = args; n; n = n->next) {
            arg_count++;
        }
        
        /* If we have exactly the fixed params (no varargs provided), create empty array */
        if (arg_count == fixed_param_count) {
            if (!codegen_varargs_tail(mg, cp, varargs_param, NULL, false)) {
                if (custom_descriptor) {
                    free(custom_descriptor);
                }
                if (static_import_class) {
                    free(static_import_class);
                }
                return false;
            }
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
        /* Before defaulting to the CURRENT class (correct only for an
         * implicit, receiver-less call), prefer the actual receiver's own
         * resolved type when this is an explicit-receiver instance call -
         * mirrors the same "use the receiver's type, not the enclosing
         * class" preference already applied a few branches above for
         * other receiver expression kinds (AST_IDENTIFIER/AST_NEW_OBJECT/
         * etc.), just missing here for the case this fell through to
         * (notably a receiver that is ITSELF a method call, e.g.
         * "q.getType().name()" - AST_METHOD_CALL as a receiver sets
         * "receiver" but never target_class). Left unfixed, a method with
         * no symbol resolvable in genesis's own symbol tables - such as
         * an ENUM's inherited java.lang.Enum built-ins (name(), which
         * unlike ordinal() had no dedicated handling anywhere) - silently
         * defaulted to an invokevirtual whose owner is the ENCLOSING
         * class instead of the receiver's real type, confirmed against
         * gumdrop's own DoQStreamHandler, whose "q.getType().name()"
         * (getType() returning the cross-file enum DnsType) produced
         * "invokevirtual DoQStreamHandler.name()" (VerifyError: "Bad
         * type on operand stack", the real DnsType receiver not
         * assignable to DoQStreamHandler). JVMS 5.4.3.3: invokevirtual's
         * own method resolution already walks the receiver class's
         * superclass chain, so naming the receiver's own class here is
         * correct even for an inherited method never redeclared there -
         * exactly what real javac itself emits for this shape. */
        if (!is_static && receiver) {
            type_t *recv_type = receiver->sem_type;
            if (!recv_type && mg->class_gen && mg->class_gen->sem) {
                recv_type = get_expression_type(mg->class_gen->sem, receiver);
            }
            if (recv_type && recv_type->kind == TYPE_TYPEVAR && recv_type->data.type_var.bound) {
                recv_type = recv_type->data.type_var.bound;
            }
            if (recv_type && recv_type->kind == TYPE_CLASS && recv_type->data.class_type.name) {
                target_class = class_to_internal_name(recv_type->data.class_type.name);
                symbol_t *recv_sym = recv_type->data.class_type.symbol;
                is_interface_call = (recv_sym && (recv_sym->kind == SYM_INTERFACE || recv_sym->kind == SYM_ANNOTATION));
            }
        }
        if (!target_class) {
            target_class = mg->class_gen ? mg->class_gen->internal_name : "java/lang/Object";
        }
    }

    /* Use interface methodref for interface calls (both instance and static) */
    uint16_t methodref;
    if (is_interface_call) {
        methodref = cp_add_interface_methodref(cp, target_class, method_name, descriptor);
    } else {
        methodref = cp_add_methodref(cp, target_class, method_name, descriptor);
    }
    free(descriptor);
    
    /* Slots the arguments occupy on the stack after conversion to the parameter
     * types; needed for invokeinterface's count operand and to update tracking. */
    /* Use slot count for args (long/double take 2 slots) */
    /* IMPORTANT: Use parameter types, not argument types, because widening
     * conversions (e.g., int -> long) have already updated the stack to
     * match the parameter types. */
    int arg_slots;
    if (custom_descriptor) {
        /* custom_descriptor is the AUTHORITATIVE descriptor actually
         * emitted for this call (e.g. an enum's synthetic values()/
         * valueOf(String), or a resolved print-method overload) - built
         * independently of method_sym just below, which can be a stale
         * or outright mismatched symbol for exactly these synthetic
         * cases: nothing in genesis's own symbol tables actually
         * declares "valueOf"/"values" on an enum (they're compiler-
         * generated), so semantic analysis's own method_sym resolution
         * can land on an unrelated same-named method from a completely
         * different class. Confirmed against gumdrop's own
         * DnssecTrustAnchorUpdater.loadState(), where "KeyState.
         * valueOf(...)" (KeyState a nested enum) got a 2-parameter
         * method_sym instead of the real 1-parameter synthetic
         * valueOf(String) - undercounting arg_slots by one and popping
         * one slot too many below, wrapping mg->stack_depth to a huge
         * unsigned value and, from that point on, corrupting every
         * later max_stack comparison for the rest of the method
         * (VerifyError: "Operand stack overflow", confirmed reduced to
         * a minimal repro and traced by hand against the real bytecode
         * before finding this). Parse arg_slots directly from
         * custom_descriptor's own parameter list instead of trusting
         * method_sym whenever both are present. */
        method_descriptor_t *md = descriptor_parse_method(custom_descriptor);
        if (md) {
            arg_slots = 0;
            for (int i = 0; i < md->param_count; i++) {
                arg_slots += (md->params[i].type == DESC_LONG ||
                              md->params[i].type == DESC_DOUBLE) ? 2 : 1;
            }
            method_descriptor_free(md);
        } else {
            arg_slots = calculate_arg_slot_count(mg, args);
        }
    } else if (is_varargs_method && varargs_param) {
        /* For varargs, we pushed: fixed args + 1 array (regardless of varargs count) */
        /* Count fixed parameter slots based on PARAMETER types */
        arg_slots = 0;
        if (method_sym && method_sym->data.method_data.parameters) {
            slist_t *param = method_sym->data.method_data.parameters;
            for (int idx = 0; idx < fixed_param_count && param; idx++, param = param->next) {
                symbol_t *p = (symbol_t *)param->data;
                if (p && p->type) {
                    arg_slots += (p->type->kind == TYPE_LONG || p->type->kind == TYPE_DOUBLE) ? 2 : 1;
                } else {
                    arg_slots += 1;
                }
            }
        } else {
            /* Fallback to argument types if method symbol unavailable */
        int idx = 0;
        for (slist_t *n = args; n && idx < fixed_param_count; n = n->next, idx++) {
            ast_node_t *arg = (ast_node_t *)n->data;
            type_kind_t kind = get_arg_type_kind(mg, arg);
            arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
            }
        }
        /* Add 1 for the varargs array */
        arg_slots += 1;
    } else if (method_sym && method_sym->data.method_data.parameters) {
        /* Count slots based on PARAMETER types (accounts for widening conversions) */
        arg_slots = 0;
        for (slist_t *param = method_sym->data.method_data.parameters; param; param = param->next) {
            symbol_t *p = (symbol_t *)param->data;
            if (p && p->type) {
                arg_slots += (p->type->kind == TYPE_LONG || p->type->kind == TYPE_DOUBLE) ? 2 : 1;
    } else {
                arg_slots += 1;
            }
        }
    } else {
        /* Fallback to argument types if method symbol unavailable */
        arg_slots = calculate_arg_slot_count(mg, args);
    }

    /* Emit invoke instruction */
    if (is_static) {
        bc_emit(mg->code, OP_INVOKESTATIC);
        bc_emit_u2(mg->code, methodref);
        /* Calling static interface method requires class file version 52 */
        if (is_interface_call && mg->class_gen) {
            mg->class_gen->has_default_methods = true;
        }
    } else if (use_invokespecial) {
        /* super.method() requires invokespecial to call the superclass method directly */
        bc_emit(mg->code, OP_INVOKESPECIAL);
        bc_emit_u2(mg->code, methodref);
    } else if (is_interface_call) {
        /* invokeinterface has 5 bytes: opcode + index(2) + count(1) + zero(1) */
        bc_emit(mg->code, OP_INVOKEINTERFACE);
        bc_emit_u2(mg->code, methodref);
        /* count = argument slots + 1 (for receiver) */
        int slot_count = arg_slots + 1;
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
    } else if (!is_void_return && expr->sem_type && expr->sem_type->kind == TYPE_ARRAY &&
               method_sym && method_sym->type &&
               ((method_sym->type->kind == TYPE_ARRAY &&
                 method_sym->type->data.array_type.element_type &&
                 method_sym->type->data.array_type.element_type->kind == TYPE_TYPEVAR) ||
                method_sym->type->kind == TYPE_TYPEVAR)) {
        /* Same idea, for a method that returns T[] (e.g.
         * Collection<T>.toArray(T[] a)) - this erases to Object[] (or the
         * type variable's bound array) at the JVM level, same as a bare
         * type-variable return above - OR for a method that returns a
         * bare T (e.g. List<T>.get(int)) whose type ARGUMENT at this
         * call site happens to itself be an array type (e.g. byte[] for
         * a List<byte[]>) - erasing to plain Object either way. If
         * semantic analysis determined a more specific array type at
         * this call site (e.g. String[] for list.toArray(new String[0]),
         * or byte[] for a List<byte[]>.get(i)), narrow it with a
         * checkcast - without this, the erased Object/Object[] was left
         * on the stack wherever the result was used, and the verifier
         * rejected it ("Bad type on operand stack ... not assignable to
         * '[Ljava/lang/String;'", or "Invalid type: 'java/lang/Object'"
         * at whatever instruction - e.g. a baload for a byte[] element
         * access - first required the real array type). Unlike a plain
         * class checkcast, an array checkcast's constant-pool entry is
         * the full descriptor (e.g. "[Ljava/lang/String;"), not an
         * unwrapped internal name. */
        char *actual_desc = type_to_descriptor(expr->sem_type);
        char *erased_desc = type_to_descriptor(method_sym->type);
        if (actual_desc && erased_desc && strcmp(actual_desc, erased_desc) != 0) {
            uint16_t class_idx = cp_add_class(cp, actual_desc);
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, class_idx);
        }
        free(actual_desc);
        free(erased_desc);
    }
    
    /* Update stack: pop receiver (if any) and args, push return value.
     * arg_slots was computed above, from the parameter types. */
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
                    /* Push actual class type for stackmap */
                    if (method_sym->type->data.class_type.name) {
                        char *internal = class_to_internal_name(method_sym->type->data.class_type.name);
                        mg_push_object(mg, internal);
                        free(internal);
                    } else {
                        mg_push_object(mg, "java/lang/Object");
                    }
                    break;
                case TYPE_ARRAY:
                    /* Array return type - build descriptor */
                    if (method_sym->type->data.array_type.element_type) {
                        char *desc = type_to_descriptor(method_sym->type);
                        if (desc) {
                            mg_push_object(mg, desc);
                            free(desc);
                        } else {
                            mg_push_object(mg, "[Ljava/lang/Object;");
                        }
                    } else {
                        mg_push_object(mg, "[Ljava/lang/Object;");
                    }
                    break;
                case TYPE_TYPEVAR:
                    /* A bare type-variable return (e.g. T in
                     * List<T>.get(int)) - method_sym->type never reflects
                     * the call site's own substituted type argument, so
                     * falling to the plain-int default below would track
                     * this as an int even when the checkcast just above
                     * (for a TYPE_CLASS or TYPE_ARRAY substitution)
                     * narrowed the real value to a reference type -
                     * fine as long as the value is consumed before any
                     * frame gets recorded, but wrong (a stale int where a
                     * real reference belongs) the moment it crosses a
                     * loop/if/try boundary first. Prefer expr->sem_type
                     * (the call site's own resolved type) when it's more
                     * specific than a bare type variable; otherwise erase
                     * to Object, matching what the actual bytecode
                     * produces with no narrowing checkcast at all. */
                    if (expr->sem_type && expr->sem_type->kind == TYPE_CLASS) {
                        if (expr->sem_type->data.class_type.name) {
                            char *internal = class_to_internal_name(expr->sem_type->data.class_type.name);
                            mg_push_object(mg, internal);
                            free(internal);
                        } else {
                            mg_push_object(mg, "java/lang/Object");
                        }
                    } else if (expr->sem_type && expr->sem_type->kind == TYPE_ARRAY) {
                        char *desc = type_to_descriptor(expr->sem_type);
                        if (desc) {
                            mg_push_object(mg, desc);
                            free(desc);
                        } else {
                            mg_push_object(mg, "[Ljava/lang/Object;");
                        }
                    } else {
                        mg_push_object(mg, "java/lang/Object");
                    }
                    break;
                default:          mg_push_int(mg); break;  /* boolean, byte, char, short, int */
            }
        } else {
            /* Unknown return type - assume int */
            mg_push_int(mg);
        }
    }
    
    if (static_import_class) {
        free(static_import_class);
    }
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
    
    /* Push 'this' reference. Track it on mg->stackmap as
     * uninitializedThis (matching what local[0] already holds at this
     * point in a constructor, per JVMS 4.10.1.4/4.10.1.6), NOT the
     * class's own real type - "this" genuinely isn't a fully
     * constructed instance yet until the invokespecial below actually
     * runs. Pushing the real type here "happens to work" for the
     * common case, where nothing branches between this aload_0 and the
     * invokespecial (so no explicit frame is ever recorded while this
     * value is still on the stack) - but a constructor ARGUMENT that
     * itself branches (e.g. a ternary, "this(..., masked ?
     * generateMaskingKey() : null, ...)") forces an explicit frame to
     * be recorded at its merge point, mid-argument-list, while "this"
     * is still sitting deeper on the stack: the wrong (already-
     * initialized) tracked type there produced a RECORDED frame that
     * disagreed with the real verifier's own forward dataflow (which
     * correctly still sees uninitializedThis) - VerifyError
     * "Inconsistent stackmap frames ... Type uninitializedThis
     * (current frame, stack[N]) is not assignable to
     * '<TheClass>'". Confirmed against gumdrop's own
     * WebSocketFrame(int,byte[],boolean,boolean)'s delegating
     * this(...) call, whose masking-key argument is exactly such a
     * ternary. stackmap_init_object() (called right after the
     * invokespecial below) still correctly promotes local[0] from
     * uninitializedThis to the real type once construction actually
     * completes - this stack-side push was the only place still using
     * the wrong type. */
    bc_emit(mg->code, OP_ALOAD_0);
    mg_push_uninitialized_this(mg);

    /* An enum's own constructors are compiler-extended with a leading
     * (String name, int ordinal) pair (JVMS 4.1) - already occupying
     * local slots 1 and 2 of every constructor by the time any of its
     * bodies run - which a user-written this(...) call between two of
     * the enum's own constructors never spells out explicitly (Java
     * doesn't let source code reference them at all). Without
     * explicitly forwarding them here, the target constructor - whose
     * own descriptor genesis's enum-constant-instantiation codegen
     * already correctly prefixes with (Ljava/lang/String;I) - would be
     * called with those two arguments simply missing. */
    bool is_enum_this_call = is_this_call && mg->class_gen &&
        mg->class_gen->class_sym && mg->class_gen->class_sym->kind == SYM_ENUM;
    if (is_enum_this_call) {
        bc_emit(mg->code, OP_ALOAD_1);
        mg_push_object(mg, "java/lang/String");
        bc_emit(mg->code, OP_ILOAD_2);
        mg_push_int(mg);
    }

    /* A member inner class's constructors take the enclosing instance as a
     * leading parameter (slot 1), which a user-written this(...) between two
     * of its own constructors never spells out. */
    bool forward_outer = is_this_call && mg->class_gen && mg->class_gen->is_inner_class &&
        !mg->class_gen->is_local_class && !mg->class_gen->is_anonymous_class &&
        mg->class_gen->outer_class_internal;
    /* The same for super(...) when the superclass is a sibling inner class,
     * a member of the very class that encloses this one: the enclosing
     * instance this class received is the one its superclass needs. */
    bool super_outer_in_descriptor = false;
    if (!is_this_call && !forward_outer && mg->class_gen && mg->class_gen->is_inner_class &&
        !mg->class_gen->is_local_class && !mg->class_gen->is_anonymous_class &&
        mg->class_gen->outer_class_internal && mg->class_gen->class_sym) {
        symbol_t *super_sym = mg->class_gen->class_sym->data.class_data.superclass;
        symbol_t *super_outer = super_sym ? super_sym->data.class_data.enclosing_class : NULL;
        if (super_sym && super_outer && super_outer->qualified_name &&
            !(super_sym->modifiers & MOD_STATIC) &&
            super_sym->kind == SYM_CLASS) {
            char *so = class_to_internal_name(super_outer->qualified_name);
            if (so && strcmp(so, mg->class_gen->outer_class_internal) == 0) {
                forward_outer = true;
                /* A class-file superclass's stored descriptor already starts
                 * with the enclosing instance. */
                super_outer_in_descriptor = expr->sem_symbol &&
                    expr->sem_symbol->kind == SYM_CONSTRUCTOR && !expr->sem_symbol->ast &&
                    expr->sem_symbol->data.method_data.descriptor;
            }
            free(so);
        }
    }
    if (forward_outer) {
        bc_emit(mg->code, OP_ALOAD_1);
        mg_push_object(mg, mg->class_gen->outer_class_internal);
    }

    /* Generate constructor arguments, each converted to the resolved target
     * constructor's declared parameter type (widening int to long/double,
     * boxing, unboxing) - the descriptor below names those types. */
    int arg_count = (is_enum_this_call ? 2 : 0) + (forward_outer ? 1 : 0);
    slist_t *ctor_param_node = (expr->sem_symbol && expr->sem_symbol->kind == SYM_CONSTRUCTOR) ?
        expr->sem_symbol->data.method_data.parameters : NULL;
    for (slist_t *node = expr->data.node.children; node; node = node->next) {
        ast_node_t *arg = (ast_node_t *)node->data;
        if (!codegen_expr(mg, arg, cp)) {
            return false;
        }
        if (ctor_param_node) {
            coerce_arg_to_param(mg, cp, arg, (symbol_t *)ctor_param_node->data, false);
            ctor_param_node = ctor_param_node->next;
        }
        arg_count++;
    }
    
    /* Build constructor descriptor - prefer the resolved target
     * constructor's own declared parameter types (semantic.c's
     * AST_EXPLICIT_CTOR_CALL case stores this on expr->sem_symbol) over
     * inferring one from the argument expressions' own types. A
     * superclass constructor parameter typed as a class-level type
     * variable (e.g. "T" in "GenericBase<T>(int, T, T)") is erased to
     * Object in its real, compiled descriptor - but an argument's own
     * concrete type (e.g. an enum constant passed for that parameter)
     * is not, so building the descriptor from arguments alone produced
     * one that named the argument's concrete type instead, which didn't
     * match the constructor actually compiled for the superclass
     * (NoSuchMethodError at the first call, since the verifier doesn't
     * check that a referenced method exists). Falls back to the old,
     * argument-inferred descriptor only if semantic analysis didn't
     * resolve a target constructor (should not normally happen). */
    char *descriptor = NULL;
    if (expr->sem_symbol && expr->sem_symbol->kind == SYM_CONSTRUCTOR &&
        !is_enum_this_call && !expr->sem_symbol->ast &&
        expr->sem_symbol->data.method_data.descriptor) {
        /* Classfile-loaded target constructor (no source AST - see
         * create_type_stub()'s own "the symbol is just a type stub"
         * comment for this convention) - use its own exact, already-
         * erased descriptor directly rather than reconstructing one
         * param-by-param below. A constructor parameter typed as a
         * BOUNDED class-level type variable (e.g. "M" in
         * "ForwardingJavaFileManager<M extends JavaFileManager>(M
         * fileManager)") erases to its bound (JavaFileManager), not
         * Object - but the type_t for a classfile-loaded parameter
         * symbol doesn't reliably carry that bound (reading it would
         * need the class's own generic Signature attribute, which
         * genesis's classfile loader doesn't parse for method
         * parameters), so reconstructing the descriptor from param->type
         * one type_to_descriptor() call at a time silently fell back to
         * Object for exactly this case - a "(Ljava/lang/Object;)V"
         * invokespecial where the real, compiled constructor is
         * "(Ljavax/tools/JavaFileManager;)V": NoSuchMethodError at
         * runtime, despite genesis itself compiling without complaint.
         * The classfile's own descriptor has no such gap - javac already
         * baked the correct erasure into it when IT compiled the
         * superclass. Confirmed against gumdrop's own
         * InMemoryJavaCompiler$InMemoryFileManager, whose
         * "super(fileManager)" (extending javax.tools.
         * ForwardingJavaFileManager<StandardJavaFileManager>) depends on
         * this exact resolution. */
        descriptor = strdup(expr->sem_symbol->data.method_data.descriptor);
    } else if (expr->sem_symbol && expr->sem_symbol->kind == SYM_CONSTRUCTOR) {
        /* Build "(<param descriptors>)V" directly from the resolved
         * constructor's own parameters - not via method_to_descriptor(),
         * which also appends method->type's own descriptor as the return
         * type. A constructor symbol's ->type isn't reliably void (some
         * other code sets it to the enclosing class's own type for
         * unrelated purposes, e.g. resolving a "new Foo(...)" expression's
         * overall type), so that would append a multi-character class
         * descriptor - and the single-byte "overwrite the last char with
         * V" patch below only works for a single-character return type,
         * silently corrupting the descriptor otherwise (e.g.
         * "(I)LParamParent;" -> the broken "(I)LParamParentV"). */
        string_t *desc = string_new("(");
        if (is_enum_this_call) {
            /* The resolved constructor's own declared parameter list
             * (below) only ever holds the user-written parameters - the
             * compiler-added (String, int) prefix (see the comment at
             * this function's own aload_1/iload_2 forwarding above) is
             * never part of it and must be added here too, to match the
             * real compiled descriptor genesis's enum-constant-
             * instantiation codegen already builds for this exact
             * target constructor. */
            string_append(desc, "Ljava/lang/String;I");
        }
        for (slist_t *p = expr->sem_symbol->data.method_data.parameters; p; p = p->next) {
            symbol_t *param = (symbol_t *)p->data;
            if (param && param->type) {
                char *param_desc = type_to_descriptor(param->type);
                string_append(desc, param_desc);
                free(param_desc);
            }
        }
        string_append(desc, ")V");
        descriptor = string_free(desc, false);
    } else {
        descriptor = build_method_descriptor(expr->data.node.children, NULL);
        /* Ensure descriptor has void return (only safe here since this
         * path's descriptor is inferred purely from argument expressions,
         * whose own inferred "return type" placeholder is always a single
         * character). */
        size_t len = strlen(descriptor);
        if (len > 0 && descriptor[len - 1] != 'V') {
            descriptor[len - 1] = 'V';
        }
    }
    
    if (forward_outer && !super_outer_in_descriptor && descriptor && descriptor[0] == '(') {
        size_t dl = strlen(descriptor) + strlen(mg->class_gen->outer_class_internal) + 4;
        char *with_outer = malloc(dl);
        snprintf(with_outer, dl, "(L%s;%s", mg->class_gen->outer_class_internal, descriptor + 1);
        free(descriptor);
        descriptor = with_outer;
    }

    /* Emit: invokespecial <init> */
    uint16_t init_ref = cp_add_methodref(cp, target_class, "<init>", descriptor);
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    
    free(descriptor);
    
    /* invokespecial consumes object ref and args, leaves nothing */
    mg_pop_typed(mg, arg_count + 1);
    
    /* Mark 'this' as initialized after explicit super()/this() (JEP 513) */
    if (mg->stackmap && mg->class_gen && mg->class_gen->internal_name) {
        stackmap_init_object(mg->stackmap, 0, cp, mg->class_gen->internal_name);
    }
    
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
    uint16_t new_offset = (uint16_t)mg->code->length;  /* Save offset before emitting */
    bc_emit(mg->code, OP_NEW);
    bc_emit_u2(mg->code, class_ref);
    mg_push_uninitialized(mg, new_offset);  /* Uninitialized object reference */
    
    /* Emit: dup (we need two refs: one for invokespecial, one to keep) */
    bc_emit(mg->code, OP_DUP);
    mg_push_uninitialized(mg, new_offset);  /* Duplicated uninitialized reference */
    
    /* Check if the target class is an inner class (non-static nested class) */
    /* We need to pass the outer instance as first constructor argument */
    bool is_inner_class = false;
    bool is_local_class = false;
    char *outer_internal = NULL;
    slist_t *captured_vars = NULL;  /* Captured variables for local/anonymous classes */
    
    /* Check for explicit outer instance (qualified new: outer.new Inner()) */
    ast_node_t *explicit_outer = (ast_node_t *)expr->data.node.extra;
    
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
        
        /* Check if the enclosing method/context is static */
        /* For anonymous classes in static contexts, they don't have an enclosing instance */
        bool enclosing_is_static = false;
        
        /* If the target class is marked static, it's a static nested class */
        if (target_sym->modifiers & MOD_STATIC) {
            enclosing_is_static = true;
        }
        
        /* For local/anonymous classes, check the enclosing method */
        if (is_local_class || target_sym->data.class_data.is_anonymous_class) {
            if (target_sym->data.class_data.enclosing_method &&
                (target_sym->data.class_data.enclosing_method->modifiers & MOD_STATIC)) {
                /* Defined in static method */
                enclosing_is_static = true;
            }
            /* NOTE: deliberately no "no enclosing_method -> static" fallback
             * here. An anonymous/local class created directly inside a
             * field initializer or instance initializer block also has no
             * enclosing_method (it's not inside any method at all), but
             * that's NOT necessarily a static context - an *instance*
             * field initializer's anonymous class still needs to capture
             * the enclosing instance (e.g. "private final X x = new
             * Y(){...};" on a non-static field runs during <init>, not
             * <clinit>). target_sym->modifiers already correctly reflects
             * this either way (computed once in semantic.c from
             * sem->in_static_field_init, which IS static-field/static-
             * init-block-aware), so trust that instead of re-deriving a
             * wrong answer from "no enclosing method" alone. Without this
             * fix, such an anonymous class's own constructor was correctly
             * generated to accept the enclosing instance (this$0), but the
             * call site here skipped pushing it and used the wrong (no-
             * arg) invokespecial descriptor - NoSuchMethodError at runtime. */
            /* Also check if we're currently in a static method (e.g., lambda method) */
            if (mg->is_static) {
                enclosing_is_static = true;
            }
        }
        /* For member inner classes, check if WE are in a static context */
        else if (mg->is_static && !explicit_outer) {
            /* Can't create non-static inner class from static context without explicit outer */
            /* This should be a semantic error, but handle it gracefully */
            /* Note: If there IS an explicit outer (outer.new Inner()), then we CAN create
             * the inner class even from a static context, so don't set enclosing_is_static. */
            enclosing_is_static = true;
        }
        
        if (target_sym->data.class_data.enclosing_class &&
            !(target_sym->modifiers & MOD_STATIC) &&
            (!enclosing_is_static || explicit_outer)) {
            /* This is an inner/anonymous class - need to pass enclosing instance.
             * Even if we're in a static context, if there's an explicit outer
             * (outer.new Inner()), we can still create the inner class. */
            is_inner_class = true;
            symbol_t *enclosing = target_sym->data.class_data.enclosing_class;
            outer_internal = class_to_internal_name(enclosing->qualified_name);
            
            /* Check for explicit outer instance from qualified new: outer.new Inner() */
            if (explicit_outer) {
                /* Generate code to load the explicit outer instance */
                if (!codegen_expr(mg, explicit_outer, cp)) {
                    free(internal_name);
                    free(outer_internal);
                    return false;
                }
            }
            /* Find the correct enclosing instance by traversing the this$0 chain.
             * We need to find where 'enclosing' is in the chain from our class. */
            else if (mg->class_gen && mg->class_gen->class_sym) {
                /* Start with 'this' */
                bc_emit(mg->code, OP_ALOAD_0);
                /* Push with actual class type for stackmap */
                if (mg->class_gen->internal_name) {
                    mg_push_object(mg, mg->class_gen->internal_name);
                } else {
                    mg_push_null(mg);
                }
                
                /* Traverse the enclosing class chain to find how many levels deep */
                symbol_t *current = mg->class_gen->class_sym;
                bool first_iteration = true;
                
                while (current && current != enclosing) {
                    symbol_t *cur_enclosing = current->data.class_data.enclosing_class;
                    if (!cur_enclosing) {
                        break;
                    }
                    
                    /* Get this$0 from current class to get to cur_enclosing */
                    char *cur_internal = class_to_internal_name(current->qualified_name);
                    char *enc_internal = class_to_internal_name(cur_enclosing->qualified_name);
                    size_t desc_len = strlen(enc_internal) + 3;
                    char *desc = malloc(desc_len);
                    snprintf(desc, desc_len, "L%s;", enc_internal);
                    
                    /* Use the already-computed this$0 ref for the first iteration if available */
                    uint16_t this0_ref;
                    if (first_iteration && mg->class_gen->this_dollar_zero_ref) {
                        this0_ref = mg->class_gen->this_dollar_zero_ref;
                    } else {
                        this0_ref = cp_add_fieldref(cp, cur_internal, "this$0", desc);
                    }
                    
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, this0_ref);
                    /* Stack unchanged: popped old, pushed enclosing */
                    
                    free(cur_internal);
                    free(enc_internal);
                    free(desc);
                    
                    /* If we found the target enclosing class, we're done */
                    if (cur_enclosing == enclosing) {
                        break;
                    }
                    
                    current = cur_enclosing;
                    first_iteration = false;
                }
            }
        }
    }
    
    /* For local classes, push captured variable values */
    if (is_local_class && captured_vars) {
        for (slist_t *cap = captured_vars; cap; cap = cap->next) {
            symbol_t *var_sym = (symbol_t *)cap->data;
            if (!var_sym || !var_sym->name) {
                continue;
            }
            
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
                } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
                    /* Reference types including type variables (erase to Object) */
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_null(mg);  /* Object reference */
                } else {
                    /* Integer types */
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push_int(mg);
                }
            } else if (mg->class_gen) {
                /* Not a plain local of the *current* method - this happens
                 * for a variable captured by a class nested inside another
                 * local/anonymous class, where the variable's true home
                 * scope is further out than the immediately enclosing
                 * method (e.g. a parameter of the class that the current
                 * one is itself defined inside, relayed here via semantic
                 * analysis's propagate_capture_to_enclosing_classes()).
                 * The current class captured it too for exactly this
                 * reason, so it's available as this.val$<name> - load it
                 * from there instead. */
                char field_name[300];
                snprintf(field_name, sizeof(field_name), "val$%s", var_sym->name);
                field_gen_t *cap_field = hashtable_lookup(mg->class_gen->field_map, field_name);
                if (cap_field) {
                    bc_emit(mg->code, OP_ALOAD_0);
                    /* Track the real pushed type (the current class), not
                     * null - mg_push_null() records a VT_NULL stackmap
                     * entry for what is actually a known, non-null `this`
                     * reference, mistyping this stack slot for the rest
                     * of this expression's codegen (see the identical fix
                     * a few hundred lines down, at the instance-field-
                     * assignment site, for the full explanation and the
                     * bug this caused). */
                    if (mg->class_gen && mg->class_gen->internal_name) {
                        mg_push_object(mg, mg->class_gen->internal_name);
                    } else {
                        mg_push_null(mg);
                    }
                    uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                         cap_field->name, cap_field->descriptor);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, fieldref);
                    mg_pop_typed(mg, 1);  /* getfield consumed the aload_0 ref */
                    switch (cap_field->descriptor[0]) {
                        case 'J': mg_push_long(mg); break;
                        case 'D': mg_push_double(mg); break;
                        case 'F': mg_push_float(mg); break;
                        case 'L':
                        case '[': mg_push_object_from_descriptor(mg, cap_field->descriptor); break;
                        default:  mg_push_int(mg); break;
                    }
                }
            }
        }
    }
    
    /* Check if constructor is varargs */
    symbol_t *ctor_sym_for_varargs = expr->sem_symbol;
    bool is_varargs_ctor = (ctor_sym_for_varargs &&
                           ctor_sym_for_varargs->kind == SYM_CONSTRUCTOR &&
                           (ctor_sym_for_varargs->modifiers & MOD_VARARGS));
    
    /* Get varargs info */
    int fixed_param_count = 0;
    symbol_t *varargs_param = NULL;
    if (is_varargs_ctor && ctor_sym_for_varargs->data.method_data.parameters) {
        /* Count parameters and find last (varargs) one */
        for (slist_t *p = ctor_sym_for_varargs->data.method_data.parameters; p; p = p->next) {
            fixed_param_count++;
            varargs_param = (symbol_t *)p->data;
        }
        fixed_param_count--;  /* Last param is varargs */
    }
    
    /* Declared parameters, when the descriptor is built from them (see below) */
    slist_t *ctor_param_node = NULL;
    if (expr->sem_symbol && expr->sem_symbol->kind == SYM_CONSTRUCTOR) {
        ctor_param_node = expr->sem_symbol->data.method_data.parameters;
    }
    
    /* Generate constructor arguments */
    /* Skip AST_BLOCK if this is an anonymous class (the block is the class body, not an argument) */
    int arg_index = 0;
    bool varargs_array_pushed = false;
    for (slist_t *node = children->next; node; node = node->next, arg_index++) {
        ast_node_t *arg = (ast_node_t *)node->data;
        
        /* Skip anonymous class body block */
        if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) {
            break;
        }
        
        /* Check if we've hit the varargs position */
        if (is_varargs_ctor && arg_index == fixed_param_count && varargs_param) {
            varargs_array_pushed = true;
            if (!codegen_varargs_tail(mg, cp, varargs_param, node, is_anonymous_class)) {
                free(internal_name);
                if (outer_internal) {
                    free(outer_internal);
                }
                return false;
            }
            break;  /* Done with arguments */
        }
        
        if (!codegen_expr(mg, arg, cp)) {
            free(internal_name);
            if (outer_internal) {
                free(outer_internal);
            }
            return false;
        }
        
        /* The descriptor below is built from the constructor's declared
         * parameter types, so convert the argument to match (boxing for
         * type-variable parameters, unboxing, widening). */
        if (ctor_param_node) {
            coerce_arg_to_param(mg, cp, arg, (symbol_t *)ctor_param_node->data, false);
            ctor_param_node = ctor_param_node->next;
        }
    }

    /* Handle case where a varargs constructor is called with no varargs
     * arguments at all - the loop above never took its "hit the varargs
     * position" branch (varargs_array_pushed stays false), since there
     * was nothing left to iterate once the fixed parameters were
     * consumed. The descriptor built below always declares the trailing
     * array parameter, so an empty array must still be pushed for it
     * here, or the actual argument count on the stack falls one short of
     * what invokespecial's descriptor requires. Mirrors the identical
     * "no varargs arguments" case already handled for a plain (non-
     * constructor) varargs method call a bit earlier in this file.
     *
     * Checking arg_index == fixed_param_count here instead, rather than
     * this dedicated flag, looked equivalent but wasn't: the loop above
     * also leaves arg_index sitting at exactly fixed_param_count when it
     * DID take the varargs branch and then broke out of the loop (break
     * happens before the for-loop's own arg_index++), so that condition
     * can't tell "no varargs args were passed" apart from "the varargs
     * array was already pushed, for a call with EXACTLY one varargs
     * argument" - wrongly pushing a second, spurious empty array in the
     * latter case. */
    if (is_varargs_ctor && varargs_param && !varargs_array_pushed) {
        if (!codegen_varargs_tail(mg, cp, varargs_param, NULL, false)) {
            free(internal_name);
            if (outer_internal) {
                free(outer_internal);
            }
            return false;
        }
    }

    /* Build constructor descriptor */
    string_t *desc = string_new("(");
    
    /* For inner classes, add outer instance as first parameter */
    if (is_inner_class && outer_internal) {
        string_append(desc, "L");
        string_append(desc, outer_internal);
        string_append(desc, ";");
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
    }
    
    /* Add explicit argument types - use constructor parameter types if available,
     * otherwise fall back to inferring from argument expressions */
    symbol_t *ctor_sym = expr->sem_symbol;
    if (ctor_sym && ctor_sym->kind == SYM_CONSTRUCTOR &&
        ctor_sym->data.method_data.descriptor && !is_local_class) {
        /* Constructor loaded from a class file: its own descriptor is exact,
         * whereas one rebuilt from generic parameter types would erase a type
         * variable to Object instead of to its bound. A local class is
         * excluded as its descriptor is assembled with synthetic params. A
         * member INNER class's stored descriptor starts with the enclosing
         * instance, already appended above, so that first parameter is
         * skipped. */
        char *stored = strdup(ctor_sym->data.method_data.descriptor);
        char *close = stored ? strchr(stored, ')') : NULL;
        if (close) {
            *close = '\0';
            char *params = stored + 1;  /* skip the leading '(' */
            if (is_inner_class && *params == 'L') {
                char *semi = strchr(params, ';');
                params = semi ? semi + 1 : params;
            }
            string_append(desc, params);
        }
        free(stored);
    } else if (ctor_sym && ctor_sym->kind == SYM_CONSTRUCTOR && ctor_sym->data.method_data.parameters) {
        /* Use the constructor's declared parameter types */
        for (slist_t *param_node = ctor_sym->data.method_data.parameters; param_node; param_node = param_node->next) {
            symbol_t *param_sym = (symbol_t *)param_node->data;
            if (param_sym && param_sym->type) {
                char *param_desc = type_to_descriptor(param_sym->type);
                string_append(desc, param_desc);
                free(param_desc);
            }
        }
    } else {
        /* Fall back to inferring from arguments */
        for (slist_t *node = children->next; node; node = node->next) {
            ast_node_t *arg = (ast_node_t *)node->data;
            
            /* Skip anonymous class body block */
            if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) {
                break;
            }
            
            /* An anonymous class's own constructor takes each argument at
             * its static type (codegen_anonymous_class() builds it the same
             * way) - infer_arg_descriptor() alone says Object for "this". */
            type_t *anon_arg_type = NULL;
            if (is_anonymous_class && arg->type == AST_THIS_EXPR && mg->class_gen &&
                mg->class_gen->internal_name) {
                /* Unqualified "this": the class being generated. Evaluating it
                 * through semantic state here would use whatever class the
                 * analysis left current. */
                string_append(desc, "L");
                string_append(desc, mg->class_gen->internal_name);
                string_append(desc, ";");
                continue;
            }
            if (is_anonymous_class) {
                anon_arg_type = arg->sem_type;
                if (!anon_arg_type && mg->class_gen && mg->class_gen->sem) {
                    anon_arg_type = get_expression_type(mg->class_gen->sem, arg);
                }
            }
            if (anon_arg_type) {
                char *anon_arg_desc = type_to_descriptor(anon_arg_type);
                string_append(desc, anon_arg_desc);
                free(anon_arg_desc);
            } else {
                const char *arg_desc = infer_arg_descriptor(arg);
                string_append(desc, arg_desc);
            }
        }
    }
    
    string_append(desc, ")V");
    char *descriptor = string_free(desc, false);
    
    if (outer_internal) {
        free(outer_internal);
    }
    
    /* Emit: invokespecial <init> */
    uint16_t init_ref = cp_add_methodref(cp, internal_name, "<init>", descriptor);
    bc_emit(mg->code, OP_INVOKESPECIAL);
    bc_emit_u2(mg->code, init_ref);
    
    free(descriptor);
    
    /* invokespecial consumes object ref and args, leaves nothing */
    /* But we have a dup'd reference still on stack */
    /* Calculate stack slots consumed (long/double take 2 slots) */
    int arg_slots = 0;
    if (is_inner_class) {
        arg_slots++;
    }  /* Outer instance */
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
    /* Arguments were coerced to the declared parameter types, so those decide
     * the slot count (a boxed long is one slot, an int widened to long two).
     *
     * For a varargs constructor, every raw argument from fixed_param_count
     * onward was packed into a SINGLE array by the argument-generation
     * loop above (one array reference pushed, regardless of how many
     * source arguments fed it, including zero) - counting each of those
     * raw arguments as its own slot here, as a plain 1:1 zip against
     * children would, overcounts and pops one slot too many per extra
     * vararg, corrupting mg->stack_depth for the rest of the method (the
     * same "popped more than was actually pushed" underflow already seen
     * elsewhere this session for other call shapes). Only the fixed
     * parameters get zipped 1:1 against their own arguments; the varargs
     * parameter itself always contributes exactly one slot. */
    if (is_varargs_ctor && varargs_param) {
        slist_t *slot_param = ctor_sym_for_varargs->data.method_data.parameters;
        slist_t *node = children->next;
        for (int i = 0; i < fixed_param_count && node; i++, node = node->next) {
            symbol_t *param = slot_param ? (symbol_t *)slot_param->data : NULL;
            type_kind_t kind = param && param->type ? param->type->kind
                                                      : get_expr_type_kind(mg, (ast_node_t *)node->data);
            arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
            if (slot_param) {
                slot_param = slot_param->next;
            }
        }
        arg_slots += 1;  /* The varargs array itself - always 1 slot. */
    } else {
        slist_t *slot_param = NULL;
        if (expr->sem_symbol && expr->sem_symbol->kind == SYM_CONSTRUCTOR) {
            slot_param = expr->sem_symbol->data.method_data.parameters;
        }
        for (slist_t *node = children->next; node; node = node->next) {
            ast_node_t *arg = (ast_node_t *)node->data;
            if (is_anonymous_class && arg->type == AST_BLOCK && !node->next) {
                break;
            }
            type_kind_t kind = get_expr_type_kind(mg, arg);
            if (slot_param) {
                symbol_t *param = (symbol_t *)slot_param->data;
                if (param && param->type) {
                    kind = param->type->kind;
                }
                slot_param = slot_param->next;
            }
            arg_slots += (kind == TYPE_LONG || kind == TYPE_DOUBLE) ? 2 : 1;
        }
    }
    mg_pop_typed(mg, arg_slots + 1);  /* +1 for object reference */
    
    /* Mark the uninitialized object as initialized in stackmap.
     * The remaining stack entry is the dup'd reference. */
    if (mg->stackmap) {
        stackmap_init_object(mg->stackmap, new_offset, mg->cp, internal_name);
    }
    
    free(internal_name);
    
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
        if (elem_kind == TYPE_BOOLEAN) {
            elem_desc = strdup("Z");
        } else if (elem_kind == TYPE_BYTE) {
            elem_desc = strdup("B");
        } else if (elem_kind == TYPE_CHAR) {
            elem_desc = strdup("C");
        } else if (elem_kind == TYPE_SHORT) {
            elem_desc = strdup("S");
        } else if (elem_kind == TYPE_INT) {
            elem_desc = strdup("I");
        } else if (elem_kind == TYPE_LONG) {
            elem_desc = strdup("J");
        } else if (elem_kind == TYPE_FLOAT) {
            elem_desc = strdup("F");
        } else if (elem_kind == TYPE_DOUBLE) {
            elem_desc = strdup("D");
        } else if (elem_kind == TYPE_CLASS && elem_type && elem_type->data.class_type.name) {
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
    
    /* Build array type descriptor for stackmap tracking */
    char *array_type_desc = NULL;
    if (arr_dims > 1 || elem_kind == TYPE_CLASS) {
        /* Reference array - build [L...; or [[[... descriptor */
        if (elem_kind == TYPE_CLASS && elem_type && elem_type->data.class_type.name) {
            char *internal = class_to_internal_name(elem_type->data.class_type.name);
            size_t len = strlen(internal) + arr_dims + 3;  /* [[[L + name + ; + null */
            array_type_desc = malloc(len);
            memset(array_type_desc, '[', arr_dims);
            snprintf(array_type_desc + arr_dims, len - arr_dims, "L%s;", internal);
            free(internal);
        } else {
            size_t len = arr_dims + 20;
            array_type_desc = malloc(len);
            memset(array_type_desc, '[', arr_dims);
            strcpy(array_type_desc + arr_dims, "Ljava/lang/Object;");
        }
    } else {
        /* Primitive array - build [I, [J, etc. */
        char prim_char = 'I';
        switch (elem_kind) {
            case TYPE_BOOLEAN: prim_char = 'Z'; break;
            case TYPE_BYTE:    prim_char = 'B'; break;
            case TYPE_CHAR:    prim_char = 'C'; break;
            case TYPE_SHORT:   prim_char = 'S'; break;
            case TYPE_INT:     prim_char = 'I'; break;
            case TYPE_LONG:    prim_char = 'J'; break;
            case TYPE_FLOAT:   prim_char = 'F'; break;
            case TYPE_DOUBLE:  prim_char = 'D'; break;
            default: prim_char = 'I';
        }
        array_type_desc = malloc(3);
        array_type_desc[0] = '[';
        array_type_desc[1] = prim_char;
        array_type_desc[2] = '\0';
    }

    /* newarray/anewarray/multianewarray replace the size int(s) with the new
     * array reference - a real TYPE change even though the word count is
     * unchanged for the single-dimension case (the "no net change" comment
     * above refers only to word count). mg_push_int(mg) above tracked the
     * size as an int on mg->stackmap; correct that to the actual array
     * reference type now, or the array's own creation leaves a stale
     * "Integer" entry buried under every subsequent per-element dup/index/
     * value push for the rest of this array literal. That stale entry is
     * invisible for a straight-line element store (no stackmap frame is
     * recorded mid-store to observe it), but corrupts any StackMapTable
     * frame recorded while it's still buried on the stack - e.g. a later
     * element whose own value is a conditional/ternary expression, which
     * requires recording a frame at its branch target and bakes the wrong
     * type in (VerifyError: "Inconsistent stackmap frames"). */
    mg_pop_typed(mg, 1);
    mg_push_object(mg, array_type_desc);

    /* Populate the array with initializer values */
    int index = 0;
    for (slist_t *node = expr->data.node.children; node; node = node->next) {
        ast_node_t *elem_expr = (ast_node_t *)node->data;
        
        /* Duplicate array reference */
        bc_emit(mg->code, OP_DUP);
        mg_push_object(mg, array_type_desc);  /* Duplicated array reference */

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
            
            /* Widen int literals to long/float/double if needed.
             * Integer literals are compiled as int, but array may need wider type. */
            type_kind_t expr_kind = get_expr_type_kind(mg, elem_expr);
            if (expr_kind == TYPE_INT || expr_kind == TYPE_BYTE || 
                expr_kind == TYPE_SHORT || expr_kind == TYPE_CHAR) {
                if (elem_kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_I2L);
                    mg_pop_typed(mg, 1);  /* Pop int */
                    mg_push_long(mg);     /* Push long */
                } else if (elem_kind == TYPE_FLOAT) {
                    bc_emit(mg->code, OP_I2F);
                    /* Stack size unchanged: int -> float, both 1 slot */
                } else if (elem_kind == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_I2D);
                    mg_pop_typed(mg, 1);  /* Pop int */
                    mg_push_double(mg);   /* Push double (2 slots) */
                } else if (elem_kind == TYPE_CLASS &&
                           !(elem_expr->sem_type && elem_expr->sem_type->kind == TYPE_CLASS)) {
                    /* int/short/byte/char element of a reference-typed array
                     * ("new Object[] { delay, attempt }"): box it, as the
                     * long/boolean/... case below already does - see there
                     * for why an already-boxed wrapper is excluded. */
                    emit_boxing(mg, cp, expr_kind);
                }
            } else if (expr_kind == TYPE_LONG && elem_kind == TYPE_DOUBLE) {
                bc_emit(mg->code, OP_L2D);
                /* Stack size unchanged: long -> double, both 2 slots */
            } else if (expr_kind == TYPE_LONG && elem_kind == TYPE_FLOAT) {
                bc_emit(mg->code, OP_L2F);
                mg_pop_typed(mg, 1);  /* Pop long's second slot */
            } else if (expr_kind == TYPE_FLOAT && elem_kind == TYPE_DOUBLE) {
                bc_emit(mg->code, OP_F2D);
                mg_push(mg, 1);  /* float -> double gains a slot */
            } else if (elem_kind == TYPE_CLASS && expr_kind >= TYPE_BOOLEAN && expr_kind <= TYPE_DOUBLE &&
                       !(elem_expr->sem_type && elem_expr->sem_type->kind == TYPE_CLASS)) {
                /* Array element type is a reference (e.g. "Object[] pair =
                 * { someLongExpr, someEnum };") but this particular
                 * element's own expression is primitive - box it before
                 * the AASTORE below, exactly like a plain assignment or
                 * method-call argument already would. Without this, a raw
                 * primitive (e.g. a `long`, 2 stack words) got stored
                 * straight into a reference-typed array slot: VerifyError
                 * "Bad type on operand stack ... not assignable to
                 * 'java/lang/Object'" (or java.lang.Long/etc., depending
                 * on the array's own element type). Confirmed against
                 * gumdrop's own LossDetector.ptoTimeAndSpace()'s "new
                 * Object[] { nowMillis + duration, space }".
                 *
                 * The extra `elem_expr->sem_type->kind == TYPE_CLASS`
                 * exclusion matters: get_expr_type_kind() deliberately
                 * reports a WRAPPER-typed expression's own UNDERLYING
                 * PRIMITIVE kind "for arithmetic" (see its own comment) -
                 * e.g. "Long.valueOf(7)" (genuinely sem_type TYPE_CLASS
                 * "Long", already a reference on the stack) comes back as
                 * expr_kind == TYPE_LONG from that call. Without this
                 * exclusion, an ALREADY-boxed wrapper value got re-boxed
                 * here - emit_boxing(TYPE_LONG) expects a genuine 2-word
                 * primitive long on the stack, not the 1-word reference
                 * actually there, corrupting the stack outright.
                 * Confirmed against gumdrop-shaped
                 * "Object[] ids = { Long.valueOf(7), ... }" (this
                 * session's own CastToArrayTypeVerifyTest). */
                emit_boxing(mg, cp, expr_kind);
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
        /* elem_kind is the array's ultimate LEAF scalar type, not what
         * THIS dimension's own store opcode operates on (see the
         * dims>1/store_elem_kind comment above, in the sibling ASSIGNMENT
         * codegen, for the identical distinction). At any outer dimension
         * (arr_dims > 1) the value just stored is a SUB-ARRAY REFERENCE
         * via AASTORE - one word, like any other reference - regardless
         * of whether the leaf type is long/double; the extra word only
         * exists at the leaf dimension's own LASTORE/DASTORE. Popping it
         * here too undercounted mg->stack_depth by one word for every
         * outer-dimension element of a long[][]/double[][] (or deeper)
         * literal, which - for such a literal used directly as a method
         * argument - eventually went negative in AST_EXPR_STMT's unsigned
         * "slots to pop" computation, wrapping to 65535 and emitting a
         * spurious POP2 after the call: VerifyError "Operand stack
         * overflow"/"underflow" despite the actual array-construction
         * bytecode being correct throughout. Confirmed against gumdrop's
         * own LossDetectorTest, whose "new long[][] { { 0, 0 } }" method
         * argument hits exactly this shape. */
        if (arr_dims == 1 && (elem_kind == TYPE_LONG || elem_kind == TYPE_DOUBLE)) {
            mg_pop_typed(mg, 1);  /* Wide types, leaf dimension only */
        }

        index++;
    }

    free(array_type_desc);

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
        /* codegen_array_init() requires array_init->sem_type to already
         * be set (semantic.c's own AST_NEW_ARRAY handling in
         * get_expression_type() sets it as a side effect) - reliably
         * true when this whole "new Type[]{...}" expression was itself
         * visited by that function, but NOT when it's nested as one
         * element of an OUTER array initializer (e.g. `Object[] ids =
         * {a, b, new byte[]{9, 9}};` - the outer initializer's own
         * element-binding pass doesn't recurse through the exact same
         * AST_NEW_ARRAY-visiting path for each element). Force it here
         * if missing, rather than depending on whichever caller already
         * happened to trigger it. */
        if ((!array_init->sem_type || array_init->sem_type->kind != TYPE_ARRAY) &&
            mg->class_gen && mg->class_gen->sem) {
            get_expression_type(mg->class_gen->sem, expr);
        }
        /* Delegate to codegen_array_init(), which already handles this
         * correctly and generally: it reads array_init->sem_type (a
         * proper type_t with the real dimensions/element_type, reliably
         * populated by semantic.c's AST_NEW_ARRAY handling - including
         * for a multi-dimensional literal like `new int[][] { {2, 1} }`,
         * whose element-type AST node the parser leaves as a nested
         * AST_ARRAY_TYPE rather than the primitive it actually is) and
         * recurses correctly for each nested dimension. The hand-rolled
         * version this replaced derived everything from that same
         * (dimension-losing) type_node instead, so a multi-dimensional
         * literal fell into its reference-type/ANEWARRAY branch with no
         * real class name available, silently defaulting to
         * "java/lang/Object" (i.e. producing an Object[] instead of,
         * say, int[][]) - rejected by the verifier the moment that value
         * was used somewhere requiring the real array type. */
        return codegen_array_init(mg, array_init, cp);
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
    
    /* total_dims (parser.c's own "new Type[n][]..." dimension count,
     * stashed in the AST node's flags field) can exceed dim_count (the
     * number of dimensions that actually got a size EXPRESSION) - e.g.
     * "new byte[n][]" has dim_count=1 (only the outer size is given) but
     * total_dims=2 (it's still creating a 2-D array, just leaving the
     * inner arrays unallocated). Falling into the dim_count==1 branch
     * below unconditionally, as this used to, treated it as a TRUE
     * single-dimensional array of the LEAF element type - NEWARRAY byte,
     * producing a bare "[B" instead of "[[B" - which then broke every
     * later use of the value as the 2-D array it was declared as (e.g.
     * an AASTORE storing a byte[] into "values[i]" needs an arrayref
     * that's ACTUALLY "[[B", not "[B"): VerifyError the moment a store
     * or load exposed the mismatch. Confirmed against gumdrop's own
     * MessageIndexEntry, whose "byte[][] values = new byte[DESCRIPTOR_
     * COUNT][];" is exactly this shape. */
    int total_dims = (int)expr->data.node.flags;
    if (total_dims < dim_count) {
        total_dims = dim_count;
    }

    if (dim_count == 1 && total_dims == 1) {
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
    } else if (dim_count == 1 && total_dims > 1) {
        /* "new Type[n][]...[]" (dim_count-many trailing dims left empty):
         * ANEWARRAY, whose class operand names the COMPONENT type - an
         * array with (total_dims - dim_count) fewer dimensions than the
         * whole declared type, e.g. "[B" (byte[]) for "new byte[n][]"'s
         * component. */
        char *elem_desc = ast_type_to_descriptor(type_node);
        int component_extra_dims = total_dims - dim_count;
        size_t desc_len = strlen(elem_desc) + (size_t)component_extra_dims + 1;
        char *component_desc = malloc(desc_len);
        memset(component_desc, '[', (size_t)component_extra_dims);
        strcpy(component_desc + component_extra_dims, elem_desc);
        free(elem_desc);

        uint16_t class_ref = cp_add_class(cp, component_desc);
        free(component_desc);

        bc_emit(mg->code, OP_ANEWARRAY);
        bc_emit_u2(mg->code, class_ref);
        /* Stack: size -> arrayref (no net change) */
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
 * Compound assignment (E1 op= E2, JLS 15.26.2): given the LHS's current
 * value already on the stack (of kind/class lhs_kind/lhs_class), generate
 * E2, apply the operator with the correct numeric promotion, and narrow the
 * result back to the LHS's own type -- the implicit cast that only compound
 * assignment gets, and the reason "byte b; b += 1;" or "long a; a += 2;"
 * needs different bytecode from a plain binary "+" (which requires an
 * explicit cast to assign back to the narrower/original type).
 *
 * A boxed numeric LHS (Integer i; i += 1;) is unboxed first and reboxed at
 * the end, exactly as if the wrapper's own primitive were being narrowed.
 * A String LHS with += is concatenation (also JLS 15.26.2's special case),
 * built with a StringBuilder; the LHS value, already on the stack, is
 * appended after being moved below a newly created one with OP_SWAP (both
 * are always 1 slot).
 *
 * Leaves the (narrowed/reboxed, or concatenated) result on the stack in
 * place of the two operands, and returns false only on a genuine codegen
 * failure while generating E2.
 */
static bool codegen_compound_rhs(method_gen_t *mg, const_pool_t *cp, ast_node_t *value,
                                 type_kind_t lhs_kind, const char *lhs_class, token_type_t op)
{
    bool is_string_lhs = (lhs_kind == TYPE_CLASS && lhs_class &&
        (strcmp(lhs_class, "java.lang.String") == 0 ||
         strcmp(lhs_class, "java/lang/String") == 0 ||
         strcmp(lhs_class, "String") == 0));

    if (is_string_lhs && op == TOK_PLUS_ASSIGN) {
        /* Build the StringBuilder, then SWAP it below the LHS value that is
         * already on the stack, rather than the other way around as a fresh
         * concatenation would. */
        uint16_t sb_class = cp_add_class(cp, "java/lang/StringBuilder");
        uint16_t new_offset = (uint16_t)mg->code->length;
        bc_emit(mg->code, OP_NEW);
        bc_emit_u2(mg->code, sb_class);
        mg_push_uninitialized(mg, new_offset);
        bc_emit(mg->code, OP_DUP);
        mg_push_uninitialized(mg, new_offset);
        uint16_t init_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "<init>", "()V");
        bc_emit(mg->code, OP_INVOKESPECIAL);
        bc_emit_u2(mg->code, init_ref);
        mg_pop_typed(mg, 1);
        if (mg->stackmap) {
            stackmap_init_object(mg->stackmap, new_offset, mg->cp, "java/lang/StringBuilder");
        }

        /* Stack: [lhs_string, sb] -> [sb, lhs_string] */
        bc_emit(mg->code, OP_SWAP);
        uint16_t append_str = cp_add_methodref(cp, "java/lang/StringBuilder", "append",
                                               "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, append_str);
        mg_pop_typed(mg, 1);  /* consumes sb + lhs_string, leaves sb */

        if (!codegen_expr(mg, value, cp)) {
            return false;
        }
        const char *append_desc = get_append_descriptor(mg, value);
        uint16_t append_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "append", append_desc);
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, append_ref);
        mg_pop_typed(mg, (strncmp(append_desc, "(J)", 3) == 0 ||
                          strncmp(append_desc, "(D)", 3) == 0) ? 2 : 1);

        uint16_t tostring_ref = cp_add_methodref(cp, "java/lang/StringBuilder", "toString",
                                                 "()Ljava/lang/String;");
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, tostring_ref);
        mg_pop_typed(mg, 1);
        mg_push_object(mg, "java/lang/String");
        return true;
    }

    /* A boxed numeric LHS: unbox now, rebox at the end. */
    bool lhs_was_wrapper = false;
    if (lhs_kind == TYPE_CLASS && lhs_class) {
        type_kind_t wrapper_prim = get_primitive_for_wrapper(lhs_class);
        if (wrapper_prim != TYPE_UNKNOWN) {
            char *internal = class_to_internal_name(lhs_class);
            emit_unboxing(mg, cp, wrapper_prim, internal);
            free(internal);
            lhs_kind = wrapper_prim;
            lhs_was_wrapper = true;
        }
    }

    bool lhs_boolean = (lhs_kind == TYPE_BOOLEAN);
    bool is_shift = (op == TOK_LSHIFT_ASSIGN || op == TOK_RSHIFT_ASSIGN || op == TOK_URSHIFT_ASSIGN);

    type_kind_t rhs_kind;
    const char *rhs_class;
    value_kind_and_class(mg, value, &rhs_kind, &rhs_class);
    if (rhs_kind == TYPE_CLASS && rhs_class) {
        type_kind_t unboxed = get_primitive_for_wrapper(rhs_class);
        if (unboxed != TYPE_UNKNOWN) {
            rhs_kind = unboxed;  /* for op_type purposes only; unboxed below */
        }
    }

    /* Binary numeric promotion (JLS 5.6.2) between the LHS's own type and
     * the RHS's -- same rule codegen_binary_expr uses for a plain operator
     * -- except a shift, whose right operand never joins the promotion, and
     * boolean &=/|=/^=, which stays boolean (IAND/IOR/IXOR either way). */
    type_kind_t op_type;
    if (lhs_boolean) {
        op_type = TYPE_BOOLEAN;
    } else if (is_shift) {
        op_type = (lhs_kind == TYPE_LONG) ? TYPE_LONG : TYPE_INT;
    } else if (lhs_kind == TYPE_DOUBLE || rhs_kind == TYPE_DOUBLE) {
        op_type = TYPE_DOUBLE;
    } else if (lhs_kind == TYPE_FLOAT || rhs_kind == TYPE_FLOAT) {
        op_type = TYPE_FLOAT;
    } else if (lhs_kind == TYPE_LONG || rhs_kind == TYPE_LONG) {
        op_type = TYPE_LONG;
    } else {
        op_type = TYPE_INT;  /* byte/short/char/int all promote to int */
    }

    if (!lhs_boolean && lhs_kind != op_type) {
        emit_widen_primitive(mg, lhs_kind, op_type);
    }

    if (!codegen_expr(mg, value, cp)) {
        return false;
    }

    /* Convert the RHS to match: for a shift count, unboxing only (it is
     * never widened to op_type, exactly as in codegen_binary_expr);
     * otherwise the same unbox-then-widen an assignment would do. */
    {
        type_kind_t raw_rhs_kind;
        const char *raw_rhs_class;
        value_kind_and_class(mg, value, &raw_rhs_kind, &raw_rhs_class);
        if (is_shift) {
            type_kind_t effective_rhs_kind = raw_rhs_kind;
            if (raw_rhs_kind == TYPE_CLASS && raw_rhs_class) {
                type_kind_t unboxed = get_primitive_for_wrapper(raw_rhs_class);
                if (unboxed != TYPE_UNKNOWN) {
                    char *internal = class_to_internal_name(raw_rhs_class);
                    emit_unboxing(mg, cp, unboxed, internal);
                    free(internal);
                    effective_rhs_kind = unboxed;
                }
            }
            /* A `long`-typed shift count needs narrowing to a single-word
             * int for ISHL/LSHL/etc - see the identical fix (and its full
             * explanation) in codegen_binary_expr() for the plain "<<"/
             * ">>"/">>>" operators. */
            if (effective_rhs_kind == TYPE_LONG) {
                bc_emit(mg->code, OP_L2I);
                mg_pop_typed(mg, 2);
                mg_push_int(mg);
            }
        } else {
            coerce_stack_value(mg, cp, raw_rhs_kind, raw_rhs_class, op_type, NULL);
        }
    }

    switch (op) {
        case TOK_PLUS_ASSIGN:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LADD); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FADD); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DADD); break;
                default:          bc_emit(mg->code, OP_IADD); break;
            }
            break;
        case TOK_MINUS_ASSIGN:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LSUB); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FSUB); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DSUB); break;
                default:          bc_emit(mg->code, OP_ISUB); break;
            }
            break;
        case TOK_STAR_ASSIGN:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LMUL); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FMUL); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DMUL); break;
                default:          bc_emit(mg->code, OP_IMUL); break;
            }
            break;
        case TOK_SLASH_ASSIGN:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LDIV); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FDIV); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DDIV); break;
                default:          bc_emit(mg->code, OP_IDIV); break;
            }
            break;
        case TOK_MOD_ASSIGN:
            switch (op_type) {
                case TYPE_LONG:   bc_emit(mg->code, OP_LREM); break;
                case TYPE_FLOAT:  bc_emit(mg->code, OP_FREM); break;
                case TYPE_DOUBLE: bc_emit(mg->code, OP_DREM); break;
                default:          bc_emit(mg->code, OP_IREM); break;
            }
            break;
        case TOK_AND_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LAND : OP_IAND);
            break;
        case TOK_OR_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LOR : OP_IOR);
            break;
        case TOK_XOR_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LXOR : OP_IXOR);
            break;
        case TOK_LSHIFT_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LSHL : OP_ISHL);
            break;
        case TOK_RSHIFT_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LSHR : OP_ISHR);
            break;
        case TOK_URSHIFT_ASSIGN:
            bc_emit(mg->code, op_type == TYPE_LONG ? OP_LUSHR : OP_IUSHR);
            break;
        default:
            fprintf(stderr, "codegen: unknown compound assignment operator (token %d)\n", op);
            return false;
    }

    /* The operator consumed both operands (op_type-sized left and, for a
     * shift, a 1-slot right regardless of op_type) and left one
     * op_type-sized result. */
    {
        int result_slots = (op_type == TYPE_LONG || op_type == TYPE_DOUBLE) ? 2 : 1;
        int operand_slots = is_shift ? result_slots + 1 : result_slots * 2;
        mg_pop_typed(mg, operand_slots);
        switch (op_type) {
            case TYPE_LONG:   mg_push_long(mg); break;
            case TYPE_DOUBLE: mg_push_double(mg); break;
            case TYPE_FLOAT:  mg_push_float(mg); break;
            default:          mg_push_int(mg); break;
        }
    }

    /* Narrow the result back to the LHS's own type: the implicit cast that
     * makes "byte b; b += 1;" legal without writing "b = (byte)(b + 1);". */
    if (!lhs_boolean && op_type != lhs_kind) {
        switch (op_type) {
            case TYPE_INT:
                if (lhs_kind == TYPE_BYTE) {
                    bc_emit(mg->code, OP_I2B);
                } else if (lhs_kind == TYPE_SHORT) {
                    bc_emit(mg->code, OP_I2S);
                } else if (lhs_kind == TYPE_CHAR) {
                    bc_emit(mg->code, OP_I2C);
                }
                break;
            case TYPE_LONG:
                bc_emit(mg->code, OP_L2I);
                mg_pop_typed(mg, 2);
                mg_push_int(mg);
                if (lhs_kind == TYPE_BYTE) {
                    bc_emit(mg->code, OP_I2B);
                } else if (lhs_kind == TYPE_SHORT) {
                    bc_emit(mg->code, OP_I2S);
                } else if (lhs_kind == TYPE_CHAR) {
                    bc_emit(mg->code, OP_I2C);
                }
                break;
            case TYPE_FLOAT:
                if (lhs_kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_F2L);
                    mg_pop_typed(mg, 1);
                    mg_push_long(mg);
                } else {
                    bc_emit(mg->code, OP_F2I);
                    mg_pop_typed(mg, 1);
                    mg_push_int(mg);
                    if (lhs_kind == TYPE_BYTE) {
                        bc_emit(mg->code, OP_I2B);
                    } else if (lhs_kind == TYPE_SHORT) {
                        bc_emit(mg->code, OP_I2S);
                    } else if (lhs_kind == TYPE_CHAR) {
                        bc_emit(mg->code, OP_I2C);
                    }
                }
                break;
            case TYPE_DOUBLE:
                switch (lhs_kind) {
                    case TYPE_LONG:
                        bc_emit(mg->code, OP_D2L);
                        break;
                    case TYPE_FLOAT:
                        bc_emit(mg->code, OP_D2F);
                        mg_pop_typed(mg, 1);
                        mg_push_float(mg);
                        break;
                    default:
                        bc_emit(mg->code, OP_D2I);
                        mg_pop_typed(mg, 1);
                        mg_push_int(mg);
                        if (lhs_kind == TYPE_BYTE) {
                            bc_emit(mg->code, OP_I2B);
                        } else if (lhs_kind == TYPE_SHORT) {
                            bc_emit(mg->code, OP_I2S);
                        } else if (lhs_kind == TYPE_CHAR) {
                            bc_emit(mg->code, OP_I2C);
                        }
                        break;
                }
                break;
            default:
                break;
        }
    }

    if (lhs_was_wrapper) {
        emit_boxing(mg, cp, lhs_kind);
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
                /* Load current value, then generate the RHS, apply the
                 * operator and narrow back to kind (JLS 15.26.2). */
                mg_emit_load_local(mg, slot, kind);
                if (!codegen_compound_rhs(mg, cp, value, kind, local_info->class_name, op)) {
                    return false;
                }
            } else {
                /* Simple assignment: box, unbox or widen to match kind */
                if (!codegen_expr(mg, value, cp)) {
                    return false;
                }
                type_kind_t val_kind;
                const char *val_class;
                value_kind_and_class(mg, value, &val_kind, &val_class);
                if (local_info->is_ref) {
                    coerce_stack_value(mg, cp, val_kind, val_class,
                                       TYPE_CLASS, local_info->class_name);
                } else {
                    coerce_stack_value(mg, cp, val_kind, val_class, kind, NULL);
                }
            }

            /* DUP the value so assignment expression can be chained (a = b = 0)
             * The duplicate will be left on the stack as the expression's value */
            if (kind == TYPE_LONG || kind == TYPE_DOUBLE) {
                bc_emit(mg->code, OP_DUP2);
                mg_push(mg, 2);
            } else {
                bc_emit(mg->code, OP_DUP);
                mg_push(mg, 1);
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
                        if (local_info->class_name) {
                            stackmap_set_local_object(mg->stackmap, slot, mg->cp, local_info->class_name);
                        }
                        break;
                    case TYPE_ARRAY:
                        /* local_info->class_name is only ever populated for
                         * TYPE_CLASS locals (see local_var_info_t's own
                         * comment: "Class internal name (for class types,
                         * NULL otherwise)") - an array-typed local's element
                         * kind/class aren't tracked there at all outside the
                         * parameter-registration path, so that check always
                         * failed here and this slot's stackmap tracking was
                         * silently never updated for a plain "arr = new
                         * T[n];" assignment. Build the real array descriptor
                         * from the right-hand side's own semantic type
                         * instead - reliable now that semantic.c's
                         * AST_EXPR_STMT and AST_ASSIGNMENT_EXPR cases ensure
                         * value->sem_type gets computed for exactly this
                         * shape. */
                        if (value->sem_type && value->sem_type->kind == TYPE_ARRAY) {
                            char *arr_desc = type_to_descriptor(value->sem_type);
                            if (arr_desc) {
                                stackmap_set_local_object(mg->stackmap, slot, mg->cp, arr_desc);
                                free(arr_desc);
                            }
                        } else if (local_info->class_name) {
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
                    if (compound) {
                        type_kind_t field_kind;
                        char field_class[512];
                        descriptor_kind_and_class(field->descriptor, &field_kind,
                                                  field_class, sizeof(field_class));
                        if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                                  field_class[0] ? field_class : NULL, op)) {
                            return false;
                        }
                    } else {
                        if (!codegen_expr(mg, value, cp)) {
                            return false;
                        }
                        coerce_value_to_descriptor(mg, cp, value, field->descriptor);
                    }

                    /* DUP the value so assignment expression can be chained (a = b = 0)
                     * The duplicate will be left on the stack as the expression's value */
                    bool field_is_wide = (field->descriptor[0] == 'J' || field->descriptor[0] == 'D');
                    bc_emit(mg->code, field_is_wide ? OP_DUP2 : OP_DUP);
                    mg_push(mg, field_is_wide ? 2 : 1);

                    /* Store to static field */
                    uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                         field->name, field->descriptor);
                    bc_emit(mg->code, OP_PUTSTATIC);
                    bc_emit_u2(mg->code, fieldref);
                    mg_pop_typed(mg, field_is_wide ? 2 : 1);  /* PUTSTATIC consumes the original, DUP's copy remains */
                    return true;
                } else if (!mg->is_static) {
                    /* Instance field assignment: this.field = value */

                    /* Load 'this' */
                    bc_emit(mg->code, OP_ALOAD_0);
                    /* Track the real pushed type (the current class), not
                     * null - mg_push_null() records a VT_NULL stackmap
                     * entry for what is actually a known, non-null `this`
                     * reference. Harmless as long as `this` is immediately
                     * consumed, but if any OTHER value gets pushed and a
                     * stack-map frame recorded before that happens (e.g. a
                     * null-comparison inside the RHS's own ternary
                     * condition, recording its own internal branch-target
                     * frame with `this` still buried underneath), the
                     * recorded frame wrongly types that slot as `null`
                     * instead of the current class - rejected the moment
                     * the real, non-null value reaches it
                     * (`VerifyError: Inconsistent stackmap frames ...
                     * Type 'X' ... not assignable to null`). */
                    if (mg->class_gen && mg->class_gen->internal_name) {
                        mg_push_object(mg, mg->class_gen->internal_name);
                    } else {
                        mg_push_null(mg);
                    }

                    if (compound) {
                        /* Duplicate 'this' for getfield, then load current value */
                        bc_emit(mg->code, OP_DUP);
                        mg_dup(mg);
                        
                        uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                             field->name, field->descriptor);
                        bc_emit(mg->code, OP_GETFIELD);
                        bc_emit_u2(mg->code, fieldref);
                        /* getfield consumes the duplicated ref (1 slot) and
                         * pushes the field's value - 1 slot for most kinds,
                         * but 2 for a wide (long/double) field, so this is
                         * NOT unconditionally a net-zero change to the
                         * tracked stack depth as a stale comment here used
                         * to claim. Mirror the sibling this.field/inherited-
                         * field and static-field compound-assignment
                         * branches, which both already do this correctly:
                         * pop the consumed ref, then push by the field's
                         * actual kind. Missing this for a wide field left
                         * mg->stack_depth undercounting the real bytecode
                         * stack by one slot from here on, so a later
                         * expression-statement's "pop vs pop2" choice
                         * (AST_EXPR_STMT, sized from stack_depth deltas)
                         * used a single POP for what was actually a 2-slot
                         * long/double value - which the verifier rejects
                         * outright, since POP requires a category-1 type. */
                        mg_pop_typed(mg, 1);
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
                    if (compound) {
                        type_kind_t field_kind;
                        char field_class[512];
                        descriptor_kind_and_class(field->descriptor, &field_kind,
                                                  field_class, sizeof(field_class));
                        if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                                  field_class[0] ? field_class : NULL, op)) {
                            return false;
                        }
                    } else {
                        if (!codegen_expr(mg, value, cp)) {
                            return false;
                        }
                        coerce_value_to_descriptor(mg, cp, value, field->descriptor);
                    }

                    /* DUP_X1 to keep a copy of value for chained assignments
                     * Stack before: [this, value]
                     * Stack after:  [value, this, value]
                     * Then PUTFIELD consumes [this, value], leaving [value]
                     *
                     * mg_dup_x1()/mg_dup2_x1() (not a raw mg_push()) also
                     * reorder mg->stackmap's own tracked types to match -
                     * without that, the stackmap's type array never
                     * reflected this real reordering, corrupting every
                     * stack-map frame recorded later in the same method
                     * (VerifyError: "Inconsistent stackmap frames"). */
                    if (field->descriptor[0] == 'J' || field->descriptor[0] == 'D') {
                        bc_emit(mg->code, OP_DUP2_X1);
                        mg_dup2_x1(mg);
                    } else {
                        bc_emit(mg->code, OP_DUP_X1);
                        mg_dup_x1(mg);
                    }

                    /* Store to field: putfield pops object ref and value */
                    uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                         field->name, field->descriptor);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, fieldref);
                    /* putfield consumes ref (1) + value (1 or 2 slots)
                     * DUP_X1/DUP2_X1 copy remains on stack */
                    if (field->descriptor[0] == 'J' || field->descriptor[0] == 'D') {
                        mg_pop_typed(mg, 3);  /* ref=1 + long/double=2, copy remains */
                    } else {
                        mg_pop_typed(mg, 2);  /* ref=1 + other=1, copy remains */
                    }
                    return true;
                }
                /* Instance field in static context - error handled below */
            }
            
            /* Check if this is an inherited field from superclass (set by semantic analysis) */
            if (target->sem_symbol && target->sem_symbol->kind == SYM_FIELD &&
                !(target->sem_symbol->modifiers & MOD_STATIC) && !mg->is_static) {
                symbol_t *field_sym = target->sem_symbol;
                /* Find which class the field belongs to by checking superclass chain */
                symbol_t *field_class = NULL;
                symbol_t *search = mg->class_gen->class_sym->data.class_data.superclass;
                while (search) {
                    if (search->data.class_data.members) {
                        symbol_t *found = scope_lookup_local(search->data.class_data.members, name);
                        /* Match by name, not by pointer, since symbols can be different instances */
                        if (found && found->kind == SYM_FIELD && 
                            strcmp(found->name, field_sym->name) == 0) {
                            field_class = search;
                            break;
                        }
                    }
                    search = search->data.class_data.superclass;
                }
                
                if (field_class && field_class->qualified_name) {
                    char *class_internal = class_to_internal_name(field_class->qualified_name);
                    char *field_desc = type_to_descriptor(field_sym->type);
                    
                    /* Load 'this' for putfield */
                    bc_emit(mg->code, OP_ALOAD_0);
                    /* Track the real pushed type, not null - see the
                     * identical fix (and full explanation) at the
                     * sibling own-class instance-field-assignment site
                     * above. */
                    if (mg->class_gen && mg->class_gen->internal_name) {
                        mg_push_object(mg, mg->class_gen->internal_name);
                    } else {
                        mg_push_null(mg);
                    }

                    if (compound) {
                        /* Duplicate 'this' for getfield, then load current value */
                        bc_emit(mg->code, OP_DUP);
                        mg_dup(mg);
                        
                        uint16_t fieldref = cp_add_fieldref(mg->cp, class_internal,
                                                             name, field_desc);
                        bc_emit(mg->code, OP_GETFIELD);
                        bc_emit_u2(mg->code, fieldref);
                        mg_pop_typed(mg, 1);
                        switch (field_desc[0]) {
                            case 'J': mg_push_long(mg); break;
                            case 'D': mg_push_double(mg); break;
                            case 'F': mg_push_float(mg); break;
                            case 'L': 
                            case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                            default:  mg_push_int(mg); break;
                        }
                    }
                    
                    /* Generate right-hand side */
                    if (compound) {
                        type_kind_t field_kind;
                        char field_class_name[512];
                        descriptor_kind_and_class(field_desc, &field_kind,
                                                  field_class_name, sizeof(field_class_name));
                        if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                                  field_class_name[0] ? field_class_name : NULL, op)) {
                            free(class_internal);
                            free(field_desc);
                            return false;
                        }
                    } else {
                        if (!codegen_expr(mg, value, cp)) {
                            free(class_internal);
                            free(field_desc);
                            return false;
                        }
                        coerce_value_to_descriptor(mg, cp, value, field_desc);
                    }

                    /* DUP_X1 for chained assignments */
                    if (field_desc[0] == 'J' || field_desc[0] == 'D') {
                        bc_emit(mg->code, OP_DUP2_X1);
                        mg_push(mg, 2);
                    } else {
                        bc_emit(mg->code, OP_DUP_X1);
                        mg_push(mg, 1);
                    }
                    
                    /* Store to superclass field */
                    uint16_t fieldref = cp_add_fieldref(mg->cp, class_internal,
                                                         name, field_desc);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, fieldref);
                    if (field_desc[0] == 'J' || field_desc[0] == 'D') {
                        mg_pop_typed(mg, 3);
                    } else {
                        mg_pop_typed(mg, 2);
                    }
                    
                    free(class_internal);
                    free(field_desc);
                    return true;
                }
            }
            
            /* Check all enclosing classes for nested/local/anonymous classes */
            symbol_t *class_sym = mg->class_gen->class_sym;
            symbol_t *enclosing = class_sym ? class_sym->data.class_data.enclosing_class : NULL;
            while (enclosing) {
                if (enclosing->data.class_data.members) {
                    symbol_t *outer_field = scope_lookup_local(
                        enclosing->data.class_data.members, name);
                    if (outer_field && outer_field->kind == SYM_FIELD) {
                        char *outer_internal = class_to_internal_name(enclosing->qualified_name);
                        char *field_desc = type_to_descriptor(outer_field->type);
                        
                        if (outer_field->modifiers & MOD_STATIC) {
                            /* Static field in enclosing class */
                            if (compound) {
                                /* Load current value first */
                                uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                                     name, field_desc);
                                bc_emit(mg->code, OP_GETSTATIC);
                                bc_emit_u2(mg->code, fieldref);
                                switch (field_desc[0]) {
                                    case 'J': mg_push_long(mg); break;
                                    case 'D': mg_push_double(mg); break;
                                    case 'F': mg_push_float(mg); break;
                                    case 'L': 
                                    case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                                    default:  mg_push_int(mg); break;
                                }
                            }
                            
                            /* Generate right-hand side */
                            if (compound) {
                                type_kind_t field_kind;
                                char field_class_name[512];
                                descriptor_kind_and_class(field_desc, &field_kind,
                                                          field_class_name, sizeof(field_class_name));
                                if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                        field_class_name[0] ? field_class_name : NULL, op)) {
                                    free(outer_internal);
                                    free(field_desc);
                                    return false;
                                }
                            } else {
                                if (!codegen_expr(mg, value, cp)) {
                                    free(outer_internal);
                                    free(field_desc);
                                    return false;
                                }
                                coerce_value_to_descriptor(mg, cp, value, field_desc);
                            }

                            /* DUP for chained assignments */
                            bool outer_static_wide = (field_desc[0] == 'J' || field_desc[0] == 'D');
                            bc_emit(mg->code, outer_static_wide ? OP_DUP2 : OP_DUP);
                            mg_push(mg, outer_static_wide ? 2 : 1);

                            /* Store to outer static field */
                            uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                                 name, field_desc);
                            bc_emit(mg->code, OP_PUTSTATIC);
                            bc_emit_u2(mg->code, fieldref);
                            mg_pop_typed(mg, outer_static_wide ? 2 : 1);

                            free(outer_internal);
                            free(field_desc);
                            return true;
                        } else if (mg->class_gen->is_inner_class ||
                                   mg->class_gen->is_local_class ||
                                   mg->class_gen->is_anonymous_class) {
                            /* Instance field in enclosing class - access via this$0,
                             * walking the full this$0 chain (not just one hop) since
                             * 'enclosing' may be two or more levels of anonymous/
                             * inner/local class away from the current class. */
                            codegen_load_enclosing_this(mg, cp, enclosing);
                            mg_pop_typed(mg, 1);
                            mg_push_object(mg, outer_internal);

                            if (compound) {
                                /* Duplicate outer instance for getfield */
                                bc_emit(mg->code, OP_DUP);
                                mg_dup(mg);
                                
                                uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                                     name, field_desc);
                                bc_emit(mg->code, OP_GETFIELD);
                                bc_emit_u2(mg->code, fieldref);
                                mg_pop_typed(mg, 1);
                                switch (field_desc[0]) {
                                    case 'J': mg_push_long(mg); break;
                                    case 'D': mg_push_double(mg); break;
                                    case 'F': mg_push_float(mg); break;
                                    case 'L': 
                                    case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                                    default:  mg_push_int(mg); break;
                                }
                            }
                            
                            /* Generate right-hand side */
                            if (compound) {
                                type_kind_t field_kind;
                                char field_class_name[512];
                                descriptor_kind_and_class(field_desc, &field_kind,
                                                          field_class_name, sizeof(field_class_name));
                                if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                        field_class_name[0] ? field_class_name : NULL, op)) {
                                    free(outer_internal);
                                    free(field_desc);
                                    return false;
                                }
                            } else {
                                if (!codegen_expr(mg, value, cp)) {
                                    free(outer_internal);
                                    free(field_desc);
                                    return false;
                                }
                                coerce_value_to_descriptor(mg, cp, value, field_desc);
                            }

                            /* DUP_X1 for chained assignments */
                            if (field_desc[0] == 'J' || field_desc[0] == 'D') {
                                bc_emit(mg->code, OP_DUP2_X1);
                                mg_push(mg, 2);
                            } else {
                                bc_emit(mg->code, OP_DUP_X1);
                                mg_push(mg, 1);
                            }

                            /* Store to outer instance field */
                            uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                                 name, field_desc);
                            bc_emit(mg->code, OP_PUTFIELD);
                            bc_emit_u2(mg->code, fieldref);
                            if (field_desc[0] == 'J' || field_desc[0] == 'D') {
                                mg_pop_typed(mg, 3);
                            } else {
                                mg_pop_typed(mg, 2);
                            }
                            
                            free(outer_internal);
                            free(field_desc);
                            return true;
                        }
                        
                        free(outer_internal);
                        free(field_desc);
                    }
                }
                enclosing = enclosing->data.class_data.enclosing_class;
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

            /* resolve_class_name() only knows a handful of well-known JDK
             * classes, the current class, and a nested class of the
             * current class - it never looks at imports (its own comment
             * says so: "TODO: Check imports"). Fall back to semantic
             * analysis's own resolution, the same way the READ side of a
             * static field access (the AST_IDENTIFIER branch of
             * codegen_field_access, a few hundred lines above) already
             * does: receiver->sem_symbol is set to the imported class's
             * real symbol regardless of which package it's in. Without
             * this, assigning to a static field of an imported class not
             * in resolve_class_name()'s whitelist (e.g. "StorageExecutor.
             * workThreadObserver = ...", StorageExecutor imported from a
             * different package) fell through to the "instance field
             * assignment" path below with a bare class-name AST_IDENTIFIER
             * as the "receiver" - codegen_expr() on that identifier is not
             * a real expression and pushes nothing, so the value alone
             * ended up on the stack where [receiver, value] was expected,
             * and the DUP_X1/PUTFIELD sequence that follows saw an empty
             * or short stack (VerifyError: "Operand stack underflow" /
             * "Attempt to pop empty stack") - confirmed against gumdrop's
             * own ZoneFilePersistenceTest, which does exactly this. */
            if (!class_name && receiver->sem_symbol &&
                (receiver->sem_symbol->kind == SYM_CLASS ||
                 receiver->sem_symbol->kind == SYM_INTERFACE ||
                 receiver->sem_symbol->kind == SYM_ENUM)) {
                static __thread char external_class_name[256];
                if (receiver->sem_symbol->qualified_name) {
                    char *internal = class_to_internal_name(receiver->sem_symbol->qualified_name);
                    strncpy(external_class_name, internal, sizeof(external_class_name) - 1);
                    external_class_name[sizeof(external_class_name) - 1] = '\0';
                    free(internal);
                    class_name = external_class_name;
                }
            }

            if (class_name) {
                /* Static field assignment */
                const char *field_desc = get_known_static_field_descriptor(class_name, field_name);

                if (!field_desc && mg->class_gen && strcmp(class_name, mg->class_gen->internal_name) == 0) {
                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
                    if (field) {
                        field_desc = field->descriptor;
                    }
                }

                /* Same external-class fallback as the read path: an
                 * imported class's field isn't in either lookup above
                 * (it's neither "well-known" nor the current class), but
                 * its symbol - and members - are available via
                 * receiver->sem_symbol once semantic analysis has run. */
                if (!field_desc && receiver->sem_symbol &&
                    (receiver->sem_symbol->kind == SYM_CLASS ||
                     receiver->sem_symbol->kind == SYM_INTERFACE ||
                     receiver->sem_symbol->kind == SYM_ENUM)) {
                    symbol_t *ext_class = receiver->sem_symbol;
                    if (ext_class->data.class_data.members) {
                        symbol_t *field_sym = scope_lookup_local(
                            ext_class->data.class_data.members, field_name);
                        if (field_sym && field_sym->kind == SYM_FIELD && field_sym->type) {
                            static __thread char ext_field_desc_buf[256];
                            char *desc = type_to_descriptor(field_sym->type);
                            if (desc) {
                                strncpy(ext_field_desc_buf, desc, sizeof(ext_field_desc_buf) - 1);
                                ext_field_desc_buf[sizeof(ext_field_desc_buf) - 1] = '\0';
                                free(desc);
                                field_desc = ext_field_desc_buf;
                            }
                        }
                    }
                }

                if (!field_desc) {
                    fprintf(stderr, "codegen: cannot resolve static field for assignment: %s.%s\n",
                            recv_name, field_name);
                    return false;
                }
                
                bool static_field_is_wide = (field_desc[0] == 'J' || field_desc[0] == 'D');

                if (compound) {
                    /* Load current value, generate the RHS, apply the
                     * operator and narrow back (JLS 15.26.2) */
                    uint16_t getref = cp_add_fieldref(cp, class_name, field_name, field_desc);
                    bc_emit(mg->code, OP_GETSTATIC);
                    bc_emit_u2(mg->code, getref);
                    switch (field_desc[0]) {
                        case 'J': mg_push_long(mg); break;
                        case 'D': mg_push_double(mg); break;
                        case 'F': mg_push_float(mg); break;
                        case 'L':
                        case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                        default:  mg_push_int(mg); break;
                    }

                    type_kind_t field_kind;
                    char field_class_name[512];
                    descriptor_kind_and_class(field_desc, &field_kind,
                                              field_class_name, sizeof(field_class_name));
                    if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                              field_class_name[0] ? field_class_name : NULL, op)) {
                        return false;
                    }
                } else {
                    /* Generate value, then box, unbox or widen to the field's type */
                    if (!codegen_expr(mg, value, cp)) {
                        return false;
                    }
                    coerce_value_to_descriptor(mg, cp, value, field_desc);
                }

                /* DUP the value so the assignment expression can be chained
                 * (a = Test.sfield = 0), as the identifier-target branches do */
                bc_emit(mg->code, static_field_is_wide ? OP_DUP2 : OP_DUP);
                mg_push(mg, static_field_is_wide ? 2 : 1);

                /* Emit putstatic */
                uint16_t fieldref = cp_add_fieldref(cp, class_name, field_name, field_desc);
                bc_emit(mg->code, OP_PUTSTATIC);
                bc_emit_u2(mg->code, fieldref);
                mg_pop_typed(mg, static_field_is_wide ? 2 : 1);  /* PUTSTATIC consumes the original, DUP's copy remains */

                return true;
            }
        }
        
        /* Instance field assignment */
        /* Determine field class and descriptor before generating the
         * receiver: compound assignment needs the descriptor to load the
         * current value right after the receiver. */
        const char *recv_class = "java/lang/Object";
        const char *field_desc = "I";  /* Default to int */
        bool field_desc_owned = false;

        /* Try to get receiver class from semantic info */
        if (receiver->sem_type && receiver->sem_type->kind == TYPE_CLASS) {
            if (receiver->sem_type->data.class_type.name) {
                recv_class = class_to_internal_name(receiver->sem_type->data.class_type.name);
            }

            /* Look up field descriptor from receiver's class symbol -
             * walking the superclass chain too, since the field may be
             * inherited (see lookup_field_with_superclass()). */
            symbol_t *class_sym = receiver->sem_type->data.class_type.symbol;
            if (class_sym) {
                symbol_t *field_sym = lookup_field_with_superclass(class_sym, field_name);
                if (field_sym && field_sym->type) {
                    char *desc = type_to_descriptor(field_sym->type);
                    if (desc) {
                        field_desc = desc;
                        field_desc_owned = true;
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
                field_desc_owned = false;
            } else if (!field_desc_owned && mg->class_gen->class_sym) {
                /* mg->class_gen->field_map only ever holds fields declared
                 * directly on THIS class - a field inherited from a
                 * superclass (e.g. "this.deliveryCount" where
                 * deliveryCount is declared on a superclass, not the
                 * current class) is never in it, so this lookup alone
                 * silently fell through with field_desc still at its
                 * hardcoded "I" (int) default from above - the earlier,
                 * correctly superclass-aware lookup via
                 * lookup_field_with_superclass() (right above this
                 * block) never even ran for a bare "this" receiver,
                 * since AST_THIS_EXPR nodes carry no sem_type for that
                 * check to key off. Faking an int descriptor for what's
                 * actually e.g. a long field picked the narrow DUP_X1
                 * instead of DUP2_X1 below (VerifyError: "Bad type on
                 * operand stack", a stray long's second half where a
                 * category-1 value was expected) - walk the superclass
                 * chain here too, the same way the sem_type-based branch
                 * above already does. */
                symbol_t *field_sym = lookup_field_with_superclass(mg->class_gen->class_sym, field_name);
                if (field_sym && field_sym->type) {
                    char *desc = type_to_descriptor(field_sym->type);
                    if (desc) {
                        field_desc = desc;
                        field_desc_owned = true;
                    }
                }
            }
        }

        bool field_is_wide = (field_desc[0] == 'J' || field_desc[0] == 'D');

        /* Generate receiver */
        if (!codegen_expr(mg, receiver, cp)) {
            if (field_desc_owned) {
                free((char *)field_desc);
            }
            return false;
        }

        if (compound) {
            /* Duplicate the receiver for getfield, then load the current
             * value, generate the RHS, apply the operator and narrow back
             * to field_desc's type (JLS 15.26.2). */
            bc_emit(mg->code, OP_DUP);
            mg_dup(mg);
            uint16_t getref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
            bc_emit(mg->code, OP_GETFIELD);
            bc_emit_u2(mg->code, getref);
            mg_pop_typed(mg, 1);  /* getfield consumes the duplicated receiver */
            switch (field_desc[0]) {
                case 'J': mg_push_long(mg); break;
                case 'D': mg_push_double(mg); break;
                case 'F': mg_push_float(mg); break;
                case 'L':
                case '[': mg_push_object_from_descriptor(mg, field_desc); break;
                default:  mg_push_int(mg); break;
            }

            type_kind_t field_kind;
            char field_class_name[512];
            descriptor_kind_and_class(field_desc, &field_kind,
                                      field_class_name, sizeof(field_class_name));
            if (!codegen_compound_rhs(mg, cp, value, field_kind,
                                      field_class_name[0] ? field_class_name : NULL, op)) {
                if (field_desc_owned) {
                    free((char *)field_desc);
                }
                return false;
            }
        } else {
            /* Generate value, then box, unbox or widen to the field's type */
            if (!codegen_expr(mg, value, cp)) {
                if (field_desc_owned) {
                    free((char *)field_desc);
                }
                return false;
            }
            coerce_value_to_descriptor(mg, cp, value, field_desc);
        }

        /* DUP_X1 to keep a copy of the value for chained assignments
         * (a = obj.field = 0), as the this.field/inherited-field branches
         * above do.
         * Stack before: [receiver, value]
         * Stack after:  [value, receiver, value]
         * Then PUTFIELD consumes [receiver, value], leaving [value] */
        if (field_is_wide) {
            bc_emit(mg->code, OP_DUP2_X1);
            mg_push(mg, 2);
        } else {
            bc_emit(mg->code, OP_DUP_X1);
            mg_push(mg, 1);
        }

        /* Emit putfield */
        uint16_t fieldref = cp_add_fieldref(cp, recv_class, field_name, field_desc);
        bc_emit(mg->code, OP_PUTFIELD);
        bc_emit_u2(mg->code, fieldref);
        /* putfield consumes ref (1) + value (1 or 2 slots); the DUP_X1/
         * DUP2_X1 copy remains */
        mg_pop_typed(mg, field_is_wide ? 3 : 2);

        if (field_desc_owned) {
            free((char *)field_desc);
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
        
        /* Element kind/class, needed both to load the current value below
         * (compound only) and, after the new value is generated, to convert
         * it to the element type (both compound and simple). */
        type_kind_t elem_kind = TYPE_INT;
        const char *elem_class = NULL;

        /* For compound assignment, we need to load the current value first */
        if (compound) {
            /* Stack: arrayref, index */
            /* Need: arrayref, index, arrayref, index for load, then value for store */
            bc_emit(mg->code, OP_DUP2);  /* Duplicate arrayref and index */
            /* Push the duplicated types: arrayref (null) and index (int) */
            mg_push_null(mg);
            mg_push_int(mg);

            /* Load current value */
            if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    elem_kind = elem_type->kind;
                    if (elem_kind == TYPE_CLASS) {
                        elem_class = elem_type->data.class_type.name;
                    }
                }
            } else if (array_expr->type == AST_IDENTIFIER) {
                const char *arr_name = array_expr->data.leaf.name;
                if (mg_local_is_array(mg, arr_name)) {
                    elem_kind = mg_local_array_elem_kind(mg, arr_name);
                    if (elem_kind == TYPE_CLASS) {
                        elem_class = mg_local_array_elem_class(mg, arr_name);
                    }
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
                    break;
                case TYPE_FLOAT:
                    bc_emit(mg->code, OP_FALOAD);
                    break;
                case TYPE_DOUBLE:
                    bc_emit(mg->code, OP_DALOAD);
                    break;
                case TYPE_CLASS:
                case TYPE_ARRAY:
                    bc_emit(mg->code, OP_AALOAD);
                    break;
                default:
                    bc_emit(mg->code, OP_IALOAD);
                    break;
            }
            /* Load consumed the duplicated arrayref+index (2 tracked
             * slots) and pushed the loaded value - pop both placeholder
             * entries and push the value's real type, or the array's own
             * (stale) type is left on the tracked stack in its place (see
             * the identical fix in AST_ARRAY_ACCESS's own load codegen). */
            mg_pop_typed(mg, 2);
            switch (elem_kind) {
                case TYPE_LONG:   mg_push_long(mg); break;
                case TYPE_FLOAT:  mg_push_float(mg); break;
                case TYPE_DOUBLE: mg_push_double(mg); break;
                case TYPE_CLASS:
                    {
                        char *internal = class_to_internal_name(elem_class ? elem_class : "java.lang.Object");
                        mg_push_object(mg, internal);
                        free(internal);
                    }
                    break;
                case TYPE_ARRAY:
                    mg_push_object(mg, "java/lang/Object");
                    break;
                default:
                    mg_push_int(mg);
                    break;
            }
        }
        
        /* Generate value expression */
        if (compound) {
            /* elem_kind/elem_class were computed above (in scope: they are
             * declared inside "if (compound)" but this whole target branch
             * is only reached that way for a compound assignment). */
            if (!codegen_compound_rhs(mg, cp, value, elem_kind, elem_class, op)) {
                return false;
            }
        } else {
            if (!codegen_expr(mg, value, cp)) {
                return false;
            }
        }

        /* Determine element type and emit appropriate store opcode */
        type_kind_t store_elem_kind = TYPE_INT;
        const char *store_elem_class = NULL;
        if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
            /* array_expr->sem_type's own array_type struct stores the
             * LEAF element type (e.g. "byte" for byte[][]) plus a
             * separate dimensions count - NOT one type_t per dimension -
             * so element_type->kind alone only tells you the eventual
             * SCALAR type, not what ONE level of indexing on THIS array
             * actually produces. Indexing a multi-dimensional array once
             * still yields a sub-ARRAY (a reference type, dimensions-1),
             * not the leaf scalar - e.g. "byte[][] values = ...;
             * values[i] = someByteArray;" needs AASTORE (storing a
             * reference), not BASTORE, even though the leaf element type
             * is byte. Mirrors the identical dimensions>1 check already
             * used correctly in codegen_array_init() for an array
             * LITERAL's own element stores. Without this, every
             * assignment through one level of a >1-dimensional
             * array emitted the LEAF type's scalar store instead:
             * VerifyError "Bad type on operand stack ... not assignable
             * to integer" the moment the actual (reference) value reached
             * it. Confirmed against gumdrop's own
             * MessageIndexEntry.buildVariableData(), whose
             * "values[DESC_LOCATION] = toBytes(location);" on a
             * byte[][] is exactly this shape. */
            int dims = array_expr->sem_type->data.array_type.dimensions;
            if (dims > 1) {
                store_elem_kind = TYPE_ARRAY;
            } else {
                type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                if (elem_type) {
                    store_elem_kind = elem_type->kind;
                    if (store_elem_kind == TYPE_CLASS) {
                        store_elem_class = elem_type->data.class_type.name;
                    }
                }
            }
        } else if (array_expr->type == AST_IDENTIFIER) {
            const char *arr_name = array_expr->data.leaf.name;
            if (mg_local_is_array(mg, arr_name)) {
                store_elem_kind = mg_local_array_elem_kind(mg, arr_name);
                if (store_elem_kind == TYPE_CLASS) {
                    store_elem_class = mg_local_array_elem_class(mg, arr_name);
                }
            }
        }

        if (!compound) {
            /* Box, unbox or widen to the array's element type */
            type_kind_t val_kind;
            const char *val_class;
            value_kind_and_class(mg, value, &val_kind, &val_class);
            coerce_stack_value(mg, cp, val_kind, val_class, store_elem_kind, store_elem_class);
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
    
    /* Handle parenthesized expression - unwrap and recurse */
    if (target->type == AST_PARENTHESIZED) {
        slist_t *paren_children = target->data.node.children;
        if (paren_children) {
            ast_node_t *inner = (ast_node_t *)paren_children->data;
            if (inner) {
                /* Create a modified assignment with unwrapped target */
                ast_node_t *unwrapped = ast_new(AST_ASSIGNMENT_EXPR, expr->line, expr->column);
                unwrapped->data.node.op_token = expr->data.node.op_token;
                unwrapped->sem_type = expr->sem_type;
                ast_add_child(unwrapped, inner);
                ast_add_child(unwrapped, value);
                bool result = codegen_assignment(mg, unwrapped, cp);
                /* Don't free children since they belong to original nodes */
                unwrapped->data.node.children = NULL;
                ast_free(unwrapped);
                return result;
            }
        }
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
        if (rule->type != AST_SWITCH_RULE) {
            continue;
        }
        
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
        
        if (!pattern) {
            continue;
        }
        
        /* Get pattern type and variable */
        const char *var_name = pattern->data.node.name;
        slist_t *pattern_children = pattern->data.node.children;
        if (!pattern_children) {
            continue;
        }
        
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
        
        if (!type_internal) {
            continue;
        }
        
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
                        if (comp_type) {
                            kind = comp_type->kind;
                        }
                        
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

/**
 * A field declared with a type-variable type (T value) is erased to its bound
 * in the class file. When the use site's type is more specific (a field of a
 * Box<Integer>), the loaded value must be cast back, as is done for methods
 * returning a type variable.
 */
static void checkcast_generic_field(method_gen_t *mg, const_pool_t *cp, ast_node_t *expr)
{
    symbol_t *field = expr->sem_symbol;
    
    /* Semantic analysis does not always record the field symbol on the node;
     * find it from the receiver's class (and its superclasses) instead. */
    if ((!field || field->kind != SYM_FIELD) && expr->type == AST_FIELD_ACCESS &&
        expr->data.node.name && expr->data.node.children) {
        ast_node_t *receiver = (ast_node_t *)expr->data.node.children->data;
        type_t *rt = receiver ? receiver->sem_type : NULL;
        symbol_t *cls = (rt && rt->kind == TYPE_CLASS) ? rt->data.class_type.symbol : NULL;
        field = NULL;
        for (; cls && !field; cls = cls->data.class_data.superclass) {
            if (cls->data.class_data.members) {
                symbol_t *found = scope_lookup_local(cls->data.class_data.members,
                                                     expr->data.node.name);
                if (found && found->kind == SYM_FIELD) {
                    field = found;
                }
            }
        }
    }
    if (!field || field->kind != SYM_FIELD || !field->type ||
        field->type->kind != TYPE_TYPEVAR || !expr->sem_type) {
        return;
    }

    /* The type variable's resolved use-site type can itself be an array
     * (e.g. "T value" in FieldValue<byte[]>) - JVMS 4.4.1 allows a
     * CHECKCAST's class constant to be either a binary class name or an
     * array descriptor, so this needs its own branch rather than being
     * folded into the TYPE_CLASS case below. Without it, this whole
     * function returned early for an array-resolved type variable, never
     * emitting any checkcast at all: the erased field stayed typed as
     * Object, rejected the moment it was used somewhere requiring the
     * real array type (e.g. as a byte[] argument to another call):
     * VerifyError "Bad type on operand stack ... Object ... is not
     * assignable to '[B'". Confirmed against gumdrop's own
     * ProtobufParserTest, whose generic "FieldValue<byte[]>.value" hits
     * exactly this via "new String(handler.bytes.get(0).value, ...)". */
    if (expr->sem_type->kind == TYPE_ARRAY) {
        char *array_desc = type_to_descriptor(expr->sem_type);
        if (!array_desc) {
            return;
        }
        uint16_t class_idx = cp_add_class(cp, array_desc);
        bc_emit(mg->code, OP_CHECKCAST);
        bc_emit_u2(mg->code, class_idx);
        /* CHECKCAST changes the verifier's own tracked type for this
         * slot from the field's erased declaration (Object) to the real
         * array type - without updating mg->stackmap to match, a later
         * frame recorded while this value is still on the stack (e.g. as
         * one argument of a multi-argument call) would still show the
         * stale Object type. Mirrors the identical fix already applied
         * to arraylength and the this$0-walking GETFIELD earlier this
         * session. */
        mg_pop_typed(mg, 1);
        mg_push_object_from_descriptor(mg, array_desc);
        free(array_desc);
        return;
    }

    if (expr->sem_type->kind != TYPE_CLASS || !expr->sem_type->data.class_type.name) {
        return;
    }

    const char *target = expr->sem_type->data.class_type.name;
    const char *erased = "java.lang.Object";
    type_t *bound = field->type->data.type_var.bound;
    if (bound && bound->kind == TYPE_CLASS && bound->data.class_type.name) {
        erased = bound->data.class_type.name;
    }
    if (strcmp(target, erased) == 0) {
        return;
    }

    char *internal = class_to_internal_name(target);
    uint16_t class_idx = cp_add_class(cp, internal);
    bc_emit(mg->code, OP_CHECKCAST);
    bc_emit_u2(mg->code, class_idx);
    mg_pop_typed(mg, 1);
    mg_push_object(mg, internal);
    free(internal);
}

bool codegen_expr(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp)
{
    if (!expr) {
        return false;
    }
    
    switch (expr->type) {
        case AST_LITERAL:
            return codegen_literal(mg, expr, cp);
        
        case AST_IDENTIFIER:
            if (!codegen_identifier(mg, expr)) {
                return false;
            }
            checkcast_generic_field(mg, cp, expr);
            return true;
        
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
                        switch (get_expr_type_kind(mg, operand)) {
                            case TYPE_LONG:   bc_emit(mg->code, OP_LNEG); break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_FNEG); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_DNEG); break;
                            default:          bc_emit(mg->code, OP_INEG); break;
                        }
                        return true;
                    
                    case TOK_PLUS:
                        /* Unary plus: no-op */
                        return codegen_expr(mg, operand, cp);
                    
                    case TOK_NOT:
                        /* Logical not: if val != 0 push 0, else push 1 */
                        if (!codegen_expr(mg, operand, cp)) {
                            return false;
                        }
                        
                        /* Auto-unbox Boolean wrapper if needed */
                        if (operand->sem_type && operand->sem_type->kind == TYPE_CLASS && 
                            operand->sem_type->data.class_type.name) {
                            type_kind_t prim = get_primitive_for_wrapper(operand->sem_type->data.class_type.name);
                            if (prim == TYPE_BOOLEAN) {
                                char *internal = class_to_internal_name(operand->sem_type->data.class_type.name);
                                emit_unboxing(mg, cp, prim, internal);
                                free(internal);
                            }
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
                        /* Bitwise not: xor with -1. A `long` operand needs
                         * a `long` -1 and LXOR, not an `int` -1 and IXOR -
                         * int semantics on a long value corrupts the
                         * operand's own type on the verifier's operand
                         * stack: "Bad type on operand stack ... long_2nd
                         * ... is not assignable to integer". Confirmed
                         * against gumdrop's own
                         * PacketNumberCodec.decode()'s "~pnMask", where
                         * pnMask is declared long. Mirrors the
                         * op_type==TYPE_LONG check every OTHER bitwise
                         * operator (TOK_BITAND/TOK_BITOR/TOK_CARET/etc.)
                         * already has, just missing here since this is a
                         * unary (not binary) operator with its own
                         * separate codegen path. */
                        if (!codegen_expr(mg, operand, cp)) {
                            return false;
                        }
                        if (get_expr_type_kind(mg, operand) == TYPE_LONG) {
                            bc_emit(mg->code, OP_ICONST_M1);
                            bc_emit(mg->code, OP_I2L);
                            mg_push_long(mg);
                            bc_emit(mg->code, OP_LXOR);
                            mg_pop_typed(mg, 4);  /* both long operands (2 words each) */
                            mg_push_long(mg);
                        } else {
                            bc_emit(mg->code, OP_ICONST_M1);
                            mg_push_int(mg);
                            bc_emit(mg->code, OP_IXOR);
                            mg_pop_typed(mg, 2);  /* both int operands */
                            mg_push_int(mg);
                        }
                        return true;
                    
                    case TOK_INC:
                    case TOK_DEC:
                        {
                            /* Pre/post increment/decrement */
                            /* Parser sets flags=1 for postfix (x++), flags=0 for prefix (++x) */
                            bool is_post = (expr->data.node.flags == 1);
                            bool is_inc = (op == TOK_INC);
                            int delta = is_inc ? 1 : -1;
                            
                            if (operand->type == AST_IDENTIFIER) {
                                /* Local variable increment/decrement */
                                const char *name = operand->data.leaf.name;
                                local_var_info_t *inc_info = (local_var_info_t *)hashtable_lookup(mg->locals, name);
                                
                                if (inc_info) {
                                    uint16_t slot = inc_info->slot;
                                    type_kind_t inc_kind = inc_info->kind;

                                    if (inc_kind == TYPE_LONG || inc_kind == TYPE_DOUBLE ||
                                        inc_kind == TYPE_FLOAT) {
                                        /* JVM's iinc only ever operates on a single 32-bit
                                         * int-categorized local slot - there's no wide
                                         * equivalent, so a long/double/float local variable
                                         * can't use the iload/iinc shortcut below at all (doing
                                         * so silently truncated/corrupted the value and left
                                         * the slot's own stackmap-tracked type - long/double/
                                         * float - out of sync with the int-only iload/iinc
                                         * that was actually emitted, producing "VerifyError:
                                         * Bad local variable type"). Load, add/subtract 1, and
                                         * store back explicitly instead, duplicating whichever
                                         * value (old for post, new for pre) this expression
                                         * itself evaluates to - the same "dup before/after the
                                         * arithmetic" shape already used for a plain local
                                         * assignment's own chaining support just above. */
                                        bool wide = (inc_kind == TYPE_LONG || inc_kind == TYPE_DOUBLE);
                                        mg_emit_load_local(mg, slot, inc_kind);

                                        if (is_post) {
                                            bc_emit(mg->code, wide ? OP_DUP2 : OP_DUP);
                                            mg_push(mg, wide ? 2 : 1);
                                        }

                                        switch (inc_kind) {
                                            case TYPE_LONG:
                                                bc_emit(mg->code, OP_LCONST_1);
                                                mg_push_long(mg);
                                                bc_emit(mg->code, is_inc ? OP_LADD : OP_LSUB);
                                                mg_pop_typed(mg, 2);
                                                break;
                                            case TYPE_DOUBLE:
                                                bc_emit(mg->code, OP_DCONST_1);
                                                mg_push_double(mg);
                                                bc_emit(mg->code, is_inc ? OP_DADD : OP_DSUB);
                                                mg_pop_typed(mg, 2);
                                                break;
                                            default: /* TYPE_FLOAT */
                                                bc_emit(mg->code, OP_FCONST_1);
                                                mg_push_float(mg);
                                                bc_emit(mg->code, is_inc ? OP_FADD : OP_FSUB);
                                                mg_pop_typed(mg, 1);
                                                break;
                                        }

                                        if (!is_post) {
                                            bc_emit(mg->code, wide ? OP_DUP2 : OP_DUP);
                                            mg_push(mg, wide ? 2 : 1);
                                        }

                                        mg_emit_store_local(mg, slot, inc_kind);

                                        /* mg_emit_store_local only adjusts the operand-stack
                                         * depth - refresh the slot's own stackmap-tracked
                                         * local type too, mirroring the identical update
                                         * after a plain local assignment. */
                                        if (mg->stackmap) {
                                            switch (inc_kind) {
                                                case TYPE_LONG:
                                                    stackmap_set_local_long(mg->stackmap, slot);
                                                    break;
                                                case TYPE_DOUBLE:
                                                    stackmap_set_local_double(mg->stackmap, slot);
                                                    break;
                                                default:
                                                    stackmap_set_local_float(mg->stackmap, slot);
                                                    break;
                                            }
                                        }
                                        return true;
                                    }

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
                                
                                /* Check for static field first */
                                if (mg->class_gen) {
                                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
                                    if (field && (field->access_flags & ACC_STATIC)) {
                                        /* Static field: ClassName.field++ or ++ClassName.field */
                                        uint16_t fieldref = cp_add_fieldref(mg->cp, mg->class_gen->internal_name,
                                                                             field->name, field->descriptor);
                                        
                                        if (is_post) {
                                            /* Post: getstatic, dup, iconst_1, iadd/isub, putstatic */
                                            /* Stack trace: [] -> [old] -> [old,old] -> [old,old,1] -> [old,new] -> [old] */
                                            bc_emit(mg->code, OP_GETSTATIC);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_push_int(mg);  /* [old] */
                                            bc_emit(mg->code, OP_DUP);
                                            mg_push_int(mg);  /* [old,old] */
                                            bc_emit(mg->code, OP_ICONST_1);
                                            mg_push_int(mg);  /* [old,old,1] */
                                            bc_emit(mg->code, is_inc ? OP_IADD : OP_ISUB);
                                            mg_pop_typed(mg, 1);   /* [old,new] */
                                            bc_emit(mg->code, OP_PUTSTATIC);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_pop_typed(mg, 1);   /* [old] - putstatic consumes value */
                                            /* Result: old value on stack, stack_depth = 1 */
                                        } else {
                                            /* Pre: getstatic, iconst_1, iadd/isub, dup, putstatic */
                                            /* Stack trace: [] -> [old] -> [old,1] -> [new] -> [new,new] -> [new] */
                                            bc_emit(mg->code, OP_GETSTATIC);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_push_int(mg);  /* [old] */
                                            bc_emit(mg->code, OP_ICONST_1);
                                            mg_push_int(mg);  /* [old,1] */
                                            bc_emit(mg->code, is_inc ? OP_IADD : OP_ISUB);
                                            mg_pop_typed(mg, 1);   /* [new] */
                                            bc_emit(mg->code, OP_DUP);
                                            mg_push_int(mg);  /* [new,new] */
                                            bc_emit(mg->code, OP_PUTSTATIC);
                                            bc_emit_u2(mg->code, fieldref);
                                            mg_pop_typed(mg, 1);   /* [new] - putstatic consumes value */
                                            /* Result: new value on stack, stack_depth = 1 */
                                        }
                                        return true;
                                    }
                                }
                                
                                /* Check for instance field */
                                if (mg->class_gen && !mg->is_static) {
                                    field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
                                    if (field && !(field->access_flags & ACC_STATIC)) {
                                        return codegen_this_instance_field_incdec(mg, cp,
                                            mg->class_gen->internal_name, field->name,
                                            field->descriptor, is_post, is_inc);
                                    }
                                }

                                /* Inherited instance field on this (superclass) */
                                if (operand->sem_symbol && operand->sem_symbol->kind == SYM_FIELD &&
                                    !(operand->sem_symbol->modifiers & MOD_STATIC) &&
                                    !mg->is_static && mg->class_gen && mg->class_gen->class_sym) {
                                    symbol_t *field_sym = operand->sem_symbol;
                                    symbol_t *field_class = NULL;
                                    symbol_t *search = mg->class_gen->class_sym->data.class_data.superclass;
                                    while (search) {
                                        if (search->data.class_data.members &&
                                            scope_lookup_local(search->data.class_data.members,
                                                               name) == field_sym) {
                                            field_class = search;
                                            break;
                                        }
                                        search = search->data.class_data.superclass;
                                    }
                                    if (field_class && field_class->qualified_name) {
                                        char *class_internal =
                                            class_to_internal_name(field_class->qualified_name);
                                        char *field_desc = type_to_descriptor(field_sym->type);
                                        bool ok = codegen_this_instance_field_incdec(mg, cp,
                                            class_internal, name, field_desc, is_post, is_inc);
                                        free(class_internal);
                                        free(field_desc);
                                        if (ok) {
                                            return true;
                                        }
                                    }
                                }
                                
                                /* Check enclosing class fields for inner/anonymous classes */
                                if (mg->class_gen && mg->class_gen->class_sym && mg->class_gen->is_inner_class) {
                                    symbol_t *class_sym = mg->class_gen->class_sym;
                                    symbol_t *enclosing = class_sym->data.class_data.enclosing_class;
                                    
                                    while (enclosing) {
                                        if (enclosing->data.class_data.members) {
                                            symbol_t *outer_field = scope_lookup_local(
                                                enclosing->data.class_data.members, name);
                                            if (outer_field && outer_field->kind == SYM_FIELD &&
                                                !(outer_field->modifiers & MOD_STATIC)) {
                                                /* Found instance field in enclosing class */
                                                char *outer_internal = class_to_internal_name(enclosing->qualified_name);
                                                char *field_desc = type_to_descriptor(outer_field->type);
                                                
                                                /* Access via this$0, walking the full this$0
                                                 * chain (not just one hop) since 'enclosing' may
                                                 * be two or more levels of anonymous/inner/local
                                                 * class away from the current class. */
                                                /* Stack: [] -> [this] -> ... -> [outer] */
                                                codegen_load_enclosing_this(mg, cp, enclosing);
                                                mg_pop_typed(mg, 1);
                                                mg_push_object(mg, outer_internal);
                                                
                                                /* Now do field++ on the outer instance */
                                                uint16_t fieldref = cp_add_fieldref(mg->cp, outer_internal,
                                                                                     name, field_desc);

                                                /* A long/double outer field is WIDE (2 stack
                                                 * slots): unconditionally using DUP_X1/ICONST_1/
                                                 * IADD here (as this branch used to) silently
                                                 * corrupted the value and DUP_X1 on a wide value
                                                 * is rejected outright by the verifier ("Type
                                                 * long_2nd ... not assignable to category1
                                                 * type") - the same bug already fixed for the
                                                 * sibling "obj.field++" (AST_FIELD_ACCESS) branch
                                                 * just below, never applied here too. Confirmed
                                                 * against gumdrop's own Pop3ProtocolHandler,
                                                 * whose "failedAuthAttempts++" (a long field of
                                                 * the OUTER Pop3ProtocolHandler, incremented from
                                                 * a doubly-nested anonymous callback two this$0
                                                 * hops away) hits exactly this path. */
                                                type_kind_t field_kind = outer_field->type ? outer_field->type->kind : TYPE_INT;
                                                uint8_t const1_op = OP_ICONST_1;
                                                uint8_t add_op = OP_IADD;
                                                uint8_t sub_op = OP_ISUB;
                                                bool is_wide = false;
                                                switch (field_kind) {
                                                    case TYPE_LONG:
                                                        const1_op = OP_LCONST_1; add_op = OP_LADD; sub_op = OP_LSUB;
                                                        is_wide = true;
                                                        break;
                                                    case TYPE_DOUBLE:
                                                        const1_op = OP_DCONST_1; add_op = OP_DADD; sub_op = OP_DSUB;
                                                        is_wide = true;
                                                        break;
                                                    case TYPE_FLOAT:
                                                        const1_op = OP_FCONST_1; add_op = OP_FADD; sub_op = OP_FSUB;
                                                        break;
                                                    default:
                                                        break;
                                                }

                                                if (is_post) {
                                                    /* Post: dup, getfield, dup_x1/dup2_x1, const1, iadd/isub, putfield */
                                                    /* Stack: [outer] -> [outer,outer] -> [outer,old] -> [old,outer,old] -> [old,outer,new] -> [old] */
                                                    bc_emit(mg->code, OP_DUP);
                                                    mg_push_object(mg, outer_internal);  /* [outer,outer] */
                                                    bc_emit(mg->code, OP_GETFIELD);
                                                    bc_emit_u2(mg->code, fieldref);
                                                    mg_pop_typed(mg, 1);
                                                    switch (field_kind) {
                                                        case TYPE_LONG:   mg_push_long(mg); break;
                                                        case TYPE_DOUBLE: mg_push_double(mg); break;
                                                        case TYPE_FLOAT:  mg_push_float(mg); break;
                                                        default:          mg_push_int(mg); break;
                                                    }
                                                    if (is_wide) {
                                                        bc_emit(mg->code, OP_DUP2_X1);
                                                        mg_dup2_x1(mg);  /* [old,outer,old] */
                                                    } else {
                                                        bc_emit(mg->code, OP_DUP_X1);
                                                        mg_dup_x1(mg);   /* [old,outer,old] */
                                                    }
                                                    bc_emit(mg->code, const1_op);
                                                    mg_push(mg, is_wide ? 2 : 1);  /* [old,outer,old,1] */
                                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                                    mg_pop_typed(mg, is_wide ? 2 : 1);  /* [old,outer,new] */
                                                    bc_emit(mg->code, OP_PUTFIELD);
                                                    bc_emit_u2(mg->code, fieldref);
                                                    mg_pop_typed(mg, is_wide ? 3 : 2);  /* [old] */
                                                } else {
                                                    /* Pre: dup, getfield, const1, iadd/isub, dup_x1/dup2_x1, putfield */
                                                    /* Stack: [outer] -> [outer,outer] -> [outer,old] -> [outer,old,1] -> [outer,new] -> [new,outer,new] -> [new] */
                                                    bc_emit(mg->code, OP_DUP);
                                                    mg_push_object(mg, outer_internal);  /* [outer,outer] */
                                                    bc_emit(mg->code, OP_GETFIELD);
                                                    bc_emit_u2(mg->code, fieldref);
                                                    mg_pop_typed(mg, 1);
                                                    switch (field_kind) {
                                                        case TYPE_LONG:   mg_push_long(mg); break;
                                                        case TYPE_DOUBLE: mg_push_double(mg); break;
                                                        case TYPE_FLOAT:  mg_push_float(mg); break;
                                                        default:          mg_push_int(mg); break;
                                                    }
                                                    bc_emit(mg->code, const1_op);
                                                    mg_push(mg, is_wide ? 2 : 1);  /* [outer,old,1] */
                                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                                    mg_pop_typed(mg, is_wide ? 2 : 1);  /* [outer,new] */
                                                    if (is_wide) {
                                                        bc_emit(mg->code, OP_DUP2_X1);
                                                        mg_dup2_x1(mg);  /* [new,outer,new] */
                                                    } else {
                                                        bc_emit(mg->code, OP_DUP_X1);
                                                        mg_dup_x1(mg);   /* [new,outer,new] */
                                                    }
                                                    bc_emit(mg->code, OP_PUTFIELD);
                                                    bc_emit_u2(mg->code, fieldref);
                                                    mg_pop_typed(mg, is_wide ? 3 : 2);  /* [new] */
                                                }

                                                free(outer_internal);
                                                free(field_desc);
                                                return true;
                                            }
                                        }
                                        enclosing = enclosing->data.class_data.enclosing_class;
                                    }
                                }
                                
                                fprintf(stderr, "codegen: cannot resolve variable for increment/decrement: %s\n", name);
                                return false;
                            }
                            
                            /* Handle field access (obj.field++) */
                            if (operand->type == AST_FIELD_ACCESS) {
                                const char *field_name = operand->data.node.name;
                                slist_t *op_children = operand->data.node.children;
                                if (!op_children || !field_name) {
                                    fprintf(stderr, "codegen: invalid field access for increment\n");
                                    return false;
                                }
                                
                                ast_node_t *receiver = (ast_node_t *)op_children->data;

                                /* Get field descriptor and class info from semantic analysis */
                                const char *obj_class = "java/lang/Object";
                                const char *field_desc = "I";  /* Default to int */
                                type_kind_t field_kind = TYPE_INT;

                                /* An explicit "this.field++" receiver is AST_THIS_EXPR,
                                 * whose own sem_type is never cached onto the node itself -
                                 * get_expression_type()'s AST_THIS_EXPR case (semantic.c)
                                 * computes and returns sem->current_class->type on demand
                                 * but never assigns it to expr->sem_type, unlike an
                                 * AST_IDENTIFIER receiver (e.g. "obj.field++"), which does
                                 * get sem_type populated during normal expression type-
                                 * checking. Calling get_expression_type() here as a generic
                                 * fallback doesn't work either: it's a codegen-time call
                                 * into semantic.c, and by then sem->current_class is stale
                                 * (it only tracks "current position" during semantic
                                 * analysis's own, already-finished AST walk) - so use
                                 * mg->class_gen->class_sym->type directly instead, the
                                 * codegen-time equivalent. Without this, the whole block
                                 * below was skipped, leaving the java.lang.Object/int
                                 * placeholders above to reach the classfile as a real
                                 * fieldref: NoSuchFieldError at runtime for ANY
                                 * "this.field++"/"this.field--", widening or not. Confirmed
                                 * against gumdrop's own Quota.incrementMessageCount()'s
                                 * "this.messageCount++". */
                                type_t *receiver_type = receiver->sem_type;
                                if ((!receiver_type || receiver_type->kind != TYPE_CLASS) &&
                                    receiver->type == AST_THIS_EXPR &&
                                    mg->class_gen && mg->class_gen->class_sym) {
                                    receiver_type = mg->class_gen->class_sym->type;
                                } else if ((!receiver_type || receiver_type->kind != TYPE_CLASS) &&
                                           mg->class_gen && mg->class_gen->sem) {
                                    receiver_type = get_expression_type(mg->class_gen->sem, receiver);
                                }

                                if (receiver_type && receiver_type->kind == TYPE_CLASS) {
                                    if (receiver_type->data.class_type.name) {
                                        char *internal = class_to_internal_name(receiver_type->data.class_type.name);
                                        obj_class = internal;
                                    }

                                    /* Look up the field in the class to get its type -
                                     * walking the superclass chain too, since the field
                                     * may be inherited (see lookup_field_with_superclass()). */
                                    symbol_t *class_sym = receiver_type->data.class_type.symbol;
                                    if (class_sym) {
                                        symbol_t *field_sym = lookup_field_with_superclass(class_sym, field_name);
                                        if (field_sym && field_sym->type) {
                                            field_desc = type_to_descriptor(field_sym->type);
                                            field_kind = field_sym->type->kind;
                                        }
                                    }
                                }

                                /* Evaluate receiver to put object reference on stack */
                                if (!codegen_expr(mg, receiver, cp)) {
                                    return false;
                                }

                                uint16_t fieldref = cp_add_fieldref(cp, obj_class, field_name, field_desc);

                                /* Choose the const/add/sub ops for the field's
                                 * actual type - defaulting to the int-family
                                 * ones covers byte/char/short/boolean/int
                                 * alike (all use iconst/iadd/isub), same as
                                 * the array-access version of this code just
                                 * below. A long/double field is also WIDE
                                 * (2 stack slots): unconditionally using
                                 * DUP_X1/ICONST_1/IADD here (as this code
                                 * used to) silently corrupted a long/double
                                 * field's value, and DUP_X1 on a wide value
                                 * is rejected outright by the verifier
                                 * ("Type long_2nd ... not assignable to
                                 * category1 type") - confirmed against
                                 * gumdrop's own SecondaryZoneRefresher.check(),
                                 * whose "state.generation++" on a long field
                                 * hits exactly this path. */
                                uint8_t const1_op = OP_ICONST_1;
                                uint8_t add_op = OP_IADD;
                                uint8_t sub_op = OP_ISUB;
                                bool is_wide = false;
                                switch (field_kind) {
                                    case TYPE_LONG:
                                        const1_op = OP_LCONST_1; add_op = OP_LADD; sub_op = OP_LSUB;
                                        is_wide = true;
                                        break;
                                    case TYPE_DOUBLE:
                                        const1_op = OP_DCONST_1; add_op = OP_DADD; sub_op = OP_DSUB;
                                        is_wide = true;
                                        break;
                                    case TYPE_FLOAT:
                                        const1_op = OP_FCONST_1; add_op = OP_FADD; sub_op = OP_FSUB;
                                        break;
                                    default:
                                        break;
                                }

                                if (is_post) {
                                    /* Post-increment: result is OLD value
                                     * Stack: [] -> [obj] -> [obj,obj] -> [obj,old] -> [old,obj,old] -> [old,obj,old,1] -> [old,obj,new] -> [old] */
                                    bc_emit(mg->code, OP_DUP);
                                    mg_push_object(mg, obj_class);  /* [obj,obj] */
                                    bc_emit(mg->code, OP_GETFIELD);
                                    bc_emit_u2(mg->code, fieldref);
                                    /* getfield: pop ref, push value (1 or 2
                                     * slots): [obj,old] */
                                    mg_pop_typed(mg, 1);
                                    switch (field_kind) {
                                        case TYPE_LONG:   mg_push_long(mg); break;
                                        case TYPE_DOUBLE: mg_push_double(mg); break;
                                        case TYPE_FLOAT:  mg_push_float(mg); break;
                                        default:          mg_push_int(mg); break;
                                    }
                                    if (is_wide) {
                                        bc_emit(mg->code, OP_DUP2_X1);
                                        mg_dup2_x1(mg);  /* [old,obj,old] */
                                    } else {
                                        bc_emit(mg->code, OP_DUP_X1);
                                        mg_dup_x1(mg);   /* [old,obj,old] */
                                    }
                                    bc_emit(mg->code, const1_op);
                                    mg_push(mg, is_wide ? 2 : 1);  /* [old,obj,old,1] */
                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                    mg_pop_typed(mg, is_wide ? 2 : 1);   /* [old,obj,new] */
                                    bc_emit(mg->code, OP_PUTFIELD);
                                    bc_emit_u2(mg->code, fieldref);
                                    mg_pop_typed(mg, is_wide ? 3 : 2);   /* [old] - putfield consumes ref and value, leaving old */
                                } else {
                                    /* Pre: obj -> [obj,obj] -> [obj,old] -> [obj,old,1] -> [obj,new] -> [new,obj,new] -> [new] */
                                    bc_emit(mg->code, OP_DUP);
                                    mg_push_object(mg, obj_class);  /* [obj,obj] */
                                    bc_emit(mg->code, OP_GETFIELD);
                                    bc_emit_u2(mg->code, fieldref);
                                    /* getfield: pop ref, push value (1 or 2
                                     * slots): [obj,old] */
                                    mg_pop_typed(mg, 1);
                                    switch (field_kind) {
                                        case TYPE_LONG:   mg_push_long(mg); break;
                                        case TYPE_DOUBLE: mg_push_double(mg); break;
                                        case TYPE_FLOAT:  mg_push_float(mg); break;
                                        default:          mg_push_int(mg); break;
                                    }
                                    bc_emit(mg->code, const1_op);
                                    mg_push(mg, is_wide ? 2 : 1);  /* [obj,old,1] */
                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                    mg_pop_typed(mg, is_wide ? 2 : 1);   /* [obj,new] */
                                    if (is_wide) {
                                        bc_emit(mg->code, OP_DUP2_X1);
                                        mg_dup2_x1(mg);  /* [new,obj,new] */
                                    } else {
                                        bc_emit(mg->code, OP_DUP_X1);
                                        mg_dup_x1(mg);   /* [new,obj,new] */
                                    }
                                    bc_emit(mg->code, OP_PUTFIELD);
                                    bc_emit_u2(mg->code, fieldref);
                                    mg_pop_typed(mg, is_wide ? 3 : 2);   /* [new] - putfield consumes ref and value */
                                }
                                return true;
                            }
                            
                            /* Handle array access (arr[i]++) */
                            if (operand->type == AST_ARRAY_ACCESS) {
                                slist_t *arr_children = operand->data.node.children;
                                if (!arr_children || !arr_children->next) {
                                    fprintf(stderr, "codegen: malformed array access for increment\n");
                                    return false;
                                }
                                
                                ast_node_t *array_expr = (ast_node_t *)arr_children->data;
                                ast_node_t *index_expr = (ast_node_t *)arr_children->next->data;
                                
                                /* Determine element type from semantic type */
                                type_kind_t elem_kind = TYPE_INT;  /* Default to int */
                                if (operand->sem_type) {
                                    elem_kind = operand->sem_type->kind;
                                } else if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                                    type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                                    if (elem_type) {
                                        elem_kind = elem_type->kind;
                                    }
                                }
                                
                                /* Choose appropriate load/store/add opcodes based on element type */
                                uint8_t load_op, store_op, add_op, sub_op, const1_op;
                                bool is_wide = false;  /* long/double take 2 stack slots */
                                
                                switch (elem_kind) {
                                    case TYPE_LONG:
                                        load_op = OP_LALOAD;
                                        store_op = OP_LASTORE;
                                        add_op = OP_LADD;
                                        sub_op = OP_LSUB;
                                        const1_op = OP_LCONST_1;
                                        is_wide = true;
                                        break;
                                    case TYPE_FLOAT:
                                        load_op = OP_FALOAD;
                                        store_op = OP_FASTORE;
                                        add_op = OP_FADD;
                                        sub_op = OP_FSUB;
                                        const1_op = OP_FCONST_1;
                                        break;
                                    case TYPE_DOUBLE:
                                        load_op = OP_DALOAD;
                                        store_op = OP_DASTORE;
                                        add_op = OP_DADD;
                                        sub_op = OP_DSUB;
                                        const1_op = OP_DCONST_1;
                                        is_wide = true;
                                        break;
                                    case TYPE_BYTE:
                                    case TYPE_BOOLEAN:
                                        load_op = OP_BALOAD;
                                        store_op = OP_BASTORE;
                                        add_op = OP_IADD;
                                        sub_op = OP_ISUB;
                                        const1_op = OP_ICONST_1;
                                        break;
                                    case TYPE_CHAR:
                                        load_op = OP_CALOAD;
                                        store_op = OP_CASTORE;
                                        add_op = OP_IADD;
                                        sub_op = OP_ISUB;
                                        const1_op = OP_ICONST_1;
                                        break;
                                    case TYPE_SHORT:
                                        load_op = OP_SALOAD;
                                        store_op = OP_SASTORE;
                                        add_op = OP_IADD;
                                        sub_op = OP_ISUB;
                                        const1_op = OP_ICONST_1;
                                        break;
                                    default:  /* TYPE_INT and others */
                                        load_op = OP_IALOAD;
                                        store_op = OP_IASTORE;
                                        add_op = OP_IADD;
                                        sub_op = OP_ISUB;
                                        const1_op = OP_ICONST_1;
                                        break;
                                }
                                
                                /* Generate array ref */
                                if (!codegen_expr(mg, array_expr, cp)) {
                                    return false;
                                }
                                
                                /* Generate index */
                                if (!codegen_expr(mg, index_expr, cp)) {
                                    return false;
                                }
                                
                                if (is_post) {
                                    /* Post-increment: arr[i]++ - result is OLD value
                                     * Stack: [arr, i] -> [arr, i, arr, i] -> [arr, i, old] ->
                                     *        [old, arr, i, old] -> [old, arr, i, old, 1] ->
                                     *        [old, arr, i, new] -> [old] */
                                    bc_emit(mg->code, OP_DUP2);
                                    mg_push(mg, 2);  /* [arr, i, arr, i] */
                                    bc_emit(mg->code, load_op);
                                    /* load_op consumes arr+i (2 words) and pushes
                                     * the element value - for a WIDE element
                                     * (long/double, 2 words) that's a net
                                     * ZERO change in tracked stack depth, not
                                     * -1: unconditionally popping 1 here
                                     * (correct only for a narrow, 1-word
                                     * element) undercounted the wide case by
                                     * one word, propagating through every
                                     * later push/pop in this same sequence
                                     * and ultimately computing max_stack one
                                     * word too small for the whole method
                                     * (VerifyError: "Operand stack overflow",
                                     * confirmed against gumdrop's own
                                     * DoubleHistogram.HistogramBuckets.record(),
                                     * whose "counts[bucket]++" on a long[]
                                     * hits exactly this path). */
                                    mg_pop_typed(mg, is_wide ? 0 : 1);  /* [arr, i, old] - load consumes arr,i pushes value */
                                    if (is_wide) {
                                        bc_emit(mg->code, OP_DUP2_X2);
                                        mg_push(mg, 2);  /* [old, arr, i, old] for long/double */
                                    } else {
                                        bc_emit(mg->code, OP_DUP_X2);
                                        mg_push(mg, 1);  /* [old, arr, i, old] */
                                    }
                                    bc_emit(mg->code, const1_op);
                                    mg_push(mg, is_wide ? 2 : 1);  /* [old, arr, i, old, 1] */
                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                    mg_pop_typed(mg, is_wide ? 2 : 1);  /* [old, arr, i, new] */
                                    bc_emit(mg->code, store_op);
                                    mg_pop_typed(mg, is_wide ? 4 : 3);  /* [old] - store consumes arr,i,value */
                                } else {
                                    /* Pre-increment: ++arr[i] - result is NEW value
                                     * Stack: [arr, i] -> [arr, i, arr, i] -> [arr, i, old] ->
                                     *        [arr, i, old, 1] -> [arr, i, new] ->
                                     *        [new, arr, i, new] -> [new] */
                                    bc_emit(mg->code, OP_DUP2);
                                    mg_push(mg, 2);  /* [arr, i, arr, i] */
                                    bc_emit(mg->code, load_op);
                                    /* See the identical wide/narrow net-change
                                     * distinction (and its full reasoning) in
                                     * the post-increment branch above. */
                                    mg_pop_typed(mg, is_wide ? 0 : 1);  /* [arr, i, old] */
                                    bc_emit(mg->code, const1_op);
                                    mg_push(mg, is_wide ? 2 : 1);  /* [arr, i, old, 1] */
                                    bc_emit(mg->code, is_inc ? add_op : sub_op);
                                    mg_pop_typed(mg, is_wide ? 2 : 1);  /* [arr, i, new] */
                                    if (is_wide) {
                                        bc_emit(mg->code, OP_DUP2_X2);
                                        mg_push(mg, 2);  /* [new, arr, i, new] for long/double */
                                    } else {
                                        bc_emit(mg->code, OP_DUP_X2);
                                        mg_push(mg, 1);  /* [new, arr, i, new] */
                                    }
                                    bc_emit(mg->code, store_op);
                                    mg_pop_typed(mg, is_wide ? 4 : 3);  /* [new] */
                                }
                                return true;
                            }
                            
                            fprintf(stderr, "codegen: increment/decrement on non-identifier/non-field/non-array not yet implemented\n");
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
            /* Push 'this' with actual class type for stackmap */
            if (mg->class_gen && mg->class_gen->internal_name) {
                mg_push_object(mg, mg->class_gen->internal_name);
            } else {
                mg_push_null(mg);
            }
            return true;
        
        case AST_SUPER_EXPR:
            /* 'super' is also 'this' - just a marker for invokespecial to superclass */
            bc_emit(mg->code, OP_ALOAD_0);
            /* Push 'this' with actual class type for stackmap */
            if (mg->class_gen && mg->class_gen->internal_name) {
                mg_push_object(mg, mg->class_gen->internal_name);
            } else {
                mg_push_null(mg);
            }
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
                const char *elem_class = NULL;      /* class name, when elem_kind == TYPE_CLASS */
                int remaining_dims = 0;             /* dims left, when is_multi_dim_intermediate */

                if (array_expr->sem_type && array_expr->sem_type->kind == TYPE_ARRAY) {
                    type_t *elem_type = array_expr->sem_type->data.array_type.element_type;
                    if (elem_type) {
                        elem_kind = elem_type->kind;
                        if (elem_kind == TYPE_CLASS) {
                            elem_class = elem_type->data.class_type.name;
                        } else if (elem_kind != TYPE_BOOLEAN && elem_kind != TYPE_BYTE &&
                                   elem_kind != TYPE_CHAR && elem_kind != TYPE_SHORT &&
                                   elem_kind != TYPE_INT && elem_kind != TYPE_LONG &&
                                   elem_kind != TYPE_FLOAT && elem_kind != TYPE_DOUBLE &&
                                   elem_kind != TYPE_ARRAY) {
                            /* A type variable or wildcard element (the T[] a generic
                             * method returns): a reference, erased to its bound or
                             * Object - never the int the default would load. */
                            type_t *bound = (elem_kind == TYPE_TYPEVAR) ? elem_type->data.type_var.bound : NULL;
                            elem_kind = TYPE_CLASS;
                            elem_class = (bound && bound->kind == TYPE_CLASS) ?
                                bound->data.class_type.name : NULL;
                        }
                    }
                    /* Check if this is a multi-dim array with more dimensions */
                    if (array_expr->sem_type->data.array_type.dimensions > 1) {
                        is_multi_dim_intermediate = true;
                        remaining_dims = array_expr->sem_type->data.array_type.dimensions - 1;
                    }
                } else if (array_expr->type == AST_IDENTIFIER) {
                    /* Look up local variable's array info */
                    const char *arr_name = array_expr->data.leaf.name;
                    if (mg_local_is_array(mg, arr_name)) {
                        int dims = mg_local_array_dims(mg, arr_name);
                        if (dims > 1) {
                            /* This access yields another array */
                            is_multi_dim_intermediate = true;
                            remaining_dims = dims - 1;
                        } else {
                            elem_kind = mg_local_array_elem_kind(mg, arr_name);
                        }
                        if (elem_kind == TYPE_CLASS || is_multi_dim_intermediate) {
                            elem_class = mg_local_array_elem_class(mg, arr_name);
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
                                remaining_dims = dims - depth;
                            } else {
                                elem_kind = mg_local_array_elem_kind(mg, arr_name);
                            }
                            if (elem_kind == TYPE_CLASS || is_multi_dim_intermediate) {
                                elem_class = mg_local_array_elem_class(mg, arr_name);
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
                            break;
                        case TYPE_FLOAT:
                            bc_emit(mg->code, OP_FALOAD);
                            break;
                        case TYPE_DOUBLE:
                            bc_emit(mg->code, OP_DALOAD);
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

                /* Stack: arrayref, index -> value. Both operands (a single
                 * category-1 slot each) must be popped from the tracked
                 * stackmap, then the actual LOADED value's type pushed in
                 * their place - not just the raw slot count adjusted, or
                 * the array's own (now-stale) reference type is left
                 * masquerading as the loaded element's type, which the
                 * verifier rejects the moment anything but a reference
                 * type (where the mismatch usually happens to still be
                 * assignable) is actually loaded. */
                mg_pop_typed(mg, 2);
                if (is_multi_dim_intermediate) {
                    char *sub_array_desc = build_array_descriptor(elem_kind, elem_class, remaining_dims);
                    mg_push_object_from_descriptor(mg, sub_array_desc);
                    free(sub_array_desc);
                } else {
                    switch (elem_kind) {
                        case TYPE_LONG:   mg_push_long(mg); break;
                        case TYPE_FLOAT:  mg_push_float(mg); break;
                        case TYPE_DOUBLE: mg_push_double(mg); break;
                        case TYPE_CLASS:
                            {
                                char *internal = class_to_internal_name(elem_class ? elem_class : "java.lang.Object");
                                mg_push_object(mg, internal);
                                free(internal);
                            }
                            break;
                        case TYPE_ARRAY:
                            /* Nested array element type without a tracked
                             * dimension count (rare) - fall back to a bare
                             * Object reference, matching the existing
                             * "shouldn't happen" fallback elsewhere. */
                            mg_push_object(mg, "java/lang/Object");
                            break;
                        default:
                            mg_push_int(mg);
                            break;
                    }
                }
                return true;
            }
        
        case AST_FIELD_ACCESS:
            if (!codegen_field_access(mg, expr, cp)) {
                return false;
            }
            checkcast_generic_field(mg, cp, expr);
            return true;
        
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
                char *class_ref = NULL;
                bool needs_free = false;
                
                if (type_node->type == AST_CLASS_TYPE) {
                    /* Prefer the semantically resolved (and properly
                     * package-qualified) type, the same way AST_CAST_EXPR
                     * does below - the bare AST source name is only right
                     * for an already-qualified or java.lang name; a
                     * same-package type (needing no import, e.g. a
                     * self-referential "p instanceof Thing" inside Thing
                     * itself) has no qualification in the source at all. */
                    const char *class_name = NULL;
                    if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                        if (type_node->sem_type->data.class_type.symbol &&
                            type_node->sem_type->data.class_type.symbol->qualified_name) {
                            class_name = type_node->sem_type->data.class_type.symbol->qualified_name;
                        } else if (type_node->sem_type->data.class_type.name) {
                            class_name = type_node->sem_type->data.class_type.name;
                        }
                    }
                    if (!class_name) {
                        class_name = type_node->data.node.name;
                    }
                    const char *resolved = resolve_java_lang_class(class_name);
                    class_ref = class_to_internal_name(resolved);
                    needs_free = true;
                } else if (type_node->type == AST_ARRAY_TYPE) {
                    /* Array type: byte[], String[][], etc.
                     * Use the array descriptor as class name (e.g., "[B", "[[Ljava/lang/String;") */
                    class_ref = ast_type_to_descriptor(type_node);
                    needs_free = true;
                } else if (type_node->type == AST_PRIMITIVE_TYPE) {
                    /* instanceof doesn't work with primitives */
                    fprintf(stderr, "codegen: instanceof not supported for primitive types\n");
                    return false;
                }
                
                if (!class_ref) {
                    fprintf(stderr, "codegen: instanceof without class type\n");
                    return false;
                }
                
                uint16_t class_index = cp_add_class(cp, class_ref);
                if (needs_free) {
                    free(class_ref);
                }
                
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
                char *target_class_owned = NULL;  /* non-NULL only for the AST_ARRAY_TYPE branch's heap-allocated descriptor - freed after use */
                
                if (type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = type_node->data.leaf.name;
                    if (strcmp(prim_name, "int") == 0) {
                        target_kind = TYPE_INT;
                    } else if (strcmp(prim_name, "long") == 0) {
                        target_kind = TYPE_LONG;
                    } else if (strcmp(prim_name, "float") == 0) {
                        target_kind = TYPE_FLOAT;
                    } else if (strcmp(prim_name, "double") == 0) {
                        target_kind = TYPE_DOUBLE;
                    } else if (strcmp(prim_name, "byte") == 0) {
                        target_kind = TYPE_BYTE;
                    } else if (strcmp(prim_name, "short") == 0) {
                        target_kind = TYPE_SHORT;
                    } else if (strcmp(prim_name, "char") == 0) {
                        target_kind = TYPE_CHAR;
                    } else if (strcmp(prim_name, "boolean") == 0) {
                        target_kind = TYPE_BOOLEAN;
                    }
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
                    } else if (type_node->sem_type && type_node->sem_type->kind == TYPE_TYPEVAR) {
                        /* Casting to a TYPE VARIABLE (e.g. "(A) expr" inside
                         * "<A extends BasicFileAttributes> A m(...)") must
                         * erase to the type variable's bound (or
                         * java.lang.Object if unbounded), exactly like every
                         * other type-variable erasure site in this codebase
                         * (type_to_descriptor()'s own TYPE_TYPEVAR case,
                         * used for method/field descriptors) - not fall
                         * through to the AST-name fallback below, which
                         * literally uses the type PARAMETER's own name
                         * ("A") as if it were a real class. That produced
                         * "checkcast A", a class that doesn't exist, and
                         * the JVM's own class-loading for it failed with
                         * NoClassDefFoundError/ClassNotFoundException the
                         * moment the method actually ran - confirmed
                         * against gumdrop's own MemoryFileSystemProvider.
                         * readAttributes() ("<A extends BasicFileAttributes>
                         * A readAttributes(...) { ...; return (A) new
                         * MemoryFileAttributes(...); }"). */
                        type_t *bound = type_node->sem_type->data.type_var.bound;
                        if (bound && bound->kind == TYPE_CLASS) {
                            target_class = bound->data.class_type.symbol &&
                                bound->data.class_type.symbol->qualified_name ?
                                bound->data.class_type.symbol->qualified_name :
                                bound->data.class_type.name;
                        }
                        if (!target_class) {
                            target_class = "java.lang.Object";
                        }
                    } else {
                        target_class = type_node->data.node.name;
                    }
                } else if (type_node->type == AST_ARRAY_TYPE) {
                    target_kind = TYPE_ARRAY;
                    /* Resolve (forcing it if some earlier pass hasn't
                     * already) to a proper type_t with real dimensions/
                     * element type, then build the full array descriptor
                     * ("[B", "[[I", "[Ljava/lang/String;", ...) from it -
                     * target_class doubles as "the checkcast constant's
                     * name" below, and for an array type that name IS
                     * the full descriptor (JVMS 4.4.1: a CONSTANT_Class
                     * name can be either a binary class name or an array
                     * descriptor) - not an unwrapped internal name like a
                     * plain class target uses. Without this, a cast to
                     * an array type (e.g. `(byte[]) obj`) emitted NO
                     * checkcast at all (target_class stayed NULL, so the
                     * "if (target_class)" guard below skipped emitting
                     * anything) - silently leaving the operand's
                     * pre-cast type on the stack, rejected the moment
                     * the result was used somewhere requiring the real,
                     * narrower array type. */
                    type_t *array_type = type_node->sem_type;
                    if ((!array_type || array_type->kind != TYPE_ARRAY) &&
                        mg->class_gen && mg->class_gen->sem) {
                        array_type = semantic_resolve_type(mg->class_gen->sem, type_node);
                    }
                    if (array_type && array_type->kind == TYPE_ARRAY) {
                        target_class_owned = type_to_descriptor(array_type);
                        target_class = target_class_owned;
                    }
                }
                
                /* Generate the operand expression */
                if (!codegen_expr(mg, operand, cp)) {
                    return false;
                }
                
                /* Determine source type from operand's semantic type. Not
                 * always already populated at this point - e.g. as an
                 * array dimension expression, "(int) ((totalBits + 7) /
                 * 8)" in "new byte[(int) ((totalBits + 7) / 8)]", nothing
                 * else visits the cast's OWN operand with the general
                 * expression type-checker first. Without this, source_kind
                 * fell through to TYPE_UNKNOWN -> defaulted to TYPE_INT
                 * below (assuming an unknown source is already an int) -
                 * which, for a genuinely LONG-typed operand cast to int,
                 * made source_kind == target_kind look like a no-op cast,
                 * silently DROPPING the cast entirely (no L2I emitted) and
                 * leaving a long (2 stack words) where newarray's size
                 * operand needs a single-word int: VerifyError "Bad type
                 * on operand stack ... long_2nd ... not assignable to
                 * integer". Confirmed against gumdrop's own
                 * Huffman.encode(). */
                type_kind_t source_kind = TYPE_UNKNOWN;
                if (operand->sem_type) {
                    source_kind = operand->sem_type->kind;
                } else if (mg->class_gen && mg->class_gen->sem) {
                    type_t *forced = get_expression_type(mg->class_gen->sem, operand);
                    if (forced) {
                        source_kind = forced->kind;
                    }
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
                    free(target_class_owned);
                    return true;
                }
                
                /* A reference-typed operand (e.g. Object, as returned by
                 * java.lang.reflect.Method.invoke()) cast to a primitive
                 * type is an UNBOXING cast (JLS 5.5): checkcast to the
                 * target's own wrapper class, then unbox - never a
                 * primitive-to-primitive conversion opcode, which the
                 * switch below (keyed on a PRIMITIVE source_kind) has no
                 * case for at all. A boxed/reference source silently fell
                 * through with NO conversion emitted at all, leaving the
                 * raw reference on the stack: VerifyError "Bad type on
                 * operand stack ... Object ... is not assignable to
                 * integer". Confirmed against gumdrop's own
                 * H3ClientStreamTest.testExtractStatusReturns200(): "int
                 * result = (int) m.invoke(null, ...);". */
                if (source_kind == TYPE_CLASS) {
                    const char *wrapper_class;
                    switch (target_kind) {
                        case TYPE_INT: wrapper_class = "java/lang/Integer"; break;
                        case TYPE_LONG: wrapper_class = "java/lang/Long"; break;
                        case TYPE_DOUBLE: wrapper_class = "java/lang/Double"; break;
                        case TYPE_FLOAT: wrapper_class = "java/lang/Float"; break;
                        case TYPE_BYTE: wrapper_class = "java/lang/Byte"; break;
                        case TYPE_SHORT: wrapper_class = "java/lang/Short"; break;
                        case TYPE_CHAR: wrapper_class = "java/lang/Character"; break;
                        case TYPE_BOOLEAN: wrapper_class = "java/lang/Boolean"; break;
                        default: wrapper_class = NULL; break;
                    }
                    if (wrapper_class) {
                        uint16_t class_index = cp_add_class(cp, wrapper_class);
                        bc_emit(mg->code, OP_CHECKCAST);
                        bc_emit_u2(mg->code, class_index);
                        emit_unboxing(mg, cp, target_kind, wrapper_class);
                    }
                    free(target_class_owned);
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
                
                /* Emit conversion opcodes. Every branch below pops the
                 * source value's own stackmap entry/entries (by WORD
                 * count: 1 for int/float, 2 for long/double) and pushes a
                 * freshly, correctly-typed replacement via the type-aware
                 * mg_push_*() helpers - never a raw mg_push()/mg_pop() or
                 * a bare mg_pop_typed() left unpaired. Two distinct gaps
                 * existed here before: (1) a conversion between two
                 * category-2 (wide) types or two category-1 types with
                 * the SAME word count either side (I2F, L2D, F2I, D2L)
                 * did nothing at all, leaving the SOURCE type's own stale
                 * tag on mg->stackmap even though the real runtime value
                 * had changed type - invisible for straight-line code,
                 * but baked into any StackMapTable frame recorded while
                 * that value is still on the stack (e.g. a ternary branch
                 * evaluating "(long) (doubleExpr)"): VerifyError
                 * "Inconsistent stackmap frames ... Type long ... is not
                 * assignable to double" (or the reverse). Confirmed
                 * against gumdrop's own
                 * MdnsCache.scheduleNextRefreshStage()'s ternary,
                 * "stage == 0 ? (long) (...) : (long) (...)". (2) a
                 * conversion INTO or OUT OF a wide type via a bare
                 * mg_push(mg,1)/mg_pop_typed(mg,1) adjusted the RAW WORD
                 * COUNT correctly but, for the wide side, only
                 * touched/removed ONE of that type's two required
                 * stackmap entries - e.g. L2I's own mg_pop_typed(mg,1)
                 * discarded only the long's trailing TOP placeholder,
                 * leaving the actual VT_LONG entry itself behind
                 * (mis-tagged) as the new top-of-stack instead of a fresh
                 * VT_INTEGER. Byte/short/char are deliberately folded
                 * into the "integer" verification type throughout (JVMS
                 * 4.10.1.2: the JVM operand stack has no separate
                 * byte/short/char type) - only int/long/float/double ever
                 * need a distinct mg_push_*() call here. */
                switch (source_kind) {
                    case TYPE_INT:
                    case TYPE_BYTE:
                    case TYPE_SHORT:
                    case TYPE_CHAR:
                    case TYPE_BOOLEAN:
                        switch (target_kind) {
                            case TYPE_LONG:   bc_emit(mg->code, OP_I2L); mg_pop_typed(mg, 1); mg_push_long(mg); break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_I2F); mg_pop_typed(mg, 1); mg_push_float(mg); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_I2D); mg_pop_typed(mg, 1); mg_push_double(mg); break;
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
                                mg_pop_typed(mg, 2);
                                mg_push_int(mg);
                                if (target_kind == TYPE_BYTE) {
                                    bc_emit(mg->code, OP_I2B);
                                } else if (target_kind == TYPE_SHORT) {
                                    bc_emit(mg->code, OP_I2S);
                                } else if (target_kind == TYPE_CHAR) {
                                    bc_emit(mg->code, OP_I2C);
                                }
                                break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_L2F); mg_pop_typed(mg, 2); mg_push_float(mg); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_L2D); mg_pop_typed(mg, 2); mg_push_double(mg); break;
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
                                mg_pop_typed(mg, 1);
                                mg_push_int(mg);
                                if (target_kind == TYPE_BYTE) {
                                    bc_emit(mg->code, OP_I2B);
                                } else if (target_kind == TYPE_SHORT) {
                                    bc_emit(mg->code, OP_I2S);
                                } else if (target_kind == TYPE_CHAR) {
                                    bc_emit(mg->code, OP_I2C);
                                }
                                break;
                            case TYPE_LONG:   bc_emit(mg->code, OP_F2L); mg_pop_typed(mg, 1); mg_push_long(mg); break;
                            case TYPE_DOUBLE: bc_emit(mg->code, OP_F2D); mg_pop_typed(mg, 1); mg_push_double(mg); break;
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
                                mg_pop_typed(mg, 2);
                                mg_push_int(mg);
                                if (target_kind == TYPE_BYTE) {
                                    bc_emit(mg->code, OP_I2B);
                                } else if (target_kind == TYPE_SHORT) {
                                    bc_emit(mg->code, OP_I2S);
                                } else if (target_kind == TYPE_CHAR) {
                                    bc_emit(mg->code, OP_I2C);
                                }
                                break;
                            case TYPE_LONG:   bc_emit(mg->code, OP_D2L); mg_pop_typed(mg, 2); mg_push_long(mg); break;
                            case TYPE_FLOAT:  bc_emit(mg->code, OP_D2F); mg_pop_typed(mg, 2); mg_push_float(mg); break;
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

                /* Auto-unbox Boolean to boolean for ternary condition,
                 * mirroring AST_IF_STMT's own identical fix - a boxed
                 * Boolean condition (e.g. "(Boolean) value ? 1 : 0") left
                 * a java/lang/Boolean reference on the stack right where
                 * the ifeq below requires an int, failing verification. */
                if (condition->sem_type && condition->sem_type->kind == TYPE_CLASS &&
                    condition->sem_type->data.class_type.name &&
                    strcmp(condition->sem_type->data.class_type.name, "java.lang.Boolean") == 0) {
                    uint16_t unbox_ref = cp_add_methodref(cp,
                        "java/lang/Boolean", "booleanValue", "()Z");
                    bc_emit(mg->code, OP_INVOKEVIRTUAL);
                    bc_emit_u2(mg->code, unbox_ref);
                    /* Stack stays same size (Boolean -> int) */
                }

                /* ifeq else_branch (jump if condition is false/0) */
                size_t ifeq_pos = mg->code->length;
                bc_emit(mg->code, OP_IFEQ);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);  /* Condition consumed */
                
                /* Save stackmap state AFTER consuming condition.
                 * This preserves the full type information of items on the stack. */
                stackmap_state_t *saved_state = NULL;
                if (mg->stackmap) {
                    saved_state = stackmap_save_state(mg->stackmap);
                }
                int saved_stack_depth = mg->stack_depth;

                /* Generate then branch */
                if (!codegen_expr(mg, then_expr, cp)) {
                    stackmap_state_free(saved_state);
                    return false;
                }

                /* Numeric promotion (JLS 15.25): "cond ? someInt : someLong" must
                 * leave the same type on both paths, or the two branches disagree
                 * on stack depth/shape at the merge point below. A no-op for a
                 * reference-typed ternary. */
                if (expr->sem_type) {
                    type_kind_t then_kind;
                    const char *then_class;
                    value_kind_and_class(mg, then_expr, &then_kind, &then_class);
                    coerce_stack_value(mg, cp, then_kind, then_class,
                                       expr->sem_type->kind,
                                       expr->sem_type->kind == TYPE_CLASS ?
                                           expr->sem_type->data.class_type.name : NULL);
                }

                /* goto end (skip else branch) */
                size_t goto_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                
                /* Patch ifeq to jump here (else branch) */
                uint16_t else_offset = (uint16_t)(mg->code->length - ifeq_pos);
                bc_patch_u2(mg->code, ifeq_pos + 1, else_offset);
                
                /* Restore stackmap state for else branch (same state as after condition consumed) */
                if (saved_state && mg->stackmap) {
                    stackmap_restore_state(mg->stackmap, saved_state);
                }
                mg->stack_depth = saved_stack_depth;
                
                /* Record frame at else branch target */
                mg_record_frame(mg);
                
                /* Generate else branch */
                if (!codegen_expr(mg, else_expr, cp)) {
                    stackmap_state_free(saved_state);
                    return false;
                }

                /* Same promotion as the then branch, above */
                if (expr->sem_type) {
                    type_kind_t else_kind;
                    const char *else_class;
                    value_kind_and_class(mg, else_expr, &else_kind, &else_class);
                    coerce_stack_value(mg, cp, else_kind, else_class,
                                       expr->sem_type->kind,
                                       expr->sem_type->kind == TYPE_CLASS ?
                                           expr->sem_type->data.class_type.name : NULL);
                }

                /* Patch goto to jump here (end) */
                uint16_t end_offset = (uint16_t)(mg->code->length - goto_pos);
                bc_patch_u2(mg->code, goto_pos + 1, end_offset);

                /* mg->stackmap's tracked stack-top type at this point just
                 * reflects whichever branch was generated last (the else
                 * branch) - not a real merge of both incoming edges. The
                 * then branch's goto also reaches this exact merge point,
                 * possibly with a different reference type on the stack
                 * (e.g. a concrete class vs. a bare `null` literal in the
                 * other branch) - recording the frame from just one side
                 * left the *other* side's actual value unassignable to the
                 * declared frame. JLS 15.25 already computed the ternary's
                 * own overall type as the correct join of both branches
                 * (expr->sem_type); use that directly for the frame instead,
                 * mirroring the same correction codegen_identifier() makes
                 * for a reference-typed local's tracked type.
                 *
                 * A ternary whose own type is a bare type variable (e.g. a
                 * generic method's "return local != null ? local : fallback;"
                 * where both operands have type T) previously fell through
                 * this gate entirely (TYPE_TYPEVAR was never included),
                 * leaving the else branch's own possibly-wrong tracked type
                 * in place uncorrected - confirmed against gumdrop's own
                 * TlsConfig.coalesce(T, T), where the merge frame ended up
                 * recorded as the null type instead of java/lang/Object
                 * (VerifyError: "Inconsistent stackmap frames"/"Type
                 * java/lang/Object ... is not assignable to null").
                 * type_to_descriptor() already erases TYPE_TYPEVAR to its
                 * bound (or Object if unbounded), same as every other
                 * TYPE_TYPEVAR erasure site in this codebase, so including
                 * it here needs no special-casing beyond the type check. */
                if (mg->stackmap && expr->sem_type &&
                    (expr->sem_type->kind == TYPE_CLASS || expr->sem_type->kind == TYPE_ARRAY ||
                     expr->sem_type->kind == TYPE_TYPEVAR)) {
                    char *merged_desc = type_to_descriptor(expr->sem_type);
                    if (merged_desc) {
                        stackmap_pop(mg->stackmap, 1);
                        mg_push_object_from_descriptor(mg, merged_desc);
                        mg->stack_depth--;  /* mg_push_object_from_descriptor increments, but we already pushed */
                        free(merged_desc);
                    }
                }

                /* Record frame at merge point */
                mg_record_frame(mg);
                
                stackmap_state_free(saved_state);
                
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
                        if (has_type_patterns) {
                            break;
                        }
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
                 * If captures this: instance method, this is implicit
                 * If captures locals (either case): add captured vars as params
                 * SAM params always added at the end */
                string_t *impl_desc = string_new("(");
                
                /* Add captured variable types to descriptor.
                 * Even for instance methods (captures_this), we need to add
                 * captured local variables as explicit parameters. */
                for (slist_t *cap = captures; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (var_sym && var_sym->type) {
                        char *cap_desc = type_to_descriptor(var_sym->type);
                        string_append(impl_desc, cap_desc);
                        free(cap_desc);
                    }
                }
                
                /* Add SAM parameters to implementation method.
                 * Use the lambda's actual parameter types (from sem_type) which have
                 * type arguments substituted, not the SAM's type variable types. */
                slist_t *sam_params = sam->data.method_data.parameters;
                slist_t *children = expr->data.node.children;
                ast_node_t *params_node = children ? (ast_node_t *)children->data : NULL;
                
                /* Build a list of actual parameter types from the lambda params */
                type_t *actual_param_types[16];
                int actual_param_count = 0;
                
                if (params_node) {
                    if (params_node->type == AST_IDENTIFIER) {
                        /* Single parameter - use its sem_type */
                        if (params_node->sem_type && actual_param_count < 16) {
                            actual_param_types[actual_param_count++] = params_node->sem_type;
                        }
                    } else if (params_node->type == AST_BINARY_EXPR) {
                        /* Multiple comma-separated parameters */
                        ast_node_t *stack[32];
                        int stack_top = 0;
                        stack[stack_top++] = params_node;
                        
                        while (stack_top > 0) {
                            ast_node_t *node = stack[--stack_top];
                            if (!node) {
                                continue;
                            }
                            
                            if (node->type == AST_IDENTIFIER) {
                                if (node->sem_type && actual_param_count < 16) {
                                    actual_param_types[actual_param_count++] = node->sem_type;
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
                    } else if (params_node->type == AST_PARENTHESIZED) {
                        slist_t *inner_children = params_node->data.node.children;
                        if (inner_children) {
                            ast_node_t *inner = (ast_node_t *)inner_children->data;
                            if (inner && inner->type == AST_IDENTIFIER && inner->sem_type) {
                                actual_param_types[actual_param_count++] = inner->sem_type;
                            } else if (inner && inner->type == AST_BINARY_EXPR) {
                                /* Multiple params in parens */
                                ast_node_t *stack[32];
                                int stack_top = 0;
                                stack[stack_top++] = inner;
                                
                                while (stack_top > 0) {
                                    ast_node_t *node = stack[--stack_top];
                                    if (!node) {
                                        continue;
                                    }
                                    
                                    if (node->type == AST_IDENTIFIER && node->sem_type) {
                                        if (actual_param_count < 16) {
                                            actual_param_types[actual_param_count++] = node->sem_type;
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
                            }
                        }
                    } else if (params_node->type == AST_LAMBDA_PARAMS) {
                        /* Typed lambda params - use their sem_type */
                        for (slist_t *node = params_node->data.node.children; node; node = node->next) {
                            ast_node_t *param = (ast_node_t *)node->data;
                            if (param && param->sem_type && actual_param_count < 16) {
                                actual_param_types[actual_param_count++] = param->sem_type;
                            }
                        }
                    }
                }
                
                /* Use actual param types if available, otherwise fall back to SAM types */
                if (actual_param_count > 0) {
                    for (int i = 0; i < actual_param_count; i++) {
                        char *param_desc = type_to_descriptor(actual_param_types[i]);
                        string_append(impl_desc, param_desc);
                        free(param_desc);
                    }
                } else {
                    /* Fall back to SAM param types */
                for (slist_t *p = sam_params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type) {
                        char *param_desc = type_to_descriptor(param->type);
                        string_append(impl_desc, param_desc);
                        free(param_desc);
                        }
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
                
                /* Allocate slots for captured variables.
                 * Even for instance methods (captures_this), we need slots for
                 * captured local variables which come as explicit parameters. */
                for (slist_t *cap = captures; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (var_sym && var_sym->name && var_sym->type) {
                        type_kind_t cap_kind = var_sym->type->kind;
                        /* Type variables erase to Object (reference type) */
                        if (cap_kind == TYPE_TYPEVAR) {
                            cap_kind = TYPE_CLASS;
                        }
                        local_var_info_t *info = local_var_info_new(slot, cap_kind);
                        info->is_ref = (cap_kind == TYPE_CLASS || cap_kind == TYPE_ARRAY);
                        hashtable_insert(lambda_mg->locals, var_sym->name, info);
                        
                        int size = (var_sym->type->kind == TYPE_LONG || 
                                   var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                        slot += size;
                    }
                }
                
                /* Allocate slots for SAM parameters */
                /* Note: children and params_node already defined above */
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
                    if (!is_unnamed_name(params_node->data.leaf.name)) {
                        hashtable_insert(lambda_mg->locals, params_node->data.leaf.name, info);
                    }
                    
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
                        if (!node) {
                            continue;
                        }
                        
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
                        /* Type variables erase to Object (reference type) */
                        if (kind == TYPE_TYPEVAR || kind == TYPE_WILDCARD || kind == TYPE_UNKNOWN) {
                            kind = TYPE_CLASS;
                        }
                        local_var_info_t *info = local_var_info_new(slot, kind);
                        info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                        if (!is_unnamed_name(param_id->data.leaf.name)) {
                            hashtable_insert(lambda_mg->locals, param_id->data.leaf.name, info);
                        }
                        
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
                            if (!is_unnamed_name(inner->data.leaf.name)) {
                                hashtable_insert(lambda_mg->locals, inner->data.leaf.name, info);
                            }
                            
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
                                if (!node) {
                                    continue;
                                }
                                
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
                                /* Type variables erase to Object (reference type) */
                                if (kind == TYPE_TYPEVAR || kind == TYPE_WILDCARD || kind == TYPE_UNKNOWN) {
                                    kind = TYPE_CLASS;
                                }
                                local_var_info_t *info = local_var_info_new(slot, kind);
                                info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                                if (!is_unnamed_name(param_id->data.leaf.name)) {
                                    hashtable_insert(lambda_mg->locals, param_id->data.leaf.name, info);
                                }
                                
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
                        if (param->type != AST_PARAMETER) {
                            continue;
                        }
                        
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
                        
                        /* Type variables erase to Object (reference type) */
                        if (kind == TYPE_TYPEVAR || kind == TYPE_WILDCARD || kind == TYPE_UNKNOWN) {
                            kind = TYPE_CLASS;
                        }
                        
                        local_var_info_t *info = local_var_info_new(slot, kind);
                        info->is_ref = (kind == TYPE_CLASS || kind == TYPE_ARRAY);
                        if (!is_unnamed_name(param_name)) {
                            hashtable_insert(lambda_mg->locals, param_name, info);
                        }
                        
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
                        
                        /* Get body expression type for boxing decision.
                         * If sem_type is not set on body, compute it from the expression. */
                        type_t *body_type = body->sem_type;
                        if (!body_type && mg->class_gen && mg->class_gen->sem) {
                            body_type = get_expression_type(mg->class_gen->sem, body);
                        }
                        
                        /* Emit appropriate return instruction.
                         * If SAM return type is Object/typevar but body is primitive,
                         * we need to box the primitive before returning. */
                        if (sam->type) {
                            bool sam_is_reference = (sam->type->kind == TYPE_CLASS || 
                                                     sam->type->kind == TYPE_TYPEVAR ||
                                                     sam->type->kind == TYPE_ARRAY);
                            bool body_is_primitive = (body_type && 
                                                      body_type->kind >= TYPE_BOOLEAN &&
                                                      body_type->kind <= TYPE_DOUBLE);
                            
                            if (sam_is_reference && body_is_primitive) {
                                /* Box the primitive return value */
                                emit_boxing(lambda_mg, cp, body_type->kind);
                                bc_emit_u1(lambda_mg->code, OP_ARETURN);
                                mg_pop(lambda_mg, 1);
                            } else {
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
                
                /* Create method type entries for bootstrap args.
                 * sam_mt = erased SAM descriptor (e.g., (Object)V)
                 * spec_mt = specialized SAM descriptor (e.g., (String)V) */
                uint16_t sam_mt = cp_add_method_type(cg->cp, sam_descriptor);
                
                /* Build specialized SAM descriptor using actual param types */
                string_t *spec_desc = string_new("(");
                if (actual_param_count > 0) {
                    for (int i = 0; i < actual_param_count; i++) {
                        char *desc = type_to_descriptor(actual_param_types[i]);
                        string_append(spec_desc, desc);
                        free(desc);
                    }
                } else {
                    /* Fall back to SAM params */
                    for (slist_t *p = sam_params; p; p = p->next) {
                        symbol_t *param = (symbol_t *)p->data;
                        if (param && param->type) {
                            char *desc = type_to_descriptor(param->type);
                            string_append(spec_desc, desc);
                            free(desc);
                        }
                    }
                }
                string_append(spec_desc, ")");
                /* Return type - substitute type args */
                if (sam->type) {
                    slist_t *type_args = func_interface->data.class_type.type_args;
                    type_t *ret_type = sam->type;
                    if (ret_type->kind == TYPE_TYPEVAR && type_args) {
                        /* Substitute type variable with actual type arg */
                        ret_type = (type_t *)type_args->data;
                    }
                    char *ret_desc = type_to_descriptor(ret_type);
                    string_append(spec_desc, ret_desc);
                    free(ret_desc);
                } else {
                    string_append(spec_desc, "V");
                }
                uint16_t spec_mt = cp_add_method_type(cg->cp, spec_desc->str);
                string_free(spec_desc, true);
                
                /* Create method handle for implementation method */
                uint16_t impl_ref;
                method_handle_kind_t mh_kind;
                
                /* For interface instance methods, use invokeInterface/InterfaceMethodref 
                 * For class instance methods, use invokeSpecial/Methodref
                 * For static methods, use invokeStatic/Methodref */
                bool is_interface_method = (cg->class_sym && cg->class_sym->kind == SYM_INTERFACE);
                
                if (captures_this) {
                    if (is_interface_method) {
                        /* Interface instance lambda method */
                        impl_ref = cp_add_interface_methodref(cg->cp, cg->internal_name, 
                                                              lambda_method_name, impl_desc->str);
                        mh_kind = REF_invokeInterface;
                    } else {
                        /* Class instance lambda method */
                        impl_ref = cp_add_methodref(cg->cp, cg->internal_name, 
                                                    lambda_method_name, impl_desc->str);
                        mh_kind = REF_invokeSpecial;
                    }
                } else {
                    /* Static lambda method */
                    impl_ref = cp_add_methodref(cg->cp, cg->internal_name,
                                                lambda_method_name, impl_desc->str);
                    mh_kind = REF_invokeStatic;
                }
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
                    /* Track the real pushed type, not null - see the
                     * identical fix (and full explanation) at the
                     * instance-field-assignment site earlier in this
                     * file. */
                    if (cg && cg->internal_name) {
                        mg_push_object(mg, cg->internal_name);
                    } else {
                        mg_push_null(mg);
                    }
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
                
                symbol_t *sam = get_functional_interface_sam(iface_sym);
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
                        if (atype < 0) {
                            atype = 10;
                        }  /* Default to T_INT */
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
                
                /* Build SAM descriptor (erased) */
                char *sam_descriptor = method_to_descriptor(sam);
                
                /* Build specialized SAM descriptor using type arguments from functional interface.
                 * For Consumer<String>, the erased SAM is (Object)V but specialized is (String)V
                 * For BiFunction<T,U,R>, we need to match T->arg0, U->arg1, R->arg2
                 * 
                 * Type variable mapping: Track unique type var names and map to type args in order.
                 * E.g., for BiFunction's apply(T,U)->R: T->0, U->1, R->2 */
                char *specialized_sam_desc = NULL;
                slist_t *type_args = func_interface->data.class_type.type_args;
                if (type_args) {
                    /* Build a mapping of type variable names to type arguments.
                     * Assume type variables are named in order (T, U, R or similar). */
                    const char *type_var_names[16] = {0};
                    int type_var_count = 0;
                    
                    /* First pass: collect unique type variable names from SAM params and return */
                    for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                        symbol_t *param = (symbol_t *)p->data;
                        if (param && param->type && param->type->kind == TYPE_TYPEVAR &&
                            param->type->data.type_var.name) {
                            const char *name = param->type->data.type_var.name;
                            /* Check if we've seen this type variable */
                            bool seen = false;
                            for (int i = 0; i < type_var_count; i++) {
                                if (strcmp(type_var_names[i], name) == 0) {
                                    seen = true;
                                    break;
                                }
                            }
                            if (!seen && type_var_count < 16) {
                                type_var_names[type_var_count++] = name;
                            }
                        }
                    }
                    /* Add return type variable if present and different */
                    if (sam->type && sam->type->kind == TYPE_TYPEVAR && sam->type->data.type_var.name) {
                        const char *name = sam->type->data.type_var.name;
                        bool seen = false;
                        for (int i = 0; i < type_var_count; i++) {
                            if (strcmp(type_var_names[i], name) == 0) {
                                seen = true;
                                break;
                            }
                        }
                        if (!seen && type_var_count < 16) {
                            type_var_names[type_var_count++] = name;
                        }
                    }
                    
                    string_t *spec_desc = string_new("(");
                    /* Substitute type variables in parameters */
                    for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                        symbol_t *param = (symbol_t *)p->data;
                        if (param && param->type) {
                            type_t *param_type = param->type;
                            if (param_type->kind == TYPE_TYPEVAR && param_type->data.type_var.name) {
                                /* Find type arg for this type variable name */
                                const char *var_name = param_type->data.type_var.name;
                                type_t *actual = NULL;
                                for (int i = 0; i < type_var_count; i++) {
                                    if (strcmp(type_var_names[i], var_name) == 0) {
                                        slist_t *ta = type_args;
                                        for (int j = 0; j < i && ta; j++) {
                                            ta = ta->next;
                                        }
                                        if (ta) {
                                            actual = (type_t *)ta->data;
                                        }
                                        break;
                                    }
                                }
                                if (actual) {
                                    param_type = actual;
                                }
                            }
                            char *desc = type_to_descriptor(param_type);
                            string_append(spec_desc, desc);
                            free(desc);
                        }
                    }
                    string_append(spec_desc, ")");
                    /* Return type */
                    if (sam->type) {
                        type_t *ret_type = sam->type;
                        if (ret_type->kind == TYPE_TYPEVAR && ret_type->data.type_var.name) {
                            /* Find type arg for this type variable name */
                            const char *var_name = ret_type->data.type_var.name;
                            type_t *actual = NULL;
                            for (int i = 0; i < type_var_count; i++) {
                                if (strcmp(type_var_names[i], var_name) == 0) {
                                    slist_t *ta = type_args;
                                    for (int j = 0; j < i && ta; j++) {
                                        ta = ta->next;
                                    }
                                    if (ta) {
                                        actual = (type_t *)ta->data;
                                    }
                                    break;
                                }
                            }
                            if (actual) {
                                ret_type = actual;
                            }
                        }
                        char *ret_desc = type_to_descriptor(ret_type);
                        string_append(spec_desc, ret_desc);
                        free(ret_desc);
                    } else {
                        string_append(spec_desc, "V");
                    }
                    specialized_sam_desc = string_free(spec_desc, false);
                }
                
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
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
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
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
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
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
                    return false;
                }
                
                /* Bootstrap arguments for LambdaMetafactory.metafactory:
                 * 1. MethodType - erased SAM descriptor
                 * 2. MethodHandle - implementation method handle
                 * 3. MethodType - specialized SAM descriptor
                 */
                uint16_t erased_type = cp_add_method_type(cp, sam_descriptor);
                const char *spec_desc_str = specialized_sam_desc ? specialized_sam_desc : sam_descriptor;
                uint16_t specialized_type = cp_add_method_type(cp, spec_desc_str);
                if (erased_type == 0 || specialized_type == 0) {
                    fprintf(stderr, "codegen: failed to add method types\n");
                    free(target_internal);
                    free(iface_internal);
                    free(sam_descriptor);
                    free(method_descriptor);
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
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
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
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
                    if (specialized_sam_desc) {
                        free(specialized_sam_desc);
                    }
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
                if (specialized_sam_desc) {
                    free(specialized_sam_desc);
                }
                
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
                    
                    if (strcmp(prim_name, "boolean") == 0) {
                        wrapper_class = "java/lang/Boolean";
                    } else if (strcmp(prim_name, "byte") == 0) {
                        wrapper_class = "java/lang/Byte";
                    } else if (strcmp(prim_name, "char") == 0) {
                        wrapper_class = "java/lang/Character";
                    } else if (strcmp(prim_name, "short") == 0) {
                        wrapper_class = "java/lang/Short";
                    } else if (strcmp(prim_name, "int") == 0) {
                        wrapper_class = "java/lang/Integer";
                    } else if (strcmp(prim_name, "long") == 0) {
                        wrapper_class = "java/lang/Long";
                    } else if (strcmp(prim_name, "float") == 0) {
                        wrapper_class = "java/lang/Float";
                    } else if (strcmp(prim_name, "double") == 0) {
                        wrapper_class = "java/lang/Double";
                    } else if (strcmp(prim_name, "void") == 0) {
                        wrapper_class = "java/lang/Void";
                    }
                    
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
            fprintf(stderr, "codegen: unhandled expression type: %s at line %d\n",
                    ast_type_name(expr->type), expr->line);
            return false;
    }
}

bool codegen_expression(method_gen_t *mg, ast_node_t *expr)
{
    return codegen_expr(mg, expr, mg->cp);
}

