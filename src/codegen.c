/*
 * codegen.c
 * Bytecode generation for the JVM
 * Copyright (C) 2016, 2020, 2026 Chris Burdess <dog@gnu.org>
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

/* ========================================================================
 * Annotation Retention Policy
 * ======================================================================== */

/* ========================================================================
 * Bytecode Buffer Implementation
 * ======================================================================== */

bytecode_t *bytecode_new(void)
{
    bytecode_t *bc = calloc(1, sizeof(bytecode_t));
    if (!bc) {
        return NULL;
    }
    
    bc->capacity = 256;
    bc->code = malloc(bc->capacity);
    if (!bc->code) {
        free(bc);
        return NULL;
    }
    
    bc->length = 0;
    bc->max_stack = 0;
    bc->max_locals = 0;
    
    return bc;
}

void bytecode_free(bytecode_t *bc)
{
    if (!bc) {
        return;
    }
    free(bc->code);
    free(bc);
}

static void bc_ensure_capacity(bytecode_t *bc, uint32_t needed)
{
    if (bc->length + needed > bc->capacity) {
        while (bc->length + needed > bc->capacity) {
            bc->capacity *= 2;
        }
        bc->code = realloc(bc->code, bc->capacity);
    }
}

void bc_emit(bytecode_t *bc, uint8_t opcode)
{
    bc_ensure_capacity(bc, 1);
    bc->code[bc->length++] = opcode;
}

void bc_emit_u1(bytecode_t *bc, uint8_t value)
{
    bc_ensure_capacity(bc, 1);
    bc->code[bc->length++] = value;
}

void bc_emit_u2(bytecode_t *bc, uint16_t value)
{
    bc_ensure_capacity(bc, 2);
    bc->code[bc->length++] = (value >> 8) & 0xFF;
    bc->code[bc->length++] = value & 0xFF;
}

void bc_emit_u4(bytecode_t *bc, uint32_t value)
{
    bc_ensure_capacity(bc, 4);
    bc->code[bc->length++] = (value >> 24) & 0xFF;
    bc->code[bc->length++] = (value >> 16) & 0xFF;
    bc->code[bc->length++] = (value >> 8) & 0xFF;
    bc->code[bc->length++] = value & 0xFF;
}

void bc_emit_s1(bytecode_t *bc, int8_t value)
{
    bc_emit_u1(bc, (uint8_t)value);
}

void bc_emit_s2(bytecode_t *bc, int16_t value)
{
    bc_emit_u2(bc, (uint16_t)value);
}

uint32_t bc_offset(bytecode_t *bc)
{
    return bc->length;
}

void bc_patch_u2(bytecode_t *bc, uint32_t offset, uint16_t value)
{
    bc->code[offset] = (value >> 8) & 0xFF;
    bc->code[offset + 1] = value & 0xFF;
}

/* ========================================================================
 * Label Implementation
 * ======================================================================== */

label_t *label_new(void)
{
    label_t *label = calloc(1, sizeof(label_t));
    if (label) {
        label->offset = -1;
    }
    return label;
}

void label_free(label_t *label)
{
    if (label) {
        slist_free(label->fixups);
        free(label);
    }
}

void label_bind(label_t *label, bytecode_t *bc)
{
    label->offset = bc_offset(bc);
    
    /* Patch all forward references */
    for (slist_t *node = label->fixups; node; node = node->next) {
        uint32_t fixup_offset = (uint32_t)(uintptr_t)node->data;
        int16_t relative = label->offset - (fixup_offset - 2);
        bc_patch_u2(bc, fixup_offset, (uint16_t)relative);
    }
}

/**
 * Bind a label at current bytecode offset and record a StackMapTable frame.
 * Use this for branch targets that need verification frames.
 */
void mg_bind_label(method_gen_t *mg, label_t *label)
{
    if (!mg || !label) return;
    
    /* Bind the label */
    label_bind(label, mg->code);
    
    /* Record a stackmap frame at this branch target */
    if (mg->stackmap && label->offset >= 0) {
        stackmap_record_frame(mg->stackmap, (uint16_t)label->offset);
    }
}

/**
 * Record a StackMapTable frame at the current bytecode offset.
 * Call this at branch targets when not using the label system.
 */
void mg_record_frame(method_gen_t *mg)
{
    if (!mg || !mg->stackmap) return;
    stackmap_record_frame(mg->stackmap, (uint16_t)mg->code->length);
}

/**
 * Record a StackMapTable frame at an exception handler.
 * At exception handlers, the JVM clears the stack and pushes the exception.
 * This function updates the stackmap to reflect the exception on the stack.
 * 
 * Note: We save and restore the stackmap locals count to ensure that locals
 * allocated inside the handler don't affect frames recorded later.
 */
void mg_record_exception_handler_frame(method_gen_t *mg, const char *exception_class)
{
    if (!mg || !mg->stackmap) return;
    
    /* At exception handler, stack is cleared and exception is pushed */
    stackmap_clear_stack(mg->stackmap);
    stackmap_push_object(mg->stackmap, mg->cp, 
                         exception_class ? exception_class : "java/lang/Throwable");
    
    /* Record the frame with exception on stack */
    stackmap_record_frame(mg->stackmap, (uint16_t)mg->code->length);
    
    /* Pop the exception from stackmap (it will be consumed by astore) */
    stackmap_pop(mg->stackmap, 1);
}

/**
 * Save the current stackmap locals count.
 * Use this before entering a scoped region (e.g., catch block) where locals
 * may be allocated that shouldn't be visible after the region.
 */
uint16_t mg_save_locals_count(method_gen_t *mg)
{
    if (!mg || !mg->stackmap) return 0;
    return stackmap_get_locals_count(mg->stackmap);
}

/**
 * Restore the stackmap locals count to a previously saved value.
 * Use this after exiting a scoped region to "forget" locals allocated within.
 */
void mg_restore_locals_count(method_gen_t *mg, uint16_t count)
{
    if (!mg || !mg->stackmap) return;
    stackmap_set_locals_count(mg->stackmap, count);
}

void label_emit_jump(label_t *label, bytecode_t *bc, uint8_t opcode)
{
    bc_emit(bc, opcode);
    
    if (label->offset >= 0) {
        /* Backward jump - calculate offset */
        int16_t relative = label->offset - bc_offset(bc);
        bc_emit_s2(bc, relative);
    } else {
        /* Forward jump - emit placeholder and record fixup */
        uint32_t fixup_pos = bc_offset(bc);
        bc_emit_u2(bc, 0);
        
        if (!label->fixups) {
            label->fixups = slist_new((void *)(uintptr_t)fixup_pos);
        } else {
            slist_append(label->fixups, (void *)(uintptr_t)fixup_pos);
        }
    }
}

/* ========================================================================
 * Modifier Conversion: MOD_* (semantic) to ACC_* (class file)
 * ======================================================================== */

/**
 * Convert semantic modifier flags (MOD_*) to JVM access flags (ACC_*).
 * The semantic modifiers don't match JVM access flag values!
 * 
 * MOD_ values (genesis.h):     ACC_ values (classfile.h):
 *   MOD_PUBLIC     0x0001        ACC_PUBLIC      0x0001
 *   MOD_PRIVATE    0x0002        ACC_PRIVATE     0x0002
 *   MOD_PROTECTED  0x0004        ACC_PROTECTED   0x0004
 *   MOD_STATIC     0x0008        ACC_STATIC      0x0008
 *   MOD_FINAL      0x0010        ACC_FINAL       0x0010
 *   MOD_ABSTRACT   0x0020        ACC_ABSTRACT    0x0400   << different!
 *   MOD_NATIVE     0x0040        ACC_NATIVE      0x0100   << different!
 *   MOD_SYNCHRONIZED 0x0080      ACC_SYNCHRONIZED 0x0020  << different!
 *   MOD_TRANSIENT  0x0100        ACC_TRANSIENT   0x0080   << different!
 *   MOD_VOLATILE   0x0200        ACC_VOLATILE    0x0040   << different!
 *   MOD_STRICTFP   0x0400        ACC_STRICT      0x0800   << different!
 */
static uint16_t mods_to_access_flags(uint16_t mods)
{
    uint16_t acc = 0;
    
    /* These match directly */
    if (mods & MOD_PUBLIC)    acc |= ACC_PUBLIC;
    if (mods & MOD_PRIVATE)   acc |= ACC_PRIVATE;
    if (mods & MOD_PROTECTED) acc |= ACC_PROTECTED;
    if (mods & MOD_STATIC)    acc |= ACC_STATIC;
    if (mods & MOD_FINAL)     acc |= ACC_FINAL;
    
    /* These need translation */
    if (mods & MOD_ABSTRACT)     acc |= ACC_ABSTRACT;
    if (mods & MOD_NATIVE)       acc |= ACC_NATIVE;
    if (mods & MOD_SYNCHRONIZED) acc |= ACC_SYNCHRONIZED;
    if (mods & MOD_TRANSIENT)    acc |= ACC_TRANSIENT;
    if (mods & MOD_VOLATILE)     acc |= ACC_VOLATILE;
    if (mods & MOD_STRICTFP)     acc |= ACC_STRICT;
    
    /* MOD_VARARGS becomes ACC_VARARGS (both 0x0080 but for methods only) */
    if (mods & MOD_VARARGS) acc |= ACC_VARARGS;
    
    return acc;
}

/* ========================================================================
 * Method Generation Implementation
 * ======================================================================== */

/**
 * Create a new local variable info structure.
 */
local_var_info_t *local_var_info_new(uint16_t slot, type_kind_t kind)
{
    local_var_info_t *info = calloc(1, sizeof(local_var_info_t));
    if (!info) {
        return NULL;
    }
    info->slot = slot;
    info->kind = kind;
    info->array_elem_kind = TYPE_UNKNOWN;
    return info;
}

/**
 * Free a local variable info structure.
 */
void local_var_info_free(local_var_info_t *info)
{
    if (!info) {
        return;
    }
    free(info->class_name);
    free(info->array_elem_class);
    free(info);
}

/**
 * Free callback for hashtable iteration.
 */
static void local_var_info_free_cb(void *data)
{
    local_var_info_free((local_var_info_t *)data);
}

method_gen_t *method_gen_new(class_gen_t *cg, symbol_t *method)
{
    method_gen_t *mg = calloc(1, sizeof(method_gen_t));
    if (!mg) {
        return NULL;
    }
    
    mg->code = bytecode_new();
    mg->cp = cg->cp;
    mg->class_gen = cg;
    mg->locals = hashtable_new();
    mg->method = method;
    mg->loop_stack = NULL;
    
    if (method) {
        mg->is_static = (method->modifiers & MOD_STATIC) != 0;
        mg->is_constructor = (method->kind == SYM_CONSTRUCTOR);
    }
    
    /* Slot 0 is 'this' for instance methods */
    if (!mg->is_static) {
        mg->next_slot = 1;
        mg->max_locals = 1;
    }
    
    /* Initialize StackMapTable tracking */
    mg->stackmap = stackmap_new();
    if (mg->stackmap && cg->internal_name) {
        stackmap_init_method(mg->stackmap, mg, mg->is_static, cg->internal_name);
    }
    
    return mg;
}

/* Loop context helpers for break/continue */
static loop_context_t *loop_context_new(size_t continue_target, const char *label)
{
    loop_context_t *ctx = calloc(1, sizeof(loop_context_t));
    if (!ctx) {
        return NULL;
    }
    ctx->continue_target = continue_target;
    ctx->break_offsets = NULL;
    ctx->label = label;
    return ctx;
}

static void loop_context_free(loop_context_t *ctx)
{
    if (!ctx) {
        return;
    }
    /* Free break_offsets list (just the nodes, data is size_t cast to void*) */
    slist_t *node = ctx->break_offsets;
    while (node) {
        slist_t *next = node->next;
        free(node);
        node = next;
    }
    free(ctx);
}

void mg_push_loop(method_gen_t *mg, size_t continue_target, const char *label)
{
    loop_context_t *ctx = loop_context_new(continue_target, label);
    if (ctx) {
        mg->loop_stack = slist_prepend(mg->loop_stack, ctx);
    }
}

void mg_pop_loop(method_gen_t *mg, size_t loop_end)
{
    if (!mg->loop_stack) {
        return;
    }
    
    loop_context_t *ctx = (loop_context_t *)mg->loop_stack->data;
    
    /* Patch all break statements to jump to loop_end */
    slist_t *node = ctx->break_offsets;
    while (node) {
        size_t break_pos = (size_t)(uintptr_t)node->data;
        int16_t offset = (int16_t)(loop_end - break_pos);
        mg->code->code[break_pos + 1] = (offset >> 8) & 0xFF;
        mg->code->code[break_pos + 2] = offset & 0xFF;
        node = node->next;
    }
    
    /* Pop and free the context */
    slist_t *old = mg->loop_stack;
    mg->loop_stack = mg->loop_stack->next;
    loop_context_free(ctx);
    free(old);
}

/**
 * Find a loop context by label name.
 * Returns the loop context with the matching label, or NULL if not found.
 */
loop_context_t *mg_find_loop_by_label(method_gen_t *mg, const char *label)
{
    if (!label || !mg->loop_stack) {
        return NULL;
    }
    
    for (slist_t *node = mg->loop_stack; node; node = node->next) {
        loop_context_t *ctx = (loop_context_t *)node->data;
        if (ctx->label && strcmp(ctx->label, label) == 0) {
            return ctx;
        }
    }
    
    return NULL;
}

/**
 * Add a break position to a specific loop context (for labeled breaks).
 */
void mg_add_break_to_context(loop_context_t *ctx, size_t break_pos)
{
    if (!ctx) {
        return;
    }
    ctx->break_offsets = slist_prepend(ctx->break_offsets, (void *)(uintptr_t)break_pos);
}

/**
 * Add an exception handler entry to the method.
 */
void mg_add_exception_handler(method_gen_t *mg, uint16_t start_pc,
                               uint16_t end_pc, uint16_t handler_pc,
                               uint16_t catch_type)
{
    exception_entry_t *entry = calloc(1, sizeof(exception_entry_t));
    if (!entry) {
        return;
    }
    entry->start_pc = start_pc;
    entry->end_pc = end_pc;
    entry->handler_pc = handler_pc;
    entry->catch_type = catch_type;
    
    /* Append to the list (order matters for exception matching) */
    if (!mg->exception_handlers) {
        mg->exception_handlers = slist_new(entry);
    } else {
        slist_t *tail = mg->exception_handlers;
        while (tail->next) {
            tail = tail->next;
        }
        slist_append(tail, entry);
    }
}

void method_gen_free(method_gen_t *mg)
{
    if (!mg) {
        return;
    }
    
    bytecode_free(mg->code);
    hashtable_free_full(mg->locals, local_var_info_free_cb);
    
    for (slist_t *node = mg->labels; node; node = node->next) {
        label_free((label_t *)node->data);
    }
    slist_free(mg->labels);
    
    /* Free any remaining loop contexts */
    while (mg->loop_stack) {
        slist_t *old = mg->loop_stack;
        mg->loop_stack = mg->loop_stack->next;
        loop_context_free((loop_context_t *)old->data);
        free(old);
    }
    
    slist_free_full(mg->exception_handlers, free);
    slist_free_full(mg->local_var_table, free);
    slist_free(mg->line_numbers);
    stackmap_free(mg->stackmap);
    
    free(mg);
}

/**
 * Record a line number entry mapping current bytecode offset to source line.
 * Only records if the line is different from the last recorded line.
 */
void mg_record_line(method_gen_t *mg, int line)
{
    if (!mg || !mg->code || line <= 0) {
        return;
    }
    
    uint16_t pc = (uint16_t)mg->code->length;
    
    /* Check if we already have an entry for this line */
    if (mg->line_numbers) {
        /* Get the last entry */
        slist_t *last = mg->line_numbers;
        while (last->next) {
            last = last->next;
        }
        line_number_entry_t *prev = (line_number_entry_t *)last->data;
        /* Don't add duplicate entries for the same line */
        if (prev->line_number == (uint16_t)line) {
            return;
        }
    }
    
    line_number_entry_t *entry = calloc(1, sizeof(line_number_entry_t));
    if (!entry) {
        return;
    }
    entry->start_pc = pc;
    entry->line_number = (uint16_t)line;
    
    if (!mg->line_numbers) {
        mg->line_numbers = slist_new(entry);
    } else {
        slist_append(mg->line_numbers, entry);
    }
}

/**
 * Record a local variable for the LocalVariableTable attribute.
 * The type_ast parameter is optional and used for type annotations.
 */
void mg_record_local_var(method_gen_t *mg, const char *name, const char *descriptor,
                          uint16_t slot, uint16_t start_pc, ast_node_t *type_ast)
{
    if (!mg || !name) {
        return;
    }
    
    local_var_t *lv = calloc(1, sizeof(local_var_t));
    if (!lv) {
        return;
    }
    
    lv->name = strdup(name);
    lv->descriptor = descriptor ? strdup(descriptor) : strdup("I");
    lv->slot = slot;
    lv->start_pc = start_pc;
    lv->length = 0;  /* Will be set when method ends */
    lv->type_ast = type_ast;  /* Store for type annotations */
    
    if (!mg->local_var_table) {
        mg->local_var_table = slist_new(lv);
    } else {
        slist_append(mg->local_var_table, lv);
    }
}

uint16_t mg_allocate_local(method_gen_t *mg, const char *name, type_t *type)
{
    uint16_t slot = mg->next_slot;
    
    /* Long and double take 2 slots */
    int size = 1;
    type_kind_t kind = TYPE_INT;  /* Default to int for unknown types */
    if (type) {
        kind = type->kind;
        if (kind == TYPE_LONG || kind == TYPE_DOUBLE) {
            size = 2;
        }
    }
    
    mg->next_slot += size;
    if (mg->next_slot > mg->max_locals) {
        mg->max_locals = mg->next_slot;
    }
    
    /* Create local variable info structure */
    local_var_info_t *info = local_var_info_new(slot, kind);
    if (!info) {
        return slot;
    }
    
    /* Track if this is a reference type (object/array) */
    if (type && (type->kind == TYPE_CLASS || type->kind == TYPE_ARRAY)) {
        info->is_ref = true;
        
        /* Store the class name for class types - prefer qualified name from symbol */
        if (type->kind == TYPE_CLASS && type->data.class_type.name) {
            const char *class_name = type->data.class_type.name;
            if (type->data.class_type.symbol && type->data.class_type.symbol->qualified_name) {
                class_name = type->data.class_type.symbol->qualified_name;
            }
            info->class_name = class_to_internal_name(class_name);
        }
    }
    
    hashtable_insert(mg->locals, name, info);
    
    /* Update StackMapTable local tracking */
    if (mg->stackmap) {
        if (type && type->kind == TYPE_CLASS && type->data.class_type.name) {
            /* Prefer the symbol's qualified name if available */
            const char *class_name = type->data.class_type.name;
            if (type->data.class_type.symbol && type->data.class_type.symbol->qualified_name) {
                class_name = type->data.class_type.symbol->qualified_name;
            }
            const char *internal = class_to_internal_name(class_name);
            stackmap_set_local_object(mg->stackmap, slot, mg->cp, internal);
        } else if (type && type->kind == TYPE_ARRAY) {
            /* Array types - use the proper array descriptor like [I, [Ljava/lang/String; */
            char *array_desc = type_to_descriptor(type);
            stackmap_set_local_object(mg->stackmap, slot, mg->cp, array_desc);
            free(array_desc);
        } else {
            switch (kind) {
                case TYPE_LONG:   stackmap_set_local_long(mg->stackmap, slot); break;
                case TYPE_DOUBLE: stackmap_set_local_double(mg->stackmap, slot); break;
                case TYPE_FLOAT:  stackmap_set_local_float(mg->stackmap, slot); break;
                default:          stackmap_set_local_int(mg->stackmap, slot); break;
            }
        }
    }
    
    return slot;
}

/* Get local variable info by name */
static local_var_info_t *mg_get_local_info(method_gen_t *mg, const char *name)
{
    return (local_var_info_t *)hashtable_lookup(mg->locals, name);
}

/* Check if a local variable is a reference type */
bool mg_local_is_ref(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    return info ? info->is_ref : false;
}

/* Get the class name of a local variable (internal format, e.g., "com/example/Foo") */
const char *mg_local_class_name(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    return info ? info->class_name : NULL;
}

/* Check if a local variable is an array type */
bool mg_local_is_array(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    return info ? info->is_array : false;
}

/* Get the element type kind of an array local variable */
type_kind_t mg_local_array_elem_kind(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    if (info && info->is_array && info->array_elem_kind != TYPE_UNKNOWN) {
        return info->array_elem_kind;
    }
    return TYPE_INT;  /* Default to int */
}

/* Get the element class name of an array local variable (for object arrays) */
const char *mg_local_array_elem_class(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    return info ? info->array_elem_class : NULL;
}

/* Get the dimension count of an array local variable */
int mg_local_array_dims(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    if (info && info->is_array && info->array_dims > 0) {
        return info->array_dims;
    }
    return 1;  /* Default to 1 dimension */
}

uint16_t mg_get_local(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    if (!info && !mg->is_static && strcmp(name, "this") == 0) {
        return 0;  /* 'this' is always slot 0 for instance methods */
    }
    return info ? info->slot : 0;
}

/* Get the type kind of a local variable */
type_kind_t mg_get_local_type(method_gen_t *mg, const char *name)
{
    local_var_info_t *info = mg_get_local_info(mg, name);
    if (info) {
        return info->kind;
    }
    /* Default to int for unknown types */
    return TYPE_INT;
}

/* Emit the correct load instruction for a local variable */
void mg_emit_load_local(method_gen_t *mg, uint16_t slot, type_kind_t kind)
{
    switch (kind) {
        case TYPE_LONG:
            if (slot <= 3) {
                bc_emit(mg->code, OP_LLOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_LLOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push_long(mg);
            break;
            
        case TYPE_DOUBLE:
            if (slot <= 3) {
                bc_emit(mg->code, OP_DLOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_DLOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push_double(mg);
            break;
            
        case TYPE_FLOAT:
            if (slot <= 3) {
                bc_emit(mg->code, OP_FLOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_FLOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push_float(mg);
            break;
            
        case TYPE_CLASS:
        case TYPE_ARRAY:
        case TYPE_TYPEVAR:  /* Type variables erase to Object (reference type) */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ALOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_ALOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push_null(mg);  /* Use null for object type tracking */
            break;
            
        default:
            /* int, boolean, byte, char, short all use iload */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ILOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_ILOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push_int(mg);
            break;
    }
}

/* Emit the correct store instruction for a local variable */
void mg_emit_store_local(method_gen_t *mg, uint16_t slot, type_kind_t kind)
{
    switch (kind) {
        case TYPE_LONG:
            if (slot <= 3) {
                bc_emit(mg->code, OP_LSTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_LSTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 2);
            break;
            
        case TYPE_DOUBLE:
            if (slot <= 3) {
                bc_emit(mg->code, OP_DSTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_DSTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 2);
            break;
            
        case TYPE_FLOAT:
            if (slot <= 3) {
                bc_emit(mg->code, OP_FSTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_FSTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 1);
            break;
            
        case TYPE_CLASS:
        case TYPE_ARRAY:
        case TYPE_TYPEVAR:  /* Type variables erase to Object (reference type) */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ASTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_ASTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 1);
            break;
            
        default:
            /* int, boolean, byte, char, short all use istore */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ISTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_ISTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 1);
            break;
    }
}

void mg_push(method_gen_t *mg, int slots)
{
    mg->stack_depth += slots;
    if (mg->stack_depth > mg->max_stack) {
        mg->max_stack = mg->stack_depth;
    }
}

void mg_pop(method_gen_t *mg, int slots)
{
    mg->stack_depth -= slots;
}

/* Type-aware stack operations - update both runtime and stackmap tracking */

void mg_push_int(method_gen_t *mg)
{
    mg_push(mg, 1);
    if (mg->stackmap) stackmap_push_int(mg->stackmap);
}

void mg_push_long(method_gen_t *mg)
{
    mg_push(mg, 2);
    if (mg->stackmap) stackmap_push_long(mg->stackmap);
}

void mg_push_float(method_gen_t *mg)
{
    mg_push(mg, 1);
    if (mg->stackmap) stackmap_push_float(mg->stackmap);
}

void mg_push_double(method_gen_t *mg)
{
    mg_push(mg, 2);
    if (mg->stackmap) stackmap_push_double(mg->stackmap);
}

void mg_push_null(method_gen_t *mg)
{
    mg_push(mg, 1);
    if (mg->stackmap) stackmap_push_null(mg->stackmap);
}

void mg_push_object(method_gen_t *mg, const char *class_name)
{
    mg_push(mg, 1);
    if (mg->stackmap && class_name) {
        stackmap_push_object(mg->stackmap, mg->cp, class_name);
    }
}

void mg_push_uninitialized(method_gen_t *mg, uint16_t new_offset)
{
    mg_push(mg, 1);
    if (mg->stackmap) stackmap_push_uninitialized(mg->stackmap, new_offset);
}

void mg_pop_typed(method_gen_t *mg, int slots)
{
    mg_pop(mg, slots);
    if (mg->stackmap) stackmap_pop(mg->stackmap, slots);
}

/* ========================================================================
 * Type Descriptor Generation
 * ======================================================================== */

char *type_to_descriptor(type_t *type)
{
    if (!type) {
        return strdup("V");
    }
    
    switch (type->kind) {
        case TYPE_VOID:    return strdup("V");
        case TYPE_BOOLEAN: return strdup("Z");
        case TYPE_BYTE:    return strdup("B");
        case TYPE_CHAR:    return strdup("C");
        case TYPE_SHORT:   return strdup("S");
        case TYPE_INT:     return strdup("I");
        case TYPE_LONG:    return strdup("J");
        case TYPE_FLOAT:   return strdup("F");
        case TYPE_DOUBLE:  return strdup("D");
        
        case TYPE_CLASS:
            {
                const char *name = type->data.class_type.name;
                char *internal = class_to_internal_name(name);
                size_t len = strlen(internal) + 3;
                char *desc = malloc(len);
                snprintf(desc, len, "L%s;", internal);
                free(internal);
                return desc;
            }
        
        case TYPE_ARRAY:
            {
                char *elem_desc = type_to_descriptor(type->data.array_type.element_type);
                size_t len = strlen(elem_desc) + type->data.array_type.dimensions + 1;
                char *desc = malloc(len);
                char *p = desc;
                for (int i = 0; i < type->data.array_type.dimensions; i++) {
                    *p++ = '[';
                }
                strcpy(p, elem_desc);
                free(elem_desc);
                return desc;
            }
        
        case TYPE_TYPEVAR:
            {
                /* Type erasure: use the bound type's descriptor */
                type_t *bound = type->data.type_var.bound;
                if (bound) {
                    return type_to_descriptor(bound);
                }
                /* Unbounded type variable erases to Object */
                return strdup("Ljava/lang/Object;");
            }
        
        default:
            return strdup("Ljava/lang/Object;");
    }
}

char *method_to_descriptor(symbol_t *method)
{
    if (!method) {
        return strdup("()V");
    }
    
    string_t *desc = string_new("(");
    
    /* Parameters */
    for (slist_t *node = method->data.method_data.parameters; node; node = node->next) {
        symbol_t *param = (symbol_t *)node->data;
        if (param->type) {
            char *param_desc = type_to_descriptor(param->type);
            string_append(desc, param_desc);
            free(param_desc);
        }
    }
    
    string_append_c(desc, ')');
    
    /* Return type */
    char *ret_desc = type_to_descriptor(method->type);
    string_append(desc, ret_desc);
    free(ret_desc);
    
    return string_free(desc, false);
}

char *class_to_internal_name(const char *class_name)
{
    if (!class_name) {
        return strdup("java/lang/Object");
    }
    
    char *internal = strdup(class_name);
    for (char *p = internal; *p; p++) {
        if (*p == '.') {
            *p = '/';
        }
    }
    return internal;
}

/* ========================================================================
 * Generic Signature Generation
 * 
 * Signature format (JVM Spec 4.7.9.1):
 * ClassSignature:    <TypeParameter+>? SuperclassSignature SuperinterfaceSignature*
 * MethodSignature:   <TypeParameter+>? (TypeSignature*) ReturnType ThrowsSignature*
 * FieldSignature:    ReferenceTypeSignature
 * 
 * TypeParameter:     Identifier ClassBound InterfaceBound*
 * ClassBound:        : ReferenceTypeSignature?
 * InterfaceBound:    : ReferenceTypeSignature
 * 
 * TypeSignature:     Z | C | B | S | I | F | J | D | ReferenceTypeSignature
 * ReferenceTypeSignature: L ClassName TypeArguments? ; | [ TypeSignature | T Identifier ;
 * TypeArguments:     < TypeArgument+ >
 * TypeArgument:      * | + ReferenceTypeSignature | - ReferenceTypeSignature | ReferenceTypeSignature
 * ======================================================================== */

/**
 * Generate a type signature from a type_t.
 * Unlike type_to_descriptor, this preserves type variable references.
 */
static char *type_to_signature(type_t *type)
{
    if (!type) {
        return strdup("Ljava/lang/Object;");
    }
    
    switch (type->kind) {
        case TYPE_VOID:    return strdup("V");
        case TYPE_BOOLEAN: return strdup("Z");
        case TYPE_BYTE:    return strdup("B");
        case TYPE_CHAR:    return strdup("C");
        case TYPE_SHORT:   return strdup("S");
        case TYPE_INT:     return strdup("I");
        case TYPE_LONG:    return strdup("J");
        case TYPE_FLOAT:   return strdup("F");
        case TYPE_DOUBLE:  return strdup("D");
        
        case TYPE_CLASS:
            {
                const char *name = type->data.class_type.name;
                char *internal = class_to_internal_name(name);
                
                /* Check for type arguments */
                if (type->data.class_type.type_args) {
                    string_t *sig_str = string_new("L");
                    string_append(sig_str, internal);
                    string_append(sig_str, "<");
                    
                    for (slist_t *arg = type->data.class_type.type_args; arg; arg = arg->next) {
                        type_t *arg_type = (type_t *)arg->data;
                        char *arg_sig = type_to_signature(arg_type);
                        string_append(sig_str, arg_sig);
                        free(arg_sig);
                    }
                    
                    string_append(sig_str, ">;");
                    free(internal);
                    return string_free(sig_str, false);
                }
                
                size_t len = strlen(internal) + 3;
                char *sig = malloc(len);
                snprintf(sig, len, "L%s;", internal);
                free(internal);
                return sig;
            }
        
        case TYPE_ARRAY:
            {
                char *elem_sig = type_to_signature(type->data.array_type.element_type);
                size_t len = strlen(elem_sig) + type->data.array_type.dimensions + 1;
                char *sig = malloc(len);
                char *p = sig;
                for (int i = 0; i < type->data.array_type.dimensions; i++) {
                    *p++ = '[';
                }
                strcpy(p, elem_sig);
                free(elem_sig);
                return sig;
            }
        
        case TYPE_TYPEVAR:
            {
                /* Type variable reference: T<name>; */
                const char *name = type->data.type_var.name;
                if (name) {
                    size_t len = strlen(name) + 3;
                    char *sig = malloc(len);
                    snprintf(sig, len, "T%s;", name);
                    return sig;
                }
                return strdup("Ljava/lang/Object;");
            }
        
        case TYPE_WILDCARD:
            {
                /* Wildcard signatures:
                 * * = unbounded (?)
                 * +ReferenceTypeSignature = upper bound (? extends T)
                 * -ReferenceTypeSignature = lower bound (? super T) */
                if (type->data.wildcard.bound_kind == 0 || !type->data.wildcard.bound) {
                    /* Unbounded wildcard */
                    return strdup("*");
                } else if (type->data.wildcard.bound_kind == 1) {
                    /* Upper bound (? extends T) */
                    char *bound_sig = type_to_signature(type->data.wildcard.bound);
                    size_t len = strlen(bound_sig) + 2;
                    char *sig = malloc(len);
                    snprintf(sig, len, "+%s", bound_sig);
                    free(bound_sig);
                    return sig;
                } else {
                    /* Lower bound (? super T) */
                    char *bound_sig = type_to_signature(type->data.wildcard.bound);
                    size_t len = strlen(bound_sig) + 2;
                    char *sig = malloc(len);
                    snprintf(sig, len, "-%s", bound_sig);
                    free(bound_sig);
                    return sig;
                }
            }
        
        default:
            return strdup("Ljava/lang/Object;");
    }
}

/**
 * Generate type parameters signature: <T:Ljava/lang/Object;E:Ljava/lang/Number;>
 * type_params is a list of AST_TYPE_PARAMETER nodes.
 */
static char *generate_type_params_signature(slist_t *type_params)
{
    if (!type_params) {
        return NULL;
    }
    
    string_t *sig = string_new("<");
    
    for (slist_t *node = type_params; node; node = node->next) {
        ast_node_t *tp = (ast_node_t *)node->data;
        if (tp->type != AST_TYPE_PARAMETER) {
            continue;
        }
        
        const char *name = tp->data.node.name;
        if (!name) {
            continue;
        }
        
        /* Append type parameter name */
        string_append(sig, name);
        
        /* Append class bound and interface bounds
         * Format: T:ClassBound:InterfaceBound1:InterfaceBound2
         * If first bound is an interface, use empty class bound: T::InterfaceBound */
        string_append_c(sig, ':');
        
        /* Check for bounds (children of type parameter) */
        bool has_class_bound = false;
        if (tp->data.node.children) {
            bool first_bound = true;
            for (slist_t *bnode = tp->data.node.children; bnode; bnode = bnode->next) {
                ast_node_t *bound = (ast_node_t *)bnode->data;
                if (!bound || bound->type != AST_CLASS_TYPE) {
                    continue;
                }
                
                /* Use sem_type if available (has fully qualified name) */
                const char *bound_name = NULL;
                if (bound->sem_type && bound->sem_type->kind == TYPE_CLASS) {
                    bound_name = bound->sem_type->data.class_type.name;
                }
                if (!bound_name) {
                    bound_name = bound->data.node.name;
                }
                if (bound_name) {
                    /* Check if this is an interface (for determining class bound vs interface bound)
                     * For now, assume first bound is class bound unless it looks like an interface */
                    if (!first_bound) {
                        /* Additional bounds are interface bounds */
                        string_append_c(sig, ':');
                    }
                    
                    char *internal = class_to_internal_name(bound_name);
                    string_append_c(sig, 'L');
                    string_append(sig, internal);
                    string_append_c(sig, ';');
                    free(internal);
                    
                    if (first_bound) {
                        has_class_bound = true;
                    }
                    first_bound = false;
                }
            }
        }
        /* If no explicit bound, use Object */
        if (!has_class_bound) {
            string_append(sig, "Ljava/lang/Object;");
        }
    }
    
    string_append_c(sig, '>');
    
    return string_free(sig, false);
}

/**
 * Generate class signature from class declaration AST and symbol.
 * Returns NULL if no signature is needed (non-generic class with non-generic superclass).
 */
char *generate_class_signature(ast_node_t *class_decl, symbol_t *class_sym)
{
    if (!class_decl) {
        return NULL;
    }
    
    /* Look for type parameters in children */
    slist_t *type_params = NULL;
    
    for (slist_t *node = class_decl->data.node.children; node; node = node->next) {
        ast_node_t *child = (ast_node_t *)node->data;
        if (!child) continue;
        
        if (child->type == AST_TYPE_PARAMETER) {
            if (!type_params) {
                type_params = slist_new(child);
            } else {
                slist_append(type_params, child);
            }
        }
    }
    
    /* Check if superclass is parameterized */
    bool has_parameterized_super = false;
    type_t *super_type = NULL;
    if (class_sym && class_sym->data.class_data.superclass_type) {
        super_type = class_sym->data.class_data.superclass_type;
        if (super_type->kind == TYPE_CLASS && super_type->data.class_type.type_args) {
            has_parameterized_super = true;
        }
    }
    
    /* If no type parameters and no parameterized superclass, no signature needed */
    if (!type_params && !has_parameterized_super) {
        return NULL;
    }
    
    string_t *sig = string_new("");
    
    /* Type parameters: <T:Ljava/lang/Object;> */
    if (type_params) {
        char *tp_sig = generate_type_params_signature(type_params);
        if (tp_sig) {
            string_append(sig, tp_sig);
            free(tp_sig);
        }
        slist_free(type_params);
    }
    
    /* Superclass signature */
    if (super_type) {
        char *super_sig = type_to_signature(super_type);
        if (super_sig) {
            string_append(sig, super_sig);
            free(super_sig);
        } else {
            string_append(sig, "Ljava/lang/Object;");
        }
    } else {
        string_append(sig, "Ljava/lang/Object;");
    }
    
    return string_free(sig, false);
}

/**
 * Generate method signature from method declaration AST.
 * Returns NULL if no signature is needed (non-generic method).
 */
char *generate_method_signature(ast_node_t *method_decl, symbol_t *method_sym)
{
    if (!method_decl) {
        return NULL;
    }
    
    /* Collect method type parameters and check if method uses generics */
    slist_t *type_params = NULL;
    bool has_generic_params = false;
    bool has_generic_return = false;
    
    /* Check for method-level type parameters */
    for (slist_t *node = method_decl->data.node.children; node; node = node->next) {
        ast_node_t *child = (ast_node_t *)node->data;
        if (child && child->type == AST_TYPE_PARAMETER) {
            if (!type_params) {
                type_params = slist_new(child);
            } else {
                slist_append(type_params, child);
            }
        }
    }
    
    /* Check if return type uses generics (type variable or has type arguments) */
    if (method_sym && method_sym->type) {
        if (method_sym->type->kind == TYPE_TYPEVAR) {
            has_generic_return = true;
        } else if (method_sym->type->kind == TYPE_CLASS && 
                   method_sym->type->data.class_type.type_args) {
            has_generic_return = true;
        } else if (method_sym->type->kind == TYPE_ARRAY && 
                   method_sym->type->data.array_type.element_type) {
            /* Check array element type */
            type_t *elem = method_sym->type->data.array_type.element_type;
            if (elem->kind == TYPE_TYPEVAR ||
                (elem->kind == TYPE_CLASS && elem->data.class_type.type_args)) {
                has_generic_return = true;
            }
        }
    }
    
    /* Check if any parameter uses generics */
    if (method_sym && method_sym->data.method_data.parameters) {
        for (slist_t *node = method_sym->data.method_data.parameters; node; node = node->next) {
            symbol_t *param = (symbol_t *)node->data;
            if (param->type) {
                if (param->type->kind == TYPE_TYPEVAR) {
                    has_generic_params = true;
                    break;
                } else if (param->type->kind == TYPE_CLASS && 
                           param->type->data.class_type.type_args) {
                    has_generic_params = true;
                    break;
                } else if (param->type->kind == TYPE_ARRAY && 
                           param->type->data.array_type.element_type) {
                    type_t *elem = param->type->data.array_type.element_type;
                    if (elem->kind == TYPE_TYPEVAR ||
                        (elem->kind == TYPE_CLASS && elem->data.class_type.type_args)) {
                        has_generic_params = true;
                        break;
                    }
                }
            }
        }
    }
    
    /* Only generate signature if method has type parameters or uses them */
    if (!type_params && !has_generic_params && !has_generic_return) {
        return NULL;
    }
    
    string_t *sig = string_new("");
    
    /* Type parameters (if method has its own) */
    if (type_params) {
        char *tp_sig = generate_type_params_signature(type_params);
        if (tp_sig) {
            string_append(sig, tp_sig);
            free(tp_sig);
        }
        slist_free(type_params);
    }
    
    /* Parameter signatures */
    string_append_c(sig, '(');
    if (method_sym && method_sym->data.method_data.parameters) {
        for (slist_t *node = method_sym->data.method_data.parameters; node; node = node->next) {
            symbol_t *param = (symbol_t *)node->data;
            if (param->type) {
                char *param_sig = type_to_signature(param->type);
                string_append(sig, param_sig);
                free(param_sig);
            }
        }
    }
    string_append_c(sig, ')');
    
    /* Return type signature - constructors always use V, not the class type */
    bool is_constructor = (method_decl->type == AST_CONSTRUCTOR_DECL) ||
                          (method_sym && method_sym->kind == SYM_CONSTRUCTOR);
    
    if (is_constructor) {
        string_append_c(sig, 'V');
    } else if (method_sym && method_sym->type) {
        char *ret_sig = type_to_signature(method_sym->type);
        string_append(sig, ret_sig);
        free(ret_sig);
    } else {
        string_append_c(sig, 'V');
    }
    
    return string_free(sig, false);
}

/**
 * Generate field signature if field type uses generics.
 * Returns NULL if no signature is needed.
 */
char *generate_field_signature(type_t *field_type)
{
    if (!field_type) {
        return NULL;
    }
    
    /* Need signature if field uses type variable */
    if (field_type->kind == TYPE_TYPEVAR) {
        return type_to_signature(field_type);
    }
    
    /* Need signature if field type is parameterized (e.g., List<String>) */
    if (field_type->kind == TYPE_CLASS && field_type->data.class_type.type_args) {
        return type_to_signature(field_type);
    }
    
    /* Check array element type */
    if (field_type->kind == TYPE_ARRAY && field_type->data.array_type.element_type) {
        type_t *elem = field_type->data.array_type.element_type;
        if (elem->kind == TYPE_TYPEVAR) {
            return type_to_signature(field_type);
        }
        /* Also check for parameterized element type */
        if (elem->kind == TYPE_CLASS && elem->data.class_type.type_args) {
            return type_to_signature(field_type);
        }
    }
    
    return NULL;
}

/*
 * Convert an AST type node to a JVM type descriptor
 */
char *ast_type_to_descriptor(ast_node_t *type_node)
{
    if (!type_node) {
        return strdup("V");
    }
    
    switch (type_node->type) {
        case AST_PRIMITIVE_TYPE:
            {
                const char *name = type_node->data.leaf.name;
                if (!name) {
                    return strdup("I");
                }
                
                if (strcmp(name, "void") == 0) {
                    return strdup("V");
                }
                if (strcmp(name, "boolean") == 0) {
                    return strdup("Z");
                }
                if (strcmp(name, "byte") == 0) {
                    return strdup("B");
                }
                if (strcmp(name, "char") == 0) {
                    return strdup("C");
                }
                if (strcmp(name, "short") == 0) {
                    return strdup("S");
                }
                if (strcmp(name, "int") == 0) {
                    return strdup("I");
                }
                if (strcmp(name, "long") == 0) {
                    return strdup("J");
                }
                if (strcmp(name, "float") == 0) {
                    return strdup("F");
                }
                if (strcmp(name, "double") == 0) {
                    return strdup("D");
                }
                return strdup("I");  /* Default to int */
            }
        
        case AST_CLASS_TYPE:
            {
                /* Check for type variable (generics) - use erased type */
                if (type_node->sem_type && type_node->sem_type->kind == TYPE_TYPEVAR) {
                    type_t *bound = type_node->sem_type->data.type_var.bound;
                    if (bound && bound->kind == TYPE_CLASS && bound->data.class_type.name) {
                        char *internal = class_to_internal_name(bound->data.class_type.name);
                        size_t len = strlen(internal) + 3;
                        char *desc = malloc(len);
                        snprintf(desc, len, "L%s;", internal);
                        free(internal);
                        return desc;
                    }
                    /* Unbounded type variable erases to Object */
                    return strdup("Ljava/lang/Object;");
                }
                
                /* Use resolved type if available (from semantic analysis) */
                const char *name = NULL;
                if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                    name = type_node->sem_type->data.class_type.name;
                }
                if (!name) {
                    name = type_node->data.node.name;
                }
                if (!name) {
                    return strdup("Ljava/lang/Object;");
                }
                
                /* Check for common java.lang types (if not already qualified) */
                if (strchr(name, '.') == NULL) {
                    if (strcmp(name, "String") == 0) {
                        return strdup("Ljava/lang/String;");
                    }
                    if (strcmp(name, "Object") == 0) {
                        return strdup("Ljava/lang/Object;");
                    }
                    if (strcmp(name, "Class") == 0) {
                        return strdup("Ljava/lang/Class;");
                    }
                    if (strcmp(name, "Number") == 0) {
                        return strdup("Ljava/lang/Number;");
                    }
                    if (strcmp(name, "Integer") == 0) {
                        return strdup("Ljava/lang/Integer;");
                    }
                    if (strcmp(name, "Boolean") == 0) {
                        return strdup("Ljava/lang/Boolean;");
                    }
                    if (strcmp(name, "Character") == 0) {
                        return strdup("Ljava/lang/Character;");
                    }
                    if (strcmp(name, "Byte") == 0) {
                        return strdup("Ljava/lang/Byte;");
                    }
                    if (strcmp(name, "Short") == 0) {
                        return strdup("Ljava/lang/Short;");
                    }
                    if (strcmp(name, "Long") == 0) {
                        return strdup("Ljava/lang/Long;");
                    }
                    if (strcmp(name, "Float") == 0) {
                        return strdup("Ljava/lang/Float;");
                    }
                    if (strcmp(name, "Double") == 0) {
                        return strdup("Ljava/lang/Double;");
                    }
                }
                
                char *internal = class_to_internal_name(name);
                size_t len = strlen(internal) + 3;
                char *desc = malloc(len);
                snprintf(desc, len, "L%s;", internal);
                free(internal);
                return desc;
            }
        
        case AST_ARRAY_TYPE:
            {
                /* Get element type */
                slist_t *children = type_node->data.node.children;
                if (children) {
                    char *elem_desc = ast_type_to_descriptor((ast_node_t *)children->data);
                    size_t len = strlen(elem_desc) + 2;
                    char *desc = malloc(len);
                    snprintf(desc, len, "[%s", elem_desc);
                    free(elem_desc);
                    return desc;
                }
                return strdup("[Ljava/lang/Object;");
            }
        
        case AST_IDENTIFIER:
            {
                /* Identifier used as type name (e.g., in String.class or String[].class) */
                const char *name = type_node->data.leaf.name;
                if (!name) {
                    return strdup("Ljava/lang/Object;");
                }
                
                /* Check for common java.lang types */
                if (strcmp(name, "String") == 0) {
                    return strdup("Ljava/lang/String;");
                }
                if (strcmp(name, "Object") == 0) {
                    return strdup("Ljava/lang/Object;");
                }
                if (strcmp(name, "Class") == 0) {
                    return strdup("Ljava/lang/Class;");
                }
                if (strcmp(name, "Number") == 0) {
                    return strdup("Ljava/lang/Number;");
                }
                if (strcmp(name, "Integer") == 0) {
                    return strdup("Ljava/lang/Integer;");
                }
                if (strcmp(name, "Boolean") == 0) {
                    return strdup("Ljava/lang/Boolean;");
                }
                if (strcmp(name, "Character") == 0) {
                    return strdup("Ljava/lang/Character;");
                }
                if (strcmp(name, "Byte") == 0) {
                    return strdup("Ljava/lang/Byte;");
                }
                if (strcmp(name, "Short") == 0) {
                    return strdup("Ljava/lang/Short;");
                }
                if (strcmp(name, "Long") == 0) {
                    return strdup("Ljava/lang/Long;");
                }
                if (strcmp(name, "Float") == 0) {
                    return strdup("Ljava/lang/Float;");
                }
                if (strcmp(name, "Double") == 0) {
                    return strdup("Ljava/lang/Double;");
                }
                if (strcmp(name, "System") == 0) {
                    return strdup("Ljava/lang/System;");
                }
                if (strcmp(name, "Math") == 0) {
                    return strdup("Ljava/lang/Math;");
                }
                if (strcmp(name, "StringBuilder") == 0) {
                    return strdup("Ljava/lang/StringBuilder;");
                }
                if (strcmp(name, "StringBuffer") == 0) {
                    return strdup("Ljava/lang/StringBuffer;");
                }
                if (strcmp(name, "Throwable") == 0) {
                    return strdup("Ljava/lang/Throwable;");
                }
                if (strcmp(name, "Exception") == 0) {
                    return strdup("Ljava/lang/Exception;");
                }
                if (strcmp(name, "RuntimeException") == 0) {
                    return strdup("Ljava/lang/RuntimeException;");
                }
                if (strcmp(name, "Error") == 0) {
                    return strdup("Ljava/lang/Error;");
                }
                
                /* Assume it's a class in the default package or needs resolution */
                char *internal = class_to_internal_name(name);
                size_t len = strlen(internal) + 3;
                char *desc = malloc(len);
                snprintf(desc, len, "L%s;", internal);
                free(internal);
                return desc;
            }
        
        case AST_FIELD_ACCESS:
            {
                /* Qualified type name like java.lang.String */
                /* Build the full qualified name from the field access chain */
                string_t *name = string_new(NULL);
                ast_node_t *node = type_node;
                
                /* Collect name parts in reverse order */
                slist_t *parts = NULL;
                while (node) {
                    if (node->type == AST_FIELD_ACCESS) {
                        if (node->data.node.name) {
                            slist_t *new_part = slist_new(strdup(node->data.node.name));
                            new_part->next = parts;
                            parts = new_part;
                        }
                        slist_t *children = node->data.node.children;
                        node = children ? (ast_node_t *)children->data : NULL;
                    } else if (node->type == AST_IDENTIFIER) {
                        slist_t *new_part = slist_new(strdup(node->data.leaf.name));
                        new_part->next = parts;
                        parts = new_part;
                        break;
                    } else {
                        break;
                    }
                }
                
                /* Build the qualified name */
                bool first = true;
                for (slist_t *p = parts; p; p = p->next) {
                    if (!first) {
                        string_append_c(name, '/');
                    }
                    string_append(name, (char *)p->data);
                    free(p->data);
                    first = false;
                }
                slist_free(parts);
                
                char *full_name = string_free(name, false);
                size_t len = strlen(full_name) + 3;
                char *desc = malloc(len);
                snprintf(desc, len, "L%s;", full_name);
                free(full_name);
                return desc;
            }
        
        default:
            return strdup("Ljava/lang/Object;");
    }
}

/**
 * Extract throws clause exception types from a method AST node.
 * Returns a list of internal class names (e.g., "java/io/IOException").
 * The caller owns the returned list and its strings.
 */
static slist_t *extract_throws_types(ast_node_t *method_decl, semantic_t *sem)
{
    if (!method_decl) {
        return NULL;
    }
    
    slist_t *throws_list = NULL;
    
    /* Throws types are children with flags=0xFFFF */
    for (slist_t *child = method_decl->data.node.children; child; child = child->next) {
        ast_node_t *node = (ast_node_t *)child->data;
        if (node && node->data.node.flags == 0xFFFF) {
            /* This is a throws type node - get the internal class name */
            char *internal_name = NULL;
            
            /* Try to get fully qualified name from resolved sem_type first */
            if (node->sem_type && node->sem_type->kind == TYPE_CLASS && 
                node->sem_type->data.class_type.name) {
                const char *fqn = node->sem_type->data.class_type.name;
                /* Convert dots to slashes for internal format */
                size_t len = strlen(fqn);
                internal_name = malloc(len + 1);
                if (internal_name) {
                    for (size_t i = 0; i <= len; i++) {
                        internal_name[i] = (fqn[i] == '.') ? '/' : fqn[i];
                    }
                }
            } else if (sem) {
                /* Use semantic analyzer to resolve the type */
                type_t *resolved = semantic_resolve_type(sem, node);
                if (resolved && resolved->kind == TYPE_CLASS && resolved->data.class_type.name) {
                    const char *fqn = resolved->data.class_type.name;
                    /* Convert dots to slashes for internal format */
                    size_t len = strlen(fqn);
                    internal_name = malloc(len + 1);
                    if (internal_name) {
                        for (size_t i = 0; i <= len; i++) {
                            internal_name[i] = (fqn[i] == '.') ? '/' : fqn[i];
                        }
                    }
                }
            }
            
            /* Fallback to ast_type_to_descriptor if still not resolved */
            if (!internal_name) {
                char *desc = ast_type_to_descriptor(node);
                if (desc && desc[0] == 'L') {
                    /* Convert "Ljava/io/IOException;" to "java/io/IOException" */
                    size_t len = strlen(desc);
                    internal_name = malloc(len - 1);  /* -2 for L; +1 for null */
                    if (internal_name) {
                        strncpy(internal_name, desc + 1, len - 2);
                        internal_name[len - 2] = '\0';
                    }
                }
                free(desc);
            }
            
            if (internal_name) {
                if (!throws_list) {
                    throws_list = slist_new(internal_name);
                } else {
                    slist_append(throws_list, internal_name);
                }
            }
        }
    }
    
    return throws_list;
}

/* ========================================================================
 * invokedynamic Support
 * ======================================================================== */

/**
 * Ensure the LambdaMetafactory bootstrap method is registered.
 * Creates bootstrap_methods table if needed.
 * Returns the BSM index for LambdaMetafactory.metafactory.
 */
int cg_ensure_lambda_metafactory(class_gen_t *cg)
{
    if (!cg) {
        return -1;
    }
    
    /* Return cached index if already set up */
    if (cg->lambda_metafactory_bsm_idx >= 0) {
        return cg->lambda_metafactory_bsm_idx;
    }
    
    /* Create bootstrap methods table on demand */
    if (!cg->bootstrap_methods) {
        cg->bootstrap_methods = bootstrap_methods_new();
        if (!cg->bootstrap_methods) {
            return -1;
        }
        
        /* Pre-add BootstrapMethods attribute name */
        cp_add_utf8(cg->cp, "BootstrapMethods");
    }
    
    /* Add the LambdaMetafactory bootstrap method */
    cg->lambda_metafactory_bsm_idx = indy_add_lambda_metafactory(cg->cp, cg->bootstrap_methods);
    if (cg->lambda_metafactory_bsm_idx >= 0) {
        cg->uses_invokedynamic = true;
    }
    
    return cg->lambda_metafactory_bsm_idx;
}

/**
 * Emit an invokedynamic instruction.
 * The instruction format is: invokedynamic indexbyte1 indexbyte2 0 0
 */
void mg_emit_invokedynamic(method_gen_t *mg, uint16_t indy_cp_index)
{
    if (!mg || !mg->code) {
        return;
    }
    
    bc_emit_u1(mg->code, OP_INVOKEDYNAMIC);
    bc_emit_u2(mg->code, indy_cp_index);
    bc_emit_u1(mg->code, 0);  /* Must be zero */
    bc_emit_u1(mg->code, 0);  /* Must be zero */
    
    /* Mark class as using invokedynamic */
    if (mg->class_gen) {
        mg->class_gen->uses_invokedynamic = true;
    }
}

/* ========================================================================
 * Class Generation Implementation
 * ======================================================================== */

class_gen_t *class_gen_new(semantic_t *sem, symbol_t *class_sym)
{
    class_gen_t *cg = calloc(1, sizeof(class_gen_t));
    if (!cg) {
        return NULL;
    }
    
    cg->cp = const_pool_new();
    cg->sem = sem;
    cg->class_sym = class_sym;
    cg->field_map = hashtable_new();
    
    /* Default target version (can be overridden via cg_set_target_version) */
    cg->target_version = 45;  /* Java 1.1 default */
    
    /* Pre-add standard attribute names to constant pool */
    cp_add_utf8(cg->cp, "Code");
    cp_add_utf8(cg->cp, "LineNumberTable");
    cp_add_utf8(cg->cp, "LocalVariableTable");
    cp_add_utf8(cg->cp, "SourceFile");
    cp_add_utf8(cg->cp, "InnerClasses");
    
    if (class_sym) {
        /* Set up class info */
        /* Note: ACC_STATIC is only valid in InnerClasses attribute, not in class access_flags
         * Convert MOD_ flags to ACC_ flags (they have different values!) */
        cg->access_flags = mods_to_access_flags(class_sym->modifiers) & ~ACC_STATIC;
        
        /* Check if this is an interface or annotation */
        bool is_interface = (class_sym->kind == SYM_INTERFACE);
        bool is_annotation = (class_sym->kind == SYM_ANNOTATION);
        
        if (is_interface || is_annotation) {
            /* Interfaces and annotations must have ACC_INTERFACE and ACC_ABSTRACT flags */
            cg->access_flags |= ACC_INTERFACE | ACC_ABSTRACT;
            if (is_annotation) {
                cg->access_flags |= ACC_ANNOTATION;
            }
        } else {
            /* Add ACC_SUPER flag (always set in modern class files for classes) */
            cg->access_flags |= 0x0020;
        }
        
        cg->internal_name = class_to_internal_name(class_sym->qualified_name);
        cg->this_class = cp_add_class(cg->cp, cg->internal_name);
        
        /* Superclass - default to java/lang/Object */
        cg->superclass = strdup("java/lang/Object");
        cg->super_class = cp_add_class(cg->cp, cg->superclass);
        
        if (class_sym->data.class_data.superclass) {
            free(cg->superclass);
            cg->superclass = class_to_internal_name(
                class_sym->data.class_data.superclass->qualified_name);
            cg->super_class = cp_add_class(cg->cp, cg->superclass);
        }
        
        /* Add implemented interfaces to the interfaces list */
        slist_t *iface_list = class_sym->data.class_data.interfaces;
        while (iface_list) {
            symbol_t *iface_sym = (symbol_t *)iface_list->data;
            if (iface_sym && iface_sym->qualified_name) {
                char *iface_internal = class_to_internal_name(iface_sym->qualified_name);
                uint16_t iface_index = cp_add_class(cg->cp, iface_internal);
                free(iface_internal);
                
                /* Store as uintptr_t to fit in void* */
                if (!cg->interfaces) {
                    cg->interfaces = slist_new((void *)(uintptr_t)iface_index);
                } else {
                    slist_append(cg->interfaces, (void *)(uintptr_t)iface_index);
                }
            }
            iface_list = iface_list->next;
        }
        
        /* Annotations implicitly extend java.lang.annotation.Annotation */
        if (is_annotation) {
            uint16_t annot_iface = cp_add_class(cg->cp, "java/lang/annotation/Annotation");
            if (!cg->interfaces) {
                cg->interfaces = slist_new((void *)(uintptr_t)annot_iface);
            } else {
                slist_append(cg->interfaces, (void *)(uintptr_t)annot_iface);
            }
        }
        
        /* Check if this is an inner class (non-static nested class)
         * Note: Interfaces and annotations are implicitly static, so they can't be inner classes
         * Also: Local classes defined in static methods don't have an outer instance */
        symbol_t *enclosing = class_sym->data.class_data.enclosing_class;
        bool is_local_in_static_method = class_sym->data.class_data.is_local_class &&
                                          class_sym->data.class_data.enclosing_method &&
                                          (class_sym->data.class_data.enclosing_method->modifiers & MOD_STATIC);
        if (enclosing && !(class_sym->modifiers & MOD_STATIC) && !is_interface && !is_annotation &&
            !is_local_in_static_method) {
            cg->is_inner_class = true;
            cg->outer_class_internal = class_to_internal_name(enclosing->qualified_name);
            
            /* Check if superclass is a non-static inner class.
             * If so, we don't need our own this$0 - the superclass will have it.
             * We just pass the outer instance to the superclass constructor. */
            bool super_is_inner = false;
            if (class_sym->data.class_data.superclass) {
                symbol_t *super_sym = class_sym->data.class_data.superclass;
                if (super_sym->data.class_data.enclosing_class &&
                    !(super_sym->modifiers & MOD_STATIC)) {
                    super_is_inner = true;
                }
            }
            
            /* Add synthetic this$0 field only if superclass is not an inner class */
            if (!super_is_inner) {
                field_gen_t *this0 = calloc(1, sizeof(field_gen_t));
                this0->access_flags = ACC_FINAL | ACC_SYNTHETIC;
                this0->name = strdup("this$0");
                
                /* Descriptor is LOuterClass; */
                size_t desc_len = strlen(cg->outer_class_internal) + 3;
                this0->descriptor = malloc(desc_len);
                snprintf(this0->descriptor, desc_len, "L%s;", cg->outer_class_internal);
                
                this0->name_index = cp_add_utf8(cg->cp, this0->name);
                this0->descriptor_index = cp_add_utf8(cg->cp, this0->descriptor);
                
                /* Add to field list */
                if (!cg->fields) {
                    cg->fields = slist_new(this0);
                } else {
                    slist_append(cg->fields, this0);
                }
                
                hashtable_insert(cg->field_map, this0->name, this0);
                
                /* Store field reference for later use */
                cg->this_dollar_zero_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                                                            "this$0", this0->descriptor);
            }
        }
        
        /* Check if this is a local or anonymous class with captured variables */
        if (class_sym->data.class_data.is_local_class || 
            class_sym->data.class_data.is_anonymous_class) {
            cg->is_local_class = class_sym->data.class_data.is_local_class;
            cg->is_anonymous_class = class_sym->data.class_data.is_anonymous_class;
            cg->captured_vars = class_sym->data.class_data.captured_vars;
            
            /* If superclass is also a local class with captured vars, we need to
             * inherit those captures so we can pass them to the superclass constructor */
            symbol_t *super_sym = class_sym->data.class_data.superclass;
            if (super_sym && super_sym->data.class_data.is_local_class &&
                super_sym->data.class_data.captured_vars) {
                for (slist_t *cap = super_sym->data.class_data.captured_vars; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (!var_sym) continue;
                    
                    /* Check if already in our captured_vars */
                    bool already_captured = false;
                    for (slist_t *n = cg->captured_vars; n; n = n->next) {
                        if (n->data == var_sym) {
                            already_captured = true;
                            break;
                        }
                    }
                    
                    /* Add superclass's captured var to our list */
                    if (!already_captured) {
                        if (!cg->captured_vars) {
                            cg->captured_vars = slist_new(var_sym);
                        } else {
                            slist_append(cg->captured_vars, var_sym);
                        }
                    }
                }
            }
            
            cg->captured_field_refs = hashtable_new();
            
            /* Add synthetic val$xxx fields for each captured variable */
            for (slist_t *node = cg->captured_vars; node; node = node->next) {
                symbol_t *var_sym = (symbol_t *)node->data;
                if (!var_sym || !var_sym->name) continue;
                
                /* Create field name val$varname */
                char field_name[256];
                snprintf(field_name, sizeof(field_name), "val$%s", var_sym->name);
                
                field_gen_t *cap_field = calloc(1, sizeof(field_gen_t));
                cap_field->access_flags = ACC_FINAL | ACC_SYNTHETIC;
                cap_field->name = strdup(field_name);
                
                /* Get descriptor from variable's type */
                if (var_sym->type) {
                    cap_field->descriptor = type_to_descriptor(var_sym->type);
                } else {
                    cap_field->descriptor = strdup("Ljava/lang/Object;");
                }
                
                cap_field->name_index = cp_add_utf8(cg->cp, cap_field->name);
                cap_field->descriptor_index = cp_add_utf8(cg->cp, cap_field->descriptor);
                
                /* Add to field list */
                if (!cg->fields) {
                    cg->fields = slist_new(cap_field);
                } else {
                    slist_append(cg->fields, cap_field);
                }
                
                hashtable_insert(cg->field_map, cap_field->name, cap_field);
                
                /* Store field reference for later use */
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                                                     field_name, cap_field->descriptor);
                /* Store as uintptr_t to fit in void* */
                hashtable_insert(cg->captured_field_refs, var_sym->name, 
                               (void *)(uintptr_t)field_ref);
            }
        }
        
        /* For any nested class (static or inner), add InnerClasses entry for itself.
         * For local classes, outer_class_info = 0 (per JVM spec - local classes
         * are not members of their enclosing class in the InnerClasses sense). */
        if (enclosing) {
            inner_class_entry_t *entry = calloc(1, sizeof(inner_class_entry_t));
            entry->inner_class_info = cg->this_class;
            
            /* Local classes have outer_class_info = 0 in InnerClasses attribute */
            if (class_sym->data.class_data.is_local_class) {
                entry->outer_class_info = 0;
            } else {
                entry->outer_class_info = cp_add_class(cg->cp, cg->outer_class_internal ? 
                    cg->outer_class_internal : class_to_internal_name(enclosing->qualified_name));
            }
            entry->inner_name = cp_add_utf8(cg->cp, class_sym->name);
            entry->access_flags = class_sym->modifiers & 0x001F;  /* ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED | ACC_STATIC | ACC_FINAL */
            
            cg->inner_class_entries = slist_new(entry);
            
            /* NestHost attribute (Java 11+): Find the top-level enclosing class */
            symbol_t *nest_host = enclosing;
            while (nest_host->data.class_data.enclosing_class) {
                nest_host = nest_host->data.class_data.enclosing_class;
            }
            char *nest_host_internal = class_to_internal_name(nest_host->qualified_name);
            cg->nest_host = cp_add_class(cg->cp, nest_host_internal);
            free(nest_host_internal);
        }
    }
    
    /* Initialize invokedynamic support */
    cg->bootstrap_methods = NULL;  /* Created on demand */
    cg->uses_invokedynamic = false;
    cg->lambda_metafactory_bsm_idx = -1;
    
    return cg;
}

void class_gen_set_target_version(class_gen_t *cg, int major_version)
{
    if (cg) {
        cg->target_version = major_version;
    }
}

void class_gen_free(class_gen_t *cg)
{
    if (!cg) {
        return;
    }
    
    const_pool_free(cg->cp);
    free(cg->internal_name);
    free(cg->superclass);
    free(cg->outer_class_internal);
    free(cg->signature);
    hashtable_free(cg->field_map);
    hashtable_free(cg->captured_field_refs);
    slist_free(cg->interfaces);
    
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_gen_t *fg = (field_gen_t *)node->data;
        free(fg->name);
        free(fg->descriptor);
        free(fg->signature);
        slist_free(fg->attributes);
        free(fg);
    }
    slist_free(cg->fields);
    
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mg = (method_info_gen_t *)node->data;
        bytecode_free(mg->code);
        free(mg->signature);
        slist_free(mg->exception_handlers);
        slist_free(mg->attributes);
        free(mg);
    }
    slist_free(cg->methods);
    
    /* Free inner class entries */
    for (slist_t *node = cg->inner_class_entries; node; node = node->next) {
        free(node->data);
    }
    slist_free(cg->inner_class_entries);
    
    /* Free nest members */
    for (slist_t *node = cg->nest_members; node; node = node->next) {
        free(node->data);
    }
    slist_free(cg->nest_members);
    
    slist_free(cg->nested_classes);
    slist_free(cg->local_classes);
    slist_free(cg->anonymous_classes);
    slist_free(cg->instance_field_inits);
    slist_free(cg->instance_initializers);
    
    /* Free invokedynamic support */
    if (cg->bootstrap_methods) {
        bootstrap_methods_free(cg->bootstrap_methods);
    }
    
    slist_free(cg->attributes);
    free(cg->source_file);
    free(cg);
}


/* ========================================================================
 * Interface Method Generation (abstract methods with no body)
 * ======================================================================== */

/**
 * Generate an abstract interface method (no body).
 * For default/static methods, use codegen_method() instead.
 */
static bool codegen_interface_method(class_gen_t *cg, ast_node_t *method_decl)
{
    if (!cg || !method_decl) {
        return false;
    }
    
    const char *name = method_decl->data.node.name;
    
    /* Build method descriptor from AST: (params)ReturnType */
    string_t *desc = string_new("(");
    
    /* Return type is stored in data.node.extra */
    ast_node_t *return_type = (ast_node_t *)method_decl->data.node.extra;
    
    /* Parameters are children */
    slist_t *children = method_decl->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child->type == AST_PARAMETER) {
            /* Get parameter type from first child */
            slist_t *pchildren = child->data.node.children;
            if (pchildren) {
                char *param_desc = ast_type_to_descriptor((ast_node_t *)pchildren->data);
                string_append(desc, param_desc);
                free(param_desc);
            }
        }
        children = children->next;
    }
    
    string_append(desc, ")");
    
    /* Add return type */
    if (return_type) {
        char *ret_desc = ast_type_to_descriptor(return_type);
        string_append(desc, ret_desc);
        free(ret_desc);
    } else {
        string_append(desc, "V");  /* Default to void */
    }
    
    /* Create method info */
    method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
    if (!mi) {
        string_free(desc, true);
        return false;
    }
    
    /* Interface methods are implicitly public abstract 
     * Convert MOD_ flags to ACC_ flags before adding ACC_PUBLIC and ACC_ABSTRACT */
    mi->access_flags = mods_to_access_flags(method_decl->data.node.flags) | ACC_PUBLIC | ACC_ABSTRACT;
    mi->name_index = cp_add_utf8(cg->cp, name);
    mi->descriptor_index = cp_add_utf8(cg->cp, desc->str);
    mi->code = NULL;  /* Abstract methods have no Code attribute */
    mi->ast = method_decl;  /* Store AST for annotations and defaults */
    mi->throws = extract_throws_types(method_decl, cg->sem);  /* Extract throws clause */
    
    string_free(desc, true);
    
    /* Add to methods list */
    if (!cg->methods) {
        cg->methods = slist_new(mi);
    } else {
        slist_append(cg->methods, mi);
    }
    
    return true;
}


/* ========================================================================
 * Abstract Method Generation (for abstract classes)
 * ======================================================================== */

static bool codegen_abstract_method(class_gen_t *cg, ast_node_t *method_decl)
{
    if (!cg || !method_decl) {
        return false;
    }
    
    const char *name = method_decl->data.node.name;
    
    /* Build method descriptor from AST: (params)ReturnType */
    string_t *desc = string_new("(");
    
    /* Return type is stored in data.node.extra */
    ast_node_t *return_type = (ast_node_t *)method_decl->data.node.extra;
    
    /* Parameters are children */
    slist_t *children = method_decl->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child->type == AST_PARAMETER) {
            /* Get parameter type from first child */
            slist_t *pchildren = child->data.node.children;
            if (pchildren) {
                char *param_desc = ast_type_to_descriptor((ast_node_t *)pchildren->data);
                string_append(desc, param_desc);
                free(param_desc);
            }
        }
        children = children->next;
    }
    
    string_append(desc, ")");
    
    /* Add return type */
    if (return_type) {
        char *ret_desc = ast_type_to_descriptor(return_type);
        string_append(desc, ret_desc);
        free(ret_desc);
    } else {
        string_append(desc, "V");  /* Default to void */
    }
    
    /* Create method info */
    method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
    if (!mi) {
        string_free(desc, true);
        return false;
    }
    
    /* Keep declared modifiers - convert MOD_ flags to ACC_ flags
     * For abstract methods, ensure ACC_ABSTRACT is set
     * For native methods, ACC_NATIVE is already set by mods_to_access_flags */
    mi->access_flags = mods_to_access_flags(method_decl->data.node.flags);
    if (method_decl->data.node.flags & MOD_ABSTRACT) {
        mi->access_flags |= ACC_ABSTRACT;
    }
    mi->name_index = cp_add_utf8(cg->cp, name);
    mi->descriptor_index = cp_add_utf8(cg->cp, desc->str);
    mi->code = NULL;  /* Abstract/native methods have no Code attribute */
    mi->ast = method_decl;  /* Store AST for annotations */
    mi->throws = extract_throws_types(method_decl, cg->sem);  /* Extract throws clause */
    
    string_free(desc, true);
    
    /* Add to methods list */
    if (!cg->methods) {
        cg->methods = slist_new(mi);
    } else {
        slist_append(cg->methods, mi);
    }
    
    return true;
}


/* ========================================================================
 * Method Code Generation
 * ======================================================================== */

bool codegen_method(class_gen_t *cg, ast_node_t *method_decl)
{
    if (!cg || !method_decl) {
        return false;
    }
    
    /* Get method symbol from semantic analysis result */
    const char *name = method_decl->data.node.name;
    symbol_t *method_sym = method_decl->sem_symbol;
    
    /* Fallback to lookup if sem_symbol not set (shouldn't happen normally) */
    if (!method_sym && cg->class_sym && cg->class_sym->data.class_data.members) {
        method_sym = scope_lookup_local(cg->class_sym->data.class_data.members, name);
    }
    
    /* Create method generator */
    method_gen_t *mg = method_gen_new(cg, method_sym);
    if (!mg) {
        return false;
    }
    
    /* For inner class constructors, slot 1 is the outer instance (implicit parameter) */
    bool is_constructor = (method_decl->type == AST_CONSTRUCTOR_DECL);
    bool is_static = (method_decl->data.node.flags & MOD_STATIC) != 0;
    
    /* For enum constructors, slots 1-2 are name (String) and ordinal (int) */
    if (is_constructor && cg->class_sym && cg->class_sym->kind == SYM_ENUM) {
        mg->next_slot = 3;  /* Skip slot 1 (name) and slot 2 (ordinal) */
        mg->max_locals = 3;
    }
    
    if (is_constructor && cg->is_inner_class) {
        mg->next_slot = 2;  /* Skip slot 1 (outer instance) */
        mg->max_locals = 2;
    }
    
    /* For local/anonymous class constructors, account for captured variable parameters */
    if (is_constructor && (cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars) {
        for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
            symbol_t *var_sym = (symbol_t *)cap->data;
            if (var_sym && var_sym->type) {
                int size = (var_sym->type->kind == TYPE_LONG || 
                           var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                mg->next_slot += size;
                mg->max_locals = mg->next_slot;
            }
        }
    }
    
    /* Record 'this' for instance methods (slot 0) */
    if (!is_static && cg->internal_name) {
        char *this_desc = calloc(1, strlen(cg->internal_name) + 3);
        sprintf(this_desc, "L%s;", cg->internal_name);
        mg_record_local_var(mg, "this", this_desc, 0, 0, NULL);
        free(this_desc);
    }
    
    /* Record outer instance for inner class constructors (slot 1) */
    if (is_constructor && cg->is_inner_class && cg->outer_class_internal) {
        char *outer_desc = calloc(1, strlen(cg->outer_class_internal) + 3);
        sprintf(outer_desc, "L%s;", cg->outer_class_internal);
        mg_record_local_var(mg, "this$0", outer_desc, 1, 0, NULL);
        free(outer_desc);
    }
    
    /* Allocate slots for parameters */
    slist_t *children = method_decl->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child->type == AST_PARAMETER) {
            /* Determine parameter type kind */
            type_kind_t param_kind = TYPE_INT;  /* Default */
            bool is_ref = false;
            bool is_array = false;
            bool is_varargs = (child->data.node.flags & MOD_VARARGS) != 0;
            ast_node_t *param_type_node = NULL;
            slist_t *param_children = child->data.node.children;
            if (param_children) {
                ast_node_t *type_node = (ast_node_t *)param_children->data;
                param_type_node = type_node;
                if (type_node->type == AST_ARRAY_TYPE) {
                    is_ref = true;
                    is_array = true;
                    param_kind = TYPE_ARRAY;
                } else if (type_node->type == AST_CLASS_TYPE) {
                    is_ref = true;
                    param_kind = TYPE_CLASS;
                    /* For varargs, the type is actually an array of this class */
                    if (is_varargs) {
                        is_array = true;
                        param_kind = TYPE_ARRAY;
                    }
                } else if (type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = type_node->data.leaf.name;
                    if (prim_name) {
                        if (strcmp(prim_name, "long") == 0) {
                            param_kind = TYPE_LONG;
                        } else if (strcmp(prim_name, "double") == 0) {
                            param_kind = TYPE_DOUBLE;
                        } else if (strcmp(prim_name, "float") == 0) {
                            param_kind = TYPE_FLOAT;
                        } else if (strcmp(prim_name, "boolean") == 0) {
                            param_kind = TYPE_BOOLEAN;
                        } else if (strcmp(prim_name, "byte") == 0) {
                            param_kind = TYPE_BYTE;
                        } else if (strcmp(prim_name, "short") == 0) {
                            param_kind = TYPE_SHORT;
                        } else if (strcmp(prim_name, "char") == 0) {
                            param_kind = TYPE_CHAR;
                        }
                        /* else int is the default */
                    }
                    /* For varargs with primitive type, convert to array */
                    if (is_varargs) {
                        is_ref = true;
                        is_array = true;
                        param_kind = TYPE_ARRAY;
                    }
                }
            }
            
            /* Allocate slot (long/double take 2 slots) */
            uint16_t slot = mg->next_slot;
            int slot_size = (param_kind == TYPE_LONG || param_kind == TYPE_DOUBLE) ? 2 : 1;
            mg->next_slot += slot_size;
            if (mg->next_slot > mg->max_locals) {
                mg->max_locals = mg->next_slot;
            }
            
            /* Create local variable info for this parameter */
            local_var_info_t *param_info = local_var_info_new(slot, param_kind);
            if (param_info) {
                param_info->is_ref = is_ref;
                param_info->is_array = is_array;
                
                /* For class types, also store the class name */
                const char *class_name = NULL;
                if (is_ref && param_kind == TYPE_CLASS && param_type_node && param_type_node->type == AST_CLASS_TYPE) {
                    /* Try sem_type first (has qualified name from semantic analysis) */
                    if (param_type_node->sem_type) {
                        type_t *resolved = param_type_node->sem_type;
                        /* Handle type variables by using their bound type */
                        if (resolved->kind == TYPE_TYPEVAR && resolved->data.type_var.bound) {
                            resolved = resolved->data.type_var.bound;
                        }
                        if (resolved->kind == TYPE_CLASS) {
                            class_name = resolved->data.class_type.name;
                        }
                    }
                    /* Fallback to AST node name (only if not a type variable) */
                    if (!class_name) {
                        class_name = param_type_node->data.node.name;
                        /* Check if this is a single letter which might be a type variable
                         * and default to Object if so */
                        if (class_name && strlen(class_name) == 1 && class_name[0] >= 'A' && class_name[0] <= 'Z') {
                            class_name = "java.lang.Object";
                        }
                    }
                    if (class_name) {
                        param_info->class_name = class_to_internal_name(class_name);
                    }
                }
                
                hashtable_insert(mg->locals, child->data.node.name, param_info);
            }
            
            /* Record parameter in LocalVariableTable and update StackMapTable */
            if (param_type_node) {
                char *param_desc = ast_type_to_descriptor(param_type_node);
                
                /* For varargs, convert element type descriptor to array descriptor */
                if (is_varargs && param_desc[0] != '[') {
                    /* Prepend '[' to make it an array descriptor */
                    size_t len = strlen(param_desc);
                    char *array_desc = malloc(len + 2);
                    array_desc[0] = '[';
                    strcpy(array_desc + 1, param_desc);
                    free(param_desc);
                    param_desc = array_desc;
                }
                
                mg_record_local_var(mg, child->data.node.name, param_desc, slot, 0, param_type_node);
                
                /* Update StackMapTable for this parameter using the actual type descriptor */
                if (mg->stackmap) {
                    if (is_array || (is_ref && param_desc[0] == 'L')) {
                        /* For arrays and object types, use the descriptor (strip trailing ;) */
                        char *stackmap_type;
                        if (param_desc[0] == 'L') {
                            /* Object type: L...;  -> strip L and ; to get internal name */
                            size_t len = strlen(param_desc);
                            stackmap_type = malloc(len - 1);
                            strncpy(stackmap_type, param_desc + 1, len - 2);
                            stackmap_type[len - 2] = '\0';
                        } else {
                            /* Array type: use as-is */
                            stackmap_type = strdup(param_desc);
                        }
                        stackmap_set_local_object(mg->stackmap, slot, mg->cp, stackmap_type);
                        free(stackmap_type);
                    } else {
                        switch (param_kind) {
                            case TYPE_LONG:   stackmap_set_local_long(mg->stackmap, slot); break;
                            case TYPE_DOUBLE: stackmap_set_local_double(mg->stackmap, slot); break;
                            case TYPE_FLOAT:  stackmap_set_local_float(mg->stackmap, slot); break;
                            default:          stackmap_set_local_int(mg->stackmap, slot); break;
                        }
                    }
                }
                
                free(param_desc);
            }
        }
        children = children->next;
    }
    
    /* Find method body (block) */
    ast_node_t *body_block = NULL;
    children = method_decl->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child->type == AST_BLOCK) {
            body_block = child;
            break;
        }
        children = children->next;
    }
    
    /* For constructors, add implicit super() call if no explicit this()/super() */
    if (mg->is_constructor) {
        /* For inner classes, first store this$0 from the outer instance parameter */
        if (cg->is_inner_class && cg->this_dollar_zero_ref) {
            /* aload_0 (this) */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            /* aload_1 (outer instance - first parameter) */
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            /* putfield this$0 */
            bc_emit(mg->code, OP_PUTFIELD);
            bc_emit_u2(mg->code, cg->this_dollar_zero_ref);
            mg_pop(mg, 2);  /* Consumes this + outer */
        }
        
        /* For local/anonymous classes, store captured variables from constructor parameters */
        if ((cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars && cg->captured_field_refs) {
            /* Captured vars come after outer instance in constructor parameters */
            uint16_t slot = cg->is_inner_class ? 2 : 1;  /* Skip this + outer (if present) */
            
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (!var_sym || !var_sym->name) continue;
                
                /* Get the field ref for this captured variable */
                void *ref_ptr = hashtable_lookup(cg->captured_field_refs, var_sym->name);
                if (!ref_ptr) continue;
                uint16_t field_ref = (uint16_t)(uintptr_t)ref_ptr;
                
                /* aload_0 (this) */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                
                /* Load captured value from parameter slot */
                type_kind_t kind = TYPE_CLASS;  /* Default to reference */
                if (var_sym->type) {
                    kind = var_sym->type->kind;
                }
                
                /* Emit appropriate load instruction */
                if (kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_LLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_DLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_FLOAT) {
                    bc_emit(mg->code, OP_FLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
                    /* Reference types including type variables (erase to Object) */
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else {
                    /* Integer types (int, short, byte, char, boolean) */
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                }
                
                /* putfield val$xxx */
                bc_emit(mg->code, OP_PUTFIELD);
                bc_emit_u2(mg->code, field_ref);
                mg_pop(mg, kind == TYPE_LONG || kind == TYPE_DOUBLE ? 3 : 2);
            }
        }
        
        bool has_explicit_ctor_call = false;
        
        /* Check if first statement is this() or super() */
        if (body_block && body_block->data.node.children) {
            ast_node_t *first_stmt = (ast_node_t *)body_block->data.node.children->data;
            /* The first statement might be an expression statement containing the call */
            if (first_stmt->type == AST_EXPR_STMT && first_stmt->data.node.children) {
                ast_node_t *expr = (ast_node_t *)first_stmt->data.node.children->data;
                if (expr->type == AST_EXPLICIT_CTOR_CALL) {
                    has_explicit_ctor_call = true;
                }
            } else if (first_stmt->type == AST_EXPLICIT_CTOR_CALL) {
                has_explicit_ctor_call = true;
            }
        }
        
        if (!has_explicit_ctor_call) {
            /* Load 'this' and call superclass.<init>() */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            
            if (cg->class_sym && cg->class_sym->kind == SYM_ENUM) {
                /* For enums: super(name, ordinal) - name is slot 1, ordinal is slot 2 */
                bc_emit(mg->code, OP_ALOAD_1);  /* name */
                mg_push(mg, 1);
                bc_emit(mg->code, OP_ILOAD_2);  /* ordinal */
                mg_push(mg, 1);
                uint16_t init_ref = cp_add_methodref(cg->cp, cg->superclass, 
                                                      "<init>", "(Ljava/lang/String;I)V");
                bc_emit(mg->code, OP_INVOKESPECIAL);
                bc_emit_u2(mg->code, init_ref);
                mg_pop(mg, 3);  /* Consumes 'this', name, ordinal */
                
                /* Mark 'this' as initialized in stackmap after super() */
                if (mg->stackmap && cg->internal_name) {
                    stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
                }
            } else {
                uint16_t init_ref = cp_add_methodref(cg->cp, cg->superclass, 
                                                      "<init>", "()V");
                bc_emit(mg->code, OP_INVOKESPECIAL);
                bc_emit_u2(mg->code, init_ref);
                mg_pop(mg, 1);  /* Consumes 'this' */
            }
            
            /* Mark 'this' as initialized in stackmap after super() */
            if (mg->stackmap && cg->internal_name) {
                stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
            }
        }
        
        /* Inject instance field initializers (after super() call) */
        for (slist_t *node = cg->instance_field_inits; node; node = node->next) {
            ast_node_t *assign = (ast_node_t *)node->data;
            slist_t *assign_children = assign->data.node.children;
            if (assign_children && assign_children->next) {
                ast_node_t *field_id = (ast_node_t *)assign_children->data;
                ast_node_t *init_expr = (ast_node_t *)assign_children->next->data;
                const char *field_name = field_id->data.leaf.name;
                
                /* Look up field descriptor */
                field_gen_t *field = hashtable_lookup(cg->field_map, field_name);
                if (field) {
                    /* aload_0 (this) */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push(mg, 1);
                    
                    /* Generate initializer expression */
                    codegen_expr(mg, init_expr, cg->cp);
                    
                    /* putfield */
                    uint16_t field_ref = cp_add_fieldref(cg->cp,
                        cg->internal_name, field_name, field->descriptor);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    mg_pop(mg, 2);  /* Consumes this + value */
                }
            }
        }
        
        /* Inject instance initializer blocks (after field initializers) */
        for (slist_t *node = cg->instance_initializers; node; node = node->next) {
            ast_node_t *init_block = (ast_node_t *)node->data;
            if (init_block->data.node.children) {
                ast_node_t *block = (ast_node_t *)init_block->data.node.children->data;
                codegen_statement(mg, block);
            }
        }
    }
    
    /* Generate method body */
    if (body_block) {
        if (!codegen_statement(mg, body_block)) {
            method_gen_free(mg);
            return false;
        }
    }
    
    /* Add implicit return for void methods and constructors only.
     * For non-void methods, OP_RETURN is invalid - they must return via
     * ireturn/lreturn/freturn/dreturn/areturn. If all paths in a non-void
     * method return properly, we don't add anything. */
    ast_node_t *return_type_node = (ast_node_t *)method_decl->data.node.extra;
    bool is_void_method = (return_type_node == NULL) || 
        (return_type_node->type == AST_PRIMITIVE_TYPE && 
         return_type_node->data.leaf.name &&
         strcmp(return_type_node->data.leaf.name, "void") == 0);
    
    if ((is_void_method || is_constructor) &&
        mg->last_opcode != OP_RETURN && mg->last_opcode != OP_IRETURN &&
        mg->last_opcode != OP_LRETURN && mg->last_opcode != OP_FRETURN &&
        mg->last_opcode != OP_DRETURN && mg->last_opcode != OP_ARETURN &&
        mg->last_opcode != OP_ATHROW) {
        bc_emit(mg->code, OP_RETURN);
    }
    
    /* Update max_stack and max_locals */
    mg->code->max_stack = mg->max_stack;
    mg->code->max_locals = mg->max_locals;
    
    /* Create method info */
    method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
    /* Convert MOD_ flags to ACC_ flags (they have different values!)
     * Mask out non-JVM modifiers (default, sealed, non-sealed are Java language keywords only) */
    mi->access_flags = mods_to_access_flags(method_decl->data.node.flags & ~(MOD_DEFAULT | MOD_SEALED | MOD_NON_SEALED));
    /* Default and static methods in interfaces are implicitly public (unless private) */
    bool is_in_interface = (cg->class_sym && cg->class_sym->kind == SYM_INTERFACE);
    bool is_in_annotation = (cg->class_sym && cg->class_sym->kind == SYM_ANNOTATION);
    bool is_private = (method_decl->data.node.flags & MOD_PRIVATE) != 0;
    if (!is_private &&
        ((method_decl->data.node.flags & MOD_DEFAULT) ||
         (is_in_interface && (method_decl->data.node.flags & MOD_STATIC)))) {
        mi->access_flags |= ACC_PUBLIC;
    }
    /* Annotation methods are implicitly public abstract */
    if (is_in_annotation) {
        mi->access_flags |= ACC_PUBLIC | ACC_ABSTRACT;
    }
    mi->ast = method_decl;  /* Store AST for annotations */
    mi->throws = extract_throws_types(method_decl, cg->sem);  /* Extract throws clause */
    
    /* Add ACC_VARARGS if this method has a varargs parameter */
    if (method_sym && (method_sym->modifiers & MOD_VARARGS)) {
        mi->access_flags |= ACC_VARARGS;
    }
    
    /* Handle constructor specially - reuse is_constructor from earlier */
    if (is_constructor) {
        mi->name_index = cp_add_utf8(cg->cp, "<init>");
        
        /* Build constructor descriptor: (params)V */
        string_t *desc = string_new("(");
        
        /* For enums, constructor always takes (String name, int ordinal) as first params */
        if (cg->class_sym && cg->class_sym->kind == SYM_ENUM) {
            string_append(desc, "Ljava/lang/String;I");
        }
        
        /* For inner classes, first parameter is the outer instance */
        if (cg->is_inner_class && cg->outer_class_internal) {
            string_append(desc, "L");
            string_append(desc, cg->outer_class_internal);
            string_append(desc, ";");
        }
        
        /* For local/anonymous classes, add captured variable types after outer instance */
        if ((cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars) {
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (var_sym && var_sym->type) {
                    char *cap_desc = type_to_descriptor(var_sym->type);
                    string_append(desc, cap_desc);
                    free(cap_desc);
                }
            }
        }
        
        slist_t *params = method_decl->data.node.children;
        while (params) {
            ast_node_t *child = (ast_node_t *)params->data;
            if (child->type == AST_PARAMETER) {
                /* Get parameter type */
                slist_t *pchildren = child->data.node.children;
                if (pchildren) {
                    char *param_desc = ast_type_to_descriptor((ast_node_t *)pchildren->data);
                    string_append(desc, param_desc);
                    free(param_desc);
                }
            }
            params = params->next;
        }
        string_append(desc, ")V");
        mi->descriptor_index = cp_add_utf8(cg->cp, desc->str);
        string_free(desc, true);
    } else {
        mi->name_index = cp_add_utf8(cg->cp, name);
        char *desc = method_sym ? method_to_descriptor(method_sym) : strdup("()V");
        mi->descriptor_index = cp_add_utf8(cg->cp, desc);
        free(desc);
    }
    
    /* Generate method signature if method uses generics */
    mi->signature = generate_method_signature(method_decl, method_sym);
    
    mi->code = mg->code;
    mg->code = NULL;  /* Transfer ownership */
    
    mi->exception_handlers = mg->exception_handlers;
    mg->exception_handlers = NULL;  /* Transfer ownership */
    
    mi->line_numbers = mg->line_numbers;
    mg->line_numbers = NULL;  /* Transfer ownership */
    
    mi->local_var_table = mg->local_var_table;
    mg->local_var_table = NULL;  /* Transfer ownership */
    
    mi->stackmap = mg->stackmap;
    mg->stackmap = NULL;  /* Transfer ownership */
    
    /* If stackmap has entries, enable StackMapTable generation */
    if (mi->stackmap && mi->stackmap->num_entries > 0) {
        cg->use_stackmap = true;
    }
    
    /* Add to class */
    if (!cg->methods) {
        cg->methods = slist_new(mi);
    } else {
        slist_append(cg->methods, mi);
    }
    
    method_gen_free(mg);
    return true;
}

/* ========================================================================
 * Bridge Method Generation (for covariant return types)
 * ======================================================================== */

/**
 * Generate a bridge method for covariant return types.
 * The bridge method has the overridden method's return type and delegates
 * to the actual implementation.
 */
static bool codegen_bridge_method(class_gen_t *cg, symbol_t *method_sym)
{
    if (!cg || !method_sym || !method_sym->data.method_data.overridden_method) {
        return false;
    }
    
    symbol_t *overridden = method_sym->data.method_data.overridden_method;
    const char *name = method_sym->name;
    
    /* Create method generator */
    method_gen_t *mg = method_gen_new(cg, method_sym);
    if (!mg) {
        return false;
    }
    
    /* Bridge method body:
     * - Load 'this'
     * - Load all arguments
     * - Call the actual method (invokevirtual)
     * - Return the result (with implicit cast to parent return type)
     */
    
    /* Load 'this' */
    bc_emit(mg->code, OP_ALOAD_0);
    mg_push(mg, 1);
    
    /* Load all parameters */
    int slot = 1;
    slist_t *params = method_sym->data.method_data.parameters;
    for (slist_t *node = params; node; node = node->next) {
        symbol_t *param = (symbol_t *)node->data;
        if (param && param->type) {
            type_kind_t kind = param->type->kind;
            switch (kind) {
                case TYPE_LONG:
                    bc_emit(mg->code, slot <= 3 ? OP_LLOAD_0 + slot : OP_LLOAD);
                    if (slot > 3) bc_emit_u1(mg->code, slot);
                    mg_push(mg, 2);
                    slot += 2;
                    break;
                case TYPE_DOUBLE:
                    bc_emit(mg->code, slot <= 3 ? OP_DLOAD_0 + slot : OP_DLOAD);
                    if (slot > 3) bc_emit_u1(mg->code, slot);
                    mg_push(mg, 2);
                    slot += 2;
                    break;
                case TYPE_FLOAT:
                    bc_emit(mg->code, slot <= 3 ? OP_FLOAD_0 + slot : OP_FLOAD);
                    if (slot > 3) bc_emit_u1(mg->code, slot);
                    mg_push(mg, 1);
                    slot++;
                    break;
                case TYPE_CLASS:
                case TYPE_ARRAY:
                    bc_emit(mg->code, slot <= 3 ? OP_ALOAD_0 + slot : OP_ALOAD);
                    if (slot > 3) bc_emit_u1(mg->code, slot);
                    mg_push(mg, 1);
                    slot++;
                    break;
                default:  /* int, byte, char, short, boolean */
                    bc_emit(mg->code, slot <= 3 ? OP_ILOAD_0 + slot : OP_ILOAD);
                    if (slot > 3) bc_emit_u1(mg->code, slot);
                    mg_push(mg, 1);
                    slot++;
                    break;
            }
        }
    }
    
    /* Build descriptor for the actual method (with covariant return type) */
    char *actual_desc = method_to_descriptor(method_sym);
    
    /* Call the actual method */
    uint16_t methodref = cp_add_methodref(cg->cp, cg->internal_name, name, actual_desc);
    bc_emit(mg->code, OP_INVOKEVIRTUAL);
    bc_emit_u2(mg->code, methodref);
    
    /* Pop args and receiver, push result */
    int arg_count = 0;
    for (slist_t *node = params; node; node = node->next) arg_count++;
    mg_pop(mg, arg_count + 1);
    
    /* Return the result */
    type_t *ret_type = overridden->type;  /* Use overridden method's return type */
    if (ret_type) {
        mg_push(mg, 1);
        switch (ret_type->kind) {
            case TYPE_VOID:
                bc_emit(mg->code, OP_RETURN);
                break;
            case TYPE_LONG:
                bc_emit(mg->code, OP_LRETURN);
                break;
            case TYPE_DOUBLE:
                bc_emit(mg->code, OP_DRETURN);
                break;
            case TYPE_FLOAT:
                bc_emit(mg->code, OP_FRETURN);
                break;
            case TYPE_CLASS:
            case TYPE_ARRAY:
                bc_emit(mg->code, OP_ARETURN);
                break;
            default:
                bc_emit(mg->code, OP_IRETURN);
                break;
        }
    } else {
        bc_emit(mg->code, OP_RETURN);
    }
    
    mg->code->max_stack = mg->max_stack;
    mg->code->max_locals = slot;
    
    free(actual_desc);
    
    /* Create method_info_gen_t with bridge descriptor (overridden return type) */
    method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
    if (!mi) {
        method_gen_free(mg);
        return false;
    }
    
    /* Bridge method has ACC_BRIDGE | ACC_SYNTHETIC flags
     * Convert MOD_ flags to ACC_ flags first */
    mi->access_flags = mods_to_access_flags(method_sym->modifiers & ~MOD_PRIVATE) | ACC_PUBLIC | ACC_BRIDGE | ACC_SYNTHETIC;
    mi->name_index = cp_add_utf8(cg->cp, name);
    
    /* Build descriptor with overridden method's return type */
    char *bridge_desc = method_to_descriptor(overridden);
    mi->descriptor_index = cp_add_utf8(cg->cp, bridge_desc);
    free(bridge_desc);
    
    mi->code = mg->code;
    mg->code = NULL;
    
    /* Add to class */
    if (!cg->methods) {
        cg->methods = slist_new(mi);
    } else {
        slist_append(cg->methods, mi);
    }
    
    method_gen_free(mg);
    return true;
}

/**
 * Generate bridge methods for generic superclass inheritance.
 * When B extends A<String> and A<T> has f(T), B needs f(Object) -> super.f(Object).
 * This is needed when:
 * - Class extends a generic superclass with concrete type arguments
 * - Superclass has methods with type parameters in signature
 * - Class doesn't override those methods
 */
static void generate_superclass_bridges(class_gen_t *cg)
{
    if (!cg || !cg->class_sym) {
        return;
    }
    
    symbol_t *class_sym = cg->class_sym;
    symbol_t *super_sym = class_sym->data.class_data.superclass;
    
    if (!super_sym || !super_sym->data.class_data.members) {
        return;
    }
    
    /* Check if superclass has methods that need bridges.
     * A method needs a bridge if:
     * 1. It has type variable parameters that erase to Object
     * 2. The current class doesn't define a method with the erased signature
     */
    scope_t *super_members = super_sym->data.class_data.members;
    if (!super_members || !super_members->symbols) {
        return;
    }
    
    hashtable_t *ht = super_members->symbols;
    for (size_t i = 0; i < ht->size; i++) {
        hashtable_entry_t *entry = ht->buckets[i];
        while (entry) {
            symbol_t *method = (symbol_t *)entry->value;
            if (method && method->kind == SYM_METHOD &&
                !(method->modifiers & MOD_STATIC) &&
                !(method->modifiers & MOD_PRIVATE)) {
                
                /* Check if this method has type variable parameters */
                bool has_type_var_param = false;
                slist_t *params = method->data.method_data.parameters;
                for (slist_t *p = params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type && param->type->kind == TYPE_TYPEVAR) {
                        has_type_var_param = true;
                        break;
                    }
                }
                
                /* Also check for type variable return type */
                if (method->type && method->type->kind == TYPE_TYPEVAR) {
                    has_type_var_param = true;
                }
                
                if (!has_type_var_param) {
                    entry = entry->next;
                    continue;
                }
                
                /* Check if current class already has this method */
                bool has_override = false;
                if (class_sym->data.class_data.members && 
                    class_sym->data.class_data.members->symbols) {
                    hashtable_t *class_methods = class_sym->data.class_data.members->symbols;
                    for (size_t j = 0; j < class_methods->size && !has_override; j++) {
                        hashtable_entry_t *class_entry = class_methods->buckets[j];
                        while (class_entry && !has_override) {
                            symbol_t *class_method = (symbol_t *)class_entry->value;
                            if (class_method && class_method->kind == SYM_METHOD &&
                                class_method->name && method->name &&
                                strcmp(class_method->name, method->name) == 0) {
                                /* Count parameters to match signature */
                                int class_param_count = 0;
                                int super_param_count = 0;
                                for (slist_t *cp = class_method->data.method_data.parameters; cp; cp = cp->next)
                                    class_param_count++;
                                for (slist_t *sp = method->data.method_data.parameters; sp; sp = sp->next)
                                    super_param_count++;
                                if (class_param_count == super_param_count) {
                                    has_override = true;
                                }
                            }
                            class_entry = class_entry->next;
                        }
                    }
                }
                
                if (has_override) {
                    entry = entry->next;
                    continue;
                }
                
                /* Generate bridge method:
                 * public synthetic bridge void f(Object arg) {
                 *     super.f(arg);  // invokespecial
                 * }
                 */
                method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
                if (!mi) {
                    entry = entry->next;
                    continue;
                }
                
                /* Bridge flags: public, synthetic, bridge */
                mi->access_flags = ACC_PUBLIC | ACC_SYNTHETIC | ACC_BRIDGE;
                mi->name_index = cp_add_utf8(cg->cp, method->name);
                
                /* Build descriptor with erased types (type vars -> Object) */
                string_t *desc = string_new("(");
                int slot = 1;  /* Start after 'this' */
                for (slist_t *p = params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type) {
                        if (param->type->kind == TYPE_TYPEVAR) {
                            string_append(desc, "Ljava/lang/Object;");
                        } else {
                            char *param_desc = type_to_descriptor(param->type);
                            string_append(desc, param_desc);
                            free(param_desc);
                        }
                        /* Count slots for parameter loading */
                        if (param->type->kind == TYPE_LONG || param->type->kind == TYPE_DOUBLE) {
                            slot += 2;
                        } else {
                            slot++;
                        }
                    }
                }
                string_append(desc, ")");
                
                /* Return type */
                if (method->type) {
                    if (method->type->kind == TYPE_TYPEVAR) {
                        string_append(desc, "Ljava/lang/Object;");
                    } else {
                        char *ret_desc = type_to_descriptor(method->type);
                        string_append(desc, ret_desc);
                        free(ret_desc);
                    }
                } else {
                    string_append(desc, "V");
                }
                
                mi->descriptor_index = cp_add_utf8(cg->cp, desc->str);
                char *desc_str = strdup(desc->str);
                string_free(desc, true);
                
                /* Generate bridge method body */
                bytecode_t *code = bytecode_new();
                int max_stack = 0;
                
                /* aload_0 (this) */
                bc_emit(code, OP_ALOAD_0);
                max_stack++;
                
                /* Load all parameters */
                slot = 1;
                for (slist_t *p = params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    type_kind_t kind = TYPE_CLASS;  /* Default for type vars */
                    if (param && param->type && param->type->kind != TYPE_TYPEVAR) {
                        kind = param->type->kind;
                    }
                    
                    switch (kind) {
                        case TYPE_LONG:
                            bc_emit(code, slot <= 3 ? OP_LLOAD_0 + slot : OP_LLOAD);
                            if (slot > 3) bc_emit_u1(code, slot);
                            max_stack += 2;
                            slot += 2;
                            break;
                        case TYPE_DOUBLE:
                            bc_emit(code, slot <= 3 ? OP_DLOAD_0 + slot : OP_DLOAD);
                            if (slot > 3) bc_emit_u1(code, slot);
                            max_stack += 2;
                            slot += 2;
                            break;
                        case TYPE_FLOAT:
                            bc_emit(code, slot <= 3 ? OP_FLOAD_0 + slot : OP_FLOAD);
                            if (slot > 3) bc_emit_u1(code, slot);
                            max_stack++;
                            slot++;
                            break;
                        case TYPE_BOOLEAN:
                        case TYPE_BYTE:
                        case TYPE_CHAR:
                        case TYPE_SHORT:
                        case TYPE_INT:
                            bc_emit(code, slot <= 3 ? OP_ILOAD_0 + slot : OP_ILOAD);
                            if (slot > 3) bc_emit_u1(code, slot);
                            max_stack++;
                            slot++;
                            break;
                        default:  /* CLASS, ARRAY, TYPEVAR -> aload */
                            bc_emit(code, slot <= 3 ? OP_ALOAD_0 + slot : OP_ALOAD);
                            if (slot > 3) bc_emit_u1(code, slot);
                            max_stack++;
                            slot++;
                            break;
                    }
                }
                
                /* invokespecial super.method(desc) */
                uint16_t methodref = cp_add_methodref(cg->cp, cg->superclass, method->name, desc_str);
                bc_emit(code, OP_INVOKESPECIAL);
                bc_emit_u2(code, methodref);
                
                /* Return */
                type_kind_t ret_kind = TYPE_VOID;
                if (method->type) {
                    ret_kind = method->type->kind;
                    if (ret_kind == TYPE_TYPEVAR) ret_kind = TYPE_CLASS;
                }
                
                switch (ret_kind) {
                    case TYPE_VOID:   bc_emit(code, OP_RETURN); break;
                    case TYPE_LONG:   bc_emit(code, OP_LRETURN); break;
                    case TYPE_DOUBLE: bc_emit(code, OP_DRETURN); break;
                    case TYPE_FLOAT:  bc_emit(code, OP_FRETURN); break;
                    case TYPE_CLASS:
                    case TYPE_ARRAY:  bc_emit(code, OP_ARETURN); break;
                    default:          bc_emit(code, OP_IRETURN); break;
                }
                
                code->max_stack = max_stack;
                code->max_locals = slot;
                
                mi->code = code;
                
                free(desc_str);
                
                /* Add to methods list */
                if (!cg->methods) {
                    cg->methods = slist_new(mi);
                } else {
                    slist_append(cg->methods, mi);
                }
            }
            entry = entry->next;
        }
    }
}

/**
 * Generate bridge methods for generic interface implementations.
 * When a class implements Comparator<WebFragment> with compare(WebFragment, WebFragment),
 * we need a bridge method compare(Object, Object) that casts and delegates.
 * 
 * Also generates bridges for interface methods inherited through abstract superclasses.
 */
static void generate_interface_bridges(class_gen_t *cg)
{
    if (!cg || !cg->class_sym) {
        return;
    }
    
    symbol_t *class_sym = cg->class_sym;
    
    /* Don't generate bridges for interfaces themselves */
    if (class_sym->kind == SYM_INTERFACE) {
        return;
    }
    
    /* Process all implemented interfaces */
    for (slist_t *iface_node = class_sym->data.class_data.interfaces; 
         iface_node; iface_node = iface_node->next) {
        symbol_t *iface_sym = (symbol_t *)iface_node->data;
        if (!iface_sym || !iface_sym->data.class_data.members) {
            continue;
        }
        
        /* Iterate over interface methods */
        scope_t *iface_members = iface_sym->data.class_data.members;
        if (!iface_members || !iface_members->symbols) {
            continue;
        }
        
        hashtable_t *iface_ht = iface_members->symbols;
        for (size_t i = 0; i < iface_ht->size; i++) {
            hashtable_entry_t *entry = iface_ht->buckets[i];
            while (entry) {
                symbol_t *iface_method = (symbol_t *)entry->value;
                if (!iface_method || iface_method->kind != SYM_METHOD ||
                    (iface_method->modifiers & MOD_STATIC)) {
                    entry = entry->next;
                    continue;
                }
                
                /* Check if method has type variable parameters or return type */
                bool has_type_var = false;
                slist_t *params = iface_method->data.method_data.parameters;
                
                for (slist_t *p = params; p && !has_type_var; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type && param->type->kind == TYPE_TYPEVAR) {
                        has_type_var = true;
                    }
                }
                if (iface_method->type && iface_method->type->kind == TYPE_TYPEVAR) {
                    has_type_var = true;
                }
                
                if (!has_type_var) {
                    entry = entry->next;
                    continue;
                }
                
                /* Find the implementation in the current class (with concrete types) */
                symbol_t *impl_method = NULL;
                if (class_sym->data.class_data.members && 
                    class_sym->data.class_data.members->symbols) {
                    hashtable_t *class_ht = class_sym->data.class_data.members->symbols;
                    for (size_t j = 0; j < class_ht->size && !impl_method; j++) {
                        hashtable_entry_t *class_entry = class_ht->buckets[j];
                        while (class_entry && !impl_method) {
                            symbol_t *class_method = (symbol_t *)class_entry->value;
                            if (class_method && class_method->kind == SYM_METHOD &&
                                class_method->name && iface_method->name &&
                                strcmp(class_method->name, iface_method->name) == 0 &&
                                !(class_method->modifiers & MOD_STATIC)) {
                                /* Check parameter count matches */
                                int class_count = 0, iface_count = 0;
                                for (slist_t *cp = class_method->data.method_data.parameters; cp; cp = cp->next)
                                    class_count++;
                                for (slist_t *ip = params; ip; ip = ip->next)
                                    iface_count++;
                                if (class_count == iface_count) {
                                    impl_method = class_method;
                                }
                            }
                            class_entry = class_entry->next;
                        }
                    }
                }
                
                /* Also check superclass chain for implementation */
                if (!impl_method) {
                    symbol_t *super = class_sym->data.class_data.superclass;
                    while (super && !impl_method) {
                        if (super->data.class_data.members && 
                            super->data.class_data.members->symbols) {
                            hashtable_t *super_ht = super->data.class_data.members->symbols;
                            for (size_t j = 0; j < super_ht->size && !impl_method; j++) {
                                hashtable_entry_t *super_entry = super_ht->buckets[j];
                                while (super_entry && !impl_method) {
                                    symbol_t *super_method = (symbol_t *)super_entry->value;
                                    if (super_method && super_method->kind == SYM_METHOD &&
                                        super_method->name && iface_method->name &&
                                        strcmp(super_method->name, iface_method->name) == 0 &&
                                        !(super_method->modifiers & MOD_STATIC) &&
                                        !(super_method->modifiers & MOD_PRIVATE)) {
                                        int super_count = 0, iface_count = 0;
                                        for (slist_t *sp = super_method->data.method_data.parameters; sp; sp = sp->next)
                                            super_count++;
                                        for (slist_t *ip = params; ip; ip = ip->next)
                                            iface_count++;
                                        if (super_count == iface_count) {
                                            impl_method = super_method;
                                        }
                                    }
                                    super_entry = super_entry->next;
                                }
                            }
                        }
                        super = super->data.class_data.superclass;
                    }
                }
                
                if (!impl_method) {
                    entry = entry->next;
                    continue;
                }
                
                /* Check if implementation has concrete (non-type-var) parameter types */
                bool impl_has_concrete = true;
                for (slist_t *p = impl_method->data.method_data.parameters; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type && param->type->kind == TYPE_TYPEVAR) {
                        impl_has_concrete = false;
                        break;
                    }
                }
                
                if (!impl_has_concrete) {
                    entry = entry->next;
                    continue;
                }
                
                /* Check if we already have a bridge with the erased signature */
                string_t *erased_desc = string_new("(");
                for (slist_t *p = params; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type) {
                        if (param->type->kind == TYPE_TYPEVAR) {
                            string_append(erased_desc, "Ljava/lang/Object;");
                        } else {
                            char *pdesc = type_to_descriptor(param->type);
                            string_append(erased_desc, pdesc);
                            free(pdesc);
                        }
                    }
                }
                string_append(erased_desc, ")");
                if (iface_method->type) {
                    if (iface_method->type->kind == TYPE_TYPEVAR) {
                        string_append(erased_desc, "Ljava/lang/Object;");
                    } else {
                        char *rdesc = type_to_descriptor(iface_method->type);
                        string_append(erased_desc, rdesc);
                        free(rdesc);
                    }
                } else {
                    string_append(erased_desc, "V");
                }
                
                /* Check if we already have this method (don't duplicate) */
                bool already_has_bridge = false;
                for (slist_t *m = cg->methods; m && !already_has_bridge; m = m->next) {
                    method_info_gen_t *existing = (method_info_gen_t *)m->data;
                    if (existing) {
                        /* Get method name and descriptor from existing */
                        const char *existing_name = NULL;
                        const char *existing_desc = NULL;
                        for (uint16_t idx = 1; idx < cg->cp->count; idx++) {
                            if (cg->cp->entries[idx].type == CONST_UTF8) {
                                if (idx == existing->name_index) {
                                    existing_name = cg->cp->entries[idx].data.utf8;
                                }
                                if (idx == existing->descriptor_index) {
                                    existing_desc = cg->cp->entries[idx].data.utf8;
                                }
                            }
                        }
                        if (existing_name && existing_desc &&
                            strcmp(existing_name, iface_method->name) == 0 &&
                            strcmp(existing_desc, erased_desc->str) == 0) {
                            already_has_bridge = true;
                        }
                    }
                }
                
                if (already_has_bridge) {
                    string_free(erased_desc, true);
                    entry = entry->next;
                    continue;
                }
                
                /* Generate bridge method */
                method_info_gen_t *mi = calloc(1, sizeof(method_info_gen_t));
                if (!mi) {
                    string_free(erased_desc, true);
                    entry = entry->next;
                    continue;
                }
                
                mi->access_flags = ACC_PUBLIC | ACC_SYNTHETIC | ACC_BRIDGE;
                mi->name_index = cp_add_utf8(cg->cp, iface_method->name);
                mi->descriptor_index = cp_add_utf8(cg->cp, erased_desc->str);
                
                /* Build concrete method descriptor for invokevirtual */
                string_t *concrete_desc = string_new("(");
                for (slist_t *p = impl_method->data.method_data.parameters; p; p = p->next) {
                    symbol_t *param = (symbol_t *)p->data;
                    if (param && param->type) {
                        char *pdesc = type_to_descriptor(param->type);
                        string_append(concrete_desc, pdesc);
                        free(pdesc);
                    }
                }
                string_append(concrete_desc, ")");
                if (impl_method->type) {
                    char *rdesc = type_to_descriptor(impl_method->type);
                    string_append(concrete_desc, rdesc);
                    free(rdesc);
                } else {
                    string_append(concrete_desc, "V");
                }
                
                /* Generate bytecode:
                 * aload_0                           // this
                 * aload_1                           // first Object arg
                 * checkcast ConcreteType1           // cast to concrete type
                 * aload_2                           // second Object arg  
                 * checkcast ConcreteType2           // cast to concrete type
                 * invokevirtual this.method(ConcreteTypes)
                 * areturn/ireturn/return
                 */
                bytecode_t *code = bytecode_new();
                int max_stack = 1;  /* 'this' */
                int max_locals = 1; /* 'this' */
                
                /* aload_0 (this) */
                bc_emit(code, OP_ALOAD_0);
                
                /* Load and cast each parameter */
                int slot = 1;
                slist_t *iface_param = params;
                slist_t *impl_param = impl_method->data.method_data.parameters;
                
                while (iface_param && impl_param) {
                    symbol_t *iparam = (symbol_t *)iface_param->data;
                    symbol_t *cparam = (symbol_t *)impl_param->data;
                    
                    /* Load parameter */
                    if (slot <= 3) {
                        bc_emit(code, OP_ALOAD_0 + slot);
                    } else {
                        bc_emit(code, OP_ALOAD);
                        bc_emit_u1(code, slot);
                    }
                    max_stack++;
                    
                    /* If interface param is type var but impl param is concrete, cast */
                    if (iparam && iparam->type && iparam->type->kind == TYPE_TYPEVAR &&
                        cparam && cparam->type && cparam->type->kind == TYPE_CLASS) {
                        const char *cast_name = cparam->type->data.class_type.name;
                        if (cast_name) {
                            /* Convert dots to slashes for internal name */
                            char *internal = strdup(cast_name);
                            for (char *p = internal; *p; p++) {
                                if (*p == '.') *p = '/';
                            }
                            uint16_t cast_class = cp_add_class(cg->cp, internal);
                            bc_emit(code, OP_CHECKCAST);
                            bc_emit_u2(code, cast_class);
                            free(internal);
                        }
                    }
                    
                    slot++;
                    max_locals++;
                    iface_param = iface_param->next;
                    impl_param = impl_param->next;
                }
                
                /* invokevirtual this.concreteMethod */
                uint16_t methodref = cp_add_methodref(cg->cp, cg->internal_name, 
                                                       impl_method->name, concrete_desc->str);
                bc_emit(code, OP_INVOKEVIRTUAL);
                bc_emit_u2(code, methodref);
                
                /* Return */
                if (impl_method->type) {
                    switch (impl_method->type->kind) {
                        case TYPE_VOID:   bc_emit(code, OP_RETURN); break;
                        case TYPE_LONG:   bc_emit(code, OP_LRETURN); break;
                        case TYPE_DOUBLE: bc_emit(code, OP_DRETURN); break;
                        case TYPE_FLOAT:  bc_emit(code, OP_FRETURN); break;
                        case TYPE_BOOLEAN:
                        case TYPE_BYTE:
                        case TYPE_CHAR:
                        case TYPE_SHORT:
                        case TYPE_INT:    bc_emit(code, OP_IRETURN); break;
                        default:          bc_emit(code, OP_ARETURN); break;
                    }
                } else {
                    bc_emit(code, OP_RETURN);
                }
                
                code->max_stack = max_stack;
                code->max_locals = max_locals;
                
                mi->code = code;
                
                string_free(erased_desc, true);
                string_free(concrete_desc, true);
                
                /* Add to methods list */
                if (!cg->methods) {
                    cg->methods = slist_new(mi);
                } else {
                    slist_append(cg->methods, mi);
                }
                
                entry = entry->next;
            }
        }
    }
}

/* ========================================================================
 * Class Code Generation
 * ======================================================================== */

bool codegen_class(class_gen_t *cg, ast_node_t *class_decl)
{
    if (!cg || !class_decl) {
        return false;
    }
    
    /* Store AST for annotation access */
    cg->class_ast = class_decl;
    
    /* Generate class signature if class has type parameters or parameterized superclass */
    cg->signature = generate_class_signature(class_decl, cg->class_sym);
    
    /* Check if this is an interface */
    bool is_interface = (class_decl->type == AST_INTERFACE_DECL);
    
    /* Check if this is an annotation */
    bool is_annotation = (class_decl->type == AST_ANNOTATION_DECL);
    
    /* Check if this is an enum */
    bool is_enum = (class_decl->type == AST_ENUM_DECL);
    
    /* Check if this is a record */
    bool is_record = (class_decl->type == AST_RECORD_DECL);
    
    /* For annotations, set ACC_INTERFACE, ACC_ABSTRACT, ACC_ANNOTATION flags */
    /* Note: java.lang.annotation.Annotation interface is already added in class_gen_new */
    if (is_annotation) {
        cg->access_flags |= ACC_INTERFACE | ACC_ABSTRACT | ACC_ANNOTATION;
    }
    
    /* For enums, set ACC_ENUM flag and superclass to java.lang.Enum */
    if (is_enum) {
        cg->access_flags |= ACC_ENUM | ACC_FINAL;
        if (cg->superclass) free(cg->superclass);
        cg->superclass = strdup("java/lang/Enum");
        cg->super_class = cp_add_class(cg->cp, cg->superclass);
    }
    
    /* For records, set ACC_FINAL flag and superclass to java.lang.Record */
    if (is_record) {
        cg->access_flags |= ACC_FINAL;
        if (cg->superclass) free(cg->superclass);
        cg->superclass = strdup("java/lang/Record");
        cg->super_class = cp_add_class(cg->cp, cg->superclass);
    }
    
    /* Track whether the class has an explicit constructor */
    bool has_constructor = false;
    
    /* Collect static initializers for <clinit> generation */
    slist_t *static_initializers = NULL;  /* List of AST_INITIALIZER_BLOCK nodes */
    slist_t *static_field_inits = NULL;   /* List of {field_name, initializer} pairs */
    
    /* Initialize instance initializer lists in class_gen */
    cg->instance_initializers = NULL;
    cg->instance_field_inits = NULL;
    
    /* For sealed classes, collect permitted subclasses from AST */
    /* Children with flags == 3 are permits clause types */
    if (class_decl->data.node.flags & MOD_SEALED) {
        for (slist_t *c = class_decl->data.node.children; c; c = c->next) {
            ast_node_t *child = (ast_node_t *)c->data;
            if (child && child->data.node.flags == 3 && child->type == AST_CLASS_TYPE) {
                /* This is a permitted subclass type */
                /* Get the class name from semantic type or AST node name */
                const char *class_name = NULL;
                if (child->sem_type && child->sem_type->kind == TYPE_CLASS) {
                    class_name = child->sem_type->data.class_type.name;
                }
                if (!class_name) {
                    class_name = child->data.node.name;
                }
                if (class_name) {
                    char *internal_name = NULL;
                    
                    /* Check if it's a simple name (no $ or . or /) - if so, it's likely
                     * a sibling inner class and needs to be qualified with outer class */
                    if (!strchr(class_name, '$') && !strchr(class_name, '.') && 
                        !strchr(class_name, '/') && cg->class_sym && 
                        cg->class_sym->data.class_data.enclosing_class) {
                        /* Get the outer class (enclosing class of this sealed class) */
                        symbol_t *outer = cg->class_sym->data.class_data.enclosing_class;
                        size_t len = strlen(outer->qualified_name) + strlen(class_name) + 2;
                        char *qualified = malloc(len);
                        snprintf(qualified, len, "%s$%s", outer->qualified_name, class_name);
                        internal_name = class_to_internal_name(qualified);
                        free(qualified);
                    } else {
                        internal_name = class_to_internal_name(class_name);
                    }
                    
                    uint16_t *cp_idx = malloc(sizeof(uint16_t));
                    *cp_idx = cp_add_class(cg->cp, internal_name);
                    if (!cg->permitted_subclasses) {
                        cg->permitted_subclasses = slist_new(cp_idx);
                    } else {
                        slist_append(cg->permitted_subclasses, cp_idx);
                    }
                    free(internal_name);
                }
            }
        }
    }
    
    /* Process class members in two passes:
     * Pass 1: Collect all field declarations (so methods can reference any field)
     * Pass 2: Generate methods */
    slist_t *children = class_decl->data.node.children;
    
    /* Pass 1: Collect all fields first */
    slist_t *field_pass = children;
    while (field_pass) {
        ast_node_t *member = (ast_node_t *)field_pass->data;
        
        if (member->type == AST_FIELD_DECL) {
            /* Get field type and name from children */
            slist_t *fchildren = member->data.node.children;
            ast_node_t *field_type = NULL;
            
            while (fchildren) {
                ast_node_t *child = (ast_node_t *)fchildren->data;
                
                if (child->type == AST_PRIMITIVE_TYPE ||
                    child->type == AST_CLASS_TYPE ||
                    child->type == AST_ARRAY_TYPE) {
                    field_type = child;
                } else if (child->type == AST_VAR_DECLARATOR) {
                    /* Add field - convert MOD_ flags to ACC_ flags */
                    field_gen_t *fg = calloc(1, sizeof(field_gen_t));
                    fg->access_flags = mods_to_access_flags(member->data.node.flags);
                    fg->ast = member;  /* Store field decl AST for annotations */
                    
                    /* Interface fields are implicitly public static final */
                    if (is_interface) {
                        fg->access_flags |= ACC_PUBLIC | ACC_STATIC | ACC_FINAL;
                    }
                    
                    /* Store name and descriptor */
                    fg->name = strdup(child->data.node.name);
                    fg->descriptor = ast_type_to_descriptor(field_type);
                    
                    /* Generate field signature if type uses type variables */
                    if (field_type && field_type->sem_type) {
                        fg->signature = generate_field_signature(field_type->sem_type);
                    }
                    
                    fg->name_index = cp_add_utf8(cg->cp, fg->name);
                    fg->descriptor_index = cp_add_utf8(cg->cp, fg->descriptor);
                    
                    /* Add to field list */
                    if (!cg->fields) {
                        cg->fields = slist_new(fg);
                    } else {
                        slist_append(cg->fields, fg);
                    }
                    
                    /* Add to field lookup map */
                    hashtable_insert(cg->field_map, fg->name, fg);
                    
                    /* Check for field initializer */
                    bool is_static = (fg->access_flags & ACC_STATIC) != 0;
                    if (child->data.node.children) {
                        /* Has initializer - store for <clinit> or constructor */
                        /* Store as a fake assignment AST node */
                        ast_node_t *init_expr = (ast_node_t *)child->data.node.children->data;
                        ast_node_t *assign = ast_new(AST_ASSIGNMENT_EXPR,
                            child->line, child->column);
                        assign->data.node.name = "=";
                        /* LHS: field access */
                        ast_node_t *field_access = ast_new(AST_IDENTIFIER,
                            child->line, child->column);
                        field_access->data.leaf.name = fg->name;
                        ast_add_child(assign, field_access);
                        /* RHS: initializer */
                        ast_add_child(assign, init_expr);
                        
                        if (is_static) {
                            /* Static field - add to <clinit> */
                            if (!static_field_inits) {
                                static_field_inits = slist_new(assign);
                            } else {
                                slist_append(static_field_inits, assign);
                            }
                        } else {
                            /* Instance field - add to constructors */
                            if (!cg->instance_field_inits) {
                                cg->instance_field_inits = slist_new(assign);
                            } else {
                                slist_append(cg->instance_field_inits, assign);
                            }
                        }
                    }
                }
                fchildren = fchildren->next;
            }
        }
        field_pass = field_pass->next;
    }
    
    /* Pass 2: Process other members (methods, constructors, nested classes, etc.) */
    while (children) {
        ast_node_t *member = (ast_node_t *)children->data;
        
        switch (member->type) {
            case AST_FIELD_DECL:
                /* Already processed in pass 1, skip */
                break;
            
            case AST_METHOD_DECL:
                if (is_annotation) {
                    /* Annotation methods are implicitly abstract - they never have bodies */
                    if (!codegen_interface_method(cg, member)) {
                        return false;
                    }
                } else if (is_interface) {
                    /* Java 8+: default and static methods have bodies */
                    /* Java 9+: private methods in interfaces also have bodies */
                    if ((member->data.node.flags & MOD_DEFAULT) ||
                        (member->data.node.flags & MOD_STATIC) ||
                        (member->data.node.flags & MOD_PRIVATE)) {
                        /* Track that we have Java 8+ interface features (for class file version) */
                        cg->has_default_methods = true;
                        /* Default/static/private methods need full code generation */
                        if (!codegen_method(cg, member)) {
                            return false;
                        }
                    } else {
                        /* Abstract interface method (no body) */
                        if (!codegen_interface_method(cg, member)) {
                            return false;
                        }
                    }
                } else if (member->data.node.flags & MOD_ABSTRACT) {
                    /* Abstract method in abstract class - no body */
                    if (!codegen_abstract_method(cg, member)) {
                        return false;
                    }
                } else if (member->data.node.flags & MOD_NATIVE) {
                    /* Native method - no body, just declaration */
                    if (!codegen_abstract_method(cg, member)) {
                        return false;
                    }
                } else {
                    if (!codegen_method(cg, member)) {
                        return false;
                    }
                    
                    /* Generate bridge method if this is a covariant override.
                     * Methods are stored with keys like "call(0)" so we need to
                     * iterate through symbols to find by name. */
                    const char *method_name = member->data.node.name;
                    if (cg->class_sym && cg->class_sym->data.class_data.members) {
                        scope_t *members = cg->class_sym->data.class_data.members;
                        if (members && members->symbols) {
                            hashtable_t *ht = members->symbols;
                            for (size_t i = 0; i < ht->size; i++) {
                                hashtable_entry_t *entry = ht->buckets[i];
                                while (entry) {
                                    symbol_t *method_sym = (symbol_t *)entry->value;
                                    if (method_sym && method_sym->kind == SYM_METHOD &&
                                        method_sym->name && strcmp(method_sym->name, method_name) == 0 &&
                                        method_sym->data.method_data.overridden_method) {
                                        codegen_bridge_method(cg, method_sym);
                                    }
                                    entry = entry->next;
                                }
                            }
                        }
                    }
                }
                break;
            
            case AST_CONSTRUCTOR_DECL:
                has_constructor = true;
                if (!codegen_method(cg, member)) {
                    return false;
                }
                break;
            
            case AST_INITIALIZER_BLOCK:
                if (member->data.node.flags & MOD_STATIC) {
                    /* Collect static initializer blocks for <clinit> */
                    if (!static_initializers) {
                        static_initializers = slist_new(member);
                    } else {
                        slist_append(static_initializers, member);
                    }
                } else {
                    /* Collect instance initializer blocks for constructors */
                    if (!cg->instance_initializers) {
                        cg->instance_initializers = slist_new(member);
                    } else {
                        slist_append(cg->instance_initializers, member);
                    }
                }
                break;
            
            case AST_PARAMETER:
                {
                    /* Record components - generate private final field and public accessor */
                    if (is_record && (member->data.node.flags & MOD_RECORD_COMPONENT)) {
                        const char *comp_name = member->data.node.name;
                        
                        /* Get component type from child */
                        ast_node_t *comp_type = NULL;
                        if (member->data.node.children) {
                            comp_type = (ast_node_t *)member->data.node.children->data;
                        }
                        
                        /* Create private final field for the component */
                        field_gen_t *fg = calloc(1, sizeof(field_gen_t));
                        fg->access_flags = ACC_PRIVATE | ACC_FINAL;
                        fg->ast = member;
                        fg->name = strdup(comp_name);
                        fg->descriptor = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                        
                        fg->name_index = cp_add_utf8(cg->cp, fg->name);
                        fg->descriptor_index = cp_add_utf8(cg->cp, fg->descriptor);
                        
                        /* Add to field list */
                        if (!cg->fields) {
                            cg->fields = slist_new(fg);
                        } else {
                            slist_append(cg->fields, fg);
                        }
                        
                        /* Add to field lookup map */
                        hashtable_insert(cg->field_map, fg->name, fg);
                    }
                }
                break;
            
            case AST_ENUM_CONSTANT:
                {
                    /* Enum constants are public static final fields of the enum type */
                    const char *const_name = member->data.node.name;
                    
                    field_gen_t *fg = calloc(1, sizeof(field_gen_t));
                    fg->access_flags = ACC_PUBLIC | ACC_STATIC | ACC_FINAL | ACC_ENUM;
                    fg->ast = member;
                    fg->name = strdup(const_name);
                    
                    /* Type descriptor is the enum type itself */
                    char *desc = calloc(strlen(cg->internal_name) + 4, 1);
                    sprintf(desc, "L%s;", cg->internal_name);
                    fg->descriptor = desc;
                    
                    fg->name_index = cp_add_utf8(cg->cp, fg->name);
                    fg->descriptor_index = cp_add_utf8(cg->cp, fg->descriptor);
                    
                    /* Add to field list */
                    if (!cg->fields) {
                        cg->fields = slist_new(fg);
                    } else {
                        slist_append(cg->fields, fg);
                    }
                    
                    /* Add to field lookup map */
                    hashtable_insert(cg->field_map, fg->name, fg);
                    
                    /* Mark that we need to generate <clinit> to initialize enum constants */
                    /* We'll handle this in the static initialization section */
                }
                break;
            
            case AST_CLASS_DECL:
            case AST_INTERFACE_DECL:
            case AST_ENUM_DECL:
            case AST_RECORD_DECL:
            case AST_ANNOTATION_DECL:
                {
                    /* Nested class - collect for separate compilation */
                    if (!cg->nested_classes) {
                        cg->nested_classes = slist_new(member);
                    } else {
                        slist_append(cg->nested_classes, member);
                    }
                    
                    /* Add InnerClasses attribute entry for this nested class */
                    const char *nested_name = member->data.node.name;
                    if (nested_name && cg->class_sym) {
                        /* Build qualified name for nested class */
                        char *nested_qname = NULL;
                        size_t len = strlen(cg->class_sym->qualified_name) + strlen(nested_name) + 2;
                        nested_qname = malloc(len);
                        snprintf(nested_qname, len, "%s$%s", cg->class_sym->qualified_name, nested_name);
                        
                        char *nested_internal = class_to_internal_name(nested_qname);
                        
                        inner_class_entry_t *entry = calloc(1, sizeof(inner_class_entry_t));
                        uint16_t nested_class_idx = cp_add_class(cg->cp, nested_internal);
                        entry->inner_class_info = nested_class_idx;
                        entry->outer_class_info = cg->this_class;
                        entry->inner_name = cp_add_utf8(cg->cp, nested_name);
                        /* Convert MOD_ flags to ACC_ flags for inner class access */
                        entry->access_flags = mods_to_access_flags(member->data.node.flags);
                        
                        if (!cg->inner_class_entries) {
                            cg->inner_class_entries = slist_new(entry);
                        } else {
                            slist_append(cg->inner_class_entries, entry);
                        }
                        
                        /* NestMembers attribute (Java 11+): Add this nested class as a nest member
                         * Only if the current class is the nest host (not itself a nested class) */
                        if (cg->nest_host == 0) {
                            uint16_t *nest_member_idx = malloc(sizeof(uint16_t));
                            *nest_member_idx = nested_class_idx;
                            if (!cg->nest_members) {
                                cg->nest_members = slist_new(nest_member_idx);
                            } else {
                                slist_append(cg->nest_members, nest_member_idx);
                            }
                        }
                        
                        free(nested_internal);
                        free(nested_qname);
                    }
                }
                break;
            
            default:
                break;
        }
        
        children = children->next;
    }
    
    /* Collect record components for record types */
    slist_t *record_components = NULL;
    if (is_record) {
        /* Collect all record components (AST_PARAMETER with MOD_RECORD_COMPONENT) */
        slist_t *children_iter = class_decl->data.node.children;
        while (children_iter) {
            ast_node_t *member = (ast_node_t *)children_iter->data;
            if (member->type == AST_PARAMETER && 
                (member->data.node.flags & MOD_RECORD_COMPONENT)) {
                if (!record_components) {
                    record_components = slist_new(member);
                } else {
                    slist_append(record_components, member);
                }
            }
            children_iter = children_iter->next;
        }
    }
    
    /* Generate bridge methods for generic superclass inheritance.
     * When B extends A<String> and A<T> has f(T), B needs f(Object) -> super.f(Object). */
    if (!is_interface && !is_annotation) {
        generate_superclass_bridges(cg);
    }
    
    /* Generate bridge methods for generic interface implementations.
     * When C implements Comparator<X>, it needs compare(Object, Object) -> compare(X, X). */
    if (!is_interface && !is_annotation) {
        generate_interface_bridges(cg);
    }
    
    /* Generate default constructor if class has no explicit constructors
     * (interfaces and annotations don't have constructors) */
    if (!has_constructor && !is_interface && !is_annotation) {
        /* Generate <init> - for enums: private <init>(String,I)V
         * For records: public <init>(component types)V
         * For regular classes: public <init>()V or (LOuterClass;captured...)V */
        method_info_gen_t *init = calloc(1, sizeof(method_info_gen_t));
        
        /* Enum constructors are private */
        if (is_enum) {
            init->access_flags = ACC_PRIVATE;
        } else {
            init->access_flags = ACC_PUBLIC;
        }
        
        init->name_index = cp_add_utf8(cg->cp, "<init>");
        
        /* Build constructor descriptor */
        string_t *desc = string_new("(");
        
        /* For enums, constructor takes (String name, int ordinal) */
        if (is_enum) {
            string_append(desc, "Ljava/lang/String;I");
        }
        
        /* For records, constructor takes all component types */
        if (is_record && record_components) {
            for (slist_t *comp = record_components; comp; comp = comp->next) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                string_append(desc, comp_desc);
                free(comp_desc);
            }
        }
        
        /* For inner classes, constructor takes outer instance as parameter */
        if (cg->is_inner_class && cg->outer_class_internal) {
            string_append(desc, "L");
            string_append(desc, cg->outer_class_internal);
            string_append(desc, ";");
        }
        
        /* For local/anonymous classes, add captured variable types */
        if ((cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars) {
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (var_sym && var_sym->type) {
                    char *cap_desc = type_to_descriptor(var_sym->type);
                    string_append(desc, cap_desc);
                    free(cap_desc);
                }
            }
        }
        
        string_append(desc, ")V");
        init->descriptor_index = cp_add_utf8(cg->cp, desc->str);
        string_free(desc, true);
        
        /* Create method_gen_t for generating code */
        method_gen_t *mg = calloc(1, sizeof(method_gen_t));
        mg->code = bytecode_new();
        mg->cp = cg->cp;
        mg->class_gen = cg;
        mg->locals = hashtable_new();
        mg->method = NULL;
        mg->loop_stack = NULL;
        mg->is_static = false;
        mg->is_constructor = true;
        mg->line_numbers = NULL;
        mg->local_var_table = NULL;
        
        /* Initialize StackMapTable tracking */
        mg->stackmap = stackmap_new();
        if (mg->stackmap && cg->internal_name) {
            stackmap_init_method(mg->stackmap, mg, mg->is_static, cg->internal_name);
        }
        
        /* Calculate next slot based on parameters */
        /* Slot 0 = this, Slot 1 = outer (if inner), then captured vars */
        mg->next_slot = 1;  /* 'this' is slot 0 */
        mg->max_locals = 1;
        
        /* For enums, add slots for name (String, slot 1) and ordinal (int, slot 2) */
        if (is_enum) {
            mg->next_slot = 3;  /* this=0, name=1, ordinal=2 */
            mg->max_locals = 3;
        }
        
        /* For records, add slots for each component */
        if (is_record && record_components) {
            for (slist_t *comp = record_components; comp; comp = comp->next) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                /* Determine size based on type */
                int size = 1;
                if (comp_type && (comp_type->type == AST_PRIMITIVE_TYPE)) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    if (ptype && (strcmp(ptype, "long") == 0 || strcmp(ptype, "double") == 0)) {
                        size = 2;
                    }
                }
                mg->next_slot += size;
                mg->max_locals = mg->next_slot;
            }
        }
        
        if (cg->is_inner_class) {
            mg->next_slot = 2;  /* outer instance in slot 1 */
            mg->max_locals = 2;
        }
        
        /* Account for captured variables */
        if ((cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars) {
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (var_sym && var_sym->type) {
                    int size = (var_sym->type->kind == TYPE_LONG || 
                               var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                    mg->next_slot += size;
                    mg->max_locals = mg->next_slot;
                }
            }
        }
        
        /* Record 'this' in LocalVariableTable (slot 0) */
        if (cg->internal_name) {
            char *this_desc = calloc(1, strlen(cg->internal_name) + 3);
            sprintf(this_desc, "L%s;", cg->internal_name);
            mg_record_local_var(mg, "this", this_desc, 0, 0, NULL);
            free(this_desc);
        }
        
        /* Record outer instance for inner classes (slot 1) */
        if (cg->is_inner_class && cg->outer_class_internal) {
            char *outer_desc = calloc(1, strlen(cg->outer_class_internal) + 3);
            sprintf(outer_desc, "L%s;", cg->outer_class_internal);
            mg_record_local_var(mg, "this$0", outer_desc, 1, 0, NULL);
            free(outer_desc);
        }
        
        /* For inner classes, first store this$0 */
        if (cg->is_inner_class && cg->this_dollar_zero_ref) {
            /* aload_0 (this) */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            /* aload_1 (outer instance) */
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            /* putfield this$0 */
            bc_emit(mg->code, OP_PUTFIELD);
            bc_emit_u2(mg->code, cg->this_dollar_zero_ref);
            mg_pop(mg, 2);
        }
        
        /* For local/anonymous classes, store captured variables from constructor parameters */
        if ((cg->is_local_class || cg->is_anonymous_class) && cg->captured_vars && cg->captured_field_refs) {
            uint16_t slot = cg->is_inner_class ? 2 : 1;  /* Skip this + outer (if present) */
            
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (!var_sym || !var_sym->name) continue;
                
                /* Get the field ref for this captured variable */
                void *ref_ptr = hashtable_lookup(cg->captured_field_refs, var_sym->name);
                if (!ref_ptr) continue;
                uint16_t field_ref = (uint16_t)(uintptr_t)ref_ptr;
                
                /* aload_0 (this) */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                
                /* Load captured value from parameter slot */
                type_kind_t kind = TYPE_CLASS;
                if (var_sym->type) {
                    kind = var_sym->type->kind;
                }
                
                /* Emit appropriate load instruction */
                if (kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_LLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_DLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_FLOAT) {
                    bc_emit(mg->code, OP_FLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
                    /* Reference types including type variables (erase to Object) */
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else {
                    /* Integer types */
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                }
                
                /* putfield val$xxx */
                bc_emit(mg->code, OP_PUTFIELD);
                bc_emit_u2(mg->code, field_ref);
                mg_pop(mg, kind == TYPE_LONG || kind == TYPE_DOUBLE ? 3 : 2);
            }
        }
        
        /* aload_0 - load 'this' */
        bc_emit(mg->code, OP_ALOAD_0);
        mg_push(mg, 1);
        
        /* Check if superclass is a non-static inner class */
        bool super_is_inner = false;
        symbol_t *super_enclosing = NULL;
        if (cg->class_sym && cg->class_sym->data.class_data.superclass) {
            symbol_t *super_sym = cg->class_sym->data.class_data.superclass;
            if (super_sym->data.class_data.enclosing_class &&
                !(super_sym->modifiers & MOD_STATIC)) {
                super_is_inner = true;
                super_enclosing = super_sym->data.class_data.enclosing_class;
            }
        }
        
        /* invokespecial superclass.<init> */
        if (is_enum) {
            /* For enums: super(name, ordinal) with (Ljava/lang/String;I)V */
            /* Load name (slot 1) and ordinal (slot 2) */
            bc_emit(mg->code, OP_ALOAD_1);  /* name */
            mg_push(mg, 1);
            bc_emit(mg->code, OP_ILOAD_2);  /* ordinal */
            mg_push(mg, 1);
            uint16_t super_init = cp_add_methodref(cg->cp, cg->superclass, "<init>", "(Ljava/lang/String;I)V");
            bc_emit(mg->code, OP_INVOKESPECIAL);
            bc_emit_u2(mg->code, super_init);
            mg_pop(mg, 3);  /* pops this, name, ordinal */
            
            /* Mark 'this' as initialized in stackmap after super() */
            if (mg->stackmap && cg->internal_name) {
                stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
            }
        } else if (super_is_inner && super_enclosing && cg->is_inner_class) {
            /* Superclass is a non-static inner class - pass its enclosing instance.
             * If super's enclosing class != our enclosing class, we need to
             * traverse the this$0 chain. */
            symbol_t *our_enclosing = cg->class_sym->data.class_data.enclosing_class;
            symbol_t *super_sym = cg->class_sym->data.class_data.superclass;
            slist_t *super_captured = super_sym ? super_sym->data.class_data.captured_vars : NULL;
            int super_arg_count = 1;  /* At least the outer instance */
            
            if (our_enclosing == super_enclosing) {
                /* Same enclosing class - load directly from slot 1 */
                bc_emit(mg->code, OP_ALOAD_1);
                mg_push(mg, 1);
            } else {
                /* Different enclosing class - traverse the this$0 chain */
                bc_emit(mg->code, OP_ALOAD_1);
                mg_push(mg, 1);
                
                /* Traverse from our enclosing class up to super's enclosing class */
                symbol_t *current = our_enclosing;
                while (current && current != super_enclosing) {
                    symbol_t *current_enc = current->data.class_data.enclosing_class;
                    if (!current_enc) break;
                    
                    /* Get this$0 from current class (points to current_enc) */
                    char *cur_internal = class_to_internal_name(current->qualified_name);
                    char *enc_internal = class_to_internal_name(current_enc->qualified_name);
                    size_t desc_len = strlen(enc_internal) + 3;
                    char *this0_desc = malloc(desc_len);
                    snprintf(this0_desc, desc_len, "L%s;", enc_internal);
                    
                    uint16_t this0_ref = cp_add_fieldref(cg->cp, cur_internal, "this$0", this0_desc);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, this0_ref);
                    /* getfield pops 1, pushes 1 - no stack change */
                    
                    free(cur_internal);
                    free(enc_internal);
                    free(this0_desc);
                    
                    current = current_enc;
                }
            }
            
            /* Build super() descriptor: (LOuterClass;[captured_types])V */
            const char *enc_name = super_enclosing->qualified_name;
            char *enc_internal = class_to_internal_name(enc_name);
            string_t *super_desc = string_new("(L");
            string_append(super_desc, enc_internal);
            string_append(super_desc, ";");
            
            /* If superclass is a local class with captured vars, add those to descriptor
             * and load them from our constructor parameters */
            if (super_sym && super_sym->data.class_data.is_local_class && super_captured) {
                /* Load captured values from our constructor parameters */
                /* Our params are: this (slot 0), outer (slot 1), captured vars (slot 2+) */
                uint16_t cap_slot = 2;  /* Start after this and outer */
                
                for (slist_t *cap = super_captured; cap; cap = cap->next) {
                    symbol_t *var_sym = (symbol_t *)cap->data;
                    if (!var_sym || !var_sym->type) continue;
                    
                    /* Add type to descriptor */
                    char *cap_desc = type_to_descriptor(var_sym->type);
                    string_append(super_desc, cap_desc);
                    free(cap_desc);
                    
                    /* Load from parameter slot */
                    type_kind_t kind = var_sym->type->kind;
                    if (kind == TYPE_LONG) {
                        bc_emit(mg->code, OP_LLOAD);
                        bc_emit_u1(mg->code, (uint8_t)cap_slot);
                        mg_push(mg, 2);
                        cap_slot += 2;
                        super_arg_count += 2;
                    } else if (kind == TYPE_DOUBLE) {
                        bc_emit(mg->code, OP_DLOAD);
                        bc_emit_u1(mg->code, (uint8_t)cap_slot);
                        mg_push(mg, 2);
                        cap_slot += 2;
                        super_arg_count += 2;
                    } else if (kind == TYPE_FLOAT) {
                        bc_emit(mg->code, OP_FLOAD);
                        bc_emit_u1(mg->code, (uint8_t)cap_slot);
                        mg_push(mg, 1);
                        cap_slot++;
                        super_arg_count++;
                    } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
                        /* Reference types including type variables (erase to Object) */
                        bc_emit(mg->code, OP_ALOAD);
                        bc_emit_u1(mg->code, (uint8_t)cap_slot);
                        mg_push(mg, 1);
                        cap_slot++;
                        super_arg_count++;
                    } else {
                        /* Integer types */
                        bc_emit(mg->code, OP_ILOAD);
                        bc_emit_u1(mg->code, (uint8_t)cap_slot);
                        mg_push(mg, 1);
                        cap_slot++;
                        super_arg_count++;
                    }
                }
            }
            
            string_append(super_desc, ")V");
            uint16_t super_init = cp_add_methodref(cg->cp, cg->superclass, "<init>", super_desc->str);
            bc_emit(mg->code, OP_INVOKESPECIAL);
            bc_emit_u2(mg->code, super_init);
            mg_pop(mg, 1 + super_arg_count);  /* pops this + args */
            free(enc_internal);
            string_free(super_desc, true);
            
            /* Mark 'this' as initialized in stackmap after super() */
            if (mg->stackmap && cg->internal_name) {
                stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
            }
        } else {
            uint16_t super_init = cp_add_methodref(cg->cp, cg->superclass, "<init>", "()V");
            bc_emit(mg->code, OP_INVOKESPECIAL);
            bc_emit_u2(mg->code, super_init);
            mg_pop(mg, 1);
            
            /* Mark 'this' as initialized in stackmap after super() */
            if (mg->stackmap && cg->internal_name) {
                stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
            }
        }
        
        /* For records, initialize component fields from constructor parameters */
        if (is_record && record_components) {
            uint16_t slot = 1;  /* Components start at slot 1 (slot 0 is 'this') */
            
            for (slist_t *comp = record_components; comp; comp = comp->next) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                const char *comp_name = comp_node->data.node.name;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                
                /* Determine type kind for load instruction */
                bool is_long_or_double = false;
                bool is_float = false;
                bool is_reference = true;
                
                if (comp_type && comp_type->type == AST_PRIMITIVE_TYPE) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    is_reference = false;
                    if (ptype && strcmp(ptype, "long") == 0) {
                        is_long_or_double = true;
                    } else if (ptype && strcmp(ptype, "double") == 0) {
                        is_long_or_double = true;
                        is_float = true;  /* Use dload not lload */
                    } else if (ptype && strcmp(ptype, "float") == 0) {
                        is_float = true;
                    } else {
                        /* int, short, byte, char, boolean - handled by default case */
                    }
                } else if (comp_type && comp_type->type == AST_ARRAY_TYPE) {
                    is_reference = true;
                }
                
                /* aload_0 (this) */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                
                /* Load component value from parameter slot */
                if (is_long_or_double && !is_float) {
                    bc_emit(mg->code, OP_LLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                } else if (is_long_or_double && is_float) {
                    bc_emit(mg->code, OP_DLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                } else if (is_float) {
                    bc_emit(mg->code, OP_FLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                } else if (is_reference) {
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                } else {
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                }
                
                /* putfield componentName */
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name, comp_name, comp_desc);
                bc_emit(mg->code, OP_PUTFIELD);
                bc_emit_u2(mg->code, field_ref);
                mg_pop(mg, is_long_or_double ? 3 : 2);
                
                /* Advance slot */
                slot += is_long_or_double ? 2 : 1;
                
                free(comp_desc);
            }
        }
        
        /* Inject instance field initializers */
        for (slist_t *node = cg->instance_field_inits; node; node = node->next) {
            ast_node_t *assign = (ast_node_t *)node->data;
            slist_t *assign_children = assign->data.node.children;
            if (assign_children && assign_children->next) {
                ast_node_t *field_id = (ast_node_t *)assign_children->data;
                ast_node_t *init_expr = (ast_node_t *)assign_children->next->data;
                const char *field_name = field_id->data.leaf.name;
                
                field_gen_t *field = hashtable_lookup(cg->field_map, field_name);
                if (field) {
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push(mg, 1);
                    codegen_expr(mg, init_expr, cg->cp);
                    uint16_t field_ref = cp_add_fieldref(cg->cp,
                        cg->internal_name, field_name, field->descriptor);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    mg_pop(mg, 2);
                }
            }
        }
        
        /* Inject instance initializer blocks */
        for (slist_t *node = cg->instance_initializers; node; node = node->next) {
            ast_node_t *init_block = (ast_node_t *)node->data;
            if (init_block->data.node.children) {
                ast_node_t *block = (ast_node_t *)init_block->data.node.children->data;
                codegen_statement(mg, block);
            }
        }
        
        /* return */
        bc_emit(mg->code, OP_RETURN);
        
        /* Set max_stack and max_locals */
        mg->code->max_stack = mg->max_stack;
        mg->code->max_locals = mg->max_locals;
        
        init->code = mg->code;
        mg->code = NULL;
        
        /* Transfer debug tables */
        init->line_numbers = mg->line_numbers;
        mg->line_numbers = NULL;
        init->local_var_table = mg->local_var_table;
        mg->local_var_table = NULL;
        
        /* Transfer stackmap */
        if (mg->stackmap && mg->stackmap->num_entries > 0) {
            init->stackmap = mg->stackmap;
            mg->stackmap = NULL;
            cg->use_stackmap = true;
        }
        
        method_gen_free(mg);
        
        /* Add to methods list */
        if (!cg->methods) {
            cg->methods = slist_new(init);
        } else {
            slist_append(cg->methods, init);
        }
    }
    
    /* Add synthetic $assertionsDisabled field if needed */
    if (cg->needs_assertions) {
        field_gen_t *fg = calloc(1, sizeof(field_gen_t));
        fg->access_flags = 0x1018;  /* ACC_STATIC | ACC_FINAL | ACC_SYNTHETIC */
        fg->name = strdup("$assertionsDisabled");
        fg->descriptor = strdup("Z");
        fg->name_index = cp_add_utf8(cg->cp, fg->name);
        fg->descriptor_index = cp_add_utf8(cg->cp, fg->descriptor);
        
        if (!cg->fields) {
            cg->fields = slist_new(fg);
        } else {
            slist_append(cg->fields, fg);
        }
        hashtable_insert(cg->field_map, fg->name, fg);
    }
    
    /* Collect enum constants for clinit and $VALUES array */
    slist_t *enum_constants = NULL;  /* Track enum constant AST nodes */
    if (is_enum) {
        slist_t *children_iter = class_decl->data.node.children;
        while (children_iter) {
            ast_node_t *member = (ast_node_t *)children_iter->data;
            if (member->type == AST_ENUM_CONSTANT) {
                if (!enum_constants) {
                    enum_constants = slist_new((void *)member);  /* Store AST node */
                } else {
                    slist_append(enum_constants, (void *)member);
                }
            }
            children_iter = children_iter->next;
        }
    }
    
    /* Generate <clinit> if we have static initializers, field inits, assertions, or enum constants */
    if (static_field_inits || static_initializers || cg->needs_assertions || is_enum) {
        method_info_gen_t *clinit = calloc(1, sizeof(method_info_gen_t));
        clinit->access_flags = 0x0008;  /* ACC_STATIC */
        clinit->name_index = cp_add_utf8(cg->cp, "<clinit>");
        clinit->descriptor_index = cp_add_utf8(cg->cp, "()V");
        
        /* Create a method_gen_t manually for <clinit> (no symbol) */
        method_gen_t *mg = calloc(1, sizeof(method_gen_t));
        mg->code = bytecode_new();
        mg->cp = cg->cp;
        mg->class_gen = cg;
        mg->locals = hashtable_new();
        mg->method = NULL;
        mg->loop_stack = NULL;
        mg->is_static = true;  /* <clinit> is static */
        mg->is_constructor = false;
        mg->next_slot = 0;  /* No 'this' for static methods */
        mg->max_locals = 0;
        
        /* Initialize StackMapTable tracking */
        mg->stackmap = stackmap_new();
        if (mg->stackmap && cg->internal_name) {
            stackmap_init_method(mg->stackmap, mg, mg->is_static, cg->internal_name);
        }
        
        /* 0. Generate enum constant initializers (for enums only) */
        if (is_enum && enum_constants) {
            int ordinal = 0;
            char enum_desc[256];
            snprintf(enum_desc, sizeof(enum_desc), "L%s;", cg->internal_name);
            
            for (slist_t *ec = enum_constants; ec; ec = ec->next) {
                ast_node_t *enum_const = (ast_node_t *)ec->data;
                const char *const_name = enum_const->data.node.name;
                
                /* new EnumClass */
                uint16_t class_idx = cp_add_class(cg->cp, cg->internal_name);
                bc_emit(mg->code, OP_NEW);
                bc_emit_u2(mg->code, class_idx);
                mg_push(mg, 1);
                
                /* dup */
                bc_emit(mg->code, OP_DUP);
                mg_push(mg, 1);
                
                /* ldc "CONSTANT_NAME" */
                uint16_t name_str = cp_add_string(cg->cp, const_name);
                bc_emit(mg->code, OP_LDC_W);
                bc_emit_u2(mg->code, name_str);
                mg_push(mg, 1);
                
                /* bipush/sipush ordinal */
                if (ordinal <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_u1(mg->code, (uint8_t)ordinal);
                } else {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_u2(mg->code, (uint16_t)ordinal);
                }
                mg_push(mg, 1);
                
                /* Build constructor descriptor: (Ljava/lang/String;I<user params>)V */
                string_t *ctor_desc = string_new("(Ljava/lang/String;I");
                int arg_count = 2;  /* name + ordinal */
                
                /* Generate constructor arguments from enum constant children.
                 * Skip method/field/initializer declarations - those are for enum
                 * constant bodies (anonymous subclasses) not constructor args. */
                slist_t *args = enum_const->data.node.children;
                while (args) {
                    ast_node_t *arg = (ast_node_t *)args->data;
                    /* Skip class body members (methods, fields, initializers) */
                    if (arg->type == AST_METHOD_DECL ||
                        arg->type == AST_CONSTRUCTOR_DECL ||
                        arg->type == AST_FIELD_DECL ||
                        arg->type == AST_INITIALIZER_BLOCK) {
                        args = args->next;
                        continue;
                    }
                    /* Generate the argument expression */
                    codegen_expr(mg, arg, cg->cp);
                    arg_count++;
                    
                    /* Get the type descriptor for this argument from sem_type if available */
                    if (arg->sem_type) {
                        char *arg_desc = type_to_descriptor(arg->sem_type);
                        string_append(ctor_desc, arg_desc);
                        free(arg_desc);
                    } else if (arg->type == AST_LITERAL) {
                        /* Fallback for literals based on value */
                        const char *val = arg->data.leaf.name;
                        if (val) {
                            /* Check for double/float literals */
                            if (strchr(val, '.') != NULL || strchr(val, 'e') != NULL || strchr(val, 'E') != NULL) {
                                if (val[strlen(val) - 1] == 'f' || val[strlen(val) - 1] == 'F') {
                                    string_append(ctor_desc, "F");
                                } else {
                                    string_append(ctor_desc, "D");
                                }
                            } else if (val[strlen(val) - 1] == 'L' || val[strlen(val) - 1] == 'l') {
                                string_append(ctor_desc, "J");
                            } else {
                                string_append(ctor_desc, "I");
                            }
                        }
                    }
                    args = args->next;
                }
                string_append(ctor_desc, ")V");
                
                /* invokespecial EnumClass.<init>(descriptor) */
                uint16_t init_ref = cp_add_methodref(cg->cp, cg->internal_name, 
                    "<init>", ctor_desc->str);
                bc_emit(mg->code, OP_INVOKESPECIAL);
                bc_emit_u2(mg->code, init_ref);
                mg_pop(mg, arg_count + 1);  /* pops args + new ref */
                
                string_free(ctor_desc, true);
                
                /* putstatic EnumClass.CONSTANT_NAME */
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                    const_name, enum_desc);
                bc_emit(mg->code, OP_PUTSTATIC);
                bc_emit_u2(mg->code, field_ref);
                mg_pop(mg, 1);
                
                ordinal++;
            }
        }
        
        /* 1. Generate static field initializers (in declaration order) */
        for (slist_t *node = static_field_inits; node; node = node->next) {
            ast_node_t *assign = (ast_node_t *)node->data;
            /* The assign node has: field identifier, initializer expr */
            slist_t *assign_children = assign->data.node.children;
            if (assign_children && assign_children->next) {
                ast_node_t *field_id = (ast_node_t *)assign_children->data;
                ast_node_t *init_expr = (ast_node_t *)assign_children->next->data;
                const char *field_name = field_id->data.leaf.name;
                
                /* Look up field descriptor */
                field_gen_t *field = hashtable_lookup(cg->field_map, field_name);
                if (field) {
                    /* Generate initializer expression */
                    codegen_expr(mg, init_expr, cg->cp);
                    
                    /* putstatic field */
                    uint16_t field_ref = cp_add_fieldref(cg->cp,
                        cg->internal_name, field_name, field->descriptor);
                    bc_emit(mg->code, OP_PUTSTATIC);
                    bc_emit_u2(mg->code, field_ref);
                    mg_pop(mg, 1);
                }
            }
        }
        
        /* 2. Generate static initializer blocks (in declaration order) */
        for (slist_t *node = static_initializers; node; node = node->next) {
            ast_node_t *init_block = (ast_node_t *)node->data;
            /* The block is the first child */
            if (init_block->data.node.children) {
                ast_node_t *block = (ast_node_t *)init_block->data.node.children->data;
                codegen_statement(mg, block);
            }
        }
        
        /* 3. Generate $VALUES array initialization for enums */
        if (is_enum && enum_constants) {
            char enum_array_desc[256];
            snprintf(enum_array_desc, sizeof(enum_array_desc), "[L%s;", cg->internal_name);
            
            /* Add $VALUES field */
            field_gen_t *values_field = calloc(1, sizeof(field_gen_t));
            values_field->access_flags = ACC_PRIVATE | ACC_STATIC | ACC_FINAL | ACC_SYNTHETIC;
            values_field->name = strdup("$VALUES");
            values_field->descriptor = strdup(enum_array_desc);
            values_field->name_index = cp_add_utf8(cg->cp, values_field->name);
            values_field->descriptor_index = cp_add_utf8(cg->cp, values_field->descriptor);
            
            if (!cg->fields) {
                cg->fields = slist_new(values_field);
            } else {
                slist_append(cg->fields, values_field);
            }
            hashtable_insert(cg->field_map, values_field->name, values_field);
            
            /* Initialize $VALUES array with all enum constants */
            /* Create array: anewarray num_constants */
            int num_constants = 0;
            for (slist_t *ec = enum_constants; ec; ec = ec->next) {
                num_constants++;
            }
            
            if (num_constants <= 127) {
                bc_emit(mg->code, OP_BIPUSH);
                bc_emit_u1(mg->code, (uint8_t)num_constants);
            } else {
                bc_emit(mg->code, OP_SIPUSH);
                bc_emit_u2(mg->code, (uint16_t)num_constants);
            }
            mg_push(mg, 1);
            
            /* anewarray EnumClass */
            uint16_t elem_class_idx = cp_add_class(cg->cp, cg->internal_name);
            bc_emit(mg->code, OP_ANEWARRAY);
            bc_emit_u2(mg->code, elem_class_idx);
            /* Stack: arrayref (no change - replaces count) */
            
            char enum_desc[256];
            snprintf(enum_desc, sizeof(enum_desc), "L%s;", cg->internal_name);
            
            /* Store each enum constant in the array */
            int idx = 0;
            for (slist_t *ec = enum_constants; ec; ec = ec->next) {
                ast_node_t *enum_const = (ast_node_t *)ec->data;
                const char *const_name = enum_const->data.node.name;
                
                /* dup - keep arrayref on stack */
                bc_emit(mg->code, OP_DUP);
                mg_push(mg, 1);
                
                /* push index */
                if (idx <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_u1(mg->code, (uint8_t)idx);
                } else {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_u2(mg->code, (uint16_t)idx);
                }
                mg_push(mg, 1);
                
                /* getstatic EnumClass.CONSTANT */
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                    const_name, enum_desc);
                bc_emit(mg->code, OP_GETSTATIC);
                bc_emit_u2(mg->code, field_ref);
                mg_push(mg, 1);
                
                /* aastore */
                bc_emit(mg->code, OP_AASTORE);
                mg_pop(mg, 3);  /* arrayref, index, value */
                
                idx++;
            }
            
            /* putstatic $VALUES */
            uint16_t values_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                "$VALUES", enum_array_desc);
            bc_emit(mg->code, OP_PUTSTATIC);
            bc_emit_u2(mg->code, values_ref);
            mg_pop(mg, 1);
        }
        
        /* 4. Generate assertions initialization if needed */
        if (cg->needs_assertions) {
            /* ldc "ClassName" */
            char *class_name_dots = strdup(cg->internal_name);
            for (char *p = class_name_dots; *p; p++) {
                if (*p == '/') *p = '.';
            }
            uint16_t class_name_str = cp_add_string(cg->cp, class_name_dots);
            free(class_name_dots);
            
            bc_emit(mg->code, OP_LDC_W);
            bc_emit_u2(mg->code, class_name_str);
            mg_push(mg, 1);
            
            /* invokestatic Class.forName */
            uint16_t forName_ref = cp_add_methodref(cg->cp, 
                "java/lang/Class", "forName", "(Ljava/lang/String;)Ljava/lang/Class;");
            bc_emit(mg->code, OP_INVOKESTATIC);
            bc_emit_u2(mg->code, forName_ref);
            /* Stack: Class object (replaces string) */
            
            /* invokevirtual Class.desiredAssertionStatus */
            uint16_t method_ref = cp_add_methodref(cg->cp, 
                "java/lang/Class", "desiredAssertionStatus", "()Z");
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, method_ref);
            /* Stack: boolean (replaces Class) */
            
            /* ifne set_false */
            size_t ifne_pos = mg->code->length;
            bc_emit(mg->code, OP_IFNE);
            bc_emit_u2(mg->code, 0);
            mg_pop(mg, 1);
            
            /* iconst_1 */
            bc_emit(mg->code, OP_ICONST_1);
            mg_push(mg, 1);
            
            /* goto store */
            size_t goto_pos = mg->code->length;
            bc_emit(mg->code, OP_GOTO);
            bc_emit_u2(mg->code, 0);
            
            /* set_false: iconst_0 */
            size_t set_false_target = mg->code->length;
            bc_emit(mg->code, OP_ICONST_0);
            
            /* Patch jumps */
            int16_t ifne_offset = (int16_t)(set_false_target - ifne_pos);
            mg->code->code[ifne_pos + 1] = (ifne_offset >> 8) & 0xFF;
            mg->code->code[ifne_pos + 2] = ifne_offset & 0xFF;
            
            size_t store_pos = mg->code->length;
            int16_t goto_offset = (int16_t)(store_pos - goto_pos);
            mg->code->code[goto_pos + 1] = (goto_offset >> 8) & 0xFF;
            mg->code->code[goto_pos + 2] = goto_offset & 0xFF;
            
            /* putstatic $assertionsDisabled */
            uint16_t field_ref = cp_add_fieldref(cg->cp, 
                cg->internal_name, "$assertionsDisabled", "Z");
            bc_emit(mg->code, OP_PUTSTATIC);
            bc_emit_u2(mg->code, field_ref);
            mg_pop(mg, 1);
        }
        
        /* return */
        bc_emit(mg->code, OP_RETURN);
        mg->last_opcode = OP_RETURN;
        
        /* Store code in method info */
        mg->code->max_stack = mg->max_stack;
        mg->code->max_locals = mg->max_locals > 0 ? mg->max_locals : 0;
        clinit->code = mg->code;
        
        /* Transfer stackmap */
        if (mg->stackmap && mg->stackmap->num_entries > 0) {
            clinit->stackmap = mg->stackmap;
            mg->stackmap = NULL;
            cg->use_stackmap = true;
        }
        
        /* Add to methods list */
        if (!cg->methods) {
            cg->methods = slist_new(clinit);
        } else {
            slist_append(cg->methods, clinit);
        }
        
        /* Don't free mg->code since clinit owns it now */
        mg->code = NULL;
        method_gen_free(mg);
    }
    
    /* Free the initializer lists */
    slist_free(static_field_inits);
    slist_free(static_initializers);
    
    /* Generate values() and valueOf() for enums */
    if (is_enum && enum_constants) {
        char enum_array_desc[256];
        snprintf(enum_array_desc, sizeof(enum_array_desc), "[L%s;", cg->internal_name);
        
        /* Generate values() method: returns $VALUES.clone() */
        {
            char values_desc[256];
            snprintf(values_desc, sizeof(values_desc), "()[L%s;", cg->internal_name);
            
            method_info_gen_t *values_method = calloc(1, sizeof(method_info_gen_t));
            values_method->access_flags = ACC_PUBLIC | ACC_STATIC;
            values_method->name_index = cp_add_utf8(cg->cp, "values");
            values_method->descriptor_index = cp_add_utf8(cg->cp, values_desc);
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = true;
            mg->is_constructor = false;
            mg->next_slot = 0;
            mg->max_locals = 0;
            
            /* getstatic $VALUES */
            uint16_t values_ref = cp_add_fieldref(cg->cp, cg->internal_name,
                "$VALUES", enum_array_desc);
            bc_emit(mg->code, OP_GETSTATIC);
            bc_emit_u2(mg->code, values_ref);
            mg_push(mg, 1);
            
            /* invokevirtual Object.clone()Ljava/lang/Object; */
            uint16_t clone_ref = cp_add_methodref(cg->cp, enum_array_desc,
                "clone", "()Ljava/lang/Object;");
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, clone_ref);
            /* Stack: cloned array (no net change) */
            
            /* checkcast [LEnumClass; */
            uint16_t array_class = cp_add_class(cg->cp, enum_array_desc);
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, array_class);
            
            /* areturn */
            bc_emit(mg->code, OP_ARETURN);
            mg_pop(mg, 1);
            
            mg->code->max_stack = mg->max_stack;
            mg->code->max_locals = 0;
            values_method->code = mg->code;
            mg->code = NULL;
            
            if (!cg->methods) {
                cg->methods = slist_new(values_method);
            } else {
                slist_append(cg->methods, values_method);
            }
            
            method_gen_free(mg);
        }
        
        /* Generate valueOf(String) method - manually search $VALUES to avoid reflection issues */
        /* 
         * public static EnumClass valueOf(String name) {
         *     EnumClass[] values = $VALUES;
         *     for (int i = 0; i < values.length; i++) {
         *         if (values[i].name().equals(name)) {
         *             return values[i];
         *         }
         *     }
         *     throw new IllegalArgumentException(name);
         * }
         */
        {
            char valueof_desc[256];
            snprintf(valueof_desc, sizeof(valueof_desc), "(Ljava/lang/String;)L%s;", cg->internal_name);
            
            method_info_gen_t *valueof_method = calloc(1, sizeof(method_info_gen_t));
            valueof_method->access_flags = ACC_PUBLIC | ACC_STATIC;
            valueof_method->name_index = cp_add_utf8(cg->cp, "valueOf");
            valueof_method->descriptor_index = cp_add_utf8(cg->cp, valueof_desc);
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = true;
            mg->is_constructor = false;
            mg->next_slot = 1;  /* String name parameter in slot 0 */
            mg->max_locals = 3; /* name, values, i */
            
            /* Initialize StackMapTable tracking for valueOf method */
            mg->stackmap = stackmap_new();
            if (mg->stackmap) {
                /* Initialize with String parameter in slot 0 */
                stackmap_set_local_object(mg->stackmap, 0, cg->cp, "java/lang/String");
            }
            
            /* Prepare constant pool entries */
            char values_field_desc[256];
            snprintf(values_field_desc, sizeof(values_field_desc), "[L%s;", cg->internal_name);
            uint16_t values_field = cp_add_fieldref(cg->cp, cg->internal_name, "$VALUES", values_field_desc);
            uint16_t name_method = cp_add_methodref(cg->cp, "java/lang/Enum", "name", "()Ljava/lang/String;");
            uint16_t equals_method = cp_add_methodref(cg->cp, "java/lang/String", "equals", "(Ljava/lang/Object;)Z");
            uint16_t iae_class = cp_add_class(cg->cp, "java/lang/IllegalArgumentException");
            uint16_t iae_init = cp_add_methodref(cg->cp, "java/lang/IllegalArgumentException", "<init>", "(Ljava/lang/String;)V");
            
            /* EnumClass[] values = $VALUES; */
            bc_emit(mg->code, OP_GETSTATIC);
            bc_emit_u2(mg->code, values_field);
            if (mg->stackmap) {
                stackmap_push_object(mg->stackmap, cg->cp, values_field_desc);
            }
            bc_emit(mg->code, OP_ASTORE_1);  /* slot 1: values */
            if (mg->stackmap) {
                stackmap_pop(mg->stackmap, 1);
                stackmap_set_local_object(mg->stackmap, 1, cg->cp, values_field_desc);
            }
            
            /* int i = 0; */
            bc_emit(mg->code, OP_ICONST_0);
            if (mg->stackmap) stackmap_push_int(mg->stackmap);
            bc_emit(mg->code, OP_ISTORE_2);  /* slot 2: i */
            if (mg->stackmap) {
                stackmap_pop(mg->stackmap, 1);
                stackmap_set_local_int(mg->stackmap, 2);
            }
            
            /* Loop start (address for jump back) */
            uint32_t loop_start = bc_offset(mg->code);
            if (mg->stackmap) {
                stackmap_record_frame(mg->stackmap, (uint16_t)loop_start);
            }
            
            /* if (i >= values.length) goto throw */
            bc_emit(mg->code, OP_ILOAD_2);
            bc_emit(mg->code, OP_ALOAD_1);
            bc_emit(mg->code, OP_ARRAYLENGTH);
            bc_emit(mg->code, OP_IF_ICMPGE);
            uint32_t throw_jump = bc_offset(mg->code);
            bc_emit_u2(mg->code, 0);  /* placeholder */
            
            /* if (values[i].name().equals(name)) return values[i]; */
            bc_emit(mg->code, OP_ALOAD_1);
            bc_emit(mg->code, OP_ILOAD_2);
            bc_emit(mg->code, OP_AALOAD);
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, name_method);
            bc_emit(mg->code, OP_ALOAD_0);  /* name parameter */
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, equals_method);
            bc_emit(mg->code, OP_IFEQ);
            uint32_t skip_return_jump = bc_offset(mg->code);
            bc_emit_u2(mg->code, 0);  /* placeholder */
            
            /* return values[i]; */
            bc_emit(mg->code, OP_ALOAD_1);
            bc_emit(mg->code, OP_ILOAD_2);
            bc_emit(mg->code, OP_AALOAD);
            bc_emit(mg->code, OP_ARETURN);
            
            /* patch skip_return_jump - offset is from the IFEQ opcode (at skip_return_jump - 1) */
            uint32_t skip_return_target = bc_offset(mg->code);
            uint32_t ifeq_pc = skip_return_jump - 1;  /* IFEQ opcode position */
            bc_patch_u2(mg->code, skip_return_jump, (uint16_t)(skip_return_target - ifeq_pc));
            
            /* Record frame at the skip_return target (after the return in the match case) */
            if (mg->stackmap) {
                stackmap_record_frame(mg->stackmap, (uint16_t)skip_return_target);
            }
            
            /* i++ */
            bc_emit(mg->code, OP_IINC);
            bc_emit_u1(mg->code, 2);  /* slot 2: i */
            bc_emit_u1(mg->code, 1);  /* increment by 1 */
            
            /* goto loop_start */
            uint32_t goto_pc = bc_offset(mg->code);  /* Position of GOTO opcode */
            bc_emit(mg->code, OP_GOTO);
            int16_t back_offset = (int16_t)(loop_start - goto_pc);  /* Offset from GOTO opcode */
            bc_emit_u2(mg->code, (uint16_t)back_offset);
            
            /* throw new IllegalArgumentException(name); */
            uint32_t throw_target = bc_offset(mg->code);
            uint32_t if_icmpge_pc = throw_jump - 1;  /* IF_ICMPGE opcode position */
            bc_patch_u2(mg->code, throw_jump, (uint16_t)(throw_target - if_icmpge_pc));
            
            /* Record frame at the throw target (after loop exits) */
            if (mg->stackmap) {
                stackmap_record_frame(mg->stackmap, (uint16_t)throw_target);
            }
            
            bc_emit(mg->code, OP_NEW);
            bc_emit_u2(mg->code, iae_class);
            bc_emit(mg->code, OP_DUP);
            bc_emit(mg->code, OP_ALOAD_0);  /* name */
            bc_emit(mg->code, OP_INVOKESPECIAL);
            bc_emit_u2(mg->code, iae_init);
            bc_emit(mg->code, OP_ATHROW);
            
            mg->code->max_stack = 3;
            mg->code->max_locals = 3;
            
            valueof_method->code = mg->code;
            mg->code = NULL;
            
            /* Transfer StackMapTable to the method */
            if (mg->stackmap && mg->stackmap->num_entries > 0) {
                valueof_method->stackmap = mg->stackmap;
                mg->stackmap = NULL;  /* Transfer ownership */
                cg->use_stackmap = true;
            }
            
            if (!cg->methods) {
                cg->methods = slist_new(valueof_method);
            } else {
                slist_append(cg->methods, valueof_method);
            }
            
            method_gen_free(mg);
        }
    }
    
    /* Free enum_constants list (just the list nodes, not the names) */
    slist_free(enum_constants);
    
    /* Generate accessor methods, equals(), hashCode(), and toString() for records */
    if (is_record && record_components) {
        /* Generate accessor methods for each component */
        for (slist_t *comp = record_components; comp; comp = comp->next) {
            ast_node_t *comp_node = (ast_node_t *)comp->data;
            const char *comp_name = comp_node->data.node.name;
            ast_node_t *comp_type = NULL;
            if (comp_node->data.node.children) {
                comp_type = (ast_node_t *)comp_node->data.node.children->data;
            }
            char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
            
            /* Build method descriptor ()T */
            string_t *accessor_desc = string_new("()");
            string_append(accessor_desc, comp_desc);
            
            method_info_gen_t *accessor = calloc(1, sizeof(method_info_gen_t));
            accessor->access_flags = ACC_PUBLIC;
            accessor->name_index = cp_add_utf8(cg->cp, comp_name);
            accessor->descriptor_index = cp_add_utf8(cg->cp, accessor_desc->str);
            string_free(accessor_desc, true);
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = false;
            mg->is_constructor = false;
            mg->next_slot = 1;
            mg->max_locals = 1;
            
            /* aload_0 (this) */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            
            /* getfield componentName */
            uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name, comp_name, comp_desc);
            bc_emit(mg->code, OP_GETFIELD);
            bc_emit_u2(mg->code, field_ref);
            /* Stack changes from 1 to 1 (or 2 for long/double) */
            
            /* Determine return instruction based on type */
            if (comp_type && comp_type->type == AST_PRIMITIVE_TYPE) {
                const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                if (strcmp(ptype, "long") == 0) {
                    mg_push(mg, 1);  /* getfield for long produces 2 */
                    bc_emit(mg->code, OP_LRETURN);
                } else if (strcmp(ptype, "double") == 0) {
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_DRETURN);
                } else if (strcmp(ptype, "float") == 0) {
                    bc_emit(mg->code, OP_FRETURN);
                } else {
                    /* int, byte, short, char, boolean */
                    bc_emit(mg->code, OP_IRETURN);
                }
            } else {
                /* Reference type (class or array) */
                bc_emit(mg->code, OP_ARETURN);
            }
            
            mg->code->max_stack = mg->max_stack > 2 ? mg->max_stack : 2;
            mg->code->max_locals = 1;
            accessor->code = mg->code;
            mg->code = NULL;
            
            if (!cg->methods) {
                cg->methods = slist_new(accessor);
            } else {
                slist_append(cg->methods, accessor);
            }
            
            method_gen_free(mg);
            free(comp_desc);
        }
        
        /* Generate hashCode() method using Objects.hash(fields...) */
        {
            method_info_gen_t *hashcode = calloc(1, sizeof(method_info_gen_t));
            hashcode->access_flags = ACC_PUBLIC;
            hashcode->name_index = cp_add_utf8(cg->cp, "hashCode");
            hashcode->descriptor_index = cp_add_utf8(cg->cp, "()I");
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = false;
            mg->is_constructor = false;
            mg->next_slot = 1;
            mg->max_locals = 1;
            
            /* Count components for array size */
            int num_components = 0;
            for (slist_t *c = record_components; c; c = c->next) num_components++;
            
            /* Create array for Objects.hash(...) */
            /* bipush/sipush count */
            if (num_components <= 127) {
                bc_emit(mg->code, OP_BIPUSH);
                bc_emit_u1(mg->code, (uint8_t)num_components);
            } else {
                bc_emit(mg->code, OP_SIPUSH);
                bc_emit_u2(mg->code, (uint16_t)num_components);
            }
            mg_push(mg, 1);
            
            /* anewarray Object */
            uint16_t object_class = cp_add_class(cg->cp, "java/lang/Object");
            bc_emit(mg->code, OP_ANEWARRAY);
            bc_emit_u2(mg->code, object_class);
            /* Stack: array (no change) */
            
            /* Store each field value in array */
            int idx = 0;
            for (slist_t *comp = record_components; comp; comp = comp->next, idx++) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                const char *comp_name = comp_node->data.node.name;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                
                /* dup array */
                bc_emit(mg->code, OP_DUP);
                mg_push(mg, 1);
                
                /* push index */
                if (idx <= 127) {
                    bc_emit(mg->code, OP_BIPUSH);
                    bc_emit_u1(mg->code, (uint8_t)idx);
                } else {
                    bc_emit(mg->code, OP_SIPUSH);
                    bc_emit_u2(mg->code, (uint16_t)idx);
                }
                mg_push(mg, 1);
                
                /* aload_0 (this) and getfield */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name, comp_name, comp_desc);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, field_ref);
                /* Stack: unchanged for refs, +1 for long/double */
                
                /* Box primitive types for Objects.hash */
                if (comp_type && comp_type->type == AST_PRIMITIVE_TYPE) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    uint16_t box_method;
                    if (strcmp(ptype, "int") == 0) {
                        box_method = cp_add_methodref(cg->cp, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
                    } else if (strcmp(ptype, "long") == 0) {
                        mg_push(mg, 1);  /* long is 2 slots */
                        box_method = cp_add_methodref(cg->cp, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;");
                    } else if (strcmp(ptype, "double") == 0) {
                        mg_push(mg, 1);
                        box_method = cp_add_methodref(cg->cp, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;");
                    } else if (strcmp(ptype, "float") == 0) {
                        box_method = cp_add_methodref(cg->cp, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;");
                    } else if (strcmp(ptype, "boolean") == 0) {
                        box_method = cp_add_methodref(cg->cp, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;");
                    } else if (strcmp(ptype, "char") == 0) {
                        box_method = cp_add_methodref(cg->cp, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;");
                    } else if (strcmp(ptype, "byte") == 0) {
                        box_method = cp_add_methodref(cg->cp, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;");
                    } else { /* short */
                        box_method = cp_add_methodref(cg->cp, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;");
                    }
                    bc_emit(mg->code, OP_INVOKESTATIC);
                    bc_emit_u2(mg->code, box_method);
                    if (strcmp(ptype, "long") == 0 || strcmp(ptype, "double") == 0) {
                        mg_pop(mg, 1);  /* Consumes 2 slots, produces 1 */
                    }
                }
                
                /* aastore */
                bc_emit(mg->code, OP_AASTORE);
                mg_pop(mg, 3);
                
                free(comp_desc);
            }
            
            /* invokestatic Objects.hash([Object)I */
            uint16_t hash_method = cp_add_methodref(cg->cp, "java/util/Objects", "hash", "([Ljava/lang/Object;)I");
            bc_emit(mg->code, OP_INVOKESTATIC);
            bc_emit_u2(mg->code, hash_method);
            /* Stack: int (replaces array) */
            
            /* ireturn */
            bc_emit(mg->code, OP_IRETURN);
            mg_pop(mg, 1);
            
            mg->code->max_stack = mg->max_stack > 5 ? mg->max_stack : 5;
            mg->code->max_locals = 1;
            hashcode->code = mg->code;
            mg->code = NULL;
            
            slist_append(cg->methods, hashcode);
            method_gen_free(mg);
        }
        
        /* Generate equals(Object) method */
        {
            method_info_gen_t *equals = calloc(1, sizeof(method_info_gen_t));
            equals->access_flags = ACC_PUBLIC;
            equals->name_index = cp_add_utf8(cg->cp, "equals");
            equals->descriptor_index = cp_add_utf8(cg->cp, "(Ljava/lang/Object;)Z");
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = false;
            mg->is_constructor = false;
            mg->next_slot = 2;  /* this + obj */
            mg->max_locals = 3;  /* this, obj, casted */
            
            /* Initialize stackmap for equals method */
            mg->stackmap = stackmap_new();
            if (mg->stackmap) {
                /* Slot 0: this (record type), Slot 1: Object parameter */
                stackmap_set_local_object(mg->stackmap, 0, cg->cp, cg->internal_name);
                stackmap_set_local_object(mg->stackmap, 1, cg->cp, "java/lang/Object");
            }
            
            /* if (this == obj) return true; */
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            size_t not_same_pos = mg->code->length;
            bc_emit(mg->code, OP_IF_ACMPNE);
            bc_emit_u2(mg->code, 0);
            mg_pop(mg, 2);
            bc_emit(mg->code, OP_ICONST_1);
            bc_emit(mg->code, OP_IRETURN);
            
            /* Patch: not_same target */
            size_t not_same_target = mg->code->length;
            int16_t not_same_offset = (int16_t)(not_same_target - not_same_pos);
            mg->code->code[not_same_pos + 1] = (not_same_offset >> 8) & 0xFF;
            mg->code->code[not_same_pos + 2] = not_same_offset & 0xFF;
            
            /* Record stackmap frame at branch target */
            mg_record_frame(mg);
            
            /* if (!(obj instanceof RecordType)) return false; */
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            uint16_t this_class = cp_add_class(cg->cp, cg->internal_name);
            bc_emit(mg->code, OP_INSTANCEOF);
            bc_emit_u2(mg->code, this_class);
            size_t is_instance_pos = mg->code->length;
            bc_emit(mg->code, OP_IFNE);
            bc_emit_u2(mg->code, 0);
            mg_pop(mg, 1);
            bc_emit(mg->code, OP_ICONST_0);
            bc_emit(mg->code, OP_IRETURN);
            
            /* Patch: is_instance target */
            size_t is_instance_target = mg->code->length;
            int16_t is_instance_offset = (int16_t)(is_instance_target - is_instance_pos);
            mg->code->code[is_instance_pos + 1] = (is_instance_offset >> 8) & 0xFF;
            mg->code->code[is_instance_pos + 2] = is_instance_offset & 0xFF;
            
            /* Record stackmap frame at branch target */
            mg_record_frame(mg);
            
            /* RecordType other = (RecordType) obj; */
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_CHECKCAST);
            bc_emit_u2(mg->code, this_class);
            bc_emit(mg->code, OP_ASTORE_2);
            mg_pop(mg, 1);
            
            /* Update stackmap to reflect that slot 2 now has the casted type */
            if (mg->stackmap) {
                stackmap_set_local_object(mg->stackmap, 2, cg->cp, cg->internal_name);
            }
            
            /* Compare each field - if any differs, return false; else return true
             * We'll collect all the "fail" jump positions and patch them at the end */
            slist_t *fail_jumps = NULL;  /* List of (size_t) positions to patch */
            
            for (slist_t *comp = record_components; comp; comp = comp->next) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                const char *comp_name = comp_node->data.node.name;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name, comp_name, comp_desc);
                
                bool is_primitive = comp_type && comp_type->type == AST_PRIMITIVE_TYPE;
                
                if (is_primitive) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    /* Use direct comparison for primitives */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    
                    bc_emit(mg->code, OP_ALOAD_2);
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    
                    size_t cmp_fail_pos = mg->code->length;
                    if (ptype && strcmp(ptype, "long") == 0) {
                        mg_push(mg, 2);  /* 2 longs */
                        bc_emit(mg->code, OP_LCMP);
                        mg_pop(mg, 3);
                        bc_emit(mg->code, OP_IFNE);
                    } else if (ptype && strcmp(ptype, "double") == 0) {
                        mg_push(mg, 2);
                        bc_emit(mg->code, OP_DCMPL);
                        mg_pop(mg, 3);
                        bc_emit(mg->code, OP_IFNE);
                    } else if (ptype && strcmp(ptype, "float") == 0) {
                        mg_push(mg, 0);  /* 2 floats */
                        bc_emit(mg->code, OP_FCMPL);
                        mg_pop(mg, 1);
                        bc_emit(mg->code, OP_IFNE);
                    } else {
                        mg_pop(mg, 1);
                        bc_emit(mg->code, OP_IF_ICMPNE);
                    }
                    bc_emit_u2(mg->code, 0);  /* placeholder offset */
                    mg_pop(mg, 1);
                    
                    /* Record position to patch later */
                    size_t *pos = malloc(sizeof(size_t));
                    *pos = cmp_fail_pos;
                    if (!fail_jumps) {
                        fail_jumps = slist_new(pos);
                    } else {
                        slist_append(fail_jumps, pos);
                    }
                } else {
                    /* Use Objects.equals for references */
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    
                    bc_emit(mg->code, OP_ALOAD_2);
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    
                    uint16_t obj_equals = cp_add_methodref(cg->cp, "java/util/Objects", "equals", 
                        "(Ljava/lang/Object;Ljava/lang/Object;)Z");
                    bc_emit(mg->code, OP_INVOKESTATIC);
                    bc_emit_u2(mg->code, obj_equals);
                    mg_pop(mg, 1);  /* Consumes 2, produces 1 */
                    
                    size_t eq_fail_pos = mg->code->length;
                    bc_emit(mg->code, OP_IFEQ);  /* if equals() returns 0 (false), jump to fail */
                    bc_emit_u2(mg->code, 0);  /* placeholder */
                    mg_pop(mg, 1);
                    
                    /* Record position to patch later */
                    size_t *pos = malloc(sizeof(size_t));
                    *pos = eq_fail_pos;
                    if (!fail_jumps) {
                        fail_jumps = slist_new(pos);
                    } else {
                        slist_append(fail_jumps, pos);
                    }
                }
                
                free(comp_desc);
            }
            
            /* return true (all fields matched) */
            bc_emit(mg->code, OP_ICONST_1);
            bc_emit(mg->code, OP_IRETURN);
            
            /* return false (some field didn't match) */
            size_t return_false_target = mg->code->length;
            
            /* Record stackmap frame at return_false target */
            mg_record_frame(mg);
            
            bc_emit(mg->code, OP_ICONST_0);
            bc_emit(mg->code, OP_IRETURN);
            
            /* Patch all fail jumps to point to return_false_target */
            for (slist_t *fj = fail_jumps; fj; fj = fj->next) {
                size_t *pos_ptr = (size_t *)fj->data;
                size_t jump_pos = *pos_ptr;
                int16_t offset = (int16_t)(return_false_target - jump_pos);
                mg->code->code[jump_pos + 1] = (offset >> 8) & 0xFF;
                mg->code->code[jump_pos + 2] = offset & 0xFF;
                free(pos_ptr);
            }
            slist_free(fail_jumps);
            
            mg->code->max_stack = mg->max_stack > 4 ? mg->max_stack : 4;
            mg->code->max_locals = 3;
            
            equals->code = mg->code;
            mg->code = NULL;
            
            /* Transfer stackmap ownership to method info */
            if (mg->stackmap && mg->stackmap->num_entries > 0) {
                equals->stackmap = mg->stackmap;
                mg->stackmap = NULL;
                cg->use_stackmap = true;
            }
            
            slist_append(cg->methods, equals);
            method_gen_free(mg);
        }
        
        /* Generate toString() method using simple format "RecordName[field1=value1, ...]" */
        {
            method_info_gen_t *tostr = calloc(1, sizeof(method_info_gen_t));
            tostr->access_flags = ACC_PUBLIC;
            tostr->name_index = cp_add_utf8(cg->cp, "toString");
            tostr->descriptor_index = cp_add_utf8(cg->cp, "()Ljava/lang/String;");
            
            method_gen_t *mg = calloc(1, sizeof(method_gen_t));
            mg->code = bytecode_new();
            mg->cp = cg->cp;
            mg->class_gen = cg;
            mg->locals = hashtable_new();
            mg->is_static = false;
            mg->is_constructor = false;
            mg->next_slot = 1;
            mg->max_locals = 1;
            
            /* Use StringBuilder to build the string */
            uint16_t sb_class = cp_add_class(cg->cp, "java/lang/StringBuilder");
            uint16_t sb_init = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "<init>", "()V");
            uint16_t sb_append_str = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append", 
                "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
            uint16_t sb_append_obj = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
            uint16_t sb_append_int = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(I)Ljava/lang/StringBuilder;");
            uint16_t sb_append_long = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(J)Ljava/lang/StringBuilder;");
            uint16_t sb_append_double = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(D)Ljava/lang/StringBuilder;");
            uint16_t sb_append_float = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(F)Ljava/lang/StringBuilder;");
            uint16_t sb_append_bool = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(Z)Ljava/lang/StringBuilder;");
            uint16_t sb_append_char = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "append",
                "(C)Ljava/lang/StringBuilder;");
            uint16_t sb_tostring = cp_add_methodref(cg->cp, "java/lang/StringBuilder", "toString",
                "()Ljava/lang/String;");
            
            /* new StringBuilder() */
            bc_emit(mg->code, OP_NEW);
            bc_emit_u2(mg->code, sb_class);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_DUP);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_INVOKESPECIAL);
            bc_emit_u2(mg->code, sb_init);
            mg_pop(mg, 1);
            
            /* Get simple class name from internal name */
            const char *simple_name = cg->internal_name;
            const char *last_slash = strrchr(cg->internal_name, '/');
            if (last_slash) simple_name = last_slash + 1;
            
            /* Build prefix: "ClassName[" */
            string_t *prefix = string_new("");
            string_append(prefix, simple_name);
            string_append(prefix, "[");
            
            uint16_t prefix_str = cp_add_string(cg->cp, prefix->str);
            bc_emit(mg->code, OP_LDC_W);
            bc_emit_u2(mg->code, prefix_str);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, sb_append_str);
            mg_pop(mg, 1);  /* Consumes string, returns StringBuilder */
            
            string_free(prefix, true);
            
            /* Append each field */
            bool first = true;
            for (slist_t *comp = record_components; comp; comp = comp->next) {
                ast_node_t *comp_node = (ast_node_t *)comp->data;
                const char *comp_name = comp_node->data.node.name;
                ast_node_t *comp_type = NULL;
                if (comp_node->data.node.children) {
                    comp_type = (ast_node_t *)comp_node->data.node.children->data;
                }
                char *comp_desc = comp_type ? ast_type_to_descriptor(comp_type) : strdup("Ljava/lang/Object;");
                
                /* Append ", " if not first */
                if (!first) {
                    uint16_t comma_str = cp_add_string(cg->cp, ", ");
                    bc_emit(mg->code, OP_LDC_W);
                    bc_emit_u2(mg->code, comma_str);
                    mg_push(mg, 1);
                    bc_emit(mg->code, OP_INVOKEVIRTUAL);
                    bc_emit_u2(mg->code, sb_append_str);
                    mg_pop(mg, 1);
                }
                first = false;
                
                /* Append "fieldName=" */
                string_t *field_prefix = string_new("");
                string_append(field_prefix, comp_name);
                string_append(field_prefix, "=");
                uint16_t field_str = cp_add_string(cg->cp, field_prefix->str);
                bc_emit(mg->code, OP_LDC_W);
                bc_emit_u2(mg->code, field_str);
                mg_push(mg, 1);
                bc_emit(mg->code, OP_INVOKEVIRTUAL);
                bc_emit_u2(mg->code, sb_append_str);
                mg_pop(mg, 1);
                string_free(field_prefix, true);
                
                /* Load and append field value */
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                uint16_t field_ref = cp_add_fieldref(cg->cp, cg->internal_name, comp_name, comp_desc);
                bc_emit(mg->code, OP_GETFIELD);
                bc_emit_u2(mg->code, field_ref);
                
                /* Append based on type */
                uint16_t append_method;
                if (comp_type && comp_type->type == AST_PRIMITIVE_TYPE) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    if (strcmp(ptype, "long") == 0) {
                        mg_push(mg, 1);
                        append_method = sb_append_long;
                    } else if (strcmp(ptype, "double") == 0) {
                        mg_push(mg, 1);
                        append_method = sb_append_double;
                    } else if (strcmp(ptype, "float") == 0) {
                        append_method = sb_append_float;
                    } else if (strcmp(ptype, "boolean") == 0) {
                        append_method = sb_append_bool;
                    } else if (strcmp(ptype, "char") == 0) {
                        append_method = sb_append_char;
                    } else {
                        append_method = sb_append_int;
                    }
                } else {
                    append_method = sb_append_obj;
                }
                
                bc_emit(mg->code, OP_INVOKEVIRTUAL);
                bc_emit_u2(mg->code, append_method);
                if (comp_type && comp_type->type == AST_PRIMITIVE_TYPE) {
                    const char *ptype = comp_type->data.leaf.name;  /* leaf for primitives */
                    if (strcmp(ptype, "long") == 0 || strcmp(ptype, "double") == 0) {
                        mg_pop(mg, 2);
                    } else {
                        mg_pop(mg, 1);
                    }
                } else {
                    mg_pop(mg, 1);
                }
                
                free(comp_desc);
            }
            
            /* Append "]" */
            uint16_t suffix_str = cp_add_string(cg->cp, "]");
            bc_emit(mg->code, OP_LDC_W);
            bc_emit_u2(mg->code, suffix_str);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, sb_append_str);
            mg_pop(mg, 1);
            
            /* toString() */
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, sb_tostring);
            /* Stack: String (replaces StringBuilder) */
            
            /* areturn */
            bc_emit(mg->code, OP_ARETURN);
            mg_pop(mg, 1);
            
            mg->code->max_stack = mg->max_stack > 3 ? mg->max_stack : 3;
            mg->code->max_locals = 1;
            tostr->code = mg->code;
            mg->code = NULL;
            
            slist_append(cg->methods, tostr);
            method_gen_free(mg);
        }
    }
    
    /* Free record_components list */
    slist_free(record_components);
    
    return true;
}

/* ========================================================================
 * Anonymous Class Code Generation
 * ======================================================================== */

/**
 * Generate bytecode for an anonymous class.
 * Anonymous classes don't have AST_CLASS_DECL nodes - they have a symbol
 * with an anonymous_body (AST_BLOCK containing members).
 */
bool codegen_anonymous_class(class_gen_t *cg, symbol_t *anon_sym)
{
    if (!cg || !anon_sym || !anon_sym->data.class_data.anonymous_body) {
        return false;
    }
    
    ast_node_t *body = anon_sym->data.class_data.anonymous_body;
    
    /* Track whether we have an explicit constructor */
    bool has_constructor = false;
    
    /* Initialize instance initializer lists */
    cg->instance_initializers = NULL;
    cg->instance_field_inits = NULL;
    
    /* Collect static initializers */
    slist_t *static_initializers = NULL;
    slist_t *static_field_inits = NULL;
    
    /* Process members in the anonymous class body */
    slist_t *children = body->data.node.children;
    
    while (children) {
        ast_node_t *member = (ast_node_t *)children->data;
        
        switch (member->type) {
            case AST_FIELD_DECL:
                {
                    /* Get field type and name from children */
                    slist_t *fchildren = member->data.node.children;
                    ast_node_t *field_type = NULL;
                    
                    while (fchildren) {
                        ast_node_t *child = (ast_node_t *)fchildren->data;
                        
                        if (child->type == AST_PRIMITIVE_TYPE ||
                            child->type == AST_CLASS_TYPE ||
                            child->type == AST_ARRAY_TYPE) {
                            field_type = child;
                        } else if (child->type == AST_VAR_DECLARATOR) {
                            /* Add field - convert MOD_ flags to ACC_ flags */
                            field_gen_t *fg = calloc(1, sizeof(field_gen_t));
                            fg->access_flags = mods_to_access_flags(member->data.node.flags);
                            
                            /* Store name and descriptor */
                            fg->name = strdup(child->data.node.name);
                            fg->descriptor = ast_type_to_descriptor(field_type);
                            
                            fg->name_index = cp_add_utf8(cg->cp, fg->name);
                            fg->descriptor_index = cp_add_utf8(cg->cp, fg->descriptor);
                            
                            /* Add to field list */
                            if (!cg->fields) {
                                cg->fields = slist_new(fg);
                            } else {
                                slist_append(cg->fields, fg);
                            }
                            
                            /* Add to field lookup map */
                            hashtable_insert(cg->field_map, fg->name, fg);
                            
                            /* Check for field initializer */
                            bool is_static = (fg->access_flags & ACC_STATIC) != 0;
                            if (child->data.node.children) {
                                /* Has initializer */
                                ast_node_t *init_expr = (ast_node_t *)child->data.node.children->data;
                                ast_node_t *assign = ast_new(AST_ASSIGNMENT_EXPR,
                                    child->line, child->column);
                                assign->data.node.name = "=";
                                ast_node_t *field_access = ast_new(AST_IDENTIFIER,
                                    child->line, child->column);
                                field_access->data.leaf.name = fg->name;
                                ast_add_child(assign, field_access);
                                ast_add_child(assign, init_expr);
                                
                                if (is_static) {
                                    if (!static_field_inits) {
                                        static_field_inits = slist_new(assign);
                                    } else {
                                        slist_append(static_field_inits, assign);
                                    }
                                } else {
                                    if (!cg->instance_field_inits) {
                                        cg->instance_field_inits = slist_new(assign);
                                    } else {
                                        slist_append(cg->instance_field_inits, assign);
                                    }
                                }
                            }
                        }
                        fchildren = fchildren->next;
                    }
                }
                break;
            
            case AST_METHOD_DECL:
                if (member->data.node.flags & MOD_ABSTRACT) {
                    /* Abstract method - shouldn't happen in anonymous class */
                    if (!codegen_abstract_method(cg, member)) {
                        return false;
                    }
                } else {
                    if (!codegen_method(cg, member)) {
                        return false;
                    }
                }
                break;
            
            case AST_CONSTRUCTOR_DECL:
                /* Anonymous classes can't have explicit constructors, but just in case */
                has_constructor = true;
                if (!codegen_method(cg, member)) {
                    return false;
                }
                break;
            
            case AST_INITIALIZER_BLOCK:
                if (member->data.node.flags & MOD_STATIC) {
                    if (!static_initializers) {
                        static_initializers = slist_new(member);
                    } else {
                        slist_append(static_initializers, member);
                    }
                } else {
                    if (!cg->instance_initializers) {
                        cg->instance_initializers = slist_new(member);
                    } else {
                        slist_append(cg->instance_initializers, member);
                    }
                }
                break;
            
            default:
                break;
        }
        
        children = children->next;
    }
    
    /* Generate default constructor - anonymous classes always need one
     * unless they have an explicit one (which is uncommon) */
    if (!has_constructor) {
        method_info_gen_t *init = calloc(1, sizeof(method_info_gen_t));
        init->access_flags = 0x0000;  /* Package-private for anonymous classes */
        init->name_index = cp_add_utf8(cg->cp, "<init>");
        
        /* Build constructor descriptor */
        string_t *desc = string_new("(");
        string_t *super_desc = string_new("(");  /* For calling super() */
        
        /* For anonymous classes (like inner classes), take outer instance as parameter */
        if (cg->is_inner_class && cg->outer_class_internal) {
            string_append(desc, "L");
            string_append(desc, cg->outer_class_internal);
            string_append(desc, ";");
        }
        
        /* Check if superclass is a non-static inner class that needs enclosing instance */
        bool super_is_inner = false;
        symbol_t *super_enclosing = NULL;
        if (cg->class_sym && cg->class_sym->data.class_data.superclass) {
            symbol_t *super_sym = cg->class_sym->data.class_data.superclass;
            if (super_sym->data.class_data.enclosing_class &&
                !(super_sym->modifiers & MOD_STATIC)) {
                super_is_inner = true;
                super_enclosing = super_sym->data.class_data.enclosing_class;
                /* Add enclosing class type to super descriptor */
                string_append(super_desc, "L");
                const char *enc_name = super_enclosing->qualified_name;
                char *enc_internal = class_to_internal_name(enc_name);
                string_append(super_desc, enc_internal);
                string_append(super_desc, ";");
                free(enc_internal);
            }
        }
        
        /* Add superclass constructor argument types (for anonymous classes) */
        slist_t *super_ctor_args = NULL;
        if (cg->class_sym && cg->class_sym->data.class_data.super_ctor_args) {
            super_ctor_args = cg->class_sym->data.class_data.super_ctor_args;
            for (slist_t *arg = super_ctor_args; arg; arg = arg->next) {
                ast_node_t *arg_node = (ast_node_t *)arg->data;
                /* Use sem_type if available, otherwise try to get expression type */
                type_t *arg_type = arg_node ? arg_node->sem_type : NULL;
                if (!arg_type && arg_node && cg->sem) {
                    arg_type = get_expression_type(cg->sem, arg_node);
                }
                if (arg_type) {
                    char *arg_desc = type_to_descriptor(arg_type);
                    string_append(desc, arg_desc);
                    string_append(super_desc, arg_desc);
                    free(arg_desc);
                }
            }
        }
        
        /* Add captured variable types */
        if (cg->captured_vars) {
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (var_sym && var_sym->type) {
                    char *cap_desc = type_to_descriptor(var_sym->type);
                    string_append(desc, cap_desc);
                    free(cap_desc);
                }
            }
        }
        
        string_append(desc, ")V");
        string_append(super_desc, ")V");
        init->descriptor_index = cp_add_utf8(cg->cp, desc->str);
        string_free(desc, true);
        
        /* Create method_gen_t for generating code */
        method_gen_t *mg = calloc(1, sizeof(method_gen_t));
        mg->code = bytecode_new();
        mg->cp = cg->cp;
        mg->class_gen = cg;
        mg->locals = hashtable_new();
        mg->method = NULL;
        mg->loop_stack = NULL;
        mg->is_static = false;
        mg->is_constructor = true;
        mg->line_numbers = NULL;
        mg->local_var_table = NULL;
        
        /* Initialize StackMapTable tracking */
        mg->stackmap = stackmap_new();
        if (mg->stackmap && cg->internal_name) {
            stackmap_init_method(mg->stackmap, mg, mg->is_static, cg->internal_name);
        }
        
        /* Calculate next slot */
        mg->next_slot = 1;  /* 'this' is slot 0 */
        mg->max_locals = 1;
        
        if (cg->is_inner_class) {
            mg->next_slot = 2;  /* outer instance in slot 1 */
            mg->max_locals = 2;
        }
        
        /* Account for superclass constructor arguments */
        uint16_t super_arg_start_slot = mg->next_slot;
        if (super_ctor_args) {
            for (slist_t *arg = super_ctor_args; arg; arg = arg->next) {
                ast_node_t *arg_node = (ast_node_t *)arg->data;
                type_t *arg_type = arg_node ? arg_node->sem_type : NULL;
                if (!arg_type && arg_node && cg->sem) {
                    arg_type = get_expression_type(cg->sem, arg_node);
                }
                if (arg_type) {
                    int size = (arg_type->kind == TYPE_LONG ||
                               arg_type->kind == TYPE_DOUBLE) ? 2 : 1;
                    mg->next_slot += size;
                    mg->max_locals = mg->next_slot;
                }
            }
        }
        
        /* Account for captured variables */
        if (cg->captured_vars) {
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (var_sym && var_sym->type) {
                    int size = (var_sym->type->kind == TYPE_LONG || 
                               var_sym->type->kind == TYPE_DOUBLE) ? 2 : 1;
                    mg->next_slot += size;
                    mg->max_locals = mg->next_slot;
                }
            }
        }
        
        /* Record 'this' in LocalVariableTable */
        if (cg->internal_name) {
            char *this_desc = calloc(1, strlen(cg->internal_name) + 3);
            sprintf(this_desc, "L%s;", cg->internal_name);
            mg_record_local_var(mg, "this", this_desc, 0, 0, NULL);
            free(this_desc);
        }
        
        /* Record outer instance for inner classes */
        if (cg->is_inner_class && cg->outer_class_internal) {
            char *outer_desc = calloc(1, strlen(cg->outer_class_internal) + 3);
            sprintf(outer_desc, "L%s;", cg->outer_class_internal);
            mg_record_local_var(mg, "this$0", outer_desc, 1, 0, NULL);
            free(outer_desc);
        }
        
        /* For anonymous classes (inner), first store this$0 */
        if (cg->is_inner_class && cg->this_dollar_zero_ref) {
            bc_emit(mg->code, OP_ALOAD_0);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_ALOAD_1);
            mg_push(mg, 1);
            bc_emit(mg->code, OP_PUTFIELD);
            bc_emit_u2(mg->code, cg->this_dollar_zero_ref);
            mg_pop(mg, 2);
        }
        
        /* Store captured variables from constructor parameters */
        if (cg->captured_vars && cg->captured_field_refs) {
            uint16_t slot = cg->is_inner_class ? 2 : 1;
            
            for (slist_t *cap = cg->captured_vars; cap; cap = cap->next) {
                symbol_t *var_sym = (symbol_t *)cap->data;
                if (!var_sym || !var_sym->name) continue;
                
                void *ref_ptr = hashtable_lookup(cg->captured_field_refs, var_sym->name);
                if (!ref_ptr) continue;
                uint16_t field_ref = (uint16_t)(uintptr_t)ref_ptr;
                
                bc_emit(mg->code, OP_ALOAD_0);
                mg_push(mg, 1);
                
                type_kind_t kind = TYPE_CLASS;
                if (var_sym->type) {
                    kind = var_sym->type->kind;
                }
                
                if (kind == TYPE_LONG) {
                    bc_emit(mg->code, OP_LLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_DOUBLE) {
                    bc_emit(mg->code, OP_DLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 2);
                    slot += 2;
                } else if (kind == TYPE_FLOAT) {
                    bc_emit(mg->code, OP_FLOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
                    /* Reference types including type variables (erase to Object) */
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                } else {
                    bc_emit(mg->code, OP_ILOAD);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                    mg_push(mg, 1);
                    slot++;
                }
                
                bc_emit(mg->code, OP_PUTFIELD);
                bc_emit_u2(mg->code, field_ref);
                mg_pop(mg, kind == TYPE_LONG || kind == TYPE_DOUBLE ? 3 : 2);
            }
        }
        
        /* aload_0 - load 'this' */
        bc_emit(mg->code, OP_ALOAD_0);
        mg_push(mg, 1);
        
        /* If superclass is a non-static inner class, load enclosing instance for it.
         * The enclosing instance needed depends on which class owns the superclass:
         * - If super's enclosing class == our enclosing class, use slot 1 directly
         * - If super's enclosing class is an outer class of our enclosing class,
         *   we need to get it from the enclosing class's this$0 field chain */
        int super_arg_count = 0;
        if (super_is_inner && cg->is_inner_class && super_enclosing) {
            symbol_t *our_enclosing = cg->class_sym->data.class_data.enclosing_class;
            
            if (our_enclosing == super_enclosing) {
                /* Same enclosing class - load directly from slot 1 */
                bc_emit(mg->code, OP_ALOAD_1);
                mg_push(mg, 1);
            } else {
                /* Different enclosing class - traverse the this$0 chain.
                 * Load slot 1 (our enclosing instance), then follow the chain. */
                bc_emit(mg->code, OP_ALOAD_1);
                mg_push(mg, 1);
                
                /* Traverse from our enclosing class up to super's enclosing class */
                symbol_t *current = our_enclosing;
                while (current && current != super_enclosing) {
                    symbol_t *current_enc = current->data.class_data.enclosing_class;
                    if (!current_enc) break;
                    
                    /* Get this$0 from current class (points to current_enc) */
                    char *cur_internal = class_to_internal_name(current->qualified_name);
                    char *enc_internal = class_to_internal_name(current_enc->qualified_name);
                    size_t desc_len = strlen(enc_internal) + 3;
                    char *this0_desc = malloc(desc_len);
                    snprintf(this0_desc, desc_len, "L%s;", enc_internal);
                    
                    uint16_t this0_ref = cp_add_fieldref(cg->cp, cur_internal, "this$0", this0_desc);
                    bc_emit(mg->code, OP_GETFIELD);
                    bc_emit_u2(mg->code, this0_ref);
                    /* getfield pops 1, pushes 1 - no stack change */
                    
                    free(cur_internal);
                    free(enc_internal);
                    free(this0_desc);
                    
                    current = current_enc;
                }
            }
            super_arg_count++;
        }
        
        /* Load superclass constructor arguments */
        if (super_ctor_args) {
            uint16_t slot = super_arg_start_slot;
            for (slist_t *arg = super_ctor_args; arg; arg = arg->next) {
                ast_node_t *arg_node = (ast_node_t *)arg->data;
                type_t *arg_type = arg_node ? arg_node->sem_type : NULL;
                if (!arg_type && arg_node && cg->sem) {
                    arg_type = get_expression_type(cg->sem, arg_node);
                }
                if (arg_type) {
                    type_kind_t kind = arg_type->kind;
                    if (kind == TYPE_LONG) {
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_LLOAD_0 + slot);
                        } else {
                            bc_emit(mg->code, OP_LLOAD);
                            bc_emit_u1(mg->code, (uint8_t)slot);
                        }
                        mg_push(mg, 2);
                        slot += 2;
                    } else if (kind == TYPE_DOUBLE) {
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_DLOAD_0 + slot);
                        } else {
                            bc_emit(mg->code, OP_DLOAD);
                            bc_emit_u1(mg->code, (uint8_t)slot);
                        }
                        mg_push(mg, 2);
                        slot += 2;
                    } else if (kind == TYPE_FLOAT) {
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_FLOAD_0 + slot);
                        } else {
                            bc_emit(mg->code, OP_FLOAD);
                            bc_emit_u1(mg->code, (uint8_t)slot);
                        }
                        mg_push(mg, 1);
                        slot++;
                    } else if (kind == TYPE_BOOLEAN || kind == TYPE_BYTE ||
                               kind == TYPE_CHAR || kind == TYPE_SHORT ||
                               kind == TYPE_INT) {
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_ILOAD_0 + slot);
                        } else {
                            bc_emit(mg->code, OP_ILOAD);
                            bc_emit_u1(mg->code, (uint8_t)slot);
                        }
                        mg_push(mg, 1);
                        slot++;
                    } else {
                        /* Reference type */
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_ALOAD_0 + slot);
                        } else {
                            bc_emit(mg->code, OP_ALOAD);
                            bc_emit_u1(mg->code, (uint8_t)slot);
                        }
                        mg_push(mg, 1);
                        slot++;
                    }
                    super_arg_count++;
                }
            }
        }
        
        /* invokespecial superclass.<init> with proper descriptor */
        uint16_t super_init = cp_add_methodref(cg->cp, cg->superclass, "<init>", super_desc->str);
        bc_emit(mg->code, OP_INVOKESPECIAL);
        bc_emit_u2(mg->code, super_init);
        mg_pop(mg, 1 + super_arg_count);
        
        /* Mark 'this' as initialized in stackmap after super() */
        if (mg->stackmap && cg->internal_name) {
            stackmap_init_object(mg->stackmap, 0, cg->cp, cg->internal_name);
        }
        
        string_free(super_desc, true);
        
        /* Inject instance field initializers */
        for (slist_t *node = cg->instance_field_inits; node; node = node->next) {
            ast_node_t *assign = (ast_node_t *)node->data;
            slist_t *assign_children = assign->data.node.children;
            if (assign_children && assign_children->next) {
                ast_node_t *field_id = (ast_node_t *)assign_children->data;
                ast_node_t *init_expr = (ast_node_t *)assign_children->next->data;
                const char *field_name = field_id->data.leaf.name;
                
                field_gen_t *field = hashtable_lookup(cg->field_map, field_name);
                if (field) {
                    bc_emit(mg->code, OP_ALOAD_0);
                    mg_push(mg, 1);
                    codegen_expr(mg, init_expr, cg->cp);
                    uint16_t field_ref = cp_add_fieldref(cg->cp,
                        cg->internal_name, field_name, field->descriptor);
                    bc_emit(mg->code, OP_PUTFIELD);
                    bc_emit_u2(mg->code, field_ref);
                    mg_pop(mg, 2);
                }
            }
        }
        
        /* Inject instance initializer blocks */
        for (slist_t *node = cg->instance_initializers; node; node = node->next) {
            ast_node_t *init_block = (ast_node_t *)node->data;
            if (init_block->data.node.children) {
                ast_node_t *block = (ast_node_t *)init_block->data.node.children->data;
                codegen_statement(mg, block);
            }
        }
        
        /* return */
        bc_emit(mg->code, OP_RETURN);
        
        /* Set max_stack and max_locals */
        mg->code->max_stack = mg->max_stack;
        mg->code->max_locals = mg->max_locals;
        
        init->code = mg->code;
        mg->code = NULL;
        
        /* Transfer debug tables */
        init->line_numbers = mg->line_numbers;
        mg->line_numbers = NULL;
        init->local_var_table = mg->local_var_table;
        mg->local_var_table = NULL;
        
        /* Transfer stackmap */
        if (mg->stackmap && mg->stackmap->num_entries > 0) {
            init->stackmap = mg->stackmap;
            mg->stackmap = NULL;
            cg->use_stackmap = true;
        }
        
        method_gen_free(mg);
        
        /* Add to methods list */
        if (!cg->methods) {
            cg->methods = slist_new(init);
        } else {
            slist_append(cg->methods, init);
        }
    }
    
    /* Generate <clinit> if we have static initializers or field inits */
    if (static_field_inits || static_initializers) {
        method_info_gen_t *clinit = calloc(1, sizeof(method_info_gen_t));
        clinit->access_flags = 0x0008;  /* ACC_STATIC */
        clinit->name_index = cp_add_utf8(cg->cp, "<clinit>");
        clinit->descriptor_index = cp_add_utf8(cg->cp, "()V");
        
        method_gen_t *mg = calloc(1, sizeof(method_gen_t));
        mg->code = bytecode_new();
        mg->cp = cg->cp;
        mg->class_gen = cg;
        mg->locals = hashtable_new();
        mg->method = NULL;
        mg->loop_stack = NULL;
        mg->is_static = true;
        mg->is_constructor = false;
        mg->next_slot = 0;
        mg->max_locals = 0;
        
        /* Initialize StackMapTable tracking */
        mg->stackmap = stackmap_new();
        if (mg->stackmap && cg->internal_name) {
            stackmap_init_method(mg->stackmap, mg, mg->is_static, cg->internal_name);
        }
        
        /* Generate static field initializers */
        for (slist_t *node = static_field_inits; node; node = node->next) {
            ast_node_t *assign = (ast_node_t *)node->data;
            slist_t *assign_children = assign->data.node.children;
            if (assign_children && assign_children->next) {
                ast_node_t *field_id = (ast_node_t *)assign_children->data;
                ast_node_t *init_expr = (ast_node_t *)assign_children->next->data;
                const char *field_name = field_id->data.leaf.name;
                
                field_gen_t *field = hashtable_lookup(cg->field_map, field_name);
                if (field) {
                    codegen_expr(mg, init_expr, cg->cp);
                    uint16_t field_ref = cp_add_fieldref(cg->cp,
                        cg->internal_name, field_name, field->descriptor);
                    bc_emit(mg->code, OP_PUTSTATIC);
                    bc_emit_u2(mg->code, field_ref);
                    mg_pop(mg, 1);
                }
            }
        }
        
        /* Generate static initializer blocks */
        for (slist_t *node = static_initializers; node; node = node->next) {
            ast_node_t *init_block = (ast_node_t *)node->data;
            if (init_block->data.node.children) {
                ast_node_t *block = (ast_node_t *)init_block->data.node.children->data;
                codegen_statement(mg, block);
            }
        }
        
        /* return */
        bc_emit(mg->code, OP_RETURN);
        mg->last_opcode = OP_RETURN;
        
        mg->code->max_stack = mg->max_stack;
        mg->code->max_locals = mg->max_locals > 0 ? mg->max_locals : 0;
        clinit->code = mg->code;
        
        /* Transfer stackmap */
        if (mg->stackmap && mg->stackmap->num_entries > 0) {
            clinit->stackmap = mg->stackmap;
            mg->stackmap = NULL;
            cg->use_stackmap = true;
        }
        
        if (!cg->methods) {
            cg->methods = slist_new(clinit);
        } else {
            slist_append(cg->methods, clinit);
        }
        
        mg->code = NULL;
        method_gen_free(mg);
    }
    
    /* Free the initializer lists */
    slist_free(static_field_inits);
    slist_free(static_initializers);
    
    return true;
}

/* ========================================================================
 * Module Code Generation (module-info.class)
 * ======================================================================== */

/**
 * Convert a dotted name to internal form (replace . with /).
 */
static char *module_name_to_internal(const char *dotted)
{
    char *internal = strdup(dotted);
    for (char *p = internal; *p; p++) {
        if (*p == '.') *p = '/';
    }
    return internal;
}

/**
 * Generate module-info.class from a module declaration AST.
 * 
 * @param module_decl The AST_MODULE_DECL node
 * @param size_out    Output parameter for the size of the generated bytes
 * @return            Allocated byte array containing the class file, or NULL on error
 */
uint8_t *codegen_module(ast_node_t *module_decl, size_t *size_out)
{
    if (!module_decl || module_decl->type != AST_MODULE_DECL || !size_out) {
        return NULL;
    }
    
    const char *module_name = module_decl->data.node.name;
    bool is_open = (module_decl->data.node.flags & MOD_PUBLIC) != 0;
    
    /* Create constant pool */
    const_pool_t *cp = const_pool_new();
    
    /* Add module-info class name */
    uint16_t this_class = cp_add_class(cp, "module-info");
    
    /* Add the module name */
    uint16_t module_name_idx = cp_add_module(cp, module_name);
    
    /* Add "Module" attribute name */
    uint16_t module_attr_name = cp_add_utf8(cp, "Module");
    
    /* Collect directives from children */
    slist_t *requires_list = NULL;
    slist_t *exports_list = NULL;
    slist_t *opens_list = NULL;
    slist_t *uses_list = NULL;
    slist_t *provides_list = NULL;
    
    for (slist_t *node = module_decl->data.node.children; node; node = node->next) {
        ast_node_t *directive = (ast_node_t *)node->data;
        if (!directive) continue;
        
        switch (directive->type) {
            case AST_REQUIRES_DIRECTIVE:
                if (!requires_list) requires_list = slist_new(directive);
                else slist_append(requires_list, directive);
                break;
            case AST_EXPORTS_DIRECTIVE:
                if (!exports_list) exports_list = slist_new(directive);
                else slist_append(exports_list, directive);
                break;
            case AST_OPENS_DIRECTIVE:
                if (!opens_list) opens_list = slist_new(directive);
                else slist_append(opens_list, directive);
                break;
            case AST_USES_DIRECTIVE:
                if (!uses_list) uses_list = slist_new(directive);
                else slist_append(uses_list, directive);
                break;
            case AST_PROVIDES_DIRECTIVE:
                if (!provides_list) provides_list = slist_new(directive);
                else slist_append(provides_list, directive);
                break;
            default:
                break;
        }
    }
    
    /* Pre-add all referenced modules, packages, and classes to constant pool */
    /* Also build the attribute data */
    
    /* Requires entries: [module_idx, flags, version_idx] */
    typedef struct {
        uint16_t module_idx;
        uint16_t flags;
        uint16_t version_idx;
    } requires_entry_t;
    
    int requires_count = 0;
    requires_entry_t *requires_entries = NULL;
    
    for (slist_t *node = requires_list; node; node = node->next) {
        requires_count++;
    }
    if (requires_count > 0) {
        requires_entries = calloc(requires_count, sizeof(requires_entry_t));
        int i = 0;
        for (slist_t *node = requires_list; node; node = node->next, i++) {
            ast_node_t *req = (ast_node_t *)node->data;
            requires_entries[i].module_idx = cp_add_module(cp, req->data.node.name);
            requires_entries[i].flags = 0;
            if (req->data.node.flags & MOD_PUBLIC) {
                requires_entries[i].flags |= ACC_TRANSITIVE;
            }
            if (req->data.node.flags & MOD_STATIC) {
                requires_entries[i].flags |= ACC_STATIC_PHASE;
            }
            requires_entries[i].version_idx = 0;  /* No version */
        }
    }
    
    /* Exports entries: [package_idx, flags, to_count, to_modules...] */
    typedef struct {
        uint16_t package_idx;
        uint16_t flags;
        uint16_t to_count;
        uint16_t *to_modules;
    } exports_entry_t;
    
    int exports_count = 0;
    exports_entry_t *exports_entries = NULL;
    
    for (slist_t *node = exports_list; node; node = node->next) {
        exports_count++;
    }
    if (exports_count > 0) {
        exports_entries = calloc(exports_count, sizeof(exports_entry_t));
        int i = 0;
        for (slist_t *node = exports_list; node; node = node->next, i++) {
            ast_node_t *exp = (ast_node_t *)node->data;
            char *pkg_internal = module_name_to_internal(exp->data.node.name);
            exports_entries[i].package_idx = cp_add_package(cp, pkg_internal);
            free(pkg_internal);
            exports_entries[i].flags = 0;
            
            /* Count 'to' targets */
            int to_count = 0;
            for (slist_t *t = exp->data.node.children; t; t = t->next) {
                to_count++;
            }
            exports_entries[i].to_count = to_count;
            if (to_count > 0) {
                exports_entries[i].to_modules = calloc(to_count, sizeof(uint16_t));
                int j = 0;
                for (slist_t *t = exp->data.node.children; t; t = t->next, j++) {
                    ast_node_t *target = (ast_node_t *)t->data;
                    exports_entries[i].to_modules[j] = cp_add_module(cp, target->data.node.name);
                }
            }
        }
    }
    
    /* Opens entries: similar to exports */
    typedef struct {
        uint16_t package_idx;
        uint16_t flags;
        uint16_t to_count;
        uint16_t *to_modules;
    } opens_entry_t;
    
    int opens_count = 0;
    opens_entry_t *opens_entries = NULL;
    
    for (slist_t *node = opens_list; node; node = node->next) {
        opens_count++;
    }
    if (opens_count > 0) {
        opens_entries = calloc(opens_count, sizeof(opens_entry_t));
        int i = 0;
        for (slist_t *node = opens_list; node; node = node->next, i++) {
            ast_node_t *opn = (ast_node_t *)node->data;
            char *pkg_internal = module_name_to_internal(opn->data.node.name);
            opens_entries[i].package_idx = cp_add_package(cp, pkg_internal);
            free(pkg_internal);
            opens_entries[i].flags = 0;
            
            /* Count 'to' targets */
            int to_count = 0;
            for (slist_t *t = opn->data.node.children; t; t = t->next) {
                to_count++;
            }
            opens_entries[i].to_count = to_count;
            if (to_count > 0) {
                opens_entries[i].to_modules = calloc(to_count, sizeof(uint16_t));
                int j = 0;
                for (slist_t *t = opn->data.node.children; t; t = t->next, j++) {
                    ast_node_t *target = (ast_node_t *)t->data;
                    opens_entries[i].to_modules[j] = cp_add_module(cp, target->data.node.name);
                }
            }
        }
    }
    
    /* Uses entries: just class references */
    int uses_count = 0;
    uint16_t *uses_entries = NULL;
    
    for (slist_t *node = uses_list; node; node = node->next) {
        uses_count++;
    }
    if (uses_count > 0) {
        uses_entries = calloc(uses_count, sizeof(uint16_t));
        int i = 0;
        for (slist_t *node = uses_list; node; node = node->next, i++) {
            ast_node_t *use = (ast_node_t *)node->data;
            char *class_internal = module_name_to_internal(use->data.node.name);
            uses_entries[i] = cp_add_class(cp, class_internal);
            free(class_internal);
        }
    }
    
    /* Provides entries: [service_idx, with_count, with_classes...] */
    typedef struct {
        uint16_t service_idx;
        uint16_t with_count;
        uint16_t *with_classes;
    } provides_entry_t;
    
    int provides_count = 0;
    provides_entry_t *provides_entries = NULL;
    
    for (slist_t *node = provides_list; node; node = node->next) {
        provides_count++;
    }
    if (provides_count > 0) {
        provides_entries = calloc(provides_count, sizeof(provides_entry_t));
        int i = 0;
        for (slist_t *node = provides_list; node; node = node->next, i++) {
            ast_node_t *prov = (ast_node_t *)node->data;
            char *svc_internal = module_name_to_internal(prov->data.node.name);
            provides_entries[i].service_idx = cp_add_class(cp, svc_internal);
            free(svc_internal);
            
            /* Count implementation classes */
            int with_count = 0;
            for (slist_t *w = prov->data.node.children; w; w = w->next) {
                with_count++;
            }
            provides_entries[i].with_count = with_count;
            if (with_count > 0) {
                provides_entries[i].with_classes = calloc(with_count, sizeof(uint16_t));
                int j = 0;
                for (slist_t *w = prov->data.node.children; w; w = w->next, j++) {
                    ast_node_t *impl = (ast_node_t *)w->data;
                    char *impl_internal = module_name_to_internal(impl->data.node.name);
                    provides_entries[i].with_classes[j] = cp_add_class(cp, impl_internal);
                    free(impl_internal);
                }
            }
        }
    }
    
    /* Calculate Module attribute length */
    uint32_t module_attr_len = 2 + 2 + 2;  /* module_name_index, module_flags, module_version_index */
    module_attr_len += 2 + requires_count * 6;  /* requires_count + entries */
    module_attr_len += 2;  /* exports_count */
    for (int i = 0; i < exports_count; i++) {
        module_attr_len += 6 + exports_entries[i].to_count * 2;
    }
    module_attr_len += 2;  /* opens_count */
    for (int i = 0; i < opens_count; i++) {
        module_attr_len += 6 + opens_entries[i].to_count * 2;
    }
    module_attr_len += 2 + uses_count * 2;  /* uses_count + entries */
    module_attr_len += 2;  /* provides_count */
    for (int i = 0; i < provides_count; i++) {
        module_attr_len += 4 + provides_entries[i].with_count * 2;
    }
    
    /* Calculate total class file size */
    /* Magic + version + constant pool + access flags + this_class + super_class + 
       interfaces_count + fields_count + methods_count + attributes_count + Module attr */
    
    size_t cp_size = 0;
    for (uint16_t i = 1; i < cp->count; i++) {
        const_pool_entry_t *e = &cp->entries[i];
        switch (e->type) {
            case CONST_UTF8:
                cp_size += 3 + strlen(e->data.utf8);
                break;
            case CONST_CLASS:
            case CONST_MODULE:
            case CONST_PACKAGE:
                cp_size += 3;
                break;
            case CONST_NAME_AND_TYPE:
                cp_size += 5;
                break;
            default:
                cp_size += 1;  /* Just tag for unknown */
                break;
        }
    }
    
    size_t total_size = 4 + 2 + 2;  /* magic + minor + major */
    total_size += 2 + cp_size;      /* constant_pool_count + entries */
    total_size += 2 + 2 + 2;        /* access_flags + this_class + super_class */
    total_size += 2 + 2 + 2;        /* interfaces_count + fields_count + methods_count */
    total_size += 2;                /* attributes_count */
    total_size += 6 + module_attr_len;  /* Module attribute */
    
    /* Allocate and write */
    uint8_t *bytes = malloc(total_size);
    if (!bytes) {
        const_pool_free(cp);
        free(requires_entries);
        for (int i = 0; i < exports_count; i++) free(exports_entries[i].to_modules);
        free(exports_entries);
        for (int i = 0; i < opens_count; i++) free(opens_entries[i].to_modules);
        free(opens_entries);
        free(uses_entries);
        for (int i = 0; i < provides_count; i++) free(provides_entries[i].with_classes);
        free(provides_entries);
        slist_free(requires_list);
        slist_free(exports_list);
        slist_free(opens_list);
        slist_free(uses_list);
        slist_free(provides_list);
        return NULL;
    }
    
    uint8_t *p = bytes;
    
    /* Magic number */
    *p++ = 0xCA; *p++ = 0xFE; *p++ = 0xBA; *p++ = 0xBE;
    
    /* Version: 53.0 (Java 9) */
    *p++ = 0x00; *p++ = 0x00;  /* minor */
    *p++ = 0x00; *p++ = 0x35;  /* major = 53 */
    
    /* Constant pool count */
    *p++ = (cp->count >> 8) & 0xFF;
    *p++ = cp->count & 0xFF;
    
    /* Constant pool entries */
    for (uint16_t i = 1; i < cp->count; i++) {
        const_pool_entry_t *e = &cp->entries[i];
        *p++ = e->type;
        switch (e->type) {
            case CONST_UTF8:
                {
                    uint16_t len = strlen(e->data.utf8);
                    *p++ = (len >> 8) & 0xFF;
                    *p++ = len & 0xFF;
                    memcpy(p, e->data.utf8, len);
                    p += len;
                }
                break;
            case CONST_CLASS:
            case CONST_MODULE:
            case CONST_PACKAGE:
                *p++ = (e->data.class_index >> 8) & 0xFF;
                *p++ = e->data.class_index & 0xFF;
                break;
            case CONST_NAME_AND_TYPE:
                *p++ = (e->data.name_type.name_index >> 8) & 0xFF;
                *p++ = e->data.name_type.name_index & 0xFF;
                *p++ = (e->data.name_type.descriptor_index >> 8) & 0xFF;
                *p++ = e->data.name_type.descriptor_index & 0xFF;
                break;
            default:
                break;
        }
    }
    
    /* Access flags: ACC_MODULE (optionally | ACC_OPEN) */
    uint16_t access_flags = ACC_MODULE;
    if (is_open) access_flags |= ACC_OPEN;
    *p++ = (access_flags >> 8) & 0xFF;
    *p++ = access_flags & 0xFF;
    
    /* this_class */
    *p++ = (this_class >> 8) & 0xFF;
    *p++ = this_class & 0xFF;
    
    /* super_class = 0 */
    *p++ = 0x00; *p++ = 0x00;
    
    /* interfaces_count = 0 */
    *p++ = 0x00; *p++ = 0x00;
    
    /* fields_count = 0 */
    *p++ = 0x00; *p++ = 0x00;
    
    /* methods_count = 0 */
    *p++ = 0x00; *p++ = 0x00;
    
    /* attributes_count = 1 */
    *p++ = 0x00; *p++ = 0x01;
    
    /* Module attribute */
    *p++ = (module_attr_name >> 8) & 0xFF;
    *p++ = module_attr_name & 0xFF;
    *p++ = (module_attr_len >> 24) & 0xFF;
    *p++ = (module_attr_len >> 16) & 0xFF;
    *p++ = (module_attr_len >> 8) & 0xFF;
    *p++ = module_attr_len & 0xFF;
    
    /* module_name_index */
    *p++ = (module_name_idx >> 8) & 0xFF;
    *p++ = module_name_idx & 0xFF;
    
    /* module_flags */
    uint16_t module_flags = is_open ? ACC_OPEN : 0;
    *p++ = (module_flags >> 8) & 0xFF;
    *p++ = module_flags & 0xFF;
    
    /* module_version_index = 0 (no version) */
    *p++ = 0x00; *p++ = 0x00;
    
    /* requires_count */
    *p++ = (requires_count >> 8) & 0xFF;
    *p++ = requires_count & 0xFF;
    
    /* requires entries */
    for (int i = 0; i < requires_count; i++) {
        *p++ = (requires_entries[i].module_idx >> 8) & 0xFF;
        *p++ = requires_entries[i].module_idx & 0xFF;
        *p++ = (requires_entries[i].flags >> 8) & 0xFF;
        *p++ = requires_entries[i].flags & 0xFF;
        *p++ = (requires_entries[i].version_idx >> 8) & 0xFF;
        *p++ = requires_entries[i].version_idx & 0xFF;
    }
    
    /* exports_count */
    *p++ = (exports_count >> 8) & 0xFF;
    *p++ = exports_count & 0xFF;
    
    /* exports entries */
    for (int i = 0; i < exports_count; i++) {
        *p++ = (exports_entries[i].package_idx >> 8) & 0xFF;
        *p++ = exports_entries[i].package_idx & 0xFF;
        *p++ = (exports_entries[i].flags >> 8) & 0xFF;
        *p++ = exports_entries[i].flags & 0xFF;
        *p++ = (exports_entries[i].to_count >> 8) & 0xFF;
        *p++ = exports_entries[i].to_count & 0xFF;
        for (int j = 0; j < exports_entries[i].to_count; j++) {
            *p++ = (exports_entries[i].to_modules[j] >> 8) & 0xFF;
            *p++ = exports_entries[i].to_modules[j] & 0xFF;
        }
    }
    
    /* opens_count */
    *p++ = (opens_count >> 8) & 0xFF;
    *p++ = opens_count & 0xFF;
    
    /* opens entries */
    for (int i = 0; i < opens_count; i++) {
        *p++ = (opens_entries[i].package_idx >> 8) & 0xFF;
        *p++ = opens_entries[i].package_idx & 0xFF;
        *p++ = (opens_entries[i].flags >> 8) & 0xFF;
        *p++ = opens_entries[i].flags & 0xFF;
        *p++ = (opens_entries[i].to_count >> 8) & 0xFF;
        *p++ = opens_entries[i].to_count & 0xFF;
        for (int j = 0; j < opens_entries[i].to_count; j++) {
            *p++ = (opens_entries[i].to_modules[j] >> 8) & 0xFF;
            *p++ = opens_entries[i].to_modules[j] & 0xFF;
        }
    }
    
    /* uses_count */
    *p++ = (uses_count >> 8) & 0xFF;
    *p++ = uses_count & 0xFF;
    
    /* uses entries */
    for (int i = 0; i < uses_count; i++) {
        *p++ = (uses_entries[i] >> 8) & 0xFF;
        *p++ = uses_entries[i] & 0xFF;
    }
    
    /* provides_count */
    *p++ = (provides_count >> 8) & 0xFF;
    *p++ = provides_count & 0xFF;
    
    /* provides entries */
    for (int i = 0; i < provides_count; i++) {
        *p++ = (provides_entries[i].service_idx >> 8) & 0xFF;
        *p++ = provides_entries[i].service_idx & 0xFF;
        *p++ = (provides_entries[i].with_count >> 8) & 0xFF;
        *p++ = provides_entries[i].with_count & 0xFF;
        for (int j = 0; j < provides_entries[i].with_count; j++) {
            *p++ = (provides_entries[i].with_classes[j] >> 8) & 0xFF;
            *p++ = provides_entries[i].with_classes[j] & 0xFF;
        }
    }
    
    /* Clean up */
    const_pool_free(cp);
    free(requires_entries);
    for (int i = 0; i < exports_count; i++) free(exports_entries[i].to_modules);
    free(exports_entries);
    for (int i = 0; i < opens_count; i++) free(opens_entries[i].to_modules);
    free(opens_entries);
    free(uses_entries);
    for (int i = 0; i < provides_count; i++) free(provides_entries[i].with_classes);
    free(provides_entries);
    slist_free(requires_list);
    slist_free(exports_list);
    slist_free(opens_list);
    slist_free(uses_list);
    slist_free(provides_list);
    
    *size_out = p - bytes;
    return bytes;
}

/**
 * Generate package-info.class from a package declaration AST.
 * 
 * This is called for package-info.java files which contain
 * package-level annotations like: @Deprecated package com.example;
 * 
 * The generated class file is a synthetic interface named "package-info"
 * with ACC_INTERFACE | ACC_ABSTRACT | ACC_SYNTHETIC flags.
 * 
 * @param package_decl   The AST_PACKAGE_DECL node
 * @param annotations    List of annotation AST nodes, or NULL
 * @param sem            Semantic analyzer for annotation resolution
 * @param target_version Target class file version
 * @param size_out       Output parameter for the size of the generated bytes
 * @return               Allocated byte array containing the class file, or NULL on error
 */
uint8_t *codegen_package_info(ast_node_t *package_decl, slist_t *annotations,
                              semantic_t *sem, int target_version, size_t *size_out)
{
    if (!package_decl || package_decl->type != AST_PACKAGE_DECL || !size_out) {
        return NULL;
    }
    
    const char *package_name = package_decl->data.node.name;
    if (!package_name) {
        return NULL;
    }
    
    /* Create constant pool */
    const_pool_t *cp = const_pool_new();
    
    /* Convert package name to internal form: com.example -> com/example/package-info */
    size_t pkg_len = strlen(package_name);
    char *internal_name = malloc(pkg_len + 20);  /* room for /package-info */
    strcpy(internal_name, package_name);
    for (char *p = internal_name; *p; p++) {
        if (*p == '.') *p = '/';
    }
    strcat(internal_name, "/package-info");
    
    /* Add class entries */
    uint16_t this_class = cp_add_class(cp, internal_name);
    uint16_t super_class = cp_add_class(cp, "java/lang/Object");
    
    /* Add SourceFile attribute */
    uint16_t source_file_attr = cp_add_utf8(cp, "SourceFile");
    uint16_t source_file_name = cp_add_utf8(cp, "package-info.java");
    
    /* Process annotations from package declaration children */
    slist_t *all_annotations = NULL;
    
    /* Collect annotations from package_decl's children */
    for (slist_t *child = package_decl->data.node.children; child; child = child->next) {
        ast_node_t *annot = (ast_node_t *)child->data;
        if (annot && annot->type == AST_ANNOTATION) {
            if (!all_annotations) all_annotations = slist_new(annot);
            else slist_append(all_annotations, annot);
        }
    }
    
    /* Also include any standalone annotations passed in */
    for (slist_t *node = annotations; node; node = node->next) {
        ast_node_t *annot = (ast_node_t *)node->data;
        if (annot && annot->type == AST_ANNOTATION) {
            if (!all_annotations) all_annotations = slist_new(annot);
            else slist_append(all_annotations, annot);
        }
    }
    
    /* Build RuntimeVisibleAnnotations attribute if we have annotations */
    uint8_t *annotations_bytes = NULL;
    size_t annotations_size = 0;
    uint16_t annotations_attr_name = 0;
    
    if (all_annotations) {
        annotations_attr_name = cp_add_utf8(cp, "RuntimeVisibleAnnotations");
        
        /* Count annotations */
        int annot_count = slist_length(all_annotations);
        
        /* Build annotations data:
         * u2 num_annotations
         * annotation[num_annotations]
         *   annotation {
         *     u2 type_index
         *     u2 num_element_value_pairs
         *     element_value_pair[num_element_value_pairs]
         *   }
         */
        
        /* Calculate size and allocate */
        size_t data_size = 2;  /* num_annotations */
        for (slist_t *node = all_annotations; node; node = node->next) {
            ast_node_t *annot = (ast_node_t *)node->data;
            if (annot && annot->type == AST_ANNOTATION) {
                data_size += 2 + 2;  /* type_index + num_element_value_pairs (0 for now) */
            }
        }
        
        annotations_bytes = malloc(data_size);
        uint8_t *p = annotations_bytes;
        
        /* num_annotations */
        *p++ = (annot_count >> 8) & 0xFF;
        *p++ = annot_count & 0xFF;
        
        /* annotation entries */
        for (slist_t *node = all_annotations; node; node = node->next) {
            ast_node_t *annot = (ast_node_t *)node->data;
            if (annot && annot->type == AST_ANNOTATION) {
                /* Get annotation type name */
                const char *annot_name = annot->data.node.name;
                if (!annot_name) continue;
                
                /* Resolve to fully qualified name if needed */
                const char *full_name = annot_name;
                if (sem && strchr(annot_name, '.') == NULL) {
                    /* Try to resolve from imports */
                    for (slist_t *imp = sem->imports; imp; imp = imp->next) {
                        ast_node_t *import = (ast_node_t *)imp->data;
                        if (import && import->data.node.name) {
                            const char *imp_name = import->data.node.name;
                            /* Check if import ends with this annotation name */
                            size_t imp_len = strlen(imp_name);
                            size_t ann_len = strlen(annot_name);
                            if (imp_len > ann_len && 
                                strcmp(imp_name + imp_len - ann_len, annot_name) == 0 &&
                                imp_name[imp_len - ann_len - 1] == '.') {
                                full_name = imp_name;
                                break;
                            }
                        }
                    }
                    /* Default to java.lang if common */
                    if (strcmp(full_name, annot_name) == 0) {
                        if (strcmp(annot_name, "Deprecated") == 0 ||
                            strcmp(annot_name, "Override") == 0 ||
                            strcmp(annot_name, "SuppressWarnings") == 0 ||
                            strcmp(annot_name, "SafeVarargs") == 0 ||
                            strcmp(annot_name, "FunctionalInterface") == 0) {
                            char *buf = malloc(strlen(annot_name) + 12);
                            sprintf(buf, "java.lang.%s", annot_name);
                            full_name = buf;  /* Will leak, but small */
                        }
                    }
                }
                
                /* Convert to descriptor form: java.lang.Deprecated -> Ljava/lang/Deprecated; */
                size_t name_len = strlen(full_name);
                char *desc = malloc(name_len + 3);
                desc[0] = 'L';
                strcpy(desc + 1, full_name);
                for (char *c = desc + 1; *c; c++) {
                    if (*c == '.') *c = '/';
                }
                strcat(desc, ";");
                
                uint16_t type_idx = cp_add_utf8(cp, desc);
                free(desc);
                
                *p++ = (type_idx >> 8) & 0xFF;
                *p++ = type_idx & 0xFF;
                
                /* num_element_value_pairs = 0 (simplified - no annotation values) */
                *p++ = 0x00;
                *p++ = 0x00;
            }
        }
        
        annotations_size = p - annotations_bytes;
    }
    
    slist_free(all_annotations);
    
    /* Calculate total size */
    size_t total_size = 4;      /* magic */
    total_size += 2 + 2;        /* version */
    total_size += 2;            /* constant pool count */
    
    /* Constant pool */
    for (uint16_t i = 1; i < cp->count; i++) {
        const_pool_entry_t *e = &cp->entries[i];
        total_size += 1;  /* tag */
        switch (e->type) {
            case CONST_UTF8:
                total_size += 2 + strlen(e->data.utf8);
                break;
            case CONST_CLASS:
                total_size += 2;
                break;
            default:
                break;
        }
    }
    
    total_size += 2;            /* access flags */
    total_size += 2;            /* this_class */
    total_size += 2;            /* super_class */
    total_size += 2;            /* interfaces_count (0) */
    total_size += 2;            /* fields_count (0) */
    total_size += 2;            /* methods_count (0) */
    total_size += 2;            /* attributes_count */
    
    /* SourceFile attribute: name_idx(2) + length(4) + sourcefile_idx(2) */
    total_size += 2 + 4 + 2;
    
    /* RuntimeVisibleAnnotations attribute if present */
    if (annotations_bytes) {
        total_size += 2 + 4 + annotations_size;
    }
    
    /* Allocate and write class file */
    uint8_t *bytes = malloc(total_size);
    if (!bytes) {
        const_pool_free(cp);
        free(internal_name);
        free(annotations_bytes);
        return NULL;
    }
    
    uint8_t *p = bytes;
    
    /* Magic number */
    *p++ = 0xCA; *p++ = 0xFE; *p++ = 0xBA; *p++ = 0xBE;
    
    /* Version */
    *p++ = 0x00; *p++ = 0x00;  /* minor */
    int major = target_version > 0 ? target_version : 61;  /* default to Java 17 */
    *p++ = (major >> 8) & 0xFF;
    *p++ = major & 0xFF;
    
    /* Constant pool count */
    *p++ = (cp->count >> 8) & 0xFF;
    *p++ = cp->count & 0xFF;
    
    /* Constant pool entries */
    for (uint16_t i = 1; i < cp->count; i++) {
        const_pool_entry_t *e = &cp->entries[i];
        *p++ = e->type;
        switch (e->type) {
            case CONST_UTF8:
                {
                    uint16_t len = strlen(e->data.utf8);
                    *p++ = (len >> 8) & 0xFF;
                    *p++ = len & 0xFF;
                    memcpy(p, e->data.utf8, len);
                    p += len;
                }
                break;
            case CONST_CLASS:
                *p++ = (e->data.class_index >> 8) & 0xFF;
                *p++ = e->data.class_index & 0xFF;
                break;
            default:
                break;
        }
    }
    
    /* Access flags: ACC_INTERFACE | ACC_ABSTRACT | ACC_SYNTHETIC */
    uint16_t access_flags = ACC_INTERFACE | ACC_ABSTRACT | ACC_SYNTHETIC;
    *p++ = (access_flags >> 8) & 0xFF;
    *p++ = access_flags & 0xFF;
    
    /* this_class */
    *p++ = (this_class >> 8) & 0xFF;
    *p++ = this_class & 0xFF;
    
    /* super_class */
    *p++ = (super_class >> 8) & 0xFF;
    *p++ = super_class & 0xFF;
    
    /* interfaces_count */
    *p++ = 0x00; *p++ = 0x00;
    
    /* fields_count */
    *p++ = 0x00; *p++ = 0x00;
    
    /* methods_count */
    *p++ = 0x00; *p++ = 0x00;
    
    /* attributes_count */
    int attr_count = 1;  /* SourceFile */
    if (annotations_bytes) attr_count++;
    *p++ = (attr_count >> 8) & 0xFF;
    *p++ = attr_count & 0xFF;
    
    /* SourceFile attribute */
    *p++ = (source_file_attr >> 8) & 0xFF;
    *p++ = source_file_attr & 0xFF;
    *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x02;  /* length = 2 */
    *p++ = (source_file_name >> 8) & 0xFF;
    *p++ = source_file_name & 0xFF;
    
    /* RuntimeVisibleAnnotations attribute if present */
    if (annotations_bytes) {
        *p++ = (annotations_attr_name >> 8) & 0xFF;
        *p++ = annotations_attr_name & 0xFF;
        *p++ = (annotations_size >> 24) & 0xFF;
        *p++ = (annotations_size >> 16) & 0xFF;
        *p++ = (annotations_size >> 8) & 0xFF;
        *p++ = annotations_size & 0xFF;
        memcpy(p, annotations_bytes, annotations_size);
        p += annotations_size;
    }
    
    /* Clean up */
    const_pool_free(cp);
    free(internal_name);
    free(annotations_bytes);
    
    *size_out = p - bytes;
    return bytes;
}

