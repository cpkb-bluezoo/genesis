/*
 * stackmap.h
 * StackMapTable attribute generation for JVM verification
 * Copyright (C) 2020 Chris Burdess <dog@gnu.org>
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
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#ifndef STACKMAP_H
#define STACKMAP_H

#include <stdint.h>
#include <stdbool.h>
#include "util.h"
#include "constpool.h"

/* Forward declarations */
struct method_gen;

/* ========================================================================
 * Verification Types (JVM Spec 4.7.4)
 * ======================================================================== */

typedef enum verification_type_tag
{
    VT_TOP              = 0,    /* Uninitialized or second half of long/double */
    VT_INTEGER          = 1,    /* int, boolean, byte, char, short */
    VT_FLOAT            = 2,    /* float */
    VT_DOUBLE           = 3,    /* double (takes 2 slots, followed by TOP) */
    VT_LONG             = 4,    /* long (takes 2 slots, followed by TOP) */
    VT_NULL             = 5,    /* null reference */
    VT_UNINITIALIZED_THIS = 6,  /* 'this' before super() call */
    VT_OBJECT           = 7,    /* Object reference (has cp_index) */
    VT_UNINITIALIZED    = 8     /* New object before constructor (has offset) */
} verification_type_tag_t;

typedef struct verification_type
{
    verification_type_tag_t tag;
    union {
        uint16_t cp_index;      /* For VT_OBJECT: constant pool class index */
        uint16_t offset;        /* For VT_UNINITIALIZED: bytecode offset of 'new' */
    } data;
} verification_type_t;

/* ========================================================================
 * Stack Map Frame Types (JVM Spec 4.7.4)
 * ======================================================================== */

typedef enum stack_frame_type
{
    FRAME_SAME                  = 0,    /* Same locals as previous, empty stack (0-63) */
    FRAME_SAME_LOCALS_1_STACK   = 64,   /* Same locals, 1 stack item (64-127) */
    FRAME_SAME_LOCALS_1_EXTENDED = 247, /* Same locals, 1 stack item, u2 offset */
    FRAME_CHOP                  = 248,  /* Remove 1-3 locals (248-250) */
    FRAME_SAME_EXTENDED         = 251,  /* Same as 0-63 but u2 offset */
    FRAME_APPEND                = 252,  /* Append 1-3 locals (252-254) */
    FRAME_FULL                  = 255   /* Full frame specification */
} stack_frame_type_t;

/* ========================================================================
 * Stack Map Frame
 * ======================================================================== */

typedef struct stack_map_frame
{
    uint16_t offset;                    /* Bytecode offset */
    uint16_t offset_delta;              /* Delta from previous frame */
    
    /* Local variables at this point */
    uint16_t num_locals;
    verification_type_t *locals;        /* Array of local types */
    
    /* Operand stack at this point */
    uint16_t stack_size;
    verification_type_t *stack;         /* Array of stack types */
    
    struct stack_map_frame *next;
} stack_map_frame_t;

/* ========================================================================
 * Stack Map Table
 * ======================================================================== */

typedef struct stack_map_table
{
    uint16_t num_entries;
    stack_map_frame_t *frames;          /* Linked list of frames */
    stack_map_frame_t *last_frame;      /* Last frame added */
    
    /* Current state tracking during code generation */
    uint16_t current_locals_count;
    verification_type_t *current_locals;    /* Current local types */
    uint16_t current_locals_capacity;
    
    uint16_t current_stack_size;
    verification_type_t *current_stack;     /* Current stack types */
    uint16_t current_stack_capacity;
} stack_map_table_t;

/* ========================================================================
 * Stack Map Table API
 * ======================================================================== */

/* Create and destroy */
stack_map_table_t *stackmap_new(void);
void stackmap_free(stack_map_table_t *smt);

/* Initialize with method parameters and 'this' */
void stackmap_init_method(stack_map_table_t *smt, struct method_gen *mg, 
                          bool is_static, const char *this_class);

/* Track local variable types */
void stackmap_set_local(stack_map_table_t *smt, uint16_t slot, verification_type_t type);
void stackmap_set_local_object(stack_map_table_t *smt, uint16_t slot, 
                               const_pool_t *cp, const char *class_name);
void stackmap_set_local_int(stack_map_table_t *smt, uint16_t slot);
void stackmap_set_local_long(stack_map_table_t *smt, uint16_t slot);
void stackmap_set_local_float(stack_map_table_t *smt, uint16_t slot);
void stackmap_set_local_double(stack_map_table_t *smt, uint16_t slot);

/* Track stack types */
void stackmap_push(stack_map_table_t *smt, verification_type_t type);
void stackmap_push_int(stack_map_table_t *smt);
void stackmap_push_long(stack_map_table_t *smt);
void stackmap_push_float(stack_map_table_t *smt);
void stackmap_push_double(stack_map_table_t *smt);
void stackmap_push_null(stack_map_table_t *smt);
void stackmap_push_object(stack_map_table_t *smt, const_pool_t *cp, const char *class_name);
void stackmap_push_uninitialized(stack_map_table_t *smt, uint16_t new_offset);
void stackmap_pop(stack_map_table_t *smt, uint16_t count);
void stackmap_clear_stack(stack_map_table_t *smt);

/* Get/set locals count for save/restore around scoped regions (e.g., catch blocks) */
uint16_t stackmap_get_locals_count(stack_map_table_t *smt);
void stackmap_set_locals_count(stack_map_table_t *smt, uint16_t count);

/* Replace uninitialized with actual type (after constructor call) */
void stackmap_init_object(stack_map_table_t *smt, uint16_t new_offset, 
                          const_pool_t *cp, const char *class_name);

/* Record a frame at a branch target */
void stackmap_record_frame(stack_map_table_t *smt, uint16_t offset);

/* Get frame at specific offset (for merging at join points) */
stack_map_frame_t *stackmap_get_frame(stack_map_table_t *smt, uint16_t offset);

/* Serialize to bytes for class file */
uint8_t *stackmap_serialize(stack_map_table_t *smt, const_pool_t *cp, 
                            uint32_t *out_length);

/* ========================================================================
 * State Save/Restore (javac-style approach)
 * 
 * Used to save stackmap state at branch points and restore/merge at targets.
 * This enables correct frame recording at join points in control flow.
 * ======================================================================== */

/* Saved stackmap state */
typedef struct stackmap_state {
    uint16_t num_locals;
    verification_type_t *locals;
    uint16_t stack_size;
    verification_type_t *stack;
} stackmap_state_t;

/* Save current state (caller must free with stackmap_state_free) */
stackmap_state_t *stackmap_save_state(stack_map_table_t *smt);

/* Restore state from saved (replaces current state) */
void stackmap_restore_state(stack_map_table_t *smt, stackmap_state_t *state);

/* Restore only locals from saved state (keeps current stack) */
void stackmap_restore_locals_only(stack_map_table_t *smt, stackmap_state_t *state);

/* Free a saved state */
void stackmap_state_free(stackmap_state_t *state);

/* Get current stack depth */
uint16_t stackmap_get_stack_size(stack_map_table_t *smt);

/* Helper: create verification type for a class */
verification_type_t vtype_object(const_pool_t *cp, const char *class_name);
verification_type_t vtype_int(void);
verification_type_t vtype_long(void);
verification_type_t vtype_float(void);
verification_type_t vtype_double(void);
verification_type_t vtype_null(void);
verification_type_t vtype_top(void);

#endif /* STACKMAP_H */

