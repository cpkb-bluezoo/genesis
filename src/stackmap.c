/*
 * stackmap.c
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

#include "stackmap.h"
#include "constpool.h"
#include "codegen.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ========================================================================
 * Helper: Verification Type Constructors
 * ======================================================================== */

verification_type_t vtype_top(void)
{
    verification_type_t vt = { VT_TOP, { 0 } };
    return vt;
}

verification_type_t vtype_int(void)
{
    verification_type_t vt = { VT_INTEGER, { 0 } };
    return vt;
}

verification_type_t vtype_long(void)
{
    verification_type_t vt = { VT_LONG, { 0 } };
    return vt;
}

verification_type_t vtype_float(void)
{
    verification_type_t vt = { VT_FLOAT, { 0 } };
    return vt;
}

verification_type_t vtype_double(void)
{
    verification_type_t vt = { VT_DOUBLE, { 0 } };
    return vt;
}

verification_type_t vtype_null(void)
{
    verification_type_t vt = { VT_NULL, { 0 } };
    return vt;
}

verification_type_t vtype_object(const_pool_t *cp, const char *class_name)
{
    verification_type_t vt;
    vt.tag = VT_OBJECT;
    vt.data.cp_index = cp_add_class(cp, class_name);
    return vt;
}

verification_type_t vtype_uninitialized(uint16_t offset)
{
    verification_type_t vt;
    vt.tag = VT_UNINITIALIZED;
    vt.data.offset = offset;
    return vt;
}

verification_type_t vtype_uninitialized_this(void)
{
    verification_type_t vt = { VT_UNINITIALIZED_THIS, { 0 } };
    return vt;
}

/* ========================================================================
 * Stack Map Table Creation/Destruction
 * ======================================================================== */

stack_map_table_t *stackmap_new(void)
{
    stack_map_table_t *smt = calloc(1, sizeof(stack_map_table_t));
    if (!smt) return NULL;
    
    /* Initialize with small default capacities */
    smt->current_locals_capacity = 16;
    smt->current_locals = calloc(smt->current_locals_capacity, sizeof(verification_type_t));
    
    smt->current_stack_capacity = 16;
    smt->current_stack = calloc(smt->current_stack_capacity, sizeof(verification_type_t));
    
    return smt;
}

void stackmap_free(stack_map_table_t *smt)
{
    if (!smt) return;
    
    /* Free all frames */
    stack_map_frame_t *frame = smt->frames;
    while (frame) {
        stack_map_frame_t *next = frame->next;
        free(frame->locals);
        free(frame->stack);
        free(frame);
        frame = next;
    }
    
    free(smt->current_locals);
    free(smt->current_stack);
    free(smt);
}

/* ========================================================================
 * Method Initialization
 * ======================================================================== */

void stackmap_init_method(stack_map_table_t *smt, method_gen_t *mg, 
                          bool is_static, const char *this_class)
{
    if (!smt) return;
    
    /* Reset state */
    smt->current_locals_count = 0;
    smt->current_stack_size = 0;
    
    /* For instance methods, slot 0 is 'this' */
    if (!is_static && this_class) {
        /* In a constructor before super(), 'this' is uninitialized */
        if (mg && mg->is_constructor) {
            stackmap_set_local(smt, 0, vtype_uninitialized_this());
        } else {
            stackmap_set_local_object(smt, 0, mg ? mg->cp : NULL, this_class);
        }
    }
}

/* ========================================================================
 * Local Variable Type Tracking
 * ======================================================================== */

static void ensure_locals_capacity(stack_map_table_t *smt, uint16_t slot)
{
    if (slot >= smt->current_locals_capacity) {
        uint16_t new_cap = smt->current_locals_capacity * 2;
        while (new_cap <= slot) new_cap *= 2;
        smt->current_locals = realloc(smt->current_locals, 
                                      new_cap * sizeof(verification_type_t));
        /* Initialize new slots to TOP */
        for (uint16_t i = smt->current_locals_capacity; i < new_cap; i++) {
            smt->current_locals[i] = vtype_top();
        }
        smt->current_locals_capacity = new_cap;
    }
}

void stackmap_set_local(stack_map_table_t *smt, uint16_t slot, verification_type_t type)
{
    if (!smt) return;
    
    ensure_locals_capacity(smt, slot);
    
    /* If we're setting a slot beyond current_locals_count, clear intermediate slots
     * to TOP. This handles the case where locals were "shrunk" after exiting a scope
     * (e.g., a for loop) and then a new local is allocated at a higher slot.
     * Without this, stale values from the previous scope would be included in frames. */
    if (slot > smt->current_locals_count) {
        for (uint16_t i = smt->current_locals_count; i < slot; i++) {
            smt->current_locals[i] = vtype_top();
        }
    }
    
    smt->current_locals[slot] = type;
    
    /* Update locals count if needed */
    if (slot >= smt->current_locals_count) {
        smt->current_locals_count = slot + 1;
    }
    
    /* Long and double occupy two slots */
    if (type.tag == VT_LONG || type.tag == VT_DOUBLE) {
        ensure_locals_capacity(smt, slot + 1);
        smt->current_locals[slot + 1] = vtype_top();
        if (slot + 1 >= smt->current_locals_count) {
            smt->current_locals_count = slot + 2;
        }
    }
}

void stackmap_set_local_object(stack_map_table_t *smt, uint16_t slot, 
                               const_pool_t *cp, const char *class_name)
{
    if (!smt || !cp || !class_name) return;
    stackmap_set_local(smt, slot, vtype_object(cp, class_name));
}

void stackmap_set_local_int(stack_map_table_t *smt, uint16_t slot)
{
    if (!smt) return;
    stackmap_set_local(smt, slot, vtype_int());
}

void stackmap_set_local_long(stack_map_table_t *smt, uint16_t slot)
{
    if (!smt) return;
    stackmap_set_local(smt, slot, vtype_long());
}

void stackmap_set_local_float(stack_map_table_t *smt, uint16_t slot)
{
    if (!smt) return;
    stackmap_set_local(smt, slot, vtype_float());
}

void stackmap_set_local_double(stack_map_table_t *smt, uint16_t slot)
{
    if (!smt) return;
    stackmap_set_local(smt, slot, vtype_double());
}

/* ========================================================================
 * Stack Type Tracking
 * ======================================================================== */

static void ensure_stack_capacity(stack_map_table_t *smt, uint16_t size)
{
    if (size >= smt->current_stack_capacity) {
        uint16_t new_cap = smt->current_stack_capacity * 2;
        while (new_cap <= size) new_cap *= 2;
        smt->current_stack = realloc(smt->current_stack, 
                                     new_cap * sizeof(verification_type_t));
        smt->current_stack_capacity = new_cap;
    }
}

void stackmap_push(stack_map_table_t *smt, verification_type_t type)
{
    if (!smt) return;
    
    ensure_stack_capacity(smt, smt->current_stack_size);
    smt->current_stack[smt->current_stack_size++] = type;
    
    /* Long and double occupy two slots */
    if (type.tag == VT_LONG || type.tag == VT_DOUBLE) {
        ensure_stack_capacity(smt, smt->current_stack_size);
        smt->current_stack[smt->current_stack_size++] = vtype_top();
    }
}

void stackmap_push_int(stack_map_table_t *smt)
{
    stackmap_push(smt, vtype_int());
}

void stackmap_push_long(stack_map_table_t *smt)
{
    stackmap_push(smt, vtype_long());
}

void stackmap_push_float(stack_map_table_t *smt)
{
    stackmap_push(smt, vtype_float());
}

void stackmap_push_double(stack_map_table_t *smt)
{
    stackmap_push(smt, vtype_double());
}

void stackmap_push_null(stack_map_table_t *smt)
{
    stackmap_push(smt, vtype_null());
}

void stackmap_push_object(stack_map_table_t *smt, const_pool_t *cp, const char *class_name)
{
    if (!smt || !cp || !class_name) return;
    stackmap_push(smt, vtype_object(cp, class_name));
}

void stackmap_push_uninitialized(stack_map_table_t *smt, uint16_t new_offset)
{
    if (!smt) return;
    stackmap_push(smt, vtype_uninitialized(new_offset));
}

void stackmap_pop(stack_map_table_t *smt, uint16_t count)
{
    if (!smt) return;
    if (count > smt->current_stack_size) {
        smt->current_stack_size = 0;
    } else {
        smt->current_stack_size -= count;
    }
}

void stackmap_clear_stack(stack_map_table_t *smt)
{
    if (!smt) return;
    smt->current_stack_size = 0;
}

uint16_t stackmap_get_locals_count(stack_map_table_t *smt)
{
    return smt ? smt->current_locals_count : 0;
}

void stackmap_set_locals_count(stack_map_table_t *smt, uint16_t count)
{
    if (!smt) return;
    if (count < smt->current_locals_count) {
        smt->current_locals_count = count;
    }
}

/* ========================================================================
 * Object Initialization Tracking
 * ======================================================================== */

void stackmap_init_object(stack_map_table_t *smt, uint16_t new_offset, 
                          const_pool_t *cp, const char *class_name)
{
    if (!smt || !cp || !class_name) return;
    
    verification_type_t initialized = vtype_object(cp, class_name);
    
    /* Replace any UNINITIALIZED with the same offset in locals and stack */
    for (uint16_t i = 0; i < smt->current_locals_count; i++) {
        if (smt->current_locals[i].tag == VT_UNINITIALIZED &&
            smt->current_locals[i].data.offset == new_offset) {
            smt->current_locals[i] = initialized;
        }
        /* Also handle UninitializedThis for constructors */
        if (smt->current_locals[i].tag == VT_UNINITIALIZED_THIS) {
            smt->current_locals[i] = initialized;
        }
    }
    
    for (uint16_t i = 0; i < smt->current_stack_size; i++) {
        if (smt->current_stack[i].tag == VT_UNINITIALIZED &&
            smt->current_stack[i].data.offset == new_offset) {
            smt->current_stack[i] = initialized;
        }
    }
}

/* ========================================================================
 * Frame Recording
 * ======================================================================== */

void stackmap_record_frame(stack_map_table_t *smt, uint16_t offset)
{
    if (!smt) return;
    
    /* Check for existing frame at this offset */
    for (stack_map_frame_t *f = smt->frames; f; f = f->next) {
        if (f->offset == offset) {
            /* Join point: update to use minimum locals count.
             * This handles cases where multiple paths with different 
             * numbers of locals converge at the same point.
             * 
             * We shrink if current_locals_count < existing locals.
             * Note: current_locals_count == 0 is valid for static methods
             * with no parameters, so we don't require > 0. */
            if (smt->current_locals_count < f->num_locals) {
                /* Shrink to minimum - locals declared in one path
                   but not another should not be in the frame */
                f->num_locals = smt->current_locals_count;
                /* Reallocate locals array (or set to NULL if 0 locals) */
                if (f->num_locals > 0) {
                    verification_type_t *new_locals = malloc(f->num_locals * sizeof(verification_type_t));
                    if (new_locals) {
                        memcpy(new_locals, smt->current_locals,
                               f->num_locals * sizeof(verification_type_t));
                        free(f->locals);
                        f->locals = new_locals;
                    }
                } else {
                    /* 0 locals - free the array */
                    free(f->locals);
                    f->locals = NULL;
                }
            }
            return;
        }
    }
    
    /* Create new frame */
    stack_map_frame_t *frame = calloc(1, sizeof(stack_map_frame_t));
    if (!frame) return;
    
    frame->offset = offset;
    
    /* Copy current locals */
    frame->num_locals = smt->current_locals_count;
    if (frame->num_locals > 0) {
        frame->locals = malloc(frame->num_locals * sizeof(verification_type_t));
        memcpy(frame->locals, smt->current_locals, 
               frame->num_locals * sizeof(verification_type_t));
    }
    
    /* Copy current stack */
    frame->stack_size = smt->current_stack_size;
    if (frame->stack_size > 0) {
        frame->stack = malloc(frame->stack_size * sizeof(verification_type_t));
        memcpy(frame->stack, smt->current_stack, 
               frame->stack_size * sizeof(verification_type_t));
    }
    
    /* Insert in sorted order by offset */
    if (!smt->frames || smt->frames->offset > offset) {
        frame->next = smt->frames;
        smt->frames = frame;
        if (!smt->last_frame) {
            smt->last_frame = frame;
        }
    } else {
        stack_map_frame_t *prev = smt->frames;
        while (prev->next && prev->next->offset < offset) {
            prev = prev->next;
        }
        frame->next = prev->next;
        prev->next = frame;
        if (!frame->next) {
            smt->last_frame = frame;
        }
    }
    
    smt->num_entries++;
}

stack_map_frame_t *stackmap_get_frame(stack_map_table_t *smt, uint16_t offset)
{
    if (!smt) return NULL;
    
    for (stack_map_frame_t *f = smt->frames; f; f = f->next) {
        if (f->offset == offset) {
            return f;
        }
    }
    return NULL;
}

/* ========================================================================
 * State Save/Restore (javac-style approach)
 * ======================================================================== */

stackmap_state_t *stackmap_save_state(stack_map_table_t *smt)
{
    if (!smt) return NULL;
    
    stackmap_state_t *state = calloc(1, sizeof(stackmap_state_t));
    if (!state) return NULL;
    
    /* Save locals */
    state->num_locals = smt->current_locals_count;
    if (state->num_locals > 0) {
        state->locals = malloc(state->num_locals * sizeof(verification_type_t));
        if (state->locals) {
            memcpy(state->locals, smt->current_locals, 
                   state->num_locals * sizeof(verification_type_t));
        }
    }
    
    /* Save stack */
    state->stack_size = smt->current_stack_size;
    if (state->stack_size > 0) {
        state->stack = malloc(state->stack_size * sizeof(verification_type_t));
        if (state->stack) {
            memcpy(state->stack, smt->current_stack, 
                   state->stack_size * sizeof(verification_type_t));
        }
    }
    
    return state;
}

void stackmap_restore_state(stack_map_table_t *smt, stackmap_state_t *state)
{
    if (!smt || !state) return;
    
    /* Restore locals */
    smt->current_locals_count = state->num_locals;
    if (state->num_locals > 0 && state->locals) {
        ensure_locals_capacity(smt, state->num_locals);
        memcpy(smt->current_locals, state->locals, 
               state->num_locals * sizeof(verification_type_t));
    }
    
    /* Restore stack */
    smt->current_stack_size = state->stack_size;
    if (state->stack_size > 0 && state->stack) {
        ensure_stack_capacity(smt, state->stack_size);
        memcpy(smt->current_stack, state->stack, 
               state->stack_size * sizeof(verification_type_t));
    }
}

void stackmap_restore_locals_only(stack_map_table_t *smt, stackmap_state_t *state)
{
    if (!smt || !state) return;
    
    /* Restore only locals - keep current stack */
    smt->current_locals_count = state->num_locals;
    if (state->num_locals > 0 && state->locals) {
        ensure_locals_capacity(smt, state->num_locals);
        memcpy(smt->current_locals, state->locals, 
               state->num_locals * sizeof(verification_type_t));
    }
}

void stackmap_state_free(stackmap_state_t *state)
{
    if (!state) return;
    free(state->locals);
    free(state->stack);
    free(state);
}

uint16_t stackmap_get_stack_size(stack_map_table_t *smt)
{
    return smt ? smt->current_stack_size : 0;
}

/* ========================================================================
 * Serialization
 * ======================================================================== */

/* Helper to write a verification type */
static void write_verification_type(uint8_t **p, verification_type_t *vt)
{
    *(*p)++ = (uint8_t)vt->tag;
    if (vt->tag == VT_OBJECT) {
        *(*p)++ = (vt->data.cp_index >> 8) & 0xFF;
        *(*p)++ = vt->data.cp_index & 0xFF;
    } else if (vt->tag == VT_UNINITIALIZED) {
        *(*p)++ = (vt->data.offset >> 8) & 0xFF;
        *(*p)++ = vt->data.offset & 0xFF;
    }
}

/* Calculate size of a verification type */
static int vtype_size(verification_type_t *vt)
{
    if (vt->tag == VT_OBJECT || vt->tag == VT_UNINITIALIZED) {
        return 3;  /* tag + u2 */
    }
    return 1;  /* just tag */
}

/* Count actual verification types in a range, excluding implicit TOPs after long/double.
 * In JVM StackMapTable, long/double take 1 verification_type_info entry (not 2).
 * The TOP that follows is implicit and should not be counted or written. */
static int count_actual_vtypes(verification_type_t *locals, uint16_t start, uint16_t end)
{
    int count = 0;
    for (uint16_t i = start; i < end; i++) {
        /* Skip TOP entries that follow LONG or DOUBLE (they're implicit) */
        if (locals[i].tag == VT_TOP && i > 0 &&
            (locals[i-1].tag == VT_LONG || locals[i-1].tag == VT_DOUBLE)) {
            continue;  /* Implicit TOP, don't count */
        }
        count++;
    }
    return count;
}

/* Calculate byte size for a range of verification types, excluding implicit TOPs */
static int calc_vtypes_size(verification_type_t *locals, uint16_t start, uint16_t end)
{
    int size = 0;
    for (uint16_t i = start; i < end; i++) {
        /* Skip TOP entries that follow LONG or DOUBLE */
        if (locals[i].tag == VT_TOP && i > 0 &&
            (locals[i-1].tag == VT_LONG || locals[i-1].tag == VT_DOUBLE)) {
            continue;
        }
        size += vtype_size(&locals[i]);
    }
    return size;
}

/* Compare two frames to determine optimal encoding */
static bool frames_locals_equal(stack_map_frame_t *prev, stack_map_frame_t *curr)
{
    if (prev->num_locals != curr->num_locals) return false;
    for (uint16_t i = 0; i < prev->num_locals; i++) {
        if (prev->locals[i].tag != curr->locals[i].tag) return false;
        if (prev->locals[i].tag == VT_OBJECT &&
            prev->locals[i].data.cp_index != curr->locals[i].data.cp_index) {
            return false;
        }
        if (prev->locals[i].tag == VT_UNINITIALIZED &&
            prev->locals[i].data.offset != curr->locals[i].data.offset) {
            return false;
        }
    }
    return true;
}

/* Determine frame type and calculate size */
static int calculate_frame_size(stack_map_frame_t *prev, stack_map_frame_t *curr,
                                 uint8_t *frame_type)
{
    uint16_t offset_delta = (prev == NULL) ? curr->offset : (curr->offset - prev->offset - 1);
    
    bool same_locals = (prev == NULL && curr->num_locals == 0) ||
                       (prev != NULL && frames_locals_equal(prev, curr));
    
    if (same_locals && curr->stack_size == 0) {
        /* same_frame or same_frame_extended */
        if (offset_delta < 64) {
            *frame_type = (uint8_t)offset_delta;
            return 1;
        } else {
            *frame_type = 251;  /* same_frame_extended */
            return 3;  /* type + u2 offset_delta */
        }
    }
    
    if (same_locals && curr->stack_size == 1) {
        /* same_locals_1_stack_item or extended */
        int stack_item_size = vtype_size(&curr->stack[0]);
        if (offset_delta < 64) {
            *frame_type = (uint8_t)(64 + offset_delta);
            return 1 + stack_item_size;
        } else {
            *frame_type = 247;  /* same_locals_1_stack_item_extended */
            return 3 + stack_item_size;
        }
    }
    
    /* Check for append or chop */
    if (prev && curr->stack_size == 0) {
        int slot_diff = (int)curr->num_locals - (int)prev->num_locals;
        
        /* For append_frame, count actual verification types (not slots).
         * Long/double take 2 slots but only 1 verification_type_info entry. */
        int append_count = count_actual_vtypes(curr->locals, prev->num_locals, curr->num_locals);
        
        if (slot_diff > 0 && append_count > 0 && append_count <= 3) {
            /* Check if first prev->num_locals are the same */
            bool prefix_same = true;
            for (uint16_t i = 0; i < prev->num_locals && prefix_same; i++) {
                if (prev->locals[i].tag != curr->locals[i].tag) prefix_same = false;
                if (prev->locals[i].tag == VT_OBJECT &&
                    prev->locals[i].data.cp_index != curr->locals[i].data.cp_index) {
                    prefix_same = false;
                }
            }
            if (prefix_same) {
                /* append_frame: use actual vtype count, not slot count */
                *frame_type = (uint8_t)(251 + append_count);
                int size = 3;  /* type + u2 offset_delta */
                size += calc_vtypes_size(curr->locals, prev->num_locals, curr->num_locals);
                return size;
            }
        } else if (slot_diff < 0 && slot_diff >= -3) {
            /* For chop_frame, count how many actual vtypes are being removed */
            int chop_count = count_actual_vtypes(prev->locals, curr->num_locals, prev->num_locals);
            
            if (chop_count > 0 && chop_count <= 3) {
            /* Check if first curr->num_locals are the same */
            bool prefix_same = true;
            for (uint16_t i = 0; i < curr->num_locals && prefix_same; i++) {
                if (prev->locals[i].tag != curr->locals[i].tag) prefix_same = false;
                if (prev->locals[i].tag == VT_OBJECT &&
                    prev->locals[i].data.cp_index != curr->locals[i].data.cp_index) {
                    prefix_same = false;
                }
            }
            if (prefix_same) {
                /* chop_frame */
                    *frame_type = (uint8_t)(251 - chop_count);  /* 248, 249, or 250 */
                return 3;  /* type + u2 offset_delta */
                }
            }
        }
    }
    
    /* full_frame - use actual vtype counts (excluding implicit TOPs) */
    *frame_type = 255;
    int size = 7;  /* type + u2 offset_delta + u2 num_locals + u2 stack_size */
    size += calc_vtypes_size(curr->locals, 0, curr->num_locals);
    size += calc_vtypes_size(curr->stack, 0, curr->stack_size);
    return size;
}

uint8_t *stackmap_serialize(stack_map_table_t *smt, const_pool_t *cp, 
                            uint32_t *out_length)
{
    (void)cp;  /* May be used later for adding entries */
    
    if (!smt || smt->num_entries == 0) {
        *out_length = 0;
        return NULL;
    }
    
    /* First pass: calculate total size */
    int total_size = 2;  /* u2 number_of_entries */
    stack_map_frame_t *prev = NULL;
    for (stack_map_frame_t *f = smt->frames; f; f = f->next) {
        uint8_t frame_type;
        total_size += calculate_frame_size(prev, f, &frame_type);
        prev = f;
    }
    
    /* Allocate buffer */
    uint8_t *data = malloc(total_size);
    if (!data) {
        *out_length = 0;
        return NULL;
    }
    
    uint8_t *p = data;
    
    /* Write number_of_entries */
    *p++ = (smt->num_entries >> 8) & 0xFF;
    *p++ = smt->num_entries & 0xFF;
    
    /* Write each frame */
    prev = NULL;
    for (stack_map_frame_t *f = smt->frames; f; f = f->next) {
        uint8_t frame_type;
        calculate_frame_size(prev, f, &frame_type);
        
        uint16_t offset_delta = (prev == NULL) ? f->offset : (f->offset - prev->offset - 1);
        
        *p++ = frame_type;
        
        if (frame_type <= 63) {
            /* same_frame - no additional data */
        } else if (frame_type >= 64 && frame_type <= 127) {
            /* same_locals_1_stack_item_frame */
            write_verification_type(&p, &f->stack[0]);
        } else if (frame_type == 247) {
            /* same_locals_1_stack_item_frame_extended */
            *p++ = (offset_delta >> 8) & 0xFF;
            *p++ = offset_delta & 0xFF;
            write_verification_type(&p, &f->stack[0]);
        } else if (frame_type >= 248 && frame_type <= 250) {
            /* chop_frame */
            *p++ = (offset_delta >> 8) & 0xFF;
            *p++ = offset_delta & 0xFF;
        } else if (frame_type == 251) {
            /* same_frame_extended */
            *p++ = (offset_delta >> 8) & 0xFF;
            *p++ = offset_delta & 0xFF;
        } else if (frame_type >= 252 && frame_type <= 254) {
            /* append_frame - write only actual vtypes, skip implicit TOPs */
            *p++ = (offset_delta >> 8) & 0xFF;
            *p++ = offset_delta & 0xFF;
            uint16_t start = prev ? prev->num_locals : 0;
            for (uint16_t i = start; i < f->num_locals; i++) {
                /* Skip TOP entries that follow LONG or DOUBLE */
                if (f->locals[i].tag == VT_TOP && i > 0 &&
                    (f->locals[i-1].tag == VT_LONG || f->locals[i-1].tag == VT_DOUBLE)) {
                    continue;
                }
                write_verification_type(&p, &f->locals[i]);
            }
        } else {  /* frame_type == 255 */
            /* full_frame - count actual vtypes for num_locals */
            *p++ = (offset_delta >> 8) & 0xFF;
            *p++ = offset_delta & 0xFF;
            
            /* Count actual verification types (excluding implicit TOPs) */
            uint16_t actual_locals = (uint16_t)count_actual_vtypes(f->locals, 0, f->num_locals);
            *p++ = (actual_locals >> 8) & 0xFF;
            *p++ = actual_locals & 0xFF;
            for (uint16_t i = 0; i < f->num_locals; i++) {
                /* Skip TOP entries that follow LONG or DOUBLE */
                if (f->locals[i].tag == VT_TOP && i > 0 &&
                    (f->locals[i-1].tag == VT_LONG || f->locals[i-1].tag == VT_DOUBLE)) {
                    continue;
                }
                write_verification_type(&p, &f->locals[i]);
            }
            
            /* Same for stack - count and write actual vtypes */
            uint16_t actual_stack = (uint16_t)count_actual_vtypes(f->stack, 0, f->stack_size);
            *p++ = (actual_stack >> 8) & 0xFF;
            *p++ = actual_stack & 0xFF;
            for (uint16_t i = 0; i < f->stack_size; i++) {
                /* Skip TOP entries that follow LONG or DOUBLE */
                if (f->stack[i].tag == VT_TOP && i > 0 &&
                    (f->stack[i-1].tag == VT_LONG || f->stack[i-1].tag == VT_DOUBLE)) {
                    continue;
                }
                write_verification_type(&p, &f->stack[i]);
            }
        }
        
        prev = f;
    }
    
    *out_length = total_size;
    return data;
}

