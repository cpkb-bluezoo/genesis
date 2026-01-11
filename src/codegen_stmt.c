/*
 * codegen_stmt.c
 * Statement bytecode generation for the JVM
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

/* ========================================================================
 * Helper Functions
 * ======================================================================== */

/**
 * Resolve an exception class name to its fully qualified internal name.
 * Handles common java.lang exception classes that may be used without import.
 */
static const char *resolve_exception_class(const char *name)
{
    if (!name) {
        return "java/lang/Throwable";
    }
    
    /* If already contains a package separator, use as-is */
    if (strchr(name, '.') || strchr(name, '/')) {
        return name;
    }
    
    /* Check for common java.lang exceptions */
    static const char *java_lang_exceptions[] = {
        "ArithmeticException",
        "ArrayIndexOutOfBoundsException",
        "ArrayStoreException",
        "ClassCastException",
        "ClassNotFoundException",
        "CloneNotSupportedException",
        "EnumConstantNotPresentException",
        "Exception",
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
        "ReflectiveOperationException",
        "RuntimeException",
        "SecurityException",
        "StringIndexOutOfBoundsException",
        "Throwable",
        "TypeNotPresentException",
        "UnsupportedOperationException",
        /* Errors */
        "Error",
        "AssertionError",
        "LinkageError",
        "OutOfMemoryError",
        "StackOverflowError",
        "VirtualMachineError",
        NULL
    };
    
    for (const char **e = java_lang_exceptions; *e; e++) {
        if (strcmp(name, *e) == 0) {
            static char buf[128];
            snprintf(buf, sizeof(buf), "java/lang/%s", name);
            return buf;
        }
    }
    
    /* Not a known java.lang exception - return as-is */
    return name;
}

/* ========================================================================
 * String Switch Support (Java 7)
 * ======================================================================== */

/**
 * Hash code for a string constant (must match String.hashCode())
 */
static int32_t string_hashcode(const char *str)
{
    if (!str) return 0;
    int32_t h = 0;
    while (*str) {
        h = 31 * h + (unsigned char)*str;
        str++;
    }
    return h;
}

/**
 * Structure to track string case info
 */
typedef struct string_case_info {
    const char *str;        /* String literal value */
    int32_t hashcode;       /* Precomputed hashCode */
    int ast_idx;            /* Index in AST children list */
    ast_node_t *case_label; /* The AST_CASE_LABEL node */
} string_case_info_t;

/**
 * Compare function for sorting by hashcode
 */
static int compare_by_hashcode(const void *a, const void *b)
{
    const string_case_info_t *ca = (const string_case_info_t *)a;
    const string_case_info_t *cb = (const string_case_info_t *)b;
    if (ca->hashcode < cb->hashcode) return -1;
    if (ca->hashcode > cb->hashcode) return 1;
    return 0;
}

/**
 * Generate code for switch statement on String (Java 7+).
 * 
 * Strategy:
 * 1. Store selector string in temp local
 * 2. Call hashCode() on selector
 * 3. lookupswitch on hashCode values
 * 4. For each hash match, compare with equals() and goto case body
 * 5. Generate case bodies with break handling
 */
static bool codegen_string_switch(method_gen_t *mg, slist_t *children, int num_cases)
{
    if (num_cases == 0) {
        /* Empty switch - just pop selector and return */
        bc_emit(mg->code, OP_POP);
        mg_pop_typed(mg, 1);
        return true;
    }
    
    /* Store selector string in a temporary local */
    uint16_t selector_slot = mg->next_slot++;
    if (mg->next_slot > mg->max_locals) {
        mg->max_locals = mg->next_slot;
    }
    bc_emit(mg->code, OP_ASTORE);
    bc_emit_u1(mg->code, (uint8_t)selector_slot);
    mg_pop_typed(mg, 1);
    
    /* Update stackmap to reflect the stored selector string */
    if (mg->stackmap) {
        stackmap_set_local_object(mg->stackmap, selector_slot, mg->cp, "java/lang/String");
    }
    
    /* Collect case string info */
    string_case_info_t *cases = calloc(num_cases, sizeof(string_case_info_t));
    int case_idx = 0;
    int ast_idx = 0;
    ast_node_t *default_label = NULL;
    
    for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
        ast_node_t *case_label = (ast_node_t *)node->data;
        if (case_label->type != AST_CASE_LABEL) continue;
        
        if (case_label->data.node.name && 
            strcmp(case_label->data.node.name, "default") == 0) {
            default_label = case_label;
            continue;
        }
        
        /* Get string literal from case expression */
        slist_t *case_children = case_label->data.node.children;
        if (case_children) {
            ast_node_t *case_expr = (ast_node_t *)case_children->data;
            if (case_expr->type == AST_LITERAL && 
                case_expr->data.leaf.token_type == TOK_STRING_LITERAL) {
                cases[case_idx].str = case_expr->data.leaf.name;
                cases[case_idx].hashcode = string_hashcode(cases[case_idx].str);
                cases[case_idx].ast_idx = ast_idx;
                cases[case_idx].case_label = case_label;
                case_idx++;
            }
        }
    }
    
    int actual_cases = case_idx;
    
    /* Sort cases by hashcode */
    qsort(cases, actual_cases, sizeof(string_case_info_t), compare_by_hashcode);
    
    /* Count unique hashcodes (for lookupswitch) */
    int unique_hashes = 0;
    for (int i = 0; i < actual_cases; i++) {
        if (i == 0 || cases[i].hashcode != cases[i-1].hashcode) {
            unique_hashes++;
        }
    }
    
    /* Load selector and call hashCode() */
    bc_emit(mg->code, OP_ALOAD);
    bc_emit_u1(mg->code, (uint8_t)selector_slot);
    mg_push(mg, 1);
    
    uint16_t hashcode_ref = cp_add_methodref(mg->cp, "java/lang/String", "hashCode", "()I");
    bc_emit(mg->code, OP_INVOKEVIRTUAL);
    bc_emit_u2(mg->code, hashcode_ref);
    /* Stack: String -> int (net 0) */
    
    /* Emit lookupswitch */
    size_t switch_pos = mg->code->length;
    bc_emit(mg->code, OP_LOOKUPSWITCH);
    mg_pop_typed(mg, 1);
    
    /* Pad to 4-byte alignment */
    while ((mg->code->length) % 4 != 0) {
        bc_emit_u1(mg->code, 0);
    }
    
    /* Default offset placeholder */
    size_t default_offset_pos = mg->code->length;
    bc_emit_u4(mg->code, 0);
    
    /* Number of hash pairs */
    bc_emit_u4(mg->code, (uint32_t)unique_hashes);
    
    /* Emit hash/offset pairs (only unique hashes) */
    size_t *hash_offset_positions = calloc(unique_hashes, sizeof(size_t));
    int32_t *hash_values = calloc(unique_hashes, sizeof(int32_t));
    int hash_idx = 0;
    
    for (int i = 0; i < actual_cases; i++) {
        if (i == 0 || cases[i].hashcode != cases[i-1].hashcode) {
            hash_values[hash_idx] = cases[i].hashcode;
            bc_emit_u4(mg->code, (uint32_t)cases[i].hashcode);
            hash_offset_positions[hash_idx] = mg->code->length;
            bc_emit_u4(mg->code, 0);
            hash_idx++;
        }
    }
    
    /* Track where each case body starts (for direct jumps from equals checks) */
    size_t *case_body_positions = calloc(actual_cases, sizeof(size_t));
    slist_t *goto_patches = NULL;  /* List of goto positions to patch to switch end */
    
    /* Push switch context for break */
    mg_push_loop(mg, 0, NULL);
    
    /* Generate hash comparison blocks */
    hash_idx = 0;
    for (int i = 0; i < actual_cases; ) {
        int32_t current_hash = cases[i].hashcode;
        
        /* Patch hash table to point here */
        size_t hash_block_pos = mg->code->length;
        
        /* Record frame at hash comparison block (switch target) */
        mg_record_frame(mg);
        
        int32_t offset = (int32_t)(hash_block_pos - switch_pos);
        mg->code->code[hash_offset_positions[hash_idx] + 0] = (offset >> 24) & 0xFF;
        mg->code->code[hash_offset_positions[hash_idx] + 1] = (offset >> 16) & 0xFF;
        mg->code->code[hash_offset_positions[hash_idx] + 2] = (offset >> 8) & 0xFF;
        mg->code->code[hash_offset_positions[hash_idx] + 3] = offset & 0xFF;
        hash_idx++;
        
        /* Generate equals() checks for all strings with this hash */
        while (i < actual_cases && cases[i].hashcode == current_hash) {
            /* Load selector */
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)selector_slot);
            mg_push(mg, 1);
            
            /* Load case string constant */
            uint16_t str_idx = cp_add_string(mg->cp, cases[i].str);
            bc_emit(mg->code, OP_LDC_W);
            bc_emit_u2(mg->code, str_idx);
            mg_push(mg, 1);
            
            /* Call equals() */
            uint16_t equals_ref = cp_add_methodref(mg->cp, "java/lang/String", "equals", "(Ljava/lang/Object;)Z");
            bc_emit(mg->code, OP_INVOKEVIRTUAL);
            bc_emit_u2(mg->code, equals_ref);
            mg_pop_typed(mg, 1);  /* Popped 2, pushed 1 */
            
            /* If equals, goto case body (we'll patch this later) */
            size_t ifne_pos = mg->code->length;
            bc_emit(mg->code, OP_IFNE);
            bc_emit_u2(mg->code, 0);  /* Placeholder */
            mg_pop_typed(mg, 1);
            
            /* Remember case index and ifne position for patching */
            cases[i].ast_idx = (int)ifne_pos;  /* Reuse field to store patch pos */
            case_body_positions[i] = 0;  /* Will be set when we generate bodies */
            
            i++;
        }
        
        /* Fall through to default (will be patched) */
        size_t goto_default_pos = mg->code->length;
        bc_emit(mg->code, OP_GOTO);
        bc_emit_u2(mg->code, 0);  /* Will patch to default */
        
        /* Add to list of gotos to patch */
        if (!goto_patches) {
            goto_patches = slist_new((void *)(uintptr_t)goto_default_pos);
        } else {
            slist_append(goto_patches, (void *)(uintptr_t)goto_default_pos);
        }
    }
    
    /* Generate case bodies in original order */
    size_t default_body_pos = 0;
    ast_idx = 0;
    
    /* Save stackmap state before case bodies - each case body is entered
     * independently from the switch, so they should all have the same incoming
     * local types (not affected by assignments in other case bodies) */
    stackmap_state_t *switch_entry_state = NULL;
    if (mg->stackmap) {
        switch_entry_state = stackmap_save_state(mg->stackmap);
    }
    
    for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
        ast_node_t *case_label = (ast_node_t *)node->data;
        if (case_label->type != AST_CASE_LABEL) continue;
        
        bool is_default = (case_label->data.node.name && 
                          strcmp(case_label->data.node.name, "default") == 0);
        
        /* Restore stackmap state to switch entry state before each case body */
        if (switch_entry_state && mg->stackmap) {
            stackmap_restore_state(mg->stackmap, switch_entry_state);
        }
        
        size_t body_pos = mg->code->length;
        
        /* Record frame at case body (branch target) */
        mg_record_frame(mg);
        
        if (is_default) {
            default_body_pos = body_pos;
        } else {
            /* Find this case in our sorted array and patch the ifne */
            for (int i = 0; i < actual_cases; i++) {
                if (cases[i].case_label == case_label) {
                    size_t ifne_pos = (size_t)cases[i].ast_idx;
                    int16_t jump_offset = (int16_t)(body_pos - ifne_pos);
                    mg->code->code[ifne_pos + 1] = (jump_offset >> 8) & 0xFF;
                    mg->code->code[ifne_pos + 2] = jump_offset & 0xFF;
                    break;
                }
            }
        }
        
        /* Generate case body statements */
        slist_t *stmts = case_label->data.node.children;
        if (!is_default && stmts) {
            stmts = stmts->next;  /* Skip case expression */
        }
        while (stmts) {
            if (!codegen_statement(mg, (ast_node_t *)stmts->data)) {
                free(cases);
                free(hash_offset_positions);
                free(hash_values);
                free(case_body_positions);
                slist_free(goto_patches);
                stackmap_state_free(switch_entry_state);
                return false;
            }
            stmts = stmts->next;
        }
    }
    
    /* Free the saved state */
    stackmap_state_free(switch_entry_state);
    
    /* Switch end position */
    size_t switch_end = mg->code->length;
    
    /* Only record stackmap frame at switch end if there are break statements to patch */
    if (mg->loop_stack) {
        loop_context_t *ctx = (loop_context_t *)mg->loop_stack->data;
        if (ctx->break_offsets) {
            mg_record_frame(mg);
        }
    }
    
    /* Patch default offset in lookupswitch */
    size_t default_target = default_body_pos ? default_body_pos : switch_end;
    int32_t default_offset = (int32_t)(default_target - switch_pos);
    mg->code->code[default_offset_pos + 0] = (default_offset >> 24) & 0xFF;
    mg->code->code[default_offset_pos + 1] = (default_offset >> 16) & 0xFF;
    mg->code->code[default_offset_pos + 2] = (default_offset >> 8) & 0xFF;
    mg->code->code[default_offset_pos + 3] = default_offset & 0xFF;
    
    /* Patch goto-default jumps */
    for (slist_t *p = goto_patches; p; p = p->next) {
        size_t goto_pos = (size_t)(uintptr_t)p->data;
        int16_t jump_offset = (int16_t)(default_target - goto_pos);
        mg->code->code[goto_pos + 1] = (jump_offset >> 8) & 0xFF;
        mg->code->code[goto_pos + 2] = jump_offset & 0xFF;
    }
    
    /* Pop switch context and patch breaks */
    mg_pop_loop(mg, switch_end);
    
    /* Cleanup */
    free(cases);
    free(hash_offset_positions);
    free(hash_values);
    free(case_body_positions);
    slist_free(goto_patches);
    
    mg->last_opcode = 0;
    return true;
}

/* ========================================================================
 * Try-With-Resources Support
 * ======================================================================== */

/* Forward declaration */
bool codegen_statement(method_gen_t *mg, ast_node_t *stmt);

/**
 * Generate code for try-with-resources statement.
 *
 * For: try (Type r = expr) { body }
 * 
 * Generated structure:
 *   - Initialize resources, store in locals
 *   - try block: execute body
 *   - synthetic finally: close resources in reverse order
 *   - handle suppressed exceptions via Throwable.addSuppressed()
 */
static bool codegen_try_with_resources(method_gen_t *mg, slist_t *resources,
                                        ast_node_t *try_block,
                                        slist_t *catch_clauses,
                                        ast_node_t *finally_clause)
{
    if (!resources) {
        return false;  /* Should have at least one resource */
    }
    
    /* Count resources and allocate storage for resource slots */
    int resource_count = 0;
    for (slist_t *n = resources; n; n = n->next) {
        resource_count++;
    }
    
    uint16_t *resource_slots = malloc(resource_count * sizeof(uint16_t));
    type_t **resource_types = malloc(resource_count * sizeof(type_t *));
    if (!resource_slots || !resource_types) {
        free(resource_slots);
        free(resource_types);
        slist_free(resources);
        slist_free(catch_clauses);
        return false;
    }
    
    /* Initialize each resource
     * Two forms:
     * 1. Declaration: try (Type var = expr) - allocate slot, init, store
     * 2. Reference (Java 9+): try (existingVar) - use existing slot
     *    flags bit 1 indicates reference form
     */
    int idx = 0;
    for (slist_t *node = resources; node; node = node->next, idx++) {
        ast_node_t *res = (ast_node_t *)node->data;
        slist_t *res_children = res->data.node.children;
        bool is_reference = (res->data.node.flags & 2) != 0;
        
        if (!res_children) {
            continue;  /* Malformed resource spec */
        }
        
        if (is_reference) {
            /* Existing variable reference - just look up its slot */
            ast_node_t *var_expr = (ast_node_t *)res_children->data;
            
            /* Get the type from semantic analysis */
            type_t *res_type = res->sem_type;
            resource_types[idx] = res_type;
            
            if (var_expr->type == AST_IDENTIFIER) {
                /* Simple variable reference - look up its slot in the method generator */
                const char *var_name = var_expr->data.leaf.name;
                uint16_t slot = mg_get_local(mg, var_name);
                resource_slots[idx] = slot;
            } else if (var_expr->type == AST_FIELD_ACCESS) {
                /* Field access - need to load field value into a temp local */
                if (!codegen_expression(mg, var_expr)) {
                    free(resource_slots);
                    free(resource_types);
                    slist_free(resources);
                    slist_free(catch_clauses);
                    return false;
                }
                
                /* Allocate temp slot for the field value */
                uint16_t slot = mg_allocate_local(mg, "__resource", res_type);
                resource_slots[idx] = slot;
                
                /* Store the field value */
                if (slot <= 3) {
                    bc_emit(mg->code, OP_ASTORE_0 + slot);
                } else {
                    bc_emit(mg->code, OP_ASTORE);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                }
                mg_pop_typed(mg, 1);
            } else {
                /* Other expression - evaluate and store in temp */
                if (!codegen_expression(mg, var_expr)) {
                    free(resource_slots);
                    free(resource_types);
                    slist_free(resources);
                    slist_free(catch_clauses);
                    return false;
                }
                
                uint16_t slot = mg_allocate_local(mg, "__resource", res_type);
                resource_slots[idx] = slot;
                
                if (slot <= 3) {
                    bc_emit(mg->code, OP_ASTORE_0 + slot);
                } else {
                    bc_emit(mg->code, OP_ASTORE);
                    bc_emit_u1(mg->code, (uint8_t)slot);
                }
                mg_pop_typed(mg, 1);
            }
        } else {
            /* Declaration form */
            if (!res_children->next) {
                continue;  /* Malformed: missing initializer */
            }
            
            ast_node_t *type_node = (ast_node_t *)res_children->data;
            ast_node_t *init_expr = (ast_node_t *)res_children->next->data;
            const char *var_name = res->data.node.name;
            
            /* Resolve resource type */
            type_t *res_type = semantic_resolve_type(mg->class_gen->sem, type_node);
            resource_types[idx] = res_type;
            
            /* Generate initializer */
            if (!codegen_expression(mg, init_expr)) {
                free(resource_slots);
                free(resource_types);
                slist_free(resources);
                slist_free(catch_clauses);
                return false;
            }
            
            /* Allocate local variable for resource */
            uint16_t slot = mg_allocate_local(mg, var_name, res_type);
            resource_slots[idx] = slot;
            
            /* Store resource in local variable */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ASTORE_0 + slot);
            } else {
                bc_emit(mg->code, OP_ASTORE);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_pop_typed(mg, 1);
        }
    }
    
    /* Allocate slot for primary exception (used for suppressed exception handling) */
    type_t *throwable_type = type_new_class("java/lang/Throwable");
    uint16_t primary_exc_slot = mg_allocate_local(mg, "__primary_exc", throwable_type);
    
    /* Initialize primary exception to null */
    bc_emit(mg->code, OP_ACONST_NULL);
    mg_push(mg, 1);
    if (primary_exc_slot <= 3) {
        bc_emit(mg->code, OP_ASTORE_0 + primary_exc_slot);
    } else {
        bc_emit(mg->code, OP_ASTORE);
        bc_emit_u1(mg->code, (uint8_t)primary_exc_slot);
    }
    mg_pop_typed(mg, 1);
    
    /* Record start of protected region */
    uint16_t try_start = (uint16_t)mg->code->length;
    
    /* Save stackmap state at try block entry - this is the state needed for
     * exception handlers since exceptions can be thrown at any point in the try */
    stackmap_state_t *try_entry_state = NULL;
    if (mg->stackmap) {
        try_entry_state = stackmap_save_state(mg->stackmap);
    }
    
    /* Generate try block body */
    if (try_block && !codegen_statement(mg, try_block)) {
        stackmap_state_free(try_entry_state);
        free(resource_slots);
        free(resource_types);
        slist_free(resources);
        slist_free(catch_clauses);
        return false;
    }
    
    /* Check if try block ends with a terminating instruction */
    uint8_t try_last_op = mg->last_opcode;
    bool try_ends_with_return = (try_last_op == OP_RETURN || try_last_op == OP_ARETURN ||
                                 try_last_op == OP_IRETURN || try_last_op == OP_LRETURN ||
                                 try_last_op == OP_FRETURN || try_last_op == OP_DRETURN ||
                                 try_last_op == OP_ATHROW);
    
    /* Generate user's finally code if present (before resource cleanup) */
    if (finally_clause && finally_clause->data.node.children) {
        ast_node_t *user_finally = (ast_node_t *)finally_clause->data.node.children->data;
        if (!codegen_statement(mg, user_finally)) {
            free(resource_slots);
            free(resource_types);
            slist_free(resources);
            slist_free(catch_clauses);
            return false;
        }
    }
    
    /* Normal path: close resources in reverse order (only if try block doesn't return) */
    size_t normal_exit_goto = 0;
    bool has_normal_exit = !try_ends_with_return;
    
    if (has_normal_exit) {
        for (int i = resource_count - 1; i >= 0; i--) {
            uint16_t slot = resource_slots[i];
            
            /* if (resource != null) resource.close(); */
            /* Load resource */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ALOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_ALOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push(mg, 1);
            
            /* ifnull skip_close */
            size_t ifnull_pos = mg->code->length;
            bc_emit(mg->code, OP_IFNULL);
            bc_emit_u2(mg->code, 0);  /* Placeholder */
            mg_pop_typed(mg, 1);
            
            /* Load resource again for close() call */
            if (slot <= 3) {
                bc_emit(mg->code, OP_ALOAD_0 + slot);
            } else {
                bc_emit(mg->code, OP_ALOAD);
                bc_emit_u1(mg->code, (uint8_t)slot);
            }
            mg_push(mg, 1);
            
            /* invokeinterface AutoCloseable.close()V */
            uint16_t close_ref = cp_add_interface_methodref(mg->cp,
                "java/lang/AutoCloseable", "close", "()V");
            bc_emit(mg->code, OP_INVOKEINTERFACE);
            bc_emit_u2(mg->code, close_ref);
            bc_emit_u1(mg->code, 1);  /* count (1 for 'this') */
            bc_emit_u1(mg->code, 0);  /* reserved */
            mg_pop_typed(mg, 1);
            
            /* Patch ifnull - this is a branch target */
            uint16_t skip_close = (uint16_t)mg->code->length;
            mg_record_frame(mg);  /* Record frame at branch target */
            int16_t offset = (int16_t)(skip_close - ifnull_pos);
            mg->code->code[ifnull_pos + 1] = (offset >> 8) & 0xFF;
            mg->code->code[ifnull_pos + 2] = offset & 0xFF;
        }
        
        /* Jump past exception handlers */
        normal_exit_goto = mg->code->length;
        bc_emit(mg->code, OP_GOTO);
        bc_emit_u2(mg->code, 0);  /* Placeholder */
    }
    
    uint16_t try_end = (uint16_t)mg->code->length;
    
    /* Exception handler: store exception, close resources with suppression */
    uint16_t exc_handler_pc = (uint16_t)mg->code->length;
    
    /* Restore stackmap to try block entry state for exception handler frame.
     * The exception can be thrown at any point in the try block, so the locals
     * should be those that existed at try block entry. */
    if (try_entry_state && mg->stackmap) {
        stackmap_restore_state(mg->stackmap, try_entry_state);
    }
    
    /* Record frame at exception handler (TWR cleanup handler)
     * At exception handler, JVM clears stack and pushes exception. */
    mg_record_exception_handler_frame(mg, "java/lang/Throwable");
    
    /* Add exception handler entry for any Throwable */
    mg_add_exception_handler(mg, try_start, try_end, exc_handler_pc, 0);
    
    /* Stack has exception (pushed by JVM) - store it */
    mg_push(mg, 1);
    if (primary_exc_slot <= 3) {
        bc_emit(mg->code, OP_ASTORE_0 + primary_exc_slot);
    } else {
        bc_emit(mg->code, OP_ASTORE);
        bc_emit_u1(mg->code, (uint8_t)primary_exc_slot);
    }
    mg_pop_typed(mg, 1);
    
    /* Close resources in reverse order, with suppressed exception handling */
    for (int i = resource_count - 1; i >= 0; i--) {
        uint16_t slot = resource_slots[i];
        
        /* if (resource != null) */
        if (slot <= 3) {
            bc_emit(mg->code, OP_ALOAD_0 + slot);
        } else {
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)slot);
        }
        mg_push(mg, 1);
        
        size_t ifnull_pos = mg->code->length;
        bc_emit(mg->code, OP_IFNULL);
        bc_emit_u2(mg->code, 0);
        mg_pop_typed(mg, 1);
        
        /* try { resource.close(); } catch (Throwable t) { primary.addSuppressed(t); } */
        uint16_t close_try_start = (uint16_t)mg->code->length;
        
        /* Load resource for close() */
        if (slot <= 3) {
            bc_emit(mg->code, OP_ALOAD_0 + slot);
        } else {
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)slot);
        }
        mg_push(mg, 1);
        
        /* invokeinterface AutoCloseable.close()V */
        uint16_t close_ref = cp_add_interface_methodref(mg->cp,
            "java/lang/AutoCloseable", "close", "()V");
        bc_emit(mg->code, OP_INVOKEINTERFACE);
        bc_emit_u2(mg->code, close_ref);
        bc_emit_u1(mg->code, 1);
        bc_emit_u1(mg->code, 0);
        mg_pop_typed(mg, 1);
        
        /* Jump past suppression handler */
        size_t close_ok_goto = mg->code->length;
        bc_emit(mg->code, OP_GOTO);
        bc_emit_u2(mg->code, 0);
        
        uint16_t close_try_end = (uint16_t)mg->code->length;
        
        /* Suppression exception handler */
        uint16_t suppress_handler_pc = (uint16_t)mg->code->length;
        
        /* Record frame at suppression handler
         * At exception handler, JVM clears stack and pushes exception. */
        mg_record_exception_handler_frame(mg, "java/lang/Throwable");
        
        mg_add_exception_handler(mg, close_try_start, close_try_end, suppress_handler_pc, 0);
        
        /* Stack has suppressed exception (pushed by JVM) */
        mg_push(mg, 1);
        
        /* primary.addSuppressed(suppressed) */
        /* Load primary exception */
        if (primary_exc_slot <= 3) {
            bc_emit(mg->code, OP_ALOAD_0 + primary_exc_slot);
        } else {
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)primary_exc_slot);
        }
        mg_push(mg, 1);
        
        /* Swap so we have: primary, suppressed */
        bc_emit(mg->code, OP_SWAP);
        
        /* invokevirtual Throwable.addSuppressed(Throwable)V */
        uint16_t add_suppressed_ref = cp_add_methodref(mg->cp,
            "java/lang/Throwable", "addSuppressed", "(Ljava/lang/Throwable;)V");
        bc_emit(mg->code, OP_INVOKEVIRTUAL);
        bc_emit_u2(mg->code, add_suppressed_ref);
        mg_pop_typed(mg, 2);
        
        /* Patch close_ok_goto and ifnull - this is a branch target */
        uint16_t after_suppress = (uint16_t)mg->code->length;
        mg_record_frame(mg);  /* Record frame at branch target */
        int16_t close_offset = (int16_t)(after_suppress - close_ok_goto);
        mg->code->code[close_ok_goto + 1] = (close_offset >> 8) & 0xFF;
        mg->code->code[close_ok_goto + 2] = close_offset & 0xFF;
        
        /* Patch ifnull */
        int16_t ifnull_offset = (int16_t)(after_suppress - ifnull_pos);
        mg->code->code[ifnull_pos + 1] = (ifnull_offset >> 8) & 0xFF;
        mg->code->code[ifnull_pos + 2] = ifnull_offset & 0xFF;
    }
    
    /* Re-throw primary exception */
    if (primary_exc_slot <= 3) {
        bc_emit(mg->code, OP_ALOAD_0 + primary_exc_slot);
    } else {
        bc_emit(mg->code, OP_ALOAD);
        bc_emit_u1(mg->code, (uint8_t)primary_exc_slot);
    }
    mg_push(mg, 1);
    bc_emit(mg->code, OP_ATHROW);
    mg_pop_typed(mg, 1);
    
    /* Generate user catch clauses if present.
     * These catch exceptions from the entire TWR including re-thrown exceptions. */
    slist_t *catch_gotos = NULL;
    bool has_user_catches = (catch_clauses != NULL);
    
    /* Save locals count before catch handlers - locals allocated in catch blocks
     * should not be visible at the join point after all handlers */
    uint16_t saved_locals_count = 0;
    uint16_t saved_slot = 0;
    if (has_user_catches) {
        saved_locals_count = mg_save_locals_count(mg);
        saved_slot = mg->next_slot;
    }
    
    for (slist_t *node = catch_clauses; node; node = node->next) {
        ast_node_t *catch_clause = (ast_node_t *)node->data;
        slist_t *catch_children = catch_clause->data.node.children;
        
        if (!catch_children || !catch_children->next) {
            continue;  /* Malformed catch */
        }
        
        ast_node_t *exc_type_node = (ast_node_t *)catch_children->data;
        ast_node_t *catch_block = (ast_node_t *)catch_children->next->data;
        const char *exc_var_name = catch_clause->data.node.name;
        
        /* Get catch handler start position */
        uint16_t catch_handler_pc = (uint16_t)mg->code->length;
        
        /* Restore stackmap to try entry state for this catch handler */
        if (try_entry_state && mg->stackmap) {
            stackmap_restore_state(mg->stackmap, try_entry_state);
        }
        
        /* Get exception class name from semantic type */
        const char *exc_class_internal = "java/lang/Throwable";
        if (exc_type_node->sem_type && exc_type_node->sem_type->kind == TYPE_CLASS &&
            exc_type_node->sem_type->data.class_type.name) {
            char *internal = class_to_internal_name(exc_type_node->sem_type->data.class_type.name);
            if (internal) {
                exc_class_internal = internal;
            }
        }
        
        /* Record frame at catch handler */
        mg_record_exception_handler_frame(mg, exc_class_internal);
        
        /* Add exception handler entry - catch from entire TWR (try_start to exc_handler_pc+re-throw) */
        uint16_t exc_class_idx = cp_add_class(mg->cp, exc_class_internal);
        mg_add_exception_handler(mg, try_start, (uint16_t)mg->code->length, catch_handler_pc, exc_class_idx);
        
        /* Allocate local for exception variable */
        type_t *exc_type = exc_type_node->sem_type ? exc_type_node->sem_type : 
                          type_new_class(exc_class_internal);
        uint16_t exc_slot = mg_allocate_local(mg, exc_var_name, exc_type);
        
        /* JVM pushes exception onto stack at handler entry */
        mg_push(mg, 1);
        
        /* Store exception in local */
        if (exc_slot <= 3) {
            bc_emit(mg->code, OP_ASTORE_0 + exc_slot);
        } else {
            bc_emit(mg->code, OP_ASTORE);
            bc_emit_u1(mg->code, (uint8_t)exc_slot);
        }
        mg_pop_typed(mg, 1);
        
        /* Generate catch block */
        if (!codegen_statement(mg, catch_block)) {
            free(resource_slots);
            free(resource_types);
            slist_free(resources);
            slist_free(catch_clauses);
            slist_free(catch_gotos);
            stackmap_state_free(try_entry_state);
            return false;
        }
        
        /* Generate goto to skip other catch handlers (if not ending with return/throw) */
        uint8_t last_op = mg->last_opcode;
        bool catch_ends_with_return = (last_op == OP_RETURN || last_op == OP_ARETURN ||
                                       last_op == OP_IRETURN || last_op == OP_LRETURN ||
                                       last_op == OP_FRETURN || last_op == OP_DRETURN ||
                                       last_op == OP_ATHROW);
        
        if (!catch_ends_with_return) {
            size_t *goto_pos = malloc(sizeof(size_t));
            *goto_pos = mg->code->length;
            catch_gotos = slist_prepend(catch_gotos, goto_pos);
            bc_emit(mg->code, OP_GOTO);
            bc_emit_u2(mg->code, 0);  /* Placeholder */
        }
        
        /* Free internal name if we allocated it */
        if (strcmp(exc_class_internal, "java/lang/Throwable") != 0) {
            free((char *)exc_class_internal);
        }
    }
    
    /* Track if all catch blocks end with return/throw */
    bool all_catches_return = true;
    for (slist_t *node = catch_clauses; node; node = node->next) {
        /* If any catch didn't add a goto, it ended with return/throw */
    }
    /* If we have no gotos to patch, all catches returned */
    all_catches_return = (catch_gotos == NULL);
    
    /* Patch normal exit goto if it exists */
    uint16_t after_try = (uint16_t)mg->code->length;
    
    /* Restore locals count before recording join point frame - locals allocated
     * in catch blocks should not be visible at the join point */
    if (has_user_catches) {
        mg_restore_locals_count(mg, saved_locals_count);
        mg->next_slot = saved_slot;
        if (mg->stackmap) {
            mg->stackmap->current_locals_count = saved_locals_count;
        }
    }
    
    /* Record frame at end of try-with-resources (join point) only if:
     * 1. There's a normal exit (try block doesn't return), or
     * 2. There are catch gotos to patch (some catch path falls through) */
    bool needs_join_frame = has_normal_exit || (catch_gotos != NULL);
    if (needs_join_frame) {
        mg_record_frame(mg);
    }
    
    if (has_normal_exit) {
        int16_t exit_offset = (int16_t)(after_try - normal_exit_goto);
        mg->code->code[normal_exit_goto + 1] = (exit_offset >> 8) & 0xFF;
        mg->code->code[normal_exit_goto + 2] = exit_offset & 0xFF;
    }
    
    /* Patch catch gotos */
    for (slist_t *node = catch_gotos; node; node = node->next) {
        size_t *goto_pos = (size_t *)node->data;
        int16_t offset = (int16_t)(after_try - *goto_pos);
        mg->code->code[*goto_pos + 1] = (offset >> 8) & 0xFF;
        mg->code->code[*goto_pos + 2] = offset & 0xFF;
    }
    slist_free_full(catch_gotos, free);
    
    /* Cleanup */
    free(resource_slots);
    free(resource_types);
    slist_free(resources);
    slist_free(catch_clauses);
    stackmap_state_free(try_entry_state);
    
    mg->last_opcode = 0;
    return true;
}

/* ========================================================================
 * Statement Code Generation
 * ======================================================================== */

bool codegen_statement(method_gen_t *mg, ast_node_t *stmt)
{
    if (!stmt) {
        return true;
    }
    
    /* Record line number for debugging/stack traces */
    if (stmt->line > 0) {
        mg_record_line(mg, stmt->line);
    }
    
    switch (stmt->type) {
        case AST_BLOCK:
            {
                slist_t *children = stmt->data.node.children;
                for (slist_t *node = children; node; node = node->next) {
                    ast_node_t *child = (ast_node_t *)node->data;
                    if (!codegen_statement(mg, child)) {
                        return false;
                    }
                }
                return true;
            }
        
        case AST_EXPR_STMT:
            {
                slist_t *children = stmt->data.node.children;
                if (children) {
                    ast_node_t *expr = (ast_node_t *)children->data;
                    
                    /* Track stack depth before expression */
                    uint16_t stack_before = mg->stack_depth;
                    
                    if (!codegen_expr(mg, expr, mg->cp)) {
                        return false;
                    }
                    
                    /* Pop the result if expression left a value on stack */
                    /* Note: Assignment expressions leave their value for chaining */
                    uint16_t slots_to_pop = mg->stack_depth - stack_before;
                    if (slots_to_pop >= 2) {
                        /* Category-2 type (long/double) - use POP2 */
                        bc_emit(mg->code, OP_POP2);
                        mg_pop_typed(mg, 2);
                    } else if (slots_to_pop == 1) {
                        /* Category-1 type - use POP */
                        bc_emit(mg->code, OP_POP);
                        mg_pop_typed(mg, 1);
                    }
                }
                return true;
            }
        
        case AST_RETURN_STMT:
            {
                slist_t *children = stmt->data.node.children;
                if (children) {
                    ast_node_t *return_expr = (ast_node_t *)children->data;
                    if (!codegen_expr(mg, return_expr, mg->cp)) {
                        return false;
                    }
                    
                    /* Determine appropriate return opcode based on return expression type */
                    uint8_t return_op = OP_IRETURN;  /* Default to int */
                    
                    /* Check if return expression is a reference type */
                    if (is_string_type(return_expr)) {
                        return_op = OP_ARETURN;
                    } else if (return_expr->type == AST_NEW_OBJECT || 
                               return_expr->type == AST_NEW_ARRAY) {
                        return_op = OP_ARETURN;
                    } else if (return_expr->type == AST_THIS_EXPR) {
                        return_op = OP_ARETURN;
                    } else if (return_expr->type == AST_FIELD_ACCESS) {
                        /* Field access - check if it's a reference type field */
                        if (return_expr->sem_type) {
                            if (return_expr->sem_type->kind == TYPE_CLASS ||
                                return_expr->sem_type->kind == TYPE_ARRAY) {
                                return_op = OP_ARETURN;
                            } else if (return_expr->sem_type->kind == TYPE_LONG) {
                                return_op = OP_LRETURN;
                            } else if (return_expr->sem_type->kind == TYPE_FLOAT) {
                                return_op = OP_FRETURN;
                            } else if (return_expr->sem_type->kind == TYPE_DOUBLE) {
                                return_op = OP_DRETURN;
                            }
                        } else {
                            /* No sem_type - infer from field lookup */
                            const char *field_name = return_expr->data.node.name;
                            if (mg->class_gen) {
                                field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, field_name);
                                if (field && field->descriptor) {
                                    char desc = field->descriptor[0];
                                    if (desc == 'L' || desc == '[') {
                                        return_op = OP_ARETURN;
                                    } else if (desc == 'J') {
                                        return_op = OP_LRETURN;
                                    } else if (desc == 'F') {
                                        return_op = OP_FRETURN;
                                    } else if (desc == 'D') {
                                        return_op = OP_DRETURN;
                                    }
                                }
                            }
                        }
                    } else if (return_expr->type == AST_IDENTIFIER) {
                        /* Check if it's a reference type local or field */
                        const char *name = return_expr->data.leaf.name;
                        if (mg_local_is_ref(mg, name)) {
                            return_op = OP_ARETURN;
                        } else if (mg->class_gen) {
                            /* Check if it's a field in current class */
                            field_gen_t *field = hashtable_lookup(mg->class_gen->field_map, name);
                            if (field && field->descriptor) {
                                char desc = field->descriptor[0];
                                if (desc == 'L' || desc == '[') {
                                    return_op = OP_ARETURN;
                                } else if (desc == 'J') {
                                    return_op = OP_LRETURN;
                                } else if (desc == 'F') {
                                    return_op = OP_FRETURN;
                                } else if (desc == 'D') {
                                    return_op = OP_DRETURN;
                                }
                            } else if (mg->class_gen->class_sym) {
                                /* Check enclosing class for static nested classes */
                                symbol_t *enclosing = mg->class_gen->class_sym->data.class_data.enclosing_class;
                                if (enclosing && enclosing->data.class_data.members) {
                                    symbol_t *outer_field = scope_lookup_local(
                                        enclosing->data.class_data.members, name);
                                    if (outer_field && outer_field->kind == SYM_FIELD && outer_field->type) {
                                        type_kind_t kind = outer_field->type->kind;
                                        if (kind == TYPE_CLASS || kind == TYPE_ARRAY) {
                                            return_op = OP_ARETURN;
                                        } else if (kind == TYPE_LONG) {
                                            return_op = OP_LRETURN;
                                        } else if (kind == TYPE_FLOAT) {
                                            return_op = OP_FRETURN;
                                        } else if (kind == TYPE_DOUBLE) {
                                            return_op = OP_DRETURN;
                                        }
                                    }
                                }
                            }
                        }
                    } else if (return_expr->type == AST_LITERAL) {
                        /* Check for null literal */
                        if (return_expr->data.leaf.token_type == TOK_NULL) {
                            return_op = OP_ARETURN;
                        }
                    } else if (return_expr->type == AST_CAST_EXPR) {
                        /* Cast expression - check the target type */
                        slist_t *cast_children = return_expr->data.node.children;
                        if (cast_children) {
                            ast_node_t *type_node = (ast_node_t *)cast_children->data;
                            if (type_node->type == AST_CLASS_TYPE ||
                                type_node->type == AST_ARRAY_TYPE) {
                                return_op = OP_ARETURN;
                            } else if (type_node->type == AST_PRIMITIVE_TYPE) {
                                const char *prim = type_node->data.leaf.name;
                                if (strcmp(prim, "long") == 0) {
                                    return_op = OP_LRETURN;
                                } else if (strcmp(prim, "float") == 0) {
                                    return_op = OP_FRETURN;
                                } else if (strcmp(prim, "double") == 0) {
                                    return_op = OP_DRETURN;
                                }
                            }
                        }
                    } else if (return_expr->sem_type) {
                        /* Use semantic type info */
                        if (return_expr->sem_type->kind == TYPE_CLASS ||
                            return_expr->sem_type->kind == TYPE_ARRAY) {
                            return_op = OP_ARETURN;
                        } else if (return_expr->sem_type->kind == TYPE_LONG) {
                            return_op = OP_LRETURN;
                        } else if (return_expr->sem_type->kind == TYPE_FLOAT) {
                            return_op = OP_FRETURN;
                        } else if (return_expr->sem_type->kind == TYPE_DOUBLE) {
                            return_op = OP_DRETURN;
                        }
                    }
                    
                    /* If still default (ireturn), check method's declared return type */
                    if (return_op == OP_IRETURN && mg->method && mg->method->type) {
                        type_kind_t ret_kind = mg->method->type->kind;
                        if (ret_kind == TYPE_CLASS || ret_kind == TYPE_ARRAY) {
                            return_op = OP_ARETURN;
                        } else if (ret_kind == TYPE_LONG) {
                            return_op = OP_LRETURN;
                        } else if (ret_kind == TYPE_FLOAT) {
                            return_op = OP_FRETURN;
                        } else if (ret_kind == TYPE_DOUBLE) {
                            return_op = OP_DRETURN;
                        }
                    }
                    
                    bc_emit(mg->code, return_op);
                    mg->last_opcode = return_op;
                    mg_pop_typed(mg, 1);
                } else {
                    bc_emit(mg->code, OP_RETURN);
                    mg->last_opcode = OP_RETURN;
                }
                return true;
            }
        
        case AST_VAR_DECL:
            {
                slist_t *children = stmt->data.node.children;
                type_kind_t var_kind = TYPE_INT;  /* Default to int */
                bool is_ref_type = false;
                bool is_array_type = false;
                type_kind_t array_elem_kind = TYPE_INT;  /* Default element type */
                int array_dims = 1;  /* Dimension count for multi-dim arrays */
                const char *class_name = NULL;  /* Class name for class types */
                ast_node_t *type_ast = NULL;  /* Type AST node for type annotations */
                
                /* First child might be type */
                if (children) {
                    ast_node_t *first = (ast_node_t *)children->data;
                    type_ast = first;  /* Store for type annotations */
                    if (first->type == AST_PRIMITIVE_TYPE) {
                        /* Primitive type - determine the kind */
                        const char *prim_name = first->data.leaf.name;
                        if (prim_name) {
                            if (strcmp(prim_name, "long") == 0) {
                                var_kind = TYPE_LONG;
                            } else if (strcmp(prim_name, "float") == 0) {
                                var_kind = TYPE_FLOAT;
                            } else if (strcmp(prim_name, "double") == 0) {
                                var_kind = TYPE_DOUBLE;
                            } else if (strcmp(prim_name, "byte") == 0) {
                                var_kind = TYPE_BYTE;
                            } else if (strcmp(prim_name, "short") == 0) {
                                var_kind = TYPE_SHORT;
                            } else if (strcmp(prim_name, "char") == 0) {
                                var_kind = TYPE_CHAR;
                            } else if (strcmp(prim_name, "boolean") == 0) {
                                var_kind = TYPE_BOOLEAN;
                            }
                            /* else int is the default */
                        }
                        children = children->next;
                    } else if (first->type == AST_VAR_TYPE) {
                        /* 'var' type inference (Java 10+) - use sem_type for actual type */
                        if (first->sem_type) {
                            type_t *inferred = first->sem_type;
                            if (inferred->kind == TYPE_CLASS) {
                                is_ref_type = true;
                                var_kind = TYPE_CLASS;
                                if (inferred->data.class_type.symbol &&
                                    inferred->data.class_type.symbol->qualified_name) {
                                    class_name = class_to_internal_name(
                                        inferred->data.class_type.symbol->qualified_name);
                                } else if (inferred->data.class_type.name) {
                                    class_name = class_to_internal_name(
                                        inferred->data.class_type.name);
                                }
                            } else if (inferred->kind == TYPE_ARRAY) {
                                is_ref_type = true;
                                is_array_type = true;
                                var_kind = TYPE_ARRAY;
                                type_t *elem = inferred->data.array_type.element_type;
                                if (elem) {
                                    if (elem->kind == TYPE_CLASS) {
                                        array_elem_kind = TYPE_CLASS;
                                        if (elem->data.class_type.name) {
                                            class_name = class_to_internal_name(
                                                elem->data.class_type.name);
                                        }
                                    } else {
                                        array_elem_kind = elem->kind;
                                    }
                                }
                            } else {
                                var_kind = inferred->kind;
                            }
                        }
                        children = children->next;
                    } else if (first->type == AST_CLASS_TYPE) {
                        /* Check for 'var' type inference (legacy) - use sem_type for actual type */
                        if (first->data.node.name && strcmp(first->data.node.name, "var") == 0 &&
                            first->sem_type) {
                            /* 'var' infers type from initializer */
                            type_t *inferred = first->sem_type;
                            if (inferred->kind == TYPE_CLASS) {
                                is_ref_type = true;
                                var_kind = TYPE_CLASS;
                                if (inferred->data.class_type.symbol &&
                                    inferred->data.class_type.symbol->qualified_name) {
                                    class_name = class_to_internal_name(
                                        inferred->data.class_type.symbol->qualified_name);
                                } else if (inferred->data.class_type.name) {
                                    class_name = class_to_internal_name(
                                        inferred->data.class_type.name);
                                }
                            } else if (inferred->kind == TYPE_ARRAY) {
                                is_ref_type = true;
                                is_array_type = true;
                                var_kind = TYPE_ARRAY;
                                /* Get element type from inferred array type */
                                type_t *elem = inferred->data.array_type.element_type;
                                if (elem) {
                                    if (elem->kind == TYPE_CLASS) {
                                        array_elem_kind = TYPE_CLASS;
                                        if (elem->data.class_type.name) {
                                            class_name = class_to_internal_name(
                                                elem->data.class_type.name);
                                        }
                                    } else {
                                        array_elem_kind = elem->kind;
                                    }
                                }
                                array_dims = inferred->data.array_type.dimensions;
                            } else {
                                /* Primitive type inferred */
                                is_ref_type = false;
                                var_kind = inferred->kind;
                            }
                            children = children->next;
                        } else {
                            is_ref_type = true;
                            var_kind = TYPE_CLASS;
                            /* Get the class name and convert to internal format */
                            /* Prefer sem_type which has the resolved qualified name */
                            if (first->sem_type && first->sem_type->kind == TYPE_CLASS) {
                                if (first->sem_type->data.class_type.symbol &&
                                    first->sem_type->data.class_type.symbol->qualified_name) {
                                    class_name = class_to_internal_name(
                                        first->sem_type->data.class_type.symbol->qualified_name);
                                } else if (first->sem_type->data.class_type.name) {
                                    class_name = class_to_internal_name(
                                        first->sem_type->data.class_type.name);
                                }
                            }
                            /* Fall back to AST name if sem_type not available */
                            if (!class_name && first->data.node.name) {
                                class_name = class_to_internal_name(first->data.node.name);
                            }
                            children = children->next;
                        }
                    } else if (first->type == AST_ARRAY_TYPE) {
                        is_ref_type = true;
                        is_array_type = true;
                        var_kind = TYPE_ARRAY;
                        /* Count dimensions and determine innermost element type */
                        ast_node_t *cur = first;
                        array_dims = 0;
                        while (cur && cur->type == AST_ARRAY_TYPE) {
                            array_dims++;
                            if (cur->data.node.children) {
                                cur = (ast_node_t *)cur->data.node.children->data;
                            } else {
                                break;
                            }
                        }
                        /* cur now points to the innermost element type */
                        if (cur) {
                            if (cur->type == AST_CLASS_TYPE) {
                                array_elem_kind = TYPE_CLASS;
                                /* Get the element class name - prefer resolved semantic type */
                                if (cur->sem_type && cur->sem_type->kind == TYPE_CLASS &&
                                    cur->sem_type->data.class_type.name) {
                                    /* Use fully qualified name from semantic analysis */
                                    class_name = class_to_internal_name(cur->sem_type->data.class_type.name);
                                } else if (cur->data.node.name) {
                                    /* Fallback to AST name (resolve java.lang classes) */
                                    const char *resolved = resolve_java_lang_class(cur->data.node.name);
                                    class_name = class_to_internal_name(resolved);
                                }
                            } else if (cur->type == AST_PRIMITIVE_TYPE) {
                                const char *prim_name = cur->data.leaf.name;
                                if (prim_name) {
                                    if (strcmp(prim_name, "int") == 0) {
                                        array_elem_kind = TYPE_INT;
                                    } else if (strcmp(prim_name, "byte") == 0) {
                                        array_elem_kind = TYPE_BYTE;
                                    } else if (strcmp(prim_name, "short") == 0) {
                                        array_elem_kind = TYPE_SHORT;
                                    } else if (strcmp(prim_name, "long") == 0) {
                                        array_elem_kind = TYPE_LONG;
                                    } else if (strcmp(prim_name, "float") == 0) {
                                        array_elem_kind = TYPE_FLOAT;
                                    } else if (strcmp(prim_name, "double") == 0) {
                                        array_elem_kind = TYPE_DOUBLE;
                                    } else if (strcmp(prim_name, "char") == 0) {
                                        array_elem_kind = TYPE_CHAR;
                                    } else if (strcmp(prim_name, "boolean") == 0) {
                                        array_elem_kind = TYPE_BOOLEAN;
                                    }
                                }
                            }
                        }
                        children = children->next;
                    }
                }
                
                /* Process declarators */
                while (children) {
                    ast_node_t *decl = (ast_node_t *)children->data;
                    if (decl->type == AST_VAR_DECLARATOR) {
                        const char *name = decl->data.node.name;
                        
                        /* Allocate slot and track type */
                        uint16_t slot = mg->next_slot;
                        int size = (var_kind == TYPE_LONG || var_kind == TYPE_DOUBLE) ? 2 : 1;
                        mg->next_slot += size;
                        if (mg->next_slot > mg->max_locals) {
                            mg->max_locals = mg->next_slot;
                        }
                        
                        /* Create consolidated local variable info */
                        local_var_info_t *var_info = local_var_info_new(slot, var_kind);
                        if (var_info) {
                            var_info->is_ref = is_ref_type;
                            var_info->is_array = is_array_type;
                            if (is_ref_type && class_name) {
                                var_info->class_name = strdup(class_name);
                            }
                            if (is_array_type) {
                                var_info->array_dims = array_dims;
                                var_info->array_elem_kind = array_elem_kind;
                                if (array_elem_kind == TYPE_CLASS && class_name) {
                                    var_info->array_elem_class = strdup(class_name);
                                }
                            }
                            hashtable_insert(mg->locals, name, var_info);
                        }
                        
                        /* Record for LocalVariableTable */
                        {
                            const char *desc = NULL;
                            if (is_array_type) {
                                /* Build array descriptor */
                                char arr_desc[256];
                                int i;
                                for (i = 0; i < array_dims && i < 250; i++) {
                                    arr_desc[i] = '[';
                                }
                                if (array_elem_kind == TYPE_CLASS && class_name) {
                                    snprintf(arr_desc + i, sizeof(arr_desc) - i, "L%s;", class_name);
                                } else {
                                    /* Primitive element */
                                    static const char prims[] = "IJFDBSCZ";
                                    arr_desc[i] = prims[array_elem_kind < 8 ? array_elem_kind : 0];
                                    arr_desc[i + 1] = '\0';
                                }
                                desc = strdup(arr_desc);
                            } else if (is_ref_type && class_name) {
                                char *ref_desc = calloc(1, strlen(class_name) + 3);
                                sprintf(ref_desc, "L%s;", class_name);
                                desc = ref_desc;
                            } else {
                                /* Primitive type descriptor */
                                switch (var_kind) {
                                    case TYPE_BOOLEAN: desc = "Z"; break;
                                    case TYPE_BYTE:    desc = "B"; break;
                                    case TYPE_CHAR:    desc = "C"; break;
                                    case TYPE_SHORT:   desc = "S"; break;
                                    case TYPE_INT:     desc = "I"; break;
                                    case TYPE_LONG:    desc = "J"; break;
                                    case TYPE_FLOAT:   desc = "F"; break;
                                    case TYPE_DOUBLE:  desc = "D"; break;
                                    default:           desc = "I"; break;
                                }
                            }
                            mg_record_local_var(mg, name, desc,
                                              slot, (uint16_t)mg->code->length, type_ast);
                        }
                        
                        /* Generate initializer if present, or track uninitialized slot */
                        if (decl->data.node.children) {
                            ast_node_t *init_expr = (ast_node_t *)decl->data.node.children->data;
                            if (!codegen_expr(mg, init_expr, mg->cp)) {
                                return false;
                            }
                            
                            /* Autoboxing/unboxing: check if conversion needed.
                             * IMPORTANT: Don't use get_expr_type_kind alone as it returns 
                             * primitive types for wrapper classes (for arithmetic purposes).
                             * We need to check if the expression actually produces a reference. */
                            
                            /* Check if init expression produces a reference type */
                            bool init_is_ref = (init_expr->type == AST_NEW_OBJECT ||
                                               init_expr->type == AST_NEW_ARRAY ||
                                               (init_expr->sem_type && 
                                                (init_expr->sem_type->kind == TYPE_CLASS ||
                                                 init_expr->sem_type->kind == TYPE_ARRAY)));
                            
                            if (is_ref_type && class_name && !init_is_ref) {
                                /* Variable is a reference type and init is NOT a reference - 
                                 * check if boxing needed */
                                type_kind_t init_kind = get_expr_type_kind(mg, init_expr);
                                type_kind_t target_prim = get_primitive_for_wrapper(class_name);
                                if (target_prim != TYPE_UNKNOWN && 
                                    (init_kind == TYPE_INT || init_kind == TYPE_LONG ||
                                     init_kind == TYPE_FLOAT || init_kind == TYPE_DOUBLE ||
                                     init_kind == TYPE_BYTE || init_kind == TYPE_SHORT ||
                                     init_kind == TYPE_CHAR || init_kind == TYPE_BOOLEAN)) {
                                    /* Variable is wrapper type, init is primitive - box it */
                                    emit_boxing(mg, mg->cp, init_kind);
                                }
                            } else if (!is_ref_type) {
                                /* Variable is primitive, init might be wrapper - unbox.
                                 * Check init expression's sem_type for wrapper class types. */
                                const char *init_class = NULL;
                                if (init_expr->sem_type && init_expr->sem_type->kind == TYPE_CLASS &&
                                    init_expr->sem_type->data.class_type.name) {
                                    init_class = init_expr->sem_type->data.class_type.name;
                                } else if (init_expr->type == AST_IDENTIFIER) {
                                    /* Fallback: check local variable type for identifiers */
                                    const char *init_ident = init_expr->data.leaf.name;
                                    init_class = mg_local_class_name(mg, init_ident);
                                }
                                if (init_class) {
                                    type_kind_t unbox_to = get_primitive_for_wrapper(init_class);
                                    if (unbox_to != TYPE_UNKNOWN && var_kind == unbox_to) {
                                        char *internal = class_to_internal_name(init_class);
                                        emit_unboxing(mg, mg->cp, var_kind, internal);
                                        free(internal);
                                    }
                                }
                            }
                            
                            /* Store to local variable with correct opcode for type */
                            mg_emit_store_local(mg, slot, var_kind);
                            
                            /* Update stackmap for this local variable */
                            if (mg->stackmap) {
                                if (is_array_type) {
                                    /* Build proper array descriptor like [I, [[Ljava/lang/String; */
                                    char arr_desc[256];
                                    int i;
                                    for (i = 0; i < array_dims && i < 250; i++) {
                                        arr_desc[i] = '[';
                                    }
                                    if (array_elem_kind == TYPE_CLASS && class_name) {
                                        snprintf(arr_desc + i, sizeof(arr_desc) - i, "L%s;", class_name);
                                    } else {
                                        /* Primitive element: map type_kind_t to descriptor char
                                         * TYPE_VOID=0='V', TYPE_BOOLEAN=1='Z', TYPE_BYTE=2='B', TYPE_CHAR=3='C',
                                         * TYPE_SHORT=4='S', TYPE_INT=5='I', TYPE_LONG=6='J', TYPE_FLOAT=7='F', TYPE_DOUBLE=8='D' */
                                        static const char prims[] = "VZBCSIJFD";
                                        if (array_elem_kind <= TYPE_DOUBLE) {
                                            arr_desc[i] = prims[array_elem_kind];
                                        } else {
                                            arr_desc[i] = 'I';  /* Default to int */
                                        }
                                        arr_desc[i + 1] = '\0';
                                    }
                                    stackmap_set_local_object(mg->stackmap, slot, mg->cp, arr_desc);
                                } else if (is_ref_type && class_name) {
                                    stackmap_set_local_object(mg->stackmap, slot, mg->cp, class_name);
                                } else {
                                    switch (var_kind) {
                                        case TYPE_LONG:   stackmap_set_local_long(mg->stackmap, slot); break;
                                        case TYPE_DOUBLE: stackmap_set_local_double(mg->stackmap, slot); break;
                                        case TYPE_FLOAT:  stackmap_set_local_float(mg->stackmap, slot); break;
                                        default:          stackmap_set_local_int(mg->stackmap, slot); break;
                                    }
                                }
                            }
                        } else {
                            /* Declaration without initializer (e.g., "String value;").
                             * Track the slot in the stackmap as uninitialized (Top type).
                             * This is important for try-catch blocks where the variable
                             * might be assigned in both try and catch paths - we need to
                             * know the slot exists even before it's initialized. */
                            if (mg->stackmap) {
                                /* For uninitialized locals, just increment locals count.
                                 * The slot will be Top until a value is assigned. */
                                if (slot >= mg->stackmap->current_locals_count) {
                                    stackmap_set_local(mg->stackmap, slot, vtype_top());
                                }
                            }
                        }
                    }
                    children = children->next;
                }
                return true;
            }
        
        case AST_IF_STMT:
            {
                /* Children: condition, then_stmt, [else_stmt] */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                ast_node_t *condition = (ast_node_t *)children->data;
                ast_node_t *then_stmt = children->next ? (ast_node_t *)children->next->data : NULL;
                ast_node_t *else_stmt = (children->next && children->next->next) ?
                                        (ast_node_t *)children->next->next->data : NULL;
                
                /* Check for pattern matching instanceof (Java 16+)
                 * If condition is: obj instanceof Type patternVar
                 * We need to generate the pattern variable binding after the branch */
                ast_node_t *pattern_var = NULL;
                ast_node_t *pattern_type_node = NULL;
                ast_node_t *pattern_source = NULL;
                if (condition->type == AST_INSTANCEOF_EXPR) {
                    slist_t *inst_children = condition->data.node.children;
                    if (inst_children && inst_children->next && inst_children->next->next) {
                        pattern_source = (ast_node_t *)inst_children->data;
                        pattern_type_node = (ast_node_t *)inst_children->next->data;
                        pattern_var = (ast_node_t *)inst_children->next->next->data;
                    }
                }
                
                /* Generate condition */
                if (!codegen_expr(mg, condition, mg->cp)) {
                    return false;
                }
                
                /* Auto-unbox Boolean to boolean for if condition */
                if (condition->sem_type && condition->sem_type->kind == TYPE_CLASS &&
                    condition->sem_type->data.class_type.name &&
                    strcmp(condition->sem_type->data.class_type.name, "java.lang.Boolean") == 0) {
                    uint16_t unbox_ref = cp_add_methodref(mg->cp,
                        "java/lang/Boolean", "booleanValue", "()Z");
                    bc_emit(mg->code, OP_INVOKEVIRTUAL);
                    bc_emit_u2(mg->code, unbox_ref);
                    /* Stack stays same size (Boolean -> int) */
                }
                
                /* Save position for branch offset patching */
                size_t branch_pos = mg->code->length;
                
                /* Emit: ifeq else_label (branch if false/zero) */
                bc_emit(mg->code, OP_IFEQ);
                bc_emit_u2(mg->code, 0);  /* Placeholder - will patch */
                mg_pop_typed(mg, 1);  /* Condition consumed */
                
                /* Save stackmap state before the then block - locals allocated inside
                 * the then block (including pattern variables) should not be visible
                 * at join points. */
                stackmap_state_t *pre_then_state = NULL;
                uint16_t pre_then_slot = mg->next_slot;
                uint16_t pre_then_locals_count = 0;
                if (mg->stackmap) {
                    pre_then_state = stackmap_save_state(mg->stackmap);
                    pre_then_locals_count = mg_save_locals_count(mg);
                }
                
                /* Handle pattern variable binding for instanceof pattern matching */
                if (pattern_var && pattern_type_node) {
                    /* Re-evaluate the source expression to get the object */
                    if (!codegen_expr(mg, pattern_source, mg->cp)) {
                        return false;
                    }
                    
                    /* Get the class name for checkcast */
                    const char *class_name = NULL;
                    if (pattern_type_node->type == AST_CLASS_TYPE) {
                        class_name = pattern_type_node->data.node.name;
                    }
                    
                    if (class_name) {
                        /* Emit checkcast */
                        const char *resolved = resolve_java_lang_class(class_name);
                        char *internal_name = class_to_internal_name(resolved);
                        uint16_t class_index = cp_add_class(mg->cp, internal_name);
                        bc_emit(mg->code, OP_CHECKCAST);
                        bc_emit_u2(mg->code, class_index);
                        
                        /* Allocate local variable slot for pattern variable */
                        const char *var_name = pattern_var->data.leaf.name;
                        type_t *var_type = pattern_var->sem_type;
                        if (!var_type) {
                            /* Create class type if sem_type not set */
                            var_type = type_new_class(internal_name);
                        }
                        uint16_t slot = mg_allocate_local(mg, var_name, var_type);
                        
                        /* Emit astore to save the cast object */
                        if (slot <= 3) {
                            bc_emit(mg->code, OP_ASTORE_0 + slot);
                        } else if (slot <= 255) {
                            bc_emit(mg->code, OP_ASTORE);
                            bc_emit_u1(mg->code, slot);
                        } else {
                            bc_emit(mg->code, OP_WIDE);
                            bc_emit(mg->code, OP_ASTORE);
                            bc_emit_u2(mg->code, slot);
                        }
                        mg_pop_typed(mg, 1);  /* Object consumed by astore */
                        
                        free(internal_name);
                    }
                }
                
                /* Generate then branch */
                if (then_stmt && !codegen_statement(mg, then_stmt)) {
                    return false;
                }
                
                if (else_stmt) {
                    /* Check if then block ended with a return (don't need goto) */
                    bool then_ends_with_return = false;
                    if (mg->code->length > 0) {
                        uint8_t last_op = mg->code->code[mg->code->length - 1];
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW) {
                            then_ends_with_return = true;
                        }
                    }
                    
                    size_t goto_pos = 0;
                    if (!then_ends_with_return) {
                        /* Save position for goto past else */
                        goto_pos = mg->code->length;
                        bc_emit(mg->code, OP_GOTO);
                        bc_emit_u2(mg->code, 0);  /* Placeholder */
                    }
                    
                    /* Patch the branch to else */
                    int16_t else_offset = (int16_t)(mg->code->length - branch_pos);
                    mg->code->code[branch_pos + 1] = (else_offset >> 8) & 0xFF;
                    mg->code->code[branch_pos + 2] = else_offset & 0xFF;
                    
                    /* Restore stackmap state to before then block for else branch.
                     * Locals allocated in the then block should not be visible in else. */
                    if (pre_then_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, pre_then_state);
                        mg_restore_locals_count(mg, pre_then_locals_count);
                        mg->next_slot = pre_then_slot;
                    }
                    
                    /* Record frame at else branch target */
                    mg_record_frame(mg);
                    
                    /* Generate else branch */
                    if (!codegen_statement(mg, else_stmt)) {
                        stackmap_state_free(pre_then_state);
                        return false;
                    }
                    
                    /* Check if else block ended with a return */
                    bool else_ends_with_return = false;
                    if (mg->code->length > 0) {
                        uint8_t last_op = mg->code->code[mg->code->length - 1];
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW) {
                            else_ends_with_return = true;
                        }
                    }
                    
                    /* Patch goto to end (only if we emitted one) */
                    if (!then_ends_with_return) {
                        int16_t end_offset = (int16_t)(mg->code->length - goto_pos);
                        mg->code->code[goto_pos + 1] = (end_offset >> 8) & 0xFF;
                        mg->code->code[goto_pos + 2] = end_offset & 0xFF;
                        
                        /* Restore stackmap state for join point - locals allocated in
                         * either branch should not be in scope after the if-else */
                        if (pre_then_state && mg->stackmap) {
                            stackmap_restore_state(mg->stackmap, pre_then_state);
                            mg_restore_locals_count(mg, pre_then_locals_count);
                            mg->next_slot = pre_then_slot;
                        }
                        
                        /* Record frame at end of if-else (join point) */
                        mg_record_frame(mg);
                    }
                    
                    /* If both branches terminate, the if-else terminates
                     * Otherwise, reset last_opcode */
                    if (then_ends_with_return && else_ends_with_return) {
                        mg->last_opcode = OP_IRETURN;  /* Mark as terminating */
                    } else {
                        mg->last_opcode = 0;
                    }
                } else {
                    /* Check if then block ended with a return/throw */
                    bool then_ends_with_return = false;
                    if (mg->code->length > 0) {
                        uint8_t last_op = mg->code->code[mg->code->length - 1];
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW) {
                            then_ends_with_return = true;
                        }
                    }
                    
                    /* Patch branch to end (no else) */
                    int16_t end_offset = (int16_t)(mg->code->length - branch_pos);
                    mg->code->code[branch_pos + 1] = (end_offset >> 8) & 0xFF;
                    mg->code->code[branch_pos + 2] = end_offset & 0xFF;
                    
                    /* Restore stackmap state to before then block for fall-through path.
                     * Locals allocated inside the then block should not be visible at join point. */
                    if (pre_then_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, pre_then_state);
                        mg_restore_locals_count(mg, pre_then_locals_count);
                        mg->next_slot = pre_then_slot;
                    }
                    
                    /* Record frame at end of if (branch target) for the if-false path.
                     * Even if then-block ends with return, the if-false path still exists
                     * and may branch to this location. */
                    mg_record_frame(mg);
                    
                    /* For if-without-else, there's always a code path (the false branch)
                     * that doesn't return, so we must NOT mark this as terminating. */
                    mg->last_opcode = 0;
                }
                
                /* Free saved state */
                stackmap_state_free(pre_then_state);
                
                return true;
            }
        
        case AST_WHILE_STMT:
            {
                /* Children: condition, body */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                ast_node_t *condition = (ast_node_t *)children->data;
                ast_node_t *body = children->next ? (ast_node_t *)children->next->data : NULL;
                
                /* Save locals count before entering loop body.
                 * Variables declared inside the loop shouldn't appear in the
                 * stackmap frame at loop_start (they're not defined on first entry). */
                uint16_t saved_locals_count = mg_save_locals_count(mg);
                
                /* loop_start: (continue target) */
                size_t loop_start = mg->code->length;
                
                /* Record frame at loop start (back-edge target) */
                mg_record_frame(mg);
                
                /* Push loop context for break/continue */
                mg_push_loop(mg, loop_start, mg->pending_label);
                
                /* Generate condition */
                if (!codegen_expr(mg, condition, mg->cp)) {
                    return false;
                }
                
                /* ifeq loop_end (exit if condition false) */
                size_t branch_pos = mg->code->length;
                bc_emit(mg->code, OP_IFEQ);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);  /* Consume condition */
                
                /* Generate body */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }
                
                /* Restore locals count before goto back to loop_start.
                 * This ensures the frame at loop_start doesn't include loop-local variables. */
                mg_restore_locals_count(mg, saved_locals_count);
                
                /* goto loop_start */
                int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, back_offset);
                
                /* Patch forward branch to here (loop_end) */
                size_t loop_end = mg->code->length;
                int16_t end_offset = (int16_t)(loop_end - branch_pos);
                mg->code->code[branch_pos + 1] = (end_offset >> 8) & 0xFF;
                mg->code->code[branch_pos + 2] = end_offset & 0xFF;
                
                /* Record frame at loop end (break target) */
                mg_record_frame(mg);
                
                /* Pop loop context and patch breaks */
                mg_pop_loop(mg, loop_end);
                
                /* Reset last_opcode - loop bodies don't guarantee method termination */
                mg->last_opcode = 0;
                
                return true;
            }
        
        case AST_DO_STMT:
            {
                /* Children: body, condition */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                ast_node_t *body = (ast_node_t *)children->data;
                ast_node_t *condition = children->next ? (ast_node_t *)children->next->data : NULL;
                
                /* Save locals count before entering loop body.
                 * Variables declared inside the body shouldn't appear in the
                 * stackmap frame at loop_start on subsequent iterations. */
                uint16_t saved_locals_count = mg_save_locals_count(mg);
                
                /* loop_start: */
                size_t loop_start = mg->code->length;
                
                /* Record frame at loop start (back-edge target) */
                mg_record_frame(mg);
                
                /* continue_target is after body, before condition check */
                /* We'll set it after body generation */
                mg_push_loop(mg, loop_start, mg->pending_label);  /* Temp value, updated below */
                
                /* Generate body first */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }
                
                /* Update continue target to condition check point */
                if (mg->loop_stack) {
                    ((loop_context_t *)mg->loop_stack->data)->continue_target = mg->code->length;
                }
                
                /* Restore locals count before condition check (which branches back to loop_start) */
                mg_restore_locals_count(mg, saved_locals_count);
                
                /* Generate condition */
                if (condition) {
                    if (!codegen_expr(mg, condition, mg->cp)) {
                        return false;
                    }
                    
                    /* ifne loop_start (continue if condition true) */
                    int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                    bc_emit(mg->code, OP_IFNE);
                    bc_emit_u2(mg->code, back_offset);
                    mg_pop_typed(mg, 1);
                }
                
                /* Record frame at loop end (break target) */
                mg_record_frame(mg);
                
                /* Pop loop context and patch breaks */
                mg_pop_loop(mg, mg->code->length);
                
                /* Reset last_opcode - loop bodies don't guarantee method termination */
                mg->last_opcode = 0;
                
                return true;
            }
        
        case AST_FOR_STMT:
            {
                /* Children: init, condition, update, body */
                /* Note: any of init/condition/update can be NULL (empty) */
                slist_t *children = stmt->data.node.children;
                
                ast_node_t *init = NULL;
                ast_node_t *condition = NULL;
                ast_node_t *update = NULL;
                ast_node_t *body = NULL;
                
                /* Parse children - they should be in order */
                int idx = 0;
                while (children) {
                    ast_node_t *child = (ast_node_t *)children->data;
                    switch (idx) {
                        case 0: init = child; break;
                        case 1: condition = child; break;
                        case 2: update = child; break;
                        case 3: body = child; break;
                    }
                    idx++;
                    children = children->next;
                }
                
                /* Generate initializer */
                if (init) {
                    if (init->type == AST_VAR_DECL) {
                        if (!codegen_statement(mg, init)) {
                            return false;
                        }
                    } else {
                        if (!codegen_expr(mg, init, mg->cp)) {
                            return false;
                        }
                        /* Pop result of init expression */
                        bc_emit(mg->code, OP_POP);
                        mg_pop_typed(mg, 1);
                    }
                }
                
                /* Save locals count after init but before body.
                 * Variables declared in the loop body shouldn't appear in the
                 * stackmap frame at loop_start (they're not defined on first entry). */
                uint16_t saved_locals_count = mg_save_locals_count(mg);
                
                /* loop_start: (condition check) */
                size_t loop_start = mg->code->length;
                
                /* Record frame at loop start (back-edge target) */
                mg_record_frame(mg);
                
                size_t branch_pos = 0;
                if (condition) {
                    /* Generate condition */
                    if (!codegen_expr(mg, condition, mg->cp)) {
                        return false;
                    }
                    
                    /* ifeq loop_end */
                    branch_pos = mg->code->length;
                    bc_emit(mg->code, OP_IFEQ);
                    bc_emit_u2(mg->code, 0);  /* Placeholder */
                    mg_pop_typed(mg, 1);
                }
                
                /* Continue target is where update starts (or loop_start if no update) */
                /* We need to know where update will be, so use a placeholder */
                size_t continue_target = loop_start;  /* Will be updated */
                mg_push_loop(mg, continue_target, mg->pending_label);
                
                /* Generate body */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }
                
                /* Update continue target to here (before update expression) */
                if (mg->loop_stack) {
                    ((loop_context_t *)mg->loop_stack->data)->continue_target = mg->code->length;
                }
                
                /* Generate update */
                if (update) {
                    if (!codegen_expr(mg, update, mg->cp)) {
                        return false;
                    }
                    /* Pop update result */
                    bc_emit(mg->code, OP_POP);
                    mg_pop_typed(mg, 1);
                }
                
                /* Restore locals count before goto back to loop_start.
                 * This ensures the frame at loop_start doesn't include body-local variables. */
                mg_restore_locals_count(mg, saved_locals_count);
                
                /* goto loop_start */
                int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, back_offset);
                
                /* loop_end: */
                size_t loop_end = mg->code->length;
                
                /* Record frame at loop end (break target) */
                mg_record_frame(mg);
                
                /* Patch forward branch if we have a condition */
                if (condition) {
                    int16_t end_offset = (int16_t)(loop_end - branch_pos);
                    mg->code->code[branch_pos + 1] = (end_offset >> 8) & 0xFF;
                    mg->code->code[branch_pos + 2] = end_offset & 0xFF;
                }
                
                /* Pop loop context and patch breaks */
                mg_pop_loop(mg, loop_end);
                
                /* Reset last_opcode - loop bodies don't guarantee method termination */
                mg->last_opcode = 0;
                
                return true;
            }
        
        case AST_BREAK_STMT:
            {
                /* Jump to end of current loop or labeled statement */
                const char *label = stmt->data.node.name;
                
                if (!mg->loop_stack) {
                    fprintf(stderr, "codegen: break outside of loop\n");
                    return false;
                }
                
                /* Find the target loop context */
                loop_context_t *target_ctx = NULL;
                if (label) {
                    /* Labeled break - find the matching context */
                    target_ctx = mg_find_loop_by_label(mg, label);
                    if (!target_ctx) {
                        fprintf(stderr, "codegen: break label '%s' not found\n", label);
                        return false;
                    }
                } else {
                    /* Unlabeled break - use innermost loop */
                    target_ctx = (loop_context_t *)mg->loop_stack->data;
                }
                
                /* Emit goto with placeholder offset */
                size_t break_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Will be patched by mg_pop_loop */
                
                /* Register this break for patching */
                mg_add_break_to_context(target_ctx, break_pos);
                
                return true;
            }
        
        case AST_ENHANCED_FOR_STMT:
            {
                /* Children: type, variable, iterable, body */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                /* Get AST nodes */
                ast_node_t *type_node = (ast_node_t *)children->data;
                ast_node_t *var_node = children->next ? (ast_node_t *)children->next->data : NULL;
                ast_node_t *iterable = (children->next && children->next->next) ?
                                       (ast_node_t *)children->next->next->data : NULL;
                ast_node_t *body = (children->next && children->next->next && children->next->next->next) ?
                                   (ast_node_t *)children->next->next->next->data : NULL;
                
                if (!var_node || !iterable) {
                    fprintf(stderr, "codegen: malformed enhanced for loop\n");
                    return false;
                }
                
                const char *var_name = var_node->data.leaf.name;
                
                /* Check if iterable is an array type.
                 * For a parameter like int[] arr, we track it in local_arrays.
                 * For method calls returning collections, assume Iterable.
                 */
                bool is_array = false;
                
                /* First check semantic type if available */
                if (iterable->sem_type && iterable->sem_type->kind == TYPE_ARRAY) {
                    is_array = true;
                } else if (iterable->type == AST_IDENTIFIER) {
                    /* Check if this identifier was marked as an array type */
                    const char *iter_name = iterable->data.leaf.name;
                    if (mg_local_is_array(mg, iter_name)) {
                        is_array = true;
                    }
                    /* Also check if kind is TYPE_ARRAY */
                    if (mg_get_local_type(mg, iter_name) == TYPE_ARRAY) {
                        is_array = true;
                    }
                } else if (iterable->type == AST_METHOD_CALL) {
                    /* Method call - check return type if available */
                    if (iterable->sem_type && iterable->sem_type->kind == TYPE_ARRAY) {
                        is_array = true;
                    }
                    /* Otherwise assume Iterable */
                } else if (iterable->type == AST_NEW_ARRAY) {
                    is_array = true;
                }
                
                /* Determine loop variable type */
                type_kind_t var_kind = TYPE_INT;  /* Default */
                bool var_is_ref = false;
                if (type_node->type == AST_CLASS_TYPE) {
                    var_is_ref = true;
                    var_kind = TYPE_CLASS;
                } else if (type_node->type == AST_ARRAY_TYPE) {
                    var_is_ref = true;
                    var_kind = TYPE_ARRAY;
                } else if (type_node->type == AST_PRIMITIVE_TYPE) {
                    const char *prim_name = type_node->data.leaf.name;
                    if (prim_name) {
                        if (strcmp(prim_name, "long") == 0) var_kind = TYPE_LONG;
                        else if (strcmp(prim_name, "float") == 0) var_kind = TYPE_FLOAT;
                        else if (strcmp(prim_name, "double") == 0) var_kind = TYPE_DOUBLE;
                        else if (strcmp(prim_name, "byte") == 0) var_kind = TYPE_BYTE;
                        else if (strcmp(prim_name, "short") == 0) var_kind = TYPE_SHORT;
                        else if (strcmp(prim_name, "char") == 0) var_kind = TYPE_CHAR;
                        else if (strcmp(prim_name, "boolean") == 0) var_kind = TYPE_BOOLEAN;
                    }
                }
                
                if (is_array) {
                    /* ============================================
                     * ARRAY iteration: for (T var : arr)
                     * ============================================
                     *   T[] __arr = arr;
                     *   int __idx = 0;
                     *   while (__idx < __arr.length) {
                     *       T var = __arr[__idx];
                     *       body
                     *       __idx++;
                     *   }
                     */
                    uint16_t arr_slot = mg->next_slot++;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    uint16_t idx_slot = mg->next_slot++;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    
                    /* Allocate loop variable with proper type */
                    int var_size = (var_kind == TYPE_LONG || var_kind == TYPE_DOUBLE) ? 2 : 1;
                    uint16_t var_slot = mg->next_slot;
                    mg->next_slot += var_size;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    
                    /* Create local variable info for loop variable */
                    local_var_info_t *loop_var_info = local_var_info_new(var_slot, var_kind);
                    if (loop_var_info) {
                        loop_var_info->is_ref = var_is_ref;
                        loop_var_info->is_array = (var_kind == TYPE_ARRAY);
                        hashtable_insert(mg->locals, var_name, loop_var_info);
                    }
                    
                    /* __arr = iterable */
                    if (!codegen_expr(mg, iterable, mg->cp)) {
                        return false;
                    }
                    mg_emit_store_local(mg, arr_slot, TYPE_ARRAY);
                    
                    /* Update stackmap for array slot using actual array type */
                    if (mg->stackmap) {
                        if (iterable->sem_type && iterable->sem_type->kind == TYPE_ARRAY) {
                            /* Use the actual array type from semantic analysis */
                            char *arr_desc = type_to_descriptor(iterable->sem_type);
                            stackmap_set_local_object(mg->stackmap, arr_slot, mg->cp, arr_desc);
                            free(arr_desc);
                        } else {
                            /* Fallback to generic Object array */
                        stackmap_set_local_object(mg->stackmap, arr_slot, mg->cp, "[Ljava/lang/Object;");
                        }
                    }
                    
                    /* __idx = 0 */
                    bc_emit(mg->code, OP_ICONST_0);
                    mg_push_int(mg);  /* Index is an integer */
                    mg_emit_store_local(mg, idx_slot, TYPE_INT);
                    
                    /* Update stackmap for index slot */
                    if (mg->stackmap) {
                        stackmap_set_local_int(mg->stackmap, idx_slot);
                    }
                    
                    /* Save stackmap state before loop starts - used for loop exit frame.
                     * Locals allocated inside the loop body should not be in the exit frame
                     * since they're not live after the loop. */
                    stackmap_state_t *arr_loop_entry_state = NULL;
                    if (mg->stackmap) {
                        arr_loop_entry_state = stackmap_save_state(mg->stackmap);
                    }
                    
                    /* loop_start: */
                    size_t loop_start = mg->code->length;
                    
                    /* Record frame at loop start (back-edge target) 
                     * Note: We don't include the loop variable in the frame because
                     * it's only initialized inside the loop body */
                    mg_record_frame(mg);
                    
                    mg_push_loop(mg, loop_start, mg->pending_label);
                    
                    /* if (__idx >= __arr.length) goto loop_end */
                    mg_emit_load_local(mg, idx_slot, TYPE_INT);
                    mg_emit_load_local(mg, arr_slot, TYPE_ARRAY);
                    bc_emit(mg->code, OP_ARRAYLENGTH);
                    
                    size_t branch_pos = mg->code->length;
                    bc_emit(mg->code, OP_IF_ICMPGE);
                    bc_emit_u2(mg->code, 0);
                    mg_pop_typed(mg, 2);
                    
                    /* var = __arr[__idx] */
                    mg_emit_load_local(mg, arr_slot, TYPE_ARRAY);
                    mg_emit_load_local(mg, idx_slot, TYPE_INT);
                    
                    /* Use appropriate array load opcode */
                    switch (var_kind) {
                        case TYPE_LONG:
                            bc_emit(mg->code, OP_LALOAD);
                            mg_pop_typed(mg, 2);  /* Consumes arrayref, index */
                            mg_push(mg, 2); /* Pushes long (2 slots) */
                            break;
                        case TYPE_DOUBLE:
                            bc_emit(mg->code, OP_DALOAD);
                            mg_pop_typed(mg, 2);
                            mg_push(mg, 2);
                            break;
                        case TYPE_FLOAT:
                            bc_emit(mg->code, OP_FALOAD);
                            mg_pop_typed(mg, 1);
                            break;
                        case TYPE_BYTE:
                        case TYPE_BOOLEAN:
                            bc_emit(mg->code, OP_BALOAD);
                            mg_pop_typed(mg, 1);
                            break;
                        case TYPE_CHAR:
                            bc_emit(mg->code, OP_CALOAD);
                            mg_pop_typed(mg, 1);
                            break;
                        case TYPE_SHORT:
                            bc_emit(mg->code, OP_SALOAD);
                            mg_pop_typed(mg, 1);
                            break;
                        case TYPE_CLASS:
                        case TYPE_ARRAY:
                            bc_emit(mg->code, OP_AALOAD);
                            mg_pop_typed(mg, 1);
                            /* Add checkcast for class types (aaload returns Object) */
                            if (var_kind == TYPE_CLASS && type_node && type_node->type == AST_CLASS_TYPE) {
                                const char *type_name = type_node->data.node.name;
                                if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                                    type_name = type_node->sem_type->data.class_type.name;
                                }
                                if (type_name) {
                                    const char *internal = class_to_internal_name(type_name);
                                    uint16_t class_idx = cp_add_class(mg->cp, internal);
                                    bc_emit(mg->code, OP_CHECKCAST);
                                    bc_emit_u2(mg->code, class_idx);
                                }
                            }
                            break;
                        default:
                            bc_emit(mg->code, OP_IALOAD);
                            mg_pop_typed(mg, 1);
                            break;
                    }
                    
                    /* Store to loop variable */
                    mg_emit_store_local(mg, var_slot, var_kind);
                    
                    /* Update stackmap for loop variable */
                    if (mg->stackmap) {
                        switch (var_kind) {
                            case TYPE_LONG:
                                stackmap_set_local_long(mg->stackmap, var_slot);
                                break;
                            case TYPE_DOUBLE:
                                stackmap_set_local_double(mg->stackmap, var_slot);
                                break;
                            case TYPE_FLOAT:
                                stackmap_set_local_float(mg->stackmap, var_slot);
                                break;
                            case TYPE_CLASS:
                            case TYPE_ARRAY:
                                {
                                    /* Use the actual element type from the type node */
                                    const char *type_name = "java/lang/Object";
                                    if (type_node && type_node->sem_type && 
                                        type_node->sem_type->kind == TYPE_CLASS) {
                                        type_name = type_node->sem_type->data.class_type.name;
                                    } else if (type_node && type_node->type == AST_CLASS_TYPE) {
                                        type_name = type_node->data.node.name;
                                    }
                                    char *internal = class_to_internal_name(type_name);
                                    stackmap_set_local_object(mg->stackmap, var_slot, mg->cp, internal);
                                    free(internal);
                                }
                                break;
                            default:
                                /* int, boolean, byte, char, short */
                                stackmap_set_local_int(mg->stackmap, var_slot);
                                break;
                        }
                    }
                    
                    /* body */
                    if (body && !codegen_statement(mg, body)) {
                        return false;
                    }
                    
                    /* Update continue target */
                    if (mg->loop_stack) {
                        ((loop_context_t *)mg->loop_stack->data)->continue_target = mg->code->length;
                    }
                    
                    /* __idx++ */
                    bc_emit(mg->code, OP_IINC);
                    bc_emit_u1(mg->code, (uint8_t)idx_slot);
                    bc_emit_s1(mg->code, 1);
                    
                    /* goto loop_start */
                    int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, back_offset);
                    
                    /* loop_end: Patch branch */
                    size_t loop_end = mg->code->length;
                    int16_t end_offset = (int16_t)(loop_end - branch_pos);
                    mg->code->code[branch_pos + 1] = (end_offset >> 8) & 0xFF;
                    mg->code->code[branch_pos + 2] = end_offset & 0xFF;
                    
                    /* Record frame at loop end (break/exit target).
                     * Restore to loop entry state first - locals allocated inside the 
                     * loop body are not live at the exit point. */
                    if (arr_loop_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, arr_loop_entry_state);
                    }
                    mg_record_frame(mg);
                    stackmap_state_free(arr_loop_entry_state);
                    
                    mg_pop_loop(mg, loop_end);
                } else {
                    /* ============================================
                     * ITERABLE iteration: for (T var : collection)
                     * ============================================
                     *   Iterator __iter = collection.iterator();
                     *   while (__iter.hasNext()) {
                     *       T var = (T) __iter.next();
                     *       body
                     *   }
                     */
                    uint16_t iter_slot = mg->next_slot++;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    
                    /* Allocate loop variable - Iterable always returns reference types */
                    uint16_t var_slot = mg->next_slot++;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    
                    /* Create local variable info for Iterable loop variable */
                    local_var_info_t *iter_var_info = local_var_info_new(var_slot, var_kind);
                    if (iter_var_info) {
                        iter_var_info->is_ref = var_is_ref;
                        hashtable_insert(mg->locals, var_name, iter_var_info);
                    }
                    
                    /* __iter = collection.iterator() */
                    if (!codegen_expr(mg, iterable, mg->cp)) {
                        return false;
                    }
                    
                    /* invokeinterface java/lang/Iterable.iterator:()Ljava/util/Iterator; */
                    uint16_t iterator_ref = cp_add_interface_methodref(mg->cp, 
                        "java/lang/Iterable", "iterator", "()Ljava/util/Iterator;");
                    bc_emit(mg->code, OP_INVOKEINTERFACE);
                    bc_emit_u2(mg->code, iterator_ref);
                    bc_emit_u1(mg->code, 1);  /* count: 1 argument (this) */
                    bc_emit_u1(mg->code, 0);  /* must be zero */
                    /* Stack: collection -> iterator (no net change) */
                    
                    mg_emit_store_local(mg, iter_slot, TYPE_CLASS);
                    
                    /* Update stackmap for iterator slot (it's live across the loop) */
                    if (mg->stackmap) {
                        stackmap_set_local_object(mg->stackmap, iter_slot, mg->cp, "java/util/Iterator");
                    }
                    
                    /* Save stackmap state before loop starts - used for loop exit frame.
                     * Locals allocated inside the loop body should not be in the exit frame
                     * since they're not live after the loop. */
                    stackmap_state_t *loop_entry_state = NULL;
                    if (mg->stackmap) {
                        loop_entry_state = stackmap_save_state(mg->stackmap);
                    }
                    
                    /* loop_start: */
                    size_t loop_start = mg->code->length;
                    
                    /* Record frame at loop start (back-edge target) 
                     * Note: We don't include the loop variable in the frame because
                     * it's only initialized inside the loop body */
                    mg_record_frame(mg);
                    
                    mg_push_loop(mg, loop_start, mg->pending_label);
                    
                    /* if (!__iter.hasNext()) goto loop_end */
                    mg_emit_load_local(mg, iter_slot, TYPE_CLASS);
                    
                    /* invokeinterface java/util/Iterator.hasNext:()Z */
                    uint16_t hasNext_ref = cp_add_interface_methodref(mg->cp,
                        "java/util/Iterator", "hasNext", "()Z");
                    bc_emit(mg->code, OP_INVOKEINTERFACE);
                    bc_emit_u2(mg->code, hasNext_ref);
                    bc_emit_u1(mg->code, 1);  /* count */
                    bc_emit_u1(mg->code, 0);
                    /* Stack: iterator -> boolean (no net change) */
                    
                    size_t branch_pos = mg->code->length;
                    bc_emit(mg->code, OP_IFEQ);  /* if false, exit loop */
                    bc_emit_u2(mg->code, 0);
                    mg_pop_typed(mg, 1);
                    
                    /* var = (T) __iter.next() */
                    mg_emit_load_local(mg, iter_slot, TYPE_CLASS);
                    
                    /* invokeinterface java/util/Iterator.next:()Ljava/lang/Object; */
                    uint16_t next_ref = cp_add_interface_methodref(mg->cp,
                        "java/util/Iterator", "next", "()Ljava/lang/Object;");
                    bc_emit(mg->code, OP_INVOKEINTERFACE);
                    bc_emit_u2(mg->code, next_ref);
                    bc_emit_u1(mg->code, 1);  /* count */
                    bc_emit_u1(mg->code, 0);
                    /* Stack: iterator -> Object (no net change) */
                    
                    /* Add checkcast to the loop variable type (Iterator.next() returns Object) */
                    if (type_node && type_node->type == AST_CLASS_TYPE) {
                        const char *type_name = type_node->data.node.name;
                        /* Use qualified name from semantic type if available */
                        if (type_node->sem_type && type_node->sem_type->kind == TYPE_CLASS) {
                            type_name = type_node->sem_type->data.class_type.name;
                        }
                        if (type_name) {
                            const char *internal = class_to_internal_name(type_name);
                            uint16_t class_idx = cp_add_class(mg->cp, internal);
                            bc_emit(mg->code, OP_CHECKCAST);
                            bc_emit_u2(mg->code, class_idx);
                            /* Stack unchanged (still 1 reference) */
                        }
                    }
                    
                    /* Store to loop variable (reference type from Iterable) */
                    mg_emit_store_local(mg, var_slot, var_kind);
                    
                    /* Update stackmap for loop variable */
                    if (mg->stackmap) {
                        /* For Iterable, loop variable is always reference type */
                        const char *type_name = "java/lang/Object";
                        if (type_node && type_node->sem_type && 
                            type_node->sem_type->kind == TYPE_CLASS) {
                            type_name = type_node->sem_type->data.class_type.name;
                        } else if (type_node && type_node->type == AST_CLASS_TYPE) {
                            type_name = type_node->data.node.name;
                        }
                        char *internal = class_to_internal_name(type_name);
                        stackmap_set_local_object(mg->stackmap, var_slot, mg->cp, internal);
                        free(internal);
                    }
                    
                    /* body */
                    if (body && !codegen_statement(mg, body)) {
                        return false;
                    }
                    
                    /* Update continue target */
                    if (mg->loop_stack) {
                        ((loop_context_t *)mg->loop_stack->data)->continue_target = mg->code->length;
                    }
                    
                    /* goto loop_start */
                    int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, back_offset);
                    
                    /* loop_end: Patch branch */
                    size_t loop_end = mg->code->length;
                    int16_t end_offset = (int16_t)(loop_end - branch_pos);
                    mg->code->code[branch_pos + 1] = (end_offset >> 8) & 0xFF;
                    mg->code->code[branch_pos + 2] = end_offset & 0xFF;
                    
                    /* Record frame at loop end (break/exit target).
                     * Restore to loop entry state first - locals allocated inside the 
                     * loop body (loop variable and any inner variables) are not live
                     * at the exit point. */
                    if (loop_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, loop_entry_state);
                    }
                    mg_record_frame(mg);
                    stackmap_state_free(loop_entry_state);
                    
                    mg_pop_loop(mg, loop_end);
                }
                
                /* Reset last_opcode - loop bodies don't guarantee method termination */
                mg->last_opcode = 0;
                
                return true;
            }
        
        case AST_CONTINUE_STMT:
            {
                /* Jump to continue target of current loop or labeled loop */
                const char *label = stmt->data.node.name;
                
                if (!mg->loop_stack) {
                    fprintf(stderr, "codegen: continue outside of loop\n");
                    return false;
                }
                
                /* Find the target loop context */
                loop_context_t *target_ctx = NULL;
                if (label) {
                    /* Labeled continue - find the matching context */
                    target_ctx = mg_find_loop_by_label(mg, label);
                    if (!target_ctx) {
                        fprintf(stderr, "codegen: continue label '%s' not found\n", label);
                        return false;
                    }
                    /* Verify it's a loop (has a continue target) */
                    if (target_ctx->continue_target == 0) {
                        fprintf(stderr, "codegen: continue label '%s' does not refer to a loop\n", label);
                        return false;
                    }
                } else {
                    /* Unlabeled continue - use innermost loop */
                    target_ctx = (loop_context_t *)mg->loop_stack->data;
                }
                
                int16_t offset = (int16_t)(target_ctx->continue_target - mg->code->length);
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, offset);
                
                return true;
            }
        
        case AST_SWITCH_STMT:
            {
                /* Children: selector_expr, case_label1, case_label2, ... */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                /* Generate selector expression */
                ast_node_t *selector = (ast_node_t *)children->data;
                if (!codegen_expr(mg, selector, mg->cp)) {
                    return false;
                }
                
                /* First pass: count total case values (not labels, but actual values) */
                int num_cases = 0;
                for (slist_t *node = children->next; node; node = node->next) {
                    ast_node_t *case_label = (ast_node_t *)node->data;
                    if (case_label->type == AST_CASE_LABEL) {
                        if (!(case_label->data.node.name && 
                              strcmp(case_label->data.node.name, "default") == 0)) {
                            num_cases++;
                        }
                    }
                }
                
                /* Check if this is an enum switch (selector has sem_type of enum) */
                bool is_enum_switch = (selector->sem_type && 
                                       selector->sem_type->kind == TYPE_CLASS &&
                                       selector->sem_type->data.class_type.symbol &&
                                       selector->sem_type->data.class_type.symbol->kind == SYM_ENUM);
                
                /* Check if this is a String switch */
                bool is_string_switch = (selector->sem_type && 
                                         selector->sem_type->kind == TYPE_CLASS &&
                                         selector->sem_type->data.class_type.name &&
                                         strcmp(selector->sem_type->data.class_type.name, "java.lang.String") == 0);
                
                /* For String switch, use hashCode-based dispatch */
                if (is_string_switch) {
                    return codegen_string_switch(mg, children, num_cases);
                }
                
                /* For enum switch, call ordinal() on selector BEFORE lookupswitch */
                if (is_enum_switch) {
                    uint16_t methodref = cp_add_methodref(mg->cp, "java/lang/Enum", "ordinal", "()I");
                    bc_emit(mg->code, OP_INVOKEVIRTUAL);
                    bc_emit_u2(mg->code, methodref);
                    /* Stack unchanged: popped enum ref, pushed int ordinal */
                }
                
                /* Save position of lookupswitch */
                size_t switch_pos = mg->code->length;
                bc_emit(mg->code, OP_LOOKUPSWITCH);
                mg_pop_typed(mg, 1);
                
                /* Pad to 4-byte alignment */
                while ((mg->code->length) % 4 != 0) {
                    bc_emit_u1(mg->code, 0);
                }
                
                /* Placeholder for default offset */
                size_t default_offset_pos = mg->code->length;
                bc_emit_u4(mg->code, 0);
                
                /* Number of pairs */
                bc_emit_u4(mg->code, (uint32_t)num_cases);
                
                /* Allocate arrays: case value -> offset position in bytecode */
                int32_t *case_values = calloc(num_cases, sizeof(int32_t));
                size_t *case_offset_positions = calloc(num_cases, sizeof(size_t));
                int case_idx = 0;
                
                /* Second pass: collect all case values and their AST node indices */
                int *case_to_ast_idx = calloc(num_cases, sizeof(int));
                int ast_idx = 0;
                case_idx = 0;
                for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
                    ast_node_t *case_label = (ast_node_t *)node->data;
                    if (case_label->type == AST_CASE_LABEL) {
                        if (case_label->data.node.name && 
                            strcmp(case_label->data.node.name, "default") == 0) {
                            /* Skip default */
                        } else {
                            slist_t *case_children = case_label->data.node.children;
                            if (case_children) {
                                ast_node_t *case_expr = (ast_node_t *)case_children->data;
                                if (case_expr->type == AST_LITERAL) {
                                    case_values[case_idx] = (int32_t)case_expr->data.leaf.value.int_val;
                                } else if (case_expr->type == AST_IDENTIFIER && is_enum_switch) {
                                    /* For enum switch, semantic analysis stored the ordinal */
                                    case_values[case_idx] = (int32_t)case_expr->data.leaf.value.int_val;
                                }
                            }
                            case_to_ast_idx[case_idx] = ast_idx;
                            case_idx++;
                        }
                    }
                }
                
                /* Sort cases by value (bubble sort - small n) */
                for (int i = 0; i < num_cases - 1; i++) {
                    for (int j = 0; j < num_cases - i - 1; j++) {
                        if (case_values[j] > case_values[j + 1]) {
                            /* Swap values */
                            int32_t tmp_val = case_values[j];
                            case_values[j] = case_values[j + 1];
                            case_values[j + 1] = tmp_val;
                            /* Swap AST indices */
                            int tmp_idx = case_to_ast_idx[j];
                            case_to_ast_idx[j] = case_to_ast_idx[j + 1];
                            case_to_ast_idx[j + 1] = tmp_idx;
                        }
                    }
                }
                
                /* Emit sorted case match/offset pairs */
                for (case_idx = 0; case_idx < num_cases; case_idx++) {
                    bc_emit_u4(mg->code, (uint32_t)case_values[case_idx]);
                    case_offset_positions[case_idx] = mg->code->length;
                    bc_emit_u4(mg->code, 0);
                }
                
                /* Push switch context for break */
                mg_push_loop(mg, 0, NULL);
                
                /* Build reverse mapping: ast_idx -> sorted_idx */
                int *ast_to_sorted_idx = calloc(num_cases + 1, sizeof(int));
                for (int i = 0; i < num_cases; i++) {
                    ast_to_sorted_idx[case_to_ast_idx[i]] = i;
                }
                
                /* Third pass: generate case bodies */
                size_t default_code_pos = 0;
                ast_idx = 0;
                
                for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
                    ast_node_t *case_label = (ast_node_t *)node->data;
                    if (case_label->type != AST_CASE_LABEL) {
                        continue;
                    }
                    
                    bool is_default = case_label->data.node.name && 
                                     strcmp(case_label->data.node.name, "default") == 0;
                    
                    size_t current_code_pos = mg->code->length;
                    
                    /* Record frame at case label (branch target) */
                    mg_record_frame(mg);
                    
                    if (is_default) {
                        default_code_pos = current_code_pos;
                    } else {
                        /* Find sorted index for this case */
                        int sorted_idx = ast_to_sorted_idx[ast_idx];
                        /* Patch this case's offset */
                        int32_t offset = (int32_t)(current_code_pos - switch_pos);
                        mg->code->code[case_offset_positions[sorted_idx] + 0] = (offset >> 24) & 0xFF;
                        mg->code->code[case_offset_positions[sorted_idx] + 1] = (offset >> 16) & 0xFF;
                        mg->code->code[case_offset_positions[sorted_idx] + 2] = (offset >> 8) & 0xFF;
                        mg->code->code[case_offset_positions[sorted_idx] + 3] = offset & 0xFF;
                    }
                    
                    /* Generate statements (skip case expression for non-default) */
                    slist_t *stmts = case_label->data.node.children;
                    if (!is_default && stmts) {
                        stmts = stmts->next;
                    }
                    while (stmts) {
                        if (!codegen_statement(mg, (ast_node_t *)stmts->data)) {
                            free(case_values);
                            free(case_offset_positions);
                            free(case_to_ast_idx);
                            free(ast_to_sorted_idx);
                            return false;
                        }
                        stmts = stmts->next;
                    }
                }
                
                free(case_to_ast_idx);
                free(ast_to_sorted_idx);
                
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
                
                /* Pop switch context and patch breaks */
                mg_pop_loop(mg, switch_end);
                
                /* Reset last_opcode - switch doesn't guarantee method termination */
                mg->last_opcode = 0;
                
                free(case_values);
                free(case_offset_positions);
                
                return true;
            }
        
        case AST_EMPTY_STMT:
            /* No-op */
            return true;
        
        case AST_THROW_STMT:
            {
                /* Children: exception expression */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                /* Generate exception object */
                if (!codegen_expr(mg, (ast_node_t *)children->data, mg->cp)) {
                    return false;
                }
                
                /* athrow */
                bc_emit(mg->code, OP_ATHROW);
                mg->last_opcode = OP_ATHROW;
                mg_pop_typed(mg, 1);
                return true;
            }
        
        case AST_YIELD_STMT:
            {
                /* yield statement (Java 12+) - return value from switch expression */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    fprintf(stderr, "codegen: yield without value\n");
                    return false;
                }
                
                /* Generate yield value */
                if (!codegen_expr(mg, (ast_node_t *)children->data, mg->cp)) {
                    return false;
                }
                
                /* Emit goto - will be patched by enclosing switch expression */
                size_t goto_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                
                /* Add to yield patches list */
                mg->yield_patches = slist_prepend(mg->yield_patches, (void *)(uintptr_t)goto_pos);
                
                /* Value remains on stack */
                return true;
            }
        
        case AST_SYNCHRONIZED_STMT:
            {
                /* synchronized(expr) { body }
                 * Children:
                 *   [0] lock expression (object to synchronize on)
                 *   [1] body block
                 *
                 * Bytecode pattern:
                 *   <load lock object>
                 *   dup
                 *   astore <lock_slot>    ; save for monitorexit
                 *   monitorenter
                 *   <body>
                 *   aload <lock_slot>
                 *   monitorexit
                 *   goto after
                 * handler:               ; exception handler
                 *   astore <exc_slot>    ; save exception
                 *   aload <lock_slot>    ; load lock object
                 *   monitorexit          ; release lock
                 *   aload <exc_slot>     ; reload exception
                 *   athrow               ; rethrow
                 * after:
                 */
                slist_t *children = stmt->data.node.children;
                if (!children || !children->next) {
                    fprintf(stderr, "codegen: synchronized without expression or body\n");
                    return false;
                }
                
                ast_node_t *lock_expr = (ast_node_t *)children->data;
                ast_node_t *body = (ast_node_t *)children->next->data;
                
                /* Allocate local slots for lock object and exception */
                uint16_t lock_slot = mg->next_slot++;
                uint16_t exc_slot = mg->next_slot++;
                if (mg->next_slot > mg->max_locals) {
                    mg->max_locals = mg->next_slot;
                }
                
                /* Generate lock expression (puts object on stack) */
                if (!codegen_expr(mg, lock_expr, mg->cp)) {
                    return false;
                }
                
                /* Duplicate (one for monitorenter, one to save) */
                bc_emit(mg->code, OP_DUP);
                mg_push_object(mg, NULL);  /* DUP duplicates the object reference */
                
                /* Store lock reference for later monitorexit */
                if (lock_slot <= 3) {
                    bc_emit(mg->code, OP_ASTORE_0 + lock_slot);
                } else {
                    bc_emit(mg->code, OP_ASTORE);
                    bc_emit_u1(mg->code, (uint8_t)lock_slot);
                }
                mg_pop_typed(mg, 1);
                /* Update stackmap to track lock object in local slot */
                if (mg->stackmap) {
                    stackmap_set_local_object(mg->stackmap, lock_slot, mg->cp, "java/lang/Object");
                }
                
                /* monitorenter */
                bc_emit(mg->code, OP_MONITORENTER);
                mg_pop_typed(mg, 1);  /* consumes object reference */
                
                /* Record start of synchronized region */
                uint16_t sync_start = (uint16_t)mg->code->length;
                
                /* Generate body */
                if (!codegen_statement(mg, body)) {
                    return false;
                }
                
                /* Record end of synchronized region */
                uint16_t sync_end = (uint16_t)mg->code->length;
                
                /* Normal exit: monitorexit */
                if (lock_slot <= 3) {
                    bc_emit(mg->code, OP_ALOAD_0 + lock_slot);
                } else {
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)lock_slot);
                }
                mg_push_object(mg, NULL);  /* ALOAD loads object reference */
                bc_emit(mg->code, OP_MONITOREXIT);
                mg_pop_typed(mg, 1);
                
                /* Jump past exception handler */
                size_t goto_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                
                /* Save locals count before exception handler - the normal path
                 * doesn't have the exception slot, only the exception path does */
                uint16_t saved_locals_for_normal_exit = mg_save_locals_count(mg);
                
                /* Exception handler: catch-all */
                uint16_t handler_pc = (uint16_t)mg->code->length;
                
                /* Record stackmap frame at exception handler entry
                 * The mg_record_exception_handler_frame handles the exception on stack */
                mg_record_exception_handler_frame(mg, NULL);
                
                /* Store exception - it's pushed by the exception handler frame setup */
                mg_push_object(mg, "java/lang/Throwable");  /* Exception is on stack */
                if (exc_slot <= 3) {
                    bc_emit(mg->code, OP_ASTORE_0 + exc_slot);
                } else {
                    bc_emit(mg->code, OP_ASTORE);
                    bc_emit_u1(mg->code, (uint8_t)exc_slot);
                }
                mg_pop_typed(mg, 1);
                /* Update stackmap to track exception in local slot */
                if (mg->stackmap) {
                    stackmap_set_local_object(mg->stackmap, exc_slot, mg->cp, "java/lang/Throwable");
                }
                
                /* Load lock object and release */
                if (lock_slot <= 3) {
                    bc_emit(mg->code, OP_ALOAD_0 + lock_slot);
                } else {
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)lock_slot);
                }
                mg_push_object(mg, NULL);  /* ALOAD loads object reference */
                bc_emit(mg->code, OP_MONITOREXIT);
                mg_pop_typed(mg, 1);
                
                /* Reload and rethrow exception */
                if (exc_slot <= 3) {
                    bc_emit(mg->code, OP_ALOAD_0 + exc_slot);
                } else {
                    bc_emit(mg->code, OP_ALOAD);
                    bc_emit_u1(mg->code, (uint8_t)exc_slot);
                }
                mg_push_object(mg, "java/lang/Throwable");  /* ALOAD loads exception */
                bc_emit(mg->code, OP_ATHROW);
                mg_pop_typed(mg, 1);
                
                /* Patch goto to jump here (after exception handler) */
                uint16_t after_handler = (uint16_t)mg->code->length;
                int16_t goto_offset = (int16_t)(after_handler - goto_pos);
                mg->code->code[goto_pos + 1] = (goto_offset >> 8) & 0xFF;
                mg->code->code[goto_pos + 2] = goto_offset & 0xFF;
                
                /* Restore locals count to before exception handler state
                 * The normal path (goto target) doesn't have the exc_slot set */
                mg_restore_locals_count(mg, saved_locals_for_normal_exit);
                
                /* Record frame at goto target (after exception handler) */
                mg_record_frame(mg);
                
                /* Add exception handler entry (catch-all: catch_type = 0) */
                mg_add_exception_handler(mg, sync_start, sync_end, handler_pc, 0);
                
                return true;
            }
        
        case AST_ASSERT_STMT:
            {
                /* Children: [0] condition, [1] optional message */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return false;
                }
                
                ast_node_t *condition = (ast_node_t *)children->data;
                ast_node_t *message = children->next ? (ast_node_t *)children->next->data : NULL;
                
                /* Mark class as needing assertions */
                mg->class_gen->needs_assertions = true;
                
                /* Ensure $assertionsDisabled field ref is set up */
                if (mg->class_gen->assert_field_ref == 0) {
                    uint16_t field_ref = cp_add_fieldref(mg->cp, 
                        mg->class_gen->internal_name, 
                        "$assertionsDisabled", "Z");
                    mg->class_gen->assert_field_ref = field_ref;
                }
                
                /* Generate:
                 *   getstatic $assertionsDisabled
                 *   ifne skip        ; if assertions disabled, skip
                 *   <condition>
                 *   ifne skip        ; if condition is true, skip
                 *   new java/lang/AssertionError
                 *   dup
                 *   [<message>]      ; optional
                 *   invokespecial java/lang/AssertionError.<init>
                 *   athrow
                 * skip:
                 */
                
                /* getstatic $assertionsDisabled */
                bc_emit(mg->code, OP_GETSTATIC);
                bc_emit_u2(mg->code, mg->class_gen->assert_field_ref);
                mg_push(mg, 1);
                
                /* ifne skip (if assertions are disabled, skip) */
                size_t skip_disabled_pos = mg->code->length;
                bc_emit(mg->code, OP_IFNE);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);
                
                /* Generate condition */
                if (!codegen_expr(mg, condition, mg->cp)) {
                    return false;
                }
                
                /* ifne skip (if condition is true, skip) */
                size_t skip_cond_pos = mg->code->length;
                bc_emit(mg->code, OP_IFNE);
                bc_emit_u2(mg->code, 0);  /* Placeholder */
                mg_pop_typed(mg, 1);
                
                /* new java/lang/AssertionError */
                uint16_t ae_class = cp_add_class(mg->cp, "java/lang/AssertionError");
                bc_emit(mg->code, OP_NEW);
                bc_emit_u2(mg->code, ae_class);
                mg_push(mg, 1);
                
                /* dup */
                bc_emit(mg->code, OP_DUP);
                mg_push(mg, 1);
                
                if (message) {
                    /* Generate message expression */
                    if (!codegen_expr(mg, message, mg->cp)) {
                        return false;
                    }
                    /* invokespecial java/lang/AssertionError.<init>(Ljava/lang/Object;)V */
                    uint16_t init_ref = cp_add_methodref(mg->cp, 
                        "java/lang/AssertionError", "<init>", "(Ljava/lang/Object;)V");
                    bc_emit(mg->code, OP_INVOKESPECIAL);
                    bc_emit_u2(mg->code, init_ref);
                    mg_pop_typed(mg, 2);  /* pops dup'd ref + message */
                } else {
                    /* invokespecial java/lang/AssertionError.<init>()V */
                    uint16_t init_ref = cp_add_methodref(mg->cp, 
                        "java/lang/AssertionError", "<init>", "()V");
                    bc_emit(mg->code, OP_INVOKESPECIAL);
                    bc_emit_u2(mg->code, init_ref);
                    mg_pop_typed(mg, 1);  /* pops dup'd ref */
                }
                
                /* athrow */
                bc_emit(mg->code, OP_ATHROW);
                mg_pop_typed(mg, 1);  /* pops the AssertionError */
                
                /* skip: Patch both branches to here */
                size_t skip_target = mg->code->length;
                int16_t offset1 = (int16_t)(skip_target - skip_disabled_pos);
                mg->code->code[skip_disabled_pos + 1] = (offset1 >> 8) & 0xFF;
                mg->code->code[skip_disabled_pos + 2] = offset1 & 0xFF;
                
                int16_t offset2 = (int16_t)(skip_target - skip_cond_pos);
                mg->code->code[skip_cond_pos + 1] = (offset2 >> 8) & 0xFF;
                mg->code->code[skip_cond_pos + 2] = offset2 & 0xFF;
                
                return true;
            }
        
        case AST_TRY_STMT:
            {
                /* try { ... } catch (Type e) { ... } finally { ... }
                 * OR
                 * try (Resource r = expr) { ... } catch/finally
                 *
                 * Children:
                 *   [0..m-1] AST_RESOURCE_SPEC (for try-with-resources)
                 *   [m] try block
                 *   [m+1..n-1] catch clauses (AST_CATCH_CLAUSE)
                 *   [n] finally clause (AST_FINALLY_CLAUSE), if present
                 *
                 * Resource spec children:
                 *   [0] type node
                 *   [1] initializer expression
                 *   name: variable name
                 *
                 * Catch clause children:
                 *   [0] exception type
                 *   [1] catch block
                 *   name: exception variable name
                 *
                 * Finally clause children:
                 *   [0] finally block
                 */
                slist_t *children = stmt->data.node.children;
                if (!children) {
                    return true;  /* Empty try statement */
                }
                
                /* Collect resources, try block, catch clauses, and finally */
                slist_t *resources = NULL;
                ast_node_t *try_block = NULL;
                slist_t *catch_clauses = NULL;
                ast_node_t *finally_clause = NULL;
                
                for (slist_t *node = children; node; node = node->next) {
                    ast_node_t *child = (ast_node_t *)node->data;
                    if (child->type == AST_RESOURCE_SPEC) {
                        resources = slist_prepend(resources, child);
                    } else if (child->type == AST_CATCH_CLAUSE) {
                        catch_clauses = slist_prepend(catch_clauses, child);
                    } else if (child->type == AST_FINALLY_CLAUSE) {
                        finally_clause = child;
                    } else if (!try_block) {
                        /* First non-resource, non-catch, non-finally is try block */
                        try_block = child;
                    }
                }
                
                /* Reverse to get correct order */
                resources = slist_reverse(resources);
                catch_clauses = slist_reverse(catch_clauses);
                
                /* Handle try-with-resources */
                if (resources) {
                    return codegen_try_with_resources(mg, resources, try_block,
                                                       catch_clauses, finally_clause);
                }
                
                /* Regular try-catch-finally (no resources) */
                if (!try_block) {
                    slist_free(catch_clauses);
                    return true;
                }
                
                /* Save slot counter and stackmap state before the try-catch block.
                 * All locals allocated within try-catch (exc_slot, catch variables, etc.)
                 * should be cleaned up after the try-catch ends so they don't pollute
                 * subsequent code's stackmap frames. */
                uint16_t try_catch_saved_slot = mg->next_slot;
                uint16_t try_catch_saved_locals = 0;
                if (mg->stackmap) {
                    try_catch_saved_locals = mg_save_locals_count(mg);
                }
                
                /* Allocate a local slot for exception storage ONLY if there's a finally block.
                 * This slot is used to store the exception before executing finally code.
                 * If there's no finally, we don't need this slot and shouldn't allocate it,
                 * as it would leave an uninitialized gap in the locals causing Top in stackmap. */
                uint16_t exc_slot = 0;
                if (finally_clause) {
                    exc_slot = mg->next_slot++;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                }
                
                /* Record start of try block */
                uint16_t try_start = (uint16_t)mg->code->length;
                
                /* Save stackmap state before try block - exception handlers need
                 * the state at try entry since exceptions can be thrown at any point */
                stackmap_state_t *try_entry_state = NULL;
                if (mg->stackmap) {
                    try_entry_state = stackmap_save_state(mg->stackmap);
                }
                
                /* Generate try block */
                if (!codegen_statement(mg, try_block)) {
                    slist_free(catch_clauses);
                    stackmap_state_free(try_entry_state);
                    return false;
                }
                
                /* If finally exists, inline finally code at end of try block */
                if (finally_clause && finally_clause->data.node.children) {
                    ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                    if (!codegen_statement(mg, finally_block)) {
                        slist_free(catch_clauses);
                        return false;
                    }
                }
                
                /* Check if try block (+ inlined finally) ended with a terminating instruction
                 * on ALL code paths. We use mg->last_opcode which is set by statement codegen:
                 * - Return/throw statements set it to the return opcode
                 * - If-without-else sets it to 0 (fall-through path exists)
                 * - If-else sets it to return opcode only if BOTH branches return
                 * This is more accurate than checking the last bytecode, which might be
                 * from a branch that returns while another branch falls through. */
                bool try_ends_with_return = false;
                uint8_t last_op = mg->last_opcode;
                if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                    last_op == OP_LRETURN || last_op == OP_FRETURN ||
                    last_op == OP_DRETURN || last_op == OP_ARETURN ||
                    last_op == OP_ATHROW) {
                    try_ends_with_return = true;
                }
                
                /* Jump past all catch handlers if try block doesn't end with return/throw.
                 * Note: We DON'T emit a GOTO if try_ends_with_return is true, even if there
                 * are catch handlers, because if the try block ends with return, any branches
                 * within the try block must also be targeting return statements (or the
                 * branches come from nested if-statements whose target is after the if, not
                 * at the end of the try block). The exception is if-without-else where the
                 * false branch falls through - but we handle that by recording a frame at
                 * the branch target inside the if-statement codegen. */
                size_t try_exit_goto = 0;
                uint16_t try_exit_locals_count = 0;  /* Remember try path's locals for merging */
                if (!try_ends_with_return) {
                    try_exit_goto = mg->code->length;
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, 0);  /* Placeholder */
                    /* Remember the try path's locals count for later comparison */
                    if (mg->stackmap) {
                        try_exit_locals_count = mg->stackmap->current_locals_count;
                    }
                }
                
                uint16_t try_end = (uint16_t)mg->code->length;
                
                /* Track catch handler info: [goto_pos, catch_start, catch_end] triplets */
                slist_t *catch_gotos = NULL;   /* List of goto positions to patch */
                slist_t *catch_ranges = NULL;  /* List of catch ranges for finally handlers */
                
                /* Track if all catch blocks end with return/throw */
                bool all_catches_return = true;
                bool has_catch_clauses = (catch_clauses != NULL);
                
                /* For catch path locals tracking, we use try_catch_saved_locals which is
                 * the state BEFORE the try block. This ensures that locals declared inside
                 * the try block are NOT considered valid on the catch path (unless also
                 * assigned in the catch block, which would call stackmap_set_local). */
                
                /* Generate catch handlers */
                for (slist_t *node = catch_clauses; node; node = node->next) {
                    ast_node_t *catch_clause = (ast_node_t *)node->data;
                    slist_t *catch_children = catch_clause->data.node.children;
                    
                    if (!catch_children || !catch_children->next) {
                        continue;  /* Malformed catch */
                    }
                    
                    /* Count children to find exception types and catch block
                     * Multi-catch: catch (A | B | C e) { ... }
                     * Children: [0..n-2] exception types, [n-1] catch block */
                    int child_count = slist_length(catch_children);
                    int exc_type_count = child_count - 1;  /* Last child is the block */
                    
                    /* Get catch block (last child) */
                    slist_t *last = catch_children;
                    for (int i = 0; i < child_count - 1; i++) {
                        last = last->next;
                    }
                    ast_node_t *catch_block = (ast_node_t *)last->data;
                    const char *exc_var_name = catch_clause->data.node.name;
                    
                    /* Handler start - same for all exception types in multi-catch */
                    uint16_t handler_pc = (uint16_t)mg->code->length;
                    
                    /* Restore stackmap to try block entry state before recording handler frame.
                     * The exception can be thrown at any point in the try block, so the
                     * handler should have the locals state at try block entry. */
                    if (try_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, try_entry_state);
                    }
                    
                    /* Determine exception class name BEFORE recording handler frame.
                     * For multi-catch (more than one exception type), use Throwable
                     * as the stackmap type since the actual exception could be any of them.
                     * For single-catch, use the specific exception type. */
                    slist_t *type_node = catch_children;
                    const char *first_exc_class = "java/lang/Throwable";
                    bool is_multi_catch = (exc_type_count > 1);
                    
                    for (int i = 0; i < exc_type_count; i++) {
                        ast_node_t *exc_type = (ast_node_t *)type_node->data;
                        
                        /* Get exception class name - prefer sem_type for fully qualified name */
                        const char *exc_class_name = "java/lang/Throwable";
                        if (exc_type->sem_type && exc_type->sem_type->kind == TYPE_CLASS &&
                            exc_type->sem_type->data.class_type.name) {
                            /* Use fully qualified name from semantic analysis */
                            exc_class_name = exc_type->sem_type->data.class_type.name;
                        } else if (exc_type->type == AST_CLASS_TYPE) {
                            /* Fall back to AST name with common exception resolution */
                            exc_class_name = resolve_exception_class(exc_type->data.node.name);
                        }
                        if (i == 0) {
                            first_exc_class = exc_class_name;
                        }
                        
                        type_node = type_node->next;
                    }
                    
                    /* For multi-catch, use Throwable as the stackmap type.
                     * For single-catch, use the specific exception class. */
                    const char *stackmap_exc_class = is_multi_catch ? "java/lang/Throwable" : first_exc_class;
                    char *stackmap_exc_internal = class_to_internal_name(stackmap_exc_class);
                    
                    /* Record frame at exception handler (catch target)
                     * At exception handler, JVM clears stack and pushes exception. */
                    mg_record_exception_handler_frame(mg, stackmap_exc_internal);
                    
                    /* Add exception handler entry for each exception type */
                    type_node = catch_children;  /* Reset for second pass */
                    for (int i = 0; i < exc_type_count; i++) {
                        ast_node_t *exc_type = (ast_node_t *)type_node->data;
                        
                        /* Get exception class name - prefer sem_type for fully qualified name */
                        const char *exc_class_name = "java/lang/Throwable";
                        if (exc_type->sem_type && exc_type->sem_type->kind == TYPE_CLASS &&
                            exc_type->sem_type->data.class_type.name) {
                            exc_class_name = exc_type->sem_type->data.class_type.name;
                        } else if (exc_type->type == AST_CLASS_TYPE) {
                            exc_class_name = resolve_exception_class(exc_type->data.node.name);
                        }
                        
                        char *exc_internal_name = class_to_internal_name(exc_class_name);
                        uint16_t catch_type = cp_add_class(mg->cp, exc_internal_name);
                        free(exc_internal_name);
                        
                        /* Add exception handler entry - all point to same handler_pc */
                        mg_add_exception_handler(mg, try_start, try_end, handler_pc, catch_type);
                        
                        type_node = type_node->next;
                    }
                    
                    free(stackmap_exc_internal);
                    
                    /* Store exception to local variable - for multi-catch, use first type
                     * (a proper implementation would compute the common supertype) */
                    type_t *exc_type_t = type_new_class(first_exc_class);
                    uint16_t catch_exc_slot = mg_allocate_local(mg, exc_var_name, exc_type_t);
                    
                    /* Exception is on stack (pushed by JVM at handler entry) */
                    mg_push(mg, 1);
                    if (catch_exc_slot <= 3) {
                        bc_emit(mg->code, OP_ASTORE_0 + catch_exc_slot);
                    } else {
                        bc_emit(mg->code, OP_ASTORE);
                        bc_emit_u1(mg->code, (uint8_t)catch_exc_slot);
                    }
                    mg_pop_typed(mg, 1);
                    
                    uint16_t catch_start = (uint16_t)mg->code->length;
                    
                    /* Generate catch block */
                    if (!codegen_statement(mg, catch_block)) {
                        slist_free(catch_clauses);
                        slist_free(catch_gotos);
                        slist_free_full(catch_ranges, free);
                        return false;
                    }
                    
                    uint16_t catch_end = (uint16_t)mg->code->length;
                    
                    /* If finally exists, inline finally code */
                    if (finally_clause && finally_clause->data.node.children) {
                        ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                        if (!codegen_statement(mg, finally_block)) {
                            slist_free(catch_clauses);
                            slist_free(catch_gotos);
                            slist_free_full(catch_ranges, free);
                            return false;
                        }
                    }
                    
                    /* Save catch range for finally exception handler (before inlined finally) */
                    if (finally_clause) {
                        uint32_t *range = malloc(sizeof(uint32_t) * 2);
                        range[0] = catch_start;
                        range[1] = catch_end;
                        if (!catch_ranges) {
                            catch_ranges = slist_new(range);
                        } else {
                            slist_t *tail = slist_last(catch_ranges);
                            slist_append(tail, range);
                        }
                    }
                    
                    /* Check if catch block ended with a terminating instruction */
                    bool catch_ends_with_return = false;
                    if (mg->code->length > 0) {
                        uint8_t last_op = mg->code->code[mg->code->length - 1];
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW) {
                            catch_ends_with_return = true;
                        }
                    }
                    
                    if (!catch_ends_with_return) {
                        all_catches_return = false;
                    }
                    
                    /* Restore locals count to what it was BEFORE the try block.
                     * This removes the exception variable from the catch path's locals,
                     * and ensures that locals declared inside the try block are not
                     * considered valid on the catch path. Variables that are assigned
                     * in BOTH try and catch (like `value` in SafeMap.put) will be
                     * re-added to the stackmap when the catch block assigns them. */
                    mg_restore_locals_count(mg, try_catch_saved_locals);
                    
                    /* Jump to end (only if catch didn't end with return/throw) */
                    if (!catch_ends_with_return) {
                        size_t catch_exit_goto = mg->code->length;
                        bc_emit(mg->code, OP_GOTO);
                        bc_emit_u2(mg->code, 0);  /* Placeholder - will be patched */
                        
                        /* Store goto position for patching later (using slist_append for order) */
                        if (!catch_gotos) {
                            catch_gotos = slist_new((void *)(uintptr_t)catch_exit_goto);
                        } else {
                            slist_t *tail = slist_last(catch_gotos);
                            slist_append(tail, (void *)(uintptr_t)catch_exit_goto);
                        }
                    }
                }
                
                /* Generate finally exception handler (if finally exists) */
                uint16_t finally_handler_pc = 0;
                if (finally_clause) {
                    finally_handler_pc = (uint16_t)mg->code->length;
                    
                    /* Record frame at finally handler (exception handler target)
                     * At exception handler, JVM clears stack and pushes exception. */
                    mg_record_exception_handler_frame(mg, "java/lang/Throwable");
                    
                    /* Add exception handler for try block -> finally */
                    mg_add_exception_handler(mg, try_start, try_end, finally_handler_pc, 0);
                    
                    /* Exception is on stack (pushed by JVM at handler entry) - store it */
                    mg_push(mg, 1);
                    if (exc_slot <= 3) {
                        bc_emit(mg->code, OP_ASTORE_0 + exc_slot);
                    } else {
                        bc_emit(mg->code, OP_ASTORE);
                        bc_emit_u1(mg->code, (uint8_t)exc_slot);
                    }
                    mg_pop_typed(mg, 1);
                    
                    /* Generate finally block */
                    if (finally_clause->data.node.children) {
                        ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                        if (!codegen_statement(mg, finally_block)) {
                            slist_free(catch_clauses);
                            slist_free(catch_gotos);
                            slist_free_full(catch_ranges, free);
                            return false;
                        }
                    }
                    
                    /* Check if finally block ended with a terminating instruction
                     * If it did, we don't need to re-throw the exception */
                    bool finally_ends_with_return = false;
                    if (mg->code->length > 0) {
                        uint8_t last_op = mg->code->code[mg->code->length - 1];
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW) {
                            finally_ends_with_return = true;
                        }
                    }
                    
                    /* Re-throw exception (only if finally didn't return/throw) */
                    if (!finally_ends_with_return) {
                        if (exc_slot <= 3) {
                            bc_emit(mg->code, OP_ALOAD_0 + exc_slot);
                        } else {
                            bc_emit(mg->code, OP_ALOAD);
                            bc_emit_u1(mg->code, (uint8_t)exc_slot);
                        }
                        mg_push(mg, 1);
                        bc_emit(mg->code, OP_ATHROW);
                        mg_pop_typed(mg, 1);
                    }
                }
                
                /* End of all handlers */
                uint16_t after_try = (uint16_t)mg->code->length;
                
                /* Record frame at end of try-catch-finally (join point) - only if there's
                 * actually code that reaches here (i.e., not all paths return/throw).
                 * 
                 * The join point is reachable from:
                 * 1. Normal try block exit (GOTO from end of try block)
                 * 2. Catch block exits (GOTO from end of each catch block)
                 * 
                 * We need to compute the minimum locals from all converging paths:
                 * - try_exit_locals_count: locals at end of try block
                 * - current stackmap: reflects last processed catch block's locals
                 * 
                 * For empty catch blocks, the catch path has fewer locals than try path.
                 * For catch blocks that assign the same variables as try, both paths
                 * have the same locals count. We take the minimum.
                 * 
                 * Note: catch path's locals count was already set by mg_restore_locals_count
                 * in the catch handler loop to saved_locals_count (which is try path's count).
                 * But the catch block may have assigned additional locals, or if the catch
                 * block was empty, the exception variable was the only local added and then
                 * removed by restore. So current_locals_count reflects what's valid on ALL
                 * catch paths that flow to this point. */
                if (try_exit_goto > 0 || catch_gotos) {
                    /* If try block didn't exit (try_exit_goto == 0), only catch paths
                     * reach here, so use current stackmap state as-is.
                     * If try block exited, we need to merge with try path's locals. */
                    if (mg->stackmap && try_exit_goto > 0 && try_exit_locals_count > 0) {
                        /* Take minimum of try and catch locals counts */
                        if (try_exit_locals_count < mg->stackmap->current_locals_count) {
                            /* Try path had fewer locals (e.g., catch block added vars) */
                            mg_restore_locals_count(mg, try_exit_locals_count);
                        }
                        /* If catch path has fewer locals, current state is already correct */
                    }
                    mg_record_frame(mg);
                }
                
                /* Patch try exit goto (only if we emitted one) */
                if (try_exit_goto > 0) {
                    int16_t try_exit_offset = (int16_t)(after_try - try_exit_goto);
                    mg->code->code[try_exit_goto + 1] = (try_exit_offset >> 8) & 0xFF;
                    mg->code->code[try_exit_goto + 2] = try_exit_offset & 0xFF;
                }
                
                /* Patch catch exit gotos */
                for (slist_t *node = catch_gotos; node; node = node->next) {
                    size_t pos = (size_t)(uintptr_t)node->data;
                    int16_t offset = (int16_t)(after_try - pos);
                    mg->code->code[pos + 1] = (offset >> 8) & 0xFF;
                    mg->code->code[pos + 2] = offset & 0xFF;
                }
                slist_free(catch_gotos);
                
                /* Add finally handlers for catch blocks */
                if (finally_handler_pc > 0) {
                    for (slist_t *node = catch_ranges; node; node = node->next) {
                        uint32_t *range = (uint32_t *)node->data;
                        mg_add_exception_handler(mg, (uint16_t)range[0], 
                                                  (uint16_t)range[1],
                                                  finally_handler_pc, 0);
                    }
                }
                slist_free_full(catch_ranges, free);
                
                slist_free(catch_clauses);
                stackmap_state_free(try_entry_state);
                
                /* Restore slot counter and stackmap locals count to clean up locals
                 * that were allocated within the try-catch block (exc_slot, catch vars, etc.)
                 * This prevents these locals from polluting subsequent code's stackmap frames. */
                mg->next_slot = try_catch_saved_slot;
                if (try_catch_saved_locals > 0) {
                    mg_restore_locals_count(mg, try_catch_saved_locals);
                }
                
                /* Update last_opcode based on whether all paths terminated
                 * If try block returns and all catch blocks return, the try-catch terminates
                 * Otherwise, there's a path that reaches the end */
                if (try_ends_with_return && all_catches_return && has_catch_clauses) {
                    /* All paths return - set last_opcode to indicate termination
                     * Use IRETURN as a marker (the actual return type doesn't matter
                     * for the check in codegen.c) */
                    mg->last_opcode = OP_IRETURN;
                } else {
                    /* Some path doesn't return - reset last_opcode */
                    mg->last_opcode = 0;
                }
                
                return true;
            }
        
        case AST_LABELED_STMT:
            {
                /* A labeled statement: label: statement
                 * Children: [0] the statement
                 * Name: the label
                 */
                const char *label = stmt->data.node.name;
                slist_t *children = stmt->data.node.children;
                
                if (!children) {
                    return true;  /* Empty labeled statement */
                }
                
                ast_node_t *inner_stmt = (ast_node_t *)children->data;
                
                /* Check if the inner statement is a loop */
                bool is_loop = (inner_stmt->type == AST_WHILE_STMT ||
                               inner_stmt->type == AST_FOR_STMT ||
                               inner_stmt->type == AST_DO_STMT ||
                               inner_stmt->type == AST_ENHANCED_FOR_STMT);
                
                if (is_loop) {
                    /* For loops, set the pending label so the loop handler can use it */
                    mg->pending_label = label;
                    bool result = codegen_statement(mg, inner_stmt);
                    mg->pending_label = NULL;
                    return result;
                } else {
                    /* For non-loop statements (e.g., blocks), we still need to support
                     * labeled break. Push a "break-only" context with no continue target.
                     */
                    mg_push_loop(mg, 0, label);  /* continue_target=0 means no continue */
                    
                    bool result = codegen_statement(mg, inner_stmt);
                    
                    /* Pop loop context and patch breaks to after the statement */
                    mg_pop_loop(mg, mg->code->length);
                    
                    /* Reset last_opcode - labeled blocks don't guarantee method termination */
                    mg->last_opcode = 0;
                    
                    return result;
                }
            }
        
        case AST_CLASS_DECL:
        case AST_INTERFACE_DECL:
            {
                /* Local class declaration inside a method body */
                /* Collect it for separate compilation like nested classes */
                if (mg->class_gen) {
                    if (!mg->class_gen->local_classes) {
                        mg->class_gen->local_classes = slist_new(stmt);
                    } else {
                        slist_append(mg->class_gen->local_classes, stmt);
                    }
                    
                    /* Add InnerClasses attribute entry for the local class.
                     * For local classes, outer_class_info = 0 (no enclosing class
                     * in the inner classes attribute sense). */
                    const char *local_name = stmt->data.node.name;
                    if (local_name && stmt->sem_symbol) {
                        symbol_t *local_sym = stmt->sem_symbol;
                        char *local_internal = class_to_internal_name(local_sym->qualified_name);
                        
                        inner_class_entry_t *entry = calloc(1, sizeof(inner_class_entry_t));
                        entry->inner_class_info = cp_add_class(mg->class_gen->cp, local_internal);
                        entry->outer_class_info = 0;  /* Local class has no outer in attribute */
                        entry->inner_name = cp_add_utf8(mg->class_gen->cp, local_name);
                        /* Convert MOD_ flags to ACC_ flags (simplified for local classes) */
                        uint16_t mods = stmt->data.node.flags;
                        uint16_t acc = 0;
                        if (mods & MOD_PUBLIC)    acc |= ACC_PUBLIC;
                        if (mods & MOD_PRIVATE)   acc |= ACC_PRIVATE;
                        if (mods & MOD_PROTECTED) acc |= ACC_PROTECTED;
                        if (mods & MOD_STATIC)    acc |= ACC_STATIC;
                        if (mods & MOD_FINAL)     acc |= ACC_FINAL;
                        if (mods & MOD_ABSTRACT)  acc |= ACC_ABSTRACT;
                        entry->access_flags = acc;
                        
                        if (!mg->class_gen->inner_class_entries) {
                            mg->class_gen->inner_class_entries = slist_new(entry);
                        } else {
                            slist_append(mg->class_gen->inner_class_entries, entry);
                        }
                        
                        /* Also add to NestMembers if this class is the nest host */
                        if (mg->class_gen->nest_host == 0) {
                            uint16_t *nest_member_idx = malloc(sizeof(uint16_t));
                            *nest_member_idx = entry->inner_class_info;
                            if (!mg->class_gen->nest_members) {
                                mg->class_gen->nest_members = slist_new(nest_member_idx);
                            } else {
                                slist_append(mg->class_gen->nest_members, nest_member_idx);
                            }
                        }
                        
                        free(local_internal);
                    }
                }
                /* No bytecode generated here - class is compiled separately */
                return true;
            }
        
        default:
            fprintf(stderr, "codegen: unhandled statement type: %s\n",
                    ast_type_name(stmt->type));
            return false;
    }
}

