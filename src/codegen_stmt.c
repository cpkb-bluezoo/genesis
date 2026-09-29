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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
            static __thread char buf[128];
            snprintf(buf, sizeof(buf), "java/lang/%s", name);
            return buf;
        }
    }
    
    /* Not a known java.lang exception - return as-is */
    return name;
}

/**
 * Evaluate a switch case label that is a compile-time constant EXPRESSION
 * (not a bare literal or a named constant, both already handled by the
 * caller) - e.g. "case ('U' << 24) | ('S' << 16) | ('E' << 8) | 'R':",
 * the multi-char command-packing idiom gumdrop's own FtpProtocolHandler.
 * matchCommand() uses for every one of its ~40 case labels. Handles only
 * the operators actually needed for that idiom (shifts and bitwise/
 * arithmetic combinations of int/char literals) - matching this file's
 * existing philosophy elsewhere of handling the concrete case actually
 * needed rather than a general constant-folding evaluator (see the
 * "constant expression required" case-label handling in semantic.c for
 * the same stated approach). Before this, any case label shaped as an
 * expression (anything other than a bare literal or a plain identifier)
 * silently fell through the case_values[] collection loop's two
 * branches entirely, leaving that case's match value at its calloc()
 * zero-initialized default - so every such case collided on match value
 * 0, and the JVM verifier rejected the resulting lookupswitch outright
 * ("Bad lookupswitch instruction") the instant there was more than one
 * such case (there always is, for this idiom).
 * Returns true and writes *out on success (a genuinely constant
 * expression built entirely from literals and these operators); false
 * for anything else (an identifier, a method call, or an operator not
 * handled here), leaving *out untouched. */
static bool eval_int_constant_expr(ast_node_t *expr, int32_t *out)
{
    if (!expr) {
        return false;
    }

    if (expr->type == AST_PARENTHESIZED) {
        return expr->data.node.children &&
            eval_int_constant_expr((ast_node_t *)expr->data.node.children->data, out);
    }

    if (expr->type == AST_LITERAL) {
        if (expr->data.leaf.token_type == TOK_CHAR_LITERAL) {
            const char *sv = expr->data.leaf.value.str_val;
            *out = sv ? (int32_t)(unsigned char)sv[0] : 0;
            return true;
        }
        if (expr->data.leaf.token_type == TOK_INTEGER_LITERAL) {
            *out = (int32_t)expr->data.leaf.value.int_val;
            return true;
        }
        return false;
    }

    /* A named constant used as an OPERAND within a larger constant
     * expression (e.g. "case SEQUENCE & TAG_MASK:", where SEQUENCE and
     * TAG_MASK are each "static final int" fields) - semantic analysis
     * already resolved and populated this identifier leaf's own constant
     * value, exactly like the sibling bare-identifier case-label handling
     * at this function's caller does directly. Without this, any operator
     * combining two (or more) named constants recursed into two bare
     * AST_IDENTIFIER nodes this function had no case for, silently
     * returning false and leaving the WHOLE expression's case value at 0
     * - the same "every case defaults to 0" collision this function was
     * originally added to fix, just one level deeper (a named-constant
     * OPERAND instead of the top-level case label itself). Confirmed
     * against gumdrop's own Asn1Type.getTagName(), whose "case SEQUENCE &
     * TAG_MASK:" and "case SET & TAG_MASK:" both collapsed to match value
     * 0 - two lookupswitch pairs with the same key, which is exactly what
     * "Bad lookupswitch instruction" flags (JVMS 4.9.1's lookupswitch
     * validity rule: match values must be in strictly increasing order,
     * so a duplicate is invalid by construction). */
    if (expr->type == AST_IDENTIFIER) {
        *out = (int32_t)expr->data.leaf.value.int_val;
        return true;
    }

    if (expr->type == AST_UNARY_EXPR) {
        slist_t *children = expr->data.node.children;
        int32_t operand;
        if (!children || !eval_int_constant_expr((ast_node_t *)children->data, &operand)) {
            return false;
        }
        switch (expr->data.node.op_token) {
            case TOK_MINUS: *out = -operand; return true;
            case TOK_PLUS:  *out = operand;  return true;
            case TOK_TILDE: *out = ~operand; return true;
            default: return false;
        }
    }

    if (expr->type == AST_BINARY_EXPR) {
        slist_t *children = expr->data.node.children;
        if (!children || !children->next) {
            return false;
        }
        int32_t l, r;
        if (!eval_int_constant_expr((ast_node_t *)children->data, &l) ||
            !eval_int_constant_expr((ast_node_t *)children->next->data, &r)) {
            return false;
        }
        switch (expr->data.node.op_token) {
            case TOK_LSHIFT:  *out = l << (r & 31); return true;
            case TOK_RSHIFT:  *out = l >> (r & 31); return true;
            case TOK_URSHIFT: *out = (int32_t)((uint32_t)l >> (r & 31)); return true;
            case TOK_BITOR:   *out = l | r; return true;
            case TOK_BITAND:  *out = l & r; return true;
            case TOK_CARET:   *out = l ^ r; return true;
            case TOK_PLUS:    *out = l + r; return true;
            case TOK_MINUS:   *out = l - r; return true;
            case TOK_STAR:    *out = l * r; return true;
            default: return false;
        }
    }

    return false;
}

/**
 * Apply the proper JVM-verifier MERGE of several "reaches this point"
 * stackmap snapshots to `smt`'s own current tracked state - used for a
 * switch statement's single shared exit frame (every `break`, from any
 * case, plus a non-terminating fallthrough off the physically last
 * case, all jump to or flow into the exact same bytecode offset, so
 * there is only ever ONE frame possible there in the class file, and it
 * must be valid for every one of those incoming edges at once).
 *
 * Per JVM spec 4.10.1.4, merging two local-variable slots that disagree
 * on type yields "top" (unusable) at that slot, not an error - callers
 * differ legitimately, e.g. a local declared inside only one case's
 * body (no braces needed for it to be "in scope" for later cases, per
 * JLS 6.3, but it's only ever ASSIGNED along whichever single case
 * actually declared it) is simply untyped/unusable after the switch,
 * while a local declared BEFORE the switch and consistently assigned by
 * EVERY reachable case keeps its real type, exactly matching Java's own
 * definite-assignment rule for using such a local after the switch.
 * Slots beyond the shortest snapshot's own tracked count are dropped
 * entirely rather than padded - the JVM spec allows a frame's locals
 * array to be shorter than the method's max_locals, with everything
 * past the end implicitly "top", so this is equivalent to (and simpler
 * than) padding every snapshot to the same length first.
 *
 * `states` must be non-empty; the operand stack is always empty at a
 * switch statement's exit (unlike a switch expression, no value is
 * ever left on the stack there), so only locals need merging - callers
 * are expected to have cleared/never pushed onto smt's current stack
 * before calling this. */
static void merge_stackmap_states_into(stack_map_table_t *smt, slist_t *states)
{
    if (!smt || !states) {
        return;
    }

    stackmap_state_t *first = (stackmap_state_t *)states->data;
    uint16_t merged_count = first->num_locals;
    for (slist_t *n = states->next; n; n = n->next) {
        stackmap_state_t *s = (stackmap_state_t *)n->data;
        if (s->num_locals < merged_count) {
            merged_count = s->num_locals;
        }
    }

    stackmap_state_t merged;
    merged.num_locals = merged_count;
    merged.locals = merged_count ? malloc(merged_count * sizeof(verification_type_t)) : NULL;
    merged.stack_size = 0;
    merged.stack = NULL;

    for (uint16_t i = 0; i < merged_count; i++) {
        verification_type_t t = first->locals[i];
        bool agree = true;
        for (slist_t *n = states->next; n && agree; n = n->next) {
            stackmap_state_t *s = (stackmap_state_t *)n->data;
            verification_type_t o = s->locals[i];
            if (o.tag != t.tag ||
                (t.tag == VT_OBJECT && o.data.cp_index != t.data.cp_index) ||
                (t.tag == VT_UNINITIALIZED && o.data.offset != t.data.offset)) {
                agree = false;
            }
        }
        if (merged.locals) {
            merged.locals[i] = agree ? t : (verification_type_t){.tag = VT_TOP};
        }
    }

    stackmap_restore_state(smt, &merged);
    free(merged.locals);
}

/**
 * The boxed wrapper class (internal JVM name) for a primitive type kind,
 * e.g. TYPE_INT -> "java/lang/Integer". Mirrors emit_boxing()'s own
 * switch in codegen_expr.c, which maps the same primitive kinds to their
 * wrapper class for the opposite (box) direction. Returns NULL for a
 * non-primitive kind.
 */
static const char *wrapper_class_for_primitive(type_kind_t kind)
{
    switch (kind) {
        case TYPE_INT:     return "java/lang/Integer";
        case TYPE_LONG:    return "java/lang/Long";
        case TYPE_DOUBLE:  return "java/lang/Double";
        case TYPE_FLOAT:   return "java/lang/Float";
        case TYPE_BYTE:    return "java/lang/Byte";
        case TYPE_SHORT:   return "java/lang/Short";
        case TYPE_CHAR:    return "java/lang/Character";
        case TYPE_BOOLEAN: return "java/lang/Boolean";
        default:           return NULL;
    }
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
    
    for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
        ast_node_t *case_label = (ast_node_t *)node->data;
        if (case_label->type != AST_CASE_LABEL) continue;
        
        if (case_label->data.node.name && 
            strcmp(case_label->data.node.name, "default") == 0) {
            /* Skip default label - handled separately */
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

    /* Snapshots of every state that actually reaches the switch's shared
     * exit point (every `break`, from any case) - collected below as
     * each case is generated, mirroring AST_SWITCH_STMT's own identical
     * switch_exit_states/merge_stackmap_states_into() fix for the exact
     * same bug in that (enum/int selector) sibling switch codegen. This
     * function - the SEPARATE codegen path for a String selector - had
     * its own, never-updated copy of the same "record the exit frame
     * from whatever's live in codegen order" mistake: every case here
     * ends in `break` to a SHARED exit point, but only a case that
     * happens to declare its own local (e.g. "case \"map\": FieldDescriptor
     * mapField = ...; break;") left that local's real type in
     * mg->stackmap by the time the LAST case in AST order finished, so
     * the ONE recorded frame at that shared point silently used
     * whichever case ran last - wrong for every other case's own
     * break, which never touched that slot. VerifyError: "Inconsistent
     * stackmap frames ... not assignable" the moment a DIFFERENT case's
     * break reached the same target. Confirmed against gumdrop's own
     * ProtoFileParser.parseMessage(), whose "switch (tok) { case
     * \"option\": ...; break; ... case \"map\": FieldDescriptor
     * mapField = ...; break; ... }" is exactly this shape. */
    slist_t *string_switch_exit_states = NULL;
    bool has_default_label = false;

    for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
        ast_node_t *case_label = (ast_node_t *)node->data;
        if (case_label->type != AST_CASE_LABEL) continue;

        bool is_default = (case_label->data.node.name &&
                          strcmp(case_label->data.node.name, "default") == 0);
        if (is_default) {
            has_default_label = true;
        }

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
                for (slist_t *n = string_switch_exit_states; n; n = n->next) {
                    stackmap_state_free((stackmap_state_t *)n->data);
                }
                slist_free(string_switch_exit_states);
                return false;
            }
            stmts = stmts->next;
        }

        /* This case's own exit state, captured for the merge below -
         * either it ends in `break` (OP_GOTO, reaching the shared exit
         * directly) or, if it's the PHYSICALLY LAST case and falls off
         * the end without break/return/throw, it reaches the shared
         * exit via plain fallthrough. A case ending in return/throw
         * never reaches the shared exit at all and is correctly
         * excluded. An EMPTY-bodied case label (grouped fallthrough
         * labels sharing one body, e.g. "case \"Monday\": case \"Tuesday\":
         * ... case \"Friday\": result = 1; break;" - every label but the
         * last generates no statements at all) must also be excluded:
         * it never executes any code of its own, so mg->last_opcode is
         * just stale leftover state from whatever ran before it, and
         * capturing an "exit" snapshot for it would wrongly pull in
         * switch-ENTRY state (unassigned locals) as if it were a real
         * incoming edge to the merge. */
        bool has_body = (mg->code->length > body_pos);
        bool is_last_case = (node->next == NULL);
        bool ends_in_break = (mg->last_opcode == OP_GOTO);
        bool ends_in_terminal = (mg->last_opcode == OP_RETURN || mg->last_opcode == OP_IRETURN ||
            mg->last_opcode == OP_LRETURN || mg->last_opcode == OP_FRETURN ||
            mg->last_opcode == OP_DRETURN || mg->last_opcode == OP_ARETURN ||
            mg->last_opcode == OP_ATHROW);
        if (has_body && (ends_in_break || (is_last_case && !ends_in_terminal)) && mg->stackmap) {
            stackmap_state_t *exit_snap = stackmap_save_state(mg->stackmap);
            if (exit_snap) {
                if (!string_switch_exit_states) {
                    string_switch_exit_states = slist_new(exit_snap);
                } else {
                    slist_append(string_switch_exit_states, exit_snap);
                }
            }
        }
    }

    /* The "no case matched" edge (hash miss, or hash hit but every
     * equals() check failed) reaches the shared exit directly, carrying
     * switch-entry state, whenever there's no explicit "default:" label
     * (see "default_target = default_body_pos ? default_body_pos :
     * switch_end" below - switch_end IS the shared exit in that case). */
    if (!has_default_label && switch_entry_state) {
        stackmap_state_t *entry_snap = calloc(1, sizeof(stackmap_state_t));
        if (entry_snap) {
            entry_snap->num_locals = switch_entry_state->num_locals;
            entry_snap->locals = switch_entry_state->num_locals ?
                malloc(switch_entry_state->num_locals * sizeof(verification_type_t)) : NULL;
            if (entry_snap->locals) {
                memcpy(entry_snap->locals, switch_entry_state->locals,
                       switch_entry_state->num_locals * sizeof(verification_type_t));
            }
            if (!string_switch_exit_states) {
                string_switch_exit_states = slist_new(entry_snap);
            } else {
                slist_append(string_switch_exit_states, entry_snap);
            }
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
            if (string_switch_exit_states && mg->stackmap) {
                merge_stackmap_states_into(mg->stackmap, string_switch_exit_states);
            }
            mg_record_frame(mg);
        }
    }
    if (string_switch_exit_states) {
        for (slist_t *n = string_switch_exit_states; n; n = n->next) {
            stackmap_state_free((stackmap_state_t *)n->data);
        }
        slist_free(string_switch_exit_states);
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
            
            /* Resolve resource type - prefer sem_type set during semantic analysis
             * to ensure we get the fully qualified name with symbol reference */
            type_t *res_type = type_node->sem_type;
            if (!res_type) {
                res_type = semantic_resolve_type(mg->class_gen->sem, type_node);
            }
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

        /* Multi-catch: types then block (same layout as regular try/catch) */
        int child_count = slist_length(catch_children);
        int exc_type_count = child_count - 1;
        slist_t *last = catch_children;
        for (int i = 0; i < child_count - 1; i++) {
            last = last->next;
        }
        ast_node_t *catch_block = (ast_node_t *)last->data;
        const char *exc_var_name = catch_clause->data.node.name;
        
        /* Get catch handler start position */
        uint16_t catch_handler_pc = (uint16_t)mg->code->length;
        
        /* Restore stackmap to try entry state for this catch handler */
        if (try_entry_state && mg->stackmap) {
            stackmap_restore_state(mg->stackmap, try_entry_state);
        }
        
        const char *first_exc_class = "java/lang/Throwable";
        bool is_multi_catch = (exc_type_count > 1);
        slist_t *type_node = catch_children;
        for (int i = 0; i < exc_type_count; i++) {
            ast_node_t *exc_type = (ast_node_t *)type_node->data;
            const char *exc_class_name = "java/lang/Throwable";
            if (exc_type->sem_type && exc_type->sem_type->kind == TYPE_CLASS &&
                exc_type->sem_type->data.class_type.name) {
                exc_class_name = exc_type->sem_type->data.class_type.name;
            } else if (exc_type->type == AST_CLASS_TYPE) {
                exc_class_name = resolve_exception_class(exc_type->data.node.name);
            }
            if (i == 0) {
                first_exc_class = exc_class_name;
            }
            type_node = type_node->next;
        }

        /* For multi-catch, prefer the LUB semantic analysis already
         * computed across every alternative (catch_clause->sem_type) as
         * the HANDLER'S OWN ENTRY FRAME type, over a blanket
         * "java/lang/Throwable". The exact type recorded here for the
         * exception on the stack becomes, per the ASTORE right below,
         * the VERIFIED type of the local variable slot from this point
         * forward - a REAL verifier does not re-derive it from anything
         * else (not the exception table's own declared catch types, not
         * any of genesis's own internal bookkeeping). Using Throwable
         * here while LATER frames in the same catch body (e.g. a nested
         * try/catch's own handler entry) are built using the LUB
         * (Exception, say) is therefore a guaranteed mismatch the moment
         * any such later frame exists: VerifyError "Stack map does not
         * match the one at exception handler ... Type 'Throwable' ...
         * not assignable to 'Exception'". Confirmed against gumdrop's own
         * MessageIndex.save(), whose outer "catch (IOException |
         * RuntimeException e)" wraps a try-with-resources followed by its
         * own nested "try { Files.deleteIfExists(tempPath); } catch
         * (IOException deleteFailed) { e.addSuppressed(...); }" - exactly
         * this shape. Falls back to Throwable only if semantic analysis
         * didn't leave a usable class type (defensive; shouldn't happen
         * in practice, and merely conservative - never itself a
         * correctness bug - since every real class is assignable to
         * Throwable). */
        const char *stackmap_exc_class = first_exc_class;
        if (is_multi_catch) {
            stackmap_exc_class = "java/lang/Throwable";
            if (catch_clause->sem_type && catch_clause->sem_type->kind == TYPE_CLASS &&
                catch_clause->sem_type->data.class_type.name) {
                stackmap_exc_class = catch_clause->sem_type->data.class_type.name;
            }
        }
        char *stackmap_exc_internal = class_to_internal_name(stackmap_exc_class);
        mg_record_exception_handler_frame(mg, stackmap_exc_internal);

        type_node = catch_children;
        for (int i = 0; i < exc_type_count; i++) {
            ast_node_t *exc_type = (ast_node_t *)type_node->data;
            const char *exc_class_name = "java/lang/Throwable";
            if (exc_type->sem_type && exc_type->sem_type->kind == TYPE_CLASS &&
                exc_type->sem_type->data.class_type.name) {
                exc_class_name = exc_type->sem_type->data.class_type.name;
            } else if (exc_type->type == AST_CLASS_TYPE) {
                exc_class_name = resolve_exception_class(exc_type->data.node.name);
            }
            char *exc_internal_name = class_to_internal_name(exc_class_name);
            uint16_t exc_class_idx = cp_add_class(mg->cp, exc_internal_name);
            free(exc_internal_name);
            mg_add_exception_handler(mg, try_start, (uint16_t)mg->code->length,
                                     catch_handler_pc, exc_class_idx);
            type_node = type_node->next;
        }
        free(stackmap_exc_internal);
        
        /* Allocate local for exception variable. Prefer the LUB semantic
         * analysis already computed across every multi-catch alternative
         * (catch_clause->sem_type, set in semantic.c's AST_CATCH_CLAUSE
         * handling) over first_exc_class - using only the FIRST
         * alternative's type here (as this used to) made a later checkcast
         * against that type reject any OTHER alternative actually thrown
         * at runtime: ClassCastException. Falls back to first_exc_class
         * only if semantic analysis didn't leave a usable class type
         * (defensive; shouldn't happen in practice). */
        type_t *exc_type = (catch_clause->sem_type && catch_clause->sem_type->kind == TYPE_CLASS) ?
            catch_clause->sem_type : type_new_class(first_exc_class);
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

        /* mg->stackmap's own ongoing simulated type for this local slot
         * still reflects whatever was actually pushed by the handler-
         * entry frame (java/lang/Throwable for a multi-catch - a safe
         * blanket type valid for every alternative, separate from
         * exc_type's own LUB computed above) after the ASTORE. Without
         * re-typing it here, a LATER frame genesis records elsewhere in
         * this same catch body (e.g. a nested try/catch's own handler
         * entry, which correctly uses exc_type's real LUB) disagrees with
         * what mg->stackmap would naturally derive by walking the
         * bytecode from here - see the identical fix and its full
         * explanation at the sibling multi-catch site in the main
         * AST_TRY_STMT catch-clause loop below. */
        if (mg->stackmap && exc_type->kind == TYPE_CLASS && exc_type->data.class_type.name) {
            char *exc_local_internal = class_to_internal_name(exc_type->data.class_type.name);
            stackmap_set_local_object(mg->stackmap, exc_slot, mg->cp, exc_local_internal);
            free(exc_local_internal);
        }

        uint16_t catch_start = (uint16_t)mg->code->length;

        /* Reset last_opcode before generating the catch body - otherwise it
         * carries over stale state from the try block (e.g. OP_ATHROW, if
         * the try body's own last statement was a throw), which has
         * nothing to do with whether THIS catch body itself terminates,
         * and would wrongly mark it as terminal below if the catch body's
         * own last statement doesn't itself set/reset last_opcode (as most
         * plain statements don't). Mirrors the same reset already used
         * elsewhere (if/else, synchronized, for-loop bodies) to prevent
         * exactly this kind of stale carryover. */
        mg->last_opcode = 0;

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

        /* Generate goto to skip other catch handlers (if not ending with
         * return/throw/goto). OP_GOTO is included since a catch block
         * ending in break/continue (see the matching fix a few hundred
         * lines below, in the main AST_TRY_STMT catch-clause loop) jumps
         * unconditionally elsewhere just like return/throw - without this,
         * the epilogue goto emitted below would be unreachable dead code
         * with no stack frame recorded for it. Guarded by catch_emitted_code
         * (see the matching, more detailed comment below) since an EMPTY
         * catch block never terminates, regardless of whatever unrelated
         * value mg->last_opcode was carrying over from before this catch
         * clause began. */
        uint8_t last_op = mg->last_opcode;
        bool catch_emitted_code = mg->code->length > catch_start;
        bool catch_ends_with_return = (last_op == OP_RETURN || last_op == OP_ARETURN ||
                                       last_op == OP_IRETURN || last_op == OP_LRETURN ||
                                       last_op == OP_FRETURN || last_op == OP_DRETURN ||
                                       last_op == OP_ATHROW ||
                                       (last_op == OP_GOTO && catch_emitted_code));
        
        if (!catch_ends_with_return) {
            size_t *goto_pos = malloc(sizeof(size_t));
            *goto_pos = mg->code->length;
            catch_gotos = slist_prepend(catch_gotos, goto_pos);
            bc_emit(mg->code, OP_GOTO);
            bc_emit_u2(mg->code, 0);  /* Placeholder */
        }
    }
    
    /* Note: If we have no gotos to patch, all catches ended with return/throw */
    (void)catch_clauses;  /* Used in iteration above */
    
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

/**
 * Release every monitor currently held by an enclosing synchronized
 * statement, in innermost-first order, right before a `return` leaves the
 * method from inside one or more of them. Each release is
 * "aload lock_slot; monitorexit" - this only touches the top of the
 * operand stack, so it's safe to emit after the return value (if any) has
 * already been pushed: the value stays untouched underneath. Without this,
 * a `return` lexically inside a synchronized block skipped the monitor
 * exit entirely, leaving the lock held forever (a `return` reaching the
 * synchronized statement's own normal-exit code never happens, since
 * `codegen_statement` for AST_RETURN_STMT emits the return instruction
 * directly at that point in the bytecode stream, not a jump to the
 * synchronized statement's epilogue).
 */
static void emit_pending_monitorexits(method_gen_t *mg)
{
    for (slist_t *node = mg->sync_lock_stack; node; node = node->next) {
        uint16_t lock_slot = (uint16_t)(uintptr_t)node->data;
        if (lock_slot <= 3) {
            bc_emit(mg->code, OP_ALOAD_0 + lock_slot);
        } else {
            bc_emit(mg->code, OP_ALOAD);
            bc_emit_u1(mg->code, (uint8_t)lock_slot);
        }
        mg_push_object(mg, NULL);
        bc_emit(mg->code, OP_MONITOREXIT);
        mg_pop_typed(mg, 1);
    }
}

/**
 * Run every enclosing try statement's finally block, in innermost-first
 * order, right before a `return` leaves the method from inside one or
 * more of their try bodies - mirrors emit_pending_monitorexits() above
 * for synchronized statements. Without this, a `return` lexically inside
 * a try-with-finally (e.g. "try { return x; } finally { cleanup(); }")
 * skipped the finally block entirely: codegen_statement for
 * AST_RETURN_STMT emits the return instruction directly at that point in
 * the bytecode stream, never reaching the try statement's own inlined
 * "normal completion" copy of the finally block (which sits, unreached,
 * right after the return - and since it directly follows an
 * unconditional return, the verifier also rejects it for lacking a stack
 * frame there: "Expecting a stack map frame").
 *
 * Each finally block is re-generated (via codegen_statement) here, just
 * as the try statement's own codegen already does once per other exit
 * edge (normal completion, each catch clause, the exception handler) -
 * next_slot is saved/restored around each copy so a temp local the
 * finally block allocates itself (e.g. a nested synchronized statement's
 * lock slot) gets the same slot number as every other copy, matching the
 * established convention for those other copies (see AST_TRY_STMT).
 *
 * Does not (yet) run a finally block for a return from inside a catch
 * clause belonging to the same try/finally - only the try body itself is
 * covered, which is what an early return here can currently reach.
 *
 * `stop_depth` bounds how far up mg->finally_stack to walk: only the
 * innermost (finally_stack length - stop_depth) entries run. A `return`
 * always leaves the whole method, so it passes 0 (run everything
 * currently pending). A `break`/`continue` targeting a specific loop must
 * stop at that loop's own finally_depth (the stack's length when the loop
 * was entered) - a try statement that wraps the loop itself is never left
 * by breaking or continuing that loop, so its finally block must not run
 * here. Without this, `continue` inside a try-finally whose try body
 * simply CONTAINS the loop (continue's target is still inside the try)
 * incorrectly ran the finally block on every iteration - see gumdrop's
 * own HostsFile.parse(), whose "while ((line = reader.readLine()) !=
 * null) { ... if (line.isEmpty()) { continue; } ... }" sits inside a
 * "try { ... } finally { reader.close(); }": every `continue` closed the
 * reader early, so the very next readLine() threw (caught, logged, and
 * swallowed by the outer catch), silently truncating parse() to whatever
 * had been read before the first blank/comment line.
 */
static void emit_pending_finally_blocks(method_gen_t *mg, size_t stop_depth)
{
    size_t depth = slist_length(mg->finally_stack);
    for (slist_t *node = mg->finally_stack; node && depth > stop_depth;
         node = node->next, depth--) {
        ast_node_t *finally_block = (ast_node_t *)node->data;
        uint16_t saved_slot = mg->next_slot;
        codegen_statement(mg, finally_block);
        mg->next_slot = saved_slot;
    }
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

                    uint8_t return_op = OP_IRETURN;  /* Default to int */
                    bool return_kind_known = false;

                    /* The method's declared return type (or, for a lambda/method
                     * reference body, the SAM's) is authoritative when known: widen,
                     * box or unbox the value to match it, and take the return opcode
                     * from it directly, rather than guessing again from the
                     * expression's own type as the checks below do. Without this,
                     * "long f(int i) { return i; }" and "Integer g(int i) { return
                     * i; }" left the value as a plain int and returned with IRETURN. */
                    if (mg->method && mg->method->type) {
                        type_kind_t ret_kind = mg->method->type->kind;
                        const char *ret_class = (ret_kind == TYPE_CLASS) ?
                            mg->method->type->data.class_type.name : NULL;
                        type_kind_t from_kind;
                        const char *from_class;
                        value_kind_and_class(mg, return_expr, &from_kind, &from_class);
                        coerce_stack_value(mg, mg->cp, from_kind, from_class, ret_kind, ret_class);

                        switch (ret_kind) {
                            case TYPE_LONG:
                                return_op = OP_LRETURN;
                                return_kind_known = true;
                                break;
                            case TYPE_FLOAT:
                                return_op = OP_FRETURN;
                                return_kind_known = true;
                                break;
                            case TYPE_DOUBLE:
                                return_op = OP_DRETURN;
                                return_kind_known = true;
                                break;
                            case TYPE_CLASS:
                            case TYPE_ARRAY:
                            case TYPE_TYPEVAR:
                                return_op = OP_ARETURN;
                                return_kind_known = true;
                                break;
                            case TYPE_VOID:
                            case TYPE_UNKNOWN:
                                /* Not actually known after all (e.g. an unresolved
                                 * generic return); fall through to the heuristics
                                 * below, which is what happened before this check
                                 * existed. */
                                break;
                            default:
                                /* boolean, byte, char, short, int: all IRETURN */
                                return_op = OP_IRETURN;
                                return_kind_known = true;
                                break;
                        }
                    }

                    /* Check if return expression is a reference type */
                    if (return_kind_known) {
                        /* Already decided, and coerced, from the declared type above */
                    } else if (is_string_type(return_expr)) {
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
                                        if (kind == TYPE_CLASS || kind == TYPE_ARRAY || kind == TYPE_TYPEVAR) {
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
                            /* Check if we need to box a primitive to a wrapper type */
                            if (ret_kind == TYPE_CLASS && mg->method->type->data.class_type.name) {
                                const char *ret_class = mg->method->type->data.class_type.name;
                                type_kind_t expr_kind = get_expr_type_kind(mg, return_expr);
                                
                                /* Check if return type is a wrapper and expression is corresponding primitive */
                                if ((strcmp(ret_class, "java.lang.Integer") == 0 || strcmp(ret_class, "Integer") == 0) &&
                                    (expr_kind == TYPE_INT || expr_kind == TYPE_BYTE || expr_kind == TYPE_SHORT || expr_kind == TYPE_CHAR)) {
                                    emit_boxing(mg, mg->cp, TYPE_INT);
                                } else if ((strcmp(ret_class, "java.lang.Long") == 0 || strcmp(ret_class, "Long") == 0) &&
                                           expr_kind == TYPE_LONG) {
                                    emit_boxing(mg, mg->cp, TYPE_LONG);
                                } else if ((strcmp(ret_class, "java.lang.Float") == 0 || strcmp(ret_class, "Float") == 0) &&
                                           expr_kind == TYPE_FLOAT) {
                                    emit_boxing(mg, mg->cp, TYPE_FLOAT);
                                } else if ((strcmp(ret_class, "java.lang.Double") == 0 || strcmp(ret_class, "Double") == 0) &&
                                           expr_kind == TYPE_DOUBLE) {
                                    emit_boxing(mg, mg->cp, TYPE_DOUBLE);
                                } else if ((strcmp(ret_class, "java.lang.Boolean") == 0 || strcmp(ret_class, "Boolean") == 0) &&
                                           expr_kind == TYPE_BOOLEAN) {
                                    emit_boxing(mg, mg->cp, TYPE_BOOLEAN);
                                } else if ((strcmp(ret_class, "java.lang.Byte") == 0 || strcmp(ret_class, "Byte") == 0) &&
                                           expr_kind == TYPE_BYTE) {
                                    emit_boxing(mg, mg->cp, TYPE_BYTE);
                                } else if ((strcmp(ret_class, "java.lang.Short") == 0 || strcmp(ret_class, "Short") == 0) &&
                                           expr_kind == TYPE_SHORT) {
                                    emit_boxing(mg, mg->cp, TYPE_SHORT);
                                } else if ((strcmp(ret_class, "java.lang.Character") == 0 || strcmp(ret_class, "Character") == 0) &&
                                           expr_kind == TYPE_CHAR) {
                                    emit_boxing(mg, mg->cp, TYPE_CHAR);
                                }
                            }
                            return_op = OP_ARETURN;
                        } else if (ret_kind == TYPE_LONG) {
                            return_op = OP_LRETURN;
                        } else if (ret_kind == TYPE_FLOAT) {
                            return_op = OP_FRETURN;
                        } else if (ret_kind == TYPE_DOUBLE) {
                            return_op = OP_DRETURN;
                        }
                    }
                    
                    emit_pending_finally_blocks(mg, 0);
                    emit_pending_monitorexits(mg);
                    bc_emit(mg->code, return_op);
                    mg->last_opcode = return_op;
                    /* LRETURN/DRETURN consume a 2-slot value; popping a fixed 1
                     * here left the tracked stack permanently too deep whenever
                     * code follows the return in the same method (e.g. an early
                     * "if (x) return someLong;" before more statements). */
                    mg_pop_typed(mg, (return_op == OP_LRETURN || return_op == OP_DRETURN) ? 2 : 1);
                } else {
                    emit_pending_finally_blocks(mg, 0);
                    emit_pending_monitorexits(mg);
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
                            } else if (first->sem_type && first->sem_type->kind == TYPE_TYPEVAR) {
                                /* A generic type parameter (e.g. "T result = ...;")
                                 * is not a class at all - it must be erased to its
                                 * bound (or java.lang.Object if unbounded), exactly
                                 * as type_to_descriptor() already does for every
                                 * other TYPE_TYPEVAR use in codegen. Without this
                                 * branch, this fell straight through to the "sem_type
                                 * not available" fallback below, which used the
                                 * type variable's own bare source name ("T") as a
                                 * literal class name - the resulting classfile
                                 * referenced a nonexistent class "T", failing with
                                 * NoClassDefFoundError the first time that code
                                 * actually ran (bytecode verification doesn't check
                                 * that a referenced class exists, so this was never
                                 * caught until runtime class-loading). */
                                char *desc = type_to_descriptor(first->sem_type);
                                if (desc) {
                                    size_t len = strlen(desc);
                                    if (len >= 2 && desc[0] == 'L' && desc[len - 1] == ';') {
                                        char *erased_name = strdup(desc + 1);
                                        erased_name[len - 2] = '\0';
                                        class_name = erased_name;
                                    }
                                    free(desc);
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
                            if (!is_unnamed_name(name)) {
                                hashtable_insert(mg->locals, name, var_info);
                            }
                        }
                        
                        /* Record for LocalVariableTable (skip unnamed variables, JEP 456) */
                        if (!is_unnamed_name(name))
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
                            
                            /* Boxing, unboxing and widening: check if init expression
                             * produces a reference type before consulting get_expr_type_kind,
                             * which returns a primitive kind for wrapper classes too (for
                             * arithmetic purposes). */
                            bool init_is_ref = (init_expr->type == AST_NEW_OBJECT ||
                                               init_expr->type == AST_NEW_ARRAY ||
                                               (init_expr->sem_type &&
                                                (init_expr->sem_type->kind == TYPE_CLASS ||
                                                 init_expr->sem_type->kind == TYPE_ARRAY)));

                            if (!init_is_ref || !is_ref_type) {
                                type_kind_t init_kind;
                                const char *init_class;
                                value_kind_and_class(mg, init_expr, &init_kind, &init_class);
                                if (is_ref_type && class_name) {
                                    coerce_stack_value(mg, mg->cp, init_kind, init_class,
                                                       TYPE_CLASS, class_name);
                                } else if (!is_ref_type) {
                                    coerce_stack_value(mg, mg->cp, init_kind, init_class,
                                                       var_kind, NULL);
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
                
                /* Reset last_opcode before generating the then branch - it may
                 * otherwise carry over stale state from whatever code preceded
                 * this if-statement, which has nothing to do with whether THIS
                 * branch itself terminates. Mirrors the identical reset used
                 * before loop bodies and catch blocks. */
                mg->last_opcode = 0;

                /* Generate then branch */
                if (then_stmt && !codegen_statement(mg, then_stmt)) {
                    return false;
                }

                if (else_stmt) {
                    /* Check if then block ended with a return (don't need goto).
                     * Use mg->last_opcode (set correctly by every statement kind
                     * that can genuinely guarantee termination, e.g. a plain
                     * return/throw, or explicitly reset to 0 by e.g. an
                     * if-without-else whose own false path doesn't terminate)
                     * rather than inspecting the raw last emitted byte: an
                     * if-without-else whose then-body itself ends in a
                     * return/throw has NO bytecode at all for its own (empty)
                     * false path, so the literal last byte in the code array
                     * is still that inner return/throw's opcode even though
                     * the if-without-else AS A WHOLE does not unconditionally
                     * terminate - falsely marking this (outer) branch as
                     * terminating and skipping the goto past the else branch
                     * below, corrupting control flow into the else branch's
                     * own code (VerifyError: "Control flow falls through code
                     * end", since the method's own final statement's implicit
                     * return then never gets generated either). */
                    bool then_ends_with_return = (mg->last_opcode == OP_RETURN ||
                                                  mg->last_opcode == OP_IRETURN ||
                                                  mg->last_opcode == OP_LRETURN ||
                                                  mg->last_opcode == OP_FRETURN ||
                                                  mg->last_opcode == OP_DRETURN ||
                                                  mg->last_opcode == OP_ARETURN ||
                                                  mg->last_opcode == OP_ATHROW ||
                                                  /* A `break`/`continue` ending the then
                                                   * branch (e.g. inside a loop's own
                                                   * if/else) emits OP_GOTO and, just like
                                                   * return/throw, never falls through to
                                                   * after this if-statement either - the
                                                   * loop-exit/back-edge jump IS its only
                                                   * exit. Without this, the join-point
                                                   * "goto past else" below got emitted
                                                   * anyway, landing directly after that
                                                   * break's own goto: dead code with no
                                                   * stack map frame (VerifyError:
                                                   * "Expecting a stack map frame"),
                                                   * confirmed against gumdrop's own
                                                   * DnsMessage.decodeName(), whose
                                                   * compression-pointer branch of an
                                                   * if/else ends with `break;`. mg-
                                                   * >last_opcode is freshly reset to 0
                                                   * immediately before this branch is
                                                   * generated (see the reset above), so
                                                   * an empty/non-terminating branch can
                                                   * never leave a stale OP_GOTO here by
                                                   * accident - matching the equivalent,
                                                   * already-fixed OP_GOTO check for a
                                                   * catch clause's own ends-with-return
                                                   * test elsewhere in this file. */
                                                  mg->last_opcode == OP_GOTO);

                    size_t goto_pos = 0;
                    /* Snapshot the stackmap state as it stands right after the
                     * then branch, before it gets reset for the else branch
                     * below. If the else branch terminates (return/throw), the
                     * goto emitted here is the *only* live edge into the join
                     * point, so the join frame must reflect what's true at
                     * this goto - not whatever mg->stackmap happens to track
                     * after generating (possibly deeply nested) else code,
                     * which may have reset locals like a blank final assigned
                     * in the then branch back to Top while framing an inner
                     * throw/return branch of its own. */
                    stackmap_state_t *then_exit_state = NULL;
                    if (!then_ends_with_return) {
                        /* Save position for goto past else */
                        goto_pos = mg->code->length;
                        bc_emit(mg->code, OP_GOTO);
                        bc_emit_u2(mg->code, 0);  /* Placeholder */
                        if (mg->stackmap) {
                            then_exit_state = stackmap_save_state(mg->stackmap);
                        }
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

                    /* Reset last_opcode before generating the else branch - see
                     * the identical reset (and its full reasoning) before the
                     * then branch above. */
                    mg->last_opcode = 0;

                    /* Generate else branch */
                    if (!codegen_statement(mg, else_stmt)) {
                        stackmap_state_free(pre_then_state);
                        stackmap_state_free(then_exit_state);
                        return false;
                    }

                    /* Check if else block ended with a return - see the
                     * identical mg->last_opcode-based check (and its full
                     * reasoning) for the then branch above. */
                    bool else_ends_with_return = (mg->last_opcode == OP_RETURN ||
                                                  mg->last_opcode == OP_IRETURN ||
                                                  mg->last_opcode == OP_LRETURN ||
                                                  mg->last_opcode == OP_FRETURN ||
                                                  mg->last_opcode == OP_DRETURN ||
                                                  mg->last_opcode == OP_ARETURN ||
                                                  mg->last_opcode == OP_ATHROW ||
                                                  /* See then_ends_with_return's identical
                                                   * OP_GOTO case above - a break/continue
                                                   * ending the else branch is the same
                                                   * situation, mirrored. */
                                                  mg->last_opcode == OP_GOTO);
                    
                    /* Patch goto to end (only if we emitted one) */
                    if (!then_ends_with_return) {
                        int16_t end_offset = (int16_t)(mg->code->length - goto_pos);
                        mg->code->code[goto_pos + 1] = (end_offset >> 8) & 0xFF;
                        mg->code->code[goto_pos + 2] = end_offset & 0xFF;
                        
                        /* At join point, restore only the next_slot/locals_count to exclude
                         * variables allocated inside the branches. But DO NOT restore the
                         * stackmap local types - variables declared BEFORE the if that are
                         * assigned inside BOTH branches should retain their assigned type,
                         * not revert to Top. The else branch's final state is valid since
                         * any variables declared inside else will go out of scope anyway -
                         * UNLESS the else branch itself terminates (return/throw), in which
                         * case the goto from the then branch is the *only* live edge into
                         * this join point, and mg->stackmap's current state reflects
                         * whatever the (possibly nested) else branch left behind while
                         * framing its own internal control flow - not what's true at the
                         * goto. Use the then-exit snapshot instead in that case. */
                        if (pre_then_state && mg->stackmap) {
                            if (else_ends_with_return && then_exit_state) {
                                stackmap_restore_state(mg->stackmap, then_exit_state);
                            }
                            /* Only restore the slot allocation, not the stackmap types */
                            mg_restore_locals_count(mg, pre_then_locals_count);
                            mg->next_slot = pre_then_slot;
                        }

                        /* Record frame at end of if-else (join point) */
                        mg_record_frame(mg);
                    }
                    stackmap_state_free(then_exit_state);

                    /* If both branches terminate, the if-else terminates
                     * Otherwise, reset last_opcode */
                    if (then_ends_with_return && else_ends_with_return) {
                        mg->last_opcode = OP_IRETURN;  /* Mark as terminating */
                    } else {
                        mg->last_opcode = 0;
                    }
                } else {
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
                
                /* Generate condition as a direct branch to loop_end on
                 * false, rather than materializing a 0/1 value and doing
                 * a single ifeq on it - a top-level `&&` chain (e.g.
                 * `guard && (x = next()) != null`) is special-cased so
                 * the loop body is only ever reached via the "every
                 * operand true" edge, keeping any local an operand
                 * assigns (like `x`) at its real, narrowed type within
                 * the body - see codegen_condition_and_chain_false_branch's
                 * own comment for the full reasoning. */
                slist_t *false_positions = NULL;
                if (!codegen_condition_and_chain_false_branch(mg, mg->cp, condition, &false_positions)) {
                    return false;
                }

                /* Reset last_opcode before generating the body - otherwise it
                 * carries over stale state from whatever code preceded this
                 * loop (e.g. an earlier if-branch's own OP_IRETURN), which
                 * has nothing to do with whether THIS body itself ends in a
                 * jump, and would wrongly mark it as terminal below if the
                 * body's own last statement doesn't itself set/reset
                 * last_opcode (as most plain statements don't) - silently
                 * skipping the back-edge goto and turning the loop into a
                 * single-iteration fall-through. Mirrors the identical reset
                 * already used for catch blocks. */
                mg->last_opcode = 0;

                /* Generate body */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }

                /* Patch every `continue` in the body (each had to emit its
                 * own placeholder goto before this loop's continue_target
                 * was knowable to it - see mg_add_continue_to_context()'s
                 * comment) to jump to loop_start - a while loop's own real
                 * continue target, unlike a for-loop/do-while/array-based
                 * enhanced-for, since there's no separate update step: it's
                 * simply the condition re-check already recorded as
                 * loop_start at mg_push_loop() above, just confirmed (not
                 * changed) here now that the real value is safe to patch
                 * with. */
                if (mg->loop_stack) {
                    mg_patch_continue_offsets(mg, (loop_context_t *)mg->loop_stack->data,
                                               loop_start);
                }

                /* Check if body ended with an unconditional branch (break/continue/return/throw).
                 * If so, the back-edge is unreachable and should be skipped. */
                bool body_ends_with_jump = (mg->last_opcode == OP_GOTO ||
                                           mg->last_opcode == OP_RETURN ||
                                           mg->last_opcode == OP_ARETURN ||
                                           mg->last_opcode == OP_IRETURN ||
                                           mg->last_opcode == OP_LRETURN ||
                                           mg->last_opcode == OP_FRETURN ||
                                           mg->last_opcode == OP_DRETURN ||
                                           mg->last_opcode == OP_ATHROW);

                /* Restore locals count before goto back to loop_start.
                 * This ensures the frame at loop_start doesn't include loop-local variables. */
                mg_restore_locals_count(mg, saved_locals_count);
                
                /* goto loop_start (back-edge) - only if body doesn't end with unconditional jump */
                if (!body_ends_with_jump) {
                    int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, back_offset);
                }
                
                /* Patch every pending false-branch (one per operand of a
                 * top-level `&&` chain) to here (loop_end). They arrive
                 * with genuinely different local-variable state whenever
                 * a later operand assigned a local the earlier operand(s)
                 * never touched (e.g. `x` in
                 * `guard && (x = next()) != null`) - the head entry
                 * (chronologically first/leftmost operand) is always a
                 * safe, conservative frame for the WHOLE group, since
                 * every later operand's own local writes can only refine
                 * (assign a value whose static type is compatible with)
                 * whatever the head's snapshot already recorded - so
                 * restore from just that one rather than trusting
                 * mg->stackmap's ambient state (which by now reflects
                 * the LOOP BODY's own last statement, an edge that
                 * doesn't even reach loop_end when the body has no
                 * break). */
                size_t loop_end = mg->code->length;
                stackmap_state_t *loop_exit_state = NULL;
                for (slist_t *node = false_positions; node; ) {
                    slist_t *next = node->next;
                    pending_condition_branch_t *pending = (pending_condition_branch_t *)node->data;
                    bc_patch_u2(mg->code, pending->branch_pos + 1,
                                (uint16_t)(int16_t)(loop_end - pending->branch_pos));
                    if (!loop_exit_state) {
                        loop_exit_state = pending->state;
                    } else {
                        stackmap_state_free(pending->state);
                    }
                    free(pending);
                    free(node);
                    node = next;
                }
                if (loop_exit_state && mg->stackmap) {
                    stackmap_restore_locals_only(mg->stackmap, loop_exit_state);
                }

                /* Record frame at loop end (break target) */
                mg_record_frame(mg);
                stackmap_state_free(loop_exit_state);

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
                
                /* Reset last_opcode before generating the body - see the
                 * identical reset (and its full reasoning) in AST_WHILE_STMT
                 * just above. */
                mg->last_opcode = 0;

                /* Generate body first */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }

                /* Check if body ended with an unconditional branch */
                bool body_ends_with_terminal = (mg->last_opcode == OP_RETURN ||
                                                mg->last_opcode == OP_ARETURN ||
                                                mg->last_opcode == OP_IRETURN ||
                                                mg->last_opcode == OP_LRETURN ||
                                                mg->last_opcode == OP_FRETURN ||
                                                mg->last_opcode == OP_DRETURN ||
                                                mg->last_opcode == OP_ATHROW);
                /* A `continue` anywhere earlier in the body (on some path
                 * OTHER than the one that made the body's own last
                 * physical statement a return/throw) still needs the
                 * condition check below to actually run -
                 * body_ends_with_terminal only tells us the FALL-THROUGH
                 * exit is dead, not that every path through the body is.
                 * Capture whether any such `continue` was registered
                 * before mg_patch_continue_offsets() consumes the list. */
                loop_context_t *do_ctx = mg->loop_stack ? (loop_context_t *)mg->loop_stack->data : NULL;
                bool has_pending_continue = do_ctx && do_ctx->continue_offsets != NULL;

                /* Restore locals count before recording the continue-target
                 * frame below - body-declared locals shouldn't appear in
                 * it (they're never live at this point, no matter which
                 * incoming edge - fall-through or a `continue` from deeper
                 * in the body - reaches it), mirroring the identical
                 * restore-then-record pattern used for loop_start/loop_end
                 * elsewhere in this file. */
                mg_restore_locals_count(mg, saved_locals_count);

                /* Finalize continue target to condition check point, and
                 * patch every `continue` in the body (which had to emit
                 * its own goto before this point was known, from
                 * potentially deeper in the body than this restore
                 * reflects - fine, see the restore comment above) to jump
                 * here - see mg_patch_continue_offsets()'s comment. This is
                 * now a genuine branch target reachable from anywhere in
                 * the body via `continue`, not just when the body happens
                 * to end with its own goto, so the frame below is
                 * unconditional. */
                if (do_ctx) {
                    mg_patch_continue_offsets(mg, do_ctx, mg->code->length);
                }
                mg_record_frame(mg);

                /* Skip condition only if body ends with return/throw AND
                 * no `continue` anywhere in the body needs to reach this
                 * point - otherwise (see comment above) it's still a real,
                 * reachable branch target that must run the condition
                 * check and loop back, even though the immediately
                 * preceding fall-through into it is itself dead. */
                if (!body_ends_with_terminal || has_pending_continue) {
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
                }
                
                /* Record frame at loop end (break target). See the matching
                 * comment on AST_FOR_STMT loop-end frame recording: any
                 * dangling frame this produces (loop has no break, is the
                 * methods last statement) is pruned centrally at method
                 * finalization (stackmap_prune_out_of_bounds_frame in
                 * codegen.c), not predicted here. */
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
                
                /* Generate initializer (skip if empty placeholder) */
                if (init && init->type != AST_EMPTY_STMT) {
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
                if (condition && condition->type != AST_EMPTY_STMT) {
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
                
                /* Reset last_opcode before generating the body - see the
                 * identical reset (and its full reasoning) in AST_WHILE_STMT
                 * above. */
                mg->last_opcode = 0;

                /* Generate body */
                if (body && !codegen_statement(mg, body)) {
                    return false;
                }

                /* Check if body ended with an unconditional branch.
                 * If it's break/return/throw, the update and back-edge are unreachable.
                 * If it's continue (goto to continue_target), the update/back-edge are still reachable
                 * because continue_target is set below. But the fall-through is dead. */
                bool body_ends_with_terminal = (mg->last_opcode == OP_RETURN ||
                                                mg->last_opcode == OP_ARETURN ||
                                                mg->last_opcode == OP_IRETURN ||
                                                mg->last_opcode == OP_LRETURN ||
                                                mg->last_opcode == OP_FRETURN ||
                                                mg->last_opcode == OP_DRETURN ||
                                                mg->last_opcode == OP_ATHROW);
                /* A `continue` anywhere earlier in the body (on some path
                 * OTHER than the one that made the body's own last
                 * physical statement a return/throw) still needs the
                 * update expression and back-edge below to actually run -
                 * body_ends_with_terminal only tells us the FALL-THROUGH
                 * exit is dead, not that every path through the body is.
                 * Capture whether any such `continue` was registered
                 * before mg_patch_continue_offsets() consumes the list. */
                loop_context_t *for_ctx = mg->loop_stack ? (loop_context_t *)mg->loop_stack->data : NULL;
                bool has_pending_continue = for_ctx && for_ctx->continue_offsets != NULL;

                /* Restore locals count before recording the continue-target
                 * frame below - body-declared locals shouldn't appear in
                 * it, mirroring the identical restore-then-record pattern
                 * used for loop_start/loop_end elsewhere in this file (a
                 * `continue` reached from deeper in the body, where more
                 * locals may be declared, is still safely "assignable to"
                 * a target frame declaring fewer locals - the same
                 * reasoning already relied on for this loop's own break
                 * target). */
                mg_restore_locals_count(mg, saved_locals_count);

                /* Finalize continue target to here (before update
                 * expression), and patch every `continue` in the body to
                 * jump here - see mg_patch_continue_offsets()'s comment.
                 * This is now a genuine branch target reachable from
                 * anywhere in the body via `continue`, not just when the
                 * body happens to end with its own goto, so the frame
                 * below is unconditional. */
                if (for_ctx) {
                    mg_patch_continue_offsets(mg, for_ctx, mg->code->length);
                }
                mg_record_frame(mg);

                /* Skip update and back-edge only if body ends with
                 * return/throw AND no `continue` anywhere in the body
                 * needs to reach this point - otherwise (see comment
                 * above) it's still a real, reachable branch target that
                 * must run the update and loop back, even though the
                 * immediately preceding fall-through into it is itself
                 * dead. */
                if (!body_ends_with_terminal || has_pending_continue) {
                    /* Generate update (skip if empty placeholder) */
                    if (update && update->type != AST_EMPTY_STMT) {
                        /* Track stack depth before/after, mirroring AST_EXPR_STMT's
                         * own identical pop logic - an update clause on a long/
                         * double loop variable (e.g. "for (long i = 0; ...; i++)")
                         * leaves a category-2 (2-word) value that a single,
                         * unconditional OP_POP can't fully remove, corrupting the
                         * rest of the stack (VerifyError: "Bad type on operand
                         * stack", a stray long_2nd left behind). */
                        uint16_t stack_before = mg->stack_depth;
                        if (!codegen_expr(mg, update, mg->cp)) {
                            return false;
                        }
                        uint16_t slots_to_pop = mg->stack_depth - stack_before;
                        if (slots_to_pop >= 2) {
                            bc_emit(mg->code, OP_POP2);
                            mg_pop_typed(mg, 2);
                        } else if (slots_to_pop == 1) {
                            bc_emit(mg->code, OP_POP);
                            mg_pop_typed(mg, 1);
                        }
                    }
                    
                    /* Restore locals count before goto back to loop_start.
                     * This ensures the frame at loop_start doesn't include body-local variables. */
                    mg_restore_locals_count(mg, saved_locals_count);
                    
                    /* goto loop_start */
                    int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, back_offset);
                }
                
                /* loop_end: */
                size_t loop_end = mg->code->length;

                /* Record frame at loop end (break target). Note: if this
                 * loop has no break at all AND is the very last thing
                 * generated for the enclosing method (nothing follows this
                 * position, ever), this frame would dangle past the
                 * methods actual final instruction - handled centrally by
                 * stackmap_prune_out_of_bounds_frame() at method
                 * finalization (see codegen.c), rather than predicted here
                 * (this code cannot know in advance whether an implicit
                 * trailing return, or more enclosing statements, will end
                 * up following this exact position). */
                mg_record_frame(mg);

                /* Patch forward branch if we have a condition.
                 * "condition" is non-NULL even for "for (;;)" - the parser
                 * always fills an omitted condition with an AST_EMPTY_STMT
                 * placeholder, never NULL - so this guard must match the
                 * one above (line ~2187) that decides whether the ifeq
                 * placeholder was actually emitted at all (branch_pos left
                 * at its initial 0 otherwise). Without this check, a
                 * "for (;;)" loop patches a branch-target value into
                 * mg->code->code[1]/[2] - i.e. bytes 1-2 of the METHOD
                 * ITSELF - clobbering whatever real instruction happens to
                 * start there. */
                if (condition && condition->type != AST_EMPTY_STMT) {
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
                
                /* Run any enclosing try statement's finally block(s) before
                 * actually jumping - see emit_pending_finally_blocks()'s
                 * own comment. A `break` lexically inside a try-with-
                 * finally (e.g. gumdrop's own ScheduledTimer.run(), whose
                 * main loop's try body both breaks and continues out past
                 * a "finally { lock.unlock(); }") previously skipped the
                 * finally block entirely on this path - the same class of
                 * bug already fixed for `return`, just for a different
                 * exit statement. */
                emit_pending_finally_blocks(mg, target_ctx->finally_depth);

                /* Emit goto with placeholder offset */
                size_t break_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* Will be patched by mg_pop_loop */
                mg->last_opcode = OP_GOTO;  /* Track for dead code detection */

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
                        if (!is_unnamed_name(var_name)) {
                            hashtable_insert(mg->locals, var_name, loop_var_info);
                        }
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
                    
                    /* Finalize continue target to here (before __idx++),
                     * and patch every `continue` in the body to jump here -
                     * see mg_patch_continue_offsets()'s comment. */
                    if (arr_loop_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, arr_loop_entry_state);
                    }
                    if (mg->loop_stack) {
                        mg_patch_continue_offsets(mg, (loop_context_t *)mg->loop_stack->data,
                                                   mg->code->length);
                    }
                    mg_record_frame(mg);

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
                    
                    /* Allocate loop variable. Usually a single-slot
                     * reference (what Iterable always yields before any
                     * unboxing), but a primitive long/double loop variable
                     * (e.g. "for (long v : list)" auto-unboxing a
                     * List<Long>) needs the usual two JVM slots once
                     * unboxed, exactly like any other wide local. */
                    int var_size = (var_kind == TYPE_LONG || var_kind == TYPE_DOUBLE) ? 2 : 1;
                    uint16_t var_slot = mg->next_slot;
                    mg->next_slot += var_size;
                    if (mg->next_slot > mg->max_locals) {
                        mg->max_locals = mg->next_slot;
                    }
                    
                    /* Create local variable info for Iterable loop variable */
                    local_var_info_t *iter_var_info = local_var_info_new(var_slot, var_kind);
                    if (iter_var_info) {
                        iter_var_info->is_ref = var_is_ref;
                        if (!is_unnamed_name(var_name)) {
                            hashtable_insert(mg->locals, var_name, iter_var_info);
                        }
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
                    
                    /* Add checkcast to the loop variable type (Iterator.next() returns Object).
                     * "array_desc" holds the loop variable's own full JVM array
                     * descriptor (e.g. "[B") when it's declared as an array
                     * type - shared below for the stackmap update too, since
                     * both need the identical descriptor string. */
                    char *array_desc = NULL;
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
                    } else if (type_node && type_node->type == AST_ARRAY_TYPE) {
                        /* Loop variable declared as an array type over a
                         * plain Iterable/Collection (e.g. "for (byte[] v :
                         * list)" where list is a List<byte[]>) - "is_array"
                         * above is about the ITERABLE, not the loop
                         * variable, so this goes through the Iterator-based
                         * path here, not the array-source path further up.
                         * Iterator.next() still only returns Object; without
                         * a CHECKCAST down to the real array type, a later
                         * use of the loop variable expecting an exact array
                         * type (e.g. passing it to "new String(byte[],
                         * Charset)") finds a bare Object on the stack
                         * instead (VerifyError: "Bad type on operand
                         * stack"), confirmed against gumdrop's own
                         * SearchResultEntry.getAttributeStringValues(),
                         * which does exactly this over a List<byte[]>.
                         * JVMS 4.4.1: an array type's own CHECKCAST class
                         * constant is its full descriptor (e.g. "[B"), not
                         * an unwrapped internal name.
                         *
                         * This loop variable's own type_node is NOT
                         * semantically annotated with a sem_type the way an
                         * ordinary local variable declaration's type node is
                         * (confirmed by instrumenting this exact spot) - so,
                         * unlike the CLASS_TYPE branch just above, this
                         * walks the AST_ARRAY_TYPE chain manually (same
                         * technique as the local-variable-declaration case
                         * a little earlier in this file) instead of relying
                         * on it. */
                        int dims = 0;
                        ast_node_t *cur = type_node;
                        while (cur && cur->type == AST_ARRAY_TYPE) {
                            dims++;
                            cur = cur->data.node.children ?
                                (ast_node_t *)cur->data.node.children->data : NULL;
                        }
                        char elem_desc[256] = "Ljava/lang/Object;";
                        if (cur && cur->type == AST_CLASS_TYPE) {
                            const char *cname = (cur->sem_type && cur->sem_type->kind == TYPE_CLASS &&
                                                  cur->sem_type->data.class_type.name) ?
                                cur->sem_type->data.class_type.name : cur->data.node.name;
                            if (cname) {
                                const char *resolved = resolve_java_lang_class(cname);
                                char *internal = class_to_internal_name(resolved);
                                snprintf(elem_desc, sizeof(elem_desc), "L%s;", internal);
                                free(internal);
                            }
                        } else if (cur && cur->type == AST_PRIMITIVE_TYPE && cur->data.leaf.name) {
                            const char *p = cur->data.leaf.name;
                            const char *d = "I";
                            if (strcmp(p, "byte") == 0) d = "B";
                            else if (strcmp(p, "short") == 0) d = "S";
                            else if (strcmp(p, "char") == 0) d = "C";
                            else if (strcmp(p, "long") == 0) d = "J";
                            else if (strcmp(p, "float") == 0) d = "F";
                            else if (strcmp(p, "double") == 0) d = "D";
                            else if (strcmp(p, "boolean") == 0) d = "Z";
                            snprintf(elem_desc, sizeof(elem_desc), "%s", d);
                        }
                        char full_desc[300];
                        int i = 0;
                        for (; i < dims && i < 250; i++) {
                            full_desc[i] = '[';
                        }
                        snprintf(full_desc + i, sizeof(full_desc) - (size_t)i, "%s", elem_desc);
                        array_desc = strdup(full_desc);
                        uint16_t class_idx = cp_add_class(mg->cp, array_desc);
                        bc_emit(mg->code, OP_CHECKCAST);
                        bc_emit_u2(mg->code, class_idx);
                    } else if (type_node && type_node->type == AST_PRIMITIVE_TYPE) {
                        /* Loop variable declared as a primitive type over a
                         * plain Iterable/Collection (e.g. "for (int v :
                         * list)" where list is a List<Integer>) - JLS 14.14.2
                         * requires this to auto-unbox each element, exactly
                         * like an ordinary assignment of a boxed value to a
                         * primitive-typed variable. Iterator.next() only
                         * returns Object, so - unlike the CLASS_TYPE/
                         * ARRAY_TYPE branches above, which just need a
                         * reference-to-reference CHECKCAST - this needs a
                         * CHECKCAST down to the primitive's own wrapper class
                         * (matching real javac's own emitted bytecode
                         * exactly: checkcast Integer; invokevirtual
                         * intValue()) before unboxing, or the JVM verifier
                         * rejects the wrapper's own unboxing method call
                         * (intValue()/longValue()/etc) as not being declared
                         * on Object (VerifyError: "Bad type on operand
                         * stack"). Without any of this, the previous
                         * behavior stored the raw boxed reference straight
                         * into a primitive-typed local slot instead. */
                        const char *wrapper = wrapper_class_for_primitive(var_kind);
                        if (wrapper) {
                            uint16_t class_idx = cp_add_class(mg->cp, wrapper);
                            bc_emit(mg->code, OP_CHECKCAST);
                            bc_emit_u2(mg->code, class_idx);
                            emit_unboxing(mg, mg->cp, var_kind, wrapper);
                        }
                    }

                    /* Store to loop variable (reference type from Iterable,
                     * unless just unboxed to a primitive above) */
                    mg_emit_store_local(mg, var_slot, var_kind);

                    /* Update stackmap for loop variable */
                    if (mg->stackmap) {
                        if (array_desc) {
                            stackmap_set_local_object(mg->stackmap, var_slot, mg->cp, array_desc);
                        } else if (var_kind == TYPE_LONG) {
                            stackmap_set_local_long(mg->stackmap, var_slot);
                        } else if (var_kind == TYPE_DOUBLE) {
                            stackmap_set_local_double(mg->stackmap, var_slot);
                        } else if (var_kind == TYPE_FLOAT) {
                            stackmap_set_local_float(mg->stackmap, var_slot);
                        } else if (var_kind == TYPE_INT || var_kind == TYPE_BOOLEAN ||
                                   var_kind == TYPE_BYTE || var_kind == TYPE_SHORT ||
                                   var_kind == TYPE_CHAR) {
                            stackmap_set_local_int(mg->stackmap, var_slot);
                        } else {
                            /* For Iterable, a non-array, non-primitive loop
                             * variable is always reference type */
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
                    }
                    free(array_desc);

                    /* Reset last_opcode before generating the body - see the
                     * identical reset (and its full reasoning) in
                     * AST_WHILE_STMT. */
                    mg->last_opcode = 0;

                    /* body */
                    if (body && !codegen_statement(mg, body)) {
                        return false;
                    }

                    /* Check if body ended with an unconditional jump */
                    bool body_ends_with_jump = (mg->last_opcode == OP_GOTO ||
                                               mg->last_opcode == OP_RETURN ||
                                               mg->last_opcode == OP_ARETURN ||
                                               mg->last_opcode == OP_IRETURN ||
                                               mg->last_opcode == OP_LRETURN ||
                                               mg->last_opcode == OP_FRETURN ||
                                               mg->last_opcode == OP_DRETURN ||
                                               mg->last_opcode == OP_ATHROW);
                    
                    /* Finalize continue target to loop_start (not "here" -
                     * unlike a for-loop/do-while/array-based enhanced-for,
                     * there's no separate update step, so the real continue
                     * target is simply the hasNext()/next() re-check
                     * already recorded as loop_start at mg_push_loop()
                     * above - confirmed, not changed, here), and patch
                     * every `continue` in the body to jump there. Reusing
                     * the already-framed loop_start (rather than "just
                     * before the back-edge goto", a position with no frame
                     * of its own) avoids needing a whole new stack map
                     * frame here, mirroring AST_WHILE_STMT's identical
                     * choice. */
                    if (mg->loop_stack) {
                        mg_patch_continue_offsets(mg, (loop_context_t *)mg->loop_stack->data,
                                                   loop_start);
                    }

                    /* Only generate back-edge if body doesn't end with unconditional jump */
                    if (!body_ends_with_jump) {
                        /* goto loop_start */
                        int16_t back_offset = (int16_t)(loop_start - mg->code->length);
                        bc_emit(mg->code, OP_GOTO);
                        bc_emit_u2(mg->code, back_offset);
                    }
                    
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
                
                /* Run any enclosing try statement's finally block(s)
                 * before actually jumping - see AST_BREAK_STMT's matching
                 * comment and emit_pending_finally_blocks() itself. Must
                 * happen before computing the branch position below, since
                 * inlining the finally block's own bytecode here shifts
                 * mg->code->length. */
                emit_pending_finally_blocks(mg, target_ctx->finally_depth);

                /* Emit a placeholder goto and defer patching its real
                 * offset until the target loop's real continue_target is
                 * known (mg_patch_continue_offsets(), called by each loop
                 * construct once its body has been fully generated).
                 * Computing the offset directly here, against whatever
                 * target_ctx->continue_target happens to hold right now,
                 * is only correct for a loop whose continue target is
                 * simply its own condition re-check (while; the Iterable
                 * form of an enhanced-for) - a for-loop, do-while, or the
                 * array form of an enhanced-for all have a separate
                 * update/re-check step AFTER the body that continue must
                 * reach instead, whose bytecode position doesn't exist
                 * yet at this point (we're still generating the body).
                 * Deferring uniformly, the same way AST_BREAK_STMT already
                 * defers its own (forward) jump via
                 * mg_add_break_to_context(), keeps this correct regardless
                 * of which loop construct is involved. */
                size_t continue_pos = mg->code->length;
                bc_emit(mg->code, OP_GOTO);
                bc_emit_u2(mg->code, 0);  /* placeholder, backpatched later */
                mg->last_opcode = OP_GOTO;  /* Track for dead code detection */
                mg_add_continue_to_context(target_ctx, continue_pos);

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
                
                /* Check if this is an enum switch (selector has sem_type of enum).
                 * selector->sem_type isn't set for every selector expression
                 * shape - get_expression_type()'s AST_ARRAY_ACCESS case (among
                 * others) never stores its result back onto the node it
                 * resolved, unlike AST_IDENTIFIER, so `switch (modes[i])`
                 * left selector->sem_type NULL even though `switch (aLocal)`
                 * worked fine. Semantic analysis's own switch-statement
                 * handling always stores the resolved enum type on the switch
                 * statement node itself (stmt->sem_type) whenever the
                 * selector genuinely is an enum, regardless of its expression
                 * shape - fall back to that. */
                type_t *enum_switch_type = selector->sem_type;
                if (!enum_switch_type || enum_switch_type->kind != TYPE_CLASS ||
                    !enum_switch_type->data.class_type.symbol ||
                    enum_switch_type->data.class_type.symbol->kind != SYM_ENUM) {
                    enum_switch_type = stmt->sem_type;
                }
                bool is_enum_switch = (enum_switch_type &&
                                       enum_switch_type->kind == TYPE_CLASS &&
                                       enum_switch_type->data.class_type.symbol &&
                                       enum_switch_type->data.class_type.symbol->kind == SYM_ENUM);
                
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

                /* Save stackmap state at switch entry (selector consumed,
                 * no case body run yet) - every case label is a jump
                 * target reached *only* from the lookupswitch dispatch
                 * itself (assuming no fallthrough between cases), so each
                 * one's frame must reflect this entry state, not whatever
                 * mg->stackmap happens to track after generating whichever
                 * earlier case in AST order was compiled last. Without
                 * this, a local declared before the switch and assigned in
                 * every case (e.g. "PosixFilePermission needed;") looked
                 * assigned to every case *after* the first one that
                 * actually assigns it, but still unassigned (Top) at the
                 * first case's own declared frame - the same class of bug
                 * already fixed for AST_TRY_STMT/AST_SYNCHRONIZED_STMT's
                 * exception handlers. */
                stackmap_state_t *switch_entry_state = NULL;
                if (mg->stackmap) {
                    switch_entry_state = stackmap_save_state(mg->stackmap);
                }

                /* Snapshots of every state that actually reaches the
                 * switch's shared exit point (every `break`, from any
                 * case) - collected below as each case is generated, and
                 * merged into the single frame recorded there. See
                 * merge_stackmap_states_into()'s own doc comment. */
                slist_t *switch_exit_states = NULL;

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
                                    /* A CHAR literal's value lives in
                                     * value.str_val (a 1-byte string, e.g.
                                     * "a" - see ast_new_literal_from_lexer(),
                                     * which never populates value.int_val for
                                     * TOK_CHAR_LITERAL), not value.int_val -
                                     * unlike every other numeric literal kind
                                     * here (int/long, and true/false as 1/0).
                                     * value is a union, so blindly reading
                                     * int_val for a char literal case label
                                     * (e.g. "case '*':") read the str_val
                                     * pointer's own bit pattern reinterpreted
                                     * as an int instead of the character's
                                     * code point - a wildly wrong lookupswitch
                                     * key that could never match any actual
                                     * switch value, silently routing every
                                     * char literal case label straight to
                                     * default. Confirmed against gumdrop's
                                     * own LdapRealm.escapeLDAPFilter(), whose
                                     * switch(char) on '\\', '*', '(', ')' (and
                                     * '\u0000') never matched any of them. */
                                    if (case_expr->data.leaf.token_type == TOK_CHAR_LITERAL) {
                                        const char *sv = case_expr->data.leaf.value.str_val;
                                        case_values[case_idx] = sv ? (int32_t)(unsigned char)sv[0] : 0;
                                    } else {
                                        case_values[case_idx] = (int32_t)case_expr->data.leaf.value.int_val;
                                    }
                                } else if (case_expr->type == AST_IDENTIFIER) {
                                    /* semantic.c already resolved this case label's
                                     * own constant value onto its leaf, whether it's
                                     * an enum switch (an enum constant's ordinal) or
                                     * a plain int/char/byte/short switch (a named
                                     * "static final int FOO = n;" constant's own
                                     * literal value) - not gating on is_enum_switch
                                     * here anymore, since both are populated the
                                     * same way and this needs no different handling
                                     * for either. */
                                    case_values[case_idx] = (int32_t)case_expr->data.leaf.value.int_val;
                                } else {
                                    /* A constant EXPRESSION case label (e.g.
                                     * "case ('U' << 24) | 'S':") - see
                                     * eval_int_constant_expr()'s own doc
                                     * comment. A value that fails to
                                     * evaluate (not actually a compile-time
                                     * constant - shouldn't happen for code
                                     * that got this far past semantic
                                     * analysis) leaves this case's match
                                     * value at 0, same as before this branch
                                     * existed. */
                                    int32_t value;
                                    if (eval_int_constant_expr(case_expr, &value)) {
                                        case_values[case_idx] = value;
                                    }
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
                /* Whether the previously-generated case body is guaranteed
                 * not to fall through into the next one (ends in break's
                 * goto, or return/throw) - true before the first case,
                 * since nothing precedes it. Only restore to switch-entry
                 * state when this holds: a genuinely falling-through case
                 * (no break) reaches the next label with real, more-
                 * specific state than switch entry (e.g. a local the
                 * previous case just assigned), and resetting that to
                 * "unassigned" would be wrong for that path - the existing
                 * (unfixed) merge behavior is left alone for that case. */
                bool prev_case_terminates = true;

                /* Track whether the switch AS A WHOLE unconditionally
                 * terminates (every entry path ends in return/throw,
                 * never falling through - or reached via `break` - to
                 * right after the switch), so mg->last_opcode can reflect
                 * that below instead of always resetting to 0. Needs a
                 * `default` case (otherwise the "no label matched" path
                 * falls straight through to after the switch), no
                 * `break` ANYWHERE (a break, even in the very last case,
                 * reaches "after the switch" directly, same as an
                 * ordinary fall-through would), and the PHYSICALLY LAST
                 * case body (in source/bytecode order - default included)
                 * ending in return/throw. An earlier case whose body has
                 * code but doesn't itself end in return/throw/break is
                 * NOT disqualifying on its own: falling through with no
                 * jump at all, into the next case's bytecode, is exactly
                 * how intentional fallthrough (no `break`) works - that
                 * case's real termination status is decided by whatever
                 * it falls through into, not by its own tail. Requiring
                 * every individual case to end in return/throw (rather
                 * than just the last one, plus "no break anywhere")
                 * wrongly treated a switch like "case A: ...; // fall
                 * through \n case B: ...; return;" as non-terminating
                 * even though every actual runtime path through it does
                 * terminate, and the compiler then appended a spurious,
                 * genuinely unreachable trailing return with no stack map
                 * frame of its own right after the switch. */
                bool has_default_case = false;
                bool any_break = false;
                bool last_case_had_code = false;

                for (slist_t *node = children->next; node; node = node->next, ast_idx++) {
                    ast_node_t *case_label = (ast_node_t *)node->data;
                    if (case_label->type != AST_CASE_LABEL) {
                        continue;
                    }

                    bool is_default = case_label->data.node.name &&
                                     strcmp(case_label->data.node.name, "default") == 0;
                    if (is_default) {
                        has_default_case = true;
                    }

                    size_t current_code_pos = mg->code->length;

                    /* Restore to switch-entry state before framing and
                     * generating this case - it's reached only from the
                     * lookupswitch dispatch, not by falling through from
                     * whichever case preceded it in AST order. */
                    if (prev_case_terminates && switch_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, switch_entry_state);
                    }

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

                    /* Does this case fall through to the next (no break,
                     * return, or throw at the end)? mg->last_opcode is set
                     * to OP_GOTO by AST_BREAK_STMT specifically for this
                     * kind of check (see its "Track for dead code
                     * detection" comment). */
                    {
                        uint8_t last_op = mg->last_opcode;
                        prev_case_terminates = (last_op == OP_GOTO ||
                            last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW);

                        /* Fold into the whole-switch termination tracking
                         * (see has_default_case/any_break/
                         * last_case_had_code's own comment above) - but
                         * only for a label that actually emitted bytecode
                         * of its own. An empty fall-through label (e.g.
                         * "case RSASHA256:" with no statements before
                         * "case RSASHA512: return ...;") leaves
                         * mg->last_opcode exactly as whatever it was
                         * before this iteration - stale, unrelated state
                         * that has nothing to do with this label. Any
                         * label (not just the last) can disqualify the
                         * whole switch via `break` (OP_GOTO); only the
                         * LAST label's own termination is otherwise
                         * tracked (last_case_had_code, checked against
                         * mg->last_opcode again once the loop is over) -
                         * an earlier case whose body doesn't end in
                         * return/throw/break simply falls through into
                         * the next case's bytecode, which is ordinary,
                         * non-disqualifying switch fallthrough. */
                        if (mg->code->length > current_code_pos) {
                            if (last_op == OP_GOTO) {
                                any_break = true;
                                /* This case's `break` reaches the switch's
                                 * shared exit point directly - capture its
                                 * own local-variable state as one of the
                                 * inputs to merge_stackmap_states_into()
                                 * below, instead of letting whichever case
                                 * happens to run LAST in the loop silently
                                 * dictate everyone else's frame. */
                                if (mg->stackmap) {
                                    stackmap_state_t *exit_snap = stackmap_save_state(mg->stackmap);
                                    if (exit_snap) {
                                        if (!switch_exit_states) {
                                            switch_exit_states = slist_new(exit_snap);
                                        } else {
                                            slist_append(switch_exit_states, exit_snap);
                                        }
                                    }
                                }
                            }
                            last_case_had_code = true;
                        } else {
                            last_case_had_code = false;
                        }
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
                
                /* Record stackmap frame at switch end if it's actually a
                 * reachable jump target: either some break statement
                 * targets it, OR there's no explicit "default:" label, in
                 * which case the lookupswitch/tableswitch's own default
                 * offset (patched above) points HERE too - the "no case
                 * matched" path falls through to exactly this position
                 * (see "default_offset = switch_end - switch_pos" just
                 * above). A plain switch statement (unlike a switch
                 * expression) doesn't require exhaustiveness, so this is
                 * a perfectly ordinary, reachable path whenever the
                 * selector's actual value doesn't match any case label -
                 * missing this frame produced "VerifyError: Expecting a
                 * stackmap frame" the moment such a switch was the last
                 * statement inside an enclosing try block (or otherwise
                 * followed by more code), even though every explicit case
                 * ended in return/break. */
                if ((mg->loop_stack &&
                     ((loop_context_t *)mg->loop_stack->data)->break_offsets) ||
                    default_code_pos == 0) {
                    /* This merge point is reached by every `break` (from
                     * ANY case, already snapshotted into
                     * switch_exit_states above), PLUS - if applicable -
                     * a non-terminating fallthrough off the physically
                     * last case, PLUS - if there's no explicit `default:`
                     * - the "no case matched" edge straight from switch
                     * entry. mg->stackmap otherwise still holds whatever
                     * local-variable state the LAST case processed left
                     * behind, which is only actually correct when every
                     * one of these edges happens to agree with it. A
                     * local variable declared inside just one case (e.g.
                     * "default: String message = ...;", with no
                     * enclosing braces) is not definitely assigned on any
                     * of the OTHER paths reaching this point, so it must
                     * not appear as a typed local in the shared frame -
                     * recording it as whatever the last-processed case
                     * left in that slot (a real type there, "top"/
                     * unassigned on every other incoming edge) produced
                     * "VerifyError: Inconsistent stackmap frames ...
                     * locals[N] ... not assignable" the moment a
                     * DIFFERENT case's own `break` reached this same
                     * merge point with that slot still unassigned -
                     * confirmed against gumdrop's own FtpProtocolHandler.
                     * dispatchCommand(), whose `default:` is the only one
                     * of ~44 cases to declare a local ("String message").
                     * merge_stackmap_states_into() computes the proper
                     * JVM-spec merge across every actually-reaching edge
                     * instead (unconditionally restoring switch-ENTRY
                     * state here instead - the simpler fix tried first -
                     * is equally wrong the other way: it forgets that a
                     * local declared BEFORE the switch and consistently
                     * assigned in EVERY case is legitimately usable
                     * afterward, breaking SwitchCaseAssignVerifyTest). */
                    bool last_case_falls_through = !last_case_had_code ||
                        (mg->last_opcode != OP_GOTO &&
                         mg->last_opcode != OP_RETURN && mg->last_opcode != OP_IRETURN &&
                         mg->last_opcode != OP_LRETURN && mg->last_opcode != OP_FRETURN &&
                         mg->last_opcode != OP_DRETURN && mg->last_opcode != OP_ARETURN &&
                         mg->last_opcode != OP_ATHROW);
                    if (last_case_falls_through && mg->stackmap) {
                        stackmap_state_t *fallthrough_snap = stackmap_save_state(mg->stackmap);
                        if (fallthrough_snap) {
                            if (!switch_exit_states) {
                                switch_exit_states = slist_new(fallthrough_snap);
                            } else {
                                slist_append(switch_exit_states, fallthrough_snap);
                            }
                        }
                    }
                    if (default_code_pos == 0 && switch_entry_state) {
                        /* A copy of switch_entry_state itself (not just a
                         * pointer to it) - it's still needed afterward as
                         * a fallback and gets freed separately from
                         * everything in switch_exit_states below. */
                        stackmap_state_t *entry_snap = calloc(1, sizeof(stackmap_state_t));
                        if (entry_snap) {
                            entry_snap->num_locals = switch_entry_state->num_locals;
                            entry_snap->locals = switch_entry_state->num_locals ?
                                malloc(switch_entry_state->num_locals * sizeof(verification_type_t)) : NULL;
                            if (entry_snap->locals) {
                                memcpy(entry_snap->locals, switch_entry_state->locals,
                                       switch_entry_state->num_locals * sizeof(verification_type_t));
                            }
                            if (!switch_exit_states) {
                                switch_exit_states = slist_new(entry_snap);
                            } else {
                                slist_append(switch_exit_states, entry_snap);
                            }
                        }
                    }
                    if (switch_exit_states && mg->stackmap) {
                        merge_stackmap_states_into(mg->stackmap, switch_exit_states);
                    } else if (switch_entry_state && mg->stackmap) {
                        /* No case ever reaches the exit at all (shouldn't
                         * normally happen given the guard above, but stay
                         * safe) - fall back to entry state as before. */
                        stackmap_restore_state(mg->stackmap, switch_entry_state);
                    }
                    mg_record_frame(mg);
                }

                if (switch_exit_states) {
                    for (slist_t *n = switch_exit_states; n; n = n->next) {
                        stackmap_state_free((stackmap_state_t *)n->data);
                    }
                    slist_free(switch_exit_states);
                }

                /* Pop switch context and patch breaks */
                mg_pop_loop(mg, switch_end);
                
                /* A switch statement doesn't generally guarantee
                 * termination (there may be no default, some case may
                 * `break` past its end, or the physically last case may
                 * fall off the end without one) - reset last_opcode to 0
                 * in that case, same as always before. But when there's
                 * no `break` anywhere, and the PHYSICALLY LAST case
                 * (default included) genuinely ends in return/throw (see
                 * has_default_case/any_break/last_case_had_code's own
                 * comment above), the switch AS A WHOLE does
                 * unconditionally terminate - reflect that instead of
                 * unconditionally resetting, so enclosing code that
                 * checks mg->last_opcode (e.g. AST_TRY_STMT's own
                 * try_body_ends_with_return check) doesn't wrongly think
                 * this switch falls through and append a dead, frame-less
                 * "normal completion" goto right after it. Confirmed
                 * against gumdrop's own DnssecValidator.buildPublicKey(),
                 * whose entire try body is exactly such a switch
                 * (VerifyError: "Expecting a stack map frame" on the
                 * spurious goto). mg->last_opcode already holds the last
                 * case's own final opcode at this point (nothing between
                 * the loop above and here changes it), so there's no
                 * separate "which op" value to track. */
                bool last_case_terminates = last_case_had_code &&
                    (mg->last_opcode == OP_RETURN || mg->last_opcode == OP_IRETURN ||
                     mg->last_opcode == OP_LRETURN || mg->last_opcode == OP_FRETURN ||
                     mg->last_opcode == OP_DRETURN || mg->last_opcode == OP_ARETURN ||
                     mg->last_opcode == OP_ATHROW);
                bool switch_terminates = has_default_case && !any_break && last_case_terminates;
                mg->last_opcode = switch_terminates ? mg->last_opcode : 0;

                stackmap_state_free(switch_entry_state);
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

                /* Save stackmap state at sync_start - the exception handler's
                 * protected range starts here, so an exception could occur
                 * before the body runs at all. Its frame must reflect this
                 * entry state, not whatever locals the body goes on to
                 * assign (e.g. a local declared partway through the body),
                 * exactly like try_entry_state for AST_TRY_STMT. */
                stackmap_state_t *sync_entry_state = NULL;
                if (mg->stackmap) {
                    sync_entry_state = stackmap_save_state(mg->stackmap);
                }

                /* Push this lock onto the enclosing-synchronized stack so a
                 * `return` lexically inside the body (directly, or nested in
                 * further if/try/etc.) can release it before returning -
                 * mirrors mg->loop_stack for break/continue. */
                mg->sync_lock_stack = slist_prepend(mg->sync_lock_stack,
                    (void *)(uintptr_t)lock_slot);

                /* Generate body */
                bool body_ok = codegen_statement(mg, body);

                {
                    slist_t *old_sync = mg->sync_lock_stack;
                    mg->sync_lock_stack = mg->sync_lock_stack->next;
                    free(old_sync);
                }

                if (!body_ok) {
                    return false;
                }

                /* Record end of synchronized region */
                uint16_t sync_end = (uint16_t)mg->code->length;

                /* If the body terminates on every path (return/throw), the
                 * "normal exit" epilogue below (monitorexit + goto-past-handler)
                 * is unreachable dead code - nothing ever falls out of the body
                 * to reach it. Skip it entirely: emitting it anyway can produce
                 * a stack-map frame for the goto's target address with no
                 * actual instruction there when the synchronized statement is
                 * the last thing in the method (VerifyError: "StackMapTable
                 * error: bad offset"), since the exception handler's own path
                 * always rethrows and never falls through to that point either.
                 * mg->last_opcode (rather than re-scanning bytecode) matches
                 * the same reliable check used for try/if bodies above. */
                bool body_ends_with_return = false;
                {
                    uint8_t last_op = mg->last_opcode;
                    if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                        last_op == OP_LRETURN || last_op == OP_FRETURN ||
                        last_op == OP_DRETURN || last_op == OP_ARETURN ||
                        last_op == OP_ATHROW) {
                        body_ends_with_return = true;
                    }
                }

                size_t goto_pos = 0;
                uint16_t saved_locals_for_normal_exit = 0;
                stackmap_state_t *sync_body_exit_state = NULL;
                if (!body_ends_with_return) {
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
                    goto_pos = mg->code->length;
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, 0);  /* Placeholder */

                    /* Save locals count before exception handler - the normal path
                     * doesn't have the exception slot, only the exception path does */
                    saved_locals_for_normal_exit = mg_save_locals_count(mg);

                    /* Full snapshot of the body's normal-exit state, taken
                     * before the handler below restores mg->stackmap back to
                     * sync_entry_state for its own framing. The "after
                     * handler" join point is only reachable via this goto
                     * (the handler always rethrows), so its frame must
                     * reflect this snapshot, not whatever the handler leaves
                     * mg->stackmap tracking afterward - the same class of
                     * bug already fixed for AST_TRY_STMT's try_exit_state. */
                    if (mg->stackmap) {
                        sync_body_exit_state = stackmap_save_state(mg->stackmap);
                    }
                }
                
                /* Exception handler: catch-all */
                uint16_t handler_pc = (uint16_t)mg->code->length;

                /* Restore stackmap to sync-entry state before recording the
                 * handler frame - by this point mg->stackmap reflects
                 * whatever locals the body assigned (e.g. a local declared
                 * partway through it), but an exception reaching this
                 * handler could have been thrown before any of that ran. */
                if (sync_entry_state && mg->stackmap) {
                    stackmap_restore_state(mg->stackmap, sync_entry_state);
                }

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
                
                if (!body_ends_with_return) {
                    /* Patch goto to jump here (after exception handler) */
                    uint16_t after_handler = (uint16_t)mg->code->length;
                    int16_t goto_offset = (int16_t)(after_handler - goto_pos);
                    mg->code->code[goto_pos + 1] = (goto_offset >> 8) & 0xFF;
                    mg->code->code[goto_pos + 2] = goto_offset & 0xFF;

                    /* This goto is the only live edge into this join point
                     * (the handler always rethrows) - restore the snapshot
                     * taken right after the body, not just its locals count,
                     * since mg->stackmap now reflects sync_entry_state plus
                     * exc_slot from framing the handler above, not the
                     * body's actual exit state. */
                    if (sync_body_exit_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, sync_body_exit_state);
                    } else {
                        mg_restore_locals_count(mg, saved_locals_for_normal_exit);
                    }

                    /* Record frame at goto target (after exception handler) */
                    mg_record_frame(mg);

                    /* There's a live path past this statement (the normal exit) */
                    mg->last_opcode = 0;
                } else {
                    /* Body terminates on every path, and the handler always
                     * rethrows - so this statement itself always terminates,
                     * exactly like the body would have on its own. No dead
                     * "after" code was emitted, so there's nothing to patch or
                     * frame here. */
                    mg->last_opcode = OP_ATHROW;
                }
                stackmap_state_free(sync_body_exit_state);
                stackmap_state_free(sync_entry_state);

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
                
                /* Generate try block - with the finally block (if any)
                 * pushed onto mg->finally_stack first, so a `return`
                 * lexically inside the try body runs it before actually
                 * returning (see emit_pending_finally_blocks()). Popped
                 * again right after, since only the try body itself (not
                 * the catch clauses generated below) can currently reach
                 * that pending-finally mechanism. */
                ast_node_t *finally_block_for_return = NULL;
                if (finally_clause && finally_clause->data.node.children) {
                    finally_block_for_return = (ast_node_t *)finally_clause->data.node.children->data;
                    mg->finally_stack = slist_prepend(mg->finally_stack, finally_block_for_return);
                }

                bool try_body_ok = codegen_statement(mg, try_block);

                if (finally_block_for_return) {
                    slist_t *old_finally = mg->finally_stack;
                    mg->finally_stack = mg->finally_stack->next;
                    free(old_finally);
                }

                if (!try_body_ok) {
                    slist_free(catch_clauses);
                    stackmap_state_free(try_entry_state);
                    return false;
                }

                /* If the try body already ends with return/throw/break/
                 * continue on every path, it never falls through to here
                 * at all - by now, any `return`/`break`/`continue`
                 * reachable from inside it has already run this same
                 * finally block itself via emit_pending_finally_blocks()
                 * (see the finally_stack push/pop above and
                 * AST_BREAK_STMT/AST_CONTINUE_STMT's own calls to it),
                 * inlined right before that exit. The "normal completion"
                 * copy below would therefore be genuinely unreachable dead
                 * code - and, being the instruction immediately following
                 * an unconditional jump, is also invalid without a stack
                 * frame nothing records for it ("Expecting a stack map
                 * frame" / "Inconsistent stackmap frames"). Skip it
                 * entirely in that case, mirroring the same reachability
                 * reasoning already used elsewhere in this file (e.g.
                 * AST_SYNCHRONIZED_STMT's own body_ends_with_return
                 * check). OP_GOTO here can only mean break/continue (a
                 * loop reaching its own natural back-edge/exit is a
                 * different AST node, not part of this try body's own
                 * last_opcode), both of which now run this finally block
                 * inline via emit_pending_finally_blocks() same as
                 * return. */
                bool try_body_ends_with_return = false;
                {
                    uint8_t body_last_op = mg->last_opcode;
                    if (body_last_op == OP_RETURN || body_last_op == OP_IRETURN ||
                        body_last_op == OP_LRETURN || body_last_op == OP_FRETURN ||
                        body_last_op == OP_DRETURN || body_last_op == OP_ARETURN ||
                        body_last_op == OP_ATHROW || body_last_op == OP_GOTO) {
                        try_body_ends_with_return = true;
                    }
                }

                /* If finally exists, inline finally code at end of try block.
                 *
                 * The finally block's own AST is re-walked once per exit
                 * edge from the try (this normal-completion copy, one per
                 * catch block below, and the uncaught-exception escape
                 * handler further down) - each occurrence is a fully
                 * separate codegen_statement() call over the identical
                 * subtree. Any temp local the finally block allocates
                 * itself (e.g. a nested `synchronized` statement's lock-
                 * object slot) must therefore start from the SAME
                 * mg->next_slot baseline every time, or two copies assign
                 * the same logical temp to two different slot numbers -
                 * and since all copies' control flow reconverges (a
                 * fall-through/goto to the same point after the whole
                 * try-catch-finally), the JVM verifier sees one incoming
                 * edge with that slot holding an Object and another with
                 * it untouched ("top"), which it rejects as inconsistent
                 * stack map frames. Saving/restoring next_slot around each
                 * copy keeps every copy's slot numbering identical. */
                uint16_t finally_saved_slot = mg->next_slot;
                if (!try_body_ends_with_return &&
                    finally_clause && finally_clause->data.node.children) {
                    ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                    /* Reset before regenerating - see the matching reset
                     * and comment at the escape-handler's own copy further
                     * down for why this must not inherit last_opcode
                     * carried over from the try body (or, via
                     * emit_pending_finally_blocks(), from an earlier
                     * inlined copy of this very same finally block). */
                    mg->last_opcode = 0;
                    if (!codegen_statement(mg, finally_block)) {
                        slist_free(catch_clauses);
                        return false;
                    }
                    mg->next_slot = finally_saved_slot;
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
                bool has_try_exit_goto = false;  /* Track whether we emitted a try exit goto */
                uint16_t try_exit_locals_count = 0;  /* Remember try path's locals for merging */
                /* Full stackmap snapshot as of the try block's normal exit, before
                 * the catch-handler loop below restores mg->stackmap to
                 * try_entry_state for each handler. If every catch clause
                 * terminates (return/throw), this goto is the *only* live edge
                 * into the join point after all handlers, so the join frame
                 * must reflect this snapshot - not whatever state the last
                 * catch handler left mg->stackmap in while framing its own
                 * entry (see the matching fix in AST_IF_STMT for the same
                 * class of bug). */
                stackmap_state_t *try_exit_state = NULL;
                if (!try_ends_with_return) {
                    try_exit_goto = mg->code->length;
                    has_try_exit_goto = true;
                    bc_emit(mg->code, OP_GOTO);
                    bc_emit_u2(mg->code, 0);  /* Placeholder */
                    /* Remember the try path's locals count for later comparison */
                    if (mg->stackmap) {
                        try_exit_locals_count = mg->stackmap->current_locals_count;
                        try_exit_state = stackmap_save_state(mg->stackmap);
                    }
                }
                
                uint16_t try_end = (uint16_t)mg->code->length;
                
                /* Track catch handler info: [goto_pos, catch_start, catch_end] triplets */
                slist_t *catch_gotos = NULL;   /* List of goto positions to patch */
                slist_t *catch_ranges = NULL;  /* List of catch ranges for finally handlers */

                /* Snapshot of the smallest (safest) locals state among any
                 * catch clause that falls through normally (mirrors
                 * try_exit_state above, for the same reason: by the time
                 * the join-point frame is computed after all handlers,
                 * mg->stackmap's "current" state has been overwritten by
                 * the finally clause's own escape-handler bookkeeping
                 * below, which is not a real predecessor of the join
                 * point at all - so neither it, nor any single catch
                 * clause's own leftover state, can be trusted there.
                 * Keeping the SMALLEST snapshot across multiple falling-
                 * through catch clauses is safe: they all share the same
                 * try_catch_saved_locals prefix, and the smallest is the
                 * common subset guaranteed initialized on every such
                 * edge. */
                stackmap_state_t *catch_exit_state = NULL;
                uint16_t catch_exit_locals_count = 0;
                
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
                    
                    /* For multi-catch, prefer the LUB semantic analysis
                     * already computed (catch_clause->sem_type) over a
                     * blanket Throwable - see the identical fix and its
                     * full explanation at the sibling multi-catch site in
                     * the try-with-resources catch-clause codegen above.
                     * For single-catch, use the specific exception class. */
                    const char *stackmap_exc_class = first_exc_class;
                    if (is_multi_catch) {
                        stackmap_exc_class = "java/lang/Throwable";
                        if (catch_clause->sem_type && catch_clause->sem_type->kind == TYPE_CLASS &&
                            catch_clause->sem_type->data.class_type.name) {
                            stackmap_exc_class = catch_clause->sem_type->data.class_type.name;
                        }
                    }
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
                    
                    /* Store exception to local variable. Prefer the LUB
                     * semantic analysis already computed across every
                     * multi-catch alternative (catch_clause->sem_type, set in
                     * semantic.c's AST_CATCH_CLAUSE handling) over
                     * first_exc_class - using only the FIRST alternative's
                     * type here (as this used to) made a later checkcast
                     * against that type reject any OTHER alternative
                     * actually thrown at runtime: ClassCastException.
                     * Confirmed against gumdrop's own GrpcClient, whose
                     * "catch (ProtoParseException | ProtobufParseException e)"
                     * is exactly this shape. Falls back to first_exc_class
                     * only if semantic analysis didn't leave a usable class
                     * type (defensive; shouldn't happen in practice). */
                    type_t *exc_type_t = (catch_clause->sem_type && catch_clause->sem_type->kind == TYPE_CLASS) ?
                        catch_clause->sem_type : type_new_class(first_exc_class);
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

                    /* mg->stackmap's OWN handler-entry frame (recorded
                     * above via mg_record_exception_handler_frame(),
                     * called with stackmap_exc_class - deliberately
                     * java/lang/Throwable for a multi-catch, a safe type
                     * valid for every alternative, NOT the LUB used for
                     * exc_type_t just above) is a completely separate
                     * concept from mg->stackmap's ONGOING simulated state.
                     * The ASTORE just above stored whatever type was
                     * actually pushed onto that simulated stack (Throwable)
                     * into this local slot - so unless corrected here, any
                     * LATER frame genesis records in this same catch body
                     * (e.g. a nested try/catch's own handler entry, which
                     * correctly types this slot using exc_type_t's real
                     * LUB) disagrees with what mg->stackmap would naturally
                     * derive by walking the bytecode from here - the real
                     * JVM verifier does exactly that walk, and rejects the
                     * mismatch: VerifyError "Stack map does not match the
                     * one at exception handler ... Type 'Throwable' ...
                     * not assignable to 'Exception'". Confirmed against
                     * gumdrop's own MessageIndex.save(), whose outer
                     * "catch (IOException | RuntimeException e)" wraps a
                     * try-with-resources followed by its own nested
                     * "try { Files.deleteIfExists(tempPath); } catch
                     * (IOException deleteFailed) { e.addSuppressed(...); }"
                     * - exactly this shape. */
                    if (mg->stackmap && exc_type_t->kind == TYPE_CLASS && exc_type_t->data.class_type.name) {
                        char *exc_local_internal = class_to_internal_name(exc_type_t->data.class_type.name);
                        stackmap_set_local_object(mg->stackmap, catch_exc_slot, mg->cp, exc_local_internal);
                        free(exc_local_internal);
                    }

                    uint16_t catch_start = (uint16_t)mg->code->length;

                    /* Reset last_opcode before generating the catch body -
                     * see the matching comment at the analogous spot in the
                     * try-with-resources catch-clause codegen above. Without
                     * this, a catch body ending in an ordinary statement
                     * (which doesn't itself touch last_opcode) inherits
                     * whatever last_opcode the TRY block's own last
                     * statement left behind (e.g. OP_ATHROW from a `throw`),
                     * wrongly marking this catch as terminal below even
                     * though it plainly falls through - which can in turn
                     * make the *enclosing* method wrongly skip its own
                     * implicit trailing return (VerifyError: "Control flow
                     * falls through code end"). */
                    mg->last_opcode = 0;

                    /* Generate catch block */
                    if (!codegen_statement(mg, catch_block)) {
                        slist_free(catch_clauses);
                        slist_free(catch_gotos);
                        slist_free_full(catch_ranges, free);
                        return false;
                    }
                    
                    uint16_t catch_end = (uint16_t)mg->code->length;

                    /* If finally exists, inline finally code - but only if
                     * the catch body doesn't already end with an
                     * unconditional return/throw/break/continue, mirroring
                     * try_body_ends_with_return's own identical check above
                     * for the try block. A `throw` as the catch body's last
                     * statement emits ATHROW directly (see AST_THROW_STMT)
                     * without running this finally block first - but that's
                     * still correct: the catch range is registered below as
                     * protected by the "any -> finally handler" exception
                     * table entry (see "Save catch range for finally
                     * exception handler" just below), so the JVM's own
                     * exception dispatch already runs the finally block via
                     * that handler when the throw propagates. Appending a
                     * SECOND, redundant copy of the finally block directly
                     * after the ATHROW is therefore always dead code - and,
                     * being the instruction immediately following an
                     * unconditional branch, the verifier rejects it outright
                     * for lacking a stack map frame ("Expecting a stack map
                     * frame"), confirmed against gumdrop's own
                     * BasicRealm.setHref(), whose catch clause's last
                     * statement is `throw new RuntimeException(..., e);`
                     * wrapped in a try/finally. A `return` inside the catch
                     * body has the same dead-code placement (see
                     * emit_pending_finally_blocks()'s own doc comment: it
                     * does not yet cover a return from inside a catch
                     * clause), which is a separate, pre-existing gap this
                     * check does not newly introduce - skipping this
                     * unreachable copy changes nothing for that case, since
                     * it already never actually ran at runtime either way. */
                    bool catch_body_ends_with_return = false;
                    {
                        uint8_t body_last_op = mg->last_opcode;
                        if (body_last_op == OP_RETURN || body_last_op == OP_IRETURN ||
                            body_last_op == OP_LRETURN || body_last_op == OP_FRETURN ||
                            body_last_op == OP_DRETURN || body_last_op == OP_ARETURN ||
                            body_last_op == OP_ATHROW || body_last_op == OP_GOTO) {
                            catch_body_ends_with_return = true;
                        }
                    }

                    /* See finally_saved_slot's comment above: each inlined
                     * copy of the finally block must start temp-local
                     * allocation from the same baseline as every other
                     * copy. */
                    if (!catch_body_ends_with_return &&
                        finally_clause && finally_clause->data.node.children) {
                        ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                        uint16_t catch_finally_saved_slot = mg->next_slot;
                        if (!codegen_statement(mg, finally_block)) {
                            slist_free(catch_clauses);
                            slist_free(catch_gotos);
                            slist_free_full(catch_ranges, free);
                            return false;
                        }
                        mg->next_slot = catch_finally_saved_slot;
                    }

                    /* Save catch range for finally exception handler (before
                     * inlined finally) - but only when the catch body
                     * actually emitted bytecode. An empty catch clause
                     * (e.g. one containing only a comment) emits nothing
                     * at all, so catch_start == catch_end - registering
                     * that zero-length range as an exception handler
                     * protected range is invalid per JVMS 4.7.3
                     * (start_pc must be strictly less than end_pc), and
                     * every real JVM classloader rejects it outright at
                     * class-load time ("Illegal exception table range"),
                     * before verification even runs. There's also nothing
                     * to protect: no instructions in an empty range can
                     * ever throw. */
                    if (finally_clause && catch_end > catch_start) {
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
                    
                    /* Check if catch block ended with a terminating instruction.
                     * Use mg->last_opcode (the reliable, already-established way
                     * to answer "did the last thing terminate unconditionally" -
                     * see the matching checks for try/if/synchronized bodies
                     * elsewhere in this file) instead of re-reading the last
                     * byte physically written to the bytecode buffer, which is
                     * only ever correct for single-byte opcodes (the RETURN
                     * variants, ATHROW) and silently wrong for any multi-byte
                     * terminal instruction - in particular OP_GOTO (3 bytes),
                     * emitted for a `break`/`continue` ending the catch block
                     * (e.g. "catch (InterruptedException e) { ...; break; }"),
                     * where the buffer's last byte is just part of the jump
                     * offset, never OP_GOTO itself.
                     *
                     * Also include OP_GOTO outright, but ONLY when the catch
                     * block actually emitted bytecode of its own (mg->code
                     * grew past catch_start): mg->last_opcode is not reset
                     * generically by bc_emit for every instruction, only at a
                     * handful of specific call sites (return/throw/break/
                     * continue, plus a few explicit resets) - so an EMPTY
                     * catch block (e.g. one containing only a comment,
                     * which emits nothing itself) leaves mg->last_opcode
                     * exactly as whatever it was carried over from BEFORE this
                     * catch clause even began (e.g. the try block's own
                     * unrelated OP_GOTO/OP_ATHROW), which has nothing to do
                     * with whether this empty catch body terminates - and it
                     * never does; catch bodies must always fall through when
                     * empty. Without the catch_start guard, that stale,
                     * unrelated OP_GOTO look like a genuine break/continue and
                     * wrongly skip the epilogue goto other statements need to
                     * fall through the try/catch, producing "Control flow
                     * falls through code end". A catch block ending in a
                     * genuine break/continue always emits at least the 3-byte
                     * goto itself, so this guard never excludes the real case. */
                    bool catch_ends_with_return = false;
                    {
                        uint8_t last_op = mg->last_opcode;
                        bool catch_emitted_code = mg->code->length > catch_start;
                        if (last_op == OP_RETURN || last_op == OP_IRETURN ||
                            last_op == OP_LRETURN || last_op == OP_FRETURN ||
                            last_op == OP_DRETURN || last_op == OP_ARETURN ||
                            last_op == OP_ATHROW ||
                            (last_op == OP_GOTO && catch_emitted_code)) {
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
                        /* Snapshot THIS catch clause's own real fallthrough-
                         * exit state, keeping only the smallest seen so
                         * far - see catch_exit_state's own comment above. */
                        if (mg->stackmap) {
                            uint16_t this_catch_locals = mg->stackmap->current_locals_count;
                            if (!catch_exit_state || this_catch_locals < catch_exit_locals_count) {
                                if (catch_exit_state) {
                                    stackmap_state_free(catch_exit_state);
                                }
                                catch_exit_state = stackmap_save_state(mg->stackmap);
                                catch_exit_locals_count = this_catch_locals;
                            }
                        }

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

                    /* Restore stackmap to try block entry state before recording the
                     * handler frame, exactly like the catch-clause handlers above do.
                     * By this point mg->stackmap reflects locals from the end of the
                     * try block (and any catch blocks), which may include locals
                     * declared inside them (e.g. a local assigned partway through the
                     * try body) - but an exception reaching this handler could have
                     * been thrown before any of those assignments executed, so the
                     * frame here must not claim them as initialized. try_entry_state's
                     * locals are also a valid (guaranteed) subset at entry to every
                     * catch block, since those handlers restore to the same state. */
                    if (try_entry_state && mg->stackmap) {
                        stackmap_restore_state(mg->stackmap, try_entry_state);
                    }

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
                    /* Track exc_slot's type in the stackmap, mirroring
                     * AST_SYNCHRONIZED_STMT's own lock_slot tracking right
                     * after its astore. Without this, mg->stackmap never
                     * learns that exc_slot holds a Throwable, so any frame
                     * recorded later in this handler (e.g. at the re-throw
                     * below, once the finally block's own codegen - such as
                     * a nested synchronized statement - has saved/restored
                     * frames of its own) sees exc_slot as still unassigned
                     * ("top"), and the re-throw's own aload of exc_slot
                     * fails verification. */
                    if (mg->stackmap) {
                        stackmap_set_local_object(mg->stackmap, exc_slot, mg->cp, "java/lang/Throwable");
                    }
                    /* Generate finally block.
                     * See finally_saved_slot's comment above: each inlined
                     * copy of the finally block must start temp-local
                     * allocation from the same baseline as every other
                     * copy. */
                    if (finally_clause->data.node.children) {
                        ast_node_t *finally_block = (ast_node_t *)finally_clause->data.node.children->data;
                        uint16_t escape_finally_saved_slot = mg->next_slot;
                        /* Reset before regenerating - without this,
                         * mg->last_opcode carried over from BEFORE this
                         * copy (e.g. OP_IRETURN left by the try body's own
                         * early return, when the "normal completion" copy
                         * above was correctly skipped as unreachable) made
                         * the "did the finally block itself return/throw"
                         * check just below see a stale return opcode that
                         * has nothing to do with THIS copy's own actual
                         * termination - wrongly skipping the re-throw and
                         * silently swallowing whatever exception the try
                         * block threw (VerifyError: "Control flow falls
                         * through code end", since the method could then
                         * end without ever re-throwing or returning on
                         * this path). */
                        mg->last_opcode = 0;
                        if (!codegen_statement(mg, finally_block)) {
                            slist_free(catch_clauses);
                            slist_free(catch_gotos);
                            slist_free_full(catch_ranges, free);
                            return false;
                        }
                        mg->next_slot = escape_finally_saved_slot;
                    }

                    /* Check if finally block ended with a terminating instruction.
                     * If it did, we don't need to re-throw the exception.
                     *
                     * Use mg->last_opcode (set by statement codegen to
                     * reflect the last opcode on the actually-reachable
                     * normal-completion path), not the raw last byte in
                     * mg->code's buffer. The finally block's last
                     * *textual* statement can itself emit bytecode after
                     * its own normal-path exit - e.g. a `synchronized`
                     * statement's own exception handler (ending in athrow)
                     * is appended after the synchronized statement's
                     * normal monitorexit+goto - so the physically-last
                     * byte written is unrelated to whether the finally
                     * block's normal-completion path actually returns or
                     * throws. Peeking at that byte previously misread an
                     * unrelated trailing athrow as "the finally block
                     * always throws" and skipped the re-throw entirely,
                     * silently swallowing any exception the try block
                     * threw whenever the finally block's last statement
                     * was a synchronized block (or anything else with its
                     * own trailing exception-handler bytecode). */
                    bool finally_ends_with_return = false;
                    {
                        uint8_t last_op = mg->last_opcode;
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
                if (has_try_exit_goto || catch_gotos) {
                    /* By this point, if a finally clause exists, its escape-
                     * handler segment above has already overwritten
                     * mg->stackmap's "current" state with its own internal
                     * bookkeeping (restoring to try_entry_state, then adding
                     * exc_slot as a live Throwable local) - that segment is
                     * NOT a real predecessor of this join point at all (it
                     * always ends in athrow or an early return, never falls
                     * through here), so "current" state can never be trusted
                     * for this frame. Always restore explicitly from a real
                     * saved snapshot of whichever edge(s) actually reach
                     * here, instead of reading/trimming ambient state. */
                    if (mg->stackmap) {
                        if (has_try_exit_goto && !catch_gotos && try_exit_state) {
                            /* Every catch clause terminates (return/throw), so the
                             * try block's own exit goto is the only live edge. */
                            stackmap_restore_state(mg->stackmap, try_exit_state);
                        } else if (catch_gotos && !has_try_exit_goto && catch_exit_state) {
                            /* The try block always terminates, so the smallest
                             * falling-through catch clause's own exit is the only
                             * live edge. */
                            stackmap_restore_state(mg->stackmap, catch_exit_state);
                        } else if (has_try_exit_goto && catch_gotos &&
                                   try_exit_state && catch_exit_state) {
                            /* Both the try block and at least one catch clause
                             * fall through - restore from whichever snapshot has
                             * fewer locals (the common, guaranteed-safe subset
                             * both real edges agree on), then trim to the true
                             * minimum of the two counts. */
                            if (try_exit_locals_count <= catch_exit_locals_count) {
                                stackmap_restore_state(mg->stackmap, try_exit_state);
                                if (catch_exit_locals_count < mg->stackmap->current_locals_count) {
                                    mg_restore_locals_count(mg, catch_exit_locals_count);
                                }
                            } else {
                                stackmap_restore_state(mg->stackmap, catch_exit_state);
                                if (try_exit_locals_count < mg->stackmap->current_locals_count) {
                                    mg_restore_locals_count(mg, try_exit_locals_count);
                                }
                            }
                        } else if (has_try_exit_goto && try_exit_state) {
                            stackmap_restore_state(mg->stackmap, try_exit_state);
                        } else if (catch_exit_state) {
                            stackmap_restore_state(mg->stackmap, catch_exit_state);
                        }
                    }
                    mg_record_frame(mg);
                }
                stackmap_state_free(try_exit_state);
                stackmap_state_free(catch_exit_state);
                
                /* Patch try exit goto (only if we emitted one) */
                if (has_try_exit_goto) {
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
                        entry->access_flags = inner_class_access_flags(
                            stmt->data.node.flags,
                            stmt->type == AST_INTERFACE_DECL ? SYM_INTERFACE : SYM_CLASS);
                        
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

