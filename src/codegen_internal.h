/*
 * codegen_internal.h
 * Internal declarations shared between codegen modules
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

#ifndef CODEGEN_INTERNAL_H
#define CODEGEN_INTERNAL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "codegen.h"
#include "classfile.h"

/* ========================================================================
 * Loop Context (for break/continue handling)
 * ======================================================================== */

/**
 * Push a loop context onto the loop stack.
 * Used by loop statements to set up break/continue targets.
 *
 * @param mg              Method generator
 * @param continue_target Bytecode offset for continue (0 if not a loop)
 * @param label           Optional label name (NULL for unlabeled loops)
 */
void mg_push_loop(method_gen_t *mg, size_t continue_target, const char *label);

/**
 * Pop a loop context from the loop stack and patch break statements.
 *
 * @param mg       Method generator
 * @param loop_end Bytecode offset where the loop ends (break target)
 */
void mg_pop_loop(method_gen_t *mg, size_t loop_end);

/**
 * Find a loop context by label name.
 *
 * @param mg    Method generator
 * @param label Label to search for
 * @return      Loop context with matching label, or NULL if not found
 */
loop_context_t *mg_find_loop_by_label(method_gen_t *mg, const char *label);

/**
 * Add a break position to a specific loop context (for labeled breaks).
 *
 * @param ctx       Target loop context
 * @param break_pos Bytecode offset of the break instruction
 */
void mg_add_break_to_context(loop_context_t *ctx, size_t break_pos);

/* ========================================================================
 * Local Variable Helpers
 * ======================================================================== */

/**
 * Check if a local variable is a reference type (object or array).
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     true if reference type, false otherwise
 */
bool mg_local_is_ref(method_gen_t *mg, const char *name);

/**
 * Get the class name of a local variable (for class types).
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     Internal class name (e.g., "com/example/Foo") or NULL if not a class type
 */
const char *mg_local_class_name(method_gen_t *mg, const char *name);

/**
 * Check if a local variable is an array type.
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     true if array type, false otherwise
 */
bool mg_local_is_array(method_gen_t *mg, const char *name);

/**
 * Get the element type kind of an array local variable.
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     Element type kind (defaults to TYPE_INT if not found)
 */
type_kind_t mg_local_array_elem_kind(method_gen_t *mg, const char *name);

/**
 * Get the element class name of an array local variable.
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     Element class internal name or NULL if not an object array
 */
const char *mg_local_array_elem_class(method_gen_t *mg, const char *name);

/**
 * Resolve a simple class name to its fully qualified internal name.
 * Handles common java.lang classes that may be used without import.
 *
 * @param name Simple class name (e.g., "String", "Object")
 * @return     Internal class name (e.g., "java/lang/String")
 */
const char *resolve_java_lang_class(const char *name);

/**
 * Get the dimension count of an array local variable.
 *
 * @param mg   Method generator
 * @param name Variable name
 * @return     Dimension count (1 for single-dim, 2+ for multi-dim, 0 if not found)
 */
int mg_local_array_dims(method_gen_t *mg, const char *name);

/* ========================================================================
 * Type Descriptor Generation
 * ======================================================================== */

/**
 * Convert an AST type node to a JVM type descriptor.
 *
 * @param type_node AST node representing a type
 * @return          Allocated descriptor string (caller must free)
 */
char *ast_type_to_descriptor(ast_node_t *type_node);

/* ========================================================================
 * Exception Handler Management
 * ======================================================================== */

/**
 * Add an exception handler entry to the method.
 *
 * @param mg          Method generator
 * @param start_pc    Start of protected region (inclusive)
 * @param end_pc      End of protected region (exclusive)
 * @param handler_pc  Start of handler code
 * @param catch_type  Exception class CP index, or 0 for finally
 */
void mg_add_exception_handler(method_gen_t *mg, uint16_t start_pc,
                               uint16_t end_pc, uint16_t handler_pc,
                               uint16_t catch_type);

/* ========================================================================
 * Expression Code Generation
 * ======================================================================== */

/**
 * Generate bytecode for an expression (internal version with const pool).
 *
 * @param mg   Method generator
 * @param expr Expression AST node
 * @param cp   Constant pool
 * @return     true on success, false on error
 */
bool codegen_expr(method_gen_t *mg, ast_node_t *expr, const_pool_t *cp);

/* Autoboxing/unboxing code generation */
bool emit_boxing(method_gen_t *mg, const_pool_t *cp, type_kind_t prim_kind);
bool emit_unboxing(method_gen_t *mg, const_pool_t *cp, type_kind_t target_prim, const char *wrapper_class);
bool emit_boxing_if_needed(method_gen_t *mg, const_pool_t *cp, type_t *target, type_t *source);
bool emit_unboxing_if_needed(method_gen_t *mg, const_pool_t *cp, type_t *target, type_t *source);

/**
 * Check if an expression evaluates to a String type.
 *
 * @param expr Expression AST node
 * @return     true if String type, false otherwise
 */
bool is_string_type(ast_node_t *expr);

/**
 * Get the type kind of an expression for opcode selection.
 * Defaults to TYPE_INT if type cannot be determined.
 * 
 * @param mg   Method generator context
 * @param expr Expression AST node
 * @return     The type_kind_t of the expression
 */
type_kind_t get_expr_type_kind(method_gen_t *mg, ast_node_t *expr);

#endif /* CODEGEN_INTERNAL_H */

