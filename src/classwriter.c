/*
 * classwriter.c
 * Class file binary output for the genesis Java compiler
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
#include "classwriter.h"
#include "classfile.h"
#include "codegen.h"
#include "genesis.h"

/* Forward declarations */
static void preadd_annotations_list_cp(const_pool_t *cp, slist_t *annotations, 
                                        retention_policy_t retention);

/* ========================================================================
 * Big-Endian Write Helpers
 * ======================================================================== */

static void write_be_u1(uint8_t **p, uint8_t value)
{
    *(*p)++ = value;
}

static void write_be_u2(uint8_t **p, uint16_t value)
{
    *(*p)++ = (value >> 8) & 0xFF;
    *(*p)++ = value & 0xFF;
}

static void write_be_u4(uint8_t **p, uint32_t value)
{
    *(*p)++ = (value >> 24) & 0xFF;
    *(*p)++ = (value >> 16) & 0xFF;
    *(*p)++ = (value >> 8) & 0xFF;
    *(*p)++ = value & 0xFF;
}

/* ========================================================================
 * Annotation Retention
 * ======================================================================== */

/**
 * Get the retention policy for a known annotation.
 * For unknown annotations, returns RETENTION_CLASS (the default).
 */
retention_policy_t get_annotation_retention(const char *annotation_name)
{
    if (!annotation_name) {
        return RETENTION_CLASS;
    }
    
    /* Strip leading @ if present */
    if (annotation_name[0] == '@') {
        annotation_name++;
    }
    
    /* SOURCE retention - discarded by compiler */
    if (strcmp(annotation_name, "Override") == 0 ||
        strcmp(annotation_name, "java.lang.Override") == 0 ||
        strcmp(annotation_name, "SuppressWarnings") == 0 ||
        strcmp(annotation_name, "java.lang.SuppressWarnings") == 0) {
        return RETENTION_SOURCE;
    }
    
    /* RUNTIME retention - available via reflection */
    if (strcmp(annotation_name, "Deprecated") == 0 ||
        strcmp(annotation_name, "java.lang.Deprecated") == 0 ||
        strcmp(annotation_name, "FunctionalInterface") == 0 ||
        strcmp(annotation_name, "java.lang.FunctionalInterface") == 0 ||
        strcmp(annotation_name, "SafeVarargs") == 0 ||
        strcmp(annotation_name, "java.lang.SafeVarargs") == 0 ||
        strcmp(annotation_name, "Retention") == 0 ||
        strcmp(annotation_name, "java.lang.annotation.Retention") == 0 ||
        strcmp(annotation_name, "Target") == 0 ||
        strcmp(annotation_name, "java.lang.annotation.Target") == 0 ||
        strcmp(annotation_name, "Documented") == 0 ||
        strcmp(annotation_name, "java.lang.annotation.Documented") == 0 ||
        strcmp(annotation_name, "Inherited") == 0 ||
        strcmp(annotation_name, "java.lang.annotation.Inherited") == 0 ||
        strcmp(annotation_name, "Repeatable") == 0 ||
        strcmp(annotation_name, "java.lang.annotation.Repeatable") == 0) {
        return RETENTION_RUNTIME;
    }
    
    /* Default: CLASS retention */
    return RETENTION_CLASS;
}

/* ========================================================================
 * Annotation Writing
 * ======================================================================== */

/**
 * Write annotation element_value to buffer.
 * Returns the number of bytes written.
 */
static int write_annotation_value(uint8_t **p, const_pool_t *cp, ast_node_t *value)
{
    if (!value) return 0;
    
    uint8_t *start = *p;
    
    /* Handle different value types */
    if (value->type == AST_LITERAL) {
        token_type_t tok = value->data.leaf.token_type;
        
        if (tok == TOK_STRING_LITERAL) {
            /* String value */
            *(*p)++ = 's';  /* tag for String */
            uint16_t idx = cp_add_utf8(cp, value->data.leaf.value.str_val ? 
                                       value->data.leaf.value.str_val : "");
            write_be_u2(p, idx);
        } else if (tok == TOK_INTEGER_LITERAL) {
            /* Integer value */
            *(*p)++ = 'I';  /* tag for int */
            uint16_t idx = cp_add_integer(cp, (int32_t)value->data.leaf.value.int_val);
            write_be_u2(p, idx);
        } else if (tok == TOK_TRUE || tok == TOK_FALSE) {
            /* Boolean value */
            *(*p)++ = 'Z';  /* tag for boolean */
            uint16_t idx = cp_add_integer(cp, tok == TOK_TRUE ? 1 : 0);
            write_be_u2(p, idx);
        } else {
            /* Unknown literal type - skip */
            return 0;
        }
    } else if (value->type == AST_IDENTIFIER) {
        /* Could be an enum constant - for now, treat as string */
        *(*p)++ = 's';
        uint16_t idx = cp_add_utf8(cp, value->data.leaf.name ? value->data.leaf.name : "");
        write_be_u2(p, idx);
    } else {
        /* Unknown value type - skip */
        return 0;
    }
    
    return *p - start;
}

/**
 * Write a single annotation to the buffer.
 * Returns the number of bytes written.
 */
static int write_annotation(uint8_t **p, const_pool_t *cp, ast_node_t *annot)
{
    if (!annot || annot->type != AST_ANNOTATION) return 0;
    
    uint8_t *start = *p;
    
    /* type_index - descriptor for annotation type */
    char *type_desc = calloc(strlen(annot->data.node.name) + 4, 1);
    sprintf(type_desc, "L%s;", annot->data.node.name);
    /* Convert dots to slashes */
    for (char *c = type_desc; *c; c++) {
        if (*c == '.') *c = '/';
    }
    uint16_t type_idx = cp_add_utf8(cp, type_desc);
    free(type_desc);
    write_be_u2(p, type_idx);
    
    /* Count element-value pairs */
    uint16_t num_pairs = 0;
    slist_t *children = annot->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *pair = (ast_node_t *)n->data;
        if (pair && pair->type == AST_ANNOTATION_VALUE) {
            num_pairs++;
        }
    }
    write_be_u2(p, num_pairs);
    
    /* Write element-value pairs */
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *pair = (ast_node_t *)n->data;
        if (pair && pair->type == AST_ANNOTATION_VALUE && pair->data.node.name) {
            /* element_name_index */
            uint16_t name_idx = cp_add_utf8(cp, pair->data.node.name);
            write_be_u2(p, name_idx);
            
            /* value */
            if (pair->data.node.children) {
                ast_node_t *value = (ast_node_t *)pair->data.node.children->data;
                write_annotation_value(p, cp, value);
            }
        }
    }
    
    return *p - start;
}

/**
 * Count annotations that have the specified retention.
 */
static int count_annotations_with_retention(slist_t *annotations, retention_policy_t retention)
{
    int count = 0;
    for (slist_t *n = annotations; n; n = n->next) {
        ast_node_t *annot = (ast_node_t *)n->data;
        if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
            if (get_annotation_retention(annot->data.node.name) == retention) {
                count++;
            }
        }
    }
    return count;
}

/**
 * Write RuntimeVisibleAnnotations or RuntimeInvisibleAnnotations attribute.
 * Returns the total bytes written (0 if no annotations to write).
 */
static int write_annotations_attribute(uint8_t **p, const_pool_t *cp, 
                                        slist_t *annotations, bool runtime_visible)
{
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_annotations_with_retention(annotations, target);
    
    if (count == 0) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    const char *attr_name = runtime_visible ? 
        "RuntimeVisibleAnnotations" : "RuntimeInvisibleAnnotations";
    uint16_t attr_name_idx = cp_add_utf8(cp, attr_name);
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length (we'll fill it in later) */
    uint8_t *len_pos = *p;
    *p += 4;  /* Skip 4 bytes for u4 length */
    
    /* num_annotations */
    write_be_u2(p, (uint16_t)count);
    
    /* Write each annotation with matching retention */
    for (slist_t *n = annotations; n; n = n->next) {
        ast_node_t *annot = (ast_node_t *)n->data;
        if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
            if (get_annotation_retention(annot->data.node.name) == target) {
                write_annotation(p, cp, annot);
            }
        }
    }
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;  /* Exclude the length field itself */
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/* ========================================================================
 * Type Annotations (JSR 308, Java 8)
 * ======================================================================== */

/* Target type values for type annotations (JVM Spec §4.7.20.1) */
#define TARGET_TYPE_FIELD                    0x13
#define TARGET_TYPE_RETURN                   0x14
#define TARGET_TYPE_RECEIVER                 0x15
#define TARGET_TYPE_FORMAL_PARAMETER         0x16
#define TARGET_TYPE_THROWS                   0x17
#define TARGET_TYPE_LOCAL_VARIABLE           0x40
#define TARGET_TYPE_RESOURCE_VARIABLE        0x41
#define TARGET_TYPE_EXCEPTION_PARAMETER      0x42
#define TARGET_TYPE_INSTANCEOF               0x43
#define TARGET_TYPE_NEW                      0x44
#define TARGET_TYPE_CONSTRUCTOR_REFERENCE    0x45
#define TARGET_TYPE_METHOD_REFERENCE         0x46
#define TARGET_TYPE_CAST                     0x47
#define TARGET_TYPE_TYPE_ARGUMENT            0x48

/**
 * Count type annotations on a type node (recursive for arrays/generics).
 */
static int count_type_annotations(ast_node_t *type_node, retention_policy_t retention)
{
    if (!type_node) return 0;
    
    int count = 0;
    
    /* Count annotations directly on this type node */
    if (type_node->annotations) {
        count += count_annotations_with_retention(type_node->annotations, retention);
    }
    
    /* For array types, also count annotations on element type */
    if (type_node->type == AST_ARRAY_TYPE) {
        slist_t *children = type_node->data.node.children;
        if (children) {
            count += count_type_annotations((ast_node_t *)children->data, retention);
        }
    }
    
    /* For class types with type arguments, count annotations on type args */
    if (type_node->type == AST_CLASS_TYPE) {
        slist_t *children = type_node->data.node.children;
        for (slist_t *n = children; n; n = n->next) {
            count += count_type_annotations((ast_node_t *)n->data, retention);
        }
    }
    
    return count;
}

/**
 * Write a type_path structure for type annotations.
 * For simple types, path_length is 0. For arrays/generics, it describes location.
 * Returns bytes written.
 */
static int write_type_path(uint8_t **p, ast_node_t *type_node, ast_node_t *annotated_node)
{
    uint8_t *start = *p;
    
    /* For now, write empty type_path (path_length = 0) for simple cases */
    /* TODO: implement proper path for nested array/generic types */
    (void)type_node;
    (void)annotated_node;
    write_be_u1(p, 0);  /* path_length */
    
    return *p - start;
}

/**
 * Write a single type_annotation structure.
 * Returns bytes written.
 */
static int write_type_annotation(uint8_t **p, const_pool_t *cp, 
                                  uint8_t target_type, ast_node_t *annot,
                                  ast_node_t *type_node, ast_node_t *annotated_node)
{
    uint8_t *start = *p;
    
    /* target_type (u1) */
    write_be_u1(p, target_type);
    
    /* target_info (depends on target_type) */
    switch (target_type) {
        case TARGET_TYPE_FIELD:
        case TARGET_TYPE_RETURN:
        case TARGET_TYPE_RECEIVER:
            /* empty_target: no additional info */
            break;
        case TARGET_TYPE_FORMAL_PARAMETER:
            /* formal_parameter_target: u1 formal_parameter_index */
            /* TODO: need to pass the parameter index */
            write_be_u1(p, 0);  
            break;
        case TARGET_TYPE_LOCAL_VARIABLE:
        case TARGET_TYPE_RESOURCE_VARIABLE:
            /* localvar_target: u2 table_length, then entries */
            /* For now, write empty table */
            write_be_u2(p, 0);
            break;
        default:
            /* Other target types - write empty for now */
            break;
    }
    
    /* type_path */
    write_type_path(p, type_node, annotated_node);
    
    /* annotation (same as regular annotation) */
    write_annotation(p, cp, annot);
    
    return *p - start;
}

/**
 * Get the type node from a field declaration AST.
 */
static ast_node_t *get_field_type_node(ast_node_t *field_decl)
{
    if (!field_decl) return NULL;
    
    slist_t *children = field_decl->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && (child->type == AST_PRIMITIVE_TYPE ||
                      child->type == AST_CLASS_TYPE ||
                      child->type == AST_ARRAY_TYPE)) {
            return child;
        }
    }
    return NULL;
}

/**
 * Write RuntimeVisibleTypeAnnotations or RuntimeInvisibleTypeAnnotations for a field.
 * Returns bytes written (0 if no type annotations).
 */
static int write_field_type_annotations_attribute(uint8_t **p, const_pool_t *cp,
                                                   ast_node_t *type_node, bool runtime_visible)
{
    if (!type_node) return 0;
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_type_annotations(type_node, target);
    
    if (count == 0) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    const char *attr_name = runtime_visible ?
        "RuntimeVisibleTypeAnnotations" : "RuntimeInvisibleTypeAnnotations";
    uint16_t attr_name_idx = cp_add_utf8(cp, attr_name);
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* num_annotations */
    write_be_u2(p, (uint16_t)count);
    
    /* Write type annotations on the type and its nested types */
    /* For simplicity, just handle annotations directly on the type for now */
    if (type_node->annotations) {
        for (slist_t *n = type_node->annotations; n; n = n->next) {
            ast_node_t *annot = (ast_node_t *)n->data;
            if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                if (get_annotation_retention(annot->data.node.name) == target) {
                    write_type_annotation(p, cp, TARGET_TYPE_FIELD, annot, 
                                         type_node, type_node);
                }
            }
        }
    }
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/**
 * Get the return type node from a method declaration AST.
 */
static ast_node_t *get_method_return_type_node(ast_node_t *method_decl)
{
    if (!method_decl) return NULL;
    
    /* Return type is stored in data.node.extra for method decls */
    return (ast_node_t *)method_decl->data.node.extra;
}

/**
 * Get parameter type nodes from a method declaration.
 * Populates the types array and returns the count.
 */
static int get_method_parameter_type_nodes(ast_node_t *method_decl, ast_node_t **types, int max_params)
{
    if (!method_decl) return 0;
    
    int count = 0;
    slist_t *children = method_decl->data.node.children;
    for (slist_t *n = children; n && count < max_params; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && child->type == AST_PARAMETER) {
            /* Parameter's type is its first child */
            slist_t *param_children = child->data.node.children;
            if (param_children) {
                ast_node_t *type_node = (ast_node_t *)param_children->data;
                if (type_node && (type_node->type == AST_PRIMITIVE_TYPE ||
                                  type_node->type == AST_CLASS_TYPE ||
                                  type_node->type == AST_ARRAY_TYPE)) {
                    types[count++] = type_node;
                }
            }
        }
    }
    return count;
}

/**
 * Count type annotations on method (return type and parameters).
 */
static int count_method_type_annotations(ast_node_t *method_ast, retention_policy_t retention)
{
    if (!method_ast) return 0;
    
    int count = 0;
    
    /* Count return type annotations */
    ast_node_t *return_type = get_method_return_type_node(method_ast);
    if (return_type) {
        count += count_type_annotations(return_type, retention);
    }
    
    /* Count parameter type annotations */
    ast_node_t *param_types[256];
    int param_count = get_method_parameter_type_nodes(method_ast, param_types, 256);
    for (int i = 0; i < param_count; i++) {
        count += count_type_annotations(param_types[i], retention);
    }
    
    return count;
}

/**
 * Write RuntimeVisibleTypeAnnotations for a method (return type and parameters).
 * Returns bytes written (0 if no type annotations).
 */
static int write_method_type_annotations_attribute(uint8_t **p, const_pool_t *cp,
                                                    ast_node_t *method_ast, bool runtime_visible)
{
    if (!method_ast) return 0;
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_method_type_annotations(method_ast, target);
    
    if (count == 0) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    const char *attr_name = runtime_visible ?
        "RuntimeVisibleTypeAnnotations" : "RuntimeInvisibleTypeAnnotations";
    uint16_t attr_name_idx = cp_add_utf8(cp, attr_name);
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* num_annotations */
    write_be_u2(p, (uint16_t)count);
    
    /* Write return type annotations */
    ast_node_t *return_type = get_method_return_type_node(method_ast);
    if (return_type && return_type->annotations) {
        for (slist_t *n = return_type->annotations; n; n = n->next) {
            ast_node_t *annot = (ast_node_t *)n->data;
            if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                if (get_annotation_retention(annot->data.node.name) == target) {
                    write_type_annotation(p, cp, TARGET_TYPE_RETURN, annot,
                                         return_type, return_type);
                }
            }
        }
    }
    
    /* Write parameter type annotations */
    ast_node_t *param_types[256];
    int param_count = get_method_parameter_type_nodes(method_ast, param_types, 256);
    for (int i = 0; i < param_count; i++) {
        ast_node_t *param_type = param_types[i];
        if (param_type && param_type->annotations) {
            for (slist_t *n = param_type->annotations; n; n = n->next) {
                ast_node_t *annot = (ast_node_t *)n->data;
                if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                    if (get_annotation_retention(annot->data.node.name) == target) {
                        /* For parameters, we need to temporarily set the index */
                        /* Write target_type */
                        write_be_u1(p, TARGET_TYPE_FORMAL_PARAMETER);
                        /* Write formal_parameter_target: u1 formal_parameter_index */
                        write_be_u1(p, (uint8_t)i);
                        /* Write type_path (empty for now) */
                        write_be_u1(p, 0);
                        /* Write annotation */
                        write_annotation(p, cp, annot);
                    }
                }
            }
        }
    }
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/**
 * Count type annotations on local variables in a method.
 */
static int count_local_var_type_annotations(slist_t *local_var_table, retention_policy_t retention)
{
    int count = 0;
    for (slist_t *lv = local_var_table; lv; lv = lv->next) {
        local_var_t *var = (local_var_t *)lv->data;
        if (var && var->type_ast) {
            count += count_type_annotations(var->type_ast, retention);
        }
    }
    return count;
}

/**
 * Write RuntimeVisibleTypeAnnotations for local variables (part of Code attribute).
 * Returns bytes written (0 if no type annotations).
 */
static int write_local_var_type_annotations_attribute(uint8_t **p, const_pool_t *cp,
                                                       slist_t *local_var_table, bool runtime_visible)
{
    if (!local_var_table) return 0;
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_local_var_type_annotations(local_var_table, target);
    
    if (count == 0) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    const char *attr_name = runtime_visible ?
        "RuntimeVisibleTypeAnnotations" : "RuntimeInvisibleTypeAnnotations";
    uint16_t attr_name_idx = cp_add_utf8(cp, attr_name);
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* num_annotations */
    write_be_u2(p, (uint16_t)count);
    
    /* Write type annotations for each local variable that has them */
    for (slist_t *lv = local_var_table; lv; lv = lv->next) {
        local_var_t *var = (local_var_t *)lv->data;
        if (var && var->type_ast && var->type_ast->annotations) {
            for (slist_t *n = var->type_ast->annotations; n; n = n->next) {
                ast_node_t *annot = (ast_node_t *)n->data;
                if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                    if (get_annotation_retention(annot->data.node.name) == target) {
                        /* Write target_type = LOCAL_VARIABLE (0x40) */
                        write_be_u1(p, TARGET_TYPE_LOCAL_VARIABLE);
                        
                        /* Write localvar_target: 
                         * u2 table_length + table_length * {u2 start_pc, u2 length, u2 index} */
                        write_be_u2(p, 1);  /* table_length = 1 */
                        write_be_u2(p, var->start_pc);
                        uint16_t var_length = var->length > 0 ? var->length : 0xFFFF;
                        write_be_u2(p, var_length);
                        write_be_u2(p, var->slot);
                        
                        /* Write type_path (empty for simple types) */
                        write_be_u1(p, 0);
                        
                        /* Write annotation */
                        write_annotation(p, cp, annot);
                    }
                }
            }
        }
    }
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/**
 * Count parameters that have any runtime-visible annotations.
 * Returns total number of runtime-visible annotations across all parameters.
 */
static int count_method_parameter_annotations(ast_node_t *method_ast, retention_policy_t retention)
{
    if (!method_ast) return 0;
    
    int total = 0;
    slist_t *children = method_ast->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && child->type == AST_PARAMETER && child->annotations) {
            total += count_annotations_with_retention(child->annotations, retention);
        }
    }
    return total;
}

/**
 * Count the number of parameters in a method.
 */
static int count_method_parameters(ast_node_t *method_ast)
{
    if (!method_ast) return 0;
    
    int count = 0;
    slist_t *children = method_ast->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && child->type == AST_PARAMETER) {
            count++;
        }
    }
    return count;
}

/**
 * Write RuntimeVisibleParameterAnnotations or RuntimeInvisibleParameterAnnotations.
 * Returns the total bytes written (0 if no parameter annotations to write).
 */
static int write_parameter_annotations_attribute(uint8_t **p, const_pool_t *cp, 
                                                  ast_node_t *method_ast, bool runtime_visible)
{
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    
    /* Count total parameter annotations */
    int total_annots = count_method_parameter_annotations(method_ast, target);
    if (total_annots == 0) return 0;
    
    int num_params = count_method_parameters(method_ast);
    if (num_params == 0) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    const char *attr_name = runtime_visible ? 
        "RuntimeVisibleParameterAnnotations" : "RuntimeInvisibleParameterAnnotations";
    uint16_t attr_name_idx = cp_add_utf8(cp, attr_name);
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* num_parameters (u1) */
    *(*p)++ = (uint8_t)num_params;
    
    /* For each parameter, write its annotations */
    slist_t *children = method_ast->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && child->type == AST_PARAMETER) {
            /* Count annotations for this parameter */
            int param_annots = count_annotations_with_retention(child->annotations, target);
            write_be_u2(p, (uint16_t)param_annots);
            
            /* Write each annotation */
            if (child->annotations) {
                for (slist_t *an = child->annotations; an; an = an->next) {
                    ast_node_t *annot = (ast_node_t *)an->data;
                    if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                        if (get_annotation_retention(annot->data.node.name) == target) {
                            write_annotation(p, cp, annot);
                        }
                    }
                }
            }
        }
    }
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/**
 * Pre-add constant pool entries for parameter annotations.
 */
static void preadd_parameter_annotations_cp(const_pool_t *cp, ast_node_t *method_ast, 
                                             retention_policy_t retention)
{
    if (!method_ast) return;
    
    slist_t *children = method_ast->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *child = (ast_node_t *)n->data;
        if (child && child->type == AST_PARAMETER && child->annotations) {
            preadd_annotations_list_cp(cp, child->annotations, retention);
        }
    }
}

/**
 * Pre-add annotation constant pool entries before writing the constant pool.
 */
static void preadd_annotation_cp_entries(const_pool_t *cp, ast_node_t *annot)
{
    if (!annot || annot->type != AST_ANNOTATION) return;
    
    /* Annotation type descriptor */
    if (annot->data.node.name) {
        char *type_desc = calloc(strlen(annot->data.node.name) + 4, 1);
        sprintf(type_desc, "L%s;", annot->data.node.name);
        for (char *c = type_desc; *c; c++) {
            if (*c == '.') *c = '/';
        }
        cp_add_utf8(cp, type_desc);
        free(type_desc);
    }
    
    /* Element names and values */
    slist_t *children = annot->data.node.children;
    for (slist_t *n = children; n; n = n->next) {
        ast_node_t *pair = (ast_node_t *)n->data;
        if (pair && pair->type == AST_ANNOTATION_VALUE && pair->data.node.name) {
            cp_add_utf8(cp, pair->data.node.name);
            
            /* Value - add string or integer constant */
            if (pair->data.node.children) {
                ast_node_t *value = (ast_node_t *)pair->data.node.children->data;
                if (value && value->type == AST_LITERAL) {
                    token_type_t tok = value->data.leaf.token_type;
                    if (tok == TOK_STRING_LITERAL && value->data.leaf.value.str_val) {
                        cp_add_utf8(cp, value->data.leaf.value.str_val);
                    } else if (tok == TOK_INTEGER_LITERAL) {
                        cp_add_integer(cp, (int32_t)value->data.leaf.value.int_val);
                    } else if (tok == TOK_TRUE || tok == TOK_FALSE) {
                        cp_add_integer(cp, tok == TOK_TRUE ? 1 : 0);
                    }
                } else if (value && value->type == AST_IDENTIFIER && value->data.leaf.name) {
                    cp_add_utf8(cp, value->data.leaf.name);
                }
            }
        }
    }
}

/**
 * Pre-add all annotation CP entries for a list of annotations.
 */
static void preadd_annotations_list_cp(const_pool_t *cp, slist_t *annotations, 
                                        retention_policy_t retention)
{
    for (slist_t *n = annotations; n; n = n->next) {
        ast_node_t *annot = (ast_node_t *)n->data;
        if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
            if (get_annotation_retention(annot->data.node.name) == retention) {
                preadd_annotation_cp_entries(cp, annot);
            }
        }
    }
}

/**
 * Pre-add constant pool entries for an annotation default value.
 */
static void preadd_annotation_default_cp(const_pool_t *cp, ast_node_t *value)
{
    if (!value) return;
    
    if (value->type == AST_LITERAL) {
        token_type_t tok = value->data.leaf.token_type;
        if (tok == TOK_STRING_LITERAL && value->data.leaf.value.str_val) {
            cp_add_utf8(cp, value->data.leaf.value.str_val);
        } else if (tok == TOK_INTEGER_LITERAL) {
            cp_add_integer(cp, (int32_t)value->data.leaf.value.int_val);
        } else if (tok == TOK_TRUE || tok == TOK_FALSE) {
            cp_add_integer(cp, tok == TOK_TRUE ? 1 : 0);
        } else if (tok == TOK_LONG_LITERAL) {
            cp_add_long(cp, value->data.leaf.value.int_val);
        } else if (tok == TOK_FLOAT_LITERAL || tok == TOK_DOUBLE_LITERAL) {
            cp_add_double(cp, value->data.leaf.value.float_val);
        }
    } else if (value->type == AST_IDENTIFIER && value->data.leaf.name) {
        cp_add_utf8(cp, value->data.leaf.name);
    } else if (value->type == AST_ANNOTATION) {
        preadd_annotation_cp_entries(cp, value);
    } else if (value->type == AST_ARRAY_INIT) {
        /* Array of values */
        slist_t *children = value->data.node.children;
        for (slist_t *n = children; n; n = n->next) {
            preadd_annotation_default_cp(cp, (ast_node_t *)n->data);
        }
    }
}

/**
 * Write AnnotationDefault attribute for annotation element methods.
 * Returns the total bytes written (0 if no default value).
 */
static int write_annotation_default_attribute(uint8_t **p, const_pool_t *cp, 
                                               ast_node_t *default_value)
{
    if (!default_value) return 0;
    
    uint8_t *start = *p;
    
    /* Attribute name */
    uint16_t attr_name_idx = cp_add_utf8(cp, "AnnotationDefault");
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* Write the default element_value */
    write_annotation_value(p, cp, default_value);
    
    /* Fill in attribute_length */
    uint32_t attr_len = (*p - len_pos) - 4;
    uint8_t *save = *p;
    *p = len_pos;
    write_be_u4(p, attr_len);
    *p = save;
    
    return *p - start;
}

/* ========================================================================
 * Class File Writing
 * ======================================================================== */

uint8_t *write_class_bytes(class_gen_t *cg, size_t *size)
{
    if (!cg || !size) {
        fprintf(stderr, "write_class_bytes: invalid input (cg=%p, size=%p)\n", (void*)cg, (void*)size);
        return NULL;
    }
    
    /* Pre-add all constant pool entries needed for LocalVariableTable.
     * This must be done before writing the constant pool. */
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        for (slist_t *lv = mi->local_var_table; lv; lv = lv->next) {
            local_var_t *var = (local_var_t *)lv->data;
            cp_add_utf8(cg->cp, var->name);
            cp_add_utf8(cg->cp, var->descriptor);
        }
    }
    
    /* Pre-add annotation constant pool entries for class */
    if (cg->class_ast && cg->class_ast->annotations) {
        cp_add_utf8(cg->cp, "RuntimeVisibleAnnotations");
        preadd_annotations_list_cp(cg->cp, cg->class_ast->annotations, RETENTION_RUNTIME);
    }
    
    /* Pre-add annotation constant pool entries for methods */
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        if (mi->ast && mi->ast->annotations) {
            int method_rt_annots = count_annotations_with_retention(mi->ast->annotations, RETENTION_RUNTIME);
            if (method_rt_annots > 0) {
                cp_add_utf8(cg->cp, "RuntimeVisibleAnnotations");
                preadd_annotations_list_cp(cg->cp, mi->ast->annotations, RETENTION_RUNTIME);
            }
        }
        /* Pre-add parameter annotation constant pool entries */
        if (mi->ast) {
            int param_rt_annots = count_method_parameter_annotations(mi->ast, RETENTION_RUNTIME);
            if (param_rt_annots > 0) {
                cp_add_utf8(cg->cp, "RuntimeVisibleParameterAnnotations");
                preadd_parameter_annotations_cp(cg->cp, mi->ast, RETENTION_RUNTIME);
            }
            /* Pre-add annotation default constant pool entries */
            if (mi->ast->annotation_default) {
                cp_add_utf8(cg->cp, "AnnotationDefault");
                preadd_annotation_default_cp(cg->cp, mi->ast->annotation_default);
            }
        }
    }
    
    /* Pre-add annotation constant pool entries for fields */
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_gen_t *fg = (field_gen_t *)node->data;
        if (fg->ast && fg->ast->annotations) {
            int field_rt_annots = count_annotations_with_retention(fg->ast->annotations, RETENTION_RUNTIME);
            if (field_rt_annots > 0) {
                cp_add_utf8(cg->cp, "RuntimeVisibleAnnotations");
                preadd_annotations_list_cp(cg->cp, fg->ast->annotations, RETENTION_RUNTIME);
            }
        }
    }
    
    /* Pre-add local variable type annotation CP entries */
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        for (slist_t *lv = mi->local_var_table; lv; lv = lv->next) {
            local_var_t *var = (local_var_t *)lv->data;
            if (var && var->type_ast && var->type_ast->annotations) {
                preadd_annotations_list_cp(cg->cp, var->type_ast->annotations, RETENTION_RUNTIME);
            }
        }
    }
    
    /* Pre-add Signature attribute constant pool entries */
    if (cg->signature) {
        cp_add_utf8(cg->cp, "Signature");
        cp_add_utf8(cg->cp, cg->signature);
    }
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        if (mi->signature) {
            cp_add_utf8(cg->cp, "Signature");
            cp_add_utf8(cg->cp, mi->signature);
        }
    }
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_gen_t *fg = (field_gen_t *)node->data;
        if (fg->signature) {
            cp_add_utf8(cg->cp, "Signature");
            cp_add_utf8(cg->cp, fg->signature);
        }
    }
    
    /* Pre-add exception class names to constant pool for Exceptions attribute */
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        for (slist_t *t = mi->throws; t; t = t->next) {
            const char *exc_internal = (const char *)t->data;
            cp_add_class(cg->cp, exc_internal);
        }
    }
    
    /* Add all attribute names to constant pool BEFORE we write it.
     * These must be added now since the constant pool is immutable after serialization. */
    uint16_t code_attr_name = cp_add_utf8(cg->cp, "Code");
    uint16_t lnt_attr_name = cp_add_utf8(cg->cp, "LineNumberTable");
    uint16_t lvt_attr_name = cp_add_utf8(cg->cp, "LocalVariableTable");
    uint16_t smt_attr_name = cp_add_utf8(cg->cp, "StackMapTable");
    uint16_t bsm_attr_name = cg->uses_invokedynamic ? cp_add_utf8(cg->cp, "BootstrapMethods") : 0;
    uint16_t ps_attr_name = cg->permitted_subclasses ? cp_add_utf8(cg->cp, "PermittedSubclasses") : 0;
    uint16_t nm_attr_name = cg->nest_members ? cp_add_utf8(cg->cp, "NestMembers") : 0;
    uint16_t nh_attr_name = cg->nest_host ? cp_add_utf8(cg->cp, "NestHost") : 0;
    uint16_t exc_attr_name_index = cp_add_utf8(cg->cp, "Exceptions");  /* For throws clauses */
    /* Pre-add RuntimeVisibleTypeAnnotations for local variable type annotations */
    cp_add_utf8(cg->cp, "RuntimeVisibleTypeAnnotations");
    
    /* Calculate size (approximate) */
    size_t buf_size = 1024 * 1024;  /* 1MB should be enough for most classes */
    uint8_t *buffer = malloc(buf_size);
    if (!buffer) {
        fprintf(stderr, "write_class_bytes: malloc failed for %zu bytes\n", buf_size);
        return NULL;
    }
    
    uint8_t *p = buffer;
    
    /* Magic number */
    write_be_u4(&p, 0xCAFEBABE);
    
    /* Determine class file version based on target and features used.
     * Higher target versions are required for certain features:
     * - Java 8 (52): lambdas, default/static interface methods, invokedynamic
     * - Java 16 (60): records
     * - Java 17 (61): sealed classes
     * We use the maximum of the explicit target and feature requirements. */
    int target_major = cg->target_version;
    
    /* Minimum versions for features */
    if (cg->has_default_methods || cg->uses_invokedynamic) {
        if (target_major < 52) target_major = 52;  /* Java 8 */
    }
    if (cg->permitted_subclasses) {
        if (target_major < 61) target_major = 61;  /* Java 17 for sealed classes */
    }
    if (cg->access_flags & ACC_RECORD) {
        if (target_major < 60) target_major = 60;  /* Java 16 for records */
    }
    
    /* Write version */
    if (target_major >= 50) {
        write_be_u2(&p, 0);           /* Minor version 0 for modern classes */
        write_be_u2(&p, target_major);
    } else {
        write_be_u2(&p, CLASS_MINOR_VERSION);  /* 3 for Java 1.1 */
        write_be_u2(&p, CLASS_MAJOR_VERSION);  /* 45 for Java 1.1 */
    }
    
    /* Constant pool */
    write_be_u2(&p, cg->cp->count);
    
    for (uint16_t i = 1; i < cg->cp->count; i++) {
        const_pool_entry_t *entry = &cg->cp->entries[i];
        *p++ = entry->type;
        
        switch (entry->type) {
            case CONST_UTF8:
                {
                    uint16_t len = strlen(entry->data.utf8);
                    write_be_u2(&p, len);
                    memcpy(p, entry->data.utf8, len);
                    p += len;
                }
                break;
            
            case CONST_INTEGER:
                write_be_u4(&p, (uint32_t)entry->data.integer);
                break;
            
            case CONST_FLOAT:
                {
                    uint32_t bits;
                    memcpy(&bits, &entry->data.float_val, 4);
                    write_be_u4(&p, bits);
                }
                break;
            
            case CONST_LONG:
                write_be_u4(&p, (uint32_t)(entry->data.long_val >> 32));
                write_be_u4(&p, (uint32_t)entry->data.long_val);
                i++;  /* Skip next slot */
                break;
            
            case CONST_DOUBLE:
                {
                    uint64_t bits;
                    memcpy(&bits, &entry->data.double_val, 8);
                    write_be_u4(&p, (uint32_t)(bits >> 32));
                    write_be_u4(&p, (uint32_t)bits);
                    i++;  /* Skip next slot */
                }
                break;
            
            case CONST_CLASS:
            case CONST_STRING:
            case CONST_METHOD_TYPE:
                write_be_u2(&p, entry->data.class_index);
                break;
            
            case CONST_FIELDREF:
            case CONST_METHODREF:
            case CONST_INTERFACE_METHODREF:
                write_be_u2(&p, entry->data.ref.class_index);
                write_be_u2(&p, entry->data.ref.name_type_index);
                break;
            
            case CONST_NAME_AND_TYPE:
                write_be_u2(&p, entry->data.name_type.name_index);
                write_be_u2(&p, entry->data.name_type.descriptor_index);
                break;
            
            case CONST_METHOD_HANDLE:
                /* Format: reference_kind (u1), reference_index (u2) */
                *p++ = (uint8_t)entry->data.ref.class_index;  /* reference_kind stored here */
                write_be_u2(&p, entry->data.ref.name_type_index);  /* reference_index */
                break;
            
            case CONST_INVOKE_DYNAMIC:
                /* Format: bootstrap_method_attr_index (u2), name_and_type_index (u2) */
                write_be_u2(&p, entry->data.ref.class_index);  /* bootstrap_method_attr_index */
                write_be_u2(&p, entry->data.ref.name_type_index);  /* name_and_type_index */
                break;
            
            default:
                break;
        }
    }
    
    /* Access flags */
    write_be_u2(&p, cg->access_flags);
    
    /* This class */
    write_be_u2(&p, cg->this_class);
    
    /* Super class */
    write_be_u2(&p, cg->super_class);
    
    /* Interfaces */
    uint16_t interface_count = 0;
    for (slist_t *node = cg->interfaces; node; node = node->next) {
        interface_count++;
    }
    write_be_u2(&p, interface_count);
    for (slist_t *node = cg->interfaces; node; node = node->next) {
        write_be_u2(&p, (uint16_t)(uintptr_t)node->data);
    }
    
    /* Fields */
    uint16_t field_count = 0;
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_count++;
    }
    write_be_u2(&p, field_count);
    
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_gen_t *fg = (field_gen_t *)node->data;
        write_be_u2(&p, fg->access_flags);
        write_be_u2(&p, fg->name_index);
        write_be_u2(&p, fg->descriptor_index);
        
        /* Count field attributes */
        int field_attr_count = 0;
        
        /* Count field-level annotations with RUNTIME retention */
        int field_rt_annots = 0;
        if (fg->ast && fg->ast->annotations) {
            field_rt_annots = count_annotations_with_retention(fg->ast->annotations, RETENTION_RUNTIME);
        }
        if (field_rt_annots > 0) field_attr_count++;
        
        /* Count field type annotations with RUNTIME retention */
        ast_node_t *field_type_node = get_field_type_node(fg->ast);
        int field_type_annots = 0;
        if (field_type_node) {
            field_type_annots = count_type_annotations(field_type_node, RETENTION_RUNTIME);
        }
        if (field_type_annots > 0) field_attr_count++;
        
        /* Count Signature attribute */
        if (fg->signature) field_attr_count++;
        
        write_be_u2(&p, field_attr_count);
        
        /* Signature attribute (for field) */
        if (fg->signature) {
            uint16_t sig_attr_name = cp_add_utf8(cg->cp, "Signature");
            uint16_t sig_index = cp_add_utf8(cg->cp, fg->signature);
            write_be_u2(&p, sig_attr_name);
            write_be_u4(&p, 2);  /* attribute_length is always 2 */
            write_be_u2(&p, sig_index);
        }
        
        /* RuntimeVisibleAnnotations attribute (for field) */
        if (field_rt_annots > 0) {
            write_annotations_attribute(&p, cg->cp, fg->ast->annotations, true);
        }
        
        /* RuntimeVisibleTypeAnnotations attribute (for field type) */
        if (field_type_annots > 0) {
            write_field_type_annotations_attribute(&p, cg->cp, field_type_node, true);
        }
    }
    
    /* Methods */
    uint16_t method_count = 0;
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_count++;
    }
    write_be_u2(&p, method_count);
    
    /* Method attribute names are already added to constant pool at the start */
    for (slist_t *node = cg->methods; node; node = node->next) {
        method_info_gen_t *mi = (method_info_gen_t *)node->data;
        write_be_u2(&p, mi->access_flags);
        write_be_u2(&p, mi->name_index);
        write_be_u2(&p, mi->descriptor_index);
        
        /* Count method-level annotations with RUNTIME retention */
        int method_rt_annots = 0;
        if (mi->ast && mi->ast->annotations) {
            method_rt_annots = count_annotations_with_retention(mi->ast->annotations, RETENTION_RUNTIME);
        }
        
        /* Count parameter annotations with RUNTIME retention */
        int param_rt_annots = 0;
        if (mi->ast) {
            param_rt_annots = count_method_parameter_annotations(mi->ast, RETENTION_RUNTIME);
        }
        
        /* Count type annotations on return type and parameter types */
        int method_type_annots = 0;
        if (mi->ast) {
            method_type_annots = count_method_type_annotations(mi->ast, RETENTION_RUNTIME);
        }
        
        /* Check for annotation default value */
        bool has_annotation_default = mi->ast && mi->ast->annotation_default;
        
        /* Abstract methods (and interface methods) have no Code attribute */
        if (mi->code == NULL) {
            /* Count throws types */
            int throws_count = 0;
            for (slist_t *t = mi->throws; t; t = t->next) throws_count++;
            
            /* Count attributes: annotations + signature + parameter annotations + annotation default + type annotations + exceptions */
            int abstract_attr_count = 0;
            if (throws_count > 0) abstract_attr_count++;  /* Exceptions attribute */
            if (method_rt_annots > 0) abstract_attr_count++;
            if (param_rt_annots > 0) abstract_attr_count++;
            if (method_type_annots > 0) abstract_attr_count++;
            if (mi->signature) abstract_attr_count++;
            if (has_annotation_default) abstract_attr_count++;
            
            write_be_u2(&p, abstract_attr_count);
            
            /* Exceptions attribute (throws clause) */
            if (throws_count > 0) {
                write_be_u2(&p, exc_attr_name_index);
                write_be_u4(&p, 2 + throws_count * 2);  /* length: 2 bytes count + 2 bytes per exception */
                write_be_u2(&p, (uint16_t)throws_count);
                for (slist_t *t = mi->throws; t; t = t->next) {
                    const char *exc_internal = (const char *)t->data;
                    uint16_t exc_class = cp_add_class(cg->cp, exc_internal);  /* Returns pre-added entry (deduplicated) */
                    write_be_u2(&p, exc_class);
                }
            }
            
            /* Signature attribute */
            if (mi->signature) {
                uint16_t sig_attr_name = cp_add_utf8(cg->cp, "Signature");
                uint16_t sig_index = cp_add_utf8(cg->cp, mi->signature);
                write_be_u2(&p, sig_attr_name);
                write_be_u4(&p, 2);
                write_be_u2(&p, sig_index);
            }
            
            if (method_rt_annots > 0) {
                write_annotations_attribute(&p, cg->cp, mi->ast->annotations, true);
            }
            
            if (param_rt_annots > 0) {
                write_parameter_annotations_attribute(&p, cg->cp, mi->ast, true);
            }
            
            if (method_type_annots > 0) {
                write_method_type_annotations_attribute(&p, cg->cp, mi->ast, true);
            }
            
            if (has_annotation_default) {
                write_annotation_default_attribute(&p, cg->cp, mi->ast->annotation_default);
            }
            continue;
        }
        
        /* Count throws types for Exceptions attribute */
        int concrete_throws_count = 0;
        for (slist_t *t = mi->throws; t; t = t->next) concrete_throws_count++;
        
        /* Count method attributes: Code + optional Signature + optional annotations + parameter annotations + type annotations + exceptions */
        int method_attr_count = 1;  /* Code is always present for concrete methods */
        if (concrete_throws_count > 0) method_attr_count++;  /* Exceptions attribute */
        if (mi->signature) method_attr_count++;
        if (method_rt_annots > 0) method_attr_count++;
        if (param_rt_annots > 0) method_attr_count++;
        if (method_type_annots > 0) method_attr_count++;
        write_be_u2(&p, method_attr_count);
        
        /* Code attribute */
        write_be_u2(&p, code_attr_name);
        
        /* Count exception handlers */
        uint16_t exception_count = 0;
        for (slist_t *eh = mi->exception_handlers; eh; eh = eh->next) {
            exception_count++;
        }
        
        /* Count line number entries */
        uint16_t lnt_count = 0;
        for (slist_t *ln = mi->line_numbers; ln; ln = ln->next) {
            lnt_count++;
        }
        
        /* Count local variable entries */
        uint16_t lvt_count = 0;
        for (slist_t *lv = mi->local_var_table; lv; lv = lv->next) {
            lvt_count++;
        }
        
        /* Serialize StackMapTable if present */
        uint8_t *smt_data = NULL;
        uint32_t smt_len = 0;
        if (mi->stackmap && mi->stackmap->num_entries > 0) {
            smt_data = stackmap_serialize(mi->stackmap, cg->cp, &smt_len);
        }
        
        /* Count local variable type annotations */
        int lvt_type_annots = count_local_var_type_annotations(mi->local_var_table, RETENTION_RUNTIME);
        
        /* Pre-serialize local variable type annotations to get size */
        uint8_t *lvt_annot_data = NULL;
        uint32_t lvt_annot_len = 0;
        if (lvt_type_annots > 0) {
            /* Allocate buffer for type annotations (max 64KB should be plenty) */
            lvt_annot_data = malloc(65536);
            if (lvt_annot_data) {
                uint8_t *annot_ptr = lvt_annot_data;
                lvt_annot_len = write_local_var_type_annotations_attribute(&annot_ptr, cg->cp,
                                                                            mi->local_var_table, true);
            }
        }
        
        /* Count Code sub-attributes */
        uint16_t code_attr_count_val = 0;
        if (smt_data && smt_len > 0) code_attr_count_val++;
        if (lnt_count > 0) code_attr_count_val++;
        if (lvt_count > 0) code_attr_count_val++;
        if (lvt_annot_data && lvt_annot_len > 0) code_attr_count_val++;
        
        /* Code attribute length:
         * 2 (max_stack) + 2 (max_locals) + 4 (code_length) + code_length
         * + 2 (exception_table_length) + exception_count * 8
         * + 2 (attributes_count)
         * + StackMapTable attribute (if present): 2+4 (header) + smt_len (content)
         * + LineNumberTable attribute (if present): 2+4 (header) + 2+4*count (content)
         * + LocalVariableTable attribute (if present): 2+4 (header) + 2+10*count (content)
         * + RuntimeVisibleTypeAnnotations (if present): already serialized */
        uint32_t code_attr_len = 2 + 2 + 4 + mi->code->length + 2 + exception_count * 8 + 2;
        if (smt_data && smt_len > 0) {
            code_attr_len += 2 + 4 + smt_len;  /* name_idx(2) + attr_len(4) + data */
        }
        if (lnt_count > 0) {
            code_attr_len += 2 + 4 + 2 + lnt_count * 4;  /* name_idx(2) + attr_len(4) + count(2) + entries(4*n) */
        }
        if (lvt_count > 0) {
            code_attr_len += 2 + 4 + 2 + lvt_count * 10; /* name_idx(2) + attr_len(4) + count(2) + entries(10*n) */
        }
        if (lvt_annot_data && lvt_annot_len > 0) {
            code_attr_len += lvt_annot_len;  /* Already includes header */
        }
        write_be_u4(&p, code_attr_len);
        
        write_be_u2(&p, mi->code->max_stack);
        write_be_u2(&p, mi->code->max_locals);
        write_be_u4(&p, mi->code->length);
        
        memcpy(p, mi->code->code, mi->code->length);
        p += mi->code->length;
        
        /* Exception table */
        write_be_u2(&p, exception_count);
        for (slist_t *eh = mi->exception_handlers; eh; eh = eh->next) {
            exception_entry_t *entry = (exception_entry_t *)eh->data;
            write_be_u2(&p, entry->start_pc);
            write_be_u2(&p, entry->end_pc);
            write_be_u2(&p, entry->handler_pc);
            write_be_u2(&p, entry->catch_type);
        }
        
        /* Code attributes */
        write_be_u2(&p, code_attr_count_val);
        
        /* StackMapTable attribute (must come first per JVM spec) */
        if (smt_data && smt_len > 0) {
            write_be_u2(&p, smt_attr_name);
            write_be_u4(&p, smt_len);
            memcpy(p, smt_data, smt_len);
            p += smt_len;
            free(smt_data);
        }
        
        /* LineNumberTable attribute */
        if (lnt_count > 0) {
            write_be_u2(&p, lnt_attr_name);
            write_be_u4(&p, 2 + lnt_count * 4);  /* attribute_length */
            write_be_u2(&p, lnt_count);
            for (slist_t *ln = mi->line_numbers; ln; ln = ln->next) {
                line_number_entry_t *entry = (line_number_entry_t *)ln->data;
                write_be_u2(&p, entry->start_pc);
                write_be_u2(&p, entry->line_number);
            }
        }
        
        /* LocalVariableTable attribute */
        if (lvt_count > 0) {
            write_be_u2(&p, lvt_attr_name);
            write_be_u4(&p, 2 + lvt_count * 10);  /* attribute_length */
            write_be_u2(&p, lvt_count);
            for (slist_t *lv = mi->local_var_table; lv; lv = lv->next) {
                local_var_t *var = (local_var_t *)lv->data;
                uint16_t name_idx = cp_add_utf8(cg->cp, var->name);
                uint16_t desc_idx = cp_add_utf8(cg->cp, var->descriptor);
                /* Use code_length as length since variable scope is typically the entire method */
                uint16_t var_length = var->length > 0 ? var->length : (mi->code->length - var->start_pc);
                write_be_u2(&p, var->start_pc);
                write_be_u2(&p, var_length);
                write_be_u2(&p, name_idx);
                write_be_u2(&p, desc_idx);
                write_be_u2(&p, var->slot);
            }
        }
        
        /* RuntimeVisibleTypeAnnotations attribute (for local variables, inside Code) */
        if (lvt_annot_data && lvt_annot_len > 0) {
            memcpy(p, lvt_annot_data, lvt_annot_len);
            p += lvt_annot_len;
            free(lvt_annot_data);
        }
        
        /* Signature attribute (for method) */
        if (mi->signature) {
            uint16_t sig_attr_name = cp_add_utf8(cg->cp, "Signature");
            uint16_t sig_index = cp_add_utf8(cg->cp, mi->signature);
            write_be_u2(&p, sig_attr_name);
            write_be_u4(&p, 2);
            write_be_u2(&p, sig_index);
        }
        
        /* Exceptions attribute (throws clause) */
        if (concrete_throws_count > 0) {
            write_be_u2(&p, exc_attr_name_index);
            write_be_u4(&p, 2 + concrete_throws_count * 2);  /* length: 2 bytes count + 2 bytes per exception */
            write_be_u2(&p, (uint16_t)concrete_throws_count);
            for (slist_t *t = mi->throws; t; t = t->next) {
                const char *exc_internal = (const char *)t->data;
                uint16_t exc_class = cp_add_class(cg->cp, exc_internal);  /* Returns pre-added entry (deduplicated) */
                write_be_u2(&p, exc_class);
            }
        }
        
        /* RuntimeVisibleAnnotations attribute (for method) */
        if (method_rt_annots > 0) {
            write_annotations_attribute(&p, cg->cp, mi->ast->annotations, true);
        }
        
        /* RuntimeVisibleParameterAnnotations attribute (for method) */
        if (param_rt_annots > 0) {
            write_parameter_annotations_attribute(&p, cg->cp, mi->ast, true);
        }
        
        /* RuntimeVisibleTypeAnnotations attribute (for method return/parameters) */
        if (method_type_annots > 0) {
            write_method_type_annotations_attribute(&p, cg->cp, mi->ast, true);
        }
    }
    
    /* Class attributes */
    uint16_t class_attr_count = 0;
    if (cg->inner_class_entries) class_attr_count++;
    if (cg->signature) class_attr_count++;
    if (cg->bootstrap_methods && cg->bootstrap_methods->count > 0) class_attr_count++;
    if (cg->permitted_subclasses) class_attr_count++;
    if (cg->nest_members) class_attr_count++;  /* NestMembers (Java 11+) */
    if (cg->nest_host) class_attr_count++;     /* NestHost (Java 11+) */
    
    /* Check for class-level annotations with RUNTIME retention */
    int runtime_annot_count = 0;
    if (cg->class_ast && cg->class_ast->annotations) {
        runtime_annot_count = count_annotations_with_retention(
            cg->class_ast->annotations, RETENTION_RUNTIME);
        if (runtime_annot_count > 0) class_attr_count++;
    }
    
    write_be_u2(&p, class_attr_count);
    
    /* Signature attribute (for class) */
    if (cg->signature) {
        uint16_t sig_attr_name = cp_add_utf8(cg->cp, "Signature");
        uint16_t sig_index = cp_add_utf8(cg->cp, cg->signature);
        write_be_u2(&p, sig_attr_name);
        write_be_u4(&p, 2);
        write_be_u2(&p, sig_index);
    }
    
    /* RuntimeVisibleAnnotations attribute (for class) */
    if (runtime_annot_count > 0) {
        write_annotations_attribute(&p, cg->cp, cg->class_ast->annotations, true);
    }
    
    /* InnerClasses attribute */
    if (cg->inner_class_entries) {
        uint16_t ic_attr_name = cp_add_utf8(cg->cp, "InnerClasses");
        write_be_u2(&p, ic_attr_name);
        
        /* Count inner class entries */
        uint16_t ic_count = 0;
        for (slist_t *node = cg->inner_class_entries; node; node = node->next) {
            ic_count++;
        }
        
        /* Attribute length: 2 (number_of_classes) + ic_count * 8 */
        uint32_t ic_attr_len = 2 + ic_count * 8;
        write_be_u4(&p, ic_attr_len);
        
        write_be_u2(&p, ic_count);
        
        for (slist_t *node = cg->inner_class_entries; node; node = node->next) {
            inner_class_entry_t *entry = (inner_class_entry_t *)node->data;
            write_be_u2(&p, entry->inner_class_info);
            write_be_u2(&p, entry->outer_class_info);
            write_be_u2(&p, entry->inner_name);
            write_be_u2(&p, entry->access_flags);
        }
    }
    
    /* BootstrapMethods attribute (for invokedynamic) */
    if (cg->bootstrap_methods && cg->bootstrap_methods->count > 0 && bsm_attr_name) {
        write_be_u2(&p, bsm_attr_name);
        
        /* Calculate attribute length:
         * 2 bytes for num_bootstrap_methods
         * For each entry: 2 (bootstrap_method_ref) + 2 (num_args) + 2*num_args */
        uint32_t bsm_attr_len = 2;
        for (uint16_t i = 0; i < cg->bootstrap_methods->count; i++) {
            bsm_attr_len += 4 + 2 * cg->bootstrap_methods->methods[i].num_arguments;
        }
        write_be_u4(&p, bsm_attr_len);
        
        /* Write number of bootstrap methods */
        write_be_u2(&p, cg->bootstrap_methods->count);
        
        /* Write each bootstrap method entry */
        for (uint16_t i = 0; i < cg->bootstrap_methods->count; i++) {
            bootstrap_method_t *bm = &cg->bootstrap_methods->methods[i];
            write_be_u2(&p, bm->method_handle_index);
            write_be_u2(&p, bm->num_arguments);
            for (uint16_t j = 0; j < bm->num_arguments; j++) {
                write_be_u2(&p, bm->arguments[j]);
            }
        }
    }
    
    /* PermittedSubclasses attribute (Java 17+ sealed classes) */
    if (cg->permitted_subclasses && ps_attr_name) {
        write_be_u2(&p, ps_attr_name);
        
        /* Count permitted subclasses */
        uint16_t ps_count = 0;
        for (slist_t *node = cg->permitted_subclasses; node; node = node->next) {
            ps_count++;
        }
        
        /* Attribute length: 2 (number_of_classes) + 2 * ps_count */
        uint32_t ps_attr_len = 2 + 2 * ps_count;
        write_be_u4(&p, ps_attr_len);
        
        write_be_u2(&p, ps_count);
        
        /* Write each permitted subclass class_info index */
        for (slist_t *node = cg->permitted_subclasses; node; node = node->next) {
            uint16_t *class_idx = (uint16_t *)node->data;
            write_be_u2(&p, *class_idx);
        }
    }
    
    /* NestMembers attribute (Java 11+) - for nest host class */
    if (cg->nest_members && nm_attr_name) {
        write_be_u2(&p, nm_attr_name);
        
        /* Count nest members */
        uint16_t nm_count = 0;
        for (slist_t *node = cg->nest_members; node; node = node->next) {
            nm_count++;
        }
        
        /* Attribute length: 2 (number_of_classes) + 2 * nm_count */
        uint32_t nm_attr_len = 2 + 2 * nm_count;
        write_be_u4(&p, nm_attr_len);
        
        write_be_u2(&p, nm_count);
        
        /* Write each nest member class_info index */
        for (slist_t *node = cg->nest_members; node; node = node->next) {
            uint16_t *class_idx = (uint16_t *)node->data;
            write_be_u2(&p, *class_idx);
        }
    }
    
    /* NestHost attribute (Java 11+) - for nested classes */
    if (cg->nest_host && nh_attr_name) {
        write_be_u2(&p, nh_attr_name);
        
        /* Attribute length: 2 (host_class_info index) */
        write_be_u4(&p, 2);
        
        write_be_u2(&p, cg->nest_host);
    }
    
    *size = p - buffer;
    return buffer;
}

bool write_class_file(class_gen_t *cg, const char *output_path)
{
    size_t size;
    uint8_t *data = write_class_bytes(cg, &size);
    if (!data) {
        return false;
    }
    
    FILE *fp = fopen(output_path, "wb");
    if (!fp) {
        free(data);
        return false;
    }
    
    size_t written = fwrite(data, 1, size, fp);
    fclose(fp);
    free(data);
    
    return written == size;
}
