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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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

/**
 * A "static final" field's own ConstantValue attribute (JVMS 4.7.2) -
 * javac always emits one for a "constant variable" (JLS 4.12.4: static
 * final, of primitive or String type, initialized with a constant
 * expression); genesis instead always initializes such fields via
 * <clinit> (see the README's own documented note on this difference),
 * which works for ordinary runtime use but leaves nothing for another
 * compile unit's own compile-time constant folding to read - e.g. a
 * switch case label in a DIFFERENT file/module referencing this field by
 * name needs its value baked into the classfile at COMPILE time, not
 * read at class-INIT time. Handles a plain literal initializer (the
 * common case for a real constant declaration) and a unary +/- wrapping
 * one; a field whose initializer is itself a more complex expression
 * (referencing OTHER constants, arithmetic, ...) is intentionally left
 * without a ConstantValue attribute here, matching this file's existing
 * philosophy elsewhere of handling the concrete shape actually needed
 * rather than a fully general constant-expression evaluator - genesis's
 * own <clinit> initialization still runs regardless, so such a field
 * still works correctly for every use except cross-module case-label
 * folding of that specific shape. Confirmed against gumdrop's own
 * PageContext (jakarta.servlet.jsp) - "public static final int
 * PAGE_SCOPE = 1;" and its three siblings, referenced as bare case
 * labels by a DIFFERENT module's ELEvaluatorTest - exactly this shape.
 * Returns a constant pool index (a CONSTANT_Integer/Long/Float/Double/
 * String entry matching the field's own descriptor) for the
 * ConstantValue attribute to reference, or 0 if this field isn't
 * eligible (not static final, not a primitive/String type, or its
 * initializer isn't one of the two shapes handled above).
 */
static uint16_t field_gen_constant_value_index(const_pool_t *cp, field_gen_t *fg)
{
    if (!fg || !fg->descriptor || !fg->ast) {
        return 0;
    }
    if ((fg->access_flags & (ACC_STATIC | ACC_FINAL)) != (ACC_STATIC | ACC_FINAL)) {
        return 0;
    }
    char desc0 = fg->descriptor[0];
    bool is_string = strcmp(fg->descriptor, "Ljava/lang/String;") == 0;
    if (!is_string && strchr("ZBCSIJFD", desc0) == NULL) {
        return 0;  /* Not a primitive or String field */
    }

    /* Find the AST_VAR_DECLARATOR among fg->ast's children matching this
     * field's own name - fg->ast is the whole AST_FIELD_DECL, which may
     * declare several fields at once (e.g. "static final int A = 1, B = 2;"),
     * each with its own field_gen_t sharing the same ->ast. */
    ast_node_t *declarator = NULL;
    for (slist_t *c = fg->ast->data.node.children; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (child->type == AST_VAR_DECLARATOR && child->data.node.name &&
            strcmp(child->data.node.name, fg->name) == 0) {
            declarator = child;
            break;
        }
    }
    if (!declarator || !declarator->data.node.children) {
        return 0;
    }
    ast_node_t *init_expr = (ast_node_t *)declarator->data.node.children->data;

    /* Unwrap a single unary +/- (e.g. "= -1;"), the only compound shape
     * handled here. */
    bool negate = false;
    if (init_expr->type == AST_UNARY_EXPR &&
        (init_expr->data.node.op_token == TOK_MINUS || init_expr->data.node.op_token == TOK_PLUS) &&
        init_expr->data.node.children) {
        negate = (init_expr->data.node.op_token == TOK_MINUS);
        init_expr = (ast_node_t *)init_expr->data.node.children->data;
    }

    if (init_expr->type == AST_LITERAL) {
        token_type_t tt = init_expr->data.leaf.token_type;
        if (is_string) {
            if (tt == TOK_STRING_LITERAL && init_expr->data.leaf.value.str_val) {
                return cp_add_string(cp, init_expr->data.leaf.value.str_val);
            }
            return 0;
        }
        switch (desc0) {
            case 'J':
                if (tt == TOK_INTEGER_LITERAL || tt == TOK_LONG_LITERAL) {
                    int64_t v = init_expr->data.leaf.value.int_val;
                    return cp_add_long(cp, negate ? -v : v);
                }
                return 0;
            case 'F':
                if (tt == TOK_FLOAT_LITERAL || tt == TOK_INTEGER_LITERAL) {
                    double v = (tt == TOK_FLOAT_LITERAL) ? init_expr->data.leaf.value.float_val
                                                          : (double)init_expr->data.leaf.value.int_val;
                    return cp_add_float(cp, (float)(negate ? -v : v));
                }
                return 0;
            case 'D':
                if (tt == TOK_DOUBLE_LITERAL || tt == TOK_FLOAT_LITERAL || tt == TOK_INTEGER_LITERAL) {
                    double v = (tt == TOK_INTEGER_LITERAL) ? (double)init_expr->data.leaf.value.int_val
                                                            : init_expr->data.leaf.value.float_val;
                    return cp_add_double(cp, negate ? -v : v);
                }
                return 0;
            case 'Z':
                if (tt == TOK_TRUE) {
                    return cp_add_integer(cp, 1);
                }
                if (tt == TOK_FALSE) {
                    return cp_add_integer(cp, 0);
                }
                return 0;
            case 'C':
                if (tt == TOK_CHAR_LITERAL && init_expr->data.leaf.value.str_val) {
                    int32_t v = (unsigned char)init_expr->data.leaf.value.str_val[0];
                    return cp_add_integer(cp, negate ? -v : v);
                }
                return 0;
            default:  /* B, S, I */
                if (tt == TOK_INTEGER_LITERAL) {
                    int32_t v = (int32_t)init_expr->data.leaf.value.int_val;
                    return cp_add_integer(cp, negate ? -v : v);
                }
                return 0;
        }
    }
    return 0;
}

/* Defined in codegen_expr.c (declared there in codegen_internal.h) - builds
 * a JVM type descriptor from a type AST node (AST_PRIMITIVE_TYPE,
 * AST_CLASS_TYPE, AST_ARRAY_TYPE, ...). Reused here for a class-literal
 * annotation element value's own 'c'-tagged descriptor (see
 * write_annotation_value()'s AST_CLASS_LITERAL case). */
char *ast_type_to_descriptor(ast_node_t *type_node);

/*
 * The semantic_t for the file currently being written to a classfile, so
 * that get_annotation_retention() and write_annotation() can resolve a
 * bare annotation name (e.g. "Test") against that file's imports and the
 * classpath, without threading a sem parameter through every annotation
 * helper below. Set at the top of write_class_bytes(); thread-local since
 * compile_parallel() writes multiple classes concurrently on different
 * threads, each with its own semantic_t (mirrors g_current_semantic in
 * semantic.c).
 */
static __thread semantic_t *g_classwriter_sem = NULL;

/**
 * Resolve an annotation type name (as parsed from source, e.g. "Test" or
 * already-qualified "java.lang.Override") to a fully qualified, dot-
 * separated class name using the current file's imports. Falls back to
 * returning the input unchanged if it's already qualified, or if no
 * semantic_t/resolution is available - callers must tolerate an
 * unqualified result either way.
 */
static const char *resolve_annotation_qualified_name(const char *name)
{
    if (!name || strchr(name, '.') != NULL || !g_classwriter_sem) {
        return name;
    }
    char *qualified = semantic_resolve_annotation_type_name(g_classwriter_sem, name);
    if (!qualified) {
        return name;
    }
    const char *interned = intern(qualified);
    free(qualified);
    return interned;
}

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

    /* Unknown annotation - for one defined outside the JDK builtins above
     * (e.g. JUnit's @Test), consult its own @Retention meta-annotation via
     * the classpath rather than assuming the CLASS default outright, since
     * most third-party annotations meant for reflective use declare RUNTIME. */
    if (g_classwriter_sem) {
        return semantic_resolve_annotation_retention(g_classwriter_sem, annotation_name);
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
 *
 * qualified_annotation_name/element_name (either may be NULL) let a
 * numeric literal be widened to the element's *declared* return type
 * (e.g. "@Test(timeout = 10000)", an int literal, where
 * org.junit.Test.timeout() returns long) - without resolving this, the
 * literal was always written as a plain int constant, which the JVM
 * accepts at class-load time but rejects at reflection time with
 * AnnotationTypeMismatchException, since the element_value's own tag
 * must match the annotation interface method's real return type.
 */
static int write_annotation_value(uint8_t **p, const_pool_t *cp, ast_node_t *value,
                                   const char *qualified_annotation_name,
                                   const char *element_name)
{
    if (!value) {
        return 0;
    }

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
            /* Integer value - but the element's declared return type may
             * be wider than int (long/float/double) or narrower (byte/
             * short/char), which a constant-expression int literal is
             * allowed to target; the element_value's tag must reflect
             * that declared type, not just the literal's own int shape. */
            char ret_tag = 'I';
            if (g_classwriter_sem && qualified_annotation_name && element_name) {
                char *desc = semantic_resolve_annotation_element_descriptor(
                    g_classwriter_sem, qualified_annotation_name, element_name);
                if (desc) {
                    const char *rparen = strchr(desc, ')');
                    if (rparen && rparen[1]) {
                        ret_tag = rparen[1];
                    }
                    free(desc);
                }
            }
            long long ival = value->data.leaf.value.int_val;
            switch (ret_tag) {
                case 'J': {
                    *(*p)++ = 'J';
                    uint16_t idx = cp_add_long(cp, (int64_t)ival);
                    write_be_u2(p, idx);
                    break;
                }
                case 'F': {
                    *(*p)++ = 'F';
                    uint16_t idx = cp_add_float(cp, (float)ival);
                    write_be_u2(p, idx);
                    break;
                }
                case 'D': {
                    *(*p)++ = 'D';
                    uint16_t idx = cp_add_double(cp, (double)ival);
                    write_be_u2(p, idx);
                    break;
                }
                case 'B': case 'C': case 'S': case 'Z': {
                    *(*p)++ = (uint8_t)ret_tag;
                    uint16_t idx = cp_add_integer(cp, (int32_t)ival);
                    write_be_u2(p, idx);
                    break;
                }
                default: {
                    *(*p)++ = 'I';
                    uint16_t idx = cp_add_integer(cp, (int32_t)ival);
                    write_be_u2(p, idx);
                    break;
                }
            }
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
    } else if (value->type == AST_FIELD_ACCESS && value->data.node.name &&
               value->data.node.children) {
        /* Qualified enum constant, e.g. RetentionPolicy.RUNTIME - the
         * ordinary way to write an enum-typed annotation element. Without
         * this, the element_value_pair's value was silently skipped
         * (0 bytes written), desyncing the rest of the annotation's binary
         * layout relative to its declared num_element_value_pairs. */
        ast_node_t *receiver = (ast_node_t *)value->data.node.children->data;
        const char *enum_type_name = (receiver && receiver->type == AST_IDENTIFIER) ?
            receiver->data.leaf.name : NULL;
        if (!enum_type_name) {
            return 0;
        }
        *(*p)++ = 'e';  /* tag for enum constant */
        const char *qualified = resolve_annotation_qualified_name(enum_type_name);
        char *type_desc = calloc(strlen(qualified) + 4, 1);
        sprintf(type_desc, "L%s;", qualified);
        for (char *c = type_desc; *c; c++) {
            if (*c == '.') {
                *c = '/';
            }
        }
        uint16_t type_idx = cp_add_utf8(cp, type_desc);
        free(type_desc);
        write_be_u2(p, type_idx);
        uint16_t const_idx = cp_add_utf8(cp, value->data.node.name);
        write_be_u2(p, const_idx);
    } else if (value->type == AST_CLASS_LITERAL) {
        /* Class-valued element, e.g. @Test(expected = IllegalStateException.class) -
         * exactly like the enum-constant case above, this had NO case at
         * all here: the "Unknown value type - skip" fallback silently
         * wrote 0 bytes for the value while the element_name_index and
         * the annotation's own num_element_value_pairs count were already
         * committed assuming a value WOULD follow, desyncing the rest of
         * the annotation's binary layout - every later reflective access
         * to ANY annotation on the same construct then failed with
         * java.lang.annotation.AnnotationFormatError: "Unexpected end of
         * annotations" (the parser reading past the real end of the
         * attribute once one element's encoding came up short). Per JVMS
         * 4.7.16.1, a 'c'-tagged element_value's class_info_index points
         * to a CONSTANT_Utf8 holding the literal's own type descriptor
         * (e.g. "Ljava/lang/IllegalStateException;", or a primitive/array
         * descriptor for a class literal on those) - NOT a CONSTANT_Class
         * entry (unlike an ordinary ".class" literal used in code, which
         * does use CONSTANT_Class via ldc). */
        slist_t *lit_children = value->data.node.children;
        ast_node_t *type_node = lit_children ? (ast_node_t *)lit_children->data : NULL;
        if (!type_node) {
            return 0;
        }
        *(*p)++ = 'c';  /* tag for class */
        /* Force resolution if some earlier pass hasn't already: unlike a
         * ".class" literal used in ordinary code, nothing visits an
         * annotation's own class-literal value to resolve its type node
         * (annotation values are validated by dedicated, separate logic
         * in semantic.c, not the general expression-visiting passes), so
         * type_node->sem_type is reliably still NULL here - without
         * forcing it, ast_type_to_descriptor() fell back to the type
         * node's bare, unqualified AST source name (e.g.
         * "IllegalStateException" instead of
         * "Ljava/lang/IllegalStateException;"), which the JVM's own
         * reflection later failed to resolve as a real class at all
         * (java.lang.TypeNotPresentException). */
        if ((!type_node->sem_type) && g_classwriter_sem) {
            semantic_resolve_type(g_classwriter_sem, type_node);
        }
        char *desc = ast_type_to_descriptor(type_node);
        uint16_t desc_idx = cp_add_utf8(cp, desc ? desc : "Ljava/lang/Object;");
        free(desc);
        write_be_u2(p, desc_idx);
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
    if (!annot || annot->type != AST_ANNOTATION) {
        return 0;
    }
    
    uint8_t *start = *p;

    /* type_index - descriptor for annotation type */
    const char *qualified_name = resolve_annotation_qualified_name(annot->data.node.name);
    char *type_desc = NULL;

    /* qualified_name's dots are ambiguous: most are package separators
     * ('/'), but if the annotation type is NESTED inside another class
     * (e.g. "org.junit.runners.Parameterized.Parameters"), the dot at
     * that boundary is really a '$' in the type's real binary name.
     * Blindly slashing every dot writes a descriptor
     * ("Lorg/junit/runners/Parameterized/Parameters;") naming a class
     * that doesn't exist - the JVM's own annotation parser tolerates an
     * unresolvable element type by silently EXCLUDING the whole
     * annotation from getAnnotations()/getAnnotation(), not by erroring,
     * so this was invisible at both compile time and class-load time.
     * Confirmed against gumdrop's own DecoderTest/EncoderTest: their
     * data() method's "@Parameters(name = "...")" wrote a syntactically
     * well-formed but wrongly-named annotation, so JUnit's Parameterized
     * runner still found no @Parameters method. A classfile actually on
     * the classpath carries its own authoritative binary name (with '$'
     * already in the right place); prefer that over guessing whenever
     * the type can be loaded. */
    if (g_classwriter_sem) {
        struct classfile *cf = semantic_load_annotation_classfile(g_classwriter_sem, qualified_name);
        if (cf && cf->this_class_name) {
            type_desc = calloc(strlen(cf->this_class_name) + 4, 1);
            sprintf(type_desc, "L%s;", cf->this_class_name);
        }
    }
    if (!type_desc) {
        /* Best effort for a source-defined type not yet on the classpath,
         * or one that couldn't be resolved above. */
        type_desc = calloc(strlen(qualified_name) + 4, 1);
        sprintf(type_desc, "L%s;", qualified_name);
        for (char *c = type_desc; *c; c++) {
            if (*c == '.') {
                *c = '/';
            }
        }
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
                write_annotation_value(p, cp, value, qualified_name, pair->data.node.name);
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
    
    if (count == 0) {
        return 0;
    }
    
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
    if (!type_node) {
        return 0;
    }
    
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
    if (!field_decl) {
        return NULL;
    }
    
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
    if (!type_node) {
        return 0;
    }
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_type_annotations(type_node, target);
    
    if (count == 0) {
        return 0;
    }
    
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
    if (!method_decl) {
        return NULL;
    }
    
    /* Return type is stored in data.node.extra for method decls */
    return (ast_node_t *)method_decl->data.node.extra;
}

/**
 * Get parameter type nodes from a method declaration.
 * Populates the types array and returns the count.
 */
static int get_method_parameter_type_nodes(ast_node_t *method_decl, ast_node_t **types, int max_params)
{
    if (!method_decl) {
        return 0;
    }
    
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
    if (!method_ast) {
        return 0;
    }
    
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
    if (!method_ast) {
        return 0;
    }
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_method_type_annotations(method_ast, target);
    
    if (count == 0) {
        return 0;
    }
    
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
    if (!local_var_table) {
        return 0;
    }
    
    retention_policy_t target = runtime_visible ? RETENTION_RUNTIME : RETENTION_CLASS;
    int count = count_local_var_type_annotations(local_var_table, target);
    
    if (count == 0) {
        return 0;
    }
    
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
    if (!method_ast) {
        return 0;
    }
    
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
    if (!method_ast) {
        return 0;
    }
    
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
    if (total_annots == 0) {
        return 0;
    }
    
    int num_params = count_method_parameters(method_ast);
    if (num_params == 0) {
        return 0;
    }
    
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
    if (!method_ast) {
        return;
    }
    
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
    if (!annot || annot->type != AST_ANNOTATION) {
        return;
    }

    /* Annotation type descriptor. Must match write_annotation()'s resolved
     * name exactly - the constant pool is serialized before write_annotation()
     * runs, so any string it needs (like the fully-qualified descriptor) has
     * to already exist by the time this pre-add pass is done. Also used
     * below to resolve each element's own declared return type. See
     * write_annotation()'s own matching comment for why this can't just be
     * a blind dot-to-slash conversion of qualified_name: a NESTED
     * annotation type's real binary name has '$' at the nesting boundary,
     * and getting this pre-add pass out of sync with write_annotation()'s
     * own resolution - as it was before both were fixed together - adds a
     * DIFFERENT Utf8 entry than the one actually referenced, one index
     * past the end of the already-serialized constant pool
     * ("IllegalArgumentException: Constant pool index out of bounds" at
     * reflection time, even though the class loads and javap silently
     * shows no annotation on the member at all). */
    const char *qualified_name = resolve_annotation_qualified_name(annot->data.node.name);
    if (annot->data.node.name) {
        char *type_desc = NULL;
        if (g_classwriter_sem) {
            struct classfile *cf = semantic_load_annotation_classfile(g_classwriter_sem, qualified_name);
            if (cf && cf->this_class_name) {
                type_desc = calloc(strlen(cf->this_class_name) + 4, 1);
                sprintf(type_desc, "L%s;", cf->this_class_name);
            }
        }
        if (!type_desc) {
            type_desc = calloc(strlen(qualified_name) + 4, 1);
            sprintf(type_desc, "L%s;", qualified_name);
            for (char *c = type_desc; *c; c++) {
                if (*c == '.') {
                    *c = '/';
                }
            }
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
                        /* Must add the SAME constant pool entry (by type)
                         * that write_annotation_value() will look up when
                         * it actually writes this element's value - the
                         * constant pool is serialized once, before that
                         * happens, so an entry only cp_add_long()'d for
                         * the first time during the real write (e.g. for
                         * a long-returning element like
                         * "@Test(timeout = 10000)") would be added too
                         * late to appear in the serialized class file at
                         * all, corrupting the index the attribute
                         * references ("Constant pool index out of
                         * bounds" at reflection time). */
                        char ret_tag = 'I';
                        if (g_classwriter_sem && qualified_name && pair->data.node.name) {
                            char *desc = semantic_resolve_annotation_element_descriptor(
                                g_classwriter_sem, qualified_name, pair->data.node.name);
                            if (desc) {
                                const char *rparen = strchr(desc, ')');
                                if (rparen && rparen[1]) {
                                    ret_tag = rparen[1];
                                }
                                free(desc);
                            }
                        }
                        long long ival = value->data.leaf.value.int_val;
                        switch (ret_tag) {
                            case 'J': cp_add_long(cp, (int64_t)ival); break;
                            case 'F': cp_add_float(cp, (float)ival); break;
                            case 'D': cp_add_double(cp, (double)ival); break;
                            default:  cp_add_integer(cp, (int32_t)ival); break;
                        }
                    } else if (tok == TOK_TRUE || tok == TOK_FALSE) {
                        cp_add_integer(cp, tok == TOK_TRUE ? 1 : 0);
                    }
                } else if (value && value->type == AST_IDENTIFIER && value->data.leaf.name) {
                    cp_add_utf8(cp, value->data.leaf.name);
                } else if (value && value->type == AST_FIELD_ACCESS && value->data.node.name &&
                           value->data.node.children) {
                    /* Qualified enum constant, e.g. RetentionPolicy.RUNTIME -
                     * see write_annotation_value()'s matching 'e' tag case. */
                    ast_node_t *receiver = (ast_node_t *)value->data.node.children->data;
                    const char *enum_type_name = (receiver && receiver->type == AST_IDENTIFIER) ?
                        receiver->data.leaf.name : NULL;
                    if (enum_type_name) {
                        const char *qualified = resolve_annotation_qualified_name(enum_type_name);
                        char *type_desc = calloc(strlen(qualified) + 4, 1);
                        sprintf(type_desc, "L%s;", qualified);
                        for (char *c = type_desc; *c; c++) {
                            if (*c == '.') {
                                *c = '/';
                            }
                        }
                        cp_add_utf8(cp, type_desc);
                        free(type_desc);
                        cp_add_utf8(cp, value->data.node.name);
                    }
                } else if (value && value->type == AST_CLASS_LITERAL) {
                    /* Class-valued element - see write_annotation_value()'s
                     * matching 'c' tag case for the full explanation. Must
                     * pre-add the SAME Utf8 descriptor entry the real write
                     * pass will look up, for the same reason as the
                     * long/float/double case above: the constant pool is
                     * serialized before the real write pass runs, so an
                     * entry first added there could end up referencing an
                     * index that was never actually written to the class
                     * file at all. */
                    slist_t *lit_children = value->data.node.children;
                    ast_node_t *type_node = lit_children ? (ast_node_t *)lit_children->data : NULL;
                    if (type_node) {
                        /* Force resolution if needed - see the matching
                         * comment in write_annotation_value() for why. */
                        if ((!type_node->sem_type) && g_classwriter_sem) {
                            semantic_resolve_type(g_classwriter_sem, type_node);
                        }
                        char *desc = ast_type_to_descriptor(type_node);
                        cp_add_utf8(cp, desc ? desc : "Ljava/lang/Object;");
                        free(desc);
                    }
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
    if (!value) {
        return;
    }
    
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
    if (!default_value) {
        return 0;
    }
    
    uint8_t *start = *p;
    
    /* Attribute name */
    uint16_t attr_name_idx = cp_add_utf8(cp, "AnnotationDefault");
    write_be_u2(p, attr_name_idx);
    
    /* Reserve space for attribute_length */
    uint8_t *len_pos = *p;
    *p += 4;
    
    /* Write the default element_value. No annotation/element name to
     * resolve a declared return type against here (this writes the
     * *declaration* of the element's own default within the annotation
     * interface itself, not a usage site) - out of scope for the
     * int-literal-widening fix above. */
    write_annotation_value(p, cp, default_value, NULL, NULL);
    
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

/*
 * Choose the class file major version.
 *
 * cg->target_version is 0 when no -target was given: the version is then the
 * lowest one the class needs, never below Java 8 (52). When -target was given
 * it is a ceiling: a class that needs more is an error, never silently raised.
 *
 * Nestmates (Java 11+) are what let nested classes reach each other's private
 * members, as genesis emits no synthetic accessors. The NestHost/NestMembers
 * attributes are therefore required in automatic mode, and simply left out
 * (they are meaningless to the JVM) when the explicit target predates them.
 *
 * Returns the major version, or 0 after reporting an error.
 */
static int choose_class_version(class_gen_t *cg, bool *emit_nest)
{
    int required = 0;
    const char *why = NULL;

    if (cg->has_default_methods) {
        required = 52;
        why = "default or static interface methods";
    }
    if (cg->uses_invokedynamic) {
        required = 52;
        why = "lambdas or method references";
    }
    if (cg->is_record) {
        required = 60;
        why = "records";
    }
    if (cg->permitted_subclasses) {
        required = 61;
        why = "sealed classes";
    }

    bool nestmates = cg->nest_host || cg->nest_members;
    int target = cg->target_version;

    if (target > 0) {
        if (required > target) {
            fprintf(stderr, "error: %s: %s require class file version %d (Java %d), "
                    "but the target is class file version %d (Java %d)\n",
                    cg->internal_name ? cg->internal_name : "<unknown>", why,
                    required, required - 44, target, target - 44);
            return 0;
        }
        *emit_nest = nestmates && target >= 55;
        return target;
    }

    if (nestmates && required < 55) {
        required = 55;
    }
    *emit_nest = nestmates;
    return required < 52 ? 52 : required;
}

uint8_t *write_class_bytes(class_gen_t *cg, size_t *size)
{
    if (!cg || !size) {
        fprintf(stderr, "write_class_bytes: invalid input (cg=%p, size=%p)\n", (void*)cg, (void*)size);
        return NULL;
    }

    /* Make this file's semantic_t (imports, classpath) available to
     * get_annotation_retention()/write_annotation() for the duration of
     * this call, so they can resolve bare annotation names like "Test". */
    g_classwriter_sem = cg->sem;

    /* By the time code generation runs, semantic analysis has already
     * finished for every file in this compilation, so sem->current_class
     * is left over from whatever class pass2 analyzed last - not
     * necessarily this one. Re-point it at the class actually being
     * written here, so semantic_resolve_annotation_retention() can search
     * ITS OWN nested-type members (and its enclosing/superclass chain)
     * for an annotation type declared in the SAME compilation batch,
     * which has no classfile on disk yet to read a @Retention
     * meta-annotation from. Needed for a nested annotation type used on
     * a member of its own enclosing class (e.g. "@Marker" inside the
     * same class that declares "@interface Marker { ... }") - GitHub
     * issue #1. */
    if (cg->sem) {
        cg->sem->current_class = cg->class_sym;
    }

    bool emit_nest = false;
    int target_major = choose_class_version(cg, &emit_nest);
    if (target_major == 0) {
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

    /* Pre-add ConstantValue attribute constant pool entries for eligible
     * fields, caching each field's own resolved index on fg itself - the
     * constant pool is serialized (written to the output buffer) BEFORE
     * the fields section below, so any entry a field's ConstantValue
     * needs must already exist by then, AND (unlike most of this file's
     * other pre-added attributes) can't just be recomputed again in the
     * field-writing loop: cp_add_string() does not deduplicate the way
     * cp_add_integer()/cp_add_long()/etc. do, so a second call for the
     * same String constant would add a NEW entry after the pool was
     * already serialized, and the field would reference an index that
     * was never actually written out. */
    for (slist_t *node = cg->fields; node; node = node->next) {
        field_gen_t *fg = (field_gen_t *)node->data;
        fg->const_value_cp_index = field_gen_constant_value_index(cg->cp, fg);
        if (fg->const_value_cp_index != 0) {
            cp_add_utf8(cg->cp, "ConstantValue");
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
    uint16_t nm_attr_name = (emit_nest && cg->nest_members) ? cp_add_utf8(cg->cp, "NestMembers") : 0;
    uint16_t nh_attr_name = (emit_nest && cg->nest_host) ? cp_add_utf8(cg->cp, "NestHost") : 0;
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
                    /* entry->utf8_len (not strlen(entry->data.utf8)) is the
                     * entry's true raw byte length - data.utf8 may contain
                     * an embedded NUL byte of its own (JLS 3.10.6 octal
                     * escape, e.g. a string literal "\0alice\0s3cret") that
                     * strlen() would stop at short. Per JVMS 4.4.7, a
                     * classfile Utf8 entry also can't contain a raw 0x00
                     * byte at all - it must be "modified UTF-8" encoded as
                     * the two-byte sequence 0xC0 0x80 instead - so both the
                     * written length and bytes go through the
                     * modified-UTF8 helpers rather than a plain memcpy. */
                    size_t raw_len = entry->utf8_len;
                    uint16_t enc_len = (uint16_t)cp_utf8_modified_length(entry->data.utf8, raw_len);
                    write_be_u2(&p, enc_len);
                    cp_utf8_modified_write(&p, entry->data.utf8, raw_len);
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
        if (field_rt_annots > 0) {
            field_attr_count++;
        }
        
        /* Count field type annotations with RUNTIME retention */
        ast_node_t *field_type_node = get_field_type_node(fg->ast);
        int field_type_annots = 0;
        if (field_type_node) {
            field_type_annots = count_type_annotations(field_type_node, RETENTION_RUNTIME);
        }
        if (field_type_annots > 0) {
            field_attr_count++;
        }
        
        /* Count Signature attribute */
        if (fg->signature) {
            field_attr_count++;
        }

        /* Count ConstantValue attribute - use the index already resolved
         * and cached in the pre-add pass above, NOT a fresh call to
         * field_gen_constant_value_index() (see that pre-add pass's own
         * comment for why: cp_add_string() doesn't dedupe). */
        uint16_t const_value_index = fg->const_value_cp_index;
        if (const_value_index != 0) {
            field_attr_count++;
        }

        write_be_u2(&p, field_attr_count);

        /* Signature attribute (for field) */
        if (fg->signature) {
            uint16_t sig_attr_name = cp_add_utf8(cg->cp, "Signature");
            uint16_t sig_index = cp_add_utf8(cg->cp, fg->signature);
            write_be_u2(&p, sig_attr_name);
            write_be_u4(&p, 2);  /* attribute_length is always 2 */
            write_be_u2(&p, sig_index);
        }

        /* ConstantValue attribute (for field) */
        if (const_value_index != 0) {
            uint16_t cv_attr_name = cp_add_utf8(cg->cp, "ConstantValue");
            write_be_u2(&p, cv_attr_name);
            write_be_u4(&p, 2);  /* attribute_length is always 2 */
            write_be_u2(&p, const_value_index);
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
            for (slist_t *t = mi->throws; t; t = t->next) {
                throws_count++;
            }
            
            /* Count attributes: annotations + signature + parameter annotations + annotation default + type annotations + exceptions */
            int abstract_attr_count = 0;
            if (throws_count > 0) {
                abstract_attr_count++;
            }  /* Exceptions attribute */
            if (method_rt_annots > 0) {
                abstract_attr_count++;
            }
            if (param_rt_annots > 0) {
                abstract_attr_count++;
            }
            if (method_type_annots > 0) {
                abstract_attr_count++;
            }
            if (mi->signature) {
                abstract_attr_count++;
            }
            if (has_annotation_default) {
                abstract_attr_count++;
            }
            
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
        for (slist_t *t = mi->throws; t; t = t->next) {
            concrete_throws_count++;
        }
        
        /* Count method attributes: Code + optional Signature + optional annotations + parameter annotations + type annotations + exceptions */
        int method_attr_count = 1;  /* Code is always present for concrete methods */
        if (concrete_throws_count > 0) {
            method_attr_count++;
        }  /* Exceptions attribute */
        if (mi->signature) {
            method_attr_count++;
        }
        if (method_rt_annots > 0) {
            method_attr_count++;
        }
        if (param_rt_annots > 0) {
            method_attr_count++;
        }
        if (method_type_annots > 0) {
            method_attr_count++;
        }
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
        if (smt_data && smt_len > 0) {
            code_attr_count_val++;
        }
        if (lnt_count > 0) {
            code_attr_count_val++;
        }
        if (lvt_count > 0) {
            code_attr_count_val++;
        }
        if (lvt_annot_data && lvt_annot_len > 0) {
            code_attr_count_val++;
        }
        
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
    if (cg->inner_class_entries) {
        class_attr_count++;
    }
    if (cg->signature) {
        class_attr_count++;
    }
    if (cg->bootstrap_methods && cg->bootstrap_methods->count > 0) {
        class_attr_count++;
    }
    if (cg->permitted_subclasses) {
        class_attr_count++;
    }
    if (emit_nest && cg->nest_members) {
        class_attr_count++;
    }  /* NestMembers (Java 11+) */
    if (emit_nest && cg->nest_host) {
        class_attr_count++;
    }     /* NestHost (Java 11+) */
    
    /* Check for class-level annotations with RUNTIME retention */
    int runtime_annot_count = 0;
    if (cg->class_ast && cg->class_ast->annotations) {
        runtime_annot_count = count_annotations_with_retention(
            cg->class_ast->annotations, RETENTION_RUNTIME);
        if (runtime_annot_count > 0) {
            class_attr_count++;
        }
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
    if (emit_nest && cg->nest_members && nm_attr_name) {
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
    if (emit_nest && cg->nest_host && nh_attr_name) {
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
