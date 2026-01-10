/*
 * classfile.h
 * Copyright (C) 2016 Chris Burdess <dog@gnu.org>
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

#ifndef CLASSFILE_H
#define CLASSFILE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*
 * Java Class File Format (JVMS Chapter 4)
 *
 * This module provides structures and functions for reading Java .class files
 * to extract type information needed for compilation.
 */

/* Class file magic number */
#define CLASS_MAGIC 0xCAFEBABE

/* Constant pool tags */
typedef enum cp_tag
{
    CONSTANT_Utf8               = 1,
    CONSTANT_Integer            = 3,
    CONSTANT_Float              = 4,
    CONSTANT_Long               = 5,
    CONSTANT_Double             = 6,
    CONSTANT_Class              = 7,
    CONSTANT_String             = 8,
    CONSTANT_Fieldref           = 9,
    CONSTANT_Methodref          = 10,
    CONSTANT_InterfaceMethodref = 11,
    CONSTANT_NameAndType        = 12,
    CONSTANT_MethodHandle       = 15,
    CONSTANT_MethodType         = 16,
    CONSTANT_Dynamic            = 17,
    CONSTANT_InvokeDynamic      = 18,
    CONSTANT_Module             = 19,
    CONSTANT_Package            = 20
} cp_tag_t;

/* Access flags for classes, fields, and methods */
typedef enum class_access_flags
{
    ACC_PUBLIC       = 0x0001,
    ACC_PRIVATE      = 0x0002,
    ACC_PROTECTED    = 0x0004,
    ACC_STATIC       = 0x0008,
    ACC_FINAL        = 0x0010,
    ACC_SUPER        = 0x0020,      /* Class: treat superclass methods specially */
    ACC_SYNCHRONIZED = 0x0020,      /* Method: synchronized */
    ACC_OPEN         = 0x0020,      /* Module: open */
    ACC_TRANSITIVE   = 0x0020,      /* Module requires: transitive */
    ACC_VOLATILE     = 0x0040,      /* Field: volatile */
    ACC_BRIDGE       = 0x0040,      /* Method: bridge */
    ACC_STATIC_PHASE = 0x0040,      /* Module requires: static */
    ACC_TRANSIENT    = 0x0080,      /* Field: transient */
    ACC_VARARGS      = 0x0080,      /* Method: varargs */
    ACC_NATIVE       = 0x0100,      /* Method: native */
    ACC_INTERFACE    = 0x0200,      /* Class: interface */
    ACC_ABSTRACT     = 0x0400,
    ACC_STRICT       = 0x0800,      /* Method: strictfp */
    ACC_SYNTHETIC    = 0x1000,      /* Not in source code */
    ACC_ANNOTATION   = 0x2000,      /* Class: annotation type */
    ACC_ENUM         = 0x4000,
    ACC_RECORD       = 0x10000,     /* Class: record (Java 16+) - in class file, uses 0x0010 which is ACC_FINAL */
    ACC_MODULE       = 0x8000       /* Class: module (not a class) */
} class_access_flags_t;

/*
 * Constant pool entry
 */
typedef struct cp_info
{
    cp_tag_t tag;
    union {
        /* CONSTANT_Utf8 */
        struct {
            uint16_t length;
            char *bytes;            /* NOT null-terminated in class file */
        } utf8;

        /* CONSTANT_Integer */
        int32_t integer_value;

        /* CONSTANT_Float */
        float float_value;

        /* CONSTANT_Long */
        int64_t long_value;

        /* CONSTANT_Double */
        double double_value;

        /* CONSTANT_Class, CONSTANT_String, CONSTANT_MethodType,
           CONSTANT_Module, CONSTANT_Package */
        struct {
            uint16_t name_index;
        } class_info;

        /* CONSTANT_Fieldref, CONSTANT_Methodref, CONSTANT_InterfaceMethodref */
        struct {
            uint16_t class_index;
            uint16_t name_and_type_index;
        } ref;

        /* CONSTANT_NameAndType */
        struct {
            uint16_t name_index;
            uint16_t descriptor_index;
        } name_and_type;

        /* CONSTANT_MethodHandle */
        struct {
            uint8_t reference_kind;
            uint16_t reference_index;
        } method_handle;

        /* CONSTANT_Dynamic, CONSTANT_InvokeDynamic */
        struct {
            uint16_t bootstrap_method_attr_index;
            uint16_t name_and_type_index;
        } dynamic;
    } info;
} cp_info_t;

/*
 * Attribute info (we only parse selected attributes)
 */
typedef struct attribute_info
{
    uint16_t attribute_name_index;
    uint32_t attribute_length;
    uint8_t *info;                  /* Raw attribute data */
} attribute_info_t;

/*
 * Field info
 */
typedef struct field_info
{
    uint16_t access_flags;
    uint16_t name_index;
    uint16_t descriptor_index;
    uint16_t attributes_count;
    attribute_info_t *attributes;

    /* Resolved strings (for convenience) */
    char *name;
    char *descriptor;
} field_info_t;

/*
 * Method info
 */
typedef struct method_info
{
    uint16_t access_flags;
    uint16_t name_index;
    uint16_t descriptor_index;
    uint16_t attributes_count;
    attribute_info_t *attributes;

    /* Resolved strings (for convenience) */
    char *name;
    char *descriptor;
} method_info_t;

/*
 * Parsed class file
 */
typedef struct classfile
{
    uint32_t magic;
    uint16_t minor_version;
    uint16_t major_version;

    uint16_t constant_pool_count;
    cp_info_t *constant_pool;       /* Index 0 unused; indices 1..count-1 */

    uint16_t access_flags;
    uint16_t this_class;
    uint16_t super_class;

    uint16_t interfaces_count;
    uint16_t *interfaces;           /* Array of constant pool indices */

    uint16_t fields_count;
    field_info_t *fields;

    uint16_t methods_count;
    method_info_t *methods;

    uint16_t attributes_count;
    attribute_info_t *attributes;

    /* Resolved strings (for convenience) */
    char *this_class_name;          /* Fully qualified, with '/' separators */
    char *super_class_name;
    char **interface_names;
} classfile_t;

/*
 * Method/field descriptor parsing
 */
typedef enum descriptor_type
{
    DESC_VOID,
    DESC_BYTE,
    DESC_CHAR,
    DESC_DOUBLE,
    DESC_FLOAT,
    DESC_INT,
    DESC_LONG,
    DESC_SHORT,
    DESC_BOOLEAN,
    DESC_OBJECT,
    DESC_ARRAY
} descriptor_type_t;

typedef struct type_descriptor
{
    descriptor_type_t type;
    int array_dimensions;           /* Number of array dimensions */
    char *class_name;               /* For DESC_OBJECT: fully qualified name */
} type_descriptor_t;

typedef struct method_descriptor
{
    int param_count;
    type_descriptor_t *params;
    type_descriptor_t return_type;
} method_descriptor_t;

/*
 * Class file reading API
 */

/* Read a class file from a byte buffer */
classfile_t *classfile_read(const uint8_t *data, size_t length);

/* Read a class file from a file path */
classfile_t *classfile_read_file(const char *path);

/* Free a classfile structure */
void classfile_free(classfile_t *cf);

/* Get a UTF-8 string from the constant pool (returns allocated copy) */
char *classfile_get_utf8(classfile_t *cf, uint16_t index);

/* Get a class name from the constant pool (returns allocated copy) */
char *classfile_get_class_name(classfile_t *cf, uint16_t index);

/*
 * Descriptor parsing
 */

/* Parse a field descriptor (e.g., "Ljava/lang/String;", "[I") */
type_descriptor_t *descriptor_parse_field(const char *desc);

/* Parse a method descriptor (e.g., "(ILjava/lang/String;)V") */
method_descriptor_t *descriptor_parse_method(const char *desc);

/* Free a type descriptor */
void type_descriptor_free(type_descriptor_t *td);

/* Free a method descriptor */
void method_descriptor_free(method_descriptor_t *md);

/* Convert a type descriptor to a Java-style type name (allocated) */
char *type_descriptor_to_string(type_descriptor_t *td);

/*
 * Generic signature parsing (Signature attribute)
 * 
 * The Signature attribute stores generic type information that is
 * erased from the descriptor. Format defined in JVMS 4.7.9.1.
 */

/* Generic type kinds */
typedef enum generic_type_kind
{
    GEN_PRIMITIVE,      /* Primitive type (B, C, D, F, I, J, S, Z, V) */
    GEN_CLASS,          /* Class/interface type (L...;) */
    GEN_TYPEVAR,        /* Type variable (T...;) */
    GEN_ARRAY,          /* Array type ([...) */
    GEN_WILDCARD        /* Wildcard (*, +..., -...) */
} generic_type_kind_t;

/* Wildcard bound kinds */
typedef enum wildcard_bound_kind
{
    WILDCARD_NONE,      /* ? (unbounded) */
    WILDCARD_EXTENDS,   /* ? extends X (+) */
    WILDCARD_SUPER      /* ? super X (-) */
} wildcard_bound_kind_t;

/* Forward declaration */
struct generic_type;

/* Type argument (for parameterized types) */
typedef struct type_argument
{
    wildcard_bound_kind_t bound_kind;   /* For wildcards */
    struct generic_type *type;          /* The actual type (NULL for unbounded ?) */
    struct type_argument *next;         /* Linked list */
} type_argument_t;

/* Type parameter (for class/method declarations) */
typedef struct type_parameter
{
    char *name;                         /* Type parameter name (e.g., "T", "E") */
    struct generic_type *class_bound;   /* Class bound (may be NULL) */
    struct generic_type *interface_bounds;  /* Interface bounds (linked list) */
    struct type_parameter *next;        /* Linked list */
} type_parameter_t;

/* Generic type */
typedef struct generic_type
{
    generic_type_kind_t kind;
    union {
        char primitive;                 /* Primitive type char */
        struct {
            char *name;                 /* Binary class name */
            type_argument_t *type_args; /* Type arguments (NULL if raw) */
        } class_type;
        char *type_var_name;            /* Type variable name */
        struct generic_type *array_element;  /* Array element type */
        struct {
            wildcard_bound_kind_t bound_kind;
            struct generic_type *bound; /* NULL for unbounded */
        } wildcard;
    } data;
    struct generic_type *next;          /* For linked lists (interface bounds, etc.) */
} generic_type_t;

/* Parsed method signature */
typedef struct method_signature
{
    type_parameter_t *type_params;      /* Method type parameters (<T, U, ...>) */
    generic_type_t *params;             /* Parameter types (linked list) */
    generic_type_t *return_type;        /* Return type */
    generic_type_t *throws;             /* Throws types (linked list) */
} method_signature_t;

/* Parsed class signature */
typedef struct class_signature
{
    type_parameter_t *type_params;      /* Class type parameters */
    generic_type_t *superclass;         /* Superclass type */
    generic_type_t *interfaces;         /* Interface types (linked list) */
} class_signature_t;

/*
 * Signature attribute API
 */

/* Find an attribute by name in the attribute list */
char *classfile_get_attribute_signature(classfile_t *cf, attribute_info_t *attrs, 
                                        uint16_t attr_count);

/* Parse a generic type from a signature string */
generic_type_t *signature_parse_type(const char **sig);

/* Parse a method signature */
method_signature_t *signature_parse_method(const char *sig);

/* Parse a class signature */
class_signature_t *signature_parse_class(const char *sig);

/* Parse a field/type signature */
generic_type_t *signature_parse_field(const char *sig);

/* Free a generic type */
void generic_type_free(generic_type_t *type);

/* Free a method signature */
void method_signature_free(method_signature_t *sig);

/* Free a class signature */
void class_signature_free(class_signature_t *sig);

/* Convert a generic type to string representation (for debugging) */
char *generic_type_to_string(generic_type_t *type);

/*
 * Utility functions
 */

/* Convert class name from internal form (a/b/C) to binary form (a.b.C) */
char *classname_to_binary(const char *internal);

/* Note: classname_to_internal removed - use class_to_internal_name from codegen.h instead */

/* Get Java major version name for a class file version */
const char *classfile_version_name(uint16_t major_version);

/* Convert version string (e.g., "8", "17", "1.8") to major class version */
int classfile_version_from_string(const char *version);

/* Get Java version number from major class version */
int classfile_java_version(int major_version);

#endif /* CLASSFILE_H */

