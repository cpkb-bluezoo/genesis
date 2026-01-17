/*
 * classfile.c
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "classfile.h"
#include "util.h"

/*
 * Binary reading helpers (big-endian)
 */

typedef struct reader
{
    const uint8_t *data;
    size_t length;
    size_t pos;
} reader_t;

static bool reader_init(reader_t *r, const uint8_t *data, size_t length)
{
    r->data = data;
    r->length = length;
    r->pos = 0;
    return true;
}

static bool reader_has(reader_t *r, size_t count)
{
    return (r->pos + count) <= r->length;
}

static uint8_t read_u1(reader_t *r)
{
    if (!reader_has(r, 1)) {
        return 0;
    }
    return r->data[r->pos++];
}

static uint16_t read_u2(reader_t *r)
{
    if (!reader_has(r, 2)) {
        return 0;
    }
    uint16_t val = ((uint16_t)r->data[r->pos] << 8) |
                   ((uint16_t)r->data[r->pos + 1]);
    r->pos += 2;
    return val;
}

static uint32_t read_u4(reader_t *r)
{
    if (!reader_has(r, 4)) {
        return 0;
    }
    uint32_t val = ((uint32_t)r->data[r->pos] << 24) |
                   ((uint32_t)r->data[r->pos + 1] << 16) |
                   ((uint32_t)r->data[r->pos + 2] << 8) |
                   ((uint32_t)r->data[r->pos + 3]);
    r->pos += 4;
    return val;
}

static int64_t read_s8(reader_t *r)
{
    if (!reader_has(r, 8)) {
        return 0;
    }
    int64_t val = ((int64_t)r->data[r->pos] << 56) |
                  ((int64_t)r->data[r->pos + 1] << 48) |
                  ((int64_t)r->data[r->pos + 2] << 40) |
                  ((int64_t)r->data[r->pos + 3] << 32) |
                  ((int64_t)r->data[r->pos + 4] << 24) |
                  ((int64_t)r->data[r->pos + 5] << 16) |
                  ((int64_t)r->data[r->pos + 6] << 8) |
                  ((int64_t)r->data[r->pos + 7]);
    r->pos += 8;
    return val;
}

static bool read_bytes(reader_t *r, uint8_t *dest, size_t count)
{
    if (!reader_has(r, count)) {
        return false;
    }
    memcpy(dest, r->data + r->pos, count);
    r->pos += count;
    return true;
}

/*
 * Constant pool parsing
 */

static bool read_constant_pool(reader_t *r, classfile_t *cf)
{
    cf->constant_pool_count = read_u2(r);
    if (cf->constant_pool_count == 0) {
        return false;
    }

    /* Allocate constant pool (index 0 is unused) */
    cf->constant_pool = calloc(cf->constant_pool_count, sizeof(cp_info_t));
    if (!cf->constant_pool) {
        return false;
    }

    for (uint16_t i = 1; i < cf->constant_pool_count; i++) {
        cp_info_t *cp = &cf->constant_pool[i];
        cp->tag = (cp_tag_t)read_u1(r);

        switch (cp->tag) {
            case CONSTANT_Utf8: {
                uint16_t length = read_u2(r);
                cp->info.utf8.length = length;
                cp->info.utf8.bytes = malloc(length + 1);
                if (!cp->info.utf8.bytes) {
                    return false;
                }
                if (!read_bytes(r, (uint8_t *)cp->info.utf8.bytes, length)) {
                    return false;
                }
                cp->info.utf8.bytes[length] = '\0';
                break;
            }

            case CONSTANT_Integer: {
                uint32_t bytes = read_u4(r);
                cp->info.integer_value = (int32_t)bytes;
                break;
            }

            case CONSTANT_Float: {
                uint32_t bytes = read_u4(r);
                memcpy(&cp->info.float_value, &bytes, sizeof(float));
                break;
            }

            case CONSTANT_Long: {
                cp->info.long_value = read_s8(r);
                /* Long/Double take two constant pool entries */
                i++;
                break;
            }

            case CONSTANT_Double: {
                int64_t bits = read_s8(r);
                memcpy(&cp->info.double_value, &bits, sizeof(double));
                /* Long/Double take two constant pool entries */
                i++;
                break;
            }

            case CONSTANT_Class:
            case CONSTANT_String:
            case CONSTANT_MethodType:
            case CONSTANT_Module:
            case CONSTANT_Package:
                cp->info.class_info.name_index = read_u2(r);
                break;

            case CONSTANT_Fieldref:
            case CONSTANT_Methodref:
            case CONSTANT_InterfaceMethodref:
                cp->info.ref.class_index = read_u2(r);
                cp->info.ref.name_and_type_index = read_u2(r);
                break;

            case CONSTANT_NameAndType:
                cp->info.name_and_type.name_index = read_u2(r);
                cp->info.name_and_type.descriptor_index = read_u2(r);
                break;

            case CONSTANT_MethodHandle:
                cp->info.method_handle.reference_kind = read_u1(r);
                cp->info.method_handle.reference_index = read_u2(r);
                break;

            case CONSTANT_Dynamic:
            case CONSTANT_InvokeDynamic:
                cp->info.dynamic.bootstrap_method_attr_index = read_u2(r);
                cp->info.dynamic.name_and_type_index = read_u2(r);
                break;

            default:
                fprintf(stderr, "warning: unknown constant pool tag %d at index %d\n",
                        cp->tag, i);
                return false;
        }
    }

    return true;
}

/*
 * Attribute parsing
 */

static bool read_attributes(reader_t *r, uint16_t count, attribute_info_t **attrs)
{
    if (count == 0) {
        *attrs = NULL;
        return true;
    }

    *attrs = calloc(count, sizeof(attribute_info_t));
    if (!*attrs) {
        return false;
    }

    for (uint16_t i = 0; i < count; i++) {
        attribute_info_t *attr = &(*attrs)[i];
        attr->attribute_name_index = read_u2(r);
        attr->attribute_length = read_u4(r);

        if (attr->attribute_length > 0) {
            attr->info = malloc(attr->attribute_length);
            if (!attr->info) {
                return false;
            }
            if (!read_bytes(r, attr->info, attr->attribute_length)) {
                return false;
            }
        } else {
            attr->info = NULL;
        }
    }

    return true;
}

/*
 * Field parsing
 */

static bool read_fields(reader_t *r, classfile_t *cf)
{
    cf->fields_count = read_u2(r);
    if (cf->fields_count == 0) {
        cf->fields = NULL;
        return true;
    }

    cf->fields = calloc(cf->fields_count, sizeof(field_info_t));
    if (!cf->fields) {
        return false;
    }

    for (uint16_t i = 0; i < cf->fields_count; i++) {
        field_info_t *field = &cf->fields[i];
        field->access_flags = read_u2(r);
        field->name_index = read_u2(r);
        field->descriptor_index = read_u2(r);
        field->attributes_count = read_u2(r);

        if (!read_attributes(r, field->attributes_count, &field->attributes)) {
            return false;
        }

        /* Resolve name and descriptor */
        field->name = classfile_get_utf8(cf, field->name_index);
        field->descriptor = classfile_get_utf8(cf, field->descriptor_index);
    }

    return true;
}

/*
 * Method parsing
 */

static bool read_methods(reader_t *r, classfile_t *cf)
{
    cf->methods_count = read_u2(r);
    if (cf->methods_count == 0) {
        cf->methods = NULL;
        return true;
    }

    cf->methods = calloc(cf->methods_count, sizeof(method_info_t));
    if (!cf->methods) {
        return false;
    }

    for (uint16_t i = 0; i < cf->methods_count; i++) {
        method_info_t *method = &cf->methods[i];
        method->access_flags = read_u2(r);
        method->name_index = read_u2(r);
        method->descriptor_index = read_u2(r);
        method->attributes_count = read_u2(r);

        if (!read_attributes(r, method->attributes_count, &method->attributes)) {
            return false;
        }

        /* Resolve name and descriptor */
        method->name = classfile_get_utf8(cf, method->name_index);
        method->descriptor = classfile_get_utf8(cf, method->descriptor_index);
    }

    return true;
}

/*
 * Public API
 */

classfile_t *classfile_read(const uint8_t *data, size_t length)
{
    reader_t r;
    reader_init(&r, data, length);

    classfile_t *cf = calloc(1, sizeof(classfile_t));
    if (!cf) {
        return NULL;
    }

    /* Magic number */
    cf->magic = read_u4(&r);
    if (cf->magic != CLASS_MAGIC) {
        fprintf(stderr, "error: invalid class file magic: 0x%08X\n", cf->magic);
        classfile_free(cf);
        return NULL;
    }

    /* Version */
    cf->minor_version = read_u2(&r);
    cf->major_version = read_u2(&r);

    /* Constant pool */
    if (!read_constant_pool(&r, cf)) {
        fprintf(stderr, "error: failed to read constant pool\n");
        classfile_free(cf);
        return NULL;
    }

    /* Access flags and class info */
    cf->access_flags = read_u2(&r);
    cf->this_class = read_u2(&r);
    cf->super_class = read_u2(&r);

    /* Interfaces */
    cf->interfaces_count = read_u2(&r);
    if (cf->interfaces_count > 0) {
        cf->interfaces = calloc(cf->interfaces_count, sizeof(uint16_t));
        if (!cf->interfaces) {
            classfile_free(cf);
            return NULL;
        }
        for (uint16_t i = 0; i < cf->interfaces_count; i++) {
            cf->interfaces[i] = read_u2(&r);
        }
    }

    /* Fields */
    if (!read_fields(&r, cf)) {
        fprintf(stderr, "error: failed to read fields\n");
        classfile_free(cf);
        return NULL;
    }

    /* Methods */
    if (!read_methods(&r, cf)) {
        fprintf(stderr, "error: failed to read methods\n");
        classfile_free(cf);
        return NULL;
    }

    /* Class attributes */
    cf->attributes_count = read_u2(&r);
    if (!read_attributes(&r, cf->attributes_count, &cf->attributes)) {
        fprintf(stderr, "error: failed to read class attributes\n");
        classfile_free(cf);
        return NULL;
    }

    /* Resolve class names */
    cf->this_class_name = classfile_get_class_name(cf, cf->this_class);
    if (cf->super_class != 0) {
        cf->super_class_name = classfile_get_class_name(cf, cf->super_class);
    }

    if (cf->interfaces_count > 0) {
        cf->interface_names = calloc(cf->interfaces_count, sizeof(char *));
        if (cf->interface_names) {
            for (uint16_t i = 0; i < cf->interfaces_count; i++) {
                cf->interface_names[i] = classfile_get_class_name(cf, cf->interfaces[i]);
            }
        }
    }

    return cf;
}

classfile_t *classfile_read_file(const char *path)
{
    FILE *fp = fopen(path, "rb");
    if (!fp) {
        return NULL;
    }

    /* Get file size */
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    if (size <= 0) {
        fclose(fp);
        return NULL;
    }

    /* Read entire file */
    uint8_t *data = malloc(size);
    if (!data) {
        fclose(fp);
        return NULL;
    }

    size_t read = fread(data, 1, size, fp);
    fclose(fp);

    if (read != (size_t)size) {
        free(data);
        return NULL;
    }

    classfile_t *cf = classfile_read(data, size);
    free(data);
    return cf;
}

void classfile_free(classfile_t *cf)
{
    if (!cf) {
        return;
    }

    /* Free constant pool */
    if (cf->constant_pool) {
        for (uint16_t i = 1; i < cf->constant_pool_count; i++) {
            cp_info_t *cp = &cf->constant_pool[i];
            if (cp->tag == CONSTANT_Utf8) {
                free(cp->info.utf8.bytes);
            }
        }
        free(cf->constant_pool);
    }

    /* Free interfaces */
    free(cf->interfaces);

    /* Free fields */
    if (cf->fields) {
        for (uint16_t i = 0; i < cf->fields_count; i++) {
            field_info_t *field = &cf->fields[i];
            free(field->name);
            free(field->descriptor);
            if (field->attributes) {
                for (uint16_t j = 0; j < field->attributes_count; j++) {
                    free(field->attributes[j].info);
                }
                free(field->attributes);
            }
        }
        free(cf->fields);
    }

    /* Free methods */
    if (cf->methods) {
        for (uint16_t i = 0; i < cf->methods_count; i++) {
            method_info_t *method = &cf->methods[i];
            free(method->name);
            free(method->descriptor);
            if (method->attributes) {
                for (uint16_t j = 0; j < method->attributes_count; j++) {
                    free(method->attributes[j].info);
                }
                free(method->attributes);
            }
        }
        free(cf->methods);
    }

    /* Free class attributes */
    if (cf->attributes) {
        for (uint16_t i = 0; i < cf->attributes_count; i++) {
            free(cf->attributes[i].info);
        }
        free(cf->attributes);
    }

    /* Free resolved names */
    free(cf->this_class_name);
    free(cf->super_class_name);
    if (cf->interface_names) {
        for (uint16_t i = 0; i < cf->interfaces_count; i++) {
            free(cf->interface_names[i]);
        }
        free(cf->interface_names);
    }

    free(cf);
}

char *classfile_get_utf8(classfile_t *cf, uint16_t index)
{
    if (index == 0 || index >= cf->constant_pool_count) {
        return NULL;
    }

    cp_info_t *cp = &cf->constant_pool[index];
    if (cp->tag != CONSTANT_Utf8) {
        return NULL;
    }

    return strdup(cp->info.utf8.bytes);
}

char *classfile_get_class_name(classfile_t *cf, uint16_t index)
{
    if (index == 0 || index >= cf->constant_pool_count) {
        return NULL;
    }

    cp_info_t *cp = &cf->constant_pool[index];
    if (cp->tag != CONSTANT_Class) {
        return NULL;
    }

    return classfile_get_utf8(cf, cp->info.class_info.name_index);
}

/*
 * Descriptor parsing
 */

static const char *parse_field_descriptor(const char *desc, type_descriptor_t *td)
{
    td->array_dimensions = 0;
    td->class_name = NULL;

    /* Count array dimensions */
    while (*desc == '[') {
        td->array_dimensions++;
        desc++;
    }

    switch (*desc) {
        case 'B':
            td->type = DESC_BYTE;
            return desc + 1;
        case 'C':
            td->type = DESC_CHAR;
            return desc + 1;
        case 'D':
            td->type = DESC_DOUBLE;
            return desc + 1;
        case 'F':
            td->type = DESC_FLOAT;
            return desc + 1;
        case 'I':
            td->type = DESC_INT;
            return desc + 1;
        case 'J':
            td->type = DESC_LONG;
            return desc + 1;
        case 'S':
            td->type = DESC_SHORT;
            return desc + 1;
        case 'Z':
            td->type = DESC_BOOLEAN;
            return desc + 1;
        case 'V':
            td->type = DESC_VOID;
            return desc + 1;
        case 'L': {
            /* Object type: Lclassname; */
            td->type = DESC_OBJECT;
            desc++;
            const char *end = strchr(desc, ';');
            if (!end) {
                return NULL;
            }
            size_t len = end - desc;
            td->class_name = malloc(len + 1);
            if (td->class_name) {
                memcpy(td->class_name, desc, len);
                td->class_name[len] = '\0';
            }
            return end + 1;
        }
        default:
            return NULL;
    }
}

type_descriptor_t *descriptor_parse_field(const char *desc)
{
    type_descriptor_t *td = calloc(1, sizeof(type_descriptor_t));
    if (!td) {
        return NULL;
    }

    if (!parse_field_descriptor(desc, td)) {
        free(td);
        return NULL;
    }

    /* Don't overwrite type to DESC_ARRAY - keep element type info
     * The array_dimensions field indicates whether it's an array */
    return td;
}

method_descriptor_t *descriptor_parse_method(const char *desc)
{
    if (*desc != '(') {
        return NULL;
    }
    desc++;

    method_descriptor_t *md = calloc(1, sizeof(method_descriptor_t));
    if (!md) {
        return NULL;
    }

    /* Count parameters first */
    const char *p = desc;
    int count = 0;
    while (*p && *p != ')') {
        type_descriptor_t tmp;
        p = parse_field_descriptor(p, &tmp);
        if (!p) {
            free(md);
            return NULL;
        }
        free(tmp.class_name);
        count++;
    }

    md->param_count = count;
    if (count > 0) {
        md->params = calloc(count, sizeof(type_descriptor_t));
        if (!md->params) {
            free(md);
            return NULL;
        }

        /* Parse parameters again */
        p = desc;
        for (int i = 0; i < count; i++) {
            p = parse_field_descriptor(p, &md->params[i]);
        }
    }

    /* Skip ')' */
    if (*p != ')') {
        method_descriptor_free(md);
        return NULL;
    }
    p++;

    /* Parse return type */
    if (!parse_field_descriptor(p, &md->return_type)) {
        method_descriptor_free(md);
        return NULL;
    }

    return md;
}

void type_descriptor_free(type_descriptor_t *td)
{
    if (td) {
        free(td->class_name);
        free(td);
    }
}

void method_descriptor_free(method_descriptor_t *md)
{
    if (md) {
        if (md->params) {
            for (int i = 0; i < md->param_count; i++) {
                free(md->params[i].class_name);
            }
            free(md->params);
        }
        free(md->return_type.class_name);
        free(md);
    }
}

char *type_descriptor_to_string(type_descriptor_t *td)
{
    const char *base;
    switch (td->type) {
        case DESC_VOID:    base = "void"; break;
        case DESC_BYTE:    base = "byte"; break;
        case DESC_CHAR:    base = "char"; break;
        case DESC_DOUBLE:  base = "double"; break;
        case DESC_FLOAT:   base = "float"; break;
        case DESC_INT:     base = "int"; break;
        case DESC_LONG:    base = "long"; break;
        case DESC_SHORT:   base = "short"; break;
        case DESC_BOOLEAN: base = "boolean"; break;
        case DESC_OBJECT:
            base = td->class_name ? td->class_name : "Object";
            break;
        case DESC_ARRAY:
            base = "array";  /* This needs better handling */
            break;
        default:
            base = "?";
    }

    size_t len = strlen(base) + (td->array_dimensions * 2) + 1;
    char *result = malloc(len);
    if (!result) {
        return NULL;
    }

    strcpy(result, base);
    for (int i = 0; i < td->array_dimensions; i++) {
        strcat(result, "[]");
    }

    return result;
}

char *classname_to_binary(const char *internal)
{
    if (!internal) {
        return NULL;
    }
    char *result = strdup(internal);
    if (result) {
        for (char *p = result; *p; p++) {
            if (*p == '/') {
                *p = '.';
            }
        }
    }
    return result;
}

/* Note: classname_to_internal was removed as it duplicated class_to_internal_name in codegen.c */

const char *classfile_version_name(uint16_t major_version)
{
    switch (major_version) {
        case 45: return "Java 1.1";
        case 46: return "Java 1.2";
        case 47: return "Java 1.3";
        case 48: return "Java 1.4";
        case 49: return "Java 5";
        case 50: return "Java 6";
        case 51: return "Java 7";
        case 52: return "Java 8";
        case 53: return "Java 9";
        case 54: return "Java 10";
        case 55: return "Java 11";
        case 56: return "Java 12";
        case 57: return "Java 13";
        case 58: return "Java 14";
        case 59: return "Java 15";
        case 60: return "Java 16";
        case 61: return "Java 17";
        case 62: return "Java 18";
        case 63: return "Java 19";
        case 64: return "Java 20";
        case 65: return "Java 21";
        case 66: return "Java 22";
        case 67: return "Java 23";
        case 68: return "Java 24";
        default: return "Unknown";
    }
}

/**
 * Convert a Java version string (e.g., "8", "11", "17", "21", "1.8") to major class version.
 * Returns 0 if the version string is not recognized.
 */
int classfile_version_from_string(const char *version)
{
    if (!version) return 0;
    
    /* Handle "1.x" legacy format */
    if (strncmp(version, "1.", 2) == 0) {
        version += 2;
    }
    
    int v = atoi(version);
    if (v <= 0) return 0;
    
    /* Java 1-4 have different mapping */
    if (v <= 4) {
        return 44 + v;  /* 1->45, 2->46, 3->47, 4->48 */
    }
    /* Java 5+ has linear mapping: v -> 44 + v */
    return 44 + v;  /* 5->49, 6->50, 7->51, 8->52, ... */
}

/**
 * Get the Java version number from a major class version.
 */
int classfile_java_version(int major_version)
{
    if (major_version < 45) return 0;
    if (major_version <= 48) return major_version - 44;  /* 45->1, 46->2, 47->3, 48->4 */
    return major_version - 44;  /* 49->5, 50->6, 51->7, 52->8, ... */
}

/* ========================================================================
 * Generic Signature Parsing
 * JVMS 4.7.9.1
 * ======================================================================== */

/**
 * Find the Signature attribute and return its value.
 * Returns an allocated string or NULL if not found.
 */
char *classfile_get_attribute_signature(classfile_t *cf, attribute_info_t *attrs, 
                                        uint16_t attr_count)
{
    for (uint16_t i = 0; i < attr_count; i++) {
        char *name = classfile_get_utf8(cf, attrs[i].attribute_name_index);
        if (name && strcmp(name, "Signature") == 0) {
            free(name);
            /* Signature attribute contains just a u2 index */
            if (attrs[i].attribute_length >= 2 && attrs[i].info) {
                uint16_t sig_index = ((uint16_t)attrs[i].info[0] << 8) | attrs[i].info[1];
                return classfile_get_utf8(cf, sig_index);
            }
            return NULL;
        }
        free(name);
    }
    return NULL;
}

/* Forward declarations for signature parsing */
static generic_type_t *parse_type_signature(const char **sig);
static generic_type_t *parse_class_type_signature(const char **sig);
static type_argument_t *parse_type_arguments(const char **sig);
static type_parameter_t *parse_type_parameters(const char **sig);

/**
 * Create a new generic type
 */
static generic_type_t *generic_type_new(generic_type_kind_t kind)
{
    generic_type_t *type = calloc(1, sizeof(generic_type_t));
    if (type) {
        type->kind = kind;
    }
    return type;
}

/**
 * Parse a base type (primitive)
 * BaseType: B | C | D | F | I | J | S | Z
 */
static generic_type_t *parse_base_type(const char **sig)
{
    char c = **sig;
    switch (c) {
        case 'B': case 'C': case 'D': case 'F':
        case 'I': case 'J': case 'S': case 'Z':
            (*sig)++;
            generic_type_t *type = generic_type_new(GEN_PRIMITIVE);
            if (type) {
                type->data.primitive = c;
            }
            return type;
        default:
            return NULL;
    }
}

/**
 * Parse a type variable signature
 * TypeVariableSignature: T Identifier ;
 */
static generic_type_t *parse_type_variable(const char **sig)
{
    if (**sig != 'T') return NULL;
    (*sig)++;  /* Skip 'T' */
    
    const char *start = *sig;
    while (**sig && **sig != ';') {
        (*sig)++;
    }
    
    if (**sig != ';') return NULL;
    
    size_t len = *sig - start;
    generic_type_t *type = generic_type_new(GEN_TYPEVAR);
    if (type) {
        type->data.type_var_name = strndup(start, len);
    }
    (*sig)++;  /* Skip ';' */
    return type;
}

/**
 * Parse an array type signature
 * ArrayTypeSignature: [ TypeSignature
 */
static generic_type_t *parse_array_type(const char **sig)
{
    if (**sig != '[') return NULL;
    (*sig)++;  /* Skip '[' */
    
    generic_type_t *elem = parse_type_signature(sig);
    if (!elem) return NULL;
    
    generic_type_t *type = generic_type_new(GEN_ARRAY);
    if (type) {
        type->data.array_element = elem;
    } else {
        generic_type_free(elem);
    }
    return type;
}

/**
 * Parse type arguments
 * TypeArguments: < TypeArgument+ >
 * TypeArgument: WildcardIndicator? ReferenceTypeSignature | *
 * WildcardIndicator: + | -
 */
static type_argument_t *parse_type_arguments(const char **sig)
{
    if (**sig != '<') return NULL;
    (*sig)++;  /* Skip '<' */
    
    type_argument_t *head = NULL;
    type_argument_t *tail = NULL;
    
    while (**sig && **sig != '>') {
        type_argument_t *arg = calloc(1, sizeof(type_argument_t));
        if (!arg) break;
        
        if (**sig == '*') {
            /* Unbounded wildcard */
            arg->bound_kind = WILDCARD_NONE;
            arg->type = NULL;
            (*sig)++;
        } else if (**sig == '+') {
            /* Upper bound wildcard (? extends X) */
            arg->bound_kind = WILDCARD_EXTENDS;
            (*sig)++;
            arg->type = parse_type_signature(sig);
        } else if (**sig == '-') {
            /* Lower bound wildcard (? super X) */
            arg->bound_kind = WILDCARD_SUPER;
            (*sig)++;
            arg->type = parse_type_signature(sig);
        } else {
            /* Concrete type */
            arg->bound_kind = WILDCARD_NONE;
            arg->type = parse_type_signature(sig);
        }
        
        /* Append to list */
        if (!head) {
            head = tail = arg;
        } else {
            tail->next = arg;
            tail = arg;
        }
    }
    
    if (**sig == '>') {
        (*sig)++;  /* Skip '>' */
    }
    
    return head;
}

/**
 * Parse a simple class type segment (identifier with optional type args)
 */
static void parse_class_segment(const char **sig, char **name, type_argument_t **type_args)
{
    const char *start = *sig;
    
    /* Read identifier until < ; . or end */
    while (**sig && **sig != '<' && **sig != ';' && **sig != '.') {
        (*sig)++;
    }
    
    size_t len = *sig - start;
    *name = strndup(start, len);
    
    /* Check for type arguments */
    if (**sig == '<') {
        *type_args = parse_type_arguments(sig);
    } else {
        *type_args = NULL;
    }
}

/**
 * Parse a class type signature
 * ClassTypeSignature: L PackageSpecifier* SimpleClassTypeSignature ClassTypeSignatureSuffix* ;
 * SimpleClassTypeSignature: Identifier TypeArguments?
 * ClassTypeSignatureSuffix: . SimpleClassTypeSignature
 */
static generic_type_t *parse_class_type_signature(const char **sig)
{
    if (**sig != 'L') return NULL;
    (*sig)++;  /* Skip 'L' */
    
    /* Build the full class name, handling inner classes */
    char full_name[1024] = "";
    type_argument_t *final_type_args = NULL;
    
    while (**sig && **sig != ';') {
        char *segment = NULL;
        type_argument_t *segment_args = NULL;
        
        parse_class_segment(sig, &segment, &segment_args);
        
        if (segment) {
            if (strlen(full_name) > 0) {
                strcat(full_name, ".");
            }
            /* Convert / to . for internal names */
            for (char *p = segment; *p; p++) {
                if (*p == '/') *p = '.';
            }
            strcat(full_name, segment);
            free(segment);
        }
        
        /* Keep track of the last type arguments (for outermost class) */
        if (segment_args) {
            /* TODO: properly handle nested class type args */
            final_type_args = segment_args;
        }
        
        /* Check for inner class suffix */
        if (**sig == '.') {
            (*sig)++;  /* Skip '.' */
        }
    }
    
    if (**sig == ';') {
        (*sig)++;  /* Skip ';' */
    }
    
    generic_type_t *type = generic_type_new(GEN_CLASS);
    if (type) {
        type->data.class_type.name = strdup(full_name);
        type->data.class_type.type_args = final_type_args;
    }
    return type;
}

/**
 * Parse a type signature
 * TypeSignature: ReferenceTypeSignature | BaseType
 * ReferenceTypeSignature: ClassTypeSignature | TypeVariableSignature | ArrayTypeSignature
 */
static generic_type_t *parse_type_signature(const char **sig)
{
    if (!sig || !*sig || !**sig) return NULL;
    
    switch (**sig) {
        case 'B': case 'C': case 'D': case 'F':
        case 'I': case 'J': case 'S': case 'Z':
            return parse_base_type(sig);
        case 'V':
            (*sig)++;
            {
                generic_type_t *type = generic_type_new(GEN_PRIMITIVE);
                if (type) type->data.primitive = 'V';
                return type;
            }
        case 'L':
            return parse_class_type_signature(sig);
        case 'T':
            return parse_type_variable(sig);
        case '[':
            return parse_array_type(sig);
        default:
            return NULL;
    }
}

/**
 * Parse type parameters
 * TypeParameters: < TypeParameter+ >
 * TypeParameter: Identifier ClassBound InterfaceBound*
 * ClassBound: : ReferenceTypeSignature?
 * InterfaceBound: : ReferenceTypeSignature
 */
static type_parameter_t *parse_type_parameters(const char **sig)
{
    if (**sig != '<') return NULL;
    (*sig)++;  /* Skip '<' */
    
    type_parameter_t *head = NULL;
    type_parameter_t *tail = NULL;
    
    while (**sig && **sig != '>') {
        type_parameter_t *param = calloc(1, sizeof(type_parameter_t));
        if (!param) break;
        
        /* Parse identifier */
        const char *start = *sig;
        while (**sig && **sig != ':') {
            (*sig)++;
        }
        param->name = strndup(start, *sig - start);
        
        /* Parse class bound (: followed by optional reference type) */
        if (**sig == ':') {
            (*sig)++;  /* Skip ':' */
            /* Class bound is optional - check if there's a type */
            if (**sig == 'L' || **sig == '[' || **sig == 'T') {
                param->class_bound = parse_type_signature(sig);
            }
        }
        
        /* Parse interface bounds (each starts with :) */
        generic_type_t *iface_tail = NULL;
        while (**sig == ':') {
            (*sig)++;  /* Skip ':' */
            generic_type_t *iface = parse_type_signature(sig);
            if (iface) {
                if (!param->interface_bounds) {
                    param->interface_bounds = iface;
                    iface_tail = iface;
                } else {
                    iface_tail->next = iface;
                    iface_tail = iface;
                }
            }
        }
        
        /* Append to list */
        if (!head) {
            head = tail = param;
        } else {
            tail->next = param;
            tail = param;
        }
    }
    
    if (**sig == '>') {
        (*sig)++;  /* Skip '>' */
    }
    
    return head;
}

/**
 * Parse a method signature
 * MethodSignature: TypeParameters? ( TypeSignature* ) ReturnType ThrowsSignature*
 * ReturnType: TypeSignature | VoidDescriptor
 * ThrowsSignature: ^ ClassTypeSignature | ^ TypeVariableSignature
 */
method_signature_t *signature_parse_method(const char *sig)
{
    if (!sig) return NULL;
    
    method_signature_t *msig = calloc(1, sizeof(method_signature_t));
    if (!msig) return NULL;
    
    const char *p = sig;
    
    /* Optional type parameters */
    if (*p == '<') {
        msig->type_params = parse_type_parameters(&p);
    }
    
    /* Parameter types */
    if (*p != '(') {
        method_signature_free(msig);
        return NULL;
    }
    p++;  /* Skip '(' */
    
    generic_type_t *param_tail = NULL;
    while (*p && *p != ')') {
        generic_type_t *param = parse_type_signature(&p);
        if (param) {
            if (!msig->params) {
                msig->params = param;
                param_tail = param;
            } else {
                param_tail->next = param;
                param_tail = param;
            }
        }
    }
    
    if (*p == ')') {
        p++;  /* Skip ')' */
    }
    
    /* Return type */
    msig->return_type = parse_type_signature(&p);
    
    /* Throws signatures */
    generic_type_t *throws_tail = NULL;
    while (*p == '^') {
        p++;  /* Skip '^' */
        generic_type_t *throws = parse_type_signature(&p);
        if (throws) {
            if (!msig->throws) {
                msig->throws = throws;
                throws_tail = throws;
            } else {
                throws_tail->next = throws;
                throws_tail = throws;
            }
        }
    }
    
    return msig;
}

/**
 * Parse a class signature
 * ClassSignature: TypeParameters? SuperclassSignature SuperinterfaceSignature*
 * SuperclassSignature: ClassTypeSignature
 * SuperinterfaceSignature: ClassTypeSignature
 */
class_signature_t *signature_parse_class(const char *sig)
{
    if (!sig) return NULL;
    
    class_signature_t *csig = calloc(1, sizeof(class_signature_t));
    if (!csig) return NULL;
    
    const char *p = sig;
    
    /* Optional type parameters */
    if (*p == '<') {
        csig->type_params = parse_type_parameters(&p);
    }
    
    /* Superclass signature */
    csig->superclass = parse_class_type_signature(&p);
    
    /* Interface signatures */
    generic_type_t *iface_tail = NULL;
    while (*p == 'L') {
        generic_type_t *iface = parse_class_type_signature(&p);
        if (iface) {
            if (!csig->interfaces) {
                csig->interfaces = iface;
                iface_tail = iface;
            } else {
                iface_tail->next = iface;
                iface_tail = iface;
            }
        }
    }
    
    return csig;
}

/**
 * Parse a field/type signature
 */
generic_type_t *signature_parse_field(const char *sig)
{
    if (!sig) return NULL;
    const char *p = sig;
    return parse_type_signature(&p);
}

/**
 * Public wrapper for parse_type_signature
 */
generic_type_t *signature_parse_type(const char **sig)
{
    return parse_type_signature(sig);
}

/**
 * Free a type argument list
 */
static void type_arguments_free(type_argument_t *args)
{
    while (args) {
        type_argument_t *next = args->next;
        if (args->type) {
            generic_type_free(args->type);
        }
        free(args);
        args = next;
    }
}

/**
 * Free a type parameter list
 */
static void type_parameters_free(type_parameter_t *params)
{
    while (params) {
        type_parameter_t *next = params->next;
        free(params->name);
        if (params->class_bound) {
            generic_type_free(params->class_bound);
        }
        if (params->interface_bounds) {
            generic_type_free(params->interface_bounds);
        }
        free(params);
        params = next;
    }
}

/**
 * Free a generic type
 */
void generic_type_free(generic_type_t *type)
{
    while (type) {
        generic_type_t *next = type->next;
        
        switch (type->kind) {
            case GEN_CLASS:
                free(type->data.class_type.name);
                type_arguments_free(type->data.class_type.type_args);
                break;
            case GEN_TYPEVAR:
                free(type->data.type_var_name);
                break;
            case GEN_ARRAY:
                generic_type_free(type->data.array_element);
                break;
            case GEN_WILDCARD:
                if (type->data.wildcard.bound) {
                    generic_type_free(type->data.wildcard.bound);
                }
                break;
            default:
                break;
        }
        
        free(type);
        type = next;
    }
}

/**
 * Free a method signature
 */
void method_signature_free(method_signature_t *sig)
{
    if (!sig) return;
    
    type_parameters_free(sig->type_params);
    generic_type_free(sig->params);
    generic_type_free(sig->return_type);
    generic_type_free(sig->throws);
    free(sig);
}

/**
 * Free a class signature
 */
void class_signature_free(class_signature_t *sig)
{
    if (!sig) return;
    
    type_parameters_free(sig->type_params);
    generic_type_free(sig->superclass);
    generic_type_free(sig->interfaces);
    free(sig);
}

/**
 * Convert a generic type to string representation (for debugging)
 */
char *generic_type_to_string(generic_type_t *type)
{
    if (!type) return strdup("<null>");
    
    char buffer[1024] = "";
    
    switch (type->kind) {
        case GEN_PRIMITIVE:
            switch (type->data.primitive) {
                case 'B': return strdup("byte");
                case 'C': return strdup("char");
                case 'D': return strdup("double");
                case 'F': return strdup("float");
                case 'I': return strdup("int");
                case 'J': return strdup("long");
                case 'S': return strdup("short");
                case 'Z': return strdup("boolean");
                case 'V': return strdup("void");
                default: return strdup("?");
            }
            
        case GEN_CLASS:
            strcpy(buffer, type->data.class_type.name ? type->data.class_type.name : "?");
            if (type->data.class_type.type_args) {
                strcat(buffer, "<");
                type_argument_t *arg = type->data.class_type.type_args;
                bool first = true;
                while (arg) {
                    if (!first) strcat(buffer, ", ");
                    first = false;
                    
                    if (arg->bound_kind == WILDCARD_NONE && !arg->type) {
                        strcat(buffer, "?");
                    } else if (arg->bound_kind == WILDCARD_EXTENDS) {
                        strcat(buffer, "? extends ");
                        if (arg->type) {
                            char *s = generic_type_to_string(arg->type);
                            strcat(buffer, s);
                            free(s);
                        }
                    } else if (arg->bound_kind == WILDCARD_SUPER) {
                        strcat(buffer, "? super ");
                        if (arg->type) {
                            char *s = generic_type_to_string(arg->type);
                            strcat(buffer, s);
                            free(s);
                        }
                    } else if (arg->type) {
                        char *s = generic_type_to_string(arg->type);
                        strcat(buffer, s);
                        free(s);
                    }
                    arg = arg->next;
                }
                strcat(buffer, ">");
            }
            return strdup(buffer);
            
        case GEN_TYPEVAR:
            return strdup(type->data.type_var_name ? type->data.type_var_name : "?");
            
        case GEN_ARRAY:
            {
                char *elem = generic_type_to_string(type->data.array_element);
                snprintf(buffer, sizeof(buffer), "%s[]", elem);
                free(elem);
                return strdup(buffer);
            }
            
        case GEN_WILDCARD:
            if (type->data.wildcard.bound_kind == WILDCARD_NONE) {
                return strdup("?");
            } else if (type->data.wildcard.bound_kind == WILDCARD_EXTENDS) {
                char *bound = generic_type_to_string(type->data.wildcard.bound);
                snprintf(buffer, sizeof(buffer), "? extends %s", bound);
                free(bound);
                return strdup(buffer);
            } else {
                char *bound = generic_type_to_string(type->data.wildcard.bound);
                snprintf(buffer, sizeof(buffer), "? super %s", bound);
                free(bound);
                return strdup(buffer);
            }
            
        default:
            return strdup("<unknown>");
    }
}

/*
 * InnerClasses attribute parsing
 *
 * InnerClasses_attribute {
 *     u2 attribute_name_index;
 *     u4 attribute_length;
 *     u2 number_of_classes;
 *     {   u2 inner_class_info_index;
 *         u2 outer_class_info_index;
 *         u2 inner_name_index;
 *         u2 inner_class_access_flags;
 *     } classes[number_of_classes];
 * }
 */

void inner_class_info_free(inner_class_info_t *info)
{
    while (info) {
        inner_class_info_t *next = info->next;
        free(info->inner_class_name);
        free(info->outer_class_name);
        free(info->inner_name);
        free(info);
        info = next;
    }
}

inner_class_info_t *classfile_get_inner_classes(classfile_t *cf)
{
    if (!cf || !cf->attributes) {
        return NULL;
    }
    
    /* Find the InnerClasses attribute */
    attribute_info_t *inner_attr = NULL;
    for (uint16_t i = 0; i < cf->attributes_count; i++) {
        char *name = classfile_get_utf8(cf, cf->attributes[i].attribute_name_index);
        if (name && strcmp(name, "InnerClasses") == 0) {
            free(name);
            inner_attr = &cf->attributes[i];
            break;
        }
        free(name);
    }
    
    if (!inner_attr || !inner_attr->info || inner_attr->attribute_length < 2) {
        return NULL;
    }
    
    /* Parse the attribute */
    const uint8_t *data = inner_attr->info;
    uint16_t number_of_classes = ((uint16_t)data[0] << 8) | data[1];
    data += 2;
    
    /* Check we have enough data */
    if (inner_attr->attribute_length < (size_t)(2 + number_of_classes * 8)) {
        return NULL;
    }
    
    inner_class_info_t *head = NULL;
    inner_class_info_t *tail = NULL;
    
    for (uint16_t i = 0; i < number_of_classes; i++) {
        uint16_t inner_class_info_index = ((uint16_t)data[0] << 8) | data[1];
        uint16_t outer_class_info_index = ((uint16_t)data[2] << 8) | data[3];
        uint16_t inner_name_index = ((uint16_t)data[4] << 8) | data[5];
        uint16_t inner_class_access_flags = ((uint16_t)data[6] << 8) | data[7];
        data += 8;
        
        /* Get the class names */
        char *inner_class_name = inner_class_info_index ? 
            classfile_get_class_name(cf, inner_class_info_index) : NULL;
        char *outer_class_name = outer_class_info_index ?
            classfile_get_class_name(cf, outer_class_info_index) : NULL;
        char *inner_name = inner_name_index ?
            classfile_get_utf8(cf, inner_name_index) : NULL;
        
        /* Only add if we have a valid inner class name */
        if (inner_class_name) {
            inner_class_info_t *info = calloc(1, sizeof(inner_class_info_t));
            if (info) {
                /* Convert from internal form (a/b/C) to binary form (a.b.C) */
                info->inner_class_name = classname_to_binary(inner_class_name);
                info->outer_class_name = outer_class_name ? 
                    classname_to_binary(outer_class_name) : NULL;
                info->inner_name = inner_name ? strdup(inner_name) : NULL;
                info->access_flags = inner_class_access_flags;
                info->next = NULL;
                
                if (tail) {
                    tail->next = info;
                    tail = info;
                } else {
                    head = tail = info;
                }
            }
            free(inner_class_name);
        }
        free(outer_class_name);
        free(inner_name);
    }
    
    return head;
}
