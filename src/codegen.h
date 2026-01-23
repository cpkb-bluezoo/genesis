/*
 * codegen.h
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

#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdint.h>
#include <stdbool.h>
#include "genesis.h"
#include "constpool.h"
#include "classwriter.h"
#include "indy.h"
#include "stackmap.h"

/*
 * JVM Bytecode Instructions (opcodes)
 */
typedef enum jvm_opcode
{
    /* Constants */
    OP_NOP             = 0x00,
    OP_ACONST_NULL     = 0x01,
    OP_ICONST_M1       = 0x02,
    OP_ICONST_0        = 0x03,
    OP_ICONST_1        = 0x04,
    OP_ICONST_2        = 0x05,
    OP_ICONST_3        = 0x06,
    OP_ICONST_4        = 0x07,
    OP_ICONST_5        = 0x08,
    OP_LCONST_0        = 0x09,
    OP_LCONST_1        = 0x0A,
    OP_FCONST_0        = 0x0B,
    OP_FCONST_1        = 0x0C,
    OP_FCONST_2        = 0x0D,
    OP_DCONST_0        = 0x0E,
    OP_DCONST_1        = 0x0F,
    OP_BIPUSH          = 0x10,
    OP_SIPUSH          = 0x11,
    OP_LDC             = 0x12,
    OP_LDC_W           = 0x13,
    OP_LDC2_W          = 0x14,

    /* Loads */
    OP_ILOAD           = 0x15,
    OP_LLOAD           = 0x16,
    OP_FLOAD           = 0x17,
    OP_DLOAD           = 0x18,
    OP_ALOAD           = 0x19,
    OP_ILOAD_0         = 0x1A,
    OP_ILOAD_1         = 0x1B,
    OP_ILOAD_2         = 0x1C,
    OP_ILOAD_3         = 0x1D,
    OP_LLOAD_0         = 0x1E,
    OP_LLOAD_1         = 0x1F,
    OP_LLOAD_2         = 0x20,
    OP_LLOAD_3         = 0x21,
    OP_FLOAD_0         = 0x22,
    OP_FLOAD_1         = 0x23,
    OP_FLOAD_2         = 0x24,
    OP_FLOAD_3         = 0x25,
    OP_DLOAD_0         = 0x26,
    OP_DLOAD_1         = 0x27,
    OP_DLOAD_2         = 0x28,
    OP_DLOAD_3         = 0x29,
    OP_ALOAD_0         = 0x2A,
    OP_ALOAD_1         = 0x2B,
    OP_ALOAD_2         = 0x2C,
    OP_ALOAD_3         = 0x2D,
    OP_IALOAD          = 0x2E,
    OP_LALOAD          = 0x2F,
    OP_FALOAD          = 0x30,
    OP_DALOAD          = 0x31,
    OP_AALOAD          = 0x32,
    OP_BALOAD          = 0x33,
    OP_CALOAD          = 0x34,
    OP_SALOAD          = 0x35,

    /* Stores */
    OP_ISTORE          = 0x36,
    OP_LSTORE          = 0x37,
    OP_FSTORE          = 0x38,
    OP_DSTORE          = 0x39,
    OP_ASTORE          = 0x3A,
    OP_ISTORE_0        = 0x3B,
    OP_ISTORE_1        = 0x3C,
    OP_ISTORE_2        = 0x3D,
    OP_ISTORE_3        = 0x3E,
    OP_LSTORE_0        = 0x3F,
    OP_LSTORE_1        = 0x40,
    OP_LSTORE_2        = 0x41,
    OP_LSTORE_3        = 0x42,
    OP_FSTORE_0        = 0x43,
    OP_FSTORE_1        = 0x44,
    OP_FSTORE_2        = 0x45,
    OP_FSTORE_3        = 0x46,
    OP_DSTORE_0        = 0x47,
    OP_DSTORE_1        = 0x48,
    OP_DSTORE_2        = 0x49,
    OP_DSTORE_3        = 0x4A,
    OP_ASTORE_0        = 0x4B,
    OP_ASTORE_1        = 0x4C,
    OP_ASTORE_2        = 0x4D,
    OP_ASTORE_3        = 0x4E,
    OP_IASTORE         = 0x4F,
    OP_LASTORE         = 0x50,
    OP_FASTORE         = 0x51,
    OP_DASTORE         = 0x52,
    OP_AASTORE         = 0x53,
    OP_BASTORE         = 0x54,
    OP_CASTORE         = 0x55,
    OP_SASTORE         = 0x56,

    /* Stack */
    OP_POP             = 0x57,
    OP_POP2            = 0x58,
    OP_DUP             = 0x59,
    OP_DUP_X1          = 0x5A,
    OP_DUP_X2          = 0x5B,
    OP_DUP2            = 0x5C,
    OP_DUP2_X1         = 0x5D,
    OP_DUP2_X2         = 0x5E,
    OP_SWAP            = 0x5F,

    /* Math */
    OP_IADD            = 0x60,
    OP_LADD            = 0x61,
    OP_FADD            = 0x62,
    OP_DADD            = 0x63,
    OP_ISUB            = 0x64,
    OP_LSUB            = 0x65,
    OP_FSUB            = 0x66,
    OP_DSUB            = 0x67,
    OP_IMUL            = 0x68,
    OP_LMUL            = 0x69,
    OP_FMUL            = 0x6A,
    OP_DMUL            = 0x6B,
    OP_IDIV            = 0x6C,
    OP_LDIV            = 0x6D,
    OP_FDIV            = 0x6E,
    OP_DDIV            = 0x6F,
    OP_IREM            = 0x70,
    OP_LREM            = 0x71,
    OP_FREM            = 0x72,
    OP_DREM            = 0x73,
    OP_INEG            = 0x74,
    OP_LNEG            = 0x75,
    OP_FNEG            = 0x76,
    OP_DNEG            = 0x77,
    OP_ISHL            = 0x78,
    OP_LSHL            = 0x79,
    OP_ISHR            = 0x7A,
    OP_LSHR            = 0x7B,
    OP_IUSHR           = 0x7C,
    OP_LUSHR           = 0x7D,
    OP_IAND            = 0x7E,
    OP_LAND            = 0x7F,
    OP_IOR             = 0x80,
    OP_LOR             = 0x81,
    OP_IXOR            = 0x82,
    OP_LXOR            = 0x83,
    OP_IINC            = 0x84,

    /* Conversions */
    OP_I2L             = 0x85,
    OP_I2F             = 0x86,
    OP_I2D             = 0x87,
    OP_L2I             = 0x88,
    OP_L2F             = 0x89,
    OP_L2D             = 0x8A,
    OP_F2I             = 0x8B,
    OP_F2L             = 0x8C,
    OP_F2D             = 0x8D,
    OP_D2I             = 0x8E,
    OP_D2L             = 0x8F,
    OP_D2F             = 0x90,
    OP_I2B             = 0x91,
    OP_I2C             = 0x92,
    OP_I2S             = 0x93,

    /* Comparisons */
    OP_LCMP            = 0x94,
    OP_FCMPL           = 0x95,
    OP_FCMPG           = 0x96,
    OP_DCMPL           = 0x97,
    OP_DCMPG           = 0x98,
    OP_IFEQ            = 0x99,
    OP_IFNE            = 0x9A,
    OP_IFLT            = 0x9B,
    OP_IFGE            = 0x9C,
    OP_IFGT            = 0x9D,
    OP_IFLE            = 0x9E,
    OP_IF_ICMPEQ       = 0x9F,
    OP_IF_ICMPNE       = 0xA0,
    OP_IF_ICMPLT       = 0xA1,
    OP_IF_ICMPGE       = 0xA2,
    OP_IF_ICMPGT       = 0xA3,
    OP_IF_ICMPLE       = 0xA4,
    OP_IF_ACMPEQ       = 0xA5,
    OP_IF_ACMPNE       = 0xA6,

    /* Control */
    OP_GOTO            = 0xA7,
    OP_JSR             = 0xA8,
    OP_RET             = 0xA9,
    OP_TABLESWITCH     = 0xAA,
    OP_LOOKUPSWITCH    = 0xAB,
    OP_IRETURN         = 0xAC,
    OP_LRETURN         = 0xAD,
    OP_FRETURN         = 0xAE,
    OP_DRETURN         = 0xAF,
    OP_ARETURN         = 0xB0,
    OP_RETURN          = 0xB1,

    /* References */
    OP_GETSTATIC       = 0xB2,
    OP_PUTSTATIC       = 0xB3,
    OP_GETFIELD        = 0xB4,
    OP_PUTFIELD        = 0xB5,
    OP_INVOKEVIRTUAL   = 0xB6,
    OP_INVOKESPECIAL   = 0xB7,
    OP_INVOKESTATIC    = 0xB8,
    OP_INVOKEINTERFACE = 0xB9,
    OP_INVOKEDYNAMIC   = 0xBA,
    OP_NEW             = 0xBB,
    OP_NEWARRAY        = 0xBC,
    OP_ANEWARRAY       = 0xBD,
    OP_ARRAYLENGTH     = 0xBE,
    OP_ATHROW          = 0xBF,
    OP_CHECKCAST       = 0xC0,
    OP_INSTANCEOF      = 0xC1,
    OP_MONITORENTER    = 0xC2,
    OP_MONITOREXIT     = 0xC3,

    /* Extended */
    OP_WIDE            = 0xC4,
    OP_MULTIANEWARRAY  = 0xC5,
    OP_IFNULL          = 0xC6,
    OP_IFNONNULL       = 0xC7,
    OP_GOTO_W          = 0xC8,
    OP_JSR_W           = 0xC9
} jvm_opcode_t;

/* Array type codes for newarray instruction */
typedef enum array_type
{
    T_BOOLEAN = 4,
    T_CHAR    = 5,
    T_FLOAT   = 6,
    T_DOUBLE  = 7,
    T_BYTE    = 8,
    T_SHORT   = 9,
    T_INT     = 10,
    T_LONG    = 11
} array_type_t;

/*
 * Bytecode Buffer
 */
typedef struct bytecode
{
    uint8_t *code;
    uint32_t length;
    uint32_t capacity;
    uint16_t max_stack;
    uint16_t max_locals;
} bytecode_t;

/*
 * Label for branch targets
 */
typedef struct label
{
    int32_t offset;                 /* -1 if unresolved */
    slist_t *fixups;                /* List of offsets that reference this label */
} label_t;

/*
 * Line Number Entry (for LineNumberTable attribute)
 */
typedef struct line_number_entry
{
    uint16_t start_pc;
    uint16_t line_number;
} line_number_entry_t;

/*
 * Local Variable Info (for LocalVariableTable attribute)
 */
typedef struct local_var
{
    char *name;
    char *descriptor;
    uint16_t slot;
    uint16_t start_pc;
    uint16_t length;
    ast_node_t *type_ast;           /* Type AST node (for type annotations) */
} local_var_t;

/*
 * Exception Handler Entry
 */
typedef struct exception_entry
{
    uint16_t start_pc;
    uint16_t end_pc;
    uint16_t handler_pc;
    uint16_t catch_type;            /* 0 = finally, else index to CP_CLASS */
} exception_entry_t;

/*
 * Method Code Generation Context
 */
/* Loop context for break/continue */
typedef struct loop_context
{
    size_t continue_target;         /* Where continue jumps to (loop condition) */
    size_t break_patches;           /* List of break locations to patch */
    slist_t *break_offsets;         /* List of break instruction offsets to patch */
    const char *label;              /* Optional label name */
} loop_context_t;

/*
 * Local Variable Information
 * Consolidates all metadata for a local variable in one structure.
 */
typedef struct local_var_info
{
    uint16_t slot;                  /* Local variable slot number */
    type_kind_t kind;               /* Type kind for load/store opcode selection */
    bool is_ref;                    /* Is reference type (object or array) */
    bool is_array;                  /* Is array type */
    int array_dims;                 /* Array dimension count (0 if not array) */
    type_kind_t array_elem_kind;    /* Array element type kind (TYPE_UNKNOWN if not array) */
    char *class_name;               /* Class internal name (for class types, NULL otherwise) */
    char *array_elem_class;         /* Element class name (for object arrays, NULL otherwise) */
} local_var_info_t;

local_var_info_t *local_var_info_new(uint16_t slot, type_kind_t kind);
void local_var_info_free(local_var_info_t *info);

typedef struct method_gen
{
    bytecode_t *code;
    const_pool_t *cp;
    class_gen_t *class_gen;         /* Parent class generator */
    
    /* Local variables - single hashtable: name -> local_var_info_t* */
    hashtable_t *locals;            /* name -> local_var_info_t* */
    uint16_t next_slot;
    uint16_t max_locals;
    
    /* Stack tracking */
    uint16_t stack_depth;
    uint16_t max_stack;
    
    /* Labels for control flow */
    slist_t *labels;
    
    /* Loop context stack for break/continue */
    slist_t *loop_stack;
    
    /* Exception handlers */
    slist_t *exception_handlers;
    
    /* Local variable table (for debugging) */
    slist_t *local_var_table;
    
    /* Line number table */
    slist_t *line_numbers;
    
    /* StackMapTable for verification (Java 6+) */
    stack_map_table_t *stackmap;
    
    /* Current method info */
    symbol_t *method;
    bool is_static;
    bool is_constructor;
    
    /* Pending label for labeled statements (set by AST_LABELED_STMT, used by loops) */
    const char *pending_label;
    
    /* Last opcode emitted (for tracking implicit return needs) */
    uint8_t last_opcode;
    
    /* Switch expression yield patches - list of goto positions to patch to end */
    slist_t *yield_patches;
} method_gen_t;

/*
 * Field Generation Info
 */
typedef struct field_gen
{
    char *name;                     /* Field name */
    char *descriptor;               /* Field descriptor */
    char *signature;                /* Generic signature (if field uses type vars) */
    uint16_t access_flags;
    uint16_t name_index;
    uint16_t descriptor_index;
    slist_t *attributes;
    ast_node_t *ast;             /* Original AST (for annotations) */
} field_gen_t;

/*
 * Method Generation Info
 */
typedef struct method_info_gen
{
    uint16_t access_flags;
    uint16_t name_index;
    uint16_t descriptor_index;
    char *signature;             /* Generic signature (if method uses type params) */
    bytecode_t *code;
    slist_t *exception_handlers;
    slist_t *attributes;
    slist_t *line_numbers;       /* List of line_number_entry_t* */
    slist_t *local_var_table;    /* List of local_var_t* */
    stack_map_table_t *stackmap; /* StackMapTable for verification (Java 6+) */
    ast_node_t *ast;             /* Original AST (for annotations) */
    slist_t *throws;             /* List of exception class names (internal format) */
} method_info_gen_t;

/*
 * InnerClasses attribute entry
 */
typedef struct inner_class_entry
{
    uint16_t inner_class_info;  /* CP class index for inner class */
    uint16_t outer_class_info;  /* CP class index for outer class (0 for local/anonymous) */
    uint16_t inner_name;        /* CP utf8 index for simple name (0 for anonymous) */
    uint16_t access_flags;      /* Access flags for the inner class */
} inner_class_entry_t;

/*
 * Class Generation Context
 */
struct class_gen
{
    const_pool_t *cp;
    
    uint16_t access_flags;
    uint16_t this_class;
    uint16_t super_class;
    
    char *internal_name;            /* Internal class name (e.g., "com/example/Foo") */
    char *superclass;               /* Superclass internal name */
    
    slist_t *interfaces;            /* List of interface CP indices */
    slist_t *fields;                /* List of field_gen_t* */
    slist_t *methods;               /* List of method_info_gen_t* */
    slist_t *attributes;            /* Class attributes */
    
    /* Field lookup table: name -> field_gen_t* */
    hashtable_t *field_map;
    
    /* Source file info */
    char *source_file;
    
    /* Current class symbol */
    symbol_t *class_sym;
    
    /* Current class AST (for annotations) */
    ast_node_t *class_ast;
    
    /* Semantic analyzer for type info */
    semantic_t *sem;
    
    /* Assert support */
    bool needs_assertions;          /* True if class uses assert statements */
    uint16_t assert_field_ref;      /* CP index for $assertionsDisabled field */
    
    /* Instance initializers (injected into all constructors) */
    slist_t *instance_initializers; /* List of AST_INITIALIZER_BLOCK nodes */
    slist_t *instance_field_inits;  /* List of {field_name, initializer} pairs */
    
    /* Nested class declarations (to be compiled separately) */
    slist_t *nested_classes;        /* List of AST_CLASS_DECL/AST_INTERFACE_DECL nodes */
    
    /* invokedynamic support */
    bootstrap_methods_t *bootstrap_methods;  /* Bootstrap methods for invokedynamic */
    bool uses_invokedynamic;                 /* True if class uses invokedynamic */
    bool has_default_methods;                /* True if interface has default methods */
    bool use_stackmap;                       /* True to emit StackMapTable (requires version 50+) */
    int lambda_metafactory_bsm_idx;          /* BSM index for LambdaMetafactory (-1 if not used) */
    int lambda_counter;                      /* Counter for lambda$N naming */
    slist_t *lambda_methods;                 /* Synthetic lambda methods to generate */
    
    /* Local class declarations (classes defined inside method bodies) */
    slist_t *local_classes;         /* List of AST_CLASS_DECL nodes from method bodies */
    int local_class_counter;        /* Counter for $1, $2, etc. naming */
    
    /* Anonymous class declarations (created via new Type() { ... }) */
    slist_t *anonymous_classes;     /* List of symbol_t* for anonymous classes */
    
    /* Inner class support (non-static nested classes) */
    bool is_inner_class;            /* True if this is a non-static nested class */
    bool is_local_class;            /* True if this is a local class (in method) */
    bool is_anonymous_class;        /* True if this is an anonymous class */
    char *outer_class_internal;     /* Internal name of enclosing class */
    uint16_t this_dollar_zero_ref;  /* Field ref for this$0 field */
    
    /* Captured variables for local classes */
    slist_t *captured_vars;         /* List of symbol_t* for captured variables */
    hashtable_t *captured_field_refs; /* name -> uint16_t field ref for val$xxx */
    
    /* Target class file version (e.g., 52 for Java 8, 61 for Java 17) */
    int target_version;
    
    /* InnerClasses attribute entries */
    slist_t *inner_class_entries;   /* List of inner_class_entry_t* for InnerClasses attribute */
    
    /* Nest-based access control (Java 11+) */
    slist_t *nest_members;          /* List of uint16_t* class CP indices (for nest host) */
    uint16_t nest_host;             /* CP class index of nest host (for nested classes), 0 if host */
    
    /* Generic signature (if class has type parameters) */
    char *signature;
    
    /* Sealed classes (Java 17+) - list of permitted subclass CP indices */
    slist_t *permitted_subclasses;  /* List of uint16_t* class CP indices */
};

/*
 * Bytecode Buffer API
 */
bytecode_t *bytecode_new(void);
void bytecode_free(bytecode_t *bc);

void bc_emit(bytecode_t *bc, uint8_t opcode);
void bc_emit_u1(bytecode_t *bc, uint8_t value);
void bc_emit_u2(bytecode_t *bc, uint16_t value);
void bc_emit_u4(bytecode_t *bc, uint32_t value);
void bc_emit_s1(bytecode_t *bc, int8_t value);
void bc_emit_s2(bytecode_t *bc, int16_t value);

/* Get current bytecode offset */
uint32_t bc_offset(bytecode_t *bc);

/* Patch a 2-byte value at a specific offset */
void bc_patch_u2(bytecode_t *bc, uint32_t offset, uint16_t value);

/*
 * Label API
 */
label_t *label_new(void);
void label_free(label_t *label);
void label_bind(label_t *label, bytecode_t *bc);
void mg_bind_label(method_gen_t *mg, label_t *label);  /* Bind label and record stackmap frame */
void mg_record_frame(method_gen_t *mg);  /* Record stackmap frame at current offset */
void mg_record_exception_handler_frame(method_gen_t *mg, const char *exception_class);  /* Record frame at exception handler with exception on stack */
uint16_t mg_save_locals_count(method_gen_t *mg);  /* Save current stackmap locals count for later restore */
void mg_restore_locals_count(method_gen_t *mg, uint16_t count);  /* Restore stackmap locals count */
void label_emit_jump(label_t *label, bytecode_t *bc, uint8_t opcode);

/*
 * Method Generation API
 */
method_gen_t *method_gen_new(class_gen_t *cg, symbol_t *method);
void method_gen_free(method_gen_t *mg);

uint16_t mg_allocate_local(method_gen_t *mg, const char *name, type_t *type);
uint16_t mg_get_local(method_gen_t *mg, const char *name);
type_kind_t mg_get_local_type(method_gen_t *mg, const char *name);
void mg_enter_scope(method_gen_t *mg);
void mg_exit_scope(method_gen_t *mg);

/* Emit load/store for local variable with correct opcode for type */
void mg_emit_load_local(method_gen_t *mg, uint16_t slot, type_kind_t kind);
void mg_emit_store_local(method_gen_t *mg, uint16_t slot, type_kind_t kind);

void mg_push(method_gen_t *mg, int slots);
void mg_pop(method_gen_t *mg, int slots);

/* Type-aware stack operations (update both runtime and stackmap tracking) */
void mg_push_int(method_gen_t *mg);      /* Push integer type */
void mg_push_long(method_gen_t *mg);     /* Push long type (2 slots) */
void mg_push_float(method_gen_t *mg);    /* Push float type */
void mg_push_double(method_gen_t *mg);   /* Push double type (2 slots) */
void mg_push_null(method_gen_t *mg);     /* Push null reference */
void mg_push_object(method_gen_t *mg, const char *class_name);  /* Push object reference */
void mg_push_uninitialized(method_gen_t *mg, uint16_t new_offset);  /* Push uninitialized reference (from 'new' instruction) */
void mg_pop_typed(method_gen_t *mg, int slots);  /* Pop from both runtime and stackmap */

/* Line number and local variable recording */
void mg_record_line(method_gen_t *mg, int line);
void mg_record_local_var(method_gen_t *mg, const char *name, const char *descriptor,
                          uint16_t slot, uint16_t start_pc, ast_node_t *type_ast);
void mg_finalize_local_var(method_gen_t *mg, const char *name, uint16_t end_pc);

/*
 * Code Generation API
 */
class_gen_t *class_gen_new(semantic_t *sem, symbol_t *class_sym);
void class_gen_free(class_gen_t *cg);
void class_gen_set_target_version(class_gen_t *cg, int major_version);

/* Generate bytecode for a class */
bool codegen_class(class_gen_t *cg, ast_node_t *class_decl);
bool codegen_anonymous_class(class_gen_t *cg, symbol_t *anon_sym);

/* Generate module-info.class from module declaration */
uint8_t *codegen_module(ast_node_t *module_decl, size_t *size_out);

/* Generate bytecode for package-info.class */
uint8_t *codegen_package_info(ast_node_t *package_decl, slist_t *annotations,
                              semantic_t *sem, int target_version, size_t *size_out);

/* Generate bytecode for a method */
bool codegen_method(class_gen_t *cg, ast_node_t *method_decl);

/* Generate bytecode for a statement */
bool codegen_statement(method_gen_t *mg, ast_node_t *stmt);

/* Generate bytecode for an expression (leaves result on stack) */
bool codegen_expression(method_gen_t *mg, ast_node_t *expr);

/*
 * Type Descriptor Generation
 */

/* Convert a type to JVM type descriptor */
char *type_to_descriptor(type_t *type);

/* Convert a method to JVM method descriptor */
char *method_to_descriptor(symbol_t *method);

/* Convert internal class name (a.b.C) to JVM format (a/b/C) */
char *class_to_internal_name(const char *class_name);

/*
 * invokedynamic Support
 */

/* Ensure the LambdaMetafactory bootstrap method is registered, return BSM index */
int cg_ensure_lambda_metafactory(class_gen_t *cg);

/* Emit an invokedynamic instruction */
void mg_emit_invokedynamic(method_gen_t *mg, uint16_t indy_cp_index);

/*
 * Class File Writing
 */

/* Write a class_gen_t to a .class file */
bool write_class_file(class_gen_t *cg, const char *output_path);

/* Write a class_gen_t to a byte buffer (returns allocated buffer, sets *size) */
uint8_t *write_class_bytes(class_gen_t *cg, size_t *size);

#endif /* CODEGEN_H */

