/*
 * genesis.h
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
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#ifndef GENESIS_H
#define GENESIS_H

#include "util.h"
#include "type.h"

/* Forward declaration for classpath */
struct classpath;

#define GENESIS_VERSION "0.6"

/* ========================================================================
 * Compiler options
 * ======================================================================== */

typedef struct compiler_options
{
    char *source_version;           /* Source language version (e.g., "17") */
    char *target_version;           /* Target bytecode version */
    char *output_dir;               /* Output directory for class files */
    char *output_jar;               /* Output JAR file (mutually exclusive with output_dir) */
    char *main_class;               /* Main class for JAR manifest */
    char *classpath;                /* Classpath for resolving dependencies */
    char *sourcepath;               /* Source path for finding source files */
    slist_t *source_files;          /* List of source files to compile */
    bool verbose;                   /* Verbose output */
    bool debug_info;                /* Generate debug information */
    bool warnings;                  /* Enable warnings */
    bool werror;                    /* Treat warnings as errors */
} compiler_options_t;

compiler_options_t *compiler_options_new(void);
void compiler_options_free(compiler_options_t *opts);

/* ========================================================================
 * Source file representation
 * ======================================================================== */

typedef struct source_file
{
    char *filename;                 /* Path to source file */
    char *contents;                 /* File contents */
    size_t length;                  /* Length of contents */
} source_file_t;

source_file_t *source_file_new(const char *filename);
void source_file_free(source_file_t *src);
bool source_file_load(source_file_t *src);

/* ========================================================================
 * Dependency compilation
 * ======================================================================== */

/**
 * Compile a dependency source file if not already compiled.
 * This is called from semantic analysis when a class is loaded from source.
 * 
 * @param source_path  Path to the .java source file
 * @return true if compilation succeeded or file was already compiled
 */
bool compile_dependency(const char *source_path);

/**
 * Check if a source file has already been compiled in this session.
 */
bool is_source_compiled(const char *source_path);

/* ========================================================================
 * Token types
 * ======================================================================== */

typedef enum token_type
{
    /* End of file */
    TOK_EOF = 0,
    
    /* Literals */
    TOK_IDENTIFIER,
    TOK_INTEGER_LITERAL,
    TOK_LONG_LITERAL,
    TOK_FLOAT_LITERAL,
    TOK_DOUBLE_LITERAL,
    TOK_CHAR_LITERAL,
    TOK_STRING_LITERAL,
    TOK_TEXT_BLOCK,
    TOK_TRUE,
    TOK_FALSE,
    TOK_NULL,
    
    /* Keywords */
    TOK_ABSTRACT,
    TOK_ASSERT,
    TOK_BOOLEAN,
    TOK_BREAK,
    TOK_BYTE,
    TOK_CASE,
    TOK_CATCH,
    TOK_CHAR,
    TOK_CLASS,
    TOK_CONST,
    TOK_CONTINUE,
    TOK_DEFAULT,
    TOK_DO,
    TOK_DOUBLE,
    TOK_ELSE,
    TOK_ENUM,
    TOK_EXTENDS,
    TOK_FINAL,
    TOK_FINALLY,
    TOK_FLOAT,
    TOK_FOR,
    TOK_GOTO,
    TOK_IF,
    TOK_IMPLEMENTS,
    TOK_IMPORT,
    TOK_INSTANCEOF,
    TOK_INT,
    TOK_INTERFACE,
    TOK_LONG,
    TOK_NATIVE,
    TOK_NEW,
    TOK_PACKAGE,
    TOK_PRIVATE,
    TOK_PROTECTED,
    TOK_PUBLIC,
    TOK_RETURN,
    TOK_SHORT,
    TOK_STATIC,
    TOK_STRICTFP,
    TOK_SUPER,
    TOK_SWITCH,
    TOK_SYNCHRONIZED,
    TOK_THIS,
    TOK_THROW,
    TOK_THROWS,
    TOK_TRANSIENT,
    TOK_TRY,
    TOK_VOID,
    TOK_VOLATILE,
    TOK_WHILE,
    
    /* Contextual keywords (Java 9+) */
    TOK_VAR,
    TOK_YIELD,
    TOK_RECORD,
    TOK_SEALED,
    TOK_NON_SEALED,
    TOK_WHEN,               /* Pattern matching guard (Java 21+) */
    TOK_PERMITS,
    
    /* Module keywords (Java 9+) */
    TOK_MODULE,
    TOK_REQUIRES,
    TOK_EXPORTS,
    TOK_OPENS,
    TOK_USES,
    TOK_PROVIDES,
    TOK_WITH,
    TOK_TO,
    TOK_TRANSITIVE,
    TOK_OPEN,
    
    /* Separators */
    TOK_LPAREN,                     /* ( */
    TOK_RPAREN,                     /* ) */
    TOK_LBRACE,                     /* { */
    TOK_RBRACE,                     /* } */
    TOK_LBRACKET,                   /* [ */
    TOK_RBRACKET,                   /* ] */
    TOK_SEMICOLON,                  /* ; */
    TOK_COMMA,                      /* , */
    TOK_DOT,                        /* . */
    TOK_ELLIPSIS,                   /* ... */
    TOK_AT,                         /* @ */
    TOK_DOUBLE_COLON,               /* :: */
    
    /* Operators */
    TOK_ASSIGN,                     /* = */
    TOK_GT,                         /* > */
    TOK_LT,                         /* < */
    TOK_NOT,                        /* ! */
    TOK_TILDE,                      /* ~ */
    TOK_QUESTION,                   /* ? */
    TOK_COLON,                      /* : */
    TOK_ARROW,                      /* -> */
    TOK_EQ,                         /* == */
    TOK_GE,                         /* >= */
    TOK_LE,                         /* <= */
    TOK_NE,                         /* != */
    TOK_AND,                        /* && */
    TOK_OR,                         /* || */
    TOK_INC,                        /* ++ */
    TOK_DEC,                        /* -- */
    TOK_PLUS,                       /* + */
    TOK_MINUS,                      /* - */
    TOK_STAR,                       /* * */
    TOK_SLASH,                      /* / */
    TOK_BITAND,                     /* & */
    TOK_BITOR,                      /* | */
    TOK_CARET,                      /* ^ */
    TOK_MOD,                        /* % */
    TOK_LSHIFT,                     /* << */
    TOK_RSHIFT,                     /* >> */
    TOK_URSHIFT,                    /* >>> */
    TOK_PLUS_ASSIGN,                /* += */
    TOK_MINUS_ASSIGN,               /* -= */
    TOK_STAR_ASSIGN,                /* *= */
    TOK_SLASH_ASSIGN,               /* /= */
    TOK_AND_ASSIGN,                 /* &= */
    TOK_OR_ASSIGN,                  /* |= */
    TOK_XOR_ASSIGN,                 /* ^= */
    TOK_MOD_ASSIGN,                 /* %= */
    TOK_LSHIFT_ASSIGN,              /* <<= */
    TOK_RSHIFT_ASSIGN,              /* >>= */
    TOK_URSHIFT_ASSIGN,             /* >>>= */
    
    /* Special */
    TOK_ERROR,                      /* Lexer error */
    TOK_COMMENT                     /* Comment (if preserving) */
} token_type_t;

/* ========================================================================
 * Tokens
 * ======================================================================== */

/* Token value union - for numeric literals */
typedef union token_value {
    long long int_value;            /* Integer value */
    double float_value;             /* Float value */
} token_value_t;

/* Token type name helper */
const char *token_type_name(token_type_t type);

/* ========================================================================
 * Lexer
 * ======================================================================== */

typedef struct lexer
{
    source_file_t *source;          /* Source file being lexed */
    const char *pos;                /* Current position in source */
    const char *end;                /* End of source */
    int line;                       /* Current line number */
    int column;                     /* Current column number */
    char *error_msg;                /* Error message if any */
    
    /* Current token - embedded, no allocation needed */
    struct {
        token_type_t type;          /* Token type */
        int line;                   /* Token start line */
        int column;                 /* Token start column */
        const char *text_start;     /* Points into source or text_buf */
        size_t text_len;            /* Length of text */
        token_value_t value;        /* Numeric value if applicable */
    } token;
    
    /* Buffer for tokens that need processing (string escapes, etc.) */
    char text_buf[4096];
} lexer_t;

/* Lexer API */
lexer_t *lexer_new(source_file_t *source);
void lexer_free(lexer_t *lexer);

/* Feedforward tokenization - advance and access current token */
void lexer_advance(lexer_t *lexer);             /* Move to next token */
token_type_t lexer_type(lexer_t *lexer);        /* Current token type */
const char *lexer_text(lexer_t *lexer);         /* Current token text (null-terminated) */
size_t lexer_text_len(lexer_t *lexer);          /* Current token text length */
int lexer_line(lexer_t *lexer);                 /* Current token line */
int lexer_column(lexer_t *lexer);               /* Current token column */
token_value_t lexer_value(lexer_t *lexer);      /* Current token numeric value */

/* Lexer position for backtracking */
typedef struct lexer_pos {
    const char *pos;                /* Source position */
    int line, column;               /* Source location */
    token_type_t token_type;        /* Current token type */
    const char *token_text_start;   /* Token text start */
    size_t token_text_len;          /* Token text length */
    int token_line, token_column;   /* Token location */
    token_value_t token_value;      /* Token value */
} lexer_pos_t;

lexer_pos_t lexer_save_pos(lexer_t *lexer);     /* Save current position */
void lexer_restore_pos(lexer_t *lexer, lexer_pos_t pos);  /* Restore position */

/* ========================================================================
 * AST Node Types
 * ======================================================================== */

typedef enum ast_node_type
{
    /* Compilation unit */
    AST_COMPILATION_UNIT,
    AST_PACKAGE_DECL,
    AST_IMPORT_DECL,
    
    /* Type declarations */
    AST_CLASS_DECL,
    AST_INTERFACE_DECL,
    AST_ENUM_DECL,
    AST_RECORD_DECL,
    AST_ANNOTATION_DECL,
    
    /* Class members */
    AST_FIELD_DECL,
    AST_METHOD_DECL,
    AST_CONSTRUCTOR_DECL,
    AST_INITIALIZER_BLOCK,
    AST_ENUM_CONSTANT,
    
    /* Types */
    AST_PRIMITIVE_TYPE,
    AST_CLASS_TYPE,
    AST_ARRAY_TYPE,
    AST_TYPE_PARAMETER,
    AST_WILDCARD_TYPE,
    AST_VAR_TYPE,       /* var for type inference (Java 10+) */
    
    /* Parameters and variables */
    AST_PARAMETER,
    AST_VAR_DECL,
    AST_VAR_DECLARATOR,
    
    /* Statements */
    AST_BLOCK,
    AST_EMPTY_STMT,
    AST_EXPR_STMT,
    AST_IF_STMT,
    AST_FOR_STMT,
    AST_ENHANCED_FOR_STMT,
    AST_WHILE_STMT,
    AST_DO_STMT,
    AST_SWITCH_STMT,
    AST_SWITCH_EXPR,
    AST_CASE_LABEL,         /* Traditional case x: ... */
    AST_SWITCH_RULE,        /* Arrow-style case x -> expr or block */
    AST_TYPE_PATTERN,       /* Type pattern: case Type var -> (Java 21+) */
    AST_GUARDED_PATTERN,    /* Guarded pattern: case Type var when expr -> */
    AST_UNNAMED_PATTERN,    /* Unnamed pattern: case _ -> (matches anything) */
    AST_RECORD_PATTERN,     /* Record pattern: case Record(p1, p2) -> (Java 21+) */
    AST_RETURN_STMT,
    AST_THROW_STMT,
    AST_BREAK_STMT,
    AST_CONTINUE_STMT,
    AST_TRY_STMT,
    AST_RESOURCE_SPEC,          /* Resource specification for try-with-resources */
    AST_CATCH_CLAUSE,
    AST_FINALLY_CLAUSE,
    AST_SYNCHRONIZED_STMT,
    AST_ASSERT_STMT,
    AST_LABELED_STMT,
    AST_YIELD_STMT,
    
    /* Expressions */
    AST_LITERAL,
    AST_IDENTIFIER,
    AST_THIS_EXPR,
    AST_SUPER_EXPR,
    AST_BINARY_EXPR,
    AST_UNARY_EXPR,
    AST_CONDITIONAL_EXPR,
    AST_ASSIGNMENT_EXPR,
    AST_CAST_EXPR,
    AST_INSTANCEOF_EXPR,
    AST_METHOD_CALL,
    AST_FIELD_ACCESS,
    AST_ARRAY_ACCESS,
    AST_NEW_OBJECT,
    AST_NEW_ARRAY,
    AST_ARRAY_INIT,
    AST_LAMBDA_EXPR,
    AST_LAMBDA_PARAMS,  /* List of lambda parameters (typed or untyped) */
    AST_METHOD_REF,
    AST_CLASS_LITERAL,       /* Type.class expression */
    AST_PARENTHESIZED,
    AST_EXPLICIT_CTOR_CALL,  /* this() or super() constructor call */
    
    /* Annotations */
    AST_ANNOTATION,
    AST_ANNOTATION_VALUE,
    
    /* Modifiers */
    AST_MODIFIERS,
    
    /* Module declarations (Java 9+) */
    AST_MODULE_DECL,
    AST_REQUIRES_DIRECTIVE,
    AST_EXPORTS_DIRECTIVE,
    AST_OPENS_DIRECTIVE,
    AST_USES_DIRECTIVE,
    AST_PROVIDES_DIRECTIVE
} ast_node_type_t;

/* Forward declarations for AST and symbols (type_t, symbol_t provided by type.h) */
typedef struct ast_node ast_node_t;
typedef struct scope scope_t;

/* ========================================================================
 * AST Node Structure
 * ======================================================================== */

struct ast_node
{
    ast_node_type_t type;           /* Node type */
    int line;                       /* Source line */
    int column;                     /* Source column */
    
    /* Common fields stored in a union to save space */
    union {
        /* For identifiers, literals, operators */
        struct {
            char *name;             /* Identifier name or operator */
            token_type_t token_type;/* Original token type for literals */
            union {
                long long int_val;
                double float_val;
                char *str_val;
            } value;
        } leaf;
        
        /* For nodes with children */
        struct {
            slist_t *children;      /* Child nodes */
            uint32_t flags;         /* Modifier flags, etc. */
            char *name;             /* Optional name (class, method, etc.) */
            ast_node_t *extra;      /* Extra node (e.g., return type) */
            token_type_t op_token;  /* Operator token type for expressions */
        } node;
    } data;
    
    /* Annotations (for declarations) */
    slist_t *annotations;           /* List of AST_ANNOTATION nodes */
    
    /* Semantic analysis results (populated during semantic pass) */
    type_t *sem_type;               /* Resolved type for type nodes and expressions */
    symbol_t *sem_symbol;           /* Resolved symbol for identifiers */
    
    /* Lambda/method reference info (only for AST_LAMBDA_EXPR, AST_METHOD_REF) */
    slist_t *lambda_captures;       /* List of symbol_t* for captured variables */
    bool lambda_captures_this;      /* True if lambda captures 'this' */
    
    /* Annotation element default value (only for AST_METHOD_DECL in @interface) */
    ast_node_t *annotation_default; /* Default value for annotation elements */
};

/* Modifier flags (stored in node.flags) */
#define MOD_PUBLIC       0x0001
#define MOD_PRIVATE      0x0002
#define MOD_PROTECTED    0x0004
#define MOD_STATIC       0x0008
#define MOD_FINAL        0x0010
#define MOD_ABSTRACT     0x0020
#define MOD_NATIVE       0x0040
#define MOD_SYNCHRONIZED 0x0080
#define MOD_TRANSIENT    0x0100
#define MOD_VOLATILE     0x0200
#define MOD_STRICTFP     0x0400
#define MOD_DEFAULT      0x0800
#define MOD_SEALED       0x1000
#define MOD_NON_SEALED   0x2000
#define MOD_VARARGS          0x4000  /* Parameter is varargs (Type... args) */
#define MOD_RECORD_COMPONENT 0x8000  /* Record component (generates field + accessor) */

/* AST node flags (for non-modifier uses of data.node.flags) */
#define AST_METHOD_CALL_EXPLICIT_RECEIVER 0x10000  /* Method call has explicit receiver (obj.method()) */

/* Annotation retention policies */
typedef enum {
    RETENTION_SOURCE,   /* Discarded by compiler (e.g., @Override, @SuppressWarnings) */
    RETENTION_CLASS,    /* Recorded in class file, not available at runtime (default) */
    RETENTION_RUNTIME   /* Available at runtime via reflection (e.g., @Deprecated) */
} retention_policy_t;

/* Get retention policy for a known annotation (returns RETENTION_CLASS for unknown) */
retention_policy_t get_annotation_retention(const char *annotation_name);

/* AST node constructors */
ast_node_t *ast_new(ast_node_type_t type, int line, int column);
ast_node_t *ast_new_leaf(ast_node_type_t type, const char *name, int line, int column);
ast_node_t *ast_new_literal_from_lexer(lexer_t *lexer);  /* Create literal from current lexer token */
void ast_free(ast_node_t *node);
void ast_add_child(ast_node_t *parent, ast_node_t *child);
void ast_print(ast_node_t *node, int indent);
const char *ast_type_name(ast_node_type_t type);

/* ========================================================================
 * Parser (Iterative with feedforward lexer)
 * ======================================================================== */

/* Parser state for iterative parsing */
typedef struct parser
{
    lexer_t *lexer;                 /* Lexer for feedforward tokenization */
    source_file_t *source;          /* Source file for error messages */
    char *error_msg;                /* Error message if any */
    int error_line;                 /* Error line number */
    int error_column;               /* Error column number */
    token_type_t pending_token;     /* Pending token from split >> or >>> (0 if none) */
} parser_t;

/* Parser API */
parser_t *parser_new(lexer_t *lexer, source_file_t *source);
void parser_free(parser_t *parser);
ast_node_t *parser_parse(parser_t *parser);

/* Expression parsing - parse_expression_prec uses internal precedence enum */
ast_node_t *parse_expression(parser_t *parser);
ast_node_t *parse_type(parser_t *parser);
ast_node_t *parse_block(parser_t *parser);
ast_node_t *parse_array_initializer(parser_t *parser);
ast_node_t *parse_class_body(parser_t *parser);

/* ========================================================================
 * Type System - see type.h for type definitions
 * ========================================================================
 *
 * The type system is defined in type.h and implemented in type.c.
 * This provides type_t, type_kind_t, and all type manipulation functions.
 */

/* ========================================================================
 * Symbols
 * ======================================================================== */

/* Symbol kinds */
typedef enum symbol_kind
{
    SYM_PACKAGE,
    SYM_CLASS,
    SYM_INTERFACE,
    SYM_ENUM,
    SYM_RECORD,
    SYM_ANNOTATION,
    SYM_FIELD,
    SYM_METHOD,
    SYM_CONSTRUCTOR,
    SYM_PARAMETER,
    SYM_LOCAL_VAR,
    SYM_TYPE_PARAM,         /* Generic type parameter */
} symbol_kind_t;

/**
 * Represents a declared symbol (class, method, variable, etc.)
 */
struct symbol
{
    symbol_kind_t kind;
    char *name;                 /* Simple name */
    char *qualified_name;       /* Fully qualified name (for classes) */
    uint32_t modifiers;         /* Access modifiers (MOD_*) */
    type_t *type;               /* Symbol's type (field type, method return type) */
    scope_t *scope;             /* Scope containing this symbol */
    ast_node_t *ast;            /* AST node that declared this symbol */
    
    /* Kind-specific data */
    union {
        /* SYM_CLASS, SYM_INTERFACE */
        struct {
            symbol_t *superclass;       /* Superclass (NULL for Object) */
            slist_t *interfaces;        /* Implemented/extended interfaces */
            scope_t *members;           /* Class member scope */
            slist_t *type_params;       /* Type parameters (generics) */
            symbol_t *enclosing_class;  /* Enclosing class for nested classes */
            symbol_t *enclosing_method; /* Enclosing method for local classes */
            bool is_local_class;        /* True if this is a local class */
            bool is_anonymous_class;    /* True if this is an anonymous class */
            int local_class_counter;    /* Counter for $1, $2, ... naming */
            slist_t *captured_vars;     /* Captured local variables (for local classes) */
            ast_node_t *anonymous_body; /* AST body for anonymous classes */
        } class_data;
        
        /* SYM_METHOD, SYM_CONSTRUCTOR */
        struct {
            slist_t *parameters;        /* Parameter symbols */
            slist_t *throws;            /* Thrown exception types */
            scope_t *body_scope;        /* Method body scope */
            symbol_t *overridden_method; /* For covariant returns: method being overridden */
            char *descriptor;           /* JVM descriptor for classfile-loaded methods */
        } method_data;
        
        /* SYM_FIELD, SYM_LOCAL_VAR, SYM_PARAMETER */
        struct {
            int slot;                   /* Local variable slot (for codegen) */
            bool initialized;           /* Has been assigned a value */
            bool is_enum_constant;      /* True if this is an enum constant */
        } var_data;
    } data;
    
    int line;                   /* Source line */
    int column;                 /* Source column */
};

symbol_t *symbol_new(symbol_kind_t kind, const char *name);
void symbol_free(symbol_t *sym);
const char *symbol_kind_name(symbol_kind_t kind);

/* ========================================================================
 * Scopes
 * ======================================================================== */

/* Scope types */
typedef enum scope_type
{
    SCOPE_GLOBAL,           /* Top-level (packages, imports) */
    SCOPE_PACKAGE,          /* Package scope */
    SCOPE_CLASS,            /* Class/interface members */
    SCOPE_METHOD,           /* Method parameters + body */
    SCOPE_BLOCK,            /* Block statement { } */
    SCOPE_FOR,              /* For loop initializer */
} scope_type_t;

/**
 * Represents a lexical scope.
 */
struct scope
{
    scope_type_t type;
    scope_t *parent;            /* Enclosing scope */
    hashtable_t *symbols;       /* name -> symbol_t* */
    symbol_t *owner;            /* For class/method scopes, the owning symbol */
};

scope_t *scope_new(scope_type_t type, scope_t *parent);
void scope_free(scope_t *scope);
bool scope_define(scope_t *scope, symbol_t *symbol);
symbol_t *scope_lookup(scope_t *scope, const char *name);
symbol_t *scope_lookup_local(scope_t *scope, const char *name);
symbol_t *scope_lookup_method(scope_t *scope, const char *name);
symbol_t *scope_lookup_method_with_args(scope_t *scope, const char *name, int arg_count);

/* Forward declaration for semantic type (defined below) */
struct semantic;
symbol_t *scope_lookup_method_with_types(struct semantic *sem, scope_t *scope, 
                                         const char *name, slist_t *args);

/* ========================================================================
 * Semantic Analyzer (Iterative)
 * ======================================================================== */

/* Error/warning entry */
typedef struct diagnostic
{
    bool is_error;              /* true=error, false=warning */
    char *message;
    char *filename;
    int line;
    int column;
} diagnostic_t;

/**
 * Semantic analyzer state.
 */
typedef struct semantic
{
    scope_t *global_scope;      /* Global/package scope */
    scope_t *current_scope;     /* Current scope during analysis */
    
    hashtable_t *types;         /* Canonical type cache: name -> type_t* */
    hashtable_t *packages;      /* Package name -> scope_t* */
    
    slist_t *imports;           /* Import declarations */
    slist_t *static_imports;    /* Static import declarations */
    char *current_package;      /* Current package name */
    symbol_t *current_class;    /* Current class being analyzed */
    symbol_t *current_method;   /* Current method being analyzed */
    ast_node_t *current_lambda; /* Current lambda being analyzed (for capture tracking) */
    scope_t *lambda_enclosing_scope; /* Scope at lambda definition (parent of lambda params) */
    symbol_t *pending_pattern_var; /* Pattern variable from instanceof to add to next scope */
    type_t *target_type;         /* Target type for expression (for type inference) */
    
    slist_t *diagnostics;       /* List of diagnostic_t* */
    int error_count;
    int warning_count;
    
    bool warnings_enabled;      /* Emit warnings (default true) */
    bool werror;                /* Treat warnings as errors */
    
    int source_version;         /* Source version (e.g., 8, 11, 17, 21) */
    
    source_file_t *source;      /* Current source file */
    struct classpath *classpath; /* Classpath for resolving external types */
    slist_t *sourcepath;        /* Source path directories for finding .java files */
} semantic_t;

/* Semantic analyzer API */
semantic_t *semantic_new(struct classpath *cp);
void semantic_free(semantic_t *sem);
void semantic_set_classpath(semantic_t *sem, struct classpath *cp);
void semantic_set_sourcepath(semantic_t *sem, const char *sourcepath);
bool semantic_analyze(semantic_t *sem, ast_node_t *ast, source_file_t *source);
void semantic_error(semantic_t *sem, int line, int col, const char *fmt, ...);
void semantic_warning(semantic_t *sem, int line, int col, const char *fmt, ...);
void semantic_print_diagnostics(semantic_t *sem);

/* Type resolution */
type_t *semantic_resolve_type(semantic_t *sem, ast_node_t *type_node);
symbol_t *semantic_resolve_name(semantic_t *sem, const char *name);
type_t *get_expression_type(semantic_t *sem, ast_node_t *expr);
symbol_t *load_external_class(semantic_t *sem, const char *name);

/* External class loading (classfile_t defined in classfile.h) */
struct classfile;
symbol_t *symbol_from_classfile(semantic_t *sem, struct classfile *cf);

/* Symbol accessor functions (used by type.c for subtype checking) */
symbol_kind_t symbol_get_kind(symbol_t *sym);
const char *symbol_get_qualified_name(symbol_t *sym);
symbol_t *symbol_get_superclass(symbol_t *sym);
slist_t *symbol_get_interfaces(symbol_t *sym);

/* Superclass iteration macros - for traversing class hierarchy */
#define foreach_superclass(super, start) \
    for (symbol_t *super = symbol_get_superclass(start); \
         super != NULL; \
         super = symbol_get_superclass(super))

#define foreach_interface(iface_node, sym) \
    for (slist_t *iface_node = symbol_get_interfaces(sym); \
         iface_node != NULL; \
         iface_node = iface_node->next)

/* ========================================================================
 * Compilation
 * ======================================================================== */

/**
 * Compile a list of source files.
 * 
 * @param opts  Compiler options
 * @return      0 on success, non-zero on failure
 */
int compile(compiler_options_t *opts);

/**
 * Print the software version.
 */
void print_version(void);

/**
 * Print usage information.
 */
void print_usage(const char *program_name);

#endif /* GENESIS_H */

