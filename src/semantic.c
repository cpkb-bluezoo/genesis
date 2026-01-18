/*
 * semantic.c
 * Semantic analysis with iterative AST walking
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

#include "genesis.h"
#include "classpath.h"
#include "classfile.h"
#include <dirent.h>

/* Global pointer to current semantic analyzer for type_assignable callbacks */
static semantic_t *g_current_semantic = NULL;

/* Function for type.c to access the current semantic analyzer */
void *get_current_semantic(void)
{
    return g_current_semantic;
}

/* Forward declarations */
static bool has_annotation(ast_node_t *node, const char *short_name, const char *fqn);

/* ========================================================================
 * Symbol Accessor Functions (for type.c)
 * ======================================================================== */

symbol_kind_t symbol_get_kind(symbol_t *sym)
{
    return sym ? sym->kind : SYM_PACKAGE;
}

const char *symbol_get_qualified_name(symbol_t *sym)
{
    return sym ? sym->qualified_name : NULL;
}

symbol_t *symbol_get_superclass(symbol_t *sym)
{
    if (!sym) {
        return NULL;
    }
    if (sym->kind == SYM_CLASS || sym->kind == SYM_INTERFACE || sym->kind == SYM_ENUM) {
        return sym->data.class_data.superclass;
    }
    return NULL;
}

slist_t *symbol_get_interfaces(symbol_t *sym)
{
    if (!sym) {
        return NULL;
    }
    if (sym->kind == SYM_CLASS || sym->kind == SYM_INTERFACE || sym->kind == SYM_ENUM) {
        return sym->data.class_data.interfaces;
    }
    return NULL;
}

/* Forward declarations for lambda/method ref binding */
static bool bind_lambda_to_target_type(semantic_t *sem, ast_node_t *lambda, type_t *target_type);
static bool bind_method_ref_to_target_type(semantic_t *sem, ast_node_t *ref, type_t *target_type);
static type_t *substitute_from_receiver(type_t *type, type_t *recv_type);
static void bind_array_init_elements(semantic_t *sem, ast_node_t *init, type_t *array_type);
static type_t *infer_type_arg(type_t *param_type, type_t *arg_type, const char *var_name);
static type_t *substitute_type_var(type_t *type, const char *var_name, type_t *replacement);
static symbol_t *get_functional_interface_sam(symbol_t *iface_sym);
static char *resolve_import(semantic_t *sem, const char *simple_name);
static symbol_t *load_class_from_source(semantic_t *sem, const char *name);
static void preregister_nested_types(semantic_t *sem, ast_node_t *decl, symbol_t *sym, scope_t *class_scope);

/**
 * Check if a narrowing primitive conversion is allowed for a constant expression.
 * Per JLS 5.2, a narrowing conversion is allowed when:
 * - The source is a compile-time constant of type byte, short, char, or int
 * - The target is byte, short, or char
 * - The value fits in the target type
 * 
 * @param target_type The target type (should be byte, short, or char)
 * @param init_expr   The initializer expression
 * @param init_type   The type of the initializer (should be int or smaller)
 * @return true if narrowing conversion is allowed, false otherwise
 */
static bool narrowing_constant_allowed(type_t *target_type, ast_node_t *init_expr, type_t *init_type)
{
    if (!target_type || !init_expr || !init_type) {
        return false;
    }
    
    /* Only allow narrowing to byte, short, char */
    if (target_type->kind != TYPE_BYTE && 
        target_type->kind != TYPE_SHORT && 
        target_type->kind != TYPE_CHAR) {
        return false;
    }
    
    /* Source must be int or smaller (byte, short, char, int) */
    if (init_type->kind != TYPE_INT && 
        init_type->kind != TYPE_BYTE &&
        init_type->kind != TYPE_SHORT && 
        init_type->kind != TYPE_CHAR) {
        return false;
    }
    
    /* Check if the expression is a constant literal or unary minus on a literal */
    ast_node_t *literal_expr = init_expr;
    bool is_negated = false;
    
    /* Handle unary minus: -1, -127, etc. */
    if (init_expr->type == AST_UNARY_EXPR && 
        init_expr->data.node.name &&
        strcmp(init_expr->data.node.name, "-") == 0 &&
        init_expr->data.node.children) {
        literal_expr = (ast_node_t *)init_expr->data.node.children->data;
        is_negated = true;
    }
    
    if (literal_expr->type != AST_LITERAL) {
        return false;
    }
    
    /* Check if this is an integer or char literal (not float, string, etc.) */
    token_type_t tok_type = literal_expr->data.leaf.token_type;
    if (tok_type != TOK_INTEGER_LITERAL && tok_type != TOK_LONG_LITERAL && tok_type != TOK_CHAR_LITERAL) {
        return false;
    }
    
    /* Get the integer value */
    long long value;
    if (tok_type == TOK_CHAR_LITERAL) {
        /* Char literals store the character as a string in str_val */
        const char *str = literal_expr->data.leaf.value.str_val;
        value = (str && str[0]) ? (unsigned char)str[0] : 0;
    } else {
        value = literal_expr->data.leaf.value.int_val;
    }
    if (is_negated) {
        value = -value;
    }
    
    /* Check if the value fits in the target type */
    switch (target_type->kind) {
        case TYPE_BYTE:
            return value >= -128 && value <= 127;
        case TYPE_SHORT:
            return value >= -32768 && value <= 32767;
        case TYPE_CHAR:
            return value >= 0 && value <= 65535;
        default:
            return false;
    }
}

/**
 * Check if a member (field/method) is accessible from the given context.
 * 
 * @param member_mods     Modifiers of the member being accessed
 * @param member_class    Class that declares the member
 * @param accessing_class Class from which access is being made
 * @return true if access is allowed, false otherwise
 */
static bool check_access(uint32_t member_mods, symbol_t *member_class, symbol_t *accessing_class)
{
    /* Public is always accessible */
    if (member_mods & MOD_PUBLIC) {
        return true;
    }
    
    /* Interface members are implicitly public (unless private, Java 9+).
     * This handles cases where the symbol was loaded without MOD_PUBLIC being set. */
    if (member_class && member_class->kind == SYM_INTERFACE && 
        !(member_mods & MOD_PRIVATE)) {
        return true;
    }
    
    /* If accessing from same class, always allowed */
    if (member_class == accessing_class) {
        return true;
    }
    
    /* If no accessing class (e.g., at file level), only public is allowed */
    if (!accessing_class) {
        return false;
    }
    
    /* Private access: allowed within same class or between nested classes that share
     * the same top-level enclosing class. In Java, an outer class can access private
     * members of its inner classes, and vice versa. */
    if (member_mods & MOD_PRIVATE) {
        /* Find top-level enclosing class for both classes */
        symbol_t *member_top = member_class;
        while (member_top && member_top->data.class_data.enclosing_class) {
            member_top = member_top->data.class_data.enclosing_class;
        }
        
        symbol_t *access_top = accessing_class;
        while (access_top && access_top->data.class_data.enclosing_class) {
            access_top = access_top->data.class_data.enclosing_class;
        }
        
        /* If they share the same top-level class, private access is allowed */
        if (member_top == access_top && member_top != NULL) {
            return true;
        }
        
        return false;
    }
    
    /* Get package names for package-private and protected checks */
    const char *member_pkg = "";
    const char *access_pkg = "";
    
    if (member_class && member_class->qualified_name) {
        const char *last_dot = strrchr(member_class->qualified_name, '.');
        if (last_dot) {
            /* Has package */
            size_t pkg_len = last_dot - member_class->qualified_name;
            static char member_pkg_buf[256];
            strncpy(member_pkg_buf, member_class->qualified_name, pkg_len);
            member_pkg_buf[pkg_len] = '\0';
            member_pkg = member_pkg_buf;
        }
    }
    
    if (accessing_class && accessing_class->qualified_name) {
        const char *last_dot = strrchr(accessing_class->qualified_name, '.');
        if (last_dot) {
            size_t pkg_len = last_dot - accessing_class->qualified_name;
            static char access_pkg_buf[256];
            strncpy(access_pkg_buf, accessing_class->qualified_name, pkg_len);
            access_pkg_buf[pkg_len] = '\0';
            access_pkg = access_pkg_buf;
        }
    }
    
    bool same_package = (strcmp(member_pkg, access_pkg) == 0);
    
    /* Package-private (default): accessible within the same package */
    if (!(member_mods & MOD_PROTECTED)) {
        return same_package;
    }
    
    /* Protected: accessible within the same package OR from subclasses */
    if (same_package) {
        return true;
    }
    
    /* Check if accessing class is a subclass of member's class */
    symbol_t *current = accessing_class;
    while (current) {
        if (current == member_class) {
            return true;
        }
        current = current->data.class_data.superclass;
    }
    
    return false;
}

/**
 * Check for illegal modifier combinations on members (fields, methods).
 * Reports errors for incompatible modifier combinations.
 * 
 * @param sem Semantic analyzer context
 * @param mods The modifier flags to check
 * @param kind "field" or "method" for error messages
 * @param line Line number for error reporting
 * @param column Column number for error reporting
 */
static void check_modifier_combination(semantic_t *sem, uint32_t mods, 
                                        const char *kind, int line, int column)
{
    /* Check for multiple access modifiers */
    int access_count = 0;
    if (mods & MOD_PUBLIC) access_count++;
    if (mods & MOD_PRIVATE) access_count++;
    if (mods & MOD_PROTECTED) access_count++;
    
    if (access_count > 1) {
        semantic_error(sem, line, column,
            "illegal combination of modifiers: public, private, protected");
    }
    
    /* Check for abstract combined with illegal modifiers (methods only) */
    if ((mods & MOD_ABSTRACT) && strcmp(kind, "method") == 0) {
        if (mods & MOD_FINAL) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: abstract and final");
        }
        if (mods & MOD_PRIVATE) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: abstract and private");
        }
        if (mods & MOD_STATIC) {
            /* Allow abstract static in interfaces - check caller context */
            /* For now, report for non-interface contexts */
            if (sem->current_class && sem->current_class->kind != SYM_INTERFACE) {
                semantic_error(sem, line, column,
                    "illegal combination of modifiers: abstract and static");
            }
        }
        if (mods & MOD_NATIVE) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: abstract and native");
        }
        if (mods & MOD_SYNCHRONIZED) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: abstract and synchronized");
        }
        if (mods & MOD_STRICTFP) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: abstract and strictfp");
        }
    }
    
    /* Check for final volatile (fields only) */
    if (strcmp(kind, "field") == 0) {
        if ((mods & MOD_FINAL) && (mods & MOD_VOLATILE)) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: final and volatile");
        }
    }
    
    /* Check for native strictfp (methods only) */
    if (strcmp(kind, "method") == 0) {
        if ((mods & MOD_NATIVE) && (mods & MOD_STRICTFP)) {
            semantic_error(sem, line, column,
                "illegal combination of modifiers: native and strictfp");
        }
    }
}

/* Forward declaration for load_external_class (defined later) */
/* Load an external class by name from the classpath */
symbol_t *load_external_class(semantic_t *sem, const char *name);

/**
 * Check if a type implements Iterable.
 * 
 * This uses type_assignable() which already has deep interface inheritance checking.
 * 
 * @param sem  Semantic analyzer context
 * @param type The type to check
 * @return true if type is array or implements java.lang.Iterable
 */
static bool type_is_iterable(semantic_t *sem, type_t *type)
{
    if (!type) {
        return false;
    }
    
    /* Arrays are always iterable */
    if (type->kind == TYPE_ARRAY) {
        return true;
    }
    
    /* Only class types can implement Iterable */
    if (type->kind != TYPE_CLASS) {
        return false;
    }
    
    /* Ensure the class symbol is loaded */
    symbol_t *class_sym = type->data.class_type.symbol;
    if (!class_sym) {
        const char *class_name = type->data.class_type.name;
        if (class_name) {
            class_sym = load_external_class(sem, class_name);
            if (class_sym) {
                /* Update the type with the loaded symbol */
                type->data.class_type.symbol = class_sym;
            }
        }
    }
    
    if (!class_sym) {
        /* Can't resolve class - allow it (might be generic parameter etc.) */
        return true;
    }
    
    /* Load Iterable interface and use type_assignable for the check */
    symbol_t *iterable_sym = load_external_class(sem, "java.lang.Iterable");
    if (!iterable_sym) {
        /* Can't load Iterable - allow the code through */
        return true;
    }
    
    /* Create Iterable type and check assignability */
    type_t *iterable_type = type_new_class("java.lang.Iterable");
    iterable_type->data.class_type.symbol = iterable_sym;
    
    bool result = type_assignable(iterable_type, type);
    
    /* Note: We don't free iterable_type as types are typically cached/pooled */
    
    return result;
}

/* ========================================================================
 * Symbol Implementation
 * ======================================================================== */

/**
 * Create a new symbol.
 */
symbol_t *symbol_new(symbol_kind_t kind, const char *name)
{
    symbol_t *sym = calloc(1, sizeof(symbol_t));
    if (!sym) {
        return NULL;
    }
    
    sym->kind = kind;
    sym->name = name ? (char *)intern(name) : NULL;
    
    return sym;
}

/**
 * Free a symbol.
 */
void symbol_free(symbol_t *sym)
{
    if (!sym) {
        return;
    }
    
    /* name is interned, don't free */
    free(sym->qualified_name);  /* qualified_name is not interned */
    /* Don't free type - it's shared */
    /* Don't free scope - it has its own lifecycle */
    
    switch (sym->kind) {
        case SYM_CLASS:
        case SYM_INTERFACE:
        case SYM_ENUM:
            /* interfaces list contains symbol pointers, don't free */
            slist_free(sym->data.class_data.interfaces);
            slist_free(sym->data.class_data.type_params);
            break;
        
        case SYM_METHOD:
        case SYM_CONSTRUCTOR:
            slist_free(sym->data.method_data.parameters);
            slist_free(sym->data.method_data.throws);
            break;
        
        default:
            break;
    }
    
    free(sym);
}

/**
 * Get string name for symbol kind.
 */
const char *symbol_kind_name(symbol_kind_t kind)
{
    switch (kind) {
        case SYM_PACKAGE:     return "package";
        case SYM_CLASS:       return "class";
        case SYM_INTERFACE:   return "interface";
        case SYM_ENUM:        return "enum";
        case SYM_ANNOTATION:  return "annotation";
        case SYM_FIELD:       return "field";
        case SYM_METHOD:      return "method";
        case SYM_CONSTRUCTOR: return "constructor";
        case SYM_PARAMETER:   return "parameter";
        case SYM_LOCAL_VAR:   return "local variable";
        case SYM_TYPE_PARAM:  return "type parameter";
        default:              return "unknown";
    }
}

/* ========================================================================
 * Scope Implementation
 * ======================================================================== */

/**
 * Create a new scope.
 */
scope_t *scope_new(scope_type_t type, scope_t *parent)
{
    scope_t *scope = calloc(1, sizeof(scope_t));
    if (!scope) {
        return NULL;
    }
    
    scope->type = type;
    scope->parent = parent;
    scope->symbols = hashtable_new();
    
    return scope;
}

/**
 * Free a scope and its symbols.
 */
void scope_free(scope_t *scope)
{
    if (!scope) {
        return;
    }
    
    /* Free all symbols in this scope */
    hashtable_free_full(scope->symbols, (void (*)(void *))symbol_free);
    free(scope);
}

/**
 * Define a symbol in a scope.
 * Returns false if symbol already exists.
 */
bool scope_define(scope_t *scope, symbol_t *symbol)
{
    if (!scope || !symbol || !symbol->name) {
        return false;
    }
    
    /* For methods, we need to allow multiple symbols with the same name (overloading).
     * Use a unique key based on name and parameter types. */
    char *key = NULL;
    if (symbol->kind == SYM_METHOD || symbol->kind == SYM_CONSTRUCTOR) {
        /* Build a key like "methodName(param1Type,param2Type,...)" */
        char param_sig[512];
        char *p = param_sig;
        *p++ = '(';
        for (slist_t *param = symbol->data.method_data.parameters; param; param = param->next) {
            symbol_t *psym = (symbol_t *)param->data;
            if (psym && psym->type) {
                const char *type_name = psym->type->kind == TYPE_CLASS ? 
                    (psym->type->data.class_type.name ? psym->type->data.class_type.name : "?") :
                    (psym->type->kind == TYPE_INT ? "int" :
                     psym->type->kind == TYPE_BOOLEAN ? "boolean" :
                     psym->type->kind == TYPE_CHAR ? "char" :
                     psym->type->kind == TYPE_BYTE ? "byte" :
                     psym->type->kind == TYPE_SHORT ? "short" :
                     psym->type->kind == TYPE_LONG ? "long" :
                     psym->type->kind == TYPE_FLOAT ? "float" :
                     psym->type->kind == TYPE_DOUBLE ? "double" :
                     psym->type->kind == TYPE_ARRAY ? "[]" : "?");
                size_t len = strlen(type_name);
                if (p + len + 2 < param_sig + sizeof(param_sig)) {
                    if (p > param_sig + 1) *p++ = ',';
                    memcpy(p, type_name, len);
                    p += len;
                }
            }
        }
        *p++ = ')';
        *p = '\0';
        
        size_t key_len = strlen(symbol->name) + strlen(param_sig) + 1;
        key = malloc(key_len);
        snprintf(key, key_len, "%s%s", symbol->name, param_sig);
    } else {
        /* Check for duplicate in local scope only (non-methods) */
    if (hashtable_contains(scope->symbols, symbol->name)) {
        return false;
        }
        key = strdup(symbol->name);
    }
    
    symbol->scope = scope;
    hashtable_insert(scope->symbols, key, symbol);
    /* Note: hashtable takes ownership of the key string */
    return true;
}

/**
 * Look up a symbol, searching parent scopes.
 */
symbol_t *scope_lookup(scope_t *scope, const char *name)
{
    while (scope) {
        symbol_t *sym = hashtable_lookup(scope->symbols, name);
        if (sym) {
            return sym;
        }
        scope = scope->parent;
    }
    return NULL;
}

/**
 * Look up a symbol in local scope only.
 */
symbol_t *scope_lookup_local(scope_t *scope, const char *name)
{
    if (!scope) {
        return NULL;
    }
    return hashtable_lookup(scope->symbols, name);
}

/**
 * Look up a method by name in a scope.
 * Methods loaded from classfiles are stored with a "()" suffix to avoid
 * collisions with fields of the same name.
 */
/* Helper structure for collecting method overloads */
typedef struct {
    const char *name;     /* Method name to find */
    size_t name_len;      /* Length of method name */
    slist_t *candidates;  /* List of matching method symbols */
} method_lookup_ctx_t;

/* Callback for hashtable_foreach to collect method overloads */
static void collect_method_overloads(const char *key, void *value, void *user_data)
{
    method_lookup_ctx_t *ctx = (method_lookup_ctx_t *)user_data;
    symbol_t *sym = (symbol_t *)value;
    
    if (!sym || (sym->kind != SYM_METHOD && sym->kind != SYM_CONSTRUCTOR)) {
        return;
    }
    
    /* Check if key starts with the method name */
    if (strncmp(key, ctx->name, ctx->name_len) == 0) {
        /* Key is either "name", "name()", or "name(descriptor)" */
        char next_char = key[ctx->name_len];
        if (next_char == '\0' || next_char == '(') {
            /* This is a matching method */
            if (!ctx->candidates) {
                ctx->candidates = slist_new(sym);
            } else {
                slist_append(ctx->candidates, sym);
            }
        }
    }
}

symbol_t *scope_lookup_method(scope_t *scope, const char *name)
{
    if (!scope || !scope->symbols) {
        return NULL;
    }
    
    /* First try direct lookup (for methods defined in source code) */
    symbol_t *sym = hashtable_lookup(scope->symbols, name);
    if (sym && sym->kind == SYM_METHOD) {
        return sym;
    }
    
    /* Collect all method overloads with this name */
    method_lookup_ctx_t ctx = {
        .name = name,
        .name_len = strlen(name),
        .candidates = NULL
    };
    hashtable_foreach(scope->symbols, collect_method_overloads, &ctx);
    
    if (!ctx.candidates) {
        return NULL;
    }
    
    /* For now, return the first candidate - proper resolution happens in get_expression_type */
    sym = (symbol_t *)ctx.candidates->data;
    
    /* Free the candidates list (but not the symbols) */
    slist_free(ctx.candidates);
    
    return sym;
}

/**
 * Find all method overloads with the given name in a scope.
 * Returns a list of symbol_t* that the caller should free with slist_free().
 */
slist_t *scope_find_all_methods(scope_t *scope, const char *name)
{
    if (!scope || !scope->symbols || !name) {
        return NULL;
    }
    
    method_lookup_ctx_t ctx = {
        .name = name,
        .name_len = strlen(name),
        .candidates = NULL
    };
    hashtable_foreach(scope->symbols, collect_method_overloads, &ctx);
    
    return ctx.candidates;
}

/**
 * Find the best matching method overload for the given number of arguments.
 * Returns the method with matching parameter count, or NULL if no match.
 * 
 * @param scope The scope to search in
 * @param name The method name
 * @param arg_count Number of arguments being passed
 * @return The best matching method, or NULL if not found
 */
symbol_t *scope_lookup_method_with_args(scope_t *scope, const char *name, int arg_count)
{
    slist_t *candidates = scope_find_all_methods(scope, name);
    if (!candidates) {
        return NULL;
    }
    
    symbol_t *best_match = NULL;
    symbol_t *varargs_match = NULL;
    
    for (slist_t *node = candidates; node; node = node->next) {
        symbol_t *method = (symbol_t *)node->data;
        if (!method || (method->kind != SYM_METHOD && method->kind != SYM_CONSTRUCTOR)) {
            continue;
        }
        
        /* Count parameters */
        int param_count = 0;
        slist_t *params = method->data.method_data.parameters;
        for (slist_t *p = params; p; p = p->next) {
            param_count++;
        }
        
        /* Check for varargs */
        bool is_varargs = (method->modifiers & MOD_VARARGS) != 0;
        
        if (param_count == arg_count) {
            /* Exact match */
            best_match = method;
            break;
        } else if (is_varargs && arg_count >= param_count - 1) {
            /* Varargs match: can accept param_count-1 or more args */
            if (!varargs_match) {
                varargs_match = method;
            }
        }
    }
    
    slist_free(candidates);
    return best_match ? best_match : varargs_match;
}

/* Forward declarations */
static symbol_t *find_best_method_by_types(semantic_t *sem, slist_t *candidates, slist_t *args, type_t *recv_type);
symbol_t *scope_lookup_method_with_types_and_recv(struct semantic *sem, scope_t *scope, 
                                                   const char *name, slist_t *args, type_t *recv_type);

/**
 * Get the type name from an AST type node (for signature comparison).
 * Returns a pointer to the type name (not a copy - do not free).
 */
static const char *get_ast_type_name(ast_node_t *type_node)
{
    if (!type_node) return NULL;
    
    if (type_node->type == AST_CLASS_TYPE || type_node->type == AST_IDENTIFIER) {
        return type_node->data.node.name;
    } else if (type_node->type == AST_PRIMITIVE_TYPE) {
        /* Primitive types use data.leaf.name (created by ast_new_leaf) */
        return type_node->data.leaf.name;  /* "int", "boolean", etc. */
    } else if (type_node->type == AST_ARRAY_TYPE) {
        /* For arrays, get the element type name */
        if (type_node->data.node.children) {
            return get_ast_type_name((ast_node_t *)type_node->data.node.children->data);
        }
    }
    return NULL;
}

/**
 * Count array dimensions from an AST type node.
 */
static int get_ast_array_dims(ast_node_t *type_node)
{
    if (!type_node || type_node->type != AST_ARRAY_TYPE) return 0;
    
    int dims = 0;
    while (type_node && type_node->type == AST_ARRAY_TYPE) {
        dims++;
        if (type_node->data.node.children) {
            type_node = (ast_node_t *)type_node->data.node.children->data;
        } else {
            break;
        }
    }
    return dims;
}

/**
 * Check if a method AST node and a method symbol have the same parameter signature.
 * Used for @Override detection during pass1 before sym's parameters are populated.
 * 
 * @param method_ast The AST node of the method being checked
 * @param parent_method The parent method symbol to compare against
 * @return true if the methods have matching parameter types
 */
static bool method_ast_matches_signature(ast_node_t *method_ast, symbol_t *parent_method)
{
    if (!method_ast || !parent_method) return false;
    
    /* Count parameters in AST */
    int ast_param_count = 0;
    slist_t *ast_params = NULL;
    for (slist_t *c = method_ast->data.node.children; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (child && child->type == AST_PARAMETER) {
            ast_param_count++;
            if (!ast_params) {
                ast_params = c;  /* Remember first param */
            }
        }
    }
    
    /* Count parameters in symbol */
    int sym_param_count = 0;
    for (slist_t *p = parent_method->data.method_data.parameters; p; p = p->next) {
        sym_param_count++;
    }
    
    if (getenv("GENESIS_DEBUG_OVERRIDE")) {
        fprintf(stderr, "  comparing '%s'(%d params from AST) with '%s'(%d params from sym)\n",
            method_ast->data.node.name, ast_param_count, 
            parent_method->name, sym_param_count);
    }
    
    if (ast_param_count != sym_param_count) return false;
    
    /* Compare parameter types */
    slist_t *ast_p = ast_params;
    slist_t *sym_p = parent_method->data.method_data.parameters;
    
    while (ast_p && sym_p) {
        ast_node_t *ast_param = (ast_node_t *)ast_p->data;
        symbol_t *sym_param = (symbol_t *)sym_p->data;
        
        /* Skip non-parameter AST nodes */
        if (!ast_param || ast_param->type != AST_PARAMETER) {
            ast_p = ast_p->next;
            continue;
        }
        
        if (!sym_param || !sym_param->type) {
            ast_p = ast_p->next;
            sym_p = sym_p->next;
            continue;
        }
        
        /* Get AST parameter type from first child */
        ast_node_t *ast_type_node = NULL;
        if (ast_param->data.node.children) {
            ast_type_node = (ast_node_t *)ast_param->data.node.children->data;
        }
        
        if (!ast_type_node) {
            ast_p = ast_p->next;
            sym_p = sym_p->next;
            continue;
        }
        
        type_t *sym_type = sym_param->type;
        
        /* Compare types */
        const char *ast_name = get_ast_type_name(ast_type_node);
        int ast_dims = get_ast_array_dims(ast_type_node);
        
        if (getenv("GENESIS_DEBUG_OVERRIDE")) {
            fprintf(stderr, "    comparing AST param type '%s' (dims=%d) with sym type kind=%d\n",
                ast_name ? ast_name : "(null)", ast_dims, sym_type->kind);
        }
        
        /* Handle arrays */
        if (sym_type->kind == TYPE_ARRAY) {
            if (ast_dims == 0) return false;  /* AST not array but sym is */
            
            /* Get element type for comparison */
            type_t *elem = sym_type;
            int sym_dims = 0;
            while (elem && elem->kind == TYPE_ARRAY) {
                sym_dims++;
                elem = elem->data.array_type.element_type;
            }
            if (ast_dims != sym_dims) return false;
            
            /* Compare element types - handle both primitives and classes */
            const char *sym_name = NULL;
            if (elem) {
                switch (elem->kind) {
                    case TYPE_CLASS:
                        sym_name = elem->data.class_type.name;
                        break;
                    case TYPE_BOOLEAN: sym_name = "boolean"; break;
                    case TYPE_BYTE: sym_name = "byte"; break;
                    case TYPE_CHAR: sym_name = "char"; break;
                    case TYPE_SHORT: sym_name = "short"; break;
                    case TYPE_INT: sym_name = "int"; break;
                    case TYPE_LONG: sym_name = "long"; break;
                    case TYPE_FLOAT: sym_name = "float"; break;
                    case TYPE_DOUBLE: sym_name = "double"; break;
                    default: break;
                }
            }
            if (!ast_name || !sym_name) return false;
            
            /* Handle qualified vs simple names */
            const char *ast_simple = strrchr(ast_name, '.');
            ast_simple = ast_simple ? ast_simple + 1 : ast_name;
            const char *sym_simple = strrchr(sym_name, '.');
            sym_simple = sym_simple ? sym_simple + 1 : sym_name;
            
            if (strcmp(ast_simple, sym_simple) != 0) return false;
        } else if (ast_dims > 0) {
            return false;  /* AST is array but sym isn't */
        } else if (sym_type->kind == TYPE_CLASS) {
            const char *sym_name = sym_type->data.class_type.name;
            if (!ast_name || !sym_name) return false;
            
            /* Handle qualified vs simple names */
            const char *ast_simple = strrchr(ast_name, '.');
            ast_simple = ast_simple ? ast_simple + 1 : ast_name;
            const char *sym_simple = strrchr(sym_name, '.');
            sym_simple = sym_simple ? sym_simple + 1 : sym_name;
            
            if (getenv("GENESIS_DEBUG_OVERRIDE")) {
                fprintf(stderr, "    class compare: '%s' vs '%s'\n", ast_simple, sym_simple);
            }
            
            if (strcmp(ast_simple, sym_simple) != 0) return false;
        } else if (sym_type->kind == TYPE_TYPEVAR) {
            /* Type variable - the AST might have the concrete type */
            /* For override purposes, accept any class type as matching */
        } else {
            /* Primitive types - compare names */
            const char *sym_name = NULL;
            switch (sym_type->kind) {
                case TYPE_BOOLEAN: sym_name = "boolean"; break;
                case TYPE_BYTE: sym_name = "byte"; break;
                case TYPE_CHAR: sym_name = "char"; break;
                case TYPE_SHORT: sym_name = "short"; break;
                case TYPE_INT: sym_name = "int"; break;
                case TYPE_LONG: sym_name = "long"; break;
                case TYPE_FLOAT: sym_name = "float"; break;
                case TYPE_DOUBLE: sym_name = "double"; break;
                case TYPE_VOID: sym_name = "void"; break;
                default: break;
            }
            if (!ast_name || !sym_name || strcmp(ast_name, sym_name) != 0) {
                return false;
            }
        }
        
        ast_p = ast_p->next;
        sym_p = sym_p->next;
    }
    
    return true;
}

/**
 * Look up a method considering argument types for proper overload resolution.
 * This is the preferred method for method lookup when argument AST nodes are available.
 * 
 * @param sem Semantic analyzer context
 * @param scope The scope to search in
 * @param name The method name
 * @param args The argument list (slist of ast_node_t*)
 * @return The best matching method, or NULL if not found
 */
symbol_t *scope_lookup_method_with_types(struct semantic *sem, scope_t *scope, 
                                         const char *name, slist_t *args)
{
    return scope_lookup_method_with_types_and_recv(sem, scope, name, args, NULL);
}

/**
 * Look up a method considering argument types and receiver type for proper overload resolution.
 * 
 * @param sem Semantic analyzer context
 * @param scope The scope to search in
 * @param name The method name
 * @param args The argument list (slist of ast_node_t*)
 * @param recv_type The receiver type (for type argument substitution)
 * @return The best matching method, or NULL if not found
 */
/**
 * Collect all method overloads from a class's interfaces recursively.
 * Used to find default methods in interfaces.
 */
static void collect_methods_from_interfaces(semantic_t *sem, symbol_t *class_sym, 
                                            const char *name, slist_t **candidates)
{
    if (!class_sym || !name || !candidates) return;
    
    /* Check interfaces of this class */
    slist_t *interfaces = class_sym->data.class_data.interfaces;
    for (slist_t *iface_node = interfaces; iface_node; iface_node = iface_node->next) {
        symbol_t *iface = (symbol_t *)iface_node->data;
        if (!iface) continue;
        
        /* Try to load the interface if not fully loaded */
        if (iface->kind != SYM_INTERFACE && iface->kind != SYM_CLASS) {
            const char *iface_name = iface->qualified_name ? iface->qualified_name : iface->name;
            if (iface_name) {
                symbol_t *loaded = load_external_class(sem, iface_name);
                if (loaded) iface = loaded;
            }
        }
        
        /* Collect methods from this interface */
        if (iface->data.class_data.members) {
            slist_t *iface_methods = scope_find_all_methods(iface->data.class_data.members, name);
            for (slist_t *m = iface_methods; m; m = m->next) {
                /* Only add if not already in candidates (avoid duplicates) */
                bool found = false;
                for (slist_t *c = *candidates; c && !found; c = c->next) {
                    if (c->data == m->data) found = true;
                }
                if (!found) {
                    if (!*candidates) {
                        *candidates = slist_new(m->data);
                    } else {
                        slist_append(*candidates, m->data);
                    }
                }
            }
            slist_free(iface_methods);
        }
        
        /* Recursively check super-interfaces */
        collect_methods_from_interfaces(sem, iface, name, candidates);
    }
    
    /* Also check superclass's interfaces */
    if (class_sym->data.class_data.superclass) {
        collect_methods_from_interfaces(sem, class_sym->data.class_data.superclass, name, candidates);
    }
}

symbol_t *scope_lookup_method_with_types_and_recv(struct semantic *sem, scope_t *scope, 
                                                   const char *name, slist_t *args, type_t *recv_type)
{
    slist_t *candidates = scope_find_all_methods(scope, name);
    
    /* Also collect methods from implemented interfaces (for default methods) */
    if (recv_type && recv_type->kind == TYPE_CLASS && recv_type->data.class_type.symbol) {
        collect_methods_from_interfaces(sem, recv_type->data.class_type.symbol, name, &candidates);
    }
    
    if (!candidates) {
        return NULL;
    }
    
    symbol_t *result = find_best_method_by_types(sem, candidates, args, recv_type);
    slist_free(candidates);
    return result;
}

/**
 * Recursively look up a method in an interface hierarchy.
 * Checks the interface and all its super-interfaces.
 */
static symbol_t *lookup_method_in_interfaces(semantic_t *sem, symbol_t *class_sym, 
                                              const char *method_name)
{
    if (!class_sym || !method_name) {
        return NULL;
    }
    
    /* Check the interfaces of this class/interface */
    slist_t *interfaces = class_sym->data.class_data.interfaces;
    for (slist_t *iface_node = interfaces; iface_node; iface_node = iface_node->next) {
        symbol_t *iface = (symbol_t *)iface_node->data;
        if (!iface) continue;
        
        /* Check if interface is just a name (not fully loaded) */
        if (iface->kind != SYM_INTERFACE && iface->kind != SYM_CLASS) {
            /* Try to load the interface */
            const char *iface_name = iface->qualified_name ? iface->qualified_name : iface->name;
            if (iface_name) {
                symbol_t *loaded = load_external_class(sem, iface_name);
                if (loaded) {
                    iface = loaded;
                }
            }
        }
        
        /* Check direct members of this interface */
        if (iface->data.class_data.members) {
            symbol_t *method = scope_lookup_method(iface->data.class_data.members, method_name);
            if (method && method->kind == SYM_METHOD) {
                return method;
            }
        }
        
        /* Recursively check super-interfaces */
        symbol_t *method = lookup_method_in_interfaces(sem, iface, method_name);
        if (method) {
            return method;
        }
    }
    
    /* Also check the superclass chain's interfaces.
     * This is needed when a class extends another class that implements interfaces.
     * E.g., SNIKeyManager extends X509ExtendedKeyManager implements X509KeyManager */
    if (class_sym->data.class_data.superclass) {
        symbol_t *method = lookup_method_in_interfaces(sem, 
            class_sym->data.class_data.superclass, method_name);
        if (method) {
            return method;
        }
    }
    
    return NULL;
}

/* ========================================================================
 * Diagnostic Implementation
 * ======================================================================== */

static diagnostic_t *diagnostic_new(bool is_error, const char *filename,
                                     int line, int col, const char *message)
{
    diagnostic_t *diag = calloc(1, sizeof(diagnostic_t));
    if (!diag) {
        return NULL;
    }
    
    diag->is_error = is_error;
    diag->filename = filename ? strdup(filename) : NULL;
    diag->line = line;
    diag->column = col;
    diag->message = message ? strdup(message) : NULL;
    
    return diag;
}

static void diagnostic_free(diagnostic_t *diag)
{
    if (!diag) {
        return;
    }
    free(diag->filename);
    free(diag->message);
    free(diag);
}

/* ========================================================================
 * Semantic Analyzer Implementation
 * ======================================================================== */

/* Forward declaration needed for semantic_new */
/* Load an external class by name from the classpath */
symbol_t *load_external_class(semantic_t *sem, const char *name);

/**
 * Create a new semantic analyzer.
 */
semantic_t *semantic_new(classpath_t *cp)
{
    semantic_t *sem = calloc(1, sizeof(semantic_t));
    if (!sem) {
        return NULL;
    }
    
    sem->global_scope = scope_new(SCOPE_GLOBAL, NULL);
    sem->current_scope = sem->global_scope;
    sem->types = hashtable_new();        /* Global cache: qualified names only */
    sem->unit_types = hashtable_new();   /* Per-compilation-unit: simple names (like javac's toplevelScope) */
    sem->packages = hashtable_new();
    sem->resolved_imports = hashtable_new();  /* Cache for import resolution */
    sem->classpath = cp;
    sem->warnings_enabled = true;
    sem->werror = false;
    sem->source_version = 17;  /* Default to Java 17 */
    
    /* Pre-populate with java.lang classes from classpath if available */
    if (cp) {
        symbol_t *object_sym = load_external_class(sem, "java.lang.Object");
        if (object_sym && object_sym->type) {
            hashtable_insert(sem->types, "java.lang.Object", object_sym->type);
            hashtable_insert(sem->types, "Object", object_sym->type);
        } else {
            hashtable_insert(sem->types, "java.lang.Object", type_object());
            hashtable_insert(sem->types, "Object", type_object());
        }
        
        symbol_t *string_sym = load_external_class(sem, "java.lang.String");
        if (string_sym && string_sym->type) {
            hashtable_insert(sem->types, "java.lang.String", string_sym->type);
            hashtable_insert(sem->types, "String", string_sym->type);
        } else {
            hashtable_insert(sem->types, "java.lang.String", type_string());
            hashtable_insert(sem->types, "String", type_string());
        }
    } else {
        /* No classpath - use builtin types */
        hashtable_insert(sem->types, "java.lang.Object", type_object());
        hashtable_insert(sem->types, "java.lang.String", type_string());
        hashtable_insert(sem->types, "Object", type_object());
        hashtable_insert(sem->types, "String", type_string());
    }
    
    return sem;
}

/**
 * Set the classpath for resolving external types.
 */
void semantic_set_classpath(semantic_t *sem, classpath_t *cp)
{
    if (sem) {
        sem->classpath = cp;
    }
}

/**
 * Parse a sourcepath string into a list of directory paths.
 * This can be used before a semantic_t is created.
 * The caller is responsible for freeing the list with sourcepath_list_free().
 */
slist_t *sourcepath_parse(const char *sourcepath)
{
    if (!sourcepath) {
        return NULL;
    }
    
    slist_t *result = NULL;
    slist_t *tail = NULL;
    
    /* Parse colon-separated path */
    char *path_copy = strdup(sourcepath);
    char *saveptr = NULL;
    char *token = strtok_r(path_copy, ":", &saveptr);
    
    while (token) {
        if (strlen(token) > 0) {
            if (!result) {
                result = slist_new(strdup(token));
                tail = result;
            } else {
                tail = slist_append(tail, strdup(token));
            }
        }
        token = strtok_r(NULL, ":", &saveptr);
    }
    
    free(path_copy);
    return result;
}

/**
 * Free a sourcepath list created by sourcepath_parse().
 */
void sourcepath_list_free(slist_t *list)
{
    slist_free_full(list, free);
}

/**
 * Set the sourcepath for resolving source files.
 * The sourcepath string is colon-separated (or semicolon on Windows).
 */
void semantic_set_sourcepath(semantic_t *sem, const char *sourcepath)
{
    if (!sem || !sourcepath) {
        return;
    }
    
    /* Free existing sourcepath */
    slist_free_full(sem->sourcepath, free);
    sem->sourcepath = NULL;
    
    /* Parse colon-separated path */
    char *path_copy = strdup(sourcepath);
    char *saveptr = NULL;
    char *token = strtok_r(path_copy, ":", &saveptr);
    
    slist_t *tail = NULL;
    while (token) {
        if (strlen(token) > 0) {
            if (!sem->sourcepath) {
                sem->sourcepath = slist_new(strdup(token));
                tail = sem->sourcepath;
            } else {
                tail = slist_append(tail, strdup(token));
            }
        }
        token = strtok_r(NULL, ":", &saveptr);
    }
    
    free(path_copy);
}

/**
 * Free a semantic analyzer.
 */
void semantic_free(semantic_t *sem)
{
    if (!sem) {
        return;
    }
    
    scope_free(sem->global_scope);
    hashtable_free(sem->types);       /* Types are singletons, don't free values */
    hashtable_free(sem->unit_types);  /* Per-compilation-unit types, don't free values */
    hashtable_free(sem->packages);
    hashtable_free(sem->resolved_imports);  /* Values are interned, don't free */
    slist_free(sem->imports);
    slist_free_full(sem->sourcepath, free);
    slist_free_full(sem->source_dependencies, free);
    free(sem->current_package);
    slist_free_full(sem->diagnostics, (void (*)(void *))diagnostic_free);
    
    free(sem);
}

/**
 * Report a semantic error.
 */
void semantic_error(semantic_t *sem, int line, int col, const char *fmt, ...)
{
    /* Suppress errors when loading dependency files.
     * Errors will be reported when the dependency is compiled directly. */
    if (sem->loading_dependency) {
        return;
    }
    
    va_list args;
    char message[1024];
    
    va_start(args, fmt);
    vsnprintf(message, sizeof(message), fmt, args);
    va_end(args);
    
    const char *filename = sem->source ? sem->source->filename : NULL;
    diagnostic_t *diag = diagnostic_new(true, filename, line, col, message);
    
    if (!sem->diagnostics) {
        sem->diagnostics = slist_new(diag);
    } else {
        slist_append(sem->diagnostics, diag);
    }
    
    sem->error_count++;
}

/**
 * Report a semantic warning.
 */
void semantic_warning(semantic_t *sem, int line, int col, const char *fmt, ...)
{
    /* Skip if warnings are disabled */
    if (!sem->warnings_enabled) {
        return;
    }
    
    va_list args;
    char message[1024];
    
    va_start(args, fmt);
    vsnprintf(message, sizeof(message), fmt, args);
    va_end(args);
    
    const char *filename = sem->source ? sem->source->filename : NULL;
    
    /* If -Werror, treat as error */
    bool is_error = sem->werror;
    diagnostic_t *diag = diagnostic_new(is_error, filename, line, col, message);
    
    if (!sem->diagnostics) {
        sem->diagnostics = slist_new(diag);
    } else {
        slist_append(sem->diagnostics, diag);
    }
    
    if (sem->werror) {
        sem->error_count++;
    } else {
        sem->warning_count++;
    }
}

/**
 * Print all diagnostics.
 */
void semantic_print_diagnostics(semantic_t *sem)
{
    slist_t *list = sem->diagnostics;
    while (list) {
        diagnostic_t *diag = (diagnostic_t *)list->data;
        fprintf(stderr, "%s:%d:%d: %s: %s\n",
                diag->filename ? diag->filename : "<unknown>",
                diag->line, diag->column,
                diag->is_error ? "error" : "warning",
                diag->message);
        list = list->next;
    }
}

/* ========================================================================
 * External Class Loading
 * ======================================================================== */

/* Forward declaration for recursive loading */
/* Load an external class by name from the classpath */
symbol_t *load_external_class(semantic_t *sem, const char *name);

/**
 * Convert a generic_type_t (from signature parsing) to our internal type_t.
 * This preserves generic type information including type arguments and bounds.
 */
static type_t *generic_type_to_type(generic_type_t *gt)
{
    if (!gt) return NULL;
    
    switch (gt->kind) {
        case GEN_PRIMITIVE:
            switch (gt->data.primitive) {
                case 'B': return type_byte();
                case 'C': return type_char();
                case 'D': return type_double();
                case 'F': return type_float();
                case 'I': return type_int();
                case 'J': return type_long();
                case 'S': return type_short();
                case 'Z': return type_boolean();
                case 'V': return type_void();
                default: return type_new_primitive(TYPE_UNKNOWN);
            }
            
        case GEN_CLASS:
            {
                /* Convert internal class name (java/util/List) to binary name (java.util.List) */
                char *binary_name = classname_to_binary(gt->data.class_type.name);
                type_t *type = type_new_class(binary_name ? binary_name : gt->data.class_type.name);
                free(binary_name);
                /* Convert type arguments */
                if (gt->data.class_type.type_args) {
                    type_argument_t *arg = gt->data.class_type.type_args;
                    while (arg) {
                        type_t *arg_type = NULL;
                        if (arg->bound_kind == WILDCARD_NONE && arg->type) {
                            /* Concrete type argument */
                            arg_type = generic_type_to_type(arg->type);
                        } else if (arg->bound_kind == WILDCARD_EXTENDS && arg->type) {
                            /* ? extends X - bound_kind 1 */
                            arg_type = type_new_wildcard(1, generic_type_to_type(arg->type));
                        } else if (arg->bound_kind == WILDCARD_SUPER && arg->type) {
                            /* ? super X - bound_kind -1 */
                            arg_type = type_new_wildcard(-1, generic_type_to_type(arg->type));
                        } else {
                            /* Unbounded ? - bound_kind 0 */
                            arg_type = type_new_wildcard(0, NULL);
                        }
                        if (arg_type) {
                            /* Append to type args list - slist_append returns the new node,
                             * so we only assign on first append */
                            if (type->data.class_type.type_args == NULL) {
                                type->data.class_type.type_args = slist_new(arg_type);
                            } else {
                                slist_append(type->data.class_type.type_args, arg_type);
                            }
                        }
                        arg = arg->next;
                    }
                }
                return type;
            }
            
        case GEN_TYPEVAR:
            {
                /* Create a type variable type */
                type_t *type_var = malloc(sizeof(type_t));
                if (type_var) {
                    type_var->kind = TYPE_TYPEVAR;
                    type_var->data.type_var.name = strdup(gt->data.type_var_name ? gt->data.type_var_name : "?");
                    type_var->data.type_var.bound = NULL;
                }
                return type_var;
            }
            
        case GEN_ARRAY:
            {
                type_t *elem = generic_type_to_type(gt->data.array_element);
                return type_new_array(elem, 1);
            }
            
        case GEN_WILDCARD:
            {
                int bk = 0;
                if (gt->data.wildcard.bound_kind == WILDCARD_EXTENDS) bk = 1;
                else if (gt->data.wildcard.bound_kind == WILDCARD_SUPER) bk = -1;
                return type_new_wildcard(bk, generic_type_to_type(gt->data.wildcard.bound));
            }
            
        default:
            return type_new_primitive(TYPE_UNKNOWN);
    }
}

/**
 * Convert a type descriptor to a type_t.
 */
static type_t *type_from_descriptor(type_descriptor_t *td)
{
    if (!td) return type_new_primitive(TYPE_UNKNOWN);
    
    type_t *base_type = NULL;
    
    switch (td->type) {
        case DESC_VOID:    base_type = type_void(); break;
        case DESC_BOOLEAN: base_type = type_boolean(); break;
        case DESC_BYTE:    base_type = type_byte(); break;
        case DESC_CHAR:    base_type = type_char(); break;
        case DESC_SHORT:   base_type = type_short(); break;
        case DESC_INT:     base_type = type_int(); break;
        case DESC_LONG:    base_type = type_long(); break;
        case DESC_FLOAT:   base_type = type_float(); break;
        case DESC_DOUBLE:  base_type = type_double(); break;
        case DESC_OBJECT:
            if (td->class_name) {
                char *class_binary = classname_to_binary(td->class_name);
                base_type = type_new_class(class_binary);
                free(class_binary);
            } else {
                base_type = type_new_class("java.lang.Object");
            }
            break;
        case DESC_ARRAY:
            /* Shouldn't happen now - element type is preserved */
            base_type = type_new_class("java.lang.Object");
            break;
        default:
            base_type = type_new_primitive(TYPE_UNKNOWN);
            break;
    }
    
    /* Wrap in array type if needed */
    if (td->array_dimensions > 0 && base_type) {
        return type_new_array(base_type, td->array_dimensions);
    }
    
    return base_type;
}

/**
 * Create a symbol from a loaded class file.
 * This is used by codegen to load classes for method resolution.
 */
symbol_t *symbol_from_classfile(semantic_t *sem, classfile_t *cf)
{
    if (!cf || !cf->this_class_name) {
        return NULL;
    }
    
    /* Convert internal name to binary name */
    char *binary_name = classname_to_binary(cf->this_class_name);
    if (!binary_name) {
        return NULL;
    }
    
    /* Determine symbol kind */
    symbol_kind_t kind = SYM_CLASS;
    if (cf->access_flags & ACC_INTERFACE) {
        kind = SYM_INTERFACE;
    } else if (cf->access_flags & ACC_ENUM) {
        kind = SYM_ENUM;
    } else if (cf->access_flags & ACC_ANNOTATION) {
        kind = SYM_ANNOTATION;
    }
    
    
    /* Get simple name */
    char *simple_name = get_simple_name(binary_name);
    
    symbol_t *sym = symbol_new(kind, simple_name);
    free(simple_name);
    
    if (!sym) {
        free(binary_name);
        return NULL;
    }
    
    sym->qualified_name = binary_name;
    
    /* Convert access flags to our modifiers */
    uint32_t mods = 0;
    if (cf->access_flags & ACC_PUBLIC) mods |= MOD_PUBLIC;
    if (cf->access_flags & ACC_PRIVATE) mods |= MOD_PRIVATE;
    if (cf->access_flags & ACC_PROTECTED) mods |= MOD_PROTECTED;
    if (cf->access_flags & ACC_STATIC) mods |= MOD_STATIC;
    if (cf->access_flags & ACC_FINAL) mods |= MOD_FINAL;
    if (cf->access_flags & ACC_ABSTRACT) mods |= MOD_ABSTRACT;
    sym->modifiers = mods;
    
    /* Create class member scope */
    scope_t *class_scope = scope_new(SCOPE_CLASS, sem->global_scope);
    class_scope->owner = sym;
    sym->data.class_data.members = class_scope;
    
    /* Load class type parameters from Signature attribute */
    char *class_sig = classfile_get_attribute_signature(cf, cf->attributes, cf->attributes_count);
    class_signature_t *csig = NULL;
    if (class_sig) {
        csig = signature_parse_class(class_sig);
        if (csig && csig->type_params) {
            /* Create type_params list from parsed signature */
            for (type_parameter_t *tp = csig->type_params; tp; tp = tp->next) {
                symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp->name);
                if (tp_sym) {
                    /* Create TYPE_TYPEVAR for this parameter */
                    type_t *type_var = malloc(sizeof(type_t));
                    if (type_var) {
                        type_var->kind = TYPE_TYPEVAR;
                        type_var->data.type_var.name = strdup(tp->name);
                        type_var->data.type_var.bound = NULL;
                    }
                    tp_sym->type = type_var;
                    
                    /* Add to type_params list */
                    if (sym->data.class_data.type_params == NULL) {
                        sym->data.class_data.type_params = slist_new(tp_sym);
                    } else {
                        slist_append(sym->data.class_data.type_params, tp_sym);
                    }
                }
            }
        }
        /* Convert superclass from signature if available */
        if (csig && csig->superclass) {
            sym->data.class_data.superclass_type = generic_type_to_type(csig->superclass);
        }
        /* Free the parsed signature */
        if (csig) {
            class_signature_free(csig);
        }
        free(class_sig);
    }
    
    /* Load fields */
    int enum_ordinal = 0;  /* Track ordinal for enum constants */
    for (uint16_t i = 0; i < cf->fields_count; i++) {
        field_info_t *fi = &cf->fields[i];
        if (!fi->name) {
            continue;
        }
        
        symbol_t *field_sym = symbol_new(SYM_FIELD, fi->name);
        if (field_sym) {
            uint32_t field_mods = 0;
            if (fi->access_flags & ACC_PUBLIC) field_mods |= MOD_PUBLIC;
            if (fi->access_flags & ACC_PRIVATE) field_mods |= MOD_PRIVATE;
            if (fi->access_flags & ACC_PROTECTED) field_mods |= MOD_PROTECTED;
            if (fi->access_flags & ACC_STATIC) field_mods |= MOD_STATIC;
            if (fi->access_flags & ACC_FINAL) field_mods |= MOD_FINAL;
            field_sym->modifiers = field_mods;
            
            /* Check if this is an enum constant (ACC_ENUM flag) */
            if ((fi->access_flags & ACC_ENUM) && kind == SYM_ENUM) {
                field_sym->data.var_data.is_enum_constant = true;
                field_sym->data.var_data.enum_ordinal = enum_ordinal++;
            }
            
            /* Try to get generic signature first - this has the real generic types
             * (e.g., List<String> instead of just List) */
            char *field_sig = classfile_get_attribute_signature(cf, fi->attributes, fi->attributes_count);
            if (field_sig) {
                generic_type_t *gt = signature_parse_field(field_sig);
                if (gt) {
                    field_sym->type = generic_type_to_type(gt);
                    generic_type_free(gt);
                }
                free(field_sig);
            }
            
            /* If no signature or parsing failed, fall back to descriptor */
            if (!field_sym->type && fi->descriptor) {
                type_descriptor_t *td = descriptor_parse_field(fi->descriptor);
                if (td) {
                    field_sym->type = type_from_descriptor(td);
                    type_descriptor_free(td);
                }
            }
            
            scope_define(class_scope, field_sym);
        }
    }
    
    /* Load methods */
    for (uint16_t i = 0; i < cf->methods_count; i++) {
        method_info_t *mi = &cf->methods[i];
        if (!mi->name) {
            continue;
        }
        
        symbol_kind_t method_kind = SYM_METHOD;
        if (strcmp(mi->name, "<init>") == 0) {
            method_kind = SYM_CONSTRUCTOR;
        } else if (strcmp(mi->name, "<clinit>") == 0) {
            continue;  /* Skip class initializers */
        }
        
        /* Use method name with descriptor to uniquely identify each overload
         * This allows proper method overloading where multiple methods have the same name
         * but different parameter types */
        const char *desc = mi->descriptor ? mi->descriptor : "()V";
        size_t name_len = strlen(mi->name);
        size_t desc_len = strlen(desc);
        char *method_key = malloc(name_len + desc_len + 2);
        snprintf(method_key, name_len + desc_len + 2, "%s%s", mi->name, desc);
        
        symbol_t *method_sym = symbol_new(method_kind, mi->name);  /* Store actual name */
        if (method_sym) {
            uint32_t method_mods = 0;
            if (mi->access_flags & ACC_PUBLIC) method_mods |= MOD_PUBLIC;
            if (mi->access_flags & ACC_PRIVATE) method_mods |= MOD_PRIVATE;
            if (mi->access_flags & ACC_PROTECTED) method_mods |= MOD_PROTECTED;
            if (mi->access_flags & ACC_STATIC) method_mods |= MOD_STATIC;
            if (mi->access_flags & ACC_FINAL) method_mods |= MOD_FINAL;
            if (mi->access_flags & ACC_ABSTRACT) method_mods |= MOD_ABSTRACT;
            if (mi->access_flags & ACC_NATIVE) method_mods |= MOD_NATIVE;
            if (mi->access_flags & ACC_SYNCHRONIZED) method_mods |= MOD_SYNCHRONIZED;
            /* For interface methods, non-abstract non-static is a default method */
            if (kind == SYM_INTERFACE && 
                !(mi->access_flags & ACC_ABSTRACT) && 
                !(mi->access_flags & ACC_STATIC)) {
                method_mods |= MOD_DEFAULT;
            }
            method_sym->modifiers = method_mods;
            
            /* Store the method descriptor for use in codegen */
            if (mi->descriptor) {
                method_sym->data.method_data.descriptor = strdup(mi->descriptor);
            }
            
            /* Try to get generic signature - this has the real generic types */
            char *method_sig = classfile_get_attribute_signature(cf, mi->attributes, 
                                                                  mi->attributes_count);
            method_signature_t *msig = NULL;
            if (method_sig) {
                msig = signature_parse_method(method_sig);
                free(method_sig);
            }
            
            /* Parse types from signature (preferred) or descriptor (fallback) */
            if (msig) {
                /* Store method type parameters (e.g., <T> in <T> T foo(T arg)) */
                if (msig->type_params) {
                    for (type_parameter_t *tp = msig->type_params; tp; tp = tp->next) {
                        symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp->name);
                        if (tp_sym) {
                            type_t *type_var = malloc(sizeof(type_t));
                            if (type_var) {
                                type_var->kind = TYPE_TYPEVAR;
                                type_var->data.type_var.name = strdup(tp->name);
                                type_var->data.type_var.bound = NULL;
                                /* If there's a class bound, parse it */
                                if (tp->class_bound) {
                                    type_var->data.type_var.bound = generic_type_to_type(tp->class_bound);
                                }
                            }
                            tp_sym->type = type_var;
                            
                            if (method_sym->data.method_data.type_params == NULL) {
                                method_sym->data.method_data.type_params = slist_new(tp_sym);
                            } else {
                                slist_append(method_sym->data.method_data.type_params, tp_sym);
                            }
                        }
                    }
                }
                
                /* Use generic signature for return type and parameters */
                if (msig->return_type) {
                    method_sym->type = generic_type_to_type(msig->return_type);
                }
                
                /* Create parameter symbols from signature */
                generic_type_t *sig_param = msig->params;
                int param_idx = 0;
                while (sig_param) {
                    char param_name[16];
                    snprintf(param_name, sizeof(param_name), "arg%d", param_idx);
                    
                    symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
                    param_sym->type = generic_type_to_type(sig_param);
                    
                    slist_push(&method_sym->data.method_data.parameters, param_sym);
                    
                    sig_param = sig_param->next;
                    param_idx++;
                }
                
                method_signature_free(msig);
            } else if (mi->descriptor) {
                /* Fallback: parse erased descriptor */
                method_descriptor_t *md = descriptor_parse_method(mi->descriptor);
                if (md) {
                    /* Return type using helper - handles arrays correctly */
                    method_sym->type = type_from_descriptor(&md->return_type);
                    
                    /* Create parameter symbols */
                    for (int p = 0; p < md->param_count; p++) {
                        type_descriptor_t *param_desc = &md->params[p];
                        char param_name[16];
                        snprintf(param_name, sizeof(param_name), "arg%d", p);
                        
                        symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
                        param_sym->type = type_from_descriptor(param_desc);
                        
                        slist_push(&method_sym->data.method_data.parameters, param_sym);
                    }
                    
                    method_descriptor_free(md);
                }
            }
            
            /* Mark method and last parameter as varargs if method has ACC_VARARGS flag */
            if (mi->access_flags & ACC_VARARGS) {
                method_sym->modifiers |= MOD_VARARGS;
                slist_t *params = method_sym->data.method_data.parameters;
                if (params) {
                    /* Find last parameter */
                    while (params->next) {
                        params = params->next;
                    }
                    symbol_t *last_param = (symbol_t *)params->data;
                    if (last_param) {
                        last_param->modifiers |= MOD_VARARGS;
                    }
                }
            }
            
            /* Store with method_key to avoid field name collision */
            if (!hashtable_contains(class_scope->symbols, method_key)) {
                method_sym->scope = class_scope;
                hashtable_insert(class_scope->symbols, method_key, method_sym);
            }
        }
        free(method_key);
    }
    
    /* Load superclass if present */
    if (cf->super_class_name && strlen(cf->super_class_name) > 0) {
        char *super_binary = classname_to_binary(cf->super_class_name);
        if (super_binary) {
            /* Recursively load superclass */
            symbol_t *super_sym = load_external_class(sem, super_binary);
            if (super_sym) {
                sym->data.class_data.superclass = super_sym;
                /* If we have a parameterized superclass type from signature, populate its symbol */
                if (sym->data.class_data.superclass_type &&
                    sym->data.class_data.superclass_type->kind == TYPE_CLASS) {
                    sym->data.class_data.superclass_type->data.class_type.symbol = super_sym;
                }
            }
            free(super_binary);
        }
    }
    
    /* Load interfaces if present */
    if (cf->interfaces_count > 0 && cf->interface_names) {
        for (uint16_t i = 0; i < cf->interfaces_count; i++) {
            if (cf->interface_names[i]) {
                char *iface_binary = classname_to_binary(cf->interface_names[i]);
                if (iface_binary) {
                    symbol_t *iface_sym = load_external_class(sem, iface_binary);
                    if (iface_sym) {
                        if (!sym->data.class_data.interfaces) {
                            sym->data.class_data.interfaces = slist_new(iface_sym);
                        } else {
                            slist_append(sym->data.class_data.interfaces, iface_sym);
                        }
                    }
                    free(iface_binary);
                }
            }
        }
    }
    
    /* Create type for this class */
    type_t *type = type_new_class(sym->qualified_name);
    type->data.class_type.symbol = sym;
    sym->type = type;
    
    /* Cache the type */
    hashtable_insert(sem->types, sym->qualified_name, type);
    
    /* Also cache simple name for java.lang classes */
    if (strncmp(sym->qualified_name, "java.lang.", 10) == 0) {
        hashtable_insert(sem->types, sym->name, type);
    }
    
    /* Parse InnerClasses attribute to populate nested types.
     * This allows classes that implement interfaces to access nested types
     * defined in those interfaces (e.g., ChannelHandler.Type enum). */
    inner_class_info_t *inner_classes = classfile_get_inner_classes(cf);
    if (inner_classes) {
        for (inner_class_info_t *ic = inner_classes; ic; ic = ic->next) {
            /* Only process nested types that belong to this class */
            if (ic->outer_class_name && ic->inner_name &&
                strcmp(ic->outer_class_name, sym->qualified_name) == 0) {
                
                /* Check if already in members scope */
                if (scope_lookup_local(class_scope, ic->inner_name)) {
                    continue;
                }
                
                /* Determine the kind based on access flags */
                symbol_kind_t nested_kind = SYM_CLASS;
                if (ic->access_flags & ACC_INTERFACE) {
                    nested_kind = SYM_INTERFACE;
                } else if (ic->access_flags & ACC_ENUM) {
                    nested_kind = SYM_ENUM;
                } else if (ic->access_flags & ACC_ANNOTATION) {
                    nested_kind = SYM_ANNOTATION;
                }
                
                /* Try to fully load the nested type from its classfile.
                 * This is necessary to get enum constants, fields, and methods. 
                 * But first check if it's already cached to avoid infinite recursion. */
                symbol_t *nested_sym = NULL;
                
                /* Check if already cached in types table */
                type_t *cached_type = hashtable_lookup(sem->types, ic->inner_class_name);
                if (cached_type && cached_type->kind == TYPE_CLASS && 
                    cached_type->data.class_type.symbol) {
                    nested_sym = cached_type->data.class_type.symbol;
                } else if (sem->classpath) {
                    /* Load from classfile */
                    classfile_t *nested_cf = classpath_load_class(sem->classpath, ic->inner_class_name);
                    if (nested_cf) {
                        /* Recursively create symbol from classfile */
                        nested_sym = symbol_from_classfile(sem, nested_cf);
                        if (nested_sym) {
                            /* Set the enclosing class relationship */
                            nested_sym->data.class_data.enclosing_class = sym;
                            /* The symbol name from classfile is the binary name (e.g. ChannelHandler$Type).
                             * We need to use the simple name (e.g. Type) for scope lookup. */
                            if (ic->inner_name && strcmp(nested_sym->name, ic->inner_name) != 0) {
                                /* Names are interned, don't free - just reassign */
                                nested_sym->name = (char *)intern(ic->inner_name);
                            }
                        }
                    }
                }
                
                /* If classfile loading failed, try parsing the outer class source
                 * directly to find the nested type. This handles the case where
                 * the outer class exists as a classfile but the nested class
                 * hasn't been compiled yet.
                 * 
                 * We can't use load_class_from_source because it would return
                 * this same (partially-loaded) outer class from the cache. */
                if (!nested_sym && sem->sourcepath) {
                    if (getenv("GENESIS_DEBUG_LOAD")) {
                        fprintf(stderr, "DEBUG symbol_from_classfile: classfile loading failed for nested '%s', parsing outer source\n", ic->inner_class_name);
                    }
                    
                    /* Build source file path from outer class name */
                    char *file_path = strdup(sym->qualified_name);
                    for (char *p = file_path; *p; p++) {
                        if (*p == '.') *p = '/';
                    }
                    
                    for (slist_t *entry = sem->sourcepath; entry && !nested_sym; entry = entry->next) {
                        const char *dir = (const char *)entry->data;
                        char full_path[1024];
                        snprintf(full_path, sizeof(full_path), "%s/%s.java", dir, file_path);
                        
                        FILE *f = fopen(full_path, "r");
                        if (f) {
                            fclose(f);
                            
                            source_file_t *src = source_file_new(full_path);
                            if (src && source_file_load(src)) {
                                lexer_t *lexer = lexer_new(src, sem->source_version);
                                if (lexer) {
                                    parser_t *parser = parser_new(lexer, src);
                                    if (parser) {
                                        ast_node_t *ast = parser_parse(parser);
                                        if (ast && !parser->error_msg) {
                                            /* Find the nested type declaration in the AST */
                                            for (slist_t *child = ast->data.node.children; child; child = child->next) {
                                                ast_node_t *decl = (ast_node_t *)child->data;
                                                if (decl->type == AST_CLASS_DECL || decl->type == AST_INTERFACE_DECL ||
                                                    decl->type == AST_ENUM_DECL || decl->type == AST_RECORD_DECL) {
                                                    /* Search for nested type within this class */
                                                    for (slist_t *member = decl->data.node.children; member; member = member->next) {
                                                        ast_node_t *m = (ast_node_t *)member->data;
                                                        if ((m->type == AST_CLASS_DECL || m->type == AST_INTERFACE_DECL ||
                                                             m->type == AST_ENUM_DECL || m->type == AST_RECORD_DECL) &&
                                                            m->data.node.name && strcmp(m->data.node.name, ic->inner_name) == 0) {
                                                            /* Found it! Create symbol for this nested type */
                                                            nested_sym = symbol_new(nested_kind, ic->inner_name);
                                                            if (nested_sym) {
                                                                nested_sym->qualified_name = strdup(ic->inner_class_name);
                                                                nested_sym->modifiers = m->data.node.flags;
                                                                nested_sym->data.class_data.enclosing_class = sym;
                                                                
                                                                scope_t *nested_scope = scope_new(SCOPE_CLASS, class_scope);
                                                                nested_scope->owner = nested_sym;
                                                                nested_sym->data.class_data.members = nested_scope;
                                                                
                                                                type_t *nested_type = type_new_class(nested_sym->qualified_name);
                                                                nested_type->data.class_type.symbol = nested_sym;
                                                                nested_sym->type = nested_type;
                                                                
                                                                hashtable_insert(sem->types, nested_sym->qualified_name, nested_type);
                                                                /* Also cache by simple name for self-references */
                                                                hashtable_insert(sem->unit_types, ic->inner_name, nested_type);
                                                                
                                                                /* Set current_class for nested type resolution */
                                                                symbol_t *saved_class = sem->current_class;
                                                                sem->current_class = nested_sym;
                                                                scope_t *saved_scope = sem->current_scope;
                                                                sem->current_scope = nested_scope;
                                                                
                                                                /* Register methods and fields of this nested type */
                                                                for (slist_t *mc = m->data.node.children; mc; mc = mc->next) {
                                                                    ast_node_t *mm = (ast_node_t *)mc->data;
                                                                    if (!mm) continue;
                                                                    
                                                                    if (mm->type == AST_METHOD_DECL) {
                                                                        const char *mname = mm->data.node.name;
                                                                        if (mname) {
                                                                            symbol_t *method_sym = symbol_new(SYM_METHOD, mname);
                                                                            method_sym->modifiers = mm->data.node.flags;
                                                                            method_sym->ast = mm;
                                                                            
                                                                            /* Process method-level type parameters first */
                                                                            scope_t *method_scope = NULL;
                                                                            for (slist_t *tpc = mm->data.node.children; tpc; tpc = tpc->next) {
                                                                                ast_node_t *tp = (ast_node_t *)tpc->data;
                                                                                if (tp && tp->type == AST_TYPE_PARAMETER) {
                                                                                    if (!method_scope) {
                                                                                        method_scope = scope_new(SCOPE_METHOD, sem->current_scope);
                                                                                    }
                                                                                    const char *tp_name = tp->data.node.name;
                                                                                    if (tp_name) {
                                                                                        symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp_name);
                                                                                        type_t *type_var = malloc(sizeof(type_t));
                                                                                        if (type_var) {
                                                                                            type_var->kind = TYPE_TYPEVAR;
                                                                                            type_var->data.type_var.name = strdup(tp_name);
                                                                                            type_var->data.type_var.bound = NULL;
                                                                                        }
                                                                                        tp_sym->type = type_var;
                                                                                        scope_define(method_scope, tp_sym);
                                                                                    }
                                                                                }
                                                                            }
                                                                            
                                                                            /* Switch to method scope for type resolution */
                                                                            scope_t *outer_scope = sem->current_scope;
                                                                            if (method_scope) {
                                                                                sem->current_scope = method_scope;
                                                                            }
                                                                            
                                                                            /* Resolve return type - stored in data.node.extra */
                                                                            ast_node_t *ret_type_node = mm->data.node.extra;
                                                                            if (ret_type_node) {
                                                                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                                                                    fprintf(stderr, "DEBUG: resolving return type for method '%s', type_node=%d name='%s'\n",
                                                                                        mname, ret_type_node->type, ret_type_node->data.node.name ? ret_type_node->data.node.name : "(null)");
                                                                                }
                                                                                method_sym->type = semantic_resolve_type(sem, ret_type_node);
                                                                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                                                                    fprintf(stderr, "DEBUG: resolved return type for '%s' -> %p (kind=%d)\n",
                                                                                        mname, (void*)method_sym->type, method_sym->type ? method_sym->type->kind : -1);
                                                                                }
                                                                            }
                                                                            
                                                                            /* Process parameters */
                                                                            for (slist_t *pc = mm->data.node.children; pc; pc = pc->next) {
                                                                                ast_node_t *param = (ast_node_t *)pc->data;
                                                                                if (param && param->type == AST_PARAMETER) {
                                                                                    const char *pname = param->data.node.name;
                                                                                    symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname ? pname : "");
                                                                                    param_sym->ast = param;
                                                                                    if (param->data.node.children) {
                                                                                        ast_node_t *ptype_node = (ast_node_t *)param->data.node.children->data;
                                                                                        param_sym->type = semantic_resolve_type(sem, ptype_node);
                                                                                    }
                                                                                    if (param->data.node.flags & MOD_VARARGS) {
                                                                                        param_sym->modifiers |= MOD_VARARGS;
                                                                                        method_sym->modifiers |= MOD_VARARGS;
                                                                                    }
                                                                                    if (!method_sym->data.method_data.parameters) {
                                                                                        method_sym->data.method_data.parameters = slist_new(param_sym);
                                                                                    } else {
                                                                                        slist_append(method_sym->data.method_data.parameters, param_sym);
                                                                                    }
                                                                                }
                                                                            }
                                                                            
                                                                            /* Restore scope */
                                                                            sem->current_scope = outer_scope;
                                                                            scope_define(nested_scope, method_sym);
                                                                        }
                                                                    } else if (mm->type == AST_FIELD_DECL) {
                                                                        const char *fname = mm->data.node.name;
                                                                        if (fname) {
                                                                            symbol_t *field_sym = symbol_new(SYM_FIELD, fname);
                                                                            field_sym->modifiers = mm->data.node.flags;
                                                                            field_sym->ast = mm;
                                                                            if (mm->data.node.children) {
                                                                                ast_node_t *type_node = (ast_node_t *)mm->data.node.children->data;
                                                                                field_sym->type = semantic_resolve_type(sem, type_node);
                                                                            }
                                                                            scope_define(nested_scope, field_sym);
                                                                        }
                                                                    }
                                                                }
                                                                
                                                                /* Restore context */
                                                                sem->current_class = saved_class;
                                                                sem->current_scope = saved_scope;
                                                                
                                                                /* Also pre-register any nested types inside this nested type */
                                                                preregister_nested_types(sem, m, nested_sym, nested_scope);
                                                                
                                                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                                                    fprintf(stderr, "DEBUG symbol_from_classfile: created nested '%s' from source AST\n", ic->inner_class_name);
                                                                }
                                                            }
                                                            break;
                                                        }
                                                    }
                                                    break;
                                                }
                                            }
                                        }
                                        /* Note: Don't free ast - we've stored pointers to its children 
                                         * in symbols (param_sym->ast, field_sym->ast). The AST must outlive
                                         * the symbols that reference it. This leaks memory but avoids crashes.
                                         * TODO: Refactor to not store AST pointers in externally-loaded symbols. */
                                        /* ast_free(ast); */
                                        parser_free(parser);
                                    }
                                    lexer_free(lexer);
                                }
                            }
                            source_file_free(src);
                        }
                    }
                    free(file_path);
                }
                
                /* If source loading also failed, create a placeholder symbol */
                if (!nested_sym) {
                    nested_sym = symbol_new(nested_kind, ic->inner_name);
                    if (nested_sym) {
                        nested_sym->qualified_name = strdup(ic->inner_class_name);
                        
                        /* Convert access flags to modifiers */
                        uint32_t nested_mods = 0;
                        if (ic->access_flags & ACC_PUBLIC) {
                            nested_mods |= MOD_PUBLIC;
                        }
                        if (ic->access_flags & ACC_PRIVATE) {
                            nested_mods |= MOD_PRIVATE;
                        }
                        if (ic->access_flags & ACC_PROTECTED) {
                            nested_mods |= MOD_PROTECTED;
                        }
                        if (ic->access_flags & ACC_STATIC) {
                            nested_mods |= MOD_STATIC;
                        }
                        if (ic->access_flags & ACC_FINAL) {
                            nested_mods |= MOD_FINAL;
                        }
                        if (ic->access_flags & ACC_ABSTRACT) {
                            nested_mods |= MOD_ABSTRACT;
                        }
                        nested_sym->modifiers = nested_mods;
                        
                        /* Set enclosing class */
                        nested_sym->data.class_data.enclosing_class = sym;
                        
                        /* Create a member scope for nested type */
                        scope_t *nested_scope = scope_new(SCOPE_CLASS, class_scope);
                        nested_scope->owner = nested_sym;
                        nested_sym->data.class_data.members = nested_scope;
                        
                        /* Create type for nested symbol */
                        type_t *nested_type = type_new_class(nested_sym->qualified_name);
                        nested_type->data.class_type.symbol = nested_sym;
                        nested_sym->type = nested_type;
                        
                        /* Cache the nested type */
                        hashtable_insert(sem->types, nested_sym->qualified_name, nested_type);
                    }
                }
                
                /* Register in parent class's members */
                if (nested_sym) {
                    scope_define(class_scope, nested_sym);
                }
            }
        }
        inner_class_info_free(inner_classes);
    }
    
    return sym;
}

/**
 * Helper struct to track registered nested types for second pass.
 */
typedef struct nested_type_info {
    ast_node_t *ast;
    symbol_t *sym;
    scope_t *scope;
} nested_type_info_t;

/**
 * Recursively pre-register nested types in a class declaration AST.
 * This ensures that all nested classes, interfaces, enums, etc. are available
 * for type resolution before their declarations are fully processed.
 * 
 * This uses a two-pass approach:
 * Pass 1: Register all nested type names/symbols (no type resolution)
 * Pass 2: Populate methods/fields with resolved types (all siblings now visible)
 * 
 * @param sem The semantic analyzer
 * @param decl The AST node for the class/interface/enum declaration
 * @param parent_sym The parent class symbol
 * @param parent_scope The parent class's member scope
 */
static void preregister_nested_types(semantic_t *sem, ast_node_t *decl,
                                     symbol_t *parent_sym, scope_t *parent_scope)
{
    if (getenv("GENESIS_DEBUG_PREREGISTER")) {
        fprintf(stderr, "[PREREGISTER] Processing parent=%s with %d children\n",
                parent_sym->qualified_name, 
                (int)(decl->data.node.children ? slist_length(decl->data.node.children) : 0));
    }
    
    /* Collect nested types for second pass */
    slist_t *nested_infos = NULL;
    
    /* PASS 1: Register all nested type names/symbols first.
     * This ensures forward references between sibling nested types work. */
    for (slist_t *prescan = decl->data.node.children; prescan; prescan = prescan->next) {
        ast_node_t *nested = (ast_node_t *)prescan->data;
        if (!nested) continue;
        
        if (getenv("GENESIS_DEBUG_PREREGISTER")) {
            fprintf(stderr, "[PREREGISTER]   child type=%d name=%s\n",
                    nested->type, nested->data.node.name ? nested->data.node.name : "(null)");
        }
        
        if (nested->type == AST_CLASS_DECL ||
            nested->type == AST_INTERFACE_DECL ||
            nested->type == AST_ENUM_DECL ||
            nested->type == AST_RECORD_DECL ||
            nested->type == AST_ANNOTATION_DECL) {
            
            const char *nested_name = nested->data.node.name;
            if (!nested_name) continue;
            
            /* Check if already registered */
            symbol_t *existing = scope_lookup_local(parent_scope, nested_name);
            if (existing) {
                if (getenv("GENESIS_DEBUG_PREREGISTER")) {
                    fprintf(stderr, "[PREREGISTER]   Already registered: %s (scope=%p)\n", 
                        nested_name, (void*)parent_scope);
                }
                continue;
            }
            
            if (getenv("GENESIS_DEBUG_PREREGISTER")) {
                fprintf(stderr, "[PREREGISTER]   Not found, will register: %s in scope=%p\n", 
                    nested_name, (void*)parent_scope);
            }
            
            /* Create placeholder symbol for forward reference */
            symbol_kind_t nkind = SYM_CLASS;
            if (nested->type == AST_INTERFACE_DECL) nkind = SYM_INTERFACE;
            else if (nested->type == AST_ENUM_DECL) nkind = SYM_ENUM;
            else if (nested->type == AST_RECORD_DECL) nkind = SYM_RECORD;
            else if (nested->type == AST_ANNOTATION_DECL) nkind = SYM_ANNOTATION;
            
            symbol_t *nested_sym = symbol_new(nkind, nested_name);
            nested_sym->modifiers = nested->data.node.flags;
            nested_sym->ast = nested;
            
            /* Build qualified name */
            char nested_qname[512];
            snprintf(nested_qname, sizeof(nested_qname), "%s$%s",
                     parent_sym->qualified_name, nested_name);
            nested_sym->qualified_name = strdup(nested_qname);
            
            /* Set enclosing class for private access checks */
            nested_sym->data.class_data.enclosing_class = parent_sym;
            
            /* Create type */
            type_t *nested_type = type_new_class(nested_sym->qualified_name);
            nested_type->data.class_type.symbol = nested_sym;
            nested_sym->type = nested_type;
            
            /* Cache by qualified name in global cache,
             * by simple name in per-compilation-unit scope */
            hashtable_insert(sem->types, nested_sym->qualified_name, nested_type);
            hashtable_insert(sem->unit_types, nested_name, nested_type);
            
            /* Create member scope for nested type */
            scope_t *nested_scope = scope_new(SCOPE_CLASS, parent_scope);
            nested_scope->owner = nested_sym;
            nested_sym->data.class_data.members = nested_scope;
            
            /* Register in parent class scope */
            scope_define(parent_scope, nested_sym);
            
            if (getenv("GENESIS_DEBUG_PREREGISTER")) {
                fprintf(stderr, "[PREREGISTER]   Registered %s (kind=%d)\n",
                        nested_sym->qualified_name, nested_sym->kind);
            }
            
            /* Store symbol on AST for later lookup */
            nested->sem_symbol = nested_sym;
            
            /* Save for second pass */
            nested_type_info_t *info = malloc(sizeof(nested_type_info_t));
            info->ast = nested;
            info->sym = nested_sym;
            info->scope = nested_scope;
            if (!nested_infos) {
                nested_infos = slist_new(info);
            } else {
                slist_append(nested_infos, info);
            }
            
            /* Recursively pre-register nested types within this nested type
             * (depth-first to ensure all types at all levels are registered
             * before we start resolving types) */
            preregister_nested_types(sem, nested, nested_sym, nested_scope);
        }
    }
    
    /* PASS 2: Now that all sibling nested types are registered, populate methods/fields.
     * Type resolution can now find sibling types like HistogramSnapshot from HistogramBuckets. */
    for (slist_t *node = nested_infos; node; node = node->next) {
        nested_type_info_t *info = (nested_type_info_t *)node->data;
        ast_node_t *nested = info->ast;
        symbol_t *nested_sym = info->sym;
        scope_t *nested_scope = info->scope;
        type_t *nested_type = nested_sym->type;
        
        /* For nested enums, populate enum constants */
        if (nested->type == AST_ENUM_DECL) {
            int nested_ordinal = 0;
            for (slist_t *nc = nested->data.node.children; nc; nc = nc->next) {
                ast_node_t *nmember = (ast_node_t *)nc->data;
                if (nmember && nmember->type == AST_ENUM_CONSTANT) {
                    const char *ec_name = nmember->data.node.name;
                    if (ec_name) {
                        symbol_t *ec_sym = symbol_new(SYM_FIELD, ec_name);
                        ec_sym->modifiers = MOD_PUBLIC | MOD_STATIC | MOD_FINAL;
                        ec_sym->type = nested_type;
                        ec_sym->ast = nmember;
                        ec_sym->data.var_data.is_enum_constant = true;
                        ec_sym->data.var_data.enum_ordinal = nested_ordinal++;
                        scope_define(nested_scope, ec_sym);
                    }
                }
            }
        }
        
        /* Set current_class for type resolution */
        symbol_t *saved_class = sem->current_class;
        scope_t *saved_scope = sem->current_scope;
        sem->current_class = nested_sym;
        sem->current_scope = nested_scope;
        
        /* Pre-populate methods and fields for nested classes so static method
         * calls like NestedClass.method() can be resolved. */
        for (slist_t *nc = nested->data.node.children; nc; nc = nc->next) {
            ast_node_t *nmember = (ast_node_t *)nc->data;
            if (!nmember) {
                continue;
            }
            if (nmember->type == AST_METHOD_DECL) {
                const char *mname = nmember->data.node.name;
                if (mname) {
                    symbol_t *method_sym = symbol_new(SYM_METHOD, mname);
                    method_sym->modifiers = nmember->data.node.flags;
                    method_sym->ast = nmember;
                    
                    /* Process method-level type parameters first */
                    scope_t *method_scope = NULL;
                    for (slist_t *tpc = nmember->data.node.children; tpc; tpc = tpc->next) {
                        ast_node_t *tp = (ast_node_t *)tpc->data;
                        if (tp && tp->type == AST_TYPE_PARAMETER) {
                            if (!method_scope) {
                                method_scope = scope_new(SCOPE_METHOD, sem->current_scope);
                            }
                            const char *tp_name = tp->data.node.name;
                            if (tp_name) {
                                symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp_name);
                                type_t *type_var = malloc(sizeof(type_t));
                                if (type_var) {
                                    type_var->kind = TYPE_TYPEVAR;
                                    type_var->data.type_var.name = strdup(tp_name);
                                    type_var->data.type_var.bound = NULL;
                                }
                                tp_sym->type = type_var;
                                scope_define(method_scope, tp_sym);
                            }
                        }
                    }
                    
                    /* Switch to method scope for type resolution */
                    scope_t *outer_scope = sem->current_scope;
                    if (method_scope) {
                        sem->current_scope = method_scope;
                    }
                    
                    /* Resolve return type - stored in data.node.extra */
                    ast_node_t *ret_type_node = nmember->data.node.extra;
                    if (ret_type_node) {
                        method_sym->type = semantic_resolve_type(sem, ret_type_node);
                    }
                    /* Process parameters for proper overload resolution */
                    for (slist_t *pc = nmember->data.node.children; pc; pc = pc->next) {
                        ast_node_t *param = (ast_node_t *)pc->data;
                        if (param && param->type == AST_PARAMETER) {
                            const char *pname = param->data.node.name;
                            symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname ? pname : "");
                            param_sym->ast = param;
                            if (param->data.node.children) {
                                ast_node_t *ptype_node = (ast_node_t *)param->data.node.children->data;
                                param_sym->type = semantic_resolve_type(sem, ptype_node);
                            }
                            /* Check for varargs modifier */
                            if (param->data.node.flags & MOD_VARARGS) {
                                param_sym->modifiers |= MOD_VARARGS;
                                method_sym->modifiers |= MOD_VARARGS;
                            }
                            if (!method_sym->data.method_data.parameters) {
                                method_sym->data.method_data.parameters = slist_new(param_sym);
                            } else {
                                slist_append(method_sym->data.method_data.parameters, param_sym);
                            }
                        }
                    }
                    
                    /* Restore scope */
                    sem->current_scope = outer_scope;
                    scope_define(nested_scope, method_sym);
                }
            } else if (nmember->type == AST_FIELD_DECL) {
                const char *fname = nmember->data.node.name;
                if (fname) {
                    symbol_t *field_sym = symbol_new(SYM_FIELD, fname);
                    field_sym->modifiers = nmember->data.node.flags;
                    field_sym->ast = nmember;
                    /* Resolve type if possible */
                    slist_t *fchildren = nmember->data.node.children;
                    if (fchildren) {
                        ast_node_t *type_node = (ast_node_t *)fchildren->data;
                        if (type_node) {
                            field_sym->type = semantic_resolve_type(sem, type_node);
                        }
                    }
                    scope_define(nested_scope, field_sym);
                }
            }
        }
        
        /* Restore context */
        sem->current_class = saved_class;
        sem->current_scope = saved_scope;
        
        free(info);
    }
    
    slist_free(nested_infos);
}

/**
 * Try to load a class from a source file on the sourcepath.
 * Parses the source file and extracts class declarations.
 */
static symbol_t *load_class_from_source(semantic_t *sem, const char *name)
{
    if (!sem->sourcepath || !name) {
        return NULL;
    }
    
    /* First check the type cache to avoid reloading the same class multiple times */
    type_t *cached = hashtable_lookup(sem->types, name);
    if (cached && cached->kind == TYPE_CLASS && cached->data.class_type.symbol) {
        return cached->data.class_type.symbol;
    }
    
    /* Check if this is a nested class (contains $).
     * For nested classes, we need to load the outer class first,
     * then find the nested type in its members. */
    const char *dollar = strchr(name, '$');
    if (dollar) {
        /* Extract outer class name (before the $) */
        size_t outer_len = dollar - name;
        char *outer_name = malloc(outer_len + 1);
        strncpy(outer_name, name, outer_len);
        outer_name[outer_len] = '\0';
        
        if (getenv("GENESIS_DEBUG_LOAD")) {
            fprintf(stderr, "DEBUG load: nested type, loading outer '%s'\n", outer_name);
        }
        
        /* Load the outer class */
        symbol_t *outer = load_class_from_source(sem, outer_name);
        free(outer_name);
        
        if (!outer) {
            if (getenv("GENESIS_DEBUG_LOAD")) {
                fprintf(stderr, "DEBUG load: failed to load outer class\n");
            }
            return NULL;
        }
        
        /* Find the nested type in the outer class's members.
         * The nested name is everything after the $. */
        const char *nested_name = dollar + 1;
        
        /* Handle multiple levels of nesting (e.g., Outer$Inner$Deeper) */
        const char *next_dollar = strchr(nested_name, '$');
        char *immediate_name = next_dollar ? 
            strndup(nested_name, next_dollar - nested_name) : strdup(nested_name);
        
        if (getenv("GENESIS_DEBUG_LOAD")) {
            fprintf(stderr, "DEBUG load: looking for nested '%s' in '%s' (members=%p)\n",
                immediate_name, outer->name, (void*)outer->data.class_data.members);
        }
        
        if (outer->data.class_data.members) {
            symbol_t *nested = scope_lookup(outer->data.class_data.members, immediate_name);
            if (getenv("GENESIS_DEBUG_LOAD")) {
                fprintf(stderr, "DEBUG load: scope_lookup returned %p (kind=%d)\n",
                    (void*)nested, nested ? nested->kind : -1);
            }
            if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                           nested->kind == SYM_ENUM || nested->kind == SYM_RECORD ||
                           nested->kind == SYM_ANNOTATION)) {
                free(immediate_name);
                
                /* If there are more levels, continue walking the nested chain */
                if (next_dollar) {
                    /* Walk through remaining nested levels directly */
                    const char *remaining = next_dollar + 1;  /* Skip the $ */
                    while (remaining && *remaining) {
                        const char *next = strchr(remaining, '$');
                        char *part = next ? strndup(remaining, next - remaining) : strdup(remaining);
                        
                        /* Look for this part in nested's members */
                        symbol_t *deeper = NULL;
                        if (nested->data.class_data.members) {
                            deeper = scope_lookup(nested->data.class_data.members, part);
                        }
                        free(part);
                        
                        if (!deeper || (deeper->kind != SYM_CLASS && deeper->kind != SYM_INTERFACE &&
                                        deeper->kind != SYM_ENUM && deeper->kind != SYM_RECORD &&
                                        deeper->kind != SYM_ANNOTATION)) {
                            /* Not a nested type (might be an enum constant or field) */
                            return NULL;
                        }
                        
                        nested = deeper;
                        remaining = next ? next + 1 : NULL;
                    }
                }
                return nested;
            }
        }
        free(immediate_name);
        return NULL;
    }
    
    /* Convert class name to file path (com.example.Foo -> com/example/Foo.java) */
    char *file_path = strdup(name);
    for (char *p = file_path; *p; p++) {
        if (*p == '.') {
            *p = '/';
        }
    }
    
    /* Try each sourcepath entry */
    for (slist_t *entry = sem->sourcepath; entry; entry = entry->next) {
        const char *dir = (const char *)entry->data;
        
        /* Build full path: <dir>/<package/path/ClassName>.java */
        char full_path[1024];
        snprintf(full_path, sizeof(full_path), "%s/%s.java", dir, file_path);
        
        if (getenv("GENESIS_DEBUG_LOAD")) {
            fprintf(stderr, "DEBUG load_class_from_source: trying path '%s' for '%s'\n", full_path, name);
            fflush(stderr);
        }
        
        /* Check if file exists */
        FILE *f = fopen(full_path, "r");
        if (!f) {
            /* Try just the simple name (for default package) */
            const char *simple_name = strrchr(name, '.');
            simple_name = simple_name ? simple_name + 1 : name;
            snprintf(full_path, sizeof(full_path), "%s/%s.java", dir, simple_name);
            f = fopen(full_path, "r");
        }
        
        if (f) {
            fclose(f);
            
            /* Load and parse the source file */
            source_file_t *src = source_file_new(full_path);
            if (!src || !source_file_load(src)) {
                source_file_free(src);
                continue;
            }
            
            /* Lex and parse */
            lexer_t *lexer = lexer_new(src, sem->source_version);
            if (!lexer) {
                source_file_free(src);
                continue;
            }
            
            parser_t *parser = parser_new(lexer, src);
            if (!parser) {
                lexer_free(lexer);
                source_file_free(src);
                continue;
            }
            
            ast_node_t *ast = parser_parse(parser);
            bool had_parse_error = parser->error_msg != NULL;
            parser_free(parser);
            lexer_free(lexer);
            
            if (!ast || had_parse_error) {
                ast_free(ast);
                source_file_free(src);
                continue;
            }
            
            /* Resolve type names to qualified names immediately after parsing.
             * This ensures all type references use consistent qualified names
             * from the dependency file's own imports. */
            resolve_types_in_compilation_unit(ast, sem->classpath, sem->sourcepath);
            
            /* Extract the package from this dependency file and temporarily
             * switch to it so that type resolution uses the correct package
             * context (not the main file's package). */
            const char *dep_pkg = NULL;
            if (ast->type == AST_COMPILATION_UNIT) {
                for (slist_t *c = ast->data.node.children; c; c = c->next) {
                    ast_node_t *node = (ast_node_t *)c->data;
                    if (node->type == AST_PACKAGE_DECL && node->data.node.name) {
                        dep_pkg = node->data.node.name;
                        break;
                    }
                }
            }
            
            /* Save and switch context for dependency loading.
             * Set loading_dependency to suppress error reporting - errors will be
             * reported when the dependency is compiled directly. */
            char *saved_package = sem->current_package;
            source_file_t *saved_source = sem->source;
            bool saved_loading_dependency = sem->loading_dependency;
            sem->source = src;
            sem->loading_dependency = true;  /* Suppress errors during dependency loading */
            if (dep_pkg) {
                sem->current_package = strdup(dep_pkg);
            }
            
            /* Find the class declaration we're looking for */
            const char *simple_name = strrchr(name, '.');
            simple_name = simple_name ? simple_name + 1 : name;
            
            /* Walk the AST to find class declarations */
            if (ast->type == AST_COMPILATION_UNIT) {
                for (slist_t *child = ast->data.node.children; child; child = child->next) {
                    ast_node_t *decl = (ast_node_t *)child->data;
                    if ((decl->type == AST_CLASS_DECL || 
                         decl->type == AST_INTERFACE_DECL ||
                         decl->type == AST_ENUM_DECL ||
                         decl->type == AST_RECORD_DECL ||
                         decl->type == AST_ANNOTATION_DECL) &&
                        decl->data.node.name &&
                        strcmp(decl->data.node.name, simple_name) == 0) {
                        
                        /* Create symbol from AST */
                        symbol_kind_t kind = SYM_CLASS;
                        if (decl->type == AST_INTERFACE_DECL) {
                            kind = SYM_INTERFACE;
                        } else if (decl->type == AST_ENUM_DECL) {
                            kind = SYM_ENUM;
                        } else if (decl->type == AST_RECORD_DECL) {
                            kind = SYM_RECORD;
                        } else if (decl->type == AST_ANNOTATION_DECL) {
                            kind = SYM_ANNOTATION;
                        }
                        
                        symbol_t *sym = symbol_new(kind, simple_name);
                        sym->modifiers = decl->data.node.flags;
                        sym->qualified_name = strdup(name);
                        sym->ast = decl;
                        
                        /* Mark AST as pre-registered so main semantic pass won't create a duplicate */
                        decl->sem_symbol = sym;
                        
                        /* Create class member scope */
                        scope_t *class_scope = scope_new(SCOPE_CLASS, sem->global_scope);
                        class_scope->owner = sym;
                        sym->data.class_data.members = class_scope;
                        
                        /* Create type for this class and cache it BEFORE processing members
                         * to prevent infinite recursion when a member references this class */
                        type_t *type = type_new_class(sym->qualified_name);
                        type->data.class_type.symbol = sym;
                        sym->type = type;
                        if (getenv("GENESIS_DEBUG_LOAD")) {
                            fprintf(stderr, "DEBUG load_class_from_source: CACHING type for '%s'\n", sym->qualified_name);
                            fflush(stderr);
                        }
                        /* Cache by qualified name only in global cache.
                         * Don't cache by simple name - this is a dependency file,
                         * not the current compilation unit. */
                        hashtable_insert(sem->types, sym->qualified_name, type);
                        
                        /* Pre-register all nested class/interface declarations recursively
                         * so that forward references between siblings can be resolved.
                         * This ensures methods can reference nested classes defined later,
                         * and handles deeply nested types like Outer.Inner.Deeper. */
                        if (getenv("GENESIS_DEBUG_LOAD")) {
                            fprintf(stderr, "DEBUG load_class_from_source: about to preregister nested types for '%s' (members=%p)\n",
                                sym->qualified_name, (void*)class_scope);
                            fflush(stderr);
                        }
                        preregister_nested_types(sem, decl, sym, class_scope);
                        if (getenv("GENESIS_DEBUG_LOAD")) {
                            /* List what's in the members scope after preregistration */
                            fprintf(stderr, "DEBUG load_class_from_source: after preregister for '%s', members scope (count=%zu, size=%zu):\n", 
                                    sym->qualified_name,
                                    class_scope && class_scope->symbols ? class_scope->symbols->count : 0,
                                    class_scope && class_scope->symbols ? class_scope->symbols->size : 0);
                            if (class_scope && class_scope->symbols && class_scope->symbols->count > 0) {
                                /* Iterate over the hashtable */
                                for (size_t bi = 0; bi < class_scope->symbols->size; bi++) {
                                    for (hashtable_entry_t *e = class_scope->symbols->buckets[bi]; e; e = e->next) {
                                        symbol_t *m = (symbol_t *)e->value;
                                        fprintf(stderr, "  - %s (kind=%d)\n", m->name ? m->name : "(null)", m->kind);
                                    }
                                }
                            } else {
                                fprintf(stderr, "  (no symbols)\n");
                            }
                            fflush(stderr);
                        }
                        
                        /* Set current_class BEFORE processing extends/implements so that
                         * nested class type references (like "extends ParseContext" for nested
                         * classes inside this class) can be resolved properly */
                        symbol_t *saved_class_early = sem->current_class;
                        sem->current_class = sym;
                        
                        /* First, process class-level type parameters (e.g., <T> in class Foo<T>)
                         * These need to be registered BEFORE processing extends/implements/fields
                         * so that type parameters can be resolved in field/method types */
                        for (slist_t *child = decl->data.node.children; child; child = child->next) {
                            ast_node_t *c = (ast_node_t *)child->data;
                            if (c->type == AST_TYPE_PARAMETER) {
                                const char *tp_name = c->data.node.name;
                                if (tp_name) {
                                    /* Create a type parameter symbol */
                                    symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp_name);
                                    /* Create TYPE_TYPEVAR for this parameter */
                                    type_t *type_var = malloc(sizeof(type_t));
                                    if (type_var) {
                                        type_var->kind = TYPE_TYPEVAR;
                                        type_var->data.type_var.name = strdup(tp_name);
                                        type_var->data.type_var.bound = type_new_class("java.lang.Object");
                                    }
                                    tp_sym->type = type_var;
                                    scope_define(class_scope, tp_sym);
                                    
                                    /* Add to class's type_params list */
                                    if (!sym->data.class_data.type_params) {
                                        sym->data.class_data.type_params = slist_new(tp_sym);
                                    } else {
                                        slist_append(sym->data.class_data.type_params, tp_sym);
                                    }
                                    
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG load_class_from_source: registered class type param '%s' for '%s'\n",
                                            tp_name, sym->qualified_name);
                                    }
                                }
                            }
                        }
                        
                        /* Process extends and implements to set up superclass_type and interfaces */
                        for (slist_t *member = decl->data.node.children; member; member = member->next) {
                            ast_node_t *m = (ast_node_t *)member->data;
                            if (m->type == AST_CLASS_TYPE && m->data.node.flags == 1) {
                                /* This is an extends clause - for interfaces, extends adds to interfaces list
                                 * For classes, extends sets the superclass */
                                if (sym->kind == SYM_INTERFACE) {
                                    /* Interface extends other interfaces - add to interfaces list */
                                    const char *iface_name = m->data.node.name;
                                    if (getenv("GENESIS_DEBUG_IFACE")) {
                                        fprintf(stderr, "DEBUG load_class_from_source: interface %s extends %s\n",
                                            sym->name ? sym->name : "(null)", iface_name ? iface_name : "(null)");
                                    }
                                    symbol_t *iface_sym = NULL;
                                    /* Try direct lookup */
                                    iface_sym = scope_lookup(sem->current_scope, iface_name);
                                    if (!iface_sym) {
                                        /* Try import resolution */
                                        char *qualified = resolve_import(sem, iface_name);
                                        if (qualified) {
                                            iface_sym = load_external_class(sem, qualified);
                                            free(qualified);
                                        }
                                    }
                                    if (!iface_sym) {
                                        /* Try unqualified name directly */
                                        iface_sym = load_external_class(sem, iface_name);
                                    }
                                    if (iface_sym) {
                                        if (getenv("GENESIS_DEBUG_IFACE")) {
                                            fprintf(stderr, "DEBUG load_class_from_source: resolved interface '%s' -> %p (for interface extends)\n",
                                                iface_name, (void*)iface_sym);
                                        }
                                        /* Add to interfaces list */
                                        if (!sym->data.class_data.interfaces) {
                                            sym->data.class_data.interfaces = slist_new(iface_sym);
                                        } else {
                                            slist_append(sym->data.class_data.interfaces, iface_sym);
                                        }
                                    }
                                } else {
                                    /* Class extends a class - resolve the parameterized superclass type */
                                    type_t *super_type = semantic_resolve_type(sem, m);
                                    if (getenv("GENESIS_DEBUG_SUBST")) {
                                        fprintf(stderr, "DEBUG load_class_from_source: %s extends %s (type_args=%p)\n",
                                            sym->name ? sym->name : "(null)",
                                            super_type && super_type->kind == TYPE_CLASS && super_type->data.class_type.name ?
                                                super_type->data.class_type.name : "(null)",
                                            super_type && super_type->kind == TYPE_CLASS ? 
                                                (void*)super_type->data.class_type.type_args : NULL);
                                        if (super_type && super_type->kind == TYPE_CLASS && super_type->data.class_type.type_args) {
                                            for (slist_t *ta = super_type->data.class_type.type_args; ta; ta = ta->next) {
                                                type_t *arg = (type_t*)ta->data;
                                                fprintf(stderr, "  - extends type_arg: %s (kind=%d)\n",
                                                    arg && arg->kind == TYPE_CLASS && arg->data.class_type.name ? arg->data.class_type.name :
                                                    arg && arg->kind == TYPE_TYPEVAR && arg->data.type_var.name ? arg->data.type_var.name : "(unknown)",
                                                    arg ? arg->kind : -1);
                                            }
                                        }
                                    }
                                    if (super_type && super_type->kind == TYPE_CLASS) {
                                        symbol_t *super_sym = super_type->data.class_type.symbol;
                                        if (!super_sym && super_type->data.class_type.name) {
                                            super_sym = load_external_class(sem, super_type->data.class_type.name);
                                            if (super_sym) {
                                                super_type->data.class_type.symbol = super_sym;
                                            }
                                        }
                                        if (super_sym) {
                                            sym->data.class_data.superclass = super_sym;
                                            sym->data.class_data.superclass_type = super_type;
                                        }
                                    }
                                }
                            } else if (m->type == AST_CLASS_TYPE && m->data.node.flags == 2) {
                                /* This is an implements clause - add to interfaces list */
                                const char *iface_name = m->data.node.name;
                                if (getenv("GENESIS_DEBUG_IFACE")) {
                                    fprintf(stderr, "DEBUG load_class_from_source: %s implements %s\n",
                                        sym->name ? sym->name : "(null)", iface_name ? iface_name : "(null)");
                                }
                                symbol_t *iface_sym = NULL;
                                /* Try direct lookup */
                                iface_sym = scope_lookup(sem->current_scope, iface_name);
                                if (!iface_sym) {
                                    /* Try import resolution */
                                    char *qualified = resolve_import(sem, iface_name);
                                    if (qualified) {
                                        iface_sym = load_external_class(sem, qualified);
                                        free(qualified);
                                    }
                                }
                                if (!iface_sym) {
                                    /* Try unqualified name directly */
                                    iface_sym = load_external_class(sem, iface_name);
                                }
                                if (iface_sym) {
                                    if (getenv("GENESIS_DEBUG_IFACE")) {
                                        fprintf(stderr, "DEBUG load_class_from_source: resolved interface '%s' -> %p\n",
                                            iface_name, (void*)iface_sym);
                                    }
                                    /* Add to interfaces list */
                                    if (!sym->data.class_data.interfaces) {
                                        sym->data.class_data.interfaces = slist_new(iface_sym);
                                    } else {
                                        slist_append(sym->data.class_data.interfaces, iface_sym);
                                    }
                                }
                            }
                        }
                        
                        /* Process class body to extract members */
                        /* current_class was already set above before extends processing */
                        scope_t *saved_scope = sem->current_scope;
                        sem->current_scope = class_scope;
                        
                        int enum_ordinal_src = 0;  /* Track ordinal for enum constants */
                        for (slist_t *member = decl->data.node.children; member; member = member->next) {
                            ast_node_t *m = (ast_node_t *)member->data;
                            if (m->type == AST_METHOD_DECL || m->type == AST_CONSTRUCTOR_DECL) {
                                const char *mname = m->data.node.name;
                                symbol_kind_t mkind = m->type == AST_METHOD_DECL ? SYM_METHOD : SYM_CONSTRUCTOR;
                                symbol_t *method_sym = symbol_new(mkind, mname);
                                method_sym->modifiers = m->data.node.flags;
                                method_sym->ast = m;
                                
                                /* First, process method-level type parameters (e.g., <T> in <T> T foo()) 
                                 * These need to be registered before resolving return type and param types */
                                scope_t *method_scope = NULL;
                                for (slist_t *mc = m->data.node.children; mc; mc = mc->next) {
                                    ast_node_t *child = (ast_node_t *)mc->data;
                                    if (child->type == AST_TYPE_PARAMETER) {
                                        /* Create scope for method type parameters if needed */
                                        if (!method_scope) {
                                            method_scope = scope_new(SCOPE_METHOD, sem->current_scope);
                                        }
                                        const char *tp_name = child->data.node.name;
                                        if (tp_name) {
                                            symbol_t *tp_sym = symbol_new(SYM_TYPE_PARAM, tp_name);
                                            /* Create TYPE_TYPEVAR for this parameter */
                                            type_t *type_var = malloc(sizeof(type_t));
                                            if (type_var) {
                                                type_var->kind = TYPE_TYPEVAR;
                                                type_var->data.type_var.name = strdup(tp_name);
                                                type_var->data.type_var.bound = NULL;
                                            }
                                            tp_sym->type = type_var;
                                            scope_define(method_scope, tp_sym);
                                            /* Also add to method's type_params list */
                                            if (!method_sym->data.method_data.type_params) {
                                                method_sym->data.method_data.type_params = slist_new(tp_sym);
                                            } else {
                                                slist_append(method_sym->data.method_data.type_params, tp_sym);
                                            }
                                        }
                                    }
                                }
                                
                                /* Temporarily switch to method scope for type resolution */
                                scope_t *outer_scope = sem->current_scope;
                                if (method_scope) {
                                    sem->current_scope = method_scope;
                                }
                                
                                /* Get return type */
                                if (mkind == SYM_METHOD && m->data.node.extra) {
                                    method_sym->type = semantic_resolve_type(sem, m->data.node.extra);
                                }
                                
                                /* Process parameters for overload resolution */
                                for (slist_t *pchild = m->data.node.children; pchild; pchild = pchild->next) {
                                    ast_node_t *param = (ast_node_t *)pchild->data;
                                    if (param->type == AST_PARAMETER) {
                                        const char *pname = param->data.node.name;
                                        symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname ? pname : "");
                                        param_sym->ast = param;
                                        
                                        /* Get parameter type from first child */
                                        if (param->data.node.children) {
                                            ast_node_t *type_node = (ast_node_t *)param->data.node.children->data;
                                            param_sym->type = semantic_resolve_type(sem, type_node);
                                        }
                                        
                                        /* Check for varargs modifier */
                                        if (param->data.node.flags & MOD_VARARGS) {
                                            param_sym->modifiers |= MOD_VARARGS;
                                            method_sym->modifiers |= MOD_VARARGS;
                                        }
                                        
                                        if (!method_sym->data.method_data.parameters) {
                                            method_sym->data.method_data.parameters = slist_new(param_sym);
                                        } else {
                                            slist_append(method_sym->data.method_data.parameters, param_sym);
                                        }
                                    }
                                }
                                
                                /* Restore outer scope */
                                sem->current_scope = outer_scope;
                                
                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                    fprintf(stderr, "DEBUG load_class_from_source: adding method '%s' to '%s'\n",
                                        method_sym->name, sym->qualified_name);
                                }
                                scope_define(class_scope, method_sym);
                            } else if (m->type == AST_FIELD_DECL) {
                                /* Process field declarations */
                                type_t *field_type = NULL;
                                if (m->data.node.children) {
                                    ast_node_t *type_node = (ast_node_t *)m->data.node.children->data;
                                    field_type = semantic_resolve_type(sem, type_node);
                                }
                                
                                for (slist_t *fchild = m->data.node.children; fchild; fchild = fchild->next) {
                                    ast_node_t *var = (ast_node_t *)fchild->data;
                                    if (var->type == AST_VAR_DECLARATOR && var->data.node.name) {
                                        symbol_t *field_sym = symbol_new(SYM_FIELD, var->data.node.name);
                                        field_sym->modifiers = m->data.node.flags;
                                        field_sym->type = field_type;
                                        field_sym->ast = var;
                                        scope_define(class_scope, field_sym);
                                    }
                                }
                            } else if (m->type == AST_ENUM_CONSTANT) {
                                /* Enum constants are public static final fields of the enum type */
                                const char *const_name = m->data.node.name;
                                symbol_t *const_sym = symbol_new(SYM_FIELD, const_name);
                                const_sym->modifiers = MOD_PUBLIC | MOD_STATIC | MOD_FINAL;
                                const_sym->type = sym->type;  /* Type is the enum itself */
                                const_sym->ast = m;
                                const_sym->data.var_data.is_enum_constant = true;
                                const_sym->data.var_data.enum_ordinal = enum_ordinal_src++;
                                scope_define(class_scope, const_sym);
                            } else if (m->type == AST_INTERFACE_DECL || m->type == AST_CLASS_DECL ||
                                       m->type == AST_ENUM_DECL || m->type == AST_RECORD_DECL) {
                                /* Process nested type declarations - populate their methods and fields.
                                 * The nested type was already pre-registered, now we need to fill in 
                                 * its members for method lookup (e.g., for @Override checks). */
                                const char *nested_name = m->data.node.name;
                                if (nested_name) {
                                    /* Find the pre-registered symbol */
                                    symbol_t *nested_sym = scope_lookup_local(class_scope, nested_name);
                                    if (nested_sym && nested_sym->data.class_data.members) {
                                        scope_t *nested_scope = nested_sym->data.class_data.members;
                                        
                                        /* Set context for nested type */
                                        scope_t *outer_scope = sem->current_scope;
                                        symbol_t *outer_class = sem->current_class;
                                        sem->current_scope = nested_scope;
                                        sem->current_class = nested_sym;
                                        
                                        /* Process nested type's members */
                                        for (slist_t *nc = m->data.node.children; nc; nc = nc->next) {
                                            ast_node_t *nm = (ast_node_t *)nc->data;
                                            if (nm->type == AST_METHOD_DECL || nm->type == AST_CONSTRUCTOR_DECL) {
                                                const char *nmname = nm->data.node.name;
                                                symbol_kind_t nmkind = nm->type == AST_METHOD_DECL ? SYM_METHOD : SYM_CONSTRUCTOR;
                                                symbol_t *nmethod_sym = symbol_new(nmkind, nmname);
                                                nmethod_sym->modifiers = nm->data.node.flags;
                                                nmethod_sym->ast = nm;
                                                
                                                /* Get return type */
                                                if (nmkind == SYM_METHOD && nm->data.node.extra) {
                                                    nmethod_sym->type = semantic_resolve_type(sem, nm->data.node.extra);
                                                }
                                                
                                                /* Process parameters */
                                                for (slist_t *pc = nm->data.node.children; pc; pc = pc->next) {
                                                    ast_node_t *param = (ast_node_t *)pc->data;
                                                    if (param->type == AST_PARAMETER) {
                                                        const char *pname = param->data.node.name;
                                                        symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname ? pname : "");
                                                        param_sym->ast = param;
                                                        if (param->data.node.children) {
                                                            ast_node_t *ptype_node = (ast_node_t *)param->data.node.children->data;
                                                            param_sym->type = semantic_resolve_type(sem, ptype_node);
                                                        }
                                                        if (!nmethod_sym->data.method_data.parameters) {
                                                            nmethod_sym->data.method_data.parameters = slist_new(param_sym);
                                                        } else {
                                                            slist_append(nmethod_sym->data.method_data.parameters, param_sym);
                                                        }
                                                    }
                                                }
                                                
                                                scope_define(nested_scope, nmethod_sym);
                                            } else if (nm->type == AST_FIELD_DECL) {
                                                type_t *nfield_type = NULL;
                                                if (nm->data.node.children) {
                                                    ast_node_t *ntype_node = (ast_node_t *)nm->data.node.children->data;
                                                    nfield_type = semantic_resolve_type(sem, ntype_node);
                                                }
                                                for (slist_t *nfc = nm->data.node.children; nfc; nfc = nfc->next) {
                                                    ast_node_t *nvar = (ast_node_t *)nfc->data;
                                                    if (nvar->type == AST_VAR_DECLARATOR && nvar->data.node.name) {
                                                        symbol_t *nfield_sym = symbol_new(SYM_FIELD, nvar->data.node.name);
                                                        nfield_sym->modifiers = nm->data.node.flags;
                                                        nfield_sym->type = nfield_type;
                                                        nfield_sym->ast = nvar;
                                                        scope_define(nested_scope, nfield_sym);
                                                    }
                                                }
                                            }
                                        }
                                        
                                        /* Restore context */
                                        sem->current_scope = outer_scope;
                                        sem->current_class = outer_class;
                                    }
                                }
                            }
                        }
                        
                        /* Restore previous scope and class */
                        sem->current_scope = saved_scope;
                        sem->current_class = saved_class_early;
                        
                        /* Like javac's ClassFinder, also register OTHER top-level types
                         * in this file so they can be resolved later without re-parsing.
                         * This handles cases where a file contains multiple types, e.g.,
                         * AsyncTimeout.java containing AsyncTimeoutCallback interface. */
                        for (slist_t *other = ast->data.node.children; other; other = other->next) {
                            ast_node_t *other_decl = (ast_node_t *)other->data;
                            if ((other_decl->type == AST_CLASS_DECL || 
                                 other_decl->type == AST_INTERFACE_DECL ||
                                 other_decl->type == AST_ENUM_DECL ||
                                 other_decl->type == AST_RECORD_DECL ||
                                 other_decl->type == AST_ANNOTATION_DECL) &&
                                other_decl->data.node.name &&
                                strcmp(other_decl->data.node.name, simple_name) != 0) {
                                
                                /* Different type in the same file - register it */
                                const char *other_name = other_decl->data.node.name;
                                
                                /* Build qualified name using same package as requested class.
                                 * The 'name' variable is the full qualified name like
                                 * 'org.bluezoo.gumdrop.servlet.AsyncTimeout', and simple_name
                                 * is 'AsyncTimeout'. The package is everything before. */
                                char other_qualified[512];
                                const char *last_dot = strrchr(name, '.');
                                if (last_dot) {
                                    size_t pkg_len = (size_t)(last_dot - name);
                                    snprintf(other_qualified, sizeof(other_qualified), 
                                             "%.*s.%s", (int)pkg_len, name, other_name);
                                } else {
                                    strncpy(other_qualified, other_name, sizeof(other_qualified) - 1);
                                    other_qualified[sizeof(other_qualified) - 1] = '\0';
                                }
                                
                                /* Check if already registered */
                                type_t *existing = hashtable_lookup(sem->types, other_qualified);
                                if (!existing) {
                                    /* Create a placeholder symbol */
                                    symbol_kind_t other_kind = SYM_CLASS;
                                    if (other_decl->type == AST_INTERFACE_DECL) {
                                        other_kind = SYM_INTERFACE;
                                    } else if (other_decl->type == AST_ENUM_DECL) {
                                        other_kind = SYM_ENUM;
                                    } else if (other_decl->type == AST_RECORD_DECL) {
                                        other_kind = SYM_RECORD;
                                    } else if (other_decl->type == AST_ANNOTATION_DECL) {
                                        other_kind = SYM_ANNOTATION;
                                    }
                                    
                                    symbol_t *other_sym = symbol_new(other_kind, other_name);
                                    other_sym->modifiers = other_decl->data.node.flags;
                                    other_sym->qualified_name = strdup(other_qualified);
                                    other_sym->ast = other_decl;
                                    other_decl->sem_symbol = other_sym;
                                    
                                    /* Create member scope */
                                    scope_t *other_scope = scope_new(SCOPE_CLASS, sem->global_scope);
                                    other_scope->owner = other_sym;
                                    other_sym->data.class_data.members = other_scope;
                                    
                                    /* Create and cache type by qualified name only
                                     * (this is a dependency file, not current unit) */
                                    type_t *other_type = type_new_class(other_qualified);
                                    other_type->data.class_type.symbol = other_sym;
                                    other_sym->type = other_type;
                                    
                                    hashtable_insert(sem->types, other_qualified, other_type);
                                    
                                    if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
                                        fprintf(stderr, "DEBUG load_class_from_source: also registered '%s' from same file\n", 
                                                other_qualified);
                                    }
                                }
                            }
                        }
                        
                        /* Track this source file as a dependency that needs compilation.
                         * We don't call compile_dependency here because that would create
                         * a fresh semantic analyzer with an empty types cache. Instead,
                         * we track the dependency and compile it after the main file's
                         * semantic analysis is complete. */
                        char *dep_path = strdup(src->filename);
                        sem->source_dependencies = slist_append(sem->source_dependencies, dep_path);
                        
                        /* Restore context */
                        if (dep_pkg) {
                            free(sem->current_package);
                            sem->current_package = saved_package;
                        }
                        sem->source = saved_source;
                        sem->loading_dependency = saved_loading_dependency;
                        
                        /* Don't free ast - it's now referenced by the symbol */
                        /* Note: Don't free src either since sym->ast references nodes with filenames from src */
                        free(file_path);
                        return sym;
                    }
                }
                
                /* Even if we didn't find the exact class requested, register ALL
                 * types in this file. This handles cases like AsyncTimeout.java
                 * which contains AsyncTimeoutCallback and AsyncTimeoutHandle but
                 * no class named AsyncTimeout. */
                for (slist_t *all = ast->data.node.children; all; all = all->next) {
                    ast_node_t *all_decl = (ast_node_t *)all->data;
                    if ((all_decl->type == AST_CLASS_DECL || 
                         all_decl->type == AST_INTERFACE_DECL ||
                         all_decl->type == AST_ENUM_DECL ||
                         all_decl->type == AST_RECORD_DECL ||
                         all_decl->type == AST_ANNOTATION_DECL) &&
                        all_decl->data.node.name) {
                        
                        const char *decl_name = all_decl->data.node.name;
                        
                        /* Build qualified name */
                        char all_qualified[512];
                        const char *last_dot = strrchr(name, '.');
                        if (last_dot) {
                            size_t pkg_len = (size_t)(last_dot - name);
                            snprintf(all_qualified, sizeof(all_qualified), 
                                     "%.*s.%s", (int)pkg_len, name, decl_name);
                        } else {
                            /* Get package from AST if name has no dot */
                            ast_node_t *pkg_node = NULL;
                            for (slist_t *c = ast->data.node.children; c; c = c->next) {
                                ast_node_t *node = (ast_node_t *)c->data;
                                if (node->type == AST_PACKAGE_DECL) {
                                    pkg_node = node;
                                    break;
                                }
                            }
                            if (pkg_node && pkg_node->data.node.name) {
                                snprintf(all_qualified, sizeof(all_qualified), 
                                         "%s.%s", pkg_node->data.node.name, decl_name);
                            } else {
                                strncpy(all_qualified, decl_name, sizeof(all_qualified) - 1);
                                all_qualified[sizeof(all_qualified) - 1] = '\0';
                            }
                        }
                        
                        /* Check if already registered */
                        type_t *existing = hashtable_lookup(sem->types, all_qualified);
                        if (!existing) {
                            /* Create a placeholder symbol */
                            symbol_kind_t all_kind = SYM_CLASS;
                            if (all_decl->type == AST_INTERFACE_DECL) {
                                all_kind = SYM_INTERFACE;
                            } else if (all_decl->type == AST_ENUM_DECL) {
                                all_kind = SYM_ENUM;
                            } else if (all_decl->type == AST_RECORD_DECL) {
                                all_kind = SYM_RECORD;
                            } else if (all_decl->type == AST_ANNOTATION_DECL) {
                                all_kind = SYM_ANNOTATION;
                            }
                            
                            symbol_t *all_sym = symbol_new(all_kind, decl_name);
                            all_sym->modifiers = all_decl->data.node.flags;
                            all_sym->qualified_name = strdup(all_qualified);
                            all_sym->ast = all_decl;
                            all_decl->sem_symbol = all_sym;
                            
                            /* Create member scope */
                            scope_t *all_scope = scope_new(SCOPE_CLASS, sem->global_scope);
                            all_scope->owner = all_sym;
                            all_sym->data.class_data.members = all_scope;
                            
                            /* Create and cache type by qualified name only
                             * (this is a dependency file, not current unit) */
                            type_t *all_type = type_new_class(all_qualified);
                            all_type->data.class_type.symbol = all_sym;
                            all_sym->type = all_type;
                            
                            hashtable_insert(sem->types, all_qualified, all_type);
                            
                            /* Also populate methods for @Override checks */
                            for (slist_t *mc = all_decl->data.node.children; mc; mc = mc->next) {
                                ast_node_t *member = (ast_node_t *)mc->data;
                                if (member->type == AST_METHOD_DECL) {
                                    const char *method_name = member->data.node.name;
                                    symbol_t *method_sym = symbol_new(SYM_METHOD, method_name);
                                    method_sym->modifiers = member->data.node.flags;
                                    method_sym->ast = member;
                                    
                                    /* Interface methods are implicitly public (and abstract unless default/static/private) */
                                    if (all_kind == SYM_INTERFACE) {
                                        bool is_private = (method_sym->modifiers & MOD_PRIVATE) != 0;
                                        bool is_static = (method_sym->modifiers & MOD_STATIC) != 0;
                                        bool is_default = (method_sym->modifiers & MOD_DEFAULT) != 0;
                                        if (!is_private) method_sym->modifiers |= MOD_PUBLIC;
                                        if (!is_default && !is_static && !is_private) method_sym->modifiers |= MOD_ABSTRACT;
                                    } else if (all_kind == SYM_ANNOTATION) {
                                        method_sym->modifiers |= MOD_PUBLIC | MOD_ABSTRACT;
                                    }
                                    
                                    /* Get return type */
                                    if (member->data.node.extra) {
                                        method_sym->type = semantic_resolve_type(sem, member->data.node.extra);
                                    }
                                    
                                    /* Process parameters */
                                    for (slist_t *pc = member->data.node.children; pc; pc = pc->next) {
                                        ast_node_t *param = (ast_node_t *)pc->data;
                                        if (param->type == AST_PARAMETER) {
                                            const char *pname = param->data.node.name;
                                            symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname ? pname : "");
                                            param_sym->ast = param;
                                            if (param->data.node.children) {
                                                ast_node_t *ptype_node = (ast_node_t *)param->data.node.children->data;
                                                param_sym->type = semantic_resolve_type(sem, ptype_node);
                                            }
                                            if (!method_sym->data.method_data.parameters) {
                                                method_sym->data.method_data.parameters = slist_new(param_sym);
                                            } else {
                                                slist_append(method_sym->data.method_data.parameters, param_sym);
                                            }
                                        }
                                    }
                                    
                                    scope_define(all_scope, method_sym);
                                }
                            }
                            
                            if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
                                fprintf(stderr, "DEBUG load_class_from_source: registered '%s' with methods (type not found in file)\n", 
                                        all_qualified);
                            }
                            
                            /* Track this source file as a dependency */
                            char *dep_path = strdup(src->filename);
                            sem->source_dependencies = slist_append(sem->source_dependencies, dep_path);
                        }
                    }
                }
            }
            
            /* Restore context */
            if (dep_pkg) {
                free(sem->current_package);
                sem->current_package = saved_package;
            }
            sem->source = saved_source;
            sem->loading_dependency = saved_loading_dependency;
            
            /* Don't free ast if we registered types from it */
            source_file_free(src);
        }
    }
    
    free(file_path);
        return NULL;
    }
    
/**
 * Try to load a class from the classpath or sourcepath.
 * 
 * For nested classes, we need to handle the ambiguity between package separators
 * and nested class separators. We use a strategy similar to javac's classCandidates:
 * generate all possible interpretations of where the package ends and class begins.
 * 
 * For example, "pkg.Outer.Inner" could be:
 *   - pkg/Outer/Inner.class  (Outer and Inner are packages - unlikely but valid)
 *   - pkg/Outer$Inner.class  (pkg.Outer is package, Outer$Inner is class)
 *   - pkg$Outer$Inner.class  (pkg is package, Outer$Inner is class - unlikely)
 * 
 * We try progressively from right to left, converting dots to $ until we find a match.
 * Unlike the previous implementation, we do NOT rely on capitalization conventions
 * since class names can be lowercase (e.g., javax.servlet.http.HttpSession where
 * 'http' is a package, not a class).
 */
symbol_t *load_external_class(semantic_t *sem, const char *name)
{
    /* First check the type cache to avoid reloading the same class multiple times */
    type_t *cached = hashtable_lookup(sem->types, name);
    if (cached && cached->kind == TYPE_CLASS && cached->data.class_type.symbol) {
        if (getenv("GENESIS_DEBUG_LOAD")) {
            fprintf(stderr, "DEBUG load_external_class: cache hit for '%s' -> sym=%p members=%p\n",
                name, (void*)cached->data.class_type.symbol,
                (void*)cached->data.class_type.symbol->data.class_data.members);
        }
        return cached->data.class_type.symbol;
    }
    if (getenv("GENESIS_DEBUG_LOAD")) {
        fprintf(stderr, "DEBUG load_external_class: cache miss for '%s'\n", name);
    }
    
    /* Try the exact name on classpath (works for top-level classes) */
    if (sem->classpath) {
        classfile_t *cf = classpath_load_class(sem->classpath, name);
        if (cf) {
            if (getenv("GENESIS_DEBUG_LOAD")) {
                fprintf(stderr, "DEBUG load_external_class: loaded '%s' from classpath\n", name);
            }
            return symbol_from_classfile(sem, cf);
        }
    }
    
    /* Then try sourcepath (.java files) */
    if (getenv("GENESIS_DEBUG_LOAD")) {
        fprintf(stderr, "DEBUG load_external_class: calling load_class_from_source for '%s' (sourcepath=%p)\n", 
                name, (void*)sem->sourcepath);
    }
    symbol_t *sym = load_class_from_source(sem, name);
    if (getenv("GENESIS_DEBUG_LOAD")) {
        fprintf(stderr, "DEBUG load_external_class: load_class_from_source returned %p for '%s'\n", 
                (void*)sym, name);
    }
    if (sym) {
        return sym;
    }
    
    /* Generate class candidates by trying each dot position as a potential
     * class/nested-class boundary. Unlike javac's classCandidates which builds
     * a list, we try each candidate immediately.
     * 
     * For "a.b.c.D.E.F", we try:
     *   a.b.c.D.E$F       (E$F is a nested class in package a.b.c.D)
     *   a.b.c.D$E$F       (D$E$F is a nested class in package a.b.c)
     *   a.b.c$D$E$F       (c$D$E$F is a nested class in package a.b)
     *   a.b$c$D$E$F       etc.
     *   a$b$c$D$E$F       (b$c$D$E$F is a nested class in package a)
     */
    size_t len = strlen(name);
    char *candidate = strdup(name);
    
    /* Find the rightmost dot and work backwards */
    char *p = candidate + len - 1;
    
    while (p > candidate) {
        /* Find the previous dot */
        while (p > candidate && *p != '.') {
            p--;
        }
        
        if (*p == '.') {
            /* Convert this dot to $ */
            *p = '$';
            
            /* Try classpath with this candidate */
            if (sem->classpath) {
                classfile_t *cf = classpath_load_class(sem->classpath, candidate);
                if (cf) {
                    symbol_t *result = symbol_from_classfile(sem, cf);
                    if (result && result->type) {
                        /* Also cache under the original name for future lookups */
                        hashtable_insert(sem->types, strdup(name), result->type);
                    }
                    free(candidate);
                    return result;
                }
            }
            
            /* Try sourcepath with this candidate */
            sym = load_class_from_source(sem, candidate);
            if (sym) {
                if (sym->type) {
                    /* Also cache under the original name for future lookups */
                    hashtable_insert(sem->types, strdup(name), sym->type);
                }
                free(candidate);
                return sym;
            }
            
            /* Move to the next position (the dot is now $ so just decrement) */
            p--;
        }
    }
    
    free(candidate);
    return NULL;
}

/**
 * Ensure a class type has its symbol loaded for proper subtype checking.
 * This is needed when types are created from descriptors without full class info.
 */
static void ensure_type_symbol_loaded(semantic_t *sem, type_t *type)
{
    if (!type || type->kind != TYPE_CLASS) return;
    if (type->data.class_type.symbol) return;  /* Already has symbol */
    
    const char *class_name = type->data.class_type.name;
    if (!class_name) return;
    
    symbol_t *sym = load_external_class(sem, class_name);
    if (sym && sym->type) {
        type->data.class_type.symbol = sym;
    }
}

/**
 * Resolve a simple class name to a fully qualified name using imports.
 * Returns a newly allocated string or NULL if not found.
 * 
 * Import resolution order:
 * 1. Single-type imports (import java.util.List;)
 * 2. On-demand imports (import java.util.*;)
 * 3. java.lang.* (implicit)
 */
static char *resolve_import(semantic_t *sem, const char *simple_name)
{
    if (!simple_name) {
        return NULL;
    }
    
    /* Check cache first - returns strdup'd copy for API compatibility */
    const char *cached = hashtable_lookup(sem->resolved_imports, simple_name);
    if (cached) {
        return strdup(cached);
    }
    
    /* Check if this is a qualified inner class name like "Map.Entry" */
    const char *dot = strchr(simple_name, '.');
    if (dot != NULL) {
        /* Split on first dot - e.g., "Map.Entry" → "Map" and "Entry" */
        size_t outer_len = dot - simple_name;
        char *outer_name = strndup(simple_name, outer_len);
        const char *inner_part = dot + 1;  /* "Entry" or "Entry.Foo" etc. */
        
        /* Recursively resolve the outer name */
        char *qualified_outer = resolve_import(sem, outer_name);
        free(outer_name);
        
        if (qualified_outer) {
            /* Build qualified name using $ for inner class: "java.util.Map$Entry" */
            size_t qo_len = strlen(qualified_outer);
            size_t result_len = qo_len + 1 + strlen(inner_part) + 1;
            char *result = malloc(result_len);
            snprintf(result, result_len, "%s$%s", qualified_outer, inner_part);
            free(qualified_outer);
            
            /* Replace any remaining dots in inner_part with $ (for nested inner classes) */
            for (char *p = result + qo_len + 1; *p; p++) {
                if (*p == '.') {
                    *p = '$';
                }
            }
            
            /* Cache the result */
            hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(result));
            return result;
        }
        
        /* Couldn't resolve outer name - return original */
        return strdup(simple_name);
    }
    
    /* Check single-type imports first */
    for (slist_t *node = sem->imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Check if it's a wildcard import (ends with .*) */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            continue;  /* Handle on-demand imports later */
        }
        
        /* Single-type import: check if simple name matches */
        const char *last_dot = strrchr(import_name, '.');
        const char *import_simple = last_dot ? last_dot + 1 : import_name;
        
        if (strcmp(import_simple, simple_name) == 0) {
            /* Cache and return */
            hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(import_name));
            return strdup(import_name);
        }
    }
    
    /* Check if simple_name is a nested type in any single-type-imported class
     * e.g., "import javax.tools.JavaFileManager;" allows using "Location" 
     * to refer to JavaFileManager.Location */
    for (slist_t *node = sem->imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Skip wildcard imports */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            continue;
        }
        
        /* Try loading the imported class and check for a nested type */
        symbol_t *imported_sym = load_external_class(sem, import_name);
        if (imported_sym && imported_sym->data.class_data.members) {
            symbol_t *nested = scope_lookup_local(imported_sym->data.class_data.members, simple_name);
            if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                           nested->kind == SYM_ENUM || nested->kind == SYM_RECORD)) {
                /* Found nested type - return qualified name with $ separator */
                size_t result_len = strlen(import_name) + 1 + strlen(simple_name) + 1;
                char *result = malloc(result_len);
                snprintf(result, result_len, "%s$%s", import_name, simple_name);
                /* Cache and return */
                hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(result));
                return result;
            }
        }
    }
    
    /* Check on-demand imports */
    for (slist_t *node = sem->imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Check if it's a wildcard import (ends with .*) */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            /* Try package.SimpleName */
            char qualified[512];
            snprintf(qualified, sizeof(qualified), "%.*s%s", 
                     (int)(len - 1), import_name, simple_name);
            
            /* Try to find the class in classpath */
            if (sem->classpath) {
                classfile_t *cf = classpath_load_class(sem->classpath, qualified);
                if (cf) {
                    /* Don't free cf - it's owned by the classpath cache */
                    /* Cache and return */
                    hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(qualified));
                    return strdup(qualified);
                }
            }
        }
    }
    
    /* Try same-package resolution (implicit import of current package) */
    if (sem->current_package) {
        char same_package[512];
        snprintf(same_package, sizeof(same_package), "%s.%s", 
                 sem->current_package, simple_name);
        
        /* Try classpath first */
        if (sem->classpath) {
            classfile_t *cf = classpath_load_class(sem->classpath, same_package);
            if (cf) {
                /* Cache and return */
                hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(same_package));
                return strdup(same_package);
            }
        }
        
        /* Try sourcepath */
        if (sem->sourcepath) {
            symbol_t *src_sym = load_class_from_source(sem, same_package);
            if (src_sym) {
                /* Cache and return */
                hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(same_package));
                return strdup(same_package);
            }
        }
    }
    
    /* Try java.lang.* (implicit import) */
    char java_lang[256];
    snprintf(java_lang, sizeof(java_lang), "java.lang.%s", simple_name);
    if (sem->classpath) {
        classfile_t *cf = classpath_load_class(sem->classpath, java_lang);
        if (cf) {
            /* Don't free cf - it's owned by the classpath cache */
            /* Cache and return */
            hashtable_insert(sem->resolved_imports, simple_name, (void *)intern(java_lang));
            return strdup(java_lang);
        }
    }
    
    return NULL;
}

/* ========================================================================
 * Early Type Resolution (Post-Parse Pass)
 * ========================================================================
 * These functions resolve type names to fully qualified names immediately
 * after parsing, before the AST is used for any other purpose.
 * This ensures type names are stored consistently throughout the AST.
 */

/**
 * Check if a class exists on the sourcepath by looking for its .java file.
 * This is a simple existence check without parsing.
 * 
 * @param qualified_name  Fully qualified class name (e.g., "org.bluezoo.Foo")
 * @param sourcepath_list List of sourcepath directory strings, or NULL
 */
static bool class_exists_on_sourcepath(const char *qualified_name, slist_t *sourcepath_list)
{
    if (!qualified_name || !sourcepath_list) {
        return false;
    }
    
    /* Build path from qualified name (e.g., "org.bluezoo.Foo" -> "org/bluezoo/Foo.java") */
    char path[1024];
    size_t len = strlen(qualified_name);
    if (len >= sizeof(path) - 10) {
        return false;
    }
    
    /* Convert dots to path separators */
    char class_path[512];
    strncpy(class_path, qualified_name, sizeof(class_path) - 1);
    class_path[sizeof(class_path) - 1] = '\0';
    for (char *p = class_path; *p; p++) {
        if (*p == '.') *p = '/';
        if (*p == '$') *p = '/';  /* Inner classes might be separate files or not */
    }
    
    /* Try each sourcepath entry */
    for (slist_t *entry = sourcepath_list; entry; entry = entry->next) {
        const char *dir = (const char *)entry->data;
        snprintf(path, sizeof(path), "%s/%s.java", dir, class_path);
        FILE *f = fopen(path, "r");
        if (f) {
            fclose(f);
            return true;
        }
    }
    
    return false;
}

/**
 * Resolve a simple type name to a fully qualified name using imports.
 * This is a standalone function that doesn't require a full semantic_t.
 * 
 * @param simple_name     Simple type name (e.g., "Map", "List")
 * @param imports         List of AST_IMPORT_DECL nodes from the file
 * @param package         Current package name (may be NULL)
 * @param classpath       Classpath for verifying qualified names
 * @param sourcepath_list List of sourcepath directories for verifying source files
 * @return Newly allocated qualified name, or NULL if not resolved
 */
static char *resolve_type_name_with_imports(const char *simple_name, 
                                            slist_t *imports,
                                            const char *package,
                                            classpath_t *classpath,
                                            slist_t *sourcepath_list)
{
    if (!simple_name) {
        return NULL;
    }
    
    /* If contains a dot, it might be a qualified name like "Outer.Inner".
     * We need to resolve the first component (Outer) via imports,
     * then append the rest (.Inner). */
    const char *dot = strchr(simple_name, '.');
    if (dot) {
        /* Extract the first component */
        size_t first_len = dot - simple_name;
        char *first_part = malloc(first_len + 1);
        strncpy(first_part, simple_name, first_len);
        first_part[first_len] = '\0';
        
        /* Try to resolve just the first part */
        char *resolved_first = resolve_type_name_with_imports(first_part, imports, 
            package, classpath, sourcepath_list);
        free(first_part);
        
        if (resolved_first) {
            /* Combine resolved first part with the rest */
            const char *rest = dot;  /* includes the leading dot */
            size_t total_len = strlen(resolved_first) + strlen(rest) + 1;
            char *result = malloc(total_len);
            snprintf(result, total_len, "%s%s", resolved_first, rest);
            free(resolved_first);
            return result;
        }
        
        /* If first part couldn't be resolved, return as-is (might be fully qualified already) */
        return strdup(simple_name);
    }
    
    /* Check single-type imports first */
    for (slist_t *node = imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        /* Skip static imports */
        if (import->data.node.flags & MOD_STATIC) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Skip wildcard imports for now */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            continue;
        }
        
        /* Single-type import: check if simple name matches */
        const char *last_dot = strrchr(import_name, '.');
        const char *import_simple = last_dot ? last_dot + 1 : import_name;
        
        if (strcmp(import_simple, simple_name) == 0) {
            return strdup(import_name);
        }
    }
    
    /* Check if simple_name is a nested type in any single-type-imported class
     * e.g., "import javax.tools.JavaFileManager;" allows using "Location" 
     * to refer to JavaFileManager.Location */
    for (slist_t *node = imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        /* Skip static imports */
        if (import->data.node.flags & MOD_STATIC) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Skip wildcard imports */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            continue;
        }
        
        /* Try loading the imported class and check for a nested type */
        if (classpath) {
            /* Try to load the imported class from classpath */
            classfile_t *imported_cf = classpath_load_class(classpath, import_name);
            if (imported_cf) {
                /* Check if it has an InnerClasses entry for our simple_name */
                char nested_class_name[512];
                snprintf(nested_class_name, sizeof(nested_class_name), "%s$%s", 
                         import_name, simple_name);
                /* Try to load the nested class directly */
                classfile_t *nested_cf = classpath_load_class(classpath, nested_class_name);
                if (nested_cf) {
                    return strdup(nested_class_name);
                }
            }
        }
    }
    
    /* Check on-demand (wildcard) imports */
    for (slist_t *node = imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        /* Skip static imports */
        if (import->data.node.flags & MOD_STATIC) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        /* Check if it's a wildcard import (ends with .*) */
        size_t len = strlen(import_name);
        if (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.') {
            /* Try package.SimpleName */
            char qualified[512];
            snprintf(qualified, sizeof(qualified), "%.*s%s", 
                     (int)(len - 1), import_name, simple_name);
            
            /* Verify the class exists in classpath or sourcepath */
            if (classpath) {
                classfile_t *cf = classpath_load_class(classpath, qualified);
                if (cf) {
                    return strdup(qualified);
                }
            }
            if (class_exists_on_sourcepath(qualified, sourcepath_list)) {
                return strdup(qualified);
            }
        }
    }
    
    /* Try same-package resolution */
    if (package) {
        char same_package[512];
        snprintf(same_package, sizeof(same_package), "%s.%s", package, simple_name);
        
        if (classpath) {
            classfile_t *cf = classpath_load_class(classpath, same_package);
            if (cf) {
                return strdup(same_package);
            }
        }
        if (class_exists_on_sourcepath(same_package, sourcepath_list)) {
            return strdup(same_package);
        }
    }
    
    /* Try java.lang.* (implicit import) */
    char java_lang[256];
    snprintf(java_lang, sizeof(java_lang), "java.lang.%s", simple_name);
    if (classpath) {
        classfile_t *cf = classpath_load_class(classpath, java_lang);
        if (cf) {
            return strdup(java_lang);
        }
    }
    /* java.lang classes won't be on sourcepath, no need to check */
    
    /* Could not resolve - return NULL (caller should keep original name) */
    return NULL;
}

/**
 * Check if a simple name is a nested type within a class declaration.
 * This is used to prevent premature resolution of local nested types.
 */
static bool is_local_nested_type(ast_node_t *class_decl, const char *name)
{
    if (!class_decl || !name) return false;
    
    /* Only check within class-like declarations */
    if (class_decl->type != AST_CLASS_DECL && 
        class_decl->type != AST_INTERFACE_DECL &&
        class_decl->type != AST_ENUM_DECL &&
        class_decl->type != AST_RECORD_DECL) {
        return false;
    }
    
    /* Search children for nested type declarations with matching name */
    for (slist_t *child = class_decl->data.node.children; child; child = child->next) {
        ast_node_t *member = (ast_node_t *)child->data;
        if (!member) continue;
        
        if ((member->type == AST_CLASS_DECL ||
             member->type == AST_INTERFACE_DECL ||
             member->type == AST_ENUM_DECL ||
             member->type == AST_RECORD_DECL ||
             member->type == AST_ANNOTATION_DECL) &&
            member->data.node.name &&
            strcmp(member->data.node.name, name) == 0) {
            return true;
        }
    }
    
    return false;
}

/**
 * Recursively resolve type names in an AST node.
 * Updates data.node.name to the fully qualified name for type references.
 * 
 * @param enclosing_class The enclosing class declaration (for checking local nested types)
 */
static void resolve_types_in_node_with_context(ast_node_t *node,
                                               slist_t *imports,
                                               const char *package,
                                               classpath_t *classpath,
                                               slist_t *sourcepath_list,
                                               ast_node_t *enclosing_class)
{
    if (!node) return;
    
    /* Track the enclosing class for nested type resolution */
    ast_node_t *new_enclosing = enclosing_class;
    if (node->type == AST_CLASS_DECL || node->type == AST_INTERFACE_DECL ||
        node->type == AST_ENUM_DECL || node->type == AST_RECORD_DECL) {
        new_enclosing = node;
    }
    
    switch (node->type) {
        case AST_CLASS_TYPE:
            /* Resolve the class name */
            if (node->data.node.name) {
                const char *name = node->data.node.name;
                
                /* Check if this is a local nested type - if so, don't resolve it yet.
                 * Semantic analysis will handle resolution using the local scope. */
                bool is_simple_name = (strchr(name, '.') == NULL);
                if (is_simple_name && enclosing_class && is_local_nested_type(enclosing_class, name)) {
                    /* Skip - let semantic analysis handle this local type */
                    break;
                }
                
                char *qualified = resolve_type_name_with_imports(
                    name, imports, package, classpath, sourcepath_list);
                if (qualified) {
                    /* Note: name is interned, don't free it - just reassign */
                    node->data.node.name = qualified;
                }
            }
            /* Also resolve type arguments (children) */
            break;
            
        case AST_IDENTIFIER:
            /* Identifiers may be used as type names in certain contexts
             * (e.g., in field/method types). We'll resolve them when
             * they're actually used as types. */
            break;
            
        default:
            break;
    }
    
    /* Recursively process children */
    if (node->type != AST_LITERAL && node->type != AST_IDENTIFIER &&
        node->type != AST_THIS_EXPR && node->type != AST_SUPER_EXPR &&
        node->type != AST_PRIMITIVE_TYPE && node->type != AST_VAR_TYPE) {
        for (slist_t *child = node->data.node.children; child; child = child->next) {
            resolve_types_in_node_with_context((ast_node_t *)child->data, imports, package, 
                                               classpath, sourcepath_list, new_enclosing);
        }
        
        /* Process extra node (e.g., return type) */
        if (node->data.node.extra) {
            resolve_types_in_node_with_context(node->data.node.extra, imports, package, 
                                               classpath, sourcepath_list, new_enclosing);
        }
    }
}

/**
 * Wrapper for backward compatibility.
 */
static void resolve_types_in_node(ast_node_t *node,
                                  slist_t *imports,
                                  const char *package,
                                  classpath_t *classpath,
                                  slist_t *sourcepath_list)
{
    resolve_types_in_node_with_context(node, imports, package, classpath, sourcepath_list, NULL);
}

/**
 * Resolve all type names in a compilation unit to fully qualified names.
 * This should be called immediately after parsing, before any semantic analysis.
 * 
 * @param ast             The parsed AST (must be AST_COMPILATION_UNIT)
 * @param classpath       The classpath for verifying type names
 * @param sourcepath_list List of sourcepath directories for verifying source files
 */
void resolve_types_in_compilation_unit(ast_node_t *ast, classpath_t *classpath, 
                                       slist_t *sourcepath_list)
{
    if (!ast || ast->type != AST_COMPILATION_UNIT) {
        return;
    }
    
    /* Extract package and imports from the compilation unit */
    const char *package = NULL;
    slist_t *imports = NULL;
    
    for (slist_t *child = ast->data.node.children; child; child = child->next) {
        ast_node_t *decl = (ast_node_t *)child->data;
        if (decl->type == AST_PACKAGE_DECL) {
            package = decl->data.node.name;
        } else if (decl->type == AST_IMPORT_DECL) {
            if (!imports) {
                imports = slist_new(decl);
            } else {
                slist_append(imports, decl);
            }
        }
    }
    
    /* Resolve types in the entire AST */
    resolve_types_in_node(ast, imports, package, classpath, sourcepath_list);
    
    /* Free the imports list (not the AST nodes, just the list) */
    slist_free(imports);
}

/**
 * Resolve a static import for a field.
 * Returns the class symbol and sets *field_sym if found.
 * 
 * Static import examples:
 *   import static java.lang.Math.PI;      -> Math.PI
 *   import static java.lang.Math.*;       -> Math.<any static field>
 */
static symbol_t *resolve_static_import_field(semantic_t *sem, const char *field_name, 
                                             symbol_t **field_sym)
{
    if (!field_name || !sem->static_imports) {
        return NULL;
    }
    
    for (slist_t *node = sem->static_imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        size_t len = strlen(import_name);
        bool is_wildcard = (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.');
        
        if (is_wildcard) {
            /* Wildcard static import: import static pkg.Class.* */
            /* Extract class name (everything before .*) */
            char class_name[512];
            snprintf(class_name, sizeof(class_name), "%.*s", (int)(len - 2), import_name);
            
            /* Load the class */
            symbol_t *class_sym = load_external_class(sem, class_name);
            if (class_sym && class_sym->data.class_data.members) {
                /* Look for static field with matching name */
                symbol_t *field = scope_lookup_local(class_sym->data.class_data.members, field_name);
                if (field && field->kind == SYM_FIELD && (field->modifiers & MOD_STATIC)) {
                    if (field_sym) *field_sym = field;
                    return class_sym;
                }
            }
        } else {
            /* Single-member static import: import static pkg.Class.member */
            const char *last_dot = strrchr(import_name, '.');
            if (last_dot) {
                const char *member_name = last_dot + 1;
                if (strcmp(member_name, field_name) == 0) {
                    /* Extract class name */
                    char class_name[512];
                    size_t class_len = last_dot - import_name;
                    snprintf(class_name, sizeof(class_name), "%.*s", (int)class_len, import_name);
                    
                    /* Load the class */
                    symbol_t *class_sym = load_external_class(sem, class_name);
                    if (class_sym && class_sym->data.class_data.members) {
                        symbol_t *field = scope_lookup_local(class_sym->data.class_data.members, field_name);
                        if (field && field->kind == SYM_FIELD && (field->modifiers & MOD_STATIC)) {
                            if (field_sym) *field_sym = field;
                            return class_sym;
                        }
                    }
                }
            }
        }
    }
    
    return NULL;
}

/**
 * Find the best method overload from a list of candidates based on argument types.
 * @param sem The semantic analyzer
 * @param candidates List of method symbols
 * @param args List of argument AST nodes
 * @return The best matching method or NULL
 */
static symbol_t *find_best_method_by_types(semantic_t *sem, slist_t *candidates, slist_t *args, type_t *recv_type)
{
    if (!candidates) return NULL;
    
    /* Count arguments */
    int arg_count = 0;
    for (slist_t *a = args; a; a = a->next) arg_count++;
    
    symbol_t *best_match = NULL;
    int best_score = -1;
    symbol_t *varargs_match = NULL;
    
    for (slist_t *node = candidates; node; node = node->next) {
        symbol_t *method = (symbol_t *)node->data;
        if (!method || (method->kind != SYM_METHOD && method->kind != SYM_CONSTRUCTOR)) {
            continue;
        }
        
        /* Count parameters */
        int param_count = 0;
        slist_t *params = method->data.method_data.parameters;
        for (slist_t *p = params; p; p = p->next) {
            param_count++;
        }
        
        bool is_varargs = (method->modifiers & MOD_VARARGS) != 0;
        
        if (param_count != arg_count && !(is_varargs && arg_count >= param_count - 1)) {
            continue;  /* Wrong number of arguments */
        }
        
        /* Score this method based on how well argument types match */
        int score = 0;
        slist_t *arg_node = args;
        slist_t *param_node = params;
        bool type_mismatch = false;
        
        while (arg_node && param_node) {
            ast_node_t *arg = (ast_node_t *)arg_node->data;
            symbol_t *param = (symbol_t *)param_node->data;
            
            if (arg && param && param->type) {
                /* Unwrap parentheses to find lambda/method ref */
                ast_node_t *unwrapped_arg = arg;
                while (unwrapped_arg->type == AST_PARENTHESIZED && 
                       unwrapped_arg->data.node.children) {
                    unwrapped_arg = (ast_node_t *)unwrapped_arg->data.node.children->data;
                }
                
                /* Special handling for lambda/method ref arguments - they match any functional interface */
                if (unwrapped_arg->type == AST_LAMBDA_EXPR || unwrapped_arg->type == AST_METHOD_REF) {
                    /* First check if lambda parameter count matches SAM parameter count */
                    if (unwrapped_arg->type == AST_LAMBDA_EXPR && param->type && 
                        param->type->kind == TYPE_CLASS) {
                        /* Count lambda parameters.
                         * Lambda structure: first children are params, last child is body.
                         * - Typed params: AST_PARAMETER nodes
                         * - Inferred single param (x -> body): AST_IDENTIFIER 
                         * - Inferred multiple params (x, y -> body): First child is comma expr
                         * - Empty params (() -> body): AST_PARENTHESIZED with no children */
                        int lambda_param_count = 0;
                        slist_t *lambda_children = unwrapped_arg->data.node.children;
                        
                        /* Count all AST_PARAMETER nodes first (typed parameters).
                         * Parameters may be direct children or inside AST_LAMBDA_PARAMS */
                        for (slist_t *c = lambda_children; c; c = c->next) {
                            ast_node_t *child = (ast_node_t *)c->data;
                            if (child && child->type == AST_PARAMETER) {
                                lambda_param_count++;
                            } else if (child && child->type == AST_LAMBDA_PARAMS) {
                                /* Parameters are inside AST_LAMBDA_PARAMS node */
                                for (slist_t *pc = child->data.node.children; pc; pc = pc->next) {
                                    ast_node_t *pchild = (ast_node_t *)pc->data;
                                    if (pchild && pchild->type == AST_PARAMETER) {
                                        lambda_param_count++;
                                    }
                                }
                            }
                        }
                        
                        /* If no typed params, check for inferred params OR typed params inside parentheses */
                        if (lambda_param_count == 0 && lambda_children) {
                            ast_node_t *first = (ast_node_t *)lambda_children->data;
                            if (first) {
                                if (first->type == AST_IDENTIFIER) {
                                    /* Single inferred param: x -> body */
                                    lambda_param_count = 1;
                                } else if (first->type == AST_BINARY_EXPR &&
                                           first->data.node.name &&
                                           strcmp(first->data.node.name, ",") == 0) {
                                    /* Multiple inferred params: x, y -> body
                                     * Count identifiers in the comma expression */
                                    int comma_count = 1;
                                    ast_node_t *comma_expr = first;
                                    while (comma_expr->type == AST_BINARY_EXPR && 
                                           comma_expr->data.node.name &&
                                           strcmp(comma_expr->data.node.name, ",") == 0 &&
                                           comma_expr->data.node.children &&
                                           comma_expr->data.node.children->next) {
                                        comma_count++;
                                        comma_expr = (ast_node_t *)comma_expr->data.node.children->data;
                                    }
                                    lambda_param_count = comma_count + 1;
                                } else if (first->type == AST_PARENTHESIZED) {
                                    /* Parenthesized params - could be inferred OR typed */
                                    if (first->data.node.children) {
                                        /* Count AST_PARAMETER nodes inside parentheses */
                                        for (slist_t *pc = first->data.node.children; pc; pc = pc->next) {
                                            ast_node_t *pchild = (ast_node_t *)pc->data;
                                            if (pchild && pchild->type == AST_PARAMETER) {
                                                lambda_param_count++;
                                            }
                                        }
                                        /* If no typed params, check for inferred */
                                        if (lambda_param_count == 0) {
                                            ast_node_t *inner = (ast_node_t *)first->data.node.children->data;
                                            if (inner && inner->type == AST_IDENTIFIER) {
                                                lambda_param_count = 1;
                                            } else if (inner && inner->type == AST_BINARY_EXPR &&
                                                       inner->data.node.name &&
                                                       strcmp(inner->data.node.name, ",") == 0) {
                                                /* Multiple params: (x, y) */
                                                int comma_count = 1;
                                                ast_node_t *comma_expr = inner;
                                                while (comma_expr->type == AST_BINARY_EXPR && 
                                                       comma_expr->data.node.name &&
                                                       strcmp(comma_expr->data.node.name, ",") == 0 &&
                                                       comma_expr->data.node.children &&
                                                       comma_expr->data.node.children->next) {
                                                    comma_count++;
                                                    comma_expr = (ast_node_t *)comma_expr->data.node.children->data;
                                                }
                                                lambda_param_count = comma_count + 1;
                                            }
                                        }
                                    }
                                    /* else: empty () means 0 params */
                                }
                            }
                        }
                        
                        /* Get SAM parameter count */
                        symbol_t *param_class = param->type->data.class_type.symbol;
                        if (!param_class) {
                            const char *name = param->type->data.class_type.name;
                            if (name) {
                                param_class = load_external_class(sem, name);
                            }
                        }
                        if (param_class) {
                            symbol_t *sam = get_functional_interface_sam(param_class);
                            if (sam) {
                                int sam_param_count = 0;
                                for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                                    sam_param_count++;
                                }
                                /* If parameter counts don't match, this overload is not viable */
                                if (lambda_param_count != sam_param_count) {
                                    type_mismatch = true;
                                    break;
                                }
                            }
                        }
                    }
                    
                    /* Determine if lambda is void-compatible or value-returning */
                    bool lambda_is_void_compatible = false;
                    bool lambda_returns_value = false;
                    
                    if (unwrapped_arg->type == AST_LAMBDA_EXPR) {
                        ast_node_t *body = NULL;
                        slist_t *lambda_children = unwrapped_arg->data.node.children;
                        /* Skip parameters and empty parentheses to get body
                         * Lambda children can be:
                         * - AST_PARAMETER nodes (typed params)
                         * - AST_PARENTHESIZED (empty () or identifier params)
                         * - The actual body expression/block
                         */
                        for (slist_t *c = lambda_children; c; c = c->next) {
                            ast_node_t *child = (ast_node_t *)c->data;
                            if (!child) continue;
                            /* Skip parameter-related nodes */
                            if (child->type == AST_PARAMETER) continue;
                            /* Skip parenthesized param lists (empty () becomes AST_PARENTHESIZED with no children) */
                            if (child->type == AST_PARENTHESIZED && !child->data.node.children) continue;
                            /* Found the body */
                            body = child;
                            break;
                        }
                        
                        if (body) {
                            /* Unwrap parenthesized expressions around the body */
                            while (body->type == AST_PARENTHESIZED && body->data.node.children) {
                                body = (ast_node_t *)body->data.node.children->data;
                            }
                            
                            if (body->type == AST_BLOCK) {
                                /* Block body - check for return statements */
                                /* Simple heuristic: look at direct children for return statements */
                                slist_t *stmts = body->data.node.children;
                                for (slist_t *s = stmts; s; s = s->next) {
                                    ast_node_t *stmt = (ast_node_t *)s->data;
                                    if (stmt && stmt->type == AST_RETURN_STMT) {
                                        if (stmt->data.node.children) {
                                            lambda_returns_value = true;
                                        }
                                    }
                                }
                                /* If no value return found, it's void compatible */
                                if (!lambda_returns_value) {
                                    lambda_is_void_compatible = true;
                                }
                            } else {
                                /* Expression body - check structurally what kind of expression */
                                /* JLS 15.27.3: Statement expressions are void-compatible:
                                 * Assignment, PreIncrement, PreDecrement, PostIncrement, PostDecrement
                                 * Method Invocation, Class Instance Creation */
                                switch (body->type) {
                                    case AST_METHOD_CALL:
                                    case AST_ASSIGNMENT_EXPR:
                                    case AST_UNARY_EXPR:    /* ++x, --x, x++, x-- */
                                    case AST_NEW_OBJECT:
                                        /* These are "statement expressions" - void-compatible */
                                        /* But they can also return a value if the target expects it */
                                        /* Mark as both for now - let actual binding determine */
                                        lambda_is_void_compatible = true;
                                        break;
                                    default:
                                        /* Pure expressions (binary ops, literals, etc.) return values */
                                        lambda_returns_value = true;
                                        break;
                                }
                            }
                        }
                    }
                    
                    /* Get the SAM return type of the parameter's functional interface */
                    bool param_is_void_returning = false;
                    if (param->type && param->type->kind == TYPE_CLASS) {
                        symbol_t *param_class = param->type->data.class_type.symbol;
                        if (!param_class) {
                            const char *name = param->type->data.class_type.name;
                            if (name) {
                                param_class = load_external_class(sem, name);
                            }
                        }
                        if (param_class) {
                            symbol_t *sam = get_functional_interface_sam(param_class);
                            /* Void methods may have NULL type or TYPE_VOID */
                            if (sam && (!sam->type || sam->type->kind == TYPE_VOID)) {
                                param_is_void_returning = true;
                            }
                        }
                    }
                    
                    /* Score based on void compatibility match */
                    if (lambda_is_void_compatible && param_is_void_returning) {
                        score += 70;  /* Void lambda matches void SAM - higher score */
                    } else if (lambda_returns_value && !param_is_void_returning) {
                        score += 70;  /* Value lambda matches non-void SAM - higher score */
                    } else {
                        score += 50;  /* Mismatch but still compatible */
                    }
                    
                    arg_node = arg_node->next;
                    param_node = param_node->next;
                    continue;
                }
                
                type_t *arg_type = get_expression_type(sem, arg);
                type_t *param_type = param->type;
                
                /* Substitute type arguments from receiver into parameter type.
                 * E.g., for Function<Integer, String>.apply(T t), param_type is T,
                 * which should be substituted with Integer from recv_type. */
                if (recv_type && param_type) {
                    param_type = substitute_from_receiver(param_type, recv_type);
                }
                
                /* For varargs, the last parameter is an array. Individual args match the element type. */
                bool is_last_param = (param_node->next == NULL);
                bool is_varargs_param = is_varargs && is_last_param && param_type->kind == TYPE_ARRAY;
                
                /* Get the effective param type for comparison */
                type_t *compare_type = param_type;
                if (is_varargs_param && arg_type && arg_type->kind != TYPE_ARRAY) {
                    /* Individual vararg - compare against element type */
                    compare_type = param_type->data.array_type.element_type;
                }
                
                if (arg_type && compare_type) {
                    /* Type variables accept any reference type (after boxing primitives) */
                    if (compare_type->kind == TYPE_TYPEVAR) {
                        /* Type variables can match any type - will be inferred during type checking.
                         * Primitives will be boxed to their wrapper classes. */
                        score += 60;  /* Good match, but not as good as exact type match */
                        arg_node = arg_node->next;
                        /* For varargs, don't advance param_node after the last param */
                        if (!is_varargs || param_node->next != NULL) {
                            param_node = param_node->next;
                        }
                        continue;
                    }
                    
                    /* For class types, check if classes match exactly */
                    bool exact_match = false;
                    if (arg_type->kind == TYPE_CLASS && compare_type->kind == TYPE_CLASS) {
                        const char *arg_name = arg_type->data.class_type.name;
                        const char *param_name = compare_type->data.class_type.name;
                        if (arg_name && param_name && strcmp(arg_name, param_name) == 0) {
                            exact_match = true;
                        }
                    } else if (arg_type->kind == TYPE_ARRAY && compare_type->kind == TYPE_ARRAY) {
                        /* For arrays, element types must match exactly */
                        exact_match = type_equals(arg_type, compare_type);
                    } else if (arg_type->kind == compare_type->kind && 
                               arg_type->kind != TYPE_ARRAY) {
                        /* For other non-class/non-array types, kind match is exact match */
                        exact_match = true;
                    }
                    
                    if (exact_match) {
                        score += 100;
                    }
                    /* Widening/compatible conversion gets lower score */
                    else {
                        /* Ensure both source and target symbols are loaded for interface hierarchy checking */
                        if (compare_type->kind == TYPE_CLASS && !compare_type->data.class_type.symbol) {
                            const char *class_name = compare_type->data.class_type.name;
                            if (class_name) {
                                symbol_t *sym = load_external_class(sem, class_name);
                                if (sym) {
                                    compare_type->data.class_type.symbol = sym;
                                }
                            }
                        }
                        if (arg_type->kind == TYPE_CLASS && !arg_type->data.class_type.symbol) {
                            const char *class_name = arg_type->data.class_type.name;
                            if (class_name) {
                                symbol_t *sym = load_external_class(sem, class_name);
                                if (sym) {
                                    arg_type->data.class_type.symbol = sym;
                                }
                            }
                        }
                        
                        /* Check for widening reference conversion first (higher priority than boxing).
                         * Java's method resolution prefers:
                         * 1. Exact match
                         * 2. Widening reference/primitive conversion
                         * 3. Boxing/unboxing conversion
                         * Both arg and param are class types and arg is subtype of param */
                        bool is_widening_ref = (arg_type->kind == TYPE_CLASS && 
                                                compare_type->kind == TYPE_CLASS);
                        
                        /* Boxing/unboxing has lower priority than reference widening */
                        bool needs_boxing = type_needs_boxing(compare_type, arg_type);
                        bool needs_unboxing = type_needs_unboxing(compare_type, arg_type);
                        
                        if (is_widening_ref && !needs_boxing && !needs_unboxing) {
                            /* Reference type widening (e.g., Boolean -> Object) */
                            if (type_assignable(compare_type, arg_type)) {
                                score += 75;  /* Higher priority than boxing/unboxing */
                            } else {
                                type_mismatch = true;
                                break;
                            }
                        } else if (needs_boxing || needs_unboxing) {
                            score += 40;  /* Lower priority - boxing/unboxing */
                        } else if (type_assignable(compare_type, arg_type)) {
                            score += 50;  /* Other compatible conversions */
                    }
                    /* Type mismatch */
                    else {
                        type_mismatch = true;
                        break;
                        }
                    }
                }
            }
            
            arg_node = arg_node->next;
            /* For varargs, don't advance param_node after the last param */
            if (!is_varargs || param_node->next != NULL) {
                param_node = param_node->next;
            }
        }
        
        if (type_mismatch) continue;
        
        /* For varargs methods, when the argument count equals the parameter count
         * and the last argument is an array compatible with the varargs array,
         * treat it as a direct array pass (not varargs expansion).
         * This gives it the same priority as non-varargs methods. */
        bool is_direct_array_to_varargs = false;
        if (is_varargs && arg_count == param_count && args) {
            /* Get the last argument */
            slist_t *last_arg_node = args;
            while (last_arg_node->next) last_arg_node = last_arg_node->next;
            ast_node_t *last_arg = (ast_node_t *)last_arg_node->data;
            type_t *last_arg_type = get_expression_type(sem, last_arg);
            
            /* Get the varargs parameter type (last parameter) */
            slist_t *last_param_node = method->data.method_data.parameters;
            while (last_param_node && last_param_node->next) last_param_node = last_param_node->next;
            symbol_t *last_param = last_param_node ? (symbol_t *)last_param_node->data : NULL;
            
            /* If both are arrays, it's a direct array pass */
            if (last_arg_type && last_arg_type->kind == TYPE_ARRAY &&
                last_param && last_param->type && last_param->type->kind == TYPE_ARRAY) {
                is_direct_array_to_varargs = true;
            }
        }
        
        if (is_varargs && arg_count >= param_count - 1 && !is_direct_array_to_varargs) {
            /* Varargs match with expansion - lower priority than exact match */
            if (!varargs_match || score > best_score) {
                varargs_match = method;
            }
        } else if ((param_count == arg_count || is_direct_array_to_varargs) && score > best_score) {
            best_match = method;
            best_score = score;
        } else if ((param_count == arg_count || is_direct_array_to_varargs) && score == best_score && best_match) {
            /* Same score - prefer method with more specific return type.
             * This handles bridge methods where the override returns a more specific type.
             * E.g., ByteBuffer.array() returns byte[] vs Buffer.array() returns Object
             * E.g., ByteBuffer.slice() returns ByteBuffer vs Buffer.slice() returns Buffer */
            type_t *new_ret = method->type;
            type_t *old_ret = best_match->type;
            if (old_ret && old_ret->kind == TYPE_CLASS && new_ret &&
                old_ret->data.class_type.name &&
                strcmp(old_ret->data.class_type.name, "java.lang.Object") == 0 &&
                (new_ret->kind == TYPE_ARRAY || 
                 (new_ret->kind == TYPE_CLASS && new_ret->data.class_type.name &&
                  strcmp(new_ret->data.class_type.name, "java.lang.Object") != 0))) {
                /* Current best returns Object but new method returns more specific type */
                best_match = method;
            } else if (old_ret && old_ret->kind == TYPE_CLASS && 
                       new_ret && new_ret->kind == TYPE_CLASS &&
                       old_ret->data.class_type.name && new_ret->data.class_type.name) {
                /* Both return class types - check if one is a subtype of the other.
                 * Prefer the more specific one (subclass).
                 * E.g., prefer ByteBuffer over Buffer */
                symbol_t *old_sym = old_ret->data.class_type.symbol;
                symbol_t *new_sym = new_ret->data.class_type.symbol;
                
                /* If we don't have symbols, try to load them */
                if (!old_sym && old_ret->data.class_type.name) {
                    old_sym = load_external_class(sem, old_ret->data.class_type.name);
                }
                if (!new_sym && new_ret->data.class_type.name) {
                    new_sym = load_external_class(sem, new_ret->data.class_type.name);
                }
                
                /* Check if new_ret's class is a subclass of old_ret's class */
                if (new_sym && old_ret->data.class_type.name) {
                    symbol_t *search = new_sym;
                    while (search) {
                        if (search->data.class_data.superclass) {
                            const char *super_name = search->data.class_data.superclass->qualified_name ?
                                search->data.class_data.superclass->qualified_name :
                                search->data.class_data.superclass->name;
                            if (super_name && strcmp(super_name, old_ret->data.class_type.name) == 0) {
                                /* new method returns a subtype of old method's return type */
                                best_match = method;
                                break;
                            }
                        }
                        search = search->data.class_data.superclass;
                    }
                }
            }
        }
    }
    
    return best_match ? best_match : varargs_match;
}

/**
 * Resolve a static import for a method.
 * Returns the class symbol and sets *method_sym if found.
 * @param args The argument list for type-based overload resolution (can be NULL)
 */
static symbol_t *resolve_static_import_method(semantic_t *sem, const char *method_name,
                                              slist_t *args, symbol_t **method_sym)
{
    if (!method_name || !sem->static_imports) {
        return NULL;
    }
    
    for (slist_t *node = sem->static_imports; node; node = node->next) {
        ast_node_t *import = (ast_node_t *)node->data;
        if (!import || import->type != AST_IMPORT_DECL) {
            continue;
        }
        
        const char *import_name = import->data.node.name;
        if (!import_name) {
            continue;
        }
        
        size_t len = strlen(import_name);
        bool is_wildcard = (len > 2 && import_name[len-1] == '*' && import_name[len-2] == '.');
        
        if (is_wildcard) {
            /* Wildcard static import: import static pkg.Class.* */
            char class_name[512];
            snprintf(class_name, sizeof(class_name), "%.*s", (int)(len - 2), import_name);
            
            symbol_t *class_sym = load_external_class(sem, class_name);
            if (class_sym && class_sym->data.class_data.members) {
                /* Get all method overloads and find best match by type */
                slist_t *candidates = scope_find_all_methods(class_sym->data.class_data.members, method_name);
                symbol_t *method = find_best_method_by_types(sem, candidates, args, NULL);
                slist_free(candidates);
                
                if (method && method->kind == SYM_METHOD && (method->modifiers & MOD_STATIC)) {
                    if (method_sym) *method_sym = method;
                    return class_sym;
                }
            }
        } else {
            /* Single-member static import: import static pkg.Class.member */
            const char *last_dot = strrchr(import_name, '.');
            if (last_dot) {
                const char *member_name = last_dot + 1;
                if (strcmp(member_name, method_name) == 0) {
                    char class_name[512];
                    size_t class_len = last_dot - import_name;
                    snprintf(class_name, sizeof(class_name), "%.*s", (int)class_len, import_name);
                    
                    symbol_t *class_sym = load_external_class(sem, class_name);
                    if (class_sym && class_sym->data.class_data.members) {
                        /* Get all method overloads and find best match by type */
                        slist_t *candidates = scope_find_all_methods(class_sym->data.class_data.members, method_name);
                        symbol_t *method = find_best_method_by_types(sem, candidates, args, NULL);
                        slist_free(candidates);
                        
                        if (method && method->kind == SYM_METHOD && (method->modifiers & MOD_STATIC)) {
                            if (method_sym) *method_sym = method;
                            return class_sym;
                        }
                    }
                }
            }
        }
    }
    
    return NULL;
}

/* ========================================================================
 * Package-Private Type Resolution (like javac's ClassFinder.fillIn)
 * ======================================================================== */

/**
 * Scan the package directory and preregister all classes found.
 * This enables resolution of package-private types defined in other files
 * within the same package (like javac does in ClassFinder.fillIn).
 *
 * This function scans ALL .java files in the package directory and loads them,
 * because a type like 'AsyncTimeoutCallback' might be defined in 'AsyncTimeout.java'
 * (multiple types per file is allowed in Java for non-public types).
 *
 * Returns: The symbol if found by scanning, NULL otherwise.
 */
static symbol_t *scan_package_for_type(semantic_t *sem, const char *type_name, const char *package_name)
{
    if (!sem->sourcepath || !package_name || !type_name) {
        return NULL;
    }
    
    /* Build package directory path: srcpath/pkg/path/ */
    char pkg_path[512];
    strncpy(pkg_path, package_name, sizeof(pkg_path) - 1);
    pkg_path[sizeof(pkg_path) - 1] = '\0';
    for (char *p = pkg_path; *p; p++) {
        if (*p == '.') *p = '/';
    }
    
    /* Try each sourcepath entry */
    for (slist_t *sp = sem->sourcepath; sp; sp = sp->next) {
        const char *src_root = (const char *)sp->data;
        if (!src_root) continue;
        
        char dir_path[1024];
        snprintf(dir_path, sizeof(dir_path), "%s/%s", src_root, pkg_path);
        
        /* Try to open the directory */
        DIR *dir = opendir(dir_path);
        if (!dir) continue;
        
        if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
            fprintf(stderr, "DEBUG scan_package: scanning '%s' for type '%s'\n", 
                    dir_path, type_name);
        }
        
        struct dirent *entry;
        while ((entry = readdir(dir)) != NULL) {
            /* Skip non-.java files */
            size_t len = strlen(entry->d_name);
            if (len < 6 || strcmp(entry->d_name + len - 5, ".java") != 0) {
                continue;
            }
            
            /* Extract primary class name from filename */
            char primary_class[256];
            strncpy(primary_class, entry->d_name, len - 5);
            primary_class[len - 5] = '\0';
            
            /* Build qualified name for the primary class */
            char qualified[512];
            snprintf(qualified, sizeof(qualified), "%s.%s", package_name, primary_class);
            
            /* Skip if already loaded */
            if (hashtable_lookup(sem->types, qualified)) {
                continue;
            }
            
            if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
                fprintf(stderr, "DEBUG scan_package: loading '%s'\n", qualified);
            }
            
            /* Load this source file - this will parse it and register all
             * top-level types (including interfaces, enums, etc.) */
            symbol_t *sym = load_external_class(sem, qualified);
            
            /* After loading, check if we found the type we're looking for */
            if (sym) {
                char target_qualified[512];
                snprintf(target_qualified, sizeof(target_qualified), "%s.%s", 
                         package_name, type_name);
                type_t *found_type = hashtable_lookup(sem->types, target_qualified);
                if (found_type && found_type->kind == TYPE_CLASS && 
                    found_type->data.class_type.symbol) {
                    closedir(dir);
                    if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
                        fprintf(stderr, "DEBUG scan_package: found '%s' after loading '%s'\n", 
                                type_name, qualified);
                    }
                    return found_type->data.class_type.symbol;
                }
                
                /* Also check simple name cache */
                found_type = hashtable_lookup(sem->types, type_name);
                if (found_type && found_type->kind == TYPE_CLASS && 
                    found_type->data.class_type.symbol) {
                    closedir(dir);
                    if (getenv("GENESIS_DEBUG_PKG_SCAN")) {
                        fprintf(stderr, "DEBUG scan_package: found '%s' (simple) after loading '%s'\n", 
                                type_name, qualified);
                    }
                    return found_type->data.class_type.symbol;
                }
            }
        }
        closedir(dir);
    }
    
    return NULL;
}

/* ========================================================================
 * Type Resolution
 * ======================================================================== */

/**
 * Resolve an AST type node to a type_t.
 */
type_t *semantic_resolve_type(semantic_t *sem, ast_node_t *type_node)
{
    if (!type_node) {
        return type_new_primitive(TYPE_UNKNOWN);
    }
    
    /* Return cached result if already resolved */
    if (type_node->sem_type) {
        return type_node->sem_type;
    }
    
    switch (type_node->type) {
        case AST_PRIMITIVE_TYPE:
            {
                const char *name = type_node->data.leaf.name;
                if (strcmp(name, "void") == 0) {
                    return type_void();
                }
                if (strcmp(name, "boolean") == 0) {
                    return type_boolean();
                }
                if (strcmp(name, "byte") == 0) {
                    return type_byte();
                }
                if (strcmp(name, "char") == 0) {
                    return type_char();
                }
                if (strcmp(name, "short") == 0) {
                    return type_short();
                }
                if (strcmp(name, "int") == 0) {
                    return type_int();
                }
                if (strcmp(name, "long") == 0) {
                    return type_long();
                }
                if (strcmp(name, "float") == 0) {
                    return type_float();
                }
                if (strcmp(name, "double") == 0) {
                    return type_double();
                }
                
                semantic_error(sem, type_node->line, type_node->column,
                              "Unknown primitive type: %s", name);
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_CLASS_TYPE:
            {
                const char *name = type_node->data.node.name;
                
                
                /* Check if the name matches the current class (self-reference) */
                if (sem->current_class && sem->current_class->name &&
                    strcmp(name, sem->current_class->name) == 0) {
                    /* This is a reference to the current class itself */
                    if (sem->current_class->type) {
                        type_node->sem_type = sem->current_class->type;
                        return sem->current_class->type;
                    }
                }
                
                /* Check if it's a type parameter of the current method */
                if (sem->current_method && sem->current_method->data.method_data.body_scope) {
                    symbol_t *type_param = scope_lookup_local(
                        sem->current_method->data.method_data.body_scope, name);
                    if (type_param && type_param->kind == SYM_TYPE_PARAM) {
                        if (type_param->type) {
                            type_node->sem_type = type_param->type;
                            return type_param->type;
                        }
                    }
                }
                
                /* Also check current_scope - during preregistration, method type params
                 * are added to a method_scope that becomes current_scope but current_method
                 * is not set. Walk up the scope chain to find method scopes. */
                for (scope_t *s = sem->current_scope; s; s = s->parent) {
                    if (s->type == SCOPE_METHOD) {
                        symbol_t *type_param = scope_lookup_local(s, name);
                        if (type_param && type_param->kind == SYM_TYPE_PARAM) {
                            if (type_param->type) {
                                type_node->sem_type = type_param->type;
                                return type_param->type;
                            }
                        }
                    }
                }
                
                /* Check if it's a type parameter of the current class */
                if (sem->current_class && sem->current_class->data.class_data.members) {
                    symbol_t *type_param = scope_lookup_local(
                        sem->current_class->data.class_data.members, name);
                    if (type_param && type_param->kind == SYM_TYPE_PARAM) {
                        if (type_param->type) {
                            type_node->sem_type = type_param->type;
                            return type_param->type;
                        }
                    }
                }
                
                /* Check if it's a nested class/enum/record of the current class.
                 * The name might be fully qualified (e.g., "pkg.ExportThread" from
                 * resolve_types_in_compilation_unit), so also check with simple name. */
                if (sem->current_class && sem->current_class->data.class_data.members) {
                    /* First try the name as-is */
                    symbol_t *nested = scope_lookup_local(
                        sem->current_class->data.class_data.members, name);
                    
                    /* If not found, try extracting the simple name (after last dot) */
                    if (!nested && strchr(name, '.')) {
                        const char *simple = strrchr(name, '.');
                        if (simple) {
                            simple++;  /* Skip the dot */
                            nested = scope_lookup_local(
                                sem->current_class->data.class_data.members, simple);
                        }
                    }
                    
                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                   nested->kind == SYM_ENUM || nested->kind == SYM_RECORD)) {
                        if (nested->type) {
                            /* Check for type arguments */
                            slist_t *children = type_node->data.node.children;
                            if (children && nested->type->kind == TYPE_CLASS) {
                                /* Has type arguments - create parameterized type */
                                type_t *param_type = type_new_class(nested->type->data.class_type.name ? 
                                    nested->type->data.class_type.name : name);
                                param_type->data.class_type.symbol = nested;
                                
                                /* Resolve each type argument */
                                for (slist_t *c = children; c; c = c->next) {
                                    ast_node_t *arg = (ast_node_t *)c->data;
                                    type_t *arg_type = semantic_resolve_type(sem, arg);
                                    if (param_type->data.class_type.type_args == NULL) {
                                        param_type->data.class_type.type_args = slist_new(arg_type);
                                    } else {
                                        slist_append(param_type->data.class_type.type_args, arg_type);
                                    }
                                }
                                
                                type_node->sem_type = param_type;
                                return param_type;
                            }
                            /* Ensure the type's symbol field is set */
                            if (nested->type->kind == TYPE_CLASS && 
                                nested->type->data.class_type.symbol == NULL) {
                                nested->type->data.class_type.symbol = nested;
                            }
                            /* Cache in per-compilation-unit scope so later lookups
                             * with current_class=(null) can still find it.
                             * Also cache by qualified name in global cache. */
                            if (nested->qualified_name) {
                                if (!hashtable_lookup(sem->types, nested->qualified_name)) {
                                    hashtable_insert(sem->types, nested->qualified_name, nested->type);
                                }
                            }
                            if (!hashtable_lookup(sem->unit_types, name)) {
                                hashtable_insert(sem->unit_types, name, nested->type);
                            }
                            type_node->sem_type = nested->type;
                            return nested->type;
                        }
                    }
                }
                
                /* Check if it's a nested type of an implemented interface.
                 * Walk up the enclosing class chain to check their interfaces too. */
                {
                    symbol_t *check_class = sem->current_class;
                    while (check_class) {
                        if (getenv("GENESIS_DEBUG_IFACE_NESTED")) {
                            fprintf(stderr, "DEBUG iface_nested: looking for '%s', check_class=%s, interfaces=%p\n",
                                name, 
                                check_class->name ? check_class->name : "(unnamed)",
                                (void*)check_class->data.class_data.interfaces);
                        }
                        if (check_class->data.class_data.interfaces) {
                            for (slist_t *iface = check_class->data.class_data.interfaces; 
                                 iface; iface = iface->next) {
                                symbol_t *iface_sym = (symbol_t *)iface->data;
                                if (getenv("GENESIS_DEBUG_IFACE_NESTED")) {
                                    fprintf(stderr, "DEBUG iface_nested:   checking interface '%s' (kind=%d, members=%p)\n",
                                        iface_sym ? (iface_sym->name ? iface_sym->name : "(unnamed)") : "(null)",
                                        iface_sym ? iface_sym->kind : -1,
                                        iface_sym ? (void*)iface_sym->data.class_data.members : NULL);
                                }
                                /* Only check valid interface symbols */
                                if (iface_sym && (iface_sym->kind == SYM_INTERFACE || 
                                                  iface_sym->kind == SYM_CLASS) &&
                                    iface_sym->data.class_data.members) {
                                    symbol_t *nested = scope_lookup_local(
                                        iface_sym->data.class_data.members, name);
                                    if (getenv("GENESIS_DEBUG_IFACE_NESTED")) {
                                        fprintf(stderr, "DEBUG iface_nested:   looked up '%s' in '%s' -> %p (kind=%d, type=%p)\n",
                                            name, iface_sym->name ? iface_sym->name : "(null)",
                                            (void*)nested, nested ? nested->kind : -1, nested ? (void*)nested->type : NULL);
                                    }
                                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                                   nested->kind == SYM_ENUM || nested->kind == SYM_RECORD)) {
                                        if (nested->type) {
                                            /* Ensure the type's symbol field is set */
                                            if (nested->type->kind == TYPE_CLASS && 
                                                nested->type->data.class_type.symbol == NULL) {
                                                nested->type->data.class_type.symbol = nested;
                                            }
                                            if (getenv("GENESIS_DEBUG_IFACE_NESTED")) {
                                                fprintf(stderr, "DEBUG iface_nested:   RETURNING type for '%s'\n", name);
                                            }
                                            type_node->sem_type = nested->type;
                                            return nested->type;
                                        }
                                    }
                                }
                            }
                        }
                        /* Move to enclosing class */
                        check_class = check_class->data.class_data.enclosing_class;
                    }
                }
                
                /* Check for qualified local class names like "Outer.Inner" where Outer is local */
                const char *dot = strchr(name, '.');
                if (dot != NULL && sem->current_scope) {
                    size_t outer_len = dot - name;
                    char *outer_name = strndup(name, outer_len);
                    const char *rest = dot + 1;
                    
                    /* Look up outer in current scope (for local classes) */
                    symbol_t *nested = scope_lookup(sem->current_scope, outer_name);
                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                   nested->kind == SYM_ENUM || nested->kind == SYM_RECORD)) {
                        /* Walk the chain of inner classes */
                        while (rest && nested && nested->data.class_data.members) {
                            const char *next_dot = strchr(rest, '.');
                            char *inner_name;
                            if (next_dot) {
                                inner_name = strndup(rest, next_dot - rest);
                                rest = next_dot + 1;
                            } else {
                                inner_name = strdup(rest);
                                rest = NULL;
                            }
                            
                            symbol_t *inner = scope_lookup_local(
                                nested->data.class_data.members, inner_name);
                            free(inner_name);
                            
                            if (inner && (inner->kind == SYM_CLASS || inner->kind == SYM_INTERFACE ||
                                         inner->kind == SYM_ENUM || inner->kind == SYM_RECORD)) {
                                nested = inner;
                            } else {
                                nested = NULL;
                            }
                        }
                        
                        if (nested && nested->type) {
                            free(outer_name);
                            type_node->sem_type = nested->type;
                            return nested->type;
                        }
                    }
                    free(outer_name);
                }
                
                /* Check for qualified nested class names like "Inner.InnerInner" or "Outer.Inner" */
                dot = strchr(name, '.');
                if (dot != NULL && sem->current_class) {
                    /* Split on first dot - e.g., "Outer.Inner" → "Outer" and "Inner" */
                    size_t outer_len = dot - name;
                    char *outer_name = strndup(name, outer_len);
                    const char *rest = dot + 1;
                    symbol_t *nested = NULL;
                    
                    /* Check if outer_name is the current class itself (e.g., MyClass.Inner from inside MyClass) */
                    if (strcmp(outer_name, sem->current_class->name) == 0) {
                        /* Start from current class's members */
                        nested = sem->current_class;
                    } else if (sem->current_class->data.class_data.members) {
                        /* Look up outer in current class's members */
                        nested = scope_lookup_local(
                            sem->current_class->data.class_data.members, outer_name);
                        if (!nested || (nested->kind != SYM_CLASS && nested->kind != SYM_INTERFACE &&
                                       nested->kind != SYM_ENUM && nested->kind != SYM_RECORD)) {
                            nested = NULL;
                        }
                    }
                    
                    if (nested && nested->data.class_data.members) {
                        /* Walk the chain of inner classes */
                        while (rest && nested) {
                            const char *next_dot = strchr(rest, '.');
                            char *inner_name;
                            if (next_dot) {
                                inner_name = strndup(rest, next_dot - rest);
                                rest = next_dot + 1;
                            } else {
                                inner_name = strdup(rest);
                                rest = NULL;
                            }
                            
                            symbol_t *inner = scope_lookup_local(
                                nested->data.class_data.members, inner_name);
                            free(inner_name);
                            
                            if (inner && (inner->kind == SYM_CLASS || inner->kind == SYM_INTERFACE ||
                                         inner->kind == SYM_ENUM || inner->kind == SYM_RECORD)) {
                                nested = inner;
                            } else {
                                nested = NULL;
                            }
                        }
                        
                        if (nested && nested->type) {
                            free(outer_name);
                            type_node->sem_type = nested->type;
                            return nested->type;
                        }
                    }
                    free(outer_name);
                }
                
                /* Check if it's a nested class of the enclosing class (for inner classes) */
                if (sem->current_class && sem->current_class->data.class_data.enclosing_class) {
                    symbol_t *enclosing = sem->current_class->data.class_data.enclosing_class;
                    if (enclosing->data.class_data.members) {
                        symbol_t *sibling = scope_lookup_local(
                            enclosing->data.class_data.members, name);
                        if (sibling && (sibling->kind == SYM_CLASS || sibling->kind == SYM_INTERFACE ||
                                        sibling->kind == SYM_ENUM || sibling->kind == SYM_RECORD)) {
                            if (sibling->type) {
                                type_node->sem_type = sibling->type;
                                return sibling->type;
                            }
                        }
                    }
                }
                
                /* Check current scope for local classes (defined inside method bodies) */
                if (sem->current_scope) {
                    symbol_t *local = scope_lookup(sem->current_scope, name);
                    if (local && (local->kind == SYM_CLASS || local->kind == SYM_INTERFACE ||
                                  local->kind == SYM_ENUM || local->kind == SYM_RECORD)) {
                        if (local->type) {
                            /* Check for type arguments */
                            slist_t *children = type_node->data.node.children;
                            if (children && local->type->kind == TYPE_CLASS) {
                                /* Has type arguments - create parameterized type */
                                type_t *param_type = type_new_class(local->type->data.class_type.name ? 
                                    local->type->data.class_type.name : name);
                                param_type->data.class_type.symbol = local;
                                
                                /* Resolve each type argument */
                                for (slist_t *c = children; c; c = c->next) {
                                    ast_node_t *arg = (ast_node_t *)c->data;
                                    type_t *arg_type = semantic_resolve_type(sem, arg);
                                    if (param_type->data.class_type.type_args == NULL) {
                                        param_type->data.class_type.type_args = slist_new(arg_type);
                                    } else {
                                        slist_append(param_type->data.class_type.type_args, arg_type);
                                    }
                                }
                                
                                type_node->sem_type = param_type;
                                return param_type;
                            }
                            type_node->sem_type = local->type;
                            return local->type;
                        }
                    }
                }
                
                /* Check per-compilation-unit type scope first (like javac's toplevelScope).
                 * This contains types defined in the current file and imported types,
                 * keyed by simple name. For parallel compilation, each thread has its own
                 * unit_types, avoiding race conditions. */
                type_t *cached = hashtable_lookup(sem->unit_types, name);
                
                /* If not in unit scope, check global cache with qualified name.
                 * The global cache only uses qualified names to avoid conflicts. */
                if (!cached) {
                    cached = hashtable_lookup(sem->types, name);
                }
                
                if (cached) {
                    /* Check for diamond operator - infer type args from target type */
                    bool has_diamond = (type_node->data.node.flags & 0x1000) != 0;
                    if (has_diamond && sem->target_type && 
                        sem->target_type->kind == TYPE_CLASS &&
                        sem->target_type->data.class_type.type_args &&
                        cached->kind == TYPE_CLASS) {
                        /* Diamond inference: use target type's arguments */
                        const char *cached_name = cached->data.class_type.name ? 
                                                  cached->data.class_type.name : name;
                        type_t *param_type = type_new_class(cached_name);
                        param_type->data.class_type.symbol = cached->data.class_type.symbol;
                        
                        /* Copy type arguments from target type */
                        for (slist_t *c = sem->target_type->data.class_type.type_args; c; c = c->next) {
                            type_t *arg_type = (type_t *)c->data;
                            if (param_type->data.class_type.type_args == NULL) {
                                param_type->data.class_type.type_args = slist_new(arg_type);
                            } else {
                                slist_append(param_type->data.class_type.type_args, arg_type);
                            }
                        }
                        
                        type_node->sem_type = param_type;
                        return param_type;
                    }
                    
                    /* Even if cached, check for type arguments */
                    slist_t *children = type_node->data.node.children;
                    if (children && cached->kind == TYPE_CLASS) {
                        /* Has type arguments - create parameterized type */
                        /* Use the cached type's name (which should be fully qualified) */
                        const char *cached_name = cached->data.class_type.name ? 
                                                  cached->data.class_type.name : name;
                        type_t *param_type = type_new_class(cached_name);
                        param_type->data.class_type.symbol = cached->data.class_type.symbol;
                        
                        /* Resolve each type argument */
                        for (slist_t *c = children; c; c = c->next) {
                            ast_node_t *arg = (ast_node_t *)c->data;
                            type_t *arg_type = semantic_resolve_type(sem, arg);
                            if (param_type->data.class_type.type_args == NULL) {
                                param_type->data.class_type.type_args = slist_new(arg_type);
                            } else {
                                slist_append(param_type->data.class_type.type_args, arg_type);
                            }
                        }
                        
                        type_node->sem_type = param_type;
                        return param_type;
                    }
                    type_node->sem_type = cached;
                    return cached;
                }
                
                /* Resolve simple name to qualified name using imports */
                char *qualified = resolve_import(sem, name);
                if (qualified) {
                    /* Check cache with qualified name */
                    cached = hashtable_lookup(sem->types, qualified);
                    if (cached) {
                        /* Check for diamond operator - infer type args from target type */
                        bool has_diamond = (type_node->data.node.flags & 0x1000) != 0;
                        if (has_diamond && sem->target_type && 
                            sem->target_type->kind == TYPE_CLASS &&
                            sem->target_type->data.class_type.type_args) {
                            /* Diamond inference: use target type's arguments */
                            type_t *param_type = type_new_class(qualified);
                            param_type->data.class_type.symbol = cached->data.class_type.symbol;
                            
                            /* Copy type arguments from target type */
                            for (slist_t *c = sem->target_type->data.class_type.type_args; c; c = c->next) {
                                type_t *arg_type = (type_t *)c->data;
                                if (param_type->data.class_type.type_args == NULL) {
                                    param_type->data.class_type.type_args = slist_new(arg_type);
                                } else {
                                    slist_append(param_type->data.class_type.type_args, arg_type);
                                }
                            }
                            
                            type_node->sem_type = param_type;
                            free(qualified);
                            return param_type;
                        }
                        
                        /* Even if cached, check for type arguments */
                        slist_t *children = type_node->data.node.children;
                        if (children) {
                            /* Has type arguments - create parameterized type */
                            type_t *param_type = type_new_class(qualified);
                            param_type->data.class_type.symbol = cached->data.class_type.symbol;
                            
                            /* Resolve each type argument */
                            for (slist_t *c = children; c; c = c->next) {
                                ast_node_t *arg = (ast_node_t *)c->data;
                                type_t *arg_type = semantic_resolve_type(sem, arg);
                                if (param_type->data.class_type.type_args == NULL) {
                                    param_type->data.class_type.type_args = slist_new(arg_type);
                                } else {
                                    slist_append(param_type->data.class_type.type_args, arg_type);
                                }
                            }
                            
                            type_node->sem_type = param_type;
                            free(qualified);
                            return param_type;
                        }
                        type_node->sem_type = cached;
                        free(qualified);
                        return cached;
                    }
                    
                    /* Try loading from classpath */
                    if (sem->classpath) {
                        symbol_t *sym = load_external_class(sem, qualified);
                        if (sym && sym->type) {
                            /* Check for diamond operator - infer type args from target type */
                            bool has_diamond = (type_node->data.node.flags & 0x1000) != 0;
                            if (has_diamond && sem->target_type && 
                                sem->target_type->kind == TYPE_CLASS &&
                                sem->target_type->data.class_type.type_args) {
                                /* Diamond inference: use target type's arguments */
                                type_t *param_type = type_new_class(qualified);
                                param_type->data.class_type.symbol = sym;
                                
                                /* Copy type arguments from target type */
                                for (slist_t *c = sem->target_type->data.class_type.type_args; c; c = c->next) {
                                    type_t *arg_type = (type_t *)c->data;
                                    if (param_type->data.class_type.type_args == NULL) {
                                        param_type->data.class_type.type_args = slist_new(arg_type);
                                    } else {
                                        slist_append(param_type->data.class_type.type_args, arg_type);
                                    }
                                }
                                
                                type_node->sem_type = param_type;
                                free(qualified);
                                return param_type;
                            }
                            
                            /* Check for type arguments (generics) */
                            slist_t *children = type_node->data.node.children;
                            if (children) {
                                /* Has type arguments - create parameterized type */
                                type_t *param_type = type_new_class(qualified);
                                param_type->data.class_type.symbol = sym;
                                
                                /* Resolve each type argument */
                                for (slist_t *c = children; c; c = c->next) {
                                    ast_node_t *arg = (ast_node_t *)c->data;
                                    type_t *arg_type = semantic_resolve_type(sem, arg);
                                    if (param_type->data.class_type.type_args == NULL) {
                                        param_type->data.class_type.type_args = slist_new(arg_type);
                                    } else {
                                        slist_append(param_type->data.class_type.type_args, arg_type);
                                    }
                                }
                                
                                type_node->sem_type = param_type;
                                free(qualified);
                                return param_type;
                            }
                            
                            /* No type arguments - cache by qualified name globally,
                             * simple name in per-compilation-unit scope */
                            hashtable_insert(sem->types, qualified, sym->type);
                            hashtable_insert(sem->unit_types, name, sym->type);
                            type_node->sem_type = sym->type;
                            free(qualified);
                            return sym->type;
                        }
                    }
                    
                    /* Create type with qualified name - cache appropriately */
                    type_t *type = type_new_class(qualified);
                    hashtable_insert(sem->types, qualified, type);
                    hashtable_insert(sem->unit_types, name, type);
                    type_node->sem_type = type;
                    free(qualified);
                    return type;
                }
                
                /* No import found - try as-is (might be already qualified) */
                if (sem->classpath) {
                    symbol_t *sym = load_external_class(sem, name);
                    if (sym && sym->type) {
                        return sym->type;
                    }
                }
                
                /* Check if it's a locally-defined class in current scope */
                symbol_t *local_class = scope_lookup(sem->current_scope, name);
                if (local_class && (local_class->kind == SYM_CLASS || 
                                     local_class->kind == SYM_INTERFACE ||
                                     local_class->kind == SYM_ENUM ||
                                     local_class->kind == SYM_RECORD) && 
                    local_class->type) {
                    /* Ensure the type's symbol pointer is set */
                    if (!local_class->type->data.class_type.symbol) {
                        local_class->type->data.class_type.symbol = local_class;
                    }
                    /* Cache in per-compilation-unit scope and return */
                    hashtable_insert(sem->unit_types, name, local_class->type);
                    type_node->sem_type = local_class->type;
                    return local_class->type;
                }
                
                /* Check if it's a type parameter (generics) */
                symbol_t *type_param = scope_lookup(sem->current_scope, name);
                if (type_param && type_param->kind == SYM_TYPE_PARAM && type_param->type) {
                    type_node->sem_type = type_param->type;
                    return type_param->type;
                }
                
                /* Last resort: scan the package directory for the type.
                 * This handles package-private types defined in other files
                 * within the same package (like javac's ClassFinder.fillIn). */
                if (sem->current_package) {
                    symbol_t *pkg_sym = scan_package_for_type(sem, name, sem->current_package);
                    if (pkg_sym && pkg_sym->type) {
                        /* Cache in per-compilation-unit scope */
                        if (!hashtable_lookup(sem->unit_types, name)) {
                            hashtable_insert(sem->unit_types, name, pkg_sym->type);
                        }
                        type_node->sem_type = pkg_sym->type;
                        return pkg_sym->type;
                    }
                }
                
                /* Type cannot be resolved - report error */
                semantic_error(sem, type_node->line, type_node->column,
                              "Cannot resolve type: %s", name);
                type_t *type = type_new_class(name);
                /* Cache error type in unit_types to avoid repeated errors */
                hashtable_insert(sem->unit_types, name, type);
                return type;
            }
        
        case AST_ARRAY_TYPE:
            {
                /* First child is element type */
                slist_t *children = type_node->data.node.children;
                if (!children) {
                    semantic_error(sem, type_node->line, type_node->column,
                                  "Array type has no element type");
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                type_t *element = semantic_resolve_type(sem, children->data);
                int dims = 1;
                
                /* Count nested array dimensions */
                while (element && element->kind == TYPE_ARRAY) {
                    dims += element->data.array_type.dimensions;
                    element = element->data.array_type.element_type;
                }
                
                return type_new_array(element, dims);
            }
        
        case AST_WILDCARD_TYPE:
            {
                /* Wildcard type: ?, ? extends T, ? super T */
                const char *bound_name = type_node->data.node.name;
                type_t *bound = NULL;
                int bound_kind = 0;  /* 0=unbounded */
                
                if (bound_name && strcmp(bound_name, "extends") == 0) {
                    bound_kind = 1;
                    /* First child is bound type */
                    if (type_node->data.node.children) {
                        bound = semantic_resolve_type(sem, type_node->data.node.children->data);
                    }
                } else if (bound_name && strcmp(bound_name, "super") == 0) {
                    bound_kind = -1;
                    /* First child is bound type */
                    if (type_node->data.node.children) {
                        bound = semantic_resolve_type(sem, type_node->data.node.children->data);
                    }
                }
                
                return type_new_wildcard(bound_kind, bound);
            }
        
        case AST_VAR_TYPE:
            /* var type - type inference required by caller */
            /* Return unknown type; caller must infer from context */
            return type_new_primitive(TYPE_UNKNOWN);
        
        case AST_IDENTIFIER:
            {
                /* Identifier used as a type - resolve as a class name */
                const char *name = type_node->data.leaf.name;
                
                /* First try local scope lookup for type parameters */
                symbol_t *sym = scope_lookup(sem->current_scope, name);
                
                if (sym && sym->kind == SYM_TYPE_PARAM && sym->type) {
                    type_node->sem_type = sym->type;
                    return sym->type;
                }
                
                /* Try local scope lookup for classes/interfaces */
                if (sym && (sym->kind == SYM_CLASS || sym->kind == SYM_INTERFACE ||
                            sym->kind == SYM_ENUM || sym->kind == SYM_RECORD)) {
                    type_t *type = type_new_class(sym->qualified_name ? 
                                                  sym->qualified_name : sym->name);
                    type->data.class_type.symbol = sym;
                    type_node->sem_type = type;
                    return type;
                }
                
                /* Try import resolution (including java.lang.*) */
                char *qualified = resolve_import(sem, name);
                if (qualified) {
                    sym = load_external_class(sem, qualified);
                    if (sym) {
                        type_t *type = type_new_class(qualified);
                        type->data.class_type.symbol = sym;
                        type_node->sem_type = type;
                        free(qualified);
                        return type;
                    }
                    free(qualified);
                }
                
                semantic_error(sem, type_node->line, type_node->column,
                              "Cannot resolve type: %s", name);
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_INTERSECTION_TYPE:
            {
                /* Intersection type: Type1 & Type2 & ...
                 * Resolve each component type and create intersection type */
                slist_t *types = NULL;
                slist_t *children = type_node->data.node.children;
                
                for (slist_t *c = children; c; c = c->next) {
                    ast_node_t *child = (ast_node_t *)c->data;
                    type_t *component = semantic_resolve_type(sem, child);
                    
                    if (!types) {
                        types = slist_new(component);
                    } else {
                        slist_append(types, component);
                    }
                }
                
                type_t *intersection = type_new_intersection(types);
                type_node->sem_type = intersection;
                return intersection;
            }
        
        case AST_FIELD_ACCESS:
            {
                /* Qualified type name (e.g., java.io.File, Map.Entry)
                 * Build the full qualified name from the field access chain */
                char fqn[1024];
                fqn[0] = '\0';
                
                /* Traverse the chain to build the qualified name */
                ast_node_t *node = type_node;
                slist_t *parts = NULL;
                
                while (node) {
                    if (node->type == AST_FIELD_ACCESS) {
                        if (node->data.node.name) {
                            parts = slist_prepend(parts, (void *)node->data.node.name);
                        }
                        /* Get the child (the receiver) */
                        slist_t *children = node->data.node.children;
                        node = children ? (ast_node_t *)children->data : NULL;
                    } else if (node->type == AST_IDENTIFIER) {
                        parts = slist_prepend(parts, (void *)node->data.leaf.name);
                        break;
                    } else {
                        break;
                    }
                }
                
                /* Build qualified name from parts */
                for (slist_t *p = parts; p; p = p->next) {
                    const char *part = (const char *)p->data;
                    if (fqn[0] != '\0') {
                        strcat(fqn, ".");
                    }
                    strcat(fqn, part);
                }
                slist_free(parts);
                
                if (fqn[0] == '\0') {
                    semantic_error(sem, type_node->line, type_node->column,
                                  "Cannot resolve type from field access");
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                /* Try to resolve as a class name */
                symbol_t *sym = load_external_class(sem, fqn);
                if (sym) {
                    type_t *type = type_new_class(fqn);
                    type->data.class_type.symbol = sym;
                    type_node->sem_type = type;
                    return type;
                }
                
                /* Try inner class resolution (using $ for inner classes) */
                char *inner_fqn = strdup(fqn);
                for (char *p = inner_fqn; *p; p++) {
                    if (*p == '.') {
                        *p = '$';
                        sym = load_external_class(sem, inner_fqn);
                        if (sym) {
                            type_t *type = type_new_class(inner_fqn);
                            type->data.class_type.symbol = sym;
                            type_node->sem_type = type;
                            free(inner_fqn);
                            return type;
                        }
                        *p = '.';  /* Restore for next iteration */
                    }
                }
                free(inner_fqn);
                
                semantic_error(sem, type_node->line, type_node->column,
                              "Cannot resolve type: %s", fqn);
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        default:
            semantic_error(sem, type_node->line, type_node->column,
                          "Invalid type node: %s", ast_type_name(type_node->type));
            return type_new_primitive(TYPE_UNKNOWN);
    }
}

/**
 * Resolve a simple name to a symbol.
 */
symbol_t *semantic_resolve_name(semantic_t *sem, const char *name)
{
    return scope_lookup(sem->current_scope, name);
}

/* ========================================================================
 * Iterative AST Walking - Analysis Passes
 * ======================================================================== */

/* Walk state for iterative traversal */
typedef enum walk_state
{
    WALK_ENTER,             /* First visit to node */
    WALK_CHILDREN,          /* Processing children */
    WALK_EXIT               /* Leaving node */
} walk_state_t;

/* Stack frame for iterative walking */
typedef struct walk_frame
{
    ast_node_t *node;
    walk_state_t state;
    slist_t *child_iter;    /* Current child being processed */
    scope_t *saved_scope;   /* Scope to restore on exit */
    symbol_t *saved_class;  /* Class to restore on exit */
    symbol_t *saved_method; /* Method to restore on exit */
    void *extra;            /* Extra data for specific passes */
} walk_frame_t;

/**
 * Create a walk frame.
 */
static walk_frame_t *walk_frame_new(ast_node_t *node)
{
    walk_frame_t *frame = calloc(1, sizeof(walk_frame_t));
    if (!frame) {
        return NULL;
    }
    frame->node = node;
    frame->state = WALK_ENTER;
    return frame;
}

/**
 * Get children list for a node (handles leaf vs internal nodes).
 */
static slist_t *ast_get_children(ast_node_t *node)
{
    if (!node) {
        return NULL;
    }
    
    switch (node->type) {
        case AST_LITERAL:
        case AST_IDENTIFIER:
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
        case AST_PRIMITIVE_TYPE:
        case AST_VAR_TYPE:
        case AST_EMPTY_STMT:
        case AST_BREAK_STMT:
        case AST_CONTINUE_STMT:
            /* Leaf nodes */
            return NULL;
        default:
            return node->data.node.children;
    }
}

/* ========================================================================
 * Pattern Variable Definition (for pattern matching)
 * 
 * Recursively processes patterns and defines pattern variables in scope.
 * Handles type patterns, record patterns, and unnamed patterns.
 * ======================================================================== */

static void semantic_define_pattern_vars(semantic_t *sem, ast_node_t *pattern, scope_t *scope)
{
    if (!pattern) return;
    
    if (pattern->type == AST_TYPE_PATTERN) {
        /* Type pattern: Type varName - define varName with Type */
        const char *var_name = pattern->data.node.name;
        slist_t *pattern_children = pattern->data.node.children;
        if (pattern_children) {
            ast_node_t *type_node = (ast_node_t *)pattern_children->data;
            type_t *pattern_type = semantic_resolve_type(sem, type_node);
            
            /* Store type on pattern for codegen */
            pattern->sem_type = pattern_type;
            
            if (var_name) {
                /* Create pattern variable symbol */
                symbol_t *sym = symbol_new(SYM_LOCAL_VAR, var_name);
                sym->type = pattern_type;
                sym->modifiers = MOD_FINAL;
                sym->ast = pattern;
                sym->line = pattern->line;
                sym->column = pattern->column;
                
                pattern->sem_symbol = sym;
                scope_define(scope, sym);
            }
        }
    } else if (pattern->type == AST_RECORD_PATTERN) {
        /* Record pattern: RecordType(p1, p2, ...) */
        slist_t *children = pattern->data.node.children;
        if (!children) return;
        
        /* First child is the record type */
        ast_node_t *type_node = (ast_node_t *)children->data;
        type_t *record_type = semantic_resolve_type(sem, type_node);
        pattern->sem_type = record_type;
        
        /* Recursively process component patterns */
        int comp_index = 0;
        for (slist_t *c = children->next; c; c = c->next, comp_index++) {
            ast_node_t *comp_pattern = (ast_node_t *)c->data;
            
            /* For type patterns without explicit type, infer from record component */
            if (comp_pattern->type == AST_TYPE_PATTERN && 
                comp_pattern->data.node.name &&
                !comp_pattern->data.node.children) {
                /* This is just a variable name - infer type from record component */
                /* For now, skip this case - we require explicit types */
            }
            
            /* Recursively define variables from this component pattern */
            semantic_define_pattern_vars(sem, comp_pattern, scope);
        }
    }
    /* AST_UNNAMED_PATTERN has no variables to define */
}

/* ========================================================================
 * Pass 1: Collect Declarations
 * 
 * Walks the AST and registers all type declarations (classes, interfaces)
 * and their members (fields, methods) in the symbol table.
 * ======================================================================== */

static void pass1_collect_declarations(semantic_t *sem, ast_node_t *ast)
{
    if (!ast) {
        return;
    }
    
    /* Stack for iterative traversal */
    int stack_capacity = 256;
    int stack_top = 0;
    walk_frame_t **stack = malloc(sizeof(walk_frame_t *) * stack_capacity);
    
    stack[stack_top++] = walk_frame_new(ast);
    
    while (stack_top > 0) {
        walk_frame_t *frame = stack[stack_top - 1];
        ast_node_t *node = frame->node;
        
        if (!node) {
            free(stack[--stack_top]);
            continue;
        }
        
        switch (frame->state) {
            case WALK_ENTER:
                /* Process node on entry */
                switch (node->type) {
                    case AST_PACKAGE_DECL:
                        free(sem->current_package);
                        sem->current_package = node->data.node.name 
                            ? strdup(node->data.node.name) : NULL;
                        break;
                    
                    case AST_IMPORT_DECL:
                        /* Store imports for later resolution */
                        if (node->data.node.flags & MOD_STATIC) {
                            /* Static import - store separately */
                            if (!sem->static_imports) {
                                sem->static_imports = slist_new(node);
                            } else {
                                slist_append(sem->static_imports, node);
                            }
                        } else {
                            /* Regular import */
                            if (!sem->imports) {
                                sem->imports = slist_new(node);
                            } else {
                                slist_append(sem->imports, node);
                            }
                        }
                        break;
                    
                    case AST_MODULE_DECL:
                    case AST_REQUIRES_DIRECTIVE:
                    case AST_EXPORTS_DIRECTIVE:
                    case AST_OPENS_DIRECTIVE:
                    case AST_USES_DIRECTIVE:
                    case AST_PROVIDES_DIRECTIVE:
                        /* Module declarations don't need semantic analysis */
                        /* Skip children by not setting WALK_CHILDREN */
                        frame->state = WALK_EXIT;
                        continue;
                    
                    case AST_CLASS_DECL:
                    case AST_INTERFACE_DECL:
                    case AST_ENUM_DECL:
                    case AST_RECORD_DECL:
                    case AST_ANNOTATION_DECL:
                        {
                            const char *name = node->data.node.name;
                            symbol_kind_t kind = SYM_CLASS;
                            if (node->type == AST_INTERFACE_DECL) {
                                kind = SYM_INTERFACE;
                            }
                            if (node->type == AST_ENUM_DECL) {
                                kind = SYM_ENUM;
                            }
                            if (node->type == AST_RECORD_DECL) {
                                kind = SYM_RECORD;
                                /* Records require Java 16+ */
                                if (sem->source_version < 16) {
                                    semantic_error(sem, node->line, node->column,
                                        "records are not supported in -source %d (use 16 or higher)",
                                        sem->source_version);
                                }
                            }
                            if (node->type == AST_ANNOTATION_DECL) {
                                kind = SYM_ANNOTATION;
                            }
                            
                            /* Sealed classes require Java 17+ */
                            if (node->data.node.flags & MOD_SEALED) {
                                if (sem->source_version < 17) {
                                    semantic_error(sem, node->line, node->column,
                                        "sealed classes are not supported in -source %d (use 17 or higher)",
                                        sem->source_version);
                                }
                            }
                            
                            /* @FunctionalInterface is only valid on interfaces */
                            if (kind != SYM_INTERFACE &&
                                has_annotation(node, "FunctionalInterface", "java.lang.FunctionalInterface")) {
                                semantic_error(sem, node->line, node->column,
                                    "Unexpected @FunctionalInterface annotation");
                            }
                            
                            /* Check if this class was pre-registered (for forward reference support) */
                            symbol_t *sym = node->sem_symbol;
                            bool was_preregistered = (sym != NULL);
                            
                            if (!sym) {
                                sym = symbol_new(kind, name);
                            }
                            sym->modifiers = node->data.node.flags;
                            
                            /* Check for illegal modifiers on top-level classes */
                            if (!sem->current_class) {
                                /* This is a top-level class */
                                if (sym->modifiers & MOD_STATIC) {
                                    semantic_error(sem, node->line, node->column,
                                        "modifier 'static' not allowed on top-level class");
                                }
                                if (sym->modifiers & MOD_PRIVATE) {
                                    semantic_error(sem, node->line, node->column,
                                        "modifier 'private' not allowed on top-level class");
                                }
                                if (sym->modifiers & MOD_PROTECTED) {
                                    semantic_error(sem, node->line, node->column,
                                        "modifier 'protected' not allowed on top-level class");
                                }
                                
                                /* Public class must match filename */
                                if ((sym->modifiers & MOD_PUBLIC) && sem->source && sem->source->filename) {
                                    /* Extract basename from filename */
                                    const char *filepath = sem->source->filename;
                                    const char *basename = strrchr(filepath, '/');
                                    if (!basename) basename = strrchr(filepath, '\\');
                                    basename = basename ? basename + 1 : filepath;
                                    
                                    /* Remove .java extension */
                                    size_t baselen = strlen(basename);
                                    if (baselen > 5 && strcmp(basename + baselen - 5, ".java") == 0) {
                                        char expected[256];
                                        size_t namelen = baselen - 5;
                                        if (namelen < sizeof(expected)) {
                                            strncpy(expected, basename, namelen);
                                            expected[namelen] = '\0';
                                            if (strcmp(expected, name) != 0) {
                                                semantic_error(sem, node->line, node->column,
                                                    "class %s is public, should be declared in a file named %s.java",
                                                    name, name);
                                            }
                                        }
                                    }
                                }
                            }
                            
                            /* Records are implicitly final */
                            if (kind == SYM_RECORD) {
                                sym->modifiers |= MOD_FINAL;
                                node->data.node.flags |= MOD_FINAL;
                            }
                            
                            /* Nested enums, interfaces, and records are implicitly static */
                            if (sem->current_class && 
                                (kind == SYM_ENUM || kind == SYM_INTERFACE || kind == SYM_RECORD)) {
                                sym->modifiers |= MOD_STATIC;
                                node->data.node.flags |= MOD_STATIC;
                            }
                            
                            sym->ast = node;
                            sym->line = node->line;
                            sym->column = node->column;
                            
                            /* Build qualified name (skip if pre-registered - already has correct name) */
                            if (!was_preregistered) {
                            if (sem->current_method && sem->current_class) {
                                /* Local class inside a method - use OuterClass$NClassName format.
                                 * The number N is per-name: unique names all get $1, 
                                 * duplicate names get $1, $2, etc. (like javac) */
                                const char *outer_qname = sem->current_class->qualified_name;
                                
                                /* Initialize per-name counter hashtable if needed */
                                if (!sem->current_class->data.class_data.local_class_name_counts) {
                                    sem->current_class->data.class_data.local_class_name_counts = hashtable_new();
                                }
                                
                                /* Look up or initialize count for this name */
                                int *count_ptr = (int *)hashtable_lookup(
                                    sem->current_class->data.class_data.local_class_name_counts, name);
                                int local_id;
                                if (count_ptr) {
                                    local_id = ++(*count_ptr);
                                } else {
                                    int *new_count = malloc(sizeof(int));
                                    *new_count = 1;
                                    hashtable_insert(sem->current_class->data.class_data.local_class_name_counts, 
                                                     name, new_count);
                                    local_id = 1;
                                }
                                
                                size_t len = strlen(outer_qname) + strlen(name) + 12; /* room for $N */
                                sym->qualified_name = malloc(len);
                                snprintf(sym->qualified_name, len, "%s$%d%s", outer_qname, local_id, name);
                                sym->data.class_data.enclosing_class = sem->current_class;
                                sym->data.class_data.enclosing_method = sem->current_method;
                                sym->data.class_data.is_local_class = true;
                            } else if (sem->current_class) {
                                /* Nested class - use OuterClass$NestedClass format */
                                const char *outer_qname = sem->current_class->qualified_name;
                                size_t len = strlen(outer_qname) + strlen(name) + 2;
                                sym->qualified_name = malloc(len);
                                snprintf(sym->qualified_name, len, "%s$%s", outer_qname, name);
                                sym->data.class_data.enclosing_class = sem->current_class;
                            } else if (sem->current_package) {
                                size_t len = strlen(sem->current_package) + strlen(name) + 2;
                                sym->qualified_name = malloc(len);
                                snprintf(sym->qualified_name, len, "%s.%s", 
                                        sem->current_package, name);
                            } else {
                                sym->qualified_name = strdup(name);
                            }
                            
                            /* Create type for this class */
                            type_t *type = type_new_class(sym->qualified_name);
                            type->data.class_type.symbol = sym;
                            sym->type = type;
                            
                            /* Cache the type - qualified name in global cache,
                             * simple name in per-compilation-unit scope */
                            if (getenv("GENESIS_DEBUG_LOAD")) {
                                fprintf(stderr, "DEBUG main pass: caching type '%s' (sym=%p, members=%p)\n",
                                    sym->qualified_name, (void*)sym, (void*)sym->data.class_data.members);
                            }
                            hashtable_insert(sem->types, sym->qualified_name, type);
                            /* For local classes, don't cache by short name - they are only 
                             * visible in their local scope and different methods can have
                             * local classes with the same short name */
                            if (!sym->data.class_data.is_local_class) {
                                hashtable_insert(sem->unit_types, name, type);
                            }
                            
                            } /* End of !was_preregistered */
                            
                            /* Store symbol reference on AST node for code generation */
                            node->sem_symbol = sym;
                            
                            /* For pre-registered classes (nested class forward refs), skip scope_define
                             * and class_scope creation - they were already done during pre-registration */
                            scope_t *class_scope;
                            if (was_preregistered) {
                                class_scope = sym->data.class_data.members;
                            } else {
                            /* For local classes, skip the duplicate check.
                             * Same-named local classes in different scopes are allowed
                             * (e.g., Local in method body and Local in anonymous class).
                             * They will get unique qualified names later ($1Local vs $1$1Local).
                             * Just define without checking for duplicates. */
                            if (sym->data.class_data.is_local_class) {
                                /* Don't use scope_define which checks for duplicates.
                                 * Just insert directly (allows shadowing in same scope). */
                                hashtable_insert(sem->current_scope->symbols, name, sym);
                                sym->scope = sem->current_scope;
                            } else if (!scope_define(sem->current_scope, sym)) {
                                semantic_error(sem, node->line, node->column,
                                              "Duplicate class declaration: %s", name);
                            }
                            
                            /* Create class member scope - parent is current scope so that
                             * local classes can access variables from enclosing block scopes */
                                class_scope = scope_new(SCOPE_CLASS, sem->current_scope);
                            class_scope->owner = sym;
                            sym->data.class_data.members = class_scope;
                            }
                            
                            /* For enums, pre-assign ordinals to enum constants so they're available
                             * during later semantic analysis (e.g., in switch statements) */
                            if (kind == SYM_ENUM) {
                                int ordinal = 0;
                                for (slist_t *enum_scan = node->data.node.children; enum_scan; enum_scan = enum_scan->next) {
                                    ast_node_t *child = (ast_node_t *)enum_scan->data;
                                    if (child && child->type == AST_ENUM_CONSTANT) {
                                        /* Store ordinal on the AST node for later use when creating the symbol */
                                        child->data.node.flags = ordinal;  /* Reuse flags field to store ordinal temporarily */
                                        ordinal++;
                                    }
                                }
                            }
                            
                            /* Pre-register all nested class/interface declarations recursively
                             * so that forward references between siblings can be resolved.
                             * E.g., interface SAM { Foo m(); } class Foo {} - SAM needs to see Foo
                             * This also handles deeply nested types like Outer.Inner.Deeper. */
                            preregister_nested_types(sem, node, sym, class_scope);
                            
                            /* Pre-pass: resolve extends/implements BEFORE processing methods.
                             * This ensures nested types from interfaces are accessible when
                             * resolving method return types. Like javac's MemberEnter phase.
                             * We just load them here without adding - the main pass will add. */
                            for (slist_t *prescan = node->data.node.children; prescan; prescan = prescan->next) {
                                ast_node_t *child = (ast_node_t *)prescan->data;
                                if (child && (child->type == AST_CLASS_TYPE ||
                                              child->type == AST_PRIMITIVE_TYPE ||
                                              child->type == AST_ARRAY_TYPE) &&
                                    child->data.node.flags == 2) {
                                    /* implements - pre-load the interface symbol and cache on AST node.
                                     * This ensures nested types in the interface are accessible
                                     * when resolving method return types later. */
                                    const char *iface_name = child->data.node.name;
                                    symbol_t *iface_sym = scope_lookup(sem->current_scope, iface_name);
                                    if (!iface_sym) {
                                        char *qualified = resolve_import(sem, iface_name);
                                        if (qualified) {
                                            iface_sym = load_external_class(sem, qualified);
                                            free(qualified);
                                        }
                                        if (!iface_sym) {
                                            iface_sym = load_external_class(sem, iface_name);
                                        }
                                    }
                                    /* Store on AST node for the main pass to use */
                                    if (iface_sym && !child->sem_symbol) {
                                        child->sem_symbol = iface_sym;
                                    }
                                    /* Also add to interfaces list now so nested types are accessible */
                                    if (iface_sym && (iface_sym->kind == SYM_INTERFACE ||
                                                      iface_sym->kind == SYM_CLASS)) {
                                        bool dup = false;
                                        for (slist_t *i = sym->data.class_data.interfaces; i; i = i->next) {
                                            if (i->data == iface_sym) { dup = true; break; }
                                        }
                                        if (!dup) {
                                            if (!sym->data.class_data.interfaces) {
                                                sym->data.class_data.interfaces = slist_new(iface_sym);
                                            } else {
                                                slist_append(sym->data.class_data.interfaces, iface_sym);
                                            }
                                        }
                                    }
                                }
                            }
                            
                            /* Process type parameters (generics) using two-pass approach
                             * to handle self-referencing bounds like K, M extends Map<K, ?> */
                            slist_t *type_param_symbols = NULL;  /* Track symbols for second pass */
                            slist_t *type_param_ast_nodes = NULL;  /* Track AST nodes for second pass */
                            
                            /* Pass 1: Add all type params to class_scope with Object bound */
                            slist_t *children = node->data.node.children;
                            while (children) {
                                ast_node_t *child = (ast_node_t *)children->data;
                                if (child->type == AST_TYPE_PARAMETER) {
                                    const char *param_name = child->data.node.name;
                                    if (param_name) {
                                        /* Check for duplicate type parameter */
                                        if (sym->data.class_data.type_params) {
                                            for (slist_t *tp = sym->data.class_data.type_params; tp; tp = tp->next) {
                                                symbol_t *existing = (symbol_t *)tp->data;
                                                if (existing && existing->name && 
                                                    strcmp(existing->name, param_name) == 0) {
                                                    semantic_error(sem, child->line, child->column,
                                                        "Duplicate type parameter: '%s'", param_name);
                                                    break;
                                                }
                                            }
                                        }
                                        
                                        /* Create a type parameter symbol */
                                        symbol_t *type_param = symbol_new(SYM_TYPE_PARAM, param_name);
                                        type_param->ast = child;
                                        type_param->line = child->line;
                                        type_param->column = child->column;
                                        
                                        /* Create a type variable type with Object bound initially */
                                        type_t *type_var = malloc(sizeof(type_t));
                                        type_var->kind = TYPE_TYPEVAR;
                                        type_var->data.type_var.name = strdup(param_name);
                                            type_var->data.type_var.bound = type_new_class("java.lang.Object");
                                        
                                        type_param->type = type_var;
                                        
                                        /* Add to class's type parameters list */
                                        if (!sym->data.class_data.type_params) {
                                            sym->data.class_data.type_params = slist_new(type_param);
                                        } else {
                                            slist_append(sym->data.class_data.type_params, type_param);
                                        }
                                        
                                        /* Define in class scope so it can be looked up */
                                        scope_define(class_scope, type_param);
                                        
                                        /* Track for second pass */
                                        if (!type_param_symbols) {
                                            type_param_symbols = slist_new(type_param);
                                            type_param_ast_nodes = slist_new(child);
                                        } else {
                                            slist_append(type_param_symbols, type_param);
                                            slist_append(type_param_ast_nodes, child);
                                        }
                                    }
                                }
                                children = children->next;
                            }
                            
                            /* Set scope to class_scope so type param lookups work */
                            scope_t *saved_scope_for_tparams = sem->current_scope;
                            sem->current_scope = class_scope;
                            
                            /* Pass 2: Resolve actual bounds (now all type params are in scope) */
                            slist_t *sym_iter = type_param_symbols;
                            slist_t *ast_iter = type_param_ast_nodes;
                            while (sym_iter && ast_iter) {
                                symbol_t *type_param = (symbol_t *)sym_iter->data;
                                ast_node_t *tp_ast = (ast_node_t *)ast_iter->data;
                                
                                if (tp_ast->data.node.children && type_param->type) {
                                    ast_node_t *bound_node = tp_ast->data.node.children->data;
                                    type_t *bound = semantic_resolve_type(sem, bound_node);
                                    if (bound) {
                                        type_param->type->data.type_var.bound = bound;
                                    }
                                }
                                
                                sym_iter = sym_iter->next;
                                ast_iter = ast_iter->next;
                            }
                            
                            /* Clean up tracking lists */
                            if (type_param_symbols) slist_free(type_param_symbols);
                            if (type_param_ast_nodes) slist_free(type_param_ast_nodes);
                            
                            /* Restore scope */
                            sem->current_scope = saved_scope_for_tparams;
                            
                            /* Process extends and implements from children 
                             * Parser marks: flags=1 for extends, flags=2 for implements
                             */
                            children = node->data.node.children;
                            while (children) {
                                ast_node_t *child = (ast_node_t *)children->data;
                                if (child->type == AST_CLASS_TYPE ||
                                    child->type == AST_PRIMITIVE_TYPE ||
                                    child->type == AST_ARRAY_TYPE) {
                                    if (child->data.node.flags == 1) {
                                        /* extends - set superclass */
                                        
                                        /* Enums cannot extend other types */
                                        if (kind == SYM_ENUM) {
                                            semantic_error(sem, child->line, child->column,
                                                "enum types may not extend classes");
                                        }
                                        
                                        /* Annotation types cannot extend other types */
                                        if (kind == SYM_ANNOTATION) {
                                            semantic_error(sem, child->line, child->column,
                                                "'extends' not allowed for @interfaces");
                                        }
                                        const char *super_name = child->data.node.name;
                                        symbol_t *super_sym = scope_lookup(sem->current_scope, super_name);
                                        /* For classes extending classes OR interfaces extending interfaces */
                                        bool super_sym_ok = super_sym && 
                                            ((super_sym->kind == SYM_CLASS) ||
                                             (kind == SYM_INTERFACE && super_sym->kind == SYM_INTERFACE));
                                        if (!super_sym_ok) {
                                            /* Try import resolution first */
                                            char *qualified = resolve_import(sem, super_name);
                                            if (qualified) {
                                                super_sym = load_external_class(sem, qualified);
                                                free(qualified);
                                            }
                                            super_sym_ok = super_sym && 
                                                ((super_sym->kind == SYM_CLASS) ||
                                                 (kind == SYM_INTERFACE && super_sym->kind == SYM_INTERFACE));
                                        }
                                        if (!super_sym_ok) {
                                            /* Try same package first */
                                            if (sem->current_package) {
                                                char same_package[512];
                                                snprintf(same_package, sizeof(same_package), "%s.%s",
                                                         sem->current_package, super_name);
                                                super_sym = scope_lookup(sem->global_scope, same_package);
                                                super_sym_ok = super_sym && 
                                                    ((super_sym->kind == SYM_CLASS) ||
                                                     (kind == SYM_INTERFACE && super_sym->kind == SYM_INTERFACE));
                                                if (!super_sym_ok) {
                                                    super_sym = load_external_class(sem, same_package);
                                                }
                                            }
                                            super_sym_ok = super_sym && 
                                                ((super_sym->kind == SYM_CLASS) ||
                                                 (kind == SYM_INTERFACE && super_sym->kind == SYM_INTERFACE));
                                            if (!super_sym_ok) {
                                                /* Try loading from classpath with simple name */
                                            super_sym = load_external_class(sem, super_name);
                                            }
                                            if (!super_sym) {
                                                /* Try java.lang prefix */
                                                char qualified[256];
                                                snprintf(qualified, sizeof(qualified), 
                                                         "java.lang.%s", super_name);
                                                super_sym = load_external_class(sem, qualified);
                                            }
                                        }
                                        /* Check if trying to extend java.lang.Enum directly */
                                        bool is_enum_extends = false;
                                        if (strcmp(super_name, "Enum") == 0 || 
                                            strcmp(super_name, "java.lang.Enum") == 0) {
                                            is_enum_extends = true;
                                        } else if (super_sym && super_sym->qualified_name && 
                                                   strcmp(super_sym->qualified_name, "java.lang.Enum") == 0) {
                                            is_enum_extends = true;
                                        }
                                        if (is_enum_extends) {
                                            semantic_error(sem, child->line, child->column,
                                                "classes cannot directly extend 'java.lang.Enum'");
                                        }
                                        
                                        if (super_sym) {
                                            /* Check if trying to extend enum or annotation */
                                            if (super_sym->kind == SYM_ENUM) {
                                                semantic_error(sem, child->line, child->column,
                                                    "cannot inherit from enum '%s'", super_name);
                                            } else if (super_sym->kind == SYM_ANNOTATION) {
                                                semantic_error(sem, child->line, child->column,
                                                    "cannot inherit from annotation type '%s'", super_name);
                                            } else if (super_sym->kind == SYM_INTERFACE && 
                                                       kind == SYM_CLASS) {
                                                /* Classes cannot extend interfaces */
                                                semantic_error(sem, child->line, child->column,
                                                    "no interface expected here");
                                            }
                                        }
                                        
                                        /* For interfaces extending other interfaces, add to interfaces list */
                                        if (kind == SYM_INTERFACE && super_sym && 
                                            super_sym->kind == SYM_INTERFACE) {
                                            /* Check for repeated interface */
                                            bool duplicate = false;
                                            for (slist_t *inode = sym->data.class_data.interfaces;
                                                 inode; inode = inode->next) {
                                                if (inode->data == super_sym) {
                                                    semantic_error(sem, child->line, child->column,
                                                        "repeated interface");
                                                    duplicate = true;
                                                    break;
                                                }
                                            }
                                            if (!duplicate) {
                                                if (!sym->data.class_data.interfaces) {
                                                    sym->data.class_data.interfaces = slist_new(super_sym);
                                                } else {
                                                    slist_append(sym->data.class_data.interfaces, super_sym);
                                                }
                                            }
                                        }
                                        else if (super_sym && super_sym->kind == SYM_CLASS) {
                                            /* Check if superclass is final */
                                            if (super_sym->modifiers & MOD_FINAL) {
                                                semantic_error(sem, child->line, child->column,
                                                    "Cannot extend final class '%s'", super_name);
                                            }
                                            /* Check for cyclic inheritance (extends itself) */
                                            if (super_sym == sym) {
                                                semantic_error(sem, child->line, child->column,
                                                    "Cyclic inheritance involving '%s'", name);
                                            } else {
                                                /* Check if any ancestor in the chain is the current class */
                                                symbol_t *ancestor = super_sym->data.class_data.superclass;
                                                bool cyclic = false;
                                                while (ancestor) {
                                                    if (ancestor == sym) {
                                                        semantic_error(sem, child->line, child->column,
                                                            "Cyclic inheritance involving '%s'", name);
                                                        cyclic = true;
                                                        break;
                                                    }
                                                    ancestor = ancestor->data.class_data.superclass;
                                                }
                                                if (!cyclic) {
                                            sym->data.class_data.superclass = super_sym;
                                                }
                                            }
                                        }
                                    } else if (child->data.node.flags == 2) {
                                        /* implements - add to interfaces list */
                                        const char *iface_name = child->data.node.name;
                                        if (getenv("GENESIS_DEBUG_IFACE")) {
                                            fprintf(stderr, "DEBUG iface: loading interface '%s' for class '%s'\n",
                                                iface_name, sym->name);
                                        }
                                        symbol_t *iface_sym = scope_lookup(sem->current_scope, iface_name);
                                        if (!iface_sym) {
                                            /* Try import resolution */
                                            char *qualified = resolve_import(sem, iface_name);
                                            if (qualified) {
                                                iface_sym = load_external_class(sem, qualified);
                                                free(qualified);
                                            }
                                            if (!iface_sym) {
                                                /* Try unqualified name directly */
                                                iface_sym = load_external_class(sem, iface_name);
                                            }
                                            if (!iface_sym) {
                                                /* Try java.lang prefix for standard interfaces */
                                                char qualified_buf[256];
                                                snprintf(qualified_buf, sizeof(qualified_buf), 
                                                         "java.lang.%s", iface_name);
                                                iface_sym = load_external_class(sem, qualified_buf);
                                            }
                                        }
                                        if (getenv("GENESIS_DEBUG_IFACE")) {
                                            fprintf(stderr, "DEBUG iface: resolved '%s' -> %p (kind=%d)\n",
                                                iface_name, (void*)iface_sym, iface_sym ? iface_sym->kind : -1);
                                        }
                                        if (iface_sym) {
                                            /* Check that we're implementing an interface, not a class */
                                            if (iface_sym->kind != SYM_INTERFACE &&
                                                iface_sym->kind != SYM_ANNOTATION) {
                                                semantic_error(sem, child->line, child->column,
                                                    "interface expected here");
                                            }
                                            /* Check for repeated interface.
                                             * If pre-pass marked this AST node, it's already added - skip.
                                             * Only report error if this is a DIFFERENT AST node for same interface. */
                                            bool duplicate = false;
                                            bool was_prepass = (child->sem_symbol == iface_sym);
                                            for (slist_t *inode = sym->data.class_data.interfaces;
                                                 inode; inode = inode->next) {
                                                if (inode->data == iface_sym) {
                                                    duplicate = true;
                                                    if (!was_prepass) {
                                                        /* Actual duplicate in source code */
                                                        semantic_error(sem, child->line, child->column,
                                                            "repeated interface");
                                                    }
                                                    break;
                                                }
                                            }
                                            if (!duplicate) {
                                            if (!sym->data.class_data.interfaces) {
                                                sym->data.class_data.interfaces = slist_new(iface_sym);
                                            } else {
                                                slist_append(sym->data.class_data.interfaces, iface_sym);
                                                }
                                            }
                                        }
                                    }
                                }
                                children = children->next;
                            }
                            
                            /* For enums, implicitly set superclass to java.lang.Enum */
                            if (kind == SYM_ENUM && !sym->data.class_data.superclass) {
                                symbol_t *enum_super = load_external_class(sem, "java.lang.Enum");
                                if (enum_super) {
                                    sym->data.class_data.superclass = enum_super;
                                }
                            }
                            
                            /* For records, implicitly set superclass to java.lang.Record */
                            if (kind == SYM_RECORD && !sym->data.class_data.superclass) {
                                symbol_t *record_super = load_external_class(sem, "java.lang.Record");
                                if (record_super) {
                                    sym->data.class_data.superclass = record_super;
                                }
                                /* If java.lang.Record not available (JDK < 16), fall back to Object */
                                if (!sym->data.class_data.superclass) {
                                    symbol_t *object_super = load_external_class(sem, "java.lang.Object");
                                    if (object_super) {
                                        sym->data.class_data.superclass = object_super;
                                    }
                                }
                                
                                /* Register synthetic methods: toString, hashCode, equals
                                 * These are generated by codegen but need to be visible during semantic analysis */
                                if (class_scope) {
                                    /* toString() -> String */
                                    symbol_t *tostring_sym = symbol_new(SYM_METHOD, "toString");
                                    tostring_sym->modifiers = MOD_PUBLIC;
                                    tostring_sym->type = type_new_class("java.lang.String");
                                    tostring_sym->scope = class_scope;
                                    hashtable_insert(class_scope->symbols, "toString()", tostring_sym);
                                    
                                    /* hashCode() -> int */
                                    symbol_t *hashcode_sym = symbol_new(SYM_METHOD, "hashCode");
                                    hashcode_sym->modifiers = MOD_PUBLIC;
                                    hashcode_sym->type = type_new_primitive(TYPE_INT);
                                    hashcode_sym->scope = class_scope;
                                    hashtable_insert(class_scope->symbols, "hashCode()", hashcode_sym);
                                    
                                    /* equals(Object) -> boolean */
                                    symbol_t *equals_sym = symbol_new(SYM_METHOD, "equals");
                                    equals_sym->modifiers = MOD_PUBLIC;
                                    equals_sym->type = type_new_primitive(TYPE_BOOLEAN);
                                    equals_sym->scope = class_scope;
                                    hashtable_insert(class_scope->symbols, "equals()", equals_sym);
                                }
                            }
                            
                            /* Save and switch scope */
                            frame->saved_scope = sem->current_scope;
                            frame->saved_class = sem->current_class;
                            sem->current_scope = class_scope;
                            sem->current_class = sym;
                        }
                        break;
                    
                    case AST_FIELD_DECL:
                        {
                            /* Get field type */
                            slist_t *children = node->data.node.children;
                            type_t *field_type = NULL;
                            
                            if (children && ((ast_node_t *)children->data)->type != AST_VAR_DECLARATOR) {
                                field_type = semantic_resolve_type(sem, children->data);
                                children = children->next;
                            }
                            
                            /* Process each declarator */
                            while (children) {
                                ast_node_t *decl = children->data;
                                if (decl->type == AST_VAR_DECLARATOR) {
                                    const char *name = decl->data.node.name;
                                    
                                    symbol_t *sym = symbol_new(SYM_FIELD, name);
                                    sym->modifiers = node->data.node.flags;
                                    sym->type = field_type;
                                    sym->ast = decl;
                                    sym->line = decl->line;
                                    sym->column = decl->column;
                                    
                                    /* Check for illegal modifier combinations on fields */
                                    check_modifier_combination(sem, sym->modifiers, "field",
                                                               node->line, node->column);
                                    
                                    if (!scope_define(sem->current_scope, sym)) {
                                        semantic_error(sem, decl->line, decl->column,
                                                      "Duplicate field: %s", name);
                                    }
                                    
                                    /* Field initializer type checking is done in pass2
                                     * to allow forward references to methods */
                                }
                                children = children->next;
                            }
                        }
                        break;
                    
                    case AST_ENUM_CONSTANT:
                        {
                            /* Enum constants are public static final fields of the enum type */
                            const char *name = node->data.node.name;
                            
                            /* Check if already pre-registered (from preregister_nested_types) */
                            symbol_t *existing = scope_lookup_local(sem->current_scope, name);
                            if (existing && existing->kind == SYM_FIELD && 
                                existing->data.var_data.is_enum_constant) {
                                /* Already registered - just store symbol on AST for codegen */
                                node->sem_symbol = existing;
                                break;
                            }
                            
                            symbol_t *sym = symbol_new(SYM_FIELD, name);
                            sym->modifiers = MOD_PUBLIC | MOD_STATIC | MOD_FINAL;
                            /* Type is the enclosing enum type */
                            if (sem->current_class && sem->current_class->type) {
                                sym->type = sem->current_class->type;
                            }
                            sym->ast = node;
                            sym->line = node->line;
                            sym->column = node->column;
                            sym->data.var_data.is_enum_constant = true;
                            /* Ordinal was pre-assigned and stored in flags field during enum decl processing */
                            sym->data.var_data.enum_ordinal = node->data.node.flags;
                            
                            if (!scope_define(sem->current_scope, sym)) {
                                semantic_error(sem, node->line, node->column,
                                              "Duplicate enum constant: %s", name);
                            }
                            
                            /* Store symbol on AST for codegen */
                            node->sem_symbol = sym;
                        }
                        break;
                    
                    case AST_PARAMETER:
                        {
                            /* Record components - register as field and accessor method */
                            if (node->data.node.flags & MOD_RECORD_COMPONENT) {
                                const char *name = node->data.node.name;
                                
                                /* Get component type */
                                type_t *comp_type = NULL;
                                if (node->data.node.children) {
                                    comp_type = semantic_resolve_type(sem, node->data.node.children->data);
                                }
                                
                                /* Register as private final field */
                                symbol_t *field_sym = symbol_new(SYM_FIELD, name);
                                field_sym->modifiers = MOD_PRIVATE | MOD_FINAL;
                                field_sym->type = comp_type;
                                field_sym->ast = node;
                                field_sym->line = node->line;
                                field_sym->column = node->column;
                                
                                if (!scope_define(sem->current_scope, field_sym)) {
                                    semantic_error(sem, node->line, node->column,
                                                  "Duplicate record component: %s", name);
                                }
                                
                                /* Register accessor method with same name as component */
                                symbol_t *accessor_sym = symbol_new(SYM_METHOD, name);
                                accessor_sym->modifiers = MOD_PUBLIC;
                                accessor_sym->type = comp_type;  /* Return type */
                                accessor_sym->ast = node;  /* Point to component AST */
                                accessor_sym->line = node->line;
                                accessor_sym->column = node->column;
                                accessor_sym->scope = sem->current_scope;
                                
                                /* Define with "()" suffix to mark as method - use hashtable directly */
                                size_t name_len = strlen(name);
                                char *method_key = malloc(name_len + 3);
                                snprintf(method_key, name_len + 3, "%s()", name);
                                hashtable_insert(sem->current_scope->symbols, method_key, accessor_sym);
                                free(method_key);
                                
                                /* Store symbol on AST for codegen */
                                node->sem_symbol = field_sym;
                            }
                        }
                        break;
                    
                    case AST_METHOD_DECL:
                    case AST_CONSTRUCTOR_DECL:
                        {
                            const char *name = node->data.node.name;
                            symbol_kind_t kind = node->type == AST_METHOD_DECL 
                                ? SYM_METHOD : SYM_CONSTRUCTOR;
                            
                            symbol_t *sym = symbol_new(kind, name);
                            sym->modifiers = node->data.node.flags;
                            sym->ast = node;
                            sym->line = node->line;
                            sym->column = node->column;
                            
                            /* Store symbol on AST node for pass2 lookup (handles overloads) */
                            node->sem_symbol = sym;
                            
                            /* Annotation methods are implicitly public abstract */
                            if (kind == SYM_METHOD && 
                                sem->current_class && 
                                sem->current_class->kind == SYM_ANNOTATION) {
                                sym->modifiers |= MOD_PUBLIC | MOD_ABSTRACT;
                                
                                /* Annotation methods cannot have throws clause */
                                for (slist_t *child = node->data.node.children; child; child = child->next) {
                                    ast_node_t *ch = (ast_node_t *)child->data;
                                    if (ch && (ch->type == AST_CLASS_TYPE || ch->type == AST_IDENTIFIER) &&
                                        ch->data.node.flags == 0xFFFF) { /* throws marker */
                                        semantic_error(sem, ch->line, ch->column,
                                            "throws clause not allowed in @interface members");
                                        break;
                                    }
                                }
                            }
                            
                            /* Interface methods are implicitly public (and abstract unless default/static/private) */
                            if (kind == SYM_METHOD && 
                                sem->current_class && 
                                sem->current_class->kind == SYM_INTERFACE) {
                                bool is_private = (sym->modifiers & MOD_PRIVATE) != 0;
                                bool is_static = (sym->modifiers & MOD_STATIC) != 0;
                                bool is_default = (sym->modifiers & MOD_DEFAULT) != 0;
                                
                                /* Non-private interface methods are implicitly public */
                                if (!is_private) {
                                    sym->modifiers |= MOD_PUBLIC;
                                }
                                
                                /* Non-default, non-static, non-private interface methods are implicitly abstract */
                                if (!is_default && !is_static && !is_private) {
                                    sym->modifiers |= MOD_ABSTRACT;
                                }
                            }
                            
                            /* Private interface methods require Java 9+ */
                            if (kind == SYM_METHOD && 
                                sem->current_class && 
                                sem->current_class->kind == SYM_INTERFACE &&
                                (sym->modifiers & MOD_PRIVATE)) {
                                if (sem->source_version < 9) {
                                    semantic_error(sem, node->line, node->column,
                                        "private interface methods are not supported in -source %d (use 9 or higher)",
                                        sem->source_version);
                                }
                            }
                            
                            /* Check for illegal modifier combinations on methods */
                            if (kind == SYM_METHOD) {
                                check_modifier_combination(sem, sym->modifiers, "method",
                                                           node->line, node->column);
                            }
                            
                            /* NOTE: Return type is resolved later, after method_scope is created
                             * and type parameters are added. This allows the return type to 
                             * reference method type parameters like <T> T foo() */
                            
                            /* Check if method has a body (an AST_BLOCK child) */
                            bool has_body = false;
                            for (slist_t *child = node->data.node.children; child; child = child->next) {
                                ast_node_t *ch = (ast_node_t *)child->data;
                                if (ch && ch->type == AST_BLOCK) {
                                    has_body = true;
                                    break;
                                }
                            }
                            
                            /* Check for abstract method with body */
                            if ((sym->modifiers & MOD_ABSTRACT) && has_body) {
                                semantic_error(sem, node->line, node->column,
                                    "abstract method cannot have a body");
                            }
                            
                            /* Check for native method with body */
                            if ((sym->modifiers & MOD_NATIVE) && has_body) {
                                semantic_error(sem, node->line, node->column,
                                    "native method cannot have a body");
                            }
                            
                            /* Check for non-abstract method without body (in non-interface) */
                            if (kind == SYM_METHOD && !(sym->modifiers & MOD_ABSTRACT) && 
                                !(sym->modifiers & MOD_NATIVE) && !has_body) {
                                /* Interface methods without body are implicitly abstract */
                                if (sem->current_class && sem->current_class->kind != SYM_INTERFACE) {
                                    semantic_error(sem, node->line, node->column,
                                        "missing method body, or declare abstract");
                                }
                            }
                            
                            /* Check for method override and covariant returns */
                            bool actually_overrides = false;
                            if (kind == SYM_METHOD && sem->current_class) {
                                symbol_t *super = sem->current_class->data.class_data.superclass;
                                /* If no explicit superclass, check java.lang.Object for classes/enums/records */
                                if (!super && sem->current_class->kind != SYM_INTERFACE) {
                                    super = load_external_class(sem, "java.lang.Object");
                                }
                                while (super) {
                                    if (super->data.class_data.members) {
                                        /* Methods are stored with keys like "name(N)" where N is param count.
                                         * We need to iterate and find by name prefix, then check signature match. */
                                        scope_t *ms = super->data.class_data.members;
                                        symbol_t *parent_method = NULL;
                                        if (ms->symbols) {
                                            size_t name_len = strlen(name);
                                            for (size_t bi = 0; bi < ms->symbols->size && !parent_method; bi++) {
                                                hashtable_entry_t *entry = ms->symbols->buckets[bi];
                                                while (entry) {
                                                    /* Check if key starts with method name */
                                                    if (strncmp(entry->key, name, name_len) == 0) {
                                                        char next_char = entry->key[name_len];
                                                        /* Key should be "name(N)" or just "name" */
                                                        if (next_char == '\0' || next_char == '(') {
                                                            symbol_t *s = (symbol_t *)entry->value;
                                                            if (s && s->kind == SYM_METHOD) {
                                                                /* Check if signatures match (same param types) 
                                                                 * Use AST-based comparison since sym's params 
                                                                 * aren't populated yet in pass1 */
                                                                if (method_ast_matches_signature(node, s)) {
                                                                    parent_method = s;
                                                                    break;
                                                                }
                                                            }
                                                        }
                                                    }
                                                    entry = entry->next;
                                                }
                                            }
                                        }
                                        if (parent_method && parent_method->kind == SYM_METHOD) {
                                            actually_overrides = true;
                                            
                                            /* Check for final method override */
                                            if (parent_method->modifiers & MOD_FINAL) {
                                                semantic_error(sem, node->line, node->column,
                                                    "Cannot override final method '%s' from '%s'",
                                                    name, super->name);
                                            }
                                            
                                            /* Check return type compatibility (covariant returns) */
                                            if (sym->type && parent_method->type) {
                                                /* Return type must be same or subtype */
                                                if (!type_assignable(parent_method->type, sym->type)) {
                                                    char *parent_str = type_to_string(parent_method->type);
                                                    char *child_str = type_to_string(sym->type);
                                                    semantic_error(sem, node->line, node->column,
                                                        "Return type '%s' is not compatible with '%s' in overridden method",
                                                        child_str, parent_str);
                                                    free(parent_str);
                                                    free(child_str);
                                                }
                                                
                                                /* Track if we need a bridge method (different return types) */
                                                if (!type_equals(sym->type, parent_method->type)) {
                                                    sym->data.method_data.overridden_method = parent_method;
                                                }
                                            }
                                            break;  /* Found the method, stop searching */
                                        }
                                    }
                                    super = super->data.class_data.superclass;
                                }
                                
                                /* Also check implemented interfaces for bridge method needs.
                                 * When implementing a generic interface like Callable<Map>,
                                 * the interface method T call() erases to Object call(),
                                 * but our method Map call() needs a bridge Object call().
                                 * 
                                 * Use lookup_method_in_interfaces which recursively searches
                                 * the interface hierarchy (important for interfaces that extend
                                 * other interfaces, e.g., Locator2 extends Locator). */
                                if (!sym->data.method_data.overridden_method) {
                                    symbol_t *iface_method = lookup_method_in_interfaces(sem, 
                                        sem->current_class, name);
                                    
                                    if (iface_method && iface_method->kind == SYM_METHOD) {
                                        /* Found matching method in interface hierarchy */
                                        actually_overrides = true;
                                        
                                        /* Check if return types differ after erasure.
                                         * Interface method with type variable returns Object after erasure.
                                         * Our concrete method returns the actual type. */
                                        if (iface_method->type && sym->type) {
                                            /* Check if interface method has type variable return */
                                            type_t *iface_ret = iface_method->type;
                                            
                                            if (iface_ret->kind == TYPE_TYPEVAR) {
                                                /* Interface returns type variable - erases to Object or bound */
                                                /* Our method returns concrete type - need bridge */
                                                if (sym->type->kind == TYPE_CLASS || 
                                                    sym->type->kind == TYPE_ARRAY) {
                                                    /* Different erasure - need bridge */
                                                    
                                                    /* Create a synthetic "overridden" method symbol 
                                                     * with Object return type for the bridge */
                                                    symbol_t *bridge_target = calloc(1, sizeof(symbol_t));
                                                    bridge_target->kind = SYM_METHOD;
                                                    bridge_target->name = strdup(name);
                                                    bridge_target->modifiers = iface_method->modifiers;
                                                    bridge_target->type = type_new_class("java.lang.Object");
                                                    bridge_target->data.method_data.parameters = 
                                                        iface_method->data.method_data.parameters;
                                                    sym->data.method_data.overridden_method = bridge_target;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            /* Check @Override annotation - method must actually override something */
                            if (kind == SYM_METHOD && node->annotations) {
                                for (slist_t *annot_node = node->annotations; annot_node; annot_node = annot_node->next) {
                                    ast_node_t *annot = (ast_node_t *)annot_node->data;
                                    if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
                                        const char *annot_name = annot->data.node.name;
                                        if (strcmp(annot_name, "Override") == 0 ||
                                            strcmp(annot_name, "java.lang.Override") == 0) {
                                            if (!actually_overrides) {
                                                /* For enum constant bodies, the method may override a method
                                                 * from the enum class itself (not just superclass). Check if
                                                 * a method with the same name exists in the current enum.
                                                 * 
                                                 * Since enum constants are processed before enum methods during
                                                 * the tree walk, we need to search the enum's AST directly
                                                 * rather than the members scope (which may not have the method yet). */
                                                if (sem->current_class && 
                                                    sem->current_class->kind == SYM_ENUM &&
                                                    sem->current_class->ast) {
                                                    /* Search enum AST for a METHOD_DECL with the same name */
                                                    ast_node_t *enum_ast = sem->current_class->ast;
                                                    slist_t *enum_children = enum_ast->data.node.children;
                                                    for (slist_t *c = enum_children; c && !actually_overrides; c = c->next) {
                                                        ast_node_t *member = (ast_node_t *)c->data;
                                                        if (member && member->type == AST_METHOD_DECL &&
                                                            member->data.node.name &&
                                                            strcmp(member->data.node.name, name) == 0 &&
                                                            member != node) {
                                                            /* Found a method in the enum with the same name */
                                                            actually_overrides = true;
                                                        }
                                                    }
                                                }
                                            }
                                            if (!actually_overrides) {
                                                semantic_error(sem, node->line, node->column,
                                                    "Method '%s' does not override a method from its superclass or interfaces",
                                                    name);
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                            
                            /* Count parameters from AST for overload resolution */
                            /* (reserved for future use in overload resolution) */
                            (void)0; /* param counting placeholder */
                            
                            /* Create method scope (key generation moved after params) */
                            scope_t *method_scope = scope_new(SCOPE_METHOD, sem->current_scope);
                            method_scope->owner = sym;
                            sym->data.method_data.body_scope = method_scope;
                            
                            /* Set current_method so type parameters can be looked up */
                            symbol_t *saved_method = sem->current_method;
                            sem->current_method = sym;
                            
                            /* Process method type parameters (generics) using two-pass approach
                             * to handle self-referencing bounds like T extends Comparable<T> */
                            slist_t *seen_type_params = NULL;  /* Track seen type params for duplicate check */
                            slist_t *type_param_symbols = NULL;  /* Track symbols for second pass */
                            slist_t *type_param_ast_nodes = NULL;  /* Track AST nodes for second pass */
                            
                            /* Pass 1: Add all type params to method_scope with Object bound */
                            slist_t *children = node->data.node.children;
                            while (children) {
                                ast_node_t *child = children->data;
                                if (child->type == AST_TYPE_PARAMETER) {
                                    const char *param_name = child->data.node.name;
                                    if (param_name) {
                                        /* Check for duplicate type parameter */
                                        for (slist_t *tp = seen_type_params; tp; tp = tp->next) {
                                            const char *existing = (const char *)tp->data;
                                            if (existing && strcmp(existing, param_name) == 0) {
                                                semantic_error(sem, child->line, child->column,
                                                    "Duplicate type parameter: '%s'", param_name);
                                                break;
                                            }
                                        }
                                        if (!seen_type_params) {
                                            seen_type_params = slist_new((void*)param_name);
                                        } else {
                                            slist_append(seen_type_params, (void*)param_name);
                                        }
                                        
                                        /* Create a type parameter symbol */
                                        symbol_t *type_param = symbol_new(SYM_TYPE_PARAM, param_name);
                                        type_param->ast = child;
                                        type_param->line = child->line;
                                        type_param->column = child->column;
                                        
                                        /* Create a type variable type with Object bound initially */
                                        type_t *type_var = malloc(sizeof(type_t));
                                        type_var->kind = TYPE_TYPEVAR;
                                        type_var->data.type_var.name = strdup(param_name);
                                        type_var->data.type_var.bound = type_new_class("java.lang.Object");
                                        
                                        type_param->type = type_var;
                                        
                                        /* Define in method scope so it can be looked up */
                                        scope_define(method_scope, type_param);
                                        
                                        /* Track for second pass */
                                        if (!type_param_symbols) {
                                            type_param_symbols = slist_new(type_param);
                                            type_param_ast_nodes = slist_new(child);
                                        } else {
                                            slist_append(type_param_symbols, type_param);
                                            slist_append(type_param_ast_nodes, child);
                                        }
                                    }
                                }
                                children = children->next;
                            }
                            if (seen_type_params) slist_free(seen_type_params);
                            
                            /* Set scope to method_scope so type param lookups work */
                            scope_t *saved_scope = sem->current_scope;
                            sem->current_scope = method_scope;
                            
                            /* Pass 2: Resolve actual bounds (now all type params are in scope) */
                            slist_t *sym_iter = type_param_symbols;
                            slist_t *ast_iter = type_param_ast_nodes;
                            while (sym_iter && ast_iter) {
                                symbol_t *type_param = (symbol_t *)sym_iter->data;
                                ast_node_t *tp_ast = (ast_node_t *)ast_iter->data;
                                
                                if (tp_ast->data.node.children && type_param->type) {
                                    ast_node_t *bound_node = tp_ast->data.node.children->data;
                                            type_t *bound = semantic_resolve_type(sem, bound_node);
                                            if (bound) {
                                        type_param->type->data.type_var.bound = bound;
                                    }
                                }
                                
                                sym_iter = sym_iter->next;
                                ast_iter = ast_iter->next;
                            }
                            
                            /* Clean up tracking lists */
                            if (type_param_symbols) slist_free(type_param_symbols);
                            if (type_param_ast_nodes) slist_free(type_param_ast_nodes);
                            
                            /* Resolve return type (now type params are available) */
                            if (node->data.node.extra) {
                                sym->type = semantic_resolve_type(sem, node->data.node.extra);
                            } else if (kind == SYM_CONSTRUCTOR) {
                                /* Constructor return type is the class */
                                if (sem->current_class) {
                                    sym->type = sem->current_class->type;
                                }
                            }
                            
                            /* Keep method_scope as current_scope for parameter processing,
                             * since parameters can also use type params like <T> void foo(T x) */
                            
                            /* Process parameters */
                            children = node->data.node.children;
                            bool has_varargs = false;
                            while (children) {
                                ast_node_t *child = children->data;
                                if (child->type == AST_PARAMETER) {
                                    const char *param_name = child->data.node.name;
                                    symbol_t *param = symbol_new(SYM_PARAMETER, param_name);
                                    param->modifiers = child->data.node.flags;
                                    param->ast = child;
                                    param->line = child->line;
                                    param->column = child->column;
                                    
                                    bool is_varargs = (child->data.node.flags & MOD_VARARGS) != 0;
                                    
                                    /* Validate varargs is the last parameter */
                                    if (has_varargs) {
                                        semantic_error(sem, child->line, child->column,
                                            "Varargs parameter must be the last parameter");
                                    }
                                    if (is_varargs) {
                                        has_varargs = true;
                                        /* Mark the method as having varargs */
                                        sym->modifiers |= MOD_VARARGS;
                                    }
                                    
                                    /* Parameter type */
                                    if (child->data.node.children) {
                                        param->type = semantic_resolve_type(sem, 
                                            child->data.node.children->data);
                                        
                                        /* Varargs: convert type to array type (String... -> String[]) */
                                        if (is_varargs && param->type && 
                                            param->type->kind != TYPE_ARRAY) {
                                            type_t *array_type = type_new_array(param->type, 1);
                                            param->type = array_type;
                                        }
                                    }
                                    
                                    scope_define(method_scope, param);
                                    
                                    if (!sym->data.method_data.parameters) {
                                        sym->data.method_data.parameters = slist_new(param);
                                    } else {
                                        slist_append(sym->data.method_data.parameters, param);
                                    }
                                }
                                children = children->next;
                            }
                            
                            /* Store method with unique key for overloading support.
                             * Use format: name(param_type_kinds) to distinguish overloads with same param count.
                             * This must be done AFTER parameters are processed so we have their types. */
                            {
                                char method_key[1024];
                                char *pk = method_key;
                                pk += snprintf(pk, sizeof(method_key), "%s(", name);
                                
                                /* Build parameter type signature using type kinds and class names */
                                for (slist_t *plist = sym->data.method_data.parameters; plist; plist = plist->next) {
                                    symbol_t *ps = (symbol_t *)plist->data;
                                    if (ps && ps->type) {
                                        switch (ps->type->kind) {
                                            case TYPE_VOID:   pk += snprintf(pk, method_key + sizeof(method_key) - pk, "V"); break;
                                            case TYPE_BYTE:   pk += snprintf(pk, method_key + sizeof(method_key) - pk, "B"); break;
                                            case TYPE_SHORT:  pk += snprintf(pk, method_key + sizeof(method_key) - pk, "S"); break;
                                            case TYPE_INT:    pk += snprintf(pk, method_key + sizeof(method_key) - pk, "I"); break;
                                            case TYPE_LONG:   pk += snprintf(pk, method_key + sizeof(method_key) - pk, "J"); break;
                                            case TYPE_FLOAT:  pk += snprintf(pk, method_key + sizeof(method_key) - pk, "F"); break;
                                            case TYPE_DOUBLE: pk += snprintf(pk, method_key + sizeof(method_key) - pk, "D"); break;
                                            case TYPE_CHAR:   pk += snprintf(pk, method_key + sizeof(method_key) - pk, "C"); break;
                                            case TYPE_BOOLEAN:pk += snprintf(pk, method_key + sizeof(method_key) - pk, "Z"); break;
                                            case TYPE_CLASS:
                                                if (ps->type->data.class_type.name) {
                                                    pk += snprintf(pk, method_key + sizeof(method_key) - pk, "L%s;", 
                                                        ps->type->data.class_type.name);
                                                } else {
                                                    pk += snprintf(pk, method_key + sizeof(method_key) - pk, "L?;");
                                                }
                                                break;
                                            case TYPE_ARRAY:
                                                pk += snprintf(pk, method_key + sizeof(method_key) - pk, "[");
                                                break;
                                            case TYPE_TYPEVAR:
                                                /* Type variable - use name or Object */
                                                if (ps->type->data.type_var.name) {
                                                    pk += snprintf(pk, method_key + sizeof(method_key) - pk, "T%s;",
                                                        ps->type->data.type_var.name);
                                                } else {
                                                    pk += snprintf(pk, method_key + sizeof(method_key) - pk, "Ljava/lang/Object;");
                                                }
                                                break;
                                            default:
                                                pk += snprintf(pk, method_key + sizeof(method_key) - pk, "?");
                                                break;
                                        }
                                    }
                                }
                                snprintf(pk, method_key + sizeof(method_key) - pk, ")");
                                
                                /* Use hashtable_insert directly to support overloads */
                                if (saved_scope && saved_scope->symbols) {
                                    hashtable_insert(saved_scope->symbols, method_key, sym);
                                    sym->scope = saved_scope;
                                }
                            }
                            
                            /* Validate main method signature */
                            if (kind == SYM_METHOD && strcmp(name, "main") == 0) {
                                bool valid_main = true;
                                
                                /* Must be public and static */
                                if (!(sym->modifiers & MOD_PUBLIC) || 
                                    !(sym->modifiers & MOD_STATIC)) {
                                    semantic_error(sem, node->line, node->column,
                                        "'main' method must be declared 'public static'");
                                    valid_main = false;
                                }
                                
                                /* Must return void */
                                if (!sym->type || sym->type->kind != TYPE_VOID) {
                                    semantic_error(sem, node->line, node->column,
                                        "'main' method must have return type 'void'");
                                    valid_main = false;
                                }
                                
                                /* Check if this is an entry-point main method.
                                 * A main method can have any signature - it just won't be
                                 * usable as a JVM entry point unless it has String[] param.
                                 * We don't enforce this; it's valid Java to have a differently-
                                 * signed main method. */
                                (void)valid_main;  /* Currently unused, reserved for future use */
                            }
                            
                            /* Restore scope and method before saving them in frame 
                             * The frame will save these and restore later in WALK_EXIT */
                            sem->current_scope = saved_scope;
                            sem->current_method = saved_method;
                            
                            /* Save and switch scope for processing method body */
                            frame->saved_scope = sem->current_scope;
                            frame->saved_method = sem->current_method;
                            sem->current_scope = method_scope;
                            sem->current_method = sym;
                        }
                        break;
                    
                    case AST_NEW_OBJECT:
                        {
                            /* Check for anonymous class body (last child is AST_BLOCK) */
                            slist_t *children = node->data.node.children;
                            if (!children) break;
                            
                            /* Find the last child to see if it's a block (anonymous body) */
                            ast_node_t *last_child = NULL;
                            ast_node_t *first_child = (ast_node_t *)children->data;
                            for (slist_t *c = children; c; c = c->next) {
                                last_child = (ast_node_t *)c->data;
                            }
                            
                            if (last_child && last_child->type == AST_BLOCK) {
                                /* This is an anonymous class - set up the class symbol in pass1 */
                                ast_node_t *anon_body = last_child;
                                
                                /* Check if already processed (avoid re-processing) */
                                if (anon_body->sem_symbol && anon_body->sem_symbol->kind == SYM_CLASS) {
                                    /* Already set up - save/restore context and continue */
                                    frame->saved_class = sem->current_class;
                                    frame->saved_scope = sem->current_scope;
                                    sem->current_class = anon_body->sem_symbol;
                                    if (anon_body->sem_symbol->data.class_data.members) {
                                        sem->current_scope = anon_body->sem_symbol->data.class_data.members;
                                    }
                                    break;
                                }
                                
                                /* Generate anonymous class name */
                                int anon_id = 1;
                                char anon_name[256];
                                if (sem->current_class && sem->current_class->qualified_name) {
                                    anon_id = ++sem->current_class->data.class_data.local_class_counter;
                                    snprintf(anon_name, sizeof(anon_name), "%s$%d",
                                             sem->current_class->qualified_name, anon_id);
                                } else {
                                    snprintf(anon_name, sizeof(anon_name), "$%d", anon_id);
                                }
                                
                                /* Create anonymous class symbol */
                                symbol_t *anon_sym = symbol_new(SYM_CLASS, anon_name);
                                anon_sym->qualified_name = strdup(anon_name);
                                anon_sym->ast = node;
                                anon_sym->data.class_data.enclosing_class = sem->current_class;
                                anon_sym->data.class_data.enclosing_method = sem->current_method;
                                anon_sym->data.class_data.is_anonymous_class = true;
                                anon_sym->data.class_data.anonymous_body = anon_body;
                                
                                /* Determine if in static context */
                                bool in_static_context = false;
                                if (sem->current_method) {
                                    in_static_context = (sem->current_method->modifiers & MOD_STATIC) != 0;
                                } else {
                                    /* Not inside a method - check if static field init */
                                    in_static_context = sem->in_static_field_init;
                                }
                                if (sem->current_lambda != NULL) {
                                    in_static_context = true;
                                }
                                anon_sym->modifiers = in_static_context ? MOD_STATIC : 0;
                                
                                /* Resolve base type */
                                type_t *base_type = semantic_resolve_type(sem, first_child);
                                
                                /* Determine if base type is class or interface */
                                if (base_type && base_type->kind == TYPE_CLASS && 
                                    base_type->data.class_type.symbol) {
                                    symbol_t *base_sym = base_type->data.class_type.symbol;
                                    if (base_sym->kind == SYM_INTERFACE) {
                                        /* Implementing an interface - extend Object */
                                        anon_sym->data.class_data.superclass = 
                                            load_external_class(sem, "java.lang.Object");
                                        anon_sym->data.class_data.interfaces = slist_new(base_sym);
                                    } else {
                                        /* Extending a class */
                                        anon_sym->data.class_data.superclass = base_sym;
                                    }
                                }
                                
                                /* Create type for anonymous class */
                                type_t *anon_type = type_new_class(anon_name);
                                anon_type->data.class_type.symbol = anon_sym;
                                anon_sym->type = anon_type;
                                
                                /* Cache the type */
                                hashtable_insert(sem->types, anon_name, anon_type);
                                
                                /* Create class member scope */
                                scope_t *class_scope = scope_new(SCOPE_CLASS, sem->current_scope);
                                class_scope->owner = anon_sym;
                                anon_sym->data.class_data.members = class_scope;
                                
                                /* Mark the anonymous class body block with the class symbol */
                                anon_body->sem_symbol = anon_sym;
                                node->sem_symbol = anon_sym;
                                
                                /* Save and switch context */
                                frame->saved_class = sem->current_class;
                                frame->saved_scope = sem->current_scope;
                                sem->current_class = anon_sym;
                                sem->current_scope = class_scope;
                            }
                        }
                        break;
                    
                    default:
                        break;
                }
                
                /* Prepare to process children */
                frame->state = WALK_CHILDREN;
                frame->child_iter = ast_get_children(node);
                break;
            
            case WALK_CHILDREN:
                /* Push next child onto stack */
                if (frame->child_iter) {
                    ast_node_t *child = frame->child_iter->data;
                    frame->child_iter = frame->child_iter->next;
                    
                    /* Grow stack if needed */
                    if (stack_top >= stack_capacity) {
                        stack_capacity *= 2;
                        stack = realloc(stack, sizeof(walk_frame_t *) * stack_capacity);
                    }
                    
                    stack[stack_top++] = walk_frame_new(child);
                } else {
                    frame->state = WALK_EXIT;
                }
                break;
            
            case WALK_EXIT:
                /* Restore scope on exit */
                switch (node->type) {
                    case AST_CLASS_DECL:
                        {
                            /* Check that non-abstract class implements all abstract methods */
                            symbol_t *class_sym = node->sem_symbol;
                            if (class_sym && !(class_sym->modifiers & MOD_ABSTRACT)) {
                                /* Check superclass abstract methods */
                                symbol_t *super = class_sym->data.class_data.superclass;
                                while (super) {
                                    scope_t *super_members = super->data.class_data.members;
                                    if (super_members && super_members->symbols) {
                                        hashtable_t *ht = super_members->symbols;
                                        for (size_t bi = 0; bi < ht->size; bi++) {
                                            for (hashtable_entry_t *e = ht->buckets[bi]; e; e = e->next) {
                                                symbol_t *method = (symbol_t *)e->value;
                                                if (method && method->kind == SYM_METHOD &&
                                                    (method->modifiers & MOD_ABSTRACT)) {
                                                    /* Check if any class in chain provides implementation */
                                                    bool implemented = false;
                                                    /* Search from this class up to (but not including) the abstract method's class */
                                                    symbol_t *check_class = class_sym;
                                                    while (check_class && check_class != super && !implemented) {
                                                        scope_t *check_members = check_class->data.class_data.members;
                                                        if (check_members && check_members->symbols) {
                                                            hashtable_t *ht2 = check_members->symbols;
                                                            for (size_t bi2 = 0; bi2 < ht2->size && !implemented; bi2++) {
                                                                for (hashtable_entry_t *e2 = ht2->buckets[bi2]; e2 && !implemented; e2 = e2->next) {
                                                                    symbol_t *impl = (symbol_t *)e2->value;
                                                                    if (impl && impl->kind == SYM_METHOD &&
                                                                        strcmp(impl->name, method->name) == 0 &&
                                                                        !(impl->modifiers & MOD_ABSTRACT)) {
                                                                        implemented = true;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                        check_class = check_class->data.class_data.superclass;
                                                    }
                                                    if (!implemented) {
                                                        semantic_error(sem, node->line, node->column,
                                                            "%s is not abstract and does not override abstract method %s() in %s",
                                                            class_sym->name, method->name, super->name);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    super = super->data.class_data.superclass;
                                }
                            }
                            sem->current_scope = frame->saved_scope;
                            sem->current_class = frame->saved_class;
                        }
                        break;
                    
                    case AST_INTERFACE_DECL:
                    case AST_ENUM_DECL:
                    case AST_RECORD_DECL:
                    case AST_ANNOTATION_DECL:
                        sem->current_scope = frame->saved_scope;
                        sem->current_class = frame->saved_class;
                        break;
                    
                    case AST_METHOD_DECL:
                    case AST_CONSTRUCTOR_DECL:
                        sem->current_scope = frame->saved_scope;
                        sem->current_method = frame->saved_method;
                        break;
                    
                    case AST_NEW_OBJECT:
                        {
                            /* Restore context if this was an anonymous class */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *last_child = NULL;
                                for (slist_t *c = children; c; c = c->next) {
                                    last_child = (ast_node_t *)c->data;
                                }
                                if (last_child && last_child->type == AST_BLOCK) {
                                    /* This was an anonymous class */
                                    if (frame->saved_scope) {
                                        sem->current_scope = frame->saved_scope;
                                    }
                                    if (frame->saved_class) {
                                        sem->current_class = frame->saved_class;
                                    }
                                }
                            }
                        }
                        break;
                    
                    default:
                        break;
                }
                
                free(stack[--stack_top]);
                break;
        }
    }
    
    free(stack);
}

/* ========================================================================
 * Pass 2: Resolve Names and Check Types
 * 
 * Walks the AST again, now with all declarations known, to:
 * - Resolve identifier references to their declarations
 * - Type-check expressions
 * - Check method calls match signatures
 * ======================================================================== */

/* Forward declaration */
type_t *get_expression_type(semantic_t *sem, ast_node_t *expr);

/**
 * Recursively collect all yield expressions from a statement tree into a list.
 */
static void collect_yields(ast_node_t *stmt, slist_t **yields)
{
    if (!stmt) return;
    
    switch (stmt->type) {
        case AST_YIELD_STMT:
            {
                slist_t *children = stmt->data.node.children;
                if (children) {
                    ast_node_t *expr = (ast_node_t *)children->data;
                    if (!*yields) {
                        *yields = slist_new(expr);
                    } else {
                        slist_append(*yields, expr);
                    }
                }
                return;
            }
        
        case AST_BLOCK:
            {
                slist_t *children = stmt->data.node.children;
                for (slist_t *c = children; c; c = c->next) {
                    collect_yields((ast_node_t *)c->data, yields);
                }
                return;
            }
        
        case AST_IF_STMT:
            {
                /* Check then and else branches */
                slist_t *children = stmt->data.node.children;
                if (children && children->next) {
                    collect_yields((ast_node_t *)children->next->data, yields);
                    if (children->next->next) {
                        collect_yields((ast_node_t *)children->next->next->data, yields);
                    }
                }
                return;
            }
        
        case AST_FOR_STMT:
        case AST_ENHANCED_FOR_STMT:
        case AST_WHILE_STMT:
        case AST_DO_STMT:
            {
                /* Check loop body */
                slist_t *children = stmt->data.node.children;
                if (children) {
                    /* Body is usually the last child */
                    ast_node_t *last = NULL;
                    for (slist_t *c = children; c; c = c->next) {
                        last = (ast_node_t *)c->data;
                    }
                    collect_yields(last, yields);
                }
                return;
            }
        
        case AST_TRY_STMT:
            {
                /* Check try block and catch blocks */
                slist_t *children = stmt->data.node.children;
                for (slist_t *c = children; c; c = c->next) {
                    collect_yields((ast_node_t *)c->data, yields);
                }
                return;
            }
        
        case AST_CATCH_CLAUSE:
            {
                /* Catch body is last child */
                slist_t *children = stmt->data.node.children;
                if (children) {
                    ast_node_t *last = NULL;
                    for (slist_t *c = children; c; c = c->next) {
                        last = (ast_node_t *)c->data;
                    }
                    collect_yields(last, yields);
                }
                return;
            }
        
        case AST_SYNCHRONIZED_STMT:
            {
                /* Body is second child */
                slist_t *children = stmt->data.node.children;
                if (children && children->next) {
                    collect_yields((ast_node_t *)children->next->data, yields);
                }
                return;
            }
        
        default:
            return;
    }
}

/**
 * Recursively find the first NON-NULL yield expression in a statement tree.
 * If all yields are null, returns the first yield expression.
 */
static ast_node_t *find_first_yield(ast_node_t *stmt)
{
    slist_t *yields = NULL;
    collect_yields(stmt, &yields);
    
    if (!yields) return NULL;
    
    /* First pass: look for non-null literal yields */
    ast_node_t *first = (ast_node_t *)yields->data;
    for (slist_t *y = yields; y; y = y->next) {
        ast_node_t *expr = (ast_node_t *)y->data;
        /* Check if this yield is NOT a null literal */
        if (expr->type != AST_LITERAL || 
            expr->data.leaf.token_type != TOK_NULL) {
            slist_free(yields);
            return expr;
        }
    }
    
    /* All yields are null, return first one */
    slist_free(yields);
    return first;
}

/**
 * Get the type of an expression (iterative).
 */
type_t *get_expression_type(semantic_t *sem, ast_node_t *expr)
{
    if (!expr) {
        return type_new_primitive(TYPE_UNKNOWN);
    }
    
    switch (expr->type) {
        case AST_LITERAL:
            switch (expr->data.leaf.token_type) {
                case TOK_INTEGER_LITERAL: return type_int();
                case TOK_LONG_LITERAL:    return type_long();
                case TOK_FLOAT_LITERAL:   return type_float();
                case TOK_DOUBLE_LITERAL:  return type_double();
                case TOK_CHAR_LITERAL:    return type_char();
                case TOK_STRING_LITERAL:  return type_string();
                case TOK_TEXT_BLOCK:
                    /* Text blocks require Java 15+ */
                    if (sem->source_version < 15) {
                        semantic_error(sem, expr->line, expr->column,
                            "text blocks are not supported in -source %d (use 15 or higher)",
                            sem->source_version);
                    }
                    return type_string();
                case TOK_TRUE:
                case TOK_FALSE:           return type_boolean();
                case TOK_NULL:            return type_null();
                default:                  return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_IDENTIFIER:
            {
                /* If sem_type already set (e.g., enum switch case), return it */
                if (expr->sem_type) {
                    return expr->sem_type;
                }
                
                const char *name = expr->data.leaf.name;
                symbol_t *sym = scope_lookup(sem->current_scope, name);
                if (sym) {
                    /* Check if this is a captured variable from enclosing method.
                     * This happens when:
                     * 1. We're inside a local or anonymous class
                     * 2. The symbol is a local variable or parameter
                     * 3. The current method (inside local class) is different from
                     *    the enclosing method (where the local class is defined) */
                    if (sem && sem->current_class && 
                        sem->current_class->kind == SYM_CLASS &&
                        (sem->current_class->data.class_data.is_local_class ||
                         sem->current_class->data.class_data.is_anonymous_class) &&
                        (sym->kind == SYM_LOCAL_VAR || sym->kind == SYM_PARAMETER)) {
                        symbol_t *enclosing = sem->current_class->data.class_data.enclosing_method;
                        /* If we're in a method inside the local class (current_method != enclosing),
                         * and the variable is a local/param, it must be captured */
                        if (enclosing && 
                            (enclosing->kind == SYM_METHOD || enclosing->kind == SYM_CONSTRUCTOR) &&
                            sem->current_method && 
                            sem->current_method != enclosing) {
                            /* Add to captured_vars if not already there */
                            slist_t *captured = sem->current_class->data.class_data.captured_vars;
                            bool already_captured = false;
                            for (slist_t *n = captured; n; n = n->next) {
                                if (n->data == sym) {
                                    already_captured = true;
                                    break;
                                }
                            }
                            if (!already_captured) {
                                if (!captured) {
                                    sem->current_class->data.class_data.captured_vars = slist_new(sym);
                                } else {
                                    slist_append(captured, sym);
                                }
                            }
                        }
                    }
                    
                    /* Check if this is a captured variable for a lambda.
                     * This happens when:
                     * 1. We're inside a lambda (sem->current_lambda is set)
                     * 2. The symbol is a local variable or parameter
                     * 3. The symbol is defined in the enclosing scope (not in lambda scope) */
                    if (sem && sem->current_lambda &&
                        (sym->kind == SYM_LOCAL_VAR || sym->kind == SYM_PARAMETER)) {
                        /* Check if this variable is from outside the lambda scope */
                        scope_t *var_scope = NULL;
                        for (scope_t *s = sem->current_scope; s; s = s->parent) {
                            if (scope_lookup_local(s, name) == sym) {
                                var_scope = s;
                                break;
                            }
                        }
                        /* If the variable is defined in or above lambda_enclosing_scope,
                         * it's a capture */
                        bool is_capture = false;
                        if (var_scope && sem->lambda_enclosing_scope) {
                            for (scope_t *s = sem->lambda_enclosing_scope; s; s = s->parent) {
                                if (s == var_scope) {
                                    is_capture = true;
                                    break;
                                }
                            }
                        }
                        
                        if (is_capture) {
                            /* Add to lambda's captures if not already there */
                            ast_node_t *lambda = sem->current_lambda;
                            bool already_captured = false;
                            for (slist_t *n = lambda->lambda_captures; n; n = n->next) {
                                if (n->data == sym) {
                                    already_captured = true;
                                    break;
                                }
                            }
                            if (!already_captured) {
                                if (!lambda->lambda_captures) {
                                    lambda->lambda_captures = slist_new(sym);
                                } else {
                                    slist_append(lambda->lambda_captures, sym);
                                }
                            }
                        }
                    }
                    
                    /* Store the type on the expression for codegen */
                    expr->sem_type = sym->type;
                    return sym->type;
                }
                
                /* Check static imports for static field */
                symbol_t *field_sym = NULL;
                symbol_t *import_class = resolve_static_import_field(sem, name, &field_sym);
                if (import_class && field_sym) {
                    /* Store the resolved class on the expression for codegen */
                    expr->sem_symbol = field_sym;
                    return field_sym->type;
                }
                
                /* Check for inherited field from superclass before falling back to class lookup.
                 * This ensures 'sessionConfig' finds the field, not a class 'SessionConfig'. */
                if (sem->current_class) {
                    /* Determine if we're in static context */
                    bool in_static = sem->in_static_field_init ||
                        (sem->current_method && (sem->current_method->modifiers & MOD_STATIC));
                    
                    symbol_t *search_class = sem->current_class->data.class_data.superclass;
                    while (search_class) {
                        if (search_class->data.class_data.members) {
                            symbol_t *inherited = scope_lookup_local(
                                search_class->data.class_data.members, name);
                            if (inherited && inherited->kind == SYM_FIELD &&
                                !(inherited->modifiers & MOD_PRIVATE)) {
                                /* Skip instance fields in static context */
                                if ((inherited->modifiers & MOD_STATIC) || !in_static) {
                                    expr->sem_symbol = inherited;
                                    expr->sem_type = inherited->type;
                                    return inherited->type;
                                }
                            }
                        }
                        search_class = search_class->data.class_data.superclass;
                    }
                }
                
                /* Could be a class reference - check per-compilation-unit scope first,
                 * then global types cache */
                type_t *type = hashtable_lookup(sem->unit_types, name);
                if (!type) {
                    type = hashtable_lookup(sem->types, name);
                }
                if (type) {
                    /* Set sem_symbol if we have a class symbol */
                    if (type->kind == TYPE_CLASS && type->data.class_type.symbol) {
                        expr->sem_symbol = type->data.class_type.symbol;
                    }
                    expr->sem_type = type;
                    return type;
                }
                
                /* Check if it's a nested type of an implemented interface.
                 * Walk up the enclosing class chain to check their interfaces too. */
                {
                    symbol_t *check_class = sem->current_class;
                    while (check_class) {
                        if (check_class->data.class_data.interfaces) {
                            for (slist_t *iface = check_class->data.class_data.interfaces;
                                 iface; iface = iface->next) {
                                symbol_t *iface_sym = (symbol_t *)iface->data;
                                if (iface_sym && (iface_sym->kind == SYM_INTERFACE ||
                                                  iface_sym->kind == SYM_CLASS) &&
                                    iface_sym->data.class_data.members) {
                                    symbol_t *nested = scope_lookup_local(
                                        iface_sym->data.class_data.members, name);
                                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                                   nested->kind == SYM_ENUM || nested->kind == SYM_RECORD) &&
                                        nested->type) {
                                        expr->sem_symbol = nested;
                                        expr->sem_type = nested->type;
                                        return nested->type;
                                    }
                                }
                            }
                        }
                        check_class = check_class->data.class_data.enclosing_class;
                    }
                }
                
                /* Try resolving through imports */
                char *qualified = resolve_import(sem, name);
                if (qualified) {
                    /* Check unit_types first (in case import was already cached there),
                     * then global types cache */
                    type = hashtable_lookup(sem->unit_types, qualified);
                    if (!type) {
                        type = hashtable_lookup(sem->types, qualified);
                    }
                    if (type) {
                        if (type->kind == TYPE_CLASS && type->data.class_type.symbol) {
                            expr->sem_symbol = type->data.class_type.symbol;
                        }
                        expr->sem_type = type;
                        free(qualified);
                        return type;
                    }
                    sym = load_external_class(sem, qualified);
                    if (sym && sym->type) {
                        expr->sem_symbol = sym;
                        expr->sem_type = sym->type;
                        free(qualified);
                        return sym->type;
                    }
                    free(qualified);
                }
                
                /* Check if it's an inherited field from superclass.
                 * Also check enclosing class hierarchy for inner classes. */
                {
                    /* Start with current class, then walk enclosing classes */
                    scope_t *class_scope = sem->current_scope;
                    while (class_scope) {
                        /* Find the enclosing class scope */
                        while (class_scope && class_scope->type != SCOPE_CLASS) {
                            class_scope = class_scope->parent;
                        }
                        if (!class_scope || !class_scope->owner) break;
                        
                        symbol_t *class_sym = class_scope->owner;
                        if (class_sym && (class_sym->kind == SYM_CLASS || 
                                          class_sym->kind == SYM_ENUM ||
                                          class_sym->kind == SYM_INTERFACE)) {
                            if (getenv("GENESIS_DEBUG_IFIELD")) {
                                fprintf(stderr, "DEBUG inherited field '%s': checking class '%s'\n",
                                    name, class_sym->name ? class_sym->name : "(null)");
                            }
                            
                            /* First check the class itself (for enclosing class fields) */
                            if (class_sym != sem->current_class && class_sym->data.class_data.members) {
                                symbol_t *field = scope_lookup_local(class_sym->data.class_data.members, name);
                                if (field && field->kind == SYM_FIELD) {
                                    expr->sem_symbol = field;
                                    expr->sem_type = field->type;
                                    return field->type;
                                }
                            }
                            
                            /* Then check superclass chain */
                            symbol_t *search_class = class_sym->data.class_data.superclass;
                            while (search_class) {
                                if (getenv("GENESIS_DEBUG_IFIELD")) {
                                    fprintf(stderr, "DEBUG inherited field '%s': searching superclass '%s' (members=%p)\n",
                                        name, search_class->name ? search_class->name : "(null)",
                                        (void*)search_class->data.class_data.members);
                                }
                                if (search_class->data.class_data.members) {
                                    symbol_t *field = scope_lookup_local(
                                        search_class->data.class_data.members, name);
                                    if (field && field->kind == SYM_FIELD) {
                                        /* Check access - inherited fields must be accessible */
                                        if (!(field->modifiers & MOD_PRIVATE)) {
                                            expr->sem_symbol = field;
                                            expr->sem_type = field->type;
                                            return field->type;
                                        }
                                    }
                                }
                                search_class = search_class->data.class_data.superclass;
                            }
                        }
                        
                        /* Move to enclosing scope */
                        class_scope = class_scope->parent;
                    }
                }
                
                /* Try loading from classpath directly */
                sym = load_external_class(sem, name);
                if (!sym) {
                    /* Try java.lang prefix */
                    char java_lang[256];
                    snprintf(java_lang, sizeof(java_lang), "java.lang.%s", name);
                    sym = load_external_class(sem, java_lang);
                }
                
                if (sym && sym->type) {
                    expr->sem_symbol = sym;
                    expr->sem_type = sym->type;
                    return sym->type;
                }
                
                semantic_error(sem, expr->line, expr->column,
                              "Cannot resolve symbol: %s", name);
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_THIS_EXPR:
            if (sem->current_class) {
                /* Check if we're in a static context */
                if (sem->current_method && !sem->current_lambda &&
                    (sem->current_method->modifiers & MOD_STATIC)) {
                    /* Only report error once per node (use sem_type as marker) */
                    if (!expr->sem_type) {
                        semantic_error(sem, expr->line, expr->column,
                            "cannot use 'this' in a static context");
                        expr->sem_type = type_new_primitive(TYPE_UNKNOWN);
                    }
                    return expr->sem_type;
                }
                /* Track 'this' capture for lambdas */
                if (sem->current_lambda) {
                    sem->current_lambda->lambda_captures_this = true;
                }
                return sem->current_class->type;
            }
            semantic_error(sem, expr->line, expr->column,
                          "'this' used outside of class context");
            return type_new_primitive(TYPE_UNKNOWN);
        
        case AST_SUPER_EXPR:
            if (sem->current_class && sem->current_class->data.class_data.superclass) {
                /* Check if we're in a static context */
                if (sem->current_method && !sem->current_lambda &&
                    (sem->current_method->modifiers & MOD_STATIC)) {
                    if (!expr->sem_type) {
                        semantic_error(sem, expr->line, expr->column,
                            "cannot use 'super' in a static context");
                        expr->sem_type = type_new_primitive(TYPE_UNKNOWN);
                    }
                    return expr->sem_type;
                }
                /* Track 'super' capture for lambdas (same as 'this') */
                if (sem->current_lambda) {
                    sem->current_lambda->lambda_captures_this = true;
                }
                return sem->current_class->data.class_data.superclass->type;
            }
            if (sem->current_class) {
                /* No explicit superclass - assume Object */
                symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
                if (obj_sym && obj_sym->type) {
                    return obj_sym->type;
                }
            }
            semantic_error(sem, expr->line, expr->column,
                          "'super' used outside of class context or class has no superclass");
            return type_new_primitive(TYPE_UNKNOWN);
        
        case AST_BINARY_EXPR:
            {
                slist_t *children = expr->data.node.children;
                if (!children || !children->next) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                type_t *left_type = get_expression_type(sem, children->data);
                type_t *right_type = get_expression_type(sem, children->next->data);
                const char *op = expr->data.node.name;
                
                /* Warn about division by literal zero */
                if (strcmp(op, "/") == 0 || strcmp(op, "%") == 0) {
                    ast_node_t *right = (ast_node_t *)children->next->data;
                    if (right->type == AST_LITERAL && right->data.leaf.name &&
                        strcmp(right->data.leaf.name, "0") == 0) {
                        semantic_warning(sem, expr->line, expr->column,
                            "division by zero");
                    }
                }
                
                /* String concatenation */
                if (strcmp(op, "+") == 0) {
                    if (type_equals(left_type, type_string()) ||
                        type_equals(right_type, type_string())) {
                        return type_string();
                    }
                }
                
                /* Comparison operators */
                if (strcmp(op, "==") == 0 || strcmp(op, "!=") == 0 ||
                    strcmp(op, "<") == 0 || strcmp(op, ">") == 0 ||
                    strcmp(op, "<=") == 0 || strcmp(op, ">=") == 0) {
                    return type_boolean();
                }
                
                /* Logical operators */
                if (strcmp(op, "&&") == 0 || strcmp(op, "||") == 0) {
                    return type_boolean();
                }
                
                /* Bitwise operators require integral operands (not float/double) */
                if (strcmp(op, "&") == 0 || strcmp(op, "|") == 0 || strcmp(op, "^") == 0 ||
                    strcmp(op, "<<") == 0 || strcmp(op, ">>") == 0 || strcmp(op, ">>>") == 0) {
                    /* Check that operands are integral types (not float/double) */
                    type_kind_t lk = left_type->kind;
                    type_kind_t rk = right_type->kind;
                    /* Handle wrapper types */
                    if (lk == TYPE_CLASS && left_type->data.class_type.name) {
                        lk = get_primitive_for_wrapper(left_type->data.class_type.name);
                    }
                    if (rk == TYPE_CLASS && right_type->data.class_type.name) {
                        rk = get_primitive_for_wrapper(right_type->data.class_type.name);
                    }
                    bool left_integral = (lk == TYPE_INT || lk == TYPE_LONG || 
                                          lk == TYPE_BYTE || lk == TYPE_SHORT || lk == TYPE_CHAR);
                    bool right_integral = (rk == TYPE_INT || rk == TYPE_LONG || 
                                           rk == TYPE_BYTE || rk == TYPE_SHORT || rk == TYPE_CHAR);
                    if (!left_integral || !right_integral) {
                        semantic_error(sem, expr->line, expr->column,
                            "bad operand types for binary operator '%s': %s and %s",
                            op, type_to_string(left_type), type_to_string(right_type));
                        return type_new_primitive(TYPE_UNKNOWN);
                    }
                    /* Result is int or long based on binary numeric promotion */
                    if (lk == TYPE_LONG || rk == TYPE_LONG) {
                        return type_long();
                    }
                    return type_int();
                }
                
                /* Get effective type kinds (handles unboxing wrappers) */
                type_kind_t left_kind = left_type->kind;
                type_kind_t right_kind = right_type->kind;
                
                /* Handle wrapper types - get underlying primitive */
                if (left_kind == TYPE_CLASS && left_type->data.class_type.name) {
                    type_kind_t prim = get_primitive_for_wrapper(left_type->data.class_type.name);
                    if (prim != TYPE_UNKNOWN) left_kind = prim;
                }
                if (right_kind == TYPE_CLASS && right_type->data.class_type.name) {
                    type_kind_t prim = get_primitive_for_wrapper(right_type->data.class_type.name);
                    if (prim != TYPE_UNKNOWN) right_kind = prim;
                }
                
                /* Numeric binary operators - promote */
                bool left_numeric = type_is_numeric(left_type) || 
                    (left_type->kind == TYPE_CLASS && left_type->data.class_type.name &&
                     get_primitive_for_wrapper(left_type->data.class_type.name) != TYPE_UNKNOWN);
                bool right_numeric = type_is_numeric(right_type) ||
                    (right_type->kind == TYPE_CLASS && right_type->data.class_type.name &&
                     get_primitive_for_wrapper(right_type->data.class_type.name) != TYPE_UNKNOWN);
                
                if (left_numeric && right_numeric) {
                    /* Numeric promotion rules */
                    if (left_kind == TYPE_DOUBLE || right_kind == TYPE_DOUBLE) {
                        return type_double();
                    }
                    if (left_kind == TYPE_FLOAT || right_kind == TYPE_FLOAT) {
                        return type_float();
                    }
                    if (left_kind == TYPE_LONG || right_kind == TYPE_LONG) {
                        return type_long();
                    }
                    return type_int();
                }
                
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_UNARY_EXPR:
            {
                slist_t *children = expr->data.node.children;
                if (!children) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                type_t *operand_type = get_expression_type(sem, children->data);
                const char *op = expr->data.node.name;
                
                if (strcmp(op, "!") == 0) {
                    return type_boolean();
                }
                
                return operand_type;
            }
        
        case AST_ASSIGNMENT_EXPR:
            {
                /* Type of assignment is the type of left side */
                slist_t *children = expr->data.node.children;
                if (children) {
                    return get_expression_type(sem, children->data);
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_METHOD_CALL:
            {
                const char *method_name = expr->data.node.name;
                if (!method_name) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                slist_t *children = expr->data.node.children;
                symbol_t *target_class = NULL;
                symbol_t *found_method = NULL;
                type_t *recv_type_for_subst = NULL;  /* Track receiver type for type arg substitution */
                
                /* Count arguments early for overload resolution.
                 * Total children minus 1 for receiver (if explicit) */
                int arg_count = 0;
                bool has_explicit_receiver = (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER) != 0;
                for (slist_t *c = children; c; c = c->next) {
                    arg_count++;
                }
                if (has_explicit_receiver && arg_count > 0) {
                    arg_count--;  /* Don't count the receiver as an argument */
                }
                
                /* Determine the target class for the method */
                if (children && has_explicit_receiver) {
                    ast_node_t *first = (ast_node_t *)children->data;
                    
                    /* Check if first child is a receiver (only when has_explicit_receiver) */
                    if (first->type == AST_IDENTIFIER) {
                        const char *recv_name = first->data.leaf.name;
                        
                        /* First check if it's a class/enum name (for static method calls) */
                        symbol_t *class_sym = scope_lookup(sem->current_scope, recv_name);
                        
                        /* Also check in current class members for nested types */
                        if (!class_sym && sem->current_class && 
                            sem->current_class->data.class_data.members) {
                            class_sym = scope_lookup_local(
                                sem->current_class->data.class_data.members, recv_name);
                        }
                        
                        /* Check the types cache as a fallback for nested types.
                         * Check per-compilation-unit scope first, then global cache. */
                        if (!class_sym) {
                            type_t *type = hashtable_lookup(sem->unit_types, recv_name);
                            if (!type) {
                                type = hashtable_lookup(sem->types, recv_name);
                            }
                            if (type && type->kind == TYPE_CLASS && type->data.class_type.symbol) {
                                class_sym = type->data.class_type.symbol;
                            }
                        }
                        
                        /* Check superclass chain and their interfaces for inherited nested types.
                         * This handles cases like SimpleJavaFileObject extends JavaFileObject,
                         * where JavaFileObject.Kind should be accessible as just "Kind". */
                        if (!class_sym && sem->current_class) {
                            symbol_t *check_class = sem->current_class;
                            
                            /* Check current class and all enclosing classes */
                            while (check_class && !class_sym) {
                                /* Check superclass chain of this class */
                                symbol_t *super = check_class->data.class_data.superclass;
                                while (super && !class_sym) {
                                    /* Check superclass members for nested type */
                                    if (super->data.class_data.members) {
                                        symbol_t *nested = scope_lookup_local(
                                            super->data.class_data.members, recv_name);
                                        if (nested && (nested->kind == SYM_CLASS ||
                                                       nested->kind == SYM_INTERFACE ||
                                                       nested->kind == SYM_ENUM ||
                                                       nested->kind == SYM_RECORD)) {
                                            class_sym = nested;
                                            break;
                                        }
                                    }
                                    
                                    /* Check interfaces of this superclass */
                                    for (slist_t *iface = super->data.class_data.interfaces;
                                         iface && !class_sym; iface = iface->next) {
                                        symbol_t *iface_sym = (symbol_t *)iface->data;
                                        if (iface_sym && iface_sym->data.class_data.members) {
                                            symbol_t *nested = scope_lookup_local(
                                                iface_sym->data.class_data.members, recv_name);
                                            if (nested && (nested->kind == SYM_CLASS ||
                                                           nested->kind == SYM_INTERFACE ||
                                                           nested->kind == SYM_ENUM ||
                                                           nested->kind == SYM_RECORD)) {
                                                class_sym = nested;
                                                break;
                                            }
                                        }
                                    }
                                    
                                    super = super->data.class_data.superclass;
                                }
                                
                                /* Check directly implemented interfaces of this class */
                                if (!class_sym && check_class->data.class_data.interfaces) {
                                    for (slist_t *iface = check_class->data.class_data.interfaces;
                                         iface; iface = iface->next) {
                                        symbol_t *iface_sym = (symbol_t *)iface->data;
                                        if (iface_sym && (iface_sym->kind == SYM_INTERFACE ||
                                                          iface_sym->kind == SYM_CLASS) &&
                                            iface_sym->data.class_data.members) {
                                            symbol_t *nested = scope_lookup_local(
                                                iface_sym->data.class_data.members, recv_name);
                                            if (getenv("GENESIS_DEBUG_MCALL")) {
                                                fprintf(stderr, "DEBUG mcall: looked up '%s' in interface '%s' -> %p\n",
                                                    recv_name, iface_sym->name ? iface_sym->name : "(null)",
                                                    (void*)nested);
                                            }
                                            if (nested && (nested->kind == SYM_CLASS ||
                                                           nested->kind == SYM_INTERFACE ||
                                                           nested->kind == SYM_ENUM ||
                                                           nested->kind == SYM_RECORD)) {
                                                class_sym = nested;
                                                break;
                                            }
                                        }
                                    }
                                }
                                
                                check_class = check_class->data.class_data.enclosing_class;
                            }
                        }
                        
                        /* Try to load external class if not found locally */
                        if (!class_sym) {
                            /* First try resolving through imports */
                            char *qualified = resolve_import(sem, recv_name);
                            if (qualified) {
                                class_sym = load_external_class(sem, qualified);
                                free(qualified);
                            }
                            
                        if (!class_sym) {
                            class_sym = load_external_class(sem, recv_name);
                            }
                            /* Also try java.lang prefix for standard classes */
                            if (!class_sym) {
                                char java_lang[256];
                                snprintf(java_lang, sizeof(java_lang), "java.lang.%s", recv_name);
                                class_sym = load_external_class(sem, java_lang);
                            }
                        }
                        
                        if (class_sym && (class_sym->kind == SYM_CLASS || 
                                         class_sym->kind == SYM_INTERFACE ||
                                         class_sym->kind == SYM_ENUM)) {
                            /* It's a static method call on a class/enum type */
                            target_class = class_sym;
                            
                            /* Mark the identifier as a class reference for codegen */
                            first->sem_symbol = class_sym;
                            first->sem_type = class_sym->type;
                            
                            if (getenv("GENESIS_DEBUG_MCALL")) {
                                fprintf(stderr, "DEBUG mcall: target_class='%s' (kind=%d, members=%p) for method '%s'\n",
                                    class_sym->name ? class_sym->name : "(null)",
                                    class_sym->kind,
                                    (void*)class_sym->data.class_data.members,
                                    method_name);
                            }
                            
                            /* For enums, synthesize values() and valueOf() return types */
                            if (class_sym->kind == SYM_ENUM) {
                                if (strcmp(method_name, "values") == 0) {
                                    /* values() returns EnumType[] */
                                    type_t *array_type = type_new_array(class_sym->type, 1);
                                    return array_type;
                                } else if (strcmp(method_name, "valueOf") == 0) {
                                    /* valueOf(String) returns EnumType */
                                    return class_sym->type;
                                }
                            }
                            
                            /* Look up method in the class (use type-based resolution) */
                            if (class_sym->data.class_data.members) {
                                /* Arguments are children->next since first child is the receiver */
                                slist_t *args = children->next;
                                symbol_t *method = scope_lookup_method_with_types(sem,
                                    class_sym->data.class_data.members, method_name, args);
                                if (getenv("GENESIS_DEBUG_MCALL")) {
                                    fprintf(stderr, "DEBUG mcall: scope_lookup_method_with_types for '%s' -> %p\n",
                                        method_name, (void*)method);
                                }
                                if (method && method->kind == SYM_METHOD) {
                                    found_method = method;
                                }
                            }
                        }
                        
                        /* Otherwise check if it's a variable */
                        if (!target_class) {
                            symbol_t *sym = scope_lookup(sem->current_scope, recv_name);
                            
                            /* Check inherited fields from superclass chain */
                            if (!sym && sem->current_class) {
                                symbol_t *search_class = sem->current_class->data.class_data.superclass;
                                while (search_class && !sym) {
                                    if (search_class->data.class_data.members) {
                                        sym = scope_lookup_local(
                                            search_class->data.class_data.members, recv_name);
                                        if (sym && sym->kind != SYM_FIELD) {
                                            sym = NULL;  /* Only look for fields */
                                        }
                                    }
                                    search_class = search_class->data.class_data.superclass;
                                }
                            }
                            
                            if (sym && (sym->kind == SYM_LOCAL_VAR || sym->kind == SYM_PARAMETER ||
                                        sym->kind == SYM_FIELD)) {
                                /* Call get_expression_type on the identifier to trigger
                                 * capture detection if we're inside a lambda */
                                get_expression_type(sem, first);
                                
                                /* It's a variable - get its type to find the class */
                                type_t *recv_type = sym->type;
                                
                                /* Ensure the type has its symbol loaded for method lookup */
                                ensure_type_symbol_loaded(sem, recv_type);
                                
                                /* Track for type arg substitution.
                                 * If recv_type doesn't have type_args but has a parameterized superclass,
                                 * use the superclass_type for substitution (e.g., EntityStack extends ArrayDeque<E>) */
                                recv_type_for_subst = recv_type;
                                if (getenv("GENESIS_DEBUG_SUBST") && recv_type && recv_type->kind == TYPE_CLASS) {
                                    fprintf(stderr, "DEBUG recv_type check: '%s' type_args=%p symbol=%p superclass_type=%p\n",
                                        recv_type->data.class_type.name ? recv_type->data.class_type.name : "(null)",
                                        (void*)recv_type->data.class_type.type_args,
                                        (void*)recv_type->data.class_type.symbol,
                                        recv_type->data.class_type.symbol ? (void*)recv_type->data.class_type.symbol->data.class_data.superclass_type : NULL);
                                }
                                if (recv_type && recv_type->kind == TYPE_CLASS &&
                                    !recv_type->data.class_type.type_args &&
                                    recv_type->data.class_type.symbol &&
                                    recv_type->data.class_type.symbol->data.class_data.superclass_type &&
                                    recv_type->data.class_type.symbol->data.class_data.superclass_type->kind == TYPE_CLASS &&
                                    recv_type->data.class_type.symbol->data.class_data.superclass_type->data.class_type.type_args) {
                                    /* Only use superclass_type if it has type arguments for substitution */
                                    recv_type_for_subst = recv_type->data.class_type.symbol->data.class_data.superclass_type;
                                    if (getenv("GENESIS_DEBUG_SUBST")) {
                                        type_t *st = recv_type_for_subst;
                                        fprintf(stderr, "DEBUG recv_type_for_subst: using superclass_type '%s' (type_args=%p)\n",
                                            st && st->kind == TYPE_CLASS && st->data.class_type.name ? st->data.class_type.name : "(null)",
                                            st && st->kind == TYPE_CLASS ? (void*)st->data.class_type.type_args : NULL);
                                        if (st && st->kind == TYPE_CLASS && st->data.class_type.type_args) {
                                            for (slist_t *ta = st->data.class_type.type_args; ta; ta = ta->next) {
                                                type_t *arg = (type_t*)ta->data;
                                                fprintf(stderr, "  - type_arg: %s (kind=%d)\n",
                                                    arg && arg->kind == TYPE_CLASS && arg->data.class_type.name ? arg->data.class_type.name :
                                                    arg && arg->kind == TYPE_TYPEVAR && arg->data.type_var.name ? arg->data.type_var.name : "(unknown)",
                                                    arg ? arg->kind : -1);
                                            }
                                        }
                                    }
                                }
                                
                                /* Set sem_type on the receiver node so later processing skips it */
                                first->sem_type = recv_type;
                                
                                /* For type variables, use the bound type for method lookup */
                                if (recv_type && recv_type->kind == TYPE_TYPEVAR && 
                                    recv_type->data.type_var.bound) {
                                    recv_type = recv_type->data.type_var.bound;
                                }
                                
                                /* For wildcards with extends bound, use the bound type for method lookup */
                                if (recv_type && recv_type->kind == TYPE_WILDCARD && 
                                    recv_type->data.wildcard.bound_kind == 1 &&
                                    recv_type->data.wildcard.bound) {
                                    recv_type = recv_type->data.wildcard.bound;
                                }
                                
                                /* Handle method calls on arrays specially.
                                 * Arrays have clone() which returns the array type (covariant). */
                                if (recv_type && recv_type->kind == TYPE_ARRAY) {
                                    if (strcmp(method_name, "clone") == 0) {
                                        /* clone() on arrays returns the array type itself */
                                        expr->sem_type = recv_type;
                                        return recv_type;
                                    }
                                    /* For other methods (getClass, toString, etc.), use Object */
                                    symbol_t *obj_class = load_external_class(sem, "java.lang.Object");
                                    if (obj_class && obj_class->data.class_data.members) {
                                        slist_t *args = children->next;
                                        symbol_t *method = scope_lookup_method_with_types(sem,
                                            obj_class->data.class_data.members, method_name, args);
                                        if (method && method->kind == SYM_METHOD && method->type) {
                                            expr->sem_symbol = method;
                                            expr->sem_type = method->type;
                                            return method->type;
                                        }
                                    }
                                }
                                
                                if (recv_type && recv_type->kind == TYPE_CLASS &&
                                    recv_type->data.class_type.symbol) {
                                    symbol_t *recv_class = recv_type->data.class_type.symbol;
                                    symbol_t *method = NULL;
                                    /* Arguments are children->next since first child is the receiver */
                                    slist_t *args = children->next;
                                    
                                    /* Check direct members */
                                    if (recv_class->data.class_data.members) {
                                        method = scope_lookup_method_with_types_and_recv(sem,
                                            recv_class->data.class_data.members, method_name, args, recv_type_for_subst);
                                    }
                                    
                                    /* Check superclass chain */
                                    if (!method || method->kind != SYM_METHOD) {
                                        symbol_t *super = recv_class->data.class_data.superclass;
                                        while (super && (!method || method->kind != SYM_METHOD)) {
                                            if (super->data.class_data.members) {
                                                method = scope_lookup_method_with_types_and_recv(sem,
                                                    super->data.class_data.members, method_name, args, recv_type_for_subst);
                                            }
                                            super = super->data.class_data.superclass;
                                        }
                                    }
                                    
                                    /* Check interface hierarchy */
                                    if (!method || method->kind != SYM_METHOD) {
                                        method = lookup_method_in_interfaces(sem, recv_class, method_name);
                                    }
                                    
                                    /* For enums, also check java.lang.Enum explicitly */
                                    if ((!method || method->kind != SYM_METHOD) && 
                                        recv_class->kind == SYM_ENUM) {
                                        symbol_t *enum_class = load_external_class(sem, "java.lang.Enum");
                                        if (enum_class && enum_class->data.class_data.members) {
                                            method = scope_lookup_method(
                                                enum_class->data.class_data.members, method_name);
                                        }
                                    }
                                    
                                    /* For any class type, also check java.lang.Object */
                                    if (!method || method->kind != SYM_METHOD) {
                                        symbol_t *obj_class = load_external_class(sem, "java.lang.Object");
                                        if (obj_class && obj_class->data.class_data.members) {
                                            method = scope_lookup_method(
                                                obj_class->data.class_data.members, method_name);
                                        }
                                    }
                                    
                                    if (method && method->kind == SYM_METHOD) {
                                        target_class = recv_class;
                                        found_method = method;
                                    }
                                }
                            }
                        }
                    } else if (first->type == AST_THIS_EXPR) {
                        target_class = sem->current_class;
                    } else if (first->type == AST_FIELD_ACCESS) {
                        type_t *recv_type = get_expression_type(sem, first);
                        if (recv_type && recv_type->kind == TYPE_ARRAY) {
                            /* Method call on array - handle specially.
                             * Arrays have clone() which returns the array type (covariant).
                             * Other methods come from java.lang.Object. */
                            if (strcmp(method_name, "clone") == 0) {
                                /* clone() on arrays returns the array type itself */
                                expr->sem_type = recv_type;
                                return recv_type;
                            }
                            /* For other methods (getClass, toString, etc.), use Object */
                            target_class = load_external_class(sem, "java.lang.Object");
                            recv_type_for_subst = recv_type;
                        } else if (recv_type && recv_type->kind == TYPE_CLASS) {
                            target_class = recv_type->data.class_type.symbol;
                            /* Load class if symbol not yet set (e.g., from classfile descriptor) */
                            if (!target_class && recv_type->data.class_type.name) {
                                target_class = load_external_class(sem, recv_type->data.class_type.name);
                            }
                            /* Track for type arg substitution */
                            recv_type_for_subst = recv_type;
                            if (!recv_type->data.class_type.type_args && target_class &&
                                target_class->data.class_data.superclass_type &&
                                target_class->data.class_data.superclass_type->kind == TYPE_CLASS &&
                                target_class->data.class_data.superclass_type->data.class_type.type_args) {
                                /* Only use superclass_type if it has type arguments for substitution */
                                recv_type_for_subst = target_class->data.class_data.superclass_type;
                            }
                        }
                    } else if ((first->type == AST_LITERAL || 
                                first->type == AST_NEW_OBJECT ||
                                first->type == AST_ARRAY_ACCESS ||
                                first->type == AST_PARENTHESIZED ||
                                first->type == AST_CAST_EXPR ||
                                first->type == AST_CONDITIONAL_EXPR ||
                                first->type == AST_METHOD_CALL ||
                               first->type == AST_CLASS_LITERAL ||
                               first->type == AST_FIELD_ACCESS) &&
                               (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER)) {
                        /* Handle method calls on literals and other expressions */
                        /* e.g., "abc".trim(), new String().length(), (obj).method() */
                        /* Also handles chained method calls: "abc".trim().toUpperCase() */
                        /* Only when there's an explicit receiver (dot notation) */
                        type_t *recv_type = get_expression_type(sem, first);
                        recv_type_for_subst = recv_type;  /* Track for type arg substitution */
                        if (recv_type && recv_type->kind == TYPE_CLASS &&
                            !recv_type->data.class_type.type_args &&
                            recv_type->data.class_type.symbol &&
                            recv_type->data.class_type.symbol->data.class_data.superclass_type &&
                            recv_type->data.class_type.symbol->data.class_data.superclass_type->kind == TYPE_CLASS &&
                            recv_type->data.class_type.symbol->data.class_data.superclass_type->data.class_type.type_args) {
                            /* Only use superclass_type if it has type arguments for substitution */
                            recv_type_for_subst = recv_type->data.class_type.symbol->data.class_data.superclass_type;
                        }
                        
                        /* For chained method calls, set sem_type on the receiver so
                         * codegen can access it when generating the call chain */
                        if (recv_type && first->type == AST_METHOD_CALL && !first->sem_type) {
                            first->sem_type = recv_type;
                        }
                        if (recv_type && recv_type->kind == TYPE_ARRAY) {
                            /* Method call on array - handle specially.
                             * Arrays have clone() which returns the array type (covariant).
                             * Other methods come from java.lang.Object. */
                            if (strcmp(method_name, "clone") == 0) {
                                /* clone() on arrays returns the array type itself */
                                expr->sem_type = recv_type;
                                return recv_type;
                            }
                            /* For other methods (getClass, toString, etc.), use Object */
                            symbol_t *obj_class = load_external_class(sem, "java.lang.Object");
                            if (obj_class && obj_class->data.class_data.members) {
                                slist_t *args = children->next;
                                symbol_t *method = scope_lookup_method_with_types(sem,
                                    obj_class->data.class_data.members, method_name, args);
                                if (method && method->kind == SYM_METHOD && method->type) {
                                    expr->sem_symbol = method;
                                    expr->sem_type = method->type;
                                    return method->type;
                                }
                            }
                        } else if (recv_type && recv_type->kind == TYPE_CLASS) {
                            /* Get symbol from type, or load externally if needed */
                            target_class = recv_type->data.class_type.symbol;
                            if (!target_class && recv_type->data.class_type.name) {
                                /* Try to load the class (e.g., for String literals) */
                                target_class = load_external_class(sem, 
                                    recv_type->data.class_type.name);
                            }
                            
                            /* Look up method in the class and its superclasses */
                            symbol_t *method = NULL;
                            symbol_t *recv_class = target_class;
                            /* Arguments are children->next since first child is the receiver */
                            slist_t *args = children->next;
                            
                            /* Check direct members (using type-based resolution) */
                            if (recv_class && recv_class->data.class_data.members) {
                                method = scope_lookup_method_with_types(sem,
                                    recv_class->data.class_data.members, method_name, args);
                            }
                            
                            /* Check superclass chain */
                            if (recv_class && (!method || method->kind != SYM_METHOD)) {
                                /* Walk up the superclass chain to find the method */
                                symbol_t *super = recv_class->data.class_data.superclass;
                                type_t *superclass_type_at_depth = recv_class->data.class_data.superclass_type;
                                while (super && (!method || method->kind != SYM_METHOD)) {
                                    if (super->data.class_data.members) {
                                        method = scope_lookup_method_with_types(sem,
                                            super->data.class_data.members, method_name, args);
                                        if (method && method->kind == SYM_METHOD) {
                                            /* Method found in superclass at this depth.
                                             * Use the parameterized superclass type for substitution */
                                            if (superclass_type_at_depth) {
                                                recv_type_for_subst = superclass_type_at_depth;
                                    }
                                            break;
                                        }
                                    }
                                    /* Move to next level: use this superclass's superclass_type */
                                    superclass_type_at_depth = super->data.class_data.superclass_type;
                                    super = super->data.class_data.superclass;
                                }
                            }
                            
                            /* Check interface hierarchy */
                            if (recv_class && (!method || method->kind != SYM_METHOD)) {
                                /* For interface methods, use the receiver's superclass_type if available 
                                 * and it has type arguments for substitution */
                                method = lookup_method_in_interfaces(sem, recv_class, method_name);
                                if (method && method->kind == SYM_METHOD && 
                                    recv_class->data.class_data.superclass_type &&
                                    recv_class->data.class_data.superclass_type->kind == TYPE_CLASS &&
                                    recv_class->data.class_data.superclass_type->data.class_type.type_args) {
                                    recv_type_for_subst = recv_class->data.class_data.superclass_type;
                                }
                            }
                            
                            /* For any class type, also check java.lang.Object */
                            if (!method || method->kind != SYM_METHOD) {
                                symbol_t *obj_class = load_external_class(sem, "java.lang.Object");
                                if (obj_class && obj_class->data.class_data.members) {
                                    method = scope_lookup_method_with_types(sem,
                                        obj_class->data.class_data.members, method_name, args);
                                }
                            }
                            
                            if (method && method->kind == SYM_METHOD) {
                                found_method = method;
                            }
                        }
                    }
                }
                
                /* If no receiver found, check current class for method */
                if (!target_class && sem->current_class) {
                    target_class = sem->current_class;
                }
                
                /* Look up the method in the target class if not already found */
                /* For methods without explicit receiver, arguments are all children */
                slist_t *method_args = has_explicit_receiver ? children->next : children;
                if (!found_method && target_class && target_class->data.class_data.members) {
                    found_method = scope_lookup_method_with_types(sem,
                        target_class->data.class_data.members, method_name, method_args);
                }
                
                /* If not found, check superclass chain (including java.lang.Enum for enums) */
                if (!found_method && target_class) {
                    symbol_t *super = target_class->data.class_data.superclass;
                    type_t *superclass_type_at_depth = target_class->data.class_data.superclass_type;
                    if (getenv("GENESIS_DEBUG_SUBST")) {
                        fprintf(stderr, "DEBUG implicit method '%s': target_class='%s' superclass_type=%p\n",
                            method_name ? method_name : "(null)",
                            target_class->name ? target_class->name : "(null)",
                            (void*)superclass_type_at_depth);
                        if (superclass_type_at_depth && superclass_type_at_depth->kind == TYPE_CLASS) {
                            fprintf(stderr, "  superclass_type: '%s' type_args=%p\n",
                                superclass_type_at_depth->data.class_type.name ? superclass_type_at_depth->data.class_type.name : "(null)",
                                (void*)superclass_type_at_depth->data.class_type.type_args);
                        }
                    }
                    while (!found_method && super) {
                        if (super->data.class_data.members) {
                            found_method = scope_lookup_method_with_types(sem,
                                super->data.class_data.members, method_name, method_args);
                            if (found_method && found_method->kind == SYM_METHOD) {
                                /* Use parameterized superclass type for substitution */
                                if (superclass_type_at_depth) {
                                    recv_type_for_subst = superclass_type_at_depth;
                                    if (getenv("GENESIS_DEBUG_SUBST")) {
                                        fprintf(stderr, "DEBUG implicit method '%s' found in '%s', using superclass_type '%s' (type_args=%p)\n",
                                            method_name, super->name ? super->name : "(null)",
                                            superclass_type_at_depth->kind == TYPE_CLASS && superclass_type_at_depth->data.class_type.name ?
                                                superclass_type_at_depth->data.class_type.name : "(null)",
                                            superclass_type_at_depth->kind == TYPE_CLASS ? 
                                                (void*)superclass_type_at_depth->data.class_type.type_args : NULL);
                                    }
                                }
                                break;
                            }
                        }
                        superclass_type_at_depth = super->data.class_data.superclass_type;
                        super = super->data.class_data.superclass;
                    }
                    
                    /* For enums without explicit superclass, check java.lang.Enum */
                    if (!found_method && target_class->kind == SYM_ENUM) {
                        symbol_t *enum_class = load_external_class(sem, "java.lang.Enum");
                        if (enum_class && enum_class->data.class_data.members) {
                            found_method = scope_lookup_method_with_types(sem,
                                enum_class->data.class_data.members, method_name, method_args);
                        }
                    }
                    
                    /* Check interfaces of current class and superclass chain for default methods */
                    if (!found_method) {
                        symbol_t *check_class = target_class;
                        while (check_class && !found_method) {
                            /* Check interfaces implemented by this class */
                            for (slist_t *iface = check_class->data.class_data.interfaces;
                                 iface && !found_method; iface = iface->next) {
                                symbol_t *iface_sym = (symbol_t *)iface->data;
                                if (iface_sym && iface_sym->data.class_data.members) {
                                    found_method = scope_lookup_method_with_types(sem,
                                        iface_sym->data.class_data.members, method_name, method_args);
                                }
                                /* Also check superinterfaces recursively */
                                if (!found_method && iface_sym) {
                                    found_method = lookup_method_in_interfaces(sem, iface_sym, method_name);
                                }
                            }
                            check_class = check_class->data.class_data.superclass;
                        }
                    }
                }
                
                /* If not found and we're in a nested class without explicit receiver,
                 * check enclosing classes for the method (Java allows calling outer class methods) */
                if (!found_method && !has_explicit_receiver && sem->current_class &&
                    sem->current_class->data.class_data.enclosing_class) {
                    symbol_t *enclosing = sem->current_class->data.class_data.enclosing_class;
                    while (!found_method && enclosing) {
                        /* Look in enclosing class */
                        if (enclosing->data.class_data.members) {
                            found_method = scope_lookup_method_with_types(sem,
                                enclosing->data.class_data.members, method_name, method_args);
                        }
                        
                        /* Check superclass chain of enclosing class too */
                        if (!found_method) {
                            symbol_t *super = enclosing->data.class_data.superclass;
                            while (!found_method && super) {
                                if (super->data.class_data.members) {
                                    found_method = scope_lookup_method_with_types(sem,
                                        super->data.class_data.members, method_name, method_args);
                                }
                                super = super->data.class_data.superclass;
                            }
                        }
                        
                        if (found_method) {
                            target_class = enclosing;
                        }
                        
                        enclosing = enclosing->data.class_data.enclosing_class;
                    }
                }
                
                if (found_method && found_method->kind == SYM_METHOD && found_method->type) {
                    /* Check access control (skip if target is current class) */
                    if (target_class != sem->current_class &&
                        !check_access(found_method->modifiers, target_class, sem->current_class)) {
                        semantic_error(sem, expr->line, expr->column,
                            "method '%s' has %s access in '%s'",
                            method_name,
                            (found_method->modifiers & MOD_PRIVATE) ? "private" :
                            (found_method->modifiers & MOD_PROTECTED) ? "protected" : "package-private",
                            target_class->name);
                    }
                    
                    /* Infer type arguments from target type if available.
                     * E.g., for Arrays.asList() returning List<T>, if target is List<Function<String,Object>>,
                     * infer T = Function<String,Object> */
                    type_t *inferred_type_arg = NULL;
                    if (sem->target_type && sem->target_type->kind == TYPE_CLASS &&
                        sem->target_type->data.class_type.type_args &&
                        found_method->type && found_method->type->kind == TYPE_CLASS) {
                        /* Check if return type has type arguments that match target */
                        slist_t *ret_type_args = found_method->type->data.class_type.type_args;
                        slist_t *target_type_args = sem->target_type->data.class_type.type_args;
                        if (ret_type_args && target_type_args) {
                            type_t *ret_arg = (type_t *)ret_type_args->data;
                            type_t *target_arg = (type_t *)target_type_args->data;
                            /* If return type arg is a type variable, infer from target */
                            if (ret_arg && ret_arg->kind == TYPE_TYPEVAR && target_arg) {
                                inferred_type_arg = target_arg;
                            }
                        }
                    }
                    
                    /* Check argument types against parameter types */
                    slist_t *params = found_method->data.method_data.parameters;
                    slist_t *args = children;
                    
                    /* Skip receiver argument if present - check if explicit receiver flag is set */
                    if (args && target_class && (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER)) {
                        /* Skip the first argument (receiver) */
                                        args = args->next;
                    }
                    
                    /* Track inferred type arguments for static generic methods */
                    type_t *inferred_T = NULL;  /* For type variable T (most common) */
                    type_t *inferred_R = NULL;  /* For type variable R (Function return type) */
                    type_t *inferred_E = NULL;  /* For type variable E (collection element) */
                    type_t *inferred_A = NULL;  /* For type variable A (Files.readAttributes etc) */
                    
                    /* Check each argument type */
                    slist_t *param_node = params;
                    slist_t *arg_node = args;
                    int arg_index = 0;
                    
                    while (param_node && arg_node) {
                        symbol_t *param = (symbol_t *)param_node->data;
                        ast_node_t *arg = (ast_node_t *)arg_node->data;
                        
                        type_t *param_type = param->type;
                        
                        /* Substitute receiver type arguments into parameter type */
                        if (recv_type_for_subst && param_type) {
                            param_type = substitute_from_receiver(param_type, recv_type_for_subst);
                        }
                        
                        /* Check if this is a varargs parameter */
                        bool is_varargs_param = (param->modifiers & MOD_VARARGS) != 0;
                        bool is_last_param = (param_node->next == NULL);
                        
                        /* For varargs, get the element type for lambda/method ref binding */
                        type_t *bind_type = param_type;
                        if (is_varargs_param && is_last_param && param_type &&
                            param_type->kind == TYPE_ARRAY) {
                            bind_type = param_type->data.array_type.element_type;
                        }
                        
                        /* Substitute inferred type argument if bind_type is a type variable.
                         * Only use target-inferred type for lambda/method ref binding.
                         * For regular arguments, we'll infer from the argument type instead. */
                        type_t *bind_type_for_lambda = bind_type;
                        if (bind_type_for_lambda && bind_type_for_lambda->kind == TYPE_TYPEVAR && inferred_type_arg) {
                            bind_type_for_lambda = inferred_type_arg;
                        }
                        /* Also substitute if bind_type is an array of a type variable (e.g., T[]) */
                        else if (bind_type_for_lambda && bind_type_for_lambda->kind == TYPE_ARRAY && inferred_type_arg) {
                            type_t *elem_type = bind_type_for_lambda->data.array_type.element_type;
                            if (elem_type && elem_type->kind == TYPE_TYPEVAR) {
                                bind_type_for_lambda = type_new_array(inferred_type_arg, bind_type_for_lambda->data.array_type.dimensions);
                            }
                        }
                        
                        /* For lambda/method ref arguments, bind to parameter type first */
                        /* Unwrap parentheses to find lambda/method ref */
                        ast_node_t *unwrapped_arg = arg;
                        while (unwrapped_arg->type == AST_PARENTHESIZED && 
                               unwrapped_arg->data.node.children) {
                            unwrapped_arg = (ast_node_t *)unwrapped_arg->data.node.children->data;
                        }
                        if (unwrapped_arg->type == AST_LAMBDA_EXPR && bind_type_for_lambda) {
                            bind_lambda_to_target_type(sem, unwrapped_arg, bind_type_for_lambda);
                        } else if (unwrapped_arg->type == AST_METHOD_REF && bind_type_for_lambda) {
                            bind_method_ref_to_target_type(sem, unwrapped_arg, bind_type_for_lambda);
                        }
                        
                        type_t *arg_type = get_expression_type(sem, arg);
                        
                        /* Infer type arguments from argument types for generic methods */
                        if (arg_type && param->type) {
                            /* For varargs (T[]), use the element type for inference */
                            type_t *infer_from = param->type;
                            if (is_varargs_param && is_last_param && 
                                infer_from->kind == TYPE_ARRAY) {
                                infer_from = infer_from->data.array_type.element_type;
                            }
                            
                            if (!inferred_T) {
                                inferred_T = infer_type_arg(infer_from, arg_type, "T");
                            }
                            if (!inferred_E) {
                                inferred_E = infer_type_arg(infer_from, arg_type, "E");
                            }
                            if (!inferred_R) {
                                inferred_R = infer_type_arg(infer_from, arg_type, "R");
                            }
                            if (!inferred_A) {
                                inferred_A = infer_type_arg(infer_from, arg_type, "A");
                            }
                        }
                        
                        if (is_varargs_param && is_last_param && bind_type) {
                            /* Varargs parameter - bind_type is the element type */
                            
                            /* Ensure symbols loaded for subtype checks */
                            ensure_type_symbol_loaded(sem, param_type);
                            ensure_type_symbol_loaded(sem, arg_type);
                            ensure_type_symbol_loaded(sem, bind_type);
                            
                            /* Check if argument is array (passing whole array) */
                            if (arg_type && arg_type->kind == TYPE_ARRAY && param_type->kind == TYPE_ARRAY) {
                                if (!type_assignable(param_type, arg_type)) {
                                    char *expected = type_to_string(param_type);
                                    char *actual = type_to_string(arg_type);
                                    semantic_error(sem, arg->line, arg->column,
                                        "argument %d: incompatible types - expected %s, got %s",
                                        arg_index + 1, expected, actual);
                                    free(expected);
                                    free(actual);
                                }
                                /* Only one array arg for varargs - done */
                                break;
                            }
                            
                            /* Check element type for individual varargs.
                             * Skip check if bind_type is a type variable - the type will be
                             * inferred from the argument type instead of checked against it. */
                            if (bind_type && bind_type->kind != TYPE_TYPEVAR &&
                                arg_type && !type_assignable(bind_type, arg_type)) {
                                char *expected = type_to_string(bind_type);
                                char *actual = type_to_string(arg_type);
                                semantic_error(sem, arg->line, arg->column,
                                    "argument %d: incompatible types - expected %s, got %s",
                                    arg_index + 1, expected, actual);
                                free(expected);
                                free(actual);
                            }
                            
                            /* Continue checking remaining varargs */
                            arg_node = arg_node->next;
                            arg_index++;
                            continue;
                        }
                        
                        /* Regular parameter type check */
                        /* Ensure symbols are loaded for proper subtype checking */
                        ensure_type_symbol_loaded(sem, param_type);
                        ensure_type_symbol_loaded(sem, arg_type);
                        
                        /* Check for void argument (void method return used as arg) */
                        if (arg_type && arg_type->kind == TYPE_VOID) {
                            semantic_error(sem, arg->line, arg->column,
                                "'void' type not allowed here");
                        }
                        else if (param_type && arg_type && !type_assignable(param_type, arg_type)) {
                            char *expected = type_to_string(param_type);
                            char *actual = type_to_string(arg_type);
                            semantic_error(sem, arg->line, arg->column,
                                "argument %d: incompatible types - expected %s, got %s",
                                arg_index + 1, expected, actual);
                            free(expected);
                            free(actual);
                        }
                        
                        param_node = param_node->next;
                        arg_node = arg_node->next;
                        arg_index++;
                    }
                    
                    /* Store the found method for codegen */
                    expr->sem_symbol = found_method;
                    
                    /* Substitute receiver type arguments into return type */
                    type_t *return_type = found_method->type;
                    if (recv_type_for_subst && return_type) {
                        /* If method returns Object and we have parameterized superclass,
                         * look for generic version of method in superclass hierarchy */
                        if (return_type->kind == TYPE_CLASS && 
                            return_type->data.class_type.name &&
                            strcmp(return_type->data.class_type.name, "java.lang.Object") == 0 &&
                            recv_type_for_subst->kind == TYPE_CLASS &&
                            recv_type_for_subst->data.class_type.type_args &&
                            recv_type_for_subst->data.class_type.symbol) {
                            /* Look for method in superclass with generic return type */
                            symbol_t *super_sym = recv_type_for_subst->data.class_type.symbol;
                            if (super_sym && super_sym->data.class_data.members) {
                                symbol_t *super_method = scope_lookup_method_with_types(sem,
                                    super_sym->data.class_data.members, method_name, children->next);
                                if (super_method && super_method->type && 
                                    super_method->type->kind == TYPE_TYPEVAR) {
                                    /* Use the generic superclass method's return type */
                                    return_type = super_method->type;
                                }
                            }
                        }
                        
                        return_type = substitute_from_receiver(return_type, recv_type_for_subst);
                        /* Ensure the substituted type has its symbol loaded */
                        ensure_type_symbol_loaded(sem, return_type);
                    }
                    
                    /* Substitute inferred type arguments for generic methods.
                     * This handles method-level type params like R in map<R>().
                     * Both static and instance methods may have type parameters. */
                    if (return_type) {
                        if (inferred_T) {
                            return_type = substitute_type_var(return_type, "T", inferred_T);
                        }
                        if (inferred_E) {
                            return_type = substitute_type_var(return_type, "E", inferred_E);
                        }
                        if (inferred_R) {
                            return_type = substitute_type_var(return_type, "R", inferred_R);
                        }
                        if (inferred_A) {
                            return_type = substitute_type_var(return_type, "A", inferred_A);
                        }
                    }
                    
                    /* Store the resolved return type for use by outer expressions.
                     * This is critical for method overload resolution - e.g., println(obj.get())
                     * needs to know the actual return type of get() to select the right println. */
                    expr->sem_type = return_type;
                    
                    return return_type;
                }
                
                /* Check static imports for static method */
                symbol_t *method_sym = NULL;
                symbol_t *import_class = resolve_static_import_method(sem, method_name, children, &method_sym);
                if (import_class && method_sym) {
                    /* Store the import class on the expression for codegen */
                    expr->sem_symbol = method_sym;
                    return method_sym->type;
                }
                
                /* Method not found - return unknown */
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_FIELD_ACCESS:
            {
                /* Field access: object.field */
                slist_t *children = expr->data.node.children;
                if (!children) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                /* First child is the object expression */
                ast_node_t *object_expr = (ast_node_t *)children->data;
                
                /* Get the field name (stored in node.name) */
                const char *field_name = expr->data.node.name;
                if (!field_name) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                /* Handle qualified 'this' (ClassName.this) and qualified 'super' (ClassName.super) */
                if (strcmp(field_name, "this") == 0 || strcmp(field_name, "super") == 0) {
                    /* The object expression should be a class name identifier */
                    if (object_expr->type == AST_IDENTIFIER) {
                        const char *class_name = object_expr->data.leaf.name;
                        /* Find the named class in enclosing class chain */
                        symbol_t *target_class = sem->current_class;
                        while (target_class) {
                            if (target_class->name && strcmp(target_class->name, class_name) == 0) {
                                break;
                            }
                            target_class = target_class->data.class_data.enclosing_class;
                        }
                        
                        if (target_class) {
                            if (strcmp(field_name, "this") == 0) {
                                /* Track 'this' capture for lambdas */
                                if (sem->current_lambda) {
                                    sem->current_lambda->lambda_captures_this = true;
                                }
                                return target_class->type;
                            } else {
                                /* qualified 'super' */
                                if (target_class->data.class_data.superclass) {
                                    if (sem->current_lambda) {
                                        sem->current_lambda->lambda_captures_this = true;
                                    }
                                    return target_class->data.class_data.superclass->type;
                                }
                            }
                        }
                    }
                    /* Couldn't resolve qualified this/super */
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                /* Before recursing, check if this could be a fully qualified class name
                 * or a class reference that needs import resolution.
                 * Build the parts from the identifier chain and try to resolve. */
                {
                    char fqn[1024];
                    fqn[0] = '\0';
                    ast_node_t *node = expr;
                    ast_node_t *root_node = NULL;  /* Track the root of the chain */
                    slist_t *visited_nodes = NULL;  /* Track nodes to mark if FQN succeeds */
                    slist_t *parts = NULL;  /* List of parts from root to current */
                    
                    /* Traverse the chain backwards to collect parts */
                    while (node && (node->type == AST_FIELD_ACCESS || node->type == AST_IDENTIFIER)) {
                        visited_nodes = slist_prepend(visited_nodes, node);
                        root_node = node;  /* Remember the last valid node */
                        
                        const char *part = (node->type == AST_FIELD_ACCESS) ?
                                          node->data.node.name : node->data.leaf.name;
                        if (part) {
                            parts = slist_prepend(parts, (void *)part);
                        }
                        
                        if (node->type == AST_FIELD_ACCESS) {
                            slist_t *ch = node->data.node.children;
                            node = ch ? (ast_node_t *)ch->data : NULL;
                        } else {
                            break;  /* Reached the root identifier */
                        }
                    }
                    
                    /* Only try class resolution if the root is an identifier
                     * (not 'this', 'super', method call, etc.) */
                    bool is_valid_fqn_chain = root_node && root_node->type == AST_IDENTIFIER;
                    
                    if (is_valid_fqn_chain && parts) {
                        /* Strategy: Try progressively longer prefixes as class names.
                         * For "ElementDeclaration.ContentType.EMPTY":
                         * 1. Try to resolve "ElementDeclaration" as a class
                         *    (via imports, same-package, or as qualified name)
                         * 2. If found, look for "ContentType" as a nested class
                         * 3. If found, look for "EMPTY" as a field/constant in ContentType
                         */
                        symbol_t *resolved_class = NULL;
                        slist_t *remaining_parts = parts;
                        
                        /* First, try to resolve just the first part via imports/same-package */
                        const char *first_part = (const char *)parts->data;
                        
                        if (getenv("GENESIS_DEBUG_LOAD")) {
                            fprintf(stderr, "DEBUG get_expr_type: trying to resolve '%s' (root of chain)\n", first_part);
                            fprintf(stderr, "DEBUG get_expr_type: current_package='%s'\n", 
                                sem->current_package ? sem->current_package : "(null)");
                        }
                        
                        /* IMPORTANT: First check if the first part is a local variable or parameter.
                         * If it is, skip FQN resolution entirely - this is not a class reference.
                         * This prevents "server.field" from being confused with "Server.field" when
                         * there's a parameter named 'server' and a class named 'Server'. */
                        symbol_t *local_sym = scope_lookup(sem->current_scope, first_part);
                        
                        /* Check for local nested types FIRST (class/enum/interface/record).
                         * This handles inner classes/enums being referenced before imports are checked.
                         * E.g., "State.IDLE" where State is a nested enum in the current class. */
                        if (local_sym && (local_sym->kind == SYM_CLASS ||
                                          local_sym->kind == SYM_INTERFACE ||
                                          local_sym->kind == SYM_ENUM ||
                                          local_sym->kind == SYM_RECORD)) {
                            /* Found a local nested type - use it directly, skipping import resolution */
                            resolved_class = local_sym;
                            remaining_parts = parts->next;
                            if (root_node && root_node->type == AST_IDENTIFIER) {
                                root_node->sem_symbol = local_sym;
                                root_node->sem_type = local_sym->type;
                            }
                            /* Jump to field lookup below */
                        }
                        else if (local_sym && (local_sym->kind == SYM_LOCAL_VAR || 
                                          local_sym->kind == SYM_PARAMETER ||
                                          local_sym->kind == SYM_FIELD)) {
                            /* It's a local variable, parameter, or field reference.
                             * Skip FQN resolution - fall through to normal field access. */
                            if (getenv("GENESIS_DEBUG_LOAD")) {
                                fprintf(stderr, "DEBUG get_expr_type: '%s' is a local/param/field (kind=%d), skipping FQN resolution\n",
                                    first_part, local_sym->kind);
                            }
                            slist_free(parts);
                            slist_free(visited_nodes);
                            goto normal_field_access;
                        }
                        
                        /* First, check if the first part is a nested type of an implemented interface
                         * OR a nested type inherited from the superclass chain.
                         * This handles cases like "Type.TCP" where Type is defined in ChannelHandler,
                         * or "Kind.SOURCE" where Kind is JavaFileObject$Kind inherited via SimpleJavaFileObject.
                         * Skip if we already found it as a local nested type. */
                        symbol_t *first_sym = NULL;
                        if (!resolved_class && sem->current_class) {
                            /* Check superclass chain and their interfaces */
                            symbol_t *check_class = sem->current_class;
                            while (check_class && !first_sym) {
                                /* Check superclass chain */
                                symbol_t *super = check_class->data.class_data.superclass;
                                while (super && !first_sym) {
                                    /* Check superclass members */
                                    if (super->data.class_data.members) {
                                        first_sym = scope_lookup_local(
                                            super->data.class_data.members, first_part);
                                        if (first_sym && !(first_sym->kind == SYM_CLASS ||
                                                           first_sym->kind == SYM_INTERFACE ||
                                                           first_sym->kind == SYM_ENUM)) {
                                            first_sym = NULL;  /* Not a type, ignore */
                                        }
                                    }
                                    
                                    /* Check interfaces of this superclass */
                                    for (slist_t *iface = super->data.class_data.interfaces;
                                         iface && !first_sym; iface = iface->next) {
                                        symbol_t *iface_sym = (symbol_t *)iface->data;
                                        if (iface_sym && iface_sym->data.class_data.members) {
                                            first_sym = scope_lookup_local(
                                                iface_sym->data.class_data.members, first_part);
                                            if (first_sym && !(first_sym->kind == SYM_CLASS ||
                                                               first_sym->kind == SYM_INTERFACE ||
                                                               first_sym->kind == SYM_ENUM)) {
                                                first_sym = NULL;  /* Not a type, ignore */
                                            }
                                        }
                                    }
                                    
                                    super = super->data.class_data.superclass;
                                }
                                
                                /* Check directly implemented interfaces */
                                for (slist_t *iface = check_class->data.class_data.interfaces;
                                     iface && !first_sym; iface = iface->next) {
                                    symbol_t *iface_sym = (symbol_t *)iface->data;
                                    if (iface_sym && iface_sym->data.class_data.members) {
                                        first_sym = scope_lookup_local(
                                            iface_sym->data.class_data.members, first_part);
                                        if (first_sym && !(first_sym->kind == SYM_CLASS ||
                                                           first_sym->kind == SYM_INTERFACE ||
                                                           first_sym->kind == SYM_ENUM)) {
                                            first_sym = NULL;  /* Not a type, ignore */
                                        }
                                    }
                                }
                                
                                check_class = check_class->data.class_data.enclosing_class;
                            }
                        }
                        
                        /* Then try import resolution (skip if we already found the type) */
                        if (!resolved_class && !first_sym) {
                            char *qualified_first = resolve_import(sem, first_part);
                        
                        if (getenv("GENESIS_DEBUG_LOAD")) {
                            fprintf(stderr, "DEBUG get_expr_type: resolve_import returned '%s'\n",
                                qualified_first ? qualified_first : "(null)");
                        }
                        
                        if (qualified_first) {
                            first_sym = load_external_class(sem, qualified_first);
                            if (getenv("GENESIS_DEBUG_LOAD")) {
                                fprintf(stderr, "DEBUG get_expr_type: load_external_class('%s') returned %p (kind=%d)\n",
                                    qualified_first, (void*)first_sym, first_sym ? first_sym->kind : -1);
                            }
                            free(qualified_first);
                            }
                        }
                        
                        if (!resolved_class && first_sym && (first_sym->kind == SYM_CLASS || 
                            first_sym->kind == SYM_INTERFACE || first_sym->kind == SYM_ENUM)) {
                            /* First part resolves to a class, now find nested classes */
                            resolved_class = first_sym;
                            remaining_parts = parts->next;
                            
                            /* Mark the first identifier node with the class symbol */
                            if (root_node && root_node->type == AST_IDENTIFIER) {
                                root_node->sem_symbol = first_sym;
                                root_node->sem_type = first_sym->type;
                            }
                            
                            /* Track which visited_node corresponds to current part.
                             * visited_nodes and parts are both in root-to-leaf order
                             * (due to prepend). visited_nodes[0] is root (identifier),
                             * visited_nodes[1] is first field access, etc. */
                            slist_t *current_visited = visited_nodes ? visited_nodes->next : NULL;
                            
                            /* Traverse remaining parts looking for nested classes */
                            while (remaining_parts && resolved_class) {
                                const char *nested_name = (const char *)remaining_parts->data;
                                symbol_t *nested = NULL;
                                
                                /* Look in class members for nested class */
                                if (resolved_class->data.class_data.members) {
                                    nested = scope_lookup_local(
                                        resolved_class->data.class_data.members, nested_name);
                                }
                                
                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                    fprintf(stderr, "DEBUG get_expr_type: looked up '%s' in '%s', found=%p kind=%d\n",
                                        nested_name, resolved_class->name, (void*)nested, nested ? nested->kind : -1);
                                }
                                
                                /* If not found in members, try loading directly using $ convention.
                                 * This handles the case where a class was loaded from classpath
                                 * and its nested types weren't pre-registered. */
                                if (!nested && resolved_class->qualified_name) {
                                    char nested_fqn[512];
                                    snprintf(nested_fqn, sizeof(nested_fqn), "%s$%s",
                                             resolved_class->qualified_name, nested_name);
                                    
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG get_expr_type: trying to load nested class '%s'\n", nested_fqn);
                                    }
                                    
                                    nested = load_external_class(sem, nested_fqn);
                                    
                                    /* If found, add to members scope for future lookups */
                                    if (nested && resolved_class->data.class_data.members) {
                                        scope_define(resolved_class->data.class_data.members, nested);
                                    }
                                }
                                
                                if (nested && (nested->kind == SYM_CLASS || 
                                    nested->kind == SYM_INTERFACE || nested->kind == SYM_ENUM)) {
                                    resolved_class = nested;
                                    
                                    /* Mark the corresponding AST node with the class symbol */
                                    if (current_visited) {
                                        ast_node_t *node = (ast_node_t *)current_visited->data;
                                        node->sem_symbol = nested;
                                        node->sem_type = nested->type;
                                        current_visited = current_visited->next;
                                    }
                                    
                                    remaining_parts = remaining_parts->next;
                                } else {
                                    /* Not a nested class, remaining parts are fields */
                                    break;
                                }
                            }
                        }
                        
                        /* If we didn't resolve through imports, try as a fully qualified name */
                        if (!resolved_class) {
                            /* Build full FQN and try progressively shorter prefixes */
                            for (slist_t *p = parts; p; p = p->next) {
                                const char *part = (const char *)p->data;
                                if (fqn[0] != '\0') {
                                    strcat(fqn, ".");
                                }
                                strcat(fqn, part);
                            }
                            
                            /* Try as-is first */
                        symbol_t *class_sym = load_external_class(sem, fqn);
                        if (class_sym && class_sym->type) {
                                resolved_class = class_sym;
                                remaining_parts = NULL;  /* All parts are the class name */
                            }
                        }
                        
                        /* If we found a class but have remaining parts, look up the field */
                        if (resolved_class) {
                            if (!remaining_parts) {
                                /* Entire chain is a class reference */
                            for (slist_t *n = visited_nodes; n; n = n->next) {
                                ast_node_t *visited = (ast_node_t *)n->data;
                                    visited->sem_type = resolved_class->type;
                            }
                            slist_free(visited_nodes);
                                slist_free(parts);
                                expr->sem_symbol = resolved_class;
                                return resolved_class->type;
                            } else {
                                /* We have a class followed by field(s).
                                 * Look up the remaining parts as fields. */
                                symbol_t *current_class = resolved_class;
                                type_t *current_type = resolved_class->type;
                                
                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                    fprintf(stderr, "DEBUG get_expr_type: have class '%s', looking up remaining fields\n",
                                        current_class ? current_class->name : "(null)");
                                }
                                
                                while (remaining_parts) {
                                    const char *field_name_part = (const char *)remaining_parts->data;
                                    symbol_t *field_sym = NULL;
                                    
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG get_expr_type: looking up field '%s' in class '%s' (members=%p)\n",
                                            field_name_part, 
                                            current_class ? current_class->name : "(null)",
                                            current_class ? (void*)current_class->data.class_data.members : NULL);
                                    }
                                    
                                    /* Look up field in class hierarchy */
                                    symbol_t *search_class = current_class;
                                    while (search_class && !field_sym) {
                                        if (search_class->data.class_data.members) {
                                            field_sym = scope_lookup_local(
                                                search_class->data.class_data.members, field_name_part);
                                        }
                                        search_class = search_class->data.class_data.superclass;
                                    }
                                    
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG get_expr_type: field lookup returned %p (kind=%d, type=%p)\n",
                                            (void*)field_sym, field_sym ? field_sym->kind : -1,
                                            field_sym ? (void*)field_sym->type : NULL);
                                    }
                                    
                                    if (!field_sym) {
                                        /* Field not found. If the class was preregistered but not fully
                                         * analyzed (has AST but members are empty), try to extract
                                         * fields from the AST on-demand. */
                                        if (current_class && current_class->ast && 
                                            current_class->data.class_data.members &&
                                            current_class->data.class_data.members->symbols->count == 0) {
                                            /* Lazy field population for preregistered classes */
                                            ast_node_t *class_ast = current_class->ast;
                                            scope_t *class_members = current_class->data.class_data.members;
                                            
                                            for (slist_t *mc = class_ast->data.node.children; mc; mc = mc->next) {
                                                ast_node_t *member = (ast_node_t *)mc->data;
                                                if (!member || member->type != AST_FIELD_DECL) continue;
                                                
                                                ast_node_t *type_node = NULL;
                                                for (slist_t *fc = member->data.node.children; fc; fc = fc->next) {
                                                    ast_node_t *child = (ast_node_t *)fc->data;
                                                    if (!child) continue;
                                                    
                                                    if (child->type == AST_CLASS_TYPE || 
                                                        child->type == AST_PRIMITIVE_TYPE ||
                                                        child->type == AST_ARRAY_TYPE) {
                                                        type_node = child;
                                                    } else if (child->type == AST_VAR_DECLARATOR && type_node) {
                                                        const char *fname = child->data.node.name;
                                                        if (fname && !scope_lookup_local(class_members, fname)) {
                                                            symbol_t *fsym = symbol_new(SYM_FIELD, fname);
                                                            fsym->modifiers = member->data.node.flags;
                                                            fsym->ast = child;
                                                            fsym->type = semantic_resolve_type(sem, type_node);
                                                            scope_define(class_members, fsym);
                                                        }
                                                    }
                                                }
                                            }
                                            
                                            /* Try lookup again after populating */
                                            field_sym = scope_lookup_local(class_members, field_name_part);
                                        }
                                        
                                        if (!field_sym) {
                                            /* Still not found, bail out */
                                            break;
                                        }
                                    }
                                    
                                    if (field_sym->kind == SYM_CLASS || 
                                        field_sym->kind == SYM_INTERFACE || 
                                        field_sym->kind == SYM_ENUM) {
                                        /* It's a nested type, continue traversal */
                                        current_class = field_sym;
                                        current_type = field_sym->type;
                                    } else {
                                        /* It's a field/constant */
                                        current_type = field_sym->type;
                                        current_class = NULL;  /* Can't traverse further */
                                        remaining_parts = remaining_parts->next;
                                        
                                        /* If this is the last part, we found it */
                                        if (!remaining_parts && current_type) {
                                            /* Ensure the returned type has its symbol set for chained access */
                                            if (current_type->kind == TYPE_CLASS &&
                                                !current_type->data.class_type.symbol &&
                                                current_type->data.class_type.name) {
                                                symbol_t *type_sym = load_external_class(sem, current_type->data.class_type.name);
                                                if (type_sym) {
                                                    current_type->data.class_type.symbol = type_sym;
                                                }
                                            }
                                            slist_free(visited_nodes);
                                            slist_free(parts);
                                            return current_type;
                                        }
                                        break;
                                    }
                                    
                                    remaining_parts = remaining_parts->next;
                                }
                                
                                /* Check if we consumed all parts */
                                if (!remaining_parts && current_type) {
                                    /* Ensure the returned type has its symbol set for chained access */
                                    if (current_type->kind == TYPE_CLASS &&
                                        !current_type->data.class_type.symbol &&
                                        current_type->data.class_type.name) {
                                        symbol_t *type_sym = load_external_class(sem, current_type->data.class_type.name);
                                        if (type_sym) {
                                            current_type->data.class_type.symbol = type_sym;
                                        }
                                    }
                                    slist_free(visited_nodes);
                                    slist_free(parts);
                                    return current_type;
                                }
                            }
                        }
                        slist_free(parts);
                    }
                    slist_free(visited_nodes);
                }
                
                /* Special case: Type.class returns Class<Type> (class literal) */
                if (strcmp(field_name, "class") == 0) {
                    /* The receiver should be a type (class or primitive).
                     * We resolve it as a type, not as an expression. */
                    type_t *receiver_type = semantic_resolve_type(sem, object_expr);
                    
                    type_t *class_type = type_new_class("java.lang.Class");
                    if (receiver_type && receiver_type->kind != TYPE_UNKNOWN) {
                        class_type->data.class_type.type_args = slist_new(receiver_type);
                    }
                    class_type->data.class_type.symbol = load_external_class(sem, "java.lang.Class");
                    return class_type;
                }
                
                /* Not a FQN class - treat as regular field access */
                normal_field_access:
                ;  /* Empty statement after label required in C */
                type_t *object_type = get_expression_type(sem, object_expr);
                
                if (getenv("GENESIS_DEBUG_LOAD")) {
                    fprintf(stderr, "DEBUG get_expr_type: field access '%s', object_type kind=%d, name='%s', symbol=%p\n",
                        field_name,
                        object_type ? object_type->kind : -1,
                        (object_type && object_type->kind == TYPE_CLASS) ? 
                            object_type->data.class_type.name : "(n/a)",
                        (object_type && object_type->kind == TYPE_CLASS) ? 
                            (void*)object_type->data.class_type.symbol : NULL);
                }
                
                /* Special case: array.length returns int */
                if (object_type && object_type->kind == TYPE_ARRAY &&
                    strcmp(field_name, "length") == 0) {
                    return type_new_primitive(TYPE_INT);
                }
                
                /* Look up the field in the object's type, including superclasses */
                if (object_type && object_type->kind == TYPE_CLASS && 
                    object_type->data.class_type.symbol) {
                    symbol_t *class_sym = object_type->data.class_type.symbol;
                    
                    if (getenv("GENESIS_DEBUG_LOAD")) {
                        fprintf(stderr, "DEBUG get_expr_type: looking up field '%s' in class '%s' (sym=%p, members=%p, count=%zu)\n",
                            field_name,
                            class_sym->name ? class_sym->name : "(null)",
                            (void*)class_sym,
                            (void*)class_sym->data.class_data.members,
                            class_sym->data.class_data.members ? 
                                class_sym->data.class_data.members->symbols->count : (size_t)0);
                    }
                    
                    /* Search class hierarchy for field */
                    symbol_t *search_class = class_sym;
                    while (search_class) {
                        if (search_class->data.class_data.members) {
                        symbol_t *field = scope_lookup_local(
                                search_class->data.class_data.members, field_name);
                            if (getenv("GENESIS_DEBUG_LOAD")) {
                                fprintf(stderr, "DEBUG get_expr_type: field lookup '%s' in '%s' returned %p\n",
                                    field_name, search_class->name ? search_class->name : "(null)", (void*)field);
                            }
                            
                            /* If field not found but class has AST, try lazy field population.
                             * This handles classes that were preregistered but not fully analyzed. */
                            if (!field && search_class->ast) {
                                ast_node_t *class_ast = search_class->ast;
                                scope_t *class_members = search_class->data.class_data.members;
                                bool populated = false;
                                
                                /* Save and set current_class so nested types can be resolved */
                                symbol_t *saved_class = sem->current_class;
                                sem->current_class = search_class;
                                
                                for (slist_t *mc = class_ast->data.node.children; mc; mc = mc->next) {
                                    ast_node_t *member = (ast_node_t *)mc->data;
                                    if (!member || member->type != AST_FIELD_DECL) continue;
                                    
                                    ast_node_t *type_node = NULL;
                                    for (slist_t *fc = member->data.node.children; fc; fc = fc->next) {
                                        ast_node_t *child = (ast_node_t *)fc->data;
                                        if (!child) continue;
                                        
                                        if (child->type == AST_CLASS_TYPE || 
                                            child->type == AST_PRIMITIVE_TYPE ||
                                            child->type == AST_ARRAY_TYPE) {
                                            type_node = child;
                                        } else if (child->type == AST_VAR_DECLARATOR && type_node) {
                                            const char *fname = child->data.node.name;
                                            if (fname && !scope_lookup_local(class_members, fname)) {
                                                symbol_t *fsym = symbol_new(SYM_FIELD, fname);
                                                fsym->modifiers = member->data.node.flags;
                                                fsym->ast = child;
                                                fsym->type = semantic_resolve_type(sem, type_node);
                                                scope_define(class_members, fsym);
                                                populated = true;
                                            }
                                        }
                                    }
                                }
                                
                                /* Restore current_class */
                                sem->current_class = saved_class;
                                
                                /* Retry lookup after population */
                                if (populated) {
                                    field = scope_lookup_local(class_members, field_name);
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG get_expr_type: after lazy population, field '%s' lookup returned %p\n",
                                            field_name, (void*)field);
                                    }
                                }
                            }
                            
                        if (field) {
                                if (getenv("GENESIS_DEBUG_LOAD")) {
                                    fprintf(stderr, "DEBUG get_expr_type: found field '%s' kind=%d type=%p (type_kind=%d)\n",
                                        field_name, field->kind, (void*)field->type,
                                        field->type ? field->type->kind : -1);
                                }
                            /* Check access control */
                            if (!check_access(field->modifiers, class_sym, sem->current_class)) {
                                semantic_error(sem, expr->line, expr->column,
                                    "field '%s' has %s access in '%s'",
                                    field_name,
                                    (field->modifiers & MOD_PRIVATE) ? "private" :
                                    (field->modifiers & MOD_PROTECTED) ? "protected" : "package-private",
                                    class_sym->name);
                            }
                            if (field->type) {
                                /* Substitute type parameters with actual type arguments */
                                type_t *field_type = substitute_from_receiver(field->type, object_type);
                                    
                                    /* Ensure the returned type has its symbol set for chained access */
                                    if (field_type && field_type->kind == TYPE_CLASS &&
                                        !field_type->data.class_type.symbol &&
                                        field_type->data.class_type.name) {
                                        symbol_t *field_type_sym = load_external_class(sem, field_type->data.class_type.name);
                                        if (field_type_sym) {
                                            field_type->data.class_type.symbol = field_type_sym;
                                        }
                                    }
                                    
                                    if (getenv("GENESIS_DEBUG_LOAD")) {
                                        fprintf(stderr, "DEBUG get_expr_type: returning field_type=%p (kind=%d, name=%s, symbol=%p)\n",
                                            (void*)field_type, field_type->kind,
                                            field_type->kind == TYPE_CLASS ? 
                                                field_type->data.class_type.name : "(n/a)",
                                            field_type->kind == TYPE_CLASS ?
                                                (void*)field_type->data.class_type.symbol : NULL);
                                    }
                                return field_type;
                            }
                        }
                        }
                        /* Move to superclass */
                        search_class = search_class->data.class_data.superclass;
                    }
                }
                
                /* If object is 'this', look up in current class */
                if (object_expr->type == AST_THIS_EXPR && sem->current_class) {
                    if (sem->current_class->data.class_data.members) {
                        symbol_t *field = scope_lookup_local(
                            sem->current_class->data.class_data.members, field_name);
                        if (field && field->type) {
                            /* Always accessible from same class */
                            return field->type;
                        }
                    }
                }
                
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_NEW_OBJECT:
            {
                slist_t *children = expr->data.node.children;
                if (!children) {
                    return type_new_primitive(TYPE_UNKNOWN);
                }
                
                /* First child is the type (class or interface being extended/implemented) */
                ast_node_t *type_node = (ast_node_t *)children->data;
                type_t *base_type = NULL;
                
                /* Check for qualified new: outer.new Inner()
                 * In this case, 'Inner' should be resolved in context of outer's type */
                ast_node_t *explicit_outer = (ast_node_t *)expr->data.node.extra;
                if (explicit_outer && type_node && type_node->type == AST_CLASS_TYPE) {
                    /* Get the outer instance's type */
                    type_t *outer_type = get_expression_type(sem, explicit_outer);
                    if (outer_type && outer_type->kind == TYPE_CLASS) {
                        /* Look up the inner class in the outer class's members */
                        symbol_t *outer_sym = outer_type->data.class_type.symbol;
                        if (!outer_sym) {
                            outer_sym = load_external_class(sem, outer_type->data.class_type.name);
                        }
                        if (outer_sym && outer_sym->data.class_data.members) {
                            const char *inner_name = type_node->data.node.name;
                            symbol_t *inner_sym = scope_lookup_local(
                                outer_sym->data.class_data.members, inner_name);
                            if (inner_sym && (inner_sym->kind == SYM_CLASS || 
                                              inner_sym->kind == SYM_INTERFACE)) {
                                if (inner_sym->type) {
                                    base_type = inner_sym->type;
                                }
                            }
                        }
                    }
                }
                
                /* Normal type resolution if not a qualified new or inner class not found */
                if (!base_type) {
                    base_type = semantic_resolve_type(sem, type_node);
                }
                
                /* Store resolved type on the type node for codegen */
                if (base_type && type_node) {
                    type_node->sem_type = base_type;
                }
                
                /* Check if there's an anonymous class body:
                 * Children: type, [arg, arg, ...], body (AST_BLOCK with class members) */
                ast_node_t *anon_body = NULL;
                for (slist_t *c = children->next; c; c = c->next) {
                    ast_node_t *child = (ast_node_t *)c->data;
                    /* The class body is an AST_BLOCK that follows all arguments */
                    if (child->type == AST_BLOCK && !c->next) {
                        anon_body = child;
                        break;
                    }
                }
                
                /* Check if trying to instantiate an abstract class, interface, or enum 
                 * (without anonymous body for abstract/interface) */
                if (base_type && base_type->kind == TYPE_CLASS &&
                    base_type->data.class_type.symbol) {
                    symbol_t *class_sym = base_type->data.class_type.symbol;
                    
                    /* Enums cannot be instantiated at all */
                    if (class_sym->kind == SYM_ENUM) {
                        semantic_error(sem, expr->line, expr->column,
                            "enum types may not be instantiated");
                    }
                    /* Abstract classes and interfaces cannot be instantiated without anonymous body */
                    else if (!anon_body) {
                        if ((class_sym->modifiers & MOD_ABSTRACT) ||
                            class_sym->kind == SYM_INTERFACE) {
                            const char *kind = class_sym->kind == SYM_INTERFACE ? 
                                               "interface" : "abstract class";
                            semantic_error(sem, expr->line, expr->column,
                                "%s %s cannot be instantiated", kind, class_sym->name);
                        }
                    }
                }
                
                if (anon_body && sem->current_class) {
                    /* Check if anonymous class has already been set up (from pass1) */
                    if (anon_body->sem_symbol && anon_body->sem_symbol->kind == SYM_CLASS &&
                        anon_body->sem_symbol->data.class_data.is_anonymous_class) {
                        /* Already set up, but may need to bind lambda arguments.
                         * Collect constructor arguments and bind lambdas to parameter types. */
                        slist_t *ctor_args = NULL;
                        for (slist_t *c = children->next; c; c = c->next) {
                            ast_node_t *child = (ast_node_t *)c->data;
                            if (child == anon_body) break;
                            if (!ctor_args) {
                                ctor_args = slist_new(child);
                            } else {
                                slist_append(ctor_args, child);
                            }
                        }
                        
                        /* Find and bind lambdas to superclass constructor parameters */
                        symbol_t *anon_sym = anon_body->sem_symbol;
                        symbol_t *base_sym = anon_sym->data.class_data.superclass;
                        if (!base_sym && base_type && base_type->kind == TYPE_CLASS) {
                            base_sym = base_type->data.class_type.symbol;
                        }
                        
                        if (ctor_args && base_sym && base_sym->data.class_data.members) {
                            slist_t *ctors = scope_find_all_methods(
                                base_sym->data.class_data.members, "<init>");
                            if (!ctors && base_sym->name) {
                                ctors = scope_find_all_methods(
                                    base_sym->data.class_data.members, base_sym->name);
                            }
                            if (ctors) {
                                symbol_t *ctor = find_best_method_by_types(sem, ctors, ctor_args, NULL);
                                if (ctor && ctor->data.method_data.parameters) {
                                    slist_t *piter = ctor->data.method_data.parameters;
                                    slist_t *aiter = ctor_args;
                                    while (piter && aiter) {
                                        symbol_t *param = (symbol_t *)piter->data;
                                        ast_node_t *arg = (ast_node_t *)aiter->data;
                                        if (param && param->type && arg) {
                                            if (arg->type == AST_LAMBDA_EXPR) {
                                                bind_lambda_to_target_type(sem, arg, param->type);
                                            } else if (arg->type == AST_METHOD_REF) {
                                                bind_method_ref_to_target_type(sem, arg, param->type);
                                            }
                                        }
                                        piter = piter->next;
                                        aiter = aiter->next;
                                    }
                                    if (!expr->sem_symbol) {
                                        expr->sem_symbol = ctor;
                                    }
                                }
                                slist_free(ctors);
                            }
                        }
                        if (ctor_args) slist_free(ctor_args);
                        
                        /* Now evaluate constructor argument types */
                        for (slist_t *c = children->next; c; c = c->next) {
                            ast_node_t *child = (ast_node_t *)c->data;
                            if (child == anon_body) break;
                            get_expression_type(sem, child);
                        }
                        
                        /* Return the existing type */
                        if (!expr->sem_symbol) {
                            expr->sem_symbol = anon_body->sem_symbol;
                        }
                        expr->sem_type = anon_body->sem_symbol->type;
                        return anon_body->sem_symbol->type;
                    }
                    
                    /* This is an anonymous class - create a synthetic class symbol */
                    int anon_id = ++sem->current_class->data.class_data.local_class_counter;
                    char anon_name[256];
                    snprintf(anon_name, sizeof(anon_name), "%s$%d", 
                             sem->current_class->qualified_name, anon_id);
                    
                    symbol_t *anon_sym = symbol_new(SYM_CLASS, anon_name);
                    anon_sym->qualified_name = strdup(anon_name);
                    
                    /* Anonymous classes in static contexts are implicitly static */
                    /* Static context means: static method, static field initializer, or inside a static lambda */
                    bool in_static_context = false;
                    if (sem->current_method) {
                        /* Inside a method - check if static method */
                        in_static_context = (sem->current_method->modifiers & MOD_STATIC) != 0;
                    } else {
                        /* Not inside a method - either field initializer or static initializer block.
                         * Check in_static_field_init flag set during field processing. */
                        in_static_context = sem->in_static_field_init;
                    }
                    /* Lambdas generate static methods, so anonymous classes inside them are static */
                    if (sem->current_lambda != NULL) {
                        in_static_context = true;
                    }
                    anon_sym->modifiers = in_static_context ? MOD_STATIC : 0;
                    anon_sym->data.class_data.enclosing_class = sem->current_class;
                    anon_sym->data.class_data.enclosing_method = sem->current_method;
                    anon_sym->data.class_data.is_anonymous_class = true;
                    anon_sym->data.class_data.anonymous_body = anon_body;
                    
                    /* Mark the anonymous class body block with the class symbol
                     * so pass2 can identify it and set up the proper class context */
                    anon_body->sem_symbol = anon_sym;
                    
                    /* Collect constructor arguments (everything between type and body) */
                    slist_t *ctor_args = NULL;
                    for (slist_t *c = children->next; c; c = c->next) {
                        ast_node_t *child = (ast_node_t *)c->data;
                        if (child == anon_body) break;  /* Stop at the body */
                        /* Add constructor argument */
                        if (!ctor_args) {
                            ctor_args = slist_new(child);
                        } else {
                            slist_append(ctor_args, child);
                        }
                    }
                    anon_sym->data.class_data.super_ctor_args = ctor_args;
                    
                    /* Determine if base type is class or interface */
                    if (base_type && base_type->kind == TYPE_CLASS && 
                        base_type->data.class_type.symbol) {
                        symbol_t *base_sym = base_type->data.class_type.symbol;
                        if (base_sym->kind == SYM_INTERFACE) {
                            /* Implementing an interface - extend Object */
                            anon_sym->data.class_data.superclass = NULL;  /* Will default to Object */
                            anon_sym->data.class_data.interfaces = slist_new(base_sym);
                        } else {
                            /* Extending a class */
                            anon_sym->data.class_data.superclass = base_sym;
                            
                            /* Bind lambda/method-ref arguments to base class constructor parameters */
                            if (ctor_args && base_sym->data.class_data.members) {
                                /* Find constructors in base class */
                                slist_t *ctors = scope_find_all_methods(
                                    base_sym->data.class_data.members, "<init>");
                                if (!ctors && base_sym->name) {
                                    ctors = scope_find_all_methods(
                                        base_sym->data.class_data.members, base_sym->name);
                                }
                                if (ctors) {
                                    symbol_t *ctor = find_best_method_by_types(sem, ctors, ctor_args, NULL);
                                    if (ctor && ctor->data.method_data.parameters) {
                                        /* Bind lambdas/method-refs to parameter types */
                                        slist_t *piter = ctor->data.method_data.parameters;
                                        slist_t *aiter = ctor_args;
                                        while (piter && aiter) {
                                            symbol_t *param = (symbol_t *)piter->data;
                                            ast_node_t *arg = (ast_node_t *)aiter->data;
                                            if (param && param->type && arg) {
                                                if (arg->type == AST_LAMBDA_EXPR) {
                                                    bind_lambda_to_target_type(sem, arg, param->type);
                                                } else if (arg->type == AST_METHOD_REF) {
                                                    bind_method_ref_to_target_type(sem, arg, param->type);
                                                }
                                            }
                                            piter = piter->next;
                                            aiter = aiter->next;
                                        }
                                        /* Also store the constructor for codegen */
                                        expr->sem_symbol = ctor;
                                    }
                                    slist_free(ctors);
                                }
                            }
                            
                            /* If extending a non-static inner class and we're in a lambda,
                             * mark the lambda as capturing 'this' */
                            if (base_sym->data.class_data.enclosing_class &&
                                !(base_sym->modifiers & MOD_STATIC) &&
                                sem->current_lambda) {
                                sem->current_lambda->lambda_captures_this = true;
                            }
                        }
                    }
                    
                    /* Create type for anonymous class */
                    type_t *anon_type = type_new_class(anon_name);
                    anon_type->data.class_type.symbol = anon_sym;
                    anon_sym->type = anon_type;
                    
                    /* Cache the type by qualified name */
                    hashtable_insert(sem->types, anon_name, anon_type);
                    
                    /* Create class member scope - parent is current scope for variable capture */
                    scope_t *class_scope = scope_new(SCOPE_CLASS, sem->current_scope);
                    class_scope->owner = anon_sym;
                    anon_sym->data.class_data.members = class_scope;
                    
                    /* Process anonymous class body members (fields, methods) */
                    /* Save current class context and switch to anonymous class */
                    symbol_t *saved_class = sem->current_class;
                    scope_t *saved_scope = sem->current_scope;
                    sem->current_class = anon_sym;
                    sem->current_scope = class_scope;
                    
                    /* Process each member in the anonymous class body */
                    slist_t *body_children = anon_body->data.node.children;
                    for (slist_t *m = body_children; m; m = m->next) {
                        ast_node_t *member = (ast_node_t *)m->data;
                        
                        if (member->type == AST_FIELD_DECL) {
                            /* Process field declaration */
                            slist_t *fchildren = member->data.node.children;
                            type_t *field_type = NULL;
                            if (fchildren && ((ast_node_t *)fchildren->data)->type != AST_VAR_DECLARATOR) {
                                field_type = semantic_resolve_type(sem, fchildren->data);
                                fchildren = fchildren->next;
                            }
                            while (fchildren) {
                                ast_node_t *decl = (ast_node_t *)fchildren->data;
                                if (decl->type == AST_VAR_DECLARATOR) {
                                    const char *fname = decl->data.node.name;
                                    symbol_t *field_sym = symbol_new(SYM_FIELD, fname);
                                    field_sym->modifiers = member->data.node.flags;
                                    field_sym->type = field_type;
                                    field_sym->ast = decl;
                                    scope_define(class_scope, field_sym);
                                }
                                fchildren = fchildren->next;
                            }
                        } else if (member->type == AST_METHOD_DECL || 
                                   member->type == AST_CONSTRUCTOR_DECL) {
                            /* Process method/constructor declaration */
                            const char *mname = member->data.node.name;
                            symbol_kind_t mkind = member->type == AST_METHOD_DECL ? 
                                                  SYM_METHOD : SYM_CONSTRUCTOR;
                            symbol_t *method_sym = symbol_new(mkind, mname);
                            method_sym->modifiers = member->data.node.flags;
                            method_sym->ast = member;
                            
                            /* Store symbol on AST node for pass2 lookup */
                            member->sem_symbol = method_sym;
                            
                            /* Get return type from node.extra (not children) */
                            if (mkind == SYM_METHOD && member->data.node.extra) {
                                method_sym->type = semantic_resolve_type(sem, member->data.node.extra);
                            }
                            
                            /* Create method body scope */
                            scope_t *method_scope = scope_new(SCOPE_METHOD, class_scope);
                            method_scope->owner = method_sym;
                            method_sym->data.method_data.body_scope = method_scope;
                            
                            scope_define(class_scope, method_sym);
                            
                            /* Process method parameters and body within anonymous class context */
                            symbol_t *saved_method = sem->current_method;
                            scope_t *saved_method_scope = sem->current_scope;
                            sem->current_method = method_sym;
                            sem->current_scope = method_scope;
                            
                            /* Process parameters */
                            slist_t *mchildren = member->data.node.children;
                            while (mchildren) {
                                ast_node_t *mchild = (ast_node_t *)mchildren->data;
                                if (mchild->type == AST_PARAMETER) {
                                    /* Get parameter type and name */
                                    slist_t *pchildren = mchild->data.node.children;
                                    type_t *param_type = NULL;
                                    if (pchildren) {
                                        param_type = semantic_resolve_type(sem, pchildren->data);
                                    }
                                    const char *pname = mchild->data.node.name;
                                    
                                    symbol_t *param = symbol_new(SYM_PARAMETER, pname);
                                    param->type = param_type;
                                    param->ast = mchild;
                                    scope_define(method_scope, param);
                                } else if (mchild->type == AST_BLOCK) {
                                    /* Method body will be processed by pass2's AST walker
                                     * after the anonymous class context is set up.
                                     * Captured variable detection happens during pass2. */
                                }
                                mchildren = mchildren->next;
                            }
                            
                            sem->current_method = saved_method;
                            sem->current_scope = saved_method_scope;
                        }
                    }
                    
                    /* Restore context */
                    sem->current_class = saved_class;
                    sem->current_scope = saved_scope;
                    
                    /* Store symbol reference on expression for code generation */
                    expr->sem_symbol = anon_sym;
                    
                    /* Return the base type (for assignment compatibility) */
                    expr->sem_type = base_type;
                    return base_type;
                }
                
                /* Collect constructor arguments first (don't evaluate lambdas yet) */
                slist_t *args = NULL;
                slist_t *args_tail = NULL;
                for (slist_t *c = children->next; c; c = c->next) {
                    ast_node_t *arg = (ast_node_t *)c->data;
                    /* Skip anonymous class body */
                    if (arg->type != AST_BLOCK) {
                        /* Build argument list for constructor resolution */
                        if (!args) {
                            args = slist_new(arg);
                            args_tail = args;
                        } else {
                            args_tail = slist_append(args_tail, arg);
                        }
                    }
                }
                
                /* Resolve the best matching constructor */
                symbol_t *ctor = NULL;
                if (base_type && base_type->kind == TYPE_CLASS) {
                    symbol_t *target_class = base_type->data.class_type.symbol;
                    if (!target_class && base_type->data.class_type.name) {
                        target_class = load_external_class(sem, base_type->data.class_type.name);
                    }
                    
                    if (target_class && target_class->data.class_data.members) {
                        /* Check for explicit outer instance (qualified new: outer.new Inner()) */
                        ast_node_t *explicit_outer = (ast_node_t *)expr->data.node.extra;
                        
                        /* Type-check explicit outer instance to detect captures */
                        if (explicit_outer) {
                            get_expression_type(sem, explicit_outer);
                        }
                        
                        /* If target is a non-static inner class and we're in a lambda,
                         * mark the lambda as capturing 'this' only if there's NO explicit outer.
                         * With explicit outer (outer.new Inner()), we capture 'outer' instead of 'this'. */
                        if (target_class->data.class_data.enclosing_class &&
                            !(target_class->modifiers & MOD_STATIC) &&
                            sem->current_lambda &&
                            !explicit_outer) {
                            sem->current_lambda->lambda_captures_this = true;
                        }
                        
                        /* Find all constructors - they're stored with <init> as the name */
                        slist_t *ctors = scope_find_all_methods(target_class->data.class_data.members, "<init>");
                        
                        /* Also try the class name (for source-defined constructors) */
                        if (!ctors && target_class->name) {
                            ctors = scope_find_all_methods(target_class->data.class_data.members, 
                                                          target_class->name);
                        }
                        
                        /* Find best matching constructor (this evaluates non-lambda arg types) */
                        if (ctors) {
                            ctor = find_best_method_by_types(sem, ctors, args, NULL);
                            if (ctor) {
                                expr->sem_symbol = ctor;
                            }
                            slist_free(ctors);
                        }
                    }
                }
                
                /* Bind lambda/method-ref arguments to constructor parameter types */
                if (ctor && ctor->data.method_data.parameters && args) {
                    slist_t *piter = ctor->data.method_data.parameters;
                    slist_t *aiter = args;
                    while (piter && aiter) {
                        symbol_t *param = (symbol_t *)piter->data;
                        ast_node_t *arg = (ast_node_t *)aiter->data;
                        
                        if (param && param->type && arg) {
                            if (arg->type == AST_LAMBDA_EXPR) {
                                bind_lambda_to_target_type(sem, arg, param->type);
                            } else if (arg->type == AST_METHOD_REF) {
                                bind_method_ref_to_target_type(sem, arg, param->type);
                            }
                        }
                        
                        piter = piter->next;
                        aiter = aiter->next;
                    }
                }
                
                /* Now evaluate argument types (including bound lambdas) */
                for (slist_t *a = args; a; a = a->next) {
                    ast_node_t *arg = (ast_node_t *)a->data;
                    get_expression_type(sem, arg);
                }
                
                if (args) slist_free(args);
                
                expr->sem_type = base_type;
                return base_type;
            }
        
        case AST_NEW_ARRAY:
            {
                slist_t *children = expr->data.node.children;
                if (children) {
                    type_t *elem = semantic_resolve_type(sem, children->data);
                    
                    /* Get total dimensions from flags (set by parser).
                     * This includes empty brackets like in new byte[3][]. */
                    int dims = (int)expr->data.node.flags;
                    
                    /* Find initializer if present */
                    ast_node_t *initializer = NULL;
                    for (slist_t *c = children->next; c; c = c->next) {
                        ast_node_t *child = c->data;
                        if (child->type == AST_ARRAY_INIT) {
                            initializer = child;
                            break;
                        }
                    }
                    
                    /* Check for missing array dimensions */
                    if (dims == 0 && !initializer) {
                        semantic_error(sem, expr->line, expr->column,
                            "array dimension missing");
                    }
                    
                    if (dims == 0) {
                        dims = 1;
                    }
                    
                    /* Flatten array dimensions if element is already an array type.
                     * This handles cases like new Block<?>[][]{ ... } where the parser
                     * unwraps one level of AST_ARRAY_TYPE. */
                    while (elem && elem->kind == TYPE_ARRAY) {
                        dims += elem->data.array_type.dimensions;
                        elem = elem->data.array_type.element_type;
                    }
                    
                    type_t *array_type = type_new_array(elem, dims);
                    
                    /* If there's an initializer, bind lambda/method ref elements */
                    if (initializer && array_type) {
                        initializer->sem_type = array_type;
                        bind_array_init_elements(sem, initializer, array_type);
                    }
                    
                    return array_type;
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_CONDITIONAL_EXPR:
            {
                /* Type is common type of true/false branches */
                slist_t *children = expr->data.node.children;
                if (children && children->next && children->next->next) {
                    type_t *then_type = get_expression_type(sem, children->next->data);
                    type_t *else_type = get_expression_type(sem, children->next->next->data);
                    
                    /* If one branch is null, use the type of the other branch */
                    if (then_type && then_type->kind == TYPE_NULL && else_type) {
                        return else_type;
                    }
                    if (else_type && else_type->kind == TYPE_NULL && then_type) {
                        return then_type;
                    }
                    
                    /* Otherwise use the then branch type (could be improved to find LUB) */
                    return then_type;
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_SWITCH_EXPR:
            {
                /* Switch expression type is inferred from first non-pattern case branch */
                slist_t *children = expr->data.node.children;
                if (children) {
                    /* Skip selector, look at first switch rule */
                    for (slist_t *c = children->next; c; c = c->next) {
                        ast_node_t *rule = (ast_node_t *)c->data;
                        if (rule->type == AST_SWITCH_RULE) {
                            /* Check if this rule has type patterns - if so, skip it
                             * as the pattern variable isn't in scope yet during type inference */
                            slist_t *rule_children = rule->data.node.children;
                            bool has_pattern = false;
                            ast_node_t *body = NULL;
                            ast_node_t *pattern = NULL;
                            
                            for (slist_t *rc = rule_children; rc; rc = rc->next) {
                                ast_node_t *child = (ast_node_t *)rc->data;
                                if (child->type == AST_TYPE_PATTERN || 
                                    child->type == AST_GUARDED_PATTERN ||
                                    child->type == AST_UNNAMED_PATTERN ||
                                    child->type == AST_RECORD_PATTERN) {
                                    has_pattern = true;
                                    pattern = child;
                                    if (child->type == AST_GUARDED_PATTERN) {
                                        /* Get the type pattern from guarded pattern */
                                        slist_t *gp_children = child->data.node.children;
                                        if (gp_children) {
                                            pattern = (ast_node_t *)gp_children->data;
                                        }
                                    }
                                }
                                body = child;  /* Last child is body */
                            }
                            
                            if (body && !has_pattern) {
                                if (body->type == AST_BLOCK) {
                                    /* Recursively look for yield in block (handles nested if/for/etc.) */
                                    ast_node_t *yield_expr = find_first_yield(body);
                                    if (yield_expr) {
                                        return get_expression_type(sem, yield_expr);
                                    }
                                } else if (body->type == AST_THROW_STMT) {
                                    /* throw doesn't yield a value, continue to next case */
                                    continue;
                                } else {
                                    /* Expression case */
                                    return get_expression_type(sem, body);
                                }
                            } else if (has_pattern && pattern && pattern->type == AST_TYPE_PATTERN) {
                                /* For type patterns, we need to set up a temporary scope
                                 * to resolve the pattern variable */
                                scope_t *saved = sem->current_scope;
                                scope_t *temp = scope_new(SCOPE_BLOCK, sem->current_scope);
                                
                                /* Define pattern variable */
                                const char *var_name = pattern->data.node.name;
                                slist_t *pattern_children = pattern->data.node.children;
                                if (pattern_children && var_name) {
                                    ast_node_t *type_node = (ast_node_t *)pattern_children->data;
                                    type_t *pattern_type = semantic_resolve_type(sem, type_node);
                                    
                                    symbol_t *sym = symbol_new(SYM_LOCAL_VAR, var_name);
                                    sym->type = pattern_type;
                                    sym->modifiers = MOD_FINAL;
                                    scope_define(temp, sym);
                                }
                                
                                sem->current_scope = temp;
                                type_t *result = NULL;
                                
                                if (body) {
                                    if (body->type == AST_BLOCK) {
                                        /* Recursively look for yield in block */
                                        ast_node_t *yield_expr = find_first_yield(body);
                                        if (yield_expr) {
                                            result = get_expression_type(sem, yield_expr);
                                        }
                                    } else if (body->type != AST_THROW_STMT) {
                                        result = get_expression_type(sem, body);
                                    }
                                }
                                
                                sem->current_scope = saved;
                                if (result) return result;
                            }
                        }
                    }
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_PARENTHESIZED:
            {
                slist_t *children = expr->data.node.children;
                if (children) {
                    type_t *inner_type = get_expression_type(sem, children->data);
                    expr->sem_type = inner_type;  /* Propagate inner type to parenthesized node */
                    return inner_type;
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_ARRAY_INIT:
            {
                /* Array initializer - return sem_type if set by semantic analysis */
                if (expr->sem_type) {
                    return expr->sem_type;
                }
                /* Otherwise, try to infer from first element */
                slist_t *children = expr->data.node.children;
                if (children) {
                    type_t *elem_type = get_expression_type(sem, children->data);
                    if (elem_type && elem_type->kind != TYPE_UNKNOWN) {
                        return type_new_array(elem_type, 1);
                    }
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_ARRAY_ACCESS:
            {
                /* Array element type - handle multi-dimensional arrays */
                slist_t *children = expr->data.node.children;
                if (children) {
                    ast_node_t *array_expr = (ast_node_t *)children->data;
                    type_t *array_type = get_expression_type(sem, array_expr);
                    if (array_type && array_type->kind == TYPE_ARRAY) {
                        int dims = array_type->data.array_type.dimensions;
                        type_t *elem_type = array_type->data.array_type.element_type;
                        
                        if (dims > 1) {
                            /* Multi-dimensional: return array with one fewer dimension */
                            return type_new_array(elem_type, dims - 1);
                        } else {
                            /* Single dimension: return the element type */
                            if (elem_type) {
                                return elem_type;
                            }
                        }
                    }
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_INSTANCEOF_EXPR:
            {
                /* Process the left-hand expression to trigger variable capture detection */
                slist_t *children = expr->data.node.children;
                if (children) {
                    get_expression_type(sem, (ast_node_t *)children->data);
                }
                /* instanceof always returns boolean */
                return type_new_primitive(TYPE_BOOLEAN);
            }
        
        case AST_CAST_EXPR:
            {
                /* Cast type is the target type - use semantic_resolve_type for proper resolution */
                slist_t *children = expr->data.node.children;
                if (children) {
                    ast_node_t *type_node = (ast_node_t *)children->data;
                    type_t *cast_type = semantic_resolve_type(sem, type_node);
                    expr->sem_type = cast_type;  /* Store for codegen wrapper detection */
                    
                    /* If the operand is a lambda or method reference, bind it to the cast type.
                     * This handles cases like: (SAM)() -> { } where the cast provides the target type. */
                    if (children->next && cast_type) {
                        ast_node_t *operand = (ast_node_t *)children->next->data;
                        type_t *target_for_lambda = cast_type;
                        
                        /* For intersection types, find the functional interface component */
                        if (cast_type->kind == TYPE_INTERSECTION) {
                            slist_t *types = cast_type->data.intersection.types;
                            for (slist_t *t = types; t; t = t->next) {
                                type_t *component = (type_t *)t->data;
                                if (component->kind == TYPE_CLASS) {
                                    /* Check if this component has a SAM (is a functional interface) */
                                    symbol_t *sym = component->data.class_type.symbol;
                                    if (!sym && component->data.class_type.name) {
                                        sym = load_external_class(sem, component->data.class_type.name);
                                        if (sym) {
                                            component->data.class_type.symbol = sym;
                                        }
                                    }
                                    if (sym) {
                                        symbol_t *sam = get_functional_interface_sam(sym);
                                        if (sam) {
                                            target_for_lambda = component;
                                            break;  /* Use first functional interface found */
                                        }
                                    }
                                }
                            }
                        }
                        
                        if (target_for_lambda->kind == TYPE_CLASS) {
                            if (operand && operand->type == AST_LAMBDA_EXPR) {
                                bind_lambda_to_target_type(sem, operand, target_for_lambda);
                            } else if (operand && operand->type == AST_METHOD_REF) {
                                bind_method_ref_to_target_type(sem, operand, target_for_lambda);
                            }
                        }
                    }
                    
                    return cast_type;
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_LAMBDA_EXPR:
        case AST_METHOD_REF:
            /* Lambda/method ref type is determined by target context.
             * If sem_type has been set by context, use that.
             * Otherwise, we can't determine the type yet. */
            if (expr->sem_type) {
                return expr->sem_type;
            }
            return type_new_primitive(TYPE_UNKNOWN);
        
        case AST_CLASS_LITERAL:
            {
                /* Type.class returns Class<Type> */
                type_t *class_type = type_new_class("java.lang.Class");
                slist_t *children = expr->data.node.children;
                if (children) {
                    ast_node_t *type_node = (ast_node_t *)children->data;
                    type_t *arg_type = semantic_resolve_type(sem, type_node);
                    if (arg_type && arg_type->kind != TYPE_UNKNOWN) {
                        class_type->data.class_type.type_args = slist_new(arg_type);
                    }
                }
                /* Set sem_type for codegen to detect it's a reference type */
                expr->sem_type = class_type;
                return class_type;
            }
        
        case AST_EXPLICIT_CTOR_CALL:
            {
                /* this() or super() constructor call - bind lambda arguments */
                const char *call_type = expr->data.node.name;
                bool is_this_call = call_type && strcmp(call_type, "this") == 0;
                
                /* Determine target class */
                symbol_t *target_class = NULL;
                if (is_this_call) {
                    target_class = sem->current_class;
                } else {
                    /* super() - find superclass */
                    if (sem->current_class && sem->current_class->data.class_data.superclass) {
                        target_class = sem->current_class->data.class_data.superclass;
                    }
                    if (!target_class && sem->current_class) {
                        /* Try to load from superclass symbol or ast */
                        target_class = load_external_class(sem, "java.lang.Object");
                    }
                }
                
                if (target_class && target_class->data.class_data.members) {
                    /* Find matching constructor */
                    slist_t *ctors = scope_find_all_methods(target_class->data.class_data.members, "<init>");
                    if (!ctors && target_class->name) {
                        ctors = scope_find_all_methods(target_class->data.class_data.members, target_class->name);
                    }
                    
                    /* Count arguments */
                    int arg_count = 0;
                    for (slist_t *a = expr->data.node.children; a; a = a->next) {
                        arg_count++;
                    }
                    
                    /* Find best matching constructor - prefer one that accepts lambda */
                    symbol_t *best_ctor = NULL;
                    symbol_t *fallback_ctor = NULL;
                    
                    /* Check if any argument is a lambda */
                    bool has_lambda_arg = false;
                    for (slist_t *a = expr->data.node.children; a; a = a->next) {
                        ast_node_t *arg = (ast_node_t *)a->data;
                        /* Unwrap parentheses */
                        while (arg && arg->type == AST_PARENTHESIZED && arg->data.node.children) {
                            arg = (ast_node_t *)arg->data.node.children->data;
                        }
                        if (arg && (arg->type == AST_LAMBDA_EXPR || arg->type == AST_METHOD_REF)) {
                            has_lambda_arg = true;
                            break;
                        }
                    }
                    
                    for (slist_t *c = ctors; c; c = c->next) {
                        symbol_t *ctor = (symbol_t *)c->data;
                        if (ctor && ctor->kind == SYM_CONSTRUCTOR) {
                            int param_count = 0;
                            bool accepts_functional_interface = false;
                            for (slist_t *p = ctor->data.method_data.parameters; p; p = p->next) {
                                symbol_t *pp = (symbol_t*)p->data;
                                /* Check if this param could accept a lambda */
                                if (pp && pp->type && pp->type->kind == TYPE_CLASS) {
                                    /* Interface or class type - could be functional interface */
                                    accepts_functional_interface = true;
                                }
                                param_count++;
                            }
                            if (param_count == arg_count) {
                                if (has_lambda_arg && accepts_functional_interface) {
                                    /* Prefer constructor that accepts functional interface */
                                    best_ctor = ctor;
                                } else if (!fallback_ctor) {
                                    fallback_ctor = ctor;
                                }
                            }
                        }
                    }
                    
                    if (!best_ctor) {
                        best_ctor = fallback_ctor;
                    }
                    
                    /* Bind lambda arguments to constructor parameter types */
                    if (best_ctor) {
                        slist_t *param_node = best_ctor->data.method_data.parameters;
                        slist_t *arg_node = expr->data.node.children;
                        
                        while (param_node && arg_node) {
                            symbol_t *param = (symbol_t *)param_node->data;
                            ast_node_t *arg = (ast_node_t *)arg_node->data;
                            
                            if (arg && param && param->type) {
                                if (arg->type == AST_LAMBDA_EXPR) {
                                    bind_lambda_to_target_type(sem, arg, param->type);
                                } else if (arg->type == AST_METHOD_REF) {
                                    bind_method_ref_to_target_type(sem, arg, param->type);
                                }
                            }
                            
                            /* Type-check the argument */
                            get_expression_type(sem, arg);
                            
                            param_node = param_node->next;
                            arg_node = arg_node->next;
                        }
                    }
                }
                
                /* Constructor calls are statements, return void */
                return type_void();
            }
        
        default:
            return type_new_primitive(TYPE_UNKNOWN);
    }
}

/* ========================================================================
 * Annotation Helpers
 * ======================================================================== */

/**
 * Check if an AST node has a specific annotation.
 * Checks both short and fully qualified names.
 */
static bool has_annotation(ast_node_t *node, const char *short_name, const char *fqn)
{
    if (!node || !node->annotations) {
        return false;
    }
    
    for (slist_t *n = node->annotations; n; n = n->next) {
        ast_node_t *annot = (ast_node_t *)n->data;
        if (annot && annot->type == AST_ANNOTATION && annot->data.node.name) {
            const char *annot_name = annot->data.node.name;
            if (strcmp(annot_name, short_name) == 0 ||
                strcmp(annot_name, fqn) == 0) {
                return true;
            }
        }
    }
    return false;
}

/* Helper struct for @FunctionalInterface validation */
typedef struct {
    int abstract_count;
    symbol_t *sam;
} fi_validation_t;

/**
 * Get the substituted type name for a type. Type variables are replaced with
 * substitutions from type_args_map if available, otherwise erased to bounds.
 * This is used for @FunctionalInterface validation to detect override-equivalent methods.
 */
static const char *fi_get_substituted_name(type_t *type, hashtable_t *type_args_map)
{
    if (!type) return "java.lang.Object";
    
    switch (type->kind) {
        case TYPE_TYPEVAR: {
            /* Check if we have a substitution for this type variable */
            const char *var_name = type->data.type_var.name;
            if (type_args_map && var_name) {
                const char *subst = (const char *)hashtable_lookup(type_args_map, var_name);
                if (subst) {
                    return subst;  /* Use substituted type */
                }
            }
            /* No substitution - use bound's erasure, or Object if unbounded */
            if (type->data.type_var.bound) {
                return fi_get_substituted_name(type->data.type_var.bound, type_args_map);
            }
            return "java.lang.Object";
        }
            
        case TYPE_CLASS:
            return type->data.class_type.name ? type->data.class_type.name : "java.lang.Object";
            
        case TYPE_ARRAY: {
            /* Arrays: we need element type + "[]" */
            static char array_buf[256];
            const char *elem = fi_get_substituted_name(type->data.array_type.element_type, type_args_map);
            snprintf(array_buf, sizeof(array_buf), "%s[]", elem);
            return array_buf;
        }
            
        case TYPE_BOOLEAN: return "boolean";
        case TYPE_BYTE: return "byte";
        case TYPE_CHAR: return "char";
        case TYPE_SHORT: return "short";
        case TYPE_INT: return "int";
        case TYPE_LONG: return "long";
        case TYPE_FLOAT: return "float";
        case TYPE_DOUBLE: return "double";
        case TYPE_VOID: return "void";
        default: return "java.lang.Object";
    }
}

/**
 * Build a substituted method signature for @FunctionalInterface comparison.
 * Returns "methodName(param1,param2,...)" with type variables substituted.
 */
static char *fi_build_substituted_sig(symbol_t *method, hashtable_t *type_args_map)
{
    if (!method) return NULL;
    
    char buf[512];
    char *p = buf;
    p += snprintf(p, buf + sizeof(buf) - p, "%s(", method->name);
    
    slist_t *params = method->data.method_data.parameters;
    bool first = true;
    for (slist_t *pn = params; pn; pn = pn->next) {
        symbol_t *param = (symbol_t *)pn->data;
        if (param && param->type) {
            if (!first) {
                p += snprintf(p, buf + sizeof(buf) - p, ",");
            }
            p += snprintf(p, buf + sizeof(buf) - p, "%s", 
                fi_get_substituted_name(param->type, type_args_map));
            first = false;
        }
    }
    snprintf(p, buf + sizeof(buf) - p, ")");
    return strdup(buf);
}

/**
 * Helper to count abstract methods, tracking signatures to avoid duplicates.
 * Used for @FunctionalInterface validation.
 * Uses substituted signatures to handle generic type argument inheritance.
 */
static void fi_count_abstract_with_sigs(scope_t *members, fi_validation_t *validation, 
                                        hashtable_t *seen_signatures, hashtable_t *type_args_map)
{
    if (!members || !members->symbols) return;
    
    for (size_t bi = 0; bi < members->symbols->size; bi++) {
        for (hashtable_entry_t *e = members->symbols->buckets[bi]; e; e = e->next) {
            symbol_t *method = (symbol_t *)e->value;
            if (method && method->kind == SYM_METHOD) {
                /* Check if method is abstract (not static, not default) */
                if (!(method->modifiers & MOD_STATIC) &&
                    !(method->modifiers & MOD_DEFAULT)) {
                    /* Build substituted signature for comparison */
                    char *subst_sig = fi_build_substituted_sig(method, type_args_map);
                    if (subst_sig && !hashtable_lookup(seen_signatures, subst_sig)) {
                        /* Note: hashtable takes ownership of subst_sig key */
                        hashtable_insert(seen_signatures, subst_sig, method);
                        validation->abstract_count++;
                        if (validation->abstract_count == 1) {
                            validation->sam = method;
                        }
                    } else {
                        free(subst_sig);  /* Already seen this signature */
                    }
                }
            }
        }
    }
}

/**
 * Build type args map from interface symbol's type parameters and AST's type arguments.
 * Maps type parameter names (e.g., "T", "N") to their substituted type names (e.g., "java.lang.Integer").
 */
static hashtable_t *fi_build_type_args_map(symbol_t *iface, ast_node_t *extends_node)
{
    if (!iface || !extends_node) return NULL;
    
    /* Get the interface's type parameters */
    slist_t *type_params = iface->data.class_data.type_params;
    if (!type_params) return NULL;  /* No type parameters to substitute */
    
    /* Get the type arguments from the AST (children of the extends node) */
    slist_t *type_args = extends_node->data.node.children;
    if (!type_args) return NULL;  /* No type arguments provided */
    
    hashtable_t *map = hashtable_new();
    
    /* Pair up type params with type args */
    slist_t *param_node = type_params;
    slist_t *arg_node = type_args;
    while (param_node && arg_node) {
        symbol_t *param_sym = (symbol_t *)param_node->data;
        ast_node_t *arg_ast = (ast_node_t *)arg_node->data;
        
        if (param_sym && param_sym->name && arg_ast) {
            /* Get the type argument name from AST */
            const char *arg_name = NULL;
            if (arg_ast->type == AST_CLASS_TYPE) {
                arg_name = arg_ast->data.node.name;
            } else if (arg_ast->type == AST_IDENTIFIER) {
                arg_name = arg_ast->data.leaf.name;
            }
            
            if (arg_name) {
                /* Resolve to fully qualified name if it's a simple name */
                char *qualified = NULL;
                if (strcmp(arg_name, "Integer") == 0) qualified = strdup("java.lang.Integer");
                else if (strcmp(arg_name, "String") == 0) qualified = strdup("java.lang.String");
                else if (strcmp(arg_name, "Long") == 0) qualified = strdup("java.lang.Long");
                else if (strcmp(arg_name, "Double") == 0) qualified = strdup("java.lang.Double");
                else if (strcmp(arg_name, "Float") == 0) qualified = strdup("java.lang.Float");
                else if (strcmp(arg_name, "Boolean") == 0) qualified = strdup("java.lang.Boolean");
                else if (strcmp(arg_name, "Byte") == 0) qualified = strdup("java.lang.Byte");
                else if (strcmp(arg_name, "Short") == 0) qualified = strdup("java.lang.Short");
                else if (strcmp(arg_name, "Character") == 0) qualified = strdup("java.lang.Character");
                else if (strcmp(arg_name, "Object") == 0) qualified = strdup("java.lang.Object");
                else if (strcmp(arg_name, "Number") == 0) qualified = strdup("java.lang.Number");
                else qualified = strdup(arg_name);  /* Use as-is for other types */
                
                hashtable_insert(map, strdup(param_sym->name), qualified);
            }
        }
        
        param_node = param_node->next;
        arg_node = arg_node->next;
    }
    
    return map;
}

/**
 * Recursively count abstract methods in an interface and its super-interfaces.
 * type_args_map contains the type argument substitutions for the current interface.
 */
static void fi_count_inherited_abstract(symbol_t *iface, fi_validation_t *validation,
                                        hashtable_t *seen_signatures, hashtable_t *type_args_map)
{
    if (!iface) return;
    
    /* Count methods in this interface with type argument substitution */
    fi_count_abstract_with_sigs(iface->data.class_data.members, validation, 
                                seen_signatures, type_args_map);
    
    /* Recursively check super-interfaces */
    /* Note: we don't have type args for recursive calls without AST access */
    /* This is a simplification - ideally we'd track all type arg chains */
    slist_t *super_interfaces = iface->data.class_data.interfaces;
    for (slist_t *si = super_interfaces; si; si = si->next) {
        symbol_t *super_iface = (symbol_t *)si->data;
        if (super_iface && super_iface->kind == SYM_INTERFACE) {
            fi_count_inherited_abstract(super_iface, validation, seen_signatures, type_args_map);
        }
    }
}

/**
 * Validate @FunctionalInterface annotation on an interface.
 * Reports an error if the interface doesn't have exactly one abstract method.
 */
static void validate_functional_interface(semantic_t *sem, ast_node_t *node, symbol_t *sym)
{
    if (!node || !sym || sym->kind != SYM_INTERFACE) {
        return;
    }
    
    /* Check if it has @FunctionalInterface annotation */
    if (!has_annotation(node, "FunctionalInterface", "java.lang.FunctionalInterface")) {
        return;
    }
    
    /* Count abstract methods including inherited ones */
    fi_validation_t validation = { 0, NULL };
    hashtable_t *seen_signatures = hashtable_new();
    
    /* First count methods directly in this interface (no substitution needed) */
    fi_count_abstract_with_sigs(sym->data.class_data.members, &validation, seen_signatures, NULL);
    
    /* Then count inherited methods with type argument substitution */
    slist_t *children = node->data.node.children;
    for (; children; children = children->next) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child && child->type == AST_CLASS_TYPE && child->data.node.flags == 1) {
            /* This is an extends clause (flags=1) */
            const char *super_name = child->data.node.name;
            
            /* Find the super interface symbol */
            symbol_t *super_iface = NULL;
            for (slist_t *si = sym->data.class_data.interfaces; si; si = si->next) {
                symbol_t *iface = (symbol_t *)si->data;
                if (iface && iface->name && strcmp(iface->name, super_name) == 0) {
                    super_iface = iface;
                    break;
                }
            }
            
            if (super_iface) {
                /* Build type argument map from AST */
                hashtable_t *type_args_map = fi_build_type_args_map(super_iface, child);
                
                /* Count methods from super interface with substitution */
                fi_count_inherited_abstract(super_iface, &validation, seen_signatures, type_args_map);
                
                if (type_args_map) {
                    hashtable_free(type_args_map);
                }
            }
        }
    }
    
    hashtable_free(seen_signatures);
    
    if (validation.abstract_count == 0) {
        semantic_error(sem, node->line, node->column,
            "Interface %s annotated with @FunctionalInterface contains no abstract methods",
            sym->name);
    } else if (validation.abstract_count > 1) {
        semantic_error(sem, node->line, node->column,
            "Interface %s annotated with @FunctionalInterface contains more than one abstract method",
            sym->name);
    }
}

/* ========================================================================
 * Lambda Expression Support
 * ======================================================================== */

/**
 * Infer a type variable's value from a parameter type and argument type.
 * For example: if param is T[] and arg is String, infer T = String.
 * 
 * @param param_type The parameter type (may contain TYPE_TYPEVAR)
 * @param arg_type The argument type (concrete type)
 * @param var_name The type variable name to look for (e.g., "T")
 * @return The inferred type, or NULL if not inferrable
 */
static type_t *infer_type_arg(type_t *param_type, type_t *arg_type, const char *var_name)
{
    if (!param_type || !arg_type || !var_name) {
        return NULL;
    }
    
    /* Direct type variable match */
    if (param_type->kind == TYPE_TYPEVAR) {
        const char *pname = param_type->data.type_var.name;
        if (pname && strcmp(pname, var_name) == 0) {
            /* Type variables can only hold reference types, so box primitives */
            return type_boxed(arg_type);
        }
        return NULL;
    }
    
    /* Array type: T[] with String[] => T = String */
    if (param_type->kind == TYPE_ARRAY && arg_type->kind == TYPE_ARRAY) {
        return infer_type_arg(param_type->data.array_type.element_type, 
                              arg_type->data.array_type.element_type, var_name);
    }
    
    /* Array type: T[] with String (varargs individual element) => T = String */
    if (param_type->kind == TYPE_ARRAY) {
        return infer_type_arg(param_type->data.array_type.element_type, arg_type, var_name);
    }
    
    /* Parameterized type: List<T> with List<String> => T = String */
    if (param_type->kind == TYPE_CLASS && arg_type->kind == TYPE_CLASS) {
        slist_t *param_args = param_type->data.class_type.type_args;
        slist_t *arg_args = arg_type->data.class_type.type_args;
        
        while (param_args && arg_args) {
            type_t *p = (type_t *)param_args->data;
            type_t *a = (type_t *)arg_args->data;
            type_t *result = infer_type_arg(p, a, var_name);
            if (result) return result;
            param_args = param_args->next;
            arg_args = arg_args->next;
        }
    }
    
    /* Wildcard type: ? super T or ? extends T */
    if (param_type->kind == TYPE_WILDCARD && param_type->data.wildcard.bound) {
        return infer_type_arg(param_type->data.wildcard.bound, arg_type, var_name);
    }
    
    return NULL;
}

/**
 * Substitute a type variable in a type with a concrete type.
 * This creates a new type with the type variable replaced.
 * 
 * @param type The type to substitute in (may contain TYPE_TYPEVAR)
 * @param var_name The type variable name to replace (e.g., "T")
 * @param replacement The type to substitute in place of the variable
 * @return A new type with substitution applied, or the original type if unchanged
 */
static type_t *substitute_type_var(type_t *type, const char *var_name, type_t *replacement)
{
    if (!type || !var_name || !replacement) {
        return type;
    }
    
    /* Direct type variable match */
    if (type->kind == TYPE_TYPEVAR) {
        const char *tname = type->data.type_var.name;
        if (tname && strcmp(tname, var_name) == 0) {
            return replacement;
        }
        return type;
    }
    
    /* Array type */
    if (type->kind == TYPE_ARRAY) {
        type_t *new_elem = substitute_type_var(type->data.array_type.element_type, 
                                               var_name, replacement);
        if (new_elem != type->data.array_type.element_type) {
            return type_new_array(new_elem, type->data.array_type.dimensions);
        }
        return type;
    }
    
    /* Parameterized type: substitute in type arguments */
    if (type->kind == TYPE_CLASS && type->data.class_type.type_args) {
        slist_t *new_args = NULL;
        bool changed = false;
        
        for (slist_t *a = type->data.class_type.type_args; a; a = a->next) {
            type_t *orig_arg = (type_t *)a->data;
            type_t *new_arg = substitute_type_var(orig_arg, var_name, replacement);
            if (new_arg != orig_arg) changed = true;
            if (new_args == NULL) {
                new_args = slist_new(new_arg);
            } else {
                slist_append(new_args, new_arg);
            }
        }
        
        if (changed) {
            type_t *new_type = type_new_class(type->data.class_type.name);
            new_type->data.class_type.symbol = type->data.class_type.symbol;
            new_type->data.class_type.type_args = new_args;
            return new_type;
        }
        return type;
    }
    
    /* Wildcard type */
    if (type->kind == TYPE_WILDCARD && type->data.wildcard.bound) {
        type_t *new_bound = substitute_type_var(type->data.wildcard.bound, 
                                                var_name, replacement);
        if (new_bound != type->data.wildcard.bound) {
            return type_new_wildcard(type->data.wildcard.bound_kind, new_bound);
        }
        return type;
    }
    
    return type;
}

/**
 * Substitute type variables in a parameterized type with the receiver's type arguments.
 * This is used for method parameter types where the method's parameter type may reference
 * type parameters of the receiver class (e.g., Stream.map takes Function<? super T, R>
 * where T is Stream's element type).
 * 
 * @param type The type to substitute (may contain TYPE_TYPEVAR or parameterized types)
 * @param recv_type The receiver type with actual type arguments (e.g., Stream<String>)
 * @return A new type with substitutions applied, or the original type if no substitution needed
 */
static type_t *substitute_from_receiver(type_t *type, type_t *recv_type)
{
    if (!type || !recv_type || recv_type->kind != TYPE_CLASS) {
        return type;
    }
    
    slist_t *recv_type_args = recv_type->data.class_type.type_args;
    if (!recv_type_args) {
        if (getenv("GENESIS_DEBUG_SUBST")) {
            fprintf(stderr, "DEBUG substitute_from_receiver: no type_args on recv_type '%s'\n",
                recv_type->data.class_type.name ? recv_type->data.class_type.name : "(null)");
        }
        return type;  /* No type arguments on receiver */
    }
    
    if (type->kind == TYPE_TYPEVAR) {
        const char *var_name = type->data.type_var.name;
        if (!var_name) return type;
        
        if (getenv("GENESIS_DEBUG_SUBST")) {
            fprintf(stderr, "DEBUG substitute_from_receiver: substituting '%s' from recv '%s'\n",
                var_name, recv_type->data.class_type.name ? recv_type->data.class_type.name : "(null)");
        }
        
        /* Try to use the class's actual type parameters list to find position */
        symbol_t *recv_sym = recv_type->data.class_type.symbol;
        if (getenv("GENESIS_DEBUG_SUBST")) {
            fprintf(stderr, "DEBUG substitute typevar '%s': recv_sym=%p type_params=%p\n",
                var_name, (void*)recv_sym,
                recv_sym ? (void*)recv_sym->data.class_data.type_params : NULL);
        }
        if (recv_sym && recv_sym->data.class_data.type_params) {
            /* Find the position of this type variable in the class's type params */
            slist_t *type_params = recv_sym->data.class_data.type_params;
            int idx = 0;
            for (slist_t *tp = type_params; tp; tp = tp->next, idx++) {
                symbol_t *param_sym = (symbol_t *)tp->data;
                if (getenv("GENESIS_DEBUG_SUBST")) {
                    fprintf(stderr, "  checking type_param[%d]: '%s'\n", idx,
                        param_sym && param_sym->name ? param_sym->name : "(null)");
                }
                if (param_sym && param_sym->name && strcmp(param_sym->name, var_name) == 0) {
                    /* Found matching type parameter - get corresponding type arg */
                    slist_t *arg = recv_type_args;
                    for (int i = 0; i < idx && arg; i++) {
                        arg = arg->next;
                    }
                    if (arg) {
                        type_t *result = (type_t *)arg->data;
                        if (getenv("GENESIS_DEBUG_SUBST")) {
                            fprintf(stderr, "DEBUG substituted '%s' -> '%s'\n", var_name,
                                result && result->kind == TYPE_CLASS && result->data.class_type.name ?
                                    result->data.class_type.name : "(unknown)");
                        }
                        return result;
                    }
                    break;
                }
            }
        }
        
        /* Fallback: Common type parameter names and their positions:
         * Function<T, R>: T is 1st, R is 2nd
         * BiFunction<T, U, R>: T is 1st, U is 2nd, R is 3rd
         * Consumer<T>: T is 1st
         * Supplier<T>: T is 1st (used as return type)
         * Predicate<T>: T is 1st
         * Map<K, V>: K is 1st, V is 2nd
         * Stream<T>/List<E>: T/E is 1st
         */
        slist_t *arg = recv_type_args;
        
        /* First type argument: T, E, K */
        if (strcmp(var_name, "T") == 0 || strcmp(var_name, "E") == 0 ||
            strcmp(var_name, "K") == 0) {
            if (arg) {
                type_t *result = (type_t *)arg->data;
                if (getenv("GENESIS_DEBUG_SUBST")) {
                    fprintf(stderr, "DEBUG fallback substituted '%s' -> '%s'\n", var_name,
                        result && result->kind == TYPE_CLASS && result->data.class_type.name ?
                            result->data.class_type.name : "(unknown)");
                }
                return result;
            }
        }
        /* Second type argument: R (for Function), U, V */
        else if (strcmp(var_name, "R") == 0 || strcmp(var_name, "U") == 0 ||
                 strcmp(var_name, "V") == 0) {
            if (arg && arg->next) {
                type_t *result = (type_t *)arg->next->data;
                if (getenv("GENESIS_DEBUG_SUBST")) {
                    fprintf(stderr, "DEBUG fallback substituted '%s' -> '%s'\n", var_name,
                        result && result->kind == TYPE_CLASS && result->data.class_type.name ?
                            result->data.class_type.name : "(unknown)");
                }
                return result;
            }
        }
        
        if (getenv("GENESIS_DEBUG_SUBST")) {
            fprintf(stderr, "DEBUG FAILED to substitute '%s' - no matching type param\n", var_name);
        }
        /* For unknown type variables, try to match by position
         * This is a fallback for other generic types */
        return type;
    }
    
    if (type->kind == TYPE_CLASS && type->data.class_type.type_args) {
        /* Recursively substitute in type arguments */
        slist_t *new_args = NULL;
        bool changed = false;
        
        for (slist_t *a = type->data.class_type.type_args; a; a = a->next) {
            type_t *orig_arg = (type_t *)a->data;
            type_t *new_arg = substitute_from_receiver(orig_arg, recv_type);
            if (new_arg != orig_arg) changed = true;
            /* slist_append returns new node, only assign on first append */
            if (new_args == NULL) {
                new_args = slist_new(new_arg);
            } else {
                slist_append(new_args, new_arg);
            }
        }
        
        if (changed) {
            type_t *new_type = type_new_class(type->data.class_type.name);
            new_type->data.class_type.symbol = type->data.class_type.symbol;
            new_type->data.class_type.type_args = new_args;
            return new_type;
        }
        return type;
    }
    
    if (type->kind == TYPE_ARRAY) {
        type_t *new_elem = substitute_from_receiver(type->data.array_type.element_type, recv_type);
        if (new_elem != type->data.array_type.element_type) {
            return type_new_array(new_elem, type->data.array_type.dimensions);
        }
        return type;
    }
    
    if (type->kind == TYPE_WILDCARD && type->data.wildcard.bound) {
        type_t *new_bound = substitute_from_receiver(type->data.wildcard.bound, recv_type);
        if (new_bound != type->data.wildcard.bound) {
            return type_new_wildcard(type->data.wildcard.bound_kind, new_bound);
        }
        return type;
    }
    
    return type;
}

/**
 * Unwrap a wildcard type to its bound, for use in lambda parameter/return types.
 * - For ? super T (contravariant): return T (used for parameters)
 * - For ? extends T (covariant): return T (used for return types)
 * - For unbounded ?: return Object
 * - For non-wildcard: return as-is
 */
static type_t *unwrap_wildcard(type_t *type)
{
    if (!type || type->kind != TYPE_WILDCARD) {
        return type;
    }
    
    if (type->data.wildcard.bound) {
        return type->data.wildcard.bound;
    }
    
    /* Unbounded ? - return Object */
    return type_new_class("java.lang.Object");
}

/**
 * Substitute type variables in a type with their actual type arguments.
 * @param type The type to substitute (may contain TYPE_TYPEVAR)
 * @param iface_sym The interface symbol (to get type parameter names)
 * @param type_args The actual type arguments (list of type_t*)
 * @return A new type with type variables substituted, or the original type if no substitution needed
 */
static type_t *substitute_type_args(type_t *type, symbol_t *iface_sym, slist_t *type_args)
{
    if (!type || !iface_sym || !type_args) {
        return type;
    }
    
    if (type->kind == TYPE_TYPEVAR) {
        /* Look up the type variable in the interface's type parameters */
        const char *var_name = type->data.type_var.name;
        if (!var_name) return type;
        
        /* Get the interface's type parameter declarations to find the position */
        slist_t *type_params = iface_sym->data.class_data.type_params;
        
        /* Match type variable name against declared type parameters in order */
        int idx = 0;
        for (slist_t *tp = type_params; tp; tp = tp->next, idx++) {
            symbol_t *param_sym = (symbol_t *)tp->data;
            if (param_sym && param_sym->name && strcmp(param_sym->name, var_name) == 0) {
                /* Found matching type parameter - get corresponding type arg */
        slist_t *arg = type_args;
                for (int i = 0; i < idx && arg; i++) {
                    arg = arg->next;
                }
                if (arg) {
                    return (type_t *)arg->data;
                }
                break;
            }
        }
        
        /* Fallback: try common functional interface conventions if no type_params available */
        if (!type_params) {
            slist_t *arg = type_args;
        if (strcmp(var_name, "T") == 0) {
            if (arg) return (type_t *)arg->data;
        } else if (strcmp(var_name, "R") == 0) {
            if (arg && arg->next) return (type_t *)arg->next->data;
        } else if (strcmp(var_name, "U") == 0) {
            if (arg && arg->next) return (type_t *)arg->next->data;
        } else if (strcmp(var_name, "V") == 0) {
            if (arg && arg->next && arg->next->next) return (type_t *)arg->next->next->data;
        }
        /* Fallback: for any other type variable, try first type arg */
        if (arg) return (type_t *)arg->data;
        }
        
        return type;  /* No substitution found */
    }
    
    if (type->kind == TYPE_ARRAY) {
        /* Substitute element type */
        type_t *new_elem = substitute_type_args(type->data.array_type.element_type, iface_sym, type_args);
        if (new_elem != type->data.array_type.element_type) {
            return type_new_array(new_elem, type->data.array_type.dimensions);
        }
        return type;
    }
    
    if (type->kind == TYPE_WILDCARD && type->data.wildcard.bound) {
        /* Substitute bound type */
        type_t *new_bound = substitute_type_args(type->data.wildcard.bound, iface_sym, type_args);
        if (new_bound != type->data.wildcard.bound) {
            return type_new_wildcard(type->data.wildcard.bound_kind, new_bound);
        }
        return type;
    }
    
    if (type->kind == TYPE_CLASS && type->data.class_type.type_args) {
        /* Parameterized type (e.g., Map.Entry<K, V>) - substitute type arguments */
        slist_t *new_type_args = NULL;
        bool changed = false;
        
        for (slist_t *a = type->data.class_type.type_args; a; a = a->next) {
            type_t *orig_arg = (type_t *)a->data;
            type_t *new_arg = substitute_type_args(orig_arg, iface_sym, type_args);
            if (new_arg != orig_arg) changed = true;
            if (new_type_args == NULL) {
                new_type_args = slist_new(new_arg);
            } else {
                slist_append(new_type_args, new_arg);
            }
        }
        
        if (changed) {
            type_t *new_type = type_new_class(type->data.class_type.name);
            new_type->data.class_type.symbol = type->data.class_type.symbol;
            new_type->data.class_type.type_args = new_type_args;
            return new_type;
        }
        return type;
    }
    
    /* Even if type doesn't have type_args, it might be a class type that needs symbol loading */
    if (type->kind == TYPE_CLASS && !type->data.class_type.symbol && type->data.class_type.name) {
        /* Symbol not set - the substituted type will need to have symbol loaded later */
        /* For now, just return the type; codegen will load the symbol */
        return type;
    }
    
    return type;
}

// /* Helper struct for SAM search */
// typedef struct {
//     symbol_t *sam;
//     int abstract_count;
// } sam_search_t;
// 
// static void sam_search_fn(const char *key, void *value, void *user_data)
// {
//     (void)key;
//     sam_search_t *search = (sam_search_t *)user_data;
//     symbol_t *sym = (symbol_t *)value;
//     
//     if (sym && sym->kind == SYM_METHOD) {
//         /* Check if method is abstract (interface methods are implicitly abstract) */
//         if (!(sym->modifiers & MOD_STATIC) &&
//             !(sym->modifiers & MOD_DEFAULT)) {
//             /* Exclude public Object methods per JLS 9.8
//              * Note: clone() and finalize() are PROTECTED, not public, so they count */
//             const char *name = sym->name;
//             if (name) {
//                 /* Skip public Object methods */
//                 if (strcmp(name, "equals") == 0 || 
//                     strcmp(name, "hashCode") == 0 ||
//                     strcmp(name, "toString") == 0 ||
//                     strcmp(name, "getClass") == 0 ||
//                     strcmp(name, "notify") == 0 ||
//                     strcmp(name, "notifyAll") == 0 ||
//                     strcmp(name, "wait") == 0) {
//                     return;  /* Skip public Object methods */
//                 }
//             }
//             
//             /* This is an abstract instance method */
//             search->abstract_count++;
//             if (search->abstract_count == 1) {
//                 search->sam = sym;
//             } else {
//                 /* More than one - not a functional interface */
//                 search->sam = NULL;
//             }
//         }
//     }
// }

/* Helper to recursively collect abstract methods from interface hierarchy */
static void collect_sam_from_interface(symbol_t *iface, hashtable_t *seen_methods, 
                                       hashtable_t *default_methods,
                                       symbol_t **sam, int *count)
{
    if (!iface || iface->kind != SYM_INTERFACE) return;
    
    /* First pass: collect default methods from this interface */
    scope_t *members = iface->data.class_data.members;
    if (members && members->symbols) {
        for (size_t bi = 0; bi < members->symbols->size; bi++) {
            for (hashtable_entry_t *e = members->symbols->buckets[bi]; e; e = e->next) {
                symbol_t *sym = (symbol_t *)e->value;
                if (sym && sym->kind == SYM_METHOD && (sym->modifiers & MOD_DEFAULT)) {
                    /* This is a default method - record it to "cancel out" abstract versions */
                    if (sym->name && !hashtable_lookup(default_methods, sym->name)) {
                        hashtable_insert(default_methods, sym->name, sym);
                    }
                }
            }
        }
    }
    
    /* Second pass: collect abstract methods */
    if (members && members->symbols) {
        for (size_t bi = 0; bi < members->symbols->size; bi++) {
            for (hashtable_entry_t *e = members->symbols->buckets[bi]; e; e = e->next) {
                symbol_t *sym = (symbol_t *)e->value;
                if (sym && sym->kind == SYM_METHOD &&
                    !(sym->modifiers & MOD_STATIC) &&
                    !(sym->modifiers & MOD_DEFAULT)) {
                    /* Skip public Object methods per JLS 9.8 
                     * Note: clone() and finalize() are PROTECTED, not public, so they count */
                    const char *name = sym->name;
                    if (name && (strcmp(name, "equals") == 0 || 
                                 strcmp(name, "hashCode") == 0 ||
                                 strcmp(name, "toString") == 0 ||
                                 strcmp(name, "getClass") == 0 ||
                                 strcmp(name, "notify") == 0 ||
                                 strcmp(name, "notifyAll") == 0 ||
                                 strcmp(name, "wait") == 0)) {
                        continue;
                    }
                    
                    /* Skip if a default method with this name exists (overrides abstract) */
                    if (name && hashtable_lookup(default_methods, name)) {
                        continue;
                    }
                    
                    /* Check if we've already seen a method with this name
                     * (handles diamond inheritance) */
                    if (name && !hashtable_lookup(seen_methods, name)) {
                        hashtable_insert(seen_methods, name, sym);
                        (*count)++;
                        if (*count == 1) {
                            *sam = sym;
                        } else {
                            *sam = NULL;  /* More than one unique method */
                        }
                    }
                }
            }
        }
    }
    
    /* Recursively search superinterfaces */
    slist_t *super_ifaces = iface->data.class_data.interfaces;
    for (slist_t *node = super_ifaces; node; node = node->next) {
        symbol_t *super = (symbol_t *)node->data;
        collect_sam_from_interface(super, seen_methods, default_methods, sam, count);
    }
}

/**
 * Find the single abstract method (SAM) of a functional interface.
 * Returns the method symbol if found, NULL otherwise.
 * Handles diamond inheritance by tracking seen method names.
 * Handles default method overrides (default overrides abstract).
 */
static symbol_t *get_functional_interface_sam(symbol_t *iface_sym)
{
    if (!iface_sym) {
        return NULL;
    }
    
    /* Must be an interface */
    if (iface_sym->kind != SYM_INTERFACE) {
        return NULL;
    }
    
    /* Use hashtable to track seen method names (for diamond inheritance) */
    hashtable_t *seen_methods = hashtable_new();
    hashtable_t *default_methods = hashtable_new();
    symbol_t *sam = NULL;
    int count = 0;
    
    collect_sam_from_interface(iface_sym, seen_methods, default_methods, &sam, &count);
    
    hashtable_free(seen_methods);
    hashtable_free(default_methods);
    
    return (count == 1) ? sam : NULL;
}

/**
 * Recursively scan a lambda body for expressions that require 'this' capture.
 * This includes:
 * - Explicit 'this' references
 * - Creation of non-static inner classes (new Inner())
 * - Anonymous classes extending non-static inner classes
 */
static void scan_lambda_body_for_this_capture(semantic_t *sem, ast_node_t *node, ast_node_t *lambda)
{
    if (!node || !lambda) return;
    
    /* Check if this node requires 'this' capture */
    if (node->type == AST_THIS_EXPR) {
        /* Explicit 'this' reference */
        lambda->lambda_captures_this = true;
        return;
    }
    
    if (node->type == AST_SUPER_EXPR) {
        /* 'super' requires 'this' because super.method() is invokespecial on 'this' */
        lambda->lambda_captures_this = true;
        return;
    }
    
    /* Check for qualified super: K.super (AST_FIELD_ACCESS with name "super") */
    if (node->type == AST_FIELD_ACCESS && node->data.node.name &&
        strcmp(node->data.node.name, "super") == 0) {
        /* Qualified super (K.super) also requires 'this' */
        lambda->lambda_captures_this = true;
        return;
    }
    
    if (node->type == AST_NEW_OBJECT) {
        /* Check if creating a non-static inner class */
        slist_t *children = node->data.node.children;
        
        /* Check for explicit outer instance (qualified new: outer.new Inner()) */
        ast_node_t *explicit_outer = (ast_node_t *)node->data.node.extra;
        
        /* If there's an explicit outer instance, we DON'T need to capture 'this' 
         * for this inner class creation - we'll use the explicit outer instead.
         * The variable holding the outer instance will be captured separately. */
        if (explicit_outer) {
            /* Recursively scan the explicit outer expression - it might require captures */
            scan_lambda_body_for_this_capture(sem, explicit_outer, lambda);
        } else if (children) {
            /* No explicit outer - check if it's an inner class requiring implicit 'this' */
            ast_node_t *type_node = (ast_node_t *)children->data;
            type_t *base_type = semantic_resolve_type(sem, type_node);
            
            if (base_type && base_type->kind == TYPE_CLASS && base_type->data.class_type.symbol) {
                symbol_t *target_class = base_type->data.class_type.symbol;
                
                /* If target is a non-static inner class with NO explicit outer, we need 'this' */
                if (target_class->data.class_data.enclosing_class &&
                    !(target_class->modifiers & MOD_STATIC)) {
                    /* But local classes defined in a static method don't need 'this' capture */
                    bool needs_this = true;
                    if (target_class->data.class_data.is_local_class &&
                        target_class->data.class_data.enclosing_method &&
                        (target_class->data.class_data.enclosing_method->modifiers & MOD_STATIC)) {
                        needs_this = false;
                    }
                    if (needs_this) {
                        lambda->lambda_captures_this = true;
                    }
                }
                
                /* If creating a local class that captures variables, the lambda
                 * needs to capture those same variables to pass to the constructor */
                if (target_class->data.class_data.is_local_class) {
                    /* If superclass is also a local class with captures, inherit them.
                     * This must happen before we process the captures so the target class
                     * has the correct captured_vars for constructor generation. */
                    symbol_t *super_sym = target_class->data.class_data.superclass;
                    if (super_sym && super_sym->data.class_data.is_local_class &&
                        super_sym->data.class_data.captured_vars) {
                        for (slist_t *cap = super_sym->data.class_data.captured_vars; cap; cap = cap->next) {
                            /* Check if already in target class's captures */
                            bool already = false;
                            for (slist_t *tc = target_class->data.class_data.captured_vars; tc; tc = tc->next) {
                                if (tc->data == cap->data) {
                                    already = true;
                                    break;
                                }
                            }
                            if (!already) {
                                if (!target_class->data.class_data.captured_vars) {
                                    target_class->data.class_data.captured_vars = slist_new(cap->data);
                                } else {
                                    slist_append(target_class->data.class_data.captured_vars, cap->data);
                                }
                            }
                        }
                    }
                    
                    /* Now add all captures to the lambda */
                    slist_t *local_class_captures = target_class->data.class_data.captured_vars;
                    for (slist_t *cap = local_class_captures; cap; cap = cap->next) {
                        symbol_t *captured_var = (symbol_t *)cap->data;
                        if (!captured_var) continue;
                        
                        /* Check if already in lambda captures */
                        bool already_captured = false;
                        for (slist_t *lc = lambda->lambda_captures; lc; lc = lc->next) {
                            if (lc->data == captured_var) {
                                already_captured = true;
                                break;
                            }
                        }
                        
                        /* Add to lambda captures if not already there */
                        if (!already_captured) {
                            if (!lambda->lambda_captures) {
                                lambda->lambda_captures = slist_new(captured_var);
                            } else {
                                slist_append(lambda->lambda_captures, captured_var);
                            }
                        }
                    }
                }
            }
        }
    }
    
    /* Check if identifier references an instance field of the current class */
    if (node->type == AST_IDENTIFIER && sem->current_class) {
        const char *name = node->data.leaf.name;
        if (name && sem->current_class->data.class_data.members) {
            symbol_t *sym = scope_lookup_local(sem->current_class->data.class_data.members, name);
            if (sym && sym->kind == SYM_FIELD && !(sym->modifiers & MOD_STATIC)) {
                /* Identifier refers to an instance field - need 'this' capture */
                lambda->lambda_captures_this = true;
            }
        }
    }
    
    /* Check if method call is to an instance method without explicit receiver */
    if (node->type == AST_METHOD_CALL && sem->current_class) {
        const char *method_name = node->data.node.name;
        bool has_explicit_receiver = (node->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER) != 0;
        
        if (!has_explicit_receiver && method_name) {
            symbol_t *method_sym = NULL;
            
            /* Look for the method in current class/interface */
            if (sem->current_class->data.class_data.members) {
                method_sym = scope_lookup_local(
                    sem->current_class->data.class_data.members, method_name);
            }
            
            /* Also search current scope (might catch method defined earlier in same body) */
            if (!method_sym && sem->current_scope) {
                symbol_t *found = scope_lookup(sem->current_scope, method_name);
                if (found && found->kind == SYM_METHOD) {
                    method_sym = found;
                }
            }
            
            /* For interfaces, search superinterfaces too */
            if (!method_sym && sem->current_class->kind == SYM_INTERFACE) {
                slist_t *ifaces = sem->current_class->data.class_data.interfaces;
                for (slist_t *i = ifaces; i && !method_sym; i = i->next) {
                    symbol_t *iface = (symbol_t *)i->data;
                    if (iface && iface->data.class_data.members) {
                        method_sym = scope_lookup_local(iface->data.class_data.members, method_name);
                    }
                }
            }
            
            /* If we're inside a method of the class/interface, assume implicit instance call */
            /* Even if method_sym is null (might be declared later in the file), if the call
             * is unqualified in a default method or instance method, it implies 'this'. */
            if (method_sym && method_sym->kind == SYM_METHOD &&
                !(method_sym->modifiers & MOD_STATIC)) {
                /* Implicit this.method() call - need 'this' capture */
                lambda->lambda_captures_this = true;
            }
            /* Heuristic: if we're in a non-static context and call an unqualified method
             * that we can't resolve, assume it's an instance method call */
            else if (!method_sym && sem->current_method && 
                     !(sem->current_method->modifiers & MOD_STATIC)) {
                /* Can't resolve the method, but we're in non-static context calling
                 * unqualified method - likely an instance method call */
                lambda->lambda_captures_this = true;
            }
        }
    }
    
    /* Recursively scan children */
    switch (node->type) {
        case AST_BLOCK:
        case AST_EXPR_STMT:
        case AST_IF_STMT:
        case AST_WHILE_STMT:
        case AST_FOR_STMT:
        case AST_ENHANCED_FOR_STMT:
        case AST_DO_STMT:
        case AST_TRY_STMT:
        case AST_CATCH_CLAUSE:
        case AST_SWITCH_STMT:
        case AST_SWITCH_RULE:
        case AST_SYNCHRONIZED_STMT:
        case AST_RETURN_STMT:
        case AST_THROW_STMT:
        case AST_NEW_OBJECT:
        case AST_NEW_ARRAY:
        case AST_METHOD_CALL:
        case AST_FIELD_ACCESS:
        case AST_ARRAY_ACCESS:
        case AST_ASSIGNMENT_EXPR:
        case AST_BINARY_EXPR:
        case AST_UNARY_EXPR:
        case AST_CONDITIONAL_EXPR:
        case AST_CAST_EXPR:
        case AST_INSTANCEOF_EXPR:
        case AST_PARENTHESIZED:
        case AST_VAR_DECL:
        case AST_LAMBDA_EXPR:
        case AST_VAR_DECLARATOR:
            {
                slist_t *children = node->data.node.children;
                for (slist_t *c = children; c; c = c->next) {
                    ast_node_t *child = (ast_node_t *)c->data;
                    if (child) {
                        scan_lambda_body_for_this_capture(sem, child, lambda);
                    }
                }
                /* Also check extra (e.g., for statement updates, ternary else) */
                if (node->data.node.extra) {
                    scan_lambda_body_for_this_capture(sem, node->data.node.extra, lambda);
                }
            }
            break;
            
        default:
            break;
    }
}

/**
 * Recursively bind lambda expressions within an array initializer to the array's element type.
 * This handles nested array initializers for multi-dimensional arrays.
 */
static void bind_array_init_elements(semantic_t *sem, ast_node_t *init, type_t *array_type)
{
    if (!init || init->type != AST_ARRAY_INIT || !array_type || array_type->kind != TYPE_ARRAY) {
        return;
    }
    
    /* Get the element type */
    type_t *elem_type = array_type->data.array_type.element_type;
    int dims = array_type->data.array_type.dimensions;
    
    /* For multi-dimensional arrays, the immediate element type is an array with one less dimension */
    type_t *immediate_elem_type = elem_type;
    if (dims > 1) {
        immediate_elem_type = type_new_array(elem_type, dims - 1);
    }
    
    /* Process each element in the initializer */
    slist_t *children = init->data.node.children;
    for (slist_t *c = children; c; c = c->next) {
        ast_node_t *elem = (ast_node_t *)c->data;
        if (!elem) continue;
        
        if (elem->type == AST_LAMBDA_EXPR) {
            /* Bind lambda to the element type */
            bind_lambda_to_target_type(sem, elem, immediate_elem_type);
        } else if (elem->type == AST_METHOD_REF) {
            /* Bind method reference to the element type */
            bind_method_ref_to_target_type(sem, elem, immediate_elem_type);
        } else if (elem->type == AST_ARRAY_INIT) {
            /* Nested array initializer for multi-dimensional arrays */
            elem->sem_type = immediate_elem_type;
            bind_array_init_elements(sem, elem, immediate_elem_type);
        }
    }
}

/**
 * Bind lambda expression to a target functional interface type.
 * Sets up lambda parameters in scope and sets sem_type.
 * Returns true if successful.
 */
static bool bind_lambda_to_target_type(semantic_t *sem, ast_node_t *lambda, type_t *target_type)
{
    if (!lambda || lambda->type != AST_LAMBDA_EXPR || !target_type) {
        return false;
    }
    
    /* Target must be a class/interface type */
    if (target_type->kind != TYPE_CLASS) {
        return false;
    }
    
    symbol_t *iface_sym = target_type->data.class_type.symbol;
    if (!iface_sym) {
        /* Try to load the interface */
        if (target_type->data.class_type.name) {
            iface_sym = load_external_class(sem, target_type->data.class_type.name);
            if (iface_sym) {
                target_type->data.class_type.symbol = iface_sym;
            }
        }
    if (!iface_sym) {
        return false;
        }
    }
    
    /* Get the SAM */
    symbol_t *sam = get_functional_interface_sam(iface_sym);
    if (!sam) {
        return false;
    }
    
    /* Set the lambda's type */
    lambda->sem_type = target_type;
    
    /* Store SAM info on the lambda for codegen */
    lambda->data.node.name = strdup(sam->name);  /* SAM method name */
    lambda->sem_symbol = sam;  /* Store SAM symbol for full access in codegen */
    
    /* Get lambda parameters (first child) */
    slist_t *children = lambda->data.node.children;
    if (!children) {
        return false;
    }
    ast_node_t *params = (ast_node_t *)children->data;
    
    /* Create a scope for the lambda body */
    scope_t *lambda_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
    
    /* Get SAM parameters and type arguments for substitution */
    slist_t *sam_params = sam->data.method_data.parameters;
    slist_t *type_args = target_type->data.class_type.type_args;
    
    /* Handle different parameter patterns:
     * - (x) -> ... : params is AST_IDENTIFIER
     * - (x, y) -> ... : params is AST_BINARY_EXPR with comma
     * - () -> ... : params might be null or empty
     */
    if (params->type == AST_IDENTIFIER) {
        /* Single identifier parameter */
        const char *param_name = params->data.leaf.name;
        
        /* Get type from SAM's first parameter, with type argument substitution */
        type_t *param_type = type_new_primitive(TYPE_UNKNOWN);
        if (sam_params) {
            symbol_t *sam_param = (symbol_t *)sam_params->data;
            if (sam_param && sam_param->type) {
                /* Substitute type variables with actual type arguments */
                param_type = substitute_type_args(sam_param->type, iface_sym, type_args);
                /* Unwrap wildcards for lambda parameters */
                param_type = unwrap_wildcard(param_type);
                        /* Ensure the type symbol is loaded for method lookups */
                        ensure_type_symbol_loaded(sem, param_type);
                /* If still a type variable (unsubstituted), fall back to Object */
                if (param_type->kind == TYPE_TYPEVAR) {
                    param_type = type_new_class("java.lang.Object");
                    symbol_t *obj = load_external_class(sem, "java.lang.Object");
                    if (obj) param_type->data.class_type.symbol = obj;
                }
            }
        }
        
        /* Define parameter in lambda scope */
        symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
        param_sym->type = param_type;
        param_sym->line = params->line;
        param_sym->column = params->column;
        scope_define(lambda_scope, param_sym);
        
        /* Store the type on the parameter node */
        params->sem_type = param_type;
    } else if (params->type == AST_BINARY_EXPR) {
        /* Multiple parameters separated by comma */
        /* Flatten the binary expression tree into a list of parameters using a stack */
        ast_node_t *param_list[16];  /* Max 16 parameters should be enough */
        int param_count = 0;
        
        /* Use a simple iterative approach with stack simulation */
        ast_node_t *stack[32];
        int stack_top = 0;
        stack[stack_top++] = params;
        
        while (stack_top > 0) {
            ast_node_t *node = stack[--stack_top];
            if (!node) continue;
            
            if (node->type == AST_IDENTIFIER) {
                if (param_count < 16) {
                    param_list[param_count++] = node;
                }
            } else if (node->type == AST_BINARY_EXPR) {
                slist_t *bc = node->data.node.children;
                /* Push right child first so left is processed first */
                if (bc && bc->next && stack_top < 32) {
                    stack[stack_top++] = (ast_node_t *)bc->next->data;
                }
                if (bc && stack_top < 32) {
                    stack[stack_top++] = (ast_node_t *)bc->data;
                }
            }
        }
        
        /* Bind each parameter to its type from the SAM */
        slist_t *sam_param_node = sam_params;
        for (int i = 0; i < param_count && sam_param_node; i++) {
            ast_node_t *param_id = param_list[i];
            const char *param_name = param_id->data.leaf.name;
            
            symbol_t *sam_param = (symbol_t *)sam_param_node->data;
            type_t *raw_type = (sam_param && sam_param->type) ? 
                                 sam_param->type : type_new_primitive(TYPE_UNKNOWN);
            type_t *param_type = substitute_type_args(raw_type, iface_sym, type_args);
            /* Ensure the type symbol is loaded for method lookups */
            ensure_type_symbol_loaded(sem, param_type);
            
            symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
            param_sym->type = param_type;
            param_sym->line = param_id->line;
            param_sym->column = param_id->column;
            scope_define(lambda_scope, param_sym);
            
            param_id->sem_type = param_type;
            sam_param_node = sam_param_node->next;
        }
    } else if (params->type == AST_PARENTHESIZED) {
        /* Parenthesized parameter(s) - can be single or multiple with comma */
        slist_t *inner_children = params->data.node.children;
        if (inner_children) {
            ast_node_t *inner = (ast_node_t *)inner_children->data;
            if (inner && inner->type == AST_IDENTIFIER) {
                /* Single parenthesized parameter */
                const char *param_name = inner->data.leaf.name;
                
                /* Get type from SAM's first parameter, with substitution */
                type_t *param_type = type_new_primitive(TYPE_UNKNOWN);
                if (sam_params) {
                    symbol_t *sam_param = (symbol_t *)sam_params->data;
                    if (sam_param && sam_param->type) {
                        param_type = substitute_type_args(sam_param->type, iface_sym, type_args);
                        /* Unwrap wildcards for lambda parameters */
                        param_type = unwrap_wildcard(param_type);
                        /* Ensure the type symbol is loaded for method lookups */
                        ensure_type_symbol_loaded(sem, param_type);
                    }
                }
                
                /* Define parameter in lambda scope */
                symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
                param_sym->type = param_type;
                param_sym->line = inner->line;
                param_sym->column = inner->column;
                scope_define(lambda_scope, param_sym);
                
                /* Store the type on the parameter node */
                inner->sem_type = param_type;
            } else if (inner && inner->type == AST_BINARY_EXPR) {
                /* Multiple comma-separated parameters inside parens */
                ast_node_t *param_list[16];
                int param_count = 0;
                
                ast_node_t *stack[32];
                int stack_top = 0;
                stack[stack_top++] = inner;
                
                while (stack_top > 0) {
                    ast_node_t *node = stack[--stack_top];
                    if (!node) continue;
                    
                    if (node->type == AST_IDENTIFIER) {
                        if (param_count < 16) {
                            param_list[param_count++] = node;
                        }
                    } else if (node->type == AST_BINARY_EXPR) {
                        slist_t *bc = node->data.node.children;
                        if (bc && bc->next && stack_top < 32) {
                            stack[stack_top++] = (ast_node_t *)bc->next->data;
                        }
                        if (bc && stack_top < 32) {
                            stack[stack_top++] = (ast_node_t *)bc->data;
                        }
                    }
                }
                
                slist_t *sam_param_node = sam_params;
                for (int i = 0; i < param_count && sam_param_node; i++) {
                    ast_node_t *param_id = param_list[i];
                    const char *param_name = param_id->data.leaf.name;
                    
                    symbol_t *sam_param = (symbol_t *)sam_param_node->data;
                    type_t *raw_type = (sam_param && sam_param->type) ? 
                                         sam_param->type : type_new_primitive(TYPE_UNKNOWN);
                    type_t *param_type = substitute_type_args(raw_type, iface_sym, type_args);
                    /* Ensure the type symbol is loaded for method lookups */
                    ensure_type_symbol_loaded(sem, param_type);
                    
                    symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
                    param_sym->type = param_type;
                    param_sym->line = param_id->line;
                    param_sym->column = param_id->column;
                    scope_define(lambda_scope, param_sym);
                    
                    param_id->sem_type = param_type;
                    sam_param_node = sam_param_node->next;
                }
            }
        }
    } else if (params->type == AST_LAMBDA_PARAMS) {
        /* Typed lambda parameters: (Type name, Type name, ...) or (var name, var name, ...) */
        slist_t *sam_param_node = sam_params;
        for (slist_t *node = params->data.node.children; node; node = node->next) {
            ast_node_t *param = (ast_node_t *)node->data;
            if (param->type != AST_PARAMETER) continue;
            
            const char *param_name = param->data.node.name;
            type_t *param_type = NULL;
            
            /* Check if parameter has explicit type */
            if (param->data.node.children) {
                ast_node_t *type_node = (ast_node_t *)param->data.node.children->data;
                if (type_node->type == AST_VAR_TYPE) {
                    /* var - infer type from SAM parameter with substitution */
                    if (sam_param_node) {
                        symbol_t *sam_param = (symbol_t *)sam_param_node->data;
                        if (sam_param && sam_param->type) {
                            param_type = substitute_type_args(sam_param->type, iface_sym, type_args);
                            /* Ensure the type symbol is loaded for method lookups */
                            ensure_type_symbol_loaded(sem, param_type);
                        }
                    }
                } else {
                    /* Explicit type - resolve it (no substitution needed) */
                    param_type = semantic_resolve_type(sem, type_node);
                }
            }
            
            /* If no type resolved, get from SAM with substitution */
            if (!param_type && sam_param_node) {
                symbol_t *sam_param = (symbol_t *)sam_param_node->data;
                if (sam_param && sam_param->type) {
                    param_type = substitute_type_args(sam_param->type, iface_sym, type_args);
                    /* Unwrap wildcards for lambda parameters */
                    param_type = unwrap_wildcard(param_type);
                    /* Ensure the type symbol is loaded for method lookups */
                    ensure_type_symbol_loaded(sem, param_type);
                }
            }
            if (!param_type) {
                param_type = type_new_primitive(TYPE_UNKNOWN);
            }
            
            /* Define parameter in lambda scope */
            symbol_t *param_sym = symbol_new(SYM_PARAMETER, param_name);
            param_sym->type = param_type;
            param_sym->line = param->line;
            param_sym->column = param->column;
            scope_define(lambda_scope, param_sym);
            
            /* Store type on the parameter node */
            param->sem_type = param_type;
            
            if (sam_param_node) {
                sam_param_node = sam_param_node->next;
            }
        }
    }
    /* Empty params () - nothing to bind */
    
    /* Infer type arguments from explicitly typed lambda parameters.
     * For example: TU<T, U>.foo(U u) with lambda (Integer x) -> ... 
     * should infer U = Integer.
     * We need to update the lambda's sem_type to have concrete type args. */
    if (type_args && params->type == AST_LAMBDA_PARAMS) {
        slist_t *sam_param_node = sam_params;
        slist_t *new_type_args = NULL;
        slist_t *type_params = iface_sym->data.class_data.type_params;
        bool any_inferred = false;
        
        /* Initialize new_type_args with existing type_args */
        for (slist_t *ta = type_args; ta; ta = ta->next) {
            if (!new_type_args) {
                new_type_args = slist_new(ta->data);
            } else {
                slist_append(new_type_args, ta->data);
            }
        }
        
        /* For each explicitly typed parameter, try to infer type arguments */
        for (slist_t *node = params->data.node.children; node && sam_param_node; node = node->next) {
            ast_node_t *param = (ast_node_t *)node->data;
            if (param->type != AST_PARAMETER) continue;
            
            /* Check if parameter has explicit type */
            if (param->data.node.children) {
                ast_node_t *type_node = (ast_node_t *)param->data.node.children->data;
                if (type_node->type != AST_VAR_TYPE) {
                    /* Explicit type - resolve it */
                    type_t *explicit_type = semantic_resolve_type(sem, type_node);
                    symbol_t *sam_param = (symbol_t *)sam_param_node->data;
                    
                    if (explicit_type && sam_param && sam_param->type) {
                        /* Try to infer type arguments from explicit_type matching sam_param->type */
                        slist_t *tp_node = type_params;
                        slist_t *ta_node = new_type_args;
                        while (tp_node && ta_node) {
                            symbol_t *tp = (symbol_t *)tp_node->data;
                            if (tp && tp->name) {
                                type_t *inferred = infer_type_arg(sam_param->type, explicit_type, tp->name);
                                if (inferred) {
                                    /* Replace the type arg at this position */
                                    ta_node->data = inferred;
                                    any_inferred = true;
                                }
                            }
                            tp_node = tp_node->next;
                            ta_node = ta_node->next;
                        }
                    }
                }
            }
            sam_param_node = sam_param_node->next;
        }
        
        /* If we inferred any types, update the lambda's type */
        if (any_inferred && new_type_args) {
            type_t *new_type = type_new_class(target_type->data.class_type.name);
            new_type->data.class_type.symbol = iface_sym;
            new_type->data.class_type.type_args = new_type_args;
            lambda->sem_type = new_type;
            type_args = new_type_args;  /* Update for later use */
        }
    }
    
    /* Check the lambda body with the lambda scope */
    if (children->next) {
        ast_node_t *body = (ast_node_t *)children->next->data;
        
        /* Save current scope and switch to lambda scope */
        scope_t *saved_scope = sem->current_scope;
        ast_node_t *saved_lambda = sem->current_lambda;
        scope_t *saved_enclosing = sem->lambda_enclosing_scope;
        
        sem->current_scope = lambda_scope;
        sem->current_lambda = lambda;  /* Track this lambda for capture detection */
        sem->lambda_enclosing_scope = saved_scope;  /* Remember where captures come from */
        
        /* Initialize capture tracking on the lambda node */
        lambda->lambda_captures = NULL;
        lambda->lambda_captures_this = false;
        
        /* Type-check the body expression/block */
        if (body) {
            if (body->type == AST_BLOCK) {
                /* Block body - we need to scan for inner class creations to detect 'this' capture.
                 * Full analysis is deferred to the main pass, but we need to know about captures
                 * now to generate the correct lambda method signature.
                 * 
                 * Recursively scan the block for AST_NEW_OBJECT nodes that create non-static 
                 * inner classes, which require capturing 'this'.
                 */
                scan_lambda_body_for_this_capture(sem, body, lambda);
                
                /* For block bodies, try to infer type arguments from return statements.
                 * Scan the block for AST_RETURN_STMT and get the expression type. */
                if (sam->type && sam->type->kind != TYPE_VOID) {
                    type_t *expected_type = substitute_type_args(sam->type, iface_sym, type_args);
                    expected_type = unwrap_wildcard(expected_type);
                    
                    if (expected_type && expected_type->kind == TYPE_TYPEVAR) {
                        /* Return type is still a type variable - scan for return statements */
                        type_t *body_type = NULL;
                        
                        /* Simple scan for first return statement's expression type */
                        for (slist_t *stmt = body->data.node.children; stmt && !body_type; stmt = stmt->next) {
                            ast_node_t *s = (ast_node_t *)stmt->data;
                            if (s && s->type == AST_RETURN_STMT && s->data.node.children) {
                                ast_node_t *ret_expr = (ast_node_t *)s->data.node.children->data;
                                if (ret_expr) {
                                    body_type = get_expression_type(sem, ret_expr);
                                }
                            }
                        }
                        
                        /* If we found a return type, update the lambda's type */
                        if (body_type && body_type->kind != TYPE_UNKNOWN && 
                            body_type->kind != TYPE_TYPEVAR) {
                            if (lambda->sem_type && lambda->sem_type->kind == TYPE_CLASS) {
                                type_t *new_type = type_new_class(lambda->sem_type->data.class_type.name);
                                new_type->data.class_type.symbol = lambda->sem_type->data.class_type.symbol;
                                
                                /* Replace type variable in type args with body_type */
                                slist_t *orig_args = lambda->sem_type->data.class_type.type_args;
                                slist_t *new_args = NULL;
                                while (orig_args) {
                                    type_t *arg = (type_t *)orig_args->data;
                                    type_t *new_arg = arg;
                                    type_t *arg_unwrapped = arg;
                                    if (arg->kind == TYPE_WILDCARD && arg->data.wildcard.bound) {
                                        arg_unwrapped = arg->data.wildcard.bound;
                                    }
                                    /* If this arg is the type variable in expected_type, substitute */
                                    if (arg_unwrapped->kind == TYPE_TYPEVAR && 
                                        arg_unwrapped->data.type_var.name &&
                                        expected_type->data.type_var.name &&
                                        strcmp(arg_unwrapped->data.type_var.name, 
                                               expected_type->data.type_var.name) == 0) {
                                        /* Box primitives */
                                        switch (body_type->kind) {
                                            case TYPE_INT: new_arg = type_new_class("java.lang.Integer"); break;
                                            case TYPE_LONG: new_arg = type_new_class("java.lang.Long"); break;
                                            case TYPE_DOUBLE: new_arg = type_new_class("java.lang.Double"); break;
                                            case TYPE_FLOAT: new_arg = type_new_class("java.lang.Float"); break;
                                            case TYPE_BOOLEAN: new_arg = type_new_class("java.lang.Boolean"); break;
                                            case TYPE_BYTE: new_arg = type_new_class("java.lang.Byte"); break;
                                            case TYPE_SHORT: new_arg = type_new_class("java.lang.Short"); break;
                                            case TYPE_CHAR: new_arg = type_new_class("java.lang.Character"); break;
                                            default: new_arg = body_type; break;
                                        }
                                    }
                                    if (!new_args) {
                                        new_args = slist_new(new_arg);
                                    } else {
                                        slist_append(new_args, new_arg);
                                    }
                                    orig_args = orig_args->next;
                                }
                                new_type->data.class_type.type_args = new_args;
                                lambda->sem_type = new_type;
                            }
                        }
                    }
                }
            } else {
                /* Expression body - check and get its type */
                
                /* If the body is a nested lambda, bind it to the SAM's return type first.
                 * This handles cases like: s1 -> s2 -> s1 + s2
                 * where the inner lambda needs to be bound before we can get its type.
                 * Also handle parenthesized nested lambdas: () -> (() -> ...) */
                type_t *expected_return_type = NULL;
                if (sam->type && sam->type->kind != TYPE_VOID) {
                    expected_return_type = substitute_type_args(sam->type, iface_sym, type_args);
                    expected_return_type = unwrap_wildcard(expected_return_type);
                    
                    /* Unwrap parentheses to find nested lambda */
                    ast_node_t *inner = body;
                    while (inner && inner->type == AST_PARENTHESIZED && 
                           inner->data.node.children) {
                        inner = (ast_node_t *)inner->data.node.children->data;
                    }
                    
                    if (inner && inner->type == AST_LAMBDA_EXPR && expected_return_type && 
                        expected_return_type->kind == TYPE_CLASS) {
                        bind_lambda_to_target_type(sem, inner, expected_return_type);
                    } else if (inner && inner->type == AST_METHOD_REF && expected_return_type && 
                               expected_return_type->kind == TYPE_CLASS) {
                        bind_method_ref_to_target_type(sem, inner, expected_return_type);
                    }
                }
                
                type_t *body_type = get_expression_type(sem, body);
                
                /* Scan for 'this' capture (same as block body) */
                scan_lambda_body_for_this_capture(sem, body, lambda);
                
                /* Store body type for codegen (used for boxing decision) */
                if (body_type && body_type->kind != TYPE_UNKNOWN) {
                    body->sem_type = body_type;
                }
                
                /* Check return type matches SAM return type (with type arg substitution) */
                if (sam->type && sam->type->kind != TYPE_VOID) {
                    type_t *expected_type = expected_return_type ? expected_return_type :
                                            substitute_type_args(sam->type, iface_sym, type_args);
                    /* Unwrap wildcards for return type check */
                    expected_type = unwrap_wildcard(expected_type);
                    
                    /* For unbounded type variables (R not substituted), accept any type
                     * but infer R from the body type for method return type inference */
                    if (expected_type && expected_type->kind == TYPE_TYPEVAR && body_type) {
                        /* R is unsubstituted - infer it from body type.
                         * Update lambda's sem_type to include inferred R. */
                        if (lambda->sem_type && lambda->sem_type->kind == TYPE_CLASS) {
                            /* Create new type with inferred type argument */
                            type_t *new_type = type_new_class(lambda->sem_type->data.class_type.name);
                            new_type->data.class_type.symbol = lambda->sem_type->data.class_type.symbol;
                            
                            /* Copy type args, substituting R with body_type.
                             * Type args may be wildcards like "? extends R" so unwrap them.
                             * Since external classes may not have type_params loaded, we check
                             * if the type arg contains a type variable named R directly. */
                            slist_t *orig_args = lambda->sem_type->data.class_type.type_args;
                            slist_t *new_args = NULL;
                            while (orig_args) {
                                type_t *arg = (type_t *)orig_args->data;
                                type_t *new_arg = arg;
                                /* Unwrap wildcards to check if it's a type variable */
                                type_t *arg_unwrapped = arg;
                                if (arg->kind == TYPE_WILDCARD && arg->data.wildcard.bound) {
                                    arg_unwrapped = arg->data.wildcard.bound;
                                }
                                /* If this is a type variable that represents the return type (R or T),
                                 * substitute with body_type.
                                 * If body_type is a primitive, use the boxed wrapper type
                                 * since type variables are reference types. */
                                const char *var_name = arg_unwrapped->kind == TYPE_TYPEVAR && 
                                                       arg_unwrapped->data.type_var.name ?
                                                       arg_unwrapped->data.type_var.name : NULL;
                                if (var_name && (strcmp(var_name, "R") == 0 || strcmp(var_name, "T") == 0)) {
                                    /* Box primitives to their wrapper types */
                                    switch (body_type->kind) {
                                        case TYPE_INT:
                                            new_arg = type_new_class("java.lang.Integer");
                                            break;
                                        case TYPE_LONG:
                                            new_arg = type_new_class("java.lang.Long");
                                            break;
                                        case TYPE_DOUBLE:
                                            new_arg = type_new_class("java.lang.Double");
                                            break;
                                        case TYPE_FLOAT:
                                            new_arg = type_new_class("java.lang.Float");
                                            break;
                                        case TYPE_BOOLEAN:
                                            new_arg = type_new_class("java.lang.Boolean");
                                            break;
                                        case TYPE_BYTE:
                                            new_arg = type_new_class("java.lang.Byte");
                                            break;
                                        case TYPE_SHORT:
                                            new_arg = type_new_class("java.lang.Short");
                                            break;
                                        case TYPE_CHAR:
                                            new_arg = type_new_class("java.lang.Character");
                                            break;
                                        default:
                                            new_arg = body_type;
                                            break;
                                    }
                                }
                                if (!new_args) {
                                    new_args = slist_new(new_arg);
                                } else {
                                    slist_append(new_args, new_arg);
                                }
                                orig_args = orig_args->next;
                            }
                            new_type->data.class_type.type_args = new_args;
                            lambda->sem_type = new_type;
                        }
                    } else if (expected_type && expected_type->kind != TYPE_TYPEVAR &&
                        !type_assignable(expected_type, body_type)) {
                        char *expected = type_to_string(expected_type);
                        char *actual = type_to_string(body_type);
                        semantic_error(sem, body->line, body->column,
                            "Lambda body has incompatible return type: expected %s, got %s",
                            expected, actual);
                        free(expected);
                        free(actual);
                    }
                }
            }
        }
        
        /* Restore scope and lambda tracking */
        sem->current_scope = saved_scope;
        sem->current_lambda = saved_lambda;
        sem->lambda_enclosing_scope = saved_enclosing;
    }
    
    return true;
}

/**
 * Bind method reference to a target functional interface type.
 * Resolves the referenced method and sets sem_type.
 * Returns true if successful.
 */
static bool bind_method_ref_to_target_type(semantic_t *sem, ast_node_t *ref, type_t *target_type)
{
    if (!ref || ref->type != AST_METHOD_REF || !target_type) {
        return false;
    }
    
    /* Target must be a class/interface type */
    if (target_type->kind != TYPE_CLASS) {
        return false;
    }
    
    symbol_t *iface_sym = target_type->data.class_type.symbol;
    if (!iface_sym && target_type->data.class_type.name) {
        /* Try to load external class */
        iface_sym = load_external_class(sem, target_type->data.class_type.name);
        if (iface_sym) {
            target_type->data.class_type.symbol = iface_sym;
        }
    }
    if (!iface_sym) {
        return false;
    }
    
    /* Get the SAM */
    symbol_t *sam = get_functional_interface_sam(iface_sym);
    if (!sam) {
        return false;
    }
    
    /* Get the target type/expression and method name from the AST node
     * Children[0] = type/expression
     * data.node.name = method name (or "new" for constructor) 
     */
    slist_t *children = ref->data.node.children;
    if (!children) {
        return false;
    }
    ast_node_t *target_node = (ast_node_t *)children->data;
    const char *method_name = ref->data.node.name;
    
    if (!target_node || !method_name) {
        return false;
    }
    
    /* Determine the kind of method reference and resolve the target class */
    type_t *target_class_type = NULL;
    symbol_t *target_class_sym = NULL;
    bool is_constructor = (strcmp(method_name, "new") == 0);
    bool is_static = false;
    bool is_bound = false;
    symbol_t *resolved_method = NULL;
    
    /* Note: We could validate SAM parameter count here but for now we trust
     * the method resolution to find the correct matching method. */
    (void)sam;  /* SAM is validated but not directly used here */
    
    /* Handle array type references like String[]::new or String[]::clone */
    bool is_array_constructor = false;
    
    if (target_node->type == AST_ARRAY_TYPE) {
        /* Get the element type from the array type node */
        ast_node_t *elem_node = NULL;
        if (target_node->data.node.children) {
            elem_node = (ast_node_t *)target_node->data.node.children->data;
        }
        
        /* Resolve the element type */
        type_t *elem_type = elem_node ? semantic_resolve_type(sem, elem_node) : NULL;
        if (!elem_type) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot resolve array element type");
            return false;
        }
        
        /* Create array type */
        target_class_type = type_new_array(elem_type, 1);
        
        if (is_constructor) {
            /* Array constructor reference: Type[]::new */
        is_array_constructor = true;
        
        /* For array constructors, we don't have a "resolved method" - the array
         * creation is handled specially in codegen */
        resolved_method = NULL;
        } else {
            /* Unbound array method reference: Type[]::clone
             * The SAM takes an array as first parameter, e.g., Function<int[], int[]>
             * Treat as Object for method lookup (arrays have clone() etc.) */
            symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
            if (obj_sym) {
                target_class_sym = obj_sym;
            }
            
            /* Find the method in Object (e.g., clone) */
            int sam_param_count = 0;
            for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                sam_param_count++;
            }
            
            /* For unbound method reference, the first SAM param is the receiver,
             * so method takes sam_param_count - 1 params */
            if (target_class_sym && target_class_sym->data.class_data.members) {
                resolved_method = scope_lookup_method_with_args(
                    target_class_sym->data.class_data.members, method_name, sam_param_count - 1);
            }
            if (!resolved_method || resolved_method->kind != SYM_METHOD) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot find method '%s' for array method reference", method_name);
                return false;
            }
            
            /* Mark as not bound - receiver comes from SAM param */
            is_bound = false;
        }
        
    } else if (target_node->type == AST_SUPER_EXPR) {
        /* super::method - bound method reference to superclass
         * This is valid in instance context (instance fields, instance methods, lambdas) */
        if (!sem->current_class) {
            semantic_error(sem, ref->line, ref->column,
                "'super' used outside of class context");
            return false;
        }
        if (!sem->current_class->data.class_data.superclass) {
            semantic_error(sem, ref->line, ref->column,
                "'super' used in class with no superclass");
            return false;
        }
        
        /* Check for static context - NOT allowed in static methods/fields 
         * But allowed in anonymous class instance initializers even inside static methods */
        if (sem->current_method && (sem->current_method->modifiers & MOD_STATIC) &&
            !sem->current_lambda) {
            /* Check if we're in an anonymous class (which is always instance context) */
            bool in_anonymous_class = sem->current_class && 
                                      sem->current_class->data.class_data.is_anonymous_class;
            if (!in_anonymous_class) {
                semantic_error(sem, ref->line, ref->column,
                    "cannot use 'super' in a static method");
                return false;
            }
        }
        
        target_class_sym = sem->current_class->data.class_data.superclass;
        target_class_type = target_class_sym->type;
        is_bound = true;
        
        if (is_constructor) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot use constructor reference with 'super'");
            return false;
        }
        
        /* Find the method in superclass */
        if (target_class_sym->data.class_data.members) {
            resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
        }
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot find method '%s' in superclass for method reference", method_name);
            return false;
        }
        
        /* Track this capture for lambda codegen */
        if (sem->current_lambda) {
            sem->current_lambda->lambda_captures_this = true;
        }
        
    } else if (target_node->type == AST_THIS_EXPR) {
        /* this::method - bound method reference to current instance
         * Similar to super but uses current class instead of superclass */
        if (!sem->current_class) {
            semantic_error(sem, ref->line, ref->column,
                "'this' used outside of class context");
            return false;
        }
        
        /* Check for static context */
        if (sem->current_method && (sem->current_method->modifiers & MOD_STATIC) &&
            !sem->current_lambda) {
            bool in_anonymous_class = sem->current_class && 
                                      sem->current_class->data.class_data.is_anonymous_class;
            if (!in_anonymous_class) {
                semantic_error(sem, ref->line, ref->column,
                    "cannot use 'this' in a static method");
                return false;
            }
        }
        
        target_class_sym = sem->current_class;
        target_class_type = target_class_sym->type;
        is_bound = true;
        
        if (is_constructor) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot use constructor reference with 'this'");
            return false;
        }
        
        /* Find the method in current class and superclass chain */
        if (target_class_sym->data.class_data.members) {
            resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
        }
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            symbol_t *super = target_class_sym->data.class_data.superclass;
            while (super && (!resolved_method || resolved_method->kind != SYM_METHOD)) {
                if (super->data.class_data.members) {
                    resolved_method = scope_lookup_method(super->data.class_data.members, method_name);
                }
                super = super->data.class_data.superclass;
            }
        }
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot find method '%s' for method reference", method_name);
            return false;
        }
        
        /* Track this capture for lambda codegen */
        if (sem->current_lambda) {
            sem->current_lambda->lambda_captures_this = true;
        }
        
    } else if (target_node->type == AST_IDENTIFIER || target_node->type == AST_CLASS_TYPE) {
        /* Could be:
         * - ClassName::method (static or unbound instance method)
         * - ClassName::new (constructor reference)
         * - variableName::method (bound instance method reference)
         * - ClassName<TypeArgs>::method (parameterized type reference)
         * - ClassName<TypeArgs>::new (parameterized constructor reference)
         */
        const char *name = (target_node->type == AST_IDENTIFIER) ?
                          target_node->data.leaf.name :
                          target_node->data.node.name;
        
        /* First, try to resolve as a variable (for bound method references) */
        symbol_t *var_sym = scope_lookup(sem->current_scope, name);
        if (var_sym && (var_sym->kind == SYM_LOCAL_VAR || var_sym->kind == SYM_PARAMETER ||
                        var_sym->kind == SYM_FIELD)) {
            /* This is a bound method reference like instance::method */
            target_class_type = var_sym->type;
            if (!target_class_type || (target_class_type->kind != TYPE_CLASS &&
                                        target_class_type->kind != TYPE_ARRAY)) {
                semantic_error(sem, ref->line, ref->column,
                    "Method reference target '%s' must be a class type", name);
                return false;
            }
            
            /* For array types, treat as Object for method lookup (arrays have clone() etc.) */
            if (target_class_type->kind == TYPE_ARRAY) {
                symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
                if (obj_sym) {
                    target_class_sym = obj_sym;
                }
            } else {
            target_class_sym = target_class_type->data.class_type.symbol;
            }
            is_bound = true;
            
            if (is_constructor) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot use constructor reference with instance expression");
                return false;
            }
            
            /* Find the instance method - check class and superclass chain */
            if (target_class_sym && target_class_sym->data.class_data.members) {
                resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
            }
            /* Search superclass chain if not found */
            if (!resolved_method || resolved_method->kind != SYM_METHOD) {
                symbol_t *super = target_class_sym ? target_class_sym->data.class_data.superclass : NULL;
                while (super && (!resolved_method || resolved_method->kind != SYM_METHOD)) {
                    if (super->data.class_data.members) {
                        resolved_method = scope_lookup_method(super->data.class_data.members, method_name);
                    }
                    super = super->data.class_data.superclass;
                }
            }
            /* If still not found, try Object (for interface types) */
            if (!resolved_method || resolved_method->kind != SYM_METHOD) {
                symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
                if (obj_sym && obj_sym->data.class_data.members) {
                    resolved_method = scope_lookup_method(obj_sym->data.class_data.members, method_name);
                }
            }
            if (!resolved_method || resolved_method->kind != SYM_METHOD) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot find method '%s' for bound method reference", method_name);
                return false;
            }
            if (resolved_method->modifiers & MOD_STATIC) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot bind static method '%s' to instance", method_name);
                return false;
            }
        } else {
            /* Try to resolve as a class (for static/unbound references or ::new) */
            /* First try local scope lookup */
            target_class_sym = scope_lookup(sem->current_scope, name);
            if (target_class_sym && (target_class_sym->kind == SYM_CLASS || 
                                     target_class_sym->kind == SYM_INTERFACE)) {
                const char *qname = target_class_sym->qualified_name ? 
                                   target_class_sym->qualified_name : target_class_sym->name;
                target_class_type = type_new_class(qname);
                target_class_type->data.class_type.symbol = target_class_sym;
            }
            
            /* If not found locally, try import resolution (including java.lang.*) */
            if (!target_class_type) {
                char *qualified_name = resolve_import(sem, name);
                if (qualified_name) {
                    target_class_sym = load_external_class(sem, qualified_name);
                    if (target_class_sym) {
                        target_class_type = type_new_class(qualified_name);
                        target_class_type->data.class_type.symbol = target_class_sym;
                    }
                    free(qualified_name);
                }
            }
            
            if (!target_class_type) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot resolve type '%s' for method reference", name);
                return false;
            }
            
            if (is_constructor) {
                /* ClassName::new - constructor reference
                 * Look for constructor with matching param count based on SAM */
                int sam_param_count = 0;
                for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                    sam_param_count++;
                }
                
                if (target_class_sym->data.class_data.members) {
                    /* For source-defined constructors, try key format name(N) */
                    char ctor_key[256];
                    snprintf(ctor_key, sizeof(ctor_key), "%s(%d)", name, sam_param_count);
                    resolved_method = scope_lookup_local(target_class_sym->data.class_data.members, ctor_key);
                    if (!resolved_method || resolved_method->kind != SYM_CONSTRUCTOR) {
                        /* Try <init>(N) for classfile-loaded constructors */
                        snprintf(ctor_key, sizeof(ctor_key), "<init>(%d)", sam_param_count);
                        resolved_method = scope_lookup_local(target_class_sym->data.class_data.members, ctor_key);
                    }
                    /* If still not found, try to find constructor with matching param count */
                    if (!resolved_method || resolved_method->kind != SYM_CONSTRUCTOR) {
                        slist_t *ctors = scope_find_all_methods(target_class_sym->data.class_data.members, name);
                        if (ctors) {
                            for (slist_t *c = ctors; c; c = c->next) {
                                symbol_t *ctor = (symbol_t *)c->data;
                                if (ctor && ctor->kind == SYM_CONSTRUCTOR) {
                                    /* Count constructor parameters */
                                    int ctor_param_count = 0;
                                    for (slist_t *cp = ctor->data.method_data.parameters; cp; cp = cp->next) {
                                        ctor_param_count++;
                                    }
                                    if (ctor_param_count == sam_param_count) {
                                    resolved_method = ctor;
                                        break;
                                    }
                                }
                            }
                            slist_free(ctors);
                        }
                    }
                    /* Also try <init> constructors if not found yet */
                    if (!resolved_method || resolved_method->kind != SYM_CONSTRUCTOR) {
                        slist_t *inits = scope_find_all_methods(target_class_sym->data.class_data.members, "<init>");
                        if (inits) {
                            for (slist_t *c = inits; c; c = c->next) {
                                symbol_t *ctor = (symbol_t *)c->data;
                                if (ctor && ctor->kind == SYM_CONSTRUCTOR) {
                                    int ctor_param_count = 0;
                                    for (slist_t *cp = ctor->data.method_data.parameters; cp; cp = cp->next) {
                                        ctor_param_count++;
                                    }
                                    if (ctor_param_count == sam_param_count) {
                                        resolved_method = ctor;
                                        break;
                                    }
                                }
                            }
                            slist_free(inits);
                        }
                    }
                }
                if (!resolved_method) {
                    semantic_error(sem, ref->line, ref->column,
                        "Cannot find constructor for %s::new with %d parameter(s)", 
                        name, sam_param_count);
                    return false;
                }
            } else {
                /* ClassName::method - look up the method
                 * We need to find a method with matching parameter count:
                 * - For static methods: SAM param count
                 * - For unbound instance methods: SAM param count - 1 (first param is receiver) */
                int sam_param_count = 0;
                for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
                    sam_param_count++;
                }
                
                if (target_class_sym->data.class_data.members) {
                    /* First try to find static method with exact SAM param count */
                    resolved_method = scope_lookup_method_with_args(
                        target_class_sym->data.class_data.members, method_name, sam_param_count);
                    
                    /* If found, check if it's static */
                    if (resolved_method && resolved_method->kind == SYM_METHOD) {
                        is_static = (resolved_method->modifiers & MOD_STATIC) != 0;
                    }
                    
                    /* If not found or not static, try unbound instance method with SAM-1 params */
                    if (!resolved_method || resolved_method->kind != SYM_METHOD) {
                        resolved_method = scope_lookup_method_with_args(
                            target_class_sym->data.class_data.members, method_name, sam_param_count - 1);
                        if (resolved_method && resolved_method->kind == SYM_METHOD) {
                            is_static = (resolved_method->modifiers & MOD_STATIC) != 0;
                        }
                    }
                }
                if (resolved_method && resolved_method->kind == SYM_METHOD) {
                    is_static = (resolved_method->modifiers & MOD_STATIC) != 0;
                } else {
                    semantic_error(sem, ref->line, ref->column,
                        "Cannot find method '%s' in %s", method_name, name);
                    return false;
                }
            }
        }
    } else {
        /* expression::method - bound instance method reference
         * (more complex expressions like field access, method call, etc.) */
        target_class_type = get_expression_type(sem, target_node);
        if (!target_class_type || (target_class_type->kind != TYPE_CLASS && 
                                    target_class_type->kind != TYPE_ARRAY)) {
            const char *target_name = target_node->data.node.name ? 
                                      target_node->data.node.name : "";
            semantic_error(sem, ref->line, ref->column,
                "Method reference target%s%s%s must be a class type",
                target_name[0] ? " '" : "",
                target_name,
                target_name[0] ? "'" : "");
            return false;
        }
        
        /* For array types, treat as Object for method lookup (arrays have clone() etc.) */
        if (target_class_type->kind == TYPE_ARRAY) {
            symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
            if (obj_sym) {
                target_class_sym = obj_sym;
            }
        } else {
        target_class_sym = target_class_type->data.class_type.symbol;
        }
        
        /* If symbol is NULL, try to load the external class */
        if (!target_class_sym && target_class_type->data.class_type.name) {
            target_class_sym = load_external_class(sem, target_class_type->data.class_type.name);
            if (target_class_sym) {
                target_class_type->data.class_type.symbol = target_class_sym;
            }
        }
        
        is_bound = true;
        
        if (is_constructor) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot use constructor reference with instance expression");
            return false;
        }
        
        /* Find the instance method - match SAM param count */
        int sam_param_count = 0;
        for (slist_t *p = sam->data.method_data.parameters; p; p = p->next) {
            sam_param_count++;
        }
        
        if (target_class_sym && target_class_sym->data.class_data.members) {
            resolved_method = scope_lookup_method_with_args(
                target_class_sym->data.class_data.members, method_name, sam_param_count);
        }
        /* Search superclass chain if not found */
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            symbol_t *super = target_class_sym ? target_class_sym->data.class_data.superclass : NULL;
            while (super && (!resolved_method || resolved_method->kind != SYM_METHOD)) {
                if (super->data.class_data.members) {
                    resolved_method = scope_lookup_method_with_args(
                        super->data.class_data.members, method_name, sam_param_count);
                }
                super = super->data.class_data.superclass;
            }
        }
        /* If still not found, try Object (for interface types) */
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            symbol_t *obj_sym = load_external_class(sem, "java.lang.Object");
            if (obj_sym && obj_sym->data.class_data.members) {
                resolved_method = scope_lookup_method_with_args(
                    obj_sym->data.class_data.members, method_name, sam_param_count);
            }
        }
        if (!resolved_method || resolved_method->kind != SYM_METHOD) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot find method '%s' for bound method reference", method_name);
            return false;
        }
        if (resolved_method->modifiers & MOD_STATIC) {
            semantic_error(sem, ref->line, ref->column,
                "Cannot bind static method '%s' to instance", method_name);
            return false;
        }
    }
    
    /* Set the method reference's type and store info for codegen */
    ref->sem_type = target_type;
    ref->sem_symbol = resolved_method;  /* The resolved method */
    
    /* Store target class type on the target node */
    target_node->sem_type = target_class_type;
    
    /* Store method reference kind info for codegen.
     * We use the data.node.flags field to store reference kind:
     *   MOD_STATIC = static method reference
     *   MOD_PRIVATE = bound (has receiver to capture)
     *   MOD_ABSTRACT = constructor reference
     *   MOD_NATIVE = array constructor reference (Type[]::new)
     *   None of above = unbound instance method
     */
    ref->data.node.flags = 0;
    if (is_array_constructor) {
        ref->data.node.flags |= MOD_NATIVE;  /* Repurpose for "is array constructor" */
    } else if (is_constructor) {
        ref->data.node.flags |= MOD_ABSTRACT;  /* Repurpose for "is constructor" */
    } else if (is_static) {
        ref->data.node.flags |= MOD_STATIC;
    } else if (is_bound) {
        ref->data.node.flags |= MOD_PRIVATE;  /* Repurpose for "is bound" */
    }
    /* Unbound instance method: no special flag */
    
    return true;
}

/**
 * Resolve superclass and interface references for all classes.
 * This runs after pass1 so all class symbols are registered,
 * handling forward references where class A extends B and B is defined later.
 */
static void resolve_type_hierarchy(semantic_t *sem, ast_node_t *ast)
{
    if (!ast) {
        return;
    }
    
    /* Use a simple recursive traversal to find all class declarations */
    if (ast->type == AST_CLASS_DECL || ast->type == AST_INTERFACE_DECL ||
        ast->type == AST_ENUM_DECL || ast->type == AST_RECORD_DECL) {
        
        symbol_t *sym = ast->sem_symbol;
        if (!sym) {
            return;
        }
        
        /* Skip if both superclass and superclass_type already resolved */
        if (sym->data.class_data.superclass && sym->data.class_data.superclass_type) {
            goto process_children;
        }
        
        /* Process extends and implements from children */
        slist_t *children = ast->data.node.children;
        while (children) {
            ast_node_t *child = (ast_node_t *)children->data;
            if (child->type == AST_CLASS_TYPE ||
                child->type == AST_PRIMITIVE_TYPE ||
                child->type == AST_ARRAY_TYPE) {
                if (child->data.node.flags == 1 && 
                    (!sym->data.class_data.superclass || !sym->data.class_data.superclass_type)) {
                    /* extends - set superclass and/or superclass_type */
                    /* Resolve the full parameterized type from this AST node */
                    type_t *super_type = semantic_resolve_type(sem, child);
                    
                    /* Get the base symbol from the type */
                    symbol_t *super_sym = NULL;
                    if (super_type && super_type->kind == TYPE_CLASS) {
                        super_sym = super_type->data.class_type.symbol;
                        if (!super_sym && super_type->data.class_type.name) {
                            super_sym = load_external_class(sem, super_type->data.class_type.name);
                            if (super_sym) {
                                super_type->data.class_type.symbol = super_sym;
                            }
                        }
                    }
                    
                    if (super_sym && (super_sym->kind == SYM_CLASS ||
                                      super_sym->kind == SYM_RECORD)) {
                        if (!sym->data.class_data.superclass) {
                        sym->data.class_data.superclass = super_sym;
                        }
                        if (!sym->data.class_data.superclass_type) {
                            sym->data.class_data.superclass_type = super_type;  /* Store parameterized type */
                        }
                    }
                } else if (child->data.node.flags == 2) {
                    /* implements/extends for interfaces - already handled in pass1 */
                }
            }
            children = children->next;
        }
    }
    
process_children:
    /* Recurse into children - use ast_get_children for safe traversal */
    {
        slist_t *children = ast_get_children(ast);
        for (slist_t *c = children; c; c = c->next) {
            ast_node_t *child = (ast_node_t *)c->data;
            if (child) {
                resolve_type_hierarchy(sem, child);
            }
        }
    }
}

static void pass2_check_types(semantic_t *sem, ast_node_t *ast)
{
    if (!ast) {
        return;
    }
    
    /* Stack for iterative traversal */
    int stack_capacity = 256;
    int stack_top = 0;
    walk_frame_t **stack = malloc(sizeof(walk_frame_t *) * stack_capacity);
    
    stack[stack_top++] = walk_frame_new(ast);
    
    /* Reset to global scope */
    sem->current_scope = sem->global_scope;
    sem->current_class = NULL;
    sem->current_method = NULL;
    
    while (stack_top > 0) {
        walk_frame_t *frame = stack[stack_top - 1];
        ast_node_t *node = frame->node;
        
        if (!node) {
            free(stack[--stack_top]);
            continue;
        }
        
        switch (frame->state) {
            case WALK_ENTER:
                switch (node->type) {
                    case AST_MODULE_DECL:
                    case AST_REQUIRES_DIRECTIVE:
                    case AST_EXPORTS_DIRECTIVE:
                    case AST_OPENS_DIRECTIVE:
                    case AST_USES_DIRECTIVE:
                    case AST_PROVIDES_DIRECTIVE:
                        /* Module declarations don't need type checking */
                        frame->state = WALK_EXIT;
                        continue;
                    
                    case AST_CLASS_DECL:
                    case AST_INTERFACE_DECL:
                    case AST_ENUM_DECL:
                    case AST_RECORD_DECL:
                    case AST_ANNOTATION_DECL:
                        {
                            const char *name = node->data.node.name;
                            symbol_t *sym = scope_lookup(sem->current_scope, name);
                            if (sym && (sym->kind == SYM_CLASS || sym->kind == SYM_INTERFACE ||
                                       sym->kind == SYM_ENUM || sym->kind == SYM_RECORD ||
                                       sym->kind == SYM_ANNOTATION)) {
                                frame->saved_scope = sem->current_scope;
                                frame->saved_class = sem->current_class;
                                
                                /* For local classes, update the member scope's parent to the
                                 * current pass 2 scope (which includes block scopes where
                                 * local variables are defined) */
                                if (sym->kind == SYM_CLASS &&
                                    sym->data.class_data.is_local_class && 
                                    sym->data.class_data.members) {
                                    sym->data.class_data.members->parent = sem->current_scope;
                                }
                                
                                sem->current_scope = sym->data.class_data.members;
                                sem->current_class = sym;
                            }
                        }
                        break;
                    
                    case AST_METHOD_DECL:
                    case AST_CONSTRUCTOR_DECL:
                        {
                            /* For overloaded methods, we can't just look up by name - 
                             * we need to find the symbol whose AST matches this node.
                             * Use sem_symbol if set (it should have been set in pass1). */
                            symbol_t *sym = node->sem_symbol;
                            
                            if (sym && sym->data.method_data.body_scope) {
                                frame->saved_scope = sem->current_scope;
                                frame->saved_method = sem->current_method;
                                sem->current_scope = sym->data.method_data.body_scope;
                                sem->current_method = sym;
                            }
                        }
                        break;
                    
                    case AST_FIELD_DECL:
                        {
                            /* Field initializer type checking - deferred from pass1
                             * to allow forward references to methods */
                            slist_t *children = node->data.node.children;
                            type_t *field_type = NULL;
                            
                            /* Track if we're in a static field initializer.
                             * This affects whether anonymous classes created here are static. */
                            bool saved_static_field_init = sem->in_static_field_init;
                            sem->in_static_field_init = (node->data.node.flags & MOD_STATIC) != 0;
                            
                            if (children && ((ast_node_t *)children->data)->type != AST_VAR_DECLARATOR) {
                                field_type = semantic_resolve_type(sem, children->data);
                                children = children->next;
                            }
                            
                            while (children) {
                                ast_node_t *decl = children->data;
                                if (decl->type == AST_VAR_DECLARATOR && decl->data.node.children) {
                                    ast_node_t *init_expr = (ast_node_t *)decl->data.node.children->data;
                                    
                                    /* Unwrap parentheses for lambda/method-ref */
                                    ast_node_t *unwrapped = init_expr;
                                    while (unwrapped->type == AST_PARENTHESIZED && 
                                           unwrapped->data.node.children) {
                                        unwrapped = (ast_node_t *)unwrapped->data.node.children->data;
                                    }
                                    
                                    /* Handle lambda/method reference with target typing */
                                    if (unwrapped->type == AST_LAMBDA_EXPR) {
                                        bind_lambda_to_target_type(sem, unwrapped, field_type);
                                    } else if (unwrapped->type == AST_METHOD_REF) {
                                        bind_method_ref_to_target_type(sem, unwrapped, field_type);
                                    } else if (unwrapped->type == AST_ARRAY_INIT && field_type) {
                                        /* Propagate target type to array initializer for proper 
                                         * element type inference (e.g., double[] x = {1, 2, 3}
                                         * should create a double[] not int[]) */
                                        unwrapped->sem_type = field_type;
                                        bind_array_init_elements(sem, unwrapped, field_type);
                                    }
                                    
                                    /* Set target type for type inference in method calls */
                                    type_t *saved_target = sem->target_type;
                                    sem->target_type = field_type;
                                    type_t *init_type = get_expression_type(sem, init_expr);
                                    sem->target_type = saved_target;
                                    
                                    if (init_type && !init_expr->sem_type) {
                                        init_expr->sem_type = init_type;
                                    }
                                    
                                    if (field_type && init_type && !type_assignable(field_type, init_type) &&
                                        !narrowing_constant_allowed(field_type, init_expr, init_type)) {
                                        char *expected = type_to_string(field_type);
                                        char *actual = type_to_string(init_type);
                                        semantic_error(sem, decl->line, decl->column,
                                            "Incompatible types: cannot convert %s to %s",
                                            actual, expected);
                                        free(expected);
                                        free(actual);
                                    }
                                }
                                children = children->next;
                            }
                            
                            sem->in_static_field_init = saved_static_field_init;
                        }
                        break;
                    
                    case AST_BLOCK:
                        {
                            /* Check if this is an anonymous class body (marked in get_expression_type) */
                            symbol_t *anon_sym = node->sem_symbol;
                            if (anon_sym && anon_sym->kind == SYM_CLASS && 
                                anon_sym->data.class_data.is_anonymous_class) {
                                /* This is an anonymous class body - set up class context */
                                frame->saved_scope = sem->current_scope;
                                frame->saved_class = sem->current_class;
                                
                                /* Update the anonymous class scope's parent to current scope
                                 * so that captured variables can be found */
                                if (anon_sym->data.class_data.members) {
                                    anon_sym->data.class_data.members->parent = sem->current_scope;
                                    sem->current_scope = anon_sym->data.class_data.members;
                                }
                                sem->current_class = anon_sym;
                            } else {
                                /* Regular block - create block scope */
                            scope_t *block_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                            frame->saved_scope = sem->current_scope;
                            sem->current_scope = block_scope;
                            frame->extra = block_scope;  /* Remember to free */
                            
                            /* Check for pending pattern variable from instanceof pattern matching.
                             * This is consumed only by the first block entered (the then block). */
                            if (sem->pending_pattern_var) {
                                scope_define(block_scope, sem->pending_pattern_var);
                                sem->pending_pattern_var = NULL;
                                }
                            }
                        }
                        break;
                    
                    case AST_CATCH_CLAUSE:
                        {
                            /* Catch clause: catch (ExceptionType varName) { body }
                             * Children: [0] exception type, [1] catch block
                             * name: exception variable name
                             * Create scope and declare exception variable */
                            scope_t *catch_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                            frame->saved_scope = sem->current_scope;
                            sem->current_scope = catch_scope;
                            frame->extra = catch_scope;
                            
                            slist_t *children = node->data.node.children;
                            const char *exc_var_name = node->data.node.name;
                            
                            if (children && exc_var_name) {
                                ast_node_t *type_node = (ast_node_t *)children->data;
                                type_t *exc_type = semantic_resolve_type(sem, type_node);
                                
                                symbol_t *sym = symbol_new(SYM_LOCAL_VAR, exc_var_name);
                                sym->type = exc_type;
                                sym->ast = node;
                                sym->line = node->line;
                                sym->column = node->column;
                                sym->data.var_data.initialized = true;  /* Exception is assigned by JVM */
                                scope_define(catch_scope, sym);
                            }
                        }
                        break;
                    
                    case AST_SWITCH_RULE:
                        {
                            /* Switch rule: case pattern -> body
                             * Check for type patterns and create scope if needed */
                            slist_t *children = node->data.node.children;
                            for (slist_t *c = children; c; c = c->next) {
                                ast_node_t *child = (ast_node_t *)c->data;
                                
                                /* Check for type pattern or guarded pattern */
                                ast_node_t *pattern = NULL;
                                ast_node_t *guard = NULL;
                                
                                if (child->type == AST_GUARDED_PATTERN) {
                                    slist_t *gp_children = child->data.node.children;
                                    if (gp_children) {
                                        pattern = (ast_node_t *)gp_children->data;
                                        if (gp_children->next) {
                                            guard = (ast_node_t *)gp_children->next->data;
                                        }
                                    }
                                } else if (child->type == AST_TYPE_PATTERN) {
                                    pattern = child;
                                } else if (child->type == AST_UNNAMED_PATTERN) {
                                    /* Unnamed pattern: matches anything, no variable binding */
                                    pattern = child;
                                } else if (child->type == AST_RECORD_PATTERN) {
                                    /* Record pattern: case RecordType(p1, p2) -> */
                                    pattern = child;
                                }
                                
                                if (pattern && (pattern->type == AST_TYPE_PATTERN || 
                                               pattern->type == AST_UNNAMED_PATTERN ||
                                               pattern->type == AST_RECORD_PATTERN)) {
                                    /* Create scope for pattern variable */
                                    scope_t *rule_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                                    frame->saved_scope = sem->current_scope;
                                    sem->current_scope = rule_scope;
                                    frame->extra = rule_scope;
                                    
                                    /* Process pattern and define variables */
                                    semantic_define_pattern_vars(sem, pattern, rule_scope);
                                    
                                    (void)guard;  /* Guard processed during traversal */
                                    break;  /* Only need one scope per rule */
                                }
                            }
                        }
                        break;
                    
                    case AST_ENHANCED_FOR_STMT:
                        {
                            /* Enhanced for loop: for (Type var : iterable) { body }
                             * Children: type, variable(identifier), iterable, body
                             * Create scope and declare loop variable */
                            scope_t *loop_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                            frame->saved_scope = sem->current_scope;
                            sem->current_scope = loop_scope;
                            frame->extra = loop_scope;
                            sem->loop_depth++;
                            
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                ast_node_t *type_node = (ast_node_t *)children->data;
                                ast_node_t *var_node = (ast_node_t *)children->next->data;
                                
                                type_t *var_type = semantic_resolve_type(sem, type_node);
                                const char *var_name = var_node->data.leaf.name;
                                
                                symbol_t *sym = symbol_new(SYM_LOCAL_VAR, var_name);
                                sym->type = var_type;
                                sym->ast = var_node;
                                sym->line = var_node->line;
                                sym->column = var_node->column;
                                
                                scope_define(loop_scope, sym);
                                
                                /* Get iterable expression type for codegen */
                                if (children->next->next) {
                                    ast_node_t *iterable = (ast_node_t *)children->next->next->data;
                                    type_t *iter_type = get_expression_type(sem, iterable);
                                    iterable->sem_type = iter_type;
                                    
                                    /* Validate: iterable must be array or implement Iterable */
                                    if (iter_type && iter_type->kind != TYPE_UNKNOWN) {
                                        if (!type_is_iterable(sem, iter_type)) {
                                            char *type_str = type_to_string(iter_type);
                                            semantic_error(sem, iterable->line, iterable->column,
                                                "enhanced for loop requires array or Iterable, got %s",
                                                type_str);
                                            free(type_str);
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_VAR_DECL:
                        {
                            /* Local variable declaration */
                            slist_t *children = node->data.node.children;
                            type_t *var_type = NULL;
                            bool is_var_inference = false;
                            ast_node_t *type_node = NULL;
                            
                            if (children) {
                                ast_node_t *first = children->data;
                                if (first->type != AST_VAR_DECLARATOR) {
                                    type_node = first;
                                    /* Check for 'var' type inference (Java 10+) */
                                    if (first->type == AST_VAR_TYPE) {
                                        is_var_inference = true;
                                        /* var_type will be inferred from initializer */
                                    } else if (first->type == AST_CLASS_TYPE &&
                                               first->data.node.name &&
                                               strcmp(first->data.node.name, "var") == 0) {
                                        /* Legacy check for var parsed as identifier */
                                        is_var_inference = true;
                                    } else {
                                        var_type = semantic_resolve_type(sem, first);
                                    }
                                    children = children->next;
                                }
                            }
                            
                            while (children) {
                                ast_node_t *decl = children->data;
                                if (decl->type == AST_VAR_DECLARATOR) {
                                    const char *name = decl->data.node.name;
                                    type_t *actual_type = var_type;
                                    
                                    /* Handle C-style array dimensions (e.g., int x[][]) */
                                    int extra_dims = decl->data.node.flags;
                                    if (extra_dims > 0 && actual_type) {
                                        for (int i = 0; i < extra_dims; i++) {
                                            actual_type = type_new_array(actual_type, 1);
                                        }
                                    }
                                    
                                    /* For 'var', infer type from initializer */
                                    if (is_var_inference) {
                                        if (!decl->data.node.children) {
                                            semantic_error(sem, decl->line, decl->column,
                                                "'var' declaration requires initializer");
                                        } else {
                                            ast_node_t *init_expr = decl->data.node.children->data;
                                            actual_type = get_expression_type(sem, init_expr);
                                            
                                            /* Check for invalid var inference cases */
                                            if (actual_type->kind == TYPE_NULL) {
                                                semantic_error(sem, decl->line, decl->column,
                                                    "Cannot infer type for 'var' from 'null'");
                                            } else if (actual_type->kind == TYPE_VOID) {
                                                semantic_error(sem, decl->line, decl->column,
                                                    "Cannot infer type for 'var' from void");
                                            } else if (actual_type->kind == TYPE_UNKNOWN) {
                                                semantic_error(sem, decl->line, decl->column,
                                                    "Cannot infer type for 'var'");
                                            }
                                            
                                            /* Store inferred type on type node for codegen */
                                            if (type_node) {
                                                type_node->sem_type = actual_type;
                                            }
                                        }
                                    }
                                    
                                    symbol_t *sym = symbol_new(SYM_LOCAL_VAR, name);
                                    sym->type = actual_type;
                                    sym->ast = decl;
                                    sym->line = decl->line;
                                    sym->column = decl->column;
                                    
                                    if (!scope_define(sem->current_scope, sym)) {
                                        semantic_error(sem, decl->line, decl->column,
                                                      "Variable '%s' already defined", name);
                                    }
                                    
                                    /* Check initializer type (for non-var declarations) */
                                    if (!is_var_inference && decl->data.node.children) {
                                        ast_node_t *init_expr = decl->data.node.children->data;
                                        
                                        /* For array initializers, set the target type and bind lambda elements */
                                        if (init_expr->type == AST_ARRAY_INIT && actual_type) {
                                            init_expr->sem_type = actual_type;
                                            /* Recursively bind lambda/method ref elements to element type */
                                            if (actual_type->kind == TYPE_ARRAY) {
                                                bind_array_init_elements(sem, init_expr, actual_type);
                                            }
                                        }
                                        
                                        /* Unwrap parentheses for lambda/method-ref */
                                        ast_node_t *unwrapped = init_expr;
                                        while (unwrapped->type == AST_PARENTHESIZED && 
                                               unwrapped->data.node.children) {
                                            unwrapped = (ast_node_t *)unwrapped->data.node.children->data;
                                        }
                                        
                                        /* Handle lambda/method reference with target typing */
                                        if (unwrapped->type == AST_LAMBDA_EXPR) {
                                            if (bind_lambda_to_target_type(sem, unwrapped, actual_type)) {
                                                /* Lambda bound successfully, skip normal type check */
                                                children = children->next;
                                                continue;
                                            }
                                        } else if (unwrapped->type == AST_METHOD_REF) {
                                            if (bind_method_ref_to_target_type(sem, unwrapped, actual_type)) {
                                                /* Method ref bound successfully, skip normal type check */
                                                children = children->next;
                                                continue;
                                            }
                                        }
                                        
                                        /* Set target type for type inference in method calls */
                                        type_t *saved_target = sem->target_type;
                                        sem->target_type = actual_type;
                                        type_t *init_type = get_expression_type(sem, init_expr);
                                        sem->target_type = saved_target;
                                        
                                        /* Ensure symbols loaded for proper subtype checking */
                                        ensure_type_symbol_loaded(sem, init_type);
                                        if (actual_type && !type_assignable(actual_type, init_type) &&
                                            !narrowing_constant_allowed(actual_type, init_expr, init_type)) {
                                            char *expected = type_to_string(actual_type);
                                            char *actual = type_to_string(init_type);
                                            semantic_error(sem, decl->line, decl->column,
                                                "Incompatible types: cannot convert %s to %s",
                                                actual, expected);
                                            free(expected);
                                            free(actual);
                                        }
                                    }
                                }
                                children = children->next;
                            }
                        }
                        break;
                    
                    case AST_RESOURCE_SPEC:
                        {
                            /* Resource specification in try-with-resources
                             * Two forms:
                             * 1. Declaration: Children: [0] type node, [1] initializer
                             *    Name: variable name, flags bit 1 = 0
                             * 2. Reference (Java 9+): Children: [0] identifier/expr
                             *    Name: NULL, flags bit 1 = 1
                             */
                            bool is_reference = (node->data.node.flags & 2) != 0;
                            slist_t *children = node->data.node.children;
                            
                            if (is_reference) {
                                /* Existing variable reference */
                                if (children) {
                                    ast_node_t *var_expr = (ast_node_t *)children->data;
                                    
                                    /* Look up the variable */
                                    if (var_expr->type == AST_IDENTIFIER) {
                                        const char *var_name = var_expr->data.leaf.name;
                                        symbol_t *var_sym = scope_lookup(sem->current_scope, var_name);
                                        
                                        if (!var_sym) {
                                            semantic_error(sem, node->line, node->column,
                                                          "Cannot resolve symbol: %s", var_name);
                                        } else {
                                            /* Store the resolved type on the node */
                                            node->sem_type = var_sym->type;
                                            var_expr->sem_symbol = var_sym;
                                            
                                            /* Check if effectively final */
                                            /* Note: Full effectively-final analysis would require
                                             * tracking all assignments, but for now we accept
                                             * explicitly final or unassigned-after-init variables */
                                            if (!(var_sym->modifiers & MOD_FINAL)) {
                                                /* TODO: Check effectively final */
                                            }
                                            
                                            /* TODO: Check that type implements AutoCloseable */
                                        }
                                    } else if (var_expr->type == AST_FIELD_ACCESS) {
                                        /* Field access like this.reader or obj.stream */
                                        type_t *expr_type = get_expression_type(sem, var_expr);
                                        node->sem_type = expr_type;
                                        /* TODO: Check effectively final for fields */
                                        /* TODO: Check that type implements AutoCloseable */
                                    } else {
                                        /* Other expression types */
                                        type_t *expr_type = get_expression_type(sem, var_expr);
                                        node->sem_type = expr_type;
                                    }
                                }
                            } else {
                                /* Declaration form */
                                if (children) {
                                    ast_node_t *type_node = (ast_node_t *)children->data;
                                    type_t *res_type = semantic_resolve_type(sem, type_node);
                                    const char *name = node->data.node.name;
                                    
                                    node->sem_type = res_type;
                                    
                                    if (name) {
                                        symbol_t *sym = symbol_new(SYM_LOCAL_VAR, name);
                                        sym->type = res_type;
                                        sym->ast = node;
                                        sym->line = node->line;
                                        sym->column = node->column;
                                        sym->modifiers = MOD_FINAL;  /* Resources are implicitly final */
                                        
                                        if (!scope_define(sem->current_scope, sym)) {
                                            semantic_error(sem, node->line, node->column,
                                                          "Variable '%s' already defined", name);
                                        }
                                    }
                                    
                                    /* Process the initializer expression to ensure its types are resolved */
                                    if (children->next) {
                                        ast_node_t *init_expr = (ast_node_t *)children->next->data;
                                        get_expression_type(sem, init_expr);
                                    }
                                    
                                    /* TODO: Check that resource type implements AutoCloseable */
                                }
                            }
                        }
                        break;
                    
                    case AST_RETURN_STMT:
                        {
                            /* Check return type matches method */
                            if (sem->current_method) {
                                type_t *expected = sem->current_method->type;
                                /* Constructors are effectively void for return purposes */
                                bool is_constructor = (sem->current_method->kind == SYM_CONSTRUCTOR);
                                bool is_void_method = is_constructor || 
                                                      (expected && type_equals(expected, type_void()));
                                
                                slist_t *children = node->data.node.children;
                                if (children) {
                                    /* Bind lambda/method-ref to expected return type before evaluating */
                                    ast_node_t *ret_expr = (ast_node_t *)children->data;
                                    if (!is_void_method && expected && expected->kind == TYPE_CLASS) {
                                        if (ret_expr->type == AST_LAMBDA_EXPR) {
                                            bind_lambda_to_target_type(sem, ret_expr, expected);
                                        } else if (ret_expr->type == AST_METHOD_REF) {
                                            bind_method_ref_to_target_type(sem, ret_expr, expected);
                                        }
                                    }
                                    
                                    /* Return with value */
                                    type_t *actual = get_expression_type(sem, ret_expr);
                                    
                                    if (is_void_method) {
                                        /* Cannot return a value from void method */
                                        semantic_error(sem, node->line, node->column,
                                            "cannot return a value from method whose result type is void");
                                    } else if (expected && !type_assignable(expected, actual) &&
                                               !narrowing_constant_allowed(expected, ret_expr, actual)) {
                                        char *exp_str = type_to_string(expected);
                                        char *act_str = type_to_string(actual);
                                        semantic_error(sem, node->line, node->column,
                                            "Incompatible return type: expected %s, got %s",
                                            exp_str, act_str);
                                        free(exp_str);
                                        free(act_str);
                                    }
                                } else if (!is_void_method && expected) {
                                    /* Return without value in non-void method */
                                    semantic_error(sem, node->line, node->column,
                                        "Missing return value");
                                }
                            }
                        }
                        break;
                    
                    case AST_CONTINUE_STMT:
                        {
                            /* Continue must be inside a loop */
                            if (sem->loop_depth == 0) {
                                semantic_error(sem, node->line, node->column,
                                    "'continue' outside of loop");
                            }
                        }
                        break;
                    
                    case AST_BREAK_STMT:
                        {
                            /* Break must be inside a loop or switch */
                            if (sem->loop_depth == 0 && sem->switch_depth == 0) {
                                semantic_error(sem, node->line, node->column,
                                    "'break' outside switch or loop");
                            }
                        }
                        break;
                    
                    case AST_EXPR_STMT:
                        {
                            /* Check that the expression is a valid statement expression */
                            /* Valid: assignment, pre/post increment/decrement, method call, new object, ctor call */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *expr = (ast_node_t *)children->data;
                                if (expr) {
                                    bool valid_stmt = false;
                                    switch (expr->type) {
                    case AST_ASSIGNMENT_EXPR:
                                        case AST_METHOD_CALL:
                                        case AST_NEW_OBJECT:
                                        case AST_EXPLICIT_CTOR_CALL:  /* this() or super() */
                                            valid_stmt = true;
                                            break;
                                        case AST_UNARY_EXPR:
                                            {
                                                token_type_t op = expr->data.node.op_token;
                                                if (op == TOK_INC || op == TOK_DEC) {
                                                    valid_stmt = true;
                                                }
                                            }
                                            break;
                                        default:
                                            break;
                                    }
                                    if (!valid_stmt) {
                                        semantic_error(sem, node->line, node->column,
                                            "not a statement");
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_EXPLICIT_CTOR_CALL:
                        {
                            /* this() or super() can only be called from a constructor */
                            if (!sem->current_method || sem->current_method->kind != SYM_CONSTRUCTOR) {
                                semantic_error(sem, node->line, node->column,
                                    "call to super must be first statement in constructor");
                            }
                            
                            /* Type-check arguments and bind lambdas to target types */
                            get_expression_type(sem, node);
                        }
                        break;
                    
                    case AST_INITIALIZER_BLOCK:
                        {
                            /* Instance initializers not allowed in interfaces */
                            if (sem->current_class && sem->current_class->kind == SYM_INTERFACE &&
                                !(node->data.node.flags & MOD_STATIC)) {
                                semantic_error(sem, node->line, node->column,
                                    "initializers not permitted in interfaces");
                            }
                        }
                        break;
                    
                    case AST_TRY_STMT:
                        {
                            /* Try must have catch, finally, or resources */
                            slist_t *children = node->data.node.children;
                            bool has_catch = false;
                            bool has_finally = false;
                            bool has_resources = false;
                            
                            /* Check children for catch, finally, and resource specs */
                            for (slist_t *c = children; c; c = c->next) {
                                ast_node_t *child = (ast_node_t *)c->data;
                                if (child) {
                                    if (child->type == AST_CATCH_CLAUSE) {
                                        has_catch = true;
                                    } else if (child->type == AST_RESOURCE_SPEC) {
                                        has_resources = true;
                                    } else if (child->type == AST_FINALLY_CLAUSE) {
                                        has_finally = true;
                                    }
                                }
                            }
                            
                            if (!has_catch && !has_finally && !has_resources) {
                                semantic_error(sem, node->line, node->column,
                                    "'try' without 'catch', 'finally' or resource declarations");
                            }
                        }
                        break;
                    
                    case AST_UNARY_EXPR:
                        {
                            /* Check for increment/decrement on final variables */
                            token_type_t op = node->data.node.op_token;
                            if (op == TOK_INC || op == TOK_DEC) {
                                slist_t *children = node->data.node.children;
                                if (children) {
                                    ast_node_t *operand = (ast_node_t *)children->data;
                                    if (operand && operand->type == AST_IDENTIFIER) {
                                        const char *name = operand->data.leaf.name;
                                        symbol_t *sym = scope_lookup(sem->current_scope, name);
                                        if (sym && (sym->modifiers & MOD_FINAL)) {
                                            if (sym->kind == SYM_LOCAL_VAR) {
                                                semantic_error(sem, node->line, node->column,
                                                    "cannot assign a value to final variable '%s'", name);
                                            } else if (sym->kind == SYM_PARAMETER) {
                                                semantic_error(sem, node->line, node->column,
                                                    "final parameter '%s' may not be assigned", name);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_ASSIGNMENT_EXPR:
                        {
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                ast_node_t *left = (ast_node_t *)children->data;
                                
                                /* Cannot assign to 'this' */
                                if (left->type == AST_THIS_EXPR) {
                                    semantic_error(sem, node->line, node->column,
                                        "cannot assign a value to 'this'");
                                }
                                
                                /* Check for final variable assignment */
                                if (left->type == AST_IDENTIFIER) {
                                    const char *name = left->data.leaf.name;
                                    symbol_t *sym = scope_lookup(sem->current_scope, name);
                                    if (sym && (sym->modifiers & MOD_FINAL)) {
                                        /* Final variable - check if this is initialization */
                                        if (sym->kind == SYM_LOCAL_VAR) {
                                            /* TODO: Track definite assignment for smarter checking */
                                            semantic_error(sem, node->line, node->column,
                                                "cannot assign a value to final variable '%s'", name);
                                        } else if (sym->kind == SYM_PARAMETER) {
                                            semantic_error(sem, node->line, node->column,
                                                "final parameter '%s' may not be assigned", name);
                                        } else if (sym->kind == SYM_FIELD) {
                                            /* Final field - can only assign in constructor/initializer */
                                            bool in_constructor = sem->current_method && 
                                                sem->current_method->kind == SYM_CONSTRUCTOR;
                                            bool is_static_field = (sym->modifiers & MOD_STATIC) != 0;
                                            
                                            /* Static final fields can only be assigned in static initializer
                                               Instance final fields can only be assigned in constructor */
                                            if (is_static_field) {
                                                /* Static final - only in <clinit> (no current_method) */
                                                if (sem->current_method) {
                                                    semantic_error(sem, node->line, node->column,
                                                        "Cannot assign to static final field '%s'", name);
                                                }
                                            } else {
                                                /* Instance final - only in constructor or instance initializer */
                                                if (!in_constructor && sem->current_method) {
                                                    semantic_error(sem, node->line, node->column,
                                                        "Cannot assign to final field '%s'", name);
                                                }
                                            }
                                        }
                                    }
                                } else if (left->type == AST_FIELD_ACCESS) {
                                    /* Field access: obj.field or this.field */
                                    const char *field_name = left->data.node.name;
                                    slist_t *fa_children = left->data.node.children;
                                    if (fa_children) {
                                        ast_node_t *obj = (ast_node_t *)fa_children->data;
                                        type_t *obj_type = get_expression_type(sem, obj);
                                        if (obj_type && obj_type->kind == TYPE_CLASS &&
                                            obj_type->data.class_type.symbol) {
                                            symbol_t *class_sym = obj_type->data.class_type.symbol;
                                            if (class_sym->data.class_data.members) {
                                                symbol_t *field = scope_lookup_local(
                                                    class_sym->data.class_data.members, field_name);
                                                if (field && (field->modifiers & MOD_FINAL)) {
                                                    /* Check context - same rules as above */
                                                    bool in_constructor = sem->current_method && 
                                                        sem->current_method->kind == SYM_CONSTRUCTOR;
                                                    bool is_static_field = (field->modifiers & MOD_STATIC) != 0;
                                                    bool is_same_class = (class_sym == sem->current_class);
                                                    
                                                    if (is_static_field) {
                                                        if (sem->current_method) {
                                                            semantic_error(sem, node->line, node->column,
                                                                "Cannot assign to static final field '%s'", field_name);
                                                        }
                                                    } else if (is_same_class) {
                                                        if (!in_constructor && sem->current_method) {
                                                            semantic_error(sem, node->line, node->column,
                                                                "Cannot assign to final field '%s'", field_name);
                                                        }
                                                    } else {
                                                        /* Assigning to final field of another object */
                                                        semantic_error(sem, node->line, node->column,
                                                            "Cannot assign to final field '%s' of class '%s'",
                                                            field_name, class_sym->name);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                
                                type_t *left_type = get_expression_type(sem, left);
                                ast_node_t *right = (ast_node_t *)children->next->data;
                                
                                /* Handle lambda/method reference with target typing */
                                if (right->type == AST_LAMBDA_EXPR) {
                                    if (bind_lambda_to_target_type(sem, right, left_type)) {
                                        /* Lambda bound successfully, skip normal type check */
                                        break;
                                    }
                                } else if (right->type == AST_METHOD_REF) {
                                    if (bind_method_ref_to_target_type(sem, right, left_type)) {
                                        /* Method ref bound successfully, skip normal type check */
                                        break;
                                    }
                                }
                                
                                type_t *right_type = get_expression_type(sem, right);
                                
                                /* Ensure symbols loaded for proper subtype checking */
                                ensure_type_symbol_loaded(sem, right_type);
                                
                                /* Compound assignment operators (+=, -=, |=, etc.) include
                                 * an implicit cast, so b |= 0x80 is valid even though the
                                 * RHS operation produces int. Only check types for simple
                                 * assignment (TOK_ASSIGN). */
                                token_type_t op = node->data.node.op_token;
                                bool is_compound = (op != TOK_ASSIGN && op != 0);
                                
                                if (!is_compound &&
                                    !type_assignable(left_type, right_type) &&
                                    !narrowing_constant_allowed(left_type, right, right_type)) {
                                    char *left_str = type_to_string(left_type);
                                    char *right_str = type_to_string(right_type);
                                    semantic_error(sem, node->line, node->column,
                                        "Incompatible types: cannot convert %s to %s",
                                        right_str, left_str);
                                    free(left_str);
                                    free(right_str);
                                }
                            }
                        }
                        break;
                    
                    case AST_LAMBDA_EXPR:
                        {
                            /* Lambda expression - set up lambda scope if target type is known.
                             * If sem_type isn't set yet (for nested lambdas in block bodies),
                             * we may need to call get_expression_type on the parent method call. */
                            if (node->sem_type && node->sem_type->kind == TYPE_CLASS) {
                                /* Create a lambda scope using the type's interface symbol */
                                scope_t *lambda_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                                frame->saved_scope = sem->current_scope;
                                frame->extra = lambda_scope;  /* Remember to free */
                                sem->current_scope = lambda_scope;
                                
                                /* Save current_method and set to SAM so return type checks work */
                                frame->saved_method = sem->current_method;
                                if (node->sem_symbol) {
                                    /* sem_symbol is the SAM method from bind_lambda_to_target_type */
                                    sem->current_method = node->sem_symbol;
                                }
                                
                                /* Define lambda parameters from the first child (if AST_IDENTIFIER) */
                                slist_t *lambda_children = node->data.node.children;
                                if (lambda_children) {
                                    ast_node_t *params = (ast_node_t *)lambda_children->data;
                                    if (params && params->type == AST_IDENTIFIER) {
                                        /* Single identifier parameter - define it in scope */
                                        symbol_t *param_sym = symbol_new(SYM_PARAMETER, params->data.leaf.name);
                                        param_sym->type = params->sem_type ? params->sem_type : type_new_primitive(TYPE_UNKNOWN);
                                        param_sym->line = params->line;
                                        param_sym->column = params->column;
                                        scope_define(lambda_scope, param_sym);
                                        params->sem_symbol = param_sym;
                                    } else if (params && params->type == AST_LAMBDA_PARAMS) {
                                        /* Multiple parameters - define each one */
                                        for (slist_t *p = params->data.node.children; p; p = p->next) {
                                            ast_node_t *param_node = (ast_node_t *)p->data;
                                            if (param_node && param_node->type == AST_PARAMETER) {
                                                const char *pname = param_node->data.node.name;
                                                if (pname) {
                                                    symbol_t *param_sym = symbol_new(SYM_PARAMETER, pname);
                                                    param_sym->type = param_node->sem_type ? param_node->sem_type : type_new_primitive(TYPE_UNKNOWN);
                                                    param_sym->line = param_node->line;
                                                    param_sym->column = param_node->column;
                                                    scope_define(lambda_scope, param_sym);
                                                    param_node->sem_symbol = param_sym;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_METHOD_REF:
                        {
                            /* Method ref already handled in assignment/decl context.
                             * Skip children - there's nothing to analyze. */
                            if (node->sem_type) {
                                frame->state = WALK_EXIT;  /* Skip children */
                                frame->child_iter = NULL;
                            }
                        }
                        break;
                    
                    case AST_IDENTIFIER:
                        {
                            /* If sem_type already set (e.g., enum switch case or lambda param), skip check */
                            if (node->sem_type) {
                                break;
                            }
                            
                            /* Check that identifier resolves */
                            const char *name = node->data.leaf.name;
                            symbol_t *sym = scope_lookup(sem->current_scope, name);
                            if (sym) {
                                /* Set sem_type for code generation */
                                node->sem_type = sym->type;
                            } else {
                                /* Check static imports for static field */
                                symbol_t *field_sym = NULL;
                                symbol_t *import_class = resolve_static_import_field(sem, name, &field_sym);
                                if (import_class && field_sym) {
                                    /* Resolved via static import - set sem_type */
                                    node->sem_type = field_sym->type;
                                    node->sem_symbol = field_sym;
                                    break;
                                }
                                
                                /* Check if it's an inherited field from superclass */
                                if (sem->current_class) {
                                    symbol_t *search_class = sem->current_class->data.class_data.superclass;
                                    while (search_class) {
                                        if (search_class->data.class_data.members) {
                                            symbol_t *field = scope_lookup_local(
                                                search_class->data.class_data.members, name);
                                            if (field && field->kind == SYM_FIELD) {
                                                /* Found inherited field */
                                                node->sem_symbol = field;
                                                node->sem_type = field->type;
                                                break;
                                            }
                                        }
                                        search_class = search_class->data.class_data.superclass;
                                    }
                                    if (node->sem_type) {
                                        break;  /* Successfully resolved inherited field */
                                    }
                                }
                                
                                /* Could be a class name - check types */
                                type_t *type = hashtable_lookup(sem->types, name);
                                if (type) {
                                    node->sem_type = type;
                                } else {
                                    /* First try resolving through imports */
                                    char *qualified = resolve_import(sem, name);
                                    if (qualified) {
                                        sym = load_external_class(sem, qualified);
                                        if (sym) {
                                            node->sem_type = sym->type;
                                            node->sem_symbol = sym;
                                            free(qualified);
                                            break;
                                        }
                                        free(qualified);
                                    }
                                    
                                    /* Try loading from classpath directly */
                                    sym = load_external_class(sem, name);
                                    if (!sym) {
                                        /* Try java.lang prefix */
                                        char java_lang[256];
                                        snprintf(java_lang, sizeof(java_lang), 
                                                 "java.lang.%s", name);
                                        sym = load_external_class(sem, java_lang);
                                    }
                                    if (sym) {
                                        node->sem_type = sym->type;
                                        node->sem_symbol = sym;
                                    } else {
                                        semantic_error(sem, node->line, node->column,
                                                      "Cannot resolve symbol: %s", name);
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_FIELD_ACCESS:
                        {
                            /* Check field access and set sem_type for codegen */
                            type_t *field_type = get_expression_type(sem, node);
                            if (field_type && !node->sem_type) {
                                node->sem_type = field_type;
                            }
                        }
                        break;
                    
                    case AST_METHOD_CALL:
                        {
                            /* Check method call and set sem_type for codegen.
                             * Skip if already processed (e.g., field initializer with target type) */
                            if (!node->sem_type) {
                            type_t *return_type = get_expression_type(sem, node);
                                if (return_type) {
                                node->sem_type = return_type;
                                }
                            }
                        }
                        break;
                    
                    case AST_NEW_OBJECT:
                        {
                            /* Process new object expression to resolve type and constructor.
                             * This ensures the type node's sem_type is set for codegen.
                             * Always call get_expression_type to set up anonymous class
                             * sem_symbol on the body block before walking into children. */
                            type_t *obj_type = get_expression_type(sem, node);
                            if (obj_type && !node->sem_type) {
                                node->sem_type = obj_type;
                            }
                        }
                        break;
                    
                    case AST_IF_STMT:
                        {
                            /* Check that if condition is boolean */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *condition = (ast_node_t *)children->data;
                                type_t *cond_type = get_expression_type(sem, condition);
                                /* Allow boolean primitive or java.lang.Boolean (auto-unboxing) */
                                bool is_boolean = (cond_type && cond_type->kind == TYPE_BOOLEAN);
                                if (!is_boolean && cond_type && cond_type->kind == TYPE_CLASS &&
                                    cond_type->data.class_type.name &&
                                    strcmp(cond_type->data.class_type.name, "java.lang.Boolean") == 0) {
                                    is_boolean = true;
                                }
                                if (cond_type && !is_boolean) {
                                    char *type_str = type_to_string(cond_type);
                                    semantic_error(sem, condition->line, condition->column,
                                        "if condition must be boolean, got %s", type_str);
                                    free(type_str);
                                }
                                
                                /* Warn about constant condition */
                                if (condition->type == AST_LITERAL && condition->data.leaf.name &&
                                    (strcmp(condition->data.leaf.name, "true") == 0 ||
                                     strcmp(condition->data.leaf.name, "false") == 0)) {
                                    semantic_warning(sem, condition->line, condition->column,
                                        "condition is always %s", condition->data.leaf.name);
                                }
                                
                                /* Handle pattern matching for instanceof (Java 16+)
                                 * If condition is: obj instanceof Type patternVar
                                 * Then patternVar should be in scope for the then block */
                                if (condition->type == AST_INSTANCEOF_EXPR) {
                                    slist_t *inst_children = condition->data.node.children;
                                    if (inst_children && inst_children->next && inst_children->next->next) {
                                        ast_node_t *type_node = (ast_node_t *)inst_children->next->data;
                                        ast_node_t *pattern_var = (ast_node_t *)inst_children->next->next->data;
                                        
                                        if (pattern_var && pattern_var->type == AST_IDENTIFIER) {
                                            /* Pattern matching in instanceof requires Java 16+ */
                                            if (sem->source_version < 16) {
                                                semantic_error(sem, condition->line, condition->column,
                                                    "pattern matching for instanceof is not supported in -source %d (use 16 or higher)",
                                                    sem->source_version);
                                            }
                                            /* Resolve the pattern type */
                                            type_t *pattern_type = semantic_resolve_type(sem, type_node);
                                            
                                            /* Create symbol for the pattern variable */
                                            symbol_t *sym = symbol_new(SYM_LOCAL_VAR, pattern_var->data.leaf.name);
                                            sym->type = pattern_type;
                                            sym->ast = pattern_var;
                                            sym->line = pattern_var->line;
                                            sym->column = pattern_var->column;
                                            
                                            /* Store the symbol on the pattern variable AST node */
                                            pattern_var->sem_symbol = sym;
                                            pattern_var->sem_type = pattern_type;
                                            
                                            /* Store in condition for codegen to use */
                                            condition->sem_symbol = sym;
                                            
                                            /* Set pending pattern var to be added to next block scope */
                                            sem->pending_pattern_var = sym;
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_WHILE_STMT:
                        {
                            /* Check that while condition is boolean */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *condition = (ast_node_t *)children->data;
                                type_t *cond_type = get_expression_type(sem, condition);
                                if (cond_type && cond_type->kind != TYPE_BOOLEAN) {
                                    char *type_str = type_to_string(cond_type);
                                    semantic_error(sem, condition->line, condition->column,
                                        "while condition must be boolean, got %s", type_str);
                                    free(type_str);
                                }
                                
                                /* Warn about constant false condition (dead code) */
                                if (condition->type == AST_LITERAL && condition->data.leaf.name &&
                                    strcmp(condition->data.leaf.name, "false") == 0) {
                                    semantic_warning(sem, condition->line, condition->column,
                                        "loop body is never executed");
                                }
                            }
                            sem->loop_depth++;
                        }
                        break;
                    
                    case AST_DO_STMT:
                        {
                            /* Check that do-while condition is boolean */
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                /* do-while: children are [body, condition] */
                                ast_node_t *condition = (ast_node_t *)children->next->data;
                                type_t *cond_type = get_expression_type(sem, condition);
                                if (cond_type && cond_type->kind != TYPE_BOOLEAN) {
                                    char *type_str = type_to_string(cond_type);
                                    semantic_error(sem, condition->line, condition->column,
                                        "do-while condition must be boolean, got %s", type_str);
                                    free(type_str);
                                }
                            }
                            sem->loop_depth++;
                        }
                        break;
                    
                    case AST_FOR_STMT:
                        {
                            /* For loop: for (init; condition; update) body
                             * Children: [init, condition, update, body]
                             * Create a scope for loop variables declared in init.
                             * Condition check is done in WALK_EXIT after init is processed. */
                            scope_t *for_scope = scope_new(SCOPE_FOR, sem->current_scope);
                            frame->saved_scope = sem->current_scope;
                            sem->current_scope = for_scope;
                            frame->extra = for_scope;
                            sem->loop_depth++;
                        }
                        break;
                    
                    case AST_SWITCH_STMT:
                        {
                            /* Track switch depth for break validation */
                            sem->switch_depth++;
                            
                            /* Create a new scope for the switch statement body.
                             * Each switch statement (including nested ones) gets its own scope
                             * so variables declared in different switch blocks don't conflict. */
                            scope_t *switch_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                            frame->saved_scope = sem->current_scope;
                            sem->current_scope = switch_scope;
                            frame->extra = switch_scope;
                            
                            /* Switch statement: check if selector is enum type
                             * If so, resolve case label identifiers as enum constants.
                             * Also check for duplicate case labels. */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *selector = (ast_node_t *)children->data;
                                type_t *sel_type = get_expression_type(sem, selector);
                                
                                /* Track seen case values for duplicate detection */
                                hashtable_t *seen_cases = hashtable_new();
                                bool seen_default = false;
                                
                                /* Check if selector is an enum type */
                                /* If the type doesn't have a symbol yet, try to load it */
                                if (sel_type && sel_type->kind == TYPE_CLASS &&
                                    !sel_type->data.class_type.symbol &&
                                    sel_type->data.class_type.name) {
                                    symbol_t *sym = load_external_class(sem, sel_type->data.class_type.name);
                                    if (sym) {
                                        sel_type->data.class_type.symbol = sym;
                                    }
                                }
                                
                                if (sel_type && sel_type->kind == TYPE_CLASS &&
                                    sel_type->data.class_type.symbol &&
                                    sel_type->data.class_type.symbol->kind == SYM_ENUM) {
                                    symbol_t *enum_sym = sel_type->data.class_type.symbol;
                                    
                                    /* Store enum symbol on switch node for codegen */
                                    node->sem_type = sel_type;
                                    
                                    /* Process case labels */
                                    for (slist_t *cnode = children->next; cnode; cnode = cnode->next) {
                                        ast_node_t *case_label = (ast_node_t *)cnode->data;
                                        if (case_label->type == AST_CASE_LABEL) {
                                            /* Skip default */
                                            if (case_label->data.node.name &&
                                                strcmp(case_label->data.node.name, "default") == 0) {
                                                continue;
                                            }
                                            
                                            /* Get case expression (first child) */
                                            slist_t *case_children = case_label->data.node.children;
                                            if (case_children) {
                                                ast_node_t *case_expr = (ast_node_t *)case_children->data;
                                                
                                                /* If case expression is identifier, resolve as enum constant */
                                                if (case_expr->type == AST_IDENTIFIER) {
                                                    const char *const_name = case_expr->data.leaf.name;
                                                    
                                                    /* Look up enum constant in the enum's member scope first */
                                                    bool found = false;
                                                    int ordinal = -1;
                                                    
                                                    if (enum_sym->data.class_data.members) {
                                                        symbol_t *const_sym = scope_lookup(enum_sym->data.class_data.members, const_name);
                                                        if (const_sym && const_sym->kind == SYM_FIELD &&
                                                            const_sym->data.var_data.is_enum_constant) {
                                                            /* Found in symbol table - get ordinal from symbol */
                                                            ordinal = const_sym->data.var_data.enum_ordinal;
                                                            found = true;
                                                        }
                                                    }
                                                    
                                                    /* Fallback: try looking through AST if symbol table didn't work */
                                                    if (!found) {
                                                    ast_node_t *enum_decl = enum_sym->ast;
                                                    if (enum_decl) {
                                                            ordinal = 0;
                                                        slist_t *enum_children = ast_get_children(enum_decl);
                                                        for (slist_t *ec = enum_children; ec; ec = ec->next) {
                                                            ast_node_t *enum_member = (ast_node_t *)ec->data;
                                                            if (enum_member->type == AST_ENUM_CONSTANT) {
                                                                const char *member_name = enum_member->data.node.name;
                                                                if (member_name && strcmp(member_name, const_name) == 0) {
                                                                    found = true;
                                                                    break;
                                                                }
                                                                ordinal++;
                                                            }
                                                        }
                                                    }
                                                    }
                                                    
                                                    if (found) {
                                                        /* Store ordinal on case expression */
                                                        case_expr->sem_type = sel_type;
                                                        case_expr->data.leaf.value.int_val = ordinal;
                                                    } else {
                                                        semantic_error(sem, case_expr->line, case_expr->column,
                                                            "Cannot resolve symbol: %s", const_name);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                
                                /* Check for duplicate case labels and constant expression requirement */
                                for (slist_t *cnode = children->next; cnode; cnode = cnode->next) {
                                    ast_node_t *case_label = (ast_node_t *)cnode->data;
                                    if (case_label->type == AST_CASE_LABEL) {
                                        /* Check for duplicate default */
                                        if (case_label->data.node.name &&
                                            strcmp(case_label->data.node.name, "default") == 0) {
                                            if (seen_default) {
                                                semantic_error(sem, case_label->line, case_label->column,
                                                    "duplicate default label");
                                            }
                                            seen_default = true;
                                            continue;
                                        }
                                        
                                        /* Get case expression */
                                        slist_t *case_children = case_label->data.node.children;
                                        if (case_children) {
                                            ast_node_t *case_expr = (ast_node_t *)case_children->data;
                                            if (case_expr && case_expr->type == AST_LITERAL) {
                                                /* Use the literal value as key */
                                                const char *val = case_expr->data.leaf.name;
                                                if (val) {
                                                    if (hashtable_lookup(seen_cases, val)) {
                                                        semantic_error(sem, case_label->line, case_label->column,
                                                            "duplicate case label");
                                                    } else {
                                                        hashtable_insert(seen_cases, val, (void *)1);
                                                    }
                                                }
                                            } else if (case_expr && case_expr->type == AST_IDENTIFIER) {
                                                /* Must be a constant (final static field or enum constant) */
                                                const char *name = case_expr->data.leaf.name;
                                                symbol_t *sym = scope_lookup(sem->current_scope, name);
                                                
                                                /* If not found in scope, try current class members (for interface constants) */
                                                if (!sym && sem->current_class && sem->current_class->data.class_data.members) {
                                                    sym = scope_lookup_local(sem->current_class->data.class_data.members, name);
                                                }
                                                
                                                /* Also check implemented interfaces for constants */
                                                if (!sym && sem->current_class) {
                                                    slist_t *ifaces = sem->current_class->data.class_data.interfaces;
                                                    for (slist_t *i = ifaces; i && !sym; i = i->next) {
                                                        symbol_t *iface = (symbol_t *)i->data;
                                                        if (iface && iface->data.class_data.members) {
                                                            sym = scope_lookup_local(iface->data.class_data.members, name);
                                                        }
                                                    }
                                                }
                                                
                                                if (sym && sym->kind == SYM_FIELD) {
                                                    /* Check if it's final (constant expression).
                                                     * Interface fields are implicitly final but may not have MOD_FINAL
                                                     * if declared in a source file that's not fully processed. */
                                                    bool is_interface_field = (sem->current_class && 
                                                                              sem->current_class->kind == SYM_INTERFACE);
                                                    if (!is_interface_field && !(sym->modifiers & MOD_FINAL)) {
                                                        semantic_error(sem, case_expr->line, case_expr->column,
                                                            "constant expression required");
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                hashtable_free(seen_cases);
                            }
                        }
                        break;
                    
                    case AST_SWITCH_EXPR:
                        {
                            /* Switch expressions require Java 14+ (preview in 12-13) */
                            if (sem->source_version < 14) {
                                semantic_error(sem, node->line, node->column,
                                    "switch expressions are not supported in -source %d (use 14 or higher)",
                                    sem->source_version);
                            }
                            
                            /* Switch expression: similar to switch statement but yields a value */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *selector = (ast_node_t *)children->data;
                                type_t *sel_type = get_expression_type(sem, selector);
                                
                                /* Store selector type on switch node for codegen */
                                node->sem_type = sel_type;
                                
                                /* Process switch rules */
                                for (slist_t *cnode = children->next; cnode; cnode = cnode->next) {
                                    ast_node_t *rule = (ast_node_t *)cnode->data;
                                    if (rule->type == AST_SWITCH_RULE) {
                                        /* Skip default */
                                        if (rule->data.node.name &&
                                            strcmp(rule->data.node.name, "default") == 0) {
                                            continue;
                                        }
                                        
                                        /* Get case patterns/expressions */
                                        slist_t *rule_children = rule->data.node.children;
                                        for (slist_t *rc = rule_children; rc; rc = rc->next) {
                                            ast_node_t *case_item = (ast_node_t *)rc->data;
                                            
                                            /* Handle patterns (Java 21+) */
                                            if (case_item->type == AST_TYPE_PATTERN ||
                                                case_item->type == AST_GUARDED_PATTERN ||
                                                case_item->type == AST_UNNAMED_PATTERN ||
                                                case_item->type == AST_RECORD_PATTERN) {
                                                /* Pattern matching in switch requires Java 21+ */
                                                if (sem->source_version < 21) {
                                                    semantic_error(sem, case_item->line, case_item->column,
                                                        "pattern matching in switch is not supported in -source %d (use 21 or higher)",
                                                        sem->source_version);
                                                }
                                                
                                                ast_node_t *pattern = case_item;
                                                ast_node_t *guard = NULL;
                                                
                                                if (case_item->type == AST_GUARDED_PATTERN) {
                                                    /* Guarded pattern: first child is type pattern, second is guard */
                                                    slist_t *gp_children = case_item->data.node.children;
                                                    if (gp_children) {
                                                        pattern = (ast_node_t *)gp_children->data;
                                                        if (gp_children->next) {
                                                            guard = (ast_node_t *)gp_children->next->data;
                                                        }
                                                    }
                                                }
                                                
                                                /* Handle patterns using helper function */
                                                if (pattern->type == AST_TYPE_PATTERN ||
                                                    pattern->type == AST_RECORD_PATTERN) {
                                                    semantic_define_pattern_vars(sem, pattern, sem->current_scope);
                                                }
                                                /* AST_UNNAMED_PATTERN has no variables to define */
                                                
                                                /* Process guard expression if present */
                                                if (guard) {
                                                    get_expression_type(sem, guard);
                                                }
                                            }
                                            /* Handle enum constant identifiers */
                                            else if (case_item->type == AST_IDENTIFIER &&
                                                     sel_type && sel_type->kind == TYPE_CLASS &&
                                                     sel_type->data.class_type.symbol &&
                                                     sel_type->data.class_type.symbol->kind == SYM_ENUM) {
                                                symbol_t *enum_sym = sel_type->data.class_type.symbol;
                                                const char *const_name = case_item->data.leaf.name;
                                                
                                                bool found = false;
                                                ast_node_t *enum_decl = enum_sym->ast;
                                                if (enum_decl) {
                                                    int ordinal = 0;
                                                    slist_t *enum_children = ast_get_children(enum_decl);
                                                    for (slist_t *ec = enum_children; ec; ec = ec->next) {
                                                        ast_node_t *enum_member = (ast_node_t *)ec->data;
                                                        if (enum_member->type == AST_ENUM_CONSTANT) {
                                                            const char *member_name = enum_member->data.node.name;
                                                            if (member_name && strcmp(member_name, const_name) == 0) {
                                                                case_item->sem_type = sel_type;
                                                                case_item->data.leaf.value.int_val = ordinal;
                                                                found = true;
                                                                break;
                                                            }
                                                            ordinal++;
                                                        }
                                                    }
                                                }
                                                if (!found) {
                                                    semantic_error(sem, case_item->line, case_item->column,
                                                        "Cannot resolve symbol: %s", const_name);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_CONDITIONAL_EXPR:
                        {
                            /* Check that ternary condition is boolean */
                            slist_t *children = node->data.node.children;
                            if (children) {
                                ast_node_t *condition = (ast_node_t *)children->data;
                                type_t *cond_type = get_expression_type(sem, condition);
                                /* Allow boolean or java.lang.Boolean (auto-unboxing) */
                                bool is_boolean = cond_type && 
                                    (cond_type->kind == TYPE_BOOLEAN ||
                                     (cond_type->kind == TYPE_CLASS && 
                                      cond_type->data.class_type.name &&
                                      strcmp(cond_type->data.class_type.name, "java.lang.Boolean") == 0));
                                if (cond_type && !is_boolean) {
                                    char *type_str = type_to_string(cond_type);
                                    semantic_error(sem, condition->line, condition->column,
                                        "ternary condition must be boolean, got %s", type_str);
                                    free(type_str);
                                }
                            }
                        }
                        break;
                    
                    case AST_ARRAY_ACCESS:
                        {
                            /* Check that array index is integer */
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                ast_node_t *index = (ast_node_t *)children->next->data;
                                type_t *index_type = get_expression_type(sem, index);
                                if (index_type && !type_is_integral(index_type)) {
                                    char *type_str = type_to_string(index_type);
                                    semantic_error(sem, index->line, index->column,
                                        "array index must be integer, got %s", type_str);
                                    free(type_str);
                                }
                            }
                        }
                        break;
                    
                    default:
                        break;
                }
                
                /* Transition to WALK_CHILDREN unless a case already set WALK_EXIT */
                if (frame->state != WALK_EXIT) {
                    frame->state = WALK_CHILDREN;
                    frame->child_iter = ast_get_children(node);
                }
                break;
            
            case WALK_CHILDREN:
                if (frame->child_iter) {
                    ast_node_t *child = frame->child_iter->data;
                    frame->child_iter = frame->child_iter->next;
                    
                    if (stack_top >= stack_capacity) {
                        stack_capacity *= 2;
                        stack = realloc(stack, sizeof(walk_frame_t *) * stack_capacity);
                    }
                    
                    stack[stack_top++] = walk_frame_new(child);
                } else {
                    frame->state = WALK_EXIT;
                }
                break;
            
            case WALK_EXIT:
                switch (node->type) {
                    case AST_CLASS_DECL:
                    case AST_ENUM_DECL:
                    case AST_RECORD_DECL:
                    case AST_ANNOTATION_DECL:
                        sem->current_scope = frame->saved_scope;
                        sem->current_class = frame->saved_class;
                        break;
                    
                    case AST_INTERFACE_DECL:
                        {
                            /* Validate @FunctionalInterface before restoring scope */
                            symbol_t *iface_sym = node->sem_symbol;
                            if (iface_sym) {
                                validate_functional_interface(sem, node, iface_sym);
                            }
                            sem->current_scope = frame->saved_scope;
                            sem->current_class = frame->saved_class;
                        }
                        break;
                    
                    case AST_METHOD_DECL:
                    case AST_CONSTRUCTOR_DECL:
                        sem->current_scope = frame->saved_scope;
                        sem->current_method = frame->saved_method;
                        break;
                    
                    case AST_BLOCK:
                        {
                            /* Check if this was an anonymous class body */
                            symbol_t *anon_sym = node->sem_symbol;
                            if (anon_sym && anon_sym->kind == SYM_CLASS && 
                                anon_sym->data.class_data.is_anonymous_class) {
                                /* Restore class context */
                        if (frame->saved_scope) {
                        sem->current_scope = frame->saved_scope;
                                }
                                if (frame->saved_class || sem->current_class == anon_sym) {
                                    sem->current_class = frame->saved_class;
                                }
                            } else {
                                /* Regular block - restore scope */
                                if (frame->saved_scope) {
                                    sem->current_scope = frame->saved_scope;
                                }
                        }
                        /* Note: We don't free block scopes here because captured variables
                         * in local classes hold references to symbols defined in these scopes.
                         * The scopes will be freed when semantic_free is called. */
                        }
                        break;
                    
                    case AST_SWITCH_RULE:
                        /* Restore scope if we created one */
                        if (frame->saved_scope) {
                            sem->current_scope = frame->saved_scope;
                        }
                        break;
                    
                    case AST_ENHANCED_FOR_STMT:
                        /* Restore scope and decrement loop depth */
                        if (frame->saved_scope) {
                            sem->current_scope = frame->saved_scope;
                        }
                        sem->loop_depth--;
                        break;
                    
                    case AST_CATCH_CLAUSE:
                        {
                            /* Restore scope */
                            if (frame->saved_scope) {
                                sem->current_scope = frame->saved_scope;
                            }
                            
                            /* Warn about empty catch blocks */
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                ast_node_t *catch_block = (ast_node_t *)children->next->data;
                                if (catch_block && catch_block->type == AST_BLOCK) {
                                    slist_t *stmts = catch_block->data.node.children;
                                    if (!stmts) {
                                        semantic_warning(sem, node->line, node->column,
                                            "empty catch block");
                                    }
                                }
                            }
                        }
                        break;
                    
                    case AST_LAMBDA_EXPR:
                        /* Restore scope if we created one for the lambda */
                        if (frame->saved_scope) {
                            sem->current_scope = frame->saved_scope;
                        }
                        /* Restore current_method */
                        if (frame->saved_method || node->sem_symbol) {
                            sem->current_method = frame->saved_method;
                        }
                        break;
                    
                    case AST_FOR_STMT:
                        {
                            /* Check for condition type (after init has been processed) */
                            slist_t *children = node->data.node.children;
                            if (children && children->next) {
                                ast_node_t *condition = (ast_node_t *)children->next->data;
                                /* Condition might be empty (infinite loop) */
                                if (condition && condition->type != AST_EMPTY_STMT) {
                                    type_t *cond_type = get_expression_type(sem, condition);
                                    if (cond_type && cond_type->kind != TYPE_BOOLEAN) {
                                        char *type_str = type_to_string(cond_type);
                                        semantic_error(sem, condition->line, condition->column,
                                            "for condition must be boolean, got %s", type_str);
                                        free(type_str);
                                    }
                                }
                            }
                            sem->current_scope = frame->saved_scope;
                            sem->loop_depth--;
                        }
                        break;
                    
                    case AST_WHILE_STMT:
                    case AST_DO_STMT:
                        sem->loop_depth--;
                        break;
                    
                    case AST_SWITCH_STMT:
                        sem->switch_depth--;
                        /* Restore scope from before the switch */
                        if (frame->saved_scope) {
                            sem->current_scope = frame->saved_scope;
                        }
                        break;
                    
                    default:
                        break;
                }
                
                free(stack[--stack_top]);
                break;
        }
    }
    
    free(stack);
}

/* ========================================================================
 * Main Analysis Entry Point
 * ======================================================================== */

/**
 * Perform semantic analysis on an AST.
 */
bool semantic_analyze(semantic_t *sem, ast_node_t *ast, source_file_t *source)
{
    if (!sem || !ast) {
        return false;
    }
    
    /* Set global for type_assignable callbacks */
    g_current_semantic = sem;
    
    sem->source = source;
    sem->error_count = 0;
    sem->warning_count = 0;
    
    /* Reset state */
    sem->current_scope = sem->global_scope;
    sem->current_class = NULL;
    sem->current_method = NULL;
    free(sem->current_package);
    sem->current_package = NULL;
    
    /* Clear per-compilation-unit type scope (like javac's toplevelScope).
     * This ensures simple names from previous files don't pollute the current file.
     * For parallel compilation, each thread would have its own semantic_t with
     * its own unit_types, so no synchronization is needed. */
    if (sem->unit_types) {
        /* Clear the hashtable instead of freeing and reallocating.
         * This avoids potential memory issues with frequent reallocation. */
        for (size_t i = 0; i < sem->unit_types->size; i++) {
            hashtable_entry_t *entry = sem->unit_types->buckets[i];
            while (entry) {
                hashtable_entry_t *next = entry->next;
                free(entry->key);
                free(entry);
                entry = next;
            }
            sem->unit_types->buckets[i] = NULL;
        }
        sem->unit_types->count = 0;
    }
    
    /* Pass 1: Collect all declarations */
    pass1_collect_declarations(sem, ast);
    
    /* Pass 1.5: Resolve type hierarchy (superclass/interfaces) */
    /* This handles forward references where class A extends B and B is defined later */
    resolve_type_hierarchy(sem, ast);
    
    /* Pass 2: Resolve names and check types */
    pass2_check_types(sem, ast);
    
    /* Clear global */
    g_current_semantic = NULL;
    
    return sem->error_count == 0;
}

