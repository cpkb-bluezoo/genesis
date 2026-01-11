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
static type_t *infer_type_arg(type_t *param_type, type_t *arg_type, const char *var_name);
static type_t *substitute_type_var(type_t *type, const char *var_name, type_t *replacement);

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
    sym->name = name ? strdup(name) : NULL;
    
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
    
    free(sym->name);
    free(sym->qualified_name);
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
    
    /* Check for duplicate in local scope only */
    if (hashtable_contains(scope->symbols, symbol->name)) {
        return false;
    }
    
    symbol->scope = scope;
    hashtable_insert(scope->symbols, symbol->name, symbol);
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

/* Forward declaration for find_best_method_by_types */
static symbol_t *find_best_method_by_types(semantic_t *sem, slist_t *candidates, slist_t *args);

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
    slist_t *candidates = scope_find_all_methods(scope, name);
    if (!candidates) {
        return NULL;
    }
    
    symbol_t *result = find_best_method_by_types(sem, candidates, args);
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
    sem->types = hashtable_new();
    sem->packages = hashtable_new();
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
    hashtable_free(sem->types);  /* Types are singletons, don't free values */
    hashtable_free(sem->packages);
    slist_free(sem->imports);
    slist_free_full(sem->sourcepath, free);
    free(sem->current_package);
    slist_free_full(sem->diagnostics, (void (*)(void *))diagnostic_free);
    
    free(sem);
}

/**
 * Report a semantic error.
 */
void semantic_error(semantic_t *sem, int line, int col, const char *fmt, ...)
{
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
                type_t *type = type_new_class(gt->data.class_type.name);
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
    
    /* Load fields */
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
            
            /* Parse field type - use helper that handles arrays correctly */
            if (fi->descriptor) {
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
    
    return sym;
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
            lexer_t *lexer = lexer_new(src);
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
            parser_free(parser);
            lexer_free(lexer);
            
            if (!ast || parser->error_msg) {
                ast_free(ast);
                source_file_free(src);
                continue;
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
                        
                        /* Create class member scope */
                        scope_t *class_scope = scope_new(SCOPE_CLASS, sem->global_scope);
                        class_scope->owner = sym;
                        sym->data.class_data.members = class_scope;
                        
                        /* Create type for this class and cache it BEFORE processing members
                         * to prevent infinite recursion when a member references this class */
                        type_t *type = type_new_class(sym->qualified_name);
                        type->data.class_type.symbol = sym;
                        sym->type = type;
                        hashtable_insert(sem->types, sym->qualified_name, type);
                        hashtable_insert(sem->types, sym->name, type);
                        
                        /* Process class body to extract members */
                        for (slist_t *member = decl->data.node.children; member; member = member->next) {
                            ast_node_t *m = (ast_node_t *)member->data;
                            if (m->type == AST_METHOD_DECL || m->type == AST_CONSTRUCTOR_DECL) {
                                const char *mname = m->data.node.name;
                                symbol_kind_t mkind = m->type == AST_METHOD_DECL ? SYM_METHOD : SYM_CONSTRUCTOR;
                                symbol_t *method_sym = symbol_new(mkind, mname);
                                method_sym->modifiers = m->data.node.flags;
                                method_sym->ast = m;
                                
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
                                scope_define(class_scope, const_sym);
                            }
                        }
                        
                        /* Trigger compilation of this dependency */
                        compile_dependency(full_path);
                        
                        /* Don't free ast - it's now referenced by the symbol */
                        source_file_free(src);
                        free(file_path);
                        return sym;
                    }
                }
            }
            
            ast_free(ast);
            source_file_free(src);
        }
    }
    
    free(file_path);
        return NULL;
    }
    
/**
 * Try to load a class from the classpath or sourcepath.
 */
symbol_t *load_external_class(semantic_t *sem, const char *name)
{
    /* First try classpath (compiled .class files) */
    if (sem->classpath) {
        classfile_t *cf = classpath_load_class(sem->classpath, name);
        if (cf) {
    return symbol_from_classfile(sem, cf);
        }
    }
    
    /* Then try sourcepath (.java files) */
    return load_class_from_source(sem, name);
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
            return strdup(import_name);
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
                return strdup(same_package);
            }
        }
        
        /* Try sourcepath */
        if (sem->sourcepath) {
            symbol_t *src_sym = load_class_from_source(sem, same_package);
            if (src_sym) {
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
            return strdup(java_lang);
        }
    }
    
    return NULL;
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
static symbol_t *find_best_method_by_types(semantic_t *sem, slist_t *candidates, slist_t *args)
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
        int param_idx = 0;
        
        while (arg_node && param_node) {
            ast_node_t *arg = (ast_node_t *)arg_node->data;
            symbol_t *param = (symbol_t *)param_node->data;
            param_idx++;
            
            if (arg && param && param->type) {
                /* Special handling for lambda/method ref arguments - they match any functional interface */
                if (arg->type == AST_LAMBDA_EXPR || arg->type == AST_METHOD_REF) {
                    /* For now, assume lambda matches functional interface parameter.
                     * Proper binding happens later in argument type checking. */
                    score += 50;  /* Give lower score than exact match */
                    arg_node = arg_node->next;
                    param_node = param_node->next;
                    continue;
                }
                
                type_t *arg_type = get_expression_type(sem, arg);
                type_t *param_type = param->type;
                
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
                    /* For class types, check if classes match exactly */
                    bool exact_match = false;
                    if (arg_type->kind == TYPE_CLASS && compare_type->kind == TYPE_CLASS) {
                        const char *arg_name = arg_type->data.class_type.name;
                        const char *param_name = compare_type->data.class_type.name;
                        if (arg_name && param_name && strcmp(arg_name, param_name) == 0) {
                            exact_match = true;
                        }
                    } else if (arg_type->kind == compare_type->kind) {
                        /* For non-class types, kind match is exact match */
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
                symbol_t *method = find_best_method_by_types(sem, candidates, args);
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
                        symbol_t *method = find_best_method_by_types(sem, candidates, args);
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
                
                /* Check if it's a nested class/enum/record of the current class */
                if (sem->current_class && sem->current_class->data.class_data.members) {
                    symbol_t *nested = scope_lookup_local(
                        sem->current_class->data.class_data.members, name);
                    if (nested && (nested->kind == SYM_CLASS || nested->kind == SYM_INTERFACE ||
                                   nested->kind == SYM_ENUM || nested->kind == SYM_RECORD)) {
                        if (nested->type) {
                            type_node->sem_type = nested->type;
                            return nested->type;
                        }
                    }
                }
                
                /* Check for qualified nested class names like "Inner.InnerInner" or "Outer.Inner" */
                const char *dot = strchr(name, '.');
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
                            type_node->sem_type = local->type;
                            return local->type;
                        }
                    }
                }
                
                /* Check type cache */
                type_t *cached = hashtable_lookup(sem->types, name);
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
                            
                            /* No type arguments - cache and return raw type */
                            hashtable_insert(sem->types, name, sym->type);
                            if (strcmp(name, qualified) != 0) {
                                hashtable_insert(sem->types, qualified, sym->type);
                            }
                            type_node->sem_type = sym->type;
                            free(qualified);
                            return sym->type;
                        }
                    }
                    
                    /* Create type with qualified name */
                    type_t *type = type_new_class(qualified);
                    hashtable_insert(sem->types, name, type);
                    hashtable_insert(sem->types, qualified, type);
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
                                     local_class->kind == SYM_INTERFACE) && 
                    local_class->type) {
                    /* Cache it and return */
                    hashtable_insert(sem->types, name, local_class->type);
                    type_node->sem_type = local_class->type;
                    return local_class->type;
                }
                
                /* Type cannot be resolved - report error */
                semantic_error(sem, type_node->line, type_node->column,
                              "Cannot resolve type: %s", name);
                type_t *type = type_new_class(name);
                hashtable_insert(sem->types, name, type);
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
                
                /* First try local scope lookup */
                symbol_t *sym = scope_lookup(sem->current_scope, name);
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
        
        /* Get record component types from the symbol */
        symbol_t *record_sym = NULL;
        if (record_type && record_type->kind == TYPE_CLASS) {
            record_sym = record_type->data.class_type.symbol;
        }
        
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
                            
                            symbol_t *sym = symbol_new(kind, name);
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
                            
                            /* Build qualified name */
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
                            
                            /* Cache the type */
                            hashtable_insert(sem->types, sym->qualified_name, type);
                            /* For local classes, don't cache by short name - they are only 
                             * visible in their local scope and different methods can have
                             * local classes with the same short name */
                            if (!sym->data.class_data.is_local_class) {
                                hashtable_insert(sem->types, name, type);
                            }
                            
                            /* Store symbol reference on AST node for code generation */
                            node->sem_symbol = sym;
                            
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
                            scope_t *class_scope = scope_new(SCOPE_CLASS, sem->current_scope);
                            class_scope->owner = sym;
                            sym->data.class_data.members = class_scope;
                            
                            /* Process type parameters (generics) from children */
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
                                        
                                        /* Create a type variable type for this parameter */
                                        type_t *type_var = malloc(sizeof(type_t));
                                        type_var->kind = TYPE_TYPEVAR;
                                        type_var->data.type_var.name = strdup(param_name);
                                        type_var->data.type_var.bound = NULL;
                                        
                                        /* Process bounds (extends clause) */
                                        if (child->data.node.children) {
                                            ast_node_t *bound_node = child->data.node.children->data;
                                            /* Resolve bound type - for now, use Object as fallback */
                                            type_t *bound = semantic_resolve_type(sem, bound_node);
                                            if (bound) {
                                                type_var->data.type_var.bound = bound;
                                            }
                                        }
                                        
                                        /* If no bound specified, use java.lang.Object */
                                        if (!type_var->data.type_var.bound) {
                                            type_var->data.type_var.bound = type_new_class("java.lang.Object");
                                        }
                                        
                                        type_param->type = type_var;
                                        
                                        /* Add to class's type parameters list */
                                        if (!sym->data.class_data.type_params) {
                                            sym->data.class_data.type_params = slist_new(type_param);
                                        } else {
                                            slist_append(sym->data.class_data.type_params, type_param);
                                        }
                                        
                                        /* Define in class scope so it can be looked up */
                                        scope_define(class_scope, type_param);
                                    }
                                }
                                children = children->next;
                            }
                            
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
                                        if (!super_sym || super_sym->kind != SYM_CLASS) {
                                            /* Try same package first */
                                            if (sem->current_package) {
                                                char same_package[512];
                                                snprintf(same_package, sizeof(same_package), "%s.%s",
                                                         sem->current_package, super_name);
                                                super_sym = scope_lookup(sem->global_scope, same_package);
                                                if (!super_sym || super_sym->kind != SYM_CLASS) {
                                                    super_sym = load_external_class(sem, same_package);
                                                }
                                            }
                                            if (!super_sym || super_sym->kind != SYM_CLASS) {
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
                                        if (super_sym && super_sym->kind == SYM_CLASS) {
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
                                        if (iface_sym) {
                                            /* Check that we're implementing an interface, not a class */
                                            if (iface_sym->kind != SYM_INTERFACE &&
                                                iface_sym->kind != SYM_ANNOTATION) {
                                                semantic_error(sem, child->line, child->column,
                                                    "interface expected here");
                                            }
                                            /* Check for repeated interface */
                                            bool duplicate = false;
                                            for (slist_t *inode = sym->data.class_data.interfaces;
                                                 inode; inode = inode->next) {
                                                if (inode->data == iface_sym) {
                                                    semantic_error(sem, child->line, child->column,
                                                        "repeated interface");
                                                    duplicate = true;
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
                                    
                                    /* Check field initializer if present */
                                    if (decl->data.node.children) {
                                        ast_node_t *init_expr = (ast_node_t *)decl->data.node.children->data;
                                        
                                        /* Handle lambda/method reference with target typing */
                                        if (init_expr->type == AST_LAMBDA_EXPR) {
                                            bind_lambda_to_target_type(sem, init_expr, field_type);
                                        } else if (init_expr->type == AST_METHOD_REF) {
                                            bind_method_ref_to_target_type(sem, init_expr, field_type);
                                        }
                                        
                                        /* Set target type for type inference in method calls */
                                        type_t *saved_target = sem->target_type;
                                        sem->target_type = field_type;
                                        type_t *init_type = get_expression_type(sem, init_expr);
                                        sem->target_type = saved_target;
                                        
                                        /* Mark as processed to avoid duplicate checks in pass2 */
                                        if (init_type && !init_expr->sem_type) {
                                            init_expr->sem_type = init_type;
                                        }
                                        
                                        if (field_type && init_type && !type_assignable(field_type, init_type)) {
                                            char *expected = type_to_string(field_type);
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
                    
                    case AST_ENUM_CONSTANT:
                        {
                            /* Enum constants are public static final fields of the enum type */
                            const char *name = node->data.node.name;
                            
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
                            
                            /* Return type */
                            if (node->data.node.extra) {
                                sym->type = semantic_resolve_type(sem, node->data.node.extra);
                            } else if (kind == SYM_CONSTRUCTOR) {
                                /* Constructor return type is the class */
                                if (sem->current_class) {
                                    sym->type = sem->current_class->type;
                                }
                            }
                            
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
                                while (super) {
                                        if (super->data.class_data.members) {
                                        /* Methods are stored with keys like "name(N)" where N is param count.
                                         * We need to iterate and find by name prefix. */
                                        scope_t *ms = super->data.class_data.members;
                                        symbol_t *parent_method = NULL;
                                        if (ms->symbols) {
                                            size_t name_len = strlen(name);
                                            for (size_t bi = 0; bi < ms->symbols->size && !parent_method; bi++) {
                                                hashtable_entry_t *entry = ms->symbols->buckets[bi];
                                                while (entry && !parent_method) {
                                                    /* Check if key starts with method name */
                                                    if (strncmp(entry->key, name, name_len) == 0) {
                                                        char next_char = entry->key[name_len];
                                                        /* Key should be "name(N)" or just "name" */
                                                        if (next_char == '\0' || next_char == '(') {
                                                            symbol_t *s = (symbol_t *)entry->value;
                                                            if (s && s->kind == SYM_METHOD) {
                                                                parent_method = s;
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
                                 * but our method Map call() needs a bridge Object call(). */
                                if (!sym->data.method_data.overridden_method && 
                                    sem->current_class->data.class_data.interfaces) {
                                    for (slist_t *iface_node = sem->current_class->data.class_data.interfaces;
                                         iface_node && !sym->data.method_data.overridden_method;
                                         iface_node = iface_node->next) {
                                        /* interfaces list contains symbol_t* not type_t* */
                                        symbol_t *iface_sym = (symbol_t *)iface_node->data;
                                        if (!iface_sym || !iface_sym->data.class_data.members) {
                                            continue;
                                        }
                                        
                                        /* Find matching method in interface.
                                         * Methods from classfiles are stored with keys like "call()Ljava/lang/Object;"
                                         * so we need to iterate through all symbols to find by name. */
                                        symbol_t *iface_method = NULL;
                                        scope_t *members = iface_sym->data.class_data.members;
                                        if (members && members->symbols) {
                                            hashtable_t *ht = members->symbols;
                                            for (size_t i = 0; i < ht->size && !iface_method; i++) {
                                                hashtable_entry_t *entry = ht->buckets[i];
                                                while (entry && !iface_method) {
                                                    symbol_t *s = (symbol_t *)entry->value;
                                                    if (s && s->kind == SYM_METHOD && s->name &&
                                                        strcmp(s->name, name) == 0) {
                                                        iface_method = s;
                                                    }
                                                    entry = entry->next;
                                                }
                                            }
                                        }
                                        if (!iface_method || iface_method->kind != SYM_METHOD) continue;
                                        
                                        /* Found matching method in interface */
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
                            int param_count = 0;
                            {
                                slist_t *params = node->data.node.children;
                                while (params) {
                                    ast_node_t *child = params->data;
                                    if (child->type == AST_PARAMETER) {
                                        param_count++;
                                    }
                                    params = params->next;
                                }
                            }
                            
                            /* Store method with unique key for overloading support.
                             * Use format: name(N) where N is parameter count */
                            char method_key[512];
                            snprintf(method_key, sizeof(method_key), "%s(%d)", name, param_count);
                            
                            /* Use hashtable_insert directly to support overloads */
                            if (sem->current_scope && sem->current_scope->symbols) {
                                hashtable_insert(sem->current_scope->symbols, method_key, sym);
                                sym->scope = sem->current_scope;
                            }
                            
                            /* Create method scope */
                            scope_t *method_scope = scope_new(SCOPE_METHOD, sem->current_scope);
                            method_scope->owner = sym;
                            sym->data.method_data.body_scope = method_scope;
                            
                            /* Set current_method so type parameters can be looked up */
                            symbol_t *saved_method = sem->current_method;
                            sem->current_method = sym;
                            
                            /* Process method type parameters (generics) first */
                            slist_t *seen_type_params = NULL;  /* Track seen type params for duplicate check */
                            slist_t *children = node->data.node.children;
                            while (children) {
                                ast_node_t *child = children->data;
                                if (child->type == AST_TYPE_PARAMETER) {
                                    const char *param_name = child->data.node.name;
                                    if (param_name) {
                                        /* Check for duplicate type parameter */
                                        bool is_duplicate = false;
                                        for (slist_t *tp = seen_type_params; tp; tp = tp->next) {
                                            const char *existing = (const char *)tp->data;
                                            if (existing && strcmp(existing, param_name) == 0) {
                                                semantic_error(sem, child->line, child->column,
                                                    "Duplicate type parameter: '%s'", param_name);
                                                is_duplicate = true;
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
                                        
                                        /* Create a type variable type */
                                        type_t *type_var = malloc(sizeof(type_t));
                                        type_var->kind = TYPE_TYPEVAR;
                                        type_var->data.type_var.name = strdup(param_name);
                                        type_var->data.type_var.bound = NULL;
                                        
                                        /* Process bounds (extends clause) */
                                        if (child->data.node.children) {
                                            ast_node_t *bound_node = child->data.node.children->data;
                                            type_t *bound = semantic_resolve_type(sem, bound_node);
                                            if (bound) {
                                                type_var->data.type_var.bound = bound;
                                            }
                                        }
                                        
                                        /* If no bound specified, use java.lang.Object */
                                        if (!type_var->data.type_var.bound) {
                                            type_var->data.type_var.bound = type_new_class("java.lang.Object");
                                        }
                                        
                                        type_param->type = type_var;
                                        
                                        /* Define in method scope so it can be looked up */
                                        scope_define(method_scope, type_param);
                                    }
                                }
                                children = children->next;
                            }
                            if (seen_type_params) slist_free(seen_type_params);
                            
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
                            
                            /* Restore current_method to what it was before processing params
                             * The frame will save this and restore later in WALK_EXIT */
                            sem->current_method = saved_method;
                            
                            /* Save and switch scope for processing method body */
                            frame->saved_scope = sem->current_scope;
                            frame->saved_method = sem->current_method;
                            sem->current_scope = method_scope;
                            sem->current_method = sym;
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
                                                    /* Check if this class provides implementation */
                                                    bool implemented = false;
                                                    scope_t *class_members = class_sym->data.class_data.members;
                                                    if (class_members && class_members->symbols) {
                                                        hashtable_t *ht2 = class_members->symbols;
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
 * Recursively process all expressions in an AST node to detect captured variables.
 * This is used for anonymous class method bodies.
 */
static void process_expressions_for_capture(semantic_t *sem, ast_node_t *node)
{
    if (!node) return;
    
    /* For expression nodes, call get_expression_type to trigger captured var detection */
    switch (node->type) {
        case AST_IDENTIFIER:
        case AST_LITERAL:
            /* Leaf nodes - call get_expression_type but don't recurse into children */
            get_expression_type(sem, node);
            return;  /* No children to process */
            
        case AST_METHOD_CALL:
        case AST_FIELD_ACCESS:
        case AST_ASSIGNMENT_EXPR:
        case AST_BINARY_EXPR:
        case AST_UNARY_EXPR:
        case AST_CONDITIONAL_EXPR:
        case AST_NEW_OBJECT:
        case AST_NEW_ARRAY:
        case AST_ARRAY_ACCESS:
        case AST_CAST_EXPR:
        case AST_INSTANCEOF_EXPR:
        case AST_PARENTHESIZED:
            get_expression_type(sem, node);
            break;
        default:
            break;
    }
    
    /* Recursively process children - skip leaf nodes that use data.leaf union */
    /* Leaf nodes are created via ast_new_leaf() and use data.leaf instead of data.node.
     * Accessing data.node.children on these would give garbage (e.g., the name string). */
    switch (node->type) {
        case AST_LITERAL:
        case AST_IDENTIFIER:
        case AST_THIS_EXPR:
        case AST_SUPER_EXPR:
        case AST_PRIMITIVE_TYPE:
        case AST_VAR_TYPE:
            /* Leaf nodes - no children to process */
            return;
        default:
            break;
    }
    
        slist_t *children = node->data.node.children;
        for (slist_t *c = children; c; c = c->next) {
            if (c->data) {
                process_expressions_for_capture(sem, (ast_node_t *)c->data);
        }
    }
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
                
                /* Could be a class reference - check types cache */
                type_t *type = hashtable_lookup(sem->types, name);
                if (type) {
                    /* Set sem_symbol if we have a class symbol */
                    if (type->kind == TYPE_CLASS && type->data.class_type.symbol) {
                        expr->sem_symbol = type->data.class_type.symbol;
                    }
                    expr->sem_type = type;
                    return type;
                }
                
                /* Try resolving through imports first */
                char *qualified = resolve_import(sem, name);
                if (qualified) {
                    type = hashtable_lookup(sem->types, qualified);
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
                
                /* Check if it's an inherited field from superclass */
                if (sem->current_class) {
                    symbol_t *search_class = sem->current_class->data.class_data.superclass;
                    while (search_class) {
                        if (search_class->data.class_data.members) {
                            symbol_t *field = scope_lookup_local(
                                search_class->data.class_data.members, name);
                            if (field && field->kind == SYM_FIELD) {
                                /* Store as if it's our own field for codegen */
                                expr->sem_symbol = field;
                                expr->sem_type = field->type;
                                return field->type;
                            }
                        }
                        search_class = search_class->data.class_data.superclass;
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
                if (children) {
                    ast_node_t *first = (ast_node_t *)children->data;
                    
                    /* Check if first child could be a receiver */
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
                        
                        /* Check the types cache as a fallback for nested types */
                        if (!class_sym) {
                            type_t *type = hashtable_lookup(sem->types, recv_name);
                            if (type && type->kind == TYPE_CLASS && type->data.class_type.symbol) {
                                class_sym = type->data.class_type.symbol;
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
                                if (method && method->kind == SYM_METHOD) {
                                    found_method = method;
                                }
                            }
                        }
                        
                        /* Otherwise check if it's a variable */
                        if (!target_class) {
                            symbol_t *sym = scope_lookup(sem->current_scope, recv_name);
                            if (sym && (sym->kind == SYM_LOCAL_VAR || sym->kind == SYM_PARAMETER ||
                                        sym->kind == SYM_FIELD)) {
                                /* Call get_expression_type on the identifier to trigger
                                 * capture detection if we're inside a lambda */
                                get_expression_type(sem, first);
                                
                                /* It's a variable - get its type to find the class */
                                type_t *recv_type = sym->type;
                                recv_type_for_subst = recv_type;  /* Track for type arg substitution */
                                
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
                                
                                if (recv_type && recv_type->kind == TYPE_CLASS &&
                                    recv_type->data.class_type.symbol) {
                                    symbol_t *recv_class = recv_type->data.class_type.symbol;
                                    symbol_t *method = NULL;
                                    /* Arguments are children->next since first child is the receiver */
                                    slist_t *args = children->next;
                                    
                                    /* Check direct members */
                                    if (recv_class->data.class_data.members) {
                                        method = scope_lookup_method_with_types(sem,
                                            recv_class->data.class_data.members, method_name, args);
                                    }
                                    
                                    /* Check superclass chain */
                                    if (!method || method->kind != SYM_METHOD) {
                                        symbol_t *super = recv_class->data.class_data.superclass;
                                        while (super && (!method || method->kind != SYM_METHOD)) {
                                            if (super->data.class_data.members) {
                                                method = scope_lookup_method_with_types(sem,
                                                    super->data.class_data.members, method_name, args);
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
                        if (recv_type && recv_type->kind == TYPE_CLASS) {
                            target_class = recv_type->data.class_type.symbol;
                            /* Load class if symbol not yet set (e.g., from classfile descriptor) */
                            if (!target_class && recv_type->data.class_type.name) {
                                target_class = load_external_class(sem, recv_type->data.class_type.name);
                            }
                            recv_type_for_subst = recv_type;  /* Track for type arg substitution */
                        }
                    } else if ((first->type == AST_LITERAL || 
                                first->type == AST_NEW_OBJECT ||
                                first->type == AST_ARRAY_ACCESS ||
                                first->type == AST_PARENTHESIZED ||
                                first->type == AST_CAST_EXPR ||
                                first->type == AST_CONDITIONAL_EXPR ||
                                first->type == AST_METHOD_CALL ||
                                first->type == AST_CLASS_LITERAL) &&
                               (expr->data.node.flags & AST_METHOD_CALL_EXPLICIT_RECEIVER)) {
                        /* Handle method calls on literals and other expressions */
                        /* e.g., "abc".trim(), new String().length(), (obj).method() */
                        /* Also handles chained method calls: "abc".trim().toUpperCase() */
                        /* Only when there's an explicit receiver (dot notation) */
                        type_t *recv_type = get_expression_type(sem, first);
                        recv_type_for_subst = recv_type;  /* Track for type arg substitution */
                        
                        /* For chained method calls, set sem_type on the receiver so
                         * codegen can access it when generating the call chain */
                        if (recv_type && first->type == AST_METHOD_CALL && !first->sem_type) {
                            first->sem_type = recv_type;
                        }
                        if (recv_type && recv_type->kind == TYPE_CLASS) {
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
                                symbol_t *super = recv_class->data.class_data.superclass;
                                while (super && (!method || method->kind != SYM_METHOD)) {
                                    if (super->data.class_data.members) {
                                        method = scope_lookup_method_with_types(sem,
                                            super->data.class_data.members, method_name, args);
                                    }
                                    super = super->data.class_data.superclass;
                                }
                            }
                            
                            /* Check interface hierarchy */
                            if (recv_class && (!method || method->kind != SYM_METHOD)) {
                                method = lookup_method_in_interfaces(sem, recv_class, method_name);
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
                    while (!found_method && super) {
                        if (super->data.class_data.members) {
                            found_method = scope_lookup_method_with_types(sem,
                                super->data.class_data.members, method_name, method_args);
                        }
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
                        
                        /* Substitute inferred type argument if bind_type is a type variable */
                        if (bind_type && bind_type->kind == TYPE_TYPEVAR && inferred_type_arg) {
                            bind_type = inferred_type_arg;
                        }
                        /* Also substitute if bind_type is an array of a type variable (e.g., T[]) */
                        else if (bind_type && bind_type->kind == TYPE_ARRAY && inferred_type_arg) {
                            type_t *elem_type = bind_type->data.array_type.element_type;
                            if (elem_type && elem_type->kind == TYPE_TYPEVAR) {
                                bind_type = type_new_array(inferred_type_arg, bind_type->data.array_type.dimensions);
                            }
                        }
                        
                        /* For lambda/method ref arguments, bind to parameter type first */
                        if (arg->type == AST_LAMBDA_EXPR && bind_type) {
                            bind_lambda_to_target_type(sem, arg, bind_type);
                        } else if (arg->type == AST_METHOD_REF && bind_type) {
                            bind_method_ref_to_target_type(sem, arg, bind_type);
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
                            
                            /* Check element type for individual varargs */
                            if (bind_type && arg_type && !type_assignable(bind_type, arg_type)) {
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
                        return_type = substitute_from_receiver(return_type, recv_type_for_subst);
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
                
                /* Before recursing, check if this could be a fully qualified class name.
                 * Build the full name from the identifier chain and try to load it. */
                {
                    char fqn[1024];
                    fqn[0] = '\0';
                    ast_node_t *node = expr;
                    slist_t *visited_nodes = NULL;  /* Track nodes to mark if FQN succeeds */
                    
                    /* Traverse the chain backwards to build the FQN */
                    while (node && (node->type == AST_FIELD_ACCESS || node->type == AST_IDENTIFIER)) {
                        visited_nodes = slist_prepend(visited_nodes, node);
                        
                        const char *part = (node->type == AST_FIELD_ACCESS) ?
                                          node->data.node.name : node->data.leaf.name;
                        if (part) {
                            char temp[1024];
                            if (fqn[0] == '\0') {
                                snprintf(temp, sizeof(temp), "%s", part);
                            } else {
                                snprintf(temp, sizeof(temp), "%s.%s", part, fqn);
                            }
                            strncpy(fqn, temp, sizeof(fqn) - 1);
                        }
                        
                        if (node->type == AST_FIELD_ACCESS) {
                            slist_t *ch = node->data.node.children;
                            node = ch ? (ast_node_t *)ch->data : NULL;
                        } else {
                            break;  /* Reached the root identifier */
                        }
                    }
                    
                    /* Try to load this as a class name */
                    if (fqn[0] != '\0') {
                        symbol_t *class_sym = load_external_class(sem, fqn);
                        if (class_sym && class_sym->type) {
                            /* It's a class reference! Mark all visited nodes with the type
                             * so they don't trigger "Cannot resolve symbol" errors later */
                            for (slist_t *n = visited_nodes; n; n = n->next) {
                                ast_node_t *visited = (ast_node_t *)n->data;
                                visited->sem_type = class_sym->type;
                            }
                            slist_free(visited_nodes);
                            expr->sem_symbol = class_sym;
                            return class_sym->type;
                        }
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
                type_t *object_type = get_expression_type(sem, object_expr);
                
                /* Special case: array.length returns int */
                if (object_type && object_type->kind == TYPE_ARRAY &&
                    strcmp(field_name, "length") == 0) {
                    return type_new_primitive(TYPE_INT);
                }
                
                /* Look up the field in the object's type, including superclasses */
                if (object_type && object_type->kind == TYPE_CLASS && 
                    object_type->data.class_type.symbol) {
                    symbol_t *class_sym = object_type->data.class_type.symbol;
                    
                    /* Search class hierarchy for field */
                    symbol_t *search_class = class_sym;
                    while (search_class) {
                        if (search_class->data.class_data.members) {
                        symbol_t *field = scope_lookup_local(
                                search_class->data.class_data.members, field_name);
                        if (field) {
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
                                return field->type;
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
                type_t *base_type = semantic_resolve_type(sem, type_node);
                
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
                    /* This is an anonymous class - create a synthetic class symbol */
                    int anon_id = ++sem->current_class->data.class_data.local_class_counter;
                    char anon_name[256];
                    snprintf(anon_name, sizeof(anon_name), "%s$%d", 
                             sem->current_class->qualified_name, anon_id);
                    
                    symbol_t *anon_sym = symbol_new(SYM_CLASS, anon_name);
                    anon_sym->qualified_name = strdup(anon_name);
                    
                    /* Anonymous classes in static contexts are implicitly static */
                    /* Static context means: static method, static initializer, or inside a static lambda */
                    bool in_static_context = (sem->current_method == NULL) ||  /* static initializer */
                                            (sem->current_method->modifiers & MOD_STATIC) ||  /* static method */
                                            (sem->current_lambda != NULL);  /* inside lambda (lambdas are static methods) */
                    anon_sym->modifiers = in_static_context ? MOD_STATIC : 0;
                    anon_sym->data.class_data.enclosing_class = sem->current_class;
                    anon_sym->data.class_data.enclosing_method = sem->current_method;
                    anon_sym->data.class_data.is_anonymous_class = true;
                    anon_sym->data.class_data.anonymous_body = anon_body;
                    
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
                                    /* Method body - process statements to detect captured variables */
                                    scope_t *body_scope = scope_new(SCOPE_BLOCK, method_scope);
                                    sem->current_scope = body_scope;
                                    
                                    /* Recursively process expressions in method body */
                                    process_expressions_for_capture(sem, mchild);
                                    
                                    sem->current_scope = method_scope;
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
                
                /* Process constructor arguments to ensure their types are resolved
                 * (including nested new expressions) */
                slist_t *args = NULL;
                slist_t *args_tail = NULL;
                for (slist_t *c = children->next; c; c = c->next) {
                    ast_node_t *arg = (ast_node_t *)c->data;
                    /* Skip anonymous class body */
                    if (arg->type != AST_BLOCK) {
                        get_expression_type(sem, arg);
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
                if (base_type && base_type->kind == TYPE_CLASS) {
                    symbol_t *target_class = base_type->data.class_type.symbol;
                    if (!target_class && base_type->data.class_type.name) {
                        target_class = load_external_class(sem, base_type->data.class_type.name);
                    }
                    
                    if (target_class && target_class->data.class_data.members) {
                        /* Find all constructors - they're stored with <init> as the name */
                        slist_t *ctors = scope_find_all_methods(target_class->data.class_data.members, "<init>");
                        
                        /* Also try the class name (for source-defined constructors) */
                        if (!ctors && target_class->name) {
                            ctors = scope_find_all_methods(target_class->data.class_data.members, 
                                                          target_class->name);
                        }
                        
                        /* Find best matching constructor */
                        if (ctors) {
                            symbol_t *ctor = find_best_method_by_types(sem, ctors, args);
                            if (ctor) {
                                expr->sem_symbol = ctor;
                            }
                            slist_free(ctors);
                        }
                    }
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
                    /* Count dimensions from the expression */
                    int dims = 0;
                    bool has_initializer = false;
                    slist_t *c = children->next;
                    while (c) {
                        ast_node_t *child = c->data;
                        if (child->type == AST_ARRAY_INIT) {
                            has_initializer = true;
                        } else {
                            dims++;
                        }
                        c = c->next;
                    }
                    
                    /* Check for missing array dimensions */
                    if (dims == 0 && !has_initializer) {
                        semantic_error(sem, expr->line, expr->column,
                            "array dimension missing");
                    }
                    
                    if (dims == 0) {
                        dims = 1;
                    }
                    return type_new_array(elem, dims);
                }
                return type_new_primitive(TYPE_UNKNOWN);
            }
        
        case AST_CONDITIONAL_EXPR:
            {
                /* Type is common type of true/false branches */
                slist_t *children = expr->data.node.children;
                if (children && children->next && children->next->next) {
                    return get_expression_type(sem, children->next->data);
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
                                    /* Look for yield in block */
                                    slist_t *stmts = body->data.node.children;
                                    for (slist_t *s = stmts; s; s = s->next) {
                                        ast_node_t *stmt = (ast_node_t *)s->data;
                                        if (stmt->type == AST_YIELD_STMT) {
                                            slist_t *yield_children = stmt->data.node.children;
                                            if (yield_children) {
                                                return get_expression_type(sem, yield_children->data);
                                            }
                                        }
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
                                        /* Look for yield in block */
                                        slist_t *stmts = body->data.node.children;
                                        for (slist_t *s = stmts; s; s = s->next) {
                                            ast_node_t *stmt = (ast_node_t *)s->data;
                                            if (stmt->type == AST_YIELD_STMT) {
                                                slist_t *yield_children = stmt->data.node.children;
                                                if (yield_children) {
                                                    result = get_expression_type(sem, yield_children->data);
                                                    break;
                                                }
                                            }
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
                    return get_expression_type(sem, children->data);
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
            /* instanceof always returns boolean */
            return type_new_primitive(TYPE_BOOLEAN);
        
        case AST_CAST_EXPR:
            {
                /* Cast type is the target type - use semantic_resolve_type for proper resolution */
                slist_t *children = expr->data.node.children;
                if (children) {
                    ast_node_t *type_node = (ast_node_t *)children->data;
                    return semantic_resolve_type(sem, type_node);
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

static void fi_count_abstract_fn(const char *key, void *value, void *user_data)
{
    (void)key;
    fi_validation_t *validation = (fi_validation_t *)user_data;
    symbol_t *sym = (symbol_t *)value;
    
    if (sym && sym->kind == SYM_METHOD) {
        /* Check if method is abstract (not static, not default) */
        if (!(sym->modifiers & MOD_STATIC) &&
            !(sym->modifiers & MOD_DEFAULT)) {
            validation->abstract_count++;
            if (validation->abstract_count == 1) {
                validation->sam = sym;
            }
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
    
    /* Count abstract methods */
    scope_t *members = sym->data.class_data.members;
    if (!members || !members->symbols) {
        semantic_error(sem, node->line, node->column,
            "Interface %s annotated with @FunctionalInterface has no methods",
            sym->name);
        return;
    }
    
    fi_validation_t validation = { 0, NULL };
    hashtable_foreach(members->symbols, fi_count_abstract_fn, &validation);
    
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
            return arg_type;
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
        return type;  /* No type arguments on receiver */
    }
    
    if (type->kind == TYPE_TYPEVAR) {
        const char *var_name = type->data.type_var.name;
        if (!var_name) return type;
        
        /* Common type parameter names and their positions:
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
            if (arg) return (type_t *)arg->data;
        }
        /* Second type argument: R (for Function), U, V */
        else if (strcmp(var_name, "R") == 0 || strcmp(var_name, "U") == 0 ||
                 strcmp(var_name, "V") == 0) {
            if (arg && arg->next) return (type_t *)arg->next->data;
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
        
        /* Get first type argument for matching */
        slist_t *arg = type_args;
        
        /* Iterate through symbols looking for type parameters in order */
        /* For now, use a simpler approach: check if var_name matches T (first) or R (second) etc. */
        /* This is a heuristic - proper implementation would track type parameter order */
        
        /* Common functional interface patterns:
         * Function<T,R>: T is first arg, R is second
         * Consumer<T>: T is first arg  
         * Supplier<T>: T is first arg (return type)
         * Predicate<T>: T is first arg
         * BiFunction<T,U,R>: T=first, U=second, R=third
         */
        
        /* Simple single-letter matching (common functional interface conventions) */
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

/* Helper struct for SAM search */
typedef struct {
    symbol_t *sam;
    int abstract_count;
} sam_search_t;

static void sam_search_fn(const char *key, void *value, void *user_data)
{
    (void)key;
    sam_search_t *search = (sam_search_t *)user_data;
    symbol_t *sym = (symbol_t *)value;
    
    if (sym && sym->kind == SYM_METHOD) {
        /* Check if method is abstract (interface methods are implicitly abstract) */
        if (!(sym->modifiers & MOD_STATIC) &&
            !(sym->modifiers & MOD_DEFAULT)) {
            /* This is an abstract instance method */
            search->abstract_count++;
            if (search->abstract_count == 1) {
                search->sam = sym;
            } else {
                /* More than one - not a functional interface */
                search->sam = NULL;
            }
        }
    }
}

/**
 * Find the single abstract method (SAM) of a functional interface.
 * Returns the method symbol if found, NULL otherwise.
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
    
    scope_t *members = iface_sym->data.class_data.members;
    if (!members || !members->symbols) {
        return NULL;
    }
    
    /* Find the single abstract method */
    sam_search_t search = { NULL, 0 };
    hashtable_foreach(members->symbols, sam_search_fn, &search);
    
    return (search.abstract_count == 1) ? search.sam : NULL;
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
                /* Block body - nested lambdas will be bound by the main semantic pass
                 * when it visits the lambda body. We don't do early binding here because
                 * local variables haven't been defined yet and get_expression_type would
                 * fail to resolve them. The main pass will handle nested lambdas naturally
                 * when it encounters method calls with lambda arguments.
                 * 
                 * Note: We used to do a mini-analysis here to find nested lambdas, but
                 * that caused "Cannot resolve symbol" errors for local variables like:
                 *   list.forEach(item -> {
                 *       String s = item;
                 *       System.out.println(s);  // s not defined yet during mini-analysis
                 *   });
                 */
                 (void)0;  /* Empty statement - block body handled by main pass */
            } else {
                /* Expression body - check and get its type */
                type_t *body_type = get_expression_type(sem, body);
                
                /* Check return type matches SAM return type (with type arg substitution) */
                if (sam->type && sam->type->kind != TYPE_VOID) {
                    type_t *expected_type = substitute_type_args(sam->type, iface_sym, type_args);
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
                                /* If this is type variable R, substitute with body_type.
                                 * If body_type is a primitive, use the boxed wrapper type
                                 * since R is a reference type. */
                                if (arg_unwrapped->kind == TYPE_TYPEVAR && arg_unwrapped->data.type_var.name &&
                                    strcmp(arg_unwrapped->data.type_var.name, "R") == 0) {
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
    
    /* Handle array constructor references like String[]::new */
    bool is_array_constructor = false;
    
    if (target_node->type == AST_ARRAY_TYPE) {
        /* Array constructor reference: Type[]::new */
        if (!is_constructor) {
            semantic_error(sem, ref->line, ref->column,
                "Array type can only be used with ::new");
            return false;
        }
        
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
        is_array_constructor = true;
        
        /* For array constructors, we don't have a "resolved method" - the array
         * creation is handled specially in codegen */
        resolved_method = NULL;
        
    } else if (target_node->type == AST_IDENTIFIER) {
        /* Could be:
         * - ClassName::method (static or unbound instance method)
         * - ClassName::new (constructor reference)
         * - variableName::method (bound instance method reference)
         */
        const char *name = target_node->data.leaf.name;
        
        /* First, try to resolve as a variable (for bound method references) */
        symbol_t *var_sym = scope_lookup(sem->current_scope, name);
        if (var_sym && (var_sym->kind == SYM_LOCAL_VAR || var_sym->kind == SYM_PARAMETER ||
                        var_sym->kind == SYM_FIELD)) {
            /* This is a bound method reference like instance::method */
            target_class_type = var_sym->type;
            if (!target_class_type || target_class_type->kind != TYPE_CLASS) {
                semantic_error(sem, ref->line, ref->column,
                    "Method reference target '%s' must be a class type", name);
                return false;
            }
            
            target_class_sym = target_class_type->data.class_type.symbol;
            is_bound = true;
            
            if (is_constructor) {
                semantic_error(sem, ref->line, ref->column,
                    "Cannot use constructor reference with instance expression");
                return false;
            }
            
            /* Find the instance method */
            if (target_class_sym && target_class_sym->data.class_data.members) {
                resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
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
                 * Look for constructor with matching param count */
                if (target_class_sym->data.class_data.members) {
                    /* For source-defined constructors, try key format name(N)
                     * For simplicity, try no-arg first, then collect all */
                    char ctor_key[256];
                    snprintf(ctor_key, sizeof(ctor_key), "%s(0)", name);
                    resolved_method = scope_lookup_local(target_class_sym->data.class_data.members, ctor_key);
                    if (!resolved_method || resolved_method->kind != SYM_CONSTRUCTOR) {
                        /* Try <init>() for classfile-loaded constructors */
                        resolved_method = scope_lookup_local(target_class_sym->data.class_data.members, "<init>()");
                    }
                    /* If still not found, try to find any constructor using scope_find_all_methods */
                    if (!resolved_method || resolved_method->kind != SYM_CONSTRUCTOR) {
                        slist_t *ctors = scope_find_all_methods(target_class_sym->data.class_data.members, name);
                        if (ctors) {
                            for (slist_t *c = ctors; c; c = c->next) {
                                symbol_t *ctor = (symbol_t *)c->data;
                                if (ctor && ctor->kind == SYM_CONSTRUCTOR) {
                                    resolved_method = ctor;
                                    break;  /* Take first matching constructor */
                                }
                            }
                            slist_free(ctors);
                        }
                    }
                }
                if (!resolved_method) {
                    semantic_error(sem, ref->line, ref->column,
                        "Cannot find constructor for %s::new", name);
                    return false;
                }
            } else {
                /* ClassName::method - look up the method */
                if (target_class_sym->data.class_data.members) {
                    resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
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
        if (!target_class_type || target_class_type->kind != TYPE_CLASS) {
            semantic_error(sem, ref->line, ref->column,
                "Method reference target must be a class type");
            return false;
        }
        
        target_class_sym = target_class_type->data.class_type.symbol;
        
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
        
        /* Find the instance method */
        if (target_class_sym && target_class_sym->data.class_data.members) {
            resolved_method = scope_lookup_method(target_class_sym->data.class_data.members, method_name);
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
        
        /* Skip if superclass already resolved */
        if (sym->data.class_data.superclass) {
            goto process_children;
        }
        
        /* Process extends and implements from children */
        slist_t *children = ast->data.node.children;
        while (children) {
            ast_node_t *child = (ast_node_t *)children->data;
            if (child->type == AST_CLASS_TYPE ||
                child->type == AST_PRIMITIVE_TYPE ||
                child->type == AST_ARRAY_TYPE) {
                if (child->data.node.flags == 1 && !sym->data.class_data.superclass) {
                    /* extends - set superclass */
                    const char *super_name = child->data.node.name;
                    symbol_t *super_sym = scope_lookup(sem->global_scope, super_name);
                    if (!super_sym || (super_sym->kind != SYM_CLASS && 
                                       super_sym->kind != SYM_RECORD)) {
                        /* Try same package first */
                        if (sem->current_package) {
                            char same_package[512];
                            snprintf(same_package, sizeof(same_package), "%s.%s",
                                     sem->current_package, super_name);
                            super_sym = scope_lookup(sem->global_scope, same_package);
                            if (!super_sym || (super_sym->kind != SYM_CLASS && 
                                               super_sym->kind != SYM_RECORD)) {
                                super_sym = load_external_class(sem, same_package);
                            }
                        }
                        if (!super_sym || (super_sym->kind != SYM_CLASS && 
                                           super_sym->kind != SYM_RECORD)) {
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
                    if (super_sym && (super_sym->kind == SYM_CLASS ||
                                      super_sym->kind == SYM_RECORD)) {
                        sym->data.class_data.superclass = super_sym;
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
                    
                    case AST_BLOCK:
                        {
                            /* Create block scope */
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
                                        
                                        /* For array initializers, set the target type */
                                        if (init_expr->type == AST_ARRAY_INIT && actual_type) {
                                            init_expr->sem_type = actual_type;
                                        }
                                        
                                        /* Handle lambda/method reference with target typing */
                                        if (init_expr->type == AST_LAMBDA_EXPR) {
                                            if (bind_lambda_to_target_type(sem, init_expr, actual_type)) {
                                                /* Lambda bound successfully, skip normal type check */
                                                children = children->next;
                                                continue;
                                            }
                                        } else if (init_expr->type == AST_METHOD_REF) {
                                            if (bind_method_ref_to_target_type(sem, init_expr, actual_type)) {
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
                                        if (actual_type && !type_assignable(actual_type, init_type)) {
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
                                bool is_void_method = expected && type_equals(expected, type_void());
                                
                                slist_t *children = node->data.node.children;
                                if (children) {
                                    /* Return with value */
                                    type_t *actual = get_expression_type(sem, children->data);
                                    
                                    if (is_void_method) {
                                        /* Cannot return a value from void method */
                                        semantic_error(sem, node->line, node->column,
                                            "cannot return a value from method whose result type is void");
                                    } else if (expected && !type_assignable(expected, actual)) {
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
                                    }
                                }
                            }
                            /* Check for finally flag on the try node */
                            if (node->data.node.extra) {
                                has_finally = true;
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
                                
                                if (!type_assignable(left_type, right_type)) {
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
                            /* Lambda expression - set up lambda scope if target type is known */
                            if (node->sem_type && node->sem_type->kind == TYPE_CLASS) {
                                /* Create a lambda scope using the type's interface symbol */
                                scope_t *lambda_scope = scope_new(SCOPE_BLOCK, sem->current_scope);
                                frame->saved_scope = sem->current_scope;
                                frame->extra = lambda_scope;  /* Remember to free */
                                sem->current_scope = lambda_scope;
                                
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
                             * This ensures the type node's sem_type is set for codegen. */
                            if (!node->sem_type) {
                                type_t *obj_type = get_expression_type(sem, node);
                                if (obj_type) {
                                    node->sem_type = obj_type;
                                }
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
                                                    
                                                    /* Look up enum constant by iterating through enum's AST */
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
                                                                    /* Found - store ordinal on case expression */
                                                                    case_expr->sem_type = sel_type;
                                                                    case_expr->data.leaf.value.int_val = ordinal;
                                                                    found = true;
                                                                    break;
                                                                }
                                                                ordinal++;
                                                            }
                                                        }
                                                    }
                                                    if (!found) {
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
                                                if (sym && sym->kind == SYM_FIELD) {
                                                    /* Check if it's final (constant expression) */
                                                    if (!(sym->modifiers & MOD_FINAL)) {
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
                                if (cond_type && cond_type->kind != TYPE_BOOLEAN) {
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
                    case AST_SWITCH_RULE:
                        /* Restore scope if we created one */
                        if (frame->saved_scope) {
                            sem->current_scope = frame->saved_scope;
                        }
                        /* Note: We don't free block scopes here because captured variables
                         * in local classes hold references to symbols defined in these scopes.
                         * The scopes will be freed when semantic_free is called. */
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
    
    sem->source = source;
    sem->error_count = 0;
    sem->warning_count = 0;
    
    /* Reset state */
    sem->current_scope = sem->global_scope;
    sem->current_class = NULL;
    sem->current_method = NULL;
    free(sem->current_package);
    sem->current_package = NULL;
    
    /* Pass 1: Collect all declarations */
    pass1_collect_declarations(sem, ast);
    
    /* Pass 1.5: Resolve type hierarchy (superclass/interfaces) */
    /* This handles forward references where class A extends B and B is defined later */
    resolve_type_hierarchy(sem, ast);
    
    /* Pass 2: Resolve names and check types */
    pass2_check_types(sem, ast);
    
    return sem->error_count == 0;
}

