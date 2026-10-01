/*
 * genesis.c
 * Main entry point for the genesis Java compiler
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "genesis.h"
#include "classpath.h"
#include "classfile.h"
#include "codegen.h"
#include "encoding.h"
#include "jarwriter.h"

#include <limits.h>    /* For PATH_MAX */
#include <sys/stat.h>  /* For mkdir() */
#include <sys/time.h>  /* For gettimeofday() */
#include <sys/wait.h>  /* For waitpid() */
#include <poll.h>      /* For poll() */
#include <signal.h>    /* For SIGPIPE */
#include <pthread.h>   /* For parallel compilation */

/* ========================================================================
 * Phase timing (GENESIS_DEBUG_TIMING)
 * ======================================================================== */

static double g_timing_last = 0.0;

static double timing_now(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;
}

/**
 * With GENESIS_DEBUG_TIMING set, report on stderr the wall-clock time since
 * the previous call (or since timing_start()) against the given phase name.
 */
static void timing_mark(const char *phase)
{
    if (!g_debug_timing) {
        return;
    }
    double now = timing_now();
    fprintf(stderr, "timing: %-28s %8.1f ms\n", phase, (now - g_timing_last) * 1000.0);
    g_timing_last = now;
}

static void timing_start(void)
{
    if (g_debug_timing) {
        g_timing_last = timing_now();
    }
}

/* ========================================================================
 * Parallel Compilation Infrastructure
 * ======================================================================== */

/* Get number of CPU cores */
static int get_cpu_count(void)
{
#ifdef _SC_NPROCESSORS_ONLN
    int count = (int)sysconf(_SC_NPROCESSORS_ONLN);
    return count > 0 ? count : 1;
#else
    return 4;  /* Default fallback */
#endif
}

/* ========================================================================
 * Compiler options
 * ======================================================================== */

/**
 * Create new compiler options with default values.
 */
compiler_options_t *compiler_options_new(void)
{
    compiler_options_t *opts = malloc(sizeof(compiler_options_t));
    if (!opts) {
        return NULL;
    }
    
    opts->source_version = strdup("25");
    opts->target_version = NULL;  /* Unset: computed per class from its features */
    opts->output_dir = NULL;
    opts->output_jar = NULL;
    opts->main_class = NULL;
    opts->classpath = NULL;
    opts->sourcepath = NULL;
    opts->source_files = NULL;
    opts->verbose = false;
    opts->debug_info = true;
    opts->warnings = true;
    opts->werror = false;
    opts->jobs = -1;  /* -1 = auto-detect thread count (parallel default) */
    
    return opts;
}

/**
 * Free compiler options.
 */
void compiler_options_free(compiler_options_t *opts)
{
    if (!opts) {
        return;
    }
    
    free(opts->source_version);
    free(opts->target_version);
    free(opts->output_dir);
    free(opts->output_jar);
    free(opts->main_class);
    free(opts->classpath);
    free(opts->sourcepath);
    slist_free(opts->source_files);
    free(opts);
}

/* ========================================================================
 * Source file handling
 * ======================================================================== */

/**
 * Create a new source file object.
 */
source_file_t *source_file_new(const char *filename)
{
    source_file_t *src = malloc(sizeof(source_file_t));
    if (!src) {
        return NULL;
    }
    
    src->filename = strdup(filename);
    src->contents = NULL;
    src->length = 0;
    
    return src;
}

/**
 * Free a source file object.
 */
void source_file_free(source_file_t *src)
{
    if (!src) {
        return;
    }
    
    free(src->filename);
    free(src->contents);
    free(src);
}

/**
 * Load source file contents from disk.
 * 
 * Detects encoding via BOM (defaults to UTF-8 if no BOM).
 * Supports UTF-8, UTF-16 (BE/LE), and UTF-32 (BE/LE).
 * Validates UTF-8 encoding and reports errors for invalid sequences.
 */
bool source_file_load(source_file_t *src)
{
    if (!src || !src->filename) {
        return false;
    }
    
    /* Read raw file contents */
    size_t raw_length;
    char *raw_data = file_get_contents(src->filename, &raw_length);
    if (!raw_data) {
        return false;
    }
    
    /* Decode source file with encoding detection */
    encoding_result_t result;
    src->contents = decode_source_file((const uint8_t *)raw_data, raw_length,
                                       &src->length, &result);
    free(raw_data);
    
    if (!result.valid) {
        fprintf(stderr, "%s:%d:%d: error: %s\n",
                src->filename,
                result.error_line ? result.error_line : 1,
                result.error_column ? result.error_column : 1,
                result.error_msg ? result.error_msg : "invalid encoding");
        encoding_result_free(&result);
        return false;
    }
    
    encoding_result_free(&result);
    return src->contents != NULL;
}

/* ========================================================================
 * Compilation
 * ======================================================================== */

/* Global classpath for the compilation session */
static classpath_t *g_classpath = NULL;

/* Global tracking of compiled sources to avoid duplicate compilation */
static hashtable_t *g_compiled_sources = NULL;

/* Global compiler options for dependency compilation */
static compiler_options_t *g_opts = NULL;

/* Global JAR writer for JAR output mode */
static jar_writer_t *g_jar_writer = NULL;

/* Serializes additions to g_jar_writer from the code generation threads */
static pthread_mutex_t g_jar_output_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Note: Shared type cache for parallel compilation was removed due to complexity.
 * Cross-file source dependencies require a more sophisticated multi-phase approach.
 * Current parallel compilation works well for independent files or files that only
 * depend on classpath types (JAR/class files). */

/**
 * Initialize the classpath from JDK and user options.
 */
static bool init_classpath(compiler_options_t *opts)
{
    if (g_classpath) {
        return true;  /* Already initialized */
    }
    
    g_classpath = classpath_new();
    if (!g_classpath) {
        fprintf(stderr, "error: failed to create classpath\n");
        return false;
    }
    
    /* Detect and set up JDK */
    jdk_info_t *jdk = jdk_detect();
    if (jdk) {
        if (opts->verbose) {
            jdk_info_print(jdk);
        }
        
        if (!classpath_setup_jdk(g_classpath, jdk)) {
            if (opts->verbose) {
                printf("warning: could not set up JDK boot classpath\n");
            }
        }
        jdk_info_free(jdk);
    } else {
        if (opts->verbose) {
            printf("warning: no JDK detected, external classes will not be resolved\n");
        }
    }
    
    /* Add user classpath */
    if (opts->classpath) {
        classpath_add_path(g_classpath, opts->classpath, false);
    }
    
    /* Add output directory to classpath (for multi-file compilation) */
    if (opts->output_dir) {
        classpath_add(g_classpath, opts->output_dir);
    }
    
    /* Add current directory to classpath */
    classpath_add(g_classpath, ".");
    
    return true;
}

/**
 * Free the global classpath.
 */
static void free_classpath(void)
{
    if (g_classpath) {
        classpath_free(g_classpath);
        g_classpath = NULL;
    }
}

/* Forward declaration for dependency compilation */
static int compile_file(source_file_t *src, compiler_options_t *opts);

/**
 * Write a generated class file to output (file or JAR).
 * 
 * @param cg             Class generator with bytecode
 * @param qualified_name Fully qualified class name (e.g., "com.example.Foo$Inner")
 * @param opts           Compiler options
 * @return               true on success
 */
static bool output_class(class_gen_t *cg, const char *qualified_name, 
                         compiler_options_t *opts)
{
    /* Generate class bytes */
    size_t size;
    uint8_t *bytes = write_class_bytes(cg, &size);
    if (!bytes) {
        return false;
    }
    
    bool success = false;
    
    if (g_jar_writer) {
        /* JAR output mode. The writer is one shared stream, and code
         * generation runs on several threads: entries are added one at a
         * time. */
        pthread_mutex_lock(&g_jar_output_mutex);
        success = jar_writer_add_class(g_jar_writer, qualified_name, bytes, size);
        if (success && opts->verbose) {
            printf("Added to JAR: %s.class\n", qualified_name);
        }
        pthread_mutex_unlock(&g_jar_output_mutex);
    } else {
        /* Directory output mode. Every class goes to its own file, so
         * the code generation threads write without any locking. */
        const char *output_dir = opts->output_dir ? opts->output_dir : ".";
        char output_path[1024];
        
        /* Convert qualified name to path (com.example.Foo -> com/example/Foo.class) */
        char *path_name = strdup(qualified_name);
        for (char *p = path_name; *p; p++) {
            if (*p == '.') {
                *p = '/';
            }
        }
        
        snprintf(output_path, sizeof(output_path), "%s/%s.class", output_dir, path_name);
        
        /* Write file, with plain system calls: one class file is one
         * write, and stdio only adds a buffer allocation and an fstat() per
         * file to that. The package directory nearly always exists already
         * (one mkdir per path component per class adds up to thousands of
         * system calls on a large build), so the parent directories are
         * only created when the open fails for want of one. */
        int fd = open(output_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
        if (fd < 0 && errno == ENOENT) {
            char *last_slash = strrchr(output_path, '/');
            if (last_slash) {
                *last_slash = '\0';
                char *slash = output_path;
                while ((slash = strchr(slash + 1, '/')) != NULL) {
                    *slash = '\0';
                    mkdir(output_path, 0755);
                    *slash = '/';
                }
                mkdir(output_path, 0755);
                *last_slash = '/';
            }
            fd = open(output_path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
        }
        if (fd >= 0) {
            size_t written = 0;
            while (written < size) {
                ssize_t n = write(fd, bytes + written, size - written);
                if (n < 0 && errno == EINTR) {
                    continue;
                }
                if (n <= 0) {
                    break;
                }
                written += (size_t)n;
            }
            if (close(fd) == 0 && written == size) {
                success = true;
                if (opts->verbose) {
                    printf("Generated: %s\n", output_path);
                }
            } else {
                fprintf(stderr, "output_class: write failed for %s\n", output_path);
            }
        } else {
            fprintf(stderr, "output_class: open failed for %s\n", output_path);
        }
        
        free(path_name);
    }
    
    free(bytes);
    return success;
}

/**
 * Canonical form of a source path, used as the key for "already compiled
 * in this session": the same file can be named on the command line one
 * way (relative, or through a symlink) and reached through a -sourcepath
 * entry another way. Caller frees.
 */
static char *canonical_source_path(const char *source_path)
{
    char resolved[PATH_MAX];
    if (realpath(source_path, resolved)) {
        return strdup(resolved);
    }
    return strdup(source_path);
}

/**
 * Check if a source file has already been compiled in this session.
 */
bool is_source_compiled(const char *source_path)
{
    if (!g_compiled_sources || !source_path) {
        return false;
    }
    char *key = canonical_source_path(source_path);
    bool compiled = key && hashtable_lookup(g_compiled_sources, key) != NULL;
    free(key);
    return compiled;
}

/**
 * Record a source file as compiled (or being compiled) in this session.
 */
static void mark_source_compiled(const char *source_path)
{
    if (!source_path) {
        return;
    }
    if (!g_compiled_sources) {
        g_compiled_sources = hashtable_new();
    }
    char *key = canonical_source_path(source_path);
    if (key) {
        hashtable_insert(g_compiled_sources, key, (void *)1);
        free(key);
    }
}

/**
 * Compile a dependency source file if not already compiled.
 * This is called from semantic analysis when a class is loaded from source.
 */
bool compile_dependency(const char *source_path)
{
    if (!source_path || !g_opts) {
        return false;
    }
    
    /* Check if already compiled in this session */
    if (is_source_compiled(source_path)) {
        return true;
    }
    
    /* Mark as compiled before actually compiling to handle circular deps */
    mark_source_compiled(source_path);
    
    /* Create source file and compile */
    source_file_t *src = source_file_new(source_path);
    if (!src) {
        return false;
    }
    
    if (g_opts->verbose) {
        printf("  Compiling dependency: %s\n", source_path);
    }
    
    int result = compile_file(src, g_opts);
    source_file_free(src);
    
    return result == 0;
}

/* Forward declarations for recursive processing */
static void process_nested_classes(semantic_t *sem, class_gen_t *outer_cg, 
                                   symbol_t *outer_sym, compiler_options_t *opts,
                                   int target_major);
static void process_local_classes(semantic_t *sem, class_gen_t *outer_cg,
                                  compiler_options_t *opts, int target_major);
static void process_anonymous_classes(semantic_t *sem, class_gen_t *outer_cg,
                                      compiler_options_t *opts, int target_major);
static void collect_nest_members_recursive(semantic_t *sem, class_gen_t *scan_cg,
                                           const_pool_t *host_cp, slist_t **nest_members,
                                           int target_major);

/**
 * Recursively process nested classes (nested classes of nested classes, etc.)
 */
static void process_nested_classes(semantic_t *sem, class_gen_t *outer_cg, 
                                   symbol_t *outer_sym, compiler_options_t *opts,
                                   int target_major)
{
    for (slist_t *nested = outer_cg->nested_classes; nested; nested = nested->next) {
        ast_node_t *nested_decl = (ast_node_t *)nested->data;
        const char *nested_name = nested_decl->data.node.name;
        
        /* Look up nested class symbol in outer class's members */
        symbol_t *nested_sym = scope_lookup_local(
            outer_sym->data.class_data.members, nested_name);
        if (!nested_sym) {
            fprintf(stderr, "error: cannot find nested class symbol: %s\n", nested_name);
            continue;
        }
        
        /* Create class generator for nested class */
        class_gen_t *nested_cg = class_gen_new(sem, nested_sym);
        if (!nested_cg) {
            fprintf(stderr, "error: failed to create class generator for nested class\n");
            continue;
        }
        class_gen_set_target_version(nested_cg, target_major);
        
        /* Generate bytecode for nested class */
        if (codegen_class(nested_cg, nested_decl)) {
            if (!output_class(nested_cg, nested_sym->qualified_name, opts)) {
                fprintf(stderr, "error: failed to write nested class: %s\n", 
                        nested_sym->qualified_name);
            }
            
            /* Recursively process this nested class's own nested classes */
            process_nested_classes(sem, nested_cg, nested_sym, opts, target_major);
            
            /* Process local classes inside this nested class's methods */
            process_local_classes(sem, nested_cg, opts, target_major);
            
            /* Process anonymous classes inside this nested class's methods */
            process_anonymous_classes(sem, nested_cg, opts, target_major);
        } else {
            fprintf(stderr, "error: code generation failed for nested class: %s\n", nested_name);
        }
        
        class_gen_free(nested_cg);
    }
}

/**
 * Recursively process local classes (classes defined inside method bodies).
 * Handles local classes inside local classes (e.g., Local3 inside Local2.test2()).
 */
static void process_local_classes(semantic_t *sem, class_gen_t *outer_cg,
                                  compiler_options_t *opts, int target_major)
{
    for (slist_t *local = outer_cg->local_classes; local; local = local->next) {
        ast_node_t *local_decl = (ast_node_t *)local->data;
        const char *local_name = local_decl->data.node.name;
        
        /* Get symbol from AST node - set during semantic analysis */
        symbol_t *local_sym = local_decl->sem_symbol;
        
        if (!local_sym) {
            fprintf(stderr, "error: cannot find local class symbol: %s\n", local_name);
            continue;
        }
        
        /* Create class generator for local class */
        class_gen_t *local_cg = class_gen_new(sem, local_sym);
        if (!local_cg) {
            fprintf(stderr, "error: failed to create class generator for local class\n");
            continue;
        }
        class_gen_set_target_version(local_cg, target_major);
        
        /* Generate bytecode for local class */
        if (codegen_class(local_cg, local_decl)) {
            if (!output_class(local_cg, local_sym->qualified_name, opts)) {
                fprintf(stderr, "error: failed to write local class: %s\n",
                        local_sym->qualified_name);
            }
            
            /* Recursively process local classes inside this local class's methods */
            process_local_classes(sem, local_cg, opts, target_major);
            
            /* Process anonymous classes inside this local class's methods */
            process_anonymous_classes(sem, local_cg, opts, target_major);
        } else {
            fprintf(stderr, "error: code generation failed for local class: %s\n", local_name);
        }
        
        class_gen_free(local_cg);
    }
}

/**
 * Process anonymous classes.
 */
static void process_anonymous_classes(semantic_t *sem, class_gen_t *outer_cg,
                                      compiler_options_t *opts, int target_major)
{
    for (slist_t *anon = outer_cg->anonymous_classes; anon; anon = anon->next) {
        symbol_t *anon_sym = (symbol_t *)anon->data;
        
        if (!anon_sym || !anon_sym->data.class_data.anonymous_body) {
            fprintf(stderr, "error: invalid anonymous class symbol\n");
            continue;
        }
        
        /* Create class generator for anonymous class */
        class_gen_t *anon_cg = class_gen_new(sem, anon_sym);
        if (!anon_cg) {
            fprintf(stderr, "error: failed to create class generator for anonymous class\n");
            continue;
        }
        class_gen_set_target_version(anon_cg, target_major);
        
        /* Generate bytecode for anonymous class */
        if (codegen_anonymous_class(anon_cg, anon_sym)) {
            if (!output_class(anon_cg, anon_sym->qualified_name, opts)) {
                fprintf(stderr, "error: failed to write anonymous class: %s\n",
                        anon_sym->qualified_name);
            }
            
            /* Process local classes inside anonymous class methods */
            process_local_classes(sem, anon_cg, opts, target_major);
            
            /* Recursively process anonymous classes inside anonymous class methods */
            process_anonymous_classes(sem, anon_cg, opts, target_major);
        } else {
            fprintf(stderr, "error: code generation failed for anonymous class: %s\n", 
                    anon_sym->qualified_name);
        }
        
        class_gen_free(anon_cg);
    }
}

/**
 * The JVM's nest-based access control (Java 11+) requires the nest HOST
 * (always the outermost top-level class, however deep the nesting) to
 * declare every nest member - every nested, local and anonymous class at
 * every depth - in its own NestMembers attribute; each member in turn
 * declares the host in its own NestHost attribute. Without a class
 * actually being listed there, a JVM refuses private-member access between
 * it and the host at runtime (IllegalAccessError: "current type is not
 * listed as a nest member"), even though the classfile otherwise verifies
 * fine - nest membership isn't checked by the bytecode verifier, only by
 * the access-control check the first time such an access actually runs.
 *
 * The host's own classfile bytes are finalized and written by
 * output_class() immediately after codegen_class() returns, but nested,
 * local and anonymous classes below the *first* level are only discovered
 * later, by the process_nested_classes()/process_local_classes()/
 * process_anonymous_classes() calls that follow - each of which discovers
 * one more level down only once it runs. So by the time any of those
 * deeper descendants are known, the host's own bytes (and its
 * NestMembers attribute) have already been written out.
 *
 * This walks the same discovery structure those functions do - reusing
 * codegen_class()/codegen_anonymous_class() themselves on throwaway,
 * never-written class_gen_t instances purely to populate each level's own
 * nested_classes/local_classes/anonymous_classes lists exactly the way
 * the real pass later will - to build the host's complete, correct
 * nest_members list before the host itself is ever written out.
 */
static void collect_nest_members_recursive(semantic_t *sem, class_gen_t *scan_cg,
                                           const_pool_t *host_cp, slist_t **nest_members,
                                           int target_major)
{
    for (slist_t *anon = scan_cg->anonymous_classes; anon; anon = anon->next) {
        symbol_t *anon_sym = (symbol_t *)anon->data;
        if (!anon_sym || !anon_sym->data.class_data.anonymous_body ||
            !anon_sym->qualified_name) {
            continue;
        }
        char *internal = class_to_internal_name(anon_sym->qualified_name);
        uint16_t *idx = malloc(sizeof(uint16_t));
        *idx = cp_add_class(host_cp, internal);
        free(internal);
        if (*nest_members) {
            slist_append(*nest_members, idx);
        } else {
            *nest_members = slist_new(idx);
        }

        class_gen_t *tmp_cg = class_gen_new(sem, anon_sym);
        if (tmp_cg) {
            class_gen_set_target_version(tmp_cg, target_major);
            if (codegen_anonymous_class(tmp_cg, anon_sym)) {
                collect_nest_members_recursive(sem, tmp_cg, host_cp, nest_members, target_major);
            }
            class_gen_free(tmp_cg);
        }
    }

    for (slist_t *local = scan_cg->local_classes; local; local = local->next) {
        ast_node_t *local_decl = (ast_node_t *)local->data;
        symbol_t *local_sym = local_decl->sem_symbol;
        if (!local_sym || !local_sym->qualified_name) {
            continue;
        }
        char *internal = class_to_internal_name(local_sym->qualified_name);
        uint16_t *idx = malloc(sizeof(uint16_t));
        *idx = cp_add_class(host_cp, internal);
        free(internal);
        if (*nest_members) {
            slist_append(*nest_members, idx);
        } else {
            *nest_members = slist_new(idx);
        }

        class_gen_t *tmp_cg = class_gen_new(sem, local_sym);
        if (tmp_cg) {
            class_gen_set_target_version(tmp_cg, target_major);
            if (codegen_class(tmp_cg, local_decl)) {
                collect_nest_members_recursive(sem, tmp_cg, host_cp, nest_members, target_major);
            }
            class_gen_free(tmp_cg);
        }
    }

    for (slist_t *nested = scan_cg->nested_classes; nested; nested = nested->next) {
        ast_node_t *nested_decl = (ast_node_t *)nested->data;
        const char *nested_name = nested_decl->data.node.name;
        symbol_t *nested_sym = scan_cg->class_sym && scan_cg->class_sym->data.class_data.members ?
            scope_lookup_local(scan_cg->class_sym->data.class_data.members, nested_name) : NULL;
        if (!nested_sym || !nested_sym->qualified_name) {
            continue;
        }
        char *internal = class_to_internal_name(nested_sym->qualified_name);
        uint16_t *idx = malloc(sizeof(uint16_t));
        *idx = cp_add_class(host_cp, internal);
        free(internal);
        if (*nest_members) {
            slist_append(*nest_members, idx);
        } else {
            *nest_members = slist_new(idx);
        }

        class_gen_t *tmp_cg = class_gen_new(sem, nested_sym);
        if (tmp_cg) {
            class_gen_set_target_version(tmp_cg, target_major);
            if (codegen_class(tmp_cg, nested_decl)) {
                collect_nest_members_recursive(sem, tmp_cg, host_cp, nest_members, target_major);
            }
            class_gen_free(tmp_cg);
        }
    }
}

/**
 * Compile a single source file.
 */
static int compile_file(source_file_t *src, compiler_options_t *opts)
{
    if (opts->verbose) {
        printf("Compiling %s\n", src->filename);
    }
    
    /* Load source file */
    if (!source_file_load(src)) {
        fprintf(stderr, "error: cannot read file: %s\n", src->filename);
        return 1;
    }
    
    /* Lexical analysis - feedforward tokenization */
    int source_version = classfile_java_version(
        classfile_version_from_string(opts->source_version));
    lexer_t *lexer = lexer_new(src, source_version);
    if (!lexer) {
        fprintf(stderr, "error: failed to create lexer for: %s\n", src->filename);
        return 1;
    }
    
    /* Check for initial lexer error */
    if (lexer_type(lexer) == TOK_ERROR) {
        fprintf(stderr, "%s:%d:%d: error: %s\n",
                src->filename, lexer_line(lexer), lexer_column(lexer),
                lexer_text(lexer));
        lexer_free(lexer);
        return 1;
    }
    
    /* Parsing (syntax analysis) */
    parser_t *parser = parser_new(lexer, src);
    if (!parser) {
        fprintf(stderr, "error: failed to create parser for: %s\n", src->filename);
        lexer_free(lexer);
        return 1;
    }
    
    ast_node_t *ast = parser_parse(parser);
    
    /* Check for parser errors */
    if (parser->error_msg) {
        fprintf(stderr, "%s:%d:%d: error: %s\n",
                src->filename, parser->error_line, parser->error_column,
                parser->error_msg);
        parser_free(parser);
        lexer_free(lexer);
        /* Don't free AST - it may be referenced by symbols from other compilation units */
        /* ast_free(ast); */
        return 1;
    }
    
    parser_free(parser);
    lexer_free(lexer);
    
    if (!ast) {
        fprintf(stderr, "error: failed to parse: %s\n", src->filename);
        return 1;
    }
    
    /* Resolve type names to fully qualified names immediately after parsing.
     * This ensures all type references use consistent qualified names. */
    slist_t *sp_list = sourcepath_parse(opts->sourcepath);
    resolve_types_in_compilation_unit(ast, g_classpath, sp_list, NULL);
    sourcepath_list_free(sp_list);
    
    if (opts->verbose) {
        /* Print AST for debugging */
        printf("AST for %s:\n", src->filename);
        ast_print(ast, 0);
        printf("\n");
    }
    
    /* Semantic analysis */
    semantic_t *sem = semantic_new(g_classpath);
    if (!sem) {
        fprintf(stderr, "error: failed to create semantic analyzer\n");
        /* Don't free AST - it may be referenced by symbols from other compilation units */
        /* ast_free(ast); */
        return 1;
    }
    
    /* Set warning options */
    sem->warnings_enabled = opts->warnings;
    sem->werror = opts->werror;
    
    /* Set source version for feature validation */
    sem->source_version = classfile_java_version(
        classfile_version_from_string(opts->source_version));
    
    /* Set sourcepath if provided */
    if (opts->sourcepath) {
        semantic_set_sourcepath(sem, opts->sourcepath);
    }
    
    bool sem_ok = semantic_analyze(sem, ast, src);
    
    if (sem->error_count > 0 || sem->warning_count > 0) {
        semantic_print_diagnostics(sem);
    }
    
    if (!sem_ok) {
        fprintf(stderr, "%d error(s)\n", sem->error_count);
        semantic_free(sem);
        /* Don't free AST - it may be referenced by symbols from other compilation units */
        /* ast_free(ast); */
        return 1;
    }
    
    if (opts->verbose) {
        printf("Semantic analysis completed: %d error(s), %d warning(s)\n",
               sem->error_count, sem->warning_count);
    }
    
    /* Compile source dependencies discovered during semantic analysis.
     * These are source files that were loaded for type checking but
     * need to be compiled to .class files. */
    for (slist_t *dep = sem->source_dependencies; dep; dep = dep->next) {
        const char *dep_path = (const char *)dep->data;
        if (dep_path && !is_source_compiled(dep_path)) {
            compile_dependency(dep_path);
        }
    }
    
    /* Code generation */
    if (ast->type == AST_COMPILATION_UNIT) {
        /* Check if this is a package-info.java file */
        bool is_package_info = false;
        const char *base_name = strrchr(src->filename, '/');
        base_name = base_name ? base_name + 1 : src->filename;
        if (strcmp(base_name, "package-info.java") == 0) {
            is_package_info = true;
        }
        
        /* Find class/module declarations and generate bytecode */
        slist_t *children = ast->data.node.children;
        
        /* For package-info.java, look for package declaration with annotations */
        if (is_package_info) {
            ast_node_t *package_decl = NULL;
            for (slist_t *c = children; c; c = c->next) {
                ast_node_t *child = (ast_node_t *)c->data;
                if (child && child->type == AST_PACKAGE_DECL) {
                    package_decl = child;
                    break;
                }
            }
            
            if (package_decl) {
                int target_major = classfile_version_from_string(opts->target_version);
                size_t size;
                uint8_t *bytes = codegen_package_info(package_decl, NULL, sem, target_major, &size);
                if (bytes) {
                    bool success = false;
                    const char *pkg_name = package_decl->data.node.name;
                    
                    /* Build qualified class name: com.example -> com/example/package-info */
                    size_t name_len = pkg_name ? strlen(pkg_name) : 0;
                    char *qname = malloc(name_len + 20);
                    if (pkg_name) {
                        strcpy(qname, pkg_name);
                        for (char *p = qname; *p; p++) {
                            if (*p == '.') {
                                *p = '/';
                            }
                        }
                        strcat(qname, "/package-info");
                    } else {
                        strcpy(qname, "package-info");
                    }
                    
                    if (g_jar_writer) {
                        /* JAR output mode */
                        success = jar_writer_add_class(g_jar_writer, qname, bytes, size);
                        if (success && opts->verbose) {
                            printf("Added to JAR: %s.class\n", qname);
                        }
                    } else {
                        /* Directory output mode */
                        const char *output_dir = opts->output_dir ? opts->output_dir : ".";
                        char output_path[1024];
                        snprintf(output_path, sizeof(output_path), "%s/%s.class", output_dir, qname);
                        
                        /* Create subdirectories if needed */
                        char *dir_path = strdup(output_path);
                        char *last_slash = strrchr(dir_path, '/');
                        if (last_slash) {
                            *last_slash = '\0';
                            char *p = dir_path;
                            while (*p) {
                                if (*p == '/') {
                                    *p = '\0';
                                    mkdir(dir_path, 0755);
                                    *p = '/';
                                }
                                p++;
                            }
                            mkdir(dir_path, 0755);
                        }
                        free(dir_path);
                        
                        FILE *fp = fopen(output_path, "wb");
                        if (fp) {
                            if (fwrite(bytes, 1, size, fp) == size) {
                                success = true;
                                if (opts->verbose) {
                                    printf("Generated: %s\n", output_path);
                                }
                            }
                            fclose(fp);
                        }
                    }
                    
                    if (!success) {
                        fprintf(stderr, "error: failed to write package-info.class\n");
                    }
                    free(bytes);
                    free(qname);
                } else {
                    fprintf(stderr, "error: failed to generate package-info.class\n");
                }
            }
            
            /* package-info.java files don't have type declarations */
            semantic_free(sem);
            /* Don't free AST - it may be referenced by symbols from other compilation units */
            /* ast_free(ast); */
            return 0;
        }
        
        while (children) {
            ast_node_t *child = (ast_node_t *)children->data;
            
            /* Handle module declaration (module-info.java) */
            if (child->type == AST_MODULE_DECL) {
                size_t size;
                uint8_t *bytes = codegen_module(child, &size);
                if (bytes) {
                    bool success = false;
                    
                    if (g_jar_writer) {
                        /* JAR output mode */
                        success = jar_writer_add_class(g_jar_writer, "module-info", bytes, size);
                        if (success && opts->verbose) {
                            printf("Added to JAR: module-info.class\n");
                        }
                    } else {
                        /* Directory output mode */
                        const char *output_dir = opts->output_dir ? opts->output_dir : ".";
                        char output_path[1024];
                        snprintf(output_path, sizeof(output_path), "%s/module-info.class", output_dir);
                        
                        FILE *fp = fopen(output_path, "wb");
                        if (fp) {
                            if (fwrite(bytes, 1, size, fp) == size) {
                                success = true;
                                if (opts->verbose) {
                                    printf("Generated: %s\n", output_path);
                                }
                            }
                            fclose(fp);
                        }
                    }
                    
                    if (!success) {
                        fprintf(stderr, "error: failed to write module-info.class\n");
                    }
                    free(bytes);
                } else {
                    fprintf(stderr, "error: failed to generate module-info.class\n");
                }
                
                children = children->next;
                continue;
            }
            
            if (child->type == AST_CLASS_DECL ||
                child->type == AST_INTERFACE_DECL ||
                child->type == AST_ENUM_DECL ||
                child->type == AST_RECORD_DECL ||
                child->type == AST_ANNOTATION_DECL) {
                
                /* Get the class name */
                const char *class_name = child->data.node.name;
                if (!class_name) {
                    children = children->next;
                    continue;
                }
                
                /* Look up the class symbol */
                symbol_t *class_sym = scope_lookup(sem->global_scope, class_name);
                if (!class_sym) {
                    fprintf(stderr, "error: cannot find class symbol: %s\n", class_name);
                    children = children->next;
                    continue;
                }
                
                /* Create class generator */
                class_gen_t *cg = class_gen_new(sem, class_sym);
                if (!cg) {
                    fprintf(stderr, "error: failed to create class generator\n");
                    children = children->next;
                    continue;
                }
                
                /* Set target class file version */
                int target_major = classfile_version_from_string(opts->target_version);
                class_gen_set_target_version(cg, target_major);
                
                /* Generate bytecode */
                if (codegen_class(cg, child)) {
                    /* Discover every nested/local/anonymous class at every
                     * depth below this one and add them all to this class's
                     * own NestMembers attribute before it's written out -
                     * see collect_nest_members_recursive()'s own comment. */
                    if (cg->nest_host == 0) {
                        collect_nest_members_recursive(sem, cg, cg->cp, &cg->nest_members, target_major);
                    }

                    /* A descendant discovered during that same recursive scan
                     * may have registered a synthetic field-accessor need on
                     * THIS class (see pending_field_accessor_t's own comment,
                     * genesis.h) - after codegen_class() already returned, so
                     * emit it now, before this class's bytes are finalized. */
                    generate_pending_field_accessors(cg);

                    /* Output the class */
                    const char *qname = class_sym->qualified_name ?
                                        class_sym->qualified_name : class_name;
                    if (!output_class(cg, qname, opts)) {
                        fprintf(stderr, "error: failed to write class file: %s\n", qname);
                    }
                    
                    /* Process nested classes recursively */
                    process_nested_classes(sem, cg, class_sym, opts, target_major);
                    
                    /* Process local classes (defined inside method bodies) - recursively */
                    process_local_classes(sem, cg, opts, target_major);
                    
                    /* Process anonymous classes - recursively */
                    process_anonymous_classes(sem, cg, opts, target_major);
                } else {
                    fprintf(stderr, "error: code generation failed for: %s\n", class_name);
                }
                
                class_gen_free(cg);
            }
            
            children = children->next;
        }
    }
    
    semantic_free(sem);
    /* Don't free AST - it may be referenced by symbols from other compilation units */
    /* ast_free(ast); */
    return 0;
}

/* ========================================================================
 * Parallel Compilation Implementation
 * ======================================================================== */

/* Result of parsing a single file */
typedef struct parse_result {
    char *filename;
    source_file_t *source;
    ast_node_t *ast;
    char *error_msg;
    int error_line;
    int error_column;
    semantic_t *sem;  /* Semantic analyzer (set after serial semantic phase) */
    bool sem_failed;  /* Semantic analysis reported errors: no code generation */
} parse_result_t;

/* Mutex-protected counter shared between worker threads (C99 has no atomics) */
typedef struct shared_counter {
    pthread_mutex_t mutex;
    int value;
} shared_counter_t;

static void counter_init(shared_counter_t *c)
{
    pthread_mutex_init(&c->mutex, NULL);
    c->value = 0;
}

static void counter_destroy(shared_counter_t *c)
{
    pthread_mutex_destroy(&c->mutex);
}

/* Returns the value before the increment */
static int counter_fetch_add(shared_counter_t *c, int n)
{
    int old;

    pthread_mutex_lock(&c->mutex);
    old = c->value;
    c->value += n;
    pthread_mutex_unlock(&c->mutex);
    return old;
}

static int counter_load(shared_counter_t *c)
{
    int v;

    pthread_mutex_lock(&c->mutex);
    v = c->value;
    pthread_mutex_unlock(&c->mutex);
    return v;
}

/* Shared state for parallel parsing phase */
typedef struct parse_phase_state {
    char **filenames;
    int file_count;
    shared_counter_t next_index;
    parse_result_t *results;
    int source_version;
} parse_phase_state_t;

/* Shared state for parallel codegen phase */
typedef struct codegen_phase_state {
    parse_result_t *results;
    int file_count;
    shared_counter_t next_index;
    shared_counter_t error_count;
    compiler_options_t *opts;
    pthread_mutex_t output_mutex;
    type_registry_t *registry;  /* Shared type registry for cross-file resolution */
} codegen_phase_state_t;

/**
 * Extract package name from a compilation unit AST.
 */
static const char *get_package_from_ast(ast_node_t *ast)
{
    if (!ast || ast->type != AST_COMPILATION_UNIT) {
        return NULL;
    }
    
    slist_t *children = ast->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        if (child && child->type == AST_PACKAGE_DECL) {
            return child->data.node.name;
        }
        children = children->next;
    }
    return NULL;
}

/**
 * Create a member scope for an interface stub, extracting method names from AST.
 * This allows @Override checks to find interface methods.
 * Only call this for interfaces - classes/enums get full member population
 * during semantic analysis.
 */
static void populate_interface_stub_methods(symbol_t *sym, ast_node_t *decl)
{
    if (!sym || !decl || !decl->data.node.children || sym->kind != SYM_INTERFACE) {
        return;
    }
    
    /* Create member scope */
    sym->data.class_data.members = scope_new(SCOPE_CLASS, NULL);
    if (!sym->data.class_data.members) {
        return;
    }
    
    /* Walk AST children looking for method declarations */
    for (slist_t *child = decl->data.node.children; child; child = child->next) {
        ast_node_t *member = (ast_node_t *)child->data;
        if (!member) {
            continue;
        }
        
        if (member->type == AST_METHOD_DECL) {
            const char *name = member->data.node.name;
            if (name) {
                /* Create method stub - include return type for lazy resolution */
                symbol_t *method_sym = calloc(1, sizeof(symbol_t));
                if (method_sym) {
                    method_sym->kind = SYM_METHOD;
                    method_sym->name = strdup(name);
                    method_sym->modifiers = member->data.node.flags;
                    method_sym->ast = member;  /* Keep AST for type resolution */
                    /* Interface methods are implicitly public abstract (unless static/default) */
                    if (!(method_sym->modifiers & MOD_PRIVATE)) {
                        method_sym->modifiers |= MOD_PUBLIC;
                    }
                    if (!(method_sym->modifiers & MOD_STATIC) && 
                        !(method_sym->modifiers & MOD_DEFAULT)) {
                        method_sym->modifiers |= MOD_ABSTRACT;
                    }
                    scope_define(sym->data.class_data.members, method_sym);
                }
            }
        }
    }
}

/**
 * Create a minimal symbol entry for a type declaration.
 * This creates a stub symbol that will be fully populated during semantic analysis.
 */
static symbol_t *create_type_stub(const char *qname, ast_node_t *decl, 
                                  const char *package)
{
    symbol_t *sym = calloc(1, sizeof(symbol_t));
    if (!sym) {
        return NULL;
    }
    
    /* Set basic symbol info */
    switch (decl->type) {
        case AST_INTERFACE_DECL:
        case AST_ANNOTATION_DECL:
            sym->kind = SYM_INTERFACE;
            break;
        case AST_ENUM_DECL:
            sym->kind = SYM_ENUM;
            break;
        case AST_RECORD_DECL:
            sym->kind = SYM_RECORD;
            break;
        default:
            sym->kind = SYM_CLASS;
            break;
    }
    
    sym->name = strdup(decl->data.node.name);
    sym->qualified_name = strdup(qname);
    
    /* Store package for type resolution */
    if (package) {
        sym->data.class_data.package = strdup(package);
    }
    /* Don't store AST reference - the symbol is just a type stub,
     * the real symbol with AST will be created by the file's own
     * semantic analyzer */
    sym->ast = NULL;
    
    /* Extract modifiers from AST - modifiers are stored in node.flags */
    sym->modifiers = decl->data.node.flags;
    
    /* Create a type for this symbol */
    type_t *type = type_new_class(qname);
    if (type) {
        type->data.class_type.symbol = sym;
        sym->type = type;
    }
    
    /* Extract type parameters (generics like <E>, <K,V>) from AST children */
    if (decl->data.node.children) {
        for (slist_t *child = decl->data.node.children; child; child = child->next) {
            ast_node_t *member = (ast_node_t *)child->data;
            if (member && member->type == AST_TYPE_PARAMETER && member->data.node.name) {
                /* Create a symbol for the type parameter */
                symbol_t *tparam_sym = calloc(1, sizeof(symbol_t));
                if (tparam_sym) {
                    tparam_sym->kind = SYM_TYPE_PARAM;
                    tparam_sym->name = strdup(member->data.node.name);
                    tparam_sym->ast = member;
                    
                    /* Create a type variable type (TYPE_TYPEVAR) */
                    type_t *tvar_type = malloc(sizeof(type_t));
                    if (tvar_type) {
                        tvar_type->kind = TYPE_TYPEVAR;
                        tvar_type->data.type_var.name = strdup(member->data.node.name);
                        tvar_type->data.type_var.bound = NULL;
                        tparam_sym->type = tvar_type;
                    }
                    
                    /* Add to type_params list */
                    if (!sym->data.class_data.type_params) {
                        sym->data.class_data.type_params = slist_new(tparam_sym);
                    } else {
                        slist_append(sym->data.class_data.type_params, tparam_sym);
                    }
                }
            }
        }
    }
    
    /* For interfaces, populate method stubs for @Override checks */
    if (sym->kind == SYM_INTERFACE) {
        populate_interface_stub_methods(sym, decl);
    }
    
    /* For enums, populate enum constants */
    if (sym->kind == SYM_ENUM && decl->data.node.children) {
        sym->data.class_data.members = scope_new(SCOPE_CLASS, NULL);
        if (sym->data.class_data.members) {
            int enum_ordinal = 0;
            for (slist_t *child = decl->data.node.children; child; child = child->next) {
                ast_node_t *member = (ast_node_t *)child->data;
                if (member && member->type == AST_ENUM_CONSTANT && member->data.node.name) {
                    symbol_t *const_sym = calloc(1, sizeof(symbol_t));
                    if (const_sym) {
                        const_sym->kind = SYM_FIELD;
                        const_sym->name = strdup(member->data.node.name);
                        const_sym->modifiers = MOD_PUBLIC | MOD_STATIC | MOD_FINAL;
                        const_sym->type = type;  /* Type is the enum itself */
                        const_sym->data.var_data.is_enum_constant = true;
                        const_sym->data.var_data.enum_ordinal = enum_ordinal++;
                        scope_define(sym->data.class_data.members, const_sym);
                    }
                }
            }
        }
    }
    
    return sym;
}

/**
 * Register a type declaration and any nested types into the registry.
 * This is a recursive function that handles nested classes.
 * Also sets the completer on each symbol for lazy member population.
 */
static void register_type_decl(type_registry_t *reg, ast_node_t *decl, 
                               const char *parent_qname, const char *package,
                               completer_context_t *completer_ctx)
{
    if (!decl || !decl->data.node.name) {
        return;
    }
    
    /* Build qualified name */
    char *qname;
    if (parent_qname) {
        /* Nested type: ParentClass$NestedClass */
        size_t len = strlen(parent_qname) + 1 + strlen(decl->data.node.name) + 1;
        qname = malloc(len);
        if (qname) {
            snprintf(qname, len, "%s$%s", parent_qname, decl->data.node.name);
        }
    } else if (package) {
        /* Top-level type in package */
        size_t len = strlen(package) + 1 + strlen(decl->data.node.name) + 1;
        qname = malloc(len);
        if (qname) {
            snprintf(qname, len, "%s.%s", package, decl->data.node.name);
        }
    } else {
        /* Default package */
        qname = strdup(decl->data.node.name);
    }
    
    if (!qname) {
        return;
    }
    
    /* Create symbol stub and register it */
    symbol_t *sym = create_type_stub(qname, decl, package);
    if (sym) {
        if (parent_qname) {
            symbol_t *parent_sym = type_registry_lookup(reg, parent_qname);
            if (parent_sym) {
                sym->data.class_data.enclosing_class = parent_sym;
                /* JLS 9.5: a member type of an interface is implicitly
                 * public and static, regardless of what the source
                 * actually wrote. This registry stub is exactly what a
                 * DIFFERENT file in the same compile batch resolves a
                 * cross-file reference to this type through - codegen.c's
                 * own codegen_class() already forces this for the type's
                 * OWN compiled classfile when the file declaring it is
                 * compiled directly, but a caller in another file builds
                 * its "does this nested class need an outer-this
                 * constructor argument" decision (codegen_expr.c's
                 * codegen_new_object(), checking target_sym->modifiers &
                 * MOD_STATIC) from THIS stub's modifiers, not from that
                 * other file's own compiled output - so leaving this
                 * stub's modifiers unmodified left every cross-file
                 * caller still assuming an inner (non-static) class
                 * layout, constructing a "new DirectoryChangeResult(...)"
                 * call site with a leading outer-instance argument that
                 * the actual (correctly-fixed) constructor no longer
                 * accepts: NoSuchMethodError at runtime. Confirmed
                 * against gumdrop's own FtpFileSystem (an interface)
                 * declaring "class DirectoryChangeResult { ... }",
                 * constructed from BasicFTPFileSystem in a different
                 * file/package. */
                if (parent_sym->kind == SYM_INTERFACE) {
                    sym->modifiers |= MOD_PUBLIC | MOD_STATIC;
                }
            }
        }
        /* Set the completer for lazy member population (javac-style) */
        if (completer_ctx) {
            symbol_set_completer(sym, class_symbol_completer, completer_ctx);
        }
        type_registry_register(reg, qname, sym, decl);
    }
    
    /* Recursively register nested types */
    if (decl->data.node.children) {
        for (slist_t *child = decl->data.node.children; child; child = child->next) {
            ast_node_t *member = (ast_node_t *)child->data;
            if (member && (member->type == AST_CLASS_DECL ||
                member->type == AST_INTERFACE_DECL ||
                member->type == AST_ENUM_DECL ||
                member->type == AST_RECORD_DECL ||
                member->type == AST_ANNOTATION_DECL)) {
                if (debug_getenv("GENESIS_DEBUG_REGISTRY")) {
                    fprintf(stderr, "DEBUG register: nested type '%s' in '%s'\n",
                            member->data.node.name ? member->data.node.name : "(null)",
                            qname ? qname : "(null)");
                }
                register_type_decl(reg, member, qname, package, completer_ctx);
            }
        }
    }
    
    free(qname);
}

/**
 * Register all types from an AST compilation unit into the shared registry.
 * This runs during Phase 2 (Type Entry) to make all types visible.
 * Sets completer on each type for lazy member population.
 */
static void register_types_from_ast(type_registry_t *reg, ast_node_t *ast,
                                    completer_context_t *completer_ctx)
{
    if (!reg || !ast || ast->type != AST_COMPILATION_UNIT) {
        return;
    }
    
    const char *package = get_package_from_ast(ast);
    
    /* Walk children looking for type declarations */
    slist_t *children = ast->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        
        if (child && (child->type == AST_CLASS_DECL ||
            child->type == AST_INTERFACE_DECL ||
            child->type == AST_ENUM_DECL ||
            child->type == AST_RECORD_DECL ||
            child->type == AST_ANNOTATION_DECL)) {
            
            /* Count children of the class decl */
            if (debug_getenv("GENESIS_DEBUG_REGISTRY")) {
                int child_count = 0;
                for (slist_t *c = child->data.node.children; c; c = c->next) {
                    child_count++;
                }
                fprintf(stderr, "DEBUG register: type '%s' has %d children (from package '%s', ast=%p, cu=%p)\n",
                        child->data.node.name ? child->data.node.name : "(null)", 
                        child_count,
                        package ? package : "(default)",
                        (void*)child, (void*)ast);
            }
            
            register_type_decl(reg, child, NULL, package, completer_ctx);
        }
        
        children = children->next;
    }
}

/**
 * Worker thread for Phase 1: Parallel parsing.
 * Each thread parses files independently - no shared state.
 */
static void *parse_phase_worker(void *arg)
{
    parse_phase_state_t *state = (parse_phase_state_t *)arg;
    
    while (1) {
        int idx = counter_fetch_add(&state->next_index, 1);
        if (idx >= state->file_count) {
            break;
        }
        
        char *filename = state->filenames[idx];
        parse_result_t *result = &state->results[idx];
        
        result->filename = filename;
        result->source = NULL;
        result->ast = NULL;
        result->error_msg = NULL;
        
        /* Create and load source file */
        result->source = source_file_new(filename);
        if (!result->source) {
            result->error_msg = strdup("cannot allocate source file");
            continue;
        }
        
        if (!source_file_load(result->source)) {
            result->error_msg = strdup("cannot read source file");
            continue;
        }
        
        /* Lexing */
        lexer_t *lexer = lexer_new(result->source, state->source_version);
        if (!lexer) {
            result->error_msg = strdup("failed to create lexer");
            continue;
        }
        
        if (lexer_type(lexer) == TOK_ERROR) {
            result->error_msg = strdup(lexer_text(lexer));
            result->error_line = lexer_line(lexer);
            result->error_column = lexer_column(lexer);
            lexer_free(lexer);
            continue;
        }
        
        /* Parsing */
        parser_t *parser = parser_new(lexer, result->source);
        if (!parser) {
            result->error_msg = strdup("failed to create parser");
            lexer_free(lexer);
            continue;
        }
        
        result->ast = parser_parse(parser);
        
        if (parser->error_msg) {
            result->error_msg = strdup(parser->error_msg);
            result->error_line = parser->error_line;
            result->error_column = parser->error_column;
        }
        
        parser_free(parser);
        lexer_free(lexer);
    }
    
    return NULL;
}

/* Shared state for the type name qualification phase (Phase 2b) */
typedef struct qualify_phase_state {
    parse_result_t *results;
    int file_count;
    shared_counter_t next_index;
    slist_t *sourcepath_list;
    type_registry_t *registry;
} qualify_phase_state_t;

/**
 * Worker thread for Phase 2b: qualify the type names of one file at a time.
 * A file's names are resolved from its own package and imports against the
 * registry (complete, and only read from now on), the classpath (which
 * locks its caches) and the sourcepath, and written into that file's own
 * AST: the files do not affect one another.
 *
 * Each thread remembers what it has asked the classpath and the sourcepath
 * (see resolve_types_in_compilation_unit_cached()). Without that the
 * threads ask the classpath the same questions file after file, and spend
 * their time queueing for its lock: the phase took longer on ten threads
 * than on one.
 */
static void *qualify_phase_worker(void *arg)
{
    qualify_phase_state_t *state = (qualify_phase_state_t *)arg;
    hashtable_t *probes = hashtable_new_sized(4096);

    while (1) {
        int idx = counter_fetch_add(&state->next_index, 1);
        if (idx >= state->file_count) {
            break;
        }
        parse_result_t *pr = &state->results[idx];
        if (pr->ast && !pr->error_msg) {
            resolve_types_in_compilation_unit_cached(pr->ast, g_classpath,
                                                     state->sourcepath_list,
                                                     state->registry, probes);
        }
    }

    hashtable_free(probes);
    return NULL;
}

/**
 * Generate and write the class files of one analyzed source file. Returns
 * the number of errors. `message_mutex`, when not NULL, serializes the
 * error messages of the code generation threads.
 */
static int generate_file_code(parse_result_t *pr, compiler_options_t *opts,
                              pthread_mutex_t *message_mutex)
{
    int errors = 0;

    /* Skip files with parse errors or semantic errors */
    if (pr->error_msg || !pr->ast || !pr->sem || pr->sem_failed) {
        return 0;
    }
    
    semantic_t *sem = pr->sem;
    
    /* Code generation for each top-level type */
    slist_t *children = pr->ast->data.node.children;
    while (children) {
        ast_node_t *child = (ast_node_t *)children->data;
        
        if (child && (child->type == AST_CLASS_DECL ||
            child->type == AST_INTERFACE_DECL ||
            child->type == AST_ENUM_DECL ||
            child->type == AST_RECORD_DECL ||
            child->type == AST_ANNOTATION_DECL)) {
            
            const char *class_name = child->data.node.name;
            if (!class_name) {
                children = children->next;
                continue;
            }
            
            /* Get class symbol */
            symbol_t *class_sym = child->sem_symbol;
            if (!class_sym && sem->current_class) {
                class_sym = sem->current_class;
            }
            
            class_gen_t *cg = class_gen_new(sem, class_sym);
            if (!cg) {
                if (message_mutex) {
                    pthread_mutex_lock(message_mutex);
                }
                fprintf(stderr, "error: cannot create class generator for %s\n",
                        class_name);
                if (message_mutex) {
                    pthread_mutex_unlock(message_mutex);
                }
                errors++;
                children = children->next;
                continue;
            }
            
            /* Target major version (0 = automatic); must be set before the
             * class is generated and written */
            int target_major = classfile_version_from_string(opts->target_version);
            class_gen_set_target_version(cg, target_major);
            
            if (!codegen_class(cg, child)) {
                if (message_mutex) {
                    pthread_mutex_lock(message_mutex);
                }
                fprintf(stderr, "error: code generation failed for: %s\n",
                        class_name);
                if (message_mutex) {
                    pthread_mutex_unlock(message_mutex);
                }
                errors++;
                class_gen_free(cg);
                children = children->next;
                continue;
            }
            
            /* Discover every nested/local/anonymous class at every
             * depth below this one and add them all to this class's
             * own NestMembers attribute before it's written out - see
             * collect_nest_members_recursive()'s own comment. */
            if (cg->nest_host == 0) {
                collect_nest_members_recursive(sem, cg, cg->cp, &cg->nest_members, target_major);
            }

            /* A descendant discovered during that same recursive scan may
             * have registered a synthetic field-accessor need on THIS
             * class (see pending_field_accessor_t's own comment,
             * genesis.h) - after codegen_class() already returned, so
             * emit it now, before this class's bytes are finalized. */
            generate_pending_field_accessors(cg);

            /* Write class file */
            const char *qname = class_sym ? class_sym->qualified_name : class_name;
            bool write_ok = output_class(cg, qname, opts);
            
            if (!write_ok) {
                errors++;
            }
            
            /* Process nested classes (static and non-static inner classes) */
            process_nested_classes(sem, cg, class_sym, opts, target_major);
            
            /* Process local classes (classes defined inside method bodies) */
            process_local_classes(sem, cg, opts, target_major);
            
            /* Process anonymous classes */
            process_anonymous_classes(sem, cg, opts, target_major);
            
            class_gen_free(cg);
        }
        
        children = children->next;
    }

    return errors;
}

/**
 * Worker thread for parallel codegen ONLY.
 * Semantic analysis is done serially before this runs.
 * This only does bytecode generation and writing, which is stateless.
 */
static void *codegen_phase_worker(void *arg)
{
    codegen_phase_state_t *state = (codegen_phase_state_t *)arg;
    
    while (1) {
        int idx = counter_fetch_add(&state->next_index, 1);
        if (idx >= state->file_count) {
            break;
        }
        
        int errors = generate_file_code(&state->results[idx], state->opts,
                                        &state->output_mutex);
        if (errors > 0) {
            counter_fetch_add(&state->error_count, errors);
        }
    }
    
    return NULL;
}

/**
 * Run semantic analysis on one parsed file, leaving the analyzer in
 * pr->sem (NULL if one could not be created). Returns whether the analysis
 * succeeded. Diagnostics stay in the analyzer; nothing is printed.
 */
static bool analyze_file(compiler_options_t *opts, type_registry_t *registry,
                         parse_result_t *pr)
{
    /* Create semantic analyzer with shared registry for cross-file type resolution.
     * The registry allows resolving types from other files in the same compilation batch. */
    semantic_t *sem = semantic_new_with_registry(g_classpath, registry);
    pr->sem = sem;
    if (!sem) {
        return false;
    }
    
    sem->warnings_enabled = opts->warnings;
    sem->werror = opts->werror;
    sem->source_version = classfile_java_version(
        classfile_version_from_string(opts->source_version));
    
    if (opts->sourcepath) {
        semantic_set_sourcepath(sem, opts->sourcepath);
    }
    
    /* Type names were already qualified in Phase 2b */
    return semantic_analyze(sem, pr->ast, pr->source);
}

/* ========================================================================
 * Semantic analysis and code generation in worker processes
 * ========================================================================
 *
 * Semantic analysis is by far the largest part of a compilation, and it
 * cannot run on several threads: analysing a file completes and updates
 * symbols that every other file's analysis reads (the shared type registry,
 * the symbols of the classes loaded from class files, the types hanging off
 * all of them), none of it under any lock. One analyzer after another on a
 * single thread, the rest of the machine sits idle for most of the run.
 *
 * What analysing a file does NOT depend on is the analysis of any other
 * file: everything files need of one another was entered in the registry by
 * the serial phases before (types, members, resolved signatures). So the
 * work is divided between processes instead of threads. Once the registry is
 * complete the compiler forks its workers; each inherits the whole state -
 * ASTs, registry, classpath caches - as a private copy-on-write image, and
 * analyses and generates code for the files it takes from a common queue,
 * free to modify its own copy of every symbol exactly as the single
 * analysing thread always did. Nothing is shared, so nothing races.
 *
 * The workers report to the compiler process over a pipe each: the
 * diagnostics of every file (printed by the compiler process in the order
 * of the files, once it is known that the batch stands), source files
 * loaded through -sourcepath (which make the batch start over, as in the
 * single-process path), and error counts. Code generation only starts when
 * the compiler process says so, after every file has been analysed.
 */

/* A batch smaller than this is compiled in this process: forking and
 * priming a set of workers costs more than it saves. */
#define WORKER_MIN_FILES 32

#define WORKERS_UNAVAILABLE (-2)

/* Messages from a worker to the compiler process */
#define WORKER_MSG_FILE 'F'         /* A file has been analysed: index, error
                                     * count (-1: no analyzer), failed flag in
                                     * `flag`, diagnostics text as payload */
#define WORKER_MSG_DEPENDENCY 'P'   /* A file loaded through -sourcepath by
                                     * file `index`: its path as payload */
#define WORKER_MSG_ANALYZED 'A'     /* Analysis of this worker's files is over */
#define WORKER_MSG_GENERATED 'C'    /* Code generation is over: errors in `value` */

/* Replies from the compiler process */
#define WORKER_CMD_GENERATE 'G'
#define WORKER_CMD_ABANDON 'X'

typedef struct worker_msg
{
    char type;
    char flag;
    int index;
    int value;
    unsigned int length;            /* Bytes of payload that follow */
} worker_msg_t;

typedef struct worker_proc
{
    pid_t pid;
    int report_fd;                  /* Read end of the worker's report pipe */
    int control_fd;                 /* Write end of the worker's control pipe */
    char *buf;                      /* Report bytes not yet consumed */
    size_t len;
    size_t cap;
    bool analyzed;                  /* WORKER_MSG_ANALYZED received */
    bool closed;                    /* Report pipe at end of file */
} worker_proc_t;

/* What the workers reported about one file */
typedef struct worker_file_report
{
    bool reported;
    bool failed;
    int error_count;
    char *diagnostics;
    slist_t *dependencies;          /* Paths (strings), in the order reported */
    slist_t *dependencies_tail;
} worker_file_report_t;

static bool write_fully(int fd, const void *data, size_t size)
{
    const char *p = (const char *)data;
    while (size > 0) {
        ssize_t n = write(fd, p, size);
        if (n < 0 && errno == EINTR) {
            continue;
        }
        if (n <= 0) {
            return false;
        }
        p += n;
        size -= (size_t)n;
    }
    return true;
}

static bool worker_send(int fd, char type, char flag, int index, int value,
                        const char *payload)
{
    worker_msg_t msg;
    memset(&msg, 0, sizeof(msg));
    msg.type = type;
    msg.flag = flag;
    msg.index = index;
    msg.value = value;
    msg.length = payload ? (unsigned int)strlen(payload) : 0;
    if (!write_fully(fd, &msg, sizeof(msg))) {
        return false;
    }
    return msg.length == 0 || write_fully(fd, payload, msg.length);
}

/**
 * The life of a worker process. Never returns.
 */
static void worker_process_run(compiler_options_t *opts, type_registry_t *registry,
                               parse_result_t *results, int file_count,
                               int queue_fd, int report_fd, int control_fd,
                               bool report_dependencies)
{
    /* This process has one thread */
    intern_set_concurrent(false);

    /* GENESIS_DEBUG_PAUSE holds each worker at its start as well, for a
     * profiler to attach to it */
    if (g_debug_pause > 0) {
        sleep((unsigned int)g_debug_pause);
    }

    int *claimed = malloc((size_t)(file_count > 0 ? file_count : 1) * sizeof(int));
    int claimed_count = 0;
    if (!claimed) {
        _exit(3);
    }

    /* Analysis: files are taken from the queue one at a time until it is
     * empty. Symbols built from class files are shared between this
     * worker's own analyzers, as in the single-process path. */
    type_registry_set_external_sharing(registry, true);
    for (;;) {
        int idx;
        ssize_t n = read(queue_fd, &idx, sizeof(idx));
        if (n < 0 && errno == EINTR) {
            continue;
        }
        if (n != (ssize_t)sizeof(idx)) {
            break;
        }
        if (idx < 0 || idx >= file_count) {
            _exit(3);
        }
        parse_result_t *pr = &results[idx];
        claimed[claimed_count++] = idx;

        bool sem_ok = analyze_file(opts, registry, pr);
        semantic_t *sem = pr->sem;
        if (!sem) {
            if (!worker_send(report_fd, WORKER_MSG_FILE, 1, idx, -1, NULL)) {
                _exit(3);
            }
            continue;
        }
        pr->sem_failed = !sem_ok;

        if (report_dependencies) {
            for (slist_t *dep = sem->source_dependencies; dep; dep = dep->next) {
                const char *dep_path = (const char *)dep->data;
                if (dep_path &&
                    !worker_send(report_fd, WORKER_MSG_DEPENDENCY, 0, idx, 0, dep_path)) {
                    _exit(3);
                }
            }
        }

        char *text = (sem->error_count > 0 || sem->warning_count > 0) ?
            semantic_format_diagnostics(sem) : NULL;
        if (!worker_send(report_fd, WORKER_MSG_FILE, sem_ok ? 0 : 1, idx,
                         sem->error_count, text)) {
            _exit(3);
        }
        free(text);
    }
    type_registry_set_external_sharing(registry, false);
    close(queue_fd);

    fflush(stdout);
    fflush(stderr);
    if (!worker_send(report_fd, WORKER_MSG_ANALYZED, 0, 0, 0, NULL)) {
        _exit(3);
    }

    /* Code generation, if the compiler process says the batch stands */
    char command = 0;
    for (;;) {
        ssize_t n = read(control_fd, &command, 1);
        if (n < 0 && errno == EINTR) {
            continue;
        }
        if (n != 1) {
            command = WORKER_CMD_ABANDON;
        }
        break;
    }
    if (command != WORKER_CMD_GENERATE) {
        _exit(0);
    }

    int errors = 0;
    for (int i = 0; i < claimed_count; i++) {
        errors += generate_file_code(&results[claimed[i]], opts, NULL);
    }

    fflush(stdout);
    fflush(stderr);
    if (!worker_send(report_fd, WORKER_MSG_GENERATED, 0, 0, errors, NULL)) {
        _exit(3);
    }
    _exit(0);
}

/**
 * Consume the complete messages in a worker's report buffer.
 */
static void worker_consume_reports(worker_proc_t *worker, worker_file_report_t *reports,
                                   int file_count, int *codegen_errors, bool *protocol_error)
{
    size_t pos = 0;
    while (worker->len - pos >= sizeof(worker_msg_t)) {
        worker_msg_t msg;
        memcpy(&msg, worker->buf + pos, sizeof(msg));
        if (worker->len - pos - sizeof(msg) < msg.length) {
            break;
        }
        const char *payload = worker->buf + pos + sizeof(msg);
        pos += sizeof(msg) + msg.length;

        switch (msg.type) {
            case WORKER_MSG_FILE:
            case WORKER_MSG_DEPENDENCY:
                if (msg.index < 0 || msg.index >= file_count) {
                    *protocol_error = true;
                    break;
                }
                {
                    worker_file_report_t *report = &reports[msg.index];
                    char *text = msg.length ? malloc(msg.length + 1) : NULL;
                    if (text) {
                        memcpy(text, payload, msg.length);
                        text[msg.length] = '\0';
                    }
                    if (msg.type == WORKER_MSG_FILE) {
                        report->reported = true;
                        report->failed = msg.flag != 0;
                        report->error_count = msg.value;
                        report->diagnostics = text;
                    } else if (text) {
                        slist_t *node = slist_new(text);
                        if (report->dependencies_tail) {
                            report->dependencies_tail->next = node;
                        } else {
                            report->dependencies = node;
                        }
                        report->dependencies_tail = node;
                    }
                }
                break;
            case WORKER_MSG_ANALYZED:
                worker->analyzed = true;
                break;
            case WORKER_MSG_GENERATED:
                *codegen_errors += msg.value;
                break;
            default:
                *protocol_error = true;
                break;
        }
    }
    if (pos > 0) {
        memmove(worker->buf, worker->buf + pos, worker->len - pos);
        worker->len -= pos;
    }
}

/* qsort() has no context argument: the files being ordered */
static parse_result_t *g_queue_sort_results = NULL;

/**
 * Order file indexes by decreasing source size (and by index for equal
 * sizes, so that the order is the same on every run).
 */
static int compare_queue_by_source_size(const void *a, const void *b)
{
    int ia = *(const int *)a;
    int ib = *(const int *)b;
    source_file_t *sa = g_queue_sort_results[ia].source;
    source_file_t *sb = g_queue_sort_results[ib].source;
    size_t la = sa ? sa->length : 0;
    size_t lb = sb ? sb->length : 0;
    if (la != lb) {
        return la > lb ? -1 : 1;
    }
    return ia < ib ? -1 : (ia > ib ? 1 : 0);
}

/**
 * Run semantic analysis and code generation for a batch in worker
 * processes (see the comment at the head of this section).
 *
 * Returns 0 when the batch was compiled (*sem_errors and *codegen_errors
 * are set), -1 when it has to start over with the files in
 * *dependencies_out added, or WORKERS_UNAVAILABLE when the workers could
 * not be started at all, in which case nothing has been done and the
 * caller compiles the batch in this process.
 */
static int compile_with_worker_processes(compiler_options_t *opts, int worker_count,
                                         parse_result_t *results, int file_count,
                                         type_registry_t *registry,
                                         slist_t **dependencies_out,
                                         int *sem_errors, int *codegen_errors)
{
    int i;
    int result = 0;

    *sem_errors = 0;
    *codegen_errors = 0;

    /* The files to analyse */
    int *queue = malloc((size_t)(file_count > 0 ? file_count : 1) * sizeof(int));
    worker_file_report_t *reports = calloc((size_t)(file_count > 0 ? file_count : 1),
                                           sizeof(worker_file_report_t));
    worker_proc_t *workers = calloc((size_t)worker_count, sizeof(worker_proc_t));
    struct pollfd *fds = calloc((size_t)worker_count + 1, sizeof(struct pollfd));
    if (!queue || !reports || !workers || !fds) {
        free(queue);
        free(reports);
        free(workers);
        free(fds);
        return WORKERS_UNAVAILABLE;
    }
    int queue_count = 0;
    for (i = 0; i < file_count; i++) {
        if (results[i].ast && !results[i].error_msg) {
            queue[queue_count++] = i;
        }
    }
    if (worker_count > queue_count) {
        worker_count = queue_count;
    }

    /* Largest files first. The phase is over when the last worker is, and
     * a very large file taken off the queue late leaves one worker busy
     * with it long after the others have run out of work. */
    g_queue_sort_results = results;
    qsort(queue, (size_t)queue_count, sizeof(int), compare_queue_by_source_size);
    g_queue_sort_results = NULL;

    int queue_pipe[2];
    if (worker_count < 2 || pipe(queue_pipe) != 0) {
        free(queue);
        free(reports);
        free(workers);
        free(fds);
        return WORKERS_UNAVAILABLE;
    }

    /* Whatever is buffered must not be written once by each worker too */
    fflush(stdout);
    fflush(stderr);

    /* A worker that died must cost an error, not a SIGPIPE */
    void (*old_sigpipe)(int) = signal(SIGPIPE, SIG_IGN);

    int started = 0;
    bool start_failed = false;
    for (i = 0; i < worker_count; i++) {
        int report_pipe[2];
        int control_pipe[2];
        if (pipe(report_pipe) != 0) {
            start_failed = true;
            break;
        }
        if (pipe(control_pipe) != 0) {
            close(report_pipe[0]);
            close(report_pipe[1]);
            start_failed = true;
            break;
        }
        pid_t pid = fork();
        if (pid < 0) {
            close(report_pipe[0]);
            close(report_pipe[1]);
            close(control_pipe[0]);
            close(control_pipe[1]);
            start_failed = true;
            break;
        }
        if (pid == 0) {
            /* Worker: keep only its own ends of the pipes */
            close(queue_pipe[1]);
            close(report_pipe[0]);
            close(control_pipe[1]);
            for (int w = 0; w < started; w++) {
                close(workers[w].report_fd);
                close(workers[w].control_fd);
            }
            signal(SIGPIPE, old_sigpipe == SIG_ERR ? SIG_DFL : old_sigpipe);
            worker_process_run(opts, registry, results, file_count, queue_pipe[0],
                               report_pipe[1], control_pipe[0], dependencies_out != NULL);
            _exit(3);
        }
        close(report_pipe[1]);
        close(control_pipe[0]);
        workers[i].pid = pid;
        workers[i].report_fd = report_pipe[0];
        workers[i].control_fd = control_pipe[1];
        started++;
    }
    close(queue_pipe[0]);

    if (start_failed) {
        /* Nothing has been handed out yet: call the workers off and let
         * the caller do the work in this process */
        close(queue_pipe[1]);
        for (i = 0; i < started; i++) {
            kill(workers[i].pid, SIGKILL);
            close(workers[i].report_fd);
            close(workers[i].control_fd);
            waitpid(workers[i].pid, NULL, 0);
        }
        if (old_sigpipe != SIG_ERR) {
            signal(SIGPIPE, old_sigpipe);
        }
        free(queue);
        free(reports);
        free(workers);
        free(fds);
        return WORKERS_UNAVAILABLE;
    }

    /* The queue is fed without ever blocking on it: this process must
     * keep reading the reports, or a worker with a full report pipe would
     * stop taking files off the queue. */
    int queue_fd = queue_pipe[1];
    int queue_flags = fcntl(queue_fd, F_GETFL, 0);
    if (queue_flags >= 0) {
        fcntl(queue_fd, F_SETFL, queue_flags | O_NONBLOCK);
    }
    int queue_pos = 0;

    bool protocol_error = false;
    bool decided = false;       /* The workers have been told what to do next */
    bool abandon = false;
    slist_t *new_dependencies = NULL;

    for (;;) {
        int nfds = 0;
        for (i = 0; i < worker_count; i++) {
            if (!workers[i].closed) {
                fds[nfds].fd = workers[i].report_fd;
                fds[nfds].events = POLLIN;
                fds[nfds].revents = 0;
                nfds++;
            }
        }
        int report_fds = nfds;
        if (queue_fd >= 0) {
            fds[nfds].fd = queue_fd;
            fds[nfds].events = POLLOUT;
            fds[nfds].revents = 0;
            nfds++;
        }
        if (report_fds == 0) {
            break;
        }
        if (poll(fds, (nfds_t)nfds, -1) < 0) {
            if (errno == EINTR) {
                continue;
            }
            protocol_error = true;
            break;
        }

        /* Hand out more files. Each index goes out in a write of its own,
         * which a pipe delivers whole to exactly one reader. */
        if (queue_fd >= 0 && (fds[nfds - 1].revents & (POLLOUT | POLLERR | POLLHUP))) {
            while (queue_pos < queue_count) {
                ssize_t n = write(queue_fd, &queue[queue_pos], sizeof(int));
                if (n == (ssize_t)sizeof(int)) {
                    queue_pos++;
                    continue;
                }
                if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)) {
                    break;
                }
                /* No worker left to read the queue */
                protocol_error = true;
                queue_pos = queue_count;
            }
            if (queue_pos >= queue_count) {
                close(queue_fd);
                queue_fd = -1;
            }
        }

        /* Read what the workers have to say */
        int f = 0;
        for (i = 0; i < worker_count; i++) {
            worker_proc_t *worker = &workers[i];
            if (worker->closed) {
                continue;
            }
            short revents = fds[f++].revents;
            if (!(revents & (POLLIN | POLLHUP | POLLERR))) {
                continue;
            }
            if (worker->cap - worker->len < 65536) {
                size_t cap = worker->cap ? worker->cap * 2 : 131072;
                char *buf = realloc(worker->buf, cap);
                if (!buf) {
                    protocol_error = true;
                    worker->closed = true;
                    continue;
                }
                worker->buf = buf;
                worker->cap = cap;
            }
            ssize_t n = read(worker->report_fd, worker->buf + worker->len,
                             worker->cap - worker->len);
            if (n < 0 && (errno == EINTR || errno == EAGAIN)) {
                continue;
            }
            if (n <= 0) {
                worker->closed = true;
                if (worker->len > 0) {
                    protocol_error = true;   /* Died in the middle of a message */
                }
                continue;
            }
            worker->len += (size_t)n;
            worker_consume_reports(worker, reports, file_count, codegen_errors,
                                   &protocol_error);
        }

        /* Once every worker has analysed its files (or is gone), decide
         * whether the batch stands, and tell them */
        if (!decided) {
            bool all_analyzed = queue_fd < 0;
            for (i = 0; i < worker_count && all_analyzed; i++) {
                if (!workers[i].analyzed && !workers[i].closed) {
                    all_analyzed = false;
                }
            }
            if (!all_analyzed) {
                continue;
            }
            decided = true;

            /* A worker that went away without finishing took the analysis
             * of some files with it */
            for (i = 0; i < worker_count; i++) {
                if (!workers[i].analyzed) {
                    protocol_error = true;
                }
            }
            for (i = 0; i < queue_count && !protocol_error; i++) {
                if (!reports[queue[i]].reported) {
                    protocol_error = true;
                }
            }

            /* Source files loaded through -sourcepath that are not already
             * part of the compilation, in the order of the files that
             * loaded them */
            if (!protocol_error && dependencies_out) {
                for (i = 0; i < file_count; i++) {
                    for (slist_t *dep = reports[i].dependencies; dep; dep = dep->next) {
                        const char *dep_path = (const char *)dep->data;
                        if (is_source_compiled(dep_path)) {
                            continue;
                        }
                        mark_source_compiled(dep_path);
                        char *copy = strdup(dep_path);
                        if (!new_dependencies) {
                            new_dependencies = slist_new(copy);
                        } else {
                            slist_append(new_dependencies, copy);
                        }
                    }
                }
            }

            abandon = protocol_error || new_dependencies != NULL;
            if (!abandon) {
                timing_mark("semantic analysis");

                /* The batch stands: report on it, in the order of the files */
                for (i = 0; i < file_count; i++) {
                    worker_file_report_t *report = &reports[i];
                    if (!report->reported) {
                        continue;
                    }
                    if (report->error_count < 0) {
                        fprintf(stderr, "error: cannot create semantic analyzer for %s\n",
                                results[i].filename);
                        (*sem_errors)++;
                        continue;
                    }
                    if (report->diagnostics) {
                        fputs(report->diagnostics, stderr);
                    }
                    if (report->failed) {
                        fprintf(stderr, "%d error(s) in %s\n", report->error_count,
                                results[i].filename);
                        (*sem_errors)++;
                    }
                }
                if (opts->verbose) {
                    printf("Phase 5a complete: %d semantic errors\n", *sem_errors);
                    printf("Phase 5b: Code generation in %d worker processes...\n",
                           worker_count);
                }
                fflush(stdout);
            }

            char command = abandon ? WORKER_CMD_ABANDON : WORKER_CMD_GENERATE;
            for (i = 0; i < worker_count; i++) {
                if (!workers[i].closed && write(workers[i].control_fd, &command, 1) != 1) {
                    protocol_error = true;
                }
                close(workers[i].control_fd);
                workers[i].control_fd = -1;
            }
        }
    }

    if (queue_fd >= 0) {
        close(queue_fd);
    }

    /* Collect the workers */
    for (i = 0; i < worker_count; i++) {
        if (workers[i].control_fd >= 0) {
            close(workers[i].control_fd);
        }
        close(workers[i].report_fd);
        int status = 0;
        pid_t waited;
        do {
            waited = waitpid(workers[i].pid, &status, 0);
        } while (waited < 0 && errno == EINTR);
        if (waited < 0 || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            if (waited >= 0 && WIFSIGNALED(status)) {
                fprintf(stderr, "error: compiler worker process terminated by signal %d\n",
                        WTERMSIG(status));
            } else {
                fprintf(stderr, "error: compiler worker process failed\n");
            }
            protocol_error = true;
        }
        free(workers[i].buf);
    }
    if (old_sigpipe != SIG_ERR) {
        signal(SIGPIPE, old_sigpipe);
    }

    if (protocol_error) {
        /* Part of the batch was not compiled; what was said about the rest
         * (if anything) stands, and the compilation fails */
        slist_free_full(new_dependencies, free);
        new_dependencies = NULL;
        (*codegen_errors)++;
        result = 0;
    } else if (new_dependencies) {
        *dependencies_out = new_dependencies;
        result = -1;
    }

    for (i = 0; i < file_count; i++) {
        free(reports[i].diagnostics);
        slist_free_full(reports[i].dependencies, free);
    }
    free(queue);
    free(reports);
    free(workers);
    free(fds);
    return result;
}

/**
 * Compile one batch of source files through the full pipeline.
 *
 *   Phase 1: Parse all files (threads; no shared state)
 *   Phase 2: Register types (serial)
 *   Phase 2b: Qualify type names (threads; each file's own AST)
 *   Phase 3-4: Enter members, resolve member types (serial)
 *   Phase 5a: Semantic analysis
 *   Phase 5b: Code generation
 *
 * With more than one job allowed, phase 5 runs in worker processes, each
 * analysing and then generating code for its share of the files (see
 * compile_with_worker_processes()). Otherwise - -j1, JAR output, a small
 * batch - the files are analysed one after another in this process and
 * their code is generated by threads.
 *
 * Returns the number of errors. With dependencies_out set, the batch
 * stops after semantic analysis whenever that analysis had to load a
 * source file through -sourcepath that is not yet part of the
 * compilation: it then returns -1, prints nothing, and *dependencies_out
 * holds the paths (malloc'd strings, caller frees) of those files, for
 * the caller to add and start over.
 */
static int compile_batch(compiler_options_t *opts, int thread_count,
                         char **filenames, int file_count,
                         slist_t **dependencies_out)
{
    int i = 0;
    int source_version = classfile_java_version(
        classfile_version_from_string(opts->source_version));

    if (dependencies_out) {
        *dependencies_out = NULL;
    }

    if (opts->verbose) {
        printf("Parallel compilation with %d threads for %d files\n", 
               thread_count, file_count);
    }
    
    /* Allocate parse results */
    parse_result_t *parse_results = calloc(file_count, sizeof(parse_result_t));
    if (!parse_results) {
        fprintf(stderr, "error: out of memory\n");
        return 1;
    }
    
    /* Thread attributes for larger stack */
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 8 * 1024 * 1024);
    
    /* ================================================================
     * Phase 1: Parallel Parsing
     * Disable AST pool for thread-safe parsing - each thread will use malloc.
     * ================================================================ */
    ast_pool_disable();
    
    /* Note: We don't enable type_cache because type comparison already 
     * uses name-based comparison (class_names_equal in type_equals).
     * The type cache was causing issues with shared type_args modification. */
    
    if (opts->verbose) {
        printf("Phase 1: Parsing %d files...\n", file_count);
    }
    
    parse_phase_state_t parse_state = {
        .filenames = filenames,
        .file_count = file_count,
        .results = parse_results,
        .source_version = source_version
    };
    counter_init(&parse_state.next_index);
    
    pthread_t *threads = malloc(thread_count * sizeof(pthread_t));
    for (i = 0; i < thread_count; i++) {
        pthread_create(&threads[i], &attr, parse_phase_worker, &parse_state);
    }
    for (i = 0; i < thread_count; i++) {
        pthread_join(threads[i], NULL);
    }
    counter_destroy(&parse_state.next_index);
    timing_mark("parse");
    
    /* Report parse errors */
    int parse_errors = 0;
    for (i = 0; i < file_count; i++) {
        if (parse_results[i].error_msg) {
            fprintf(stderr, "%s:%d:%d: error: %s\n",
                    filenames[i],
                    parse_results[i].error_line,
                    parse_results[i].error_column,
                    parse_results[i].error_msg);
            parse_errors++;
        }
    }
    
    if (parse_errors > 0) {
        fprintf(stderr, "%d parse error(s)\n", parse_errors);
    }
    
    if (opts->verbose) {
        printf("Phase 1 complete: %d files parsed, %d errors\n",
               file_count - parse_errors, parse_errors);
    }
    
    /* ================================================================
     * Phase 2: Type Entry (serial)
     * Register all types from all parsed ASTs into a shared registry.
     * This enables cross-file type resolution during parallel analysis.
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 2: Registering types...\n");
    }
    
    /* Create shared type registry */
    type_registry_t *registry = type_registry_new();
    if (!registry) {
        fprintf(stderr, "error: failed to create type registry\n");
        free(parse_results);
        free(threads);
        return 1;
    }
    
    /* Create completer context for lazy member population (javac-style) */
    completer_context_t *completer_ctx = completer_context_new(registry, g_classpath);
    
    /* Register all types from successfully parsed files */
    int types_registered = 0;
    for (i = 0; i < file_count; i++) {
        if (parse_results[i].ast && !parse_results[i].error_msg) {
            if (debug_getenv("GENESIS_DEBUG_REGISTRY")) {
                fprintf(stderr, "DEBUG Phase 2: registering types from file[%d] = '%s'\n",
                        i, parse_results[i].filename ? parse_results[i].filename : "(null)");
            }
            register_types_from_ast(registry, parse_results[i].ast, completer_ctx);
            types_registered++;
        }
    }
    
    /* Nothing registers a type after this point */
    type_registry_seal(registry);

    if (opts->verbose) {
        printf("Phase 2 complete: types registered from %d files\n", types_registered);
    }
    timing_mark("register types");
    
    /* ================================================================
     * Phase 2b: Type Name Qualification (parallel)
     * Resolve simple type names to fully qualified names using imports.
     * This MUST happen BEFORE member entry so that field/method types
     * are stored with fully qualified names, not simple names.
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 2b: Qualifying type names...\n");
    }
    
    /* Resolve type names in all ASTs before extracting member signatures */
    slist_t *sp_list = sourcepath_parse(opts->sourcepath);
    qualify_phase_state_t qualify_state = {
        .results = parse_results,
        .file_count = file_count,
        .sourcepath_list = sp_list,
        .registry = registry
    };
    counter_init(&qualify_state.next_index);
    for (i = 0; i < thread_count; i++) {
        pthread_create(&threads[i], &attr, qualify_phase_worker, &qualify_state);
    }
    for (i = 0; i < thread_count; i++) {
        pthread_join(threads[i], NULL);
    }
    counter_destroy(&qualify_state.next_index);
    sourcepath_list_free(sp_list);
    timing_mark("qualify type names");
    
    if (opts->verbose) {
        printf("Phase 2b complete: type names qualified\n");
    }
    
    /* ================================================================
     * Phase 3: Member Entry (serial)
     * Add method and field signatures to all registered types.
     * Type references now use fully qualified names (from Phase 2b).
     * 
     * NOTE: Even with completers set on symbols, we still do eager population
     * here to avoid race conditions during parallel Phase 5. The completers
     * serve as a fallback for types loaded dynamically during analysis.
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 3: Entering members...\n");
    }
    
    registry_enter_members(registry, g_classpath);
    timing_mark("enter members");
    
    if (opts->verbose) {
        printf("Phase 3 complete: members entered\n");
    }
    
    /* ================================================================
     * Phase 4: Type Resolution (serial)
     * Resolve all unresolved type references in method signatures,
     * superclass declarations, and interface implementations.
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 4: Resolving types...\n");
    }
    
    registry_resolve_types(registry, g_classpath);
    timing_mark("resolve member types");
    
    /* Pre-load common JDK types into classpath cache */
    semantic_t *init_sem = semantic_new(g_classpath);
    if (init_sem) {
        semantic_free(init_sem);
    }
    
    if (opts->verbose) {
        printf("Phase 4 complete: types resolved\n");
        
        /* Debug: print method return types */
        if (debug_getenv("GENESIS_DEBUG_REGISTRY")) {
            for (size_t i = 0; i < registry->types->size; i++) {
                hashtable_entry_t *entry = registry->types->buckets[i];
                while (entry) {
                    symbol_t *sym = (symbol_t *)entry->value;
                    if (sym && sym->data.class_data.members) {
                        fprintf(stderr, "DEBUG registry type '%s' members:\n", 
                                sym->qualified_name ? sym->qualified_name : sym->name);
                        for (size_t j = 0; j < sym->data.class_data.members->symbols->size; j++) {
                            hashtable_entry_t *mem = sym->data.class_data.members->symbols->buckets[j];
                            while (mem) {
                                symbol_t *member = (symbol_t *)mem->value;
                                if (member && member->kind == SYM_METHOD) {
                                    fprintf(stderr, "  method '%s' return_type=%p (%s)\n",
                                            member->name,
                                            (void*)member->type,
                                            member->type ? type_to_string(member->type) : "NULL");
                                }
                                mem = mem->next;
                            }
                        }
                    }
                    entry = entry->next;
                }
            }
        }
    }
    
    /* ================================================================
     * Phase 5a: Semantic Analysis
     * Never on several threads of one process: the analyzers update
     * symbols they all share (see the comment on worker processes above).
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 5a: Semantic analysis...\n");
    }
    
    int sem_errors = 0;

    /* A batch of any size, compiled to a directory with more than one job
     * allowed, is analysed and generated by worker processes - see
     * compile_with_worker_processes(). (Class files are written by
     * whichever worker generates them; a JAR is a single stream, written
     * by this process.) */
    if (thread_count > 1 && !g_jar_writer && file_count >= WORKER_MIN_FILES) {
        int worker_sem_errors = 0;
        int worker_codegen_errors = 0;
        int outcome = compile_with_worker_processes(opts, thread_count, parse_results,
                                                    file_count, registry, dependencies_out,
                                                    &worker_sem_errors,
                                                    &worker_codegen_errors);
        if (outcome != WORKERS_UNAVAILABLE) {
            int worker_total = outcome < 0 ? -1 :
                parse_errors + worker_sem_errors + worker_codegen_errors;
            if (outcome == 0) {
                timing_mark("code generation");
                if (opts->verbose) {
                    printf("Phase 5b complete: %d codegen errors\n", worker_codegen_errors);
                    printf("Total: %d parse + %d semantic + %d codegen = %d errors\n",
                           parse_errors, worker_sem_errors, worker_codegen_errors,
                           worker_total);
                }
            }
            for (i = 0; i < file_count; i++) {
                free(parse_results[i].error_msg);
            }
            free(parse_results);
            free(threads);
            pthread_attr_destroy(&attr);
            type_registry_free(registry);
            timing_mark("cleanup");
            return worker_total;
        }
    }

    bool *sem_failed = calloc(file_count > 0 ? file_count : 1, sizeof(bool));
    slist_t *new_dependencies = NULL;

    /* The analyzers run one after another here, so the symbols they build
     * for classes loaded from class files can be shared between them. */
    type_registry_set_external_sharing(registry, true);
    for (i = 0; i < file_count; i++) {
        parse_result_t *pr = &parse_results[i];
        
        /* Skip files with parse errors */
        if (pr->error_msg || !pr->ast) {
            pr->sem = NULL;
            continue;
        }
        
        /* Semantic analysis. Diagnostics are printed only once it is known
         * that this batch is the final one (see below). */
        bool sem_ok = analyze_file(opts, registry, pr);
        semantic_t *sem = pr->sem;
        if (!sem) {
            fprintf(stderr, "error: cannot create semantic analyzer for %s\n", pr->filename);
            sem_errors++;
            continue;
        }
        sem_failed[i] = !sem_ok;

        /* Source files this analyzer loaded through -sourcepath that are
         * not already part of the compilation */
        if (dependencies_out) {
            for (slist_t *dep = sem->source_dependencies; dep; dep = dep->next) {
                const char *dep_path = (const char *)dep->data;
                if (!dep_path || is_source_compiled(dep_path)) {
                    continue;
                }
                mark_source_compiled(dep_path);
                char *copy = strdup(dep_path);
                if (!new_dependencies) {
                    new_dependencies = slist_new(copy);
                } else {
                    slist_append(new_dependencies, copy);
                }
            }
        }
    }

    type_registry_set_external_sharing(registry, false);
    timing_mark("semantic analysis");

    /* Anything loaded through -sourcepath means this batch is incomplete:
     * hand the newly found files back so the caller can restart with them
     * included, and say nothing about this batch - a symbol that came in
     * from the sourcepath loader is a second-class copy (members entered by
     * a separate, less complete path), and diagnostics based on it are not
     * trustworthy. The restarted batch has every such class in the shared
     * registry instead, where each file is analysed exactly as it is when
     * named on the command line. */
    if (new_dependencies) {
        for (i = 0; i < file_count; i++) {
            if (parse_results[i].sem) {
                semantic_free(parse_results[i].sem);
                parse_results[i].sem = NULL;
            }
            free(parse_results[i].error_msg);
        }
        free(sem_failed);
        free(parse_results);
        free(threads);
        pthread_attr_destroy(&attr);
        type_registry_free(registry);
        *dependencies_out = new_dependencies;
        return -1;
    }

    for (i = 0; i < file_count; i++) {
        parse_result_t *pr = &parse_results[i];
        semantic_t *sem = pr->sem;
        if (!sem) {
            continue;
        }
        if (sem->error_count > 0 || sem->warning_count > 0) {
            semantic_print_diagnostics(sem);
        }
        if (sem_failed[i]) {
            fprintf(stderr, "%d error(s) in %s\n", sem->error_count, pr->filename);
            sem_errors++;
            /* The analyzer is kept until the end of the batch like the
             * others (symbols shared between the analyzers may refer to
             * this file's classes); the file just gets no code generated. */
            pr->sem_failed = true;
        }
    }
    free(sem_failed);
    
    if (opts->verbose) {
        printf("Phase 5a complete: %d semantic errors\n", sem_errors);
    }
    
    /* ================================================================
     * Phase 5b: Parallel Code Generation (threads)
     * Semantic analysis is complete, codegen is stateless and safe.
     * ================================================================ */
    if (opts->verbose) {
        printf("Phase 5b: Parallel code generation...\n");
    }
    
    codegen_phase_state_t codegen_state = {
        .results = parse_results,
        .file_count = file_count,
        .opts = opts,
        .registry = registry
    };
    pthread_mutex_init(&codegen_state.output_mutex, NULL);
    counter_init(&codegen_state.next_index);
    counter_init(&codegen_state.error_count);
    
    for (i = 0; i < thread_count; i++) {
        pthread_create(&threads[i], &attr, codegen_phase_worker, &codegen_state);
    }
    for (i = 0; i < thread_count; i++) {
        pthread_join(threads[i], NULL);
    }
    
    pthread_attr_destroy(&attr);
    pthread_mutex_destroy(&codegen_state.output_mutex);
    free(threads);
    timing_mark("code generation");
    
    /* Free semantic analyzers */
    for (i = 0; i < file_count; i++) {
        if (parse_results[i].sem) {
            semantic_free(parse_results[i].sem);
            parse_results[i].sem = NULL;
        }
    }
    
    int codegen_errors = counter_load(&codegen_state.error_count);
    counter_destroy(&codegen_state.next_index);
    counter_destroy(&codegen_state.error_count);
    int total_errors = parse_errors + sem_errors + codegen_errors;

    if (opts->verbose) {
        printf("Phase 5b complete: %d codegen errors\n", codegen_errors);
        printf("Total: %d parse + %d semantic + %d codegen = %d errors\n",
               parse_errors, sem_errors, codegen_errors, total_errors);
    }
    

    /* Cleanup */
    for (i = 0; i < file_count; i++) {
        free(parse_results[i].error_msg);
        /* Don't free source/ast - may be referenced */
    }
    free(parse_results);
    
    /* Free type registry */
    if (registry) {
        type_registry_free(registry);
    }
    timing_mark("cleanup");

    return total_errors;
}

/**
 * Compile all source files named on the command line, then whatever they
 * pulled in through -sourcepath.
 * 
 * `thread_count` is the number of jobs: threads for parsing, worker
 * processes (or threads) for analysis and code generation - see
 * compile_batch().
 */
static int compile_parallel(compiler_options_t *opts, int thread_count)
{
    if (!opts || !opts->source_files) {
        fprintf(stderr, "error: no source files\n");
        return 1;
    }
    
    /* Count source files and build array */
    int file_count = 0;
    for (slist_t *list = opts->source_files; list; list = list->next) {
        file_count++;
    }
    
    char **filenames = malloc(file_count * sizeof(char *));
    if (!filenames) {
        fprintf(stderr, "error: out of memory\n");
        return 1;
    }
    
    int i = 0;
    for (slist_t *list = opts->source_files; list; list = list->next) {
        filenames[i++] = (char *)list->data;
    }
    
    /* With a single worker the phases run strictly one after another
     * (this thread only waits for it), so nothing needs the intern table's
     * locks. */
    intern_set_concurrent(thread_count > 1);

    /* Initialize classpath (thread-safe cache) */
    if (!init_classpath(opts)) {
        free(filenames);
        return 1;
    }
    
    timing_mark("classpath setup");

    /* Set up global state */
    g_opts = opts;
    g_compiled_sources = hashtable_new();
    
    /* Create JAR writer if outputting to JAR */
    if (opts->output_jar) {
        g_jar_writer = jar_writer_new(opts->output_jar, opts->main_class);
        if (!g_jar_writer) {
            fprintf(stderr, "error: cannot create JAR file: %s\n", opts->output_jar);
            hashtable_free(g_compiled_sources);
            g_compiled_sources = NULL;
            g_opts = NULL;
            free(filenames);
            free_classpath();
            return 1;
        }
        if (opts->verbose) {
            printf("Creating JAR: %s\n", opts->output_jar);
        }
    }
    
    /* The files named on the command line, plus - when a -sourcepath is
     * given - whatever they turn out to need from it. javac compiles such
     * implicitly loaded sources to class files too; the classes generated
     * here refer to them, so without that the output cannot run on its own
     * (NoClassDefFoundError).
     *
     * Rather than compiling those files afterwards one at a time through
     * compile_file() (the old single-file driver: a private analyzer per
     * file with no shared registry, and never the recipient of the fixes
     * made to this pipeline), the batch is restarted with them added, so
     * that every class involved is analysed through the same path as one
     * named on the command line. The restart repeats only when something
     * new was found, so an ordinary build with nothing to pull in runs
     * exactly once. */
    /* (Only a -sourcepath compilation ever asks whether a file is already
     * part of the compilation, and canonicalizing every name is not free.) */
    if (opts->sourcepath) {
        for (i = 0; i < file_count; i++) {
            mark_source_compiled(filenames[i]);
        }
    }
    int total_errors;
    for (;;) {
        slist_t *dependencies = NULL;
        total_errors = compile_batch(opts, thread_count, filenames, file_count,
                                     opts->sourcepath ? &dependencies : NULL);
        if (total_errors >= 0) {
            break;
        }
        int dep_count = 0;
        for (slist_t *dep = dependencies; dep; dep = dep->next) {
            dep_count++;
        }
        char **grown = realloc(filenames, (file_count + dep_count) * sizeof(char *));
        if (!grown) {
            fprintf(stderr, "error: out of memory\n");
            slist_free_full(dependencies, free);
            total_errors = 1;
            break;
        }
        filenames = grown;
        for (slist_t *dep = dependencies; dep; dep = dep->next) {
            /* Ownership of the string moves to filenames; it is never freed
             * (like the command-line names, which live in opts). */
            filenames[file_count++] = (char *)dep->data;
        }
        slist_free(dependencies);
        if (opts->verbose) {
            printf("Restarting with %d source file(s) loaded through -sourcepath (%d files)\n",
                   dep_count, file_count);
        }
    }

    /* Finalize JAR if in JAR output mode */
    if (g_jar_writer) {
        if (total_errors > 0) {
            jar_writer_abort(g_jar_writer);
            if (opts->verbose) {
                printf("JAR creation aborted due to errors\n");
            }
        } else {
            if (jar_writer_close(g_jar_writer)) {
                if (opts->verbose) {
                    printf("Created: %s\n", opts->output_jar);
                }
            } else {
                fprintf(stderr, "error: failed to finalize JAR file\n");
                total_errors++;
            }
        }
        g_jar_writer = NULL;
    }
    
    if (g_debug_timing && g_classpath) {
        fprintf(stderr, "timing: classpath: %d classes loaded, %zu names not found\n",
                g_classpath->classes_loaded,
                g_classpath->negative_cache ? g_classpath->negative_cache->count : (size_t)0);
    }

    hashtable_free(g_compiled_sources);
    g_compiled_sources = NULL;
    g_opts = NULL;
    free(filenames);
    free_classpath();
    
    if (opts->verbose) {
        printf("Compilation complete: %d error(s)\n", total_errors);
    }
    
    return total_errors > 0 ? 1 : 0;
}

/* ========================================================================
 * Command-line interface
 * ======================================================================== */

/**
 * Read arguments from a file (javac @file syntax).
 * 
 * Format:
 *   - One argument per line
 *   - Lines starting with # are comments
 *   - Whitespace is trimmed
 *   - Empty lines are ignored
 * 
 * Returns a dynamically allocated array of arguments (NULL-terminated).
 * Caller must free the array and each string.
 */
static char **read_argument_file(const char *filename, int *arg_count)
{
    *arg_count = 0;
    
    /* Read file contents */
    size_t file_size;
    char *contents = file_get_contents(filename, &file_size);
    if (!contents) {
        fprintf(stderr, "error: cannot read argument file: %s\n", filename);
        return NULL;
    }
    
    /* Count lines and allocate array (overestimate) */
    int max_args = 100;
    char **args = malloc(sizeof(char *) * (max_args + 1));
    if (!args) {
        free(contents);
        return NULL;
    }
    
    /* Parse line by line */
    char *line = contents;
    char *end = contents + file_size;
    
    while (line < end) {
        /* Find end of line */
        char *line_end = line;
        while (line_end < end && *line_end != '\n' && *line_end != '\r') {
            line_end++;
        }
        
        /* Null-terminate line */
        char saved = *line_end;
        *line_end = '\0';
        
        /* Trim leading whitespace */
        while (*line == ' ' || *line == '\t') {
            line++;
        }
        
        /* Trim trailing whitespace */
        char *trim_end = line_end - 1;
        while (trim_end >= line && (*trim_end == ' ' || *trim_end == '\t')) {
            *trim_end = '\0';
            trim_end--;
        }
        
        /* Skip empty lines and comments */
        if (*line != '\0' && *line != '#') {
            /* Grow array if needed */
            if (*arg_count >= max_args) {
                max_args *= 2;
                char **new_args = realloc(args, sizeof(char *) * (max_args + 1));
                if (!new_args) {
                    for (int i = 0; i < *arg_count; i++) {
                        free(args[i]);
                    }
                    free(args);
                    free(contents);
                    return NULL;
                }
                args = new_args;
            }
            
            /* Add argument */
            args[(*arg_count)++] = strdup(line);
        }
        
        /* Move to next line */
        *line_end = saved;
        if (line_end < end && *line_end == '\r') {
            line_end++;
        }
        if (line_end < end && *line_end == '\n') {
            line_end++;
        }
        line = line_end;
    }
    
    args[*arg_count] = NULL;
    free(contents);
    return args;
}

/**
 * Print version information.
 */
void print_version(void)
{
    printf("%s %s\n", PACKAGE_NAME, GENESIS_VERSION);
    printf("Copyright (C) 2016, 2020, 2026 Chris Burdess <" PACKAGE_BUGREPORT ">\n");
    printf("License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>\n");
    printf("This is free software: you are free to change and redistribute it.\n");
    printf("There is NO WARRANTY, to the extent permitted by law.\n");
}

/**
 * Print usage information.
 */
void print_usage(const char *program_name)
{
    printf("Usage: %s [options] <source files>\n", program_name);
    printf("       %s @<filename>\n", program_name);
    printf("\n");
    printf("Options:\n");
    printf("  @<filename>         Read options and filenames from file\n");
    printf("  -d <directory>      Specify output directory for class files\n");
    printf("  -jar <file.jar>     Output to JAR file instead of directory\n");
    printf("  -main-class <class> Specify main class for JAR manifest\n");
    printf("  -cp <path>          Specify classpath\n");
    printf("  -classpath <path>   Specify classpath\n");
    printf("  -sourcepath <path>  Specify source path\n");
    printf("  -source <version>   Specify source version (default: 25)\n");
    printf("  --source <version>  Specify source version (default: 25)\n");
    printf("  -target <version>   Specify target version (default: automatic, at least 8)\n");
    printf("  --target <version>  Specify target version (default: automatic, at least 8)\n");
    printf("  -release <version>  Specify release version (sets source and target)\n");
    printf("  --release <version> Specify release version (sets source and target)\n");
    printf("  -g                  Generate debugging information\n");
    printf("  -g:none             Do not generate debugging information\n");
    printf("  -nowarn             Disable warnings\n");
    printf("  -Werror             Treat warnings as errors\n");
    printf("  -verbose            Enable verbose output\n");
    printf("  -j[N]               Run N jobs at once (default: one per processor;\n");
    printf("                      -j1 compiles in a single thread of one process)\n");
    printf("  -version            Print version information\n");
    printf("  -help               Print this help message\n");
    printf("\nReport bugs to <" PACKAGE_BUGREPORT ">.\n");
}

/**
 * Program entry point.
 */
int main(int argc, char **argv)
{
    /* Note whether any GENESIS_DEBUG_* tracing is requested */
    debug_env_init();

    /* GENESIS_DEBUG_PAUSE=<seconds>: wait before doing anything, so that a
     * profiler attaching by process name catches the whole run (the first
     * phases are over within a few hundred milliseconds). */
    if (g_debug_pause > 0) {
        sleep((unsigned int)g_debug_pause);
    }
    timing_start();

    /* Initialize string interning for performance */
    intern_init();
    
    /* Initialize AST node pool for efficient allocation */
    ast_pool_init();
    
    compiler_options_t *opts = compiler_options_new();
    if (!opts) {
        fprintf(stderr, "error: failed to allocate compiler options\n");
        intern_cleanup();
        return 1;
    }
    
    slist_t *files_tail = NULL;
    bool expect_value = false;
    char **value_ptr = NULL;
    
    /* Parse command-line arguments */
    for (int i = 1; i < argc; i++) {
        /* Handle @file argument files */
        if (argv[i][0] == '@') {
            const char *argfile = argv[i] + 1;
            int file_argc = 0;
            char **file_argv = read_argument_file(argfile, &file_argc);
            
            if (!file_argv) {
                compiler_options_free(opts);
                return 1;
            }
            
            /* Recursively process arguments from file */
            for (int j = 0; j < file_argc; j++) {
                char *arg = file_argv[j];
                
                if (expect_value) {
                    free(*value_ptr);
                    *value_ptr = strdup(arg);
                    expect_value = false;
                    value_ptr = NULL;
                    continue;
                }
                
                if (arg[0] == '-') {
                    /* Process as option (same logic as below) */
                    if (strcmp(arg, "-version") == 0 || strcmp(arg, "--version") == 0) {
                        print_version();
                        for (int k = 0; k < file_argc; k++) {
                            free(file_argv[k]);
                        }
                        free(file_argv);
                        compiler_options_free(opts);
                        return 0;
                    }
                    else if (strcmp(arg, "-help") == 0 || strcmp(arg, "--help") == 0) {
                        print_usage(argv[0]);
                        for (int k = 0; k < file_argc; k++) {
                            free(file_argv[k]);
                        }
                        free(file_argv);
                        compiler_options_free(opts);
                        return 0;
                    }
                    else if (strcmp(arg, "-d") == 0) {
                        expect_value = true;
                        value_ptr = &opts->output_dir;
                    }
                    else if (strcmp(arg, "-jar") == 0) {
                        expect_value = true;
                        value_ptr = &opts->output_jar;
                    }
                    else if (strcmp(arg, "-main-class") == 0) {
                        expect_value = true;
                        value_ptr = &opts->main_class;
                    }
                    else if (strcmp(arg, "-cp") == 0 || strcmp(arg, "-classpath") == 0) {
                        expect_value = true;
                        value_ptr = &opts->classpath;
                    }
                    else if (strcmp(arg, "-sourcepath") == 0) {
                        expect_value = true;
                        value_ptr = &opts->sourcepath;
                    }
                    else if (strcmp(arg, "-source") == 0 || strcmp(arg, "--source") == 0) {
                        expect_value = true;
                        value_ptr = &opts->source_version;
                    }
                    else if (strcmp(arg, "-target") == 0 || strcmp(arg, "--target") == 0) {
                        expect_value = true;
                        value_ptr = &opts->target_version;
                    }
                    else if (strcmp(arg, "-release") == 0 || strcmp(arg, "--release") == 0) {
                        /* Get next argument from file */
                        if (j + 1 < file_argc) {
                            j++;
                            free(opts->source_version);
                            free(opts->target_version);
                            opts->source_version = strdup(file_argv[j]);
                            opts->target_version = strdup(file_argv[j]);
                        } else {
                            fprintf(stderr, "error: --release requires a version argument\n");
                            for (int k = 0; k < file_argc; k++) {
                                free(file_argv[k]);
                            }
                            free(file_argv);
                            compiler_options_free(opts);
                            return 1;
                        }
                    }
                    else if (strcmp(arg, "-g") == 0) {
                        opts->debug_info = true;
                    }
                    else if (strcmp(arg, "-g:none") == 0) {
                        opts->debug_info = false;
                    }
                    else if (strcmp(arg, "-nowarn") == 0) {
                        opts->warnings = false;
                    }
                    else if (strcmp(arg, "-Werror") == 0) {
                        opts->werror = true;
                    }
                    else if (strcmp(arg, "-verbose") == 0) {
                        opts->verbose = true;
                    }
                    else {
                        fprintf(stderr, "warning: unrecognized option: %s\n", arg);
                    }
                }
                else {
                    /* Source file */
                    if (!str_has_suffix(arg, ".java")) {
                        fprintf(stderr, "error: not a Java source file: %s\n", arg);
                        for (int k = 0; k < file_argc; k++) {
                            free(file_argv[k]);
                        }
                        free(file_argv);
                        compiler_options_free(opts);
                        return 1;
                    }
                    
                    if (!opts->source_files) {
                        opts->source_files = slist_new(strdup(arg));
                        files_tail = opts->source_files;
                    } else {
                        files_tail = slist_append(files_tail, strdup(arg));
                    }
                }
            }
            
            /* Free argument file contents */
            for (int k = 0; k < file_argc; k++) {
                free(file_argv[k]);
            }
            free(file_argv);
            continue;
        }
        
        if (expect_value) {
            free(*value_ptr);
            *value_ptr = strdup(argv[i]);
            expect_value = false;
            value_ptr = NULL;
            continue;
        }
        
        if (argv[i][0] == '-') {
            if (strcmp(argv[i], "-version") == 0 ||
                strcmp(argv[i], "--version") == 0) {
                print_version();
                compiler_options_free(opts);
                return 0;
            }
            else if (strcmp(argv[i], "-help") == 0 || 
                     strcmp(argv[i], "--help") == 0) {
                print_usage(argv[0]);
                compiler_options_free(opts);
                return 0;
            }
            else if (strcmp(argv[i], "-d") == 0) {
                expect_value = true;
                value_ptr = &opts->output_dir;
            }
            else if (strcmp(argv[i], "-jar") == 0) {
                expect_value = true;
                value_ptr = &opts->output_jar;
            }
            else if (strcmp(argv[i], "-main-class") == 0) {
                expect_value = true;
                value_ptr = &opts->main_class;
            }
            else if (strcmp(argv[i], "-cp") == 0 || 
                     strcmp(argv[i], "-classpath") == 0) {
                expect_value = true;
                value_ptr = &opts->classpath;
            }
            else if (strcmp(argv[i], "-sourcepath") == 0) {
                expect_value = true;
                value_ptr = &opts->sourcepath;
            }
            else if (strcmp(argv[i], "-source") == 0 ||
                     strcmp(argv[i], "--source") == 0) {
                expect_value = true;
                value_ptr = &opts->source_version;
            }
            else if (strcmp(argv[i], "-target") == 0 ||
                     strcmp(argv[i], "--target") == 0) {
                expect_value = true;
                value_ptr = &opts->target_version;
            }
            else if (strcmp(argv[i], "-release") == 0 ||
                     strcmp(argv[i], "--release") == 0) {
                if (i + 1 >= argc) {
                    fprintf(stderr, "error: --release requires a version argument\n");
                    compiler_options_free(opts);
                    return 1;
                }
                i++;
                free(opts->source_version);
                free(opts->target_version);
                opts->source_version = strdup(argv[i]);
                opts->target_version = strdup(argv[i]);
            }
            else if (strcmp(argv[i], "-g") == 0) {
                opts->debug_info = true;
            }
            else if (strcmp(argv[i], "-g:none") == 0) {
                opts->debug_info = false;
            }
            else if (strcmp(argv[i], "-nowarn") == 0) {
                opts->warnings = false;
            }
            else if (strcmp(argv[i], "-Werror") == 0) {
                opts->werror = true;
            }
            else if (strcmp(argv[i], "-verbose") == 0) {
                opts->verbose = true;
            }
            else if (strcmp(argv[i], "-j") == 0 || 
                     strncmp(argv[i], "-j", 2) == 0) {
                /* -j or -jN for parallel compilation
                 * -j alone = auto-detect thread count
                 * -jN = use N threads (N > 0)
                 * -j0 = serial mode (same as --serial) */
                int jobs = -1;  /* -1 = auto-detect thread count */
                if (strlen(argv[i]) > 2) {
                    /* -jN format */
                    jobs = atoi(argv[i] + 2);
                } else if (i + 1 < argc && argv[i + 1][0] != '-') {
                    /* -j N format */
                    jobs = atoi(argv[++i]);
                }
                /* -j alone means auto-detect (jobs stays -1) */
                opts->jobs = jobs;
            }
            else if (strcmp(argv[i], "--jobs") == 0) {
                if (i + 1 >= argc) {
                    fprintf(stderr, "error: --jobs requires a value\n");
                    compiler_options_free(opts);
                    return 1;
                }
                opts->jobs = atoi(argv[++i]);
            }
            else {
                fprintf(stderr, "warning: unrecognized option: %s\n", argv[i]);
            }
        }
        else {
            /* Source file */
            if (!str_has_suffix(argv[i], ".java")) {
                fprintf(stderr, "error: not a Java source file: %s\n", argv[i]);
                compiler_options_free(opts);
                return 1;
            }
            
            if (!opts->source_files) {
                opts->source_files = slist_new(strdup(argv[i]));
                files_tail = opts->source_files;
            } else {
                files_tail = slist_append(files_tail, strdup(argv[i]));
            }
        }
    }
    
    if (expect_value) {
        fprintf(stderr, "error: missing value for option\n");
        compiler_options_free(opts);
        return 1;
    }
    if (!opts->source_files) {
        fprintf(stderr, "error: no source files\n");
        print_usage(argv[0]);
        compiler_options_free(opts);
        return 1;
    }
    
    /* Compile using parallel infrastructure
     * This handles the shared type registry correctly for all cases.
     * -j1 or --serial = single thread, -j or default = auto-detect */
    ast_pool_cleanup();  /* Disable pooled allocation */
    int thread_count = opts->jobs;
    if (thread_count <= 0) {
        thread_count = get_cpu_count();
    }
    int result = compile_parallel(opts, thread_count);
    
    /* Clean up */
    slist_free_full(opts->source_files, free);
    opts->source_files = NULL;
    compiler_options_free(opts);
    
    /* Clean up AST pool */
    ast_pool_cleanup();
    
    /* Clean up string interning */
    intern_cleanup();
    
    return result;
}

