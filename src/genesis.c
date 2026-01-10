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

#include "genesis.h"
#include "classpath.h"
#include "classfile.h"
#include "codegen.h"
#include "encoding.h"
#include "jarwriter.h"

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
    
    opts->source_version = strdup("17");
    opts->target_version = strdup("17");
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
        /* JAR output mode */
        success = jar_writer_add_class(g_jar_writer, qualified_name, bytes, size);
        if (success && opts->verbose) {
            printf("Added to JAR: %s.class\n", qualified_name);
        }
    } else {
        /* Directory output mode */
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
        
        /* Create parent directories */
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
        
        /* Write file */
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
        
        free(path_name);
    }
    
    free(bytes);
    return success;
}

/**
 * Check if a source file has already been compiled in this session.
 */
bool is_source_compiled(const char *source_path)
{
    if (!g_compiled_sources || !source_path) {
        return false;
    }
    return hashtable_lookup(g_compiled_sources, source_path) != NULL;
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
    if (!g_compiled_sources) {
        g_compiled_sources = hashtable_new();
    }
    hashtable_insert(g_compiled_sources, source_path, (void *)1);
    
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
    lexer_t *lexer = lexer_new(src);
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
        ast_free(ast);
        return 1;
    }
    
    parser_free(parser);
    lexer_free(lexer);
    
    if (!ast) {
        fprintf(stderr, "error: failed to parse: %s\n", src->filename);
        return 1;
    }
    
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
        ast_free(ast);
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
        ast_free(ast);
        return 1;
    }
    
    if (opts->verbose) {
        printf("Semantic analysis completed: %d error(s), %d warning(s)\n",
               sem->error_count, sem->warning_count);
    }
    
    /* Code generation */
    if (ast->type == AST_COMPILATION_UNIT) {
        /* Find class/module declarations and generate bytecode */
        slist_t *children = ast->data.node.children;
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
                    /* Output the class */
                    const char *qname = class_sym->qualified_name ? 
                                        class_sym->qualified_name : class_name;
                    if (!output_class(cg, qname, opts)) {
                        fprintf(stderr, "error: failed to write class file: %s\n", qname);
                    }
                    
                    /* Process nested classes */
                    for (slist_t *nested = cg->nested_classes; nested; nested = nested->next) {
                        ast_node_t *nested_decl = (ast_node_t *)nested->data;
                        const char *nested_name = nested_decl->data.node.name;
                        
                        /* Look up nested class symbol in outer class's members */
                        symbol_t *nested_sym = scope_lookup_local(
                            class_sym->data.class_data.members, nested_name);
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
                        } else {
                            fprintf(stderr, "error: code generation failed for nested class: %s\n", nested_name);
                        }
                        
                        class_gen_free(nested_cg);
                    }
                    
                    /* Process local classes (defined inside method bodies) */
                    for (slist_t *local = cg->local_classes; local; local = local->next) {
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
                        } else {
                            fprintf(stderr, "error: code generation failed for local class: %s\n", local_name);
                        }
                        
                        class_gen_free(local_cg);
                    }
                    
                    /* Process anonymous classes */
                    for (slist_t *anon = cg->anonymous_classes; anon; anon = anon->next) {
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
                        } else {
                            fprintf(stderr, "error: code generation failed for anonymous class: %s\n", 
                                    anon_sym->qualified_name);
                        }
                        
                        class_gen_free(anon_cg);
                    }
                } else {
                    fprintf(stderr, "error: code generation failed for: %s\n", class_name);
                }
                
                class_gen_free(cg);
            }
            
            children = children->next;
        }
    }
    
    semantic_free(sem);
    ast_free(ast);
    return 0;
}

/**
 * Compile all source files.
 */
int compile(compiler_options_t *opts)
{
    if (!opts || !opts->source_files) {
        fprintf(stderr, "error: no source files\n");
        return 1;
    }
    
    /* Initialize classpath */
    if (!init_classpath(opts)) {
        return 1;
    }
    
    /* Set up global state for dependency compilation */
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
            free_classpath();
            return 1;
        }
        if (opts->verbose) {
            printf("Creating JAR: %s\n", opts->output_jar);
        }
    }
    
    int errors = 0;
    slist_t *list = opts->source_files;
    
    while (list) {
        char *filename = (char *)list->data;
        
        /* Skip if already compiled as a dependency */
        if (!is_source_compiled(filename)) {
            /* Mark as compiled */
            hashtable_insert(g_compiled_sources, filename, (void *)1);
            
            source_file_t *src = source_file_new(filename);
            
            if (src) {
                int result = compile_file(src, opts);
                if (result != 0) {
                    errors++;
                }
                source_file_free(src);
            } else {
                fprintf(stderr, "error: cannot allocate source file: %s\n", filename);
                errors++;
            }
        }
        
        list = slist_next(list);
    }
    
    /* Finalize JAR if in JAR output mode */
    if (g_jar_writer) {
        if (errors > 0) {
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
                errors++;
            }
        }
        g_jar_writer = NULL;
    }
    
    /* Clean up global state */
    hashtable_free(g_compiled_sources);
    g_compiled_sources = NULL;
    g_opts = NULL;
    
    
    /* Clean up classpath */
    free_classpath();
    
    return errors > 0 ? 1 : 0;
}

/* ========================================================================
 * Command-line interface
 * ======================================================================== */

/**
 * Print version information.
 */
void print_version(void)
{
    printf("genesis version %s\n", GENESIS_VERSION);
    printf("Copyright (C) 2016, 2020, 2026 Chris Burdess\n");
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
    printf("\n");
    printf("Options:\n");
    printf("  -d <directory>      Specify output directory for class files\n");
    printf("  -jar <file.jar>     Output to JAR file instead of directory\n");
    printf("  -main-class <class> Specify main class for JAR manifest\n");
    printf("  -cp <path>          Specify classpath\n");
    printf("  -classpath <path>   Specify classpath\n");
    printf("  -sourcepath <path>  Specify source path\n");
    printf("  -source <version>   Specify source version (default: 17)\n");
    printf("  -target <version>   Specify target version (default: 17)\n");
    printf("  -release <version>  Specify release version (sets source and target)\n");
    printf("  -g                  Generate debugging information\n");
    printf("  -g:none             Do not generate debugging information\n");
    printf("  -nowarn             Disable warnings\n");
    printf("  -Werror             Treat warnings as errors\n");
    printf("  -verbose            Enable verbose output\n");
    printf("  -version            Print version information\n");
    printf("  -help               Print this help message\n");
}

/**
 * Program entry point.
 */
int main(int argc, char **argv)
{
    compiler_options_t *opts = compiler_options_new();
    if (!opts) {
        fprintf(stderr, "error: failed to allocate compiler options\n");
        return 1;
    }
    
    slist_t *files_tail = NULL;
    bool expect_value = false;
    char **value_ptr = NULL;
    
    /* Parse command-line arguments */
    for (int i = 1; i < argc; i++) {
        if (expect_value) {
            free(*value_ptr);
            *value_ptr = strdup(argv[i]);
            expect_value = false;
            value_ptr = NULL;
            continue;
        }
        
        if (argv[i][0] == '-') {
            if (strcmp(argv[i], "-version") == 0) {
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
            else if (strcmp(argv[i], "-source") == 0) {
                expect_value = true;
                value_ptr = &opts->source_version;
            }
            else if (strcmp(argv[i], "-target") == 0) {
                expect_value = true;
                value_ptr = &opts->target_version;
            }
            else if (strcmp(argv[i], "-release") == 0) {
                if (i + 1 >= argc) {
                    fprintf(stderr, "error: -release requires a version argument\n");
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
    
    /* Compile */
    int result = compile(opts);
    
    /* Clean up */
    slist_free_full(opts->source_files, free);
    opts->source_files = NULL;
    compiler_options_free(opts);
    
    return result;
}

