/*
 * FunctionalInterfaceSamTest.c - a superinterface stored without its methods
 *
 * Phase 4 of a batch can attach to an interface a symbol for its
 * superinterface that has the superinterface chain and no methods and no
 * completer (symbol_from_classfile_minimal). Code generation runs after
 * semantic analysis has finished, when no analyzer is current, so the single
 * abstract method of
 *
 *     interface Box extends Consumer<String> {
 *         static Box get() { return System.out::println; }
 *     }
 *
 * - which comes entirely from Consumer - was not found ("codegen: cannot
 * find SAM in functional interface"). The test analyses Box for real, puts
 * such a stub in place of Consumer and asks for the SAM the way code
 * generation does. It is built and run by test/run-tests.sh.
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "genesis.h"
#include "classpath.h"

static const char *SOURCE_TEXT =
    "import java.util.function.Consumer;\n"
    "interface Box extends Consumer<String> {\n"
    "    static Box get() { return System.out::println; }\n"
    "}\n";

static int failures = 0;

static void expect_sam(const char *what, semantic_t *sem, symbol_t *box)
{
    /* As generate_file_code() does: the file's analyzer is current */
    semantic_t *previous = semantic_set_current(sem);
    symbol_t *sam = get_functional_interface_sam(box);
    semantic_set_current(previous);
    if (!sam || !sam->name || strcmp(sam->name, "accept") != 0) {
        printf("%s: SAM of Box is %s, expected accept\n", what,
               sam && sam->name ? sam->name : "(none)");
        failures++;
    }
}

/* What symbol_from_classfile_minimal() makes: the chain, no members */
static symbol_t *make_stub(symbol_t *like)
{
    symbol_t *stub = calloc(1, sizeof(symbol_t));
    if (!stub) {
        exit(2);
    }
    stub->kind = SYM_INTERFACE;
    stub->name = like->name;
    stub->qualified_name = like->qualified_name;
    stub->type = like->type;
    return stub;
}

int main(void)
{
    intern_init();
    ast_pool_init();

    classpath_t *cp = classpath_new();
    jdk_info_t *jdk = jdk_detect();
    if (!cp || !jdk || !classpath_setup_jdk(cp, jdk)) {
        printf("no JDK to read java.util.function.Consumer from\n");
        return 2;
    }
    jdk_info_free(jdk);

    char path[] = "/tmp/FunctionalInterfaceSamTestXXXXXX";
    int fd = mkstemp(path);
    if (fd < 0) {
        return 2;
    }
    FILE *f = fdopen(fd, "w");
    fputs(SOURCE_TEXT, f);
    fclose(f);

    source_file_t *src = source_file_new(path);
    if (!src || !source_file_load(src)) {
        return 2;
    }
    lexer_t *lexer = lexer_new(src, 17);
    parser_t *parser = parser_new(lexer, src);
    ast_node_t *ast = parser_parse(parser);
    if (!ast || parser->error_msg) {
        printf("parse failed\n");
        return 2;
    }
    resolve_types_in_compilation_unit(ast, cp, NULL, NULL);

    semantic_t *sem = semantic_new(cp);
    if (!sem || !semantic_analyze(sem, ast, src)) {
        printf("semantic analysis failed\n");
        return 2;
    }
    remove(path);

    symbol_t *box = NULL;
    for (slist_t *c = ast->data.node.children; c; c = c->next) {
        ast_node_t *child = (ast_node_t *)c->data;
        if (child && child->type == AST_INTERFACE_DECL) {
            box = child->sem_symbol;
        }
    }
    if (!box || !box->data.class_data.interfaces) {
        printf("Box has no superinterface\n");
        return 2;
    }

    /* As analysed: the baseline */
    expect_sam("as analysed", sem, box);

    /* Box's own list holds a stub; the analyzer's cache has the full class */
    symbol_t *consumer = (symbol_t *)box->data.class_data.interfaces->data;
    box->data.class_data.interfaces->data = make_stub(consumer);
    expect_sam("stub in the interface list", sem, box);

    /* The cache holds the stub too: the class file has to be read again */
    type_t *cached = hashtable_lookup(sem->types, consumer->qualified_name);
    if (cached && cached->kind == TYPE_CLASS) {
        cached->data.class_type.symbol = (symbol_t *)box->data.class_data.interfaces->data;
    }
    expect_sam("stub in the cache as well", sem, box);

    if (failures == 0) {
        printf("ok\n");
    }
    return failures == 0 ? 0 : 1;
}
