package pkgclasslit;

/*
 * Regression test: a class literal used as a `synchronized` statement's
 * lock expression, on a class that lives in a real (non-default)
 * package - exactly gumdrop's own HostsFile.java shape
 * ("synchronized (HostsFile.class) { ... }") - used to throw:
 *
 *   java.lang.NoClassDefFoundError: Foo
 *
 * i.e. the class literal's own internal name lost its package prefix
 * entirely.
 *
 * Root cause: two independent gaps, both needed together:
 *
 * 1. codegen.c's ast_type_to_descriptor() (used to build the ldc
 *    CONSTANT_Class this ".class" literal compiles to), in its
 *    AST_IDENTIFIER branch (an identifier used as a bare type name, e.g.
 *    "Foo" in "Foo.class"), never consulted type_node->sem_type at all -
 *    unlike its sibling AST_CLASS_TYPE branch just above, which does.
 *    It went straight to the bare source identifier, only special-casing
 *    a small hardcoded list of common java.lang types (String, Object,
 *    etc.) - anything else (like a type in a real package) got the bare
 *    name with no package prefix. Fixed by checking type_node->sem_type
 *    first, mirroring AST_CLASS_TYPE.
 *
 * 2. Even with (1) fixed, this was still unreachable in practice: no
 *    semantic-analysis pass ever visited (called get_expression_type on)
 *    a synchronized statement's own lock expression at all - unlike an
 *    if/while/for's own condition, which each have their own explicit
 *    case in the same pass2 statement walker (needed for their "must be
 *    boolean" checks). Without visiting it, AST_CLASS_LITERAL's own
 *    get_expression_type case (which resolves and self-annotates
 *    type_node->sem_type) never ran, so type_node->sem_type stayed NULL
 *    regardless of fix (1). Fixed by adding an AST_SYNCHRONIZED_STMT case
 *    to that same walker that resolves the lock expression's type (no
 *    validation needed - any reference type is a legal monitor).
 */
public final class Foo {
    private static int counter;

    public static void bump() {
        synchronized (Foo.class) {
            counter++;
        }
    }

    public static int get() {
        synchronized (Foo.class) {
            return counter;
        }
    }
}
