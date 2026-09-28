/*
 * Regression test: a self-referential `instanceof` check against the
 * enclosing class itself (e.g. a private static `cast(Object o)` helper
 * doing `if (!(o instanceof Thing)) ...` inside class Thing) used to
 * produce a classfile that threw at *runtime*, not compile time:
 *
 *   java.lang.NoClassDefFoundError: Thing
 *   Caused by: java.lang.ClassNotFoundException: Thing
 *
 * Reproduces only when the class is in a non-default package (needed here
 * via this test's own "instanceoftest" package - see the wiring for this
 * test in run-tests.sh, since the top-level auto-discovery loop only
 * compiles files directly under test/src/java8/, not packaged
 * subdirectories).
 *
 * get_expression_type()'s AST_INSTANCEOF_EXPR case in semantic.c never
 * resolved the right-hand type at all (unlike AST_CAST_EXPR, which calls
 * semantic_resolve_type() on its target type node and caches the result on
 * type_node->sem_type) - it only recursed into the left-hand operand, for
 * capture detection. codegen_expr.c's AST_INSTANCEOF_EXPR codegen then had
 * nothing but the type's bare, unqualified AST source name
 * (type_node->data.node.name) to build the `instanceof` instruction's
 * class constant from. For an imported type this happens to still resolve
 * correctly by luck (or is masked by other lookups), but a same-package
 * type - and especially a self-reference, which needs no import statement
 * at all - has no qualification anywhere in the source text, so the
 * instanceof instruction ended up checking against a bogus unqualified
 * class name ("Thing" instead of "instanceoftest/Thing"), which the JVM
 * then failed to load at runtime. Fixed by having semantic.c resolve (and
 * cache) the instanceof's type node the same way AST_CAST_EXPR does, and
 * having codegen_expr.c prefer that resolved, properly qualified type over
 * the bare AST name - mirroring AST_CAST_EXPR's own codegen exactly.
 */
package instanceoftest;

public class InstanceofSelfQualifyVerifyTest {
    boolean marker;

    private static InstanceofSelfQualifyVerifyTest cast(Object o) {
        if (!(o instanceof InstanceofSelfQualifyVerifyTest)) {
            throw new ClassCastException();
        }
        return (InstanceofSelfQualifyVerifyTest) o;
    }

    public static void main(String[] args) {
        InstanceofSelfQualifyVerifyTest t = new InstanceofSelfQualifyVerifyTest();
        t.marker = true;
        InstanceofSelfQualifyVerifyTest t2 = cast(t);
        if (t2 != t || !t2.marker) {
            throw new RuntimeException("expected cast() to return the same instance");
        }
        System.out.println("All tests passed!");
    }
}
