/*
 * Regression test: a type nested inside another class (e.g.
 * "CrossFileNestedGenericHelper.Gc<T>", an interface nested inside class
 * CrossFileNestedGenericHelper) referenced *with type arguments* from a
 * *different* source file compiled in the same batch - used to produce a
 * classfile that failed at class-loading time despite compiling cleanly:
 *
 *   java.lang.NoClassDefFoundError: CrossFileNestedGenericHelper/Gc
 *
 * semantic_resolve_type()'s AST_CLASS_TYPE case, in every branch that
 * builds a *parameterized* type for a name resolved via import (whether
 * found in the compilation unit's type cache or freshly loaded from the
 * classpath/shared registry), built the parameterized type_t with
 * type_new_class(qualified) - "qualified" being the raw, dot-separated
 * name produced by import resolution (e.g. "CrossFileNestedGenericHelper.Gc").
 * For a nested type, class_to_internal_name() (a blind dot-to-slash
 * replacement with no notion of nesting) then turned that into
 * ".../Gc" instead of ".../...$Gc" - using '/' as if the dot before "Gc"
 * were a package separator rather than a nested-class separator.
 *
 * The *raw* (non-generic) reference to the same type didn't hit this bug,
 * because that code path returns the already-resolved symbol's own type
 * directly (already correctly named, e.g. "...$Gc") instead of building a
 * fresh type_t from the raw qualified string - so this only ever surfaced
 * for a *parameterized* reference (with type arguments, or via diamond
 * inference) to a *nested* type resolved *across files*: resolving the
 * outer class's own nested type from within the *same* file (e.g. that
 * outer class building its own method descriptors) instead finds the
 * type via its members list, which is also already correctly named.
 *
 * Fixed by preferring the already-resolved type/symbol's own
 * class_type.name over the raw "qualified" string when building each
 * parameterized type_t.
 */
public class CrossFileNestedGenericTypeTest {
    void accept(CrossFileNestedGenericHelper.Gc<Integer> x) {
        x.done(42);
    }

    public static void main(String[] args) {
        final int[] captured = { -1 };
        CrossFileNestedGenericHelper.Gc<Integer> cb =
                new CrossFileNestedGenericHelper.Gc<Integer>() {
            public void done(Integer t) {
                captured[0] = t;
            }
        };
        new CrossFileNestedGenericTypeTest().accept(cb);
        if (captured[0] != 42) {
            throw new RuntimeException("expected 42, got " + captured[0]);
        }
        System.out.println("All tests passed!");
    }
}
