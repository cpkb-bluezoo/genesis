import java.util.ArrayList;
import java.util.List;

/*
 * Regression test: an anonymous class that BOTH captures an enclosing
 * instance (so it needs a "this$0" field/constructor parameter) AND
 * extends a superclass whose constructor requires explicit arguments
 * (e.g. "new SomeBuilder(name) { ... }") - matching gumdrop's own
 * Meter.counterBuilder(), which does exactly this:
 *
 *   public LongCounter.Builder counterBuilder(String name) {
 *       return new LongCounter.Builder(name) {
 *           public LongCounter build() {
 *               LongCounter counter = super.build();
 *               instruments.add(counter);   // captures enclosing Meter
 *               return counter;
 *           }
 *       };
 *   }
 *
 * Used to throw at runtime (class loads fine, but the actual invocation
 * fails - the verifier doesn't check that a referenced method exists):
 *
 *   java.lang.NoSuchMethodError: 'void Outer$1.<init>(Outer, String)'
 *
 * Root cause: semantic.c collects an anonymous class's own explicit
 * super-constructor arguments (from "new SuperClass(args) { ... }") into
 * a list and stores it on the anonymous class symbol as
 * data.class_data.super_ctor_args - codegen_anonymous_class() already
 * correctly reads this to forward the arguments to super() when
 * generating the synthesized constructor. But that storage only ever
 * happened in the code path that creates the anonymous class symbol for
 * the FIRST time - the far more common path (the symbol already exists,
 * having been created earlier by pass1) recomputed the identical
 * argument list purely to bind lambda arguments to constructor parameter
 * types, then discarded it entirely (slist_free) without ever storing it
 * on the symbol. So super_ctor_args was in practice never set for any
 * ordinary anonymous class reaching that far more common path, and the
 * synthesized constructor silently dropped the explicit argument,
 * calling a nonexistent no-arg super() instead - while the actual call
 * site (which correctly resolves the real constructor via a different
 * code path) still passed the argument, producing a NoSuchMethodError
 * whose declared parameter list is missing exactly the explicit
 * argument.
 *
 * Fixed by also storing the recomputed list onto the symbol in that
 * "already set up" path (guarded so a repeat call doesn't leak the
 * previous list).
 */
public class AnonymousClassSuperCtorArgsWithCaptureVerifyTest {
    static class Builder {
        final String name;
        Builder(String name) {
            this.name = name;
        }
        String build() {
            return "built:" + name;
        }
    }

    List<String> built = new ArrayList<>();

    Builder makeBuilder(String name) {
        return new Builder(name) {
            @Override
            String build() {
                String result = super.build();
                built.add(result);
                return result;
            }
        };
    }

    public static void main(String[] args) {
        AnonymousClassSuperCtorArgsWithCaptureVerifyTest outer =
                new AnonymousClassSuperCtorArgsWithCaptureVerifyTest();

        Builder b = outer.makeBuilder("hello");
        String result = b.build();

        if (!"built:hello".equals(result)) {
            throw new RuntimeException("expected built:hello, got " + result);
        }
        if (outer.built.size() != 1 || !"built:hello".equals(outer.built.get(0))) {
            throw new RuntimeException("expected captured list [built:hello], got " + outer.built);
        }

        System.out.println("AnonymousClassSuperCtorArgsWithCaptureVerifyTest passed!");
    }
}
