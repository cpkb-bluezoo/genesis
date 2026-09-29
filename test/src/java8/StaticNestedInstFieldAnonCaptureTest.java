/*
 * Regression test: an anonymous class created directly inside a *non*-
 * static instance field initializer of a *static* nested class (e.g.
 * "private static final class Holder { private final ExecutorService
 * loop = Executors.newSingleThreadExecutor(new ThreadFactory() {...}); }"
 * - exactly gumdrop's CryptoExecutorTest/StorageExecutorTest's own
 * "LoopEndpoint" helper) used to throw at construction:
 *
 *   java.lang.NoSuchMethodError: Holder$1: method 'void <init>()' not
 *   found
 *
 * even though the anonymous class doesn't reference any instance member
 * of Holder at all - by JLS 15.9.5, an anonymous class not in a static
 * context still implicitly captures the enclosing instance (Holder.this)
 * regardless of whether it actually uses it, and Holder's own
 * *instance* field initializer runs during Holder's <init>, not
 * <clinit> - so the anonymous class here needed a this$0-capturing
 * constructor.
 *
 * The anonymous class's own constructor WAS correctly generated to
 * accept the enclosing instance (semantic.c already tracks this
 * correctly via sem->in_static_field_init, gated on the FIELD's own
 * MOD_STATIC flag) - the bug was purely at the *call site*
 * (codegen_expr.c's "new Anonymous(){...}" instantiation codegen,
 * inlined into Holder's generated constructor): its enclosing_is_static
 * computation had a fallback "if (!target_sym->data.class_data.
 * enclosing_method) enclosing_is_static = true;" reasoning that "no
 * enclosing method" (this anonymous class isn't inside any method body -
 * it's directly in a field initializer) must mean a static initializer
 * context - but an *instance* field initializer or instance
 * initializer block also has no enclosing method, and is NOT a static
 * context. This unconditionally (and wrongly) overrode the correct,
 * already-computed non-static verdict, skipping the outer-instance push
 * and invokespecial descriptor. Fixed by removing that fallback and
 * relying solely on target_sym->modifiers (already correctly computed by
 * semantic.c from the same in_static_field_init reasoning either way).
 */
public class StaticNestedInstFieldAnonCaptureTest {
    interface Factory {
        Object make();
    }

    private static final class Holder {
        private final Factory factory = new Factory() {
            @Override
            public Object make() {
                return new Object();
            }
        };

        Object get() {
            return factory.make();
        }
    }

    public static void main(String[] args) {
        Holder h = new Holder();
        if (h.get() == null) {
            throw new RuntimeException("factory.make() returned null");
        }
        System.out.println("StaticNestedInstFieldAnonCaptureTest passed!");
    }
}
