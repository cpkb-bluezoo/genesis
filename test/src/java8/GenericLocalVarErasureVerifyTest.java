/*
 * Regression test: a local variable declared with a generic type parameter
 * as its type (e.g. "T result = null;" inside a generic method's body, or
 * an anonymous class capturing such a method's own type parameter) used to
 * produce a classfile that failed at *runtime*, not compile or verify time:
 *
 *   java.lang.NoClassDefFoundError: T
 *   Caused by: java.lang.ClassNotFoundException: T
 *
 * codegen_stmt.c's AST_VAR_DECL handling, for a declared type written as a
 * class type (AST_CLASS_TYPE, not "var"), only recognized
 * first->sem_type->kind == TYPE_CLASS - for anything else (including
 * TYPE_TYPEVAR, a generic type parameter like "T"), it fell straight
 * through to a fallback meant for when sem_type isn't available at all,
 * which used the type node's bare AST source name directly as a class
 * name. A type variable is not a class and JLS type erasure requires it be
 * replaced with its bound (java.lang.Object if unbounded) - which
 * type_to_descriptor() already does correctly everywhere else in codegen -
 * but this path built the local's StackMapTable tracking (and the class
 * constant used for its LocalVariableTable entry) from the literal string
 * "T", producing a CONSTANT_Class entry for a class that doesn't exist.
 * The verifier doesn't check a referenced class actually exists, so this
 * was only ever caught the first time the JVM actually tried to resolve
 * it. Fixed by erasing a TYPE_TYPEVAR sem_type via type_to_descriptor()
 * (stripping the leading 'L' and trailing ';') before falling back to the
 * raw AST name.
 */
import java.util.concurrent.Callable;

public class GenericLocalVarErasureVerifyTest {
    interface Callback<T> {
        void done(T result);
    }

    <T> void submit(Callable<T> operation, Callback<T> callback) {
        final Runnable task = new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable error = null;
                try {
                    result = operation.call();
                } catch (Throwable t) {
                    error = t;
                }
                final T finalResult = result;
                if (error == null) {
                    callback.done(finalResult);
                }
            }
        };
        task.run();
    }

    public static void main(String[] args) {
        GenericLocalVarErasureVerifyTest g = new GenericLocalVarErasureVerifyTest();
        final boolean[] ran = { false };
        g.submit(new Callable<String>() {
            public String call() {
                return "hello";
            }
        }, new Callback<String>() {
            public void done(String result) {
                if (!"hello".equals(result)) {
                    throw new RuntimeException("expected hello, got " + result);
                }
                ran[0] = true;
            }
        });
        if (!ran[0]) {
            throw new RuntimeException("callback did not run");
        }
        System.out.println("All tests passed!");
    }
}
