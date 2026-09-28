/*
 * Regression test: an anonymous class created inside a *static* field's
 * initializer (e.g. "private static final Executor X = new Executor()
 * {...};") used to throw at class-init time despite compiling cleanly:
 *
 *   java.lang.NoSuchMethodError: AnonClassStaticFieldInitTest$1: method
 *   'void <init>()' not found
 *
 * pass1_collect_declarations() is an iterative (stack-based) AST walker
 * that generically descends into every node's children - including a
 * static field's initializer expression, several levels deep - well before
 * pass2's own, separate field-initializer type-checking pass runs. When
 * that generic descent reaches a "new SomeInterface() { ... }" anonymous
 * class expression inside the initializer, pass1's own AST_NEW_OBJECT
 * handling creates (and permanently caches, on the anonymous body's own
 * AST node) the anonymous class's symbol, including its is-static
 * classification - based on sem->in_static_field_init, a flag pass1's
 * AST_FIELD_DECL case never touched (only pass2's later, deferred
 * counterpart did). So by the time pass1 reached the nested anonymous
 * class, that flag was always false (its default), and the anonymous
 * class was permanently misclassified as a non-static inner class - given
 * a synthetic this$0 field and an enclosing-instance constructor parameter
 * by codegen's class-generation side. But the separate call-site codegen
 * (generating the "new" expression itself) correctly treated it as static,
 * since it derives its own answer independently - and generated a no-arg
 * constructor call. The result: a classfile where the anonymous class
 * itself only defines a one-argument constructor, but every caller invokes
 * a no-argument one - a mismatch caught only at runtime, not compile time.
 *
 * Fixed by having pass1's AST_FIELD_DECL case also save/set/restore
 * sem->in_static_field_init around its own subtree's traversal (mirroring
 * the scope/class/method save-restore idiom this same iterative walker
 * already uses via its WALK_ENTER/WALK_EXIT frames), so the flag is already
 * correct by the time pass1's generic descent reaches a nested anonymous
 * class expression.
 */
import java.util.concurrent.Executor;

public class AnonClassStaticFieldInitTest {
    private static final Executor SAME_THREAD = new Executor() {
        @Override
        public void execute(Runnable command) {
            command.run();
        }
    };

    public static void main(String[] args) {
        final boolean[] ran = { false };
        SAME_THREAD.execute(new Runnable() {
            public void run() {
                ran[0] = true;
            }
        });
        if (!ran[0]) {
            throw new RuntimeException("expected SAME_THREAD.execute() to run the command");
        }
        System.out.println("All tests passed!");
    }
}
