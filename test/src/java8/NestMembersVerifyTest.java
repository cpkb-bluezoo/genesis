/*
 * Regression test: an anonymous (or local) inner class directly accessing
 * a *private* field or method of its enclosing class - not a captured
 * local variable, which uses its own separate val$xxx-field mechanism, but
 * the enclosing instance's own private member, reached the ordinary way
 * (an unqualified reference resolved to the outer class's own field) -
 * used to compile and verify cleanly, but throw at the first access:
 *
 *   java.lang.IllegalAccessError: class Outer$1 tried to access private
 *   field Outer.field (... current type is not listed as a nest member)
 *
 * Since Java 11, this kind of access relies on the classfile format's
 * "nestmates" feature (JVMS 4.7.28/4.7.29): the outer class's own
 * classfile must list every nested/local/anonymous class - at every
 * depth - in its NestMembers attribute, and each of those classes must in
 * turn declare the outer class as its NestHost. Without both sides
 * present, the JVM refuses the access at runtime (this isn't checked by
 * the bytecode verifier at all, only by this separate access-control
 * check the first time the access actually happens) - even though
 * genesis's classfiles were otherwise entirely correct.
 *
 * genesis's own NestHost-side logic (in each nested class's own
 * class_gen_new()) was already correct - every such class already
 * declared its own NestHost pointing at the true top-level enclosing
 * class. The missing half was the NestMembers side: nothing ever told the
 * *outer* class's own classfile which nested/local/anonymous classes
 * existed below it, because - architecturally - the outer class's bytes
 * are finalized and written out immediately after its own codegen
 * finishes, while its nested/local/anonymous descendants are only
 * discovered afterward, one level at a time, as each is separately
 * codegen'd and written. By the time any of them were known, the outer
 * class had already been written without them.
 *
 * Fixed by a new genesis.c function, collect_nest_members_recursive(),
 * called right after the outer class's own codegen finishes but before it
 * is written out: it walks the exact same discovery structure the real
 * (deferred) processing later uses - reusing codegen_class()/
 * codegen_anonymous_class() themselves on throwaway, never-written
 * class_gen_t instances purely to enumerate each level's own nested/
 * local/anonymous classes, transitively - to build the complete
 * NestMembers list up front.
 */
public class NestMembersVerifyTest {
    private String secret = "outer-secret";

    private String makeInner() {
        Runnable r = new Runnable() {
            public void run() {
                /* Direct access to the outer instance's own private field,
                 * not a captured local - exercises the nest-member gap. */
                secret = secret + "-touched";
            }
        };
        r.run();
        return secret;
    }

    private String makeDoublyNested() {
        final String[] result = { null };
        final String captured = secret;
        Runnable outer = new Runnable() {
            public void run() {
                Runnable inner = new Runnable() {
                    public void run() {
                        /* Two levels of anonymous-class nesting, both
                         * needing NestMembers entries in the same
                         * top-level host (a *direct* private-field access
                         * two levels up, via a multi-hop this$0 chain, is
                         * a separate, not-yet-fixed bug - see
                         * current_task.md - so this uses a captured local
                         * instead, to isolate the NestMembers-at-depth
                         * behavior this test targets). */
                        result[0] = captured;
                    }
                };
                inner.run();
            }
        };
        outer.run();
        return result[0];
    }

    public static void main(String[] args) {
        NestMembersVerifyTest t = new NestMembersVerifyTest();
        String result = t.makeInner();
        if (!"outer-secret-touched".equals(result)) {
            throw new RuntimeException("expected outer-secret-touched, got " + result);
        }
        String nested = t.makeDoublyNested();
        if (!"outer-secret-touched".equals(nested)) {
            throw new RuntimeException("expected outer-secret-touched, got " + nested);
        }
        System.out.println("All tests passed!");
    }
}
