/*
 * Regression test: an instance (or static) field initializer whose value
 * comes from unboxing a wrapper-returning call to a wide primitive type
 * (e.g. "private long timeout = Long.getLong(\"some.prop\", 5000L);" -
 * relying on implicit Long -> long auto-unboxing), immediately followed
 * by another statement that records its own stack-map frame (here, an
 * "if (workerCount < 1) throw ...;" check, whose boolean condition
 * genesis materializes via the classic ifxx/iconst_1/goto/iconst_0
 * idiom) - exactly gumdrop's own Gumdrop(int) constructor shape - used
 * to throw:
 *
 *   java.lang.VerifyError: Inconsistent stackmap frames at branch target N
 *   Reason: Current frame's stack size doesn't match stackmap.
 *
 * Root cause: THREE independent, compounding bugs, each masking the
 * next until fixed in order:
 *
 * 1. emit_unboxing() (codegen_expr.c) only bumped the raw
 *    mg->stack_depth counter for a wide (long/double) unboxed result
 *    ("if (stack_adjust > 0) mg_push(mg, stack_adjust);") - never
 *    touching mg->stackmap at all, so the wrapper reference's own
 *    (now-stale) stackmap entry was never popped/replaced by a
 *    correctly-typed primitive entry. Fixed by popping the wrapper's
 *    entry and pushing a properly-typed one via the type-aware
 *    mg_push_long()/_double()/_float()/_int() helpers.
 *
 * 2. Fixing (1) alone still left corruption, traced to
 *    codegen_expr.c's plain-assignment-to-a-field codegen (the
 *    "DUP_X1/DUP2_X1 to keep a copy of value for chained assignments"
 *    comment): OP_DUP2_X1 (used for a wide field) reorders the *real*
 *    JVM stack (duplicating the top category-2 value below the
 *    receiver reference), but the code only called a raw
 *    "mg_push(mg, 2)" afterward - never reordering mg->stackmap's own
 *    tracked types to match. Fixed by adding stackmap_dup_x1()/
 *    stackmap_dup2_x1() (stackmap.c) plus mg_dup_x1()/mg_dup2_x1()
 *    wrappers (codegen.c), used at that call site instead.
 *
 * 3. Even with (1) and (2) fixed, THIS test still failed - because
 *    instance/static field *initializers* (as opposed to an ordinary
 *    assignment expression elsewhere in a method body) are injected by
 *    a separate, dedicated code path in codegen.c (several near-
 *    identical copies, for different constructor-generation contexts),
 *    which doesn't need the DUP_X1 dance at all (a field initializer's
 *    assigned value is never reused) - but still pushed/popped the
 *    receiver reference and the initializer's value using raw
 *    mg_push()/mg_pop() (word-counter only), never mg_push_object()/
 *    mg_pop_typed(). This meant mg->stackmap never got the receiver
 *    entry added NOR the initializer's pushed value(s) removed at all -
 *    for a wide initializer value, its 2 tracking slots permanently
 *    leaked into mg->stackmap's tracked state, corrupting every
 *    stack-map frame recorded afterward in the same method. Fixed by
 *    switching every such site (there are several, covering different
 *    constructor-generation and static-initializer code paths) to the
 *    type-aware mg_push_object()/mg_pop_typed().
 */
public class LongFieldInitStackmapVerifyTest {
    private static final long DEFAULT = 5000L;
    private final Object lock = new Object();
    private long timeout = Long.getLong("genesis.test.timeout.prop.does.not.exist", DEFAULT);

    LongFieldInitStackmapVerifyTest(int workerCount) {
        if (workerCount < 1) {
            throw new IllegalArgumentException("workerCount must be at least 1");
        }
    }

    public static void main(String[] args) {
        LongFieldInitStackmapVerifyTest t = new LongFieldInitStackmapVerifyTest(3);
        if (t.timeout != 5000L) {
            throw new RuntimeException("expected 5000, got " + t.timeout);
        }
        if (t.lock == null) {
            throw new RuntimeException("expected non-null lock");
        }
        System.out.println("LongFieldInitStackmapVerifyTest passed!");
    }
}
