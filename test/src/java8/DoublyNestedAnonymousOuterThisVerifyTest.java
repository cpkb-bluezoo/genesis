/*
 * Bug: an anonymous class nested inside ANOTHER anonymous class, referring
 * to the OUTERMOST class via an explicit qualified "Outer.this" (two
 * lexical levels up), threw "NoSuchFieldError: Class ...$1 does not have
 * member field 'Outer this$1'" at runtime. Confirmed against gumdrop's own
 * ServletWebConnection, whose constructor passes "new Runnable() { public
 * void run() { ... ServletWebConnection.this.state.execute(new Runnable()
 * { public void run() { ServletWebConnection.this.state...; } }); } });"
 * hits exactly this shape.
 *
 * Root cause: codegen_load_enclosing_this() (codegen_expr.c), when walking
 * MULTIPLE levels of enclosing instance to reach a non-immediate ancestor,
 * named each hop's synthetic field "this$<depth>" (this$0, this$1, this$2,
 * ...) using an incrementing loop counter. Real javac (and genesis's own
 * field-creation code elsewhere) always names a class's OWN synthetic
 * outer-instance field "this$0", regardless of nesting depth - reaching an
 * ancestor two levels up means chaining TWO separate "this$0" getfields
 * (one per class, each named "this$0" on its own class), never a single
 * field literally named "this$1". Fixed by always using "this$0" as the
 * field name for every hop.
 */
public class DoublyNestedAnonymousOuterThisVerifyTest {
    String label = "outer";
    String[] captured = new String[1];

    /*
     * The branch lives in the OUTER anonymous class only, matching
     * gumdrop's own ServletWebConnection shape exactly - the innermost
     * anonymous class's run() is a single straight-line statement, no
     * branch of its own. (A branch inside the INNERMOST doubly-nested
     * anonymous class's own method hits a separate, still-open
     * StackMapTable bug - see current_task.md's "still open" section -
     * not exercised by gumdrop and not this bug.)
     */
    Runnable get() {
        return new Runnable() {
            public void run() {
                if (DoublyNestedAnonymousOuterThisVerifyTest.this.label != null) {
                    Runnable inner = new Runnable() {
                        public void run() {
                            captured[0] = DoublyNestedAnonymousOuterThisVerifyTest.this.label;
                        }
                    };
                    inner.run();
                }
            }
        };
    }

    public static void main(String[] args) {
        DoublyNestedAnonymousOuterThisVerifyTest t = new DoublyNestedAnonymousOuterThisVerifyTest();
        t.get().run();
        if (!"outer".equals(t.captured[0])) {
            throw new RuntimeException("expected 'outer' but got " + t.captured[0]);
        }
        System.out.println("DoublyNestedAnonymousOuterThisVerifyTest passed!");
    }
}
