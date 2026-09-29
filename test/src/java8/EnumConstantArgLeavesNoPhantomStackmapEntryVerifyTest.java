/**
 * Bug: constructing each enum constant in <clinit> ("new E; dup; ldc name;
 * bipush ordinal; <user args>; invokespecial <init>; putstatic") popped the
 * invokespecial's operands with a raw, untyped pop that only adjusted the
 * runtime stack-depth counter (used for max_stack), not mg->stackmap (the
 * verifier type-tracking list actually serialized into StackMapTable
 * frames) - even though each user-supplied constructor argument (here,
 * "code") WAS pushed through the normal typed codegen_expr() path and so
 * WAS added to mg->stackmap. The result: one phantom "int" leaked into
 * mg->stackmap's tracked stack per enum constant, never popped, even
 * though the real bytecode is perfectly balanced. A later stackmap frame
 * recorded anywhere else in the SAME <clinit> - here, the enum's own
 * static block with a for-each loop over values() populating a lookup map
 * (needed to get a real frame recorded in <clinit> at all) - then claimed
 * a bogus operand stack padded with one leftover "int" per constant
 * constructed before it: ClassFormatError "bad type array size" once
 * enough constants exist to actually exceed the method's declared
 * max_stack. Confirmed against gumdrop's own HttpStatus (64 constants, a
 * single "int code" constructor arg, plus a static lookup-map-building
 * for-each loop - EXACTLY this shape, just at enum scope instead of a
 * wrapper class, which is why an earlier, structurally-different draft of
 * this test failed to reproduce the bug at all).
 */
public class EnumConstantArgLeavesNoPhantomStackmapEntryVerifyTest {
    enum Code {
        A(1), B(2), C(3), D(4), E(5), F(6), G(7), H(8), I(9), J(10),
        K(11), L(12), M(13), N(14), O(15), P(16), Q(17), R(18), S(19), T(20);

        final int value;

        Code(int value) {
            this.value = value;
        }

        private static final java.util.Map<Integer, Code> BY_VALUE = new java.util.HashMap<Integer, Code>();

        static {
            for (Code c : values()) {
                if (c.value > 0) {
                    BY_VALUE.put(c.value, c);
                }
            }
        }

        static Code fromValue(int value) {
            return BY_VALUE.get(value);
        }
    }

    public static void main(String[] args) {
        if (Code.fromValue(3) != Code.C) {
            throw new RuntimeException("expected C for value 3, got " + Code.fromValue(3));
        }
        if (Code.fromValue(20) != Code.T) {
            throw new RuntimeException("expected T for value 20, got " + Code.fromValue(20));
        }
    }
}
